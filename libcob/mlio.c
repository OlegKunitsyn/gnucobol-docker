/*
   Copyright (C) 2018-2020 Free Software Foundation, Inc.
   Written by Edward Hart, Simon Sobisch

   This file is part of GnuCOBOL.

   The GnuCOBOL runtime library is free software: you can redistribute it
   and/or modify it under the terms of the GNU Lesser General Public License
   as published by the Free Software Foundation, either version 3 of the
   License, or (at your option) any later version.

   GnuCOBOL is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU Lesser General Public License for more details.

   You should have received a copy of the GNU Lesser General Public License
   along with GnuCOBOL.  If not, see <https://www.gnu.org/licenses/>.
*/


#include <config.h>

#include <string.h>
#include <stddef.h>
#include <ctype.h>
#include <stdio.h>

/* Force symbol exports */
#define	COB_LIB_EXPIMP
#include "libcob.h"
#include "coblocal.h"

/* note: checked library instead of headers as those may not be usable! */
#ifdef WITH_XML2
#if !defined (HAVE_LIBXML_XMLVERSION_H) || \
    !defined (HAVE_LIBXML_XMLWRITER_H) || \
	!defined (HAVE_LIBXML_URI_H)
#error XML2 without necessary headers
#endif
#include <libxml/xmlversion.h>
#include <libxml/xmlwriter.h>
#include <libxml/uri.h>
#endif

#ifdef WITH_CJSON
#if defined HAVE_CJSON_CJSON_H
#include <cjson/cJSON.h>
#elif defined HAVE_CJSON_H
#include <cJSON.h>
#else
#error CJSON without necessary header
#endif
#endif


/* Local variables */

/* de facto standard error codes */
enum xml_code_status {
	XML_OUT_FIELD_TOO_SMALL = 400,
	XML_INVALID_NAMESPACE = 416,
	XML_INVALID_CHAR_REPLACED = 417,
	XML_INVALID_NAMESPACE_PREFIX = 419,
	XML_INTERNAL_ERROR = 600
};

enum json_code_status {
	JSON_OUT_FIELD_TOO_SMALL = 1,
	JSON_INTERNAL_ERROR = 500
};


static cob_global		*cobglobptr;

/* Local functions */

#if WITH_XML2 || WITH_CJSON

static void *
get_trimmed_data (const cob_field * const f, void * (*strndup_func)(const char *, size_t))
{
	char	*str = (char *) f->data;
	size_t	len = f->size;

	/* Trim leading/trailing spaces. If f is all spaces, leave one space. */
	if (COB_FIELD_JUSTIFIED (f)) {
		for (; *str == ' ' && len > 1; ++str, --len);
	} else {
		for (; str[len - 1] == ' ' && len > 1; --len);
	}

	return (*strndup_func)(str, len);
}

static cob_pic_symbol *
get_pic_for_num_field (const size_t num_int_digits, const size_t num_dec_digits)
{
	size_t	num_pic_symbols = (size_t)2 + (2 * !!num_dec_digits) + 1;
	cob_pic_symbol	*pic = cob_malloc (num_pic_symbols * sizeof (cob_pic_symbol));
	cob_pic_symbol	*symbol = pic;

	symbol->symbol = '-';
	symbol->times_repeated = cob_max_int ((int) num_int_digits, 1);
	++symbol;

	symbol->symbol = '9';
	symbol->times_repeated = 1;
	++symbol;

	if (num_dec_digits) {
		symbol->symbol = COB_MODULE_PTR->decimal_point;
		symbol->times_repeated = 1;
		++symbol;

		symbol->symbol = '9';
		symbol->times_repeated = (int) num_dec_digits;
		++symbol;
	}

	symbol->symbol = '\0';

	return pic;
}

static void *
get_num (cob_field * const f, void * (*strndup_func)(const char *, size_t))
{
	size_t		num_integer_digits
		= cob_max_int (0, COB_FIELD_DIGITS (f) - COB_FIELD_SCALE (f));
	size_t		num_decimal_digits
		= cob_max_int (0, COB_FIELD_SCALE (f));
	cob_field_attr	attr;
	cob_field       edited_field;
        void		*num;

	/* Initialize field attribute */
	attr.type = COB_TYPE_NUMERIC_EDITED;
	attr.flags = COB_FLAG_JUSTIFIED;
	attr.scale = COB_FIELD_SCALE (f);
	attr.digits = COB_FIELD_DIGITS (f);
	attr.pic = get_pic_for_num_field (num_integer_digits,
					      num_decimal_digits);

	/* Initialize field */
	edited_field.attr = &attr;
	edited_field.size = cob_max_int (2, (int) num_integer_digits + 1);
	if (num_decimal_digits) {
		edited_field.size += 1 + num_decimal_digits;
	}
	edited_field.data = cob_malloc (edited_field.size);

	cob_move (f, &edited_field);
	num = get_trimmed_data (&edited_field, strndup_func);

	cob_free (edited_field.data);
	cob_free ((void *) edited_field.attr->pic);

	return num;

}
#endif

#if WITH_XML2

static void
set_xml_code (const unsigned int code)
{
	/* if the COBOL module never checks the code it isn't generated,
	   this also makes clear that we don't need to (and can't) set it */
	if (!COB_MODULE_PTR->xml_code) {
		return;
	}
	cob_set_field_to_uint (COB_MODULE_PTR->xml_code, code);
}

static int
is_all_spaces (const cob_field * const f)
{
	size_t	i;

	for (i = 0; i < f->size; ++i) {
		if (f->data[i] != ' ') {
			return 0;
		}
	}

	return 1;
}

static void *
xmlCharStrndup_void (const char *str, const size_t size)
{
	return (void *)xmlCharStrndup (str, size);
}

static xmlChar *
get_trimmed_xml_data (const cob_field * const f)
{
	return (xmlChar *) get_trimmed_data (f, &xmlCharStrndup_void);
}

/* Returns 1 if str contains invalid XML 1.0 chars, 0 otherwise. */
static int
has_invalid_xml_char (const cob_field * const f)
{
	size_t	i;

	/*  Char       ::=      #x9 | #xA | #xD | [#x20-#xD7FF] | [#xE000-#xFFFD] | [#x10000-#x10FFFF] */
	/* TO-DO: This assumes the data is already in UTF-8! */
	for (i = 0; i < f->size; ++i) {
		if (iscntrl (f->data[i])
		    && f->data[i] != 0x09
		    && f->data[i] != 0x0a
		    && f->data[i] != 0x0d) {
			return 1;
		}
	}

	/* TO-DO: 2/3/4-byte characters. Will this need libicu? */

	return 0;
}

static int
is_valid_xml_name (const cob_field * const f)
{
	xmlChar	*str;
	xmlChar	*c;
	int	ret;

	str = get_trimmed_xml_data (f);

	if (!cob_is_xml_namestartchar (f->data[0])) {
		ret = 0;
		goto end;
	}

	for (c = str + 1; *c; ++c) {
		if (!cob_is_xml_namechar (*c)) {
			ret = 0;
			goto end;
		}
	}

	ret = 1;

 end:
	xmlFree (str);
	return ret;
}

static xmlChar *
get_xml_name (const cob_field * const f)
{
	xmlChar	*name;
	xmlChar	*underscore;
	xmlChar	*name_with_underscore;

	name = get_trimmed_xml_data (f);

	if (name && !cob_is_xml_namestartchar (name[0])) {
		underscore = xmlCharStrdup ("_");
		if (underscore) {
			name_with_underscore = xmlStrcat (underscore, name);
		} else {
			name_with_underscore = NULL;
		}

		xmlFree (name);
		return name_with_underscore;
	} else {
		return name;
	}
}

#define IF_NEG_RETURN_ELSE_COUNT(func)			\
	do {						\
		int	macro_status = (func);		\
		if (macro_status < 0) {			\
			return macro_status;			\
		} else {				\
			*count += macro_status;		\
		}					\
	} ONCE_COB

static int
generate_xml_from_tree (xmlTextWriterPtr, cob_ml_tree *, xmlChar *, xmlChar *,
			unsigned int *);

static xmlChar *
get_name_with_hex_prefix (const cob_field * const name)
{
	xmlChar	*hex_str;
	xmlChar	*x_name;
	xmlChar	*hex_name;

	/*
	  NB: hex_str must be allocated every time because xmlStrcat will
	  realloc hex_str.
	*/
	hex_str = xmlCharStrdup ("hex.");

	x_name = get_xml_name (name);
	hex_name = xmlStrcat (hex_str, x_name);
	xmlFree (x_name);

	return hex_name;
}

static char
int_to_hex (int n)
{
	if (n < 10) {
		n = n + '0';
	} else {
		n = n - 10 + 'a';
	}
	return (char)n;
}

static xmlChar *
get_hex_xml_data (const cob_field * const f)
{
	xmlBufferPtr	buff;
	size_t		i;
	char		hex_num[3] = { '\0' };
	xmlChar		*hex_data;

	buff = xmlBufferCreate ();
	if (!buff) {
		return NULL;
	}

	for (i = 0; i < f->size; ++i) {
		hex_num[0] = int_to_hex (f->data[i] / 16);
		hex_num[1] = int_to_hex (f->data[i] % 16);
		xmlBufferWriteChar (buff, hex_num);
	}

	hex_data = xmlStrdup (xmlBufferContent (buff));
	xmlBufferFree (buff);

	return hex_data;
}

static int
generate_hex_attribute (xmlTextWriterPtr writer, cob_ml_attr *attr, unsigned int *count)
{
	xmlChar	*hex_name;
	xmlChar	*value;

	hex_name = get_name_with_hex_prefix (attr->name);
	value = get_hex_xml_data (attr->value);
	IF_NEG_RETURN_ELSE_COUNT (xmlTextWriterWriteAttribute (writer, hex_name, value));
	xmlFree (hex_name);
	xmlFree (value);

	return 0;
}

static int
generate_normal_attribute (xmlTextWriterPtr writer, cob_ml_attr *attr, unsigned int *count)
{
	xmlChar	*name;
	xmlChar	*value;

	name = get_xml_name (attr->name);
	value = get_trimmed_xml_data (attr->value);
	IF_NEG_RETURN_ELSE_COUNT (xmlTextWriterWriteAttribute (writer, name, value));
	xmlFree (name);
	xmlFree (value);

	return 0;
}

static int
generate_attributes (xmlTextWriterPtr writer, cob_ml_attr *attr, unsigned int *count)
{
	int	status;

	for (; attr; attr = attr->sibling) {
		if (attr->is_suppressed) {
			continue;
		}

		if (has_invalid_xml_char (attr->value)) {
			set_xml_code (XML_INVALID_CHAR_REPLACED);
			status = generate_hex_attribute (writer, attr, count);
		} else {
			status = generate_normal_attribute (writer, attr, count);
		}

		if (status < 0) {
			return status;
		}
	}

	return 0;
}

static int
generate_hex_element (xmlTextWriterPtr writer, cob_ml_tree *tree,
		      xmlChar *x_ns, xmlChar *x_ns_prefix, unsigned int *count)
{
	xmlChar		*hex_name;
	int		status;
	xmlChar		*hex_value;

	hex_name = get_name_with_hex_prefix (tree->name);
	IF_NEG_RETURN_ELSE_COUNT (xmlTextWriterStartElementNS (writer, x_ns_prefix,
							       hex_name, x_ns));
	xmlFree (hex_name);

        status = generate_attributes (writer, tree->attrs, count);
	if (status < 0) {
		return status;
	}

	hex_value = get_hex_xml_data (tree->content);
	IF_NEG_RETURN_ELSE_COUNT (xmlTextWriterWriteString (writer, hex_value));
	xmlFree (hex_value);

	IF_NEG_RETURN_ELSE_COUNT (xmlTextWriterEndElement (writer));

	return 0;
}


static xmlChar *
get_xml_num (cob_field * const f)
{
	return get_num (f, &xmlCharStrndup_void);
}

static int
generate_content (xmlTextWriterPtr writer, cob_ml_tree *tree, unsigned int *count)
{
	cob_field	*content = tree->content;
	xmlChar		*x_content;

	if (COB_FIELD_IS_FP (content)) {
		/* TO-DO: Implement! */
		/* TO-DO: Stop compilation if float in field */
		cob_set_exception (COB_EC_IMP_FEATURE_MISSING);
		cob_fatal_error (COB_FERROR_XML);
	} else if (COB_FIELD_IS_NUMERIC (content)) {
		x_content = get_xml_num (content);
	} else {
		x_content = get_trimmed_xml_data (content);
	}

	IF_NEG_RETURN_ELSE_COUNT (xmlTextWriterWriteString (writer, x_content));
	xmlFree (x_content);

	return 0;
}


static int
generate_normal_element (xmlTextWriterPtr writer, cob_ml_tree *tree,
			 xmlChar *x_ns, xmlChar *x_ns_prefix, unsigned int *count)
{
	int		status;
	xmlChar		*x_name;
	cob_ml_tree	*child;

	/* Start element */
	x_name = get_xml_name (tree->name);
	IF_NEG_RETURN_ELSE_COUNT (xmlTextWriterStartElementNS (writer, x_ns_prefix,
							       x_name, x_ns));
	xmlFree (x_name);

        status = generate_attributes (writer, tree->attrs, count);
	if (status < 0) {
		return status;
	}

	/* Output child elements or content. */
	if (tree->children) {
		for (child = tree->children; child; child = child->sibling) {
			/*
			  Note we only have a namespace attribute on the
			  outermost element.
			*/
			status = generate_xml_from_tree (writer, child, NULL,
							 x_ns_prefix, count);
			if (status < 0) {
				return status;
			}
		}
	} else if (tree->content) {
		status = generate_content (writer, tree, count);
		if (status < 0) {
			return status;
		}
	}

	/* Complete element */
	IF_NEG_RETURN_ELSE_COUNT (xmlTextWriterEndElement (writer));

	return 0;
}

static int
generate_element (xmlTextWriterPtr writer, cob_ml_tree *tree,
		  xmlChar *x_ns, xmlChar *x_ns_prefix, unsigned int *count)
{
	/* Check for invalid characters. */
	if (tree->content
	    && !COB_FIELD_IS_NUMERIC (tree->content)
	    && has_invalid_xml_char (tree->content)) {
		set_xml_code (XML_INVALID_CHAR_REPLACED);
		return generate_hex_element (writer, tree, x_ns, x_ns_prefix,
					     count);
	} else {
		return generate_normal_element (writer, tree, x_ns,
						x_ns_prefix, count);
	}
}

static int
generate_xml_from_tree (xmlTextWriterPtr writer, cob_ml_tree *tree,
			xmlChar *ns, xmlChar *ns_prefix, unsigned int *count)
{
	if (tree->is_suppressed) {
		return 0;
	}

	if (tree->name) {
		return generate_element (writer, tree, ns, ns_prefix, count);
	} else {
		return generate_content (writer, tree, count);
	}
}

#undef IF_NEG_RETURN_ELSE_COUNT

static void
set_xml_exception (const unsigned int code)
{
	cob_set_exception (COB_EC_XML_IMP);
	set_xml_code (code);
}

#endif

#if WITH_CJSON

static void
set_json_code (const unsigned int code)
{
	/* if the COBOL module never checks the code it isn't generated,
	   this also makes clear that we don't need to (and can't) set it */
	if (!COB_MODULE_PTR->json_code) {
		return;
	}
	cob_set_field_to_uint (COB_MODULE_PTR->json_code, code);
}

static void
set_json_exception (const unsigned int code)
{
	cob_set_exception (COB_EC_JSON_IMP);
	set_json_code (code);
}

static void *
json_strndup (const char *str, const size_t size)
{
	char	*dup = cob_malloc (size + 1);
	memcpy (dup, str, size);
	return dup;
}

static char *
get_trimmed_json_data (const cob_field * const f)
{
	return (char *) get_trimmed_data (f, &json_strndup);
}

static char *
get_json_num (cob_field * const f)
{
	return (char *) get_num (f, &json_strndup);
}

static int
generate_json_from_tree (cob_ml_tree *tree, cJSON *out)
{
	cob_ml_tree	*child;
	cJSON		*children_json = NULL;
	char		*name = NULL;
	char		*content = NULL;
	int		status = 0;

	if (tree->is_suppressed) {
		return 0;
	}

	name = get_trimmed_json_data (tree->name);
	if (tree->children) {
		children_json = cJSON_CreateObject ();
		for (child = tree->children; child; child = child->sibling) {
			status = generate_json_from_tree (child, children_json);
			if (status < 0) {
				cJSON_Delete (children_json);
				goto end;
			}
		}
		cJSON_AddItemToObject (out, name, children_json);
	} else if (tree->content) {
		if (COB_FIELD_IS_FP (tree->content)) {
			/* TO-DO: Implement! */
			/* TO-DO: Stop compilation if float in field */
			cob_set_exception (COB_EC_IMP_FEATURE_MISSING);
			cob_fatal_error (COB_FERROR_JSON);
		} else if (COB_FIELD_IS_NUMERIC (tree->content)) {
			content = get_json_num (tree->content);
			/*
			  We use AddRaw instead of AddNumber because a PIC 9(32)
			  may not be representable using the double AddNumber
			  uses internally.
			*/
			if (!cJSON_AddRawToObject (out, name, content)) {
				status = -1;
				goto end;
			}
		} else {
			content = (char *) get_trimmed_json_data (tree->content);
			if (!cJSON_AddStringToObject (out, name, content)) {
				status = -1;
				goto end;
			}
		}
	}

 end:
	if (content) {
		cob_free (content);
	}
	if (name) {
		cob_free (name);
	}
	return status;
}

#endif

/* Global functions */

int
cob_is_xml_namestartchar (const int c)
{
	/*
	  From XML 1.0 spec (https://www.w3.org/TR/xml/):
	  [4] NameStartChar ::= ":" | [A-Z] | "_" | [a-z] | [#xC0-#xD6]
                                    | [#xD8-#xF6] | [#xF8-#x2FF]
			            | [#x370-#x37D] | [#x37F-#x1FFF]
				    | [#x200C-#x200D] | [#x2070-#x218F]
				    | [#x2C00-#x2FEF] | [#x3001-#xD7FF]
				    | [#xF900-#xFDCF] | [#xFDF0-#xFFFD]
				    | [#x10000-#xEFFFF]
          [4a] NameChar ::= NameStartChar | "-" | "." | [0-9] | #xB7
                                          | [#x0300-#x036F] | [#x203F-#x2040]
	*/
	/* TO-DO: Deal with 2/3/4-byte chars. */
	return isalpha(c) || c == '_'
		|| (c >= 0xc0 && c <= 0xd6)
		|| (c >= 0xd8 && c <= 0xf6)
		|| (c >= 0xf8);
}

int
cob_is_xml_namechar (const int c)
{
	/* TO-DO: Deal with 2/3/4-byte chars. */
	return cob_is_xml_namestartchar (c) || c == '-' || c == '.' || isdigit (c)
		|| c == 0xb7;
}

/*
   check if string is a valid URI
   URI = scheme:[//authority]path[?query][#fragment]
*/
int
cob_is_valid_uri (const char *str)
{
#if WITH_XML2
	int		is_valid;
	xmlURIPtr	p;

	p = xmlParseURI (str);
	is_valid = !!p;
	if (p) {
		xmlFreeURI (p);
	}

	return is_valid;
#else
	/* scheme must start with lower-strase */
	if (!str || *str <= 'a' || *str >= 'z') return 0;

	/* scheme completes with ":" */
	str++;
	while (*str && *str != ':') str++;

	/* check for "any scheme" with any path */
	if (*str == ':' && str[1]) return 1;

	return 0;
#endif
}

#if WITH_XML2

void
cob_xml_generate (cob_field *out, cob_ml_tree *tree, cob_field *count,
		  const int with_xml_dec, cob_field *ns, cob_field *ns_prefix)
{
	xmlBufferPtr		buff;
	xmlTextWriterPtr	writer = NULL;
	int			status;
	unsigned int		chars_written = 0;
	xmlChar			*x_ns = NULL;
	xmlChar			*x_ns_prefix = NULL;
	int			buff_len;
	int			copy_len;
	int			num_newlines = 0;

	set_xml_code (0);

	buff = xmlBufferCreate ();
	if (buff == NULL) {
		set_xml_exception (XML_INTERNAL_ERROR);
		goto end;
	}

	writer = xmlNewTextWriterMemory (buff, 0);
	if (writer == NULL) {
		goto end;
	}

	if (with_xml_dec) {
		/* TO-DO: Support encoding */
		status = xmlTextWriterStartDocument (writer, NULL, NULL, NULL);
		if (status < 0) {
			set_xml_exception (XML_INTERNAL_ERROR);
			goto end;
		} else {
			chars_written += status;
		}
	}

	if (ns) {
		if (is_all_spaces (ns)) {
			x_ns = NULL;
		} else if (has_invalid_xml_char (ns)) {
			set_xml_exception (XML_INVALID_NAMESPACE);
			goto end;
		} else {
		        x_ns = get_trimmed_xml_data (ns);
			if (!cob_is_valid_uri ((const char *) x_ns)) {
				set_xml_exception (XML_INVALID_NAMESPACE);
				goto end;
			}
		}
	}

	if (ns_prefix) {
		if (is_all_spaces (ns_prefix)) {
			x_ns_prefix = NULL;
		} else if (!is_valid_xml_name (ns_prefix)) {
			set_xml_exception (XML_INVALID_NAMESPACE_PREFIX);
			goto end;
		} else {
			x_ns_prefix = get_trimmed_xml_data (ns_prefix);
		}
	}

        status = generate_xml_from_tree (writer, tree, x_ns, x_ns_prefix,
				     &chars_written);
	if (status < 0) {
		set_xml_exception (XML_INTERNAL_ERROR);
		goto end;
	}

	status = xmlTextWriterEndDocument (writer);
	if (status < 0) {
		set_xml_exception (XML_INTERNAL_ERROR);
		goto end;
	} else {
		chars_written += status;
	}

	/* Copy generated tree to output field */
	buff_len = xmlBufferLength (buff);
	copy_len = cob_min_int (buff_len, (int) out->size);
	memcpy (out->data, xmlBufferContent (buff), copy_len);
	memset (out->data + copy_len, ' ', out->size - copy_len);
	/* Remove trailing newlines */
	for (; copy_len > 0 && out->data[copy_len - 1] == '\n'; --copy_len) {
		out->data[copy_len - 1] = ' ';
		--chars_written;
		++num_newlines;
	}
	/* Raise exception if output field is too small */
	if (buff_len - num_newlines > copy_len) {
		set_xml_exception (XML_OUT_FIELD_TOO_SMALL);
		goto end;
	}

 end:
	if (x_ns) {
		xmlFree (x_ns);
	}
	if (x_ns_prefix) {
		xmlFree (x_ns_prefix);
	}
	if (writer) {
		xmlFreeTextWriter (writer);
	}
	if (buff) {
		xmlBufferFree (buff);
	}
	if (count) {
		cob_add_int (count, chars_written, 0);
	}
}

#else /* !WITH_XML2 */

void
cob_xml_generate (cob_field *out, cob_ml_tree *tree, cob_field *count,
		  const int with_xml_dec, cob_field *ns, cob_field *ns_prefix)
{
	COB_UNUSED (out);
	COB_UNUSED (tree);
	COB_UNUSED (count);
	COB_UNUSED (with_xml_dec);
	COB_UNUSED (ns);
	COB_UNUSED (ns_prefix);
}

#endif

#if WITH_CJSON

void
cob_json_generate (cob_field *out, cob_ml_tree *tree, cob_field *count)
{
	cJSON	*json;
	int	status = 0;
	char	*printed_json;
	unsigned int	print_len = 0;
	unsigned int	copy_len;
	int	num_newlines = 0;

	set_json_code (0);

	json = cJSON_CreateObject ();
	if (!json) {
		set_json_exception (JSON_INTERNAL_ERROR);
		goto end;
	}

	status = generate_json_from_tree (tree, json);
	if (status < 0) {
		set_json_exception (JSON_INTERNAL_ERROR);
		goto end;
	}

	/* TO-DO: Set cJSON to use cob_free in InitHook? */
	printed_json = cJSON_PrintUnformatted (json);
	if (!printed_json) {
		set_json_exception (JSON_INTERNAL_ERROR);
		goto end;
	}

	/* TO-DO: Duplication! */
	print_len = strlen (printed_json);
	copy_len = cob_min_int (print_len, (int) out->size);
	memcpy (out->data, printed_json, copy_len);
	memset (out->data + copy_len, ' ', out->size - copy_len);
	/* Remove trailing newlines */
	for (; copy_len > 0 && out->data[copy_len - 1] == '\n'; --copy_len) {
		out->data[copy_len - 1] = ' ';
		--print_len;
		++num_newlines;
	}
	/* Raise exception if output field is too small */
	if (print_len - num_newlines > copy_len) {
		set_json_exception (JSON_OUT_FIELD_TOO_SMALL);
		goto end;
	}

 end:
	if (json) {
		cJSON_Delete (json);
	}
	if (count && print_len) {
		cob_add_int (count, print_len, 0);
	}
}

#else /* !WITH_CJSON */

void
cob_json_generate (cob_field *out, cob_ml_tree *tree, cob_field *count)
{
	COB_UNUSED (out);
	COB_UNUSED (tree);
	COB_UNUSED (count);
}

#endif

void
cob_init_mlio (cob_global * const g)
{
#if WITH_XML2
	LIBXML_TEST_VERSION
#endif
	cobglobptr = g;
}

void
cob_exit_mlio (void)
{
#if WITH_XML2
	xmlCleanupParser ();
#endif
}
