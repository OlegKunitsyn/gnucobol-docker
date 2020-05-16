/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015 Free Software Foundation, Inc.

   This program is free software: you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation, either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

/* As a special exception, you may create a larger work that contains
   part or all of the Bison parser skeleton and distribute that work
   under terms of your choice, so long as that work isn't itself a
   parser generator using the skeleton or a modified version thereof
   as a parser skeleton.  Alternatively, if you modify or redistribute
   the parser skeleton itself, you may (at your option) remove this
   special exception, which will cause the skeleton and the resulting
   Bison output files to be licensed under the GNU General Public
   License without this special exception.

   This special exception was added by the Free Software Foundation in
   version 2.2 of Bison.  */

/* C LALR(1) parser skeleton written by Richard Stallman, by
   simplifying the original so-called "semantic" parser.  */

/* All symbols defined below should begin with yy or YY, to avoid
   infringing on user name space.  This should be done even for local
   variables, as they might otherwise be expanded by user macros.
   There are some unavoidable exceptions within include files to
   define necessary library symbols; they are noted "INFRINGES ON
   USER NAME SPACE" below.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.0.4"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 28 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:339  */

#include "config.h"

#include <stdlib.h>
#include <string.h>

#define	COB_IN_PARSER	1
#include "cobc.h"
#include "tree.h"

#ifndef	_STDLIB_H
#define	_STDLIB_H 1
#endif

#define YYSTYPE			cb_tree
#define yyerror(x)		cb_error_always ("%s", x)

#define emit_statement(x) \
do { \
  if (!skip_statements) { \
	CB_ADD_TO_CHAIN (x, current_program->exec_list); \
  } \
}  ONCE_COB

#define push_expr(type, node) \
  current_expr = cb_build_list (cb_int (type), node, current_expr)

/* Statement terminator definitions */
#define TERM_NONE		0
#define TERM_ACCEPT		1U
#define TERM_ADD		2U
#define TERM_CALL		3U
#define TERM_COMPUTE		4U
#define TERM_DELETE		5U
#define TERM_DISPLAY		6U
#define TERM_DIVIDE		7U
#define TERM_EVALUATE		8U
#define TERM_IF			9U
#define TERM_MODIFY		10U
#define TERM_MULTIPLY		11U
#define TERM_PERFORM		12U
#define TERM_READ		13U
#define TERM_RECEIVE		14U
#define TERM_RETURN		15U
#define TERM_REWRITE		16U
#define TERM_SEARCH		17U
#define TERM_START		18U
#define TERM_STRING		19U
#define TERM_SUBTRACT		20U
#define TERM_UNSTRING		21U
#define TERM_WRITE		22U
#define TERM_MAX		23U	/* Always last entry, used for array size */

#define	TERMINATOR_WARNING(x,z)	terminator_warning (x, TERM_##z, #z)
#define	TERMINATOR_ERROR(x,z)	terminator_error (x, TERM_##z, #z)
#define	TERMINATOR_CLEAR(x,z)	terminator_clear (x, TERM_##z)

/* Defines for duplicate checks */
/* Note - We use <= 16 for common item definitions and */
/* > 16 for non-common item definitions e.g. REPORT and SCREEN */
#define	SYN_CLAUSE_1		(1U << 0)
#define	SYN_CLAUSE_2		(1U << 1)
#define	SYN_CLAUSE_3		(1U << 2)
#define	SYN_CLAUSE_4		(1U << 3)
#define	SYN_CLAUSE_5		(1U << 4)
#define	SYN_CLAUSE_6		(1U << 5)
#define	SYN_CLAUSE_7		(1U << 6)
#define	SYN_CLAUSE_8		(1U << 7)
#define	SYN_CLAUSE_9		(1U << 8)
#define	SYN_CLAUSE_10		(1U << 9)
#define	SYN_CLAUSE_11		(1U << 10)
#define	SYN_CLAUSE_12		(1U << 11)
#define	SYN_CLAUSE_13		(1U << 12)
#define	SYN_CLAUSE_14		(1U << 13)
#define	SYN_CLAUSE_15		(1U << 14)
#define	SYN_CLAUSE_16		(1U << 15)
#define	SYN_CLAUSE_17		(1U << 16)
#define	SYN_CLAUSE_18		(1U << 17)
#define	SYN_CLAUSE_19		(1U << 18)
#define	SYN_CLAUSE_20		(1U << 19)
#define	SYN_CLAUSE_21		(1U << 20)
#define	SYN_CLAUSE_22		(1U << 21)
#define	SYN_CLAUSE_23		(1U << 22)
#define	SYN_CLAUSE_24		(1U << 23)
#define	SYN_CLAUSE_25		(1U << 24)
#define	SYN_CLAUSE_26		(1U << 25)
#define	SYN_CLAUSE_27		(1U << 26)
#define	SYN_CLAUSE_28		(1U << 27)
#define	SYN_CLAUSE_29		(1U << 28)
#define	SYN_CLAUSE_30		(1U << 29)
#define	SYN_CLAUSE_31		(1U << 30)
#define	SYN_CLAUSE_32		(1U << 31)

#define	EVAL_DEPTH		32
#define	PROG_DEPTH		16

/* Global variables */

struct cb_program		*current_program = NULL;
struct cb_statement		*current_statement = NULL;
struct cb_label			*current_section = NULL;
struct cb_label			*current_paragraph = NULL;
cb_tree				defined_prog_list = NULL;
int				cb_exp_line = 0;

cb_tree				cobc_printer_node = NULL;
int				functions_are_all = 0;
int				non_const_word = 0;
int				suppress_data_exceptions = 0;
unsigned int			cobc_repeat_last_token = 0;
unsigned int			cobc_in_id = 0;
unsigned int			cobc_in_procedure = 0;
unsigned int			cobc_in_repository = 0;
unsigned int			cobc_force_literal = 0;
unsigned int			cobc_cs_check = 0;
unsigned int			cobc_allow_program_name = 0;

/* Local variables */

enum tallying_phrase {
	NO_PHRASE,
	FOR_PHRASE,
	CHARACTERS_PHRASE,
	ALL_LEADING_TRAILING_PHRASES,
	VALUE_REGION_PHRASE
};

enum key_clause_type {
	NO_KEY,
	RECORD_KEY,
	RELATIVE_KEY
};

static struct cb_statement	*main_statement;

static cb_tree			current_expr;
static struct cb_field		*current_field;
static struct cb_field		*control_field;
static struct cb_field		*description_field;
static struct cb_file		*current_file;
static struct cb_cd		*current_cd;
static struct cb_report		*current_report;
static struct cb_report		*report_instance;
static struct cb_key_component	*key_component_list;

static struct cb_file		*linage_file;
static cb_tree			next_label_list;

static const char			*stack_progid[PROG_DEPTH];

static enum cb_storage		current_storage;

static cb_tree			perform_stack;
static cb_tree			qualifier;
static cb_tree			keys_list;

static cb_tree			save_tree;
static cb_tree			start_tree;

static unsigned int		check_unreached;
static unsigned int		in_declaratives;
static unsigned int		in_debugging;
static unsigned int		current_linage;
static unsigned int		report_count;
static unsigned int		first_prog;
static unsigned int		setup_from_identification;
static unsigned int		use_global_ind;
static unsigned int		same_area;
static unsigned int		inspect_keyword;
static unsigned int		main_flag_set;
static int			next_label_id;
static int			eval_level;
static int			eval_inc;
static int			eval_inc2;
static int			depth;
static int			first_nested_program;
static int			call_mode;
static int			size_mode;
static cob_flags_t		set_attr_val_on;
static cob_flags_t		set_attr_val_off;
static cob_flags_t		check_duplicate;
static cob_flags_t		check_on_off_duplicate;
static cob_flags_t		check_pic_duplicate;
static cob_flags_t		check_line_col_duplicate;
static unsigned int		skip_statements;
static unsigned int		start_debug;
static unsigned int		save_debug;
static unsigned int		needs_field_debug;
static unsigned int		needs_debug_item;
static unsigned int		env_div_seen;
static cob_flags_t		header_check;
static unsigned int		call_nothing;
static enum tallying_phrase	previous_tallying_phrase;
static cb_tree			default_rounded_mode;
static enum key_clause_type	key_type;

static enum cb_display_type	display_type;
static int			is_first_display_item;
static cb_tree			advancing_value;
static cb_tree			upon_value;
static cb_tree			line_column;

static int			term_array[TERM_MAX];
static cb_tree			eval_check[EVAL_DEPTH][EVAL_DEPTH];

static const char		*backup_source_file = NULL;
static int			backup_source_line = 0;

/* Defines for header presence */

#define	COBC_HD_ENVIRONMENT_DIVISION	(1U << 0)
#define	COBC_HD_CONFIGURATION_SECTION	(1U << 1)
#define	COBC_HD_SPECIAL_NAMES		(1U << 2)
#define	COBC_HD_INPUT_OUTPUT_SECTION	(1U << 3)
#define	COBC_HD_FILE_CONTROL		(1U << 4)
#define	COBC_HD_I_O_CONTROL		(1U << 5)
#define	COBC_HD_DATA_DIVISION		(1U << 6)
#define	COBC_HD_FILE_SECTION		(1U << 7)
#define	COBC_HD_WORKING_STORAGE_SECTION	(1U << 8)
#define	COBC_HD_LOCAL_STORAGE_SECTION	(1U << 9)
#define	COBC_HD_LINKAGE_SECTION		(1U << 10)
#define	COBC_HD_COMMUNICATION_SECTION	(1U << 11)
#define	COBC_HD_REPORT_SECTION		(1U << 12)
#define	COBC_HD_SCREEN_SECTION		(1U << 13)
#define	COBC_HD_PROCEDURE_DIVISION	(1U << 14)
#define	COBC_HD_PROGRAM_ID		(1U << 15)
#define	COBC_HD_SOURCE_COMPUTER		(1U << 16)
#define	COBC_HD_OBJECT_COMPUTER		(1U << 17)
#define	COBC_HD_REPOSITORY		(1U << 18)

/* Static functions */

static void
begin_statement (const char *name, const unsigned int term)
{
	if (check_unreached) {
		cb_warning (cb_warn_unreachable, _("unreachable statement '%s'"), name);
	}
	current_paragraph->flag_statement = 1;
	current_statement = cb_build_statement (name);
	CB_TREE (current_statement)->source_file = cb_source_file;
	CB_TREE (current_statement)->source_line = cb_source_line;
	current_statement->flag_in_debug = in_debugging;
	emit_statement (CB_TREE (current_statement));
	if (term) {
		term_array[term]++;
	}
	main_statement = current_statement;
}

/* create a new statement with base attributes of current_statement
   and set this as new current_statement */
static void
begin_implicit_statement (void)
{
	struct cb_statement	*new_statement;
	new_statement = cb_build_statement (NULL);
	new_statement->common = current_statement->common;
	new_statement->name = current_statement->name;
	new_statement->flag_in_debug = !!in_debugging;
	new_statement->flag_implicit = 1;
	current_statement = new_statement;
	main_statement->body = cb_list_add (main_statement->body,
					    CB_TREE (current_statement));
}

# if 0 /* activate only for debugging purposes for attribs
	FIXME: Replace by DEBUG_LOG function */
static
void print_bits (cob_flags_t num)
{
	unsigned int 	size = sizeof (cob_flags_t);
	unsigned int	max_pow = 1 << (size * 8 - 1);
	int 		i = 0;

	for(; i < size * 8; ++i){
		/* Print last bit and shift left. */
		fprintf (stderr, "%u ", num & max_pow ? 1 : 0);
		num = num << 1;
	}
	fprintf (stderr, "\n");
}
#endif

/* functions for storing current position and
   assigning it to a cb_tree after its parsing is finished */
static COB_INLINE
void backup_current_pos (void)
{
	backup_source_file = cb_source_file;
	backup_source_line = cb_source_line;
}

#if 0 /* currently not used */
static COB_INLINE
void set_pos_from_backup (cb_tree x)
{
	x->source_file = backup_source_file;
	x->source_line = backup_source_line;
}
#endif

static void
emit_entry (const char *name, const int encode, cb_tree using_list, cb_tree convention)
{
	cb_tree		l;
	cb_tree		label;
	cb_tree		x;
	cb_tree		entry_conv;
	struct cb_field	*f, *ret_f;
	int			param_num;
	char		buff[COB_MINI_BUFF];

	snprintf (buff, (size_t)COB_MINI_MAX, "E$%s", name);
	label = cb_build_label (cb_build_reference (buff), NULL);
	if (encode) {
		CB_LABEL (label)->name = cb_encode_program_id (name);
		CB_LABEL (label)->orig_name = name;
	} else {
		CB_LABEL (label)->name = name;
		CB_LABEL (label)->orig_name = current_program->orig_program_id;
	}
	CB_LABEL (label)->flag_begin = 1;
	CB_LABEL (label)->flag_entry = 1;
	label->source_line = backup_source_line;
	emit_statement (label);

	if (current_program->flag_debugging) {
		emit_statement (cb_build_debug (cb_debug_contents,
						"START PROGRAM", NULL));
	}

	param_num = 1;
	for (l = using_list; l; l = CB_CHAIN (l)) {
		x = CB_VALUE (l);
		if (CB_VALID_TREE (x) && cb_ref (x) != cb_error_node) {
			f = CB_FIELD (cb_ref (x));
			if (!current_program->flag_chained) {
				if (f->storage != CB_STORAGE_LINKAGE) {
					cb_error_x (x, _("'%s' is not in LINKAGE SECTION"), f->name);
				}
				if (f->flag_item_based || f->flag_external) {
					cb_error_x (x, _("'%s' cannot be BASED/EXTERNAL"), f->name);
				}
				f->flag_is_pdiv_parm = 1;
			} else {
				if (f->storage != CB_STORAGE_WORKING) {
					cb_error_x (x, _("'%s' is not in WORKING-STORAGE SECTION"), f->name);
				}
				f->flag_chained = 1;
				f->param_num = param_num;
				param_num++;
			}
			if (f->level != 01 && f->level != 77) {
				cb_error_x (x, _("'%s' not level 01 or 77"), f->name);
			}
			if (f->redefines) {
				cb_error_x (x, _ ("'%s' REDEFINES field not allowed here"), f->name);
			}
			/* add a "receiving" entry for the USING parameter */
			if (cb_listing_xref) {
				cobc_xref_link (&f->xref, CB_REFERENCE (x)->common.source_line, 1);
			}
		}
	}


	if (current_program->returning &&
		cb_ref (current_program->returning) != cb_error_node) {
		ret_f = CB_FIELD (cb_ref (current_program->returning));
		if (ret_f->redefines) {
			cb_error_x (current_program->returning,
				_("'%s' REDEFINES field not allowed here"), ret_f->name);
		}
	} else {
		ret_f = NULL;
	}

	/* Check returning item against using items when FUNCTION */
	if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION && ret_f) {
		for (l = using_list; l; l = CB_CHAIN (l)) {
			x = CB_VALUE (l);
			if (CB_VALID_TREE (x) && cb_ref (x) != cb_error_node) {
				f = CB_FIELD (cb_ref (x));
				if (ret_f == f) {
					cb_error_x (x, _("'%s' USING item duplicates RETURNING item"), f->name);
				}
			}
		}
	}

	for (l = current_program->entry_list; l; l = CB_CHAIN (l)) {
		if (strcmp ((const char *)name,
			    (const char *)(CB_LABEL(CB_PURPOSE(l))->name)) == 0) {
			cb_error_x (CB_TREE (current_statement),
				    _("ENTRY '%s' duplicated"), name);
		}
	}

	if (convention) {
		entry_conv = convention;
	} else {
		entry_conv = current_program->entry_convention;
	}

	current_program->entry_list =
		cb_list_append (current_program->entry_list,
				CB_BUILD_PAIR (label, CB_BUILD_PAIR(entry_conv, using_list)));
}

static size_t
increment_depth (void)
{
	if (++depth >= PROG_DEPTH) {
		cb_error (_("maximum nested program depth exceeded (%d)"),
			  PROG_DEPTH);
		return 1;
	}
	return 0;
}

static void
terminator_warning (cb_tree stmt, const unsigned int termid,
		    const char *name)
{
	char		terminator[32];

	check_unreached = 0;
	if (term_array[termid]) {
		term_array[termid]--;
	/* LCOV_EXCL_START */
	} else {
		cobc_err_msg ("call to '%s' without any open term for %s",
			"terminator_warning", name);
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */
	snprintf (terminator, 32, "END-%s", name);
	if (is_reserved_word (terminator)) {
		cb_warning_x (cb_warn_terminator, CB_TREE (current_statement),
			_("%s statement not terminated by %s"), name, terminator);
	}

	/* Free tree associated with terminator */
	if (stmt) {
		cobc_parse_free (stmt);
	}
}

static void
terminator_error (cb_tree stmt, const unsigned int termid, const char *name)
{
	char		terminator[32];

	check_unreached = 0;
	if (term_array[termid]) {
		term_array[termid]--;
	/* LCOV_EXCL_START */
	} else {
		cobc_err_msg ("call to '%s' without any open term for %s",
			"terminator_error", name);
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */
	snprintf (terminator, 32, "END-%s", name);
	if (is_reserved_word (terminator)) {
		cb_error_x (CB_TREE (current_statement),
			_("%s statement not terminated by %s"), name, terminator);
	} else {
		cb_error_x (CB_TREE (current_statement),
			_("%s statement not terminated"), name);
	}

	/* Free tree associated with terminator */
	if (stmt) {
		cobc_parse_free (stmt);
	}
}

static void
terminator_clear (cb_tree stmt, const unsigned int termid)
{
	struct cb_perform	*p;
	check_unreached = 0;
	if (term_array[termid]) {
		term_array[termid]--;
	/* LCOV_EXCL_START */
	} else {
		cobc_err_msg ("call to '%s' without any open term for %s",
			"terminator_warning", current_statement->name);
		COBC_ABORT ();
	}
	/* LCOV_EXCL_STOP */
	if (termid == TERM_PERFORM
	 && perform_stack) {
		p = CB_PERFORM (CB_VALUE (perform_stack));
		if (p->perform_type == CB_PERFORM_UNTIL) {
			cb_terminate_cond ();
		}
	}
	/* Free tree associated with terminator */
	if (stmt) {
		cobc_parse_free (stmt);
	}
}

static int
literal_value (cb_tree x)
{
	if (x == cb_space) {
		return ' ';
	} else if (x == cb_zero) {
		return '0';
	} else if (x == cb_quote) {
		return cb_flag_apostrophe ? '\'' : '"';
	} else if (x == cb_null) {
		return 0;
	} else if (x == cb_low) {
		return 0;
	} else if (x == cb_high) {
		return 255;
	} else if (CB_TREE_CLASS (x) == CB_CLASS_NUMERIC) {
		return cb_get_int (x);
	} else {
		return CB_LITERAL (x)->data[0];
	}
}

static void
setup_use_file (struct cb_file *fileptr)
{
	struct cb_file	*newptr;

	if (fileptr->organization == COB_ORG_SORT) {
		cb_error (_("USE statement invalid for SORT file"));
	}
	if (fileptr->flag_global) {
		newptr = cobc_parse_malloc (sizeof(struct cb_file));
		*newptr = *fileptr;
		newptr->handler = current_section;
		newptr->handler_prog = current_program;
		if (!use_global_ind) {
			current_program->local_file_list =
				cb_list_add (current_program->local_file_list,
					     CB_TREE (newptr));
		} else {
			current_program->global_file_list =
				cb_list_add (current_program->global_file_list,
					     CB_TREE (newptr));
		}
	} else {
		fileptr->handler = current_section;
	}
}

static void
emit_duplicate_clause_message (const char *clause)
{
	/* FIXME: replace by a new warning level that is set
	   to warn/error depending on cb_relaxed_syntax_checks */
	if (cb_relaxed_syntax_checks) {
		cb_warning (COBC_WARN_FILLER, _("duplicate %s clause"), clause);
	} else {
		cb_error (_("duplicate %s clause"), clause);
	}
}

static void
check_repeated (const char *clause, const cob_flags_t bitval, cob_flags_t *already_seen)
{
	if (*already_seen & bitval) {
		emit_duplicate_clause_message (clause);
	} else {
		*already_seen |= bitval;
	}
}

static void
error_if_no_page_lines_limit (const char *phrase)
{
	if (!current_report->lines && !current_report->t_lines) {
		cb_error (_("Cannot specify %s without number of lines on page"),
			  phrase);
	}
}

static void
setup_occurs (void)
{
	check_repeated ("OCCURS", SYN_CLAUSE_7, &check_pic_duplicate);
	if (current_field->indexes == COB_MAX_SUBSCRIPTS) {
		cb_error (_ ("maximum OCCURS depth exceeded (%d)"),
			COB_MAX_SUBSCRIPTS);
	} else {
		current_field->indexes++;
	}

	if (current_field->flag_unbounded) {
		if (current_field->storage != CB_STORAGE_LINKAGE) {
			cb_error_x (CB_TREE(current_field), _("'%s' is not in LINKAGE SECTION"),
				cb_name (CB_TREE(current_field)));
		}
	}

	if (current_field->flag_item_based) {
		cb_error (_ ("%s and %s are mutually exclusive"), "BASED", "OCCURS");
	} else if (current_field->flag_external) {
		cb_error (_ ("%s and %s are mutually exclusive"), "EXTERNAL", "OCCURS");
	}
	current_field->flag_occurs = 1;
}

static void
setup_occurs_min_max (cb_tree occurs_min, cb_tree occurs_max)
{
	if (occurs_max) {
		current_field->occurs_min = cb_get_int (occurs_min);
		if (occurs_max != cb_int0) {
			current_field->occurs_max = cb_get_int (occurs_max);
			if (!current_field->depending) {
				if (cb_relaxed_syntax_checks) {
					cb_warning (COBC_WARN_FILLER, _ ("TO phrase without DEPENDING phrase"));
					cb_warning (COBC_WARN_FILLER, _ ("maximum number of occurrences assumed to be exact number"));
					current_field->occurs_min = 1; /* CHECKME: why using 1 ? */
				} else {
					cb_error (_ ("TO phrase without DEPENDING phrase"));
				}
			}
			if (current_field->occurs_max <= current_field->occurs_min) {
				cb_error (_ ("OCCURS TO must be greater than OCCURS FROM"));
			}
		} else {
			current_field->occurs_max = 0;
		}
	} else {
		current_field->occurs_min = 1; /* CHECKME: why using 1 ? */
		current_field->occurs_max = cb_get_int (occurs_min);
		if (current_field->depending) {
			cb_verify (cb_odo_without_to, _ ("OCCURS DEPENDING ON without TO phrase"));
		}
	}
}

static void
check_relaxed_syntax (const cob_flags_t lev)
{
	const char	*s;

	switch (lev) {
	case COBC_HD_ENVIRONMENT_DIVISION:
		s = "ENVIRONMENT DIVISION";
		break;
	case COBC_HD_CONFIGURATION_SECTION:
		s = "CONFIGURATION SECTION";
		break;
	case COBC_HD_SPECIAL_NAMES:
		s = "SPECIAL-NAMES";
		break;
	case COBC_HD_INPUT_OUTPUT_SECTION:
		s = "INPUT-OUTPUT SECTION";
		break;
	case COBC_HD_FILE_CONTROL:
		s = "FILE-CONTROL";
		break;
	case COBC_HD_I_O_CONTROL:
		s = "I-O-CONTROL";
		break;
	case COBC_HD_DATA_DIVISION:
		s = "DATA DIVISION";
		break;
	case COBC_HD_FILE_SECTION:
		s = "FILE SECTION";
		break;
	case COBC_HD_WORKING_STORAGE_SECTION:
		s = "WORKING-STORAGE SECTION";
		break;
	case COBC_HD_LOCAL_STORAGE_SECTION:
		s = "LOCAL-STORAGE SECTION";
		break;
	case COBC_HD_LINKAGE_SECTION:
		s = "LINKAGE SECTION";
		break;
	case COBC_HD_COMMUNICATION_SECTION:
		s = "COMMUNICATION SECTION";
		break;
	case COBC_HD_REPORT_SECTION:
		s = "REPORT SECTION";
		break;
	case COBC_HD_SCREEN_SECTION:
		s = "SCREEN SECTION";
		break;
	case COBC_HD_PROCEDURE_DIVISION:
		s = "PROCEDURE DIVISION";
		break;
	case COBC_HD_PROGRAM_ID:
		s = "PROGRAM-ID";
		break;
	/* LCOV_EXCL_START */
	default:
		s = _("unknown");
		break;
	/* LCOV_EXCL_STOP */
	}
	if (cb_relaxed_syntax_checks) {
		cb_warning (COBC_WARN_FILLER, _("%s header missing - assumed"), s);
	} else {
		cb_error (_("%s header missing"), s);
	}
}

/* check if headers are present - return 0 if fine, 1 if missing
   Lev1 must always be present and is checked
   Lev2/3/4, if non-zero (forced) may be present
*/
static int
check_headers_present (const cob_flags_t lev1, const cob_flags_t lev2,
		       const cob_flags_t lev3, const cob_flags_t lev4)
{
	int ret = 0;
	if (!(header_check & lev1)) {
		header_check |= lev1;
		check_relaxed_syntax (lev1);
		ret = 1;
	}
	if (lev2) {
		if (!(header_check & lev2)) {
			header_check |= lev2;
			check_relaxed_syntax (lev2);
			ret = 1;
		}
	}
	if (lev3) {
		if (!(header_check & lev3)) {
			header_check |= lev3;
			check_relaxed_syntax (lev3);
			ret = 1;
		}
	}
	if (lev4) {
		if (!(header_check & lev4)) {
			header_check |= lev4;
			check_relaxed_syntax (lev4);
			ret = 1;
		}
	}
	return ret;
}

/*
  TO-DO: Refactor header checks - have several header_checks: division_header,
  section_header, paragraph_header, sentence_type
*/
static void
set_conf_section_part (const cob_flags_t part)
{
	header_check &= ~COBC_HD_SOURCE_COMPUTER;
	header_check &= ~COBC_HD_OBJECT_COMPUTER;
	header_check &= ~COBC_HD_SPECIAL_NAMES;
	header_check &= ~COBC_HD_REPOSITORY;
	header_check |= part;
}

static const char *
get_conf_section_part_name (const cob_flags_t part)
{
	if (part == COBC_HD_SOURCE_COMPUTER) {
		return "SOURCE-COMPUTER";
	} else if (part == COBC_HD_OBJECT_COMPUTER) {
		return "OBJECT-COMPUTER";
	} else if (part == COBC_HD_SPECIAL_NAMES) {
		return "SPECIAL-NAMES";
	} else if (part == COBC_HD_REPOSITORY) {
		return "REPOSITORY";
	/* LCOV_EXCL_START */
	} else {
		/* This should never happen (and therefore doesn't get a translation) */
		cb_error ("unexpected configuration section part " CB_FMT_LLU, part);
		COBC_ABORT ();
	/* LCOV_EXCL_STOP */
	}
}

static int
get_conf_section_part_order (const cob_flags_t part)
{
	if (part == COBC_HD_SOURCE_COMPUTER) {
		return 1;
	} else if (part == COBC_HD_OBJECT_COMPUTER) {
		return 2;
	} else if (part == COBC_HD_SPECIAL_NAMES) {
		return 3;
	} else if (part == COBC_HD_REPOSITORY) {
		return 4;
	/* LCOV_EXCL_START */
	} else {
		/* This should never happen (and therefore doesn't get a translation) */
		cb_error ("unexpected configuration section part " CB_FMT_LLU, part);
		COBC_ABORT ();
	/* LCOV_EXCL_STOP */
	}
}

static void
check_conf_section_order (const cob_flags_t part)
{
	const cob_flags_t	prev_part
		= header_check & (COBC_HD_SOURCE_COMPUTER
				  | COBC_HD_OBJECT_COMPUTER
				  | COBC_HD_SPECIAL_NAMES
				  | COBC_HD_REPOSITORY);
#define MESSAGE_LEN 100
	char			message[MESSAGE_LEN] = { '\0' };

	if (prev_part == 0) {
		return;
	}

	if (prev_part == part) {
		cb_error (_("duplicate %s"), get_conf_section_part_name (part));
	} else if (get_conf_section_part_order (part) < get_conf_section_part_order (prev_part)) {
		snprintf (message, MESSAGE_LEN, _("%s incorrectly after %s"),
			  get_conf_section_part_name (part),
			  get_conf_section_part_name (prev_part));
		cb_verify (cb_incorrect_conf_sec_order, message);
	}
}

#undef MESSAGE_LEN

static void
build_nested_special (const int ndepth)
{
	cb_tree		x;
	cb_tree		y;

	if (!ndepth) {
		return;
	}

	/* Inherit special name mnemonics from parent */
	for (x = current_program->mnemonic_spec_list; x; x = CB_CHAIN (x)) {
		y = cb_build_reference (cb_name(CB_PURPOSE(x)));
		if (CB_SYSTEM_NAME_P (CB_VALUE(x))) {
			cb_define (y, CB_VALUE(x));
		} else {
			cb_build_constant (y, CB_VALUE(x));
		}
	}
}

static void
clear_initial_values (void)
{
	perform_stack = NULL;
	current_statement = NULL;
	main_statement = NULL;
	qualifier = NULL;
	in_declaratives = 0;
	in_debugging = 0;
	use_global_ind = 0;
	check_duplicate = 0;
	check_pic_duplicate = 0;
	skip_statements = 0;
	start_debug = 0;
	save_debug = 0;
	needs_field_debug = 0;
	needs_debug_item = 0;
	env_div_seen = 0;
	header_check = 0;
	next_label_id = 0;
	current_linage = 0;
	set_attr_val_on = 0;
	set_attr_val_off = 0;
	report_count = 0;
	current_storage = CB_STORAGE_WORKING;
	eval_level = 0;
	eval_inc = 0;
	eval_inc2 = 0;
	inspect_keyword = 0;
	check_unreached = 0;
	cobc_in_id = 0;
	cobc_in_procedure = 0;
	cobc_in_repository = 0;
	cobc_force_literal = 0;
	non_const_word = 0;
	suppress_data_exceptions = 0;
	same_area = 1;
	memset ((void *)eval_check, 0, sizeof(eval_check));
	memset ((void *)term_array, 0, sizeof(term_array));
	linage_file = NULL;
	current_file = NULL;
	current_cd = NULL;
	current_report = NULL;
	report_instance = NULL;
	next_label_list = NULL;
	default_rounded_mode = cb_int (COB_STORE_ROUND);
}

/*
  We must check for redefinitions of program-names and external program names
  outside of the usual reference/word_list methods as it may have to be done in
  a case-sensitive way.
*/
static void
begin_scope_of_program_name (struct cb_program *program)
{
	const char	*prog_name = program->program_name;
	const char	*prog_id = program->orig_program_id;
	const char	*elt_name;
	const char	*elt_id;
	cb_tree		l;

	/* Error if a program with the same name has been defined. */
	for (l = defined_prog_list; l; l = CB_CHAIN (l)) {
		elt_name = ((struct cb_program *) CB_VALUE (l))->program_name;
		elt_id = ((struct cb_program *) CB_VALUE (l))->orig_program_id;
		if (cb_fold_call && strcasecmp (prog_name, elt_name) == 0) {
			cb_error_x ((cb_tree) program,
				    _("redefinition of program name '%s'"),
				    elt_name);
		} else if (strcmp (prog_id, elt_id) == 0) {
		        cb_error_x ((cb_tree) program,
				    _("redefinition of program ID '%s'"),
				    elt_id);
			return;
		}
	}

	/* Otherwise, add the program to the list. */
	defined_prog_list = cb_list_add (defined_prog_list,
					 (cb_tree) program);
}

static void
remove_program_name (struct cb_list *l, struct cb_list *prev)
{
	if (prev == NULL) {
		defined_prog_list = l->chain;
	} else {
		prev->chain = l->chain;
	}
	cobc_parse_free (l);
}

/* Remove the program from defined_prog_list, if necessary. */
static void
end_scope_of_program_name (struct cb_program *program, const unsigned char type)
{
	struct	cb_list	*prev = NULL;
	struct	cb_list *l = (struct cb_list *) defined_prog_list;

	/* create empty entry if the program has no PROCEDURE DIVISION, error for UDF */
	if (!program->entry_list) {
		if (type == COB_MODULE_TYPE_FUNCTION) {
			cb_error (_("FUNCTION '%s' has no PROCEDURE DIVISION"), program->program_name);
		} else {
			emit_entry (program->program_id, 0, NULL, NULL);
		}
	}
	program->last_source_line = backup_source_line;

	if (program->nested_level == 0) {
		return;
	}

	/* Remove any subprograms */
	l = CB_LIST (defined_prog_list);
	while (l) {
		if (CB_PROGRAM (l->value)->nested_level > program->nested_level) {
			remove_program_name (l, prev);
		} else {
			prev = l;
		}
		if (prev && prev->chain != NULL) {
			l = CB_LIST (prev->chain);
		} else {
			l = NULL;
		}
	}

	/* Remove the specified program, if it is not COMMON */
	if (!program->flag_common) {
		l = (struct cb_list *) defined_prog_list;
		while (l) {
			if (strcmp (program->orig_program_id,
				    CB_PROGRAM (l->value)->orig_program_id)
			    == 0) {
				remove_program_name (l, prev);
				if (prev && prev->chain != NULL) {
					l = CB_LIST (prev->chain);
				} else {
					l = NULL;
				}
				break;
			} else {
				prev = l;
				if (l->chain != NULL) {
					l = CB_LIST (l->chain);
				} else {
					l = NULL;
				}
			}
		}
	}
}

static void
setup_program_start (void)
{
	if (setup_from_identification) {
		setup_from_identification = 0;
		return;
	}
	current_section = NULL;
	current_paragraph = NULL;

	if (depth != 0 && first_nested_program) {
		check_headers_present (COBC_HD_PROCEDURE_DIVISION, 0, 0, 0);
	}
	first_nested_program = 1;
}

static int
setup_program (cb_tree id, cb_tree as_literal, const unsigned char type)
{
	const char	*external_name = NULL;

	setup_program_start ();

	/* finish last program/function */
	if (!first_prog) {
		if (!current_program->flag_validated) {
			current_program->flag_validated = 1;
			cb_validate_program_body (current_program);
		}

		clear_initial_values ();
		current_program = cb_build_program (current_program, depth);
		build_nested_special (depth);
		cb_set_intr_when_compiled ();
		cb_build_registers ();
	} else {
		first_prog = 0;
	}

	/* set internal name */
	if (CB_LITERAL_P (id)) {
		current_program->program_name = (char *)CB_LITERAL (id)->data;
	} else {
		current_program->program_name = CB_NAME (id);
	}
	stack_progid[depth] = current_program->program_name;
	current_program->prog_type = type;

	if (depth != 0 && type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("functions may not be defined within a program/function"));
	}

	if (increment_depth ()) {
		return 1;
	}

	/* set external name if specified */
	if (as_literal) {
		external_name = (const char *)CB_LITERAL (as_literal)->data;
	} else {
		external_name = current_program->program_name;
	}

	/* build encoded external PROGRAM-ID */
	current_program->program_id
		= cb_build_program_id (external_name, type == COB_MODULE_TYPE_FUNCTION);

	if (type == COB_MODULE_TYPE_PROGRAM) {
		if (!main_flag_set) {
			main_flag_set = 1;
			current_program->flag_main = !!cobc_flag_main;
		}
	} else { /* COB_MODULE_TYPE_FUNCTION */
		current_program->flag_recursive = 1;
	}

	if (CB_REFERENCE_P (id)) {
		cb_define (id, CB_TREE (current_program));
	}

	begin_scope_of_program_name (current_program);

	return 0;
}

static void
decrement_depth (const char *name, const unsigned char type)
{
	int	d;

	if (depth) {
		depth--;
	}

	if (!strcmp (stack_progid[depth], name)) {
		return;
	}

	if (type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("END FUNCTION '%s' is different from FUNCTION-ID '%s'"),
			  name, stack_progid[depth]);
		return;
	}

	/* Set depth to that of whatever program we just ended, if it exists. */
	for (d = depth; d >= 0; --d) {
		if (!strcmp (stack_progid[d], name)) {
			depth = d;
			return;
		}
	}

	if (depth != d) {
		cb_error (_("END PROGRAM '%s' is different from PROGRAM-ID '%s'"),
			  name, stack_progid[depth]);
	}
}

static void
clean_up_program (cb_tree name, const unsigned char type)
{
	char		*s;

	end_scope_of_program_name (current_program, type);

	if (name) {
		if (CB_LITERAL_P (name)) {
			s = (char *)(CB_LITERAL (name)->data);
		} else {
			s = (char *)(CB_NAME (name));
		}

		decrement_depth (s, type);
	}

	current_section = NULL;
	current_paragraph = NULL;
	if (!current_program->flag_validated) {
		current_program->flag_validated = 1;
		cb_validate_program_body (current_program);
	}
}

static const char *
get_literal_or_word_name (const cb_tree x)
{
	if (CB_LITERAL_P (x)) {
		return (const char *) CB_LITERAL (x)->data;
	} else { /* CB_REFERENCE_P (x) */
		return (const char *) CB_NAME (x);
	}
}

/* verify and set currency symbol used in picture (compile time) and - if no currency
   string is explicitly set (which is currently not implemented) - as currency string
   (run time for display and [de-]editing)*/
static void
set_currency_picture_symbol (const cb_tree x)
{
	unsigned char	*s		= CB_LITERAL (x)->data;

	if (CB_LITERAL (x)->size != 1) {
		cb_error_x (x, _("currency symbol must be one character long"));
		return;
	}
	switch (*s) {
	case '0':
	case '1':
	case '2':
	case '3':
	case '4':
	case '5':
	case '6':
	case '7':
	case '8':
	case '9':
	case 'A':
	case 'B':
	case 'C':
	case 'D':
	case 'E':
	case 'N':
	case 'P':
	case 'R':
	case 'S':
	case 'V':
	case 'X':
	case 'Z':
	case 'a':
	case 'b':
	case 'c':
	case 'd':
	case 'e':
	case 'n':
	case 'p':
	case 'r':
	case 's':
	case 'v':
	case 'x':
	case 'z':
	case '+':
	case '-':
	case ',':
	case '.':
	case '*':
	case '/':
	case ';':
	case '(':
	case ')':
	case '=':
	case '\'':
	case '"':
	case ' ':
#if 0 /* note: MicroFocus also dissalows L (VAX) and G (OSVS) */
	case 'L':
	case 'G':
	case 'l':
	case 'g':
#endif
		cb_error_x (x, _("invalid character '%c' in currency symbol"), s[0]);
		return;
	default:
		break;
	}
	current_program->currency_symbol = s[0];
}

/* Return 1 if the prototype name is the same as the current function's. */
static int
check_prototype_redefines_current_element (const cb_tree prototype_name)
{
	const char	*name = get_literal_or_word_name (prototype_name);

	if (strcasecmp (name, current_program->program_name) == 0) {
		cb_warning_x (COBC_WARN_FILLER, prototype_name,
			_("prototype has same name as current function and will be ignored"));
		return 1;
	}

	return 0;
}

/* Returns 1 if the prototype has been duplicated. */
static int
check_for_duplicate_prototype (const cb_tree prototype_name,
			       const cb_tree prototype)
{
	cb_tree	dup;

	if (CB_WORD_COUNT (prototype_name) > 0) {
		/* Make sure the duplicate is a prototype */
		dup = cb_ref (prototype_name);
		if (!CB_PROTOTYPE_P (dup)) {
			redefinition_error (prototype_name);
			return 1;
		}

		/* Check the duplicate prototypes match */
		if (strcmp (CB_PROTOTYPE (prototype)->ext_name,
			    CB_PROTOTYPE (dup)->ext_name)
		    || CB_PROTOTYPE (prototype)->type != CB_PROTOTYPE (dup)->type) {
			cb_error_x (prototype_name,
				    _("duplicate REPOSITORY entries for '%s' do not match"),
				    get_literal_or_word_name (prototype_name));
		} else {
			cb_warning_x (COBC_WARN_FILLER, prototype_name,
				      _("duplicate REPOSITORY entry for '%s'"),
				      get_literal_or_word_name (prototype_name));
		}
		return 1;
	}

	return 0;
}

static void
setup_prototype (cb_tree prototype_name, cb_tree ext_name,
		  const int type, const int is_current_element)
{
	cb_tree	prototype;
	int	name_redefinition_allowed;

	if (!is_current_element
	    && check_prototype_redefines_current_element (prototype_name)) {
		return;
	}

	prototype = cb_build_prototype (prototype_name, ext_name, type);

	if (!is_current_element
	    && check_for_duplicate_prototype (prototype_name, prototype)) {
		return;
	}

	name_redefinition_allowed = type == COB_MODULE_TYPE_PROGRAM
		&& is_current_element && cb_program_name_redefinition;
	if (!name_redefinition_allowed) {
		if (CB_LITERAL_P (prototype_name)) {
			cb_define (cb_build_reference ((const char *)CB_LITERAL (prototype_name)->data), prototype);
		} else {
			cb_define (prototype_name, prototype);
		}

		if (type == COB_MODULE_TYPE_PROGRAM) {
			current_program->program_spec_list =
				cb_list_add (current_program->program_spec_list, prototype);
		} else { /* COB_MODULE_TYPE_FUNCTION */
			current_program->user_spec_list =
				cb_list_add (current_program->user_spec_list, prototype);
		}
	}
}

static void
error_if_record_delimiter_incompatible (const int organization,
					const char *organization_name)
{
	int	is_compatible;

	if (!current_file->flag_delimiter) {
		return;
	}

	if (organization == COB_ORG_LINE_SEQUENTIAL) {
		is_compatible = current_file->organization == COB_ORG_SEQUENTIAL
			|| current_file->organization == COB_ORG_LINE_SEQUENTIAL;
	} else {
		is_compatible = current_file->organization == organization;
	}

	if (!is_compatible) {
		cb_error (_("ORGANIZATION %s is incompatible with RECORD DELIMITER"),
			  organization_name);
	}
}

static void
error_if_invalid_level_for_renames (cb_tree item)
{
	int	level = CB_FIELD (cb_ref (item))->level;

	if (level == 1 || level == 66 || level == 77) {
	        cb_verify (cb_renames_uncommon_levels,
			   _("RENAMES of 01-, 66- and 77-level items"));
	} else if (level == 88) {
		cb_error (_("RENAMES may not reference a level 88"));
	}
}

static int
set_current_field (cb_tree level, cb_tree name)
{
	cb_tree	x  = cb_build_field_tree (level, name, current_field,
					  current_storage, current_file, 0);
	cobc_parse_free (level);

	if (CB_INVALID_TREE (x)) {
		return 1;
	} else {
		current_field = CB_FIELD (x);
		check_pic_duplicate = 0;
	}

	return 0;
}

static void
check_not_both (const cob_flags_t flag1, const cob_flags_t flag2,
		const char *flag1_name, const char *flag2_name,
		const cob_flags_t flags, const cob_flags_t flag_to_set)
{
	if (flag_to_set == flag1 && (flags & flag2)) {
		cb_error (_("cannot specify both %s and %s"),
			  flag1_name, flag2_name);
	} else if (flag_to_set == flag2 && (flags & flag1)) {
		cb_error (_("cannot specify both %s and %s"),
			  flag1_name, flag2_name);

	}
}

static COB_INLINE COB_A_INLINE void
check_not_highlight_and_lowlight (const cob_flags_t flags,
				  const cob_flags_t flag_to_set)
{
	check_not_both (COB_SCREEN_HIGHLIGHT, COB_SCREEN_LOWLIGHT,
			"HIGHLIGHT", "LOWLIGHT", flags, flag_to_set);
}

static void
set_screen_attr (const char *clause, const cob_flags_t bitval)
{
	if (current_field->screen_flag & bitval) {
		emit_duplicate_clause_message (clause);
	} else {
		current_field->screen_flag |= bitval;
	}
}

static void
emit_conflicting_clause_message (const char *clause, const char *conflicting)
{
	if (cb_relaxed_syntax_checks) {
		cb_warning (COBC_WARN_FILLER, _("cannot specify both %s and %s; %s is ignored"),
			clause, conflicting, clause);
	} else {
		cb_error (_("cannot specify both %s and %s"),
			clause, conflicting);
	}

}

static void
set_attr_with_conflict (const char *clause, const cob_flags_t bitval,
			const char *confl_clause, const cob_flags_t confl_bit,
			const int local_check_duplicate, cob_flags_t *flags)
{
	if (local_check_duplicate && (*flags & bitval)) {
		emit_duplicate_clause_message (clause);
	} else if (*flags & confl_bit) {
		emit_conflicting_clause_message (clause, confl_clause);
	} else {
	*flags |= bitval;
	}
}

static COB_INLINE COB_A_INLINE void
set_screen_attr_with_conflict (const char *clause, const cob_flags_t bitval,
			       const char *confl_clause,
			       const cob_flags_t confl_bit)
{
	set_attr_with_conflict (clause, bitval, confl_clause, confl_bit, 1,
				&current_field->screen_flag);
}

static COB_INLINE COB_A_INLINE int
has_dispattr (const cob_flags_t attrib)
{
	return current_statement->attr_ptr
		&& current_statement->attr_ptr->dispattrs & attrib;
}

static void
attach_attrib_to_cur_stmt (void)
{
	if (!current_statement->attr_ptr) {
		current_statement->attr_ptr =
			cobc_parse_malloc (sizeof(struct cb_attr_struct));
	}
}

static COB_INLINE COB_A_INLINE void
set_dispattr (const cob_flags_t attrib)
{
	attach_attrib_to_cur_stmt ();
	current_statement->attr_ptr->dispattrs |= attrib;
}

static COB_INLINE COB_A_INLINE void
set_dispattr_with_conflict (const char *attrib_name, const cob_flags_t attrib,
			    const char *confl_name,
			    const cob_flags_t confl_attrib)
{
	attach_attrib_to_cur_stmt ();
	set_attr_with_conflict (attrib_name, attrib, confl_name, confl_attrib, 0,
				&current_statement->attr_ptr->dispattrs);
}

static void
bit_set_attr (const cb_tree on_off, const cob_flags_t attr_val)
{
	if (on_off == cb_int1) {
		set_attr_val_on |= attr_val;
	} else {
		set_attr_val_off |= attr_val;
	}
}

static void
set_field_attribs (cb_tree fgc, cb_tree bgc, cb_tree scroll,
		   cb_tree timeout, cb_tree prompt, cb_tree size_is)
{
	/* [WITH] FOREGROUND-COLOR [IS] */
	if (fgc) {
		current_statement->attr_ptr->fgc = fgc;
	}
	/* [WITH] BACKGROUND-COLOR [IS] */
	if (bgc) {
		current_statement->attr_ptr->bgc = bgc;
	}
	/* [WITH] SCROLL UP | DOWN */
	if (scroll) {
		current_statement->attr_ptr->scroll = scroll;
	}
	/* [WITH] TIME-OUT [AFTER] */
	if (timeout) {
		current_statement->attr_ptr->timeout = timeout;
	}
	/* [WITH] PROMPT CHARACTER [IS] */
	if (prompt) {
		current_statement->attr_ptr->prompt = prompt;
	}
	/* [WITH] SIZE [IS] */
	if (size_is) {
		current_statement->attr_ptr->size_is = size_is;
	}
}

static void
set_attribs (cb_tree fgc, cb_tree bgc, cb_tree scroll,
	     cb_tree timeout, cb_tree prompt, cb_tree size_is,
	     const cob_flags_t attrib)
{
	attach_attrib_to_cur_stmt ();
	set_field_attribs (fgc, bgc, scroll, timeout, prompt, size_is);

	current_statement->attr_ptr->dispattrs |= attrib;
}

static void
set_attribs_with_conflict  (cb_tree fgc, cb_tree bgc, cb_tree scroll,
			    cb_tree timeout, cb_tree prompt, cb_tree size_is,
			    const char *clause_name, const cob_flags_t attrib,
			    const char *confl_name, const cob_flags_t confl_attrib)
{
	attach_attrib_to_cur_stmt ();
	set_field_attribs (fgc, bgc, scroll, timeout, prompt, size_is);

	set_dispattr_with_conflict (clause_name, attrib, confl_name,
				    confl_attrib);
}

static cob_flags_t
zero_conflicting_flag (const cob_flags_t screen_flag, cob_flags_t parent_flag,
				const cob_flags_t flag1, const cob_flags_t flag2)
{
	if (screen_flag & flag1) {
		parent_flag &= ~flag2;
	} else if (screen_flag & flag2) {
		parent_flag &= ~flag1;
	}

	return parent_flag;
}

static cob_flags_t
zero_conflicting_flags (const cob_flags_t screen_flag, cob_flags_t parent_flag)
{
	parent_flag = zero_conflicting_flag (screen_flag, parent_flag,
					     COB_SCREEN_BLANK_LINE,
					     COB_SCREEN_BLANK_SCREEN);
	parent_flag = zero_conflicting_flag (screen_flag, parent_flag,
					     COB_SCREEN_ERASE_EOL,
					     COB_SCREEN_ERASE_EOS);
	parent_flag = zero_conflicting_flag (screen_flag, parent_flag,
					     COB_SCREEN_HIGHLIGHT,
					     COB_SCREEN_LOWLIGHT);

	return parent_flag;
}

static void
check_and_set_usage (const enum cb_usage usage)
{
	check_repeated ("USAGE", SYN_CLAUSE_5, &check_pic_duplicate);
	current_field->usage = usage;
}

static void
check_preceding_tallying_phrases (const enum tallying_phrase phrase)
{
	switch (phrase) {
	case FOR_PHRASE:
		if (previous_tallying_phrase == ALL_LEADING_TRAILING_PHRASES) {
			cb_error (_("FOR phrase cannot immediately follow ALL/LEADING/TRAILING"));
		} else if (previous_tallying_phrase == FOR_PHRASE) {
			cb_error (_("missing CHARACTERS/ALL/LEADING/TRAILING phrase after FOR phrase"));
		}
		break;

	case CHARACTERS_PHRASE:
	case ALL_LEADING_TRAILING_PHRASES:
		if (previous_tallying_phrase == NO_PHRASE) {
			cb_error (_("missing FOR phrase before CHARACTERS/ALL/LEADING/TRAILING phrase"));
		} else if (previous_tallying_phrase == CHARACTERS_PHRASE
			   || previous_tallying_phrase == ALL_LEADING_TRAILING_PHRASES) {
			cb_error (_("missing value between CHARACTERS/ALL/LEADING/TRAILING words"));
		}
		break;

	case VALUE_REGION_PHRASE:
		if (!(previous_tallying_phrase == ALL_LEADING_TRAILING_PHRASES
		      || previous_tallying_phrase == VALUE_REGION_PHRASE)) {
			cb_error (_("missing ALL/LEADING/TRAILING before value"));
		}
		break;

	/* LCOV_EXCL_START */
	default:
		/* This should never happen (and therefore doesn't get a translation) */
		cb_error ("unexpected tallying phrase");
		COBC_ABORT();
	/* LCOV_EXCL_STOP */
	}

	previous_tallying_phrase = phrase;
}

static int
has_relative_pos (struct cb_field const *field)
{
	return !!(field->screen_flag
		  & (COB_SCREEN_LINE_PLUS | COB_SCREEN_LINE_MINUS
		     | COB_SCREEN_COLUMN_PLUS | COB_SCREEN_COLUMN_MINUS));
}

static int
is_recursive_call (cb_tree target)
{
	const char *target_name = "";

	if (CB_LITERAL_P (target)) {
		target_name = (const char *)(CB_LITERAL(target)->data);
	} else if (CB_REFERENCE_P (target)
		   && CB_PROTOTYPE_P (cb_ref (target))) {
		target_name = CB_PROTOTYPE (cb_ref (target))->ext_name;
	}

	return !strcmp (target_name, current_program->orig_program_id);
}

static void
check_not_88_level (cb_tree x)
{
	struct cb_field	*f;

	if (x == cb_error_node || x->tag != CB_TAG_REFERENCE) {
		return;
	}

	f = CB_FIELD (cb_ref (x));

	if (f != (struct cb_field *) cb_error_node && f->level == 88) {
		cb_error (_("level %02d item '%s' may not be used here"), 88, cb_name (x));
	}
}

static int
is_screen_field (cb_tree x)
{
	if (CB_FIELD_P (x)) {
		return (CB_FIELD (x))->storage == CB_STORAGE_SCREEN;
	} else if (CB_REFERENCE_P (x)) {
		return is_screen_field (cb_ref (x));
	} else {
		return 0;
	}
}

static void
error_if_no_advancing_in_screen_display (cb_tree advancing)
{
	if (advancing != cb_int1) {
		cb_error (_("cannot specify NO ADVANCING in screen DISPLAY"));
	}
}

static cb_tree
get_default_display_device (void)
{
	if (current_program->flag_console_is_crt
	    || cb_console_is_crt) {
		return cb_null;
	} else {
		return cb_int0;
	}
}

static COB_INLINE COB_A_INLINE int
contains_one_screen_field (struct cb_list *x_list)
{
	return (cb_tree) x_list != cb_null
		&& cb_list_length ((cb_tree) x_list) == 1
		&& is_screen_field (x_list->value);
}

static int
contains_only_screen_fields (struct cb_list *x_list)
{
	if ((cb_tree) x_list == cb_null) {
		return 0;
	}

	for (; x_list; x_list = (struct cb_list *) x_list->chain) {
		if (!is_screen_field (x_list->value)) {
			return 0;
		}
	}

	return 1;
}

static int
contains_fields_and_screens (struct cb_list *x_list)
{
	int	field_seen = 0;
	int	screen_seen = 0;

	if ((cb_tree) x_list == cb_null) {
		return 0;
	}

	for (; x_list; x_list = (struct cb_list *) x_list->chain) {
		if (is_screen_field (x_list->value)) {
			screen_seen = 1;
		} else {
			field_seen = 1;
		}
	}

	return screen_seen && field_seen;
}

static enum cb_display_type
deduce_display_type (cb_tree x_list, cb_tree local_upon_value, cb_tree local_line_column,
		     struct cb_attr_struct * const attr_ptr)
{
	int	using_default_device_which_is_crt =
		local_upon_value == NULL && get_default_display_device () == cb_null;

	/* TODO: Separate CGI DISPLAYs here */
	if (contains_only_screen_fields ((struct cb_list *) x_list)) {
		if (!contains_one_screen_field ((struct cb_list *) x_list)
		    || attr_ptr) {
			cb_verify_x (x_list, cb_accept_display_extensions,
				     _("non-standard DISPLAY"));
		}

		if (local_upon_value != NULL && local_upon_value != cb_null) {
			cb_error_x (x_list, _("screens may only be displayed on CRT"));
		}

		return SCREEN_DISPLAY;
	} else if (contains_fields_and_screens ((struct cb_list *) x_list)) {
		cb_error_x (x_list, _("cannot mix screens and fields in the same DISPLAY statement"));
		return MIXED_DISPLAY;
	} else if (local_line_column || attr_ptr) {
		if (local_upon_value != NULL && local_upon_value != cb_null) {
			cb_error_x (x_list, _("screen clauses may only be used for DISPLAY on CRT"));
		}

		cb_verify_x (x_list, cb_accept_display_extensions,
			     _("non-standard DISPLAY"));

		return FIELD_ON_SCREEN_DISPLAY;
	} else if (local_upon_value == cb_null || using_default_device_which_is_crt) {
		/* This is the only format permitted by the standard */
		return FIELD_ON_SCREEN_DISPLAY;
	} else if (display_type == FIELD_ON_SCREEN_DISPLAY && local_upon_value == NULL) {
		/* This is for when fields without clauses follow fields with screen clauses */
		return FIELD_ON_SCREEN_DISPLAY;
	} else {
		return DEVICE_DISPLAY;
	}
}

static void
set_display_type (cb_tree x_list, cb_tree local_upon_value,
		  cb_tree local_line_column, struct cb_attr_struct * const attr_ptr)
{
	display_type = deduce_display_type (x_list, local_upon_value, local_line_column, attr_ptr);
}

static void
error_if_different_display_type (cb_tree x_list, cb_tree local_upon_value,
				 cb_tree local_line_column, struct cb_attr_struct * const attr_ptr)
{
	const enum cb_display_type	type =
		deduce_display_type (x_list, local_upon_value, local_line_column, attr_ptr);

	/* Avoid re-displaying the same error for mixed DISPLAYs */
	if (type == display_type || display_type == MIXED_DISPLAY) {
		return;
	}

	if (type != MIXED_DISPLAY) {
		if (type == SCREEN_DISPLAY || display_type == SCREEN_DISPLAY) {
			cb_error_x (x_list, _("cannot mix screens and fields in the same DISPLAY statement"));
		} else {
			/*
			  The only other option is that there is a mix of
			  FIELD_ON_SCREEN_DISPLAY and DEVICE_DISPLAY.
			*/
			cb_error_x (x_list, _("ambiguous DISPLAY; put items to display on device in separate DISPLAY"));
		}
	}

	display_type = MIXED_DISPLAY;
}

static void
error_if_not_usage_display_or_nonnumeric_lit (cb_tree x)
{
	const int	is_numeric_literal = CB_NUMERIC_LITERAL_P (x);
	const int	is_field_with_usage_not_display =
		CB_REFERENCE_P (x) && CB_FIELD (cb_ref (x))
		&& CB_FIELD (cb_ref (x))->usage != CB_USAGE_DISPLAY;

	if (is_numeric_literal) {
		cb_error_x (x, _("%s is not an alphanumeric literal"), CB_LITERAL (x)->data);
	} else if (is_field_with_usage_not_display) {
		cb_error_x (x, _("'%s' is not USAGE DISPLAY"), cb_name (x));
	}
}

static void
check_validate_item (cb_tree x)
{
	struct cb_field	*f;
	enum cb_class	tree_class;

	if (CB_INVALID_TREE(x) || x->tag != CB_TAG_REFERENCE) {
		return;
	}
	x = cb_ref (x);
	if (CB_INVALID_TREE (x) || !CB_FIELD_P (x)) {
		cb_error (_("invalid target for %s"), "VALIDATE");
		return;
	}

	f = CB_FIELD (x);
	tree_class = CB_TREE_CLASS(f);
	if (is_screen_field(x)) {
		cb_error (_("SCREEN item cannot be used here"));
	} else if (f->level == 66) {
		cb_error (_("level %02d item '%s' may not be used here"), 66, cb_name (x));
	} else if (f->flag_any_length) {
		cb_error (_("ANY LENGTH item not allowed here"));
	} else if (tree_class == CB_CLASS_INDEX
		|| tree_class == CB_CLASS_OBJECT
		|| tree_class == CB_CLASS_POINTER) {
		cb_error (_("item '%s' has wrong class for VALIDATE"), cb_name (x));
	}
}


#line 1919 "parser.c" /* yacc.c:339  */

# ifndef YY_NULLPTR
#  if defined __cplusplus && 201103L <= __cplusplus
#   define YY_NULLPTR nullptr
#  else
#   define YY_NULLPTR 0
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* In a future release of Bison, this section will be replaced
   by #include "y.tab.h".  */
#ifndef YY_YY_PARSER_H_INCLUDED
# define YY_YY_PARSER_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int yydebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    TOKEN_EOF = 0,
    THREEDIMENSIONAL = 258,
    ABSENT = 259,
    ACCEPT = 260,
    ACCESS = 261,
    ACTIVEX = 262,
    ACTION = 263,
    ADD = 264,
    ADDRESS = 265,
    ADJUSTABLE_COLUMNS = 266,
    ADVANCING = 267,
    AFTER = 268,
    ALIGNMENT = 269,
    ALL = 270,
    ALLOCATE = 271,
    ALPHABET = 272,
    ALPHABETIC = 273,
    ALPHABETIC_LOWER = 274,
    ALPHABETIC_UPPER = 275,
    ALPHANUMERIC = 276,
    ALPHANUMERIC_EDITED = 277,
    ALSO = 278,
    ALTER = 279,
    ALTERNATE = 280,
    AND = 281,
    ANY = 282,
    ARE = 283,
    AREA = 284,
    AREAS = 285,
    ARGUMENT_NUMBER = 286,
    ARGUMENT_VALUE = 287,
    ARITHMETIC = 288,
    AS = 289,
    ASCENDING = 290,
    ASCII = 291,
    ASSIGN = 292,
    AT = 293,
    ATTRIBUTE = 294,
    AUTO = 295,
    AUTO_DECIMAL = 296,
    AUTO_SPIN = 297,
    AUTOMATIC = 298,
    AWAY_FROM_ZERO = 299,
    BACKGROUND_COLOR = 300,
    BACKGROUND_HIGH = 301,
    BACKGROUND_LOW = 302,
    BACKGROUND_STANDARD = 303,
    BAR = 304,
    BASED = 305,
    BEFORE = 306,
    BELL = 307,
    BINARY = 308,
    BINARY_C_LONG = 309,
    BINARY_CHAR = 310,
    BINARY_DOUBLE = 311,
    BINARY_LONG = 312,
    BINARY_SEQUENTIAL = 313,
    BINARY_SHORT = 314,
    BIT = 315,
    BITMAP = 316,
    BITMAP_END = 317,
    BITMAP_HANDLE = 318,
    BITMAP_NUMBER = 319,
    BITMAP_START = 320,
    BITMAP_TIMER = 321,
    BITMAP_TRAILING = 322,
    BITMAP_TRANSPARENT_COLOR = 323,
    BITMAP_WIDTH = 324,
    BLANK = 325,
    BLINK = 326,
    BLOCK = 327,
    BOTTOM = 328,
    BOX = 329,
    BOXED = 330,
    BUSY = 331,
    BUTTONS = 332,
    BY = 333,
    BYTE_LENGTH = 334,
    CALENDAR_FONT = 335,
    CALL = 336,
    CANCEL = 337,
    CANCEL_BUTTON = 338,
    CAPACITY = 339,
    CARD_PUNCH = 340,
    CARD_READER = 341,
    CASSETTE = 342,
    CCOL = 343,
    CD = 344,
    CELL = 345,
    CELL_COLOR = 346,
    CELL_DATA = 347,
    CELL_FONT = 348,
    CELL_PROTECTION = 349,
    CENTER = 350,
    CENTERED_HEADINGS = 351,
    CENTURY_DATE = 352,
    CF = 353,
    CH = 354,
    CHAINING = 355,
    CHARACTER = 356,
    CHARACTERS = 357,
    CHECK_BOX = 358,
    CLASS = 359,
    CLASSIFICATION = 360,
    CLASS_NAME = 361,
    CLEAR_SELECTION = 362,
    CLINE = 363,
    CLINES = 364,
    CLOSE = 365,
    COBOL = 366,
    CODE = 367,
    CODE_SET = 368,
    COLLATING = 369,
    COL = 370,
    COLOR = 371,
    COLORS = 372,
    COLS = 373,
    COLUMN = 374,
    COLUMN_COLOR = 375,
    COLUMN_DIVIDERS = 376,
    COLUMN_FONT = 377,
    COLUMN_HEADINGS = 378,
    COLUMN_PROTECTION = 379,
    COLUMNS = 380,
    COMBO_BOX = 381,
    COMMA = 382,
    COMMAND_LINE = 383,
    COMMA_DELIM = 384,
    COMMIT = 385,
    COMMON = 386,
    COMMUNICATION = 387,
    COMP = 388,
    COMPUTE = 389,
    COMP_1 = 390,
    COMP_2 = 391,
    COMP_3 = 392,
    COMP_4 = 393,
    COMP_5 = 394,
    COMP_6 = 395,
    COMP_X = 396,
    CONCATENATE_FUNC = 397,
    CONDITION = 398,
    CONFIGURATION = 399,
    CONSTANT = 400,
    CONTAINS = 401,
    CONTENT = 402,
    CONTINUE = 403,
    CONTROL = 404,
    CONTROLS = 405,
    CONVERSION = 406,
    CONVERTING = 407,
    COPY = 408,
    COPY_SELECTION = 409,
    CORRESPONDING = 410,
    COUNT = 411,
    CRT = 412,
    CRT_UNDER = 413,
    CSIZE = 414,
    CURRENCY = 415,
    CURRENT_DATE_FUNC = 416,
    CURSOR = 417,
    CURSOR_COL = 418,
    CURSOR_COLOR = 419,
    CURSOR_FRAME_WIDTH = 420,
    CURSOR_ROW = 421,
    CURSOR_X = 422,
    CURSOR_Y = 423,
    CUSTOM_PRINT_TEMPLATE = 424,
    CYCLE = 425,
    DASHED = 426,
    DATA = 427,
    DATA_COLUMNS = 428,
    DATA_TYPES = 429,
    DATE = 430,
    DATE_ENTRY = 431,
    DAY = 432,
    DAY_OF_WEEK = 433,
    DE = 434,
    DEBUGGING = 435,
    DECIMAL_POINT = 436,
    DECLARATIVES = 437,
    DEFAULT = 438,
    DEFAULT_BUTTON = 439,
    DEFAULT_FONT = 440,
    DELETE = 441,
    DELIMITED = 442,
    DELIMITER = 443,
    DEPENDING = 444,
    DESCENDING = 445,
    DESTINATION = 446,
    DESTROY = 447,
    DETAIL = 448,
    DISABLE = 449,
    DISC = 450,
    DISK = 451,
    DISPLAY = 452,
    DISPLAY_COLUMNS = 453,
    DISPLAY_FORMAT = 454,
    DISPLAY_OF_FUNC = 455,
    DIVIDE = 456,
    DIVIDERS = 457,
    DIVIDER_COLOR = 458,
    DIVISION = 459,
    DOTDASH = 460,
    DOTTED = 461,
    DRAG_COLOR = 462,
    DROP_DOWN = 463,
    DROP_LIST = 464,
    DOWN = 465,
    DUPLICATES = 466,
    DYNAMIC = 467,
    EBCDIC = 468,
    EC = 469,
    ECHO = 470,
    EGI = 471,
    EIGHTY_EIGHT = 472,
    ENABLE = 473,
    ELSE = 474,
    EMI = 475,
    END = 476,
    END_ACCEPT = 477,
    END_ADD = 478,
    END_CALL = 479,
    END_COMPUTE = 480,
    END_COLOR = 481,
    END_DELETE = 482,
    END_DISPLAY = 483,
    END_DIVIDE = 484,
    END_EVALUATE = 485,
    END_FUNCTION = 486,
    END_IF = 487,
    END_MODIFY = 488,
    END_MULTIPLY = 489,
    END_PERFORM = 490,
    END_PROGRAM = 491,
    END_READ = 492,
    END_RECEIVE = 493,
    END_RETURN = 494,
    END_REWRITE = 495,
    END_SEARCH = 496,
    END_START = 497,
    END_STRING = 498,
    END_SUBTRACT = 499,
    END_UNSTRING = 500,
    END_WRITE = 501,
    ENGRAVED = 502,
    ENSURE_VISIBLE = 503,
    ENTRY = 504,
    ENTRY_CONVENTION = 505,
    ENTRY_FIELD = 506,
    ENTRY_REASON = 507,
    ENVIRONMENT = 508,
    ENVIRONMENT_NAME = 509,
    ENVIRONMENT_VALUE = 510,
    EOL = 511,
    EOP = 512,
    EOS = 513,
    EQUAL = 514,
    ERASE = 515,
    ERROR = 516,
    ESCAPE = 517,
    ESCAPE_BUTTON = 518,
    ESI = 519,
    EVALUATE = 520,
    EVENT = 521,
    EVENT_LIST = 522,
    EVENT_STATUS = 523,
    EXCEPTION = 524,
    EXCEPTION_CONDITION = 525,
    EXCEPTION_VALUE = 526,
    EXPAND = 527,
    EXCLUSIVE = 528,
    EXIT = 529,
    EXPONENTIATION = 530,
    EXTEND = 531,
    EXTERNAL = 532,
    EXTERNAL_FORM = 533,
    F = 534,
    FD = 535,
    FILE_CONTROL = 536,
    FILE_ID = 537,
    FILE_NAME = 538,
    FILE_POS = 539,
    FILL_COLOR = 540,
    FILL_COLOR2 = 541,
    FILL_PERCENT = 542,
    FILLER = 543,
    FINAL = 544,
    FINISH_REASON = 545,
    FIRST = 546,
    FIXED = 547,
    FIXED_FONT = 548,
    FIXED_WIDTH = 549,
    FLAT = 550,
    FLAT_BUTTONS = 551,
    FLOAT_BINARY_128 = 552,
    FLOAT_BINARY_32 = 553,
    FLOAT_BINARY_64 = 554,
    FLOAT_DECIMAL_16 = 555,
    FLOAT_DECIMAL_34 = 556,
    FLOAT_DECIMAL_7 = 557,
    FLOAT_EXTENDED = 558,
    FLOAT_LONG = 559,
    FLOAT_SHORT = 560,
    FLOATING = 561,
    FONT = 562,
    FOOTING = 563,
    FOR = 564,
    FOREGROUND_COLOR = 565,
    FOREVER = 566,
    FORMATTED_DATE_FUNC = 567,
    FORMATTED_DATETIME_FUNC = 568,
    FORMATTED_TIME_FUNC = 569,
    FRAME = 570,
    FRAMED = 571,
    FREE = 572,
    FROM = 573,
    FROM_CRT = 574,
    FULL = 575,
    FULL_HEIGHT = 576,
    FUNCTION = 577,
    FUNCTION_ID = 578,
    FUNCTION_NAME = 579,
    GENERATE = 580,
    GIVING = 581,
    GLOBAL = 582,
    GO = 583,
    GO_BACK = 584,
    GO_FORWARD = 585,
    GO_HOME = 586,
    GO_SEARCH = 587,
    GOBACK = 588,
    GRAPHICAL = 589,
    GREATER = 590,
    GREATER_OR_EQUAL = 591,
    GRID = 592,
    GROUP = 593,
    GROUP_VALUE = 594,
    HANDLE = 595,
    HAS_CHILDREN = 596,
    HEADING = 597,
    HEADING_COLOR = 598,
    HEADING_DIVIDER_COLOR = 599,
    HEADING_FONT = 600,
    HEAVY = 601,
    HEIGHT_IN_CELLS = 602,
    HIDDEN_DATA = 603,
    HIGHLIGHT = 604,
    HIGH_COLOR = 605,
    HIGH_VALUE = 606,
    HOT_TRACK = 607,
    HSCROLL = 608,
    HSCROLL_POS = 609,
    ICON = 610,
    ID = 611,
    IDENTIFIED = 612,
    IDENTIFICATION = 613,
    IF = 614,
    IGNORE = 615,
    IGNORING = 616,
    IN = 617,
    INDEPENDENT = 618,
    INDEX = 619,
    INDEXED = 620,
    INDICATE = 621,
    INITIALIZE = 622,
    INITIALIZED = 623,
    INITIATE = 624,
    INPUT = 625,
    INPUT_OUTPUT = 626,
    INQUIRE = 627,
    INSERTION_INDEX = 628,
    INSERT_ROWS = 629,
    INSPECT = 630,
    INTERMEDIATE = 631,
    INTO = 632,
    INTRINSIC = 633,
    INVALID = 634,
    INVALID_KEY = 635,
    IS = 636,
    ITEM = 637,
    ITEM_TEXT = 638,
    ITEM_TO_ADD = 639,
    ITEM_TO_DELETE = 640,
    ITEM_TO_EMPTY = 641,
    ITEM_VALUE = 642,
    I_O = 643,
    I_O_CONTROL = 644,
    JUSTIFIED = 645,
    KEPT = 646,
    KEY = 647,
    KEYBOARD = 648,
    LABEL = 649,
    LABEL_OFFSET = 650,
    LARGE_FONT = 651,
    LARGE_OFFSET = 652,
    LAST = 653,
    LAST_ROW = 654,
    LAYOUT_DATA = 655,
    LAYOUT_MANAGER = 656,
    LEADING = 657,
    LEADING_SHIFT = 658,
    LEFT = 659,
    LEFTLINE = 660,
    LEFT_TEXT = 661,
    LENGTH = 662,
    LENGTH_FUNC = 663,
    LENGTH_OF = 664,
    LESS = 665,
    LESS_OR_EQUAL = 666,
    LIMIT = 667,
    LIMITS = 668,
    LINAGE = 669,
    LINAGE_COUNTER = 670,
    LINE = 671,
    LINE_COUNTER = 672,
    LINE_LIMIT = 673,
    LINE_SEQUENTIAL = 674,
    LINES = 675,
    LINES_AT_ROOT = 676,
    LINKAGE = 677,
    LIST_BOX = 678,
    LITERAL = 679,
    LM_RESIZE = 680,
    LOCALE = 681,
    LOCALE_DATE_FUNC = 682,
    LOCALE_TIME_FUNC = 683,
    LOCALE_TIME_FROM_FUNC = 684,
    LOCAL_STORAGE = 685,
    LOCK = 686,
    LONG_DATE = 687,
    LOWER = 688,
    LOWERED = 689,
    LOWER_CASE_FUNC = 690,
    LOWLIGHT = 691,
    LOW_COLOR = 692,
    LOW_VALUE = 693,
    MAGNETIC_TAPE = 694,
    MANUAL = 695,
    MASS_UPDATE = 696,
    MAX_LINES = 697,
    MAX_PROGRESS = 698,
    MAX_TEXT = 699,
    MAX_VAL = 700,
    MEMORY = 701,
    MEDIUM_FONT = 702,
    MENU = 703,
    MERGE = 704,
    MESSAGE = 705,
    MINUS = 706,
    MIN_VAL = 707,
    MNEMONIC_NAME = 708,
    MODE = 709,
    MODIFY = 710,
    MODULES = 711,
    MOVE = 712,
    MULTILINE = 713,
    MULTIPLE = 714,
    MULTIPLY = 715,
    NAME = 716,
    NATIONAL = 717,
    NATIONAL_EDITED = 718,
    NATIONAL_OF_FUNC = 719,
    NATIVE = 720,
    NAVIGATE_URL = 721,
    NEAREST_AWAY_FROM_ZERO = 722,
    NEAREST_EVEN = 723,
    NEAREST_TOWARD_ZERO = 724,
    NEGATIVE = 725,
    NESTED = 726,
    NEW = 727,
    NEXT = 728,
    NEXT_ITEM = 729,
    NEXT_GROUP = 730,
    NEXT_PAGE = 731,
    NO = 732,
    NO_ADVANCING = 733,
    NO_AUTOSEL = 734,
    NO_AUTO_DEFAULT = 735,
    NO_BOX = 736,
    NO_DATA = 737,
    NO_DIVIDERS = 738,
    NO_ECHO = 739,
    NO_F4 = 740,
    NO_FOCUS = 741,
    NO_GROUP_TAB = 742,
    NO_KEY_LETTER = 743,
    NO_SEARCH = 744,
    NO_UPDOWN = 745,
    NORMAL = 746,
    NOT = 747,
    NOTAB = 748,
    NOTHING = 749,
    NOTIFY = 750,
    NOTIFY_CHANGE = 751,
    NOTIFY_DBLCLICK = 752,
    NOTIFY_SELCHANGE = 753,
    NOT_END = 754,
    NOT_EOP = 755,
    NOT_ESCAPE = 756,
    NOT_EQUAL = 757,
    NOT_EXCEPTION = 758,
    NOT_INVALID_KEY = 759,
    NOT_OVERFLOW = 760,
    NOT_SIZE_ERROR = 761,
    NUM_COL_HEADINGS = 762,
    NUM_ROWS = 763,
    NUMBER = 764,
    NUMBERS = 765,
    NUMERIC = 766,
    NUMERIC_EDITED = 767,
    NUMVALC_FUNC = 768,
    OBJECT = 769,
    OBJECT_COMPUTER = 770,
    OCCURS = 771,
    OF = 772,
    OFF = 773,
    OK_BUTTON = 774,
    OMITTED = 775,
    ON = 776,
    ONLY = 777,
    OPEN = 778,
    OPTIONAL = 779,
    OPTIONS = 780,
    OR = 781,
    ORDER = 782,
    ORGANIZATION = 783,
    OTHER = 784,
    OUTPUT = 785,
    OVERLAP_LEFT = 786,
    OVERLAP_TOP = 787,
    OVERLINE = 788,
    PACKED_DECIMAL = 789,
    PADDING = 790,
    PAGE = 791,
    PAGE_COUNTER = 792,
    PAGE_SETUP = 793,
    PAGED = 794,
    PARAGRAPH = 795,
    PARENT = 796,
    PASSWORD = 797,
    PERFORM = 798,
    PERMANENT = 799,
    PH = 800,
    PF = 801,
    PHYSICAL = 802,
    PICTURE = 803,
    PICTURE_SYMBOL = 804,
    PIXEL = 805,
    PLACEMENT = 806,
    PLUS = 807,
    POINTER = 808,
    POP_UP = 809,
    POSITION = 810,
    POSITION_SHIFT = 811,
    POSITIVE = 812,
    PRESENT = 813,
    PREVIOUS = 814,
    PRINT = 815,
    PRINT_NO_PROMPT = 816,
    PRINT_PREVIEW = 817,
    PRINTER = 818,
    PRINTER_1 = 819,
    PRINTING = 820,
    PRIORITY = 821,
    PROCEDURE = 822,
    PROCEDURES = 823,
    PROCEED = 824,
    PROGRAM = 825,
    PROGRAM_ID = 826,
    PROGRAM_NAME = 827,
    PROGRAM_POINTER = 828,
    PROGRESS = 829,
    PROHIBITED = 830,
    PROMPT = 831,
    PROPERTIES = 832,
    PROPERTY = 833,
    PROTECTED = 834,
    PURGE = 835,
    PUSH_BUTTON = 836,
    QUERY_INDEX = 837,
    QUEUE = 838,
    QUOTE = 839,
    RADIO_BUTTON = 840,
    RAISED = 841,
    RANDOM = 842,
    RD = 843,
    READ = 844,
    READ_ONLY = 845,
    READY_TRACE = 846,
    RECEIVE = 847,
    RECORD = 848,
    RECORD_DATA = 849,
    RECORD_TO_ADD = 850,
    RECORD_TO_DELETE = 851,
    RECORDING = 852,
    RECORDS = 853,
    RECURSIVE = 854,
    REDEFINES = 855,
    REEL = 856,
    REFERENCE = 857,
    REFERENCES = 858,
    REFRESH = 859,
    REGION_COLOR = 860,
    RELATIVE = 861,
    RELEASE = 862,
    REMAINDER = 863,
    REMOVAL = 864,
    RENAMES = 865,
    REPLACE = 866,
    REPLACING = 867,
    REPORT = 868,
    REPORTING = 869,
    REPORTS = 870,
    REPOSITORY = 871,
    REQUIRED = 872,
    RESERVE = 873,
    RESET = 874,
    RESET_TRACE = 875,
    RESET_GRID = 876,
    RESET_LIST = 877,
    RESET_TABS = 878,
    RETRY = 879,
    RETURN = 880,
    RETURNING = 881,
    REVERSE = 882,
    REVERSE_FUNC = 883,
    REVERSE_VIDEO = 884,
    REVERSED = 885,
    REWIND = 886,
    REWRITE = 887,
    RF = 888,
    RH = 889,
    RIGHT = 890,
    RIGHT_ALIGN = 891,
    RIMMED = 892,
    ROLLBACK = 893,
    ROUNDED = 894,
    ROUNDING = 895,
    ROW_COLOR = 896,
    ROW_COLOR_PATTERN = 897,
    ROW_DIVIDERS = 898,
    ROW_FONT = 899,
    ROW_HEADINGS = 900,
    ROW_PROTECTION = 901,
    RUN = 902,
    S = 903,
    SAME = 904,
    SAVE_AS = 905,
    SAVE_AS_NO_PROMPT = 906,
    SCREEN = 907,
    SCREEN_CONTROL = 908,
    SCROLL = 909,
    SCROLL_BAR = 910,
    SD = 911,
    SEARCH = 912,
    SEARCH_OPTIONS = 913,
    SEARCH_TEXT = 914,
    SECONDS = 915,
    SECTION = 916,
    SECURE = 917,
    SEGMENT = 918,
    SEGMENT_LIMIT = 919,
    SELECT = 920,
    SELECTION_INDEX = 921,
    SELECTION_TEXT = 922,
    SELECT_ALL = 923,
    SELF_ACT = 924,
    SEMI_COLON = 925,
    SEND = 926,
    SENTENCE = 927,
    SEPARATE = 928,
    SEPARATION = 929,
    SEQUENCE = 930,
    SEQUENTIAL = 931,
    SET = 932,
    SEVENTY_EIGHT = 933,
    SHADING = 934,
    SHADOW = 935,
    SHARING = 936,
    SHORT_DATE = 937,
    SHOW_LINES = 938,
    SHOW_NONE = 939,
    SHOW_SEL_ALWAYS = 940,
    SIGN = 941,
    SIGNED = 942,
    SIGNED_INT = 943,
    SIGNED_LONG = 944,
    SIGNED_SHORT = 945,
    SIXTY_SIX = 946,
    SIZE = 947,
    SIZE_ERROR = 948,
    SMALL_FONT = 949,
    SORT = 950,
    SORT_MERGE = 951,
    SORT_ORDER = 952,
    SOURCE = 953,
    SOURCE_COMPUTER = 954,
    SPACE = 955,
    SPECIAL_NAMES = 956,
    SPINNER = 957,
    SQUARE = 958,
    STANDARD = 959,
    STANDARD_1 = 960,
    STANDARD_2 = 961,
    STANDARD_BINARY = 962,
    STANDARD_DECIMAL = 963,
    START = 964,
    START_X = 965,
    START_Y = 966,
    STATIC = 967,
    STATIC_LIST = 968,
    STATUS = 969,
    STATUS_BAR = 970,
    STATUS_TEXT = 971,
    STDCALL = 972,
    STEP = 973,
    STOP = 974,
    STRING = 975,
    STYLE = 976,
    SUB_QUEUE_1 = 977,
    SUB_QUEUE_2 = 978,
    SUB_QUEUE_3 = 979,
    SUBSTITUTE_FUNC = 980,
    SUBSTITUTE_CASE_FUNC = 981,
    SUBTRACT = 982,
    SUBWINDOW = 983,
    SUM = 984,
    SUPPRESS = 985,
    SYMBOLIC = 986,
    SYNCHRONIZED = 987,
    SYSTEM_DEFAULT = 988,
    SYSTEM_OFFSET = 989,
    TAB = 990,
    TAB_TO_ADD = 991,
    TAB_TO_DELETE = 992,
    TABLE = 993,
    TALLYING = 994,
    TEMPORARY = 995,
    TAPE = 996,
    TERMINAL = 997,
    TERMINATE = 998,
    TERMINATION_VALUE = 999,
    TEST = 1000,
    TEXT = 1001,
    THAN = 1002,
    THEN = 1003,
    THREAD = 1004,
    THREADS = 1005,
    THRU = 1006,
    THUMB_POSITION = 1007,
    TILED_HEADINGS = 1008,
    TIME = 1009,
    TIME_OUT = 1010,
    TIMES = 1011,
    TITLE = 1012,
    TITLE_POSITION = 1013,
    TO = 1014,
    TOK_AMPER = 1015,
    TOK_CLOSE_PAREN = 1016,
    TOK_COLON = 1017,
    TOK_DIV = 1018,
    TOK_DOT = 1019,
    TOK_EQUAL = 1020,
    TOK_EXTERN = 1021,
    TOK_FALSE = 1022,
    TOK_FILE = 1023,
    TOK_GREATER = 1024,
    TOK_INITIAL = 1025,
    TOK_LESS = 1026,
    TOK_MINUS = 1027,
    TOK_MUL = 1028,
    TOK_NULL = 1029,
    TOK_OVERFLOW = 1030,
    TOK_OPEN_PAREN = 1031,
    TOK_PLUS = 1032,
    TOK_TRUE = 1033,
    TOP = 1034,
    TOWARD_GREATER = 1035,
    TOWARD_LESSER = 1036,
    TRADITIONAL_FONT = 1037,
    TRAILING = 1038,
    TRAILING_SHIFT = 1039,
    TRANSFORM = 1040,
    TRANSPARENT = 1041,
    TREE_VIEW = 1042,
    TRIM_FUNC = 1043,
    TRUNCATION = 1044,
    TYPE = 1045,
    U = 1046,
    UNBOUNDED = 1047,
    UNDERLINE = 1048,
    UNFRAMED = 1049,
    UNIT = 1050,
    UNLOCK = 1051,
    UNSIGNED = 1052,
    UNSIGNED_INT = 1053,
    UNSIGNED_LONG = 1054,
    UNSIGNED_SHORT = 1055,
    UNSORTED = 1056,
    UNSTRING = 1057,
    UNTIL = 1058,
    UP = 1059,
    UPDATE = 1060,
    UPON = 1061,
    UPON_ARGUMENT_NUMBER = 1062,
    UPON_COMMAND_LINE = 1063,
    UPON_ENVIRONMENT_NAME = 1064,
    UPON_ENVIRONMENT_VALUE = 1065,
    UPPER = 1066,
    UPPER_CASE_FUNC = 1067,
    USAGE = 1068,
    USE = 1069,
    USE_ALT = 1070,
    USE_RETURN = 1071,
    USE_TAB = 1072,
    USER = 1073,
    USER_DEFAULT = 1074,
    USER_FUNCTION_NAME = 1075,
    USING = 1076,
    V = 1077,
    VALIDATE = 1078,
    VALUE = 1079,
    VALUE_FORMAT = 1080,
    VARIABLE = 1081,
    VARIANT = 1082,
    VARYING = 1083,
    VERTICAL = 1084,
    VERY_HEAVY = 1085,
    VIRTUAL_WIDTH = 1086,
    VPADDING = 1087,
    VSCROLL = 1088,
    VSCROLL_BAR = 1089,
    VSCROLL_POS = 1090,
    VTOP = 1091,
    WAIT = 1092,
    WEB_BROWSER = 1093,
    WHEN = 1094,
    WHEN_COMPILED_FUNC = 1095,
    WIDTH = 1096,
    WIDTH_IN_CELLS = 1097,
    WINDOW = 1098,
    WITH = 1099,
    WORD = 1100,
    LEVEL_NUMBER = 1101,
    WORDS = 1102,
    WORKING_STORAGE = 1103,
    WRAP = 1104,
    WRITE = 1105,
    X = 1106,
    Y = 1107,
    YYYYDDD = 1108,
    YYYYMMDD = 1109,
    ZERO = 1110,
    SHIFT_PREFER = 1111
  };
#endif
/* Tokens.  */
#define TOKEN_EOF 0
#define THREEDIMENSIONAL 258
#define ABSENT 259
#define ACCEPT 260
#define ACCESS 261
#define ACTIVEX 262
#define ACTION 263
#define ADD 264
#define ADDRESS 265
#define ADJUSTABLE_COLUMNS 266
#define ADVANCING 267
#define AFTER 268
#define ALIGNMENT 269
#define ALL 270
#define ALLOCATE 271
#define ALPHABET 272
#define ALPHABETIC 273
#define ALPHABETIC_LOWER 274
#define ALPHABETIC_UPPER 275
#define ALPHANUMERIC 276
#define ALPHANUMERIC_EDITED 277
#define ALSO 278
#define ALTER 279
#define ALTERNATE 280
#define AND 281
#define ANY 282
#define ARE 283
#define AREA 284
#define AREAS 285
#define ARGUMENT_NUMBER 286
#define ARGUMENT_VALUE 287
#define ARITHMETIC 288
#define AS 289
#define ASCENDING 290
#define ASCII 291
#define ASSIGN 292
#define AT 293
#define ATTRIBUTE 294
#define AUTO 295
#define AUTO_DECIMAL 296
#define AUTO_SPIN 297
#define AUTOMATIC 298
#define AWAY_FROM_ZERO 299
#define BACKGROUND_COLOR 300
#define BACKGROUND_HIGH 301
#define BACKGROUND_LOW 302
#define BACKGROUND_STANDARD 303
#define BAR 304
#define BASED 305
#define BEFORE 306
#define BELL 307
#define BINARY 308
#define BINARY_C_LONG 309
#define BINARY_CHAR 310
#define BINARY_DOUBLE 311
#define BINARY_LONG 312
#define BINARY_SEQUENTIAL 313
#define BINARY_SHORT 314
#define BIT 315
#define BITMAP 316
#define BITMAP_END 317
#define BITMAP_HANDLE 318
#define BITMAP_NUMBER 319
#define BITMAP_START 320
#define BITMAP_TIMER 321
#define BITMAP_TRAILING 322
#define BITMAP_TRANSPARENT_COLOR 323
#define BITMAP_WIDTH 324
#define BLANK 325
#define BLINK 326
#define BLOCK 327
#define BOTTOM 328
#define BOX 329
#define BOXED 330
#define BUSY 331
#define BUTTONS 332
#define BY 333
#define BYTE_LENGTH 334
#define CALENDAR_FONT 335
#define CALL 336
#define CANCEL 337
#define CANCEL_BUTTON 338
#define CAPACITY 339
#define CARD_PUNCH 340
#define CARD_READER 341
#define CASSETTE 342
#define CCOL 343
#define CD 344
#define CELL 345
#define CELL_COLOR 346
#define CELL_DATA 347
#define CELL_FONT 348
#define CELL_PROTECTION 349
#define CENTER 350
#define CENTERED_HEADINGS 351
#define CENTURY_DATE 352
#define CF 353
#define CH 354
#define CHAINING 355
#define CHARACTER 356
#define CHARACTERS 357
#define CHECK_BOX 358
#define CLASS 359
#define CLASSIFICATION 360
#define CLASS_NAME 361
#define CLEAR_SELECTION 362
#define CLINE 363
#define CLINES 364
#define CLOSE 365
#define COBOL 366
#define CODE 367
#define CODE_SET 368
#define COLLATING 369
#define COL 370
#define COLOR 371
#define COLORS 372
#define COLS 373
#define COLUMN 374
#define COLUMN_COLOR 375
#define COLUMN_DIVIDERS 376
#define COLUMN_FONT 377
#define COLUMN_HEADINGS 378
#define COLUMN_PROTECTION 379
#define COLUMNS 380
#define COMBO_BOX 381
#define COMMA 382
#define COMMAND_LINE 383
#define COMMA_DELIM 384
#define COMMIT 385
#define COMMON 386
#define COMMUNICATION 387
#define COMP 388
#define COMPUTE 389
#define COMP_1 390
#define COMP_2 391
#define COMP_3 392
#define COMP_4 393
#define COMP_5 394
#define COMP_6 395
#define COMP_X 396
#define CONCATENATE_FUNC 397
#define CONDITION 398
#define CONFIGURATION 399
#define CONSTANT 400
#define CONTAINS 401
#define CONTENT 402
#define CONTINUE 403
#define CONTROL 404
#define CONTROLS 405
#define CONVERSION 406
#define CONVERTING 407
#define COPY 408
#define COPY_SELECTION 409
#define CORRESPONDING 410
#define COUNT 411
#define CRT 412
#define CRT_UNDER 413
#define CSIZE 414
#define CURRENCY 415
#define CURRENT_DATE_FUNC 416
#define CURSOR 417
#define CURSOR_COL 418
#define CURSOR_COLOR 419
#define CURSOR_FRAME_WIDTH 420
#define CURSOR_ROW 421
#define CURSOR_X 422
#define CURSOR_Y 423
#define CUSTOM_PRINT_TEMPLATE 424
#define CYCLE 425
#define DASHED 426
#define DATA 427
#define DATA_COLUMNS 428
#define DATA_TYPES 429
#define DATE 430
#define DATE_ENTRY 431
#define DAY 432
#define DAY_OF_WEEK 433
#define DE 434
#define DEBUGGING 435
#define DECIMAL_POINT 436
#define DECLARATIVES 437
#define DEFAULT 438
#define DEFAULT_BUTTON 439
#define DEFAULT_FONT 440
#define DELETE 441
#define DELIMITED 442
#define DELIMITER 443
#define DEPENDING 444
#define DESCENDING 445
#define DESTINATION 446
#define DESTROY 447
#define DETAIL 448
#define DISABLE 449
#define DISC 450
#define DISK 451
#define DISPLAY 452
#define DISPLAY_COLUMNS 453
#define DISPLAY_FORMAT 454
#define DISPLAY_OF_FUNC 455
#define DIVIDE 456
#define DIVIDERS 457
#define DIVIDER_COLOR 458
#define DIVISION 459
#define DOTDASH 460
#define DOTTED 461
#define DRAG_COLOR 462
#define DROP_DOWN 463
#define DROP_LIST 464
#define DOWN 465
#define DUPLICATES 466
#define DYNAMIC 467
#define EBCDIC 468
#define EC 469
#define ECHO 470
#define EGI 471
#define EIGHTY_EIGHT 472
#define ENABLE 473
#define ELSE 474
#define EMI 475
#define END 476
#define END_ACCEPT 477
#define END_ADD 478
#define END_CALL 479
#define END_COMPUTE 480
#define END_COLOR 481
#define END_DELETE 482
#define END_DISPLAY 483
#define END_DIVIDE 484
#define END_EVALUATE 485
#define END_FUNCTION 486
#define END_IF 487
#define END_MODIFY 488
#define END_MULTIPLY 489
#define END_PERFORM 490
#define END_PROGRAM 491
#define END_READ 492
#define END_RECEIVE 493
#define END_RETURN 494
#define END_REWRITE 495
#define END_SEARCH 496
#define END_START 497
#define END_STRING 498
#define END_SUBTRACT 499
#define END_UNSTRING 500
#define END_WRITE 501
#define ENGRAVED 502
#define ENSURE_VISIBLE 503
#define ENTRY 504
#define ENTRY_CONVENTION 505
#define ENTRY_FIELD 506
#define ENTRY_REASON 507
#define ENVIRONMENT 508
#define ENVIRONMENT_NAME 509
#define ENVIRONMENT_VALUE 510
#define EOL 511
#define EOP 512
#define EOS 513
#define EQUAL 514
#define ERASE 515
#define ERROR 516
#define ESCAPE 517
#define ESCAPE_BUTTON 518
#define ESI 519
#define EVALUATE 520
#define EVENT 521
#define EVENT_LIST 522
#define EVENT_STATUS 523
#define EXCEPTION 524
#define EXCEPTION_CONDITION 525
#define EXCEPTION_VALUE 526
#define EXPAND 527
#define EXCLUSIVE 528
#define EXIT 529
#define EXPONENTIATION 530
#define EXTEND 531
#define EXTERNAL 532
#define EXTERNAL_FORM 533
#define F 534
#define FD 535
#define FILE_CONTROL 536
#define FILE_ID 537
#define FILE_NAME 538
#define FILE_POS 539
#define FILL_COLOR 540
#define FILL_COLOR2 541
#define FILL_PERCENT 542
#define FILLER 543
#define FINAL 544
#define FINISH_REASON 545
#define FIRST 546
#define FIXED 547
#define FIXED_FONT 548
#define FIXED_WIDTH 549
#define FLAT 550
#define FLAT_BUTTONS 551
#define FLOAT_BINARY_128 552
#define FLOAT_BINARY_32 553
#define FLOAT_BINARY_64 554
#define FLOAT_DECIMAL_16 555
#define FLOAT_DECIMAL_34 556
#define FLOAT_DECIMAL_7 557
#define FLOAT_EXTENDED 558
#define FLOAT_LONG 559
#define FLOAT_SHORT 560
#define FLOATING 561
#define FONT 562
#define FOOTING 563
#define FOR 564
#define FOREGROUND_COLOR 565
#define FOREVER 566
#define FORMATTED_DATE_FUNC 567
#define FORMATTED_DATETIME_FUNC 568
#define FORMATTED_TIME_FUNC 569
#define FRAME 570
#define FRAMED 571
#define FREE 572
#define FROM 573
#define FROM_CRT 574
#define FULL 575
#define FULL_HEIGHT 576
#define FUNCTION 577
#define FUNCTION_ID 578
#define FUNCTION_NAME 579
#define GENERATE 580
#define GIVING 581
#define GLOBAL 582
#define GO 583
#define GO_BACK 584
#define GO_FORWARD 585
#define GO_HOME 586
#define GO_SEARCH 587
#define GOBACK 588
#define GRAPHICAL 589
#define GREATER 590
#define GREATER_OR_EQUAL 591
#define GRID 592
#define GROUP 593
#define GROUP_VALUE 594
#define HANDLE 595
#define HAS_CHILDREN 596
#define HEADING 597
#define HEADING_COLOR 598
#define HEADING_DIVIDER_COLOR 599
#define HEADING_FONT 600
#define HEAVY 601
#define HEIGHT_IN_CELLS 602
#define HIDDEN_DATA 603
#define HIGHLIGHT 604
#define HIGH_COLOR 605
#define HIGH_VALUE 606
#define HOT_TRACK 607
#define HSCROLL 608
#define HSCROLL_POS 609
#define ICON 610
#define ID 611
#define IDENTIFIED 612
#define IDENTIFICATION 613
#define IF 614
#define IGNORE 615
#define IGNORING 616
#define IN 617
#define INDEPENDENT 618
#define INDEX 619
#define INDEXED 620
#define INDICATE 621
#define INITIALIZE 622
#define INITIALIZED 623
#define INITIATE 624
#define INPUT 625
#define INPUT_OUTPUT 626
#define INQUIRE 627
#define INSERTION_INDEX 628
#define INSERT_ROWS 629
#define INSPECT 630
#define INTERMEDIATE 631
#define INTO 632
#define INTRINSIC 633
#define INVALID 634
#define INVALID_KEY 635
#define IS 636
#define ITEM 637
#define ITEM_TEXT 638
#define ITEM_TO_ADD 639
#define ITEM_TO_DELETE 640
#define ITEM_TO_EMPTY 641
#define ITEM_VALUE 642
#define I_O 643
#define I_O_CONTROL 644
#define JUSTIFIED 645
#define KEPT 646
#define KEY 647
#define KEYBOARD 648
#define LABEL 649
#define LABEL_OFFSET 650
#define LARGE_FONT 651
#define LARGE_OFFSET 652
#define LAST 653
#define LAST_ROW 654
#define LAYOUT_DATA 655
#define LAYOUT_MANAGER 656
#define LEADING 657
#define LEADING_SHIFT 658
#define LEFT 659
#define LEFTLINE 660
#define LEFT_TEXT 661
#define LENGTH 662
#define LENGTH_FUNC 663
#define LENGTH_OF 664
#define LESS 665
#define LESS_OR_EQUAL 666
#define LIMIT 667
#define LIMITS 668
#define LINAGE 669
#define LINAGE_COUNTER 670
#define LINE 671
#define LINE_COUNTER 672
#define LINE_LIMIT 673
#define LINE_SEQUENTIAL 674
#define LINES 675
#define LINES_AT_ROOT 676
#define LINKAGE 677
#define LIST_BOX 678
#define LITERAL 679
#define LM_RESIZE 680
#define LOCALE 681
#define LOCALE_DATE_FUNC 682
#define LOCALE_TIME_FUNC 683
#define LOCALE_TIME_FROM_FUNC 684
#define LOCAL_STORAGE 685
#define LOCK 686
#define LONG_DATE 687
#define LOWER 688
#define LOWERED 689
#define LOWER_CASE_FUNC 690
#define LOWLIGHT 691
#define LOW_COLOR 692
#define LOW_VALUE 693
#define MAGNETIC_TAPE 694
#define MANUAL 695
#define MASS_UPDATE 696
#define MAX_LINES 697
#define MAX_PROGRESS 698
#define MAX_TEXT 699
#define MAX_VAL 700
#define MEMORY 701
#define MEDIUM_FONT 702
#define MENU 703
#define MERGE 704
#define MESSAGE 705
#define MINUS 706
#define MIN_VAL 707
#define MNEMONIC_NAME 708
#define MODE 709
#define MODIFY 710
#define MODULES 711
#define MOVE 712
#define MULTILINE 713
#define MULTIPLE 714
#define MULTIPLY 715
#define NAME 716
#define NATIONAL 717
#define NATIONAL_EDITED 718
#define NATIONAL_OF_FUNC 719
#define NATIVE 720
#define NAVIGATE_URL 721
#define NEAREST_AWAY_FROM_ZERO 722
#define NEAREST_EVEN 723
#define NEAREST_TOWARD_ZERO 724
#define NEGATIVE 725
#define NESTED 726
#define NEW 727
#define NEXT 728
#define NEXT_ITEM 729
#define NEXT_GROUP 730
#define NEXT_PAGE 731
#define NO 732
#define NO_ADVANCING 733
#define NO_AUTOSEL 734
#define NO_AUTO_DEFAULT 735
#define NO_BOX 736
#define NO_DATA 737
#define NO_DIVIDERS 738
#define NO_ECHO 739
#define NO_F4 740
#define NO_FOCUS 741
#define NO_GROUP_TAB 742
#define NO_KEY_LETTER 743
#define NO_SEARCH 744
#define NO_UPDOWN 745
#define NORMAL 746
#define NOT 747
#define NOTAB 748
#define NOTHING 749
#define NOTIFY 750
#define NOTIFY_CHANGE 751
#define NOTIFY_DBLCLICK 752
#define NOTIFY_SELCHANGE 753
#define NOT_END 754
#define NOT_EOP 755
#define NOT_ESCAPE 756
#define NOT_EQUAL 757
#define NOT_EXCEPTION 758
#define NOT_INVALID_KEY 759
#define NOT_OVERFLOW 760
#define NOT_SIZE_ERROR 761
#define NUM_COL_HEADINGS 762
#define NUM_ROWS 763
#define NUMBER 764
#define NUMBERS 765
#define NUMERIC 766
#define NUMERIC_EDITED 767
#define NUMVALC_FUNC 768
#define OBJECT 769
#define OBJECT_COMPUTER 770
#define OCCURS 771
#define OF 772
#define OFF 773
#define OK_BUTTON 774
#define OMITTED 775
#define ON 776
#define ONLY 777
#define OPEN 778
#define OPTIONAL 779
#define OPTIONS 780
#define OR 781
#define ORDER 782
#define ORGANIZATION 783
#define OTHER 784
#define OUTPUT 785
#define OVERLAP_LEFT 786
#define OVERLAP_TOP 787
#define OVERLINE 788
#define PACKED_DECIMAL 789
#define PADDING 790
#define PAGE 791
#define PAGE_COUNTER 792
#define PAGE_SETUP 793
#define PAGED 794
#define PARAGRAPH 795
#define PARENT 796
#define PASSWORD 797
#define PERFORM 798
#define PERMANENT 799
#define PH 800
#define PF 801
#define PHYSICAL 802
#define PICTURE 803
#define PICTURE_SYMBOL 804
#define PIXEL 805
#define PLACEMENT 806
#define PLUS 807
#define POINTER 808
#define POP_UP 809
#define POSITION 810
#define POSITION_SHIFT 811
#define POSITIVE 812
#define PRESENT 813
#define PREVIOUS 814
#define PRINT 815
#define PRINT_NO_PROMPT 816
#define PRINT_PREVIEW 817
#define PRINTER 818
#define PRINTER_1 819
#define PRINTING 820
#define PRIORITY 821
#define PROCEDURE 822
#define PROCEDURES 823
#define PROCEED 824
#define PROGRAM 825
#define PROGRAM_ID 826
#define PROGRAM_NAME 827
#define PROGRAM_POINTER 828
#define PROGRESS 829
#define PROHIBITED 830
#define PROMPT 831
#define PROPERTIES 832
#define PROPERTY 833
#define PROTECTED 834
#define PURGE 835
#define PUSH_BUTTON 836
#define QUERY_INDEX 837
#define QUEUE 838
#define QUOTE 839
#define RADIO_BUTTON 840
#define RAISED 841
#define RANDOM 842
#define RD 843
#define READ 844
#define READ_ONLY 845
#define READY_TRACE 846
#define RECEIVE 847
#define RECORD 848
#define RECORD_DATA 849
#define RECORD_TO_ADD 850
#define RECORD_TO_DELETE 851
#define RECORDING 852
#define RECORDS 853
#define RECURSIVE 854
#define REDEFINES 855
#define REEL 856
#define REFERENCE 857
#define REFERENCES 858
#define REFRESH 859
#define REGION_COLOR 860
#define RELATIVE 861
#define RELEASE 862
#define REMAINDER 863
#define REMOVAL 864
#define RENAMES 865
#define REPLACE 866
#define REPLACING 867
#define REPORT 868
#define REPORTING 869
#define REPORTS 870
#define REPOSITORY 871
#define REQUIRED 872
#define RESERVE 873
#define RESET 874
#define RESET_TRACE 875
#define RESET_GRID 876
#define RESET_LIST 877
#define RESET_TABS 878
#define RETRY 879
#define RETURN 880
#define RETURNING 881
#define REVERSE 882
#define REVERSE_FUNC 883
#define REVERSE_VIDEO 884
#define REVERSED 885
#define REWIND 886
#define REWRITE 887
#define RF 888
#define RH 889
#define RIGHT 890
#define RIGHT_ALIGN 891
#define RIMMED 892
#define ROLLBACK 893
#define ROUNDED 894
#define ROUNDING 895
#define ROW_COLOR 896
#define ROW_COLOR_PATTERN 897
#define ROW_DIVIDERS 898
#define ROW_FONT 899
#define ROW_HEADINGS 900
#define ROW_PROTECTION 901
#define RUN 902
#define S 903
#define SAME 904
#define SAVE_AS 905
#define SAVE_AS_NO_PROMPT 906
#define SCREEN 907
#define SCREEN_CONTROL 908
#define SCROLL 909
#define SCROLL_BAR 910
#define SD 911
#define SEARCH 912
#define SEARCH_OPTIONS 913
#define SEARCH_TEXT 914
#define SECONDS 915
#define SECTION 916
#define SECURE 917
#define SEGMENT 918
#define SEGMENT_LIMIT 919
#define SELECT 920
#define SELECTION_INDEX 921
#define SELECTION_TEXT 922
#define SELECT_ALL 923
#define SELF_ACT 924
#define SEMI_COLON 925
#define SEND 926
#define SENTENCE 927
#define SEPARATE 928
#define SEPARATION 929
#define SEQUENCE 930
#define SEQUENTIAL 931
#define SET 932
#define SEVENTY_EIGHT 933
#define SHADING 934
#define SHADOW 935
#define SHARING 936
#define SHORT_DATE 937
#define SHOW_LINES 938
#define SHOW_NONE 939
#define SHOW_SEL_ALWAYS 940
#define SIGN 941
#define SIGNED 942
#define SIGNED_INT 943
#define SIGNED_LONG 944
#define SIGNED_SHORT 945
#define SIXTY_SIX 946
#define SIZE 947
#define SIZE_ERROR 948
#define SMALL_FONT 949
#define SORT 950
#define SORT_MERGE 951
#define SORT_ORDER 952
#define SOURCE 953
#define SOURCE_COMPUTER 954
#define SPACE 955
#define SPECIAL_NAMES 956
#define SPINNER 957
#define SQUARE 958
#define STANDARD 959
#define STANDARD_1 960
#define STANDARD_2 961
#define STANDARD_BINARY 962
#define STANDARD_DECIMAL 963
#define START 964
#define START_X 965
#define START_Y 966
#define STATIC 967
#define STATIC_LIST 968
#define STATUS 969
#define STATUS_BAR 970
#define STATUS_TEXT 971
#define STDCALL 972
#define STEP 973
#define STOP 974
#define STRING 975
#define STYLE 976
#define SUB_QUEUE_1 977
#define SUB_QUEUE_2 978
#define SUB_QUEUE_3 979
#define SUBSTITUTE_FUNC 980
#define SUBSTITUTE_CASE_FUNC 981
#define SUBTRACT 982
#define SUBWINDOW 983
#define SUM 984
#define SUPPRESS 985
#define SYMBOLIC 986
#define SYNCHRONIZED 987
#define SYSTEM_DEFAULT 988
#define SYSTEM_OFFSET 989
#define TAB 990
#define TAB_TO_ADD 991
#define TAB_TO_DELETE 992
#define TABLE 993
#define TALLYING 994
#define TEMPORARY 995
#define TAPE 996
#define TERMINAL 997
#define TERMINATE 998
#define TERMINATION_VALUE 999
#define TEST 1000
#define TEXT 1001
#define THAN 1002
#define THEN 1003
#define THREAD 1004
#define THREADS 1005
#define THRU 1006
#define THUMB_POSITION 1007
#define TILED_HEADINGS 1008
#define TIME 1009
#define TIME_OUT 1010
#define TIMES 1011
#define TITLE 1012
#define TITLE_POSITION 1013
#define TO 1014
#define TOK_AMPER 1015
#define TOK_CLOSE_PAREN 1016
#define TOK_COLON 1017
#define TOK_DIV 1018
#define TOK_DOT 1019
#define TOK_EQUAL 1020
#define TOK_EXTERN 1021
#define TOK_FALSE 1022
#define TOK_FILE 1023
#define TOK_GREATER 1024
#define TOK_INITIAL 1025
#define TOK_LESS 1026
#define TOK_MINUS 1027
#define TOK_MUL 1028
#define TOK_NULL 1029
#define TOK_OVERFLOW 1030
#define TOK_OPEN_PAREN 1031
#define TOK_PLUS 1032
#define TOK_TRUE 1033
#define TOP 1034
#define TOWARD_GREATER 1035
#define TOWARD_LESSER 1036
#define TRADITIONAL_FONT 1037
#define TRAILING 1038
#define TRAILING_SHIFT 1039
#define TRANSFORM 1040
#define TRANSPARENT 1041
#define TREE_VIEW 1042
#define TRIM_FUNC 1043
#define TRUNCATION 1044
#define TYPE 1045
#define U 1046
#define UNBOUNDED 1047
#define UNDERLINE 1048
#define UNFRAMED 1049
#define UNIT 1050
#define UNLOCK 1051
#define UNSIGNED 1052
#define UNSIGNED_INT 1053
#define UNSIGNED_LONG 1054
#define UNSIGNED_SHORT 1055
#define UNSORTED 1056
#define UNSTRING 1057
#define UNTIL 1058
#define UP 1059
#define UPDATE 1060
#define UPON 1061
#define UPON_ARGUMENT_NUMBER 1062
#define UPON_COMMAND_LINE 1063
#define UPON_ENVIRONMENT_NAME 1064
#define UPON_ENVIRONMENT_VALUE 1065
#define UPPER 1066
#define UPPER_CASE_FUNC 1067
#define USAGE 1068
#define USE 1069
#define USE_ALT 1070
#define USE_RETURN 1071
#define USE_TAB 1072
#define USER 1073
#define USER_DEFAULT 1074
#define USER_FUNCTION_NAME 1075
#define USING 1076
#define V 1077
#define VALIDATE 1078
#define VALUE 1079
#define VALUE_FORMAT 1080
#define VARIABLE 1081
#define VARIANT 1082
#define VARYING 1083
#define VERTICAL 1084
#define VERY_HEAVY 1085
#define VIRTUAL_WIDTH 1086
#define VPADDING 1087
#define VSCROLL 1088
#define VSCROLL_BAR 1089
#define VSCROLL_POS 1090
#define VTOP 1091
#define WAIT 1092
#define WEB_BROWSER 1093
#define WHEN 1094
#define WHEN_COMPILED_FUNC 1095
#define WIDTH 1096
#define WIDTH_IN_CELLS 1097
#define WINDOW 1098
#define WITH 1099
#define WORD 1100
#define LEVEL_NUMBER 1101
#define WORDS 1102
#define WORKING_STORAGE 1103
#define WRAP 1104
#define WRITE 1105
#define X 1106
#define Y 1107
#define YYYYDDD 1108
#define YYYYMMDD 1109
#define ZERO 1110
#define SHIFT_PREFER 1111

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_PARSER_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 3684 "parser.c" /* yacc.c:358  */

#ifdef short
# undef short
#endif

#ifdef YYTYPE_UINT8
typedef YYTYPE_UINT8 yytype_uint8;
#else
typedef unsigned char yytype_uint8;
#endif

#ifdef YYTYPE_INT8
typedef YYTYPE_INT8 yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef YYTYPE_UINT16
typedef YYTYPE_UINT16 yytype_uint16;
#else
typedef unsigned short int yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short int yytype_int16;
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif ! defined YYSIZE_T
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned int
# endif
#endif

#define YYSIZE_MAXIMUM ((YYSIZE_T) -1)

#ifndef YY_
# if defined YYENABLE_NLS && YYENABLE_NLS
#  if ENABLE_NLS
#   include <libintl.h> /* INFRINGES ON USER NAME SPACE */
#   define YY_(Msgid) dgettext ("bison-runtime", Msgid)
#  endif
# endif
# ifndef YY_
#  define YY_(Msgid) Msgid
# endif
#endif

#ifndef YY_ATTRIBUTE
# if (defined __GNUC__                                               \
      && (2 < __GNUC__ || (__GNUC__ == 2 && 96 <= __GNUC_MINOR__)))  \
     || defined __SUNPRO_C && 0x5110 <= __SUNPRO_C
#  define YY_ATTRIBUTE(Spec) __attribute__(Spec)
# else
#  define YY_ATTRIBUTE(Spec) /* empty */
# endif
#endif

#ifndef YY_ATTRIBUTE_PURE
# define YY_ATTRIBUTE_PURE   YY_ATTRIBUTE ((__pure__))
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# define YY_ATTRIBUTE_UNUSED YY_ATTRIBUTE ((__unused__))
#endif

#if !defined _Noreturn \
     && (!defined __STDC_VERSION__ || __STDC_VERSION__ < 201112)
# if defined _MSC_VER && 1200 <= _MSC_VER
#  define _Noreturn __declspec (noreturn)
# else
#  define _Noreturn YY_ATTRIBUTE ((__noreturn__))
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN \
    _Pragma ("GCC diagnostic push") \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")\
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END \
    _Pragma ("GCC diagnostic pop")
#else
# define YY_INITIAL_VALUE(Value) Value
#endif
#ifndef YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
# define YY_IGNORE_MAYBE_UNINITIALIZED_END
#endif
#ifndef YY_INITIAL_VALUE
# define YY_INITIAL_VALUE(Value) /* Nothing. */
#endif


#if ! defined yyoverflow || YYERROR_VERBOSE

/* The parser invokes alloca or malloc; define the necessary symbols.  */

# ifdef YYSTACK_USE_ALLOCA
#  if YYSTACK_USE_ALLOCA
#   ifdef __GNUC__
#    define YYSTACK_ALLOC __builtin_alloca
#   elif defined __BUILTIN_VA_ARG_INCR
#    include <alloca.h> /* INFRINGES ON USER NAME SPACE */
#   elif defined _AIX
#    define YYSTACK_ALLOC __alloca
#   elif defined _MSC_VER
#    include <malloc.h> /* INFRINGES ON USER NAME SPACE */
#    define alloca _alloca
#   else
#    define YYSTACK_ALLOC alloca
#    if ! defined _ALLOCA_H && ! defined EXIT_SUCCESS
#     include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
      /* Use EXIT_SUCCESS as a witness for stdlib.h.  */
#     ifndef EXIT_SUCCESS
#      define EXIT_SUCCESS 0
#     endif
#    endif
#   endif
#  endif
# endif

# ifdef YYSTACK_ALLOC
   /* Pacify GCC's 'empty if-body' warning.  */
#  define YYSTACK_FREE(Ptr) do { /* empty */; } while (0)
#  ifndef YYSTACK_ALLOC_MAXIMUM
    /* The OS might guarantee only one guard page at the bottom of the stack,
       and a page size can be as small as 4096 bytes.  So we cannot safely
       invoke alloca (N) if N exceeds 4096.  Use a slightly smaller number
       to allow for a few compiler-allocated temporary stack slots.  */
#   define YYSTACK_ALLOC_MAXIMUM 4032 /* reasonable circa 2006 */
#  endif
# else
#  define YYSTACK_ALLOC YYMALLOC
#  define YYSTACK_FREE YYFREE
#  ifndef YYSTACK_ALLOC_MAXIMUM
#   define YYSTACK_ALLOC_MAXIMUM YYSIZE_MAXIMUM
#  endif
#  if (defined __cplusplus && ! defined EXIT_SUCCESS \
       && ! ((defined YYMALLOC || defined malloc) \
             && (defined YYFREE || defined free)))
#   include <stdlib.h> /* INFRINGES ON USER NAME SPACE */
#   ifndef EXIT_SUCCESS
#    define EXIT_SUCCESS 0
#   endif
#  endif
#  ifndef YYMALLOC
#   define YYMALLOC malloc
#   if ! defined malloc && ! defined EXIT_SUCCESS
void *malloc (YYSIZE_T); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
#  ifndef YYFREE
#   define YYFREE free
#   if ! defined free && ! defined EXIT_SUCCESS
void free (void *); /* INFRINGES ON USER NAME SPACE */
#   endif
#  endif
# endif
#endif /* ! defined yyoverflow || YYERROR_VERBOSE */


#if (! defined yyoverflow \
     && (! defined __cplusplus \
         || (defined YYSTYPE_IS_TRIVIAL && YYSTYPE_IS_TRIVIAL)))

/* A type that is properly aligned for any stack member.  */
union yyalloc
{
  yytype_int16 yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (sizeof (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (sizeof (yytype_int16) + sizeof (YYSTYPE)) \
      + YYSTACK_GAP_MAXIMUM)

# define YYCOPY_NEEDED 1

/* Relocate STACK from its old location to the new one.  The
   local variables YYSIZE and YYSTACKSIZE give the old and new number of
   elements in the stack, and YYPTR gives the new location of the
   stack.  Advance YYPTR to a properly aligned location for the next
   stack.  */
# define YYSTACK_RELOCATE(Stack_alloc, Stack)                           \
    do                                                                  \
      {                                                                 \
        YYSIZE_T yynewbytes;                                            \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * sizeof (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / sizeof (*yyptr);                          \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, (Count) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYSIZE_T yyi;                         \
          for (yyi = 0; yyi < (Count); yyi++)   \
            (Dst)[yyi] = (Src)[yyi];            \
        }                                       \
      while (0)
#  endif
# endif
#endif /* !YYCOPY_NEEDED */

/* YYFINAL -- State number of the termination state.  */
#define YYFINAL  3
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   16671

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  857
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  1134
/* YYNRULES -- Number of rules.  */
#define YYNRULES  2817
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  3932

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   1111

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint16 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     1,     2,     3,     4,
       5,     6,     7,     8,     9,    10,    11,    12,    13,    14,
      15,    16,    17,    18,    19,    20,    21,    22,    23,    24,
      25,    26,    27,    28,    29,    30,    31,    32,    33,    34,
      35,    36,    37,    38,    39,    40,    41,    42,    43,    44,
      45,    46,    47,    48,    49,    50,    51,    52,    53,    54,
      55,    56,    57,    58,    59,    60,    61,    62,    63,    64,
      65,    66,    67,    68,    69,    70,    71,    72,    73,    74,
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84,
      85,    86,    87,    88,    89,    90,    91,    92,    93,    94,
      95,    96,    97,    98,    99,   100,   101,   102,   103,   104,
     105,   106,   107,   108,   109,   110,   111,   112,   113,   114,
     115,   116,   117,   118,   119,   120,   121,   122,   123,   124,
     125,   126,   127,   128,   129,   130,   131,   132,   133,   134,
     135,   136,   137,   138,   139,   140,   141,   142,   143,   144,
     145,   146,   147,   148,   149,   150,   151,   152,   153,   154,
     155,   156,   157,   158,   159,   160,   161,   162,   163,   164,
     165,   166,   167,   168,   169,   170,   171,   172,   173,   174,
     175,   176,   177,   178,   179,   180,   181,   182,   183,   184,
     185,   186,   187,   188,   189,   190,   191,   192,   193,   194,
     195,   196,   197,   198,   199,   200,   201,   202,   203,   204,
     205,   206,   207,   208,   209,   210,   211,   212,   213,   214,
     215,   216,   217,   218,   219,   220,   221,   222,   223,   224,
     225,   226,   227,   228,   229,   230,   231,   232,   233,   234,
     235,   236,   237,   238,   239,   240,   241,   242,   243,   244,
     245,   246,   247,   248,   249,   250,   251,   252,   253,   254,
     255,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
     385,   386,   387,   388,   389,   390,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,   411,   412,   413,   414,
     415,   416,   417,   418,   419,   420,   421,   422,   423,   424,
     425,   426,   427,   428,   429,   430,   431,   432,   433,   434,
     435,   436,   437,   438,   439,   440,   441,   442,   443,   444,
     445,   446,   447,   448,   449,   450,   451,   452,   453,   454,
     455,   456,   457,   458,   459,   460,   461,   462,   463,   464,
     465,   466,   467,   468,   469,   470,   471,   472,   473,   474,
     475,   476,   477,   478,   479,   480,   481,   482,   483,   484,
     485,   486,   487,   488,   489,   490,   491,   492,   493,   494,
     495,   496,   497,   498,   499,   500,   501,   502,   503,   504,
     505,   506,   507,   508,   509,   510,   511,   512,   513,   514,
     515,   516,   517,   518,   519,   520,   521,   522,   523,   524,
     525,   526,   527,   528,   529,   530,   531,   532,   533,   534,
     535,   536,   537,   538,   539,   540,   541,   542,   543,   544,
     545,   546,   547,   548,   549,   550,   551,   552,   553,   554,
     555,   556,   557,   558,   559,   560,   561,   562,   563,   564,
     565,   566,   567,   568,   569,   570,   571,   572,   573,   574,
     575,   576,   577,   578,   579,   580,   581,   582,   583,   584,
     585,   586,   587,   588,   589,   590,   591,   592,   593,   594,
     595,   596,   597,   598,   599,   600,   601,   602,   603,   604,
     605,   606,   607,   608,   609,   610,   611,   612,   613,   614,
     615,   616,   617,   618,   619,   620,   621,   622,   623,   624,
     625,   626,   627,   628,   629,   630,   631,   632,   633,   634,
     635,   636,   637,   638,   639,   640,   641,   642,   643,   644,
     645,   646,   647,   648,   649,   650,   651,   652,   653,   654,
     655,   656,   657,   658,   659,   660,   661,   662,   663,   664,
     665,   666,   667,   668,   669,   670,   671,   672,   673,   674,
     675,   676,   677,   678,   679,   680,   681,   682,   683,   684,
     685,   686,   687,   688,   689,   690,   691,   692,   693,   694,
     695,   696,   697,   698,   699,   700,   701,   702,   703,   704,
     705,   706,   707,   708,   709,   710,   711,   712,   713,   714,
     715,   716,   717,   718,   719,   720,   721,   722,   723,   724,
     725,   726,   727,   728,   729,   730,   731,   732,   733,   734,
     735,   736,   737,   738,   739,   740,   741,   742,   743,   744,
     745,   746,   747,   748,   749,   750,   751,   752,   753,   754,
     755,   756,   757,   758,   759,   760,   761,   762,   763,   764,
     765,   766,   767,   768,   769,   770,   771,   772,   773,   774,
     775,   776,   777,   778,   779,   780,   781,   782,   783,   784,
     785,   786,   787,   788,   789,   790,   791,   792,   793,   794,
     795,   796,   797,   798,   799,   800,   801,   802,   803,   804,
     805,   806,   807,   808,   809,   810,   811,   812,   813,   814,
     815,   816,   817,   818,   819,   820,   821,   822,   823,   824,
     825,   826,   827,   828,   829,   830,   831,   832,   833,   834,
     835,   836,   837,   838,   839,   840,   841,   842,   843,   844,
     845,   846,   847,   848,   849,   850,   851,   852,   853,   854,
     855,   856
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,  2855,  2855,  2855,  2888,  2889,  2893,  2893,  2902,  2903,
    2907,  2908,  2912,  2912,  2938,  2950,  2959,  2963,  2967,  2968,
    2973,  2972,  2985,  2984,  2997,  3005,  3006,  3015,  3015,  3020,
    3024,  3019,  3040,  3039,  3055,  3066,  3073,  3074,  3081,  3082,
    3085,  3086,  3090,  3099,  3108,  3109,  3116,  3117,  3121,  3125,
    3131,  3133,  3141,  3148,  3150,  3154,  3161,  3165,  3169,  3185,
    3188,  3198,  3200,  3207,  3211,  3215,  3221,  3223,  3230,  3234,
    3238,  3242,  3251,  3256,  3257,  3266,  3270,  3271,  3281,  3283,
    3287,  3288,  3292,  3293,  3294,  3295,  3296,  3303,  3302,  3313,
    3314,  3317,  3318,  3331,  3330,  3344,  3345,  3346,  3347,  3351,
    3352,  3356,  3357,  3358,  3359,  3363,  3371,  3378,  3394,  3405,
    3409,  3413,  3417,  3424,  3425,  3432,  3431,  3444,  3446,  3447,
    3454,  3455,  3459,  3463,  3469,  3470,  3477,  3484,  3489,  3500,
    3514,  3517,  3518,  3522,  3523,  3524,  3525,  3526,  3527,  3528,
    3529,  3530,  3531,  3532,  3533,  3534,  3542,  3541,  3570,  3581,
    3600,  3608,  3611,  3612,  3616,  3623,  3638,  3659,  3658,  3682,
    3688,  3694,  3700,  3706,  3712,  3722,  3726,  3733,  3737,  3742,
    3741,  3752,  3756,  3763,  3764,  3765,  3766,  3767,  3768,  3772,
    3773,  3780,  3795,  3798,  3805,  3813,  3817,  3828,  3848,  3856,
    3867,  3868,  3875,  3889,  3890,  3894,  3915,  3936,  3937,  3941,
    3945,  3963,  3965,  3969,  3976,  3978,  3988,  4011,  4077,  4080,
    4089,  4108,  4124,  4142,  4160,  4177,  4194,  4204,  4205,  4212,
    4213,  4221,  4222,  4232,  4233,  4238,  4237,  4277,  4278,  4284,
    4285,  4294,  4295,  4296,  4297,  4298,  4299,  4300,  4301,  4302,
    4303,  4304,  4305,  4306,  4326,  4331,  4340,  4350,  4362,  4374,
    4406,  4407,  4408,  4413,  4414,  4415,  4416,  4420,  4421,  4422,
    4423,  4424,  4425,  4426,  4429,  4431,  4437,  4439,  4443,  4447,
    4448,  4453,  4456,  4457,  4464,  4471,  4472,  4473,  4480,  4527,
    4530,  4535,  4534,  4546,  4550,  4555,  4565,  4573,  4588,  4606,
    4608,  4609,  4615,  4615,  4622,  4626,  4630,  4636,  4637,  4638,
    4642,  4653,  4654,  4658,  4664,  4670,  4676,  4689,  4701,  4700,
    4709,  4721,  4734,  4746,  4763,  4790,  4793,  4800,  4801,  4805,
    4805,  4809,  4814,  4832,  4844,  4851,  4852,  4858,  4866,  4867,
    4868,  4874,  4875,  4876,  4883,  4884,  4888,  4889,  4895,  4923,
    4924,  4925,  4926,  4933,  4932,  4948,  4949,  4953,  4956,  4957,
    4967,  4964,  4981,  4982,  4990,  4991,  4999,  5000,  5004,  5024,
    5023,  5040,  5047,  5051,  5057,  5058,  5062,  5072,  5087,  5088,
    5089,  5090,  5091,  5092,  5093,  5094,  5095,  5102,  5109,  5109,
    5109,  5115,  5139,  5178,  5220,  5221,  5228,  5229,  5233,  5234,
    5241,  5252,  5257,  5268,  5269,  5273,  5274,  5280,  5291,  5309,
    5310,  5314,  5315,  5316,  5320,  5327,  5334,  5343,  5352,  5353,
    5354,  5355,  5356,  5365,  5366,  5372,  5409,  5410,  5423,  5438,
    5439,  5443,  5457,  5475,  5477,  5476,  5494,  5495,  5499,  5516,
    5515,  5536,  5537,  5541,  5542,  5543,  5546,  5548,  5549,  5553,
    5554,  5558,  5559,  5560,  5561,  5562,  5563,  5564,  5565,  5566,
    5567,  5568,  5572,  5576,  5578,  5582,  5583,  5587,  5588,  5589,
    5590,  5591,  5592,  5593,  5596,  5598,  5599,  5603,  5604,  5608,
    5609,  5610,  5611,  5612,  5613,  5617,  5622,  5624,  5623,  5639,
    5643,  5643,  5661,  5662,  5666,  5667,  5668,  5670,  5669,  5685,
    5702,  5708,  5710,  5714,  5721,  5725,  5736,  5739,  5751,  5752,
    5753,  5755,  5759,  5763,  5767,  5771,  5775,  5779,  5783,  5787,
    5791,  5799,  5803,  5807,  5811,  5815,  5819,  5830,  5831,  5835,
    5836,  5840,  5841,  5842,  5846,  5847,  5851,  5876,  5879,  5887,
    5886,  5899,  5927,  5926,  5941,  5945,  5952,  5958,  5962,  5969,
    5970,  5974,  5975,  5976,  5977,  5978,  5979,  5980,  5981,  5982,
    5983,  5986,  5988,  5992,  5996,  6000,  6001,  6002,  6003,  6004,
    6005,  6006,  6007,  6008,  6009,  6010,  6011,  6012,  6013,  6014,
    6021,  6042,  6070,  6073,  6081,  6082,  6086,  6111,  6122,  6123,
    6124,  6131,  6135,  6140,  6144,  6153,  6157,  6161,  6165,  6169,
    6173,  6177,  6181,  6185,  6189,  6193,  6198,  6203,  6207,  6211,
    6215,  6220,  6224,  6229,  6233,  6238,  6242,  6246,  6254,  6258,
    6262,  6270,  6274,  6278,  6282,  6286,  6290,  6294,  6298,  6302,
    6310,  6318,  6322,  6326,  6330,  6334,  6338,  6346,  6347,  6350,
    6352,  6353,  6354,  6355,  6356,  6357,  6360,  6362,  6368,  6375,
    6388,  6397,  6398,  6407,  6414,  6426,  6444,  6445,  6449,  6450,
    6454,  6455,  6458,  6459,  6464,  6465,  6472,  6473,  6479,  6481,
    6483,  6482,  6491,  6492,  6496,  6520,  6521,  6525,  6542,  6543,
    6546,  6548,  6551,  6558,  6559,  6564,  6575,  6586,  6593,  6595,
    6596,  6606,  6617,  6646,  6645,  6654,  6655,  6659,  6660,  6663,
    6665,  6677,  6686,  6701,  6724,  6743,  6745,  6744,  6764,  6766,
    6765,  6781,  6783,  6782,  6793,  6794,  6801,  6800,  6830,  6831,
    6832,  6839,  6845,  6850,  6851,  6857,  6864,  6865,  6866,  6870,
    6877,  6878,  6882,  6892,  6931,  6942,  6943,  6957,  6970,  6971,
    6974,  6975,  6980,  6981,  6982,  6983,  6984,  6985,  6997,  7011,
    7025,  7039,  7053,  7066,  7067,  7072,  7071,  7090,  7102,  7103,
    7107,  7108,  7109,  7110,  7111,  7112,  7113,  7114,  7115,  7116,
    7117,  7118,  7119,  7120,  7121,  7122,  7126,  7133,  7137,  7141,
    7142,  7143,  7150,  7154,  7162,  7165,  7173,  7183,  7184,  7189,
    7192,  7197,  7201,  7209,  7216,  7225,  7230,  7235,  7242,  7243,
    7244,  7248,  7256,  7257,  7258,  7265,  7269,  7276,  7281,  7287,
    7294,  7300,  7310,  7314,  7321,  7323,  7327,  7331,  7335,  7339,
    7346,  7354,  7357,  7359,  7363,  7369,  7373,  7388,  7406,  7424,
    7439,  7442,  7444,  7448,  7452,  7456,  7463,  7483,  7487,  7488,
    7492,  7524,  7532,  7541,  7543,  7542,  7565,  7566,  7570,  7571,
    7575,  7578,  7577,  7639,  7659,  7638,  7705,  7723,  7725,  7729,
    7734,  7739,  7743,  7747,  7752,  7757,  7762,  7767,  7776,  7780,
    7784,  7788,  7792,  7798,  7802,  7807,  7813,  7817,  7822,  7827,
    7832,  7837,  7842,  7847,  7856,  7860,  7864,  7869,  7873,  7877,
    7881,  7885,  7889,  7893,  7897,  7908,  7913,  7918,  7919,  7920,
    7921,  7922,  7923,  7924,  7925,  7926,  7935,  7940,  7951,  7952,
    7959,  7960,  7961,  7962,  7963,  7964,  7965,  7966,  7967,  7970,
    7973,  7974,  7975,  7976,  7977,  7978,  7985,  7986,  7991,  7992,
    7995,  7997,  8001,  8002,  8006,  8007,  8011,  8012,  8016,  8017,
    8021,  8022,  8023,  8024,  8025,  8028,  8029,  8030,  8031,  8032,
    8034,  8035,  8037,  8038,  8042,  8043,  8044,  8045,  8047,  8049,
    8051,  8052,  8053,  8054,  8055,  8056,  8057,  8058,  8059,  8065,
    8066,  8067,  8068,  8069,  8070,  8071,  8072,  8073,  8074,  8078,
    8079,  8084,  8085,  8086,  8087,  8088,  8092,  8100,  8101,  8102,
    8103,  8104,  8105,  8106,  8107,  8108,  8109,  8110,  8112,  8114,
    8115,  8116,  8120,  8121,  8122,  8123,  8124,  8125,  8126,  8127,
    8128,  8129,  8134,  8135,  8136,  8137,  8138,  8139,  8140,  8141,
    8142,  8143,  8148,  8149,  8160,  8161,  8185,  8186,  8203,  8206,
    8207,  8208,  8211,  8215,  8216,  8217,  8218,  8219,  8220,  8221,
    8222,  8223,  8224,  8225,  8226,  8227,  8228,  8229,  8230,  8236,
    8237,  8238,  8258,  8259,  8260,  8261,  8262,  8263,  8264,  8265,
    8269,  8270,  8271,  8272,  8273,  8274,  8280,  8281,  8282,  8283,
    8284,  8285,  8286,  8287,  8292,  8294,  8295,  8296,  8301,  8302,
    8303,  8307,  8308,  8309,  8310,  8311,  8312,  8323,  8324,  8325,
    8326,  8331,  8334,  8335,  8336,  8337,  8338,  8340,  8345,  8346,
    8347,  8353,  8354,  8355,  8356,  8357,  8358,  8359,  8360,  8361,
    8362,  8366,  8367,  8368,  8369,  8370,  8371,  8372,  8373,  8374,
    8375,  8376,  8377,  8378,  8379,  8381,  8382,  8383,  8384,  8385,
    8386,  8387,  8388,  8389,  8390,  8391,  8392,  8393,  8394,  8395,
    8396,  8397,  8400,  8401,  8402,  8410,  8411,  8412,  8416,  8417,
    8418,  8422,  8423,  8426,  8427,  8428,  8431,  8440,  8441,  8442,
    8443,  8444,  8445,  8446,  8447,  8448,  8449,  8450,  8451,  8452,
    8454,  8455,  8456,  8457,  8458,  8459,  8460,  8461,  8462,  8463,
    8470,  8474,  8478,  8479,  8480,  8481,  8482,  8483,  8484,  8485,
    8491,  8492,  8493,  8498,  8499,  8504,  8509,  8510,  8514,  8515,
    8520,  8521,  8525,  8526,  8527,  8532,  8533,  8537,  8538,  8542,
    8543,  8547,  8548,  8552,  8556,  8557,  8561,  8562,  8566,  8574,
    8576,  8580,  8587,  8597,  8600,  8604,  8611,  8623,  8633,  8643,
    8653,  8665,  8642,  8693,  8693,  8727,  8731,  8730,  8744,  8743,
    8763,  8764,  8769,  8791,  8793,  8797,  8808,  8810,  8818,  8826,
    8834,  8840,  8844,  8878,  8881,  8894,  8899,  8909,  8937,  8939,
    8938,  8975,  8976,  8980,  8981,  8982,  9000,  9001,  9013,  9012,
    9058,  9059,  9063,  9108,  9128,  9131,  9161,  9166,  9160,  9179,
    9179,  9216,  9223,  9224,  9225,  9226,  9227,  9228,  9229,  9230,
    9231,  9232,  9233,  9234,  9235,  9236,  9237,  9238,  9239,  9240,
    9241,  9242,  9243,  9244,  9245,  9246,  9247,  9248,  9249,  9250,
    9252,  9253,  9254,  9255,  9256,  9257,  9258,  9260,  9261,  9262,
    9263,  9264,  9266,  9267,  9268,  9269,  9270,  9271,  9272,  9273,
    9274,  9275,  9276,  9277,  9278,  9279,  9280,  9281,  9282,  9283,
    9284,  9299,  9311,  9310,  9321,  9320,  9355,  9354,  9365,  9369,
    9373,  9378,  9383,  9388,  9393,  9399,  9405,  9409,  9413,  9418,
    9422,  9426,  9430,  9434,  9438,  9442,  9446,  9453,  9454,  9460,
    9462,  9466,  9467,  9471,  9472,  9476,  9480,  9481,  9490,  9491,
    9496,  9497,  9501,  9502,  9506,  9522,  9538,  9551,  9555,  9556,
    9560,  9567,  9573,  9579,  9584,  9589,  9594,  9599,  9604,  9610,
    9616,  9622,  9629,  9633,  9637,  9641,  9645,  9656,  9661,  9666,
    9671,  9676,  9681,  9687,  9692,  9697,  9703,  9709,  9715,  9721,
    9726,  9731,  9738,  9745,  9751,  9754,  9754,  9758,  9769,  9770,
    9771,  9775,  9776,  9777,  9781,  9782,  9786,  9790,  9809,  9808,
    9817,  9821,  9828,  9832,  9840,  9841,  9845,  9849,  9860,  9859,
    9869,  9873,  9885,  9886,  9894,  9893,  9902,  9903,  9907,  9913,
    9913,  9920,  9919,  9936,  9935,  9991,  9994, 10002, 10006, 10010,
   10029, 10035, 10055, 10059, 10069, 10073, 10078, 10082, 10081, 10098,
   10099, 10104, 10112, 10141, 10143, 10147, 10156, 10169, 10172, 10176,
   10180, 10185, 10208, 10209, 10213, 10214, 10218, 10222, 10226, 10237,
   10241, 10248, 10252, 10260, 10264, 10271, 10278, 10282, 10293, 10292,
   10304, 10308, 10315, 10316, 10326, 10325, 10333, 10334, 10338, 10343,
   10351, 10352, 10353, 10354, 10355, 10360, 10359, 10371, 10372, 10380,
   10379, 10388, 10395, 10399, 10409, 10420, 10438, 10437, 10446, 10453,
   10464, 10463, 10472, 10476, 10480, 10485, 10493, 10497, 10508, 10507,
   10516, 10519, 10521, 10527, 10529, 10530, 10531, 10532, 10540, 10539,
   10551, 10555, 10559, 10563, 10567, 10568, 10569, 10570, 10571, 10575,
   10583, 10592, 10593, 10598, 10597, 10641, 10645, 10652, 10653, 10657,
   10661, 10666, 10670, 10671, 10675, 10679, 10683, 10687, 10694, 10695,
   10700, 10699, 10717, 10719, 10723, 10724, 10728, 10732, 10733, 10734,
   10735, 10740, 10745, 10739, 10759, 10760, 10765, 10770, 10764, 10789,
   10788, 10809, 10810, 10811, 10815, 10816, 10821, 10824, 10831, 10844,
   10856, 10863, 10864, 10870, 10871, 10875, 10876, 10877, 10878, 10879,
   10883, 10884, 10888, 10889, 10894, 10895, 10899, 10909, 10925, 10930,
   10936, 10942, 10947, 10952, 10958, 10964, 10970, 10976, 10983, 10987,
   10991, 10995, 10999, 11004, 11009, 11014, 11019, 11025, 11030, 11035,
   11042, 11052, 11056, 11067, 11066, 11075, 11079, 11083, 11087, 11091,
   11098, 11102, 11113, 11112, 11124, 11123, 11133, 11152, 11151, 11178,
   11186, 11187, 11192, 11203, 11214, 11228, 11232, 11240, 11241, 11246,
   11255, 11264, 11269, 11278, 11279, 11284, 11359, 11360, 11361, 11365,
   11366, 11370, 11374, 11385, 11384, 11396, 11400, 11425, 11439, 11462,
   11485, 11506, 11530, 11533, 11541, 11540, 11549, 11560, 11559, 11568,
   11581, 11580, 11593, 11602, 11606, 11617, 11637, 11636, 11645, 11649,
   11655, 11662, 11665, 11672, 11678, 11684, 11689, 11701, 11700, 11708,
   11716, 11717, 11721, 11722, 11723, 11728, 11731, 11738, 11742, 11750,
   11757, 11758, 11759, 11760, 11761, 11762, 11763, 11775, 11778, 11788,
   11787, 11795, 11802, 11815, 11814, 11826, 11827, 11834, 11833, 11842,
   11846, 11847, 11848, 11852, 11853, 11854, 11855, 11862, 11861, 11882,
   11892, 11901, 11905, 11912, 11917, 11922, 11927, 11932, 11937, 11945,
   11946, 11950, 11955, 11961, 11963, 11964, 11965, 11966, 11970, 11998,
   12001, 12005, 12009, 12013, 12020, 12027, 12037, 12036, 12049, 12048,
   12061, 12062, 12066, 12070, 12081, 12080, 12088, 12092, 12103, 12102,
   12111, 12115, 12122, 12126, 12137, 12136, 12144, 12145, 12149, 12174,
   12175, 12176, 12177, 12181, 12182, 12186, 12187, 12188, 12189, 12201,
   12200, 12212, 12224, 12221, 12235, 12247, 12255, 12262, 12266, 12279,
   12286, 12298, 12301, 12306, 12310, 12323, 12330, 12331, 12335, 12336,
   12339, 12340, 12345, 12388, 12392, 12402, 12401, 12414, 12413, 12423,
   12452, 12453, 12457, 12461, 12465, 12469, 12476, 12477, 12481, 12485,
   12488, 12490, 12494, 12503, 12504, 12505, 12508, 12510, 12514, 12518,
   12522, 12530, 12531, 12535, 12536, 12540, 12544, 12554, 12565, 12564,
   12573, 12578, 12579, 12583, 12584, 12585, 12589, 12590, 12594, 12598,
   12599, 12603, 12607, 12611, 12621, 12620, 12628, 12638, 12649, 12648,
   12657, 12664, 12668, 12679, 12678, 12690, 12699, 12702, 12706, 12710,
   12717, 12721, 12731, 12743, 12742, 12751, 12755, 12764, 12765, 12770,
   12773, 12781, 12785, 12792, 12800, 12804, 12815, 12814, 12822, 12825,
   12830, 12832, 12836, 12842, 12843, 12844, 12845, 12848, 12850, 12857,
   12856, 12870, 12871, 12872, 12873, 12874, 12875, 12876, 12877, 12881,
   12882, 12886, 12887, 12893, 12902, 12909, 12910, 12914, 12918, 12922,
   12928, 12934, 12938, 12942, 12946, 12955, 12959, 12963, 12972, 12981,
   12982, 12986, 12995, 12996, 13000, 13004, 13013, 13022, 13034, 13033,
   13042, 13041, 13073, 13076, 13096, 13097, 13100, 13101, 13109, 13110,
   13115, 13120, 13130, 13147, 13152, 13162, 13180, 13179, 13189, 13202,
   13205, 13213, 13216, 13221, 13226, 13234, 13235, 13236, 13237, 13238,
   13239, 13243, 13251, 13252, 13256, 13260, 13271, 13270, 13281, 13289,
   13300, 13307, 13311, 13315, 13323, 13335, 13338, 13345, 13349, 13356,
   13357, 13358, 13359, 13366, 13365, 13374, 13381, 13381, 13391, 13392,
   13396, 13410, 13411, 13416, 13417, 13421, 13422, 13426, 13430, 13441,
   13440, 13449, 13453, 13457, 13461, 13469, 13473, 13483, 13494, 13495,
   13502, 13501, 13509, 13516, 13529, 13528, 13536, 13550, 13549, 13557,
   13574, 13573, 13583, 13591, 13592, 13597, 13598, 13603, 13610, 13611,
   13616, 13623, 13624, 13628, 13629, 13633, 13634, 13638, 13642, 13652,
   13651, 13666, 13671, 13683, 13682, 13691, 13692, 13693, 13694, 13695,
   13699, 13726, 13729, 13741, 13751, 13756, 13761, 13766, 13774, 13814,
   13815, 13819, 13862, 13872, 13895, 13896, 13897, 13898, 13902, 13911,
   13918, 13929, 13960, 13969, 13970, 13977, 13976, 13988, 13998, 13999,
   14004, 14007, 14011, 14015, 14022, 14023, 14027, 14028, 14029, 14033,
   14037, 14049, 14050, 14051, 14061, 14065, 14072, 14080, 14081, 14085,
   14086, 14090, 14098, 14099, 14104, 14105, 14106, 14116, 14120, 14127,
   14135, 14136, 14140, 14150, 14151, 14152, 14162, 14166, 14173, 14181,
   14182, 14186, 14196, 14197, 14198, 14208, 14212, 14219, 14227, 14228,
   14232, 14243, 14244, 14251, 14253, 14262, 14266, 14273, 14281, 14282,
   14286, 14296, 14297, 14307, 14311, 14318, 14326, 14327, 14331, 14341,
   14342, 14346, 14347, 14357, 14361, 14368, 14376, 14377, 14381, 14392,
   14395, 14404, 14407, 14415, 14419, 14428, 14432, 14442, 14450, 14457,
   14457, 14468, 14469, 14473, 14474, 14476, 14478, 14480, 14481, 14483,
   14484, 14485, 14486, 14487, 14489, 14490, 14491, 14494, 14496, 14500,
   14503, 14505, 14506, 14507, 14508, 14509, 14510, 14512, 14513, 14514,
   14515, 14516, 14519, 14520, 14524, 14525, 14529, 14530, 14534, 14535,
   14539, 14543, 14549, 14553, 14559, 14561, 14562, 14566, 14567, 14568,
   14572, 14573, 14574, 14578, 14582, 14586, 14587, 14588, 14591, 14592,
   14602, 14614, 14623, 14639, 14648, 14664, 14679, 14680, 14685, 14694,
   14700, 14710, 14724, 14746, 14750, 14771, 14783, 14797, 14811, 14812,
   14817, 14823, 14824, 14829, 14843, 14844, 14845, 14852, 14863, 14864,
   14868, 14876, 14877, 14881, 14893, 14897, 14904, 14913, 14914, 14920,
   14929, 14940, 14957, 14961, 14968, 14969, 14970, 14977, 14978, 14982,
   14986, 14993, 14994, 14998, 14999, 15003, 15004, 15005, 15006, 15010,
   15014, 15018, 15022, 15026, 15047, 15051, 15058, 15059, 15060, 15064,
   15065, 15066, 15067, 15068, 15072, 15076, 15083, 15084, 15085, 15086,
   15090, 15094, 15101, 15113, 15127, 15128, 15132, 15133, 15137, 15144,
   15151, 15152, 15153, 15157, 15158, 15162, 15166, 15170, 15174, 15175,
   15179, 15183, 15184, 15188, 15192, 15193, 15202, 15206, 15211, 15212,
   15218, 15222, 15226, 15230, 15231, 15237, 15241, 15245, 15246, 15250,
   15257, 15267, 15286, 15304, 15311, 15318, 15325, 15335, 15339, 15346,
   15350, 15357, 15367, 15377, 15387, 15400, 15404, 15412, 15420, 15424,
   15434, 15449, 15473, 15495, 15508, 15515, 15531, 15532, 15533, 15534,
   15535, 15536, 15540, 15544, 15561, 15565, 15572, 15573, 15574, 15575,
   15576, 15577, 15578, 15584, 15588, 15592, 15596, 15600, 15604, 15609,
   15613, 15617, 15621, 15625, 15629, 15633, 15637, 15644, 15645, 15649,
   15650, 15651, 15655, 15656, 15657, 15658, 15662, 15666, 15670, 15677,
   15681, 15685, 15692, 15699, 15706, 15716, 15716, 15732, 15739, 15749,
   15756, 15766, 15770, 15783, 15787, 15802, 15810, 15811, 15815, 15816,
   15820, 15821, 15826, 15829, 15837, 15840, 15847, 15849, 15850, 15854,
   15855, 15859, 15860, 15861, 15866, 15869, 15882, 15886, 15894, 15898,
   15902, 15906, 15910, 15914, 15918, 15922, 15929, 15930, 15934, 15935,
   15945, 15946, 15955, 15959, 15963, 15967, 15974, 15975, 15976, 15977,
   15978, 15979, 15980, 15981, 15982, 15983, 15984, 15985, 15986, 15987,
   15988, 15989, 15990, 15991, 15992, 15993, 15994, 15995, 15996, 15997,
   15998, 15999, 16000, 16001, 16002, 16003, 16004, 16005, 16006, 16007,
   16008, 16009, 16010, 16011, 16012, 16013, 16014, 16015, 16016, 16017,
   16018, 16019, 16020, 16021, 16022, 16023, 16027, 16028, 16029, 16030,
   16031, 16032, 16033, 16034, 16035, 16036, 16037, 16038, 16039, 16040,
   16041, 16042, 16043, 16044, 16045, 16046, 16047, 16048, 16055, 16055,
   16056, 16056, 16057, 16057, 16058, 16058, 16059, 16059, 16059, 16060,
   16060, 16061, 16061, 16062, 16062, 16063, 16063, 16064, 16064, 16065,
   16065, 16066, 16066, 16067, 16067, 16068, 16068, 16069, 16069, 16070,
   16070, 16071, 16071, 16072, 16072, 16073, 16073, 16074, 16074, 16075,
   16075, 16076, 16076, 16077, 16077, 16077, 16078, 16078, 16078, 16079,
   16079, 16080, 16080, 16081, 16081, 16082, 16082, 16083, 16083, 16084,
   16084, 16084, 16085, 16085, 16085, 16086, 16086, 16086, 16086, 16087,
   16087, 16087, 16088, 16088, 16089, 16089, 16090, 16090, 16090, 16091,
   16091, 16091, 16092, 16092, 16093, 16093, 16094, 16094, 16095, 16095,
   16096, 16096, 16097, 16097, 16097, 16098, 16098, 16098, 16099, 16099,
   16100, 16100, 16101, 16101, 16101, 16102, 16102, 16102, 16102, 16103,
   16103, 16104, 16104, 16105, 16105, 16106, 16106, 16107, 16107, 16108,
   16108, 16108, 16109, 16109, 16110, 16110, 16111, 16111, 16112, 16112,
   16112, 16113, 16113, 16114, 16114, 16115, 16115, 16116, 16116, 16117,
   16117, 16118, 16118, 16119, 16119, 16120, 16120, 16121, 16121, 16122,
   16122, 16122, 16123, 16123, 16124, 16124, 16125, 16125, 16129, 16129,
   16130, 16130, 16131, 16131, 16132, 16132, 16133, 16133, 16134, 16134,
   16135, 16135, 16136, 16136, 16137, 16137, 16138, 16138, 16139, 16139,
   16139, 16140, 16140, 16141, 16141, 16142, 16142, 16143, 16143, 16146,
   16147, 16148, 16152, 16152, 16153, 16153, 16154, 16154, 16155, 16155,
   16156, 16156, 16157, 16157, 16158, 16158, 16159, 16159
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "$undefined", "\"3D\"", "ABSENT", "ACCEPT",
  "ACCESS", "\"ACTIVE-X\"", "ACTION", "ADD", "ADDRESS",
  "\"ADJUSTABLE-COLUMNS\"", "ADVANCING", "AFTER", "ALIGNMENT", "ALL",
  "ALLOCATE", "ALPHABET", "ALPHABETIC", "\"ALPHABETIC-LOWER\"",
  "\"ALPHABETIC-UPPER\"", "ALPHANUMERIC", "\"ALPHANUMERIC-EDITED\"",
  "ALSO", "ALTER", "ALTERNATE", "AND", "ANY", "ARE", "AREA", "AREAS",
  "\"ARGUMENT-NUMBER\"", "\"ARGUMENT-VALUE\"", "ARITHMETIC", "AS",
  "ASCENDING", "ASCII", "ASSIGN", "AT", "ATTRIBUTE", "AUTO",
  "\"AUTO-DECIMAL\"", "\"AUTO-SPIN\"", "AUTOMATIC", "\"AWAY-FROM-ZERO\"",
  "\"BACKGROUND-COLOR\"", "\"BACKGROUND-HIGH\"", "\"BACKGROUND-LOW\"",
  "\"BACKGROUND-STANDARD\"", "BAR", "BASED", "BEFORE", "BELL", "BINARY",
  "\"BINARY-C-LONG\"", "\"BINARY-CHAR\"", "\"BINARY-DOUBLE\"",
  "\"BINARY-LONG\"", "\"BINARY-SEQUENTIAL\"", "\"BINARY-SHORT\"", "BIT",
  "BITMAP", "\"BITMAP-END\"", "\"BITMAP-HANDLE\"", "\"BITMAP-NUMBER\"",
  "\"BITMAP-START\"", "\"BITMAP-TIMER\"", "\"BITMAP-TRAILING\"",
  "\"BITMAP-TRANSPARENT-COLOR\"", "\"BITMAP-WIDTH\"", "BLANK", "BLINK",
  "BLOCK", "BOTTOM", "BOX", "BOXED", "BUSY", "BUTTONS", "BY",
  "\"BYTE-LENGTH\"", "\"CALENDAR-FONT\"", "CALL", "CANCEL",
  "\"CANCEL-BUTTON\"", "CAPACITY", "\"CARD-PUNCH\"", "\"CARD-READER\"",
  "CASSETTE", "CCOL", "CD", "CELL", "\"CELL-COLOR\"", "\"CELL-DATA\"",
  "\"CELL-FONT\"", "\"CELL-PROTECTION\"", "CENTER",
  "\"CENTERED-HEADINGS\"", "\"CENTURY-DATE\"", "CF", "CH", "CHAINING",
  "CHARACTER", "CHARACTERS", "\"CHECK-BOX\"", "CLASS", "CLASSIFICATION",
  "\"class-name\"", "\"CLEAR-SELECTION\"", "CLINE", "CLINES", "CLOSE",
  "COBOL", "CODE", "\"CODE-SET\"", "COLLATING", "COL", "COLOR", "COLORS",
  "COLS", "COLUMN", "\"COLUMN-COLOR\"", "\"COLUMN-DIVIDERS\"",
  "\"COLUMN-FONT\"", "\"COLUMN-HEADINGS\"", "\"COLUMN-PROTECTION\"",
  "COLUMNS", "\"COMBO-BOX\"", "COMMA", "\"COMMAND-LINE\"",
  "\"comma delimiter\"", "COMMIT", "COMMON", "COMMUNICATION", "COMP",
  "COMPUTE", "\"COMP-1\"", "\"COMP-2\"", "\"COMP-3\"", "\"COMP-4\"",
  "\"COMP-5\"", "\"COMP-6\"", "\"COMP-X\"", "\"FUNCTION CONCATENATE\"",
  "CONDITION", "CONFIGURATION", "CONSTANT", "CONTAINS", "CONTENT",
  "CONTINUE", "CONTROL", "CONTROLS", "CONVERSION", "CONVERTING", "COPY",
  "\"COPY-SELECTION\"", "CORRESPONDING", "COUNT", "CRT", "\"CRT-UNDER\"",
  "CSIZE", "CURRENCY", "\"FUNCTION CURRENT-DATE\"", "CURSOR",
  "\"CURSOR-COL\"", "\"CURSOR-COLOR\"", "\"CURSOR-FRAME-WIDTH\"",
  "\"CURSOR-ROW\"", "\"CURSOR-X\"", "\"CURSOR-Y\"",
  "\"CUSTOM-PRINT-TEMPLATE\"", "CYCLE", "DASHED", "DATA",
  "\"DATA-COLUMNS\"", "\"DATA-TYPES\"", "DATE", "\"DATE-ENTRY\"", "DAY",
  "\"DAY-OF-WEEK\"", "DE", "DEBUGGING", "\"DECIMAL-POINT\"",
  "DECLARATIVES", "DEFAULT", "\"DEFAULT-BUTTON\"", "\"DEFAULT-FONT\"",
  "DELETE", "DELIMITED", "DELIMITER", "DEPENDING", "DESCENDING",
  "DESTINATION", "DESTROY", "DETAIL", "DISABLE", "DISC", "DISK", "DISPLAY",
  "\"DISPLAY-COLUMNS\"", "\"DISPLAY-FORMAT\"", "\"FUNCTION DISPLAY-OF\"",
  "DIVIDE", "DIVIDERS", "\"DIVIDER-COLOR\"", "DIVISION", "DOTDASH",
  "DOTTED", "\"DRAG-COLOR\"", "\"DROP-DOWN\"", "\"DROP-LIST\"", "DOWN",
  "DUPLICATES", "DYNAMIC", "EBCDIC", "EC", "ECHO", "EGI",
  "\"level-number 88\"", "ENABLE", "ELSE", "EMI", "END", "\"END-ACCEPT\"",
  "\"END-ADD\"", "\"END-CALL\"", "\"END-COMPUTE\"", "\"END-COLOR\"",
  "\"END-DELETE\"", "\"END-DISPLAY\"", "\"END-DIVIDE\"",
  "\"END-EVALUATE\"", "\"END FUNCTION\"", "\"END-IF\"", "\"END-MODIFY\"",
  "\"END-MULTIPLY\"", "\"END-PERFORM\"", "\"END PROGRAM\"", "\"END-READ\"",
  "\"END-RECEIVE\"", "\"END-RETURN\"", "\"END-REWRITE\"", "\"END-SEARCH\"",
  "\"END-START\"", "\"END-STRING\"", "\"END-SUBTRACT\"",
  "\"END-UNSTRING\"", "\"END-WRITE\"", "ENGRAVED", "\"ENSURE-VISIBLE\"",
  "ENTRY", "\"ENTRY-CONVENTION\"", "\"ENTRY-FIELD\"", "\"ENTRY-REASON\"",
  "ENVIRONMENT", "\"ENVIRONMENT-NAME\"", "\"ENVIRONMENT-VALUE\"", "EOL",
  "EOP", "EOS", "EQUAL", "ERASE", "ERROR", "ESCAPE", "\"ESCAPE-BUTTON\"",
  "ESI", "EVALUATE", "EVENT", "\"EVENT-LIST\"", "\"EVENT STATUS\"",
  "EXCEPTION", "\"EXCEPTION CONDITION\"", "\"EXCEPTION-VALUE\"", "EXPAND",
  "EXCLUSIVE", "EXIT", "\"exponentiation operator\"", "EXTEND", "EXTERNAL",
  "\"EXTERNAL-FORM\"", "F", "FD", "\"FILE-CONTROL\"", "\"FILE-ID\"",
  "\"FILE-NAME\"", "\"FILE-POS\"", "\"FILL-COLOR\"", "\"FILL-COLOR2\"",
  "\"FILL-PERCENT\"", "FILLER", "FINAL", "\"FINISH-REASON\"", "FIRST",
  "FIXED", "\"FIXED-FONT\"", "\"FIXED-WIDTH\"", "FLAT", "\"FLAT-BUTTONS\"",
  "\"FLOAT-BINARY-128\"", "\"FLOAT-BINARY-32\"", "\"FLOAT-BINARY-64\"",
  "\"FLOAT-DECIMAL-16\"", "\"FLOAT-DECIMAL-34\"", "\"FLOAT-DECIMAL-7\"",
  "\"FLOAT-EXTENDED\"", "\"FLOAT-LONG\"", "\"FLOAT-SHORT\"", "FLOATING",
  "FONT", "FOOTING", "FOR", "\"FOREGROUND-COLOR\"", "FOREVER",
  "\"FUNCTION FORMATTED-DATE\"", "\"FUNCTION FORMATTED-DATETIME\"",
  "\"FUNCTION FORMATTED-TIME\"", "FRAME", "FRAMED", "FREE", "FROM",
  "\"FROM CRT\"", "FULL", "\"FULL-HEIGHT\"", "FUNCTION", "\"FUNCTION-ID\"",
  "\"intrinsic function name\"", "GENERATE", "GIVING", "GLOBAL", "GO",
  "\"GO-BACK\"", "\"GO-FORWARD\"", "\"GO-HOME\"", "\"GO-SEARCH\"",
  "GOBACK", "GRAPHICAL", "GREATER", "\"GREATER OR EQUAL\"", "GRID",
  "GROUP", "\"GROUP-VALUE\"", "HANDLE", "\"HAS-CHILDREN\"", "HEADING",
  "\"HEADING-COLOR\"", "\"HEADING-DIVIDER-COLOR\"", "\"HEADING-FONT\"",
  "HEAVY", "\"HEIGHT-IN-CELLS\"", "\"HIDDEN-DATA\"", "HIGHLIGHT",
  "\"HIGH-COLOR\"", "\"HIGH-VALUE\"", "\"HOT-TRACK\"", "HSCROLL",
  "\"HSCROLL-POS\"", "ICON", "ID", "IDENTIFIED", "IDENTIFICATION", "IF",
  "IGNORE", "IGNORING", "IN", "INDEPENDENT", "INDEX", "INDEXED",
  "INDICATE", "INITIALIZE", "INITIALIZED", "INITIATE", "INPUT",
  "\"INPUT-OUTPUT\"", "INQUIRE", "\"INSERTION-INDEX\"", "\"INSERT-ROWS\"",
  "INSPECT", "INTERMEDIATE", "INTO", "INTRINSIC", "INVALID",
  "\"INVALID KEY\"", "IS", "ITEM", "\"ITEM-TEXT\"", "\"ITEM-TO_ADD\"",
  "\"ITEM-TO_DELETE\"", "\"ITEM-TO_EMPTY\"", "\"ITEM-VALUE\"", "\"I-O\"",
  "\"I-O-CONTROL\"", "JUSTIFIED", "KEPT", "KEY", "KEYBOARD", "LABEL",
  "\"LABEL-OFFSET\"", "\"LARGE-FONT\"", "\"LARGE-OFFSET\"", "LAST",
  "\"LAST-ROW\"", "\"LAYOUT-DATA\"", "\"LAYOUT-MANAGER\"", "LEADING",
  "\"LEADING-SHIFT\"", "LEFT", "LEFTLINE", "\"LEFT-TEXT\"", "LENGTH",
  "\"FUNCTION LENGTH/BYTE-LENGTH\"", "\"LENGTH OF\"", "LESS",
  "\"LESS OR EQUAL\"", "LIMIT", "LIMITS", "LINAGE", "\"LINAGE-COUNTER\"",
  "LINE", "\"LINE-COUNTER\"", "\"LINE LIMIT\"", "\"LINE-SEQUENTIAL\"",
  "LINES", "\"LINES-AT-ROOT\"", "LINKAGE", "\"LIST-BOX\"", "\"Literal\"",
  "\"LM-RESIZE\"", "LOCALE", "\"FUNCTION LOCALE-DATE\"",
  "\"FUNCTION LOCALE-TIME\"", "\"FUNCTION LOCALE-TIME-FROM-SECONDS\"",
  "\"LOCAL-STORAGE\"", "LOCK", "\"LONG-DATE\"", "LOWER", "LOWERED",
  "\"FUNCTION LOWER-CASE\"", "LOWLIGHT", "\"LOW-COLOR\"", "\"LOW-VALUE\"",
  "\"MAGNETIC-TAPE\"", "MANUAL", "\"MASS-UPDATE\"", "\"MAX-LINES\"",
  "\"MAX-PROGRESS\"", "\"MAX-TEXT\"", "\"MAX-VAL\"", "MEMORY",
  "\"MEDIUM-FONT\"", "MENU", "MERGE", "MESSAGE", "MINUS", "\"MIN-VAL\"",
  "\"Mnemonic name\"", "MODE", "MODIFY", "MODULES", "MOVE", "MULTILINE",
  "MULTIPLE", "MULTIPLY", "NAME", "NATIONAL", "\"NATIONAL-EDITED\"",
  "\"FUNCTION NATIONAL-OF\"", "NATIVE", "\"NAVIGATE-URL\"",
  "\"NEAREST-AWAY-FROM-ZERO\"", "\"NEAREST-EVEN\"",
  "\"NEAREST-TOWARD-ZERO\"", "NEGATIVE", "NESTED", "NEW", "NEXT",
  "\"NEXT-ITEM\"", "\"NEXT GROUP\"", "\"NEXT PAGE\"", "NO",
  "\"NO ADVANCING\"", "\"NO-AUTOSEL\"", "\"NO-AUTO-DEFAULT\"",
  "\"NO-BOX\"", "\"NO DATA\"", "\"NO-DIVIDERS\"", "\"NO-ECHO\"",
  "\"NO-F4\"", "\"NO-FOCUS\"", "\"NO-GROUP-TAB\"", "\"NO-KEY-LETTER\"",
  "\"NO-SEARCH\"", "\"NO-UPDOWN\"", "NORMAL", "NOT", "NOTAB", "NOTHING",
  "NOTIFY", "\"NOTIFY-CHANGE\"", "\"NOTIFY-DBLCLICK\"",
  "\"NOTIFY-SELCHANGE\"", "\"NOT END\"", "\"NOT EOP\"", "\"NOT ESCAPE\"",
  "\"NOT EQUAL\"", "\"NOT EXCEPTION\"", "\"NOT INVALID KEY\"",
  "\"NOT OVERFLOW\"", "\"NOT SIZE ERROR\"", "\"NUM-COL-HEADINGS\"",
  "\"NUM-ROWS\"", "NUMBER", "NUMBERS", "NUMERIC", "\"NUMERIC-EDITED\"",
  "\"FUNCTION NUMVAL-C\"", "OBJECT", "\"OBJECT-COMPUTER\"", "OCCURS", "OF",
  "OFF", "\"OK-BUTTON\"", "OMITTED", "ON", "ONLY", "OPEN", "OPTIONAL",
  "OPTIONS", "OR", "ORDER", "ORGANIZATION", "OTHER", "OUTPUT",
  "\"OVERLAP-LEFT\"", "\"OVERLAP-TOP\"", "OVERLINE", "\"PACKED-DECIMAL\"",
  "PADDING", "PAGE", "\"PAGE-COUNTER\"", "\"PAGE-SETUP\"", "PAGED",
  "PARAGRAPH", "PARENT", "PASSWORD", "PERFORM", "PERMANENT", "PH", "PF",
  "PHYSICAL", "PICTURE", "\"PICTURE SYMBOL\"", "PIXEL", "PLACEMENT",
  "PLUS", "POINTER", "\"POP-UP\"", "POSITION", "\"POSITION-SHIFT\"",
  "POSITIVE", "PRESENT", "PREVIOUS", "PRINT", "\"PRINT-NO-PROMPT\"",
  "\"PRINT-PREVIEW\"", "PRINTER", "\"PRINTER-1\"", "PRINTING", "PRIORITY",
  "PROCEDURE", "PROCEDURES", "PROCEED", "PROGRAM", "\"PROGRAM-ID\"",
  "\"program name\"", "\"PROGRAM-POINTER\"", "PROGRESS", "PROHIBITED",
  "PROMPT", "PROPERTIES", "PROPERTY", "PROTECTED", "PURGE",
  "\"PUSH-BUTTON\"", "\"QUERY-INDEX\"", "QUEUE", "QUOTE",
  "\"RADIO-BUTTON\"", "RAISED", "RANDOM", "RD", "READ", "\"READ-ONLY\"",
  "\"READY TRACE\"", "RECEIVE", "RECORD", "\"RECORD-DATA\"",
  "\"RECORD-TO-ADD\"", "\"RECORD-TO-DELETE\"", "RECORDING", "RECORDS",
  "RECURSIVE", "REDEFINES", "REEL", "REFERENCE", "REFERENCES", "REFRESH",
  "\"REGION-COLOR\"", "RELATIVE", "RELEASE", "REMAINDER", "REMOVAL",
  "RENAMES", "REPLACE", "REPLACING", "REPORT", "REPORTING", "REPORTS",
  "REPOSITORY", "REQUIRED", "RESERVE", "RESET", "\"RESET TRACE\"",
  "\"RESET-GRID\"", "\"RESET-LIST\"", "\"RESET-TABS\"", "RETRY", "RETURN",
  "RETURNING", "REVERSE", "\"FUNCTION REVERSE\"", "\"REVERSE-VIDEO\"",
  "REVERSED", "REWIND", "REWRITE", "RF", "RH", "RIGHT", "\"RIGHT-ALIGN\"",
  "RIMMED", "ROLLBACK", "ROUNDED", "ROUNDING", "\"ROW-COLOR\"",
  "\"ROW-COLOR-PATTERN\"", "\"ROW-DIVIDERS\"", "\"ROW-FONT\"",
  "\"ROW-HEADINGS\"", "\"ROW-PROTECTION\"", "RUN", "S", "SAME",
  "\"SAVE-AS\"", "\"SAVE-AS-NO-PROMPT\"", "SCREEN", "\"SCREEN CONTROL\"",
  "SCROLL", "\"SCROLL-BAR\"", "SD", "SEARCH", "\"SEARCH-OPTIONS\"",
  "\"SEARCH-TEXT\"", "SECONDS", "SECTION", "SECURE", "SEGMENT",
  "\"SEGMENT-LIMIT\"", "SELECT", "\"SELECTION-INDEX\"",
  "\"SELECTION-TEXT\"", "\"SELECTION-ALL\"", "\"SELF-ACT\"",
  "\"semi-colon\"", "SEND", "SENTENCE", "SEPARATE", "SEPARATION",
  "SEQUENCE", "SEQUENTIAL", "SET", "\"level-number 78\"", "SHADING",
  "SHADOW", "SHARING", "\"SHORT-DATE\"", "\"SHOW-LINES\"", "\"SHOW-NONE\"",
  "\"SHOW-SEL-ALWAYS\"", "SIGN", "SIGNED", "\"SIGNED-INT\"",
  "\"SIGNED-LONG\"", "\"SIGNED-SHORT\"", "\"level-number 66\"", "SIZE",
  "\"SIZE ERROR\"", "\"SMALL-FONT\"", "SORT", "\"SORT-MERGE\"",
  "\"SORT-ORDER\"", "SOURCE", "\"SOURCE-COMPUTER\"", "SPACE",
  "\"SPECIAL-NAMES\"", "SPINNER", "SQUARE", "STANDARD", "\"STANDARD-1\"",
  "\"STANDARD-2\"", "\"STANDARD-BINARY\"", "\"STANDARD-DECIMAL\"", "START",
  "\"START-X\"", "\"START-Y\"", "STATIC", "\"STATIC-LIST\"", "STATUS",
  "\"STATUS-BAR\"", "\"STATUS-TEXT\"", "STDCALL", "STEP", "STOP", "STRING",
  "STYLE", "\"SUB-QUEUE-1\"", "\"SUB-QUEUE-2\"", "\"SUB-QUEUE-3\"",
  "\"FUNCTION SUBSTITUTE\"", "\"FUNCTION SUBSTITUTE-CASE\"", "SUBTRACT",
  "SUBWINDOW", "SUM", "SUPPRESS", "SYMBOLIC", "SYNCHRONIZED",
  "\"SYSTEM-DEFAULT\"", "\"SYSTEM-OFFSET\"", "TAB", "\"TAB-TO-ADD\"",
  "\"TAB-TO-DELETE\"", "TABLE", "TALLYING", "TEMPORARY", "TAPE",
  "TERMINAL", "TERMINATE", "\"TERMINATION-VALUE\"", "TEST", "TEXT", "THAN",
  "THEN", "THREAD", "THREADS", "THRU", "\"THUMB-POSITION\"",
  "\"TILED-HEADINGS\"", "TIME", "\"TIME-OUT\"", "TIMES", "TITLE",
  "\"TITLE-POSITION\"", "TO", "\"&\"", "\")\"", "\":\"", "\"/\"", "\".\"",
  "\"=\"", "\"EXTERN\"", "\"FALSE\"", "\"FILE\"", "\">\"", "\"INITIAL\"",
  "\"<\"", "\"-\"", "\"*\"", "\"NULL\"", "\"OVERFLOW\"", "\"(\"", "\"+\"",
  "\"TRUE\"", "TOP", "\"TOWARD-GREATER\"", "\"TOWARD-LESSER\"",
  "\"TRADITIONAL-FONT\"", "TRAILING", "\"TRAILING-SHIFT\"", "TRANSFORM",
  "TRANSPARENT", "\"TREE-VIEW\"", "\"FUNCTION TRIM\"", "TRUNCATION",
  "TYPE", "U", "UNBOUNDED", "UNDERLINE", "UNFRAMED", "UNIT", "UNLOCK",
  "UNSIGNED", "\"UNSIGNED-INT\"", "\"UNSIGNED-LONG\"",
  "\"UNSIGNED-SHORT\"", "UNSORTED", "UNSTRING", "UNTIL", "UP", "UPDATE",
  "UPON", "\"UPON ARGUMENT-NUMBER\"", "\"UPON COMMAND-LINE\"",
  "\"UPON ENVIRONMENT-NAME\"", "\"UPON ENVIRONMENT-VALUE\"", "UPPER",
  "\"FUNCTION UPPER-CASE\"", "USAGE", "USE", "\"USE-ALT\"",
  "\"USE-RETURN\"", "\"USE-TAB\"", "USER", "\"USER-DEFAULT\"",
  "\"user function name\"", "USING", "V", "VALIDATE", "VALUE",
  "\"VALUE-FORMAT\"", "VARIABLE", "VARIANT", "VARYING", "VERTICAL",
  "\"VERY-HEAVY\"", "\"VIRTUAL-WIDTH\"", "VPADDING", "VSCROLL",
  "\"VSCROLL-BAR\"", "\"VSCROLL-POS\"", "VTOP", "WAIT", "\"WEB-BROWSER\"",
  "WHEN", "\"FUNCTION WHEN-COMPILED\"", "WIDTH", "\"WIDTH-IN-CELLS\"",
  "WINDOW", "WITH", "\"Identifier\"", "\"level-number\"", "WORDS",
  "\"WORKING-STORAGE\"", "WRAP", "WRITE", "X", "Y", "YYYYDDD", "YYYYMMDD",
  "ZERO", "SHIFT_PREFER", "$accept", "start", "$@1", "compilation_group",
  "nested_list", "$@2", "source_element_list", "source_element",
  "simple_prog", "$@3", "program_definition", "function_definition",
  "_end_program_list", "end_program_list", "end_program", "$@4",
  "end_function", "$@5", "_program_body", "_identification_header",
  "identification_or_id", "program_id_paragraph", "$@6", "$@7",
  "function_id_paragraph", "$@8", "program_id_name", "end_program_name",
  "_as_literal", "_program_type", "program_type_clause",
  "init_or_recurse_and_common", "init_or_recurse", "_options_paragraph",
  "_options_clauses", "_arithmetic_clause", "arithmetic_choice",
  "_default_rounded_clause", "_entry_convention_clause", "convention_type",
  "_intermediate_rounding_clause", "intermediate_rounding_choice",
  "_environment_division", "_environment_header", "_configuration_section",
  "_configuration_header", "_configuration_paragraphs",
  "configuration_paragraphs", "configuration_paragraph",
  "source_computer_paragraph", "$@9", "_source_computer_entry",
  "_with_debugging_mode", "object_computer_paragraph", "$@10",
  "_object_computer_entry", "object_clauses_list", "object_clauses",
  "object_computer_memory", "object_computer_sequence",
  "object_computer_segment", "object_computer_class", "locale_class",
  "computer_words", "repository_paragraph", "$@11", "_repository_entry",
  "repository_list", "repository_name", "repository_name_list",
  "special_names_header", "special_names_sentence", "special_name_list",
  "special_name", "mnemonic_name_clause", "$@12", "mnemonic_choices",
  "_special_name_mnemonic_on_off", "on_off_clauses", "on_off_clauses_1",
  "alphabet_name_clause", "@13", "alphabet_definition",
  "alphabet_literal_list", "alphabet_literal", "@14",
  "alphabet_also_sequence", "alphabet_lits", "space_or_zero",
  "symbolic_characters_clause", "_sym_in_word", "symbolic_collection",
  "symbolic_chars_list", "symbolic_chars_phrase", "char_list",
  "integer_list", "symbolic_constant_clause", "symbolic_constant_list",
  "symbolic_constant", "class_name_clause", "class_item_list",
  "class_item", "_class_type", "_in_alphabet", "locale_clause",
  "currency_sign_clause", "_with_pic_symbol", "decimal_point_clause",
  "numeric_sign_clause", "cursor_clause", "crt_status_clause",
  "screen_control", "event_status", "_input_output_section",
  "_input_output_header", "_file_control_header", "_i_o_control_header",
  "_file_control_sequence", "file_control_entry", "$@15",
  "_select_clauses_or_error", "_select_clause_sequence", "select_clause",
  "assign_clause", "printer_name", "general_device_name",
  "line_seq_device_name", "_line_adv_file", "_ext_clause",
  "assignment_name", "_assignment_name", "access_mode_clause",
  "access_mode", "alternative_record_key_clause", "_password_clause",
  "password_clause", "$@16", "_suppress_clause",
  "collating_sequence_clause", "alphabet_name", "file_status_clause",
  "_file_or_sort", "lock_mode_clause", "$@17", "lock_mode", "_lock_with",
  "organization_clause", "organization", "padding_character_clause",
  "record_delimiter_clause", "$@18", "record_delimiter_option",
  "record_key_clause", "_split_keys", "source_is", "split_key_list",
  "$@19", "split_key", "relative_key_clause", "reserve_clause",
  "no_or_integer", "sharing_clause", "sharing_option", "_i_o_control",
  "i_o_control_list", "i_o_control_clause", "same_clause", "_same_option",
  "multiple_file_tape_clause", "$@20", "multiple_file_list",
  "multiple_file", "_multiple_file_position", "_data_division", "$@21",
  "_data_division_header", "_file_section_header",
  "_file_description_sequence", "file_description",
  "file_description_entry", "$@22", "file_type",
  "_file_description_clause_sequence", "file_description_clause",
  "block_contains_clause", "_records_or_characters", "record_clause",
  "_record_depending", "_from_integer", "_to_integer",
  "label_records_clause", "value_of_clause", "file_id", "valueof_name",
  "data_records_clause", "linage_clause", "_linage_sequence",
  "linage_lines", "linage_footing", "linage_top", "linage_bottom",
  "recording_mode_clause", "recording_mode", "u_or_s", "code_set_clause",
  "_for_sub_records_clause", "report_clause", "report_keyword",
  "rep_name_list", "_communication_section", "$@23",
  "_communication_description_sequence", "communication_description",
  "communication_description_entry", "$@24",
  "_communication_description_clause_sequence",
  "communication_description_clause", "_input_cd_clauses",
  "named_input_cd_clauses", "named_input_cd_clause",
  "unnamed_input_cd_clauses", "_output_cd_clauses", "output_cd_clauses",
  "output_cd_clause", "_i_o_cd_clauses", "named_i_o_cd_clauses",
  "named_i_o_cd_clause", "unnamed_i_o_cd_clauses",
  "_working_storage_section", "$@25", "_record_description_list", "$@26",
  "record_description_list", "data_description", "$@27", "level_number",
  "_filler", "_entry_name", "user_entry_name", "_const_global",
  "lit_or_length", "con_source", "fp32_usage", "fp64_usage", "fp128_usage",
  "pointer_len", "renames_entry", "_renames_thru", "condition_name_entry",
  "$@28", "constant_entry", "$@29", "constant_source",
  "constant_78_source", "constant_expression_list", "constant_expression",
  "_data_description_clause_sequence", "data_description_clause_sequence",
  "data_description_clause", "redefines_clause", "external_clause",
  "_as_extname", "_global_clause", "global_clause", "picture_clause",
  "usage_clause", "usage", "double_usage", "_font_name", "_layout_name",
  "sign_clause", "report_occurs_clause", "_occurs_step", "occurs_clause",
  "_occurs_to_integer", "_occurs_from_integer", "_occurs_integer_to",
  "_occurs_depending", "_capacity_in", "_occurs_initialized",
  "_occurs_keys_and_indexed", "$@30", "occurs_keys", "occurs_key_list",
  "occurs_key_field", "ascending_or_descending", "_occurs_indexed",
  "occurs_indexed", "occurs_index_list", "occurs_index",
  "justified_clause", "synchronized_clause", "_left_or_right",
  "blank_clause", "based_clause", "value_clause", "$@31",
  "value_item_list", "value_item", "_false_is", "any_length_clause",
  "external_form_clause", "identified_by_clause", "_local_storage_section",
  "$@32", "_linkage_section", "$@33", "_report_section", "$@34",
  "_report_description_sequence", "report_description", "$@35",
  "_report_description_options", "report_description_option",
  "control_clause", "control_field_list", "control_final_tag",
  "control_identifier_list", "control_identifier", "page_limit_clause",
  "page_line_column", "page_limit_cols", "report_int_ident",
  "_page_heading_list", "page_detail", "heading_clause", "first_detail",
  "last_heading", "last_detail", "footing_clause",
  "_report_group_description_list", "report_group_description_entry",
  "$@36", "_report_group_options", "report_group_option", "type_clause",
  "type_option", "_control_heading_final", "_or_page",
  "_control_footing_final", "next_group_clause", "next_group_plus",
  "next_page", "sum_clause_list", "_reset_clause", "data_or_final",
  "present_when_condition", "present_absent", "_page_or_id", "page_or_ids",
  "report_varying_clause", "line_clause", "line_keyword_clause",
  "_line_clause_options", "line_clause_option", "line_clause_integer",
  "column_clause", "col_keyword_clause", "_orientation",
  "_left_right_center", "col_or_plus", "column_integer_list",
  "column_integer", "source_clause", "group_indicate_clause",
  "_screen_section", "$@37", "_screen_description_list",
  "screen_description_list", "screen_description", "$@38", "$@39", "$@40",
  "_screen_options", "screen_option", "control_definition",
  "control_type_name", "control_type", "control_item",
  "_control_attributes", "control_attributes", "control_attribute",
  "control_style", "control_property", "control_style_name",
  "control_property_name", "control_style_name_generic",
  "control_property_name_generic", "control_style_name_label",
  "control_property_name_label", "control_style_name_entry_field",
  "control_property_name_entry_field", "control_style_name_push_button",
  "control_property_name_push_button", "control_style_name_check_box",
  "control_property_name_radio_button", "control_style_name_list_box",
  "control_property_name_list_box", "control_style_name_combo_box",
  "control_style_name_frame", "control_property_name_frame",
  "control_style_name_tab_control", "control_property_name_tab_control",
  "control_style_name_bar", "control_property_name_bar",
  "control_property_name_bitmap", "control_style_name_grid",
  "control_property_name_grid", "control_style_name_tree_view",
  "control_property_name_tree_view", "control_property_name_web_browser",
  "control_style_name_activex", "control_property_name_activex",
  "control_style_name_date_entry", "control_property_name_date_entry",
  "control_style_type", "control_property_type",
  "changeable_control_properties", "changeable_control_property",
  "changeable_window_properties", "changeable_window_property", "eol",
  "eos", "plus_plus", "minus_minus", "control_size", "control_size_unit",
  "_cell", "screen_line_number", "_screen_line_plus_minus",
  "screen_col_number", "_screen_col_plus_minus", "screen_occurs_clause",
  "screen_global_clause", "_procedure_division", "$@41", "$@42", "$@43",
  "$@44", "_procedure_using_chaining", "$@45", "$@46",
  "procedure_param_list", "procedure_param", "_procedure_type",
  "_size_optional", "size_is_integer", "_procedure_optional",
  "_procedure_returning", "_procedure_declaratives", "$@47",
  "_procedure_list", "procedure", "section_header", "$@48",
  "_use_statement", "paragraph_header", "invalid_statement", "_segment",
  "statement_list", "@49", "@50", "statements", "$@51", "statement",
  "accept_statement", "$@52", "accept_body", "$@53", "$@54",
  "accp_identifier", "_accept_clauses", "accept_clauses", "accept_clause",
  "accept_from_screen_clauses", "accept_from_screen_clause",
  "lines_or_number", "at_line_column", "line_number", "column_number",
  "mode_is_block", "accp_attr", "_key_dest", "key_dest", "no_echo",
  "reverse_video", "update_default", "_end_accept", "add_statement",
  "$@55", "add_body", "_add_to", "_end_add", "allocate_statement", "$@56",
  "allocate_body", "allocate_returning", "alter_statement", "$@57",
  "alter_body", "alter_entry", "_proceed_to", "call_statement", "$@58",
  "call_body", "$@59", "_mnemonic_conv", "program_or_prototype",
  "_id_or_lit_or_func_as", "nested_or_prototype", "call_using", "$@60",
  "call_param_list", "call_param", "call_type", "call_returning",
  "return_give", "null_or_omitted", "call_exception_phrases",
  "_call_on_exception", "call_on_exception", "_call_not_on_exception",
  "call_not_on_exception", "_end_call", "cancel_statement", "$@61",
  "cancel_body", "id_or_lit_or_program_name", "close_statement", "$@62",
  "close_body", "close_files", "_close_option", "close_window", "$@63",
  "_close_display_option", "compute_statement", "$@64", "compute_body",
  "_end_compute", "commit_statement", "continue_statement",
  "destroy_statement", "$@65", "destroy_body", "delete_statement", "$@66",
  "delete_body", "delete_file_list", "_end_delete", "disable_statement",
  "$@67", "enable_disable_handling", "_enable_disable_key",
  "communication_mode", "display_statement", "$@68", "display_body",
  "screen_or_device_display", "display_list", "display_atom", "$@69",
  "disp_list", "display_clauses", "display_clause", "display_upon",
  "crt_under", "display_message_box", "$@70", "_display_message_clauses",
  "display_message_clauses", "display_message_clause", "display_window",
  "$@71", "$@72", "sub_or_window", "display_floating_window", "$@73",
  "$@74", "display_initial_window", "$@75", "initial_type", "_graphical",
  "_upon_window_handle", "window_handle", "display_window_clauses",
  "display_window_clause", "no_scroll_wrap", "shadow_boxed",
  "pop_up_or_handle", "pop_up_area", "handle_is_in", "disp_attr",
  "_end_display", "divide_statement", "$@76", "divide_body", "_end_divide",
  "enable_statement", "$@77", "entry_statement", "$@78", "entry_body",
  "evaluate_statement", "$@79", "evaluate_body", "evaluate_subject_list",
  "evaluate_subject", "evaluate_condition_list", "evaluate_case_list",
  "evaluate_case", "evaluate_other", "evaluate_when_list",
  "evaluate_object_list", "evaluate_object", "_evaluate_thru_expr",
  "_end_evaluate", "exit_statement", "$@80", "exit_body",
  "exit_program_returning", "free_statement", "$@81", "free_body",
  "generate_statement", "$@82", "generate_body", "goto_statement", "$@83",
  "go_body", "goto_depending", "goback_statement", "if_statement", "$@84",
  "if_else_statements", "_if_then", "if_true", "if_false", "_end_if",
  "initialize_statement", "$@85", "initialize_body", "_initialize_filler",
  "_initialize_value", "_initialize_replacing",
  "initialize_replacing_list", "initialize_replacing_item",
  "initialize_category", "_initialize_default", "initiate_statement",
  "$@86", "initiate_body", "inquire_statement", "$@87", "inquire_body",
  "inspect_statement", "$@88", "inspect_body", "send_identifier",
  "inspect_list", "inspect_tallying", "$@89", "inspect_replacing",
  "inspect_converting", "tallying_list", "tallying_item", "replacing_list",
  "replacing_item", "rep_keyword", "replacing_region", "inspect_region",
  "inspect_before", "inspect_after", "merge_statement", "$@90",
  "modify_statement", "$@91", "modify_body", "_end_modify",
  "move_statement", "$@92", "move_body", "multiply_statement", "$@93",
  "multiply_body", "_end_multiply", "open_statement", "$@94", "open_body",
  "open_file_entry", "open_mode", "open_sharing", "open_option",
  "perform_statement", "$@95", "perform_body", "$@96", "_end_perform",
  "end_perform_or_dot", "perform_procedure", "perform_option",
  "perform_test", "cond_or_exit", "perform_varying_list",
  "perform_varying", "_by_phrase", "purge_statement", "$@97",
  "read_statement", "$@98", "read_body", "_read_into", "_lock_phrases",
  "ignoring_lock", "advancing_lock_or_retry", "_retry_phrase",
  "retry_phrase", "retry_options", "_extended_with_lock",
  "extended_with_lock", "_read_key", "read_handler", "_end_read",
  "ready_statement", "receive_statement", "$@99", "receive_body",
  "message_or_segment", "_data_sentence_phrases", "_no_data_sentence",
  "no_data_sentence", "_with_data_sentence", "with_data_sentence",
  "_end_receive", "release_statement", "$@100", "release_body",
  "reset_statement", "return_statement", "$@101", "return_body",
  "_end_return", "rewrite_statement", "$@102", "rewrite_body",
  "_with_lock", "with_lock", "_end_rewrite", "rollback_statement",
  "search_statement", "$@103", "search_body", "search_varying",
  "search_at_end", "search_whens", "search_when", "_end_search",
  "send_statement", "$@104", "send_body", "_from_identifier",
  "from_identifier", "with_indicator", "_replacing_line", "set_statement",
  "$@105", "set_body", "on_or_off", "up_or_down", "set_environment",
  "set_attr", "set_attr_clause", "set_attr_one", "set_to", "set_up_down",
  "set_to_on_off_sequence", "set_to_on_off", "set_to_true_false_sequence",
  "set_to_true_false", "set_last_exception_to_off", "set_thread_priority",
  "sort_statement", "$@106", "sort_body", "@107", "sort_key_list",
  "_key_list", "_sort_duplicates", "sort_collating", "sort_input",
  "sort_output", "start_statement", "$@108", "start_body",
  "_sizelen_clause", "_start_key", "start_op", "disallowed_op",
  "not_equal_op", "_end_start", "stop_statement", "$@109",
  "stop_returning", "_status_x", "stop_argument", "stop_literal",
  "string_statement", "$@110", "string_body", "string_items", "$@111",
  "string_item_list", "string_item", "_string_delimited",
  "string_delimiter", "_with_pointer", "_end_string", "subtract_statement",
  "$@112", "subtract_body", "_end_subtract", "suppress_statement",
  "_printing", "terminate_statement", "$@113", "terminate_body",
  "transform_statement", "$@114", "transform_body", "unlock_statement",
  "$@115", "unlock_body", "unstring_statement", "$@116", "unstring_body",
  "_unstring_delimited", "unstring_delimited_list",
  "unstring_delimited_item", "unstring_into", "unstring_into_item",
  "_unstring_into_delimiter", "_unstring_into_count", "_unstring_tallying",
  "_end_unstring", "validate_statement", "$@117", "validate_fields",
  "use_statement", "$@118", "use_phrase", "use_file_exception",
  "use_global", "use_file_exception_target", "use_debugging",
  "debugging_list", "debugging_target", "_all_refs", "use_start_end",
  "program_start_end", "use_reporting", "use_exception", "use_ex_keyw",
  "write_statement", "$@119", "write_body", "from_option", "write_option",
  "before_or_after", "write_handler", "_end_write",
  "_accept_exception_phrases", "_accp_on_exception", "accp_on_exception",
  "escape_or_exception", "_accp_not_on_exception", "accp_not_on_exception",
  "not_escape_or_not_exception", "_display_exception_phrases",
  "_disp_on_exception", "disp_on_exception", "_disp_not_on_exception",
  "disp_not_on_exception", "on_size_error_phrases", "_on_size_error",
  "on_size_error", "_not_on_size_error", "not_on_size_error",
  "_on_overflow_phrases", "_on_overflow", "on_overflow",
  "_not_on_overflow", "not_on_overflow", "return_at_end", "at_end",
  "_at_end_clause", "at_end_clause", "_not_at_end_clause",
  "not_at_end_clause", "at_eop_clauses", "_at_eop_clause", "at_eop_clause",
  "_not_at_eop_clause", "not_at_eop_clause", "_invalid_key_phrases",
  "invalid_key_phrases", "_invalid_key_sentence", "invalid_key_sentence",
  "_not_invalid_key_sentence", "not_invalid_key_sentence", "_thread_start",
  "_thread_handle", "thread_reference_optional", "_scroll_lines",
  "condition", "expr", "partial_expr", "$@120", "expr_tokens",
  "expr_token", "_not_expr", "not_expr", "condition_or_class", "eq", "gt",
  "lt", "ge", "le", "exp_list", "_e_sep", "exp", "exp_term", "exp_factor",
  "exp_unary", "exp_atom", "line_linage_page_counter", "arithmetic_x_list",
  "arithmetic_x", "record_name", "file_or_record_name", "table_name",
  "file_name_list", "file_name", "cd_name", "report_name",
  "mnemonic_name_list", "mnemonic_name", "procedure_name_list",
  "procedure_name", "label", "integer_label", "reference_list",
  "reference", "_reference", "single_reference", "optional_reference_list",
  "optional_reference", "reference_or_literal", "undefined_word",
  "unique_word", "target_x_list", "target_x", "_x_list", "x_list", "x",
  "call_x", "x_common", "report_x_list", "expr_x", "arith_x",
  "arith_nonzero_x", "non_numeric_literal", "nonzero_numeric_literal",
  "prog_or_entry", "alnum_or_id", "simple_display_value",
  "simple_display_all_value", "simple_value", "simple_all_value",
  "id_or_lit", "id_or_lit_or_func", "id_or_lit_or_length_or_func",
  "num_id_or_lit", "positive_id_or_lit", "pos_num_id_or_lit_or_zero",
  "pos_num_id_or_lit", "from_parameter", "sub_identifier",
  "table_identifier", "sub_identifier_1", "display_identifier",
  "numeric_identifier", "identifier_or_file_name", "identifier",
  "identifier_1", "identifier_list", "target_identifier",
  "target_identifier_1", "qualified_word", "subref", "refmod", "integer",
  "symbolic_integer", "unsigned_pos_integer", "report_integer",
  "class_value", "literal", "basic_literal", "basic_value", "function",
  "func_no_parm", "func_one_parm", "func_multi_parm", "func_refmod",
  "func_args", "trim_args", "length_arg", "$@121", "numvalc_args",
  "locale_dt_args", "formatted_datetime_args", "formatted_time_args",
  "not_const_word", "flag_all", "flag_duplicates", "flag_initialized",
  "flag_initialized_to", "to_init_val", "_flag_next", "_flag_not",
  "flag_optional", "flag_rounded", "round_mode", "round_choice",
  "flag_separate", "_from_idx_to_idx", "_dest_index", "error_stmt_recover",
  "verb", "scope_terminator", "_advancing", "_after", "_are", "_area",
  "_areas", "_as", "_at", "_before", "_binary", "_box", "_by",
  "_character", "_characters", "_contains", "_controls", "_control",
  "_data", "_end_of", "_file", "_for", "_from", "_in", "_in_equal",
  "_in_order", "_index", "_indicate", "_initial", "_into", "_is",
  "_is_equal", "_is_are", "_is_are_equal", "_is_in", "_key", "_line",
  "_line_or_lines", "_limits", "_lines", "_message", "_mode", "_new",
  "_number", "_number_is", "_numbers", "_of", "_on", "_on_for",
  "_onoff_status", "_other", "_procedure", "_program", "_protected",
  "_record", "_records", "_right", "_sign", "_signed", "_sign_is", "_size",
  "_standard", "_status", "_symbolic", "_tape", "_terminal", "_then",
  "_times", "_to", "_to_using", "_when", "_when_set_to", "_with",
  "coll_sequence", "column_or_col", "columns_or_cols", "column_or_cols",
  "comp_equal", "exception_or_error", "in_of", "label_option",
  "line_or_lines", "lock_records", "object_char_or_word_or_modules",
  "records", "reel_or_unit", "size_or_length", "with_dups",
  "prog_coll_sequence", "detail_keyword", "ch_keyword", "cf_keyword",
  "ph_keyword", "pf_keyword", "rh_keyword", "rf_keyword",
  "control_keyword", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,   282,   283,   284,
     285,   286,   287,   288,   289,   290,   291,   292,   293,   294,
     295,   296,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   306,   307,   308,   309,   310,   311,   312,   313,   314,
     315,   316,   317,   318,   319,   320,   321,   322,   323,   324,
     325,   326,   327,   328,   329,   330,   331,   332,   333,   334,
     335,   336,   337,   338,   339,   340,   341,   342,   343,   344,
     345,   346,   347,   348,   349,   350,   351,   352,   353,   354,
     355,   356,   357,   358,   359,   360,   361,   362,   363,   364,
     365,   366,   367,   368,   369,   370,   371,   372,   373,   374,
     375,   376,   377,   378,   379,   380,   381,   382,   383,   384,
     385,   386,   387,   388,   389,   390,   391,   392,   393,   394,
     395,   396,   397,   398,   399,   400,   401,   402,   403,   404,
     405,   406,   407,   408,   409,   410,   411,   412,   413,   414,
     415,   416,   417,   418,   419,   420,   421,   422,   423,   424,
     425,   426,   427,   428,   429,   430,   431,   432,   433,   434,
     435,   436,   437,   438,   439,   440,   441,   442,   443,   444,
     445,   446,   447,   448,   449,   450,   451,   452,   453,   454,
     455,   456,   457,   458,   459,   460,   461,   462,   463,   464,
     465,   466,   467,   468,   469,   470,   471,   472,   473,   474,
     475,   476,   477,   478,   479,   480,   481,   482,   483,   484,
     485,   486,   487,   488,   489,   490,   491,   492,   493,   494,
     495,   496,   497,   498,   499,   500,   501,   502,   503,   504,
     505,   506,   507,   508,   509,   510,   511,   512,   513,   514,
     515,   516,   517,   518,   519,   520,   521,   522,   523,   524,
     525,   526,   527,   528,   529,   530,   531,   532,   533,   534,
     535,   536,   537,   538,   539,   540,   541,   542,   543,   544,
     545,   546,   547,   548,   549,   550,   551,   552,   553,   554,
     555,   556,   557,   558,   559,   560,   561,   562,   563,   564,
     565,   566,   567,   568,   569,   570,   571,   572,   573,   574,
     575,   576,   577,   578,   579,   580,   581,   582,   583,   584,
     585,   586,   587,   588,   589,   590,   591,   592,   593,   594,
     595,   596,   597,   598,   599,   600,   601,   602,   603,   604,
     605,   606,   607,   608,   609,   610,   611,   612,   613,   614,
     615,   616,   617,   618,   619,   620,   621,   622,   623,   624,
     625,   626,   627,   628,   629,   630,   631,   632,   633,   634,
     635,   636,   637,   638,   639,   640,   641,   642,   643,   644,
     645,   646,   647,   648,   649,   650,   651,   652,   653,   654,
     655,   656,   657,   658,   659,   660,   661,   662,   663,   664,
     665,   666,   667,   668,   669,   670,   671,   672,   673,   674,
     675,   676,   677,   678,   679,   680,   681,   682,   683,   684,
     685,   686,   687,   688,   689,   690,   691,   692,   693,   694,
     695,   696,   697,   698,   699,   700,   701,   702,   703,   704,
     705,   706,   707,   708,   709,   710,   711,   712,   713,   714,
     715,   716,   717,   718,   719,   720,   721,   722,   723,   724,
     725,   726,   727,   728,   729,   730,   731,   732,   733,   734,
     735,   736,   737,   738,   739,   740,   741,   742,   743,   744,
     745,   746,   747,   748,   749,   750,   751,   752,   753,   754,
     755,   756,   757,   758,   759,   760,   761,   762,   763,   764,
     765,   766,   767,   768,   769,   770,   771,   772,   773,   774,
     775,   776,   777,   778,   779,   780,   781,   782,   783,   784,
     785,   786,   787,   788,   789,   790,   791,   792,   793,   794,
     795,   796,   797,   798,   799,   800,   801,   802,   803,   804,
     805,   806,   807,   808,   809,   810,   811,   812,   813,   814,
     815,   816,   817,   818,   819,   820,   821,   822,   823,   824,
     825,   826,   827,   828,   829,   830,   831,   832,   833,   834,
     835,   836,   837,   838,   839,   840,   841,   842,   843,   844,
     845,   846,   847,   848,   849,   850,   851,   852,   853,   854,
     855,   856,   857,   858,   859,   860,   861,   862,   863,   864,
     865,   866,   867,   868,   869,   870,   871,   872,   873,   874,
     875,   876,   877,   878,   879,   880,   881,   882,   883,   884,
     885,   886,   887,   888,   889,   890,   891,   892,   893,   894,
     895,   896,   897,   898,   899,   900,   901,   902,   903,   904,
     905,   906,   907,   908,   909,   910,   911,   912,   913,   914,
     915,   916,   917,   918,   919,   920,   921,   922,   923,   924,
     925,   926,   927,   928,   929,   930,   931,   932,   933,   934,
     935,   936,   937,   938,   939,   940,   941,   942,   943,   944,
     945,   946,   947,   948,   949,   950,   951,   952,   953,   954,
     955,   956,   957,   958,   959,   960,   961,   962,   963,   964,
     965,   966,   967,   968,   969,   970,   971,   972,   973,   974,
     975,   976,   977,   978,   979,   980,   981,   982,   983,   984,
     985,   986,   987,   988,   989,   990,   991,   992,   993,   994,
     995,   996,   997,   998,   999,  1000,  1001,  1002,  1003,  1004,
    1005,  1006,  1007,  1008,  1009,  1010,  1011,  1012,  1013,  1014,
    1015,  1016,  1017,  1018,  1019,  1020,  1021,  1022,  1023,  1024,
    1025,  1026,  1027,  1028,  1029,  1030,  1031,  1032,  1033,  1034,
    1035,  1036,  1037,  1038,  1039,  1040,  1041,  1042,  1043,  1044,
    1045,  1046,  1047,  1048,  1049,  1050,  1051,  1052,  1053,  1054,
    1055,  1056,  1057,  1058,  1059,  1060,  1061,  1062,  1063,  1064,
    1065,  1066,  1067,  1068,  1069,  1070,  1071,  1072,  1073,  1074,
    1075,  1076,  1077,  1078,  1079,  1080,  1081,  1082,  1083,  1084,
    1085,  1086,  1087,  1088,  1089,  1090,  1091,  1092,  1093,  1094,
    1095,  1096,  1097,  1098,  1099,  1100,  1101,  1102,  1103,  1104,
    1105,  1106,  1107,  1108,  1109,  1110,  1111
};
# endif

#define YYPACT_NINF -3506

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-3506)))

#define YYTABLE_NINF -2786

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
   -3506,   394,  1278, -3506, -3506, -3506,   828, -3506,   135, -3506,
   -3506,  1201, -3506, -3506, -3506,   794,  1053,  1138, -3506,  1220,
    1338, -3506, -3506, -3506,   978,   978,   808,   883,  1317,  1012,
     901,   999,  1307,  1847,   953,   960,  1024,   135,   135, -3506,
   -3506,  1036,  1491, -3506, -3506,  1145, -3506,  1065,  1191, -3506,
    1555,    69,    69,  1141,  1172,  1484,  1484,  1484,    69,  1193,
    1129,  1139,  1484,  1154,  1165,   252, -3506, -3506,  1847, -3506,
   -3506, -3506, -3506, -3506, -3506,   443, -3506, -3506, -3506, -3506,
    1537, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506,  -119,  -119,  1910,  1712,  1721, -3506, -3506,  5851,  6531,
    1194,   777, -3506,  1198,  1212, -3506, -3506, -3506, -3506,   250,
    1484, -3506,  1484, -3506,  1133,  1873,  1133,  1484,  1484, -3506,
   -3506,  1133, -3506, -3506, -3506,  1163,  1167,  1327, -3506, -3506,
   -3506,  1174, -3506, -3506, -3506,  1983,  1983,  1484, -3506,  1839,
   -3506, -3506,  1712, -3506, -3506, -3506,   913,  7063, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
     774, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
    1352, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506,   671, -3506, -3506,
    1465, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506,  1178, -3506,   -51,    73, -3506, -3506,  -116,  1484, -3506,
    1484,   514,  1133,  1608,     6, -3506, -3506, -3506, -3506,  1610,
    1259,   364,   813, -3506,  1216, -3506,  1163, -3506,  1484, -3506,
    1167, -3506,    58, -3506, -3506, -3506, -3506, -3506, -3506,   979,
    -164,  1484,    76, -3506,  1638,  1300, -3506,  1127,  1426,  1817,
    -114, -3506,  -114, -3506, -3506, -3506, -3506,    12, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506,  -281,  3959, 15657,  -174,   913,
    -143,   600,   421,  -398,    72,  -135,  7061,  8657,  -135,   913,
     802,   997,   421,  1133,  1311, -3506, -3506,  8657, -3506, -3506,
     421,  1228,   -34,  8612,  1133,   -34,  2132,  8657, -3506,  1351,
    -163,  1230,  1231,  1230,  1133,  1231,   709,    77,  1230,    75,
    1133,  1231, -3506, -3506, -3506, -3506,  1133, -3506, -3506, -3506,
   -3506, -3506, -3506,  1302, -3506,  8178, -3506, -3506,  1228,   110,
    1133,  1231,  4302,  1133,   709,  1418,  1949, -3506,   331,  1324,
   -3506, -3506,  1325,  1156,  -138, -3506,   313,  1310, -3506, -3506,
   -3506,   497, -3506, -3506,  1133, -3506,  1417, -3506,  1419,  1409,
    1978,  1484, -3506, -3506, -3506,   594, -3506, -3506, -3506, -3506,
   -3506,   379,  1993,  1484,    61,  1257, -3506,   113, -3506, -3506,
     -60, -3506,    98, -3506, -3506, -3506, -3506,  1680,  -164, -3506,
    1725,    69,    69, -3506,   979, -3506, -3506,   652, -3506, -3506,
   -3506, -3506, -3506,  1667,  1484,  1746, -3506, -3506,  1359,  1362,
   -3506, -3506,  1512, -3506,  1155,  1918, -3506,  1691,  1825,  1627,
    1353, -3506,  1133, -3506, -3506,  1374,  1376,  1379, -3506,  1380,
   12693,     6,     6, -3506,  1390,  1391,  1394, -3506, -3506, -3506,
    1395,     6, -3506, -3506, -3506, -3506, -3506,  1133, -3506,  1396,
   -3506,  1379, -3506, -3506,  1937, -3506,  8209, -3506, -3506, -3506,
   -3506,  1408, -3506, -3506,  1404,  1405,  1407, 12693,  3625, 15657,
    3625, -3506,    57,  1007, -3506,  1909, -3506, -3506, -3506,   453,
    1408, -3506, -3506,  -174, -3506,  1430, -3506,     6, -3506,  1961,
    -163, -3506, -3506,  -143, -3506, -3506, -3506, -3506, -3506,  1231,
   -3506,   892,  1627,  1969, -3506,    86, -3506,  1557, -3506, -3506,
    1302,  1408,  1231,  1970,  1609,  2053, -3506, -3506,  1133,  1462,
    1463, -3506, -3506, -3506,  1230,  1872, -3506,  1322,  2135, -3506,
   -3506, -3506, -3506, -3506,  1982,   872,  8363, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506,  1872,  7300,  1326,  1354,  1986,   205,
   -3506,  1792, -3506, -3506, -3506,  1989,    84, -3506, -3506, -3506,
    3793, -3506, -3506,  2042,   774, -3506, -3506, -3506,   421, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506,  1473, -3506, -3506,   224,
   -3506,  1228, -3506, -3506,  1133, 11764,   817, -3506,   851, -3506,
     107, -3506, -3506, -3506, -3506, -3506, -3506,  1446, 11064,   817,
    1991,  8657, -3506,  1466,  1995,  2153, -3506, -3506, -3506, -3506,
    1351, -3506,  1551, -3506, -3506,  1887,  1486, -3506, -3506,  2002,
    -140,  2003,  -154, -3506,  1922, -3506,  2007,  1609,  1397,  2011,
   -3506,  1922,  1133,  2006,  1427, -3506, -3506,  1936, 12693,  1987,
   -3506, -3506, -3506, -3506, -3506, -3506,  1804, -3506,   421, -3506,
   -3506, -3506,  1693,  -202, -3506,    81,  2223, -3506,    82, -3506,
    2019,    59,  7663, -3506, 15657,  1489, -3506,  2023,  1891,  8657,
    1133,  1133,  2025,  8416,  1228, -3506, -3506,  -348, -3506, -3506,
   -3506, -3506,  6247, -3506,  1953, -3506, -3506,  1175, -3506,  2027,
    2091, -3506, -3506,  1133, -3506,  2033,  1922,  1516,  1620,  1852,
    1163,  1163,  1163,   304,  1522, 13175, -3506, -3506, -3506,  1443,
   -3506, -3506, -3506,  1766, -3506,    69, -3506,   564, -3506,   103,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506,   939, -3506,    88, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506,   754, -3506,  1540, -3506, -3506,
    1743, -3506, -3506, -3506, -3506,  1484,  1622,  1874, -3506, -3506,
   -3506, -3506,   860,  1484,  1454,  1927, -3506,  1983,  1058,  1983,
    1536, -3506, -3506,  1546,  2133, -3506, -3506,  1680, -3506,    69,
   -3506, -3506, -3506, -3506, -3506,  1548,   101, -3506,  1484,    65,
    1677,  1554, -3506, -3506,    30,    30,  -276,  1560, -3506, -3506,
   14080, -3506,  2165,  2764, -3506,     7, -3506,  1564, 15657, 15657,
   12239, -3506, -3506, -3506,  1408, -3506,  1481,  1482, 15657, 15657,
   15657, 12693,  1483,  1570, 12693, -3506, -3506, -3506,  8865,  2005,
   -3506,  1353, 15657, -3506, 12693, 15657, -3506,  1408, -3506, -3506,
   -3506,  1203, -3506,  1964, 15657, 15657, 15657, 15657, 15657, -3506,
    1707, -3506,  1765,  1911, -3506, -3506,  4302, -3506,  1133,   892,
   -3506, -3506, -3506,   868,   -35,  1133, -3506, -3506, -3506, -3506,
   -3506, 15657,  1882, -3506,  1489, -3506,  1231, -3506, -3506, -3506,
   -3506,  1714, -3506, -3506, -3506, -3506, -3506, -3506,  -222, -3506,
    1496, -3506,  8657, -3506, -3506, -3506, -3506, -3506,  1837,  2072,
   -3506, -3506,  7300,   171,  1538,  1499,   872,   872,   872,   872,
   -3506, -3506,  8657,  8865,  1524, -3506, -3506,   802,    95, -3506,
    1504, -3506,  1507, -3506, -3506,   591, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506,  7724, -3506, -3506, -3506,  1747, -3506,
   -3506, -3506,   -26, -3506,  2129,  1181,  2061, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506,  -167, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506,  1874, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506,  -167, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, 11764, -3506,  -167,  -167,  -167,   817,
   -3506, 12693,   152, -3506, -3506,  1738, -3506, -3506,    68, 15657,
   -3506, -3506,  -168,  4877, -3506, -3506,    91, 13603,   817, -3506,
   -3506,  1593,   421, -3506, -3506,  8865, -3506,  1510,  1714,  1627,
   -3506,  1690,  1690,   981, -3506,  2015,  2015,   382,  1605,  1603,
   -3506,   437, -3506, -3506,  1615, -3506, -3506, -3506, -3506, -3506,
    1609, -3506, -3506, -3506, -3506,  1988,  8612, -3506, -3506, -3506,
    1994, -3506, -3506, -3506,  1714,  2142, -3506, -3506,  1133,  2142,
    1133,  1510,   275,  1607, -3506, -3506,  1408, -3506,  1616, -3506,
   -3506,   306,  1617,   979, -3506, -3506,  7878, -3506,  2290,   658,
      90, -3506, -3506, -3506,  1484, -3506,  -225,  8657, -3506, -3506,
     893,    -1,  1150, 15657, -3506, -3506, -3506,  1133,  8657, -3506,
    2187,  2059,  2060, -3506, -3506,  8865, -3506, -3506, -3506, -3506,
   12693, -3506, -3506, -3506, -3506, -3506,  2301,  2004, -3506, -3506,
   -3506,  1265, -3506,  1618,  1719,  1962, -3506, -3506,  1773,  1621,
   -3506,  1623, -3506, -3506, -3506,  2241, -3506,  1637, -3506, -3506,
    1624, -3506, -3506, -3506,  2359,  1625, -3506, -3506, -3506, -3506,
     939,  1558, -3506, -3506,  1310,  1967,  1874, -3506, -3506, -3506,
    -208, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506,  1942, -3506, -3506, -3506,  -284, -3506, -3506, -3506,  1829,
   -3506,  2271,   183, -3506, -3506, -3506, -3506,  1484, -3506, -3506,
     116, -3506,  -369,   535,   136, -3506, -3506, -3506,   155, -3506,
   -3506,  1484,  1406, 14080, -3506, -3506, -3506,    66,  1650, 10936,
   -3506, -3506,  1406, -3506, -3506, -3506,  1553,  1552, -3506, 12693,
    1406,  2016,  1695,  1901, -3506, -3506, -3506,  1950, -3506, -3506,
   -3506, -3506, -3506, -3506,  -161, -3506,  1133,    36,   638,  1651,
     117,  1652, -3506,   177,  -247, 12693, -3506, -3506,   231,  1653,
    1655,  1656,   178, -3506,  1408, -3506,  1657, -3506,  1133,   240,
    1659,  1627,  2089,   467, -3506,   189,  -106,   421, -3506,  1261,
    1660,   301, -3506,  1663,  1707,  1007,  1007, -3506, -3506, -3506,
     421, -3506,  1666,  -174, -3506, -3506,   -68,  2393,   612, -3506,
   -3506,  1797,  1822, -3506,   575,  1484, -3506, -3506, -3506,  1282,
     -58, -3506, -3506, -3506,  2041, -3506,  8657, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506,   -19, -3506, -3506,  5628, -3506, -3506,
    3102,  1133, -3506, -3506, -3506, -3506, -3506, -3506,  2108,   467,
    2109, -3506, -3506, -3506, -3506, -3506, -3506,  2414, -3506,  1689,
     126, -3506, -3506,    95, -3506,  1588,  1747, -3506, -3506, -3506,
   -3506, -3506,  1311, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506,  1926, -3506, -3506, -3506,  2216, -3506,  1311, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506,  1838,  1311, -3506, -3506, -3506,
    1133, -3506, -3506,  1133, -3506,  1133,  1133,  1133, -3506,  1692,
   -3506,  2371, -3506, -3506, -3506, 12784, -3506, 12693, 11168, -3506,
   -3506, -3506,  2242,    23,  1124,   -33, -3506, -3506,   166, -3506,
   -3506, -3506, -3506,  8657, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506,   421,   421,   467,
    2126,   279,  1231,  1690,  1694, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506,  2092, 11861,   -78,  2181,  1133,  -174, -3506,
    1265,  1994,  1133, -3506, -3506, -3506, -3506,  1133,    43,   740,
   -3506,  1626, -3506,  1629, -3506,  1265,   -74, 12693,  1941,   933,
     193, -3506,  -161,  1943, -3506, -3506, -3506,  8657,   979,   979,
     979,   979,   979,   979,   979,   979,   658, -3506,   685,   -58,
    -152, -3506,  1752,  1752, -3506, -3506, -3506, 15657, 14409,  1150,
    -286, -3506,  2301, -3506,  1133,  1133,   467,  2143,  1874,  1711,
    2457,  1133,  -293, -3506, -3506,  1714,  2461,   -51, -3506,  1710,
    1814,  1866,  1661,    28,  1133, -3506, -3506, 14243,    28,  2334,
    1484,  1244,  1244,  1484,   -29,  1667,  1484,  2453, -3506,  1972,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
      69,   896,  3303, -3506,  1749, -3506,  2175, -3506,   939, -3506,
   -3506, -3506, -3506, -3506,     3, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506,   782, -3506,  1484,  1801,  1975, -3506,
   -3506, -3506,  2319, -3506, -3506, -3506, -3506,  2430, -3506, -3506,
   -3506, -3506, -3506,  1343,  1133,  1091, -3506, -3506, -3506, -3506,
    1997,  1997, -3506, -3506,  1997,   169, -3506,  1484, -3506, -3506,
   -3506, -3506, -3506,  1484, -3506, -3506, -3506,  1484, -3506, -3506,
   -3506, -3506, -3506,   102, -3506, -3506, -3506,  2406, -3506, -3506,
   -3506, -3506, -3506, -3506,   -37, -3506, -3506, -3506,  2495, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506,  2117,  1828, -3506, -3506,
   -3506,  1406, -3506, -3506, -3506, -3506,   114, -3506, -3506, -3506,
   -3506,  1557, 15373,  1404, 15426,  1404, -3506,  1753, -3506, -3506,
    1133,  1404,  1404,  1404, 12693, -3506,  1557,   676,  1404,     7,
   -3506, -3506, -3506,  2018,  1824,   208,  2195,   467, 15587,  1404,
    1404,   192, -3506, -3506, -3506, -3506, -3506,  2015, -3506, -3506,
   -3506, -3506, -3506,  2048, -3506, -3506, -3506,   183, -3506, 15657,
   -3506, -3506, -3506, -3506,  2022,  2147,   522,  1538,   700, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506,  1484, -3506, -3506,
   -3506, -3506,   698, -3506,  1484, -3506,   -52,  1484, -3506, -3506,
   -3506, -3506, -3506,    31,  1484, -3506, -3506, -3506, -3506, -3506,
    1033,  1033,   421, -3506,   421,    78,    95, -3506, -3506, -3506,
    2414, -3506, -3506, -3506,  1133, -3506, -3506, -3506,  2309,  1705,
    1214,   203,  1708, -3506, -3506, -3506, -3506, -3506,   192, 12693,
   -3506, -3506,  2455, -3506,  1388, -3506, -3506, 11168, -3506,  1388,
    2225,  2227, -3506,  1856, -3506, -3506,  1484, -3506, -3506,  2145,
    2017, -3506, -3506, -3506, -3506, -3506,  8657,   421, -3506,   421,
    2009,  2009,  2020, -3506,   931, -3506, -3506, -3506,  1133, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506,  2526, -3506,  2232,
   -3506, -3506,   115,    56, -3506, -3506, -3506, -3506,  2044,  2324,
     -58, -3506,  1105, -3506, -3506, -3506, -3506,  1629,  1939, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506,  8657, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506,   -80, -3506,  1133, -3506, -3506, -3506,   886, -3506,
   -3506, -3506, 15657, -3506,  8657,  8657,   638, -3506,  1207,  -210,
    1999,  8522,  1557,  1557, -3506,   421,  1791, -3506,   192, -3506,
    2028, -3506, 12693, -3506,  2369,  1819, -3506,   740, -3506,   818,
   -3506, -3506, -3506,  1798,  1902,  1912,  1104, -3506,  1742, -3506,
    2248,  1818,   -56, -3506, -3506,  -355,   510,   532,   598,   877,
   -3506,  1729, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506,   984, -3506,  1944, -3506,
      92, -3506, -3506, -3506, -3506,  1133,  2196, -3506, -3506, -3506,
    -156, -3506, -3506, -3506,  1484, -3506, 14555, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506,  1451,   732,   258,  2249, -3506,
    1874,  1558,  1104,  1104,  1737,   578,   569,  1874,  1760,  1484,
   -3506, -3506, -3506,  -148,  1304, -3506, -3506, -3506,  1826, -3506,
    1589, -3506,  2334,  1231,  2566, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506,   160,  1484, -3506, -3506,  1750,
    1830, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506,   155,   155,   155, -3506, -3506, -3506, -3506,
     155,   155,   155, -3506, -3506,  1484,   166,   166, -3506,   169,
    2092,  1484, -3506,  1484,   426, -3506, -3506,   640, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,  2278, -3506,
   -3506, -3506,  2273, -3506, -3506, -3506, -3506, -3506, -3506,  2274,
   -3506, -3506,  1209, -3506, -3506, -3506, -3506, -3506,  1524,  2405,
   -3506,   950, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
    -168,  -168,  -168,  -168,  8657, -3506,   700, -3506,  5567,   155,
   -3506, -3506,   155, -3506, -3506, -3506, -3506,   737,  2382,   155,
     166,   166,   155, -3506,    11,   155,  2359, -3506,  1484, -3506,
   13313, -3506, -3506, -3506, -3506, -3506, -3506,  3214, 13313,   467,
    1996,   467,  1998,  6733, -3506,   546,   -41, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506,  1214, -3506,  2437, -3506, -3506,  1311,
   -3506,  1388, -3506,  1388,   192,  1840,  1840, -3506,  2598,  2563,
   -3506, -3506, -3506, -3506,  -190,  1133, -3506, -3506, -3506,   467,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506,  1134, -3506,  2380,
    1133,  8657,  1926,  2186,  2226, -3506,   630, -3506, -3506, -3506,
     713, -3506, -3506, -3506,  2448,  2139, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506,  2191, -3506, -3506, -3506,  2207, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506,   638, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506,  2119,  1850,  1484, -3506, -3506, -3506,
     640,  2278,   467,  1812, -3506,  2457, -3506,  2092,  2471,  2092,
    -210,  1392, -3506, -3506,   -44,  2539,   -51, -3506,  1870,  1974,
   -3506,  2768,  1484, -3506,  1133, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
    1782,   819, -3506, -3506,  2557,  1855,  1885, -3506, -3506, -3506,
   -3506, -3506, 11942, -3506,  2614, -3506,  2301,  1979,  1979, -3506,
    1806, -3506,  2768, -3506,  1894,  2345, -3506, -3506, -3506,  1737,
   -3506, -3506, -3506, -3506, -3506, -3506,  2237,    14,  2092,   707,
    1484, -3506, -3506,  1484, -3506, -3506,  1667,  1609,   543, -3506,
    1990,  1484,  2558,   199,   -99,   -76,  1510, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506,  1951, -3506,  2236, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506,  2605,  1484,  1231,
    1231,   939, -3506, -3506, -3506,   957, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506,   522, -3506,  1460, -3506,
   -3506,  1133,   193,   193, -3506, -3506,  2299,  2477, -3506,   774,
   -3506, -3506, -3506,  1033,  8657,  8657,  8657,  8657, -3506, -3506,
   -3506, -3506, -3506, -3506,  1627, -3506, -3506, -3506, -3506, -3506,
   -3506,  1133, -3506,  1484,  8657, -3506,  -352, -3506, -3506,   421,
   -3506,   421, -3506, -3506,  8657, -3506, -3506, -3506, -3506, -3506,
   -3506,  2592,  2488, -3506, -3506,  1388, -3506,  8657,  8657, -3506,
   -3506,  2105,  1231,     1, -3506,  1133, -3506, -3506,  2043, -3506,
   -3506, -3506,  2599,  2245, -3506,  1484,  1306, -3506, -3506,   611,
    2247,  2250, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
    1133, -3506,  2477, -3506, -3506, -3506,  1915, -3506,  1133,  2092,
   -3506,  1133, -3506, -3506, -3506, -3506, -3506,  2182,  2426, -3506,
   -3506, -3506, -3506,    69, -3506,   -51, -3506,   -51, -3506,  1920,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,  1935,
   -3506,  2768, -3506,  2222, -3506, -3506,   793,  2262, -3506, -3506,
   -3506, -3506, -3506,  2092,  2370,  1938,  1874,  1938, -3506, -3506,
    2267, -3506,   522,  2558, -3506, -3506, -3506,  2768,  1874,    47,
    1133, -3506, -3506, -3506, -3506,  1874, -3506,  1409, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506,   596,   596,  1484,  2145,
   -3506, -3506,   147, -3506,  1248,  1484,  1484,  1484,  1484, -3506,
    1788, -3506,   279,  1484,  1667, -3506,  2024,  1558,  1231, -3506,
    2146, -3506, -3506, -3506,   125, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506,   193,  2299,   640,  -101,   141, 13313, -3506,
   -3506, -3506, -3506, -3506, -3506,  1133, -3506, -3506, -3506,   640,
     640, -3506, -3506, -3506, -3506, -3506,  8657, -3506, -3506, -3506,
   -3506,  1484,  1231,  1231,  2131, -3506, -3506, -3506,  3976,  1900,
   -3506,  1133, -3506, -3506,  2044,  2324, -3506, -3506, -3506, -3506,
     640,   986, -3506, -3506,  1133, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506,   406,   388, -3506,
   -3506,  1627, -3506, -3506,  2768, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506,  1859,  1874,  1946, -3506,  2517,
   -3506,  2519, -3506, -3506, -3506, -3506, -3506, -3506, -3506,  1133,
   -3506,    26,  2607,    -6, -3506, -3506, -3506, -3506,   -20,  1484,
   -3506, -3506,  4237, -3506, -3506,   569, -3506,  1133,  1133, -3506,
   -3506, -3506, -3506,  1133,  1484, -3506, -3506, -3506,  1874, -3506,
     -23,  1952,   193, -3506, -3506, -3506, -3506, -3506,  2209,   -81,
    1627, -3506, -3506, -3506, -3506, -3506,  1133, -3506, -3506, -3506,
   -3506,  -174,  1231,  1484, 12693, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506,  1526, -3506, -3506, -3506, -3506, -3506,  2140,  2533,
   -3506, -3506,  2130,  -375, -3506,  2037, -3506,  1954,  1133, -3506,
   -3506, -3506,  1874,  2355,  1926,  1926,   201, -3506,   569,   569,
   -3506, -3506, -3506, -3506,  2416, -3506, -3506,  1894,  1874, -3506,
   -3506, -3506, -3506,  1133, -3506, -3506, -3506, -3506, -3506,   607,
   -3506, -3506,   607,  2714, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506,   607,   607,   607,   626, -3506,    83,  -358, -3506,  1133,
    1002, -3506,  2546,   193, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506,  1133, -3506, -3506,  -174, -3506,  1408, -3506, -3506,
    2115,  2026,   -49,  1926,  -122,  1228, -3506, 12485, -3506, -3506,
    -375,  1971,  1977,  1484, -3506, -3506,   201,  1133,  1133,  2301,
   -3506,  2368, -3506,  1124,  2145,  1135, -3506, -3506,  2696,  2607,
   -3506,  -358, -3506, -3506, -3506, -3506,  1624, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,  1484, -3506,
    2197, -3506, -3506, -3506,  1893, -3506,  1893, -3506,  1980, -3506,
   -3506, -3506,  1133, -3506,  1563, -3506, -3506, -3506,    63, -3506,
   -3506,  -243, -3506,  1981,  1287,  1985, -3506, -3506,   406, -3506,
     201, -3506,  1896, -3506, -3506,  1484,  1124,   569,  2549,   -85,
   -3506, -3506, -3506, -3506, -3506,  1133,  1133,   718, -3506, -3506,
   -3506, -3506, -3506, -3506,  2176,   736,    63, -3506, -3506,   832,
      -5,    93, -3506, -3506,  2504, -3506, -3506, -3506, -3506,  1896,
   -3506,  1133, -3506, -3506,  1926, -3506,  2197, -3506,  1484, -3506,
   -3506, -3506,  2229, -3506, -3506,  1926, -3506, -3506,  2234,  1133,
   -3506,    -2,  2354,  2360,  2562,  2347, -3506,   832, -3506,  1116,
     586,  1992,   232, 14913, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506,   166, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506,  1133,  1133,  2029,  1133,  -171,   715,
   -3506, -3506,  1484,  2246,  1133,  1484,  1484,  1484,  1484, -3506,
    2365,   -45,  2366, -3506,  2356, -3506,  1764, -3506, -3506,  1133,
    2609,  1202,  2376,    87,  2377,  2363, -3506,   692, -3506, -3506,
    1133,  2030, -3506,  1104,  1104,  1559, -3506, -3506, -3506, -3506,
    2434,  2659,  -125, -3506,  1484, -3506, -3506, -3506, -3506,   559,
   -3506,  1997,  1997,  1484,  -168,   -52,  1484,  8612, -3506, -3506,
   -3506, -3506,  1997,  -168, -3506,  2559,  1874, -3506,  2672, -3506,
   -3506,  -168, -3506, -3506,  1133, -3506, -3506,  1133, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,  2449,
    1997, -3506, -3506, -3506,  9481, -3506,  1940, -3506, -3506,  1268,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,  1231,  1133,
    1874, -3506,  1133,  1133,  1133,  1133,  1484,  1484,  1484,  1484,
    1484, -3506,  1133,  1484,  1484,  1484,  1484,  1484,  1484,  1484,
    1484,  1484,  1484,  1484, -3506,  1133,  1484, -3506, -3506,  1104,
    1104,   592,  1929, -3506,  1484, -3506, -3506,  1133,  1133, -3506,
   -3506,   155, -3506, -3506, -3506,  1484, -3506,  1484,   155,   166,
   -3506, -3506,   155, -3506, -3506,   155,  1938,  1484,   166, -3506,
   -3506, -3506, -3506, -3506, 10283,    74, -3506, -3506,  1938, -3506,
   -3506, -3506, -3506,  1133,  1133,  1133,  1133,  1133,  1133,  1133,
    1133,  1133,  1133,  1133,  1133,  1133,  1133,  1133,  1133,  1133,
    1133,  1133, -3506, -3506, -3506, -3506, -3506, -3506,  1400, -3506,
   -3506, 12857, -3506,   522,  1133, -3506, -3506,   822,   822, -3506,
   -3506, -3506, -3506, -3506,    70, -3506,   522, -3506, -3506, 15355,
    1353, -3506, -3506, -3506,  2368, -3506, -3506, -3506, -3506, -3506,
    1133, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506,  1133, -3506,  1542,   620,   677, -3506, -3506,   592,
    2013, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
     155, -3506, -3506,   155, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506,  1133,  1133,   187,  1484,  1484,  1790,  1484, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506,  1520, -3506, -3506, -3506,
   -3506,  1133, -3506, -3506, -3506,  1484,   592,   592, -3506,  2436,
    1484,  1484,   592, 13775,  1133,   592, -3506, -3506, -3506,   592,
     592, -3506, -3506,  2418,  2276,  1484,  1874, -3506,  1484,  1627,
   -3506,  1484,  1133, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506,    80, -3506, -3506,   757, -3506,
     563, -3506, -3506, -3506, -3506,  1531,  1133, -3506, -3506, -3506,
   -3506, -3506,  2399, -3506,  1339,  1946, 15816, 15816,  1958,  2464,
    2314,  2314,  1654, -3506, -3506,  -249,  -249, -3506,   757, -3506,
   -3506, -3506, -3506,  -249,  -249, -3506, -3506, -3506,    37,  1133,
   -3506, -3506, -3506, -3506,  1874,  1874, -3506, -3506, -3506,  1938,
    1557, 14667, -3506, -3506,  1262,  1445, -3506, -3506,  1461, -3506,
   -3506, -3506, -3506,   867,   867, -3506, -3506, -3506, -3506, 15816,
   -3506,   744,   744,  2314,  2314, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506,  1104, -3506,  1133, -3506, -3506,  2519,
   -3506,  1926,  1133, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506,     8,    64,  2709, -3506, -3506, -3506,   744, -3506,
   -3506,  2252,  2253, -3506, -3506,  2073,    42, -3506,  2264, -3506,
    2264, -3506,  2264, -3506,  2264, 15816, -3506, -3506, -3506,  1874,
   -3506, -3506, -3506, -3506,  2257, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint16 yydefact[] =
{
       2,     0,    12,     1,     3,     5,    25,     4,    73,    28,
      27,    25,     8,    10,    11,     0,     0,     0,    13,   352,
      76,     9,    32,    29,    50,    50,     0,     0,     0,  1223,
     354,     0,   217,    78,     0,     0,     0,    73,    73,    26,
      74,     0,     0,    24,  1269,     0,   356,     0,     0,    72,
     219,     0,     0,     0,  2734,  2667,  2667,  2667,     0,     0,
       0,     0,  2667,     0,     0,  2633,   146,    75,    79,    80,
      82,    83,    86,    84,    85,     0,   131,   133,   134,   135,
     182,   136,   138,   137,   139,   140,   141,   142,   143,   144,
     145,     0,     0,    53,    16,     0,   353,  1219,     0,     0,
       0,   350,    77,     0,     0,   223,  2330,  2329,   157,   201,
    2667,  2735,  2667,  2668,     0,     0,     0,  2667,  2667,    93,
     115,     0,    87,   129,  2634,     0,     0,  2667,    81,   130,
     132,     0,   181,    35,    34,    38,    38,  2667,    51,    59,
      20,    14,    17,    18,    22,    15,  1455,     0,  1332,  1428,
    1438,  1444,  1451,  1498,  1504,  1524,  1519,  1525,  1530,  1526,
    1538,  1548,  1653,  1662,  1664,  1667,  1693,  1704,  1707,  1710,
    1702,  1716,  1727,  1749,  1753,  1757,  1796,  1798,  1804,  1808,
       0,  1814,  1829,  1855,  1857,  1887,  1888,  1904,  1907,  1908,
    1913,  1922,  1923,  1936,  1949,  1988,  2006,     0,  2043,  2059,
    2068,  2070,  1251,  2074,  2077,  2080,  2099,  2135,  1271,  1272,
    1273,  1274,  1275,  1276,  1277,  1278,  1280,  1279,  1281,  1283,
    1282,  1284,  1285,  1286,  1287,  1288,  1289,  1290,  1291,  1292,
    1293,  1294,  1295,  1296,  1297,  1298,  1299,  1300,  1301,  1302,
    1303,  1304,  1305,  1306,  1307,  1308,  1309,  1310,  1311,  1312,
    1313,  1314,  1315,  1316,  1317,  1318,  1319,  1320,  1321,  1322,
    1323,  1324,  1325,  1326,  1327,  1328,  1329,  1270,   355,   362,
     363,   476,   357,   479,     0,   218,   220,   221,  2667,  2648,
    2667,     0,     0,     0,  2425,   212,  2320,   210,   215,     0,
       0,    95,   117,   214,    89,   495,   192,   193,  2667,  2331,
     184,   185,  2672,   188,  2430,  1960,  1959,   147,   151,   154,
    2715,  2667,     0,   183,     0,     0,    30,     0,     0,    61,
       0,    19,     0,  1459,  1456,  1457,  1458,  1225,  2536,  2537,
    2538,  2539,  2540,  2541,  2542,  2543,  2544,  2545,  2546,  2547,
    2548,  2534,  2586,  2587,  2588,  2589,  2590,  2591,  2592,  2593,
    2594,  2595,  2596,  2597,  2598,  2599,  2600,  2601,  2602,  2603,
    2604,  2605,  2606,  2607,  2549,  2550,  2551,  2552,  2553,  2554,
    2555,  2556,  2557,  2558,  2560,  2559,  2561,  2562,  2563,  2564,
    2565,  2566,  2567,  2568,  2569,  2570,  2571,  2572,  2573,  2574,
    2575,  2576,  2577,  2578,  2579,  2580,  2581,  2532,  2582,  2583,
    2584,  2585,  1331,  2533,  2535,     0,     0,     0,     0,  1455,
       0,     0,     0,     0,     0,  1543,     0,     0,  1543,  1455,
    2229,  1695,     0,     0,  2757,  1483,  1482,     0,  1715,  2229,
       0,     0,     0,     0,     0,     0,     0,     0,  1330,     0,
    2219,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2039,  2042,  2026,  2040,  2224,  2041,  2028,  2037,
    2029,  2038,  2412,  2416,  2046,     0,  2069,  2067,     0,  1269,
       0,     0,     0,     0,     0,     0,   423,   358,  2495,     0,
    2305,   359,     0,  2511,   331,   224,     0,     0,   202,   203,
     213,   208,  2780,  2781,     0,   206,     0,  2632,     0,  2741,
       0,  2667,  2801,   113,    94,  2631,    99,   101,   102,   103,
     104,  2631,     0,  2667,     0,     0,   116,     0,   120,    88,
      91,   194,     0,   186,  2674,  2673,   189,     0,  2715,  2718,
    2717,     0,     0,   148,   152,    39,    33,  2667,    55,    56,
      57,    58,    54,  2696,  2667,    66,    37,    36,     0,     0,
    1228,  1226,  1245,  1358,  2425,  1426,  1334,  2694,  1357,  2708,
       0,  2472,     0,  2467,  2473,     0,     0,  2479,  2450,     0,
       0,  2290,  2292,  2446,     0,     0,     0,  2470,  2451,  2353,
       0,  2294,  2449,  2471,  2447,  2474,  2475,     0,  2452,     0,
    2469,  2479,  2468,  2448,  1436,  2347,  1434,  2339,  2342,  2341,
    2345,  2442,  2444,  2346,  2476,     0,     0,     0,     0,     0,
       0,  1439,     0,  2279,  2282,  2284,  2287,  2362,  2289,  2500,
    2360,  2361,  2317,  1445,  1446,     0,  2313,  2315,  2314,  1496,
    2219,  2386,  1503,  1499,  1500,  1502,  2385,  1515,  1505,  1506,
    1507,  1510,  2708,  1522,  2420,     0,  2296,  2514,  2334,  2419,
    2424,  2335,     0,  1536,  2727,  2637,  1527,  2417,  1529,  2751,
       0,  1545,  1547,  1539,     0,  1604,  1603,  2446,  2627,  1566,
    1602,  1595,  1601,  1594,  1651,  2164,  2337,  1561,  1563,  1555,
    1556,  1591,  1557,  1558,  1604,  1560,     0,  2341,  1660,     0,
    1663,     0,  1665,  1674,  1673,  1691,     0,  1670,  1672,  2228,
    2667,  1697,  1701,  1699,  1702,  1700,  1694,  1705,  1706,  2332,
    1708,  1709,  2758,  1711,  2311,  1703,  1721,  2227,  1728,  1730,
    2307,  1750,  1751,   919,  1610,     0,     0,  1754,   918,  1758,
       0,  1760,  1761,  1762,  1797,  1992,  2406,  2407,  2509,     0,
    1802,     0,  1805,     0,  1812,     0,  1822,  1819,  1821,  1820,
    1815,  1816,  1823,  2652,  1830,  1841,     0,  2306,  1856,  1885,
    2506,  1902,     0,  1905,  2138,  2299,  1911,  2727,     0,  1920,
    2300,  2138,     0,  1934,  1927,  2302,  1937,  1940,     0,     0,
    2310,  1950,  1951,  1952,  1953,  1954,  1955,  1979,  1956,  1982,
    1957,  1958,     0,     0,  2308,     0,     0,  2405,  2424,  1989,
    2024,  2011,  2030,  2223,     0,  2414,  2415,  2057,     0,     0,
       0,     0,  2065,     0,  2071,  2072,  1257,  1263,  1252,  1253,
    1254,  1256,     0,  2075,     0,  2409,  2078,  2729,  2388,  2097,
    2083,  2387,  2389,  2100,  2101,  2149,  2138,     0,     0,   695,
       0,     0,     0,   481,     0,     0,   485,   486,   484,     0,
     361,   364,   222,     0,  2512,     0,   343,   339,   216,     0,
     334,   336,   337,   163,   162,   177,   173,   178,   159,   176,
     174,   160,   161,   175,   158,   164,   165,   167,  2439,  2435,
    2440,  2438,  2436,  2441,  2437,   204,   197,   199,  2767,   207,
       0,  2426,   211,  2800,  2742,  2667,     0,     0,    98,   100,
      96,   114,  2631,  2667,     0,     0,   127,    38,     0,    38,
       0,   118,   121,     0,     0,   195,  2431,   187,   190,     0,
    2716,   155,   149,   150,   153,     0,     0,  2697,  2667,     0,
       0,     0,    21,    23,  1233,  1233,     0,     0,  1427,  1333,
    1359,  2695,     0,     0,  2709,     0,  2443,     0,     0,     0,
       0,  2465,  2485,  2348,  2349,  2350,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2466,  1437,  1429,     0,     0,
    2340,     0,     0,  2453,     0,     0,  2363,  2364,  2365,  2286,
    2359,     0,  2285,  2502,     0,     0,     0,     0,     0,  2501,
    1442,  1447,  1449,     0,  1497,  1452,  1462,  1501,     0,  1510,
    2793,  2794,  1508,     0,  1511,     0,  1523,  1520,  2777,  2776,
    2297,     0,  2516,  2298,  2422,  2423,  1533,  1534,  1537,  1531,
    2728,  1870,  2638,  1528,  2418,  2752,  1544,  1546,  1541,  1605,
       0,  2628,     0,  1652,  1549,  1266,  1266,  1554,  2170,  2167,
    1562,  1559,  2338,  2766,  1606,     0,  2164,  2164,  2164,  2164,
    1661,  1654,     0,     0,  1466,  1692,  1668,  2229,  2229,  1669,
    1676,  1677,  1266,  2245,  2243,  2668,  2249,  2246,  2238,  2242,
    2240,  2241,  2237,  2239,  2230,  2231,  2244,  2233,     0,  1698,
    1696,  2333,  1713,  1722,  1723,  1732,     0,  1752,  1609,   999,
    1028,   996,  1078,  1013,  1012,  1077,  1079,  1101,  1080,  1064,
    1147,  1181,  1097,  1126,  1100,  1123,  1169,  1072,  1095,  1091,
    1098,  1121,  1167,   998,  1001,  1108,  1105,   997,  1104,  1103,
    1153,  1025,  1107,  1026,  1182,  1030,  1090,  1119,  1116,  1143,
    1134,  1171,   970,  1144,  1154,  1117,  1052,  1054,  1053,  1120,
    1155,  1156,  1157,  1158,  1016,  1017,  1146,  1109,  1111,  1110,
    1115,  1050,  1131,  1024,  1133,  1140,  1141,  1032,  1034,  1145,
    1037,   976,  1129,  2653,  1075,  1051,  1023,   993,  1152,   992,
     995,   994,  1150,  1142,  1118,  1102,  1163,  1138,  1139,  1074,
    1160,  1161,  1162,  1151,  1166,     0,  1027,  1128,  1124,  1127,
    1159,  1114,  1125,  1033,  1066,  1096,  1092,  1088,  1099,  1122,
    1164,  1165,  1132,  1035,  1036,  1000,  1168,  1029,  1073,  1031,
    1112,  1113,  1149,  1065,  1067,   969,  1038,  1055,  1076,  1148,
    1180,  1106,  1089,  1130,  1071,  1094,  1093,  2653,   929,   944,
     945,   946,   947,   948,   949,   950,   951,   952,   953,   954,
     955,   956,   957,   958,  1755,  1186,  2653,  2653,  2653,  1756,
    1190,     0,  1783,  1767,  1759,  1764,  1765,  1766,  1996,     0,
    2408,  2510,  2669,  1800,   922,   924,  2675,     0,  1801,  1803,
    1799,     0,     0,  1813,  1809,     0,  1817,  2766,  1870,  2708,
    1843,     0,     0,  2317,  2391,  2221,  2221,     0,  1839,     0,
    2390,  2314,   498,  2392,     0,  2220,  1886,  1858,  2507,  2508,
    2727,  1903,  1889,  1891,  1892,     0,     0,  1906,  1912,  1909,
    1860,  2301,  1921,  1914,  1870,  1929,  1935,  1924,     0,  1929,
       0,  2766,  1938,     0,  2378,  2380,  2381,  2382,     0,  1980,
    1983,     0,     0,     0,  2309,  1962,     0,  1961,     0,     0,
    2422,  2025,  2007,  2013,  2667,  2014,  2009,     0,  2027,  2032,
       0,  2274,  2272,     0,  2413,  2058,  2044,     0,  2047,  2048,
    2051,     0,     0,  2066,  2060,     0,  2073,  1258,  1262,  1255,
       0,  2730,  2731,  2079,  2098,  2081,  2629,     0,  2102,  2150,
    2136,  2140,   477,     0,     0,   698,   529,   532,     0,     0,
     482,     0,   492,   493,   487,   494,   490,  2667,  2513,   225,
    2645,   340,   341,   342,  2614,     0,   332,   335,   166,   169,
       0,     0,   198,   196,     0,     0,     0,  2799,   107,    97,
       0,  2323,   106,   122,   123,   126,   128,   124,   125,   119,
      90,     0,   191,   156,    31,    42,    45,    49,    48,  2723,
      43,    44,     0,    63,    65,    64,    62,  2667,    52,  2630,
    1233,  1230,  1236,     0,  1233,  1246,  1247,  1220,  2622,  2624,
    1364,  2667,  2151,  1360,  1361,  1363,  1365,     0,     0,  2639,
    1356,  1352,  2151,  2773,  2772,  1349,  1341,  1343,  1344,     0,
    2151,     0,     0,     0,  1372,  1336,  1347,     0,  1355,  1338,
    1354,  1339,  2375,  2374,     0,  2352,     0,  2274,  2272,     0,
    2274,     0,  2481,  2274,     0,     0,  2291,  2293,  2274,     0,
       0,     0,  2274,  2356,  2357,  2358,     0,  2295,     0,  2274,
       0,  2708,  2420,  2173,  1435,  2424,  2335,     0,  2445,     0,
       0,  2274,  2288,  2504,  1442,  2278,  2277,  2281,  2280,  2283,
       0,  1440,     0,     0,  2316,  1453,     0,  1460,  1517,  1509,
    1514,     0,     0,  2336,  2173,  2667,  2515,  2421,  1535,  2647,
    2209,  1871,  1872,  1540,     0,  1596,  1580,  2169,  1267,  2172,
    2165,  2171,  2166,  2168,     0,  1572,  1571,  1564,  1567,  1569,
       0,     0,  1592,  1599,  1552,  1553,  1550,  1551,     0,  2173,
       0,  1467,  1666,  1671,  1686,  1688,  1687,  1681,  1683,  1689,
    2229,  1678,  1675,  2229,  1679,     0,  2248,  2232,  2259,  2260,
    2261,  2250,  2757,  2267,  2270,  2269,  2271,  2263,  2256,  2258,
    2257,  2262,  2264,  2266,  2268,  2234,  2251,  2252,  2253,  2254,
    2255,  2710,  1712,  2312,  1724,  1725,  1266,  2757,  1740,  1741,
    1743,  1745,  1746,  1742,  1744,  1735,  2757,  1731,  2654,  2655,
       0,   928,  1185,     0,  1187,     0,     0,     0,  1191,     0,
    1784,     0,  1786,  1785,  1787,  1769,  1779,     0,     0,  1763,
    2798,  2711,     0,  1998,     0,  2656,  2670,  2671,     0,   923,
    2677,  2676,  2678,     0,   964,  1087,  1048,   991,  1007,  1058,
     977,  1081,  1056,  1005,   973,  1086,  1173,  1083,  1069,  1002,
    1070,  1068,  1039,  1041,  1044,  1003,  1057,  1011,  1060,  1009,
    1049,  1046,   962,  1061,  1082,   971,  1015,  1136,  1174,   985,
    1043,   979,   986,  1006,   978,  1062,  1176,  1063,   974,  1022,
    1177,   961,   968,   988,  1019,  1020,   989,  1004,   965,   966,
    1021,   959,  1042,   987,   972,  1178,  1045,  1084,   967,  1172,
    1135,  1179,  1137,   990,  1008,  1040,   960,  1085,  1175,   975,
    1010,  1018,   984,  1170,   982,   983,  1059,  1047,   980,   981,
    1014,   963,   927,   930,   931,   932,   933,   934,   935,   936,
     937,   938,   939,   940,   941,   942,   943,     0,  1806,  2173,
       0,     0,     0,     0,     0,   508,   504,   507,   506,   505,
     510,   627,   521,   517,   519,   520,   522,   518,   523,   628,
     511,  2372,   524,   525,   500,   513,   514,   515,   509,   512,
     503,   502,   499,  2651,  1841,  1832,  2229,     0,     0,  1842,
       0,  1860,     0,  2139,  2402,  2403,  2404,     0,     0,  1916,
    1266,     0,  1928,     0,  1942,  2140,     0,     0,     0,     0,
       0,  1981,     0,     0,  1985,  1984,  1976,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1964,  1965,  2509,  2209,
       0,  2031,  2745,  2745,  2275,  2276,  2427,     0,     0,     0,
    2055,  2049,  2629,  2050,     0,     0,  2173,     0,  1264,     0,
    2496,     0,  2055,  2145,  2144,  1870,  2608,   479,   424,     0,
       0,   701,     0,   574,     0,   483,   489,  2667,   496,  2635,
    2667,     0,     0,  2667,  2635,  2696,  2667,  2612,   360,     0,
     365,   368,   369,   370,   371,   372,   373,   374,   375,   376,
       0,     0,     0,  2646,  2749,  2615,  2647,   333,     0,   168,
     287,   205,   200,   209,     0,   110,   112,   111,   108,   109,
      92,    47,  2724,    41,    46,  2518,  2519,  2520,  2521,  2522,
    2523,  2524,  2525,    60,     0,  1231,  2667,     0,  1243,  1241,
    1234,  1235,  1248,  2395,  1376,  2393,  2394,     0,  2157,  2158,
    2162,  2163,  1335,  2159,  1415,  2154,  1266,  1362,  2771,  2770,
    2700,  2700,  1374,  1375,  2700,     0,  1381,  2667,  1393,  1394,
    1395,  1383,  1384,  2667,  2640,  1385,  1425,  2667,  1386,  1389,
    1387,  1388,  1390,     0,  1419,  1420,  1397,  1399,  2726,  1400,
    1423,  1421,  1422,  1391,     0,  1402,  1392,  1382,  2610,  1404,
    1424,  1407,  1366,  1396,  1401,  1406,     0,     0,  1353,  1340,
    1342,  2151,  1350,  1345,  1346,  1373,  2621,  1348,  2377,  2351,
    2376,  2514,     0,  2476,     0,  2476,  2480,     0,  2457,  2486,
       0,  2476,  2476,  2476,     0,  2459,  2514,     0,  2476,     0,
    1266,  1266,  1430,  2179,  2176,  2422,  2423,  2173,     0,  2476,
    2476,     0,  2503,  1441,  1443,  1450,  1448,  2221,  1464,  1465,
    1461,  1463,  1516,     0,  1513,  1512,  1521,     0,  1875,     0,
    1266,  1266,  1532,  2210,  2216,  2213,     0,  1606,  1582,  1269,
    1578,  1579,  1576,  1575,  1577,  1574,  1568,  2667,  1639,  1640,
    1641,  1628,     0,  1631,  2667,  1632,  2643,  2667,  1635,  1636,
    1570,  1642,  1637,     0,  2667,  1638,  1645,  1643,  1573,  1607,
    2766,  2766,     0,  1655,     0,  1473,  2229,  2229,  1685,  1266,
    1682,  2236,  2235,  2265,     0,  1266,  1726,  1717,  1720,     0,
       0,  1747,     0,  1189,  1188,  1194,  1193,  1192,     0,     0,
    1780,  1782,     0,  1775,  1789,  1776,  1777,  1768,  1771,  1789,
       0,  2380,  2797,     0,  2769,  1990,  2667,   668,   669,  2682,
       0,  2657,  1997,   926,  1184,  1183,   925,  1807,  1810,     0,
    2719,  2719,     0,  1824,  1825,  2303,   501,   516,     0,  1831,
    1837,  1838,  1266,  1834,  1848,  1844,  1849,  1845,  1850,     0,
    1840,  1847,  1862,  1893,  1861,  1266,  1266,  1910,  2198,     0,
    2209,  1917,     0,  1930,  2229,  2229,  1925,  1931,  1947,  1946,
    1945,  1944,  1943,  1963,  1986,  2399,  1987,  2398,  2400,  2401,
    1975,     0,  1978,  1967,  1968,  1969,  1973,  1970,  1974,  1971,
    1972,  1966,  2510,  2023,     0,  2020,  2021,  2015,     0,  2008,
    2796,  2795,     0,  2746,  2035,  2035,  2273,  2428,     0,  2182,
       0,     0,  2514,  2514,  2061,     0,     0,  1265,     0,  2497,
    2084,  2085,     0,  2088,  2091,  2095,  2089,  1916,  2609,     0,
     478,   426,   696,     0,     0,   833,  2672,   530,     0,   575,
       0,   527,     0,   682,   581,  2736,  2736,  2736,  2736,  2736,
     582,  2762,   583,   584,   586,   587,   588,   589,   590,   592,
     623,   621,   622,   624,   625,   591,   597,   593,  2732,   626,
     650,   594,   577,   595,   596,     0,  2739,   606,   607,   605,
     678,   609,   610,   608,  2667,   488,  2667,   553,   555,   556,
     557,   558,   559,   578,   585,   560,   561,   562,   563,   564,
     565,   566,   567,   568,   569,     0,     0,  2619,     0,  2636,
       0,     0,  2672,  2672,     0,     0,     0,     0,     0,  2667,
     419,  2613,   420,     0,     0,   421,   366,   367,     0,   226,
    2727,  2750,  2635,     0,   170,   171,  2788,  2790,  2789,   105,
      68,    69,    70,    71,    67,     0,  2667,  1240,  1244,     0,
       0,  1221,  1380,  2152,  2160,  1266,  1416,  1417,  2410,  2153,
    2155,  2161,  2701,     0,     0,     0,  2432,  1367,  2396,  2397,
       0,     0,     0,  1418,  1405,  2667,  2225,  2225,  2611,     0,
    2651,  2667,  1351,  2667,  1337,  1368,  1370,  2173,  2492,  2463,
    2494,  2464,  2458,  2490,  2460,  2461,  2462,  2488,  2528,  2483,
    2484,  2456,  2336,  2181,  2178,  2174,  2180,  2175,  2177,  2421,
    1431,  2477,     0,  2454,  2455,  2505,  2383,  2384,  1466,     0,
    2517,     0,  2215,  2218,  2211,  2217,  2212,  2214,  1542,  1597,
    2669,  2669,  2669,  2669,     0,  1581,  1583,  1584,     0,     0,
    1629,  1630,     0,  1195,  1197,  1633,  1634,     0,     0,     0,
    2225,  2225,     0,  1623,  2679,     0,  2614,  1622,  2667,  1615,
    1593,  1611,  1618,  1617,  1613,  1624,  1625,     0,  1600,  2173,
    2296,  2173,  2296,  1468,  1469,  1236,     0,  1684,  1690,  1680,
    1714,  1719,  1724,  1733,  1736,  1737,  2641,  2754,  1729,  2757,
    1734,  1789,  2379,  1789,     0,  2663,  2663,  1774,  1790,  1791,
    1772,  1778,  1773,  2768,  2000,     0,  2683,  1994,  2658,  2173,
    2720,   328,   329,   330,  1828,  1818,  2304,     0,  2222,  1835,
       0,     0,  2710,     0,  1881,  1863,  1876,  1869,  1865,  1878,
       0,  1266,  1266,  1890,  1899,  1896,  2197,  2200,  2191,  2199,
    2192,  1915,  1918,     0,  1266,  1266,  1932,  2684,  1939,  1977,
    2022,  2012,  2016,  2017,  2018,  2019,  2010,  2033,  2036,  2034,
    2429,  1266,  1266,  2045,  2188,  2185,  2667,  2053,  2052,  2054,
    2173,  2528,  2173,  1260,  2076,  2496,  2087,  2651,  2093,  2651,
    2182,  2146,  2143,  2142,  2686,   425,   479,   699,     0,     0,
     351,     0,  2667,   576,     0,   526,   691,   692,  2737,   620,
     619,   612,   611,   618,   617,   616,   615,   614,   613,  2763,
       0,     0,  2733,   676,   654,     0,   646,   570,  2740,   679,
     680,   677,     0,   554,   572,   693,  2629,  2526,  2526,  2620,
       0,   531,     0,   497,   388,   416,  2791,  2792,  2326,   397,
    2324,  2783,  2782,   390,  2328,  2327,  2692,  2633,  2651,     0,
    2667,   394,   393,  2667,   422,   228,  2696,  2727,  2759,   303,
       0,  2667,  2631,  2682,   305,     0,  2766,   291,   227,   290,
     230,   231,   236,   241,   239,   242,     0,   238,     0,   233,
     302,   234,   235,   240,   237,   232,   243,  2625,  2667,     0,
     338,     0,  1237,  1238,  1242,     0,  1232,  1249,  1251,  2156,
    1377,  1379,  1378,  1410,  1408,  1409,     0,  1412,     0,  1411,
    1413,     0,     0,     0,  1369,  1432,  2659,  2530,  2478,  1477,
    1518,  1874,  1873,  2766,     0,     0,     0,     0,  1590,  1585,
    1648,  1646,  1196,  1198,  2708,  1647,  1650,  1649,  1644,  2681,
    2680,     0,  1614,  2667,     0,  1612,     0,  1619,  1657,     0,
    1656,     0,  1470,  1471,     0,  1475,  1474,  1476,  1266,  1738,
    2642,     0,     0,  1770,  1781,  1789,  2664,     0,     0,  1792,
    1793,     0,     0,  2003,  1999,  1993,  1811,  1827,     0,  1836,
    1833,  1851,  1853,     0,  1866,  2667,  2209,  1864,  1877,     0,
       0,     0,  1880,  1901,  1898,  1894,  1900,  1895,  1897,  1919,
    1926,  1933,  2685,  1948,  2190,  2187,  2183,  2189,  2184,  2186,
       0,  2063,  2530,  2062,  2103,  1259,     0,  2086,     0,  2651,
    2090,     0,  2082,  1266,  1266,  2137,  2148,  2206,  2203,  2147,
    2687,  2688,  2141,     0,   427,   479,   697,   479,   702,     0,
     548,   550,   549,   543,   547,   545,   546,   542,   544,   541,
     688,   683,   685,     0,   528,   681,   629,   636,   602,   599,
     601,   603,   598,  2651,   648,  2755,   651,  2755,   580,   579,
       0,   571,     0,  2631,   638,   639,   535,   534,     0,   378,
       0,   415,  2325,  2693,   399,     0,   381,  2741,   408,   410,
     414,   413,   409,   411,   407,   412,     0,     0,  2667,  2682,
    2760,  2761,   266,   306,  2727,  2667,  2667,  2667,  2667,   325,
    2616,   326,     0,  2667,  2696,  2626,     0,     0,   344,   345,
     348,   172,  1239,  1251,  1269,  1398,  2784,  2785,  2226,  1414,
    1403,  1371,  2660,     0,  2659,  2173,  1486,  2665,  1598,  1589,
    1588,  1586,  1587,  2644,  1627,     0,  1616,  1620,  1621,  2173,
    2173,  1472,  2344,  2343,  2411,  1718,     0,  1748,  1788,  1795,
    1794,  2667,  2001,     0,     0,  1991,  1995,  1826,     0,     0,
    1868,     0,  1859,  1884,  2198,  2195,  1883,  1867,  1879,  2056,
    2173,  2111,  1261,  2092,     0,  2096,  2205,  2208,  2201,  2207,
    2202,  2204,   429,   428,   700,   704,   834,     0,   689,   686,
     538,  2708,   541,   533,   536,   539,   630,   631,   635,   634,
     633,   632,   600,   637,   604,     0,     0,   646,  2756,     0,
     647,   652,   573,   694,  2527,   389,   380,   379,   377,   417,
    2318,   398,  2633,   386,   395,   392,   396,   391,     0,  2667,
     268,   267,   264,   305,   301,     0,   308,     0,     0,  2617,
    2618,   324,   327,     0,  2667,   304,   286,   346,     0,   347,
    1269,     0,     0,  1433,  1266,  1266,  1266,  1454,  1493,  1489,
    2708,  2666,  1480,  1485,  1484,  1479,     0,  1626,  1659,  1658,
    1739,     0,  2004,  2667,     0,  2373,  1854,  2367,  2366,  2368,
    2229,  1882,  2193,  2194,  2196,  2064,  2622,  2134,  2133,  2112,
    2104,  2105,  2610,  2106,  2107,  2108,  2109,  2132,     0,     0,
    2094,   431,   703,   836,   687,     0,   684,     0,     0,   540,
     655,   649,     0,   656,  2710,  2710,   658,  2319,     0,     0,
     400,   401,   402,   403,     0,   382,  2650,   388,     0,   276,
     277,   275,   274,     0,   257,   258,   259,   253,   254,   271,
     260,   261,   271,     0,   262,   263,   252,   250,   251,   256,
     255,   271,   271,   271,     0,   307,     0,   315,   323,  2321,
       0,   349,     0,     0,  2531,  1491,  1495,  1492,  1487,  1494,
    1488,  1490,     0,  1478,  2002,     0,  2369,  2370,  2371,  1852,
       0,  2743,     0,  2710,  2647,     0,   705,     0,   840,   835,
     837,     0,     0,  2667,   537,   657,   658,     0,     0,  2629,
     643,   663,   664,   665,  2682,   662,   406,   405,  2621,  2633,
     387,   315,   272,   247,   273,   248,  2645,   249,   245,   246,
     269,   244,   270,   312,   311,   310,   313,   309,  2667,   317,
     279,   319,  2322,   288,   297,   296,   297,   293,     0,  2529,
    1481,  2005,     0,  2744,     0,  2130,  2129,  2128,     0,   430,
     432,  2663,   706,     0,   841,     0,   838,  2765,     0,   645,
     658,   653,     0,   659,   666,  2667,     0,     0,   384,  2498,
     265,   318,   281,   314,   280,   316,     0,     0,   295,   294,
    1250,  2131,  2779,  2778,  2721,  2124,  2118,  2119,  2121,   453,
       0,     0,   846,   847,     0,   839,   690,   644,   675,   672,
     673,     0,   661,   404,  2710,   383,   279,  2499,  2667,   321,
     322,   320,     0,   300,  2722,  2710,  2127,  2122,  2125,     0,
    2120,  2667,     0,     0,     0,     0,   434,   454,   455,   436,
     464,     0,  2667,  2738,   913,   909,   910,   903,   907,   915,
     901,   908,   900,   906,     0,   902,   904,   905,   914,   911,
     912,   844,   898,   674,   667,     0,   283,     0,     0,     0,
    2126,  2123,  2667,     0,     0,  2667,  2667,  2667,  2667,   456,
       0,  2695,     0,  2748,     0,   433,   437,   439,   438,     0,
       0,     0,     0,     0,     0,     0,   435,   465,   467,   466,
       0,     0,   710,  2672,  2672,  2689,   743,   709,   713,   714,
       0,     0,     0,   870,  2667,   858,   859,   860,   851,  2762,
     852,  2700,  2700,  2667,  2669,  2643,  2667,     0,   875,   868,
     855,   869,  2700,  2669,   856,     0,     0,   867,   877,   874,
     872,  2669,   857,   871,     0,   878,   866,     0,   893,   887,
     891,   890,   888,   892,   848,   894,   889,   873,   861,     0,
    2700,   899,   917,   916,  2509,   385,     0,   278,   282,     0,
    2786,  2787,   298,  2117,  2114,  2116,  2115,  2110,  2113,     0,
       0,   462,     0,     0,     0,     0,  2667,  2667,  2667,  2667,
    2667,   440,     0,  2667,  2667,  2667,  2667,  2667,  2667,  2667,
    2667,  2667,  2667,  2667,   468,     0,  2667,  2816,  2817,  2672,
    2672,     0,   707,   711,  2667,   719,   715,   717,   718,   720,
     722,     0,   849,   850,   883,  2667,   881,  2667,     0,     0,
     853,   854,     0,   896,   879,     0,  2755,  2667,     0,   897,
     895,  1217,   882,   847,  2509,     0,   299,   457,  2755,   461,
     459,   463,   458,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2690,  2691,  2433,  2434,   730,   725,  2686,   729,
     728,     0,   744,     0,   716,   721,   886,  1213,  1209,   884,
     864,   865,   885,   880,  1206,  1216,     0,   863,   862,  2738,
       0,   179,   180,   285,   670,   449,   445,   446,   450,   448,
       0,   451,   441,   447,   442,   443,   444,   473,   469,   470,
     474,   472,     0,   471,   723,  2687,  2688,   724,   727,     0,
       0,   745,   494,   712,  1201,  1199,  1202,  1200,  1214,  1215,
       0,  1210,  1211,     0,  1207,  1205,  1203,  1204,   876,   284,
     460,   671,     0,     0,     0,  2667,  2667,     0,  2667,   731,
     732,   733,   734,   735,   736,   726,     0,   747,   748,  1212,
    1208,     0,   475,  2803,  2802,  2667,     0,     0,  2805,     0,
    2667,  2667,     0,  2738,     0,     0,   742,   738,  2804,     0,
       0,   737,   803,  2661,  2702,  2667,     0,   802,  2667,  2708,
     746,  2667,     0,   753,   754,   755,   764,   756,   758,   761,
     749,   750,   751,   760,   762,     0,   765,   752,   812,   757,
       0,   759,   763,  2774,  2775,  2705,     0,   739,   741,   740,
    2662,   832,  2704,   811,     0,   646,     0,     0,     0,     0,
    2698,  2698,     0,  2229,   815,     0,     0,   810,   812,   814,
     818,   819,   827,   828,     0,   830,  2706,  2707,   821,     0,
    2703,   790,   788,   789,     0,     0,   783,   787,   784,  2755,
    2514,   792,  2354,  2807,     0,     0,  2809,  2811,     0,  2815,
    2813,   766,   771,  2712,  2712,   768,   772,   767,   773,     0,
    2699,   804,   804,  2698,  2698,   797,   816,   817,   813,   829,
     826,   825,   823,   824,  2672,   822,     0,   785,   786,   652,
     831,  2710,     0,   791,  2355,  2806,  2810,  2808,  2814,  2812,
    2714,  2713,   774,   779,     0,   808,   806,   798,   804,   807,
     800,     0,     0,   820,   452,   641,     0,   794,   777,   769,
     777,   782,   777,   770,   777,     0,   805,   799,   801,     0,
     640,   796,   793,   795,     0,   776,   775,   781,   780,   809,
     642,   778
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -3506, -3506, -3506, -3506, -3506, -3506, -3506,  2786, -3506, -3506,
   -3506, -3506, -3506, -3506,  2656, -3506, -3506, -3506,  2012, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506,  2707,  2478,   -46, -3506,
   -3506, -3506,  1366,  2777, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,  2735, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506,  2293,  -321, -3506, -3506,
   -3506, -3506, -3506,  2514, -3506, -3506, -3506, -3506,  2294, -3506,
   -3506, -3506, -3506,  2738, -3506, -3506, -3506, -3506,  2281, -3506,
   -3506, -3506, -3506, -3506,  1948, -3506, -3506, -1368, -3506, -3506,
   -3506, -3506, -3506,  2510, -3506, -3506, -3506, -3506,  2520, -3506,
   -3506,  1932, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -1187, -3506, -3506, -3506,  -568, -3506, -3506, -3506, -3506, -2263,
   -3506, -3506, -3506, -3506, -3506,  -491, -3506,  -147, -3506, -3506,
   -3506, -3506, -3506,  -462, -3506, -3506, -3506,  -526, -3506, -3506,
   -3506, -3506,  -146, -3506, -3506,  2008, -3506, -3506, -3506, -3506,
   -3506,  -151, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,  -373,
   -3506, -3506, -3506,  -129, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,  -616,
   -3506, -3506, -3506,  -566, -3506, -3506,  -623, -3506, -3506, -3506,
   -1833, -3506, -3506,  2021, -3506, -2981, -3506, -2226,  -829, -3506,
    -737, -1135, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -1921, -3506, -3506, -3506, -3506, -1744, -3506, -3506,   489, -3506,
   -3506, -3506, -3506,   947, -2241, -2150,   162,  -837, -3506, -3506,
   -2120, -3506, -3506, -3506, -2985, -3506, -3506, -1027, -3506, -3506,
   -2093, -3506,  -490,  -428, -3506,  1187, -3506, -2247, -3506,  -517,
   -2069, -3506, -3506, -2034, -3506, -1865, -3506,   161, -2557, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506,  -722, -3246, -3506,
   -3506,  -833, -1647, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -2633, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3505, -3506,
   -3506, -3506, -3506,  -950, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506,  -954, -3506, -3506, -3506, -3506, -3506, -3506, -3506,  -399,
   -3506, -3506, -3506,  -740, -3506, -3506, -3506, -3506,  2439, -3506,
    -659, -1244, -3506,  -581, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,  1633,  2141,
     153,  -617,  -614, -3340,  -779, -3506, -3506, -3506,  -615, -3506,
    -645, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506,  1955,   428, -3506,   341,   921, -3506, -3506, -3506,
   -3506, -1701, -3506, -3506, -3506, -3506, -3506, -3506, -3506,  -961,
   -3506, -3506,   -18, -3506,  2792, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506,  1431, -3506,   439, -3506,  -910, -3506, -3506,
    -885, -3506, -3506,   118,  1432, -1446,   882, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506,  1368, -3506, -3506, -3506,
    2277, -3506, -3506, -3506, -3506, -3506,  1365, -3506, -3506, -3506,
     408, -3506, -3506,   360, -3506, -3506,  -774, -3506, -3506, -3506,
    -235, -3506,  -231, -3506, -3506, -3506, -3506,  2275, -3506, -3506,
   -3506, -3506,  1913, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506,  2491, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
    2235, -3506, -3506, -3506,  1333, -3506, -3506, -3506, -3506, -3506,
   -3506,   409, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506,  2230,   809,  2482, -1991, -2281, -3506, -3506, -3506,
   -3506, -3506,   381, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506,  1862, -3506, -3506,
    1861, -3506, -3506,  1320,   778, -3506, -3506, -3506, -3506, -3506,
    2221, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506,   374, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506,   375,  1843, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506,  1675, -3506, -3506,   756, -3506,  1269, -3506, -3506,
   -2031,   366,   368, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,  2188,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -2793,
    1113, -3506, -3506, -3506,   350, -3506, -3506, -3506, -3506, -3506,
   -3506,  1112, -3506, -3506, -3506, -1067,   723, -3506, -3506,   351,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
     343, -3506,   342, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506,   653, -1110, -3506, -3506, -3506,
   -3506, -3506, -3506,  1630,   716, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506,  -242, -3506, -3506,
   -3506, -3506,  1088, -3506, -3506, -3506,  2169, -3506,  2168, -3506,
   -3506, -3506, -3506,  2507, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506,   683, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506,  1602, -3506, -3506,  1071, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506,   317, -3506,  1072,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506,  -401, -3506, -3506, -3506, -3506,
   -3506, -3506, -3506, -3506, -3506,  -382,  1121,  1140, -3506, -3506,
   -1204, -3506,   983, -3506, -3506,   988, -3506,  1130, -3506,  1934,
   -3506,  1959, -1509, -3506,   900, -3506,   902,   326, -3506,   344,
   -3506,   346, -3506, -3506, -3506, -2134,   -66, -1779, -3506, -3506,
      94, -3506,    85, -1807,   330, -3506,   879, -3506,   885,  2361,
   -1188,  2537, -2121, -1799,  -391, -1001, -3506, -3506,  1916, -3506,
    1945,  1398, -1677,   734,   735,   741,   743,   966,   438,  -364,
    1070,  1010, -3506,  1313,  3192,  -913,  -380,  2556,  2540,  2243,
   -2342,  -268,   964,  -451, -3506,  -757, -3506,  -339, -1626,  2031,
    -368,  -111, -3506,  1596, -3506,   311, -1400,   -43,  2719,  -378,
    -384, -3506,  -356,  2040, -3506,   198, -3506,  -901, -1985, -3506,
   -3506, -3506, -3506,  1171,  -734, -1186, -3506,   944,  -412,  2032,
   -3506, -2211,   580, -1577,  -576,  -470,  -410,  -539,  -394, -3506,
   -1949, -3506,  -195,  -549, -3506, -3506, -3506,  1308,  -582,  -609,
    -127,  2113, -3506, -1670,  1628,  -425,   520,  -559,  -271, -3506,
   -3506, -3506,   165,  2440, -3506, -3506, -3506, -3506,  1099, -3506,
   -3506, -3506, -3506, -3506, -3506, -3506, -3506, -3506,  1164, -3506,
   -2017, -3506,   937,   337,   395,   156, -3506, -3506, -3506, -3506,
    -133, -3506,   511, -3506, -3506, -2732, -3506, -3506, -3506, -1345,
   -2423, -2539, -1753, -3506, -3506, -3506, -3506,  -256,  -979, -3506,
   -1751,   839, -3506,    41, -3506, -2466, -3506,   516, -2419, -2259,
   -3506, -3506, -2359, -3506,  -607, -3506, -3506,  2489, -1862, -2104,
   -1940, -3506, -3506,  -635, -1598,  -807,  2521,   847, -3506, -3506,
   -3506,  -632, -3506, -3506, -3506,  -126, -3506,    96, -3506,  1177,
   -2136, -3506, -3506, -3506, -2871,  -400, -3506, -3506, -3506,  -294,
     651, -1447,  -929, -3506, -3506, -3506,  1384, -3506,  -596,  -485,
   -3506,  1143, -3506, -3506,  -291, -3506, -3402,  -752, -3506, -3506,
   -3506, -3506, -3506, -3506
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,     4,     5,     6,    11,    12,     7,     8,
      13,    14,   141,   142,   143,   320,   145,   322,    18,    15,
      16,    24,    35,   537,    25,    34,   135,   548,   315,   925,
    1439,  1440,  1441,    37,   138,   139,   542,   319,   545,  1446,
     931,  2414,    19,    20,    32,    33,    67,    68,    69,    70,
     294,   519,   913,    71,   291,   504,   505,   506,   507,   508,
     509,   510,  1948,   511,    72,   292,   516,   517,   518,   908,
      73,    74,    75,    76,    77,   127,   307,   923,   308,   309,
      78,   278,   874,   875,   876,  1938,  2404,   877,  3673,    79,
     132,    80,   300,   301,   302,   917,    81,   296,   297,    82,
     885,   886,   280,  1413,    83,    84,   889,    85,    86,    87,
      88,    89,    90,    49,    50,   105,   484,   277,   485,  1932,
    2399,  2400,  2740,  2741,  3231,  3232,  3233,  3234,  3112,  3301,
    3293,  2742,  3212,  2743,  3353,  3354,  3388,  3537,  2744,  1941,
    2745,  2746,  2747,  2748,  3317,  3358,  2749,  2750,  2751,  2752,
    3236,  3307,  2753,  3310,  3311,  3355,  3356,  3389,  2754,  2755,
    2980,  2756,  2203,   858,   859,   860,   861,  1404,   862,  1400,
    2988,  2989,  3129,    29,   271,    30,    46,   101,   272,   273,
     851,   274,  1397,  1920,  1921,  3098,  1922,  3385,  3207,  2949,
    1923,  1924,  2723,  3105,  1925,  1926,  3101,  3200,  3201,  3202,
    3203,  1927,  2964,  2965,  1928,  2951,  1929,  1930,  2394,   839,
    2301,  2655,  2904,  2905,  3181,  3264,  3330,  3455,  3456,  3457,
    3458,  3406,  3407,  3408,  3466,  3467,  3468,  3469,   476,  1897,
     477,   478,   843,   844,  1907,   845,  1393,  1394,   298,  2377,
    2919,  1814,  1815,  1816,  1817,  1818,   846,  2665,   847,  1902,
     848,  1903,  2701,  3073,  3074,  2920,  2355,  2356,  2357,  2358,
    2359,  2941,  2308,  2360,  2361,  2362,  2363,  2364,  3082,  3084,
    2365,  3776,  3920,  2366,  2937,  3087,  2685,  3196,  2934,  3276,
    3280,  3346,  3281,  3282,  3283,  3284,  3720,  3285,  3379,  3380,
    2367,  2368,  2691,  2369,  2370,  2371,  3068,  2921,  2922,  3186,
    2372,  2373,  2374,  1385,  2656,  1901,  2907,  2305,  3065,  3182,
    3266,  3371,  3412,  3477,  3478,  3586,  3587,  3588,  3589,  3479,
    3646,  3647,  3648,  3694,  3729,  3730,  3731,  3732,  3733,  3734,
    3582,  3652,  3738,  3753,  3780,  3781,  3851,  3909,  3925,  3913,
    3782,  3836,  3837,  3783,  3883,  3922,  3784,  3785,  3897,  3898,
    3786,  3787,  3788,  3817,  3818,  3819,  3789,  3790,  3874,  3875,
    3821,  3822,  3823,  3791,  3792,  2660,  3183,  3269,  3270,  3271,
    3373,  3374,  3534,  3413,  3524,  3431,  3432,  3531,   725,  3613,
    1263,  1264,  1265,  1266,  1772,  1228,  1773,  1229,  1774,  1230,
    1775,  1231,  1776,  1232,  1777,  1233,  1778,  1234,  1779,  1780,
    1235,  1781,  1236,  1782,  1237,  1238,  1783,  1239,  1784,  1240,
    1241,  1785,  1242,  1786,  1243,  2193,  1651,  1244,  1245,  1249,
    1250,  2515,  2516,  3708,  3709,  3663,  3716,  3717,  3596,  3713,
    3594,  3710,  3525,  3526,    43,   146,  1972,  2768,    44,   552,
     935,   934,  1450,  1451,  1452,  1968,  1969,  2419,   937,  2421,
    2993,   469,   818,   819,  1888,  2885,   820,   821,  2286,  1567,
    1568,  2109,   822,    99,   208,   209,   405,   555,   940,  2046,
     556,  1462,  1463,  1464,  2454,  2455,  1489,  2529,  1992,  1993,
    1466,  2032,  2425,  2426,  3527,  2137,  2035,   939,   210,   406,
     594,   969,   967,   211,   407,   611,  1541,   212,   408,   623,
     624,  1543,   213,   409,   629,  2087,   327,  1545,  1546,  2090,
    1592,  2145,  2543,  2544,  2545,  3006,   427,  3145,  3137,  3250,
    3138,  3248,  3139,   995,   214,   410,   633,   634,   215,   411,
     638,   639,  1002,   640,   998,  2092,   216,   412,   643,  1007,
     217,   218,   219,   414,   656,   220,   413,   653,  1016,  1019,
     221,   415,   663,  1563,   664,   222,   416,   674,   675,   676,
     677,  1043,   678,  1577,  1578,  1579,  2114,   679,  2108,  2505,
    2506,  2507,   680,  1044,  2140,   681,   682,  2107,  2793,   683,
    2141,   684,  1030,  1582,   726,  2530,  2531,  2532,  2533,  2534,
    2535,  2536,  2138,  1034,   223,   417,   688,  1051,   224,   418,
     225,   419,   692,   226,   420,   695,   696,   697,  1059,  1060,
    1061,  1602,  1062,  1597,  1598,  2148,  1056,   227,   421,   706,
     428,   228,   422,   707,   229,   423,   710,   230,   424,   713,
    1632,   231,   232,   429,  1635,  1084,  1636,  2155,  2157,   233,
     430,   718,  1085,  1645,  2161,  2554,  2555,  2556,  2558,   234,
     431,   721,   235,   432,   727,   236,   433,   729,   730,  1254,
    1255,  1668,  1256,  1257,  2177,  2178,  1665,  1666,  1667,  2171,
    2567,  2568,  2569,   237,   434,   238,   435,   740,  1270,   239,
     436,   742,   240,   437,   744,  1274,   241,   439,   750,   751,
     752,  1278,  2585,   242,   440,   754,  2212,  2850,  2213,  1285,
    1286,  1287,  2215,  2217,  2218,  3039,   243,   441,   244,   442,
     759,  1838,  2594,  2595,  2596,  1560,  1561,  1562,  2857,  2598,
    2856,  3042,  1297,   245,   246,   443,   761,  1305,  2603,  2867,
    2604,  2865,  2605,  1302,   247,   444,   763,   248,   249,   445,
     766,  1309,   250,   446,   769,  2230,  2231,  1313,   251,   252,
     447,   773,  1319,  1841,  2236,  2237,  1317,   253,   448,   776,
    1321,  1322,  1845,  2618,   254,   449,   781,   310,  1338,   782,
     783,  1866,  1867,   784,   785,   786,   787,   788,   789,   790,
     791,   255,   450,   734,  2574,  1258,  2845,  1673,  2185,  2843,
    3035,   256,   451,   800,  1869,  1346,  2264,  2265,  2266,  1342,
     257,   802,  1348,  2627,   458,   459,   258,   464,   807,   808,
     809,  1358,  1359,  1883,  2638,  2279,  1356,   259,   465,   812,
    1364,   260,   467,   261,   468,   814,   262,   470,   823,   263,
     471,   826,   264,   472,   829,  1377,  2290,  2291,  1892,  2293,
    2648,  2890,  2650,  1375,   265,   473,   833,  2886,  3051,  3170,
    3171,  3172,  3547,  3173,  3366,  3367,  3399,  3174,  3327,  3175,
    3176,  3177,   266,   474,   835,  1307,  1895,  1896,  2895,  1380,
    1982,  2429,  1983,  1984,  2423,  1985,  1986,  1037,  1572,  1038,
    1570,  1039,  2072,  2477,  2073,  2475,  2074,  2633,  2878,  2634,
    2876,  2635,  2227,  3043,  3163,  2228,  2608,  2609,  2896,  3060,
    2897,  3058,  2898,  2102,  2103,  2496,  2104,  2494,  2105,   755,
    1824,   460,  2777,   716,   717,   699,   700,  1074,  1075,  1605,
    1076,  1625,  1626,  1627,  1628,  1629,  1630,  1351,  1877,  1498,
     613,   614,   615,   616,   595,   645,  1010,   770,   771,   774,
    2204,  2205,   557,   722,   793,   794,  1082,  1288,   626,   627,
    3099,  2715,  3313,  1422,  2709,  2710,  2716,   108,   303,   708,
     647,  1041,   596,   597,  3021,   598,  3841,  1512,   618,  3156,
    1820,  3157,  1494,  2049,  2179,  2561,  1324,  2562,   635,   830,
    1289,  1974,  2437,  2246,  2247,  1833,  1975,   735,   797,   824,
    2180,  3023,   599,   462,   658,   648,   649,   463,   805,   806,
    1976,   918,  2439,  3650,   887,   600,   601,   602,   603,   604,
     605,   606,   973,   951,  1520,  1504,  1505,  1516,  1509,  1499,
    1501,   849,  2292,  3386,   990,  1534,  2082,  1300,  1267,   855,
    1013,  1556,  1963,  2944,  2787,  3005,   402,   403,   404,  2299,
    2449,  2392,  1936,  3121,  2702,  1467,  1468,  2986,  1032,  1453,
     512,   126,  2380,  1023,  2036,  2831,  2517,  1934,   281,  3208,
     756,  1650,  2192,  3003,  3801,  2837,  3146,  1078,  1678,   527,
    1683,  2811,  2577,  2873,  2902,  3581,  2954,  3460,   928,  3861,
    3595,  3803,  3828,   945,  1674,  3892,   531,  2581,  3395,  1953,
    2037,  2757,  1373,  2683,   112,  2670,  2376,   895,  3324,  2274,
    3461,  2402,  1026,  2559,  3089,  2518,  2972,  2680,  3187,  2537,
    2186,  3530,  3698,  3795,  1011,  3364,   494,  2713,  2998,  3542,
    2409,  2384,  1004,  2272,  1675,   513,  3745,  3751,  3854,  3855,
    3856,  3857,  3858,  3482
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     311,   946,   461,   285,   686,   288,   481,  1005,   732,   109,
     293,  1386,  1387,  1388,  1491,   117,  1395,   815,  1284,  1679,
    1994,   953,  1021,  2034,   714,  1552,    98,  2216,  1347,   698,
    1465,  1890,   646,  2154,  2457,  2427,  1334,  2307,   709,   796,
     736,  1015,  1939,   612,  1323,  2096,   709,  2661,   963,  2468,
    2433,  2434,   719,  2389,  2435,  1523,   736,  1599,   976,  2229,
     685,  2760,  2269,  1519,  2300,   709,  3091,   528,  1014,   625,
     106,   795,  2208,  1530,   479,  1569,   905,   106,  3365,  3911,
    2143,  2794,  2795,  2796,  2797,   642,   524,   655,  2654,  3670,
     316,   642,   772,  3810,  3411,  2610,   642,   915,  1825,  3198,
    2838,  1604,  3193, -2710,  1405,  2406,  2825,  1057,  1449,   813,
   -1224,  1409,   550,   560,   910,   723,   124,  2379,  2705,  1680,
   -2766, -2407,  1594,  2706,  2707, -1222,  1670,  2592,  1874, -2408,
    3557,  3811,  3871,  1575,  2720,  1310,   621,  2183,  2110,  2111,
    1589,  3303,  2239,   641,  1227,   654,  2240,  1822,  2571,  3096,
    2538,  3140,  1458,  1594,  3442,  1260,  1449,  2210,  1576,   983,
    3714,  2387,   733,  1631,  3585,  1874,  -480,  1660,  3134, -2757,
   -2766,   490,  3325,  2446,   760,  3644,  1443,   767,  2956,  1612,
    2841,  1988, -2766,   801,   899,  1989,  1490,   279,  3134,  1015,
    2241,  2267,  3209,   736,  1449,  1648,  1354,   890,  3242,   753,
    2762,   832,  3267,   827,  2513,  -708,  2514,   560,  2721,  1458,
     558,  1792,   619,  1676,  1449,   636,  1340,   529,  1945,   657,
   -2442,   687,  2770,  2771,  2772, -2629,   914,  1955,  2601,  2773,
    2774,  2775,  1435,   533,   642,   659,  2187,   728,   731,   553,
     728,  2520,  -708,  -708,  1455,   981,  1874,  1839,  2689,  2815,
     622,   780, -2728,   660,  1661,  2270,  1492,  2815, -2710,  1251,
   -2766,   803,  3570,  2048,  2225,  2640,  2641, -2766,  2038,   534,
    1087, -2647,  1362,   482,   279,   825,  2042,   831,   834,  1670,
    2198,   631, -2682,  1052,   625,  2006, -2621,  3369,  3539,  3267,
   -2621,  1335,  2699,  2576,  2200,  2631,  1303,  3908,  2800,   955,
    2057,  2801,  3017,   841,  2684,   133,  1874,  1874,  2805,  2975,
     546,  2808,  3206,  1367,  2812,  1437,   642,  2443,  3711,  3178,
    1042,   856,  2100,  1966,  1081,  3750,  2779,  3033,   778,  2190,
    1292,  3921,  2668,  1298, -2766,  1081,   978,   621,   621,   621,
    3308, -1224,  3655,  2763, -2766,  1008, -1224,  1003,   304,   863,
    1343,  2666,   113,  3912,   124, -2667, -1222,  3900,  3254,  3110,
    1874, -1222,  1789,  1366,  3069,  3409,  3743,   947,   492,  1874,
     652,   999,  2900,  2809,  2977,  2978,  2901,  2284,  1436,   113,
    3744,  3473,  3474,  3410,  1017,   906, -2753,  2976,    17,  1314,
    3069,  2188,  2810,  3916,     3,   661,  1495,   125,   622,  2806,
    2807,  2979,  3135,  2088,   709,  1557,  3852,  3309,  3655,   113,
    1331,  1081,  1528,   980,   980,   980,  1368,   736, -2649,  1299,
    -708,   560,  3540,  2611,  3111,  1086,   568,  3541,  1967,   632,
    1874,   642,   568, -1224,   780,   514,  2615,   568,   636,   525,
    1352,  3872,  2669,  1662,  1819,  1819,  2101,   480, -1222,   568,
    3824,  1344,  1886,   134,  1381,  2667,  1553,  1345,   547,  2407,
      51,  1294,  3321,  1024,  1458,   497, -1224, -2766, -1224, -2631,
    3812, -2495,  1681,   779,  -708, -2766,  2593,   642,   498,  2690,
     497, -1222,  1990, -1222,  1293,  1279,  1438,   622,   921,   922,
     571, -1941,   572,   498,  2191, -1941,   571,  3018,   572,   573,
    3032,   571,  3304,   572,  2089,   573, -2766,  1327,  1350,  1304,
     573,   857, -2766,   578,  2058, -2416,   304,  1659,  3141,   578,
    3094,   840,   573,   493,   578,  1946,   864,  2836,   780,  1088,
    2833,  3034,  2834,   621, -2414,   488,   578,  2281,  2602, -1941,
    2271, -2621,  2226,   568,  2112, -2621, -2766,    52,   840,   483,
     530,   888,   284,  1465,  1663, -2359,  3347,  1333,   888,   279,
    1290,  2826,   856,  3205,   554,  2632,  3279,  3210,  2480,  1456,
    2405,   114,   115,   116, -2647,   568,  2700,  1493,   121,   304,
    2099,   899,  1053,  1325,   304,  2620, -2651, -2621,   646,  1671,
     304,  2576, -2766,  2436,  1508,  1508,  1508,  1677,  1649,  1599,
      53,  3651,  1599,    54,  2059,    55,  3645,   662,  1529,   980,
    3109,  1947,   581,   113,  2077,  1361,   573,   304,   581,   888,
    3715,  1991,   888,   581,    56,  1461,   282,  -480,   283,  -708,
     578,  2842, -2629,   289,   290,  3142,  1889,  1421,  1378,   571,
    -480,   572,  3329,   312,  1793,  3097,  1469,  1554,   573,  2759,
    1956,  1957,  1958,   317,   971,  2149,  3211,   568,  2206,   582,
    3326,  3143,   578,  1227,   865,   582,   698,  1575,  1831,  1875,
     582,   284,  3873,   646,  3136,  2158,  1566,   621,   621,   621,
   -2629, -1224,   582,   515,   284,  1622,  2211,   621,   621,   621,
    1515,  3152,  1576,  1515,  3136,   497, -1222,  2722,  2184,  2781,
    1437,   621,   284,  1515,   621,  1003,  1875,   712,   498,  3558,
    1858,    57, -2647,   621,   621,   621,   621,   621, -2629,  1252,
     284,   571,  3044,   572,  3126,   832, -2621,  2815, -2359,  1859,
     573,  1557,  1821,  1821,  1564,  3665,  3443,   866, -2629,  1559,
     621,   816, -1229,  1633,   578,  2041, -2682,  3674,  1558,  1580,
    3348,   867,   857,   980,   980,   980,  2201,   568,  1959,  1672,
    1876,   581, -1227,   980,   980,   980,  1513,  2447,  3475,  1513,
    1418,   284,   568,  2955,  3671,   584,   582,   980,   868,  1513,
     980,   584,  1444,  2827,   888,   901,   584,  1875,  3305,   980,
     980,   980,   980,   980,   486,  -480,   487, -2491,   584, -2667,
    2546,   831,  3008,  1548,  3028,  3199,  2453,  3462,   582,   724,
     499,   284,  1399,  1281,   522,  1282,   980,  2115,   568,   284,
    3839,   989,   817,  2906,   456,   499,  2113,   532,  2297,   984,
     573,  1445,  2427,   551,   985,  2521,   571,  2452,   572,  1410,
    1336,  3571, -2621,   581,   578,   573,  1253,  1875,  1875,   588,
    2408,  1009,   284,   284, -2629,   588,  1682,  -708,   804,   578,
     588,  1424,  1595,  1428,  2968,  3370,  1353,  1406,  2202,    58,
     888,  1438,   588,  1596,   816, -2728,  1433,   911, -2493,  2233,
   -1229,  1835,   571,  2500,   572,  1337,  2069,   284,   709,   816,
     582,   573,   584,  1595,  1788,   646,  2888,   869,  2891,  2488,
   -1227,  1875, -2629,   299,  1596,   578,   907,  2030,   284,   284,
    1875,  3164,  1672,  3462,   107,  3144,  2076,   284,   480,  3813,
     284,   107,   284,  1058,   584,  3345,   284,   620,  3306,  3672,
     593,   284,   651,  2172,   500,  1664,   593,  -708,  2056, -2487,
   -2629,   593,   651,  2075,  1612,  2050,   631,  1081,  2785,   500,
     651,  2557, -2766,   593,    59,   817,  1819,  2403,   581,   888,
   -2629,   497, -2753,  1960,  1961,   804,   588,  2957,  3879,   651,
     817,  1875,  1962,  2070,   498,  3592,   489,  1574,  3076,  2066,
    1327, -2621,   841,  1791,  1353,   646,  2958,  3644,   621,  1879,
     582,  3543, -2489,  2714,  2853,   842,  3476,  2992,   588,  2959,
     284, -2482,  2861,   984,   581,   582,   584,  1860,   985,   841,
    1973,   284, -1218,   870,   284,   888,  3644,   897,   871,   872,
    3104, -2766,   842,  3401,  3166,  2431,   425,  1846,   501,   904,
    2818,  3292,  2820,   113,  3343,  1836,  3463,   284,   284,   502,
     499,  3334,  2612,   501, -2784,  3314, -2766,   593,  2245,  3046,
    3300,   582,  1870,   926,   502,  2501,  1325,   269,  1652,  1425,
     929, -2766,  2080,  1861,   980,  1849,  2823,  2994,   888,   284,
    2846,  1458,  3063,  2860,  3064,  3599,   888,  3045,  2469,   593,
     588,  2070,   621,  1066,  3605,  3544,  3077,  1284,  2613, -2766,
     954,  1851,  3608,  3402,  1862,   452,    62,  3548,  2711,  1327,
     425, -2785,  2644,  3545,  2861,   878,   584, -2766,  2523,  2473,
    2474,  1834,  2599,  2243,  2510,  3705,  1411,    22,  2453, -1941,
   -1941,   584,  3124,  1842,   736,  1844,  2926,   977,   620,   620,
     620,  2881,   524,  2883,  2697,   514,  2456,  1970,  3054,  2492,
    2493,  1035,  3463,   900,  2612,  1599,  2070,   646, -2621,  3392,
   -2495,   284, -2621,  2802,  1872, -2764,  2084,  1401,   980,  2187,
    2071,   593,  1880,  2467,   500,   651, -2647,   584,   873,  1469,
    -660, -2766,  3518,  2396,    65,  1325,  3890,  2262,   879,  3075,
     588,  3644,  3085,  3339,     9,  1826,    10,  2263,  2549,  3078,
    2613,  1863,   880, -2416,  2551,   588,  1637,  2668,  2546,  1638,
    3167,    -7,  1639,  1640,  2086,  3000,  3001,   129,  1327,   503,
    1827,  3593,  2153,   804,  3577,  3578, -2359, -2766,  1246,  2668,
    2927,  1613,  1614,  2397,   901, -2359, -2359,  3185,   651,  2539,
   -2359,  2541,  1638,  3814,  1515,  1639,  1640,  2159,  1966,   651,
    3079,   588,   304, -1218,  1821,  3546,  2162,  3377, -1218,  2410,
    2411,  2589, -1608,  3396,  2093,   453,  3168,    26,   501,  1402,
    1403,   593,  3268,  3519,  2606,  2607,   284,  2928,  2071,   502,
    3895,   780, -2694,  3704,  3471,  3315,   593,  3926,  1893,  3927,
    3896,  3928,  2712,  1580,  1325,  2668,  2579,  1864,    66,  1944,
     865, -2708,  3130,  3520,  -843,   279,  1615,  1616,  1326,  1550,
    3464,  2051,  2970,  2050,  3397,   106,   499,  2671,   651,  3815,
    1513, -2306,   284,  3169,  2188,   651,  1894,  3453,   454,   701,
    3642,  3643,   593, -2766,   620,  -660,   426,  2196, -2747,  2673,
    3189,  3471,  3465,  2071,  2504, -1218,  -843,  3450,   881,  3398,
    3707,   888,    27,  1967,  3521,  1551,  2283,   984,  -843,  3268,
    2511,  2942,   985,  1978,  2652,  2960,  3393,  2412,   898,  1971,
    1979,  3259, -2766,   866,  2971,    23,   323,   284, -1218, -2766,
   -1218,   455,  2642,  2524,  3705,  1036,  -418,   867,  2835,  3522,
    3656,   304,  1426,   515,  1873, -2708,  2139,  3659,  3891,  2803,
    -843,  3662,    28,  2991,  3664,  2675,  1327,  1327,  2679,  1292,
     426,  2565,  1658,   709,  1081,   758,  3464,   762, -2766,  2197,
     984,  1416,   777,  -843,   284,   985,   -40,  -418,  3645,  1420,
     456,  1658,   286,  3453,   286,  3701,  3131,  3721,  3518,   286,
     500,   284, -2708,   270, -2747,  2563,  1427,   284,  3465,  2566,
    2248,   284,  3316,   637,  1442,   480, -2247,  3645,  2862, -2621,
    1622,  1865,   284,  2525,   882,  2163,   888,  2502,  2164,  2470,
    2165,  2166,  2167,  -843,  2769, -2784,  2479, -2766,   620,   620,
     620,   284,  1325,  2181,   888, -2784,  -418,   768,   620,   620,
     620,  1514,    31,  2194,  1514,   525,  2599,  3080,  1526,  2220,
    2503,   736,   620,  1000,  1514,   620,  3133,   305,  2961,  3739,
     306,   944,  3740,    36,   620,   620,   620,   620,   620,  1247,
    3148,  3149,  3773,  2276,  2278,  2698,   284,   492,  1473,  3519,
    2472,    41, -2785,   869,   501,  1474,   457,  2225,   883,  2962,
    1294,   620, -2785,  2963,  3816,   502,  2612,   702,  -843,  2448,
     703,  3165,  2653, -1608,  2456,  2232,  3403,  2929,  3523,  3520,
    2862,  2195,  3597,  1293,   284,  3244, -2621,     9,  2646,    10,
     480,  2584,  3597,  3404,  2668,  2847,  3451,   704,  2930,   693,
    3885,  2413,    39,  1526,  1248,  3081,  1327,  3260,  3405,    42,
     694,  -418,  2613, -1218,   888,  2438,  2280,  2526, -2621,   284,
     286,   279,   538,  2098,  3706,  2726,  3277,  3278,  2280,  3707,
    3521,    -6,  -843,  3774,  3758, -2306,   621,   621, -1608,   884,
    2791,  2848,  3645,  2729,  2727,  3903,  2253,  2254,  2255,  2256,
    2257,  2258,  2259,  2260,  1419,   324,  2728,   746,  1028,  1290,
     325,  -418,  2219,  3775,    -6,  3522,    -6,  2223,  1473,   870,
    2863,  2864,  2224,  1641,  1642,  1474,  2931,    40,   853,  2893,
    3660,  2242,  1325,  2870,  2871,  1623,  2487,  1624,   705,  3667,
      47,   878,  2932,   284,  2730,  3328,  3319,  3863,  1978,    45,
    2874,  2875,   493,  1973,  2677,  1979,  1641,  1642,    48,   326,
     854,  -843,   980,   980,  3777,  -418,  2100,  1001,  2276,  2282,
    2276,  1794,  1643,  1644,  2498,    97,  2294,  2294,  -418, -2747,
    1854,  2681,  3368,  2183,   568,  3864,  2792,  3862,   113,  1909,
    -843,  1855, -2708,  2527,  2482,  3235,   628,    91,  -418,  3778,
     650,   747,   984,  2249,    92,  1643,  1644,   985,  2694,  2695,
     650,   711,  2504, -2708,   879,  2491,   888, -2446,   650,   748,
    3368, -2446,   737,  2487,  1795,  1796,  1797,  1798,   880,  1799,
    1910,   304,   765,  3886,   765,   775,  2548,   798,   737,  3901,
    3902,  2287,  2540,   304,  2542,  1608,  1609,  1610,  1371,  3888,
     986,  1326,  2100,  1372,   630,   888,   480,   573,  2663,   620,
     987,   621,   765,   621,   691,  3564,  3435,  3887,    93,  2428,
    2528,   578,   651,  1515,   873,  1526,  2687,  3439,  3286,  3287,
      96,  -843,   891,  3889,  3523,  2226,   100,   621,  2696,  1911,
    2101, -2708,  3831,  1081, -2747,  3832,  3695,  3119,  3120,   646,
    3696,  3840,  3842,  3880,  3362,  1800,  1801, -2708,   621,   102,
    3452,   539,  3363,  3724,   540,   541,   104,  2382, -2747, -2747,
   -2747,  1020,  2383,  2614,  1980,   305,  1981,  3453,   306,    -6,
    3725,   651,   103,  1611,  3113,   110,  3884,   980,   111,   980,
    1868,  3540,  3454,  2487,    51,   113,  3541,  3025,  -843,  1513,
    2778,  2778,  -843,   620,  3894,  3833,  2996,   888,  1965,   118,
    2997,   749,  1965,   980,  3726,  1526,  2486,  2395,  3779,  3748,
    1326,  3834,  2894,   119,   881,   737,  2101,  -418,  1327,   131,
    3565,  -418,   560,   120,   980,   646,  1327,  1980,  2626,  1981,
    2587,   636,  1878,  1931,  1497,  1500,  1503,  -418,   122,  -418,
   -2694,   979,   984,   982,  3566,  3567,  3568,   985,  2600,   123,
    3929,   628,  3056,  3057,  3342,  2052,  2586,   582,  2054,  3749,
    3727,  1531,  -843,   137,  2778,  2778,  2060,  3383,   140,  2463,
    2064,    52,   144,   650,  2729,   956,   957,  2067,   268,  2550,
    3728,   284,   275,  1964,  1532,   962,  1279,  3528,  2630,  3743,
    2788,  3579,  3580,  2486,  1325,   984,   276,  1977,   284,   984,
     985,   984,  2181,  3744,   985,  3450,   985,  1802,  1803,  1804,
    1805,  1806,  1807,  1808,  1809,  1810,  1537,  1538,  1539,  1326,
     287,   621,  -843,  2232,    53,  2730,  1612,    54,   295,    55,
     882,   993,   299,  2588,  3865,  3007,   650,   314,   113,   313,
    -292,  1327,   318,  2078,   438,  1514,   475,   650,    56,   561,
     466,  1912,   491,   984,   495,  3295,  2248,  2248,   985,  2438,
    3826,  3827,   496,   651,  3297,  3298,  3299,   651,   563,    94,
      95,  1913,  3736,   584,  1535,  1536,  3843,  3748,  1510,  1511,
     651,   503,   535,  1291,   536,   543,  1653,   544,  -418,  2621,
     712,  2097,  3179,   720,  -843,   757,   480,   980,   804,   837,
     775,   838,  1613,  1614,   883,  1655,  1656,  1657,   850,   852,
     892,  2508,   896,  2486,   893,  2969,   650,  1325,   903,  3756,
    3757,   894,   909,   650,   916,  3761,   920,  3844,  3797,   651,
    2248,  2248,  3798,  3799,  1811,    57,  3835,  2731,  3820,   737,
    3825,   927,   930,   932,  2732,  -843,   933,   588,  -418, -2386,
   -2386, -2386, -2386,  1046,  1047,  1048,  1049,  3743,   936,  2487,
     938,   941,   559,   943,   944,  3866,  3867,   560,  3820,   107,
     948,  3744,   949,  3825,  3870,   950,   952,  1615,  1616,  2832,
     966, -2385, -2385, -2385, -2385,   884,   958,   959,   971,  3013,
     960,   961,   964,  3245,  3246,  3247,  1584,  1585,  1586,  1587,
     972,   974,  2733,   975,   988,   994,  3072,  1326,  1326,   992,
    2672,  2674,  2676,  2678,  1006,  2734,  1012,  1018,  1280,   564,
     565,   566,  1020,  1022,  1025,  1027,  1029,  2735,   593,  1031,
    1033,   567,  1079,  2686,  3451,  1050,  1054,  1617,  2459,  1055,
    2461,  1083,  1259,  3528,  1269,  1272,  2464,  2465,  2466,  1273,
    1914,  1275,  1277,  2471,  1915,  1295,  1292,   560,   568,  1296,
    1306,  1301,  1311,  1812,  2483,  2484,  1308,  1316,  2910,  1618,
    1916,  1312,  1917,  2704,  1320,  1318,  1328,   780,  1619,  1332,
    2717,  1341,  1339,  1813,  2184,  1353,  1355,  1620,  1357,  1363,
    2736,  1370,  1374,    58,   561,  3024,  1525,  1292,  1376,  1379,
    1382,  1383,  1384,  3906,  2737,  3331,  1390,   741,  2764,  1396,
    1398,  1414,  1415,   563,  1281,   569,  1282,  1417,   304,  1421,
    1429,  1279,  2859,  -289,  1621,  1423,  3793,   651,   651,   651,
    1430,  1283,  1434,  1431,   574,   575,   576,  1447,  1448,  2249,
    2249,  1470,   577,  1496,  1457,   578,  1506,  1507,  1517,  1518,
    3184,  1527,  1533,  1540,  1542,   622,  1555,  3072,  1559,  1565,
    1036,  1035,  1583,  1600,  1581,  1591,  1603, -2747,  1634,  1647,
    1252,  2724,  1787,  2738,   888,  1823,  1828,  2739,    59,  1829,
    1830,  1525,    60,  1840,  2995,  1832,  1847,  1326,  1857,  2486,
    1679,  1837,  2248,  2248,  1882,  1848,  1850,  1884,  1885,  1449,
    1899,  1891,  1898,  1904,  1900,  1905,  1908,  1906,  1935,  1937,
     628,  1943,  1933,  2249,  2249,  2219,  1950,   620,   620,  1952,
     580,  1918,  1954,  1940,  1995,  2040,   651,  2039,  2043,  2044,
    2045,  2047,  2053,  2055,  2061, -2347,  2062,  2063,  2065,  2310,
    2068,  2079,  2081,  2375,  2378,  2085,  2381,  2091,  2094,  2386,
    2388,  2095,  2390,  2106,  2142,  2144,  3188,  2146,  1395,  3019,
    2147,  3020,  2982,  2151,   564,   565,   566,  1671,  2156,  2169,
    2160,  2168,  2199,  2182,   753,  2214,   567,   689,  2207,  2244,
    2251,  1919, -2747,    61,  2844,  2234,  2273,   715,  2235,  2285,
    2288,   582,  2289,  2298,  2302,  2303,   743,   745,  3452,  2304,
    2379,  2391,  2415,   568,   279,  2306, -2747, -2747, -2747,  2393,
    2401,  2990,  2586,  2416,  3845,  3453,  1292,  2911,  1292,  2418,
      62,  2420,  2422,  3846,  3847,  3252,  2432,  2445,  2448,  2450,
    3454,  3414,  1622,  2440,  2462,   583,  1623,  2071,  1624,  2441,
    2451, -2415,  1292,  2442,  2070,  2489,  2101,  2100,  2552,  2553,
    3093,  2573,  2560,  2564,  2572,   284, -2410,  2576,  2580,  2590,
     569,   570,  2583,  2226,  2578,  2225,    63,   571,    64,   572,
    2591,  2617,  2636,  3415,  2645,  2643,   573,  2647,  2649,   574,
     575,   576,  2657,  2658,  2659,  3416,  2662,   577,  2679,  2664,
     578,  3848,   620,   568,   620,  2663,  2703,  2688,    65,  2682,
     650,   636,  2708,  1525,  1514,   579,  2428,   584,  2718,  2761,
    2725,  3849,  3850,  2248,  2767,  2766,  2786,   651,   620, -2352,
   -2413,  3376,  2790,  2804,  2819,  3256,  2821,  3417,  2981,  2830,
    2836,  2565,   585,   586,  2566,  2849,  3014,  2854,  2855,   620,
    2601,  2602,  2869,  2872,  2631,  2632,  2884,  2889,  2903,  1281,
    3418,  1282, -2766,  2509,  2908,  2909,   970,  2925,  2764,   650,
    2512,  2933,  1292,  2519,  2936,   580,   573,  2935,  2940,  1292,
    2522,  2946,  2943,  2948,  2950,  2249,  2249,  2953,  2985,   497,
     578,   588,   651,  3002,   651,  2983,  2973,  2984,  3004,   581,
    3026,  3027,  3031,  1525,  3037,   589,  3040,  3038,  3047,  3052,
    3419,  3048,  2894,  2893,  3066,  3049,  3067,  3083,  3086,  1326,
   -1846,  3092,    66,  3053,  3088,  3070,  3055,  1326,  3153,   590,
    3125,  3128,  2575,  3160,  3190,  3192,  3194,   591,  3195,   124,
    3262,  3243,  3135,  3263,  3272, -1846,   582,   651,  3265,   651,
    2990,  3273,  2248,  3275,  3288,   970,  3296,   592,  3318,  3322,
    3323,   888,   284,  3279,  3166,  3336,  3337,  3357,  3384,  3352,
    1077,  3378,   593,  3394,  3360,  3372,  3445,   636,  2912,  3375,
    3438,  3440,  3446,  3447,  3448,  3420,  3472,  3556,  3559,  3536,
     583,  3583,  3550,  3560,  2586,  3563,   737,  3159,  3569,  3572,
    3573,  3584,  3576,  3607,  2443, -2495,  3611,  3737,  3758,  3615,
    3830,  1271,  3859,   560,  3800,  3802,  3860,  3915,  3917,  3918,
    3924,  3919,   620,  3931,  2910,  1471,  1472,    21,   321,   136,
     549,  1951,    38,   128,   902,   651,   582,  3204,   520,  3090,
     523,   912,  1326,   130,  3332,   924,   521,  1412,  3436,  3421,
    3147,  3095,  3702,  1408,  3794,  3359,   737,  3114,  3102,  3349,
    3391,   650,   584,  2248,  3289,   650,  3122,  3127,  3107,  3100,
    3561,  3449,  1349,  3158,  3574,  2693,  3161,  1279,   650,  1360,
    2309,   628,  3905,   970,  2939,  3344,  3382,   585,   586,  3180,
    3062,  2189,  3433,  2947,  1389,  3654,  3735,  1407,  3868,  3869,
    2692,  3335,  2375,  3669,   738,  3614,  2249,  1654,  3600,  3712,
    1268,  3601,  1473,  3258,  2586,  3612,  2824,  3604,  2417,  1474,
    1454,   267,  1475,  2784,  1987,  2444,  2789,   650,  3422,  2999,
     991,  2033,  2083,  2822,  3251,  2719,   588,  3249,   997,   690,
    2116,  1040,  1549,  1292,  1045,  2799,  2499,   739,  2817,  1593,
     589,  1601,   584,  2150,  2547,  1080,  2828,  3423,  1646,  2829,
    1669,  3071,  2765,  2570,  2170,  2840,  2839,  2209,  1276,  1476,
    2851,  1477,  1478,  2222,   590,  2597,  2866,  2858,  2868,  1843,
    2651,  3253,   591,  2616,  2261,  1329,  1330,   799,  2629,  3191,
    1881,  2776,  2887,  2295,  2296,  3400,  2238,  2782,  2430,  2783,
    2221,  2424,   592,  1573,  2478,  2476,  2892,   284,  3162,  2879,
    2877,  2899,  3059,  2913,  2497,  2914,   792,   593,  3197,  2495,
    1607,   996,  3061,  3274,  2915,  2916,   588,  1571,  2917,  2918,
     764,  3241,  2622,  2623,  2152,  2249,  3237,  3238,  1524,  2624,
    1606,  2625,  3239,  3434,   836,  1315,  1949,  1479,  3424,  1480,
    2952,   526,  3022,  2250,  1544,  2485,  1481,  3603,  1547,  2780,
    1432,   965,  2268,  1482,  2490,  2945,  2882,  2813,  3050,  3261,
    3350,  3697,  1942,  2911,  2814,  3132,   942,  3893,  2582,   919,
    2275,  2758,  3699,  3103,  3616,  2385,  3853,  3320,  3387,   651,
       0,   651,     0,     0,     0,  3090,     0,     0,     0,     0,
       0,     0,  1835,     0,     0,     0,     0,   593,     0,     0,
       0,  3290,   970,     0,     0,  3425,     0,     0,     0,  3426,
       0,     0,  1588,  1590,     0,   650,   650,   650,     0,   651,
       0,     0,  3291,     0,     0,     0,     0,     0,     0,     0,
       0,  3719,     0,     0,  1077,     0,  2249,     0,  3481,   568,
       0,     0,     0,     0,     0,     0,     0,  3361,  3312,     0,
       0,     0,     0,     0,  3807,     0,   628,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2117,  2118,  2119,
    2120,     0,  2880,     0,  2121,     0,     0,     0,   737,  3427,
       0,     0,   651,     0,     0,     0,  3340,  3341,     0,     0,
       0,     0,  2122,  2123,     0,  1281,     0,  1282,  2923,     0,
    1483,     0,     0,     0,  1484,     0,     0,     0,     0,     0,
       0,     0,   573,   737,   650,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   578,     0,     0,     0,
       0,     0,  2311,     0,  3459,  3470,     0,   780,  2124,  3428,
       0,     0,     0,     0,     0,     0,  1836,     0,     0,  3532,
       0,     0,     0,     0,     0,     0,  2966,     0,     0,  2967,
       0,  3703,     0,     0,  3390,  3390,     0,  2974,     0,  3551,
       0,     0,     0,  2125,  3718,     0,     0,     0,     0,  2117,
    2118,  2119,  2120,     0,  3562,     0,  2121,     0,     0,     0,
    3100,     0,     0,     0,  2987,  3575,     0,     0,     0,     0,
    2586,     0,     0,     0,  2122,  2123,     0,  3590,     0,     0,
       0,  3429,     0,     0,  2912,     0,     0,  3533,     0,     0,
       0,     0,  1834,   737,  2398,     0,     0,     0,     0,  -229,
       0,     0,     0,     0,     0,  1790,     0,     0,     0,  3609,
       0,     0,  3610,  3197,  3535,     0,  3538,     0,  -229,  3015,
    2124,     0,     0,     0,     0,     0,     0,     0,     0,   651,
    -229,   651,  3430,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   582,     0,  3617,     0,  -229,  3619,  3620,  3621,
    3622,     0,  2126,     0,     0,  2125,     0,  3628,   286,     0,
       0,  3041,     0,     0,     0,     0,  1856,     0,     0,  3606,
    3640,     0,     0,     0,     0,   650,  3649,  1871,     0,     0,
       0,     0,  3590,  3590,     0,     0,     0,     0,  1360,     0,
       0,     0,     0,     0,  2428,  1887,     0,     0,     0,     0,
       0,     0,  2127,  2428,     0,     0,  1485,  -229,     0,     0,
       0,     0,     0,  3618,     0,     0,     0,     0,  3675,  3676,
    3677,  3678,  3679,  3680,  3681,  3682,  3683,  3684,  3685,  3686,
    3687,  3688,  3689,  3690,  3691,  3692,  3693,     0,     0,     0,
     650,  2128,   650,     0,     0,     0,     0,     0,   636,  3590,
       0,     0,     0,     0,     0,     0,     0,     0,   584,     0,
       0,   636,  3661,     0,  2126,     0,     0,     0,     0,     0,
       0,  3668,     0,     0,  3108,  3722,     0,     0,     0,     0,
       0,  3115,  3116,  3117,  3118,     0,     0,  3723,     0,  3123,
       0,     0,     0,     0,  3649,   650,     0,   650,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1486,     0,
       0,     0,     0,     0,  2127,     0,     0,  3741,  3742,  2913,
       0,  2914,     0,     0,     0,   621,   621,     0,  2129,     0,
    2915,  2916,   588,     0,  2917,  2918,  3754,  3151,     0,     0,
       0,  3649,  3649,     0,     0,     0,     0,  3649,     0,  3796,
    3649,     0,     0,  2128,  3649,  3649,     0,     0,     0,     0,
     621,     0,     0,     0,     0,     0,     0,  3809,     0,     0,
    2130,     0,  1487,     0,     0,     0,     0,     0,   621,     0,
       0,     0,     0,   650,     0,     0,     0,     0,     0,   617,
       0,  3829,     0,     0,   644,     0,   970,   737,     0,  1488,
       0,   980,   980,     0,   644,     0,     0,     0,     0,     0,
       0,     0,   644,   593,     0,  3213,     0,     0,     0,     0,
       0,     0,     0,     0,  3876,  2131,     0,     0,     0,  3805,
    3240,   644,     0,     0,   621,     0,   980,     0,     0,     0,
    2129,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   980,     0,  3899,  3899,  -229,  3255,
       0,     0,     0,     0,  3257,     0,     0,  3838,     0,     0,
       0,  3904,     0,     0,     0,     0,     0,  3907,     0,     0,
       0,  2816,     0,     0,   286,     0,     0,  3910,  3914,     0,
       0,     0,     0,  3899,     0,     0,     0,  3877,  3878,     0,
       0,  3923,     0,     0,     0,     0,     0,     0,     0,  -229,
     980,     0,     0,     0,     0,     0,     0,     0,     0,  2020,
       0,  2021,  2022,     0,  -229,     0,     0,     0,     0,     0,
       0,   737,   737,   737,     0,     0,     0,  2131,   737,   737,
     737,  2132,     0,     0,   737,   737,  2133,   737,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   561,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   563,     0,     0,  3338,
       0,     0,  3930,     0,  2134,     0,     0,     0,     0,     0,
     617,   617,   617,   559,     0,     0,  2135,     0,   560,     0,
       0,     0,     0,     0,     0,     0,     0,   737,     0,  1063,
     737,     0,     0,     0,  3351,     0,     0,   737,   737,   737,
     737,  -229,     0,   737,     0,     0,     0,   644,  -229,     0,
       0,  2020,     0,  2021,  2022,     0,     0,   650,     0,   650,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  3381,     0,  2132,     0,     0,     0,     0,  2133,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   286,     0,     0,     0,   650,     0,     0,
       0,     0,     0,     0,     0,  2136,  -229,  2252,     0,     0,
     644,     0,     0,     0,  3437,     0,  2134,     0,     0,  -229,
       0,   644,     0,     0,     0,     0,     0,  3444,  2135,     0,
       0,  -229,     0,     0,     0,     0,     0,     0,  3480,  3529,
       0,     0,     0,     0,     0,   561,     0,   564,   565,   566,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   567,
     650,     0,     0,     0,   563,     0,     0,     0,  3549,     0,
       0,  3552,  3553,  3554,  3555,     0,     0,     0,     0,   559,
       0,     0,  2924,     0,   560,     0,   568,     0,  -229,  -229,
     644,     0,     0,     0,  -229,     0,     0,   644,     0,     0,
       0,     0,     0,     0,     0,     0,   617,     0,  -229,     0,
    3591,     0,     0,     0,     0,     0,     0,  2136,     0,  3598,
       0,     0,  3602,     0,     0,     0,     0,  -229,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   569,   607,     0,     0,     0,     0,     0,
     571,     0,   572,     0,     0,     0,     0,     0,     0,   573,
       0,     0,   574,   575,   576,     0,     0,     0,     0,     0,
     577,     0,     0,   578,     0,     0,     0,  -229,  1064,     0,
       0,  -229,  3623,  3624,  3625,  3626,  3627,     0,     0,  3629,
    3630,  3631,  3632,  3633,  3634,  3635,  3636,  3637,  3638,  3639,
     737,   737,  3641,     0,     0,     0,     0,     0,     0,     0,
    3653,   561,     0,     0,     0,   564,   565,   566,     0,     0,
       0,  3657,     0,  3658,   562,     0,     0,   567,   561,     0,
     563,     0,     0,  3666,     0,     0,     0,   650,     0,   650,
       0,     0,     0,     0,     0,     0,     0,   563,   580,     0,
     617,   617,   617,     0,   568,     0,     0,     0,     0,     0,
     617,   617,   617,  3036,     0,     0,     0,     0,     0,     0,
    1522,     0,   581,     0,   617,     0,     0,   617,     0,     0,
       0,     0,     0,     0,  1065,     0,   617,   617,   617,   617,
     617,     0,     0,     0,     0,  3529,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   569,   570,   617,     0,     0,     0,     0,   571,   582,
     572,     0,     0,     0,     0,     0,     0,   573,     0,     0,
     574,   575,   576,     0,     0,     0,     0,     0,   577,     0,
       0,   578,     0,     0,     0,     0,   970,     0,     0,     0,
       0,  3746,  3747,     0,  3752,  1522,   579,     0,     0,     0,
       0,     0,     0,   583,     0,     0,     0,     0,   286,     0,
       0,  3755,     0,     0,     0,     0,  3759,  3760,     0,     0,
       0,   564,   565,   566,  3106,  3106,     0,     0,     0,     0,
       0,  3804,     0,   567,  3806,  1066,     0,  3808,   564,   565,
     566,  2619,     0,     0,     0,     0,     0,     0,     0,     0,
     567,     0,     0,     0,     0,     0,   580,     0,     0,     0,
     568,   737,     0,     0,  2628,  2628,     0,     0,     0,  1067,
       0,  2639,  3214,  3215,  3216,   584,   620,   620,     0,     0,
     581,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     585,   586,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   620,     0,     0,     0,     0,     0,   569,   570,     0,
       0,     0,     0,     0,   571,     0,   572,   582,     0,   620,
       0,     0,     0,   573,   569,  3154,   574,   575,   576,     0,
       0,     0,     0,     0,   577,     0,     0,   578,     0,   588,
    3155,   609,     0,   574,   575,   576,     0,   286,     0,     0,
       0,   577,   579,   589,     0,     0,     0,     0,     0,     0,
       0,   583,     0,   286,     0,   286,   286,     0,     0,     0,
       0,   286,  3217,  3218,  3219,   620,     0,   590,     0,     0,
     737,     0,     0,     0,   561,   591,     0,     0,     0,     0,
       0,   617,     0,     0,     0,     0,     0,     0,     0,   628,
       0,     0,     0,   563,   644,   592,     0,  1522,     0,     0,
     284,     0,   580,     0,     0,     0,     0,     0,     0,     0,
     593,     0,     0,     0,     0,     0,     0,     0,     0,   580,
       0,     0,     0,   584,     0,     0,   581,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   286,   286,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   585,   586,
       0,   286,     0,   644,     0,     0,     0,  3294,     0,     0,
    3294,     0,     0,     0,     0,     0,     0,     0,     0,  3294,
    3294,  3294,  3302,   582,  2798,   617,     0,   286,     0,     0,
       0,   737,     0,     0,  1068,     0,  1069,  1522,     0,     0,
       0,     0,     0,   628,     0,  1070,  1071,   588,     0,  1072,
    1073,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   589,     0,     0,     0,   286,   286,   583,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   583,   590,     0,  3220,  3221,     0,
       0,     0,     0,   591,   564,   565,   566,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   567,     0,     0,     0,
    3222,  2852,     0,   592,     0,     0,   628,     0,   284,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   593,     0,
       0,     0,     0,  3223,     0,   286,     0,     0,     0,   584,
       0,     0,     0,   286,   286,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   628,     0,  3224,     0,     0,     0,
       0,     0,     0,     0,   585,   586,     0,     0,     0,   286,
       0,     0,     0,     0,     0,     0,     0,   587,     0,     0,
       0,   585,   586,     0,     0,     0,     0,  3441,     0,     0,
     569,     0,     0,     0,     0,   644,     0,     0,     0,   644,
       0,     0,     0,     0,     0,     0,   828,     0,     0,   574,
     575,   576,   644,   588,     0,     0,     0,   577,     0,     0,
       0,     0,   286,   286,     0,   286,     0,   589,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   589,     0,     0,  3225,     0,     0,
       0,   590,     0,     0,     0,     0,     0,     0,     0,   591,
       0,   644,     0,     0,     0,     0,     0,     0,   590,     0,
       0,     0,     0,     0,     0,     0,   591,  3226,     0,   592,
    3227,  3228,     0,     0,   284,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   593,   580,   592,     0,     0,     0,
       0,   284,     0,     0,  3229,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  3009,  3010,  3011,  3012,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  3016,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  3029,  3030,     0,
   -2509,     0,     0,     0,     0,  1089,     0,     0, -2509,     0,
       0,  1090,     0,     0,     0,     0,     0,     0,     0,   737,
       0,     0, -2509,     0,     0,     0,   737,     0,     0,     0,
     737,     0,     0,   737,     0,     0,     0,     0,  1091, -2509,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     583,     0,     0,     0,     0,     0,     0,     0, -2509,  1092,
    1093,  1094,  1095,  1096,  1097,  1098,  1099,     0,     0,     0,
   -2509, -2509, -2509,  1100, -2509,     0,     0,  1101,     0,     0,
   -2509,     0,     0,     0,     0,     0,     0,     0,  1102,  1103,
    1104,  1105, -2509, -2509, -2509,     0,     0,     0,  3230,   644,
     644,   644,     0,     0,  1106,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1107,     0,     0,  1108,  1109,  1110,
   -2509,  1111,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   737,     0,
       0,   737,     0,     0,     0,     0,     0,   585,   586,     0,
       0,  1112,     0,     0,     0,     0,     0,     0,     0,  1113,
    1114,  1115,  1116,  1117,  1118,  1119,  1120,     0, -2509,     0,
    1121,  1122,     0,     0,     0,     0,     0,     0,     0,     0,
       0, -2509,     0,     0,     0,     0,  3150,     0,     0,   617,
     617,     0,     0,     0,     0,  1123,  1124,     0,   644,  1125,
    1126,     0, -2509, -2509,  1127, -2509, -2509,     0,     0,     0,
     589,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1128,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   590,     0,     0,     0,     0,     0,
       0,     0,   591,     0, -2509,  1129,     0,     0,     0,  1130,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   -2509,     0,   592,     0,  1131,     0,     0,   284,  1132,  1133,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1134,  1135,  1136,  1137,  1138,     0,     0,  1139,     0,     0,
       0, -2509, -2509, -2509,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -2509,     0,     0,     0,     0, -2509,     0,
       0,     0,     0,     0,     0,     0,  1140,  1141,  1142,  1143,
       0,     0,     0,     0,     0,  1144,  1145,     0,  1146,     0,
    1147,  1148,  1149, -2509, -2509,  1150,     0,  1151,     0, -2509,
   -2509,  1152,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   617,     0,   617,     0,     0,     0,
    1153,  1154,     0,     0,     0,     0,     0,     0,     0,  1155,
    1156,  1157,  1158,  1159,  1160,     0,     0,     0,     0,   644,
     617,     0,  1161,     0,     0,     0,  1162,     0,     0,     0,
    1164, -2509,     0, -2509,     0,     0,     0,     0,     0,     0,
       0,   617,     0,     0,     0,     0,     0,     0, -2509,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0, -2509,
   -2509, -2509,     0,     0,  1165,     0,     0,     0,  1166,  1167,
    1168,  1169,  1170,     0,     0,     0,     0,     0,     0,  1171,
       0,     0,     0,     0,   644, -2509,   644,     0,     0,     0,
       0,     0,     0,  1172,     0,     0,     0,     0,     0,     0,
       0,  1173,     0,     0,     0,     0, -2509, -2509, -2509,     0,
   -2509,     0, -2509, -2509,     0, -2509, -2509, -2509,     0,  1261,
   -2509,     0, -2509, -2509, -2509, -2509,     0,     0,     0,     0,
       0,     0,     0,     0,  1174,  1175,     0,     0, -2509,   644,
       0,   644,     0,     0,     0,     0, -2509,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0, -2509, -2509,
       0,     0,     0,     0,     0,  1176, -2509,     0,  1177,     0,
       0, -2509,     0,     0,     0,     0,     0,     0,  1178,     0,
       0,     0,     0,  1179,     0,     0,     0,  1180,  1181,  1182,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1183,     0,     0,  1184,  1185,     0,     0,     0,  1186,
       0,     0,     0, -2509,   617,     0,     0, -2509,     0,     0,
       0,  1187,  1188,  1189,     0,     0,     0,   644,     0,     0,
       0,  1190,  1191,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1192,  1193,
    1194,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0, -2509, -2509, -2509,     0,     0,     0,  1195,  1196,
    1197,  1198, -2509,  1199,     0,     0,     0,  1200,  1201,     0,
       0,     0,     0,     0,     0,  1202,  1203,     0,     0,     0,
       0,     0,     0,  1204,  1205,  1206, -2509,     0,     0,     0,
       0,  1207,     0,     0,     0,     0,  1208,     0,     0, -2509,
   -2509, -2509, -2509,     0,     0,     0,     0,     0,   147,     0,
       0,     0,   148,     0,  1209,     0,   149,     0,     0, -2509,
   -2509,     0,     0,   150,     0,     0,     0,  1210,  1211,     0,
   -2509,   151,     0,  1212,     0,     0,     0,     0,  1262,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1213,  1214,     0,     0, -2509,     0,     0,
       0,  1215,     0,     0,     0,     0,     0,     0,     0,  1216,
   -2509, -2509,     0,     0,     0,  1217,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   152,   153,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1218,     0, -2509,     0,     0,  1458,  1219,     0,     0,
       0, -2509,     0, -2766, -2766, -2766, -2766,   154, -2509,     0,
   -2766,     0,     0,     0,     0,     0,     0,     0, -2509,     0,
       0,     0, -2509, -2509, -2509,     0,     0,   155, -2766, -2766,
       0,   156,  1220,     0,     0,     0, -2509, -2509,  1221,  1222,
   -2509, -2509,  1223, -2509,     0,   157,     0,     0,  1224, -2509,
       0,     0,     0,     0,     0,     0,     0,     0,  1225,  1226,
       0,   644,     0,   644,     0,     0,     0,     0,     0, -1268,
       0,     0,     0, -2621, -2766,     0,     0, -2621,     0,     0,
       0,     0,     0,   158,     0,     0,     0,     0,     0,   159,
       0,   160,     0,     0,   161,     0,     0,     0,   162,     0,
       0,   644,     0,     0,     0,     0,     0,     0,     0, -2766,
       0,     0,     0,     0,     0,   163, -1268,     0, -1268, -1268,
   -1268, -1268, -1268,     0, -1268, -1268, -1268, -1268,     0, -1268,
       0, -1268, -1268,     0, -1268, -1268, -1268, -1268, -1268, -1268,
   -1268, -1268, -1268, -1268,     0,     0,   164,     0,     0,     0,
       0,     0,     0,     0, -1268,     0,     0,     0,     0, -1268,
       0,     0,   165,     0,   644,     0, -1268,     0,     0,     0,
       0,   166,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   147,     0,     0,     0,   148,     0,     0,     0,
     149,     0,     0,     0,     0,     0,     0,   150,     0,     0,
       0,     0,     0,     0,     0,   151,     0,     0,     0,     0,
       0,     0,     0,     0,   167,     0,     0,     0, -2766,     0,
       0,     0,   168,     0,     0,   169,     0,     0,     0,     0,
     170,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   171,     0,     0,     0,
       0,     0,   152,   153,   172,     0,   173,     0, -2766,   174,
       0,     0,   175,     0,     0,     0,     0, -1268,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   154,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0, -2766,     0,     0,
       0,   155,     0,     0,     0,   156,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   157,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   644,     0,   644,     0,     0,   176,     0,     0,     0,
       0,     0,   177,     0,   178,     0,     0,   179,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   158,     0,     0,
     180,     0,     0,   159, -2621,   160,     0,     0,   161, -1268,
       0,     0,   162,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0, -2766,     0, -1268, -1268, -1268,   163,
   -1268, -1268, -1268, -1268,     0,     0,     0,     0,     0,     0,
       0,     0,  1461,     0,     0,     0,     0,     0,     0,     0,
     181,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     164,     0,     0,     0,     0,     0, -2766,     0,     0,     0,
     182,     0,     0,     0,     0,     0,   165,     0,     0,     0,
       0,     0,     0,     0,     0,   166,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   183,     0,     0,
       0,     0,     0,     0,     0,     0,   184,     0,   185,   186,
       0, -2766,     0,     0,     0,     0,     0,     0,   167,     0,
       0,     0,     0,     0,   187,     0,   168,     0,     0,   169,
       0,     0,     0, -2621,   170,     0,     0,   188,     0,     0,
       0,     0,   189,     0,     0,     0,     0,     0,     0,   190,
       0,     0,     0,     0,     0,   191,     0,     0,     0,     0,
     171,     0,     0,     0,     0,     0,     0,     0,   172,     0,
     173,     0,     0,   174,   192,     0,   175,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   193,     0,
       0,     0,     0,     0,   194,     0,     0,     0,   147,     0,
       0,     0,   148,     0,     0, -2766,   149, -2766, -2766,     0,
   -1268,     0,   195,   150,     0,     0,     0,     0,     0,     0,
       0,   151,     0,     0,     0,     0,   196, -2766,     0,     0,
       0,     0, -2766,     0,     0,     0,   197,   198,     0,     0,
       0,     0,     0,     0,   199,     0,     0,   200,     0,     0,
     176,     0,     0,     0,     0,     0,   177,     0,   178,     0,
     201,   179,     0,     0,     0,     0,     0,     0,     0,     0,
   -2766,     0,     0,     0,   180,     0,     0,     0,   152,   153,
       0, -1268, -2766,     0,     0,     0,     0,     0,     0,     0,
       0,     0, -1268,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   203,     0,     0,     0,     0,   154,     0,     0,
       0,     0,     0,   204,     0,     0,     0,     0,     0,   205,
       0,     0,     0,     0,   181,     0,     0,   155,     0,     0,
       0,   156,     0,     0,     0,     0,     0,     0,     0,     0,
     206,     0,     0,     0,   182,   157,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0, -1268,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   207,     0,     0,
       0, -2766,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   183,     0,   158,  1574,     0,     0,     0,     0,   159,
     184,   160,   185,   186,   161,     0,     0,     0,   162,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   187,     0,
       0,     0,     0,     0,     0,   163,     0,     0,     0,     0,
       0,   188,   888,     0,     0,     0,   189,     0,     0,     0,
       0,     0,     0,   190,     0,     0,     0,     0,     0,   191,
       0,     0,     0,     0,     0,     0,   164,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   192,     0,
       0,     0,   165,     0,     0,     0,     0,     0,     0,     0,
       0,   166,   193,     0,     0,     0,     0,     0,   194,     0,
       0,     0,   147,     0,     0,     0,   148,     0,     0,     0,
     149,     0,     0,     0,     0,     0,   195,   150,     0,     0,
       0,     0,     0,     0,     0,   151,     0,     0,     0,     0,
     196,     0,     0,     0,   167,     0,     0,     0,     0,     0,
     197,   198,   168,     0,     0,   169,     0,     0,   199,     0,
     170,   200,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   201,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   171,     0,     0,     0,
       0,     0,   152,   153,   172,   202,   173,     0,     0,   174,
       0,     0,   175,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   203,     0,     0,     0,
       0,   154,     0,     0,     0,     0,     0,   204,     0,     0,
       0,     0,     0,   205,     0,     0,     0,     0,     0,     0,
       0,   155,     0,     0,     0,   156,     0,     0,     0,     0,
       0,     0,     0,     0,   206,     0,     0,     0,     0,   157,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   176,     0,     0,     0,
       0,   207,   177,     0,   178,     0,     0,   179,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   158,     0,     0,
     180,     0,     0,   159,     0,   160,     0,     0,   161,     0,
       0,     0,   162,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -1473,     0,     0,     0,     0, -1473,   163,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     181,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     164,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     182,     0,     0,     0,     0,     0,   165,     0,     0,     0,
       0,     0,     0,     0,     0,   166,     0,     0,     0,     0,
       0,  1449,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   183,     0,     0,
       0,     0,     0,     0,     0,     0,   184,     0,   185,   186,
       0,     0,     0,     0,     0,     0,     0,     0,   167,     0,
       0,     0,     0,     0,   187,     0,   168,     0,     0,   169,
       0,     0,     0,     0,   170,     0,     0,   188,     0,     0,
       0,     0,   189,     0,     0, -1473,     0,     0,     0,   190,
   -2629,     0,     0,     0,     0,   191,     0,     0,     0,     0,
     171,     0,     0,     0, -1473,     0,     0,     0,   172,     0,
     173,     0,     0,   174,   192,     0,   175,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   193,     0,
       0,     0,     0,     0,   194,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   195,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   196,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   197,   198,     0,     0,
       0,     0,     0,     0,   199,     0,     0,   200,     0,     0,
     176,     0,     0,     0,     0,     0,   177,     0,   178,     0,
     201,   179,     0,     0,     0,     0,     0,     0,   617,   617,
       0,     0,     0,     0,   180,     0,     0,     0,     0,     0,
       0,  1369,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   203,   617,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   204,     0, -1473, -1473, -1473,     0,   205,
       0,   617,     0,     0,   181,     0,     0, -1473,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   328,     0,
     206,   559,   329,     0,   182,     0,   560,     0,     0,   330,
       0,     0,     0,     0, -1473,     0,     0,   331,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   207,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   617,     0,     0,
       0,   183,     0,     0,     0,     0,     0,     0,     0,     0,
     184,     0,   185,   186,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   187,     0,
       0, -1473, -1473,     0,   332,   333,     0,     0, -1473,     0,
   -1473,   188,     0,     0,     0,     0,   189, -1473,     0,     0,
   -1473, -1473, -1473,   190,     0,     0,     0,     0, -1473,   191,
       0, -1473,     0,   334,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0, -1473,     0,   192,     0,
       0,     0,     0,   335,     0,     0,     0,   336,     0,     0,
       0,     0,   193,   561,     0,     0,     0,     0,   194,     0,
       0,   337,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   563,     0,     0,     0,   195,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     196,     0,     0,     0,     0,     0, -1473,     0,     0,   338,
     197,   198,     0, -1473,     0,     0,     0,     0,   199,     0,
     339,   200,     0,     0,   340,     0,     0,     0,     0,     0,
   -1473,     0,     0,     0,   201,     0,     0,     0,     0,     0,
       0,     0,   341,     0,     0,   342,   343,   344,   345,     0,
     346,   347,   348,   349,     0,   350,   351,   352,   353,     0,
     354,   355,   356,   357,   358,   359,   360,   361,   362,   363,
     559,     0,   364,     0,     0,   560,   203, -1473,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   204,   365,     0,
       0,     0,     0,   205,     0, -2629,     0,   366, -1565,     0,
       0,     0,     0,     0,     0, -1565, -1565, -1565, -1565,     0,
       0,     0, -1565,     0,   206,     0,     0,     0,     0,     0,
       0, -1473,     0,     0,     0,     0,     0,   665,     0,     0,
   -1565, -1565,     0,   564,   565,   566,     0,     0,     0,     0,
     367,   207,     0,     0,     0,   567,     0,     0,   368,     0,
       0,   369,     0,     0,     0,     0,   370,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   568,     0,     0, -1565, -1565,     0,     0, -1565,
       0,     0,   371,     0,   666, -1473,     0,     0,     0,     0,
     372,     0,   373, -1473,     0,   374,     0,     0,   375,     0,
       0,     0,   561,     0,     0,     0,     0,     0,     0,     0,
       0, -1565,     0,     0,     0,     0,     0,     0, -1473, -1473,
       0,   563,     0,     0,     0,     0,     0,     0,     0,   569,
     570,     0,     0,     0,     0,     0,   571,     0,   572,     0,
       0,     0,     0,     0,     0,   667,     0,     0,   574,   575,
     576,     0,     0,     0,     0,     0,   577,     0,     0,   578,
       0,     0,     0,     0,     0,     0,     0, -1473,     0,     0,
       0,   668,   376,     0,   579,     0,     0,     0,   377,     0,
     378, -1473,     0,   379,     0,     0,     0,     0,     0,     0,
   -1473,     0,     0,     0,     0,     0,   380,     0,     0,     0,
       0,     0,     0,     0,     0, -1473,     0,     0,     0,     0,
       0,     0,     0, -1473,     0,     0,     0, -2629,     0,     0,
   -1565,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -1473,   580,     0,     0,     0, -1473,     0,
       0,   669,     0,     0,     0,     0,   381,     0, -1473,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   581,     0,
       0,     0,     0,     0,     0,     0,   382,     0,     0,     0,
   -1565,     0,   564,   565,   566,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   567,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   582,     0,     0,     0, -1565,
       0,   568,   383,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     384,     0,     0,   559,     0,     0,     0,     0,   560,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   385,   583,
       0,     0,     0,     0,     0,   386,     0,     0,     0,     0,
       0,   387,     0,     0,     0,     0,     0,     0,   569,   570,
       0,     0,     0,     0,     0,   571, -1565,   572,     0,     0,
     388,     0,     0,     0,   573,     0,     0,   574,   575,   576,
       0,     0,     0,     0,   559,   577, -1565,     0,   578,   560,
     389,     0, -2667, -2667, -2667,     0,     0,     0,     0,     0,
    1063,     0,     0,   579, -1565,     0,     0,     0,   390,     0,
       0,   584,     0,     0,     0,   670,     0,     0,     0,     0,
       0,     0,   391,     0,     0,     0,     0,     0, -1565,     0,
       0,     0,   392,   393,     0,     0,   585,   586,     0,   671,
     394,     0,     0,   395,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   561,   396,     0,     0,     0,
       0,     0,     0,   580,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   563,     0,     0,   397,     0,     0,
   -2667,   672,     0, -1565,     0,   588,     0,   581,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   398,   589,
       0,     0,     0,     0,     0, -1565,     0,     0,     0,   399,
       0,     0,     0,     0,     0,   400,   561,     0,     0,     0,
       0,     0,     0,   590,     0,     0,     0,     0,     0,     0,
       0,   591,     0,     0,   582,   563,     0,     0,   559,     0,
       0,     0,     0,   560,     0,     0,     0,     0,     0,     0,
       0,   592,     0,     0,   673,     0,   284,     0,     0,     0,
       0,     0,     0,   401,     0,     0,   593,     0,     0,     0,
       0,     0,     0,     0, -2766,     0,     0, -1565,   583, -1565,
   -1565,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0, -1565,
       0,     0,     0,     0, -1565,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   564,   565,   566,     0,     0,
       0,     0,     0, -2667,     0,     0,     0,   567,     0,   425,
       0,     0, -1565,     0,     0,     0,     0,     0,     0,  1064,
     584,     0,     0,     0, -1565,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   568,     0,     0,     0,     0,     0,
     561,     0,     0,     0,     0,   585,   586,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   564,   565,   566,   563,
       0,     0,     0,     0,     0,     0,     0,     0,   567,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0, -2667,
   -2667,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   569,   570,     0,   588,   568,     0,     0,   571,     0,
     572,     0,     0,     0,     0,     0,     0,   573,   589,     0,
     574,   575,   576, -1565,     0,     0,     0,     0,   577,     0,
       0,   578,     0,     0,     0,  1065, -1565,     0,     0,     0,
       0,     0,   590,     0,     0,     0,   579,     0,     0,     0,
     591,     0,     0,     0,     0,     0,     0,  1852,     0,     0,
       0,     0,   569,   570, -2667, -2667,     0,     0,     0,   571,
     592,   572,     0,     0, -1565,   284,     0,     0,   573,     0,
       0,   574,   575,   576, -2766,   593,     0,     0,     0,   577,
       0,     0,   578,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   580,   579,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   559,     0,
     564,   565,   566,   560, -2667,     0,     0,     0,     0,     0,
     581,     0,   567,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1066,     0,     0,   559,
       0,     0,     0,     0,   560,     0, -2667,     0,     0,   568,
       0,     0,     0,     0,     0, -2667,     0,   580,     0,     0,
       0,     0,     0,     0, -2667,     0,     0,   582,     0,     0,
    1067,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   581,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0, -2667,     0,     0,     0,     0,   569,   570,     0,   426,
       0,   583,     0,   571,     0,   572,     0,     0,     0,     0,
       0,     0,   573,     0,     0,   574,   575,   576,   582,     0,
       0,     0,     0,   577,     0,     0,   578,     0,     0,     0,
     561,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   579,     0,   810,     0,     0,     0,     0,     0,   563,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   561,   583,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   584,     0,     0,     0,     0,     0,     0,
     563,     0,     0,   559,     0,     0,     0,     0,   560,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   585,   586,
       0,   580,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   581,     0,     0,     0,     0,
       0,     0,     0,     0,   584,     0,   559,     0,     0,     0,
       0,   560,     0,     0,     0,     0,     0,   588,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   585,
     586,   589,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   582,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   590,     0,     0,     0,     0,
       0,     0,     0,   591,     0,  1068,     0,  1069,     0, -2667,
     564,   565,   566, -2667,     0, -2667,  1070,  1071,   588,     0,
    1072,  1073,   567,   592,     0,   561,   583,   888,   284,     0,
       0,     0,   589,     0,     0,     0,     0,     0,   593,     0,
       0,   564,   565,   566,   563,     0,     0,     0,     0,   568,
       0,     0,   559,   567,     0,     0,   590,   560,     0,     0,
       0,     0,     0,     0,   591,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   561,     0,
     568,     0,     0,     0,   592,     0,     0,     0,     0,   284,
    1853,     0,     0,     0,     0,     0,     0,   563,   584,   593,
       0,     0,     0,     0,     0,     0,   569,   570,     0,     0,
       0,     0,     0,   571,     0,   572,     0,     0,     0,     0,
       0,     0,   573,   585,   586,   574,   575,   576,     0,     0,
       0,     0,     0,   577,     0,     0,   578,   569,   570,     0,
       0,     0,     0,     0,   571,     0,   572,   560,     0,     0,
       0,   579,     0,   573,     0,     0,   574,   575,   576,     0,
       0,     0,     0,     0,   577,  1854,     0,   578,     0,     0,
       0,     0,   588,     0,     0,     0,  1855,     0,     0,     0,
       0,     0,   579,     0,   561,     0,   589,   559,     0,     0,
       0,     0,   560,     0,     0,   564,   565,   566,     0,     0,
       0,     0,     0,   563,     0,     0,     0,   567,     0,     0,
     590,   580,     0,     0,     0,     0,     0,     0,   591,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   568,   581,     0,     0,   592,     0,
       0,     0,   580,   284,     0,     0,     0,     0,   564,   565,
     566,     0,     0,   593,  1365,     0,     0,     0,     0,     0,
     567,     0,     0,     0,     0,     0,   581,     0,     0,     0,
       0,     0,     0,     0,   561,     0,     0,     0,     0,     0,
       0,     0,   582,     0,     0,     0,     0,   568,     0,     0,
       0,   569,   570,   563,     0,     0,     0,     0,   571,     0,
     572,     0,     0,     0,     0,     0,     0,   573,     0,     0,
     574,   575,   576,   582,     0,     0,     0,     0,   577,   561,
       0,   578,     0,     0,     0,     0,   583,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   579,     0,   563,     0,
       0,     0,     0,     0,   569,   570,     0,     0,     0,     0,
       0,   571,     0,   572,   564,   565,   566,   583,     0,     0,
     573,     0,     0,   574,   575,   576,   567,     0,     0,     0,
       0,   577,     0,     0,   578,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   579,
       0,     0,     0,   568,     0,  1521,   580,     0,   584,     0,
     560,     0,     0,   669,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     581,     0,     0,   585,   586,     0,     0,     0,     0,   584,
       0,     0,     0,     0,     0,     0,   811,     0,     0,     0,
       0,     0,     0,     0,   564,   565,   566,     0,     0,   580,
     569,   570,     0,     0,   585,   586,   567,   571,     0,   572,
       0,     0,     0,     0,     0,     0,   573,   582,     0,   574,
     575,   576,   588,   581,     0,     0,     0,   577,     0,     0,
     578,     0,     0,   568,     0,     0,   589,     0,   968,   564,
     565,   566,     0,     0,     0,   579,     0,     0,     0,     0,
       0,   567,     0,   588,     0,     0,     0,     0,     0,     0,
     590,   583,     0,     0,     0,     0,     0,   589,   591,     0,
     582,     0,     0,     0,     0,     0,     0,   561,   568,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   592,     0,
     569,   590,     0,   284,     0,     0,   563,     0,     0,   591,
       0,     0,     0,   593,     0,   580,   573,     0,     0,   574,
     575,   576,     0,     0,   583,     0,     0,   577,     0,   592,
     578,     0,     0,     0,   284,     0,     0,     0,     0,   581,
       0,     0,     0,   584,   593,   569,   570,     0,     0,     0,
       0,     0,   571,     0,   572,     0,     0,     0,     0,     0,
       0,   573,     0,     0,   574,   575,   576,     0,   585,   586,
       0,     0,   577,     0,     0,   578,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   582,     0,     0,     0,
     579,     0,     0,     0,     0,     0,   584,     0,     0,     0,
       0,     0,     0,     0,     0,   580,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   588,     0,     0,
       0,   585,   586,     0,     0,     0,     0,     0,     0,     0,
     583,   589,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     580,     0,     0,     0,     0,   590,     0,   564,   565,   566,
       0,     0,     0,   591,     0,     0,     0,     0,     0,   567,
     588,     0,     0,     0,   581,     0,   582,     0,     0,     0,
       0,     0,     0,   592,   589,     0,     0,     0,   284,     0,
       0,     0,     0,     0,  2637,     0,   568,     0,   593,     0,
       0,     0,   584,     0,     0,     0,     0,     0,   590,     0,
       0,     0,     0,     0,     0,     0,   591,     0,     0,     0,
     583,   582,     0,     0,     0,     0,     0,   585,   586,     0,
       0,     0,     0,     0,     0,     0,   592,     0,     0,     0,
       0,   284,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   593,     0,   569,   570,     0,     0,     0,     0,     0,
     571,     0,   572,     0,     0,   583,     0,     0,     0,   573,
       0,     0,   574,   575,   576,     0,   588,     0,     0,     0,
     577,     0,     0,   578,     0,     0,     0,     0,     0,     0,
     589,     0,   584,     0,     0,     0,     0,     0,   579,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   590,     0,     0,   585,   586,     0,
       0,     0,   591,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   584,     0,     0,
       0,     0,   592,     0,     0,     0,     0,   284,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   593,   580,     0,
       0,     0,   585,   586,     0,     0,   588,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     589,     0,   581,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   590,     0,     0,     0,     0,     0,
       0,   588,   591,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   589,     0,     0,     0,   582,
       0,     0,   592,     0,     0,     0,     0,   284,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   593,     0,   590,
       0,     0,     0,     0,     0,     0,     0,   591,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1089,
       0,     0,     0,   583,     0,  1090,     0,   592,     0,     0,
       0,     0,   284,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   593,     0,     0,     0,     0,     0,     0,     0,
       0,  -920,  1091,     0,     0,     0,  -920,  -920,  -920,  -920,
       0,     0,     0,  -920,  -920,  -920,  -920,  -920,  -920,     0,
    -920,  -920,     0,  1092,  1093,  1094,  1095,  1096,  1097,  1098,
    1099,  -920,  -920,     0,     0,     0,     0,  1100,     0,     0,
       0,  1101,     0,     0,     0,   584,     0,     0,     0,  -920,
       0,     0,  1102,  1103,  1104,  1105,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1106,  -920,
     585,   586,     0,     0,     0,     0,  -920,  -920,  1107,     0,
    -920,  1108,  1109,  1110,     0,  1111,     0,     0,     0,     0,
       0,     0,     0,     0,  -920,     0,  -920,  -920,  -920,  -920,
    -920,  -920,  -920,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1112,     0,     0,     0,   588,
    -920,     0,     0,  1113,  1114,  1115,  1116,  1117,  1118,  1119,
    1120,     0,     0,   589,  1121,  1122,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   590,  -920,  1123,
    1124,     0,     0,  1125,  1126,   591,     0,     0,  1127,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   592,     0,  1128,     0,     0,
     284,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     593,     0,     0,     0,     0,     0,     0,     0,     0,  1129,
       0,     0,     0,  1130,     0,     0,     0,     0,     0,     0,
       0,  -920,     0,     0,     0,     0,     0,     0,  1131,     0,
       0,     0,  1132,  1133,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1134,  1135,  1136,  1137,  1138,     0,
       0,  1139,     0,     0,     0,     0,     0,     0,  -920,  -920,
    -920,  -920,  -920,     0,     0,  -920,  -920,     0,     0,     0,
       0,  -920,     0,     0,     0,     0,     0,     0,     0,  -920,
       0,  -920,     0,     0,     0,     0,     0,     0,  -920,     0,
    1140,  1141,  1142,  1143,     0,     0,     0,     0,  -920,  1144,
    1145,  -920,  1146,     0,  1147,  1148,  1149,     0,     0,  1150,
    -920,  1151,     0,     0,     0,  1152,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -920,     0,     0,     0,     0,
       0,     0,     0,     0,  1153,  1154,     0,     0,     0,     0,
       0,     0,  -920,  1155,  1156,  1157,  1158,  1159,  1160,     0,
       0,  -920,     0,     0,     0,     0,  1161,     0,     0,     0,
    1162,     0,     0,  -920,  1164,     0,  -920,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -920,     0,     0,
       0,  -920,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -920,  1165,     0,
       0,     0,  1166,  1167,  1168,  1169,  1170,     0,     0,     0,
       0,     0,     0,  1171,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -920,     0,     0,     0,  1172,     0,     0,
       0,     0,     0,     0,     0,  1173,     0,     0,  -920,     0,
       0,     0,     0,     0,     0,  -920,     0,     0,     0,     0,
       0,     0,     0,  1261,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1174,  1175,
       0,     0,     0,     0,     0,     0,     0,  -920,     0,  -920,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -920,  -920,     0,     0,     0,  1176,
       0,     0,  1177,     0,     0,     0,     0,     0,     0,  -920,
       0,     0,  1178,     0,  -920,     0,     0,  1179,     0,     0,
       0,  1180,  1181,  1182,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -920,  1183,     0,  -920,  1184,  1185,
       0,     0,     0,  1186,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1187,  1188,  1189,     0,     0,
       0,     0,     0,     0,     0,  1190,  1191,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -920,     0,
       0,     0,  1192,  1193,  1194,     0,     0,     0,  -920,     0,
    -920,  -920,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1195,  1196,  1197,  1198,     0,  1199,     0,     0,
       0,  1200,  1201,     0,     0,     0,     0,     0,     0,  1202,
    1203,     0,     0,  -920,     0,     0,     0,  1204,  1205,  1206,
       0,     0,     0,     0,     0,  1207,     0,     0,     0,     0,
    1208,     0,     0,     0,     0,     0,     0,  -920,     0,  -920,
    -920,  -920,     0,  -920,     0,     0,     0,     0,  1209,     0,
       0,     0,     0,     0,     0,  -920,     0,     0,     0,     0,
       0,  1210,  1211,     0,     0,     0,     0,  1212,     0,     0,
       0,     0,  1262,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -920,  1213,  1214,     0,
       0,     0,     0,     0,     0,  1215,     0,     0,     0,     0,
       0,     0,     0,  1216,     0,     0,     0,     0,     0,  1217,
    -920,     0,     0,     0,     0,  -920,     0,     0,     0,     0,
       0,  -920,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -920,  1218,     0,     0,     0,     0,
       0,  1219,     0,     0,  -920,     0,     0,     0,     0,  -920,
    -920,  -920,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1089,     0,     0,  -920,     0,     0,  1090,     0,     0,
       0,     0,  -920,     0,     0,  -920,  1220,     0,     0,     0,
       0,     0,  1221,  1222,     0,     0,  1223,     0,     0,     0,
       0,     0,  1224,  -921,  1091,     0,     0,     0,  -921,  -921,
    -921,  -921,  1225,  1226,     0,  -921,  -921,  -921,  -921,  -921,
    -921,     0,  -921,  -921,     0,  1092,  1093,  1094,  1095,  1096,
    1097,  1098,  1099,  -921,  -921,     0,     0,     0,     0,  1100,
       0,     0,     0,  1101,     0,     0,     0,     0,     0,     0,
       0,  -921,     0,     0,  1102,  1103,  1104,  1105,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1106,  -921,     0,     0,     0,     0,     0,     0,  -921,  -921,
    1107,     0,  -921,  1108,  1109,  1110,     0,  1111,     0,     0,
       0,     0,     0,     0,     0,     0,  -921,     0,  -921,  -921,
    -921,  -921,  -921,  -921,  -921,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1112,     0,     0,
       0,     0,  -921,     0,     0,  1113,  1114,  1115,  1116,  1117,
    1118,  1119,  1120,     0,     0,     0,  1121,  1122,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -921,  1123,  1124,     0,     0,  1125,  1126,     0,     0,     0,
    1127,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1128,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1129,     0,     0,     0,  1130,     0,     0,     0,     0,
       0,     0,     0,  -921,     0,     0,     0,     0,     0,     0,
    1131,     0,     0,     0,  1132,  1133,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1134,  1135,  1136,  1137,
    1138,     0,     0,  1139,     0,     0,     0,     0,     0,     0,
    -921,  -921,  -921,  -921,  -921,     0,     0,  -921,  -921,     0,
       0,     0,     0,  -921,     0,     0,     0,     0,     0,     0,
       0,  -921,     0,  -921,     0,     0,     0,     0,     0,     0,
    -921,     0,  1140,  1141,  1142,  1143,     0,     0,     0,     0,
    -921,  1144,  1145,  -921,  1146,     0,  1147,  1148,  1149,     0,
       0,  1150,  -921,  1151,     0,     0,     0,  1152,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -921,     0,     0,
       0,     0,     0,     0,     0,     0,  1153,  1154,     0,     0,
       0,     0,     0,     0,  -921,  1155,  1156,  1157,  1158,  1159,
    1160,     0,     0,  -921,     0,     0,     0,     0,  1161,     0,
       0,     0,  1162,     0,     0,  -921,  1164,     0,  -921,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -921,
       0,     0,     0,  -921,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -921,
    1165,     0,     0,     0,  1166,  1167,  1168,  1169,  1170,     0,
       0,     0,     0,     0,     0,  1171,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -921,     0,     0,     0,  1172,
       0,     0,     0,     0,     0,     0,     0,  1173,     0,     0,
    -921,     0,     0,     0,     0,     0,     0,  -921,     0,     0,
       0,     0,     0,     0,     0,  1261,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1174,  1175,     0,     0,     0,     0,     0,     0,     0,  -921,
       0,  -921,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -921,  -921,     0,     0,
       0,  1176,     0,     0,  1177,     0,     0,     0,     0,     0,
       0,  -921,     0,     0,  1178,     0,  -921,     0,     0,  1179,
       0,     0,     0,  1180,  1181,  1182,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -921,  1183,     0,  -921,
    1184,  1185,     0,     0,     0,  1186,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1187,  1188,  1189,
       0,     0,     0,     0,     0,     0,     0,  1190,  1191,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -921,     0,     0,     0,  1192,  1193,  1194,     0,     0,     0,
    -921,     0,  -921,  -921,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1195,  1196,  1197,  1198,     0,  1199,
       0,     0,     0,  1200,  1201,     0,     0,     0,     0,     0,
       0,  1202,  1203,     0,     0,  -921,     0,     0,     0,  1204,
    1205,  1206,     0,     0,     0,     0,     0,  1207,     0,     0,
       0,     0,  1208,     0,     0,     0,     0,     0,     0,  -921,
       0,  -921,  -921,  -921,     0,  -921,  1996,     0,     0,     0,
    1209,  1997,  1998,  1999,  2000,     0,     0,  -921,  2001,     0,
       0,     0,     0,  1210,  1211,     0,     0,     0,     0,  1212,
       0,     0,     0,     0,  1262,     0,     0,  2002,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -921,  1213,
    1214,     0,     0,     0,     0,     0,     0,  1215,     0,     0,
       0,     0,     0,     0,     0,  1216,     0,     0,     0,     0,
       0,  1217,  -921,     0,     0,     0,     0,  -921,     0,     0,
       0,     0,  2003,  -921,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -921,  1218,     0,     0,
       0,     0,  1089,  1219,     0,     0,  -921,     0,  1090,     0,
       0,  -921,  -921,  -921,     0,  2004,     0,  2005,     0,     0,
       0,     0,     0,     0,     0,     0,  -921,     0,     0,     0,
       0,     0,     0,     0,  -921,  1091,     0,  -921,  1220,     0,
       0,     0,     0,     0,  1221,  1222,     0,     0,  1223,  2006,
       0,     0,     0,     0,  1224,     0,  1092,  1093,  1094,  1095,
    1096,  1097,  1098,  1099,  1225,  1226,     0,     0,     0,     0,
    1100,     0,     0,     0,  1101,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1102,  1103,  1104,  1105,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1106,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1107,     0,  2173,  1108,  1109,  1110,     0,  1111,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1112,     0,
       0,     0,     0,     0,     0,     0,  1113,  1114,  1115,  1116,
    1117,  1118,  1119,  1120,     0,     0,     0,  1121,  1122,     0,
       0,     0,     0,     0,     0,     0,  2007,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2008,     0,     0,     0,
       0,     0,  1123,  1124,     0,     0,  1125,  1126,     0,     0,
    2174,  1127,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2009,     0,     0,     0,     0,
    1128,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     561,     0,  1129,     0,     0,     0,  1130,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   563,
       0,  1131,     0,     0,     0,  1132,  1133,     0,     0,     0,
       0,  2010,     0,     0,     0,     0,     0,  1134,  1135,  1136,
    1137,  1138,     0,     0,  1139,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2011,
       0,     0,  2012,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1140,  1141,  1142,  1143,     0,     0,     0,
       0,     0,  1144,  1145,     0,  1146,     0,  1147,  1148,  1149,
       0,     0,  1150,  2013,  1151,     0,     0,     0,  1152,     0,
    2014,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1153,  1154,     0,
       0,     0,     0,     0,     0,     0,  1155,  1156,  1157,  1158,
    1159,  1160,     0,     0,  2015,     0,     0,     0,     0,  1161,
       0,     0,     0,  1162,     0,     0,     0,  1164,     0,  2016,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     564,   565,   566,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   567,     0,     0,     0,     0,     0,     0,     0,
       0,  1165,     0,     0,     0,  1166,  1167,  1168,  1169,  1170,
       0,     0,  2017,     0,     0,  2018,  1171,     0,     0,   568,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1172,     0,     0,     0,     0,     0,     0,     0,  1173,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  2019,     0,     0,  1261,     0,     0,     0,
       0,     0,     0,  2020,     0,  2021,  2022,     0,     0,     0,
    2175,  1174,  1175,     0,     0,     0,   569,     0,     0,     0,
       0,     0,     0,     0,     0,  2023,     0,     0,     0,     0,
    2024,     0,   573,     0,     0,   574,   575,   576,  2025,     0,
       0,     0,  1176,   577,     0,  1177,   578,     0,     0,     0,
       0,     0,     0,     0,     0,  1178,     0,     0,     0,     0,
    1179,     0,     0,     0,  1180,  1181,  1182,     0, -2725,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1183,     0,
    2026,  1184,  1185,     0,     0,     0,  1186,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1187,  1188,
    1189,     0,     0,     0,     0,     0,     0,     0,  1190,  1191,
       0,  2027,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   580,     0,     0,     0,  1192,  1193,  1194,     0,     0,
       0,  2028,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1195,  1196,  1197,  1198,     0,
    1199,     0,     0,     0,  1200,  1201,     0,     0,     0,     0,
       0,     0,  1202,  1203,     0,     0,     0,     0,     0,  2029,
    1204,  1205,  1206,     0,     0,     0,     0,     0,  1207,     0,
       0,  2030,     0,  1208,     0,     0,     0,  2031,     0,     0,
       0,     0,   582,     0,     0,     0,     0,     0,     0,     0,
       0,  1209,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1089,     0,  1210,  1211,     0,     0,  1090,     0,
    1212,     0,     0,     0,     0,  1262,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   583,     0,     0,     0,
    1213,  1214,     0,     0,     0,  1091,     0,     0,  1215,     0,
       0,     0,     0,     0,     0,     0,  1216,     0,     0,     0,
       0,     0,  1217,     0,     0,     0,  1092,  1093,  1094,  1095,
    1096,  1097,  1098,  1099,     0,     0,     0,     0,     0,     0,
    1100,     0,     0,     0,  1101,     0,     0,     0,  1218,     0,
       0,     0,     0,     0,  1219,  1102,  1103,  1104,  1105,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   584,     0,
       0,  1106,     0,     0,     0,     0,   560,     0,     0,     0,
       0,  1107,     0,     0,  1108,  1109,  1110,     0,  1111,  1220,
       0,     0,     0,   585,   586,  1221,  1222,     0,     0,  1223,
       0,     0,     0,     0,     0,  1224,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1225,  1226,     0,  1112,     0,
       0,     0,     0,     0,     0,     0,  1113,  1114,  1115,  1116,
    1117,  1118,  1119,  1120,     0,     0,     0,  1121,  1122,     0,
    1279,     0,   588,  2938,     0,     0,     0,     0,     0,     0,
       0,  2176,     0,     0,     0,     0,   589,     0,     0,     0,
       0,     0,  1123,  1124,     0,     0,  1125,  1126,     0,     0,
       0,  1127,     0,     0,     0,     0,     0,     0,     0,     0,
     590,     0,     0,     0,     0,     0,     0,     0,   591,     0,
    1128,     0,     0,     0,     0,  2314,  2315,  2316,  2317,  2318,
       0,  2319,  2320,   561,     0,     0,     0,     0,   592,     0,
       0,     0,  1129,   284,     0,     0,  1130,     0,     0,     0,
       0,     0,   563,   593,     0,     0,     0,     0,     0,     0,
       0,  1131,     0,     0,     0,  1132,  1133,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1134,  1135,  1136,
    1137,  1138,     0,     0,  1139,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2322,     0,  2323,  1801,  2324,
    2325,  2326,  2327,  2328,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1140,  1141,  1142,  1143,     0,     0,     0,
       0,     0,  1144,  1145,     0,  1146,     0,  1147,  1148,  1149,
       0,     0,  1150,     0,  1151,     0,     0,     0,  1152,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1153,  1154,  2329,
       0,     0,     0,     0,     0,     0,  1155,  1156,  1157,  1158,
    1159,  1160,     0,     0,     0,     0,     0,     0,     0,  1161,
       0,     0,     0,  1162,  1163,     0,     0,  1164,     0,     0,
       0,     0,  1280,   564,   565,   566,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   567,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1165,     0,     0,     0,  1166,  1167,  1168,  1169,  1170,
       0,     0,   568,     0,     0,     0,  1171,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1172,     0,     0,     0,     0,     0,     0,     0,  1173,  2330,
    2331,  2332,  2333,  2334,     0,     0,  1809,  2335,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1281,   569,
    1282,  1174,  1175,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2336,     0,     0,   573,     0,     0,   574,   575,
     576,     0,     0,     0,     0,     0,   577,     0,     0,   578,
       0,     0,  1176,     0,     0,  1177,  2337,     0,     0,     0,
       0,     0,     0,     0,     0,  1178,     0,     0,     0,     0,
    1179,     0,     0,     0,  1180,  1181,  1182,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1183,     0,
       0,  1184,  1185,     0,     0,     0,  1186,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1187,  1188,
    1189,     0,     0,     0,     0,     0,     0,     0,  1190,  1191,
       0,     0,     0,     0,   580,     0,     0,     0,     0,     0,
       0,   561,     0,     0,     0,  1192,  1193,  1194,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     563,     0,     0,     0,  2339,  1195,  1196,  1197,  1198,     0,
    1199,     0,     0,     0,  1200,  1201,     0,     0,     0,     0,
       0,     0,  1202,  1203,     0,     0,     0,     0,     0,     0,
    1204,  1205,  1206,     0,     0,     0,     0,     0,  1207,     0,
       0,     0,     0,  1208,     0,   582,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1209,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1210,  1211,  2341,     0,     0,     0,
    1212,     0,     0,     0,     0,     0,  3333,     0,     0,   583,
       0,     0,  -491,     0,     0,  2343,     0,     0,     0,     0,
    1213,  1214,     0,     0,     0,     0,     0,     0,  1215,     0,
       0,     0,     0,     0,     0,  2344,  1216,     0,     0,     0,
       0,     0,  1217,     0,     0,  -491,     0,     0,     0,     0,
    -491,  -491,  -491,  -491,  -491,     0,     0,  -491,  -491,  -491,
    -491,  -491,  -491,     0,  -491,  -491,  -491,     0,  1218,     0,
       0,   564,   565,   566,  1219,  -491,  -491,     0,     0,     0,
       0,   584,     0,   567,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -491,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   585,   586,  -491,  1220,
     568,     0,     0,  -491,     0,  1221,  1222,     0,     0,  1223,
    -491,  -491,     0,     0,  -491,  1224, -2766,     0,     0,     0,
       0,  -491,     0,     0,     0,  1225,  1226,     0,  -491,     0,
    -491,  -491,  -491,  -491,  -491,  -491,  -491,     0,     0,     0,
    2347,  2348,  2349,     0,     0,   588,     0,     0,     0,     0,
       0,     0,     0,     0,  -491,     0,     0,   569,   607,   589,
       0,     0,     0,     0,   571,     0,   572,     0,     0,     0,
       0,  -491,     0,   573, -1846,     0,   574,   575,   576,     0,
       0,     0,     0,   590,   577,     0,     0,   578,     0,     0,
       0,   591,  -491,     0,     0,     0,     0,     0,     0, -1846,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   592,     0,     0,     0,   888,   284,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   593,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -491,     0,     0,     0,
    2351,  2352,  2353,     0,     0,  -491,     0,     0,     0,     0,
       0,     0,   580,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1392,     0,     0,   581,     0,     0,     0,
       0,     0,  -491,  -491,  -491,  -491,  -491,     0,     0,  -491,
    -491,     0,     0,     0,     0,  -491,     0,     0,     0,  1660,
    -491,     0,     0,  -491,     0,  -491,     0,     0,     0,     0,
       0,     0,  -491,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -491,   582,     0,  -491,     0,     0,     0,     0,
       0,     0,     0,     0,  -491,   561,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -491,
       0,     0,     0,     0,   563,     0,     0,     0,  3700,     0,
       0,  -491,     0,     0,     0,     0,  -491,   583,     0,     0,
       0,     0,     0,     0,     0,  -491,     0,     0,     0,  -491,
       0,     0,     0,     0,     0,     0,  1661,  -491,     0,     0,
    -491,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -491,     0,     0,     0,  -491,     0,     0,  -491,     0,
    -491,  -491,  -491,  -491,  -491,     0,  -491,  -491,     0,     0,
       0,  -491,     0,     0,     0,     0, -1783,  -491,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   584,
       0,     0,     0,     0,     0, -1783,     0,  -491,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -491,     0,   585,   586,     0,     0,     0,  -491,
       0,     0,  -491,     0,     0,  -491,  -491,     0,     0,     0,
       0,     0,  -491,     0,     0,     0,     0,     0,     0,     0,
    -491,     0,  -491,  -491,  -491,  -491,  -491,  -491,  -491,  -491,
    1502,  -491,     0,  -491,     0,   564,   565,   566,     0,     0,
       0,   608,     0,   588,     0,   609,   610,   567,  -491,  -491,
       0,     0,     0,     0,     0,     0,     0,   589,     0,     0,
       0,     0,     0,  -491,     0,     0,     0,     0,  -491,     0,
       0,     0,     0,     0,   568,     0,     0,     0,     0,     0,
       0,   590,     0,     0,  -491,     0,     0,     0,  -491,   591,
       0,  -491,     0,     0,     0,     0,  -491,     0,     0,     0,
    -491,     0,     0,     0,     0,  1662,     0,     0,     0,   592,
       0,     0,     0,     0,   284,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   593,     0, -1783, -1783, -1783,     0,
       0,   569,  -491,     0,     0,     0,     0,     0, -1783,     0,
       0,     0,  -491,     0,  -491,  -491,     0,   573,     0,     0,
     574,   575,   576,     0,     0,     0,     0,     0,   577,     0,
       0,   578,     0,     0,     0, -1783,     0,     0,     0,     0,
    -491,     0,     0,     0,     0,  1392,     0,  -491,     0,     0,
       0,     0,     0,     0,  -491,  -491,  -491,  -491,  -491,     0,
       0,  -491,  -491,     0,     0,     0,     0,     0,     0,     0,
       0,  -491,     0,  -491,  -491,  -491,  1391,  -491,     0,     0,
       0,     0,     0,     0,     0,     0,  1663,     0,     0,  -491,
       0,     0, -1783,     0,     0,  -491,     0,  -491,     0,     0,
    -491,     0,  -491,     0,     0,     0,   580,     0, -1783,     0,
       0, -1783, -1783, -1783,     0,     0,     0,     0,     0, -1783,
    -491,  -491, -1783,     0,     0,  -491,     0,     0,  -491,  -491,
    -491,  -491,  -491,     0,  -491,  -491,     0,     0,     0,     0,
       0,     0,     0,     0,  -491,  -491,     0,  -491,     0,  -491,
       0,     0,     0,     0,     0,  -491,     0,     0,     0,  -491,
       0,     0,     0,     0,     0,     0,     0,     0,  -491,     0,
       0,     0,  -491,  -491,     0,     0,     0,   582,  -491,     0,
       0,     0,     0,  -491,  -491,  -491,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0, -1783,  -491,     0,
       0,     0,     0,     0,     0,     0,  -491,     0,  -491,  -491,
    -491,  -491,  -491,  -491,  -491,  -491,  -491,     0,     0,  -491,
       0,   583,     0,  -491,     0,     0,     0,     0,     0,     0,
     295,     0,  -491,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1458,     0,     0,     0,     0,     0,     0, -2766, -2766,
   -2766, -2766,     0,     0,     0, -2766,     0,     0, -1783,     0,
       0,     0,  -491,  -491,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -2766, -2766,     0,     0,     0,  2523,     0,
       0,  -491,     0,   584,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -491,     0,     0,     0,     0,
    -491,     0, -1783,     0,     0,  -491,     0,     0,   585,   586,
       0,     0,     0,     0,     0,     0,     0,     0, -2621, -2766,
    -491,     0, -2621,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -491,  -491,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1392, -2766,     0,     0,   588,     0,     0,
       0,     0,  -491,  -491,  -491,  -491,  -491,     0,     0,  -491,
    -491,   589,     0,     0, -1783,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -491,     0,     0,   590,     0,     0,     0, -1783,
   -1783,     0,     0,   591,     0,  -491,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -491,   592,     0,     0,     0,     0,   284,  -491,
       0,     0,     0,  -491,     0,  -491,  -491,  -491,   593,     0,
       0,     0,     0,     0,     0,  -491,  -491,     0, -1783,     0,
       0,     0,     0,     0,     0,  -491,     0,  1664,     0,     0,
       0,     0, -1783, -2766,     0,     0,     0,  -491,     0,     0,
       0,     0,     0,     0,     0,     0,  -491,     0,     0,     0,
       0,     0,     0,     0,     0,     0, -1783,     0,     0,     0,
       0,     0,     0,     0, -1783,     0,  1684,     0,     0,     0,
       0,     0,     0,     0,  1685,     0,     0,     0,     0,     0,
       0,  -491,     0, -2766, -1783,     0,     0,     0,  1686, -1783,
       0,     0,     0,     0,     0,     0,     0,  -491,     0, -1783,
    -491,     0,     0,     0,     0,  1687,     0,  -491,     0,     0,
       0,     0,     0,  2524,     0,  -491,  -491,  -491,     0,     0,
       0,     0, -2766,     0,  1688,     0,     0,     0,     0,     0,
    -491,     0,     0,     0,     0,     0,  1689,  1690,  1691,     0,
    1692,  -491,     0,     0,     0,  -491,  1693,     0,     0,     0,
       0,  -491,     0,     0,     0,     0,     0,     0,  1694,  1695,
    1696,     0,   295,     0,     0,     0,     0,     0,     0,  -491,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -491,     0,     0,  1697,     0,  -491, -2621,
       0,     0,     0,  2525,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -491, -2766,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1698,  -491,     0,     0,     0,  3762,
       0,     0,     0,     0,     0,     0,     0,  1699,     0,     0,
   -2766,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1700,  1701,
       0,  1702,  1703,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2314,  2315,
    2316,  2317,  2318,     0,  2319,  2320,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2321, -2766,     0,     0,     0,
    1704,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -491,     0,  -491,  -491,  -491,  1705,  2526, -2621,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1988,     0,     0,  1473,  1989,     0,     0,  1706,  1707,  1708,
    1474,     0,     0,     0,     0,     0,     0,  -491,  2322,     0,
    2323,  1801,  2324,  2325,  2326,  2327,  2328,     0,     0,  1709,
       0,     0,     0,     0,  1710,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -491,
   -2766,     0, -2766, -2766,     0,     0,     0,     0,     0,  1711,
    1712,     0,     0,     0,     0,  1713,  1714,     0,  -491,     0,
       0,     0, -2766,     0,     0,     0,     0, -2766,     0,     0,
       0,     0,  2329,  -491,  -491,  -491,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -491,     0,
       0,     0,     0,  2527,     0,     0,     0,     0,     0,  -491,
       0,     0,     0,     0,     0, -2766,     0,  1715,     0,  1716,
       0,     0,     0,     0,     0,     0,     0, -2766,     0,     0,
     295,     0,     0,     0,  1717,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1718,  1719,  1720,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1721,     0,     0,     0,     0,     0,     0,     0,     0,
    2528,     0,  2330,  2331,  2332,  2333,  2334,     0,     0,  1809,
    2335,     0,  1722,  1723,  1724,     0,  1725,     0,  1726,  1727,
       0,  1728,  1729,  1730,     0,     0,  1731,     0,  1732,  1733,
    1734,  1735,     0,     0,     0,     0, -2766,     0,     0,     0,
       0,     0,     0,  3763,  1736,  2336,     0,     0,  1458,     0,
   -2766,     0,  1737,     0,     0, -2766, -2766, -2766, -2766,     0,
       0,  1459, -2766,     0,  1738,  1739,     0,     0,     0,  2337,
       0,     0,  1740,     0,     0,     0,     0,  1741,     0,     0,
       0, -2766,     0,     0,     0,     0,     0,   888,     0,     0,
       0,     0,     0,     0,     0,  2338,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1742,
       0,  3764,     0,  1743,     0, -2621, -2766,     0,     0, -2621,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0, -2766,
       0, -2766,     0,     0,     0,     0,     0,  2339,  1744,  1745,
    1746,     0,     0,     0,     0,     0,     0,     0,  1747,     0,
    3765,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -2766,     0,     0,     0,     0,     0,     0,
    2312,     0,  1748,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1749,  1750,  1751,  1752,     0,
       0,  3766,     0,  2313,     0,     0,  2314,  2315,  2316,  2317,
    2318,     0,  2319,  2320,     0,  1753,  1754,     0,     0,  2341,
       0,     0,     0,  2321,     0,     0,  1755,     0,     0,     0,
       0,     0,     0,  2342,     0,     0,     0,     0,  2343,     0,
       0,     0,     0,  3767,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1756,     0,     0,     0,     0,  2344,     0,
       0,     0,     0,     0,     0,     0,  1757,  1758,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2322,     0,  2323,  1801,
    2324,  2325,  2326,  2327,  2328,     0,     0,     0,     0,  1759,
   -2766,     0,     0,     0,     0,     0,     0,  1760,     0,  1460,
   -2766,     0,     0,     0,  1761,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1762,     0,     0,     0,  1763,  1764,
    1765,     0,     0,     0,     0,     0,     0,     0,     0, -2766,
       0,     0,  1766,  1767,     0,     0,  1768,  1769,     0,  1770,
    2329,     0,     0,     0,     0,  1771,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2346,     0,  2347,  2348,  2349,     0,     0,     0,     0,
       0,     0, -2766,  3768,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0, -2766,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0, -2621,     0,     0,     0,
       0,     0,     0,     0,  3769,     0,     0,     0,     0,     0,
       0,     0,     0, -2766,     0,     0, -2766,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1461,     0,     0,     0,     0,  3770,
    2330,  2331,  2332,  2333,  2334,     0,     0,  1809,  2335,     0,
       0,   561,     0,     0,     0,     0,     0, -2766,     0,     0,
       0,     0,     0,     0, -2766,  3771,     0,     0,     0,     0,
     563,     0,     0,  2351,  2352,  2353,     0,     0,     0,     0,
       0,     0,  2312,  2336,     0,     0,     0,     0,  2354,     0,
       0,     0,     0,     0,     0,     0,     0,     0, -2766,  2306,
       0,     0,     0,  3772,     0,  2313,     0,  2337,  2314,  2315,
    2316,  2317,  2318, -2766,  2319,  2320,     0,     0,     0,     0,
       0,     0,     0,     0,   113,  2321,     0,     0,     0,     0,
       0,     0,     0,  2338,     0, -2621,     0,     0,     0,     0,
       0,     0,     0,     0,     0, -2738,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0, -2766,     0,     0, -2766,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2322,     0,
    2323,  1801,  2324,  2325,  2326,  2327,  2328, -2766,     0,     0,
       0,     0,     0,     0,     0,  2339,     0, -2766,     0, -2766,
   -2766,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   564,   565,   566,     0,     0,     0,     0,     0, -2766,
       0,     0,     0,   567, -2766,     0,     0,     0,     0,     0,
       0,     0, -2766,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2329,     0,     0,     0,     0,     0,     0,  2340,
     568,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0, -2766,     0,     0,     0,     0,  2341,     0,     0,
       0,     0,     0,     0, -2766,     0,     0,     0,     0,     0,
       0,  2342,     0,     0,     0,     0,  2343,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   561,
       0,     0,     0,     0,     0, -2766,  2344,   569,   607,     0,
       0,     0,     0,     0,   571,     0,   572,     0,   563,     0,
       0,     0,     0,   573, -2623, -2766,   574,   575,   576,     0,
       0,     0,     0,  2345,   577,     0,     0,   578,     0,     0,
       0,     0,  2330,  2331,  2332,  2333,  2334,     0,     0,  1809,
    2335,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -2766,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0, -2766,     0,     0,     0,     0,
       0, -2766,     0,     0,     0,  2336,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2337,
       0,     0,   580,     0,   888,     0,     0,     0,     0,  2346,
       0,  2347,  2348,  2349,     0,     0,   113,     0,     0,     0,
       0,     0,     0,     0,     0,  2338,   581,     0,     0,     0,
       0,     0,     0,  3483,     0,     0,     0, -2738,  3484,  3485,
    3486,  3487,     0,     0,     0,  3488,  2314,  2315,  2316,  2317,
    2318,     0,  2319,  2320,     0,  2350,     0,     0,     0,   564,
     565,   566,     0,  3489,  3490,     0,     0,     0,     0,     0,
       0,   567,     0,   582,     0,     0,     0,     0,     0,     0,
       0,  3491,     0,     0,     0,     0,     0,  -551,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2339,   568,     0,
       0,  3492,     0,     0,     0,     0, -2738,     0,  1988,  3493,
       0,     0,  1989,     0,     0,     0,     0,   583,     0,     0,
       0,  2351,  2352,  2353,     0,     0,  2322,     0,  2323,  1801,
    2324,  2325,  2326,  2327,  2328,     0,  2354,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2306,     0,     0,
       0,  2340,  3494,     0,     0,   569,   607,     0,     0,     0,
       0,     0,   571,     0,   572,     0,     0,     0,     0,  2341,
       0,   573,     0,     0,   574,   575,   576,     0,     0,     0,
       0,     0,   577,  2342,     0,   578,     0,     0,  2343,   584,
    2329,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2344,     0,
       0,     0,     0,     0,   585,   586,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2345,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    2277,     0,     0,  3495,     0,     0,     0,     0,     0,     0,
     580,   608,     0,   588,     0,   609,   610,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   589,     0,     0,
       0,     0,     0,     0,   581,     0,     0,     0,     0,     0,
    2330,  2331,  2332,  2333,  2334,     0,     0,  1809,  2335,     0,
       0,   590,     0,  3496,     0,     0,     0,     0,     0,   591,
       0,  3497,     0,  3498,     0,     0,     0,     0,     0,     0,
   -2667,  2346,     0,  2347,  2348,  2349,     0,     0,     0,   592,
    3499,   582,     0,  2336,   284,     0,     0,     0,     0,     0,
       0,     0,  3500,     0,   593,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2337,     0,     0,
       0,     0,     0,     0,     0,     0,  3881,  2350,     0,     0,
       0,     0,     0,     0,   113,   583,     0,     0,     0,     0,
       0,     0,     0,  2338,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  3501,  -552,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  3502,
       0,     0,     0,  3503,     0,     0,     0,     0, -2738,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  3504,
       0,     0,     0,  2351,  2352,  2353,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   584,  2354,     0,
       0,     0,     0,     0,     0,  2339,     0,     0,     0,  2306,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    3505,     0,   585,   586,     0,  3483,     0,  2014,     0,     0,
    3484,  3485,  3486,  3487,     0,     0,     0,  3488,  2314,  2315,
    2316,  2317,  2318,     0,  2319,  2320,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  3489,  3490,     0,     0,  3506,
       0,  2015,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   588,     0,  3491,     0,     0,  3507,  2341,     0,     0,
       0,     0,     0,     0,     0,   589,     0,     0,     0,     0,
       0,  2342,     0,  3492,     0,     0,  2343,     0,     0,     0,
    1988,  3493,     0,  3882,  1989,     0,     0,     0,     0,   590,
       0,     0,     0,     0,     0,     0,  2344,   591,  2322,  3508,
    2323,  1801,  2324,  2325,  2326,  2327,  2328,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   592,     0,     0,
       0,     0,   284,     0,  3494,   561,     0,     0,     0,     0,
       0,     0,   593,     0,     0,     0,     0,     0,     0,     0,
    3509,     0,     0,     0,   563,     0,     0,     0,     0,     0,
    2020,     0,  2021,  2022,     0,     0,     0,     0,     0,     0,
       0,     0,  2329,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   561,     0,
       0,     0,     0,     0,     0,  3510,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   563,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2346,
       0,  2347,  2348,  2349,     0,  3511,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  3495,     0,  3512,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  3513,     0,
       0,     0,  2330,  2331,  2332,  2333,  2334,     0,     0,  1809,
    2335,     0,     0,     0,     0,  3496,     0,     0,     0,     0,
       0,     0,  3514,  3497,     0,  3498,     0,  -842,     0,     0,
       0,     0, -2667,  3515,     0,   564,   565,   566,     0,     0,
       0,     0,  3499,     0,     0,  2336,     0,   567,     0,     0,
       0,     0,     0,     0,  3500,     0,  3516,     0,     0,     0,
       0,  2351,  2352,  2353,     0,     0,     0,     0,     0,  2337,
       0,     0,     0,     0,   568,     0,  2354,     0,     0,   561,
       0,     0,     0,     0,  3517,     0,   113,  2306,   564,   565,
     566,     0,     0,     0,     0,  2338,     0,     0,   563,     0,
     567,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    3501,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  3502,     0,     0,     0,  3503,     0,   568,     0,     0,
       0,   569,   607,     0,     0,     0,     0,     0,   571,     0,
     572,  3504,     0,     0,     0,     0,     0,   573,     0,   561,
     574,   575,   576,     0,     0,     0,     0,     0,   577,     0,
       0,   578,     0,     0,     0,     0,     0,  2339,   563,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  3505,     0,   569,   607,     0,     0,     0,  2014,
       0,   571,     0,   572,     0,     0,     0,     0,     0,     0,
     573,     0,     0,   574,   575,   576,     0,     0,     0,     0,
       0,   577,     0,     0,   578,     0,     0,     0,     0,     0,
       0,  3506,     0,  2015,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   580,     0,  3507,  2341,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   564,
     565,   566,     0,  2342,     0,     0,     0,     0,  2343,     0,
     581,   567,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2344,     0,
       0,  3508,     0,     0,     0,     0,     0,     0,   568,   580,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   582,   561,     0,
       0,     0,     0,   581,     0,     0,     0,     0,     0,   564,
     565,   566,  3509,     0,     0,     0,     0,   563,     0,     0,
       0,   567,  2020,     0,  2021,  2022,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   569,   607,     0,     0,     0,
       0,   583,   571,     0,   572,     0,     0,     0,   568,     0,
     582,   573,     0,     0,   574,   575,   576,  3510,     0,     0,
       0,     0,   577,     0,     0,   578,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2346,     0,  2347,  2348,  2349,     0,  3511,     0,     0,
       0,     0,     0,     0,   583,     0,     0,     0,     0,  3512,
       0,     0,     0,     0,     0,   569,   607,     0,     0,     0,
       0,     0,   571,   584,   572,     0,     0,     0,     0,     0,
       0,   573,     0,     0,   574,   575,   576,     0,     0,     0,
    3513,     0,   577,     0,     0,   578,     0,     0,   585,   586,
     580,     0,     0,     0,     0,     0,     0,  2458,     0,     0,
       0,     0,     0,     0,  3514,     0,     0,     0,     0,  -845,
       0,     0,     0,     0,   581,  3515,   584,     0,   564,   565,
     566,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     567,     0,     0,     0,     0,   608,     0,   588,  3516,   609,
     610,   585,   586,  2351,  2352,  2353,     0,     0,     0,     0,
    2460,   589,     0,     0,     0,     0,     0,   568,  2354,     0,
     580,   582,     0,     0,     0,     0,  3517,     0,     0,  2306,
       0,     0,     0,     0,     0,   590,     0,     0,     0,     0,
       0,     0,     0,   591,   581,     0,     0,     0,   608,     0,
     588,     0,   609,   610,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   592,   589,   583,     0,     0,   284,     0,
       0,     0,     0,     0,   569,   607,     0,     0,   593,     0,
       0,   571,     0,   572,     0,     0,     0,     0,   590,     0,
     573,   582,     0,   574,   575,   576,   591,     0,     0,     0,
       0,   577,     0,     0,   578,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   592,     0,     0,     0,
       0,   284,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   593,     0,     0,     0,   583,     0,   584,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   585,   586,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   580,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2481,     0,
       0,     0,     0,   581,     0,     0,     0,   584,     0,   608,
       0,   588,     0,   609,   610,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   589,     0,     0,     0,     0,
       0,     0,   585,   586,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   590,
     582,     0,     0,     0,     0,     0,     0,   591,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   592,     0,   608,
       0,   588,   284,   609,   610,     0,     0,     0,     0,     0,
       0,     0,   593,     0,   583,   589,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   590,
       0,     0,     0,     0,     0,     0,     0,   591,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   592,     0,     0,
       0,     0,   284,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   593,     0,     0,     0,   584,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   585,   586,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     588,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   589,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   590,     0,
       0,     0,     0,     0,     0,     0,   591,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   592,     0,     0,     0,
       0,   284,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   593
};

static const yytype_int16 yycheck[] =
{
     127,   560,   197,   114,   416,   116,   274,   642,   433,    52,
     121,   840,   841,   842,   943,    58,   845,   468,   755,  1263,
    1467,   570,   654,  1469,   424,  1004,    44,  1826,   802,   420,
     940,  1376,   412,  1631,  2051,  1984,   793,  1902,   422,   449,
     434,   650,  1410,   407,   778,  1554,   430,  2306,   587,  2066,
    1990,  1991,   430,  1915,  1994,   968,   450,  1058,   607,  1838,
     416,  2403,  1869,   964,  1897,   449,  2937,   309,   650,   408,
       1,   449,  1823,   974,     1,  1036,    15,     1,    15,    15,
    1589,  2500,  2501,  2502,  2503,    10,    28,    15,  2299,    15,
     136,    10,    15,    13,     1,  2229,    10,   522,  1286,    73,
    2566,  1062,  3087,    35,     1,   102,   147,    23,    78,   465,
       0,    23,   100,    15,     1,   149,   102,   146,  2381,    28,
     180,    39,    27,  2382,  2383,     0,   211,    12,   129,    39,
     175,    51,    95,  1043,   282,   767,   407,   114,   157,   158,
    1053,    58,   216,   411,   725,   413,   220,  1282,  2179,   102,
    2141,    10,    38,    27,   156,   737,    78,   235,  1043,   102,
      90,  1914,   433,   189,   289,   129,   217,    15,   269,   221,
     392,   282,   221,   210,   442,   424,   111,   445,  2717,   259,
     370,   115,   407,   451,   505,   119,   943,   309,   269,   798,
     264,  1868,   212,   587,    78,   362,   805,   491,   221,   362,
      40,   472,  3183,   471,   256,   112,   258,    15,   356,    38,
     405,  1278,   407,   381,    78,   410,   798,   381,   426,   414,
     326,   416,  2433,  2434,  2435,   147,   520,    44,   172,  2440,
    2441,  2442,   131,   157,    10,   370,    35,   432,   433,   520,
     435,   210,   149,   150,   520,   609,   129,  1314,   404,  2530,
     424,   453,    53,   388,   102,   407,   249,  2538,   190,   152,
     553,   456,   175,   424,   221,  2282,  2283,   553,  1472,   312,
     721,    21,   811,   389,   309,   470,  1480,   472,   473,   211,
    1789,   424,   381,    78,   623,   183,   115,   530,   459,  3270,
     119,   210,    34,   392,    15,   505,   450,   289,  2509,   570,
     547,  2512,   654,   678,   212,   424,   129,   129,  2519,  2732,
     424,  2522,   318,   661,  2525,   599,    10,   215,  3658,  3051,
     676,   459,   380,   692,   708,  3727,  2447,   326,   253,   362,
     755,   289,   687,   473,   308,   719,   607,   608,   609,   610,
     698,   231,  3588,   183,   112,   259,   236,   641,   424,    36,
     291,   407,   381,   289,   102,   327,   231,  3862,  3151,   212,
     129,   236,  1275,   814,  2921,   370,   179,   562,   362,   129,
     768,   639,   416,   362,  2733,  2734,   420,  1886,   277,   381,
     193,   149,   150,   388,   652,   324,   183,   188,   253,   771,
    2947,   190,   381,  3898,     0,   530,   945,   145,   424,  2520,
    2521,   477,   503,   471,   788,  1014,  3808,   765,  3654,   381,
     788,   795,   971,   608,   609,   610,   764,   811,   424,   559,
     327,    15,   593,  2230,   277,   719,   351,   598,   797,   572,
     129,    10,   351,   323,   453,   322,  2235,   351,   633,   381,
     804,   404,   797,   291,  1281,  1282,   504,   845,   323,   351,
    3790,   392,  1365,   572,   836,   511,  1005,   398,   572,   456,
      17,   755,  3255,   658,    38,   101,   356,   692,   358,   105,
     390,   846,   381,   398,   381,   360,   361,    10,   114,   635,
     101,   356,   416,   358,   755,    79,   770,   424,   531,   532,
     415,   216,   417,   114,   527,   220,   415,   849,   417,   424,
    2842,   415,   419,   417,   572,   424,   391,   778,   802,   663,
     424,   649,   288,   438,   761,   326,   424,  1251,   377,   438,
    2943,   217,   424,   517,   438,   733,   213,   770,   453,   724,
    2561,   530,  2563,   804,   326,    21,   438,  1882,   482,   264,
     692,   115,   499,   351,   563,   119,   431,   104,   217,   665,
     714,   844,   845,  1463,   402,   102,  3288,   759,   844,   309,
     755,   602,   459,  3102,   845,   775,   365,   587,  2077,   845,
    1938,    55,    56,    57,   609,   351,   318,   570,    62,   424,
    1559,   902,   377,   778,   424,  2262,   749,   416,   968,   521,
     424,   392,   477,   424,   958,   959,   960,   765,   765,  1600,
     157,  3582,  1603,   160,  1505,   162,   855,   742,   972,   804,
    2969,   819,   537,   381,  1527,   810,   424,   424,   537,   844,
     550,   555,   844,   537,   181,   454,   110,   678,   112,   536,
     438,   821,   602,   117,   118,   494,  1370,   845,   833,   415,
     691,   417,   764,   127,  1279,   598,   940,  1011,   424,  2402,
     467,   468,   469,   137,   760,   529,   676,   351,  1793,   584,
     709,   520,   438,  1244,   351,   584,  1057,  1577,  1300,   670,
     584,   845,   635,  1053,   775,  1636,  1032,   948,   949,   950,
     602,   571,   584,   570,   845,   765,   764,   958,   959,   960,
     961,  3033,  1577,   964,   775,   101,   571,   845,   675,  2450,
     599,   972,   845,   974,   975,   999,   670,   759,   114,   754,
      52,   268,   462,   984,   985,   986,   987,   988,   602,   612,
     845,   415,  2856,   417,  2987,   996,   555,  3008,   275,    71,
     424,  1340,  1281,  1282,  1028,  3606,   738,   424,   602,   624,
    1011,   764,   626,  1082,   438,  1479,   845,  3618,  1016,  1043,
    3289,   438,   649,   948,   949,   950,   477,   351,   575,   844,
     761,   537,   626,   958,   959,   960,   961,   804,   536,   964,
     897,   845,   351,   759,   700,   700,   584,   972,   465,   974,
     975,   700,   717,   824,   844,   845,   700,   670,   705,   984,
     985,   986,   987,   988,   278,   846,   280,   761,   700,   828,
    2145,   996,  2793,   998,  2835,   779,   692,   221,   584,   843,
     446,   845,   855,   407,   298,   409,  1011,  1574,   351,   845,
    3805,   368,   845,  2656,   749,   446,   845,   311,  1895,   772,
     424,   766,  2781,   821,   777,   804,   415,  2041,   417,   751,
     759,   754,   416,   537,   438,   424,   739,   670,   670,   774,
     847,   765,   845,   845,   824,   774,   765,   764,   776,   438,
     774,   907,   767,   909,  2726,  3331,   776,   764,   589,   426,
     844,   770,   774,   778,   764,   676,   919,   764,   761,  1840,
     764,  1306,   415,   183,   417,   804,  1521,   845,  1272,   764,
     584,   424,   700,   767,  1272,  1275,  2647,   584,  2649,  2087,
     764,   670,   824,   845,   778,   438,   845,   805,   845,   845,
     670,  3045,   844,   221,   845,   774,  1525,   845,   845,   839,
     845,   845,   845,   839,   700,  3284,   845,   407,   845,   855,
     855,   845,   412,  1667,   570,   783,   855,   844,   761,   761,
     824,   855,   422,  1525,   259,  1494,   424,  1331,  2457,   570,
     430,   748,   837,   855,   511,   845,  1793,  1936,   537,   844,
     824,   101,   759,   780,   781,   776,   774,  2718,  3839,   449,
     845,   670,   789,   506,   114,   416,   462,   806,   185,  1518,
    1251,   555,   678,  1277,   776,  1365,   279,   424,  1259,  1353,
     584,   276,   761,   424,  2592,   691,   764,    40,   774,   292,
     845,   761,   391,   772,   537,   584,   700,   349,   777,   678,
     855,   845,     0,   700,   845,   844,   424,   501,   705,   706,
     424,   391,   691,   191,    38,  1986,   326,  1321,   664,   513,
    2539,   424,  2541,   381,  3281,  1306,   450,   845,   845,   675,
     446,  3267,   431,   664,   424,    43,   549,   855,   855,  2856,
     424,   584,  1346,   537,   675,   355,  1251,   280,  1185,     1,
     544,   431,   761,   405,  1259,   759,   520,  2768,   844,   845,
    2579,    38,  2905,   360,  2907,  3494,   844,  2856,   402,   855,
     774,   506,  1353,   492,  3503,   370,   293,  1824,   477,   477,
     570,  1333,  3511,   261,   436,   424,   653,  3439,   520,  1370,
     326,   424,  2288,   388,   391,   351,   700,   477,    75,  2070,
    2071,  1306,  2222,  1847,   416,   552,   362,   323,   692,   844,
     845,   700,  2984,  1318,  1518,  1320,   307,   607,   608,   609,
     610,  2640,    28,  2642,   402,   322,  2046,   602,  2889,  2100,
    2101,   269,   450,   764,   431,  2146,   506,  1527,   115,   431,
     846,   845,   119,   416,   261,   767,  1540,   593,  1353,    35,
     693,   855,  1357,  2064,   570,   645,   180,   700,   855,  1463,
      35,   431,  3413,   277,   731,  1370,   309,   492,   424,  2923,
     774,   424,  2933,  3276,   356,   803,   358,   502,  2149,   396,
     477,   533,   438,   756,  2155,   774,    15,   687,  2543,    18,
     214,     0,    21,    22,  1543,  2782,  2783,   764,  1479,   845,
     828,   652,  1612,   776,  3473,  3474,   763,   477,   401,   687,
     401,   335,   336,   327,   845,   772,   773,   839,   708,  2142,
     777,  2144,    18,   476,  1505,    21,    22,  1637,   692,   719,
     447,   774,   424,   231,  1793,   530,  1646,  3340,   236,   467,
     468,  2212,   401,   517,  1548,   584,   270,   204,   664,   695,
     696,   855,  3183,  3413,  2225,  2226,   845,   448,   693,   675,
     526,   453,   156,   451,  3410,   273,   855,  3910,    13,  3912,
     536,  3914,   704,  1577,  1479,   687,  2199,   629,   845,  1416,
     351,   307,  2993,  3413,     7,   309,   410,   411,   778,   431,
     714,  1496,   759,  1852,   568,     1,   446,   797,   788,   552,
    1505,   156,   845,   327,   190,   795,    51,   731,   647,   322,
    3579,  3580,   855,   431,   804,   190,   626,  1683,   742,   797,
    3074,  3467,   746,   693,  2108,   323,    49,   221,   584,   603,
     777,   844,   204,   797,  3413,   477,  1885,   772,    61,  3270,
     652,  2696,   777,   262,   536,   648,   638,   575,   764,   824,
     269,  3160,   431,   424,   821,   571,   453,   845,   356,   477,
     358,   700,  2285,   340,   552,   503,    72,   438,  2564,  3413,
    3591,   424,   324,   570,   491,   401,  1581,  3598,   521,   652,
     103,  3602,   172,  2761,  3605,   797,  1667,  1668,   839,  1824,
     626,    13,  1249,  1787,  1788,   441,   714,   443,   477,  1787,
     772,   895,   448,   126,   845,   777,   764,   113,   855,   903,
     749,  1268,   114,   731,   116,  3651,  3003,  3674,  3669,   121,
     570,   845,   448,   656,   742,  2169,   378,   845,   746,    51,
    1850,   845,   440,   843,   928,   845,   855,   855,   837,   416,
     765,   793,   845,   420,   700,  1650,   844,   757,  1653,   783,
    1655,  1656,  1657,   176,  2425,   845,  2075,   837,   948,   949,
     950,   845,  1667,  1668,   844,   855,   172,   768,   958,   959,
     960,   961,   144,  1678,   964,   381,  2596,   694,   968,  1828,
     790,  1885,   972,   601,   974,   975,  3005,   518,   791,  3710,
     521,   517,  3713,   525,   984,   985,   986,   987,   988,   692,
    3019,  3020,  3753,  1877,  1878,   783,   845,   362,   118,  3669,
    2069,   204,   845,   584,   664,   125,   855,   221,   774,   822,
    1824,  1011,   855,   826,   777,   675,   431,   540,   251,    13,
     543,  3050,  2299,   692,  2454,  1839,   714,   728,  3413,  3669,
     837,  1678,  3492,  1824,   845,  3132,   570,   356,  2292,   358,
     845,   630,  3502,   731,   687,   431,   450,   570,   749,   767,
     308,   789,   764,  1053,   757,   782,  1847,    51,   746,   567,
     778,   277,   477,   571,   844,  1995,  1880,   554,   555,   845,
     282,   309,   465,   311,   772,     6,  3194,  3195,  1892,   777,
    3669,   323,   315,  3753,   342,   450,  1877,  1878,   757,   855,
     660,   477,   855,   365,    25,  3874,  1858,  1859,  1860,  1861,
    1862,  1863,  1864,  1865,   764,   712,    37,   276,   664,  1824,
     717,   327,  1827,  3753,   356,  3669,   358,  1832,   118,   700,
    2601,  2602,  1837,   462,   463,   125,   827,   764,   492,   257,
    3599,  1846,  1847,  2614,  2615,   769,  2081,   771,   661,  3608,
     661,   351,   843,   845,   416,  3263,  3243,    13,   262,   768,
    2631,  2632,   517,   855,   797,   269,   462,   463,   371,   766,
     524,   394,  1877,  1878,  3753,   381,   380,   795,  2052,  1884,
    2054,     1,   511,   512,  2106,   204,  1891,  1892,   394,   583,
     767,  2336,  3328,   114,   351,    51,   756,  3811,   381,    72,
     423,   778,   728,   680,  2078,  3115,   408,   764,   414,  3753,
     412,   370,   772,  1850,   764,   511,   512,   777,   277,   278,
     422,   423,  2506,   749,   424,  2099,   844,   756,   430,   388,
    3366,   760,   434,  2168,    54,    55,    56,    57,   438,    59,
     113,   424,   444,   308,   446,   447,  2147,   449,   450,  3863,
    3864,  1888,  2142,   424,  2144,    18,    19,    20,   593,   308,
     763,  1251,   380,   598,   409,   844,   845,   424,   327,  1259,
     773,  2052,   474,  2054,   419,   583,  3384,   342,   764,  1984,
     757,   438,  1272,  2064,   855,  1275,  2345,  3395,  3198,  3199,
     764,   514,   494,   342,  3669,   499,   661,  2078,   357,   172,
     504,   827,   473,  2197,   698,   476,   416,    29,    30,  2199,
     420,  3806,  3807,  3840,   261,   135,   136,   843,  2099,   764,
     714,   704,   269,   291,   707,   708,   281,   593,   722,   723,
     724,   593,   598,  2234,   501,   518,   503,   731,   521,   571,
     308,  1331,   661,   106,   606,   714,  3841,  2052,   686,  2054,
    1344,   593,   746,  2288,    17,   381,   598,  2828,   581,  2064,
    2446,  2447,   585,  1353,  3859,   536,   416,   844,  1450,   686,
     420,   530,  1454,  2078,   342,  1365,  2081,  1930,  3753,    99,
    1370,   552,   500,   764,   584,   587,   504,   593,  2169,   362,
     698,   597,    15,   764,  2099,  2285,  2177,   501,  2272,   503,
    2204,  2106,   762,  1397,   948,   949,   950,   613,   764,   615,
     156,   608,   772,   610,   722,   723,   724,   777,  2222,   764,
    3915,   623,  2893,  2894,  3279,  1497,  2204,   584,  1500,   149,
     398,   975,   655,    33,  2520,  2521,  1508,  3347,   236,  2060,
    1512,   104,   231,   645,   365,   571,   572,  1519,   764,  2154,
     418,   845,   764,  1447,   761,   581,    79,  3413,   761,   179,
     761,   412,   413,  2168,  2169,   772,   764,  1461,   845,   772,
     777,   772,  2177,   193,   777,   221,   777,   297,   298,   299,
     300,   301,   302,   303,   304,   305,   986,   987,   988,  1479,
     127,  2272,   715,  2297,   157,   416,   259,   160,   845,   162,
     700,   627,   845,  2208,  3813,  2789,   708,    34,   381,   845,
     431,  2292,   183,   762,   672,  1505,   848,   719,   181,   142,
     565,   394,   424,   772,   424,  3222,  2446,  2447,   777,  2449,
     509,   510,   783,  1523,  3231,  3232,  3233,  1527,   161,    37,
      38,   414,  3699,   700,   984,   985,    98,    99,   959,   960,
    1540,   845,   424,   755,   764,   639,  1227,   250,   764,  2264,
     759,  1555,  3051,   845,   787,   845,   845,  2272,   776,   661,
     772,   132,   335,   336,   774,  1246,  1247,  1248,   764,   764,
     673,  2109,   114,  2288,   675,  2727,   788,  2292,   105,  3746,
    3747,   692,   845,   795,   424,  3752,   381,   149,  3755,  1589,
    2520,  2521,  3759,  3760,   424,   268,   777,   528,  3788,   811,
    3790,   454,   376,   764,   535,   838,   764,   774,   824,   807,
     808,   809,   810,   807,   808,   809,   810,   179,   626,  2564,
     222,   450,    10,   318,   517,  3815,  3816,    15,  3818,   845,
     776,   193,   776,  3823,  3824,   776,   776,   410,   411,  2559,
     223,   807,   808,   809,   810,   855,   776,   776,   760,  2804,
     776,   776,   776,  3134,  3135,  3136,  1046,  1047,  1048,  1049,
     776,   776,   593,   776,   275,   224,  2923,  1667,  1668,   759,
    2316,  2317,  2318,  2319,   225,   606,   639,   227,   311,   312,
     313,   314,   593,   150,   742,   742,   334,   618,   855,    74,
     228,   324,   170,  2340,   450,   229,   424,   470,  2053,   230,
    2055,   748,   776,  3669,   233,   759,  2061,  2062,  2063,   234,
     593,    78,   681,  2068,   597,   749,  2661,    15,   351,   237,
     318,   238,   845,   553,  2079,  2080,   239,   241,    26,   502,
     613,   240,   615,  2380,   318,   828,   269,   453,   511,   566,
    2387,   242,    39,   573,   675,   776,   243,   520,   377,   244,
     681,   318,   245,   426,   142,  2824,   968,  2702,   187,   246,
     764,   661,   430,  3881,   695,  3264,   764,   155,  2415,   846,
     524,   751,   549,   161,   407,   408,   409,   675,   424,   845,
     764,    79,  2596,   714,   557,   378,  3753,  1787,  1788,  1789,
     764,   424,   764,   180,   427,   428,   429,   640,   764,  2446,
    2447,   156,   435,   759,   764,   438,   845,   845,   845,   759,
    3067,   326,   368,   626,   569,   424,   454,  3074,   624,   843,
     503,   269,   843,   839,   806,   821,   839,   583,   219,   288,
     612,  2394,   759,   764,   844,   340,   751,   768,   511,   756,
     745,  1053,   515,   221,  2776,   377,   759,  1847,    78,  2564,
    3614,   377,  2782,  2783,   187,   759,   759,   318,   318,    78,
     661,   377,   764,   610,   422,   764,   145,   764,    29,   764,
    1082,   424,   768,  2520,  2521,  2590,   454,  1877,  1878,   570,
     513,   764,   131,   845,   754,   853,  1886,   854,   392,   714,
     509,   461,   761,   761,   761,   326,   761,   761,   761,  1903,
     761,   761,   759,  1907,  1908,   759,  1910,    34,   631,  1913,
    1914,   609,  1916,   392,   326,   326,  3071,    23,  3267,  2819,
     751,  2821,  2736,   855,   312,   313,   314,   521,   232,    78,
     612,   759,   326,   211,   362,   274,   324,   417,   764,   518,
     517,   824,   698,   616,  2575,   839,   714,   427,   839,   326,
     759,   584,    15,    12,   764,   661,   436,   437,   714,   613,
     146,    28,  1966,   351,   309,   824,   722,   723,   724,   517,
     741,  2759,  2760,   692,   536,   731,  2921,   275,  2923,   524,
     653,   182,    72,   545,   546,  3140,   509,   101,    13,   392,
     746,     7,   765,  1997,   761,   628,   769,   693,   771,  2003,
     692,   326,  2947,  2007,   506,   477,   504,   380,   219,   824,
    2942,   675,   824,    78,   309,   845,   309,   392,   529,    13,
     408,   409,   522,   499,   527,   221,   699,   415,   701,   417,
     318,   612,   553,    49,   526,   764,   424,   188,   739,   427,
     428,   429,   764,   661,   652,    61,   824,   435,   839,   751,
     438,   613,  2052,   351,  2054,   327,   327,   381,   731,   635,
    1272,  2776,   845,  1275,  2064,   453,  2781,   700,   828,    23,
     764,   633,   634,  3003,   764,   845,   318,  2077,  2078,   326,
     326,  3338,   197,   221,   608,  3154,   608,   103,  2735,   172,
     770,    13,   725,   726,    51,   235,  2811,   431,   392,  2099,
     172,   482,   431,   416,   505,   775,   814,   156,    89,   407,
     126,   409,   745,  2117,   764,   661,   596,   855,  2765,  1331,
    2124,    84,  3067,  2127,   759,   513,   424,   792,    34,  3074,
    2134,   845,   673,   759,   309,  2782,  2783,   420,    53,   101,
     438,   774,  2142,   364,  2144,   714,   676,   431,   191,   537,
      78,   183,   567,  1365,   631,   788,   431,    78,   431,   764,
     176,   431,   500,   257,   764,  2880,   751,   425,   318,  2169,
     803,   424,   845,  2888,   756,   473,  2891,  2177,   567,   812,
     676,   555,  2186,   803,   845,   759,   189,   820,   189,   102,
     570,   759,   503,   180,   677,   828,   584,  2197,   588,  2199,
    2988,   767,  3132,   368,   308,   685,    12,   840,   182,   614,
     704,   844,   845,   365,    38,   764,   759,   844,   189,   542,
     700,   845,   855,   567,   764,   764,   392,  2942,   526,   764,
     521,   517,   392,   191,   407,   251,   764,   392,   392,   730,
     628,   327,   516,   407,  3032,   156,  1458,  3038,   392,   392,
     407,   112,   742,   101,   215,   846,   327,   764,   342,   839,
     381,   741,   318,    15,   366,   509,   472,    78,   536,   536,
     526,   718,  2272,   536,    26,    31,    32,    11,   142,    92,
     322,  1435,    25,    68,   511,  2285,   584,  3101,   294,  2936,
     300,   517,  2292,    75,  3265,   534,   296,   885,  3386,   315,
    3015,  2948,  3651,   875,  3753,  3316,  1518,  2974,  2955,  3291,
    3356,  1523,   700,  3243,  3207,  1527,  2982,  2988,  2967,  2950,
    3456,  3407,   802,  3038,  3467,  2356,  3041,    79,  1540,   809,
    1903,  1543,  3879,   813,  2692,  3283,  3346,   725,   726,  3054,
    2903,  1674,  3379,  2702,   843,  3587,  3699,   859,  3818,  3823,
    2354,  3270,  2356,  3613,   435,  3534,  3003,  1244,  3495,  3658,
     739,  3495,   118,  3154,  3152,  3530,  2545,  3502,  1967,   125,
     935,    99,   128,  2454,  1463,  2013,  2488,  1589,   394,  2781,
     623,  1469,  1534,  2543,  3139,  2389,   774,  3138,   633,   418,
    1577,   676,   999,  3338,   684,  2506,  2107,   435,  2537,  1057,
     788,  1060,   700,  1603,  2146,   704,  2552,   423,  1085,  2554,
    1255,   709,  2416,  2177,  1665,  2569,  2568,  1824,   750,   175,
    2590,   177,   178,  1831,   812,  2222,  2604,  2596,  2605,  1319,
    2297,  3146,   820,  2237,  1866,   786,   788,   450,  2275,  3086,
    1358,  2445,  2645,  1892,  1892,  3366,  1845,  2451,  1985,  2453,
    1830,  1983,   840,  1039,  2074,  2073,  2650,   845,  3044,  2635,
    2634,  2651,  2897,   761,  2105,   763,   449,   855,  3099,  2104,
    1074,   630,  2898,  3188,   772,   773,   774,  1038,   776,   777,
     444,  3128,  2268,  2268,  1606,  3132,  3117,  3118,   968,  2268,
    1065,  2268,  3123,  3381,   474,   772,  1420,   253,   514,   255,
    2709,   302,  2824,  1852,   993,  2081,   262,  3497,   996,  2449,
     917,   591,  1868,   269,  2097,  2698,  2641,  2526,  2882,  3172,
    3296,  3648,  1414,   275,  2528,  3004,   557,  3854,  2201,   528,
    1873,  2400,  3648,  2957,  3539,  1912,  3808,  3252,  3349,  2539,
      -1,  2541,    -1,    -1,    -1,  3192,    -1,    -1,    -1,    -1,
      -1,    -1,  3497,    -1,    -1,    -1,    -1,   855,    -1,    -1,
      -1,  3208,  1042,    -1,    -1,   581,    -1,    -1,    -1,   585,
      -1,    -1,  1052,  1053,    -1,  1787,  1788,  1789,    -1,  2579,
      -1,    -1,  3213,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  3670,    -1,    -1,  1074,    -1,  3243,    -1,  3412,   351,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  3322,  3239,    -1,
      -1,    -1,    -1,    -1,  3769,    -1,  1828,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    45,    46,    47,
      48,    -1,  2636,    -1,    52,    -1,    -1,    -1,  1850,   655,
      -1,    -1,  2642,    -1,    -1,    -1,  3277,  3278,    -1,    -1,
      -1,    -1,    70,    71,    -1,   407,    -1,   409,  2662,    -1,
     416,    -1,    -1,    -1,   420,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   424,  1885,  1886,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   438,    -1,    -1,    -1,
      -1,    -1,  1904,    -1,  3409,  3410,    -1,   453,   116,   715,
      -1,    -1,    -1,    -1,    -1,    -1,  3497,    -1,    -1,  3424,
      -1,    -1,    -1,    -1,    -1,    -1,  2720,    -1,    -1,  2723,
      -1,  3653,    -1,    -1,  3355,  3356,    -1,  2731,    -1,  3444,
      -1,    -1,    -1,   151,  3666,    -1,    -1,    -1,    -1,    45,
      46,    47,    48,    -1,  3459,    -1,    52,    -1,    -1,    -1,
    3381,    -1,    -1,    -1,  2758,  3470,    -1,    -1,    -1,    -1,
    3548,    -1,    -1,    -1,    70,    71,    -1,  3482,    -1,    -1,
      -1,   787,    -1,    -1,   526,    -1,    -1,  3424,    -1,    -1,
      -1,    -1,  3497,  1995,     1,    -1,    -1,    -1,    -1,     6,
      -1,    -1,    -1,    -1,    -1,  1275,    -1,    -1,    -1,  3514,
      -1,    -1,  3517,  3434,  3435,    -1,  3437,    -1,    25,  2813,
     116,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2819,
      37,  2821,   838,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   584,    -1,  3549,    -1,    53,  3552,  3553,  3554,
    3555,    -1,   260,    -1,    -1,   151,    -1,  3562,  2060,    -1,
      -1,  2855,    -1,    -1,    -1,    -1,  1336,    -1,    -1,  3506,
    3575,    -1,    -1,    -1,    -1,  2077,  3581,  1347,    -1,    -1,
      -1,    -1,  3587,  3588,    -1,    -1,    -1,    -1,  1358,    -1,
      -1,    -1,    -1,    -1,  3599,  1365,    -1,    -1,    -1,    -1,
      -1,    -1,   310,  3608,    -1,    -1,   652,   114,    -1,    -1,
      -1,    -1,    -1,  3550,    -1,    -1,    -1,    -1,  3623,  3624,
    3625,  3626,  3627,  3628,  3629,  3630,  3631,  3632,  3633,  3634,
    3635,  3636,  3637,  3638,  3639,  3640,  3641,    -1,    -1,    -1,
    2142,   349,  2144,    -1,    -1,    -1,    -1,    -1,  3653,  3654,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   700,    -1,
      -1,  3666,  3599,    -1,   260,    -1,    -1,    -1,    -1,    -1,
      -1,  3608,    -1,    -1,  2968,  3680,    -1,    -1,    -1,    -1,
      -1,  2975,  2976,  2977,  2978,    -1,    -1,  3692,    -1,  2983,
      -1,    -1,    -1,    -1,  3699,  2197,    -1,  2199,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   754,    -1,
      -1,    -1,    -1,    -1,   310,    -1,    -1,  3722,  3723,   761,
      -1,   763,    -1,    -1,    -1,  3806,  3807,    -1,   436,    -1,
     772,   773,   774,    -1,   776,   777,  3741,  3031,    -1,    -1,
      -1,  3746,  3747,    -1,    -1,    -1,    -1,  3752,    -1,  3754,
    3755,    -1,    -1,   349,  3759,  3760,    -1,    -1,    -1,    -1,
    3841,    -1,    -1,    -1,    -1,    -1,    -1,  3772,    -1,    -1,
     478,    -1,   818,    -1,    -1,    -1,    -1,    -1,  3859,    -1,
      -1,    -1,    -1,  2285,    -1,    -1,    -1,    -1,    -1,   407,
      -1,  3796,    -1,    -1,   412,    -1,  1566,  2299,    -1,   845,
      -1,  3806,  3807,    -1,   422,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   430,   855,    -1,  3109,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  3829,   533,    -1,    -1,    -1,  3766,
    3124,   449,    -1,    -1,  3915,    -1,  3841,    -1,    -1,    -1,
     436,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  3859,    -1,  3861,  3862,   365,  3153,
      -1,    -1,    -1,    -1,  3154,    -1,    -1,  3804,    -1,    -1,
      -1,  3876,    -1,    -1,    -1,    -1,    -1,  3882,    -1,    -1,
      -1,   477,    -1,    -1,  2386,    -1,    -1,  3892,  3893,    -1,
      -1,    -1,    -1,  3898,    -1,    -1,    -1,  3834,  3835,    -1,
      -1,  3906,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   416,
    3915,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   627,
      -1,   629,   630,    -1,   431,    -1,    -1,    -1,    -1,    -1,
      -1,  2433,  2434,  2435,    -1,    -1,    -1,   533,  2440,  2441,
    2442,   649,    -1,    -1,  2446,  2447,   654,  2449,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   161,    -1,    -1,  3273,
      -1,    -1,  3919,    -1,   692,    -1,    -1,    -1,    -1,    -1,
     608,   609,   610,    10,    -1,    -1,   704,    -1,    15,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2509,    -1,    26,
    2512,    -1,    -1,    -1,  3308,    -1,    -1,  2519,  2520,  2521,
    2522,   528,    -1,  2525,    -1,    -1,    -1,   645,   535,    -1,
      -1,   627,    -1,   629,   630,    -1,    -1,  2539,    -1,  2541,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  3345,    -1,   649,    -1,    -1,    -1,    -1,   654,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2575,    -1,    -1,    -1,  2579,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   793,   593,  1857,    -1,    -1,
     708,    -1,    -1,    -1,  3388,    -1,   692,    -1,    -1,   606,
      -1,   719,    -1,    -1,    -1,    -1,    -1,  3401,   704,    -1,
      -1,   618,    -1,    -1,    -1,    -1,    -1,    -1,  3412,  3413,
      -1,    -1,    -1,    -1,    -1,   142,    -1,   312,   313,   314,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   324,
    2642,    -1,    -1,    -1,   161,    -1,    -1,    -1,  3442,    -1,
      -1,  3445,  3446,  3447,  3448,    -1,    -1,    -1,    -1,    10,
      -1,    -1,  2664,    -1,    15,    -1,   351,    -1,   675,   676,
     788,    -1,    -1,    -1,   681,    -1,    -1,   795,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   804,    -1,   695,    -1,
    3484,    -1,    -1,    -1,    -1,    -1,    -1,   793,    -1,  3493,
      -1,    -1,  3496,    -1,    -1,    -1,    -1,   714,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   408,   409,    -1,    -1,    -1,    -1,    -1,
     415,    -1,   417,    -1,    -1,    -1,    -1,    -1,    -1,   424,
      -1,    -1,   427,   428,   429,    -1,    -1,    -1,    -1,    -1,
     435,    -1,    -1,   438,    -1,    -1,    -1,   764,   275,    -1,
      -1,   768,  3556,  3557,  3558,  3559,  3560,    -1,    -1,  3563,
    3564,  3565,  3566,  3567,  3568,  3569,  3570,  3571,  3572,  3573,
    2782,  2783,  3576,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    3584,   142,    -1,    -1,    -1,   312,   313,   314,    -1,    -1,
      -1,  3595,    -1,  3597,   155,    -1,    -1,   324,   142,    -1,
     161,    -1,    -1,  3607,    -1,    -1,    -1,  2819,    -1,  2821,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   161,   513,    -1,
     948,   949,   950,    -1,   351,    -1,    -1,    -1,    -1,    -1,
     958,   959,   960,  2845,    -1,    -1,    -1,    -1,    -1,    -1,
     968,    -1,   537,    -1,   972,    -1,    -1,   975,    -1,    -1,
      -1,    -1,    -1,    -1,   381,    -1,   984,   985,   986,   987,
     988,    -1,    -1,    -1,    -1,  3669,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   408,   409,  1011,    -1,    -1,    -1,    -1,   415,   584,
     417,    -1,    -1,    -1,    -1,    -1,    -1,   424,    -1,    -1,
     427,   428,   429,    -1,    -1,    -1,    -1,    -1,   435,    -1,
      -1,   438,    -1,    -1,    -1,    -1,  2196,    -1,    -1,    -1,
      -1,  3725,  3726,    -1,  3728,  1053,   453,    -1,    -1,    -1,
      -1,    -1,    -1,   628,    -1,    -1,    -1,    -1,  2950,    -1,
      -1,  3745,    -1,    -1,    -1,    -1,  3750,  3751,    -1,    -1,
      -1,   312,   313,   314,  2966,  2967,    -1,    -1,    -1,    -1,
      -1,  3765,    -1,   324,  3768,   492,    -1,  3771,   312,   313,
     314,  2251,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     324,    -1,    -1,    -1,    -1,    -1,   513,    -1,    -1,    -1,
     351,  3003,    -1,    -1,  2274,  2275,    -1,    -1,    -1,   526,
      -1,  2281,    85,    86,    87,   700,  3806,  3807,    -1,    -1,
     537,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     725,   726,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  3841,    -1,    -1,    -1,    -1,    -1,   408,   409,    -1,
      -1,    -1,    -1,    -1,   415,    -1,   417,   584,    -1,  3859,
      -1,    -1,    -1,   424,   408,   409,   427,   428,   429,    -1,
      -1,    -1,    -1,    -1,   435,    -1,    -1,   438,    -1,   774,
     424,   776,    -1,   427,   428,   429,    -1,  3099,    -1,    -1,
      -1,   435,   453,   788,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   628,    -1,  3115,    -1,  3117,  3118,    -1,    -1,    -1,
      -1,  3123,   195,   196,   197,  3915,    -1,   812,    -1,    -1,
    3132,    -1,    -1,    -1,   142,   820,    -1,    -1,    -1,    -1,
      -1,  1259,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  3151,
      -1,    -1,    -1,   161,  1272,   840,    -1,  1275,    -1,    -1,
     845,    -1,   513,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     855,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   513,
      -1,    -1,    -1,   700,    -1,    -1,   537,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  3198,  3199,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   725,   726,
      -1,  3213,    -1,  1331,    -1,    -1,    -1,  3219,    -1,    -1,
    3222,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  3231,
    3232,  3233,  3234,   584,  2504,  1353,    -1,  3239,    -1,    -1,
      -1,  3243,    -1,    -1,   761,    -1,   763,  1365,    -1,    -1,
      -1,    -1,    -1,  3255,    -1,   772,   773,   774,    -1,   776,
     777,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   788,    -1,    -1,    -1,  3277,  3278,   628,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   628,   812,    -1,   370,   371,    -1,
      -1,    -1,    -1,   820,   312,   313,   314,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   324,    -1,    -1,    -1,
     393,  2591,    -1,   840,    -1,    -1,  3328,    -1,   845,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   855,    -1,
      -1,    -1,    -1,   416,    -1,  3347,    -1,    -1,    -1,   700,
      -1,    -1,    -1,  3355,  3356,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  3366,    -1,   439,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   725,   726,    -1,    -1,    -1,  3381,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   738,    -1,    -1,
      -1,   725,   726,    -1,    -1,    -1,    -1,  3399,    -1,    -1,
     408,    -1,    -1,    -1,    -1,  1523,    -1,    -1,    -1,  1527,
      -1,    -1,    -1,    -1,    -1,    -1,   424,    -1,    -1,   427,
     428,   429,  1540,   774,    -1,    -1,    -1,   435,    -1,    -1,
      -1,    -1,  3434,  3435,    -1,  3437,    -1,   788,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   788,    -1,    -1,   530,    -1,    -1,
      -1,   812,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   820,
      -1,  1589,    -1,    -1,    -1,    -1,    -1,    -1,   812,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   820,   560,    -1,   840,
     563,   564,    -1,    -1,   845,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   855,   513,   840,    -1,    -1,    -1,
      -1,   845,    -1,    -1,   587,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2794,  2795,  2796,  2797,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2814,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  2837,  2838,    -1,
       3,    -1,    -1,    -1,    -1,     8,    -1,    -1,    11,    -1,
      -1,    14,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  3591,
      -1,    -1,    25,    -1,    -1,    -1,  3598,    -1,    -1,    -1,
    3602,    -1,    -1,  3605,    -1,    -1,    -1,    -1,    41,    42,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     628,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    61,    62,
      63,    64,    65,    66,    67,    68,    69,    -1,    -1,    -1,
      73,    74,    75,    76,    77,    -1,    -1,    80,    -1,    -1,
      83,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    92,
      93,    94,    95,    96,    97,    -1,    -1,    -1,   741,  1787,
    1788,  1789,    -1,    -1,   107,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   117,    -1,    -1,   120,   121,   122,
     123,   124,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  3710,    -1,
      -1,  3713,    -1,    -1,    -1,    -1,    -1,   725,   726,    -1,
      -1,   154,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   162,
     163,   164,   165,   166,   167,   168,   169,    -1,   171,    -1,
     173,   174,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   184,    -1,    -1,    -1,    -1,  3026,    -1,    -1,  1877,
    1878,    -1,    -1,    -1,    -1,   198,   199,    -1,  1886,   202,
     203,    -1,   205,   206,   207,   208,   209,    -1,    -1,    -1,
     788,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   226,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   812,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   820,    -1,   247,   248,    -1,    -1,    -1,   252,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     263,    -1,   840,    -1,   267,    -1,    -1,   845,   271,   272,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     283,   284,   285,   286,   287,    -1,    -1,   290,    -1,    -1,
      -1,   294,   295,   296,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   316,    -1,    -1,    -1,    -1,   321,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   329,   330,   331,   332,
      -1,    -1,    -1,    -1,    -1,   338,   339,    -1,   341,    -1,
     343,   344,   345,   346,   347,   348,    -1,   350,    -1,   352,
     353,   354,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  2052,    -1,  2054,    -1,    -1,    -1,
     373,   374,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   382,
     383,   384,   385,   386,   387,    -1,    -1,    -1,    -1,  2077,
    2078,    -1,   395,    -1,    -1,    -1,   399,    -1,    -1,    -1,
     403,   404,    -1,   406,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2099,    -1,    -1,    -1,    -1,    -1,    -1,   421,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   432,
     433,   434,    -1,    -1,   437,    -1,    -1,    -1,   441,   442,
     443,   444,   445,    -1,    -1,    -1,    -1,    -1,    -1,   452,
      -1,    -1,    -1,    -1,  2142,   458,  2144,    -1,    -1,    -1,
      -1,    -1,    -1,   466,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   474,    -1,    -1,    -1,    -1,   479,   480,   481,    -1,
     483,    -1,   485,   486,    -1,   488,   489,   490,    -1,   492,
     493,    -1,   495,   496,   497,   498,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   507,   508,    -1,    -1,   511,  2197,
      -1,  2199,    -1,    -1,    -1,    -1,   519,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   531,   532,
      -1,    -1,    -1,    -1,    -1,   538,   539,    -1,   541,    -1,
      -1,   544,    -1,    -1,    -1,    -1,    -1,    -1,   551,    -1,
      -1,    -1,    -1,   556,    -1,    -1,    -1,   560,   561,   562,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   574,    -1,    -1,   577,   578,    -1,    -1,    -1,   582,
      -1,    -1,    -1,   586,  2272,    -1,    -1,   590,    -1,    -1,
      -1,   594,   595,   596,    -1,    -1,    -1,  2285,    -1,    -1,
      -1,   604,   605,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   621,   622,
     623,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   635,   636,   637,    -1,    -1,    -1,   641,   642,
     643,   644,   645,   646,    -1,    -1,    -1,   650,   651,    -1,
      -1,    -1,    -1,    -1,    -1,   658,   659,    -1,    -1,    -1,
      -1,    -1,    -1,   666,   667,   668,   669,    -1,    -1,    -1,
      -1,   674,    -1,    -1,    -1,    -1,   679,    -1,    -1,   682,
     683,   684,   685,    -1,    -1,    -1,    -1,    -1,     1,    -1,
      -1,    -1,     5,    -1,   697,    -1,     9,    -1,    -1,   702,
     703,    -1,    -1,    16,    -1,    -1,    -1,   710,   711,    -1,
     713,    24,    -1,   716,    -1,    -1,    -1,    -1,   721,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   736,   737,    -1,    -1,   740,    -1,    -1,
      -1,   744,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   752,
     753,   754,    -1,    -1,    -1,   758,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    81,    82,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   784,    -1,   786,    -1,    -1,    38,   790,    -1,    -1,
      -1,   794,    -1,    45,    46,    47,    48,   110,   801,    -1,
      52,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   811,    -1,
      -1,    -1,   815,   816,   817,    -1,    -1,   130,    70,    71,
      -1,   134,   825,    -1,    -1,    -1,   829,   830,   831,   832,
     833,   834,   835,   836,    -1,   148,    -1,    -1,   841,   842,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   851,   852,
      -1,  2539,    -1,  2541,    -1,    -1,    -1,    -1,    -1,   172,
      -1,    -1,    -1,   115,   116,    -1,    -1,   119,    -1,    -1,
      -1,    -1,    -1,   186,    -1,    -1,    -1,    -1,    -1,   192,
      -1,   194,    -1,    -1,   197,    -1,    -1,    -1,   201,    -1,
      -1,  2579,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   151,
      -1,    -1,    -1,    -1,    -1,   218,   219,    -1,   221,   222,
     223,   224,   225,    -1,   227,   228,   229,   230,    -1,   232,
      -1,   234,   235,    -1,   237,   238,   239,   240,   241,   242,
     243,   244,   245,   246,    -1,    -1,   249,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   257,    -1,    -1,    -1,    -1,   262,
      -1,    -1,   265,    -1,  2642,    -1,   269,    -1,    -1,    -1,
      -1,   274,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,    -1,     5,    -1,    -1,    -1,
       9,    -1,    -1,    -1,    -1,    -1,    -1,    16,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   317,    -1,    -1,    -1,   260,    -1,
      -1,    -1,   325,    -1,    -1,   328,    -1,    -1,    -1,    -1,
     333,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   359,    -1,    -1,    -1,
      -1,    -1,    81,    82,   367,    -1,   369,    -1,   310,   372,
      -1,    -1,   375,    -1,    -1,    -1,    -1,   380,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   349,    -1,    -1,
      -1,   130,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   148,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2819,    -1,  2821,    -1,    -1,   449,    -1,    -1,    -1,
      -1,    -1,   455,    -1,   457,    -1,    -1,   460,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   186,    -1,    -1,
     473,    -1,    -1,   192,   416,   194,    -1,    -1,   197,   482,
      -1,    -1,   201,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   436,    -1,   499,   500,   501,   218,
     503,   504,   505,   506,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   454,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     523,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     249,    -1,    -1,    -1,    -1,    -1,   478,    -1,    -1,    -1,
     543,    -1,    -1,    -1,    -1,    -1,   265,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   274,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   580,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   589,    -1,   591,   592,
      -1,   533,    -1,    -1,    -1,    -1,    -1,    -1,   317,    -1,
      -1,    -1,    -1,    -1,   607,    -1,   325,    -1,    -1,   328,
      -1,    -1,    -1,   555,   333,    -1,    -1,   620,    -1,    -1,
      -1,    -1,   625,    -1,    -1,    -1,    -1,    -1,    -1,   632,
      -1,    -1,    -1,    -1,    -1,   638,    -1,    -1,    -1,    -1,
     359,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   367,    -1,
     369,    -1,    -1,   372,   657,    -1,   375,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   671,    -1,
      -1,    -1,    -1,    -1,   677,    -1,    -1,    -1,     1,    -1,
      -1,    -1,     5,    -1,    -1,   627,     9,   629,   630,    -1,
     693,    -1,   695,    16,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    24,    -1,    -1,    -1,    -1,   709,   649,    -1,    -1,
      -1,    -1,   654,    -1,    -1,    -1,   719,   720,    -1,    -1,
      -1,    -1,    -1,    -1,   727,    -1,    -1,   730,    -1,    -1,
     449,    -1,    -1,    -1,    -1,    -1,   455,    -1,   457,    -1,
     743,   460,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     692,    -1,    -1,    -1,   473,    -1,    -1,    -1,    81,    82,
      -1,   764,   704,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   775,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   785,    -1,    -1,    -1,    -1,   110,    -1,    -1,
      -1,    -1,    -1,   796,    -1,    -1,    -1,    -1,    -1,   802,
      -1,    -1,    -1,    -1,   523,    -1,    -1,   130,    -1,    -1,
      -1,   134,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     823,    -1,    -1,    -1,   543,   148,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   839,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   850,    -1,    -1,
      -1,   793,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   580,    -1,   186,   806,    -1,    -1,    -1,    -1,   192,
     589,   194,   591,   592,   197,    -1,    -1,    -1,   201,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   607,    -1,
      -1,    -1,    -1,    -1,    -1,   218,    -1,    -1,    -1,    -1,
      -1,   620,   844,    -1,    -1,    -1,   625,    -1,    -1,    -1,
      -1,    -1,    -1,   632,    -1,    -1,    -1,    -1,    -1,   638,
      -1,    -1,    -1,    -1,    -1,    -1,   249,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   657,    -1,
      -1,    -1,   265,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   274,   671,    -1,    -1,    -1,    -1,    -1,   677,    -1,
      -1,    -1,     1,    -1,    -1,    -1,     5,    -1,    -1,    -1,
       9,    -1,    -1,    -1,    -1,    -1,   695,    16,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    24,    -1,    -1,    -1,    -1,
     709,    -1,    -1,    -1,   317,    -1,    -1,    -1,    -1,    -1,
     719,   720,   325,    -1,    -1,   328,    -1,    -1,   727,    -1,
     333,   730,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   743,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   359,    -1,    -1,    -1,
      -1,    -1,    81,    82,   367,   764,   369,    -1,    -1,   372,
      -1,    -1,   375,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   785,    -1,    -1,    -1,
      -1,   110,    -1,    -1,    -1,    -1,    -1,   796,    -1,    -1,
      -1,    -1,    -1,   802,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   130,    -1,    -1,    -1,   134,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   823,    -1,    -1,    -1,    -1,   148,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   449,    -1,    -1,    -1,
      -1,   850,   455,    -1,   457,    -1,    -1,   460,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   186,    -1,    -1,
     473,    -1,    -1,   192,    -1,   194,    -1,    -1,   197,    -1,
      -1,    -1,   201,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    10,    -1,    -1,    -1,    -1,    15,   218,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     523,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     249,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     543,    -1,    -1,    -1,    -1,    -1,   265,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   274,    -1,    -1,    -1,    -1,
      -1,    78,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   580,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   589,    -1,   591,   592,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   317,    -1,
      -1,    -1,    -1,    -1,   607,    -1,   325,    -1,    -1,   328,
      -1,    -1,    -1,    -1,   333,    -1,    -1,   620,    -1,    -1,
      -1,    -1,   625,    -1,    -1,   142,    -1,    -1,    -1,   632,
     147,    -1,    -1,    -1,    -1,   638,    -1,    -1,    -1,    -1,
     359,    -1,    -1,    -1,   161,    -1,    -1,    -1,   367,    -1,
     369,    -1,    -1,   372,   657,    -1,   375,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   671,    -1,
      -1,    -1,    -1,    -1,   677,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   695,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   709,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   719,   720,    -1,    -1,
      -1,    -1,    -1,    -1,   727,    -1,    -1,   730,    -1,    -1,
     449,    -1,    -1,    -1,    -1,    -1,   455,    -1,   457,    -1,
     743,   460,    -1,    -1,    -1,    -1,    -1,    -1,  3806,  3807,
      -1,    -1,    -1,    -1,   473,    -1,    -1,    -1,    -1,    -1,
      -1,   764,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   785,  3841,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   796,    -1,   312,   313,   314,    -1,   802,
      -1,  3859,    -1,    -1,   523,    -1,    -1,   324,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     5,    -1,
     823,    10,     9,    -1,   543,    -1,    15,    -1,    -1,    16,
      -1,    -1,    -1,    -1,   351,    -1,    -1,    24,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   850,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  3915,    -1,    -1,
      -1,   580,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     589,    -1,   591,   592,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   607,    -1,
      -1,   408,   409,    -1,    81,    82,    -1,    -1,   415,    -1,
     417,   620,    -1,    -1,    -1,    -1,   625,   424,    -1,    -1,
     427,   428,   429,   632,    -1,    -1,    -1,    -1,   435,   638,
      -1,   438,    -1,   110,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   453,    -1,   657,    -1,
      -1,    -1,    -1,   130,    -1,    -1,    -1,   134,    -1,    -1,
      -1,    -1,   671,   142,    -1,    -1,    -1,    -1,   677,    -1,
      -1,   148,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   161,    -1,    -1,    -1,   695,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     709,    -1,    -1,    -1,    -1,    -1,   513,    -1,    -1,   186,
     719,   720,    -1,   520,    -1,    -1,    -1,    -1,   727,    -1,
     197,   730,    -1,    -1,   201,    -1,    -1,    -1,    -1,    -1,
     537,    -1,    -1,    -1,   743,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   219,    -1,    -1,   222,   223,   224,   225,    -1,
     227,   228,   229,   230,    -1,   232,   233,   234,   235,    -1,
     237,   238,   239,   240,   241,   242,   243,   244,   245,   246,
      10,    -1,   249,    -1,    -1,    15,   785,   584,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   796,   265,    -1,
      -1,    -1,    -1,   802,    -1,   602,    -1,   274,    38,    -1,
      -1,    -1,    -1,    -1,    -1,    45,    46,    47,    48,    -1,
      -1,    -1,    52,    -1,   823,    -1,    -1,    -1,    -1,    -1,
      -1,   628,    -1,    -1,    -1,    -1,    -1,   306,    -1,    -1,
      70,    71,    -1,   312,   313,   314,    -1,    -1,    -1,    -1,
     317,   850,    -1,    -1,    -1,   324,    -1,    -1,   325,    -1,
      -1,   328,    -1,    -1,    -1,    -1,   333,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   351,    -1,    -1,   115,   116,    -1,    -1,   119,
      -1,    -1,   359,    -1,   363,   692,    -1,    -1,    -1,    -1,
     367,    -1,   369,   700,    -1,   372,    -1,    -1,   375,    -1,
      -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   151,    -1,    -1,    -1,    -1,    -1,    -1,   725,   726,
      -1,   161,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   408,
     409,    -1,    -1,    -1,    -1,    -1,   415,    -1,   417,    -1,
      -1,    -1,    -1,    -1,    -1,   424,    -1,    -1,   427,   428,
     429,    -1,    -1,    -1,    -1,    -1,   435,    -1,    -1,   438,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   774,    -1,    -1,
      -1,   450,   449,    -1,   453,    -1,    -1,    -1,   455,    -1,
     457,   788,    -1,   460,    -1,    -1,    -1,    -1,    -1,    -1,
     797,    -1,    -1,    -1,    -1,    -1,   473,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   812,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   820,    -1,    -1,    -1,   824,    -1,    -1,
     260,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   840,   513,    -1,    -1,    -1,   845,    -1,
      -1,   520,    -1,    -1,    -1,    -1,   523,    -1,   855,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   537,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   543,    -1,    -1,    -1,
     310,    -1,   312,   313,   314,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   324,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   584,    -1,    -1,    -1,   349,
      -1,   351,   589,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     607,    -1,    -1,    10,    -1,    -1,    -1,    -1,    15,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   625,   628,
      -1,    -1,    -1,    -1,    -1,   632,    -1,    -1,    -1,    -1,
      -1,   638,    -1,    -1,    -1,    -1,    -1,    -1,   408,   409,
      -1,    -1,    -1,    -1,    -1,   415,   416,   417,    -1,    -1,
     657,    -1,    -1,    -1,   424,    -1,    -1,   427,   428,   429,
      -1,    -1,    -1,    -1,    10,   435,   436,    -1,   438,    15,
     677,    -1,    18,    19,    20,    -1,    -1,    -1,    -1,    -1,
      26,    -1,    -1,   453,   454,    -1,    -1,    -1,   695,    -1,
      -1,   700,    -1,    -1,    -1,   704,    -1,    -1,    -1,    -1,
      -1,    -1,   709,    -1,    -1,    -1,    -1,    -1,   478,    -1,
      -1,    -1,   719,   720,    -1,    -1,   725,   726,    -1,   728,
     727,    -1,    -1,   730,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   142,   743,    -1,    -1,    -1,
      -1,    -1,    -1,   513,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   161,    -1,    -1,   764,    -1,    -1,
     106,   770,    -1,   533,    -1,   774,    -1,   537,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   785,   788,
      -1,    -1,    -1,    -1,    -1,   555,    -1,    -1,    -1,   796,
      -1,    -1,    -1,    -1,    -1,   802,   142,    -1,    -1,    -1,
      -1,    -1,    -1,   812,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   820,    -1,    -1,   584,   161,    -1,    -1,    10,    -1,
      -1,    -1,    -1,    15,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   840,    -1,    -1,   843,    -1,   845,    -1,    -1,    -1,
      -1,    -1,    -1,   850,    -1,    -1,   855,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   261,    -1,    -1,   627,   628,   629,
     630,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   649,
      -1,    -1,    -1,    -1,   654,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   312,   313,   314,    -1,    -1,
      -1,    -1,    -1,   259,    -1,    -1,    -1,   324,    -1,   326,
      -1,    -1,   692,    -1,    -1,    -1,    -1,    -1,    -1,   275,
     700,    -1,    -1,    -1,   704,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   351,    -1,    -1,    -1,    -1,    -1,
     142,    -1,    -1,    -1,    -1,   725,   726,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   312,   313,   314,   161,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   324,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   335,
     336,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   408,   409,    -1,   774,   351,    -1,    -1,   415,    -1,
     417,    -1,    -1,    -1,    -1,    -1,    -1,   424,   788,    -1,
     427,   428,   429,   793,    -1,    -1,    -1,    -1,   435,    -1,
      -1,   438,    -1,    -1,    -1,   381,   806,    -1,    -1,    -1,
      -1,    -1,   812,    -1,    -1,    -1,   453,    -1,    -1,    -1,
     820,    -1,    -1,    -1,    -1,    -1,    -1,   249,    -1,    -1,
      -1,    -1,   408,   409,   410,   411,    -1,    -1,    -1,   415,
     840,   417,    -1,    -1,   844,   845,    -1,    -1,   424,    -1,
      -1,   427,   428,   429,   491,   855,    -1,    -1,    -1,   435,
      -1,    -1,   438,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   513,   453,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    10,    -1,
     312,   313,   314,    15,   470,    -1,    -1,    -1,    -1,    -1,
     537,    -1,   324,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   492,    -1,    -1,    10,
      -1,    -1,    -1,    -1,    15,    -1,   502,    -1,    -1,   351,
      -1,    -1,    -1,    -1,    -1,   511,    -1,   513,    -1,    -1,
      -1,    -1,    -1,    -1,   520,    -1,    -1,   584,    -1,    -1,
     526,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   537,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   557,    -1,    -1,    -1,    -1,   408,   409,    -1,   626,
      -1,   628,    -1,   415,    -1,   417,    -1,    -1,    -1,    -1,
      -1,    -1,   424,    -1,    -1,   427,   428,   429,   584,    -1,
      -1,    -1,    -1,   435,    -1,    -1,   438,    -1,    -1,    -1,
     142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   453,    -1,   155,    -1,    -1,    -1,    -1,    -1,   161,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   142,   628,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   700,    -1,    -1,    -1,    -1,    -1,    -1,
     161,    -1,    -1,    10,    -1,    -1,    -1,    -1,    15,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   725,   726,
      -1,   513,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   537,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   700,    -1,    10,    -1,    -1,    -1,
      -1,    15,    -1,    -1,    -1,    -1,    -1,   774,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   725,
     726,   788,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   584,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   812,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   820,    -1,   761,    -1,   763,    -1,   765,
     312,   313,   314,   769,    -1,   771,   772,   773,   774,    -1,
     776,   777,   324,   840,    -1,   142,   628,   844,   845,    -1,
      -1,    -1,   788,    -1,    -1,    -1,    -1,    -1,   855,    -1,
      -1,   312,   313,   314,   161,    -1,    -1,    -1,    -1,   351,
      -1,    -1,    10,   324,    -1,    -1,   812,    15,    -1,    -1,
      -1,    -1,    -1,    -1,   820,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
     351,    -1,    -1,    -1,   840,    -1,    -1,    -1,    -1,   845,
     692,    -1,    -1,    -1,    -1,    -1,    -1,   161,   700,   855,
      -1,    -1,    -1,    -1,    -1,    -1,   408,   409,    -1,    -1,
      -1,    -1,    -1,   415,    -1,   417,    -1,    -1,    -1,    -1,
      -1,    -1,   424,   725,   726,   427,   428,   429,    -1,    -1,
      -1,    -1,    -1,   435,    -1,    -1,   438,   408,   409,    -1,
      -1,    -1,    -1,    -1,   415,    -1,   417,    15,    -1,    -1,
      -1,   453,    -1,   424,    -1,    -1,   427,   428,   429,    -1,
      -1,    -1,    -1,    -1,   435,   767,    -1,   438,    -1,    -1,
      -1,    -1,   774,    -1,    -1,    -1,   778,    -1,    -1,    -1,
      -1,    -1,   453,    -1,   142,    -1,   788,    10,    -1,    -1,
      -1,    -1,    15,    -1,    -1,   312,   313,   314,    -1,    -1,
      -1,    -1,    -1,   161,    -1,    -1,    -1,   324,    -1,    -1,
     812,   513,    -1,    -1,    -1,    -1,    -1,    -1,   820,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   351,   537,    -1,    -1,   840,    -1,
      -1,    -1,   513,   845,    -1,    -1,    -1,    -1,   312,   313,
     314,    -1,    -1,   855,   318,    -1,    -1,    -1,    -1,    -1,
     324,    -1,    -1,    -1,    -1,    -1,   537,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   584,    -1,    -1,    -1,    -1,   351,    -1,    -1,
      -1,   408,   409,   161,    -1,    -1,    -1,    -1,   415,    -1,
     417,    -1,    -1,    -1,    -1,    -1,    -1,   424,    -1,    -1,
     427,   428,   429,   584,    -1,    -1,    -1,    -1,   435,   142,
      -1,   438,    -1,    -1,    -1,    -1,   628,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   453,    -1,   161,    -1,
      -1,    -1,    -1,    -1,   408,   409,    -1,    -1,    -1,    -1,
      -1,   415,    -1,   417,   312,   313,   314,   628,    -1,    -1,
     424,    -1,    -1,   427,   428,   429,   324,    -1,    -1,    -1,
      -1,   435,    -1,    -1,   438,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   453,
      -1,    -1,    -1,   351,    -1,    10,   513,    -1,   700,    -1,
      15,    -1,    -1,   520,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     537,    -1,    -1,   725,   726,    -1,    -1,    -1,    -1,   700,
      -1,    -1,    -1,    -1,    -1,    -1,   738,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   312,   313,   314,    -1,    -1,   513,
     408,   409,    -1,    -1,   725,   726,   324,   415,    -1,   417,
      -1,    -1,    -1,    -1,    -1,    -1,   424,   584,    -1,   427,
     428,   429,   774,   537,    -1,    -1,    -1,   435,    -1,    -1,
     438,    -1,    -1,   351,    -1,    -1,   788,    -1,   759,   312,
     313,   314,    -1,    -1,    -1,   453,    -1,    -1,    -1,    -1,
      -1,   324,    -1,   774,    -1,    -1,    -1,    -1,    -1,    -1,
     812,   628,    -1,    -1,    -1,    -1,    -1,   788,   820,    -1,
     584,    -1,    -1,    -1,    -1,    -1,    -1,   142,   351,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   840,    -1,
     408,   812,    -1,   845,    -1,    -1,   161,    -1,    -1,   820,
      -1,    -1,    -1,   855,    -1,   513,   424,    -1,    -1,   427,
     428,   429,    -1,    -1,   628,    -1,    -1,   435,    -1,   840,
     438,    -1,    -1,    -1,   845,    -1,    -1,    -1,    -1,   537,
      -1,    -1,    -1,   700,   855,   408,   409,    -1,    -1,    -1,
      -1,    -1,   415,    -1,   417,    -1,    -1,    -1,    -1,    -1,
      -1,   424,    -1,    -1,   427,   428,   429,    -1,   725,   726,
      -1,    -1,   435,    -1,    -1,   438,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   584,    -1,    -1,    -1,
     453,    -1,    -1,    -1,    -1,    -1,   700,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   513,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   774,    -1,    -1,
      -1,   725,   726,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     628,   788,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     513,    -1,    -1,    -1,    -1,   812,    -1,   312,   313,   314,
      -1,    -1,    -1,   820,    -1,    -1,    -1,    -1,    -1,   324,
     774,    -1,    -1,    -1,   537,    -1,   584,    -1,    -1,    -1,
      -1,    -1,    -1,   840,   788,    -1,    -1,    -1,   845,    -1,
      -1,    -1,    -1,    -1,   692,    -1,   351,    -1,   855,    -1,
      -1,    -1,   700,    -1,    -1,    -1,    -1,    -1,   812,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   820,    -1,    -1,    -1,
     628,   584,    -1,    -1,    -1,    -1,    -1,   725,   726,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   840,    -1,    -1,    -1,
      -1,   845,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   855,    -1,   408,   409,    -1,    -1,    -1,    -1,    -1,
     415,    -1,   417,    -1,    -1,   628,    -1,    -1,    -1,   424,
      -1,    -1,   427,   428,   429,    -1,   774,    -1,    -1,    -1,
     435,    -1,    -1,   438,    -1,    -1,    -1,    -1,    -1,    -1,
     788,    -1,   700,    -1,    -1,    -1,    -1,    -1,   453,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   812,    -1,    -1,   725,   726,    -1,
      -1,    -1,   820,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   700,    -1,    -1,
      -1,    -1,   840,    -1,    -1,    -1,    -1,   845,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   855,   513,    -1,
      -1,    -1,   725,   726,    -1,    -1,   774,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     788,    -1,   537,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   812,    -1,    -1,    -1,    -1,    -1,
      -1,   774,   820,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   788,    -1,    -1,    -1,   584,
      -1,    -1,   840,    -1,    -1,    -1,    -1,   845,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   855,    -1,   812,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   820,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     8,
      -1,    -1,    -1,   628,    -1,    14,    -1,   840,    -1,    -1,
      -1,    -1,   845,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   855,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    40,    41,    -1,    -1,    -1,    45,    46,    47,    48,
      -1,    -1,    -1,    52,    53,    54,    55,    56,    57,    -1,
      59,    60,    -1,    62,    63,    64,    65,    66,    67,    68,
      69,    70,    71,    -1,    -1,    -1,    -1,    76,    -1,    -1,
      -1,    80,    -1,    -1,    -1,   700,    -1,    -1,    -1,    88,
      -1,    -1,    91,    92,    93,    94,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   107,   108,
     725,   726,    -1,    -1,    -1,    -1,   115,   116,   117,    -1,
     119,   120,   121,   122,    -1,   124,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   133,    -1,   135,   136,   137,   138,
     139,   140,   141,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   154,    -1,    -1,    -1,   774,
     159,    -1,    -1,   162,   163,   164,   165,   166,   167,   168,
     169,    -1,    -1,   788,   173,   174,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   812,   197,   198,
     199,    -1,    -1,   202,   203,   820,    -1,    -1,   207,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   840,    -1,   226,    -1,    -1,
     845,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     855,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   248,
      -1,    -1,    -1,   252,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   260,    -1,    -1,    -1,    -1,    -1,    -1,   267,    -1,
      -1,    -1,   271,   272,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   283,   284,   285,   286,   287,    -1,
      -1,   290,    -1,    -1,    -1,    -1,    -1,    -1,   297,   298,
     299,   300,   301,    -1,    -1,   304,   305,    -1,    -1,    -1,
      -1,   310,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   318,
      -1,   320,    -1,    -1,    -1,    -1,    -1,    -1,   327,    -1,
     329,   330,   331,   332,    -1,    -1,    -1,    -1,   337,   338,
     339,   340,   341,    -1,   343,   344,   345,    -1,    -1,   348,
     349,   350,    -1,    -1,    -1,   354,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   364,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   373,   374,    -1,    -1,    -1,    -1,
      -1,    -1,   381,   382,   383,   384,   385,   386,   387,    -1,
      -1,   390,    -1,    -1,    -1,    -1,   395,    -1,    -1,    -1,
     399,    -1,    -1,   402,   403,    -1,   405,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   416,    -1,    -1,
      -1,   420,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   436,   437,    -1,
      -1,    -1,   441,   442,   443,   444,   445,    -1,    -1,    -1,
      -1,    -1,    -1,   452,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   462,    -1,    -1,    -1,   466,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   474,    -1,    -1,   477,    -1,
      -1,    -1,    -1,    -1,    -1,   484,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   492,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   507,   508,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   516,    -1,   518,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   533,   534,    -1,    -1,    -1,   538,
      -1,    -1,   541,    -1,    -1,    -1,    -1,    -1,    -1,   548,
      -1,    -1,   551,    -1,   553,    -1,    -1,   556,    -1,    -1,
      -1,   560,   561,   562,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   573,   574,    -1,   576,   577,   578,
      -1,    -1,    -1,   582,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   594,   595,   596,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   604,   605,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   617,    -1,
      -1,    -1,   621,   622,   623,    -1,    -1,    -1,   627,    -1,
     629,   630,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   641,   642,   643,   644,    -1,   646,    -1,    -1,
      -1,   650,   651,    -1,    -1,    -1,    -1,    -1,    -1,   658,
     659,    -1,    -1,   662,    -1,    -1,    -1,   666,   667,   668,
      -1,    -1,    -1,    -1,    -1,   674,    -1,    -1,    -1,    -1,
     679,    -1,    -1,    -1,    -1,    -1,    -1,   686,    -1,   688,
     689,   690,    -1,   692,    -1,    -1,    -1,    -1,   697,    -1,
      -1,    -1,    -1,    -1,    -1,   704,    -1,    -1,    -1,    -1,
      -1,   710,   711,    -1,    -1,    -1,    -1,   716,    -1,    -1,
      -1,    -1,   721,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   735,   736,   737,    -1,
      -1,    -1,    -1,    -1,    -1,   744,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   752,    -1,    -1,    -1,    -1,    -1,   758,
     759,    -1,    -1,    -1,    -1,   764,    -1,    -1,    -1,    -1,
      -1,   770,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   783,   784,    -1,    -1,    -1,    -1,
      -1,   790,    -1,    -1,   793,    -1,    -1,    -1,    -1,   798,
     799,   800,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,     8,    -1,    -1,   813,    -1,    -1,    14,    -1,    -1,
      -1,    -1,   821,    -1,    -1,   824,   825,    -1,    -1,    -1,
      -1,    -1,   831,   832,    -1,    -1,   835,    -1,    -1,    -1,
      -1,    -1,   841,    40,    41,    -1,    -1,    -1,    45,    46,
      47,    48,   851,   852,    -1,    52,    53,    54,    55,    56,
      57,    -1,    59,    60,    -1,    62,    63,    64,    65,    66,
      67,    68,    69,    70,    71,    -1,    -1,    -1,    -1,    76,
      -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    -1,    91,    92,    93,    94,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     107,   108,    -1,    -1,    -1,    -1,    -1,    -1,   115,   116,
     117,    -1,   119,   120,   121,   122,    -1,   124,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   133,    -1,   135,   136,
     137,   138,   139,   140,   141,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,    -1,    -1,
      -1,    -1,   159,    -1,    -1,   162,   163,   164,   165,   166,
     167,   168,   169,    -1,    -1,    -1,   173,   174,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     197,   198,   199,    -1,    -1,   202,   203,    -1,    -1,    -1,
     207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   226,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   248,    -1,    -1,    -1,   252,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   260,    -1,    -1,    -1,    -1,    -1,    -1,
     267,    -1,    -1,    -1,   271,   272,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   283,   284,   285,   286,
     287,    -1,    -1,   290,    -1,    -1,    -1,    -1,    -1,    -1,
     297,   298,   299,   300,   301,    -1,    -1,   304,   305,    -1,
      -1,    -1,    -1,   310,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   318,    -1,   320,    -1,    -1,    -1,    -1,    -1,    -1,
     327,    -1,   329,   330,   331,   332,    -1,    -1,    -1,    -1,
     337,   338,   339,   340,   341,    -1,   343,   344,   345,    -1,
      -1,   348,   349,   350,    -1,    -1,    -1,   354,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   364,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   373,   374,    -1,    -1,
      -1,    -1,    -1,    -1,   381,   382,   383,   384,   385,   386,
     387,    -1,    -1,   390,    -1,    -1,    -1,    -1,   395,    -1,
      -1,    -1,   399,    -1,    -1,   402,   403,    -1,   405,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   416,
      -1,    -1,    -1,   420,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   436,
     437,    -1,    -1,    -1,   441,   442,   443,   444,   445,    -1,
      -1,    -1,    -1,    -1,    -1,   452,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   462,    -1,    -1,    -1,   466,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   474,    -1,    -1,
     477,    -1,    -1,    -1,    -1,    -1,    -1,   484,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   492,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     507,   508,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   516,
      -1,   518,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   533,   534,    -1,    -1,
      -1,   538,    -1,    -1,   541,    -1,    -1,    -1,    -1,    -1,
      -1,   548,    -1,    -1,   551,    -1,   553,    -1,    -1,   556,
      -1,    -1,    -1,   560,   561,   562,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   573,   574,    -1,   576,
     577,   578,    -1,    -1,    -1,   582,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   594,   595,   596,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   604,   605,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     617,    -1,    -1,    -1,   621,   622,   623,    -1,    -1,    -1,
     627,    -1,   629,   630,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   641,   642,   643,   644,    -1,   646,
      -1,    -1,    -1,   650,   651,    -1,    -1,    -1,    -1,    -1,
      -1,   658,   659,    -1,    -1,   662,    -1,    -1,    -1,   666,
     667,   668,    -1,    -1,    -1,    -1,    -1,   674,    -1,    -1,
      -1,    -1,   679,    -1,    -1,    -1,    -1,    -1,    -1,   686,
      -1,   688,   689,   690,    -1,   692,    40,    -1,    -1,    -1,
     697,    45,    46,    47,    48,    -1,    -1,   704,    52,    -1,
      -1,    -1,    -1,   710,   711,    -1,    -1,    -1,    -1,   716,
      -1,    -1,    -1,    -1,   721,    -1,    -1,    71,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   735,   736,
     737,    -1,    -1,    -1,    -1,    -1,    -1,   744,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   752,    -1,    -1,    -1,    -1,
      -1,   758,   759,    -1,    -1,    -1,    -1,   764,    -1,    -1,
      -1,    -1,   116,   770,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   783,   784,    -1,    -1,
      -1,    -1,     8,   790,    -1,    -1,   793,    -1,    14,    -1,
      -1,   798,   799,   800,    -1,   149,    -1,   151,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   813,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   821,    41,    -1,   824,   825,    -1,
      -1,    -1,    -1,    -1,   831,   832,    -1,    -1,   835,   183,
      -1,    -1,    -1,    -1,   841,    -1,    62,    63,    64,    65,
      66,    67,    68,    69,   851,   852,    -1,    -1,    -1,    -1,
      76,    -1,    -1,    -1,    80,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    91,    92,    93,    94,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   107,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   117,    -1,    15,   120,   121,   122,    -1,   124,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   154,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,   169,    -1,    -1,    -1,   173,   174,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   310,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   320,    -1,    -1,    -1,
      -1,    -1,   198,   199,    -1,    -1,   202,   203,    -1,    -1,
     102,   207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   349,    -1,    -1,    -1,    -1,
     226,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     142,    -1,   248,    -1,    -1,    -1,   252,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   161,
      -1,   267,    -1,    -1,    -1,   271,   272,    -1,    -1,    -1,
      -1,   405,    -1,    -1,    -1,    -1,    -1,   283,   284,   285,
     286,   287,    -1,    -1,   290,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   433,
      -1,    -1,   436,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   329,   330,   331,   332,    -1,    -1,    -1,
      -1,    -1,   338,   339,    -1,   341,    -1,   343,   344,   345,
      -1,    -1,   348,   477,   350,    -1,    -1,    -1,   354,    -1,
     484,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   373,   374,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   382,   383,   384,   385,
     386,   387,    -1,    -1,   518,    -1,    -1,    -1,    -1,   395,
      -1,    -1,    -1,   399,    -1,    -1,    -1,   403,    -1,   533,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     312,   313,   314,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   324,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   437,    -1,    -1,    -1,   441,   442,   443,   444,   445,
      -1,    -1,   576,    -1,    -1,   579,   452,    -1,    -1,   351,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     466,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   474,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   617,    -1,    -1,   492,    -1,    -1,    -1,
      -1,    -1,    -1,   627,    -1,   629,   630,    -1,    -1,    -1,
     402,   507,   508,    -1,    -1,    -1,   408,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   649,    -1,    -1,    -1,    -1,
     654,    -1,   424,    -1,    -1,   427,   428,   429,   662,    -1,
      -1,    -1,   538,   435,    -1,   541,   438,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   551,    -1,    -1,    -1,    -1,
     556,    -1,    -1,    -1,   560,   561,   562,    -1,   692,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   574,    -1,
     704,   577,   578,    -1,    -1,    -1,   582,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   594,   595,
     596,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   604,   605,
      -1,   735,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   513,    -1,    -1,    -1,   621,   622,   623,    -1,    -1,
      -1,   755,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   641,   642,   643,   644,    -1,
     646,    -1,    -1,    -1,   650,   651,    -1,    -1,    -1,    -1,
      -1,    -1,   658,   659,    -1,    -1,    -1,    -1,    -1,   793,
     666,   667,   668,    -1,    -1,    -1,    -1,    -1,   674,    -1,
      -1,   805,    -1,   679,    -1,    -1,    -1,   811,    -1,    -1,
      -1,    -1,   584,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   697,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     8,    -1,   710,   711,    -1,    -1,    14,    -1,
     716,    -1,    -1,    -1,    -1,   721,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   628,    -1,    -1,    -1,
     736,   737,    -1,    -1,    -1,    41,    -1,    -1,   744,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   752,    -1,    -1,    -1,
      -1,    -1,   758,    -1,    -1,    -1,    62,    63,    64,    65,
      66,    67,    68,    69,    -1,    -1,    -1,    -1,    -1,    -1,
      76,    -1,    -1,    -1,    80,    -1,    -1,    -1,   784,    -1,
      -1,    -1,    -1,    -1,   790,    91,    92,    93,    94,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   700,    -1,
      -1,   107,    -1,    -1,    -1,    -1,    15,    -1,    -1,    -1,
      -1,   117,    -1,    -1,   120,   121,   122,    -1,   124,   825,
      -1,    -1,    -1,   725,   726,   831,   832,    -1,    -1,   835,
      -1,    -1,    -1,    -1,    -1,   841,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   851,   852,    -1,   154,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   162,   163,   164,   165,
     166,   167,   168,   169,    -1,    -1,    -1,   173,   174,    -1,
      79,    -1,   774,     1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   783,    -1,    -1,    -1,    -1,   788,    -1,    -1,    -1,
      -1,    -1,   198,   199,    -1,    -1,   202,   203,    -1,    -1,
      -1,   207,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     812,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   820,    -1,
     226,    -1,    -1,    -1,    -1,    53,    54,    55,    56,    57,
      -1,    59,    60,   142,    -1,    -1,    -1,    -1,   840,    -1,
      -1,    -1,   248,   845,    -1,    -1,   252,    -1,    -1,    -1,
      -1,    -1,   161,   855,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   267,    -1,    -1,    -1,   271,   272,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   283,   284,   285,
     286,   287,    -1,    -1,   290,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   133,    -1,   135,   136,   137,
     138,   139,   140,   141,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   329,   330,   331,   332,    -1,    -1,    -1,
      -1,    -1,   338,   339,    -1,   341,    -1,   343,   344,   345,
      -1,    -1,   348,    -1,   350,    -1,    -1,    -1,   354,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   373,   374,   197,
      -1,    -1,    -1,    -1,    -1,    -1,   382,   383,   384,   385,
     386,   387,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   395,
      -1,    -1,    -1,   399,   400,    -1,    -1,   403,    -1,    -1,
      -1,    -1,   311,   312,   313,   314,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   324,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   437,    -1,    -1,    -1,   441,   442,   443,   444,   445,
      -1,    -1,   351,    -1,    -1,    -1,   452,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     466,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   474,   297,
     298,   299,   300,   301,    -1,    -1,   304,   305,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   407,   408,
     409,   507,   508,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   340,    -1,    -1,   424,    -1,    -1,   427,   428,
     429,    -1,    -1,    -1,    -1,    -1,   435,    -1,    -1,   438,
      -1,    -1,   538,    -1,    -1,   541,   364,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   551,    -1,    -1,    -1,    -1,
     556,    -1,    -1,    -1,   560,   561,   562,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   574,    -1,
      -1,   577,   578,    -1,    -1,    -1,   582,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   594,   595,
     596,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   604,   605,
      -1,    -1,    -1,    -1,   513,    -1,    -1,    -1,    -1,    -1,
      -1,   142,    -1,    -1,    -1,   621,   622,   623,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     161,    -1,    -1,    -1,   462,   641,   642,   643,   644,    -1,
     646,    -1,    -1,    -1,   650,   651,    -1,    -1,    -1,    -1,
      -1,    -1,   658,   659,    -1,    -1,    -1,    -1,    -1,    -1,
     666,   667,   668,    -1,    -1,    -1,    -1,    -1,   674,    -1,
      -1,    -1,    -1,   679,    -1,   584,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   697,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   710,   711,   534,    -1,    -1,    -1,
     716,    -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,   628,
      -1,    -1,     7,    -1,    -1,   553,    -1,    -1,    -1,    -1,
     736,   737,    -1,    -1,    -1,    -1,    -1,    -1,   744,    -1,
      -1,    -1,    -1,    -1,    -1,   573,   752,    -1,    -1,    -1,
      -1,    -1,   758,    -1,    -1,    40,    -1,    -1,    -1,    -1,
      45,    46,    47,    48,    49,    -1,    -1,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    61,    -1,   784,    -1,
      -1,   312,   313,   314,   790,    70,    71,    -1,    -1,    -1,
      -1,   700,    -1,   324,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    88,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   725,   726,   103,   825,
     351,    -1,    -1,   108,    -1,   831,   832,    -1,    -1,   835,
     115,   116,    -1,    -1,   119,   841,   745,    -1,    -1,    -1,
      -1,   126,    -1,    -1,    -1,   851,   852,    -1,   133,    -1,
     135,   136,   137,   138,   139,   140,   141,    -1,    -1,    -1,
     688,   689,   690,    -1,    -1,   774,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   159,    -1,    -1,   408,   409,   788,
      -1,    -1,    -1,    -1,   415,    -1,   417,    -1,    -1,    -1,
      -1,   176,    -1,   424,   803,    -1,   427,   428,   429,    -1,
      -1,    -1,    -1,   812,   435,    -1,    -1,   438,    -1,    -1,
      -1,   820,   197,    -1,    -1,    -1,    -1,    -1,    -1,   828,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   840,    -1,    -1,    -1,   844,   845,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   855,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   251,    -1,    -1,    -1,
     798,   799,   800,    -1,    -1,   260,    -1,    -1,    -1,    -1,
      -1,    -1,   513,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   288,    -1,    -1,   537,    -1,    -1,    -1,
      -1,    -1,   297,   298,   299,   300,   301,    -1,    -1,   304,
     305,    -1,    -1,    -1,    -1,   310,    -1,    -1,    -1,    15,
     315,    -1,    -1,   318,    -1,   320,    -1,    -1,    -1,    -1,
      -1,    -1,   327,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   337,   584,    -1,   340,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   349,   142,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   364,
      -1,    -1,    -1,    -1,   161,    -1,    -1,    -1,     1,    -1,
      -1,     4,    -1,    -1,    -1,    -1,   381,   628,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   390,    -1,    -1,    -1,   394,
      -1,    -1,    -1,    -1,    -1,    -1,   102,   402,    -1,    -1,
     405,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   416,    -1,    -1,    -1,   420,    -1,    -1,   423,    -1,
      53,    54,    55,    56,    57,    -1,    59,    60,    -1,    -1,
      -1,   436,    -1,    -1,    -1,    -1,   142,    70,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   700,
      -1,    -1,    -1,    -1,    -1,   161,    -1,   462,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   477,    -1,   725,   726,    -1,    -1,    -1,   484,
      -1,    -1,   115,    -1,    -1,   118,   119,    -1,    -1,    -1,
      -1,    -1,   125,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     133,    -1,   135,   136,   137,   138,   139,   140,   141,   514,
     761,   516,    -1,   518,    -1,   312,   313,   314,    -1,    -1,
      -1,   772,    -1,   774,    -1,   776,   777,   324,   533,   534,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   788,    -1,    -1,
      -1,    -1,    -1,   548,    -1,    -1,    -1,    -1,   553,    -1,
      -1,    -1,    -1,    -1,   351,    -1,    -1,    -1,    -1,    -1,
      -1,   812,    -1,    -1,   197,    -1,    -1,    -1,   573,   820,
      -1,   576,    -1,    -1,    -1,    -1,   581,    -1,    -1,    -1,
     585,    -1,    -1,    -1,    -1,   291,    -1,    -1,    -1,   840,
      -1,    -1,    -1,    -1,   845,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   855,    -1,   312,   313,   314,    -1,
      -1,   408,   617,    -1,    -1,    -1,    -1,    -1,   324,    -1,
      -1,    -1,   627,    -1,   629,   630,    -1,   424,    -1,    -1,
     427,   428,   429,    -1,    -1,    -1,    -1,    -1,   435,    -1,
      -1,   438,    -1,    -1,    -1,   351,    -1,    -1,    -1,    -1,
     655,    -1,    -1,    -1,    -1,   288,    -1,   662,    -1,    -1,
      -1,    -1,    -1,    -1,   297,   298,   299,   300,   301,    -1,
      -1,   304,   305,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   686,    -1,   688,   689,   690,     1,   692,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   402,    -1,    -1,   704,
      -1,    -1,   408,    -1,    -1,   338,    -1,   340,    -1,    -1,
     715,    -1,    27,    -1,    -1,    -1,   513,    -1,   424,    -1,
      -1,   427,   428,   429,    -1,    -1,    -1,    -1,    -1,   435,
     735,   364,   438,    -1,    -1,    50,    -1,    -1,    53,    54,
      55,    56,    57,    -1,    59,    60,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   759,    70,    -1,   390,    -1,   764,
      -1,    -1,    -1,    -1,    -1,   770,    -1,    -1,    -1,   402,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   783,    -1,
      -1,    -1,   787,   416,    -1,    -1,    -1,   584,   793,    -1,
      -1,    -1,    -1,   798,   799,   800,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   513,   813,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   821,    -1,   133,   824,
     135,   136,   137,   138,   139,   140,   141,    -1,    -1,   462,
      -1,   628,    -1,   838,    -1,    -1,    -1,    -1,    -1,    -1,
     845,    -1,   475,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    38,    -1,    -1,    -1,    -1,    -1,    -1,    45,    46,
      47,    48,    -1,    -1,    -1,    52,    -1,    -1,   584,    -1,
      -1,    -1,   197,   516,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    70,    71,    -1,    -1,    -1,    75,    -1,
      -1,   534,    -1,   700,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   548,    -1,    -1,    -1,    -1,
     553,    -1,   628,    -1,    -1,   558,    -1,    -1,   725,   726,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   115,   116,
     573,    -1,   119,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   277,   278,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   288,   151,    -1,    -1,   774,    -1,    -1,
      -1,    -1,   297,   298,   299,   300,   301,    -1,    -1,   304,
     305,   788,    -1,    -1,   700,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   327,    -1,    -1,   812,    -1,    -1,    -1,   725,
     726,    -1,    -1,   820,    -1,   340,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   357,   840,    -1,    -1,    -1,    -1,   845,   364,
      -1,    -1,    -1,   686,    -1,   688,   689,   690,   855,    -1,
      -1,    -1,    -1,    -1,    -1,   698,   381,    -1,   774,    -1,
      -1,    -1,    -1,    -1,    -1,   390,    -1,   783,    -1,    -1,
      -1,    -1,   788,   260,    -1,    -1,    -1,   402,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   729,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   812,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   820,    -1,     3,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    11,    -1,    -1,    -1,    -1,    -1,
      -1,   764,    -1,   310,   840,    -1,    -1,    -1,    25,   845,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   462,    -1,   855,
     783,    -1,    -1,    -1,    -1,    42,    -1,   790,    -1,    -1,
      -1,    -1,    -1,   340,    -1,   798,   799,   800,    -1,    -1,
      -1,    -1,   349,    -1,    61,    -1,    -1,    -1,    -1,    -1,
     813,    -1,    -1,    -1,    -1,    -1,    73,    74,    75,    -1,
      77,   824,    -1,    -1,    -1,   828,    83,    -1,    -1,    -1,
      -1,   516,    -1,    -1,    -1,    -1,    -1,    -1,    95,    96,
      97,    -1,   845,    -1,    -1,    -1,    -1,    -1,    -1,   534,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   548,    -1,    -1,   123,    -1,   553,   416,
      -1,    -1,    -1,   420,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   573,   436,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   171,   600,    -1,    -1,    -1,     4,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   184,    -1,    -1,
     477,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   205,   206,
      -1,   208,   209,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    53,    54,
      55,    56,    57,    -1,    59,    60,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,   533,    -1,    -1,    -1,
     247,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   686,    -1,   688,   689,   690,   263,   554,   555,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     115,    -1,    -1,   118,   119,    -1,    -1,   294,   295,   296,
     125,    -1,    -1,    -1,    -1,    -1,    -1,   732,   133,    -1,
     135,   136,   137,   138,   139,   140,   141,    -1,    -1,   316,
      -1,    -1,    -1,    -1,   321,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   764,
     627,    -1,   629,   630,    -1,    -1,    -1,    -1,    -1,   346,
     347,    -1,    -1,    -1,    -1,   352,   353,    -1,   783,    -1,
      -1,    -1,   649,    -1,    -1,    -1,    -1,   654,    -1,    -1,
      -1,    -1,   197,   798,   799,   800,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   813,    -1,
      -1,    -1,    -1,   680,    -1,    -1,    -1,    -1,    -1,   824,
      -1,    -1,    -1,    -1,    -1,   692,    -1,   404,    -1,   406,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   704,    -1,    -1,
     845,    -1,    -1,    -1,   421,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   432,   433,   434,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   458,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     757,    -1,   297,   298,   299,   300,   301,    -1,    -1,   304,
     305,    -1,   479,   480,   481,    -1,   483,    -1,   485,   486,
      -1,   488,   489,   490,    -1,    -1,   493,    -1,   495,   496,
     497,   498,    -1,    -1,    -1,    -1,   793,    -1,    -1,    -1,
      -1,    -1,    -1,   338,   511,   340,    -1,    -1,    38,    -1,
      40,    -1,   519,    -1,    -1,    45,    46,    47,    48,    -1,
      -1,    51,    52,    -1,   531,   532,    -1,    -1,    -1,   364,
      -1,    -1,   539,    -1,    -1,    -1,    -1,   544,    -1,    -1,
      -1,    71,    -1,    -1,    -1,    -1,    -1,   844,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   390,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   586,
      -1,   416,    -1,   590,    -1,   115,   116,    -1,    -1,   119,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   149,
      -1,   151,    -1,    -1,    -1,    -1,    -1,   462,   635,   636,
     637,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   645,    -1,
     475,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   183,    -1,    -1,    -1,    -1,    -1,    -1,
      27,    -1,   669,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   682,   683,   684,   685,    -1,
      -1,   516,    -1,    50,    -1,    -1,    53,    54,    55,    56,
      57,    -1,    59,    60,    -1,   702,   703,    -1,    -1,   534,
      -1,    -1,    -1,    70,    -1,    -1,   713,    -1,    -1,    -1,
      -1,    -1,    -1,   548,    -1,    -1,    -1,    -1,   553,    -1,
      -1,    -1,    -1,   558,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   740,    -1,    -1,    -1,    -1,   573,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   753,   754,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   133,    -1,   135,   136,
     137,   138,   139,   140,   141,    -1,    -1,    -1,    -1,   786,
     310,    -1,    -1,    -1,    -1,    -1,    -1,   794,    -1,   319,
     320,    -1,    -1,    -1,   801,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   811,    -1,    -1,    -1,   815,   816,
     817,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   349,
      -1,    -1,   829,   830,    -1,    -1,   833,   834,    -1,   836,
     197,    -1,    -1,    -1,    -1,   842,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   686,    -1,   688,   689,   690,    -1,    -1,    -1,    -1,
      -1,    -1,   392,   698,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   405,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   416,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   729,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   433,    -1,    -1,   436,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   454,    -1,    -1,    -1,    -1,   764,
     297,   298,   299,   300,   301,    -1,    -1,   304,   305,    -1,
      -1,   142,    -1,    -1,    -1,    -1,    -1,   477,    -1,    -1,
      -1,    -1,    -1,    -1,   484,   790,    -1,    -1,    -1,    -1,
     161,    -1,    -1,   798,   799,   800,    -1,    -1,    -1,    -1,
      -1,    -1,    27,   340,    -1,    -1,    -1,    -1,   813,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   518,   824,
      -1,    -1,    -1,   828,    -1,    50,    -1,   364,    53,    54,
      55,    56,    57,   533,    59,    60,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   381,    70,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   390,    -1,   555,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   402,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   576,    -1,    -1,   579,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   133,    -1,
     135,   136,   137,   138,   139,   140,   141,   617,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   462,    -1,   627,    -1,   629,
     630,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   312,   313,   314,    -1,    -1,    -1,    -1,    -1,   649,
      -1,    -1,    -1,   324,   654,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   662,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   197,    -1,    -1,    -1,    -1,    -1,    -1,   516,
     351,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   692,    -1,    -1,    -1,    -1,   534,    -1,    -1,
      -1,    -1,    -1,    -1,   704,    -1,    -1,    -1,    -1,    -1,
      -1,   548,    -1,    -1,    -1,    -1,   553,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,
      -1,    -1,    -1,    -1,    -1,   735,   573,   408,   409,    -1,
      -1,    -1,    -1,    -1,   415,    -1,   417,    -1,   161,    -1,
      -1,    -1,    -1,   424,   754,   755,   427,   428,   429,    -1,
      -1,    -1,    -1,   600,   435,    -1,    -1,   438,    -1,    -1,
      -1,    -1,   297,   298,   299,   300,   301,    -1,    -1,   304,
     305,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   793,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   805,    -1,    -1,    -1,    -1,
      -1,   811,    -1,    -1,    -1,   340,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   364,
      -1,    -1,   513,    -1,   844,    -1,    -1,    -1,    -1,   686,
      -1,   688,   689,   690,    -1,    -1,   381,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   390,   537,    -1,    -1,    -1,
      -1,    -1,    -1,    40,    -1,    -1,    -1,   402,    45,    46,
      47,    48,    -1,    -1,    -1,    52,    53,    54,    55,    56,
      57,    -1,    59,    60,    -1,   732,    -1,    -1,    -1,   312,
     313,   314,    -1,    70,    71,    -1,    -1,    -1,    -1,    -1,
      -1,   324,    -1,   584,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    88,    -1,    -1,    -1,    -1,    -1,   764,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   462,   351,    -1,
      -1,   108,    -1,    -1,    -1,    -1,   783,    -1,   115,   116,
      -1,    -1,   119,    -1,    -1,    -1,    -1,   628,    -1,    -1,
      -1,   798,   799,   800,    -1,    -1,   133,    -1,   135,   136,
     137,   138,   139,   140,   141,    -1,   813,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   824,    -1,    -1,
      -1,   516,   159,    -1,    -1,   408,   409,    -1,    -1,    -1,
      -1,    -1,   415,    -1,   417,    -1,    -1,    -1,    -1,   534,
      -1,   424,    -1,    -1,   427,   428,   429,    -1,    -1,    -1,
      -1,    -1,   435,   548,    -1,   438,    -1,    -1,   553,   700,
     197,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   573,    -1,
      -1,    -1,    -1,    -1,   725,   726,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   600,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     761,    -1,    -1,   260,    -1,    -1,    -1,    -1,    -1,    -1,
     513,   772,    -1,   774,    -1,   776,   777,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   788,    -1,    -1,
      -1,    -1,    -1,    -1,   537,    -1,    -1,    -1,    -1,    -1,
     297,   298,   299,   300,   301,    -1,    -1,   304,   305,    -1,
      -1,   812,    -1,   310,    -1,    -1,    -1,    -1,    -1,   820,
      -1,   318,    -1,   320,    -1,    -1,    -1,    -1,    -1,    -1,
     327,   686,    -1,   688,   689,   690,    -1,    -1,    -1,   840,
     337,   584,    -1,   340,   845,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   349,    -1,   855,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   364,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   619,   732,    -1,    -1,
      -1,    -1,    -1,    -1,   381,   628,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   390,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   405,   764,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   416,
      -1,    -1,    -1,   420,    -1,    -1,    -1,    -1,   783,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   436,
      -1,    -1,    -1,   798,   799,   800,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   700,   813,    -1,
      -1,    -1,    -1,    -1,    -1,   462,    -1,    -1,    -1,   824,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     477,    -1,   725,   726,    -1,    40,    -1,   484,    -1,    -1,
      45,    46,    47,    48,    -1,    -1,    -1,    52,    53,    54,
      55,    56,    57,    -1,    59,    60,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    70,    71,    -1,    -1,   516,
      -1,   518,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   774,    -1,    88,    -1,    -1,   533,   534,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   788,    -1,    -1,    -1,    -1,
      -1,   548,    -1,   108,    -1,    -1,   553,    -1,    -1,    -1,
     115,   116,    -1,   806,   119,    -1,    -1,    -1,    -1,   812,
      -1,    -1,    -1,    -1,    -1,    -1,   573,   820,   133,   576,
     135,   136,   137,   138,   139,   140,   141,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   840,    -1,    -1,
      -1,    -1,   845,    -1,   159,   142,    -1,    -1,    -1,    -1,
      -1,    -1,   855,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     617,    -1,    -1,    -1,   161,    -1,    -1,    -1,    -1,    -1,
     627,    -1,   629,   630,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   197,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   142,    -1,
      -1,    -1,    -1,    -1,    -1,   662,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   161,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   686,
      -1,   688,   689,   690,    -1,   692,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   260,    -1,   704,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   735,    -1,
      -1,    -1,   297,   298,   299,   300,   301,    -1,    -1,   304,
     305,    -1,    -1,    -1,    -1,   310,    -1,    -1,    -1,    -1,
      -1,    -1,   759,   318,    -1,   320,    -1,   764,    -1,    -1,
      -1,    -1,   327,   770,    -1,   312,   313,   314,    -1,    -1,
      -1,    -1,   337,    -1,    -1,   340,    -1,   324,    -1,    -1,
      -1,    -1,    -1,    -1,   349,    -1,   793,    -1,    -1,    -1,
      -1,   798,   799,   800,    -1,    -1,    -1,    -1,    -1,   364,
      -1,    -1,    -1,    -1,   351,    -1,   813,    -1,    -1,   142,
      -1,    -1,    -1,    -1,   821,    -1,   381,   824,   312,   313,
     314,    -1,    -1,    -1,    -1,   390,    -1,    -1,   161,    -1,
     324,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     405,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   416,    -1,    -1,    -1,   420,    -1,   351,    -1,    -1,
      -1,   408,   409,    -1,    -1,    -1,    -1,    -1,   415,    -1,
     417,   436,    -1,    -1,    -1,    -1,    -1,   424,    -1,   142,
     427,   428,   429,    -1,    -1,    -1,    -1,    -1,   435,    -1,
      -1,   438,    -1,    -1,    -1,    -1,    -1,   462,   161,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   477,    -1,   408,   409,    -1,    -1,    -1,   484,
      -1,   415,    -1,   417,    -1,    -1,    -1,    -1,    -1,    -1,
     424,    -1,    -1,   427,   428,   429,    -1,    -1,    -1,    -1,
      -1,   435,    -1,    -1,   438,    -1,    -1,    -1,    -1,    -1,
      -1,   516,    -1,   518,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   513,    -1,   533,   534,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   312,
     313,   314,    -1,   548,    -1,    -1,    -1,    -1,   553,    -1,
     537,   324,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   573,    -1,
      -1,   576,    -1,    -1,    -1,    -1,    -1,    -1,   351,   513,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   584,   142,    -1,
      -1,    -1,    -1,   537,    -1,    -1,    -1,    -1,    -1,   312,
     313,   314,   617,    -1,    -1,    -1,    -1,   161,    -1,    -1,
      -1,   324,   627,    -1,   629,   630,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   408,   409,    -1,    -1,    -1,
      -1,   628,   415,    -1,   417,    -1,    -1,    -1,   351,    -1,
     584,   424,    -1,    -1,   427,   428,   429,   662,    -1,    -1,
      -1,    -1,   435,    -1,    -1,   438,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   686,    -1,   688,   689,   690,    -1,   692,    -1,    -1,
      -1,    -1,    -1,    -1,   628,    -1,    -1,    -1,    -1,   704,
      -1,    -1,    -1,    -1,    -1,   408,   409,    -1,    -1,    -1,
      -1,    -1,   415,   700,   417,    -1,    -1,    -1,    -1,    -1,
      -1,   424,    -1,    -1,   427,   428,   429,    -1,    -1,    -1,
     735,    -1,   435,    -1,    -1,   438,    -1,    -1,   725,   726,
     513,    -1,    -1,    -1,    -1,    -1,    -1,   734,    -1,    -1,
      -1,    -1,    -1,    -1,   759,    -1,    -1,    -1,    -1,   764,
      -1,    -1,    -1,    -1,   537,   770,   700,    -1,   312,   313,
     314,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     324,    -1,    -1,    -1,    -1,   772,    -1,   774,   793,   776,
     777,   725,   726,   798,   799,   800,    -1,    -1,    -1,    -1,
     734,   788,    -1,    -1,    -1,    -1,    -1,   351,   813,    -1,
     513,   584,    -1,    -1,    -1,    -1,   821,    -1,    -1,   824,
      -1,    -1,    -1,    -1,    -1,   812,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   820,   537,    -1,    -1,    -1,   772,    -1,
     774,    -1,   776,   777,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   840,   788,   628,    -1,    -1,   845,    -1,
      -1,    -1,    -1,    -1,   408,   409,    -1,    -1,   855,    -1,
      -1,   415,    -1,   417,    -1,    -1,    -1,    -1,   812,    -1,
     424,   584,    -1,   427,   428,   429,   820,    -1,    -1,    -1,
      -1,   435,    -1,    -1,   438,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   840,    -1,    -1,    -1,
      -1,   845,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   855,    -1,    -1,    -1,   628,    -1,   700,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   725,   726,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   513,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   761,    -1,
      -1,    -1,    -1,   537,    -1,    -1,    -1,   700,    -1,   772,
      -1,   774,    -1,   776,   777,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   788,    -1,    -1,    -1,    -1,
      -1,    -1,   725,   726,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   812,
     584,    -1,    -1,    -1,    -1,    -1,    -1,   820,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   840,    -1,   772,
      -1,   774,   845,   776,   777,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   855,    -1,   628,   788,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   812,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   820,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   840,    -1,    -1,
      -1,    -1,   845,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   855,    -1,    -1,    -1,   700,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   725,   726,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     774,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   788,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   812,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   820,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   840,    -1,    -1,    -1,
      -1,   845,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   855
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint16 yystos[] =
{
       0,   858,   859,     0,   860,   861,   862,   865,   866,   356,
     358,   863,   864,   867,   868,   876,   877,   253,   875,   899,
     900,   864,   323,   571,   878,   881,   204,   204,   172,  1030,
    1032,   144,   901,   902,   882,   879,   525,   890,   890,   764,
     764,   204,   567,  1291,  1295,   768,  1033,   661,   371,   970,
     971,    17,   104,   157,   160,   162,   181,   268,   426,   511,
     515,   616,   653,   699,   701,   731,   845,   903,   904,   905,
     906,   910,   921,   927,   928,   929,   930,   931,   937,   946,
     948,   953,   956,   961,   962,   964,   965,   966,   967,   968,
     969,   764,   764,   764,   875,   875,   764,   204,  1319,  1320,
     661,  1034,   764,   661,   281,   972,     1,   845,  1814,  1814,
     714,   686,  1951,   381,  1924,  1924,  1924,  1814,   686,   764,
     764,  1924,   764,   764,   102,   145,  1908,   932,   905,   764,
     930,   362,   947,   424,   572,   883,   883,    33,   891,   892,
     236,   869,   870,   871,   231,   873,  1292,     1,     5,     9,
      16,    24,    81,    82,   110,   130,   134,   148,   186,   192,
     194,   197,   201,   218,   249,   265,   274,   317,   325,   328,
     333,   359,   367,   369,   372,   375,   449,   455,   457,   460,
     473,   523,   543,   580,   589,   591,   592,   607,   620,   625,
     632,   638,   657,   671,   677,   695,   709,   719,   720,   727,
     730,   743,   764,   785,   796,   802,   823,   850,  1321,  1322,
    1345,  1350,  1354,  1359,  1381,  1385,  1393,  1397,  1398,  1399,
    1402,  1407,  1412,  1451,  1455,  1457,  1460,  1474,  1478,  1481,
    1484,  1488,  1489,  1496,  1506,  1509,  1512,  1530,  1532,  1536,
    1539,  1543,  1550,  1563,  1565,  1580,  1581,  1591,  1594,  1595,
    1599,  1605,  1606,  1614,  1621,  1638,  1648,  1657,  1663,  1674,
    1678,  1680,  1683,  1686,  1689,  1701,  1719,  1321,   764,   280,
     656,  1031,  1035,  1036,  1038,   764,   764,   974,   938,   309,
     959,  1915,  1924,  1924,   845,  1808,  1854,   127,  1808,  1924,
    1924,   911,   922,  1808,   907,   845,   954,   955,  1095,   845,
     949,   950,   951,  1815,   424,   518,   521,   933,   935,   936,
    1624,  1857,  1924,   845,    34,   885,   885,  1924,   183,   894,
     872,   871,   874,   453,   712,   717,   766,  1363,     5,     9,
      16,    24,    81,    82,   110,   130,   134,   148,   186,   197,
     201,   219,   222,   223,   224,   225,   227,   228,   229,   230,
     232,   233,   234,   235,   237,   238,   239,   240,   241,   242,
     243,   244,   245,   246,   249,   265,   274,   317,   325,   328,
     333,   359,   367,   369,   372,   375,   449,   455,   457,   460,
     473,   523,   543,   589,   607,   625,   632,   638,   657,   677,
     695,   709,   719,   720,   727,   730,   743,   764,   785,   796,
     802,   850,  1893,  1894,  1895,  1323,  1346,  1351,  1355,  1360,
    1382,  1386,  1394,  1403,  1400,  1408,  1413,  1452,  1456,  1458,
    1461,  1475,  1479,  1482,  1485,   326,   626,  1373,  1477,  1490,
    1497,  1507,  1510,  1513,  1531,  1533,  1537,  1540,   672,  1544,
    1551,  1564,  1566,  1582,  1592,  1596,  1600,  1607,  1615,  1622,
    1639,  1649,   424,   584,   647,   700,   749,   855,  1661,  1662,
    1768,  1849,  1850,  1854,  1664,  1675,   565,  1679,  1681,  1308,
    1684,  1687,  1690,  1702,  1720,   848,  1085,  1087,  1088,     1,
     845,  1798,   389,   665,   973,   975,  1924,  1924,    21,   462,
    1808,   424,   362,   517,  1973,   424,   783,   101,   114,   446,
     570,   664,   675,   845,   912,   913,   914,   915,   916,   917,
     918,   920,  1907,  1982,   322,   570,   923,   924,   925,   908,
     920,   955,  1924,   950,    28,   381,  1815,  1926,  1624,   381,
     714,  1943,  1924,   157,  1814,   424,   764,   880,   465,   704,
     707,   708,   893,   639,   250,   895,   424,   572,   884,   884,
     100,   821,  1296,   520,   845,  1324,  1327,  1799,  1849,    10,
      15,   142,   155,   161,   312,   313,   314,   324,   351,   408,
     409,   415,   417,   424,   427,   428,   429,   435,   438,   453,
     513,   537,   584,   628,   700,   725,   726,   738,   774,   788,
     812,   820,   840,   855,  1347,  1791,  1819,  1820,  1822,  1849,
    1862,  1863,  1864,  1865,  1866,  1867,  1868,   409,   772,   776,
     777,  1352,  1786,  1787,  1788,  1789,  1790,  1791,  1825,  1849,
    1863,  1865,   424,  1356,  1357,  1804,  1805,  1806,  1854,  1361,
    1363,   424,   572,  1383,  1384,  1835,  1849,   843,  1387,  1388,
    1390,  1798,    10,  1395,  1791,  1792,  1793,  1817,  1852,  1853,
    1854,  1863,   768,  1404,  1798,    15,  1401,  1849,  1851,   370,
     388,   530,   742,  1409,  1411,   306,   363,   424,   450,   520,
     704,   728,   770,   843,  1414,  1415,  1416,  1417,  1419,  1424,
    1429,  1432,  1433,  1436,  1438,  1819,  1835,  1849,  1453,  1820,
    1409,  1363,  1459,   767,   778,  1462,  1463,  1464,  1771,  1772,
    1773,   322,   540,   543,   570,   661,  1476,  1480,  1816,  1817,
    1483,  1854,   759,  1486,  1962,  1820,  1770,  1771,  1498,  1816,
     845,  1508,  1800,   149,   843,  1235,  1441,  1511,  1849,  1514,
    1515,  1849,  1862,  1865,  1640,  1844,  1845,  1854,  1235,  1441,
    1534,   155,  1538,  1820,  1541,  1820,   276,   370,   388,   530,
    1545,  1546,  1547,   362,  1552,  1766,  1917,   845,  1799,  1567,
    1798,  1583,  1799,  1593,  1794,  1854,  1597,  1798,   768,  1601,
    1794,  1795,    15,  1608,  1796,  1854,  1616,  1799,   253,   398,
     453,  1623,  1626,  1627,  1630,  1631,  1632,  1633,  1634,  1635,
    1636,  1637,  1768,  1801,  1802,  1816,  1843,  1845,  1854,  1640,
    1650,  1798,  1658,  1849,   776,  1855,  1856,  1665,  1666,  1667,
     155,   738,  1676,  1819,  1682,  1800,   764,   845,  1309,  1310,
    1313,  1314,  1319,  1685,  1846,  1849,  1688,  1798,   424,  1691,
    1836,  1849,  1865,  1703,  1849,  1721,  1795,   661,   132,  1066,
     217,   678,   691,  1089,  1090,  1092,  1103,  1105,  1107,  1878,
     764,  1037,   764,   492,   524,  1886,   459,   649,  1020,  1021,
    1022,  1023,  1025,    36,   213,   351,   424,   438,   465,   584,
     700,   705,   706,   855,   939,   940,   941,   944,   351,   424,
     438,   584,   700,   774,   855,   957,   958,  1861,   844,   963,
    1966,  1854,   673,   675,   692,  1954,   114,  1924,   764,   914,
     764,   845,   913,   105,  1924,    15,   324,   845,   926,   845,
       1,   764,   925,   909,  1966,  1862,   424,   952,  1858,  1943,
     381,  1814,  1814,   934,   935,   886,  1924,   454,  1935,  1924,
     376,   897,   764,   764,  1298,  1297,   626,  1305,   222,  1344,
    1325,   450,  1934,   318,   517,  1940,  1864,  1849,   776,   776,
     776,  1870,   776,  1850,  1863,  1865,  1973,  1973,   776,   776,
     776,   776,  1973,  1844,   776,  1870,   223,  1349,   759,  1348,
    1820,   760,   776,  1869,   776,   776,  1850,  1863,  1865,  1790,
    1849,  1786,  1790,   102,   772,   777,   763,   773,   275,   368,
    1881,  1357,   759,  1973,   224,  1380,  1766,  1384,  1391,  1798,
     601,   795,  1389,  1966,  1979,  1940,   225,  1396,   259,   765,
    1793,  1971,   639,  1887,  1855,  1856,  1405,  1798,   227,  1406,
     593,  1948,   150,  1910,  1849,   742,  1959,   742,  1799,   334,
    1439,    74,  1905,   228,  1450,   269,   503,  1734,  1736,  1738,
    1417,  1818,  1819,  1418,  1430,  1439,   807,   808,   809,   810,
     229,  1454,    78,   377,   424,   230,  1473,    23,   839,  1465,
    1466,  1467,  1469,    26,   275,   381,   492,   526,   761,   763,
     772,   773,   776,   777,  1774,  1775,  1777,  1820,  1924,   170,
    1477,  1817,  1803,   748,  1492,  1499,  1966,  1800,  1849,     8,
      14,    41,    62,    63,    64,    65,    66,    67,    68,    69,
      76,    80,    91,    92,    93,    94,   107,   117,   120,   121,
     122,   124,   154,   162,   163,   164,   165,   166,   167,   168,
     169,   173,   174,   198,   199,   202,   203,   207,   226,   248,
     252,   267,   271,   272,   283,   284,   285,   286,   287,   290,
     329,   330,   331,   332,   338,   339,   341,   343,   344,   345,
     348,   350,   354,   373,   374,   382,   383,   384,   385,   386,
     387,   395,   399,   400,   403,   437,   441,   442,   443,   444,
     445,   452,   466,   474,   507,   508,   538,   541,   551,   556,
     560,   561,   562,   574,   577,   578,   582,   594,   595,   596,
     604,   605,   621,   622,   623,   641,   642,   643,   644,   646,
     650,   651,   658,   659,   666,   667,   668,   674,   679,   697,
     710,   711,   716,   736,   737,   744,   752,   758,   784,   790,
     825,   831,   832,   835,   841,   851,   852,  1240,  1242,  1244,
    1246,  1248,  1250,  1252,  1254,  1257,  1259,  1261,  1262,  1264,
    1266,  1267,  1269,  1271,  1274,  1275,   401,   692,   757,  1276,
    1277,   152,   612,   739,  1516,  1517,  1519,  1520,  1642,   776,
    1855,   492,   721,  1237,  1238,  1239,  1240,  1885,  1276,   233,
    1535,  1820,   759,   234,  1542,    78,  1546,   681,  1548,    79,
     311,   407,   409,   424,  1097,  1556,  1557,  1558,  1804,  1837,
    1849,  1854,  1862,  1865,  1966,   749,   237,  1579,   473,   559,
    1884,   238,  1590,   450,   663,  1584,   318,  1722,   239,  1598,
    1948,   845,   240,  1604,  1722,  1796,   241,  1613,   828,  1609,
     318,  1617,  1618,  1831,  1833,  1849,  1863,  1865,   269,  1633,
    1635,  1816,   566,   759,  1802,   210,   759,   804,  1625,    39,
    1855,   242,  1656,   291,   392,   398,  1652,  1373,  1659,  1820,
    1966,  1784,  1786,   776,  1856,   243,  1673,   377,  1668,  1669,
    1820,  1849,  1844,   244,  1677,   318,  1800,   661,   764,   764,
     318,   593,   598,  1949,   245,  1700,   187,  1692,  1849,   246,
    1726,  1722,   764,   661,   430,  1160,  1095,  1095,  1095,  1090,
     764,     1,   288,  1093,  1094,  1095,   846,  1039,   524,  1814,
    1026,   593,   695,   696,  1024,     1,   764,  1022,   941,    23,
     751,   362,   958,   960,   751,   549,  1924,   675,  1857,   764,
    1924,   845,  1810,   378,   885,     1,   324,   378,   885,   764,
     764,   180,  1858,  1814,   764,   131,   277,   599,   770,   887,
     888,   889,  1924,   111,   717,   766,   896,   640,   764,    78,
    1299,  1300,  1301,  1906,  1299,   520,   845,   764,    38,    51,
     319,   454,  1328,  1329,  1330,  1334,  1337,  1902,  1903,  1966,
     156,    31,    32,   118,   125,   128,   175,   177,   178,   253,
     255,   262,   269,   416,   420,   652,   754,   818,   845,  1333,
    1802,  1969,   249,   570,  1829,  1850,   759,  1784,  1786,  1876,
    1784,  1877,   761,  1784,  1872,  1873,   845,   845,  1786,  1875,
    1875,  1875,  1824,  1849,  1863,  1865,  1874,   845,   759,  1824,
    1871,    10,  1791,  1792,  1820,  1854,  1863,   326,  1864,  1786,
    1824,  1784,   761,   368,  1882,  1787,  1787,  1788,  1788,  1788,
     626,  1353,   569,  1358,  1806,  1364,  1365,  1836,  1849,  1389,
     431,   477,  1915,  1850,  1786,   454,  1888,  1856,  1798,   624,
    1572,  1573,  1574,  1410,  1966,   843,  1819,  1316,  1317,  1316,
    1737,  1738,  1735,  1736,   806,  1334,  1337,  1420,  1421,  1422,
    1966,   806,  1440,   843,  1734,  1734,  1734,  1734,  1820,  1792,
    1820,   821,  1367,  1464,    27,   767,   778,  1470,  1471,  1772,
     839,  1467,  1468,   839,  1316,  1776,  1777,  1775,    18,    19,
      20,   106,   259,   335,   336,   410,   411,   470,   502,   511,
     520,   557,   765,   769,   771,  1778,  1779,  1780,  1781,  1782,
    1783,   189,  1487,  1804,   219,  1491,  1493,    15,    18,    21,
      22,   462,   463,   511,   512,  1500,  1504,   288,   362,   765,
    1918,  1273,  1857,  1918,  1275,  1918,  1918,  1918,  1277,  1831,
      15,   102,   291,   402,   783,  1523,  1524,  1525,  1518,  1519,
     211,   521,   844,  1644,  1941,  1981,   381,   765,  1925,  1238,
      28,   381,   765,  1927,     3,    11,    25,    42,    61,    73,
      74,    75,    77,    83,    95,    96,    97,   123,   171,   184,
     205,   206,   208,   209,   247,   263,   294,   295,   296,   316,
     321,   346,   347,   352,   353,   404,   406,   421,   432,   433,
     434,   458,   479,   480,   481,   483,   485,   486,   488,   489,
     490,   493,   495,   496,   497,   498,   511,   519,   531,   532,
     539,   544,   586,   590,   635,   636,   637,   645,   669,   682,
     683,   684,   685,   702,   703,   713,   740,   753,   754,   786,
     794,   801,   811,   815,   816,   817,   829,   830,   833,   834,
     836,   842,  1241,  1243,  1245,  1247,  1249,  1251,  1253,  1255,
    1256,  1258,  1260,  1263,  1265,  1268,  1270,   759,  1816,  1792,
    1820,  1966,  1572,  1940,     1,    54,    55,    56,    57,    59,
     135,   136,   297,   298,   299,   300,   301,   302,   303,   304,
     305,   424,   553,   573,  1098,  1099,  1100,  1101,  1102,  1124,
    1827,  1850,  1098,   340,  1767,  1767,   803,   828,   751,   756,
     745,  1948,   377,  1842,  1849,  1862,  1865,   377,  1568,  1572,
     221,  1610,  1849,  1610,  1849,  1619,  1966,   759,   759,   759,
     759,  1624,   249,   692,   767,   778,  1820,    78,    52,    71,
     349,   405,   436,   533,   629,   793,  1628,  1629,  1924,  1651,
    1966,  1820,   261,   491,   129,   670,   761,  1785,   762,  1786,
    1849,  1669,   187,  1670,   318,   318,  1792,  1820,  1311,  1831,
    1906,   377,  1695,    13,    51,  1723,  1724,  1086,   764,   661,
     422,  1162,  1106,  1108,   610,   764,   764,  1091,   145,    72,
     113,   172,   394,   414,   593,   597,   613,   615,   764,   824,
    1040,  1041,  1043,  1047,  1048,  1051,  1052,  1058,  1061,  1063,
    1064,  1924,   976,   768,  1914,    29,  1899,   764,   942,   944,
     845,   996,  1861,   424,  1857,   426,   733,   819,   919,  1810,
     454,   889,   570,  1946,   131,    44,   467,   468,   469,   575,
     780,   781,   789,  1889,  1924,  1300,   692,   797,  1302,  1303,
     602,   824,  1293,   855,  1838,  1843,  1857,  1924,   262,   269,
     501,   503,  1727,  1729,  1730,  1732,  1733,  1330,   115,   119,
     416,   555,  1335,  1336,  1968,   754,    40,    45,    46,    47,
      48,    52,    71,   116,   149,   151,   183,   310,   320,   349,
     405,   433,   436,   477,   484,   518,   533,   576,   579,   617,
     627,   629,   630,   649,   654,   662,   704,   735,   755,   793,
     805,   811,  1338,  1341,  1342,  1343,  1911,  1947,  1727,   854,
     853,  1831,  1727,   392,   714,   509,  1326,   461,   424,  1830,
    1850,  1849,  1785,   761,  1785,   761,   761,   547,   761,  1824,
    1785,   761,   761,   761,  1785,   761,  1844,  1785,   761,  1940,
     506,   693,  1739,  1741,  1743,  1855,  1856,  1792,   762,   761,
     761,   759,  1883,  1353,  1817,   759,  1804,  1362,   471,   572,
    1366,    34,  1392,  1966,   631,   609,  1739,  1924,   311,  1915,
     380,   504,  1760,  1761,  1763,  1765,   392,  1434,  1425,  1318,
     157,   158,   563,   845,  1423,  1802,  1421,    45,    46,    47,
      48,    52,    70,    71,   116,   151,   260,   310,   349,   436,
     478,   533,   649,   654,   692,   704,   793,  1342,  1449,  1849,
    1431,  1437,   326,  1739,   326,  1368,    23,   751,  1472,   529,
    1470,   855,  1778,  1962,  1941,  1494,   232,  1495,  1316,  1962,
     612,  1501,  1962,  1849,  1849,  1849,  1849,  1849,   759,    78,
    1524,  1526,  1831,    15,   102,   402,   783,  1521,  1522,  1831,
    1847,  1849,   211,   114,   675,  1645,  1967,    35,   190,  1142,
     362,   527,  1919,  1272,  1849,  1857,  1819,  1816,  1739,   326,
      15,   477,   589,  1019,  1797,  1798,  1098,   764,  1917,  1557,
     235,   764,  1553,  1555,   274,  1559,  1770,  1560,  1561,  1849,
    1804,  1724,  1568,  1849,  1849,   221,   499,  1749,  1752,  1754,
    1602,  1603,  1966,  1316,   839,   839,  1611,  1612,  1723,   216,
     220,   264,  1849,  1831,   518,   855,  1840,  1841,  1843,  1857,
    1830,   517,  1820,  1624,  1624,  1624,  1624,  1624,  1624,  1624,
    1624,  1629,   492,   502,  1653,  1654,  1655,  1779,  1885,  1760,
     407,   692,  1980,   714,  1956,  1956,  1786,   761,  1786,  1672,
    1966,  1906,  1849,  1844,  1739,   326,  1315,  1857,   759,    15,
    1693,  1694,  1879,  1696,  1849,  1672,  1696,  1572,    12,  1896,
    1087,  1067,   764,   661,   613,  1164,   824,  1152,  1119,  1120,
    1924,  1854,    27,    50,    53,    54,    55,    56,    57,    59,
      60,    70,   133,   135,   137,   138,   139,   140,   141,   197,
     297,   298,   299,   300,   301,   305,   340,   364,   390,   462,
     516,   534,   548,   553,   573,   600,   686,   688,   689,   690,
     732,   798,   799,   800,   813,  1113,  1114,  1115,  1116,  1117,
    1120,  1121,  1122,  1123,  1124,  1127,  1130,  1147,  1148,  1150,
    1151,  1152,  1157,  1158,  1159,  1924,  1953,  1096,  1924,   146,
    1909,  1924,   593,   598,  1978,  1978,  1924,  1909,  1924,  1935,
    1924,    28,  1898,   517,  1065,  1814,   277,   327,     1,   977,
     978,   741,  1958,  1915,   943,   944,   102,   456,   847,  1977,
     467,   468,   575,   789,   898,  1924,   692,  1303,   524,  1304,
     182,  1306,    72,  1731,  1732,  1339,  1340,  1847,  1849,  1728,
    1729,  1316,   509,  1937,  1937,  1937,   424,  1839,  1843,  1859,
    1924,  1924,  1924,   215,  1343,   101,   210,   804,    13,  1897,
     392,   692,  1727,   692,  1331,  1332,  1334,  1887,   734,  1869,
     734,  1869,   761,  1808,  1869,  1869,  1869,  1824,  1887,   402,
     783,  1869,  1850,  1316,  1316,  1742,  1743,  1740,  1741,  1856,
    1739,   761,  1786,  1869,  1869,  1834,  1849,  1862,  1767,   477,
    1889,  1786,  1316,  1316,  1764,  1765,  1762,  1763,  1835,  1440,
     183,   355,   757,   790,  1373,  1426,  1427,  1428,  1319,  1924,
     416,   652,  1924,   256,   258,  1278,  1279,  1913,  1962,  1924,
     210,   804,  1924,    75,   340,   420,   554,   680,   757,  1334,
    1442,  1443,  1444,  1445,  1446,  1447,  1448,  1966,  1442,  1792,
    1793,  1792,  1793,  1369,  1370,  1371,  1906,  1471,  1771,  1316,
    1849,  1316,   219,   824,  1502,  1503,  1504,   748,  1505,  1960,
     824,  1832,  1834,  1831,    78,    13,    51,  1527,  1528,  1529,
    1522,  1527,   309,   675,  1641,  1924,   392,  1929,   527,  1792,
     529,  1944,  1944,   522,   630,  1549,  1798,  1966,  1849,  1316,
      13,   318,    12,   361,  1569,  1570,  1571,  1573,  1576,  1603,
    1966,   172,   482,  1585,  1587,  1589,  1316,  1316,  1753,  1754,
    1752,  1760,   431,   477,  1771,  1770,  1611,   612,  1620,  1820,
    1779,  1849,  1780,  1781,  1782,  1783,  1786,  1660,  1820,  1660,
     761,   505,   775,  1744,  1746,  1748,   553,   692,  1671,  1820,
    1887,  1887,  1792,   764,  1832,   526,  1831,   188,  1697,   739,
    1699,  1602,   536,  1802,  1838,  1068,  1161,   764,   661,   652,
    1222,  1926,   824,   327,   751,  1104,   407,   511,   687,   797,
    1952,   797,  1952,   797,  1952,   797,  1952,   797,  1952,   839,
    1964,  1940,   635,  1950,   212,  1133,  1857,  1850,   381,   404,
     635,  1149,  1924,  1115,   277,   278,   357,   402,   783,    34,
     318,  1109,  1901,   327,  1857,   996,  1926,  1926,   845,  1811,
    1812,   520,   704,  1974,   424,  1808,  1813,  1857,   828,  1924,
     282,   356,   845,  1049,  1814,   764,     6,    25,    37,   365,
     416,   528,   535,   593,   606,   618,   681,   695,   764,   768,
     979,   980,   988,   990,   995,   997,   998,   999,  1000,  1003,
    1004,  1005,  1006,  1009,  1015,  1016,  1018,  1948,  1967,  1909,
    1797,    23,    40,   183,  1857,  1924,   845,   764,  1294,  1316,
    1838,  1838,  1838,  1838,  1838,  1838,  1924,  1769,  1841,  1769,
    1839,  1917,  1924,  1924,  1332,  1739,   318,  1891,   761,  1367,
     197,   660,   756,  1435,  1925,  1925,  1925,  1925,  1820,  1428,
    1838,  1838,   416,   652,   221,  1838,  1769,  1769,  1838,   362,
     381,  1928,  1838,  1899,  1924,  1443,   477,  1449,  1739,   608,
    1739,   608,  1370,   520,  1302,   147,   602,   824,  1494,  1503,
     172,  1912,  1962,  1527,  1527,  1832,   770,  1922,  1922,  1529,
    1528,   370,   821,  1646,  1808,  1643,  1739,   431,   477,   235,
    1554,  1561,  1820,  1941,   431,   392,  1577,  1575,  1576,  1966,
     360,   391,   837,  1316,  1316,  1588,  1589,  1586,  1587,   431,
    1316,  1316,   416,  1930,  1316,  1316,  1747,  1748,  1745,  1746,
    1924,  1739,  1891,  1739,   814,  1312,  1704,  1694,  1917,   156,
    1698,  1917,  1744,   257,   500,  1725,  1755,  1757,  1759,  1761,
     416,   420,  1931,    89,  1069,  1070,  1087,  1163,   764,   661,
      26,   275,   526,   761,   763,   772,   773,   776,   777,  1097,
    1112,  1154,  1155,  1924,  1854,   855,   307,   401,   448,   728,
     749,   827,   843,    84,  1135,   792,   759,  1131,     1,  1123,
      34,  1118,  1906,   673,  1890,  1890,   845,  1154,   759,  1046,
     309,  1062,  1812,   420,  1933,   759,  1908,  1917,   279,   292,
     648,   791,   822,   826,  1059,  1060,  1924,  1924,  1935,  1948,
     759,   821,  1963,   676,  1924,  1907,   188,  1929,  1929,   477,
    1017,  1857,  1966,   714,   431,    53,  1904,  1924,  1027,  1028,
    1798,   944,    40,  1307,  1308,  1835,   416,   420,  1975,  1340,
    1840,  1840,   364,  1920,   191,  1892,  1372,  1373,  1442,  1820,
    1820,  1820,  1820,  1940,  1849,  1924,  1820,   654,   849,  1793,
    1793,  1821,  1822,  1848,  1850,  1316,    78,   183,  1527,  1820,
    1820,   567,  1797,   326,   530,  1647,  1854,   631,    78,  1562,
     431,  1924,  1578,  1750,  1752,  1754,  1760,   431,   431,  1849,
    1892,  1705,   764,  1849,  1917,  1849,  1316,  1316,  1758,  1759,
    1756,  1757,  1814,  1087,  1087,  1165,   764,   751,  1153,  1155,
     473,   709,  1097,  1110,  1111,  1112,   185,   293,   396,   447,
     694,   782,  1125,   425,  1126,  1917,   318,  1132,   756,  1961,
    1857,  1961,   424,  1835,  1907,  1857,   102,   598,  1042,  1807,
    1808,  1053,  1857,  1954,   424,  1050,  1854,  1050,  1924,  1929,
     212,   277,   985,   606,  1004,  1924,  1924,  1924,  1924,    29,
      30,  1900,  1019,  1924,  1935,   676,   996,  1028,   555,  1029,
    1308,  1840,  1920,  1739,   269,   503,   775,  1375,  1377,  1379,
      10,   377,   494,   520,   774,  1374,  1923,  1849,  1739,  1739,
    1820,  1924,  1797,   567,   409,   424,  1826,  1828,  1849,  1865,
     803,  1849,  1753,  1751,  1752,  1739,    38,   214,   270,   327,
    1706,  1707,  1708,  1710,  1714,  1716,  1717,  1718,  1902,  1915,
    1849,  1071,  1166,  1223,  1097,   839,  1156,  1965,  1940,  1112,
     845,  1857,   759,  1131,   189,   189,  1134,  1808,    73,   779,
    1054,  1055,  1056,  1057,  1966,  1908,   318,  1045,  1916,   212,
     587,   676,   989,  1924,    85,    86,    87,   195,   196,   197,
     370,   371,   393,   416,   439,   530,   560,   563,   564,   587,
     741,   981,   982,   983,   984,  1813,  1007,  1808,  1808,  1808,
    1924,  1857,   221,   759,  1840,  1316,  1316,  1316,  1378,  1379,
    1376,  1377,  1940,  1849,  1556,  1924,  1850,  1863,  1865,  1770,
      51,  1897,   570,   180,  1072,   588,  1167,  1092,  1107,  1224,
    1225,  1226,   677,   767,  1849,   368,  1136,  1941,  1941,   365,
    1137,  1139,  1140,  1141,  1142,  1144,  1813,  1813,   308,  1046,
    1857,  1808,   424,   987,  1854,   987,    12,   987,   987,   987,
     424,   986,  1854,    58,   419,   705,   845,  1008,   698,   765,
    1010,  1011,  1808,  1809,    43,   273,   440,  1001,   182,  1840,
    1849,  1556,   614,   704,  1955,   221,   709,  1715,  1941,   764,
    1073,  1915,  1800,     1,  1094,  1226,   764,   759,  1924,  1137,
    1808,  1808,  1906,  1144,  1140,  1929,  1138,  1902,  1908,  1010,
    1914,  1924,   542,   991,   992,  1012,  1013,   844,  1002,  1002,
     764,  1849,   261,   269,  1972,    15,  1711,  1712,  1805,   530,
    1922,  1168,   764,  1227,  1228,   764,  1097,  1137,   845,  1145,
    1146,  1924,  1139,  1813,   189,  1044,  1880,  1981,   993,  1014,
    1808,  1014,   431,   638,   567,  1945,   517,   568,   603,  1713,
    1712,   191,   261,   714,   731,   746,  1078,  1079,  1080,   370,
     388,     1,  1169,  1230,     7,    49,    61,   103,   126,   176,
     251,   315,   394,   423,   514,   581,   585,   655,   715,   787,
     838,  1232,  1233,  1146,  1807,  1941,   991,  1924,   521,  1941,
     517,  1854,   156,   738,  1924,   392,   392,   191,   407,  1080,
     221,   450,   714,   731,   746,  1074,  1075,  1076,  1077,  1849,
    1934,  1957,   221,   450,   714,   746,  1081,  1082,  1083,  1084,
    1849,  1957,   764,   149,   150,   536,   764,  1170,  1171,  1176,
    1924,  1966,  1990,    40,    45,    46,    47,    48,    52,    70,
      71,    88,   108,   116,   159,   260,   310,   318,   320,   337,
     349,   405,   416,   420,   436,   477,   516,   533,   576,   617,
     662,   692,   704,   735,   759,   770,   793,   821,  1121,  1122,
    1127,  1147,  1150,  1152,  1231,  1289,  1290,  1341,  1342,  1924,
    1968,  1234,  1849,  1857,  1229,  1808,   730,   994,  1808,   459,
     593,   598,  1976,   276,   370,   388,   530,  1709,  1797,  1924,
     516,  1849,  1924,  1924,  1924,  1924,   392,   175,   754,   392,
     407,  1076,  1849,   156,   583,   698,   722,   723,   724,   392,
     175,   754,   392,   407,  1083,  1849,   742,  1926,  1926,   412,
     413,  1932,  1187,   327,   112,   289,  1172,  1173,  1174,  1175,
    1849,  1924,   416,   652,  1287,  1937,  1285,  1937,  1924,  1925,
    1278,  1279,  1924,  1842,  1285,  1925,  1857,   101,  1925,  1849,
    1849,   327,  1287,  1236,  1237,   839,  1976,  1849,  1857,  1849,
    1849,  1849,  1849,  1924,  1924,  1924,  1924,  1924,  1849,  1924,
    1924,  1924,  1924,  1924,  1924,  1924,  1924,  1924,  1924,  1924,
    1849,  1924,  1926,  1926,   424,   855,  1177,  1178,  1179,  1849,
    1860,  1092,  1188,  1924,  1174,  1175,  1838,  1924,  1924,  1838,
    1847,  1857,  1838,  1282,  1838,  1961,  1924,  1847,  1857,  1230,
      15,   700,   855,   945,  1961,  1849,  1849,  1849,  1849,  1849,
    1849,  1849,  1849,  1849,  1849,  1849,  1849,  1849,  1849,  1849,
    1849,  1849,  1849,  1849,  1180,   416,   420,  1931,  1969,  1975,
       1,  1094,  1095,  1835,   451,   552,   772,   777,  1280,  1281,
    1288,  1280,  1281,  1286,    90,   550,  1283,  1284,  1835,  1864,
    1143,  1144,  1849,  1849,   291,   308,   342,   398,   418,  1181,
    1182,  1183,  1184,  1185,  1186,  1178,  1179,   764,  1189,  1838,
    1838,  1849,  1849,   179,   193,  1983,  1924,  1924,    99,   149,
    1983,  1984,  1924,  1190,  1849,  1924,  1179,  1179,   342,  1924,
    1924,  1179,     4,   338,   416,   475,   516,   558,   698,   729,
     764,   790,   828,  1121,  1122,  1127,  1128,  1147,  1150,  1152,
    1191,  1192,  1197,  1200,  1203,  1204,  1207,  1208,  1209,  1213,
    1214,  1220,  1221,  1968,  1969,  1970,  1849,  1179,  1179,  1179,
     366,  1921,   509,  1938,  1924,  1857,  1924,  1940,  1924,  1849,
      13,    51,   390,   839,   476,   552,   777,  1210,  1211,  1212,
    1860,  1217,  1218,  1219,  1280,  1860,   509,   510,  1939,  1849,
     381,   473,   476,   536,   552,   777,  1198,  1199,  1857,  1131,
    1825,  1823,  1825,    98,   149,   536,   545,   546,   613,   633,
     634,  1193,  1983,  1984,  1985,  1986,  1987,  1988,  1989,   318,
     472,  1936,  1936,    13,    51,  1770,  1860,  1860,  1210,  1218,
    1860,    95,   404,   635,  1215,  1216,  1849,  1857,  1857,  1961,
    1887,   619,   806,  1201,  1825,   308,   308,   342,   308,   342,
     309,   521,  1942,  1942,  1825,   526,   536,  1205,  1206,  1849,
    1205,  1936,  1936,  1926,  1849,  1134,  1941,  1849,   289,  1194,
    1849,    15,   289,  1196,  1849,    78,  1205,   536,   536,   718,
    1129,   289,  1202,  1849,   526,  1195,  1195,  1195,  1195,  1825,
    1857,   536
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint16 yyr1[] =
{
       0,   857,   859,   858,   860,   860,   862,   861,   863,   863,
     864,   864,   866,   865,   867,   868,   869,   869,   870,   870,
     872,   871,   874,   873,   875,   876,   876,   877,   877,   879,
     880,   878,   882,   881,   883,   883,   884,   884,   885,   885,
     886,   886,   887,   887,   887,   887,   888,   888,   889,   889,
     890,   890,   891,   892,   892,   893,   893,   893,   893,   894,
     894,   895,   895,   896,   896,   896,   897,   897,   898,   898,
     898,   898,   899,   900,   900,   901,   902,   902,   903,   903,
     904,   904,   905,   905,   905,   905,   905,   907,   906,   908,
     908,   909,   909,   911,   910,   912,   912,   912,   912,   913,
     913,   914,   914,   914,   914,   915,   916,   917,   918,   919,
     919,   919,   919,   920,   920,   922,   921,   923,   923,   923,
     924,   924,   925,   925,   925,   925,   925,   926,   926,   927,
     928,   929,   929,   930,   930,   930,   930,   930,   930,   930,
     930,   930,   930,   930,   930,   930,   932,   931,   933,   933,
     933,   933,   934,   934,   935,   936,   936,   938,   937,   939,
     939,   939,   939,   939,   939,   940,   940,   941,   941,   942,
     941,   943,   943,   944,   944,   944,   944,   944,   944,   945,
     945,   946,   947,   947,   948,   949,   949,   950,   951,   951,
     952,   952,   953,   954,   954,   955,   956,   957,   957,   958,
     958,   959,   959,   959,   960,   960,   961,   962,   963,   963,
     964,   965,   966,   967,   968,   969,   970,   971,   971,   972,
     972,   973,   973,   974,   974,   976,   975,   977,   977,   978,
     978,   979,   979,   979,   979,   979,   979,   979,   979,   979,
     979,   979,   979,   979,   980,   980,   980,   980,   980,   980,
     981,   981,   981,   982,   982,   982,   982,   983,   983,   983,
     983,   983,   983,   983,   984,   984,   985,   985,   985,   986,
     986,   987,   987,   987,   988,   989,   989,   989,   990,   991,
     991,   993,   992,   994,   994,   994,   995,   996,   997,   998,
     998,   998,  1000,   999,  1001,  1001,  1001,  1002,  1002,  1002,
    1002,  1003,  1003,  1004,  1004,  1004,  1004,  1005,  1007,  1006,
    1008,  1008,  1008,  1008,  1009,  1010,  1010,  1011,  1011,  1013,
    1012,  1012,  1014,  1015,  1016,  1017,  1017,  1018,  1019,  1019,
    1019,  1020,  1020,  1020,  1021,  1021,  1022,  1022,  1023,  1024,
    1024,  1024,  1024,  1026,  1025,  1027,  1027,  1028,  1029,  1029,
    1031,  1030,  1032,  1032,  1033,  1033,  1034,  1034,  1035,  1037,
    1036,  1036,  1038,  1038,  1039,  1039,  1040,  1040,  1040,  1040,
    1040,  1040,  1040,  1040,  1040,  1040,  1040,  1041,  1042,  1042,
    1042,  1043,  1043,  1043,  1044,  1044,  1045,  1045,  1046,  1046,
    1047,  1048,  1048,  1049,  1049,  1050,  1050,  1051,  1052,  1053,
    1053,  1054,  1054,  1054,  1055,  1056,  1057,  1058,  1059,  1059,
    1059,  1059,  1059,  1060,  1060,  1061,  1062,  1062,  1063,  1064,
    1064,  1065,  1065,  1066,  1067,  1066,  1068,  1068,  1069,  1071,
    1070,  1072,  1072,  1073,  1073,  1073,  1074,  1074,  1074,  1075,
    1075,  1076,  1076,  1076,  1076,  1076,  1076,  1076,  1076,  1076,
    1076,  1076,  1077,  1078,  1078,  1079,  1079,  1080,  1080,  1080,
    1080,  1080,  1080,  1080,  1081,  1081,  1081,  1082,  1082,  1083,
    1083,  1083,  1083,  1083,  1083,  1084,  1085,  1086,  1085,  1087,
    1088,  1087,  1089,  1089,  1090,  1090,  1090,  1091,  1090,  1090,
    1092,  1093,  1093,  1094,  1094,  1095,  1096,  1096,  1097,  1097,
    1097,  1097,  1098,  1098,  1098,  1098,  1098,  1098,  1098,  1098,
    1098,  1098,  1098,  1098,  1098,  1098,  1098,  1099,  1099,  1100,
    1100,  1101,  1101,  1101,  1102,  1102,  1103,  1104,  1104,  1106,
    1105,  1107,  1108,  1107,  1109,  1109,  1110,  1110,  1110,  1111,
    1111,  1112,  1112,  1112,  1112,  1112,  1112,  1112,  1112,  1112,
    1112,  1113,  1113,  1114,  1114,  1115,  1115,  1115,  1115,  1115,
    1115,  1115,  1115,  1115,  1115,  1115,  1115,  1115,  1115,  1115,
    1116,  1117,  1118,  1118,  1119,  1119,  1120,  1121,  1122,  1122,
    1122,  1123,  1123,  1123,  1123,  1123,  1123,  1123,  1123,  1123,
    1123,  1123,  1123,  1123,  1123,  1123,  1123,  1123,  1123,  1123,
    1123,  1123,  1123,  1123,  1123,  1123,  1123,  1123,  1123,  1123,
    1123,  1123,  1123,  1123,  1123,  1123,  1123,  1123,  1123,  1123,
    1123,  1123,  1123,  1123,  1123,  1123,  1123,  1124,  1124,  1125,
    1125,  1125,  1125,  1125,  1125,  1125,  1126,  1126,  1127,  1127,
    1128,  1129,  1129,  1130,  1130,  1130,  1131,  1131,  1132,  1132,
    1133,  1133,  1134,  1134,  1135,  1135,  1136,  1136,  1137,  1137,
    1138,  1137,  1137,  1137,  1139,  1140,  1140,  1141,  1142,  1142,
    1143,  1143,  1144,  1145,  1145,  1146,  1147,  1148,  1149,  1149,
    1149,  1150,  1151,  1153,  1152,  1154,  1154,  1155,  1155,  1156,
    1156,  1157,  1157,  1158,  1159,  1160,  1161,  1160,  1162,  1163,
    1162,  1164,  1165,  1164,  1166,  1166,  1168,  1167,  1169,  1169,
    1169,  1170,  1170,  1170,  1170,  1171,  1172,  1172,  1172,  1173,
    1174,  1174,  1175,  1176,  1177,  1177,  1177,  1178,  1179,  1179,
    1180,  1180,  1181,  1181,  1181,  1181,  1181,  1181,  1182,  1183,
    1184,  1185,  1186,  1187,  1187,  1189,  1188,  1188,  1190,  1190,
    1191,  1191,  1191,  1191,  1191,  1191,  1191,  1191,  1191,  1191,
    1191,  1191,  1191,  1191,  1191,  1191,  1192,  1193,  1193,  1193,
    1193,  1193,  1193,  1193,  1194,  1194,  1194,  1195,  1195,  1196,
    1196,  1196,  1196,  1197,  1198,  1198,  1198,  1198,  1199,  1199,
    1199,  1200,  1201,  1201,  1201,  1202,  1202,  1203,  1203,  1203,
    1203,  1203,  1204,  1204,  1205,  1205,  1206,  1206,  1206,  1207,
    1208,  1209,  1210,  1210,  1211,  1211,  1211,  1211,  1212,  1213,
    1214,  1215,  1215,  1216,  1216,  1216,  1217,  1217,  1218,  1218,
    1219,  1220,  1221,  1222,  1223,  1222,  1224,  1224,  1225,  1225,
    1226,  1227,  1226,  1228,  1229,  1226,  1226,  1230,  1230,  1231,
    1231,  1231,  1231,  1231,  1231,  1231,  1231,  1231,  1231,  1231,
    1231,  1231,  1231,  1231,  1231,  1231,  1231,  1231,  1231,  1231,
    1231,  1231,  1231,  1231,  1231,  1231,  1231,  1231,  1231,  1231,
    1231,  1231,  1231,  1231,  1231,  1231,  1231,  1231,  1231,  1231,
    1231,  1231,  1231,  1231,  1231,  1231,  1231,  1231,  1232,  1232,
    1233,  1233,  1233,  1233,  1233,  1233,  1233,  1233,  1233,  1233,
    1233,  1233,  1233,  1233,  1233,  1233,  1234,  1234,  1235,  1235,
    1236,  1236,  1237,  1237,  1238,  1238,  1239,  1239,  1240,  1240,
    1241,  1241,  1241,  1241,  1241,  1241,  1241,  1241,  1241,  1241,
    1241,  1241,  1241,  1241,  1242,  1242,  1242,  1242,  1242,  1242,
    1242,  1242,  1242,  1242,  1242,  1242,  1242,  1242,  1242,  1243,
    1243,  1243,  1243,  1243,  1243,  1243,  1243,  1243,  1243,  1244,
    1244,  1245,  1245,  1245,  1245,  1245,  1246,  1247,  1247,  1247,
    1247,  1247,  1247,  1247,  1247,  1247,  1247,  1247,  1247,  1247,
    1247,  1247,  1248,  1248,  1248,  1248,  1248,  1248,  1248,  1248,
    1248,  1248,  1249,  1249,  1249,  1249,  1249,  1249,  1249,  1249,
    1249,  1249,  1250,  1250,  1251,  1251,  1252,  1252,  1253,  1253,
    1253,  1253,  1253,  1254,  1254,  1254,  1254,  1254,  1254,  1254,
    1254,  1254,  1254,  1254,  1254,  1254,  1254,  1254,  1254,  1255,
    1255,  1255,  1256,  1256,  1256,  1256,  1256,  1256,  1256,  1256,
    1257,  1257,  1257,  1257,  1257,  1257,  1258,  1258,  1258,  1258,
    1258,  1258,  1258,  1258,  1259,  1259,  1259,  1259,  1260,  1260,
    1260,  1261,  1261,  1261,  1261,  1261,  1261,  1262,  1262,  1262,
    1262,  1263,  1263,  1263,  1263,  1263,  1263,  1263,  1264,  1264,
    1264,  1264,  1264,  1264,  1264,  1264,  1264,  1264,  1264,  1264,
    1264,  1264,  1264,  1264,  1264,  1264,  1264,  1264,  1264,  1264,
    1264,  1264,  1264,  1264,  1264,  1264,  1264,  1264,  1264,  1264,
    1264,  1264,  1264,  1264,  1264,  1264,  1264,  1264,  1264,  1264,
    1264,  1264,  1264,  1264,  1264,  1265,  1265,  1265,  1266,  1266,
    1266,  1266,  1266,  1266,  1266,  1266,  1266,  1267,  1267,  1267,
    1267,  1267,  1267,  1267,  1267,  1267,  1267,  1267,  1267,  1267,
    1267,  1267,  1267,  1267,  1267,  1267,  1267,  1267,  1267,  1267,
    1268,  1269,  1270,  1270,  1270,  1270,  1270,  1270,  1270,  1270,
    1271,  1271,  1271,  1272,  1272,  1273,  1274,  1274,  1275,  1275,
    1276,  1276,  1277,  1277,  1277,  1278,  1278,  1279,  1279,  1280,
    1280,  1281,  1281,  1282,  1283,  1283,  1284,  1284,  1285,  1286,
    1286,  1286,  1287,  1288,  1288,  1288,  1289,  1290,  1291,  1292,
    1293,  1294,  1291,  1295,  1291,  1296,  1297,  1296,  1298,  1296,
    1299,  1299,  1300,  1301,  1301,  1301,  1302,  1302,  1302,  1302,
    1302,  1302,  1303,  1304,  1304,  1305,  1305,  1305,  1306,  1307,
    1306,  1308,  1308,  1309,  1309,  1309,  1309,  1309,  1311,  1310,
    1312,  1312,  1313,  1314,  1315,  1315,  1317,  1318,  1316,  1320,
    1319,  1319,  1321,  1321,  1321,  1321,  1321,  1321,  1321,  1321,
    1321,  1321,  1321,  1321,  1321,  1321,  1321,  1321,  1321,  1321,
    1321,  1321,  1321,  1321,  1321,  1321,  1321,  1321,  1321,  1321,
    1321,  1321,  1321,  1321,  1321,  1321,  1321,  1321,  1321,  1321,
    1321,  1321,  1321,  1321,  1321,  1321,  1321,  1321,  1321,  1321,
    1321,  1321,  1321,  1321,  1321,  1321,  1321,  1321,  1321,  1321,
    1321,  1321,  1323,  1322,  1325,  1324,  1326,  1324,  1324,  1324,
    1324,  1324,  1324,  1324,  1324,  1324,  1324,  1324,  1324,  1324,
    1324,  1324,  1324,  1324,  1324,  1324,  1324,  1327,  1327,  1328,
    1328,  1329,  1329,  1330,  1330,  1330,  1330,  1330,  1331,  1331,
    1332,  1332,  1333,  1333,  1334,  1334,  1334,  1335,  1336,  1336,
    1337,  1338,  1338,  1338,  1338,  1338,  1338,  1338,  1338,  1338,
    1338,  1338,  1338,  1338,  1338,  1338,  1338,  1338,  1338,  1338,
    1338,  1338,  1338,  1338,  1338,  1338,  1338,  1338,  1338,  1338,
    1338,  1338,  1338,  1338,  1338,  1339,  1339,  1340,  1341,  1341,
    1341,  1342,  1342,  1342,  1343,  1343,  1344,  1344,  1346,  1345,
    1347,  1347,  1347,  1347,  1348,  1348,  1349,  1349,  1351,  1350,
    1352,  1352,  1353,  1353,  1355,  1354,  1356,  1356,  1357,  1358,
    1358,  1360,  1359,  1362,  1361,  1363,  1363,  1363,  1363,  1363,
    1364,  1364,  1365,  1365,  1366,  1366,  1367,  1368,  1367,  1369,
    1369,  1370,  1370,  1371,  1371,  1371,  1371,  1372,  1372,  1372,
    1372,  1372,  1373,  1373,  1374,  1374,  1375,  1375,  1375,  1376,
    1376,  1377,  1377,  1378,  1378,  1379,  1380,  1380,  1382,  1381,
    1383,  1383,  1384,  1384,  1386,  1385,  1387,  1387,  1388,  1388,
    1389,  1389,  1389,  1389,  1389,  1391,  1390,  1392,  1392,  1394,
    1393,  1395,  1396,  1396,  1397,  1398,  1400,  1399,  1401,  1401,
    1403,  1402,  1404,  1404,  1405,  1405,  1406,  1406,  1408,  1407,
    1409,  1410,  1410,  1411,  1411,  1411,  1411,  1411,  1413,  1412,
    1414,  1414,  1414,  1414,  1414,  1414,  1414,  1414,  1414,  1415,
    1415,  1416,  1416,  1418,  1417,  1419,  1419,  1420,  1420,  1421,
    1421,  1421,  1421,  1421,  1422,  1422,  1422,  1422,  1423,  1423,
    1425,  1424,  1426,  1426,  1427,  1427,  1428,  1428,  1428,  1428,
    1428,  1430,  1431,  1429,  1432,  1432,  1434,  1435,  1433,  1437,
    1436,  1438,  1438,  1438,  1439,  1439,  1440,  1440,  1441,  1441,
    1441,  1442,  1442,  1443,  1443,  1443,  1443,  1443,  1443,  1443,
    1444,  1444,  1445,  1445,  1446,  1446,  1447,  1448,  1449,  1449,
    1449,  1449,  1449,  1449,  1449,  1449,  1449,  1449,  1449,  1449,
    1449,  1449,  1449,  1449,  1449,  1449,  1449,  1449,  1449,  1449,
    1449,  1450,  1450,  1452,  1451,  1453,  1453,  1453,  1453,  1453,
    1454,  1454,  1456,  1455,  1458,  1457,  1459,  1461,  1460,  1462,
    1463,  1463,  1464,  1464,  1464,  1465,  1465,  1466,  1466,  1467,
    1468,  1469,  1469,  1470,  1470,  1471,  1471,  1471,  1471,  1472,
    1472,  1473,  1473,  1475,  1474,  1476,  1476,  1476,  1476,  1476,
    1476,  1476,  1477,  1477,  1479,  1478,  1480,  1482,  1481,  1483,
    1485,  1484,  1486,  1487,  1487,  1488,  1490,  1489,  1491,  1491,
    1491,  1492,  1492,  1493,  1494,  1495,  1495,  1497,  1496,  1498,
    1499,  1499,  1500,  1500,  1500,  1501,  1501,  1502,  1502,  1503,
    1504,  1504,  1504,  1504,  1504,  1504,  1504,  1505,  1505,  1507,
    1506,  1508,  1508,  1510,  1509,  1511,  1511,  1513,  1512,  1514,
    1515,  1515,  1515,  1516,  1516,  1516,  1516,  1518,  1517,  1519,
    1520,  1521,  1521,  1522,  1522,  1522,  1522,  1522,  1522,  1523,
    1523,  1524,  1524,  1525,  1525,  1525,  1525,  1525,  1526,  1527,
    1527,  1527,  1527,  1527,  1528,  1529,  1531,  1530,  1533,  1532,
    1534,  1534,  1535,  1535,  1537,  1536,  1538,  1538,  1540,  1539,
    1541,  1541,  1542,  1542,  1544,  1543,  1545,  1545,  1546,  1547,
    1547,  1547,  1547,  1548,  1548,  1549,  1549,  1549,  1549,  1551,
    1550,  1552,  1553,  1552,  1552,  1554,  1554,  1555,  1555,  1556,
    1556,  1557,  1557,  1557,  1557,  1557,  1558,  1558,  1559,  1559,
    1560,  1560,  1561,  1562,  1562,  1564,  1563,  1566,  1565,  1567,
    1568,  1568,  1569,  1569,  1569,  1569,  1570,  1570,  1571,  1571,
    1572,  1572,  1573,  1574,  1574,  1574,  1575,  1575,  1576,  1576,
    1576,  1577,  1577,  1578,  1578,  1579,  1579,  1580,  1582,  1581,
    1583,  1584,  1584,  1585,  1585,  1585,  1586,  1586,  1587,  1588,
    1588,  1589,  1590,  1590,  1592,  1591,  1593,  1594,  1596,  1595,
    1597,  1598,  1598,  1600,  1599,  1601,  1602,  1602,  1603,  1603,
    1604,  1604,  1605,  1607,  1606,  1608,  1608,  1609,  1609,  1610,
    1610,  1611,  1611,  1612,  1613,  1613,  1615,  1614,  1616,  1616,
    1617,  1617,  1618,  1619,  1619,  1619,  1619,  1620,  1620,  1622,
    1621,  1623,  1623,  1623,  1623,  1623,  1623,  1623,  1623,  1624,
    1624,  1625,  1625,  1626,  1627,  1628,  1628,  1629,  1629,  1629,
    1629,  1629,  1629,  1629,  1629,  1630,  1630,  1630,  1631,  1632,
    1632,  1633,  1634,  1634,  1635,  1635,  1636,  1637,  1639,  1638,
    1641,  1640,  1642,  1642,  1643,  1643,  1644,  1644,  1645,  1645,
    1646,  1646,  1646,  1647,  1647,  1647,  1649,  1648,  1650,  1651,
    1651,  1652,  1652,  1652,  1652,  1653,  1653,  1653,  1653,  1653,
    1653,  1654,  1655,  1655,  1656,  1656,  1658,  1657,  1657,  1657,
    1659,  1659,  1659,  1659,  1659,  1660,  1660,  1661,  1661,  1662,
    1662,  1662,  1662,  1664,  1663,  1665,  1667,  1666,  1668,  1668,
    1669,  1670,  1670,  1671,  1671,  1672,  1672,  1673,  1673,  1675,
    1674,  1676,  1676,  1676,  1676,  1677,  1677,  1678,  1679,  1679,
    1681,  1680,  1682,  1682,  1684,  1683,  1685,  1687,  1686,  1688,
    1690,  1689,  1691,  1692,  1692,  1693,  1693,  1694,  1695,  1695,
    1696,  1697,  1697,  1698,  1698,  1699,  1699,  1700,  1700,  1702,
    1701,  1703,  1703,  1705,  1704,  1706,  1706,  1706,  1706,  1706,
    1707,  1708,  1708,  1709,  1709,  1709,  1709,  1709,  1710,  1711,
    1711,  1712,  1712,  1712,  1713,  1713,  1713,  1713,  1714,  1715,
    1715,  1716,  1717,  1718,  1718,  1720,  1719,  1721,  1722,  1722,
    1723,  1723,  1723,  1723,  1724,  1724,  1725,  1725,  1725,  1726,
    1726,  1727,  1727,  1727,  1728,  1728,  1729,  1730,  1730,  1731,
    1731,  1732,  1733,  1733,  1734,  1734,  1734,  1735,  1735,  1736,
    1737,  1737,  1738,  1739,  1739,  1739,  1740,  1740,  1741,  1742,
    1742,  1743,  1744,  1744,  1744,  1745,  1745,  1746,  1747,  1747,
    1748,  1749,  1749,  1750,  1750,  1751,  1751,  1752,  1753,  1753,
    1754,  1755,  1755,  1756,  1756,  1757,  1758,  1758,  1759,  1760,
    1760,  1761,  1761,  1762,  1762,  1763,  1764,  1764,  1765,  1766,
    1766,  1767,  1767,  1768,  1768,  1769,  1769,  1770,  1771,  1773,
    1772,  1774,  1774,  1775,  1775,  1775,  1775,  1775,  1775,  1775,
    1775,  1775,  1775,  1775,  1775,  1775,  1775,  1776,  1776,  1777,
    1778,  1778,  1778,  1778,  1778,  1778,  1778,  1778,  1778,  1778,
    1778,  1778,  1778,  1778,  1779,  1779,  1780,  1780,  1781,  1781,
    1782,  1783,  1784,  1784,  1785,  1785,  1785,  1786,  1786,  1786,
    1787,  1787,  1787,  1788,  1788,  1789,  1789,  1789,  1790,  1790,
    1791,  1791,  1791,  1791,  1791,  1791,  1792,  1792,  1793,  1794,
    1795,  1795,  1796,  1797,  1797,  1798,  1799,  1800,  1801,  1801,
    1802,  1803,  1803,  1804,  1805,  1805,  1805,  1806,  1807,  1807,
    1808,  1809,  1809,  1810,  1811,  1811,  1812,  1813,  1813,  1814,
    1814,  1815,  1816,  1816,  1817,  1817,  1817,  1818,  1818,  1819,
    1819,  1820,  1820,  1821,  1821,  1822,  1822,  1822,  1822,  1822,
    1822,  1822,  1822,  1822,  1823,  1823,  1824,  1824,  1824,  1825,
    1825,  1825,  1825,  1825,  1825,  1825,  1826,  1826,  1826,  1826,
    1826,  1826,  1827,  1828,  1829,  1829,  1830,  1830,  1831,  1832,
    1833,  1833,  1833,  1834,  1834,  1835,  1835,  1836,  1836,  1836,
    1837,  1837,  1837,  1838,  1838,  1838,  1839,  1839,  1840,  1840,
    1841,  1841,  1842,  1842,  1842,  1843,  1844,  1845,  1845,  1846,
    1847,  1848,  1849,  1850,  1850,  1850,  1850,  1851,  1851,  1852,
    1852,  1853,  1853,  1853,  1853,  1854,  1854,  1855,  1856,  1856,
    1857,  1858,  1859,  1860,  1860,  1861,  1861,  1861,  1861,  1861,
    1861,  1861,  1862,  1862,  1863,  1863,  1864,  1864,  1864,  1864,
    1864,  1864,  1864,  1865,  1865,  1865,  1865,  1865,  1865,  1865,
    1865,  1865,  1865,  1865,  1865,  1865,  1865,  1866,  1866,  1867,
    1867,  1867,  1868,  1868,  1868,  1868,  1869,  1869,  1869,  1870,
    1870,  1870,  1871,  1871,  1871,  1873,  1872,  1874,  1874,  1875,
    1875,  1876,  1876,  1877,  1877,  1878,  1879,  1879,  1880,  1880,
    1881,  1881,  1882,  1882,  1883,  1883,  1884,  1884,  1884,  1885,
    1885,  1886,  1886,  1886,  1887,  1887,  1888,  1888,  1889,  1889,
    1889,  1889,  1889,  1889,  1889,  1889,  1890,  1890,  1891,  1891,
    1892,  1892,  1893,  1893,  1893,  1893,  1894,  1894,  1894,  1894,
    1894,  1894,  1894,  1894,  1894,  1894,  1894,  1894,  1894,  1894,
    1894,  1894,  1894,  1894,  1894,  1894,  1894,  1894,  1894,  1894,
    1894,  1894,  1894,  1894,  1894,  1894,  1894,  1894,  1894,  1894,
    1894,  1894,  1894,  1894,  1894,  1894,  1894,  1894,  1894,  1894,
    1894,  1894,  1894,  1894,  1894,  1894,  1895,  1895,  1895,  1895,
    1895,  1895,  1895,  1895,  1895,  1895,  1895,  1895,  1895,  1895,
    1895,  1895,  1895,  1895,  1895,  1895,  1895,  1895,  1896,  1896,
    1897,  1897,  1898,  1898,  1899,  1899,  1900,  1900,  1900,  1901,
    1901,  1902,  1902,  1903,  1903,  1904,  1904,  1905,  1905,  1906,
    1906,  1907,  1907,  1908,  1908,  1909,  1909,  1910,  1910,  1911,
    1911,  1912,  1912,  1913,  1913,  1914,  1914,  1915,  1915,  1916,
    1916,  1917,  1917,  1918,  1918,  1918,  1919,  1919,  1919,  1920,
    1920,  1921,  1921,  1922,  1922,  1923,  1923,  1924,  1924,  1925,
    1925,  1925,  1926,  1926,  1926,  1927,  1927,  1927,  1927,  1928,
    1928,  1928,  1929,  1929,  1930,  1930,  1931,  1931,  1931,  1932,
    1932,  1932,  1933,  1933,  1934,  1934,  1935,  1935,  1936,  1936,
    1937,  1937,  1938,  1938,  1938,  1939,  1939,  1939,  1940,  1940,
    1941,  1941,  1942,  1942,  1942,  1943,  1943,  1943,  1943,  1944,
    1944,  1945,  1945,  1946,  1946,  1947,  1947,  1948,  1948,  1949,
    1949,  1949,  1950,  1950,  1951,  1951,  1952,  1952,  1953,  1953,
    1953,  1954,  1954,  1955,  1955,  1956,  1956,  1957,  1957,  1958,
    1958,  1959,  1959,  1960,  1960,  1961,  1961,  1962,  1962,  1963,
    1963,  1963,  1964,  1964,  1965,  1965,  1966,  1966,  1967,  1967,
    1968,  1968,  1969,  1969,  1970,  1970,  1971,  1971,  1972,  1972,
    1973,  1973,  1974,  1974,  1975,  1975,  1976,  1976,  1977,  1977,
    1977,  1978,  1978,  1979,  1979,  1980,  1980,  1981,  1981,  1982,
    1982,  1982,  1983,  1983,  1984,  1984,  1985,  1985,  1986,  1986,
    1987,  1987,  1988,  1988,  1989,  1989,  1990,  1990
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     1,     1,     0,     2,     1,     2,
       1,     1,     0,     2,     5,     5,     0,     1,     1,     2,
       0,     4,     0,     4,     3,     0,     3,     1,     1,     0,
       0,     8,     0,     6,     1,     1,     1,     1,     0,     2,
       0,     3,     1,     1,     1,     1,     2,     2,     1,     1,
       0,     3,     5,     0,     3,     1,     1,     1,     1,     0,
       5,     0,     3,     1,     1,     1,     0,     4,     1,     1,
       1,     1,     3,     0,     3,     2,     0,     3,     0,     1,
       1,     2,     1,     1,     1,     1,     1,     0,     4,     0,
       3,     0,     3,     0,     4,     0,     2,     3,     2,     1,
       2,     1,     1,     1,     1,     5,     3,     3,     4,     1,
       1,     1,     1,     1,     2,     0,     4,     0,     2,     3,
       1,     2,     3,     3,     3,     3,     3,     1,     2,     2,
       2,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     3,     2,     3,
       3,     1,     0,     1,     1,     3,     4,     0,     5,     1,
       1,     1,     1,     1,     1,     1,     2,     1,     3,     0,
       4,     1,     3,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     0,     2,     3,     1,     2,     3,     1,     2,
       1,     2,     3,     1,     2,     3,     6,     1,     2,     1,
       3,     0,     2,     2,     0,     2,     4,     5,     0,     3,
       3,     5,     3,     4,     3,     3,     5,     0,     3,     0,
       2,     0,     2,     0,     2,     0,     5,     2,     2,     0,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     5,     5,     5,     5,     5,     5,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     0,     3,     0,     1,     1,     1,
       1,     0,     1,     1,     4,     1,     1,     1,     9,     0,
       1,     0,     4,     0,     4,     3,     3,     1,     5,     0,
       1,     1,     0,     5,     2,     2,     1,     0,     4,     5,
       2,     3,     1,     1,     3,     1,     2,     4,     0,     5,
       1,     1,     1,     1,     6,     0,     2,     1,     2,     0,
       2,     2,     1,     4,     3,     1,     1,     3,     2,     2,
       2,     0,     2,     3,     1,     2,     1,     1,     5,     0,
       1,     1,     1,     0,     6,     1,     2,     2,     0,     2,
       0,    10,     0,     3,     0,     3,     0,     2,     2,     0,
       5,     3,     1,     1,     0,     2,     2,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     5,     0,     1,
       1,     4,     6,     9,     0,     3,     0,     2,     0,     2,
       3,     5,     5,     1,     1,     1,     1,     3,     5,     0,
       2,     1,     1,     1,     4,     2,     2,     4,     1,     1,
       1,     1,     1,     1,     1,     4,     0,     2,     2,     2,
       2,     1,     2,     0,     0,     5,     0,     2,     2,     0,
       5,     0,     2,     4,     3,     4,     0,     1,     1,     1,
       2,     4,     4,     4,     4,     4,     4,     4,     4,     4,
       4,     4,    11,     0,     1,     1,     2,     4,     4,     4,
       6,     4,     3,     4,     0,     1,     1,     1,     2,     4,
       4,     4,     4,     4,     4,     6,     0,     0,     5,     0,
       0,     2,     2,     3,     1,     1,     1,     0,     4,     3,
       2,     0,     1,     1,     1,     1,     0,     2,     1,     2,
       2,     3,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     5,     0,     2,     0,
       4,     5,     0,     7,     2,     2,     1,     3,     1,     1,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     0,     1,     1,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       2,     3,     0,     2,     0,     1,     2,     1,     1,     3,
       3,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     3,     3,
       4,     3,     3,     3,     4,     1,     1,     1,     1,     1,
       1,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       1,     1,     1,     1,     1,     1,     0,     1,     3,     3,
       6,     0,     2,     6,     8,     7,     0,     2,     0,     2,
       0,     2,     0,     3,     0,     3,     0,     1,     0,     2,
       0,     3,     1,     1,     1,     1,     2,     4,     1,     1,
       0,     1,     3,     1,     2,     1,     2,     2,     0,     1,
       1,     3,     1,     0,     5,     1,     2,     3,     1,     0,
       4,     2,     2,     2,     4,     0,     0,     5,     0,     0,
       5,     0,     0,     5,     0,     2,     0,     6,     0,     2,
       2,     2,     4,     1,     1,     2,     2,     1,     1,     1,
       1,     2,     1,     4,     2,     1,     3,     2,     1,     1,
       0,     2,     1,     1,     1,     1,     1,     3,     3,     4,
       4,     4,     3,     0,     2,     0,     5,     3,     0,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     1,     1,     3,
       3,     1,     1,     1,     0,     2,     2,     0,     2,     0,
       2,     2,     1,     3,     1,     2,     2,     1,     1,     1,
       1,     4,     0,     3,     2,     1,     1,     3,     4,     5,
       4,     5,     1,     1,     0,     2,     1,     1,     1,     6,
       2,     2,     0,     2,     1,     1,     2,     2,     1,     2,
       4,     0,     1,     1,     1,     1,     2,     1,     1,     2,
       1,     4,     2,     0,     0,     5,     0,     1,     2,     3,
       1,     0,     4,     0,     0,     7,     3,     0,     2,     2,
       2,     1,     1,     2,     2,     1,     1,     1,     1,     1,
       1,     1,     3,     3,     3,     3,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     4,     1,     1,     2,
       3,     2,     2,     2,     3,     3,     3,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     2,     1,     2,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       0,     1,     1,     2,     1,     3,     3,     2,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     2,     3,     3,
       1,     2,     3,     3,     3,     1,     2,     1,     2,     1,
       1,     1,     1,     2,     1,     1,     0,     1,     4,     0,
       1,     1,     4,     0,     1,     1,     3,     2,     0,     0,
       0,     0,    11,     0,     4,     0,     0,     3,     0,     3,
       1,     2,     4,     0,     2,     2,     0,     3,     3,     4,
       2,     1,     3,     0,     1,     0,     2,     2,     0,     0,
       7,     0,     2,     1,     1,     2,     1,     1,     0,     6,
       0,     2,     2,     1,     0,     1,     0,     0,     3,     0,
       2,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       2,     2,     0,     4,     0,     4,     0,     5,     3,     3,
       4,     3,     4,     3,     3,     4,     4,     3,     4,     3,
       4,     5,     3,     4,     3,     3,     3,     1,     1,     0,
       1,     1,     2,     1,     1,     1,     2,     3,     1,     2,
       1,     3,     1,     2,     2,     2,     2,     3,     3,     3,
       3,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     4,     1,
       1,     1,     1,     4,     1,     2,     1,     1,     3,     3,
       3,     3,     3,     3,     4,     0,     1,     1,     2,     1,
       1,     1,     1,     1,     1,     1,     0,     1,     0,     4,
       4,     5,     6,     8,     0,     2,     0,     1,     0,     3,
       3,     4,     0,     2,     0,     3,     1,     2,     4,     0,
       2,     0,     4,     0,     8,     0,     1,     1,     1,     1,
       1,     2,     0,     2,     1,     1,     0,     0,     3,     1,
       2,     2,     3,     0,     2,     2,     2,     0,     3,     2,
       2,     4,     1,     1,     1,     1,     0,     2,     2,     0,
       1,     2,     2,     0,     1,     2,     0,     1,     0,     3,
       1,     2,     1,     1,     0,     3,     1,     1,     2,     3,
       0,     1,     3,     3,     2,     0,     4,     0,     3,     0,
       4,     4,     0,     1,     1,     1,     0,     3,     2,     1,
       0,     4,     4,     2,     1,     2,     0,     1,     0,     3,
       3,     0,     3,     0,     2,     1,     2,     1,     0,     4,
       3,     3,     3,     3,     2,     1,     1,     1,     1,     2,
       1,     1,     2,     0,     3,     1,     1,     1,     2,     1,
       2,     1,     1,     2,     2,     2,     2,     2,     1,     1,
       0,     5,     0,     1,     1,     2,     3,     3,     3,     3,
       2,     0,     0,     5,     1,     1,     0,     0,     7,     0,
       5,     1,     1,     1,     0,     1,     0,     2,     1,     2,
       1,     1,     2,     1,     2,     1,     3,     1,     1,     2,
       3,     3,     1,     1,     1,     1,     4,     3,     1,     2,
       2,     1,     1,     2,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     3,     1,     3,     3,     3,     3,
       3,     0,     1,     0,     4,     4,     6,     6,     8,     8,
       0,     1,     0,     3,     0,     3,     3,     0,     4,     2,
       1,     3,     1,     1,     1,     2,     1,     1,     2,     2,
       3,     2,     3,     1,     3,     2,     1,     1,     1,     0,
       2,     0,     1,     0,     3,     0,     2,     1,     2,     1,
       1,     1,     0,     2,     0,     3,     1,     0,     3,     1,
       0,     3,     3,     0,     3,     2,     0,     6,     5,     3,
       2,     0,     1,     0,     0,     0,     1,     0,     3,     5,
       0,     2,     0,     3,     3,     0,     2,     1,     2,     4,
       1,     1,     1,     1,     1,     1,     1,     0,     3,     0,
       3,     1,     2,     0,     3,     2,     2,     0,     3,     2,
       1,     1,     1,     2,     1,     1,     1,     0,     3,     2,
       5,     1,     2,     2,     2,     1,     1,     1,     2,     1,
       2,     4,     2,     0,     1,     1,     1,     1,     4,     0,
       1,     1,     2,     2,     3,     3,     0,     3,     0,     4,
       2,     2,     0,     1,     0,     3,     3,     4,     0,     4,
       4,     6,     0,     1,     0,     3,     1,     2,     5,     1,
       1,     1,     1,     0,     3,     0,     3,     2,     1,     0,
       3,     4,     0,     6,     4,     0,     1,     1,     1,     1,
       3,     0,     2,     1,     3,     3,     0,     3,     1,     1,
       1,     3,     6,     0,     2,     0,     3,     0,     4,     7,
       0,     2,     0,     1,     2,     1,     2,     3,     3,     1,
       0,     1,     1,     4,     4,     2,     0,     1,     1,     3,
       2,     0,     3,     1,     1,     0,     1,     1,     0,     4,
       5,     1,     1,     0,     2,     2,     0,     1,     2,     0,
       1,     2,     0,     1,     0,     3,     2,     1,     0,     4,
       4,     0,     1,     0,     4,     5,     0,     1,     2,     3,
       0,     1,     1,     0,     4,     4,     6,     0,     2,     0,
       2,     1,     2,     3,     0,     1,     0,     3,     2,     5,
       0,     1,     2,     2,     2,     2,     2,     0,     2,     0,
       3,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     4,     3,     1,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     4,     3,     5,     4,     1,
       2,     3,     1,     2,     3,     3,     4,     4,     0,     3,
       0,     7,     0,     5,     0,     2,     0,     2,     0,     3,
       0,     2,     4,     0,     2,     4,     0,     4,     4,     0,
       3,     0,     4,     1,     1,     1,     2,     2,     2,     2,
       1,     1,     2,     1,     0,     1,     0,     4,     2,     2,
       0,     2,     1,     4,     4,     0,     1,     1,     1,     1,
       1,     1,     1,     0,     4,     5,     0,     2,     1,     2,
       2,     0,     3,     1,     1,     0,     4,     0,     1,     0,
       4,     4,     6,     6,     8,     0,     1,     2,     0,     1,
       0,     3,     1,     2,     0,     3,     5,     0,     3,     2,
       0,     4,     6,     0,     3,     1,     3,     2,     2,     2,
       3,     0,     3,     0,     3,     0,     3,     0,     1,     0,
       3,     1,     2,     0,     3,     1,     1,     1,     1,     1,
       7,     0,     1,     1,     1,     1,     1,     1,     4,     1,
       2,     1,     2,     3,     0,     1,     2,     1,     3,     1,
       1,     4,     1,     1,     1,     0,     4,     6,     0,     2,
       0,     4,     3,     3,     1,     1,     0,     1,     1,     0,
       1,     0,     2,     2,     0,     1,     3,     1,     1,     0,
       1,     2,     1,     1,     0,     2,     2,     0,     1,     2,
       0,     1,     2,     0,     2,     2,     0,     1,     2,     0,
       1,     2,     0,     2,     2,     0,     1,     2,     0,     1,
       2,     2,     2,     2,     2,     0,     1,     2,     0,     1,
       2,     2,     2,     0,     1,     2,     0,     1,     2,     0,
       1,     2,     2,     0,     1,     2,     0,     1,     2,     0,
       2,     0,     3,     2,     1,     0,     2,     1,     1,     0,
       2,     1,     2,     1,     2,     3,     3,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     0,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     1,     1,     1,     1,
       1,     1,     1,     3,     0,     1,     1,     3,     3,     1,
       3,     3,     1,     3,     1,     2,     2,     1,     3,     1,
       1,     3,     1,     3,     1,     3,     1,     2,     2,     1,
       1,     2,     1,     1,     2,     1,     1,     1,     1,     2,
       1,     0,     2,     1,     1,     1,     3,     1,     1,     2,
       1,     0,     1,     1,     1,     2,     1,     1,     1,     1,
       1,     1,     1,     2,     1,     1,     3,     0,     1,     1,
       2,     1,     1,     1,     1,     1,     1,     1,     2,     2,
       2,     4,     3,     1,     1,     2,     1,     1,     1,     1,
       1,     1,     1,     2,     2,     2,     1,     1,     1,     2,
       2,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     2,     1,
       1,     1,     1,     3,     2,     2,     1,     1,     2,     1,
       1,     3,     2,     2,     1,     1,     3,     3,     4,     5,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     1,     3,     1,     1,     1,     1,
       1,     1,     1,     2,     5,     5,     5,     4,     5,     4,
       5,     5,     5,     5,     5,     2,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     4,     5,     0,
       3,     2,     1,     3,     3,     0,     2,     1,     3,     1,
       3,     1,     3,     1,     3,     0,     0,     1,     0,     1,
       0,     1,     0,     2,     0,     2,     0,     1,     1,     0,
       1,     0,     1,     2,     0,     2,     0,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     0,     2,     0,     5,
       0,     3,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     1,
       0,     1,     0,     1,     0,     1,     0,     1,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     1,     0,     3,     0,     1,     0,     1,     0,
       1,     0,     1,     0,     1,     1,     0,     1,     2,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     1,     0,     1,     1,     0,     1,     1,     1,     0,
       1,     1,     0,     1,     0,     1,     0,     1,     1,     0,
       2,     2,     0,     1,     0,     1,     0,     1,     0,     1,
       0,     1,     0,     2,     1,     0,     1,     1,     0,     1,
       0,     1,     0,     1,     1,     0,     2,     1,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     1,     0,     1,     0,     1,     0,     1,     0,     1,
       2,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     1,     0,     1,     0,     3,     0,     1,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     2,     1,     1,     1,     1,     2,     1,     3,
       2,     1,     1,     1,     2,     1,     2,     1,     2,     1,
       2,     1,     2,     1,     2,     1,     2,     2
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                  \
do                                                              \
  if (yychar == YYEMPTY)                                        \
    {                                                           \
      yychar = (Token);                                         \
      yylval = (Value);                                         \
      YYPOPSTACK (yylen);                                       \
      yystate = *yyssp;                                         \
      goto yybackup;                                            \
    }                                                           \
  else                                                          \
    {                                                           \
      yyerror (YY_("syntax error: cannot back up")); \
      YYERROR;                                                  \
    }                                                           \
while (0)

/* Error token number */
#define YYTERROR        1
#define YYERRCODE       256



/* Enable debugging if requested.  */
#if YYDEBUG

# ifndef YYFPRINTF
#  include <stdio.h> /* INFRINGES ON USER NAME SPACE */
#  define YYFPRINTF fprintf
# endif

# define YYDPRINTF(Args)                        \
do {                                            \
  if (yydebug)                                  \
    YYFPRINTF Args;                             \
} while (0)

/* This macro is provided for backward compatibility. */
#ifndef YY_LOCATION_PRINT
# define YY_LOCATION_PRINT(File, Loc) ((void) 0)
#endif


# define YY_SYMBOL_PRINT(Title, Type, Value, Location)                    \
do {                                                                      \
  if (yydebug)                                                            \
    {                                                                     \
      YYFPRINTF (stderr, "%s ", Title);                                   \
      yy_symbol_print (stderr,                                            \
                  Type, Value); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyoutput, yytoknum[yytype], *yyvaluep);
# endif
  YYUSE (yytype);
}


/*--------------------------------.
| Print this symbol on YYOUTPUT.  |
`--------------------------------*/

static void
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep);
  YYFPRINTF (yyoutput, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yytype_int16 *yybottom, yytype_int16 *yytop)
{
  YYFPRINTF (stderr, "Stack now");
  for (; yybottom <= yytop; yybottom++)
    {
      int yybot = *yybottom;
      YYFPRINTF (stderr, " %d", yybot);
    }
  YYFPRINTF (stderr, "\n");
}

# define YY_STACK_PRINT(Bottom, Top)                            \
do {                                                            \
  if (yydebug)                                                  \
    yy_stack_print ((Bottom), (Top));                           \
} while (0)


/*------------------------------------------------.
| Report that the YYRULE is going to be reduced.  |
`------------------------------------------------*/

static void
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  unsigned long int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %lu):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[yyssp[yyi + 1 - yynrhs]],
                       &(yyvsp[(yyi + 1) - (yynrhs)])
                                              );
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule); \
} while (0)

/* Nonzero means print parse trace.  It is left uninitialized so that
   multiple parsers can coexist.  */
int yydebug;
#else /* !YYDEBUG */
# define YYDPRINTF(Args)
# define YY_SYMBOL_PRINT(Title, Type, Value, Location)
# define YY_STACK_PRINT(Bottom, Top)
# define YY_REDUCE_PRINT(Rule)
#endif /* !YYDEBUG */


/* YYINITDEPTH -- initial size of the parser's stacks.  */
#ifndef YYINITDEPTH
# define YYINITDEPTH 200
#endif

/* YYMAXDEPTH -- maximum size the stacks can grow to (effective only
   if the built-in stack extension method is used).

   Do not make this value too large; the results are undefined if
   YYSTACK_ALLOC_MAXIMUM < YYSTACK_BYTES (YYMAXDEPTH)
   evaluated with infinite-precision integer arithmetic.  */

#ifndef YYMAXDEPTH
# define YYMAXDEPTH 10000
#endif


#if YYERROR_VERBOSE

# ifndef yystrlen
#  if defined __GLIBC__ && defined _STRING_H
#   define yystrlen strlen
#  else
/* Return the length of YYSTR.  */
static YYSIZE_T
yystrlen (const char *yystr)
{
  YYSIZE_T yylen;
  for (yylen = 0; yystr[yylen]; yylen++)
    continue;
  return yylen;
}
#  endif
# endif

# ifndef yystpcpy
#  if defined __GLIBC__ && defined _STRING_H && defined _GNU_SOURCE
#   define yystpcpy stpcpy
#  else
/* Copy YYSRC to YYDEST, returning the address of the terminating '\0' in
   YYDEST.  */
static char *
yystpcpy (char *yydest, const char *yysrc)
{
  char *yyd = yydest;
  const char *yys = yysrc;

  while ((*yyd++ = *yys++) != '\0')
    continue;

  return yyd - 1;
}
#  endif
# endif

# ifndef yytnamerr
/* Copy to YYRES the contents of YYSTR after stripping away unnecessary
   quotes and backslashes, so that it's suitable for yyerror.  The
   heuristic is that double-quoting is unnecessary unless the string
   contains an apostrophe, a comma, or backslash (other than
   backslash-backslash).  YYSTR is taken from yytname.  If YYRES is
   null, do not copy; instead, return the length of what the result
   would have been.  */
static YYSIZE_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYSIZE_T yyn = 0;
      char const *yyp = yystr;

      for (;;)
        switch (*++yyp)
          {
          case '\'':
          case ',':
            goto do_not_strip_quotes;

          case '\\':
            if (*++yyp != '\\')
              goto do_not_strip_quotes;
            /* Fall through.  */
          default:
            if (yyres)
              yyres[yyn] = *yyp;
            yyn++;
            break;

          case '"':
            if (yyres)
              yyres[yyn] = '\0';
            return yyn;
          }
    do_not_strip_quotes: ;
    }

  if (! yyres)
    return yystrlen (yystr);

  return yystpcpy (yyres, yystr) - yyres;
}
# endif

/* Copy into *YYMSG, which is of size *YYMSG_ALLOC, an error message
   about the unexpected token YYTOKEN for the state stack whose top is
   YYSSP.

   Return 0 if *YYMSG was successfully written.  Return 1 if *YYMSG is
   not large enough to hold the message.  In that case, also set
   *YYMSG_ALLOC to the required number of bytes.  Return 2 if the
   required number of bytes is too large to store.  */
static int
yysyntax_error (YYSIZE_T *yymsg_alloc, char **yymsg,
                yytype_int16 *yyssp, int yytoken)
{
  YYSIZE_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
  YYSIZE_T yysize = yysize0;
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat. */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Number of reported tokens (one for the "unexpected", one per
     "expected"). */
  int yycount = 0;

  /* There are many possibilities here to consider:
     - If this state is a consistent state with a default action, then
       the only way this function was invoked is if the default action
       is an error action.  In that case, don't check for expected
       tokens because there are none.
     - The only way there can be no lookahead present (in yychar) is if
       this state is a consistent state with a default action.  Thus,
       detecting the absence of a lookahead is sufficient to determine
       that there is no unexpected or expected token to report.  In that
       case, just report a simple "syntax error".
     - Don't assume there isn't a lookahead just because this state is a
       consistent state with a default action.  There might have been a
       previous inconsistent state, consistent state with a non-default
       action, or user semantic action that manipulated yychar.
     - Of course, the expected token list depends on states to have
       correct lookahead information, and it depends on the parser not
       to perform extra reductions after fetching a lookahead from the
       scanner and before detecting a syntax error.  Thus, state merging
       (from LALR or IELR) and default reductions corrupt the expected
       token list.  However, the list is correct for canonical LR with
       one exception: it will still contain any token that will not be
       accepted due to an error action in a later state.
  */
  if (yytoken != YYEMPTY)
    {
      int yyn = yypact[*yyssp];
      yyarg[yycount++] = yytname[yytoken];
      if (!yypact_value_is_default (yyn))
        {
          /* Start YYX at -YYN if negative to avoid negative indexes in
             YYCHECK.  In other words, skip the first -YYN actions for
             this state because they are default actions.  */
          int yyxbegin = yyn < 0 ? -yyn : 0;
          /* Stay within bounds of both yycheck and yytname.  */
          int yychecklim = YYLAST - yyn + 1;
          int yyxend = yychecklim < YYNTOKENS ? yychecklim : YYNTOKENS;
          int yyx;

          for (yyx = yyxbegin; yyx < yyxend; ++yyx)
            if (yycheck[yyx + yyn] == yyx && yyx != YYTERROR
                && !yytable_value_is_error (yytable[yyx + yyn]))
              {
                if (yycount == YYERROR_VERBOSE_ARGS_MAXIMUM)
                  {
                    yycount = 1;
                    yysize = yysize0;
                    break;
                  }
                yyarg[yycount++] = yytname[yyx];
                {
                  YYSIZE_T yysize1 = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (! (yysize <= yysize1
                         && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
                    return 2;
                  yysize = yysize1;
                }
              }
        }
    }

  switch (yycount)
    {
# define YYCASE_(N, S)                      \
      case N:                               \
        yyformat = S;                       \
      break
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
    if (! (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM))
      return 2;
    yysize = yysize1;
  }

  if (*yymsg_alloc < yysize)
    {
      *yymsg_alloc = 2 * yysize;
      if (! (yysize <= *yymsg_alloc
             && *yymsg_alloc <= YYSTACK_ALLOC_MAXIMUM))
        *yymsg_alloc = YYSTACK_ALLOC_MAXIMUM;
      return 1;
    }

  /* Avoid sprintf, as that infringes on the user's name space.
     Don't have undefined behavior even if the translation
     produced a string with the wrong number of "%s"s.  */
  {
    char *yyp = *yymsg;
    int yyi = 0;
    while ((*yyp = *yyformat) != '\0')
      if (*yyp == '%' && yyformat[1] == 's' && yyi < yycount)
        {
          yyp += yytnamerr (yyp, yyarg[yyi++]);
          yyformat += 2;
        }
      else
        {
          yyp++;
          yyformat++;
        }
  }
  return 0;
}
#endif /* YYERROR_VERBOSE */

/*-----------------------------------------------.
| Release the memory associated to this symbol.  |
`-----------------------------------------------*/

static void
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep)
{
  YYUSE (yyvaluep);
  if (!yymsg)
    yymsg = "Deleting";
  YY_SYMBOL_PRINT (yymsg, yytype, yyvaluep, yylocationp);

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}




/* The lookahead symbol.  */
int yychar;

/* The semantic value of the lookahead symbol.  */
YYSTYPE yylval;
/* Number of syntax errors so far.  */
int yynerrs;


/*----------.
| yyparse.  |
`----------*/

int
yyparse (void)
{
    int yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yytype_int16 yyssa[YYINITDEPTH];
    yytype_int16 *yyss;
    yytype_int16 *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYSIZE_T yystacksize;

  int yyn;
  int yyresult;
  /* Lookahead token as an internal (translated) token number.  */
  int yytoken = 0;
  /* The variables used to return semantic value and location from the
     action routines.  */
  YYSTYPE yyval;

#if YYERROR_VERBOSE
  /* Buffer for error messages, and its allocated size.  */
  char yymsgbuf[128];
  char *yymsg = yymsgbuf;
  YYSIZE_T yymsg_alloc = sizeof yymsgbuf;
#endif

#define YYPOPSTACK(N)   (yyvsp -= (N), yyssp -= (N))

  /* The number of symbols on the RHS of the reduced rule.
     Keep to zero when no symbol should be popped.  */
  int yylen = 0;

  yyssp = yyss = yyssa;
  yyvsp = yyvs = yyvsa;
  yystacksize = YYINITDEPTH;

  YYDPRINTF ((stderr, "Starting parse\n"));

  yystate = 0;
  yyerrstatus = 0;
  yynerrs = 0;
  yychar = YYEMPTY; /* Cause a token to be read.  */
  goto yysetstate;

/*------------------------------------------------------------.
| yynewstate -- Push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
 yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;

 yysetstate:
  *yyssp = yystate;

  if (yyss + yystacksize - 1 <= yyssp)
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = yyssp - yyss + 1;

#ifdef yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        YYSTYPE *yyvs1 = yyvs;
        yytype_int16 *yyss1 = yyss;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * sizeof (*yyssp),
                    &yyvs1, yysize * sizeof (*yyvsp),
                    &yystacksize);

        yyss = yyss1;
        yyvs = yyvs1;
      }
#else /* no yyoverflow */
# ifndef YYSTACK_RELOCATE
      goto yyexhaustedlab;
# else
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yytype_int16 *yyss1 = yyss;
        union yyalloc *yyptr =
          (union yyalloc *) YYSTACK_ALLOC (YYSTACK_BYTES (yystacksize));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
#  undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif
#endif /* no yyoverflow */

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long int) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }

  YYDPRINTF ((stderr, "Entering state %d\n", yystate));

  if (yystate == YYFINAL)
    YYACCEPT;

  goto yybackup;

/*-----------.
| yybackup.  |
`-----------*/
yybackup:

  /* Do appropriate processing given the current state.  Read a
     lookahead token if we need one and don't already have one.  */

  /* First try to decide what to do without reference to lookahead token.  */
  yyn = yypact[yystate];
  if (yypact_value_is_default (yyn))
    goto yydefault;

  /* Not known => get a lookahead token if don't already have one.  */

  /* YYCHAR is either YYEMPTY or YYEOF or a valid lookahead symbol.  */
  if (yychar == YYEMPTY)
    {
      YYDPRINTF ((stderr, "Reading a token: "));
      yychar = yylex ();
    }

  if (yychar <= YYEOF)
    {
      yychar = yytoken = YYEOF;
      YYDPRINTF ((stderr, "Now at end of input.\n"));
    }
  else
    {
      yytoken = YYTRANSLATE (yychar);
      YY_SYMBOL_PRINT ("Next token is", yytoken, &yylval, &yylloc);
    }

  /* If the proper action on seeing token YYTOKEN is to reduce or to
     detect an error, take that action.  */
  yyn += yytoken;
  if (yyn < 0 || YYLAST < yyn || yycheck[yyn] != yytoken)
    goto yydefault;
  yyn = yytable[yyn];
  if (yyn <= 0)
    {
      if (yytable_value_is_error (yyn))
        goto yyerrlab;
      yyn = -yyn;
      goto yyreduce;
    }

  /* Count tokens shifted since error; after three, turn off error
     status.  */
  if (yyerrstatus)
    yyerrstatus--;

  /* Shift the lookahead token.  */
  YY_SYMBOL_PRINT ("Shifting", yytoken, &yylval, &yylloc);

  /* Discard the shifted token.  */
  yychar = YYEMPTY;

  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  goto yynewstate;


/*-----------------------------------------------------------.
| yydefault -- do the default action for the current state.  |
`-----------------------------------------------------------*/
yydefault:
  yyn = yydefact[yystate];
  if (yyn == 0)
    goto yyerrlab;
  goto yyreduce;


/*-----------------------------.
| yyreduce -- Do a reduction.  |
`-----------------------------*/
yyreduce:
  /* yyn is the number of a rule to reduce with.  */
  yylen = yyr2[yyn];

  /* If YYLEN is nonzero, implement the default value of the action:
     '$$ = $1'.

     Otherwise, the following line sets YYVAL to garbage.
     This behavior is undocumented and Bison
     users should not rely upon it.  Assigning to YYVAL
     unconditionally makes the parser a bit smaller, and it avoids a
     GCC warning that YYVAL may be used uninitialized.  */
  yyval = yyvsp[1-yylen];


  YY_REDUCE_PRINT (yyn);
  switch (yyn)
    {
        case 2:
#line 2855 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	clear_initial_values ();
	current_program = NULL;
	defined_prog_list = NULL;
	cobc_cs_check = 0;
	main_flag_set = 0;
	current_program = cb_build_program (NULL, 0);
	cb_set_intr_when_compiled ();
	cb_build_registers ();
  }
#line 11012 "parser.c" /* yacc.c:1646  */
    break;

  case 3:
#line 2866 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (!current_program->flag_validated) {
		current_program->flag_validated = 1;
		cb_validate_program_body (current_program);
	}
	if (depth > 1) {
		cb_error (_("multiple PROGRAM-ID's without matching END PROGRAM"));
	}
	if (cobc_flag_main && !main_flag_set) {
		cb_error (_("executable requested but no program found"));
	}
	if (errorcount > 0) {
		YYABORT;
	}
	if (!current_program->entry_list) {
		backup_current_pos ();
		emit_entry (current_program->program_id, 0, NULL, NULL);
	}
  }
#line 11036 "parser.c" /* yacc.c:1646  */
    break;

  case 6:
#line 2893 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	first_prog = 1;
	depth = 0;
	setup_from_identification = 0;
  }
#line 11046 "parser.c" /* yacc.c:1646  */
    break;

  case 12:
#line 2912 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree		l;

	current_section = NULL;
	current_paragraph = NULL;
	l = cb_build_alphanumeric_literal (demangle_name,
					   strlen (demangle_name));
	current_program->program_name = (char *)CB_LITERAL (l)->data;
	current_program->program_id
		= cb_build_program_id (current_program->program_name, 0);
	current_program->prog_type = COB_MODULE_TYPE_PROGRAM;
	if (!main_flag_set) {
		main_flag_set = 1;
		current_program->flag_main = cobc_flag_main;
	}
	check_relaxed_syntax (COBC_HD_PROGRAM_ID);
  }
#line 11068 "parser.c" /* yacc.c:1646  */
    break;

  case 13:
#line 2931 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	backup_current_pos ();
	clean_up_program (NULL, COB_MODULE_TYPE_PROGRAM);
  }
#line 11077 "parser.c" /* yacc.c:1646  */
    break;

  case 16:
#line 2959 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	backup_current_pos ();
	clean_up_program (NULL, COB_MODULE_TYPE_PROGRAM);
  }
#line 11086 "parser.c" /* yacc.c:1646  */
    break;

  case 20:
#line 2973 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	backup_current_pos ();
  }
#line 11094 "parser.c" /* yacc.c:1646  */
    break;

  case 21:
#line 2977 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	first_nested_program = 0;
	clean_up_program ((yyvsp[-1]), COB_MODULE_TYPE_PROGRAM);
  }
#line 11103 "parser.c" /* yacc.c:1646  */
    break;

  case 22:
#line 2985 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	backup_current_pos ();
  }
#line 11111 "parser.c" /* yacc.c:1646  */
    break;

  case 23:
#line 2989 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	clean_up_program ((yyvsp[-1]), COB_MODULE_TYPE_FUNCTION);
  }
#line 11119 "parser.c" /* yacc.c:1646  */
    break;

  case 26:
#line 3007 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	setup_program_start ();
	setup_from_identification = 1;
  }
#line 11128 "parser.c" /* yacc.c:1646  */
    break;

  case 29:
#line 3020 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_in_id = 1;
  }
#line 11136 "parser.c" /* yacc.c:1646  */
    break;

  case 30:
#line 3024 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (setup_program ((yyvsp[-1]), (yyvsp[0]), COB_MODULE_TYPE_PROGRAM)) {
		YYABORT;
	}

	setup_prototype ((yyvsp[-1]), (yyvsp[0]), COB_MODULE_TYPE_PROGRAM, 1);
  }
#line 11148 "parser.c" /* yacc.c:1646  */
    break;

  case 31:
#line 3032 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	cobc_in_id = 0;
  }
#line 11157 "parser.c" /* yacc.c:1646  */
    break;

  case 32:
#line 3040 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_in_id = 1;
  }
#line 11165 "parser.c" /* yacc.c:1646  */
    break;

  case 33:
#line 3044 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (setup_program ((yyvsp[-2]), (yyvsp[-1]), COB_MODULE_TYPE_FUNCTION)) {
		YYABORT;
	}
	setup_prototype ((yyvsp[-2]), (yyvsp[-1]), COB_MODULE_TYPE_FUNCTION, 1);
	cobc_cs_check = 0;
	cobc_in_id = 0;
  }
#line 11178 "parser.c" /* yacc.c:1646  */
    break;

  case 34:
#line 3056 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_REFERENCE_P ((yyvsp[0])) && CB_WORD_COUNT ((yyvsp[0])) > 0) {
		redefinition_error ((yyvsp[0]));
	}
	/*
	  The program name is a key part of defining the current_program, so we
	  mustn't lose it (unlike in undefined_word).
	*/
	(yyval) = (yyvsp[0]);
  }
#line 11193 "parser.c" /* yacc.c:1646  */
    break;

  case 35:
#line 3067 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_trim_program_id ((yyvsp[0]));
  }
#line 11201 "parser.c" /* yacc.c:1646  */
    break;

  case 37:
#line 3075 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_trim_program_id ((yyvsp[0]));
  }
#line 11209 "parser.c" /* yacc.c:1646  */
    break;

  case 38:
#line 3081 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 11215 "parser.c" /* yacc.c:1646  */
    break;

  case 39:
#line 3082 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 11221 "parser.c" /* yacc.c:1646  */
    break;

  case 42:
#line 3091 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (!current_program->nested_level) {
		cb_error (_("COMMON may only be used in a contained program"));
	} else {
		current_program->flag_common = 1;
		cb_add_common_prog (current_program);
	}
  }
#line 11234 "parser.c" /* yacc.c:1646  */
    break;

  case 43:
#line 3100 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (!current_program->nested_level) {
		cb_error (_("COMMON may only be used in a contained program"));
	} else {
		current_program->flag_common = 1;
		cb_add_common_prog (current_program);
	}
  }
#line 11247 "parser.c" /* yacc.c:1646  */
    break;

  case 45:
#line 3110 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING (_("CALL prototypes"));
  }
#line 11255 "parser.c" /* yacc.c:1646  */
    break;

  case 48:
#line 3122 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_program->flag_initial = 1;
  }
#line 11263 "parser.c" /* yacc.c:1646  */
    break;

  case 49:
#line 3126 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_program->flag_recursive = 1;
  }
#line 11271 "parser.c" /* yacc.c:1646  */
    break;

  case 51:
#line 3135 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 11279 "parser.c" /* yacc.c:1646  */
    break;

  case 55:
#line 3155 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
/* FIXME: the IBM-compatible ARITHMETIC should only be disabled
          for the specified program (and its nested programs)
   note: ibm-strict.conf has no OPTIONS paragraph, but ibm.conf does */
	cb_arithmetic_osvs = 0;
  }
#line 11290 "parser.c" /* yacc.c:1646  */
    break;

  case 56:
#line 3162 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("STANDARD ARITHMETIC");
  }
#line 11298 "parser.c" /* yacc.c:1646  */
    break;

  case 57:
#line 3166 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("STANDARD-BINARY ARITHMETIC");
  }
#line 11306 "parser.c" /* yacc.c:1646  */
    break;

  case 58:
#line 3170 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("STANDARD-DECIMAL ARITHMETIC");
  }
#line 11314 "parser.c" /* yacc.c:1646  */
    break;

  case 59:
#line 3185 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	default_rounded_mode = cb_int (COB_STORE_ROUND);
  }
#line 11322 "parser.c" /* yacc.c:1646  */
    break;

  case 60:
#line 3189 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		default_rounded_mode = (yyvsp[0]);
	} else {
		default_rounded_mode = cb_int (COB_STORE_ROUND);
	}
  }
#line 11334 "parser.c" /* yacc.c:1646  */
    break;

  case 62:
#line 3201 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_program->entry_convention = (yyvsp[0]);
  }
#line 11342 "parser.c" /* yacc.c:1646  */
    break;

  case 63:
#line 3208 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (CB_CONV_COBOL);
  }
#line 11350 "parser.c" /* yacc.c:1646  */
    break;

  case 64:
#line 3212 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (0);
  }
#line 11358 "parser.c" /* yacc.c:1646  */
    break;

  case 65:
#line 3216 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (CB_CONV_STDCALL);
  }
#line 11366 "parser.c" /* yacc.c:1646  */
    break;

  case 67:
#line 3224 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("INTERMEDIATE ROUNDING");
  }
#line 11374 "parser.c" /* yacc.c:1646  */
    break;

  case 68:
#line 3231 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_AWAY_FROM_ZERO);
  }
#line 11382 "parser.c" /* yacc.c:1646  */
    break;

  case 69:
#line 3235 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_EVEN);
  }
#line 11390 "parser.c" /* yacc.c:1646  */
    break;

  case 70:
#line 3239 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_PROHIBITED);
  }
#line 11398 "parser.c" /* yacc.c:1646  */
    break;

  case 71:
#line 3243 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_TRUNCATION);
  }
#line 11406 "parser.c" /* yacc.c:1646  */
    break;

  case 74:
#line 3258 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	header_check |= COBC_HD_ENVIRONMENT_DIVISION;
  }
#line 11414 "parser.c" /* yacc.c:1646  */
    break;

  case 77:
#line 3272 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_CONFIGURATION_SECTION;
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "CONFIGURATION SECTION");
	}
  }
#line 11426 "parser.c" /* yacc.c:1646  */
    break;

  case 87:
#line 3303 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION, 0, 0);
	check_conf_section_order (COBC_HD_SOURCE_COMPUTER);
	set_conf_section_part (COBC_HD_SOURCE_COMPUTER);
  }
#line 11437 "parser.c" /* yacc.c:1646  */
    break;

  case 92:
#line 3319 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_program->flag_debugging = 1;
	needs_debug_item = 1;
	cobc_cs_check = 0;
	cb_build_debug_item ();
  }
#line 11448 "parser.c" /* yacc.c:1646  */
    break;

  case 93:
#line 3331 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION, 0, 0);
	check_conf_section_order (COBC_HD_OBJECT_COMPUTER);
	set_conf_section_part (COBC_HD_OBJECT_COMPUTER);
  }
#line 11459 "parser.c" /* yacc.c:1646  */
    break;

  case 94:
#line 3338 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 11467 "parser.c" /* yacc.c:1646  */
    break;

  case 105:
#line 3364 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_memory_size_clause, "MEMORY SIZE");
  }
#line 11475 "parser.c" /* yacc.c:1646  */
    break;

  case 106:
#line 3372 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_program->collating_sequence = (yyvsp[0]);
  }
#line 11483 "parser.c" /* yacc.c:1646  */
    break;

  case 107:
#line 3379 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	int segnum;

	if (cb_verify (cb_section_segments, "SEGMENT LIMIT")) {
		segnum = cb_get_int ((yyvsp[0]));
		if (segnum == 0 || segnum > 49) {
			cb_error (_("segment-number must be in range of values 1 to 49"));
			(yyval) = NULL;
		}
	}
	/* Ignore */
  }
#line 11500 "parser.c" /* yacc.c:1646  */
    break;

  case 108:
#line 3395 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (current_program->classification) {
		cb_error (_("duplicate CLASSIFICATION clause"));
	} else {
		current_program->classification = (yyvsp[0]);
	}
  }
#line 11512 "parser.c" /* yacc.c:1646  */
    break;

  case 109:
#line 3406 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 11520 "parser.c" /* yacc.c:1646  */
    break;

  case 110:
#line 3410 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 11528 "parser.c" /* yacc.c:1646  */
    break;

  case 111:
#line 3414 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 11536 "parser.c" /* yacc.c:1646  */
    break;

  case 112:
#line 3418 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 11544 "parser.c" /* yacc.c:1646  */
    break;

  case 115:
#line 3432 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION, 0, 0);
	check_conf_section_order (COBC_HD_REPOSITORY);
	set_conf_section_part (COBC_HD_REPOSITORY);
  }
#line 11555 "parser.c" /* yacc.c:1646  */
    break;

  case 116:
#line 3439 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_in_repository = 0;
  }
#line 11563 "parser.c" /* yacc.c:1646  */
    break;

  case 119:
#line 3448 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	yyerrok;
  }
#line 11571 "parser.c" /* yacc.c:1646  */
    break;

  case 122:
#line 3460 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	functions_are_all = 1;
  }
#line 11579 "parser.c" /* yacc.c:1646  */
    break;

  case 123:
#line 3464 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-1]) != cb_error_node) {
		setup_prototype ((yyvsp[-1]), (yyvsp[0]), COB_MODULE_TYPE_FUNCTION, 0);
	}
  }
#line 11589 "parser.c" /* yacc.c:1646  */
    break;

  case 125:
#line 3471 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	  if ((yyvsp[-1]) != cb_error_node
	      && cb_verify (cb_program_prototypes, _("PROGRAM phrase"))) {
		setup_prototype ((yyvsp[-1]), (yyvsp[0]), COB_MODULE_TYPE_PROGRAM, 0);
	}
  }
#line 11600 "parser.c" /* yacc.c:1646  */
    break;

  case 126:
#line 3478 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	  yyerrok;
  }
#line 11608 "parser.c" /* yacc.c:1646  */
    break;

  case 127:
#line 3485 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_program->function_spec_list =
		cb_list_add (current_program->function_spec_list, (yyvsp[0]));
  }
#line 11617 "parser.c" /* yacc.c:1646  */
    break;

  case 128:
#line 3490 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_program->function_spec_list =
		cb_list_add (current_program->function_spec_list, (yyvsp[0]));
  }
#line 11626 "parser.c" /* yacc.c:1646  */
    break;

  case 129:
#line 3501 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_duplicate = 0;
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION, 0, 0);
	check_conf_section_order (COBC_HD_SPECIAL_NAMES);
	set_conf_section_part (COBC_HD_SPECIAL_NAMES);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	}
  }
#line 11641 "parser.c" /* yacc.c:1646  */
    break;

  case 146:
#line 3542 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	char system_name[16];
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	check_duplicate = 0;
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
		save_tree = NULL;
	} else {
		/* get system name and revert word-combination of scanner.l,
		   if necessary (e.g. SWITCH A <--> SWITCH_A) */
		system_name[15] = 0;
		strncpy(system_name, CB_NAME ((yyvsp[0])), 15);
		if (system_name [6] == '_') {
			system_name [6] = ' ';
		}
		/* lookup system name */
		save_tree = get_system_name (system_name);
		if (!save_tree) {
			cb_error_x ((yyvsp[0]), _("invalid system-name '%s'"), system_name);
		}
	}
  }
#line 11670 "parser.c" /* yacc.c:1646  */
    break;

  case 148:
#line 3571 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (save_tree) {
		if (CB_SYSTEM_NAME(save_tree)->token != CB_DEVICE_CONSOLE) {
			cb_error_x (save_tree, _("invalid %s clause"), "");
		} else {
			current_program->flag_console_is_crt = 1;
		}
	}
  }
#line 11684 "parser.c" /* yacc.c:1646  */
    break;

  case 149:
#line 3582 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (save_tree) {
		if (CB_SYSTEM_NAME(save_tree)->token != CB_FEATURE_CONVENTION) {
			cb_error_x (save_tree, _("invalid %s clause"), "SPECIAL NAMES");
		} else if (CB_VALID_TREE ((yyvsp[0]))) {
			CB_SYSTEM_NAME(save_tree)->value = (yyvsp[-2]);
			cb_define ((yyvsp[0]), save_tree);
			CB_CHAIN_PAIR (current_program->mnemonic_spec_list,
					(yyvsp[0]), save_tree);
			/* remove non-standard context-sensitive words when identical to mnemonic */
			if (strcasecmp (CB_NAME((yyvsp[0])), "EXTERN") == 0 ||
			    strcasecmp (CB_NAME((yyvsp[0])), "STDCALL") == 0 ||
			    strcasecmp (CB_NAME((yyvsp[0])), "STATIC") == 0) {
				remove_context_sensitivity (CB_NAME((yyvsp[0])), CB_CS_CALL);
			}
		}
	}
  }
#line 11707 "parser.c" /* yacc.c:1646  */
    break;

  case 150:
#line 3601 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (save_tree && CB_VALID_TREE ((yyvsp[-1]))) {
		cb_define ((yyvsp[-1]), save_tree);
		CB_CHAIN_PAIR (current_program->mnemonic_spec_list,
				(yyvsp[-1]), save_tree);
	}
  }
#line 11719 "parser.c" /* yacc.c:1646  */
    break;

  case 154:
#line 3617 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	  check_on_off_duplicate = 0;
  }
#line 11727 "parser.c" /* yacc.c:1646  */
    break;

  case 155:
#line 3624 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	/* cb_define_switch_name checks param validity */
	x = cb_define_switch_name ((yyvsp[0]), save_tree, (yyvsp[-2]) == cb_int1);
	if (x) {
		if ((yyvsp[-2]) == cb_int1) {
			check_repeated ("ON", SYN_CLAUSE_1, &check_on_off_duplicate);
		} else {
			check_repeated ("OFF", SYN_CLAUSE_2, &check_on_off_duplicate);
		}
		CB_CHAIN_PAIR (current_program->mnemonic_spec_list, (yyvsp[0]), x);
	}
  }
#line 11746 "parser.c" /* yacc.c:1646  */
    break;

  case 156:
#line 3639 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	/* cb_define_switch_name checks param validity */
	x = cb_define_switch_name ((yyvsp[0]), save_tree, (yyvsp[-2]) == cb_int1);
	if (x) {
		if ((yyvsp[-2]) == cb_int1) {
			check_repeated ("ON", SYN_CLAUSE_1, &check_on_off_duplicate);
		} else {
			check_repeated ("OFF", SYN_CLAUSE_2, &check_on_off_duplicate);
		}
		CB_CHAIN_PAIR (current_program->mnemonic_spec_list, (yyvsp[0]), x);
	}
  }
#line 11765 "parser.c" /* yacc.c:1646  */
    break;

  case 157:
#line 3659 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
		(yyval) = NULL;
	} else {
		/* Returns null on error */
		(yyval) = cb_build_alphabet_name ((yyvsp[0]));
	}
  }
#line 11782 "parser.c" /* yacc.c:1646  */
    break;

  case 158:
#line 3672 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-2])) {
		current_program->alphabet_name_list =
			cb_list_add (current_program->alphabet_name_list, (yyvsp[-2]));
	}
	cobc_cs_check = 0;
  }
#line 11794 "parser.c" /* yacc.c:1646  */
    break;

  case 159:
#line 3683 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_NATIVE;
	}
  }
#line 11804 "parser.c" /* yacc.c:1646  */
    break;

  case 160:
#line 3689 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_ASCII;
	}
  }
#line 11814 "parser.c" /* yacc.c:1646  */
    break;

  case 161:
#line 3695 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_ASCII;
	}
  }
#line 11824 "parser.c" /* yacc.c:1646  */
    break;

  case 162:
#line 3701 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_EBCDIC;
	}
  }
#line 11834 "parser.c" /* yacc.c:1646  */
    break;

  case 163:
#line 3707 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_ASCII;
	}
  }
#line 11844 "parser.c" /* yacc.c:1646  */
    break;

  case 164:
#line 3713 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (1)])) {
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->alphabet_type = CB_ALPHABET_CUSTOM;
		CB_ALPHABET_NAME ((yyvsp[(-1) - (1)]))->custom_list = (yyvsp[0]);
	}
  }
#line 11855 "parser.c" /* yacc.c:1646  */
    break;

  case 165:
#line 3723 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 11863 "parser.c" /* yacc.c:1646  */
    break;

  case 166:
#line 3727 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 11871 "parser.c" /* yacc.c:1646  */
    break;

  case 167:
#line 3734 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 11879 "parser.c" /* yacc.c:1646  */
    break;

  case 168:
#line 3738 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[-2]), (yyvsp[0]));
  }
#line 11887 "parser.c" /* yacc.c:1646  */
    break;

  case 169:
#line 3742 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[-1]));
  }
#line 11895 "parser.c" /* yacc.c:1646  */
    break;

  case 170:
#line 3746 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
  }
#line 11903 "parser.c" /* yacc.c:1646  */
    break;

  case 171:
#line 3753 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 11911 "parser.c" /* yacc.c:1646  */
    break;

  case 172:
#line 3757 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_list_add ((yyvsp[-3]), (yyvsp[0]));
  }
#line 11919 "parser.c" /* yacc.c:1646  */
    break;

  case 173:
#line 3763 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 11925 "parser.c" /* yacc.c:1646  */
    break;

  case 174:
#line 3764 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_space; }
#line 11931 "parser.c" /* yacc.c:1646  */
    break;

  case 175:
#line 3765 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_zero; }
#line 11937 "parser.c" /* yacc.c:1646  */
    break;

  case 176:
#line 3766 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_quote; }
#line 11943 "parser.c" /* yacc.c:1646  */
    break;

  case 177:
#line 3767 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_norm_high; }
#line 11949 "parser.c" /* yacc.c:1646  */
    break;

  case 178:
#line 3768 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_norm_low; }
#line 11955 "parser.c" /* yacc.c:1646  */
    break;

  case 179:
#line 3772 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_space; }
#line 11961 "parser.c" /* yacc.c:1646  */
    break;

  case 180:
#line 3773 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_zero; }
#line 11967 "parser.c" /* yacc.c:1646  */
    break;

  case 181:
#line 3781 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else if ((yyvsp[-1])) {
		CB_CHAIN_PAIR (current_program->symbolic_char_list, (yyvsp[-1]), (yyvsp[0]));
	}
  }
#line 11982 "parser.c" /* yacc.c:1646  */
    break;

  case 182:
#line 3795 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 11990 "parser.c" /* yacc.c:1646  */
    break;

  case 183:
#line 3799 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 11998 "parser.c" /* yacc.c:1646  */
    break;

  case 184:
#line 3807 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 12006 "parser.c" /* yacc.c:1646  */
    break;

  case 185:
#line 3814 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 12014 "parser.c" /* yacc.c:1646  */
    break;

  case 186:
#line 3818 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		(yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0]));
	} else {
		(yyval) = (yyvsp[-1]);
	}
  }
#line 12026 "parser.c" /* yacc.c:1646  */
    break;

  case 187:
#line 3829 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree		l1;
	cb_tree		l2;

	if (cb_list_length ((yyvsp[-2])) != cb_list_length ((yyvsp[0]))) {
		cb_error (_("invalid %s clause"), "SYMBOLIC");
		(yyval) = NULL;
	} else {
		l1 = (yyvsp[-2]);
		l2 = (yyvsp[0]);
		for (; l1; l1 = CB_CHAIN (l1), l2 = CB_CHAIN (l2)) {
			CB_PURPOSE (l1) = CB_VALUE (l2);
		}
		(yyval) = (yyvsp[-2]);
	}
  }
#line 12047 "parser.c" /* yacc.c:1646  */
    break;

  case 188:
#line 3849 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0]) == NULL) {
		(yyval) = NULL;
	} else {
		(yyval) = CB_LIST_INIT ((yyvsp[0]));
	}
  }
#line 12059 "parser.c" /* yacc.c:1646  */
    break;

  case 189:
#line 3857 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0]) == NULL) {
		(yyval) = (yyvsp[-1]);
	} else {
		(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
	}
  }
#line 12071 "parser.c" /* yacc.c:1646  */
    break;

  case 190:
#line 3867 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 12077 "parser.c" /* yacc.c:1646  */
    break;

  case 191:
#line 3868 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 12083 "parser.c" /* yacc.c:1646  */
    break;

  case 192:
#line 3877 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	}
	(void)cb_verify (cb_symbolic_constant, "SYMBOLIC CONSTANT");
  }
#line 12097 "parser.c" /* yacc.c:1646  */
    break;

  case 195:
#line 3895 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	struct cb_field *f;
	cb_tree v;

	v = CB_LIST_INIT ((yyvsp[0]));
	f = CB_FIELD (cb_build_constant ((yyvsp[-2]), v));
	f->flag_item_78 = 1;
	f->flag_constant = 1;
	f->flag_is_global = 1;
	f->level = 1;
	f->values = v;
	cb_needs_01 = 1;
	/* Ignore return value */
	(void)cb_validate_78_item (f, 0);
  }
#line 12117 "parser.c" /* yacc.c:1646  */
    break;

  case 196:
#line 3916 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		/* Returns null on error */
		x = cb_build_class_name ((yyvsp[-4]), (yyvsp[-1]));
		if (x) {
			current_program->class_name_list =
				cb_list_add (current_program->class_name_list, x);
		}
	}
  }
#line 12139 "parser.c" /* yacc.c:1646  */
    break;

  case 197:
#line 3936 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 12145 "parser.c" /* yacc.c:1646  */
    break;

  case 198:
#line 3937 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 12151 "parser.c" /* yacc.c:1646  */
    break;

  case 199:
#line 3942 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 12159 "parser.c" /* yacc.c:1646  */
    break;

  case 200:
#line 3946 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_TREE_CLASS ((yyvsp[-2])) != CB_CLASS_NUMERIC &&
	    CB_LITERAL_P ((yyvsp[-2])) && CB_LITERAL ((yyvsp[-2]))->size != 1) {
		cb_error (_("CLASS literal with THRU must have size 1"));
	}
	if (CB_TREE_CLASS ((yyvsp[0])) != CB_CLASS_NUMERIC &&
	    CB_LITERAL_P ((yyvsp[0])) && CB_LITERAL ((yyvsp[0]))->size != 1) {
		cb_error (_("CLASS literal with THRU must have size 1"));
	}
	if (literal_value ((yyvsp[-2])) <= literal_value ((yyvsp[0]))) {
		(yyval) = CB_BUILD_PAIR ((yyvsp[-2]), (yyvsp[0]));
	} else {
		(yyval) = CB_BUILD_PAIR ((yyvsp[0]), (yyvsp[-2]));
	}
  }
#line 12179 "parser.c" /* yacc.c:1646  */
    break;

  case 202:
#line 3966 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 12187 "parser.c" /* yacc.c:1646  */
    break;

  case 203:
#line 3970 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING_X ((yyvsp[0]), _("NATIONAL CLASS"));
	(yyval) = cb_int0;
  }
#line 12196 "parser.c" /* yacc.c:1646  */
    break;

  case 205:
#line 3979 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING_X ((yyvsp[0]), _("CLASS IS integer IN alphabet-name"));
	(yyval) = (yyvsp[0]);
  }
#line 12205 "parser.c" /* yacc.c:1646  */
    break;

  case 206:
#line 3989 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree	l;

	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		/* Returns null on error */
		l = cb_build_locale_name ((yyvsp[-2]), (yyvsp[0]));
		if (l) {
			current_program->locale_list =
				cb_list_add (current_program->locale_list, l);
		}
	}
  }
#line 12227 "parser.c" /* yacc.c:1646  */
    break;

  case 207:
#line 4012 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	unsigned char	*s = CB_LITERAL ((yyvsp[-1]))->data;
	unsigned int	error_ind = 0;
	unsigned int	char_seen = 0;

	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		/* FIXME: actual allowed (depending on dialect), see FR #246 */
		check_repeated ("CURRENCY", SYN_CLAUSE_1, &check_duplicate);

		/* checks of CURRENCY SIGN (being currency string) when separate */
		if ((yyvsp[0])) {
			CB_PENDING_X ((yyvsp[-1]), _("separate currency symbol and currency string"));
			while (*s) {
				switch (*s) {
				case '0':
				case '1':
				case '2':
				case '3':
				case '4':
				case '5':
				case '6':
				case '7':
				case '8':
				case '9':
				case '+':
				case '-':
				case ',':
				case '.':
				case '*':
					error_ind = 1;
					break;
				case ' ':
					break;
				default:
					char_seen = 1;
					break;
				}
				s++;
			}
			if (!char_seen) {
				error_ind = 1;
			}
		}
		if (error_ind) {
			cb_error_x ((yyvsp[-1]), _("invalid CURRENCY SIGN '%s'"), (char*)CB_LITERAL ((yyvsp[-1]))->data);
		}
		if ((yyvsp[0])) {
			set_currency_picture_symbol ((yyvsp[0]));
		} else {
			if (!error_ind) {
				set_currency_picture_symbol ((yyvsp[-1]));
			}
		}
	}
  }
#line 12292 "parser.c" /* yacc.c:1646  */
    break;

  case 208:
#line 4077 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 12300 "parser.c" /* yacc.c:1646  */
    break;

  case 209:
#line 4081 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 12308 "parser.c" /* yacc.c:1646  */
    break;

  case 210:
#line 4090 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("DECIMAL-POINT", SYN_CLAUSE_2, &check_duplicate);
		current_program->decimal_point = ',';
		current_program->numeric_separator = '.';
	}
  }
#line 12325 "parser.c" /* yacc.c:1646  */
    break;

  case 211:
#line 4109 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		current_program->flag_trailing_separate = 1;
	}
  }
#line 12340 "parser.c" /* yacc.c:1646  */
    break;

  case 212:
#line 4125 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("CURSOR", SYN_CLAUSE_3, &check_duplicate);
		current_program->cursor_pos = (yyvsp[0]);
	}
  }
#line 12356 "parser.c" /* yacc.c:1646  */
    break;

  case 213:
#line 4143 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("CRT STATUS", SYN_CLAUSE_4, &check_duplicate);
		current_program->crt_status = (yyvsp[0]);
	}
  }
#line 12372 "parser.c" /* yacc.c:1646  */
    break;

  case 214:
#line 4161 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("SCREEN CONTROL", SYN_CLAUSE_5, &check_duplicate);
		CB_PENDING ("SCREEN CONTROL");
	}
  }
#line 12388 "parser.c" /* yacc.c:1646  */
    break;

  case 215:
#line 4178 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("EVENT STATUS", SYN_CLAUSE_6, &check_duplicate);
		CB_PENDING ("EVENT STATUS");
	}
  }
#line 12404 "parser.c" /* yacc.c:1646  */
    break;

  case 216:
#line 4199 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_validate_program_environment (current_program);
  }
#line 12412 "parser.c" /* yacc.c:1646  */
    break;

  case 218:
#line 4206 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_INPUT_OUTPUT_SECTION;
  }
#line 12421 "parser.c" /* yacc.c:1646  */
    break;

  case 220:
#line 4214 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_INPUT_OUTPUT_SECTION, 0, 0);
	header_check |= COBC_HD_FILE_CONTROL;
  }
#line 12431 "parser.c" /* yacc.c:1646  */
    break;

  case 222:
#line 4223 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_INPUT_OUTPUT_SECTION, 0, 0);
	header_check |= COBC_HD_I_O_CONTROL;
  }
#line 12441 "parser.c" /* yacc.c:1646  */
    break;

  case 225:
#line 4238 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_INPUT_OUTPUT_SECTION,
			       COBC_HD_FILE_CONTROL, 0);
	check_duplicate = 0;
	if (CB_VALID_TREE ((yyvsp[0]))) {
		/* Build new file */
		current_file = build_file ((yyvsp[0]));
		current_file->optional = CB_INTEGER ((yyvsp[-1]))->val;

		/* Add file to current program list */
		CB_ADD_TO_CHAIN (CB_TREE (current_file),
				 current_program->file_list);
	} else if (current_program->file_list) {
		current_program->file_list
			= CB_CHAIN (current_program->file_list);
	}
	key_type = NO_KEY;
  }
#line 12465 "parser.c" /* yacc.c:1646  */
    break;

  case 226:
#line 4258 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	if (current_file->organization == COB_ORG_INDEXED
	    && key_type == RELATIVE_KEY) {
		cb_error_x (current_file->key,
			    _("cannot use RELATIVE KEY clause on INDEXED files"));
	} else if (current_file->organization == COB_ORG_RELATIVE
		   && key_type == RECORD_KEY) {
		cb_error_x (current_file->key,
			    _("cannot use RECORD KEY clause on RELATIVE files"));
	}

	if (CB_VALID_TREE ((yyvsp[-2]))) {
		validate_file (current_file, (yyvsp[-2]));
	}
  }
#line 12486 "parser.c" /* yacc.c:1646  */
    break;

  case 228:
#line 4279 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	yyerrok;
  }
#line 12494 "parser.c" /* yacc.c:1646  */
    break;

  case 230:
#line 4286 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	/* reset context-sensitive words for next clauses */
	cobc_cs_check = CB_CS_SELECT;
  }
#line 12503 "parser.c" /* yacc.c:1646  */
    break;

  case 244:
#line 4327 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
  }
#line 12512 "parser.c" /* yacc.c:1646  */
    break;

  case 245:
#line 4332 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	if ((yyvsp[0])) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
	} else {
		current_file->flag_fileid = 1;
	}
  }
#line 12525 "parser.c" /* yacc.c:1646  */
    break;

  case 246:
#line 4341 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	current_file->organization = COB_ORG_LINE_SEQUENTIAL;
	if ((yyvsp[0])) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
	} else {
		current_file->flag_fileid = 1;
	}
  }
#line 12539 "parser.c" /* yacc.c:1646  */
    break;

  case 247:
#line 4351 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	if ((yyvsp[0])) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
	} else {
		current_file->flag_ext_assign = 0;
		current_file->assign =
			cb_build_alphanumeric_literal ("stdout", (size_t)6);
		current_file->special = COB_SELECT_STDOUT;
	}
  }
#line 12555 "parser.c" /* yacc.c:1646  */
    break;

  case 248:
#line 4363 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	if ((yyvsp[0])) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
	} else {
		current_file->flag_ext_assign = 0;
		current_file->assign =
			cb_build_alphanumeric_literal ("stdin", (size_t)5);
		current_file->special = COB_SELECT_STDIN;
	}
  }
#line 12571 "parser.c" /* yacc.c:1646  */
    break;

  case 249:
#line 4375 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	current_file->organization = COB_ORG_LINE_SEQUENTIAL;
	if ((yyvsp[0])) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
	} else {
		/* RM/COBOL always expects an assignment name here - we ignore this
		   for PRINTER + PRINTER-1 as ACUCOBOL allows this for using as alias */
		current_file->flag_ext_assign = 0;
		if ((yyvsp[-1]) == cb_int0) {
			current_file->assign =
				cb_build_alphanumeric_literal ("PRINTER", (size_t)7);
		} else if ((yyvsp[-1]) == cb_int1) {
			current_file->assign =
				cb_build_alphanumeric_literal ("PRINTER-1", (size_t)9);
		} else {
			current_file->assign =
				cb_build_alphanumeric_literal ("LPT1", (size_t)4);
		}

	}
  }
#line 12598 "parser.c" /* yacc.c:1646  */
    break;

  case 250:
#line 4406 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 12604 "parser.c" /* yacc.c:1646  */
    break;

  case 251:
#line 4407 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 12610 "parser.c" /* yacc.c:1646  */
    break;

  case 252:
#line 4408 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int4; }
#line 12616 "parser.c" /* yacc.c:1646  */
    break;

  case 265:
#line 4432 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_file->flag_line_adv = 1;
  }
#line 12624 "parser.c" /* yacc.c:1646  */
    break;

  case 267:
#line 4440 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_file->flag_ext_assign = 1;
  }
#line 12632 "parser.c" /* yacc.c:1646  */
    break;

  case 271:
#line 4453 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 12640 "parser.c" /* yacc.c:1646  */
    break;

  case 274:
#line 4465 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ACCESS", SYN_CLAUSE_2, &check_duplicate);
  }
#line 12648 "parser.c" /* yacc.c:1646  */
    break;

  case 275:
#line 4471 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { current_file->access_mode = COB_ACCESS_SEQUENTIAL; }
#line 12654 "parser.c" /* yacc.c:1646  */
    break;

  case 276:
#line 4472 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { current_file->access_mode = COB_ACCESS_DYNAMIC; }
#line 12660 "parser.c" /* yacc.c:1646  */
    break;

  case 277:
#line 4473 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { current_file->access_mode = COB_ACCESS_RANDOM; }
#line 12666 "parser.c" /* yacc.c:1646  */
    break;

  case 278:
#line 4481 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	struct cb_alt_key *p;
	struct cb_alt_key *l;

	cb_tree composite_key;

	p = cobc_parse_malloc (sizeof (struct cb_alt_key));
	p->key = (yyvsp[-4]);
	p->component_list = NULL;
	p->duplicates = CB_INTEGER ((yyvsp[-2]))->val;
	p->password = (yyvsp[-1]);
	if ((yyvsp[0])) {
		p->tf_suppress = 1;
		p->char_suppress = CB_INTEGER ((yyvsp[0]))->val;
	} else {
		p->tf_suppress = 0;
	}
	p->next = NULL;
	
	/* handle split keys */
	if ((yyvsp[-3])) {
		/* generate field (in w-s) for composite-key */
		composite_key = cb_build_field((yyvsp[-4]));
		if (composite_key == cb_error_node) {
			YYERROR;
		} else {
			composite_key->category = CB_CATEGORY_ALPHANUMERIC; 
			((struct cb_field *)composite_key)->count = 1;
			p->key = cb_build_field_reference((struct cb_field *)composite_key, NULL);
			p->component_list = key_component_list;
		}
	}

	/* add to the end of list */
	if (current_file->alt_key_list == NULL) {
		current_file->alt_key_list = p;
	} else {
		l = current_file->alt_key_list;
		for (; l->next; l = l->next) { ; }
		l->next = p;
	}
  }
#line 12713 "parser.c" /* yacc.c:1646  */
    break;

  case 279:
#line 4527 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 12721 "parser.c" /* yacc.c:1646  */
    break;

  case 281:
#line 4535 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("PASSWORD clause");
  }
#line 12729 "parser.c" /* yacc.c:1646  */
    break;

  case 282:
#line 4539 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 12737 "parser.c" /* yacc.c:1646  */
    break;

  case 283:
#line 4546 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 12745 "parser.c" /* yacc.c:1646  */
    break;

  case 284:
#line 4551 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (literal_value ((yyvsp[0])));
  }
#line 12753 "parser.c" /* yacc.c:1646  */
    break;

  case 285:
#line 4556 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (literal_value ((yyvsp[0])));
  }
#line 12761 "parser.c" /* yacc.c:1646  */
    break;

  case 286:
#line 4566 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("COLLATING", SYN_CLAUSE_3, &check_duplicate);
	CB_PENDING ("COLLATING SEQUENCE");
  }
#line 12770 "parser.c" /* yacc.c:1646  */
    break;

  case 287:
#line 4574 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_ALPHABET_NAME_P (cb_ref ((yyvsp[0])))) {
		(yyval) = (yyvsp[0]);
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not an alphabet-name"),
			cb_name ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 12784 "parser.c" /* yacc.c:1646  */
    break;

  case 288:
#line 4589 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("STATUS", SYN_CLAUSE_4, &check_duplicate);
	current_file->file_status = (yyvsp[-1]);
	if ((yyvsp[0])) {
		/* add a compiler configuration if either */
		if (cb_std_define != CB_STD_IBM
		 && cb_std_define != CB_STD_MVS
		 && !cb_relaxed_syntax_checks) {
			cb_verify (CB_UNCONFORMABLE, "VSAM STATUS");
		} else {
			cb_warning (warningopt,
				_("%s ignored"), "VSAM STATUS");
		}
	}
  }
#line 12804 "parser.c" /* yacc.c:1646  */
    break;

  case 292:
#line 4615 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LOCK", SYN_CLAUSE_5, &check_duplicate);
  }
#line 12812 "parser.c" /* yacc.c:1646  */
    break;

  case 294:
#line 4623 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_file->lock_mode |= COB_LOCK_MANUAL;
  }
#line 12820 "parser.c" /* yacc.c:1646  */
    break;

  case 295:
#line 4627 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_file->lock_mode |= COB_LOCK_AUTOMATIC;
  }
#line 12828 "parser.c" /* yacc.c:1646  */
    break;

  case 296:
#line 4631 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_file->lock_mode |= COB_LOCK_EXCLUSIVE;
  }
#line 12836 "parser.c" /* yacc.c:1646  */
    break;

  case 299:
#line 4639 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_file->lock_mode |= COB_LOCK_MULTIPLE;
  }
#line 12844 "parser.c" /* yacc.c:1646  */
    break;

  case 300:
#line 4643 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_file->lock_mode |= COB_LOCK_MULTIPLE;
	CB_PENDING ("WITH ROLLBACK");
  }
#line 12853 "parser.c" /* yacc.c:1646  */
    break;

  case 303:
#line 4659 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ORGANIZATION", SYN_CLAUSE_6, &check_duplicate);
	error_if_record_delimiter_incompatible (COB_ORG_INDEXED, "INDEXED");
	current_file->organization = COB_ORG_INDEXED;
  }
#line 12863 "parser.c" /* yacc.c:1646  */
    break;

  case 304:
#line 4665 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ORGANIZATION", SYN_CLAUSE_6, &check_duplicate);
	error_if_record_delimiter_incompatible (COB_ORG_SEQUENTIAL, "SEQUENTIAL");
	current_file->organization = COB_ORG_SEQUENTIAL;
  }
#line 12873 "parser.c" /* yacc.c:1646  */
    break;

  case 305:
#line 4671 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ORGANIZATION", SYN_CLAUSE_6, &check_duplicate);
	error_if_record_delimiter_incompatible (COB_ORG_RELATIVE, "RELATIVE");
	current_file->organization = COB_ORG_RELATIVE;
  }
#line 12883 "parser.c" /* yacc.c:1646  */
    break;

  case 306:
#line 4677 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ORGANIZATION", SYN_CLAUSE_6, &check_duplicate);
	error_if_record_delimiter_incompatible (COB_ORG_LINE_SEQUENTIAL,
						"LINE SEQUENTIAL");
	current_file->organization = COB_ORG_LINE_SEQUENTIAL;
  }
#line 12894 "parser.c" /* yacc.c:1646  */
    break;

  case 307:
#line 4690 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PADDING", SYN_CLAUSE_7, &check_duplicate);
	cb_verify (cb_padding_character_clause, "PADDING CHARACTER");
  }
#line 12903 "parser.c" /* yacc.c:1646  */
    break;

  case 308:
#line 4701 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("RECORD DELIMITER", SYN_CLAUSE_8, &check_duplicate);
	current_file->flag_delimiter = 1;
  }
#line 12912 "parser.c" /* yacc.c:1646  */
    break;

  case 310:
#line 4710 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("RECORD DELIMITER %s only allowed with SEQUENTIAL files"),
			  "STANDARD-1");
	}

	if (cb_verify (cb_record_delimiter, _("RECORD DELIMITER clause"))) {
		cb_warning (warningopt,
			    _("%s ignored"), "RECORD DELIMITER STANDARD-1");
	}
  }
#line 12928 "parser.c" /* yacc.c:1646  */
    break;

  case 311:
#line 4722 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (current_file->organization != COB_ORG_SEQUENTIAL
	    && current_file->organization != COB_ORG_LINE_SEQUENTIAL) {
		cb_error (_("RECORD DELIMITER %s only allowed with (LINE) SEQUENTIAL files"),
			  "LINE-SEQUENTIAL");
	}

	if (cb_verify (cb_record_delimiter, _("RECORD DELIMITER clause"))
	    && cb_verify (cb_sequential_delimiters, _("LINE-SEQUENTIAL phrase"))) {
		current_file->organization = COB_ORG_LINE_SEQUENTIAL;
	}
  }
#line 12945 "parser.c" /* yacc.c:1646  */
    break;

  case 312:
#line 4735 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("RECORD DELIMITER %s only allowed with SEQUENTIAL files"),
			  "BINARY-SEQUENTIAL");
	}

	if (cb_verify (cb_record_delimiter, _("RECORD DELIMITER clause"))
	    && cb_verify (cb_sequential_delimiters, _("BINARY-SEQUENTIAL phrase"))) {
		current_file->organization = COB_ORG_SEQUENTIAL;
	}
  }
#line 12961 "parser.c" /* yacc.c:1646  */
    break;

  case 313:
#line 4747 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (current_file->organization != COB_ORG_SEQUENTIAL
	    && current_file->organization != COB_ORG_LINE_SEQUENTIAL) {
		cb_error (_("RECORD DELIMITER clause only allowed with (LINE) SEQUENTIAL files"));
	}

	if (cb_verify (cb_record_delimiter, _("RECORD DELIMITER clause"))) {
		cb_warning (warningopt,
			    _("Phrase in RECORD DELIMITER not recognized; will be ignored."));
	}
  }
#line 12977 "parser.c" /* yacc.c:1646  */
    break;

  case 314:
#line 4764 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree composite_key;

	check_repeated ("RECORD KEY", SYN_CLAUSE_9, &check_duplicate);
	current_file->key = (yyvsp[-2]);
	key_type = RECORD_KEY;

	/* handle split keys */
	if ((yyvsp[-1])) {
		/* generate field (in w-s) for composite-key */
		composite_key = cb_build_field ((yyvsp[-2]));
		if (composite_key == cb_error_node) {
			YYERROR;
		} else {
			composite_key->category = CB_CATEGORY_ALPHANUMERIC; 
			((struct cb_field *)composite_key)->count = 1;
			current_file->key = cb_build_field_reference ((struct cb_field *)composite_key, NULL);
			current_file->component_list = key_component_list;
		}
	}
	current_file->password = (yyvsp[0]);
  }
#line 13004 "parser.c" /* yacc.c:1646  */
    break;

  case 315:
#line 4790 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
  	(yyval) = NULL;
  }
#line 13012 "parser.c" /* yacc.c:1646  */
    break;

  case 316:
#line 4794 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
  	(yyval) = cb_int0;
  }
#line 13020 "parser.c" /* yacc.c:1646  */
    break;

  case 319:
#line 4805 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	key_component_list = NULL;
  }
#line 13028 "parser.c" /* yacc.c:1646  */
    break;

  case 322:
#line 4815 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	struct cb_key_component *c;
	struct cb_key_component *comp = cobc_malloc (sizeof(struct cb_key_component));
	comp->next = NULL;
	comp->component = (yyvsp[0]);
	if (key_component_list == NULL) {
		key_component_list = comp;
	} else {
		for (c = key_component_list; c->next != NULL; c = c->next);
		c->next = comp;
	}
  }
#line 13045 "parser.c" /* yacc.c:1646  */
    break;

  case 323:
#line 4833 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("RELATIVE KEY", SYN_CLAUSE_10, &check_duplicate);
	current_file->key = (yyvsp[0]);
	key_type = RELATIVE_KEY;
  }
#line 13055 "parser.c" /* yacc.c:1646  */
    break;

  case 324:
#line 4845 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("RESERVE", SYN_CLAUSE_11, &check_duplicate);
  }
#line 13063 "parser.c" /* yacc.c:1646  */
    break;

  case 327:
#line 4859 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SHARING", SYN_CLAUSE_12, &check_duplicate);
	current_file->sharing = (yyvsp[0]);
  }
#line 13072 "parser.c" /* yacc.c:1646  */
    break;

  case 328:
#line 4866 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 13078 "parser.c" /* yacc.c:1646  */
    break;

  case 329:
#line 4867 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_LOCK_OPEN_EXCLUSIVE); }
#line 13084 "parser.c" /* yacc.c:1646  */
    break;

  case 330:
#line 4868 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 13090 "parser.c" /* yacc.c:1646  */
    break;

  case 333:
#line 4877 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	yyerrok;
  }
#line 13098 "parser.c" /* yacc.c:1646  */
    break;

  case 338:
#line 4896 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree l;

	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_INPUT_OUTPUT_SECTION,
			       COBC_HD_I_O_CONTROL, 0);
	switch (CB_INTEGER ((yyvsp[-3]))->val) {
	case 0:
		/* SAME AREA */
		break;
	case 1:
		/* SAME RECORD */
		for (l = (yyvsp[0]); l; l = CB_CHAIN (l)) {
			if (CB_VALID_TREE (CB_VALUE (l))) {
				CB_FILE (cb_ref (CB_VALUE (l)))->same_clause = same_area;
			}
		}
		same_area++;
		break;
	case 2:
		/* SAME SORT-MERGE */
		break;
	}
  }
#line 13127 "parser.c" /* yacc.c:1646  */
    break;

  case 339:
#line 4923 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 13133 "parser.c" /* yacc.c:1646  */
    break;

  case 340:
#line 4924 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 13139 "parser.c" /* yacc.c:1646  */
    break;

  case 341:
#line 4925 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int2; }
#line 13145 "parser.c" /* yacc.c:1646  */
    break;

  case 342:
#line 4926 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int2; }
#line 13151 "parser.c" /* yacc.c:1646  */
    break;

  case 343:
#line 4933 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	/* Fake for TAPE */
	cobc_cs_check = CB_CS_ASSIGN;
  }
#line 13160 "parser.c" /* yacc.c:1646  */
    break;

  case 344:
#line 4938 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_INPUT_OUTPUT_SECTION,
			       COBC_HD_I_O_CONTROL, 0);
	cb_verify (cb_multiple_file_tape_clause, "MULTIPLE FILE TAPE");
	cobc_cs_check = 0;
  }
#line 13172 "parser.c" /* yacc.c:1646  */
    break;

  case 350:
#line 4967 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_storage = CB_STORAGE_WORKING;
  }
#line 13180 "parser.c" /* yacc.c:1646  */
    break;

  case 351:
#line 4976 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_validate_program_data (current_program);
  }
#line 13188 "parser.c" /* yacc.c:1646  */
    break;

  case 353:
#line 4983 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	header_check |= COBC_HD_DATA_DIVISION;
  }
#line 13196 "parser.c" /* yacc.c:1646  */
    break;

  case 355:
#line 4992 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_storage = CB_STORAGE_FILE;
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_FILE_SECTION;
  }
#line 13206 "parser.c" /* yacc.c:1646  */
    break;

  case 358:
#line 5006 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_VALID_TREE (current_file)) {
		if (CB_VALID_TREE ((yyvsp[0]))) {
			/* Do not keep Record if this is really a report */
			if (!current_file->reports) {
				finalize_file (current_file, CB_FIELD ((yyvsp[0])));
			}
		} else if (!current_file->reports) {
			cb_error (_("RECORD description missing or invalid"));
		}
	}
  }
#line 13223 "parser.c" /* yacc.c:1646  */
    break;

  case 359:
#line 5024 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_storage = CB_STORAGE_FILE;
	check_headers_present (COBC_HD_DATA_DIVISION,
			       COBC_HD_FILE_SECTION, 0, 0);
	check_duplicate = 0;
	if (CB_INVALID_TREE ((yyvsp[0]))) {
		YYERROR;
	}
	current_file = CB_FILE (cb_ref ((yyvsp[0])));
	if (CB_VALID_TREE (current_file)) {
		if ((yyvsp[-1])) {
			current_file->organization = COB_ORG_SORT;
		}
	}
  }
#line 13243 "parser.c" /* yacc.c:1646  */
    break;

  case 361:
#line 5041 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	yyerrok;
  }
#line 13251 "parser.c" /* yacc.c:1646  */
    break;

  case 362:
#line 5048 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 13259 "parser.c" /* yacc.c:1646  */
    break;

  case 363:
#line 5052 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 13267 "parser.c" /* yacc.c:1646  */
    break;

  case 366:
#line 5063 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("EXTERNAL", SYN_CLAUSE_1, &check_duplicate);
#if	0	/* RXWRXW - Global/External */
	if (current_file->flag_global) {
		cb_error (_("file cannot have both EXTERNAL and GLOBAL clauses"));
	}
#endif
	current_file->flag_external = 1;
  }
#line 13281 "parser.c" /* yacc.c:1646  */
    break;

  case 367:
#line 5073 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("GLOBAL", SYN_CLAUSE_2, &check_duplicate);
#if	0	/* RXWRXW - Global/External */
	if (current_file->flag_external) {
		cb_error (_("file cannot have both EXTERNAL and GLOBAL clauses"));
	}
#endif
	if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("%s is invalid in a user FUNCTION"), "GLOBAL");
	} else {
		current_file->flag_global = 1;
		current_program->flag_file_global = 1;
	}
  }
#line 13300 "parser.c" /* yacc.c:1646  */
    break;

  case 377:
#line 5103 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BLOCK", SYN_CLAUSE_3, &check_duplicate);
	/* ignore */
  }
#line 13309 "parser.c" /* yacc.c:1646  */
    break;

  case 381:
#line 5116 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("RECORD", SYN_CLAUSE_4, &check_duplicate);
	if (current_file->organization == COB_ORG_LINE_SEQUENTIAL) {
		cb_warning (warningopt, _("RECORD clause ignored for LINE SEQUENTIAL"));
	} else {
		current_file->record_max = cb_get_int ((yyvsp[-1]));
		if (current_file->record_max < 1)  {
			current_file->record_max = 1;
			cb_error (_("RECORD clause invalid"));
		}
		if (current_file->organization == COB_ORG_INDEXED) {
			if (current_file->record_max > MAX_FD_RECORD_IDX)  {
				current_file->record_max = MAX_FD_RECORD_IDX;
				cb_error (_("RECORD size (IDX) exceeds maximum allowed (%d)"),
					  MAX_FD_RECORD_IDX);
			}
		} else if (current_file->record_max > MAX_FD_RECORD)  {
			current_file->record_max = MAX_FD_RECORD;
			cb_error (_("RECORD size exceeds maximum allowed (%d)"),
				  MAX_FD_RECORD);
		}
	}
  }
#line 13337 "parser.c" /* yacc.c:1646  */
    break;

  case 382:
#line 5140 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	int	error_ind = 0;

	check_repeated ("RECORD", SYN_CLAUSE_4, &check_duplicate);
	if (current_file->organization == COB_ORG_LINE_SEQUENTIAL) {
		cb_warning (warningopt, _("RECORD clause ignored for LINE SEQUENTIAL"));
	} else {
		current_file->record_min = cb_get_int ((yyvsp[-3]));
		current_file->record_max = cb_get_int ((yyvsp[-1]));
		if (current_file->record_min < 0)  {
			current_file->record_min = 0;
			error_ind = 1;
		}
		if (current_file->record_max < 1)  {
			current_file->record_max = 1;
			error_ind = 1;
		}
		if (current_file->organization == COB_ORG_INDEXED) {
			if (current_file->record_max > MAX_FD_RECORD_IDX)  {
				current_file->record_max = MAX_FD_RECORD_IDX;
				cb_error (_("RECORD size (IDX) exceeds maximum allowed (%d)"),
					  MAX_FD_RECORD_IDX);
				error_ind = 1;
			}
		} else if (current_file->record_max > MAX_FD_RECORD)  {
			current_file->record_max = MAX_FD_RECORD;
			cb_error (_("RECORD size exceeds maximum allowed (%d)"),
				  MAX_FD_RECORD);
			error_ind = 1;
		}
		if (current_file->record_max <= current_file->record_min)  {
			error_ind = 1;
		}
		if (error_ind) {
			cb_error (_("RECORD clause invalid"));
		}
	}
  }
#line 13380 "parser.c" /* yacc.c:1646  */
    break;

  case 383:
#line 5180 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	int	error_ind = 0;

	check_repeated ("RECORD", SYN_CLAUSE_4, &check_duplicate);
	current_file->record_min = (yyvsp[-3]) ? cb_get_int ((yyvsp[-3])) : 0;
	current_file->record_max = (yyvsp[-2]) ? cb_get_int ((yyvsp[-2])) : 0;
	current_file->flag_check_record_varying_limits = 
		current_file->record_min == 0 || current_file->record_max == 0;
	if ((yyvsp[-3]) && current_file->record_min < 0)  {
		current_file->record_min = 0;
		error_ind = 1;
	}
	if ((yyvsp[-2]) && current_file->record_max < 1)  {
		current_file->record_max = 1;
		error_ind = 1;
	}
	if ((yyvsp[-2])) {
		if (current_file->organization == COB_ORG_INDEXED) {
			if (current_file->record_max > MAX_FD_RECORD_IDX)  {
				current_file->record_max = MAX_FD_RECORD_IDX;
				cb_error (_("RECORD size (IDX) exceeds maximum allowed (%d)"),
					  MAX_FD_RECORD_IDX);
				error_ind = 1;
			}
		} else if (current_file->record_max > MAX_FD_RECORD)  {
			current_file->record_max = MAX_FD_RECORD;
			cb_error (_("RECORD size exceeds maximum allowed (%d)"),
				  MAX_FD_RECORD);
			error_ind = 1;
		}
	}
	if (((yyvsp[-3]) || (yyvsp[-2])) && current_file->record_max <= current_file->record_min)  {
		error_ind = 1;
	}
	if (error_ind) {
		cb_error (_("RECORD clause invalid"));
	}
  }
#line 13423 "parser.c" /* yacc.c:1646  */
    break;

  case 385:
#line 5222 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_file->record_depending = (yyvsp[0]);
  }
#line 13431 "parser.c" /* yacc.c:1646  */
    break;

  case 386:
#line 5228 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 13437 "parser.c" /* yacc.c:1646  */
    break;

  case 387:
#line 5229 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 13443 "parser.c" /* yacc.c:1646  */
    break;

  case 388:
#line 5233 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 13449 "parser.c" /* yacc.c:1646  */
    break;

  case 389:
#line 5234 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 13455 "parser.c" /* yacc.c:1646  */
    break;

  case 390:
#line 5242 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LABEL", SYN_CLAUSE_5, &check_duplicate);
	cb_verify (cb_label_records_clause, "LABEL RECORDS");
  }
#line 13464 "parser.c" /* yacc.c:1646  */
    break;

  case 391:
#line 5253 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("VALUE OF", SYN_CLAUSE_6, &check_duplicate);
	cb_verify (cb_value_of_clause, "VALUE OF");
  }
#line 13473 "parser.c" /* yacc.c:1646  */
    break;

  case 392:
#line 5258 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("VALUE OF", SYN_CLAUSE_6, &check_duplicate);
	cb_verify (cb_value_of_clause, "VALUE OF");
	if (!current_file->assign) {
		current_file->assign = cb_build_assignment_name (current_file, (yyvsp[0]));
	}
  }
#line 13485 "parser.c" /* yacc.c:1646  */
    break;

  case 397:
#line 5281 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("DATA", SYN_CLAUSE_7, &check_duplicate);
	cb_verify (cb_data_records_clause, "DATA RECORDS");
  }
#line 13494 "parser.c" /* yacc.c:1646  */
    break;

  case 398:
#line 5293 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LINAGE", SYN_CLAUSE_8, &check_duplicate);
	if (current_file->organization != COB_ORG_LINE_SEQUENTIAL &&
	    current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("LINAGE clause with wrong file type"));
	} else {
		current_file->linage = (yyvsp[-2]);
		current_file->organization = COB_ORG_LINE_SEQUENTIAL;
		if (current_linage == 0) {
			linage_file = current_file;
		}
		current_linage++;
	}
  }
#line 13513 "parser.c" /* yacc.c:1646  */
    break;

  case 404:
#line 5321 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_file->latfoot = (yyvsp[0]);
  }
#line 13521 "parser.c" /* yacc.c:1646  */
    break;

  case 405:
#line 5328 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_file->lattop = (yyvsp[0]);
  }
#line 13529 "parser.c" /* yacc.c:1646  */
    break;

  case 406:
#line 5335 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_file->latbot = (yyvsp[0]);
  }
#line 13537 "parser.c" /* yacc.c:1646  */
    break;

  case 407:
#line 5344 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	check_repeated ("RECORDING", SYN_CLAUSE_9, &check_duplicate);
	/* ignore */
  }
#line 13547 "parser.c" /* yacc.c:1646  */
    break;

  case 412:
#line 5357 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("RECORDING MODE U or S can only be used with RECORD SEQUENTIAL files"));
	}
  }
#line 13557 "parser.c" /* yacc.c:1646  */
    break;

  case 415:
#line 5373 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	struct cb_alphabet_name	*al;

	check_repeated ("CODE SET", SYN_CLAUSE_10, &check_duplicate);

	if (CB_VALID_TREE ((yyvsp[-1]))) {
		al = CB_ALPHABET_NAME (cb_ref ((yyvsp[-1])));
		switch (al->alphabet_type) {
#ifdef	COB_EBCDIC_MACHINE
		case CB_ALPHABET_ASCII:
#else
		case CB_ALPHABET_EBCDIC:
#endif
		case CB_ALPHABET_CUSTOM:
			current_file->code_set = al;
			break;
		default:
			if (CB_VALID_TREE ((yyvsp[-1]))) {
				cb_warning_x (warningopt, (yyvsp[-1]), _("ignoring CODE-SET '%s'"),
						  cb_name ((yyvsp[-1])));
			}
			break;
		}
	}

	if (current_file->organization != COB_ORG_LINE_SEQUENTIAL &&
	    current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("CODE-SET clause invalid for file type"));
	}

	if (warningopt) {
		CB_PENDING ("CODE-SET");
	}
  }
#line 13596 "parser.c" /* yacc.c:1646  */
    break;

  case 417:
#line 5411 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	  if (warningopt) {
		  CB_PENDING ("FOR sub-records");
	  }

	  current_file->code_set_items = CB_LIST ((yyvsp[0]));
  }
#line 13608 "parser.c" /* yacc.c:1646  */
    break;

  case 418:
#line 5424 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("REPORT", SYN_CLAUSE_11, &check_duplicate);
	if (current_file->organization != COB_ORG_LINE_SEQUENTIAL &&
	    current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("REPORT clause with wrong file type"));
	} else {
		current_file->reports = (yyvsp[0]);
		current_file->organization = COB_ORG_LINE_SEQUENTIAL;
		current_file->flag_line_adv = 1;
	}
  }
#line 13624 "parser.c" /* yacc.c:1646  */
    break;

  case 421:
#line 5444 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_VALID_TREE ((yyvsp[0]))) {
		current_report = build_report ((yyvsp[0]));
		current_report->file = current_file;
		current_program->report_list =
			cb_list_add (current_program->report_list,
				     CB_TREE (current_report));
		if (report_count == 0) {
			report_instance = current_report;
		}
		report_count++;
	}
  }
#line 13642 "parser.c" /* yacc.c:1646  */
    break;

  case 422:
#line 5458 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_VALID_TREE ((yyvsp[0]))) {
		current_report = build_report ((yyvsp[0]));
		current_report->file = current_file;
		current_program->report_list =
			cb_list_add (current_program->report_list,
				     CB_TREE (current_report));
		if (report_count == 0) {
			report_instance = current_report;
		}
		report_count++;
	}
  }
#line 13660 "parser.c" /* yacc.c:1646  */
    break;

  case 424:
#line 5477 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_storage = CB_STORAGE_COMMUNICATION;
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_COMMUNICATION_SECTION;
	/* add a compiler configuration if either */
	if (cb_std_define != CB_STD_85
	 && cb_std_define != CB_STD_RM
	 && cb_std_define != CB_STD_GC
	 && !cb_relaxed_syntax_checks) {
		cb_verify (CB_UNCONFORMABLE, "COMMUNICATION SECTION");
	} else if (cb_verify (CB_OBSOLETE, "COMMUNICATION SECTION")) {
		CB_PENDING ("COMMUNICATION SECTION");
	}
  }
#line 13679 "parser.c" /* yacc.c:1646  */
    break;

  case 428:
#line 5501 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_VALID_TREE (current_cd)) {
		if (CB_VALID_TREE ((yyvsp[0]))) {
			cb_finalize_cd (current_cd, CB_FIELD ((yyvsp[0])));
		} else if (!current_cd->record) {
			cb_error (_("CD record missing"));
		}
	}
  }
#line 13693 "parser.c" /* yacc.c:1646  */
    break;

  case 429:
#line 5516 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	/* CD internally defines a new file */
	if (CB_VALID_TREE ((yyvsp[0]))) {
		current_cd = cb_build_cd ((yyvsp[0]));

		CB_ADD_TO_CHAIN (CB_TREE (current_cd),
				 current_program->cd_list);
	} else {
		current_cd = NULL;
		/* TO-DO: Is this necessary? */
		if (current_program->cd_list) {
			current_program->cd_list
				= CB_CHAIN (current_program->cd_list);
		}
	}
	check_duplicate = 0;
  }
#line 13715 "parser.c" /* yacc.c:1646  */
    break;

  case 477:
#line 5624 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_WORKING_STORAGE_SECTION;
	current_storage = CB_STORAGE_WORKING;
  }
#line 13725 "parser.c" /* yacc.c:1646  */
    break;

  case 478:
#line 5630 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		CB_FIELD_ADD (current_program->working_storage, CB_FIELD ((yyvsp[0])));
	}
  }
#line 13735 "parser.c" /* yacc.c:1646  */
    break;

  case 479:
#line 5639 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 13743 "parser.c" /* yacc.c:1646  */
    break;

  case 480:
#line 5643 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field = NULL;
	control_field = NULL;
	description_field = NULL;
	cb_clear_real_field ();
  }
#line 13754 "parser.c" /* yacc.c:1646  */
    break;

  case 481:
#line 5650 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	struct cb_field *p;

	for (p = description_field; p; p = p->sister) {
		cb_validate_field (p);
	}
	(yyval) = CB_TREE (description_field);
  }
#line 13767 "parser.c" /* yacc.c:1646  */
    break;

  case 487:
#line 5670 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (set_current_field ((yyvsp[-1]), (yyvsp[0]))) {
		YYERROR;
	}
    save_tree = NULL;
  }
#line 13778 "parser.c" /* yacc.c:1646  */
    break;

  case 488:
#line 5677 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (!qualifier) {
		current_field->flag_filler = 1;
	}
	if (!description_field) {
		description_field = current_field;
	}
  }
#line 13791 "parser.c" /* yacc.c:1646  */
    break;

  case 489:
#line 5686 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
#if 0 /* works fine without, leads to invalid free otherwise [COB_TREE_DEBUG] */
	/* Free tree associated with level number */
	cobc_parse_free ((yyvsp[-2]));
#endif
	yyerrok;
	cb_unput_dot ();
	check_pic_duplicate = 0;
	check_duplicate = 0;
#if 0 /* CHECKME - *Why* would we want to change the field here? */
	current_field = cb_get_real_field ();
#endif
  }
#line 13809 "parser.c" /* yacc.c:1646  */
    break;

  case 490:
#line 5703 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 13817 "parser.c" /* yacc.c:1646  */
    break;

  case 493:
#line 5715 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_filler ();
	qualifier = NULL;
	keys_list = NULL;
	non_const_word = 0;
  }
#line 13828 "parser.c" /* yacc.c:1646  */
    break;

  case 495:
#line 5726 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	qualifier = (yyvsp[0]);
	keys_list = NULL;
	non_const_word = 0;
  }
#line 13839 "parser.c" /* yacc.c:1646  */
    break;

  case 496:
#line 5736 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 13847 "parser.c" /* yacc.c:1646  */
    break;

  case 497:
#line 5740 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("%s is invalid in a user FUNCTION"), "GLOBAL");
		(yyval) = NULL;
	} else {
		(yyval) = cb_null;
	}
  }
#line 13860 "parser.c" /* yacc.c:1646  */
    break;

  case 498:
#line 5751 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 13866 "parser.c" /* yacc.c:1646  */
    break;

  case 499:
#line 5752 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_const_length ((yyvsp[0])); }
#line 13872 "parser.c" /* yacc.c:1646  */
    break;

  case 500:
#line 5753 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_const_length ((yyvsp[0])); }
#line 13878 "parser.c" /* yacc.c:1646  */
    break;

  case 501:
#line 5755 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_const_length ((yyvsp[0])); }
#line 13884 "parser.c" /* yacc.c:1646  */
    break;

  case 502:
#line 5760 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 13892 "parser.c" /* yacc.c:1646  */
    break;

  case 503:
#line 5764 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 13900 "parser.c" /* yacc.c:1646  */
    break;

  case 504:
#line 5768 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 13908 "parser.c" /* yacc.c:1646  */
    break;

  case 505:
#line 5772 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int2;
  }
#line 13916 "parser.c" /* yacc.c:1646  */
    break;

  case 506:
#line 5776 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int4;
  }
#line 13924 "parser.c" /* yacc.c:1646  */
    break;

  case 507:
#line 5780 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (8);
  }
#line 13932 "parser.c" /* yacc.c:1646  */
    break;

  case 508:
#line 5784 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int ((int)sizeof(long));
  }
#line 13940 "parser.c" /* yacc.c:1646  */
    break;

  case 509:
#line 5788 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int ((int)sizeof(void *));
  }
#line 13948 "parser.c" /* yacc.c:1646  */
    break;

  case 510:
#line 5792 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (cb_binary_comp_1) {
		(yyval) = cb_int2;
	} else {
		(yyval) = cb_int ((int)sizeof(float));
	}
  }
#line 13960 "parser.c" /* yacc.c:1646  */
    break;

  case 511:
#line 5800 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int ((int)sizeof(float));
  }
#line 13968 "parser.c" /* yacc.c:1646  */
    break;

  case 512:
#line 5804 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int ((int)sizeof(double));
  }
#line 13976 "parser.c" /* yacc.c:1646  */
    break;

  case 513:
#line 5808 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (4);
  }
#line 13984 "parser.c" /* yacc.c:1646  */
    break;

  case 514:
#line 5812 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (8);
  }
#line 13992 "parser.c" /* yacc.c:1646  */
    break;

  case 515:
#line 5816 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (16);
  }
#line 14000 "parser.c" /* yacc.c:1646  */
    break;

  case 516:
#line 5820 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	yyerrok;
	cb_unput_dot ();
	check_pic_duplicate = 0;
	check_duplicate = 0;
	current_field = cb_get_real_field ();
  }
#line 14012 "parser.c" /* yacc.c:1646  */
    break;

  case 526:
#line 5852 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (set_current_field ((yyvsp[-4]), (yyvsp[-3]))) {
		YYERROR;
	}

	if (cb_ref ((yyvsp[-1])) != cb_error_node) {
		error_if_invalid_level_for_renames ((yyvsp[-1]));
		current_field->redefines = CB_FIELD (cb_ref ((yyvsp[-1])));
	}

	if ((yyvsp[0])) {
		error_if_invalid_level_for_renames ((yyvsp[0]));
		current_field->rename_thru = CB_FIELD (cb_ref ((yyvsp[0])));
	} else {
		/* If there is no THRU clause, RENAMES acts like REDEFINES. */
		current_field->pic = current_field->redefines->pic;
	}

	cb_validate_renames_item (current_field);
  }
#line 14037 "parser.c" /* yacc.c:1646  */
    break;

  case 527:
#line 5876 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 14045 "parser.c" /* yacc.c:1646  */
    break;

  case 528:
#line 5880 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]) == cb_error_node ? NULL : (yyvsp[0]);
  }
#line 14053 "parser.c" /* yacc.c:1646  */
    break;

  case 529:
#line 5887 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (set_current_field ((yyvsp[-1]), (yyvsp[0]))) {
		YYERROR;
	}
  }
#line 14063 "parser.c" /* yacc.c:1646  */
    break;

  case 530:
#line 5893 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_validate_88_item (current_field);
  }
#line 14071 "parser.c" /* yacc.c:1646  */
    break;

  case 531:
#line 5900 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree x;
	int	level;

	cobc_cs_check = 0;
	level = cb_get_level ((yyvsp[-4]));
	/* Free tree associated with level number */
	cobc_parse_free ((yyvsp[-4]));
	if (level != 1) {
		cb_error (_("CONSTANT item not at 01 level"));
	} else if ((yyvsp[0])) {
		if (cb_verify(cb_constant_01, "01 CONSTANT")) {
			x = cb_build_constant ((yyvsp[-3]), (yyvsp[0]));
			CB_FIELD (x)->flag_item_78 = 1;
			CB_FIELD (x)->flag_constant = 1;
			CB_FIELD (x)->level = 1;
			CB_FIELD (x)->values = (yyvsp[0]);
			cb_needs_01 = 1;
			if ((yyvsp[-1])) {
				CB_FIELD (x)->flag_is_global = 1;
			}
			/* Ignore return value */
			(void)cb_validate_78_item (CB_FIELD (x), 0);
		}
	}
  }
#line 14102 "parser.c" /* yacc.c:1646  */
    break;

  case 532:
#line 5927 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (set_current_field ((yyvsp[-1]), (yyvsp[0]))) {
		YYERROR;
	}
  }
#line 14112 "parser.c" /* yacc.c:1646  */
    break;

  case 533:
#line 5934 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	/* Reset to last non-78 item */
	current_field = cb_validate_78_item (current_field, 0);
  }
#line 14121 "parser.c" /* yacc.c:1646  */
    break;

  case 534:
#line 5942 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 14129 "parser.c" /* yacc.c:1646  */
    break;

  case 535:
#line 5946 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT(cb_build_const_from ((yyvsp[0])));
  }
#line 14137 "parser.c" /* yacc.c:1646  */
    break;

  case 536:
#line 5953 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_VALID_TREE (current_field)) {
		current_field->values = (yyvsp[0]);
	}
  }
#line 14147 "parser.c" /* yacc.c:1646  */
    break;

  case 537:
#line 5959 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->values = CB_LIST_INIT (cb_build_const_start (current_field, (yyvsp[0])));
  }
#line 14155 "parser.c" /* yacc.c:1646  */
    break;

  case 538:
#line 5963 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->values = CB_LIST_INIT (cb_build_const_next (current_field));
  }
#line 14163 "parser.c" /* yacc.c:1646  */
    break;

  case 539:
#line 5969 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 14169 "parser.c" /* yacc.c:1646  */
    break;

  case 540:
#line 5970 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 14175 "parser.c" /* yacc.c:1646  */
    break;

  case 541:
#line 5974 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 14181 "parser.c" /* yacc.c:1646  */
    break;

  case 542:
#line 5975 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal ("(", 1); }
#line 14187 "parser.c" /* yacc.c:1646  */
    break;

  case 543:
#line 5976 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal (")", 1); }
#line 14193 "parser.c" /* yacc.c:1646  */
    break;

  case 544:
#line 5977 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal ("+", 1); }
#line 14199 "parser.c" /* yacc.c:1646  */
    break;

  case 545:
#line 5978 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal ("-", 1); }
#line 14205 "parser.c" /* yacc.c:1646  */
    break;

  case 546:
#line 5979 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal ("*", 1); }
#line 14211 "parser.c" /* yacc.c:1646  */
    break;

  case 547:
#line 5980 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal ("/", 1); }
#line 14217 "parser.c" /* yacc.c:1646  */
    break;

  case 548:
#line 5981 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal ("&", 1); }
#line 14223 "parser.c" /* yacc.c:1646  */
    break;

  case 549:
#line 5982 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal ("|", 1); }
#line 14229 "parser.c" /* yacc.c:1646  */
    break;

  case 550:
#line 5983 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_alphanumeric_literal ("^", 1); }
#line 14235 "parser.c" /* yacc.c:1646  */
    break;

  case 553:
#line 5993 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	save_tree = cb_int0;
  }
#line 14243 "parser.c" /* yacc.c:1646  */
    break;

  case 570:
#line 6022 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("REDEFINES", SYN_CLAUSE_1, &check_pic_duplicate);
	if (save_tree != NULL) {
		cb_verify_x ((yyvsp[0]), cb_free_redefines_position,
			     _("REDEFINES clause not following entry-name"));
	}

	current_field->redefines = cb_resolve_redefines (current_field, (yyvsp[0]));
	if (current_field->redefines == NULL) {
		current_field->flag_is_verified = 1;
		current_field->flag_invalid = 1;
		YYERROR;
	}
  }
#line 14262 "parser.c" /* yacc.c:1646  */
    break;

  case 571:
#line 6043 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("EXTERNAL", SYN_CLAUSE_2, &check_pic_duplicate);
	if (current_storage != CB_STORAGE_WORKING) {
		cb_error (_("%s not allowed here"), "EXTERNAL");
	} else if (current_field->level != 1 && current_field->level != 77) {
		cb_error (_("%s only allowed at 01/77 level"), "EXTERNAL");
	} else if (!qualifier) {
		cb_error (_("%s requires a data name"), "EXTERNAL");
#if	0	/* RXWRXW - Global/External */
	} else if (current_field->flag_is_global) {
		cb_error (_("%s and %s are mutually exclusive"), "GLOBAL", "EXTERNAL");
#endif
	} else if (current_field->flag_item_based) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "EXTERNAL");
	} else if (current_field->redefines) {
		cb_error (_("%s and %s are mutually exclusive"), "EXTERNAL", "REDEFINES");
	} else if (current_field->flag_occurs) {
		cb_error (_("%s and %s are mutually exclusive"), "EXTERNAL", "OCCURS");
	} else {
		current_field->flag_external = 1;
		current_program->flag_has_external = 1;
	}
  }
#line 14290 "parser.c" /* yacc.c:1646  */
    break;

  case 572:
#line 6070 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->ename = cb_to_cname (current_field->name);
  }
#line 14298 "parser.c" /* yacc.c:1646  */
    break;

  case 573:
#line 6074 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->ename = cb_to_cname ((const char *)CB_LITERAL ((yyvsp[0]))->data);
  }
#line 14306 "parser.c" /* yacc.c:1646  */
    break;

  case 576:
#line 6087 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("GLOBAL", SYN_CLAUSE_3, &check_pic_duplicate);
	if (current_field->level != 1 && current_field->level != 77) {
		cb_error (_("%s only allowed at 01/77 level"), "GLOBAL");
	} else if (!qualifier) {
		cb_error (_("%s requires a data name"), "GLOBAL");
#if	0	/* RXWRXW - Global/External */
	} else if (current_field->flag_external) {
		cb_error (_("%s and %s are mutually exclusive"), "GLOBAL", "EXTERNAL");
#endif
	} else if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("%s is invalid in a user FUNCTION"), "GLOBAL");
	} else if (current_storage == CB_STORAGE_LOCAL) {
		cb_error (_("%s not allowed here"), "GLOBAL");
	} else {
		current_field->flag_is_global = 1;
	}
  }
#line 14329 "parser.c" /* yacc.c:1646  */
    break;

  case 577:
#line 6112 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PICTURE", SYN_CLAUSE_4, &check_pic_duplicate);
	current_field->pic = CB_PICTURE ((yyvsp[0]));
  }
#line 14338 "parser.c" /* yacc.c:1646  */
    break;

  case 580:
#line 6125 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_ERROR);
  }
#line 14346 "parser.c" /* yacc.c:1646  */
    break;

  case 581:
#line 6132 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_BINARY);
  }
#line 14354 "parser.c" /* yacc.c:1646  */
    break;

  case 582:
#line 6136 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_BIT);
	CB_PENDING ("USAGE BIT");
  }
#line 14363 "parser.c" /* yacc.c:1646  */
    break;

  case 583:
#line 6141 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_BINARY);
  }
#line 14371 "parser.c" /* yacc.c:1646  */
    break;

  case 584:
#line 6145 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->flag_comp_1 = 1;
	if (cb_binary_comp_1) {
		check_and_set_usage (CB_USAGE_SIGNED_SHORT);
	} else {
		check_and_set_usage (CB_USAGE_FLOAT);
	}
  }
#line 14384 "parser.c" /* yacc.c:1646  */
    break;

  case 585:
#line 6154 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_DOUBLE);
  }
#line 14392 "parser.c" /* yacc.c:1646  */
    break;

  case 586:
#line 6158 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_PACKED);
  }
#line 14400 "parser.c" /* yacc.c:1646  */
    break;

  case 587:
#line 6162 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_BINARY);
  }
#line 14408 "parser.c" /* yacc.c:1646  */
    break;

  case 588:
#line 6166 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_COMP_5);
  }
#line 14416 "parser.c" /* yacc.c:1646  */
    break;

  case 589:
#line 6170 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_COMP_6);
  }
#line 14424 "parser.c" /* yacc.c:1646  */
    break;

  case 590:
#line 6174 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_COMP_X);
  }
#line 14432 "parser.c" /* yacc.c:1646  */
    break;

  case 591:
#line 6178 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_FLOAT);
  }
#line 14440 "parser.c" /* yacc.c:1646  */
    break;

  case 592:
#line 6182 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_DISPLAY);
  }
#line 14448 "parser.c" /* yacc.c:1646  */
    break;

  case 593:
#line 6186 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_INDEX);
  }
#line 14456 "parser.c" /* yacc.c:1646  */
    break;

  case 594:
#line 6190 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_PACKED);
  }
#line 14464 "parser.c" /* yacc.c:1646  */
    break;

  case 595:
#line 6194 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_POINTER);
	current_field->flag_is_pointer = 1;
  }
#line 14473 "parser.c" /* yacc.c:1646  */
    break;

  case 596:
#line 6199 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_PROGRAM_POINTER);
	current_field->flag_is_pointer = 1;
  }
#line 14482 "parser.c" /* yacc.c:1646  */
    break;

  case 597:
#line 6204 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL);
  }
#line 14490 "parser.c" /* yacc.c:1646  */
    break;

  case 598:
#line 6208 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL_WINDOW);
  }
#line 14498 "parser.c" /* yacc.c:1646  */
    break;

  case 599:
#line 6212 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL_SUBWINDOW);
  }
#line 14506 "parser.c" /* yacc.c:1646  */
    break;

  case 600:
#line 6216 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL_FONT);
	CB_PENDING ("HANDLE OF FONT");
  }
#line 14515 "parser.c" /* yacc.c:1646  */
    break;

  case 601:
#line 6221 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL_THREAD);
  }
#line 14523 "parser.c" /* yacc.c:1646  */
    break;

  case 602:
#line 6225 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL_MENU);
	CB_PENDING ("HANDLE OF MENU");
  }
#line 14532 "parser.c" /* yacc.c:1646  */
    break;

  case 603:
#line 6230 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL_VARIANT);
  }
#line 14540 "parser.c" /* yacc.c:1646  */
    break;

  case 604:
#line 6234 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_HNDL_LM);
	CB_PENDING ("HANDLE OF LAYOUT-MANAGER");
  }
#line 14549 "parser.c" /* yacc.c:1646  */
    break;

  case 605:
#line 6239 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_SIGNED_SHORT);
  }
#line 14557 "parser.c" /* yacc.c:1646  */
    break;

  case 606:
#line 6243 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_SIGNED_INT);
  }
#line 14565 "parser.c" /* yacc.c:1646  */
    break;

  case 607:
#line 6247 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
#ifdef COB_32_BIT_LONG
	check_and_set_usage (CB_USAGE_SIGNED_INT);
#else
	check_and_set_usage (CB_USAGE_SIGNED_LONG);
#endif
  }
#line 14577 "parser.c" /* yacc.c:1646  */
    break;

  case 608:
#line 6255 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_UNSIGNED_SHORT);
  }
#line 14585 "parser.c" /* yacc.c:1646  */
    break;

  case 609:
#line 6259 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_UNSIGNED_INT);
  }
#line 14593 "parser.c" /* yacc.c:1646  */
    break;

  case 610:
#line 6263 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
#ifdef COB_32_BIT_LONG
	check_and_set_usage (CB_USAGE_UNSIGNED_INT);
#else
	check_and_set_usage (CB_USAGE_UNSIGNED_LONG);
#endif
  }
#line 14605 "parser.c" /* yacc.c:1646  */
    break;

  case 611:
#line 6271 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_SIGNED_CHAR);
  }
#line 14613 "parser.c" /* yacc.c:1646  */
    break;

  case 612:
#line 6275 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_UNSIGNED_CHAR);
  }
#line 14621 "parser.c" /* yacc.c:1646  */
    break;

  case 613:
#line 6279 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_SIGNED_SHORT);
  }
#line 14629 "parser.c" /* yacc.c:1646  */
    break;

  case 614:
#line 6283 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_UNSIGNED_SHORT);
  }
#line 14637 "parser.c" /* yacc.c:1646  */
    break;

  case 615:
#line 6287 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_SIGNED_INT);
  }
#line 14645 "parser.c" /* yacc.c:1646  */
    break;

  case 616:
#line 6291 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_UNSIGNED_INT);
  }
#line 14653 "parser.c" /* yacc.c:1646  */
    break;

  case 617:
#line 6295 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_SIGNED_LONG);
  }
#line 14661 "parser.c" /* yacc.c:1646  */
    break;

  case 618:
#line 6299 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_UNSIGNED_LONG);
  }
#line 14669 "parser.c" /* yacc.c:1646  */
    break;

  case 619:
#line 6303 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
#ifdef COB_32_BIT_LONG
	check_and_set_usage (CB_USAGE_SIGNED_INT);
#else
	check_and_set_usage (CB_USAGE_SIGNED_LONG);
#endif
  }
#line 14681 "parser.c" /* yacc.c:1646  */
    break;

  case 620:
#line 6311 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
#ifdef COB_32_BIT_LONG
	check_and_set_usage (CB_USAGE_UNSIGNED_INT);
#else
	check_and_set_usage (CB_USAGE_UNSIGNED_LONG);
#endif
  }
#line 14693 "parser.c" /* yacc.c:1646  */
    break;

  case 621:
#line 6319 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_FP_BIN32);
  }
#line 14701 "parser.c" /* yacc.c:1646  */
    break;

  case 622:
#line 6323 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_FP_BIN64);
  }
#line 14709 "parser.c" /* yacc.c:1646  */
    break;

  case 623:
#line 6327 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_FP_BIN128);
  }
#line 14717 "parser.c" /* yacc.c:1646  */
    break;

  case 624:
#line 6331 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_FP_DEC64);
  }
#line 14725 "parser.c" /* yacc.c:1646  */
    break;

  case 625:
#line 6335 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_and_set_usage (CB_USAGE_FP_DEC128);
  }
#line 14733 "parser.c" /* yacc.c:1646  */
    break;

  case 626:
#line 6339 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("USAGE", SYN_CLAUSE_5, &check_pic_duplicate);
	CB_UNFINISHED ("USAGE NATIONAL");
  }
#line 14742 "parser.c" /* yacc.c:1646  */
    break;

  case 638:
#line 6369 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SIGN", SYN_CLAUSE_6, &check_pic_duplicate);
	current_field->flag_sign_clause = 1;
	current_field->flag_sign_separate = ((yyvsp[0]) ? 1 : 0);
	current_field->flag_sign_leading  = 1;
  }
#line 14753 "parser.c" /* yacc.c:1646  */
    break;

  case 639:
#line 6376 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SIGN", SYN_CLAUSE_6, &check_pic_duplicate);
	current_field->flag_sign_clause = 1;
	current_field->flag_sign_separate = ((yyvsp[0]) ? 1 : 0);
	current_field->flag_sign_leading  = 0;
  }
#line 14764 "parser.c" /* yacc.c:1646  */
    break;

  case 640:
#line 6390 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	/* most of the field attributes are set when parsing the phrases */;
	setup_occurs ();
	setup_occurs_min_max ((yyvsp[-4]), (yyvsp[-3]));
  }
#line 14774 "parser.c" /* yacc.c:1646  */
    break;

  case 642:
#line 6399 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->step_count = cb_get_int ((yyvsp[0]));
  }
#line 14782 "parser.c" /* yacc.c:1646  */
    break;

  case 643:
#line 6409 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	/* most of the field attributes are set when parsing the phrases */;
	setup_occurs ();
	setup_occurs_min_max ((yyvsp[-4]), (yyvsp[-3]));
  }
#line 14792 "parser.c" /* yacc.c:1646  */
    break;

  case 644:
#line 6416 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->flag_unbounded = 1;
	if (current_field->parent) {
		current_field->parent->flag_unbounded = 1;
	}
	current_field->depending = (yyvsp[-1]);
	/* most of the field attributes are set when parsing the phrases */;
	setup_occurs ();
	setup_occurs_min_max ((yyvsp[-6]), cb_int0);
  }
#line 14807 "parser.c" /* yacc.c:1646  */
    break;

  case 645:
#line 6428 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	setup_occurs ();
	current_field->occurs_min = (yyvsp[-3]) ? cb_get_int ((yyvsp[-3])) : 0;
	if ((yyvsp[-2])) {
		current_field->occurs_max = cb_get_int ((yyvsp[-2]));
		if (current_field->occurs_max <= current_field->occurs_min) {
			cb_error (_("OCCURS TO must be greater than OCCURS FROM"));
		}
	} else {
		current_field->occurs_max = 0;
	}
	CB_PENDING("OCCURS DYNAMIC");
  }
#line 14825 "parser.c" /* yacc.c:1646  */
    break;

  case 646:
#line 6444 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 14831 "parser.c" /* yacc.c:1646  */
    break;

  case 647:
#line 6445 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 14837 "parser.c" /* yacc.c:1646  */
    break;

  case 648:
#line 6449 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 14843 "parser.c" /* yacc.c:1646  */
    break;

  case 649:
#line 6450 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 14849 "parser.c" /* yacc.c:1646  */
    break;

  case 650:
#line 6454 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 14855 "parser.c" /* yacc.c:1646  */
    break;

  case 651:
#line 6455 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[-1]); }
#line 14861 "parser.c" /* yacc.c:1646  */
    break;

  case 653:
#line 6460 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->depending = (yyvsp[0]);
  }
#line 14869 "parser.c" /* yacc.c:1646  */
    break;

  case 655:
#line 6466 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_index ((yyvsp[0]), cb_zero, 0, current_field);
	CB_FIELD_PTR ((yyval))->index_type = CB_STATIC_INT_INDEX;
  }
#line 14878 "parser.c" /* yacc.c:1646  */
    break;

  case 657:
#line 6474 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	/* current_field->initialized = 1; */
  }
#line 14886 "parser.c" /* yacc.c:1646  */
    break;

  case 660:
#line 6483 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (!cb_relaxed_syntax_checks) {
		cb_error (_("INDEXED should follow ASCENDING/DESCENDING"));
	} else {
		cb_warning (warningopt, _("INDEXED should follow ASCENDING/DESCENDING"));
	}
  }
#line 14898 "parser.c" /* yacc.c:1646  */
    break;

  case 664:
#line 6497 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_tree		l;
		struct cb_key	*keys;
		int		i;
		int		nkeys;

		l = (yyvsp[0]);
		nkeys = cb_list_length ((yyvsp[0]));
		keys = cobc_parse_malloc (sizeof (struct cb_key) * nkeys);

		for (i = 0; i < nkeys; i++) {
			keys[i].dir = CB_PURPOSE_INT (l);
			keys[i].key = CB_VALUE (l);
			l = CB_CHAIN (l);
		}
		current_field->keys = keys;
		current_field->nkeys = nkeys;
	}
  }
#line 14923 "parser.c" /* yacc.c:1646  */
    break;

  case 667:
#line 6526 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree l;

	for (l = (yyvsp[0]); l; l = CB_CHAIN (l)) {
		CB_PURPOSE (l) = (yyvsp[-3]);
		if (qualifier && !CB_REFERENCE(CB_VALUE(l))->chain &&
		    strcasecmp (CB_NAME(CB_VALUE(l)), CB_NAME(qualifier))) {
			CB_REFERENCE(CB_VALUE(l))->chain = qualifier;
		}
	}
	keys_list = cb_list_append (keys_list, (yyvsp[0]));
	(yyval) = keys_list;
  }
#line 14941 "parser.c" /* yacc.c:1646  */
    break;

  case 668:
#line 6542 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_ASCENDING); }
#line 14947 "parser.c" /* yacc.c:1646  */
    break;

  case 669:
#line 6543 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_DESCENDING); }
#line 14953 "parser.c" /* yacc.c:1646  */
    break;

  case 672:
#line 6552 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->index_list = (yyvsp[0]);
  }
#line 14961 "parser.c" /* yacc.c:1646  */
    break;

  case 673:
#line 6558 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 14967 "parser.c" /* yacc.c:1646  */
    break;

  case 674:
#line 6560 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 14973 "parser.c" /* yacc.c:1646  */
    break;

  case 675:
#line 6565 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_index ((yyvsp[0]), cb_int1, 1U, current_field);
	CB_FIELD_PTR ((yyval))->index_type = CB_STATIC_INT_INDEX;
  }
#line 14982 "parser.c" /* yacc.c:1646  */
    break;

  case 676:
#line 6576 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("JUSTIFIED", SYN_CLAUSE_8, &check_pic_duplicate);
	current_field->flag_justified = 1;
  }
#line 14991 "parser.c" /* yacc.c:1646  */
    break;

  case 677:
#line 6587 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SYNCHRONIZED", SYN_CLAUSE_9, &check_pic_duplicate);
	current_field->flag_synchronized = 1;
  }
#line 15000 "parser.c" /* yacc.c:1646  */
    break;

  case 680:
#line 6597 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("SYNCHRONIZED RIGHT");
  }
#line 15008 "parser.c" /* yacc.c:1646  */
    break;

  case 681:
#line 6607 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BLANK", SYN_CLAUSE_10, &check_pic_duplicate);
	current_field->flag_blank_zero = 1;
  }
#line 15017 "parser.c" /* yacc.c:1646  */
    break;

  case 682:
#line 6618 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BASED", SYN_CLAUSE_11, &check_pic_duplicate);
	if (current_storage != CB_STORAGE_WORKING &&
	    current_storage != CB_STORAGE_LINKAGE &&
	    current_storage != CB_STORAGE_LOCAL) {
		cb_error (_("%s not allowed here"), "BASED");
	} else if (current_field->level != 1 && current_field->level != 77) {
		cb_error (_("%s only allowed at 01/77 level"), "BASED");
	} else if (!qualifier) {
		cb_error (_("%s requires a data name"), "BASED");
	} else if (current_field->flag_external) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "EXTERNAL");
	} else if (current_field->redefines) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "REDEFINES");
	} else if (current_field->flag_any_length) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "ANY LENGTH");
	} else if (current_field->flag_occurs) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "OCCURS");
	} else {
		current_field->flag_item_based = 1;
	}
  }
#line 15044 "parser.c" /* yacc.c:1646  */
    break;

  case 683:
#line 6646 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("VALUE", SYN_CLAUSE_12, &check_pic_duplicate);
	current_field->values = (yyvsp[0]);
  }
#line 15053 "parser.c" /* yacc.c:1646  */
    break;

  case 685:
#line 6654 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 15059 "parser.c" /* yacc.c:1646  */
    break;

  case 686:
#line 6655 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 15065 "parser.c" /* yacc.c:1646  */
    break;

  case 687:
#line 6659 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = CB_BUILD_PAIR ((yyvsp[-2]), (yyvsp[0])); }
#line 15071 "parser.c" /* yacc.c:1646  */
    break;

  case 690:
#line 6666 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (current_field->level != 88) {
		cb_error (_("FALSE clause only allowed for 88 level"));
	}
	current_field->false_88 = CB_LIST_INIT ((yyvsp[0]));
  }
#line 15082 "parser.c" /* yacc.c:1646  */
    break;

  case 691:
#line 6678 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ANY", SYN_CLAUSE_14, &check_pic_duplicate);
	if (current_field->flag_item_based) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "ANY LENGTH");
	} else {
		current_field->flag_any_length = 1;
	}
  }
#line 15095 "parser.c" /* yacc.c:1646  */
    break;

  case 692:
#line 6687 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ANY", SYN_CLAUSE_14, &check_pic_duplicate);
	if (current_field->flag_item_based) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "ANY NUMERIC");
	} else {
		current_field->flag_any_length = 1;
		current_field->flag_any_numeric = 1;
	}
  }
#line 15109 "parser.c" /* yacc.c:1646  */
    break;

  case 693:
#line 6702 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("EXTERNAL-FORM", SYN_CLAUSE_2, &check_pic_duplicate);
	CB_PENDING("EXTERNAL-FORM");
	if (current_storage != CB_STORAGE_WORKING) {
		cb_error (_("%s not allowed here"), "EXTERNAL-FORM");
	} else if (current_field->level != 1) {	/* docs say: at group level */
		cb_error (_("%s only allowed at 01 level"), "EXTERNAL-FORM");
	} else if (!qualifier) {
		cb_error (_("%s requires a data name"), "EXTERNAL-FORM");
	} else if (current_field->redefines) {
		cb_error (_("%s and %s combination not allowed"), "EXTERNAL-FORM", "REDEFINES");
	} else {
		current_field->flag_is_external_form = 1;
	}
  }
#line 15129 "parser.c" /* yacc.c:1646  */
    break;

  case 694:
#line 6725 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("IDENTIFIED BY", SYN_CLAUSE_3, &check_pic_duplicate);
	if (!current_field->flag_is_external_form) {
		CB_PENDING("EXTERNAL-FORM (IDENTIFIED BY)");
		if (current_storage != CB_STORAGE_WORKING) {
			cb_error (_("%s not allowed here"), "IDENTIFIED BY");
		} else if (!qualifier) {
			cb_error (_("%s requires a data name"), "IDENTIFIED BY");
		} else if (current_field->redefines) {
			cb_error (_("%s and %s combination not allowed"), "IDENTIFIED BY", "REDEFINES");
		}
	}
	current_field->external_form_identifier = (yyvsp[0]);
  }
#line 15148 "parser.c" /* yacc.c:1646  */
    break;

  case 696:
#line 6745 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_LOCAL_STORAGE_SECTION;
	current_storage = CB_STORAGE_LOCAL;
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "LOCAL-STORAGE");
	}
  }
#line 15161 "parser.c" /* yacc.c:1646  */
    break;

  case 697:
#line 6754 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		current_program->local_storage = CB_FIELD ((yyvsp[0]));
	}
  }
#line 15171 "parser.c" /* yacc.c:1646  */
    break;

  case 699:
#line 6766 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_LINKAGE_SECTION;
	current_storage = CB_STORAGE_LINKAGE;
  }
#line 15181 "parser.c" /* yacc.c:1646  */
    break;

  case 700:
#line 6772 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		current_program->linkage_storage = CB_FIELD ((yyvsp[0]));
	}
  }
#line 15191 "parser.c" /* yacc.c:1646  */
    break;

  case 702:
#line 6783 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	header_check |= COBC_HD_REPORT_SECTION;
	current_storage = CB_STORAGE_REPORT;
	description_field = NULL;
	current_program->flag_report = 1;
	cb_clear_real_field ();
  }
#line 15203 "parser.c" /* yacc.c:1646  */
    break;

  case 706:
#line 6801 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_INVALID_TREE ((yyvsp[0]))) {
		YYERROR;
	} else {
		current_field = NULL;
		control_field = NULL;
		description_field = NULL;
		current_report = CB_REPORT_PTR ((yyvsp[0]));
	}
	check_duplicate = 0;
  }
#line 15219 "parser.c" /* yacc.c:1646  */
    break;

  case 707:
#line 6814 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	struct cb_field *p;

	for (p = description_field; p; p = p->sister) {
		cb_validate_field (p);
	}
	current_program->report_storage = description_field;
	current_program->flag_report = 1;
	if (current_report->records == NULL) {
		current_report->records = description_field;
	}
	finalize_report (current_report, description_field);
	(yyval) = CB_TREE (description_field);
  }
#line 15238 "parser.c" /* yacc.c:1646  */
    break;

  case 710:
#line 6833 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	yyerrok;
  }
#line 15246 "parser.c" /* yacc.c:1646  */
    break;

  case 711:
#line 6840 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("GLOBAL", SYN_CLAUSE_1, &check_duplicate);
	current_report->global = 1;
	cb_error (_("GLOBAL is not allowed with RD"));
  }
#line 15256 "parser.c" /* yacc.c:1646  */
    break;

  case 712:
#line 6846 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("CODE", SYN_CLAUSE_2, &check_duplicate);
	current_report->code_clause = (yyvsp[0]);
  }
#line 15265 "parser.c" /* yacc.c:1646  */
    break;

  case 715:
#line 6858 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("CONTROL", SYN_CLAUSE_3, &check_duplicate);
  }
#line 15273 "parser.c" /* yacc.c:1646  */
    break;

  case 719:
#line 6871 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_report->control_final = 1;
  }
#line 15281 "parser.c" /* yacc.c:1646  */
    break;

  case 722:
#line 6883 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	/* Add field to current control list */
	CB_ADD_TO_CHAIN ((yyvsp[0]), current_report->controls);
  }
#line 15290 "parser.c" /* yacc.c:1646  */
    break;

  case 723:
#line 6894 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PAGE", SYN_CLAUSE_4, &check_duplicate);
	if (!current_report->heading) {
		current_report->heading = 1;
	}
	if (!current_report->first_detail) {
		current_report->first_detail = current_report->heading;
	}
	if (!current_report->last_control) {
		if (current_report->last_detail) {
			current_report->last_control = current_report->last_detail;
		} else if (current_report->footing) {
			current_report->last_control = current_report->footing;
		} else {
			current_report->last_control = current_report->lines;
		}
		if (current_report->t_last_detail) {
			current_report->t_last_control = current_report->t_last_detail;
		} else if (current_report->t_footing) {
			current_report->t_last_control = current_report->t_footing;
		} else if(current_report->t_lines) {
			current_report->t_last_control = current_report->t_lines;
		}
	}
	if (!current_report->last_detail && !current_report->footing) {
		current_report->last_detail = current_report->lines;
		current_report->footing = current_report->lines;
	} else if (!current_report->last_detail) {
		current_report->last_detail = current_report->footing;
	} else if (!current_report->footing) {
		current_report->footing = current_report->last_detail;
	}
	/* PAGE LIMIT values checked in finalize_report in typeck.c */
  }
#line 15329 "parser.c" /* yacc.c:1646  */
    break;

  case 724:
#line 6932 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_LITERAL_P ((yyvsp[-1]))) {
		current_report->lines = cb_get_int ((yyvsp[-1]));
		if (current_report->lines > 999) {
			cb_error ("PAGE LIMIT lines > 999");
		}
	} else {
		current_report->t_lines = (yyvsp[-1]);
	}
  }
#line 15344 "parser.c" /* yacc.c:1646  */
    break;

  case 726:
#line 6944 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_LITERAL_P ((yyvsp[-2]))) {
		current_report->lines = cb_get_int ((yyvsp[-2]));
		if (current_report->lines > 999) {
			cb_error ("PAGE LIMIT lines > 999");
		}
	} else {
		current_report->t_lines = (yyvsp[-2]);
	}
  }
#line 15359 "parser.c" /* yacc.c:1646  */
    break;

  case 727:
#line 6958 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	/* may be repeated later by page detail */
	check_repeated ("LINE LIMIT", SYN_CLAUSE_5, &check_duplicate);
	if (CB_LITERAL_P ((yyvsp[-1]))) {
		current_report->columns = cb_get_int ((yyvsp[-1]));
	} else {
		current_report->t_columns = (yyvsp[-1]);
	}
  }
#line 15373 "parser.c" /* yacc.c:1646  */
    break;

  case 737:
#line 6986 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LINE LIMIT", SYN_CLAUSE_5, &check_duplicate);
	if (CB_LITERAL_P ((yyvsp[0]))) {
		current_report->columns = cb_get_int ((yyvsp[0]));
	} else {
		current_report->t_columns = (yyvsp[0]);
	}
  }
#line 15386 "parser.c" /* yacc.c:1646  */
    break;

  case 738:
#line 6998 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("HEADING", SYN_CLAUSE_6, &check_duplicate);
	error_if_no_page_lines_limit ("HEADING");

	if (CB_LITERAL_P ((yyvsp[0]))) {
		current_report->heading = cb_get_int ((yyvsp[0]));
	} else {
		current_report->t_heading = (yyvsp[0]);
	}
  }
#line 15401 "parser.c" /* yacc.c:1646  */
    break;

  case 739:
#line 7012 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FIRST DETAIL", SYN_CLAUSE_7, &check_duplicate);
	error_if_no_page_lines_limit ("FIRST DETAIL");

	if (CB_LITERAL_P ((yyvsp[0]))) {
		current_report->first_detail = cb_get_int ((yyvsp[0]));
	} else {
		current_report->t_first_detail = (yyvsp[0]);
	}
  }
#line 15416 "parser.c" /* yacc.c:1646  */
    break;

  case 740:
#line 7026 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LAST CONTROL HEADING", SYN_CLAUSE_8, &check_duplicate);
	error_if_no_page_lines_limit ("LAST CONTROL HEADING");

	if (CB_LITERAL_P ((yyvsp[0]))) {
		current_report->last_control = cb_get_int ((yyvsp[0]));
	} else {
		current_report->t_last_control = (yyvsp[0]);
	}
  }
#line 15431 "parser.c" /* yacc.c:1646  */
    break;

  case 741:
#line 7040 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LAST DETAIL", SYN_CLAUSE_9, &check_duplicate);
	error_if_no_page_lines_limit ("LAST DETAIL");

	if (CB_LITERAL_P ((yyvsp[0]))) {
		current_report->last_detail = cb_get_int ((yyvsp[0]));
	} else {
		current_report->t_last_detail = (yyvsp[0]);
	}
  }
#line 15446 "parser.c" /* yacc.c:1646  */
    break;

  case 742:
#line 7054 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FOOTING", SYN_CLAUSE_10, &check_duplicate);
	error_if_no_page_lines_limit ("FOOTING");

	if (CB_LITERAL_P ((yyvsp[0]))) {
		current_report->footing = cb_get_int ((yyvsp[0]));
	} else {
		current_report->t_footing = (yyvsp[0]);
	}
  }
#line 15461 "parser.c" /* yacc.c:1646  */
    break;

  case 745:
#line 7072 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = cb_build_field_tree ((yyvsp[-1]), (yyvsp[0]), current_field, current_storage,
				 current_file, 0);
	/* Free tree associated with level number */
	cobc_parse_free ((yyvsp[-1]));
	check_pic_duplicate = 0;
	if (CB_INVALID_TREE (x)) {
		YYERROR;
	}

	current_field = CB_FIELD (x);
	if (!description_field) {
		description_field = current_field;
	}
  }
#line 15483 "parser.c" /* yacc.c:1646  */
    break;

  case 747:
#line 7091 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	/* Free tree associated with level number */
	cobc_parse_free ((yyvsp[-2]));
	cb_unput_dot ();
	yyerrok;
	check_pic_duplicate = 0;
	check_duplicate = 0;
	current_field = cb_get_real_field ();
  }
#line 15497 "parser.c" /* yacc.c:1646  */
    break;

  case 766:
#line 7127 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("TYPE", SYN_CLAUSE_16, &check_pic_duplicate);
  }
#line 15505 "parser.c" /* yacc.c:1646  */
    break;

  case 767:
#line 7134 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_HEADING;
  }
#line 15513 "parser.c" /* yacc.c:1646  */
    break;

  case 768:
#line 7138 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_PAGE_HEADING;
  }
#line 15521 "parser.c" /* yacc.c:1646  */
    break;

  case 771:
#line 7144 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if(current_report != NULL) {
		current_report->has_detail = 1;
	}
	current_field->report_flag |= COB_REPORT_DETAIL;
  }
#line 15532 "parser.c" /* yacc.c:1646  */
    break;

  case 772:
#line 7151 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_PAGE_FOOTING;
  }
#line 15540 "parser.c" /* yacc.c:1646  */
    break;

  case 773:
#line 7155 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_FOOTING;
  }
#line 15548 "parser.c" /* yacc.c:1646  */
    break;

  case 774:
#line 7162 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_CONTROL_HEADING;
  }
#line 15556 "parser.c" /* yacc.c:1646  */
    break;

  case 775:
#line 7166 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_CONTROL_HEADING;
	current_field->report_control = (yyvsp[-1]);
	if ((yyvsp[0])) {
		current_field->report_flag |= COB_REPORT_PAGE;
	}
  }
#line 15568 "parser.c" /* yacc.c:1646  */
    break;

  case 776:
#line 7174 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_CONTROL_HEADING_FINAL;
  }
#line 15576 "parser.c" /* yacc.c:1646  */
    break;

  case 777:
#line 7183 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {(yyval) = NULL;}
#line 15582 "parser.c" /* yacc.c:1646  */
    break;

  case 778:
#line 7184 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {(yyval) = cb_int0;}
#line 15588 "parser.c" /* yacc.c:1646  */
    break;

  case 779:
#line 7189 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_CONTROL_FOOTING;
  }
#line 15596 "parser.c" /* yacc.c:1646  */
    break;

  case 780:
#line 7193 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_CONTROL_FOOTING;
	current_field->report_control = (yyvsp[-1]);
  }
#line 15605 "parser.c" /* yacc.c:1646  */
    break;

  case 781:
#line 7198 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_CONTROL_FOOTING_FINAL;
  }
#line 15613 "parser.c" /* yacc.c:1646  */
    break;

  case 782:
#line 7202 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_CONTROL_FOOTING;
	current_field->report_flag |= COB_REPORT_ALL;
  }
#line 15622 "parser.c" /* yacc.c:1646  */
    break;

  case 783:
#line 7210 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("NEXT GROUP", SYN_CLAUSE_17, &check_pic_duplicate);
  }
#line 15630 "parser.c" /* yacc.c:1646  */
    break;

  case 784:
#line 7217 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_LITERAL_P((yyvsp[0])) && CB_LITERAL ((yyvsp[0]))->sign > 0) {
		current_field->report_flag |= COB_REPORT_NEXT_GROUP_PLUS;
	} else {
		current_field->report_flag |= COB_REPORT_NEXT_GROUP_LINE;
	}
	current_field->next_group_line = cb_get_int((yyvsp[0]));
  }
#line 15643 "parser.c" /* yacc.c:1646  */
    break;

  case 785:
#line 7226 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_NEXT_GROUP_PLUS;
	current_field->next_group_line = cb_get_int((yyvsp[0]));
  }
#line 15652 "parser.c" /* yacc.c:1646  */
    break;

  case 786:
#line 7231 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_NEXT_GROUP_PLUS;
	current_field->next_group_line = cb_get_int((yyvsp[0]));
  }
#line 15661 "parser.c" /* yacc.c:1646  */
    break;

  case 787:
#line 7236 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_NEXT_GROUP_PAGE;
  }
#line 15669 "parser.c" /* yacc.c:1646  */
    break;

  case 791:
#line 7249 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SUM", SYN_CLAUSE_19, &check_pic_duplicate);
	current_field->report_sum_list = (yyvsp[-1]);
	build_sum_counter( current_report, current_field);
  }
#line 15679 "parser.c" /* yacc.c:1646  */
    break;

  case 794:
#line 7259 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_sum_upon = (yyvsp[0]);
  }
#line 15687 "parser.c" /* yacc.c:1646  */
    break;

  case 795:
#line 7266 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_reset = (yyvsp[0]);
  }
#line 15695 "parser.c" /* yacc.c:1646  */
    break;

  case 796:
#line 7270 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_RESET_FINAL;
  }
#line 15703 "parser.c" /* yacc.c:1646  */
    break;

  case 797:
#line 7277 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PRESENT", SYN_CLAUSE_20, &check_pic_duplicate);
	current_field->report_when = (yyvsp[0]);
  }
#line 15712 "parser.c" /* yacc.c:1646  */
    break;

  case 798:
#line 7282 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PRESENT", SYN_CLAUSE_20, &check_pic_duplicate);
	current_field->report_flag |= COB_REPORT_PRESENT;
	current_field->report_flag &= ~COB_REPORT_BEFORE;
  }
#line 15722 "parser.c" /* yacc.c:1646  */
    break;

  case 799:
#line 7288 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PRESENT", SYN_CLAUSE_20, &check_pic_duplicate);
	current_field->report_flag |= COB_REPORT_PRESENT;
	current_field->report_flag &= ~COB_REPORT_BEFORE;
	current_field->report_flag |= COB_REPORT_PAGE;
  }
#line 15733 "parser.c" /* yacc.c:1646  */
    break;

  case 800:
#line 7295 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PRESENT", SYN_CLAUSE_20, &check_pic_duplicate);
	current_field->report_flag |= COB_REPORT_PRESENT;
	current_field->report_flag |= COB_REPORT_BEFORE;
  }
#line 15743 "parser.c" /* yacc.c:1646  */
    break;

  case 801:
#line 7301 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PRESENT", SYN_CLAUSE_20, &check_pic_duplicate);
	current_field->report_flag |= COB_REPORT_PRESENT;
	current_field->report_flag |= COB_REPORT_BEFORE;
	current_field->report_flag |= COB_REPORT_PAGE;
  }
#line 15754 "parser.c" /* yacc.c:1646  */
    break;

  case 802:
#line 7311 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_PRESENT;
  }
#line 15762 "parser.c" /* yacc.c:1646  */
    break;

  case 803:
#line 7315 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_PRESENT;
	current_field->report_flag |= COB_REPORT_NEGATE;
  }
#line 15771 "parser.c" /* yacc.c:1646  */
    break;

  case 806:
#line 7328 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_PAGE;
  }
#line 15779 "parser.c" /* yacc.c:1646  */
    break;

  case 807:
#line 7332 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_control = (yyvsp[0]);
  }
#line 15787 "parser.c" /* yacc.c:1646  */
    break;

  case 809:
#line 7340 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("RW VARYING clause");
  }
#line 15795 "parser.c" /* yacc.c:1646  */
    break;

  case 810:
#line 7347 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LINE", SYN_CLAUSE_21, &check_pic_duplicate);
	current_field->report_flag |= COB_REPORT_LINE;
  }
#line 15804 "parser.c" /* yacc.c:1646  */
    break;

  case 814:
#line 7364 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if(current_field->report_line == 0) {
		CB_PENDING ("LINE 0");
	}
  }
#line 15814 "parser.c" /* yacc.c:1646  */
    break;

  case 815:
#line 7370 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_LINE_NEXT_PAGE;
  }
#line 15822 "parser.c" /* yacc.c:1646  */
    break;

  case 816:
#line 7374 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_LINE_PLUS;
	current_field->report_line = cb_get_int((yyvsp[0]));
	if((yyvsp[0]) != cb_int0
	&& (yyvsp[0]) != cb_int1) {
		if ((CB_LITERAL_P((yyvsp[0])) && CB_LITERAL ((yyvsp[0]))->sign < 0)
		|| current_field->report_line < 0) {
			cb_error (_("positive integer value expected"));
		}
	}
	if (current_field->report_line == 0) {
		CB_PENDING ("LINE PLUS 0");
	}
  }
#line 15841 "parser.c" /* yacc.c:1646  */
    break;

  case 817:
#line 7389 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_LINE_PLUS;
	current_field->report_line = cb_get_int((yyvsp[0]));
	if((yyvsp[0]) != cb_int0
	&& (yyvsp[0]) != cb_int1) {
		if ((CB_LITERAL_P((yyvsp[0])) && CB_LITERAL ((yyvsp[0]))->sign < 0)
		|| current_field->report_line < 0) {
			cb_error (_("positive integer value expected"));
		}
	}
	if (current_field->report_line == 0) {
		CB_PENDING ("LINE PLUS 0");
	}
  }
#line 15860 "parser.c" /* yacc.c:1646  */
    break;

  case 818:
#line 7407 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_line = cb_get_int((yyvsp[0]));
	if((yyvsp[0]) != cb_int0) {
		if (CB_LITERAL_P((yyvsp[0])) && CB_LITERAL ((yyvsp[0]))->sign > 0) {
			current_field->report_flag |= COB_REPORT_LINE_PLUS;
		} else if ((CB_LITERAL_P((yyvsp[0])) && CB_LITERAL ((yyvsp[0]))->sign < 0)
			|| current_field->report_line < 0) {
			cb_error (_("positive integer value expected"));
			current_field->report_line = 1;
			current_field->report_flag |= COB_REPORT_LINE_PLUS;
		}
	}
  }
#line 15878 "parser.c" /* yacc.c:1646  */
    break;

  case 819:
#line 7425 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("COLUMN", SYN_CLAUSE_18, &check_pic_duplicate);
	if((current_field->report_flag & (COB_REPORT_COLUMN_LEFT|COB_REPORT_COLUMN_RIGHT|COB_REPORT_COLUMN_CENTER))
	&& (current_field->report_flag & COB_REPORT_COLUMN_PLUS)) {
		if (cb_relaxed_syntax_checks) {
			cb_warning (COBC_WARN_FILLER, _("PLUS is not recommended with LEFT, RIGHT or CENTER"));
		} else {
			cb_error (_("PLUS is not allowed with LEFT, RIGHT or CENTER"));
		}
	}
  }
#line 15894 "parser.c" /* yacc.c:1646  */
    break;

  case 823:
#line 7449 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_COLUMN_LEFT;
  }
#line 15902 "parser.c" /* yacc.c:1646  */
    break;

  case 824:
#line 7453 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_COLUMN_RIGHT;
  }
#line 15910 "parser.c" /* yacc.c:1646  */
    break;

  case 825:
#line 7457 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->report_flag |= COB_REPORT_COLUMN_CENTER;
  }
#line 15918 "parser.c" /* yacc.c:1646  */
    break;

  case 826:
#line 7464 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	int colnum;
	colnum = cb_get_int ((yyvsp[0]));
	if (colnum > 0) {
		if(current_field->parent
		&& current_field->parent->children == current_field) {
			cb_warning (COBC_WARN_FILLER, _("PLUS is ignored on first field of line"));
			if (current_field->step_count == 0)
				current_field->step_count = colnum;
		} else {
			current_field->report_flag |= COB_REPORT_COLUMN_PLUS;
		}
	} else {
		colnum = 0;
	}
	if(current_field->report_column == 0)
		current_field->report_column = colnum;
	current_field->report_num_col++;
  }
#line 15942 "parser.c" /* yacc.c:1646  */
    break;

  case 830:
#line 7493 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	int colnum;
	colnum = cb_get_int ((yyvsp[0]));
	if (CB_LITERAL_P((yyvsp[0])) && CB_LITERAL ((yyvsp[0]))->sign > 0) {
		if(current_field->parent
		&& current_field->parent->children == current_field) {
			cb_warning (COBC_WARN_FILLER, _("PLUS is ignored on first field of line"));
		} else {
			current_field->report_flag |= COB_REPORT_COLUMN_PLUS;
		}
	}
	if((yyvsp[0]) != cb_int1
	&& (yyvsp[0]) != cb_int0) {
		if (colnum <= 0
		|| (CB_LITERAL_P((yyvsp[0])) && CB_LITERAL ((yyvsp[0]))->sign < 0)) {
			cb_error (_("invalid COLUMN integer; must be > 0"));
			colnum = 0;
			(yyval) = cb_int0;
		} else if(colnum <= current_field->report_column) {
			cb_warning (COBC_WARN_FILLER, _("COLUMN numbers should increase"));
		}
		current_field->report_column_list =
				cb_list_append (current_field->report_column_list, CB_LIST_INIT ((yyvsp[0])));
	}
	if(current_field->report_column == 0)
		current_field->report_column = colnum;
	current_field->report_num_col++;
  }
#line 15975 "parser.c" /* yacc.c:1646  */
    break;

  case 831:
#line 7525 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SOURCE", SYN_CLAUSE_22, &check_pic_duplicate);
	current_field->report_source = (yyvsp[-1]);
  }
#line 15984 "parser.c" /* yacc.c:1646  */
    break;

  case 832:
#line 7533 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("GROUP", SYN_CLAUSE_23, &check_pic_duplicate);
	current_field->report_flag |= COB_REPORT_GROUP_INDICATE;
  }
#line 15993 "parser.c" /* yacc.c:1646  */
    break;

  case 834:
#line 7543 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = CB_CS_SCREEN;
	current_storage = CB_STORAGE_SCREEN;
	current_field = NULL;
	description_field = NULL;
	cb_clear_real_field ();
  }
#line 16005 "parser.c" /* yacc.c:1646  */
    break;

  case 835:
#line 7551 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	struct cb_field *p;

	if (description_field) {
		for (p = description_field; p; p = p->sister) {
			cb_validate_field (p);
		}
		current_program->screen_storage = description_field;
		current_program->flag_screen = 1;
	}
	cobc_cs_check = 0;
  }
#line 16022 "parser.c" /* yacc.c:1646  */
    break;

  case 841:
#line 7578 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = cb_build_field_tree ((yyvsp[-1]), (yyvsp[0]), current_field, current_storage,
				 current_file, 0);
	/* Free tree associated with level number */
	cobc_parse_free ((yyvsp[-1]));
	check_pic_duplicate = 0;
	if (CB_INVALID_TREE (x)) {
		YYERROR;
	}

	current_field = CB_FIELD (x);
	if (current_field->parent) {
		current_field->screen_foreg = current_field->parent->screen_foreg;
		current_field->screen_backg = current_field->parent->screen_backg;
		current_field->screen_prompt = current_field->parent->screen_prompt;
	}
  }
#line 16046 "parser.c" /* yacc.c:1646  */
    break;

  case 842:
#line 7598 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cob_flags_t	flags;

	if (current_field->parent) {
		flags = current_field->parent->screen_flag;
		flags &= ~COB_SCREEN_BLANK_LINE;
		flags &= ~COB_SCREEN_BLANK_SCREEN;
		flags &= ~COB_SCREEN_ERASE_EOL;
		flags &= ~COB_SCREEN_ERASE_EOS;
		flags &= ~COB_SCREEN_LINE_PLUS;
		flags &= ~COB_SCREEN_LINE_MINUS;
		flags &= ~COB_SCREEN_COLUMN_PLUS;
		flags &= ~COB_SCREEN_COLUMN_MINUS;

		flags = zero_conflicting_flags (current_field->screen_flag,
						flags);

		current_field->screen_flag |= flags;
	}

	if (current_field->screen_flag & COB_SCREEN_INITIAL) {
		if (!(current_field->screen_flag & COB_SCREEN_INPUT)) {
			cb_error (_("INITIAL specified on non-input field"));
		}
	}
	if (!qualifier) {
		current_field->flag_filler = 1;
	}

	if (likely (current_field)) {
		if (!description_field) {
			description_field = current_field;
		}
		if (current_field->flag_occurs
		    && !has_relative_pos (current_field)) {
			cb_error (_("relative LINE/COLUMN clause required with OCCURS"));
		}
	}
  }
#line 16090 "parser.c" /* yacc.c:1646  */
    break;

  case 843:
#line 7639 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = cb_build_field_tree ((yyvsp[-1]), (yyvsp[0]), current_field, current_storage,
				 current_file, 0);
	/* Free tree associated with level number */
	cobc_parse_free ((yyvsp[-1]));
	check_pic_duplicate = 0;
	if (CB_INVALID_TREE (x)) {
		YYERROR;
	}

	current_field = CB_FIELD (x);
	if (current_field->parent) {
		current_field->screen_foreg = current_field->parent->screen_foreg;
		current_field->screen_backg = current_field->parent->screen_backg;
		current_field->screen_prompt = current_field->parent->screen_prompt;
	}
  }
#line 16114 "parser.c" /* yacc.c:1646  */
    break;

  case 844:
#line 7659 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("GRAPHICAL CONTROL");
  }
#line 16122 "parser.c" /* yacc.c:1646  */
    break;

  case 845:
#line 7664 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cob_flags_t	flags;

	if (current_field->parent) {
		flags = current_field->parent->screen_flag;
		flags &= ~COB_SCREEN_BLANK_LINE;
		flags &= ~COB_SCREEN_BLANK_SCREEN;
		flags &= ~COB_SCREEN_ERASE_EOL;
		flags &= ~COB_SCREEN_ERASE_EOS;
		flags &= ~COB_SCREEN_LINE_PLUS;
		flags &= ~COB_SCREEN_LINE_MINUS;
		flags &= ~COB_SCREEN_COLUMN_PLUS;
		flags &= ~COB_SCREEN_COLUMN_MINUS;

		flags = zero_conflicting_flags (current_field->screen_flag,
						flags);

		current_field->screen_flag |= flags;
	}

	if (current_field->screen_flag & COB_SCREEN_INITIAL) {
		if (!(current_field->screen_flag & COB_SCREEN_INPUT)) {
			cb_error (_("INITIAL specified on non-input field"));
		}
	}
	if (!qualifier) {
		current_field->flag_filler = 1;
	}

	if (likely (current_field)) {
		if (!description_field) {
			description_field = current_field;
		}
		if (current_field->flag_occurs
		    && !has_relative_pos (current_field)) {
			cb_error (_("relative LINE/COLUMN clause required with OCCURS"));
		}
	}
	cobc_cs_check = CB_CS_SCREEN;
  }
#line 16167 "parser.c" /* yacc.c:1646  */
    break;

  case 846:
#line 7706 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	/* Free tree associated with level number */
	cobc_parse_free ((yyvsp[-2]));
	yyerrok;
	cb_unput_dot ();
	check_pic_duplicate = 0;
	check_duplicate = 0;
#if	1	/* RXWRXW Screen field */
	if (current_field) {
		current_field->flag_is_verified = 1;
		current_field->flag_invalid = 1;
	}
#endif
	current_field = cb_get_real_field ();
  }
#line 16187 "parser.c" /* yacc.c:1646  */
    break;

  case 849:
#line 7730 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("BLANK LINE", COB_SCREEN_BLANK_LINE,
				       "BLANK SCREEN", COB_SCREEN_BLANK_SCREEN);
  }
#line 16196 "parser.c" /* yacc.c:1646  */
    break;

  case 850:
#line 7735 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("BLANK SCREEN", COB_SCREEN_BLANK_SCREEN,
				       "BLANK LINE", COB_SCREEN_BLANK_LINE);
  }
#line 16205 "parser.c" /* yacc.c:1646  */
    break;

  case 851:
#line 7740 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("BELL", COB_SCREEN_BELL);
  }
#line 16213 "parser.c" /* yacc.c:1646  */
    break;

  case 852:
#line 7744 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("BLINK", COB_SCREEN_BLINK);
  }
#line 16221 "parser.c" /* yacc.c:1646  */
    break;

  case 853:
#line 7748 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("ERASE EOL", COB_SCREEN_ERASE_EOL,
				       "ERASE EOS", COB_SCREEN_ERASE_EOS);
  }
#line 16230 "parser.c" /* yacc.c:1646  */
    break;

  case 854:
#line 7753 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("ERASE EOS", COB_SCREEN_ERASE_EOS,
				       "ERASE EOL", COB_SCREEN_ERASE_EOL);
  }
#line 16239 "parser.c" /* yacc.c:1646  */
    break;

  case 855:
#line 7758 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("HIGHLIGHT", COB_SCREEN_HIGHLIGHT,
				       "LOWLIGHT", COB_SCREEN_LOWLIGHT);
  }
#line 16248 "parser.c" /* yacc.c:1646  */
    break;

  case 856:
#line 7763 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("LOWLIGHT", COB_SCREEN_LOWLIGHT,
				       "HIGHLIGHT", COB_SCREEN_HIGHLIGHT);
  }
#line 16257 "parser.c" /* yacc.c:1646  */
    break;

  case 857:
#line 7768 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("STANDARD intensity");
#if 0 /* in general we could simply remove high/low, but for syntax checks
	we still need a flag */
	set_screen_attr_with_conflict ("LOWLIGHT", COB_SCREEN_LOWLIGHT,
				       "HIGHLIGHT", COB_SCREEN_HIGHLIGHT);
#endif
  }
#line 16270 "parser.c" /* yacc.c:1646  */
    break;

  case 858:
#line 7777 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("BACKGROUND intensity");
  }
#line 16278 "parser.c" /* yacc.c:1646  */
    break;

  case 859:
#line 7781 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("BACKGROUND intensity");
  }
#line 16286 "parser.c" /* yacc.c:1646  */
    break;

  case 860:
#line 7785 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("BACKGROUND intensity");
  }
#line 16294 "parser.c" /* yacc.c:1646  */
    break;

  case 861:
#line 7789 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("REVERSE-VIDEO", COB_SCREEN_REVERSE);
  }
#line 16302 "parser.c" /* yacc.c:1646  */
    break;

  case 862:
#line 7793 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	/* set_screen_attr ("SIZE", COB_SCREEN_SIZE); */
	CB_PENDING ("SIZE clause");
	current_field->size = cb_get_int ((yyvsp[0]));
  }
#line 16312 "parser.c" /* yacc.c:1646  */
    break;

  case 863:
#line 7799 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING (_("screen positions from data-item"));
  }
#line 16320 "parser.c" /* yacc.c:1646  */
    break;

  case 864:
#line 7803 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING (_("screen positions from data-item"));
	CB_PENDING ("SIZE clause");
  }
#line 16329 "parser.c" /* yacc.c:1646  */
    break;

  case 865:
#line 7808 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	/* set_screen_attr ("SIZE", COB_SCREEN_SIZE); */
	CB_PENDING ("SIZE clause");
	current_field->size = cb_get_int ((yyvsp[0]));
  }
#line 16339 "parser.c" /* yacc.c:1646  */
    break;

  case 866:
#line 7814 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("UNDERLINE", COB_SCREEN_UNDERLINE);
  }
#line 16347 "parser.c" /* yacc.c:1646  */
    break;

  case 867:
#line 7818 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("OVERLINE", COB_SCREEN_OVERLINE);
	CB_PENDING ("OVERLINE");
  }
#line 16356 "parser.c" /* yacc.c:1646  */
    break;

  case 868:
#line 7823 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("GRID", COB_SCREEN_GRID);
	CB_PENDING ("GRID");
  }
#line 16365 "parser.c" /* yacc.c:1646  */
    break;

  case 869:
#line 7828 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("LEFTLINE", COB_SCREEN_LEFTLINE);
	CB_PENDING ("LEFTLINE");
  }
#line 16374 "parser.c" /* yacc.c:1646  */
    break;

  case 870:
#line 7833 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("AUTO", COB_SCREEN_AUTO,
				       "TAB", COB_SCREEN_TAB);
  }
#line 16383 "parser.c" /* yacc.c:1646  */
    break;

  case 871:
#line 7838 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("TAB", COB_SCREEN_TAB,
				       "AUTO", COB_SCREEN_AUTO);
  }
#line 16392 "parser.c" /* yacc.c:1646  */
    break;

  case 872:
#line 7843 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	set_screen_attr_with_conflict ("SECURE", COB_SCREEN_SECURE,
				       "NO-ECHO", COB_SCREEN_NO_ECHO);
  }
#line 16401 "parser.c" /* yacc.c:1646  */
    break;

  case 873:
#line 7848 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (cb_no_echo_means_secure) {
		set_screen_attr ("SECURE", COB_SCREEN_SECURE);
	} else {
		set_screen_attr_with_conflict ("NO-ECHO", COB_SCREEN_NO_ECHO,
					       "SECURE", COB_SCREEN_SECURE);
	}
  }
#line 16414 "parser.c" /* yacc.c:1646  */
    break;

  case 874:
#line 7857 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("REQUIRED", COB_SCREEN_REQUIRED);
  }
#line 16422 "parser.c" /* yacc.c:1646  */
    break;

  case 875:
#line 7861 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("FULL", COB_SCREEN_FULL);
  }
#line 16430 "parser.c" /* yacc.c:1646  */
    break;

  case 876:
#line 7865 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("PROMPT", COB_SCREEN_PROMPT);
	current_field->screen_prompt = (yyvsp[0]);
  }
#line 16439 "parser.c" /* yacc.c:1646  */
    break;

  case 877:
#line 7870 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("PROMPT", COB_SCREEN_PROMPT);
  }
#line 16447 "parser.c" /* yacc.c:1646  */
    break;

  case 878:
#line 7874 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	set_screen_attr ("INITIAL", COB_SCREEN_INITIAL);
  }
#line 16455 "parser.c" /* yacc.c:1646  */
    break;

  case 879:
#line 7878 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LINE", SYN_CLAUSE_16, &check_pic_duplicate);
  }
#line 16463 "parser.c" /* yacc.c:1646  */
    break;

  case 880:
#line 7882 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("LINES clause");	/* note: should only occur with controls */
  }
#line 16471 "parser.c" /* yacc.c:1646  */
    break;

  case 881:
#line 7886 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	//check_repeated ("CLINE", SYN_CLAUSE_5000, &check_pic_duplicate);
  }
#line 16479 "parser.c" /* yacc.c:1646  */
    break;

  case 882:
#line 7890 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("COLUMN", SYN_CLAUSE_17, &check_pic_duplicate);
  }
#line 16487 "parser.c" /* yacc.c:1646  */
    break;

  case 883:
#line 7894 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	//check_repeated ("CCOL", SYN_CLAUSE_5001, &check_pic_duplicate);
  }
#line 16495 "parser.c" /* yacc.c:1646  */
    break;

  case 884:
#line 7898 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
#if 0 /* TODO: implement, and add reverse to BACKGROUND/FOREGROUND-COLOR */
	check_repeated ("COLOR", SYN_CLAUSE_19, &check_pic_duplicate);
	set_screen_attr_with_conflict ("COLOR", COB_SCREEN_COLOR,
				       "BACKGROUND-COLOR", COB_SCREEN_BACKGROUND_COLOR);
	set_screen_attr_with_conflict ("COLOR", COB_SCREEN_COLOR,
				       "FOREGROUND-COLOR", FOREGROUND_COLOR);
#endif
	CB_PENDING ("COLOR clause");
  }
#line 16510 "parser.c" /* yacc.c:1646  */
    break;

  case 885:
#line 7909 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FOREGROUND-COLOR", SYN_CLAUSE_18, &check_pic_duplicate);
	current_field->screen_foreg = (yyvsp[0]);
  }
#line 16519 "parser.c" /* yacc.c:1646  */
    break;

  case 886:
#line 7914 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BACKGROUND-COLOR", SYN_CLAUSE_19, &check_pic_duplicate);
	current_field->screen_backg = (yyvsp[0]);
  }
#line 16528 "parser.c" /* yacc.c:1646  */
    break;

  case 895:
#line 7927 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));

	check_repeated ("USING", SYN_CLAUSE_20, &check_pic_duplicate);
	current_field->screen_from = (yyvsp[0]);
	current_field->screen_to = (yyvsp[0]);
	current_field->screen_flag |= COB_SCREEN_INPUT;
  }
#line 16541 "parser.c" /* yacc.c:1646  */
    break;

  case 896:
#line 7936 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FROM", SYN_CLAUSE_21, &check_pic_duplicate);
	current_field->screen_from = (yyvsp[0]);
  }
#line 16550 "parser.c" /* yacc.c:1646  */
    break;

  case 897:
#line 7941 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));

	check_repeated ("TO", SYN_CLAUSE_22, &check_pic_duplicate);
	current_field->screen_to = (yyvsp[0]);
	current_field->screen_flag |= COB_SCREEN_INPUT;
  }
#line 16562 "parser.c" /* yacc.c:1646  */
    break;

  case 899:
#line 7953 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check |= CB_CS_GRAPHICAL_CONTROL;
  }
#line 16570 "parser.c" /* yacc.c:1646  */
    break;

  case 1204:
#line 8556 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[-1]); }
#line 16576 "parser.c" /* yacc.c:1646  */
    break;

  case 1205:
#line 8557 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 16582 "parser.c" /* yacc.c:1646  */
    break;

  case 1206:
#line 8561 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 16588 "parser.c" /* yacc.c:1646  */
    break;

  case 1207:
#line 8562 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 16594 "parser.c" /* yacc.c:1646  */
    break;

  case 1208:
#line 8567 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		current_field->screen_line = (yyvsp[0]);
	}
  }
#line 16604 "parser.c" /* yacc.c:1646  */
    break;

  case 1210:
#line 8577 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->screen_flag |= COB_SCREEN_LINE_PLUS;
  }
#line 16612 "parser.c" /* yacc.c:1646  */
    break;

  case 1211:
#line 8581 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->screen_flag |= COB_SCREEN_LINE_MINUS;
  }
#line 16620 "parser.c" /* yacc.c:1646  */
    break;

  case 1212:
#line 8588 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		current_field->screen_column = (yyvsp[0]);
	}
  }
#line 16630 "parser.c" /* yacc.c:1646  */
    break;

  case 1213:
#line 8597 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	/* Nothing */
  }
#line 16638 "parser.c" /* yacc.c:1646  */
    break;

  case 1214:
#line 8601 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->screen_flag |= COB_SCREEN_COLUMN_PLUS;
  }
#line 16646 "parser.c" /* yacc.c:1646  */
    break;

  case 1215:
#line 8605 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_field->screen_flag |= COB_SCREEN_COLUMN_MINUS;
  }
#line 16654 "parser.c" /* yacc.c:1646  */
    break;

  case 1216:
#line 8612 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING (_("OCCURS screen items"));
	check_repeated ("OCCURS", SYN_CLAUSE_23, &check_pic_duplicate);
	current_field->occurs_max = cb_get_int ((yyvsp[-1]));
	current_field->occurs_min = current_field->occurs_max;
	current_field->indexes++;
	current_field->flag_occurs = 1;
  }
#line 16667 "parser.c" /* yacc.c:1646  */
    break;

  case 1217:
#line 8624 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING (_("GLOBAL screen items"));
  }
#line 16675 "parser.c" /* yacc.c:1646  */
    break;

  case 1218:
#line 8633 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_section = NULL;
	current_paragraph = NULL;
	check_pic_duplicate = 0;
	check_duplicate = 0;
	if (!current_program->entry_convention) {
		current_program->entry_convention = cb_int (CB_CONV_COBOL);
	}
  }
#line 16689 "parser.c" /* yacc.c:1646  */
    break;

  case 1219:
#line 8643 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_section = NULL;
	current_paragraph = NULL;
	check_pic_duplicate = 0;
	check_duplicate = 0;
	cobc_in_procedure = 1U;
	cb_set_system_names ();
	backup_current_pos ();
  }
#line 16703 "parser.c" /* yacc.c:1646  */
    break;

  case 1220:
#line 8653 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-3])) {
		if (current_program->entry_convention) {
			cb_warning (COBC_WARN_FILLER, _("overriding convention specified in ENTRY-CONVENTION"));
		}
		current_program->entry_convention = (yyvsp[-3]);
	} else if (!current_program->entry_convention) {
		current_program->entry_convention = cb_int (CB_CONV_COBOL);
	}
	header_check |= COBC_HD_PROCEDURE_DIVISION;
  }
#line 16719 "parser.c" /* yacc.c:1646  */
    break;

  case 1221:
#line 8665 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (current_program->flag_main
	 && !current_program->flag_chained && (yyvsp[-4])) {
		cb_error (_("executable program requested but PROCEDURE/ENTRY has USING clause"));
	}
	/* Main entry point */
	emit_entry (current_program->program_id, 0, (yyvsp[-4]), NULL);
	current_program->num_proc_params = cb_list_length ((yyvsp[-4]));
	if (current_program->source_name) {
		emit_entry (current_program->source_name, 1, (yyvsp[-4]), NULL);
	}
  }
#line 16736 "parser.c" /* yacc.c:1646  */
    break;

  case 1222:
#line 8678 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (current_paragraph) {
		if (current_paragraph->exit_label) {
			emit_statement (current_paragraph->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_paragraph));
	}
	if (current_section) {
		if (current_section->exit_label) {
			emit_statement (current_section->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_section));
	}
  }
#line 16755 "parser.c" /* yacc.c:1646  */
    break;

  case 1223:
#line 8693 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree label;

	/* No PROCEDURE DIVISION header here */
	/* Only a statement is allowed as first element */
	/* Thereafter, sections/paragraphs may be used */
	check_pic_duplicate = 0;
	check_duplicate = 0;
	if (!current_program->entry_convention) {
		current_program->entry_convention = cb_int (CB_CONV_COBOL);
	}
	cobc_in_procedure = 1U;
	label = cb_build_reference ("MAIN SECTION");
	current_section = CB_LABEL (cb_build_label (label, NULL));
	current_section->flag_section = 1;
	current_section->flag_dummy_section = 1;
	current_section->flag_skip_label = !!skip_statements;
	current_section->flag_declaratives = !!in_declaratives;
	current_section->xref.skip = 1;
	emit_statement (CB_TREE (current_section));
	label = cb_build_reference ("MAIN PARAGRAPH");
	current_paragraph = CB_LABEL (cb_build_label (label, NULL));
	current_paragraph->flag_declaratives = !!in_declaratives;
	current_paragraph->flag_skip_label = !!skip_statements;
	current_paragraph->flag_dummy_paragraph = 1;
	current_paragraph->xref.skip = 1;
	emit_statement (CB_TREE (current_paragraph));
	cb_set_system_names ();
  }
#line 16789 "parser.c" /* yacc.c:1646  */
    break;

  case 1225:
#line 8727 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 16797 "parser.c" /* yacc.c:1646  */
    break;

  case 1226:
#line 8731 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	call_mode = CB_CALL_BY_REFERENCE;
	size_mode = CB_SIZE_4;
  }
#line 16806 "parser.c" /* yacc.c:1646  */
    break;

  case 1227:
#line 8736 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (cb_list_length ((yyvsp[0])) > MAX_CALL_FIELD_PARAMS) {
		cb_error (_("number of parameters exceeds maximum %d"),
			  MAX_CALL_FIELD_PARAMS);
	}
	(yyval) = (yyvsp[0]);
  }
#line 16818 "parser.c" /* yacc.c:1646  */
    break;

  case 1228:
#line 8744 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	call_mode = CB_CALL_BY_REFERENCE;
	if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("CHAINING invalid in user FUNCTION"));
	} else {
		current_program->flag_chained = 1;
	}
  }
#line 16831 "parser.c" /* yacc.c:1646  */
    break;

  case 1229:
#line 8753 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (cb_list_length ((yyvsp[0])) > MAX_CALL_FIELD_PARAMS) {
		cb_error (_("number of parameters exceeds maximum %d"),
			  MAX_CALL_FIELD_PARAMS);
	}
	(yyval) = (yyvsp[0]);
  }
#line 16843 "parser.c" /* yacc.c:1646  */
    break;

  case 1230:
#line 8763 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 16849 "parser.c" /* yacc.c:1646  */
    break;

  case 1231:
#line 8765 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0])); }
#line 16855 "parser.c" /* yacc.c:1646  */
    break;

  case 1232:
#line 8770 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;
	struct cb_field	*f;

	x = cb_build_identifier ((yyvsp[0]), 0);
	if ((yyvsp[-1]) == cb_int1 && CB_VALID_TREE (x) && cb_ref (x) != cb_error_node) {
		f = CB_FIELD (cb_ref (x));
		f->flag_is_pdiv_opt = 1;
	}

	if (call_mode == CB_CALL_BY_VALUE
	    && CB_REFERENCE_P ((yyvsp[0]))
	    && CB_FIELD (cb_ref ((yyvsp[0])))->flag_any_length) {
		cb_error_x ((yyvsp[0]), _("ANY LENGTH items may only be BY REFERENCE formal parameters"));
	}

	(yyval) = CB_BUILD_PAIR (cb_int (call_mode), x);
	CB_SIZES ((yyval)) = size_mode;
  }
#line 16879 "parser.c" /* yacc.c:1646  */
    break;

  case 1234:
#line 8794 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	call_mode = CB_CALL_BY_REFERENCE;
  }
#line 16887 "parser.c" /* yacc.c:1646  */
    break;

  case 1235:
#line 8798 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (current_program->flag_chained) {
		cb_error (_("%s not allowed in CHAINED programs"), "BY VALUE");
	} else {
		CB_UNFINISHED (_("parameters passed BY VALUE"));
		call_mode = CB_CALL_BY_VALUE;
	}
  }
#line 16900 "parser.c" /* yacc.c:1646  */
    break;

  case 1237:
#line 8811 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else {
		size_mode = CB_SIZE_AUTO;
	}
  }
#line 16912 "parser.c" /* yacc.c:1646  */
    break;

  case 1238:
#line 8819 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else {
		size_mode = CB_SIZE_4;
	}
  }
#line 16924 "parser.c" /* yacc.c:1646  */
    break;

  case 1239:
#line 8827 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else {
		size_mode = CB_SIZE_AUTO | CB_SIZE_UNSIGNED;
	}
  }
#line 16936 "parser.c" /* yacc.c:1646  */
    break;

  case 1240:
#line 8835 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (size_mode) {
		size_mode |= CB_SIZE_UNSIGNED;
	}
  }
#line 16946 "parser.c" /* yacc.c:1646  */
    break;

  case 1242:
#line 8845 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	unsigned char *s = CB_LITERAL ((yyvsp[0]))->data;
	size_mode = 0;

	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else if (CB_LITERAL ((yyvsp[0]))->size != 1) {
		cb_error_x ((yyvsp[0]), _("invalid value for SIZE"));
	} else {
		size_mode = 0;
		switch (*s) {
		case '1':
			size_mode = CB_SIZE_1;
			break;
		case '2':
			size_mode = CB_SIZE_2;
			break;
		case '4':
			size_mode = CB_SIZE_4;
			break;
		case '8':
			size_mode = CB_SIZE_8;
			break;
		default:
			cb_error_x ((yyvsp[0]), _("invalid value for SIZE"));
			break;
		}
	}
  }
#line 16980 "parser.c" /* yacc.c:1646  */
    break;

  case 1243:
#line 8878 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int0;
  }
#line 16988 "parser.c" /* yacc.c:1646  */
    break;

  case 1244:
#line 8882 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (call_mode != CB_CALL_BY_REFERENCE) {
		cb_error (_("OPTIONAL only allowed for BY REFERENCE items"));
		(yyval) = cb_int0;
	} else {
		(yyval) = cb_int1;
	}
  }
#line 17001 "parser.c" /* yacc.c:1646  */
    break;

  case 1245:
#line 8894 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("RETURNING clause is required for a FUNCTION"));
	}
  }
#line 17011 "parser.c" /* yacc.c:1646  */
    break;

  case 1246:
#line 8900 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (current_program->flag_main) {
		cb_error (_("RETURNING clause cannot be OMITTED for main program"));
	}
	if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("RETURNING clause cannot be OMITTED for a FUNCTION"));
	}
	current_program->flag_void = 1;
  }
#line 17025 "parser.c" /* yacc.c:1646  */
    break;

  case 1247:
#line 8910 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	struct cb_field	*f;

	if (cb_ref ((yyvsp[0])) != cb_error_node) {
		f = CB_FIELD_PTR ((yyvsp[0]));
		/* standard rule: returning item is allocated in the
		   activating runtime element */
		if (f->storage != CB_STORAGE_LINKAGE) {
			cb_error (_("RETURNING item is not defined in LINKAGE SECTION"));
		} else if (f->level != 1 && f->level != 77) {
			cb_error (_("RETURNING item must have level 01"));
		} else if (f->flag_occurs) {
			cb_error (_("RETURNING item should not have OCCURS"));
		} else {
			if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
				if (f->flag_any_length) {
					cb_error (_("function RETURNING item may not be ANY LENGTH"));
				}

				f->flag_is_returning = 1;
			}
			current_program->returning = (yyvsp[0]);
		}
	}
  }
#line 17055 "parser.c" /* yacc.c:1646  */
    break;

  case 1249:
#line 8939 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	in_declaratives = 1;
	emit_statement (cb_build_comment ("DECLARATIVES"));
  }
#line 17064 "parser.c" /* yacc.c:1646  */
    break;

  case 1250:
#line 8945 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (needs_field_debug) {
		start_debug = 1;
	}
	in_declaratives = 0;
	in_debugging = 0;
	if (current_paragraph) {
		if (current_paragraph->exit_label) {
			emit_statement (current_paragraph->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_paragraph));
		current_paragraph = NULL;
	}
	if (current_section) {
		if (current_section->exit_label) {
			emit_statement (current_section->exit_label);
		}
		current_section->flag_fatal_check = 1;
		emit_statement (cb_build_perform_exit (current_section));
		current_section = NULL;
	}
	skip_statements = 0;
	emit_statement (cb_build_comment ("END DECLARATIVES"));
	check_unreached = 0;
  }
#line 17094 "parser.c" /* yacc.c:1646  */
    break;

  case 1255:
#line 8983 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (next_label_list) {
		cb_tree	plabel;
		char	name[32];

		snprintf (name, sizeof(name), "L$%d", next_label_id);
		plabel = cb_build_label (cb_build_reference (name), NULL);
		CB_LABEL (plabel)->flag_next_sentence = 1;
		emit_statement (plabel);
		current_program->label_list =
			cb_list_append (current_program->label_list, next_label_list);
		next_label_list = NULL;
		next_label_id++;
	}
	/* check_unreached = 0; */
	cb_end_statement();
  }
#line 17116 "parser.c" /* yacc.c:1646  */
    break;

  case 1257:
#line 9002 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	/* check_unreached = 0; */
	cb_end_statement();
  }
#line 17125 "parser.c" /* yacc.c:1646  */
    break;

  case 1258:
#line 9013 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	non_const_word = 0;
	check_unreached = 0;
	if (cb_build_section_name ((yyvsp[-1]), 0) == cb_error_node) {
		YYERROR;
	}

	/* Exit the last paragraph/section */
	if (current_paragraph) {
		if (current_paragraph->exit_label) {
			emit_statement (current_paragraph->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_paragraph));
	}
	if (current_section) {
		if (current_section->exit_label) {
			emit_statement (current_section->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_section));
	}
	if (current_program->flag_debugging && !in_debugging) {
		if (current_paragraph || current_section) {
			emit_statement (cb_build_comment (
					"DEBUGGING - Fall through"));
			emit_statement (cb_build_debug (cb_debug_contents,
					"FALL THROUGH", NULL));
		}
	}

	/* Begin a new section */
	current_section = CB_LABEL (cb_build_label ((yyvsp[-1]), NULL));
	current_section->flag_section = 1;
	/* Careful here, one negation */
	current_section->flag_real_label = !in_debugging;
	current_section->flag_declaratives = !!in_declaratives;
	current_section->flag_skip_label = !!skip_statements;
	current_paragraph = NULL;
  }
#line 17168 "parser.c" /* yacc.c:1646  */
    break;

  case 1259:
#line 9053 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	emit_statement (CB_TREE (current_section));
  }
#line 17176 "parser.c" /* yacc.c:1646  */
    break;

  case 1262:
#line 9064 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree label;

	non_const_word = 0;
	check_unreached = 0;
	if (cb_build_section_name ((yyvsp[-1]), 1) == cb_error_node) {
		YYERROR;
	}

	/* Exit the last paragraph */
	if (current_paragraph) {
		if (current_paragraph->exit_label) {
			emit_statement (current_paragraph->exit_label);
		}
		emit_statement (cb_build_perform_exit (current_paragraph));
		if (current_program->flag_debugging && !in_debugging) {
			emit_statement (cb_build_comment (
					"DEBUGGING - Fall through"));
			emit_statement (cb_build_debug (cb_debug_contents,
					"FALL THROUGH", NULL));
		}
	}

	/* Begin a new paragraph */
	if (!current_section) {
		label = cb_build_reference ("MAIN SECTION");
		current_section = CB_LABEL (cb_build_label (label, NULL));
		current_section->flag_section = 1;
		current_section->flag_dummy_section = 1;
		current_section->flag_declaratives = !!in_declaratives;
		current_section->flag_skip_label = !!skip_statements;
		current_section->xref.skip = 1;
		emit_statement (CB_TREE (current_section));
	}
	current_paragraph = CB_LABEL (cb_build_label ((yyvsp[-1]), current_section));
	current_paragraph->flag_declaratives = !!in_declaratives;
	current_paragraph->flag_skip_label = !!skip_statements;
	current_paragraph->flag_real_label = !in_debugging;
	current_paragraph->segment = current_section->segment;
	emit_statement (CB_TREE (current_paragraph));
  }
#line 17222 "parser.c" /* yacc.c:1646  */
    break;

  case 1263:
#line 9109 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	non_const_word = 0;
	check_unreached = 0;
	if (cb_build_section_name ((yyvsp[0]), 0) != cb_error_node) {
		if (is_reserved_word (CB_NAME ((yyvsp[0])))) {
			cb_error_x ((yyvsp[0]), _("'%s' is not a statement"), CB_NAME ((yyvsp[0])));
		} else if (is_default_reserved_word (CB_NAME ((yyvsp[0])))) {
			cb_error_x ((yyvsp[0]), _("unknown statement '%s'; it may exist in another dialect"),
				    CB_NAME ((yyvsp[0])));
		} else {
			cb_error_x ((yyvsp[0]), _("unknown statement '%s'"), CB_NAME ((yyvsp[0])));
		}
	}
	YYERROR;
  }
#line 17242 "parser.c" /* yacc.c:1646  */
    break;

  case 1264:
#line 9128 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 17250 "parser.c" /* yacc.c:1646  */
    break;

  case 1265:
#line 9132 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	int segnum = cb_get_int ((yyvsp[0]));

	(yyval) = NULL;
	if (cb_verify (cb_section_segments, "SECTION segment")) {
		if (segnum > 99) {
			cb_error (_("SECTION segment-number must be less than or equal to 99"));
		} else {
			if (in_declaratives && segnum > 49) {
				cb_error (_("SECTION segment-number in DECLARATIVES must be less than 50"));
			}
			if (!in_declaratives) {
				current_program->flag_segments = 1;
				current_section->segment = segnum;
			} else {
				/* Simon: old version did not allow segments in declaratives at all
					ToDo: check codegen for possible missing parts */
				CB_PENDING (_("SECTION segment within DECLARATIVES"));
			}
		}
	}
  }
#line 17277 "parser.c" /* yacc.c:1646  */
    break;

  case 1266:
#line 9161 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = current_program->exec_list;
	current_program->exec_list = NULL;
	check_unreached = 0;
  }
#line 17287 "parser.c" /* yacc.c:1646  */
    break;

  case 1267:
#line 9166 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_TREE (current_statement);
	current_statement = NULL;
  }
#line 17296 "parser.c" /* yacc.c:1646  */
    break;

  case 1268:
#line 9171 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_reverse (current_program->exec_list);
	current_program->exec_list = (yyvsp[-2]);
	current_statement = CB_STATEMENT ((yyvsp[-1]));
  }
#line 17306 "parser.c" /* yacc.c:1646  */
    break;

  case 1269:
#line 9179 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree label;

	if (!current_section) {
		label = cb_build_reference ("MAIN SECTION");
		current_section = CB_LABEL (cb_build_label (label, NULL));
		current_section->flag_section = 1;
		current_section->flag_dummy_section = 1;
		current_section->flag_skip_label = !!skip_statements;
		current_section->flag_declaratives = !!in_declaratives;
		current_section->xref.skip = 1;
		emit_statement (CB_TREE (current_section));
	}
	if (!current_paragraph) {
		label = cb_build_reference ("MAIN PARAGRAPH");
		current_paragraph = CB_LABEL (cb_build_label (label, NULL));
		CB_TREE (current_paragraph)->source_file
			= CB_TREE (current_section)->source_file;
		CB_TREE (current_paragraph)->source_line
			= CB_TREE (current_section)->source_line;
		current_paragraph->flag_declaratives = !!in_declaratives;
		current_paragraph->flag_skip_label = !!skip_statements;
		current_paragraph->flag_dummy_paragraph = 1;
		current_paragraph->xref.skip = 1;
		emit_statement (CB_TREE (current_paragraph));
	}
	if (check_headers_present (COBC_HD_PROCEDURE_DIVISION, 0, 0, 0) == 1) {
		if (current_program->prog_type == COB_MODULE_TYPE_PROGRAM) {
			backup_current_pos ();
			emit_entry (current_program->program_id, 0, NULL, NULL);
		}
	}
  }
#line 17344 "parser.c" /* yacc.c:1646  */
    break;

  case 1270:
#line 9213 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 17352 "parser.c" /* yacc.c:1646  */
    break;

  case 1271:
#line 9217 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 17360 "parser.c" /* yacc.c:1646  */
    break;

  case 1330:
#line 9286 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (cb_verify (cb_next_sentence_phrase, "NEXT SENTENCE")) {
		cb_tree label;
		char	name[32];

		begin_statement ("NEXT SENTENCE", 0);
		sprintf (name, "L$%d", next_label_id);
		label = cb_build_reference (name);
		next_label_list = cb_list_add (next_label_list, label);
		emit_statement (cb_build_goto (label, NULL));
	}
	check_unreached = 0;
  }
#line 17378 "parser.c" /* yacc.c:1646  */
    break;

  case 1331:
#line 9300 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	yyerrok;
	cobc_cs_check = 0;
  }
#line 17387 "parser.c" /* yacc.c:1646  */
    break;

  case 1332:
#line 9311 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("ACCEPT", TERM_ACCEPT);
	cobc_cs_check = CB_CS_ACCEPT;
  }
#line 17396 "parser.c" /* yacc.c:1646  */
    break;

  case 1334:
#line 9321 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	  check_duplicate = 0;
	  check_line_col_duplicate = 0;
	  line_column = NULL;
  }
#line 17406 "parser.c" /* yacc.c:1646  */
    break;

  case 1335:
#line 9327 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	/* Check for invalid use of screen clauses */
	if (current_statement->attr_ptr
	    || (!is_screen_field ((yyvsp[-3])) && line_column)) {
		cb_verify_x ((yyvsp[-3]), cb_accept_display_extensions,
			     _("non-standard ACCEPT"));
	}

	if (cb_accept_update && !has_dispattr (COB_SCREEN_NO_UPDATE)) {
		set_dispattr (COB_SCREEN_UPDATE);
	}
	if (cb_accept_auto && !has_dispattr (COB_SCREEN_TAB)) {
		set_dispattr (COB_SCREEN_AUTO);
	}
	if ((yyvsp[-3]) == cb_null && current_statement->attr_ptr) {
		if (current_statement->attr_ptr->prompt) {
			emit_conflicting_clause_message ("ACCEPT OMITTED",
				_("PROMPT clause"));
		}
		if (current_statement->attr_ptr->size_is) {
			emit_conflicting_clause_message ("ACCEPT OMITTED",
				_("SIZE IS clause"));
		}
	}
	cobc_cs_check = 0;
	cb_emit_accept ((yyvsp[-3]), line_column, current_statement->attr_ptr);
  }
#line 17438 "parser.c" /* yacc.c:1646  */
    break;

  case 1336:
#line 9355 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	  check_duplicate = 0;
	  check_line_col_duplicate = 0;
	  line_column = NULL;
  }
#line 17448 "parser.c" /* yacc.c:1646  */
    break;

  case 1337:
#line 9361 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	CB_PENDING ("ACCEPT FROM SCREEN");
  }
#line 17457 "parser.c" /* yacc.c:1646  */
    break;

  case 1338:
#line 9366 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_line_or_col ((yyvsp[-2]), 0);
  }
#line 17465 "parser.c" /* yacc.c:1646  */
    break;

  case 1339:
#line 9370 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_line_or_col ((yyvsp[-2]), 1);
  }
#line 17473 "parser.c" /* yacc.c:1646  */
    break;

  case 1340:
#line 9374 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	cb_emit_accept_date_yyyymmdd ((yyvsp[-3]));
  }
#line 17482 "parser.c" /* yacc.c:1646  */
    break;

  case 1341:
#line 9379 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	cb_emit_accept_date ((yyvsp[-2]));
  }
#line 17491 "parser.c" /* yacc.c:1646  */
    break;

  case 1342:
#line 9384 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	cb_emit_accept_day_yyyyddd ((yyvsp[-3]));
  }
#line 17500 "parser.c" /* yacc.c:1646  */
    break;

  case 1343:
#line 9389 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	cb_emit_accept_day ((yyvsp[-2]));
  }
#line 17509 "parser.c" /* yacc.c:1646  */
    break;

  case 1344:
#line 9394 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_day_of_week ((yyvsp[-2]));
  }
#line 17517 "parser.c" /* yacc.c:1646  */
    break;

  case 1345:
#line 9400 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_escape_key ((yyvsp[-3]));
  }
#line 17525 "parser.c" /* yacc.c:1646  */
    break;

  case 1346:
#line 9406 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_exception_status ((yyvsp[-3]));
  }
#line 17533 "parser.c" /* yacc.c:1646  */
    break;

  case 1347:
#line 9410 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_time ((yyvsp[-2]));
  }
#line 17541 "parser.c" /* yacc.c:1646  */
    break;

  case 1348:
#line 9414 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
	cb_emit_accept_user_name ((yyvsp[-3]));
  }
#line 17550 "parser.c" /* yacc.c:1646  */
    break;

  case 1349:
#line 9419 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_command_line ((yyvsp[-2]));
  }
#line 17558 "parser.c" /* yacc.c:1646  */
    break;

  case 1350:
#line 9423 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_environment ((yyvsp[-3]));
  }
#line 17566 "parser.c" /* yacc.c:1646  */
    break;

  case 1351:
#line 9427 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_get_environment ((yyvsp[-1]), (yyvsp[-4]));
  }
#line 17574 "parser.c" /* yacc.c:1646  */
    break;

  case 1352:
#line 9431 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_arg_number ((yyvsp[-2]));
  }
#line 17582 "parser.c" /* yacc.c:1646  */
    break;

  case 1353:
#line 9435 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_arg_value ((yyvsp[-3]));
  }
#line 17590 "parser.c" /* yacc.c:1646  */
    break;

  case 1354:
#line 9439 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_mnemonic ((yyvsp[-2]), (yyvsp[0]));
  }
#line 17598 "parser.c" /* yacc.c:1646  */
    break;

  case 1355:
#line 9443 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_accept_name ((yyvsp[-2]), (yyvsp[0]));
  }
#line 17606 "parser.c" /* yacc.c:1646  */
    break;

  case 1356:
#line 9447 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("ACCEPT MESSAGE COUNT");
  }
#line 17614 "parser.c" /* yacc.c:1646  */
    break;

  case 1358:
#line 9455 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_null;
  }
#line 17622 "parser.c" /* yacc.c:1646  */
    break;

  case 1364:
#line 9473 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	  check_repeated ("FROM CRT", SYN_CLAUSE_2, &check_duplicate);
  }
#line 17630 "parser.c" /* yacc.c:1646  */
    break;

  case 1365:
#line 9477 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	  check_repeated ("MODE IS BLOCK", SYN_CLAUSE_3, &check_duplicate);
  }
#line 17638 "parser.c" /* yacc.c:1646  */
    break;

  case 1367:
#line 9482 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated (_("TIME-OUT or BEFORE TIME clauses"), SYN_CLAUSE_4,
			&check_duplicate);
	set_attribs (NULL, NULL, NULL, (yyvsp[0]), NULL, NULL, 0);
  }
#line 17648 "parser.c" /* yacc.c:1646  */
    break;

  case 1374:
#line 9507 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	set_attr_with_conflict ("LINE", SYN_CLAUSE_1,
				_("AT screen-location"), SYN_CLAUSE_3, 1,
				&check_line_col_duplicate);

	if ((CB_LITERAL_P ((yyvsp[0])) && cb_get_int ((yyvsp[0])) == 0) || (yyvsp[0]) == cb_zero) {
		cb_verify (cb_accept_display_extensions, "LINE 0");
	}

	if (!line_column) {
		line_column = CB_BUILD_PAIR ((yyvsp[0]), cb_int0);
	} else {
		CB_PAIR_X (line_column) = (yyvsp[0]);
	}
  }
#line 17668 "parser.c" /* yacc.c:1646  */
    break;

  case 1375:
#line 9523 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	set_attr_with_conflict ("COLUMN", SYN_CLAUSE_2,
				_("AT screen-location"), SYN_CLAUSE_3, 1,
				&check_line_col_duplicate);

	if ((CB_LITERAL_P ((yyvsp[0])) && cb_get_int ((yyvsp[0])) == 0) || (yyvsp[0]) == cb_zero) {
		cb_verify (cb_accept_display_extensions, "COLUMN 0");
	}

	if (!line_column) {
		line_column = CB_BUILD_PAIR (cb_int0, (yyvsp[0]));
	} else {
		CB_PAIR_Y (line_column) = (yyvsp[0]);
	}
  }
#line 17688 "parser.c" /* yacc.c:1646  */
    break;

  case 1376:
#line 9539 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	set_attr_with_conflict (_("AT screen-location"), SYN_CLAUSE_3,
				_("LINE or COLUMN"), SYN_CLAUSE_1 | SYN_CLAUSE_2,
				1, &check_line_col_duplicate);

	cb_verify (cb_accept_display_extensions, "AT clause");

	line_column = (yyvsp[0]);
  }
#line 17702 "parser.c" /* yacc.c:1646  */
    break;

  case 1377:
#line 9551 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 17708 "parser.c" /* yacc.c:1646  */
    break;

  case 1378:
#line 9555 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 17714 "parser.c" /* yacc.c:1646  */
    break;

  case 1379:
#line 9556 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 17720 "parser.c" /* yacc.c:1646  */
    break;

  case 1380:
#line 9561 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 17728 "parser.c" /* yacc.c:1646  */
    break;

  case 1381:
#line 9568 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("AUTO", SYN_CLAUSE_5, &check_duplicate);
	set_dispattr_with_conflict ("AUTO", COB_SCREEN_AUTO,
				    "TAB", COB_SCREEN_TAB);
  }
#line 17738 "parser.c" /* yacc.c:1646  */
    break;

  case 1382:
#line 9574 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("TAB", SYN_CLAUSE_6, &check_duplicate);
	set_dispattr_with_conflict ("TAB", COB_SCREEN_TAB,
				    "AUTO", COB_SCREEN_AUTO);
  }
#line 17748 "parser.c" /* yacc.c:1646  */
    break;

  case 1383:
#line 9580 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BELL", SYN_CLAUSE_7, &check_duplicate);
	set_dispattr (COB_SCREEN_BELL);
  }
#line 17757 "parser.c" /* yacc.c:1646  */
    break;

  case 1384:
#line 9585 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BLINK", SYN_CLAUSE_8, &check_duplicate);
	set_dispattr (COB_SCREEN_BLINK);
  }
#line 17766 "parser.c" /* yacc.c:1646  */
    break;

  case 1385:
#line 9590 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("CONVERSION", SYN_CLAUSE_9, &check_duplicate);
	CB_PENDING ("ACCEPT CONVERSION");
  }
#line 17775 "parser.c" /* yacc.c:1646  */
    break;

  case 1386:
#line 9595 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FULL", SYN_CLAUSE_10, &check_duplicate);
	set_dispattr (COB_SCREEN_FULL);
  }
#line 17784 "parser.c" /* yacc.c:1646  */
    break;

  case 1387:
#line 9600 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LEFTLINE", SYN_CLAUSE_12, &check_duplicate);
	set_dispattr (COB_SCREEN_LEFTLINE);
  }
#line 17793 "parser.c" /* yacc.c:1646  */
    break;

  case 1388:
#line 9605 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LOWER", SYN_CLAUSE_13, &check_duplicate);
	set_dispattr_with_conflict ("LOWER", COB_SCREEN_LOWER,
				    "UPPER", COB_SCREEN_UPPER);
  }
#line 17803 "parser.c" /* yacc.c:1646  */
    break;

  case 1389:
#line 9611 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("HIGHLIGHT", SYN_CLAUSE_11, &check_duplicate);
	set_dispattr_with_conflict ("HIGHLIGHT", COB_SCREEN_HIGHLIGHT,
				    "LOWLIGHT", COB_SCREEN_LOWLIGHT);
  }
#line 17813 "parser.c" /* yacc.c:1646  */
    break;

  case 1390:
#line 9617 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LOWLIGHT", SYN_CLAUSE_14, &check_duplicate);
	set_dispattr_with_conflict ("LOWLIGHT", COB_SCREEN_LOWLIGHT,
				    "HIGHLIGHT", COB_SCREEN_HIGHLIGHT);
  }
#line 17823 "parser.c" /* yacc.c:1646  */
    break;

  case 1391:
#line 9624 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("SAME phrase");
	/* may not be specified along with the UNDERLINED, BLINK, REVERSED,
	HIGH, LOW, STANDARD, COLOR, FOREGROUND-COLOR, or BACKGROUND-COLOR phrases */
  }
#line 17833 "parser.c" /* yacc.c:1646  */
    break;

  case 1392:
#line 9630 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("STANDARD intensity");
  }
#line 17841 "parser.c" /* yacc.c:1646  */
    break;

  case 1393:
#line 9634 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("BACKGROUND intensity");
  }
#line 17849 "parser.c" /* yacc.c:1646  */
    break;

  case 1394:
#line 9638 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("BACKGROUND intensity");
  }
#line 17857 "parser.c" /* yacc.c:1646  */
    break;

  case 1395:
#line 9642 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("BACKGROUND intensity");
  }
#line 17865 "parser.c" /* yacc.c:1646  */
    break;

  case 1396:
#line 9646 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (cb_no_echo_means_secure) {
		check_repeated ("SECURE", SYN_CLAUSE_20, &check_duplicate);
		set_dispattr (COB_SCREEN_SECURE);
	} else {
		check_repeated ("NO-ECHO", SYN_CLAUSE_15, &check_duplicate);
		set_dispattr_with_conflict ("NO-ECHO", COB_SCREEN_NO_ECHO,
					    "SECURE", COB_SCREEN_SECURE);
	}
  }
#line 17880 "parser.c" /* yacc.c:1646  */
    break;

  case 1397:
#line 9657 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("OVERLINE", SYN_CLAUSE_16, &check_duplicate);
	set_dispattr (COB_SCREEN_OVERLINE);
  }
#line 17889 "parser.c" /* yacc.c:1646  */
    break;

  case 1398:
#line 9662 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PROMPT", SYN_CLAUSE_17, &check_duplicate);
	set_attribs (NULL, NULL, NULL, NULL, (yyvsp[0]), NULL, COB_SCREEN_PROMPT);
  }
#line 17898 "parser.c" /* yacc.c:1646  */
    break;

  case 1399:
#line 9667 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("PROMPT", SYN_CLAUSE_17, &check_duplicate);
	set_dispattr (COB_SCREEN_PROMPT);
  }
#line 17907 "parser.c" /* yacc.c:1646  */
    break;

  case 1400:
#line 9672 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("REQUIRED", SYN_CLAUSE_18, &check_duplicate);
	set_dispattr (COB_SCREEN_REQUIRED);
  }
#line 17916 "parser.c" /* yacc.c:1646  */
    break;

  case 1401:
#line 9677 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("REVERSE-VIDEO", SYN_CLAUSE_19, &check_duplicate);
	set_dispattr (COB_SCREEN_REVERSE);
  }
#line 17925 "parser.c" /* yacc.c:1646  */
    break;

  case 1402:
#line 9682 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SECURE", SYN_CLAUSE_20, &check_duplicate);
	set_dispattr_with_conflict ("SECURE", COB_SCREEN_SECURE,
				    "NO-ECHO", COB_SCREEN_NO_ECHO);
  }
#line 17935 "parser.c" /* yacc.c:1646  */
    break;

  case 1403:
#line 9688 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SIZE", SYN_CLAUSE_21, &check_duplicate);
	set_attribs (NULL, NULL, NULL, NULL, NULL, (yyvsp[0]), 0);
  }
#line 17944 "parser.c" /* yacc.c:1646  */
    break;

  case 1404:
#line 9693 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("UNDERLINE", SYN_CLAUSE_22, &check_duplicate);
	set_dispattr (COB_SCREEN_UNDERLINE);
  }
#line 17953 "parser.c" /* yacc.c:1646  */
    break;

  case 1405:
#line 9698 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("NO UPDATE", SYN_CLAUSE_23, &check_duplicate);
	set_dispattr_with_conflict ("NO UPDATE", COB_SCREEN_NO_UPDATE,
				    "UPDATE", COB_SCREEN_UPDATE);
  }
#line 17963 "parser.c" /* yacc.c:1646  */
    break;

  case 1406:
#line 9704 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("UPDATE", SYN_CLAUSE_24, &check_duplicate);
	set_dispattr_with_conflict ("UPDATE", COB_SCREEN_UPDATE,
				    "NO UPDATE", COB_SCREEN_NO_UPDATE);
  }
#line 17973 "parser.c" /* yacc.c:1646  */
    break;

  case 1407:
#line 9710 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("UPPER", SYN_CLAUSE_25, &check_duplicate);
	set_dispattr_with_conflict ("UPPER", COB_SCREEN_UPPER,
				    "LOWER", COB_SCREEN_LOWER);
  }
#line 17983 "parser.c" /* yacc.c:1646  */
    break;

  case 1408:
#line 9716 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FOREGROUND-COLOR", SYN_CLAUSE_26, &check_duplicate);
	check_repeated ("BACKGROUND-COLOR", SYN_CLAUSE_27, &check_duplicate);
	CB_PENDING ("COLOR");
  }
#line 17993 "parser.c" /* yacc.c:1646  */
    break;

  case 1409:
#line 9722 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FOREGROUND-COLOR", SYN_CLAUSE_26, &check_duplicate);
	set_attribs ((yyvsp[0]), NULL, NULL, NULL, NULL, NULL, 0);
  }
#line 18002 "parser.c" /* yacc.c:1646  */
    break;

  case 1410:
#line 9727 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BACKGROUND-COLOR", SYN_CLAUSE_27, &check_duplicate);
	set_attribs (NULL, (yyvsp[0]), NULL, NULL, NULL, NULL, 0);
  }
#line 18011 "parser.c" /* yacc.c:1646  */
    break;

  case 1411:
#line 9732 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SCROLL UP", SYN_CLAUSE_28, &check_duplicate);
	set_attribs_with_conflict (NULL, NULL, (yyvsp[0]), NULL, NULL, NULL,
				   "SCROLL UP", COB_SCREEN_SCROLL_UP,
				   "SCROLL DOWN", COB_SCREEN_SCROLL_DOWN);
  }
#line 18022 "parser.c" /* yacc.c:1646  */
    break;

  case 1412:
#line 9739 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SCROLL DOWN", SYN_CLAUSE_19, &check_duplicate);
	set_attribs_with_conflict (NULL, NULL, (yyvsp[0]), NULL, NULL, NULL,
				   "SCROLL DOWN", COB_SCREEN_SCROLL_DOWN,
				   "SCROLL UP", COB_SCREEN_SCROLL_UP);
  }
#line 18033 "parser.c" /* yacc.c:1646  */
    break;

  case 1413:
#line 9746 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated (_("TIME-OUT or BEFORE TIME clauses"), SYN_CLAUSE_4,
			&check_duplicate);
	set_attribs (NULL, NULL, NULL, (yyvsp[0]), NULL, NULL, 0);
  }
#line 18043 "parser.c" /* yacc.c:1646  */
    break;

  case 1417:
#line 9759 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("CONTROL KEY", SYN_CLAUSE_29, &check_duplicate);
	CB_PENDING ("CONTROL KEY");
#if 0 /* should generate the following *after* the ACCEPT is finished */
	cb_emit_accept_escape_key ((yyvsp[0]));
#endif
  }
#line 18055 "parser.c" /* yacc.c:1646  */
    break;

  case 1426:
#line 9787 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), ACCEPT);
  }
#line 18063 "parser.c" /* yacc.c:1646  */
    break;

  case 1427:
#line 9791 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), ACCEPT);
# if 0 /* activate only for debugging purposes for attribs
	FIXME: Replace by DEBUG_LOG function */
	if (current_statement->attr_ptr) {
		print_bits (current_statement->attr_ptr->dispattrs);
	} else {
		fputs("No Attribs", stderr);
	}
#endif
  }
#line 18079 "parser.c" /* yacc.c:1646  */
    break;

  case 1428:
#line 9809 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("ADD", TERM_ADD);
  }
#line 18087 "parser.c" /* yacc.c:1646  */
    break;

  case 1430:
#line 9818 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), '+', cb_build_binary_list ((yyvsp[-3]), '+'));
  }
#line 18095 "parser.c" /* yacc.c:1646  */
    break;

  case 1431:
#line 9822 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-3])) {
		cb_list_add ((yyvsp[-4]), (yyvsp[-3]));
	}
	cb_emit_arithmetic ((yyvsp[-1]), 0, cb_build_binary_list ((yyvsp[-4]), '+'));
  }
#line 18106 "parser.c" /* yacc.c:1646  */
    break;

  case 1432:
#line 9829 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_corresponding (cb_build_add, (yyvsp[-2]), (yyvsp[-4]), (yyvsp[-1]));
  }
#line 18114 "parser.c" /* yacc.c:1646  */
    break;

  case 1433:
#line 9833 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("ADD TABLE");
	cb_emit_tab_arithmetic (cb_build_add, (yyvsp[-4]), (yyvsp[-6]), (yyvsp[-3]), (yyvsp[-2]), (yyvsp[-1]));
  }
#line 18123 "parser.c" /* yacc.c:1646  */
    break;

  case 1434:
#line 9840 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 18129 "parser.c" /* yacc.c:1646  */
    break;

  case 1435:
#line 9841 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 18135 "parser.c" /* yacc.c:1646  */
    break;

  case 1436:
#line 9846 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), ADD);
  }
#line 18143 "parser.c" /* yacc.c:1646  */
    break;

  case 1437:
#line 9850 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), ADD);
  }
#line 18151 "parser.c" /* yacc.c:1646  */
    break;

  case 1438:
#line 9860 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("ALLOCATE", 0);
	cobc_cs_check = CB_CS_ALLOCATE;
	current_statement->flag_no_based = 1;
  }
#line 18161 "parser.c" /* yacc.c:1646  */
    break;

  case 1440:
#line 9870 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_allocate ((yyvsp[-2]), (yyvsp[0]), NULL, (yyvsp[-1]));
  }
#line 18169 "parser.c" /* yacc.c:1646  */
    break;

  case 1441:
#line 9874 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0]) == NULL) {
		cb_error_x (CB_TREE (current_statement),
			    _("ALLOCATE CHARACTERS requires RETURNING clause"));
	} else {
		cb_emit_allocate (NULL, (yyvsp[0]), (yyvsp[-3]), (yyvsp[-1]));
	}
  }
#line 18182 "parser.c" /* yacc.c:1646  */
    break;

  case 1442:
#line 9885 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 18188 "parser.c" /* yacc.c:1646  */
    break;

  case 1443:
#line 9886 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 18194 "parser.c" /* yacc.c:1646  */
    break;

  case 1444:
#line 9894 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("ALTER", 0);
	cb_verify (cb_alter_statement, "ALTER");
  }
#line 18203 "parser.c" /* yacc.c:1646  */
    break;

  case 1448:
#line 9908 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_alter ((yyvsp[-3]), (yyvsp[0]));
  }
#line 18211 "parser.c" /* yacc.c:1646  */
    break;

  case 1451:
#line 9920 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("CALL", TERM_CALL);
	cobc_cs_check = CB_CS_CALL;
	call_nothing = 0;
	cobc_allow_program_name = 1;
	backup_current_pos ();
  }
#line 18223 "parser.c" /* yacc.c:1646  */
    break;

  case 1452:
#line 9929 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 18231 "parser.c" /* yacc.c:1646  */
    break;

  case 1453:
#line 9936 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_allow_program_name = 0;
  }
#line 18239 "parser.c" /* yacc.c:1646  */
    break;

  case 1454:
#line 9943 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	int call_conv = 0;
	int call_conv_local = 0;

	if (current_program->prog_type == COB_MODULE_TYPE_PROGRAM
	    && !current_program->flag_recursive
	    && is_recursive_call ((yyvsp[-5]))) {
		cb_warning_x (COBC_WARN_FILLER, (yyvsp[-5]),
			_("recursive program call - assuming RECURSIVE attribute"));
		current_program->flag_recursive = 1;
	}
	call_conv = current_call_convention;
	if ((CB_PAIR_X ((yyvsp[0])) != NULL)
	 && (call_conv & CB_CONV_STATIC_LINK)) {
		cb_warning_x (COBC_WARN_FILLER, (yyvsp[-5]),
		    _("STATIC CALL convention ignored because of ON EXCEPTION"));
		call_conv &= ~CB_CONV_STATIC_LINK;
	}
	if ((yyvsp[-7])) {
		if (CB_INTEGER_P ((yyvsp[-7]))) {
			call_conv_local = CB_INTEGER ((yyvsp[-7]))->val;
			if ((CB_PAIR_X ((yyvsp[0])) != NULL)
			 && (call_conv_local & CB_CONV_STATIC_LINK)) {
				cb_error_x ((yyvsp[-7]), _("%s and %s are mutually exclusive"),
					"STATIC CALL", "ON EXCEPTION");
				call_conv_local &= ~CB_CONV_STATIC_LINK;
			}
			call_conv |= call_conv_local;
			if (CB_INTEGER ((yyvsp[-7]))->val & CB_CONV_COBOL) {
				call_conv &= ~CB_CONV_STDCALL;
			} else {
				call_conv &= ~CB_CONV_COBOL;
			}
		} else {
			call_conv = cb_get_int((yyvsp[-7]));
		}
	}
	/* For CALL ... RETURNING NOTHING, set the call convention bit */
	if (call_nothing) {
		call_conv |= CB_CONV_NO_RET_UPD;
	}
	cb_emit_call ((yyvsp[-5]), (yyvsp[-2]), (yyvsp[-1]), CB_PAIR_X ((yyvsp[0])), CB_PAIR_Y ((yyvsp[0])),
		      cb_int (call_conv), (yyvsp[-6]), (yyvsp[-3]), backup_source_line);
  }
#line 18288 "parser.c" /* yacc.c:1646  */
    break;

  case 1455:
#line 9991 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 18296 "parser.c" /* yacc.c:1646  */
    break;

  case 1456:
#line 9995 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (current_call_convention & CB_CONV_COBOL) {
		(yyval) = cb_int (CB_CONV_STATIC_LINK | CB_CONV_COBOL);
	} else {
		(yyval) = cb_int (CB_CONV_STATIC_LINK);
	}
  }
#line 18308 "parser.c" /* yacc.c:1646  */
    break;

  case 1457:
#line 10003 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (CB_CONV_STDCALL);
  }
#line 18316 "parser.c" /* yacc.c:1646  */
    break;

  case 1458:
#line 10007 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (0);
  }
#line 18324 "parser.c" /* yacc.c:1646  */
    break;

  case 1459:
#line 10011 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	x = cb_ref ((yyvsp[0]));
	if (CB_VALID_TREE (x)) {
		if (CB_SYSTEM_NAME(x)->token != CB_FEATURE_CONVENTION) {
			cb_error_x ((yyvsp[0]), _("invalid mnemonic name"));
			(yyval) = NULL;
		} else {
			(yyval) = CB_SYSTEM_NAME(x)->value;
		}
	} else {
		(yyval) = NULL;
	}
  }
#line 18344 "parser.c" /* yacc.c:1646  */
    break;

  case 1460:
#line 10030 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_LITERAL_P ((yyvsp[0]))) {
		cb_trim_program_id ((yyvsp[0]));
	}
  }
#line 18354 "parser.c" /* yacc.c:1646  */
    break;

  case 1461:
#line 10036 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_program_prototypes, _("CALL/CANCEL with program-prototype-name"));
	/* hack to push the prototype name */
	if ((yyvsp[0]) && CB_REFERENCE_P ((yyvsp[0]))) {
		if ((yyvsp[-1])) {
			cb_warning_x (COBC_WARN_FILLER, (yyvsp[-1]), _("id/literal ignored, using prototype name"));
		}
		(yyval) = (yyvsp[0]);
	} else if ((yyvsp[-1]) && CB_LITERAL_P ((yyvsp[-1]))) {
		(yyval) = (yyvsp[-1]);
	} else {
		cb_error (_("NESTED phrase is only valid with literal"));
		(yyval) = cb_error_node;
	}
  }
#line 18374 "parser.c" /* yacc.c:1646  */
    break;

  case 1462:
#line 10055 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 18382 "parser.c" /* yacc.c:1646  */
    break;

  case 1463:
#line 10060 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_LITERAL_P ((yyvsp[-1]))) {
		cb_trim_program_id ((yyvsp[-1]));
	}
	(yyval) = (yyvsp[-1]);
  }
#line 18393 "parser.c" /* yacc.c:1646  */
    break;

  case 1464:
#line 10070 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("NESTED phrase for CALL statement");
  }
#line 18401 "parser.c" /* yacc.c:1646  */
    break;

  case 1466:
#line 10078 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 18409 "parser.c" /* yacc.c:1646  */
    break;

  case 1467:
#line 10082 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	call_mode = CB_CALL_BY_REFERENCE;
	size_mode = CB_SIZE_4;
  }
#line 18418 "parser.c" /* yacc.c:1646  */
    break;

  case 1468:
#line 10087 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (cb_list_length ((yyvsp[0])) > MAX_CALL_FIELD_PARAMS) {
		cb_error_x (CB_TREE (current_statement),
			    _("number of parameters exceeds maximum %d"),
			    MAX_CALL_FIELD_PARAMS);
	}
	(yyval) = (yyvsp[0]);
  }
#line 18431 "parser.c" /* yacc.c:1646  */
    break;

  case 1469:
#line 10098 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 18437 "parser.c" /* yacc.c:1646  */
    break;

  case 1470:
#line 10100 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0])); }
#line 18443 "parser.c" /* yacc.c:1646  */
    break;

  case 1471:
#line 10105 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (call_mode != CB_CALL_BY_REFERENCE) {
		cb_error_x (CB_TREE (current_statement),
			    _("OMITTED only allowed when parameters are passed BY REFERENCE"));
	}
	(yyval) = CB_BUILD_PAIR (cb_int (call_mode), cb_null);
  }
#line 18455 "parser.c" /* yacc.c:1646  */
    break;

  case 1472:
#line 10113 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	int	save_mode;	/* internal single parameter only mode */

	save_mode = call_mode;
	if (call_mode != CB_CALL_BY_REFERENCE) {
		if (CB_FILE_P ((yyvsp[0])) || (CB_REFERENCE_P ((yyvsp[0])) &&
		    CB_FILE_P (CB_REFERENCE ((yyvsp[0]))->value))) {
			cb_error_x (CB_TREE (current_statement),
				    _("invalid file name reference"));
		} else if (call_mode == CB_CALL_BY_VALUE) {
			/* FIXME: compiler configuration needed, IBM allows one-byte
			          alphanumeric items [--> a `char`], too, while
			          COBOL 2002/2014 allow only numeric literals
			   --> revise after rw-merge */
			if (cb_category_is_alpha ((yyvsp[0]))) {
				cb_warning_x (COBC_WARN_FILLER, (yyvsp[0]),
					      _("BY CONTENT assumed for alphanumeric item '%s'"),
						  cb_name ((yyvsp[0])));
				call_mode = CB_CALL_BY_CONTENT;
			}
		}
	}
	(yyval) = CB_BUILD_PAIR (cb_int (call_mode), (yyvsp[0]));
	CB_SIZES ((yyval)) = size_mode;
	call_mode = save_mode;
  }
#line 18486 "parser.c" /* yacc.c:1646  */
    break;

  case 1474:
#line 10144 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	call_mode = CB_CALL_BY_REFERENCE;
  }
#line 18494 "parser.c" /* yacc.c:1646  */
    break;

  case 1475:
#line 10148 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (current_program->flag_chained) {
		cb_error_x (CB_TREE (current_statement),
			    _("%s not allowed in CHAINED programs"), "BY CONTENT");
	} else {
		call_mode = CB_CALL_BY_CONTENT;
	}
  }
#line 18507 "parser.c" /* yacc.c:1646  */
    break;

  case 1476:
#line 10157 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (current_program->flag_chained) {
		cb_error_x (CB_TREE (current_statement),
			    _("%s not allowed in CHAINED programs"), "BY VALUE");
	} else {
		call_mode = CB_CALL_BY_VALUE;
	}
  }
#line 18520 "parser.c" /* yacc.c:1646  */
    break;

  case 1477:
#line 10169 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 18528 "parser.c" /* yacc.c:1646  */
    break;

  case 1478:
#line 10173 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 18536 "parser.c" /* yacc.c:1646  */
    break;

  case 1479:
#line 10177 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_null;
  }
#line 18544 "parser.c" /* yacc.c:1646  */
    break;

  case 1480:
#line 10181 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	call_nothing = CB_CONV_NO_RET_UPD;
	(yyval) = cb_null;
  }
#line 18553 "parser.c" /* yacc.c:1646  */
    break;

  case 1481:
#line 10186 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	struct cb_field	*f;

	if (cb_ref ((yyvsp[0])) != cb_error_node) {
		f = CB_FIELD_PTR ((yyvsp[0]));
		if (f->level != 1 && f->level != 77) {
			cb_error (_("RETURNING item must have level 01 or 77"));
			(yyval) = NULL;
		} else if (f->storage != CB_STORAGE_LINKAGE &&
			   !f->flag_item_based) {
			cb_error (_("RETURNING item must be a LINKAGE SECTION item or have BASED clause"));
			(yyval) = NULL;
		} else {
			(yyval) = cb_build_address ((yyvsp[0]));
		}
	} else {
		(yyval) = NULL;
	}
  }
#line 18577 "parser.c" /* yacc.c:1646  */
    break;

  case 1486:
#line 10219 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR (NULL, NULL);
  }
#line 18585 "parser.c" /* yacc.c:1646  */
    break;

  case 1487:
#line 10223 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[-1]), (yyvsp[0]));
  }
#line 18593 "parser.c" /* yacc.c:1646  */
    break;

  case 1488:
#line 10227 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT EXCEPTION before EXCEPTION"));
	}
	(yyval) = CB_BUILD_PAIR ((yyvsp[0]), (yyvsp[-1]));
  }
#line 18605 "parser.c" /* yacc.c:1646  */
    break;

  case 1489:
#line 10238 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 18613 "parser.c" /* yacc.c:1646  */
    break;

  case 1490:
#line 10242 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 18621 "parser.c" /* yacc.c:1646  */
    break;

  case 1491:
#line 10249 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 18629 "parser.c" /* yacc.c:1646  */
    break;

  case 1492:
#line 10253 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_call_overflow, "ON OVERFLOW");
	(yyval) = (yyvsp[0]);
  }
#line 18638 "parser.c" /* yacc.c:1646  */
    break;

  case 1493:
#line 10261 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 18646 "parser.c" /* yacc.c:1646  */
    break;

  case 1494:
#line 10265 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 18654 "parser.c" /* yacc.c:1646  */
    break;

  case 1495:
#line 10272 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 18662 "parser.c" /* yacc.c:1646  */
    break;

  case 1496:
#line 10279 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), CALL);
  }
#line 18670 "parser.c" /* yacc.c:1646  */
    break;

  case 1497:
#line 10283 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), CALL);
  }
#line 18678 "parser.c" /* yacc.c:1646  */
    break;

  case 1498:
#line 10293 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("CANCEL", 0);
	cobc_allow_program_name = 1;
  }
#line 18687 "parser.c" /* yacc.c:1646  */
    break;

  case 1499:
#line 10298 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_allow_program_name = 0;
  }
#line 18695 "parser.c" /* yacc.c:1646  */
    break;

  case 1500:
#line 10305 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_cancel ((yyvsp[0]));
  }
#line 18703 "parser.c" /* yacc.c:1646  */
    break;

  case 1501:
#line 10309 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_cancel ((yyvsp[0]));
  }
#line 18711 "parser.c" /* yacc.c:1646  */
    break;

  case 1503:
#line 10317 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_program_prototypes, _("CALL/CANCEL with program-prototype-name"));
  }
#line 18719 "parser.c" /* yacc.c:1646  */
    break;

  case 1504:
#line 10326 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("CLOSE", 0);
  }
#line 18727 "parser.c" /* yacc.c:1646  */
    break;

  case 1508:
#line 10339 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	cb_emit_close ((yyvsp[-1]), (yyvsp[0]));
  }
#line 18736 "parser.c" /* yacc.c:1646  */
    break;

  case 1509:
#line 10344 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	cb_emit_close ((yyvsp[-1]), (yyvsp[0]));
  }
#line 18745 "parser.c" /* yacc.c:1646  */
    break;

  case 1510:
#line 10351 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_CLOSE_NORMAL); }
#line 18751 "parser.c" /* yacc.c:1646  */
    break;

  case 1511:
#line 10352 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_CLOSE_UNIT); }
#line 18757 "parser.c" /* yacc.c:1646  */
    break;

  case 1512:
#line 10353 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_CLOSE_UNIT_REMOVAL); }
#line 18763 "parser.c" /* yacc.c:1646  */
    break;

  case 1513:
#line 10354 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_CLOSE_NO_REWIND); }
#line 18769 "parser.c" /* yacc.c:1646  */
    break;

  case 1514:
#line 10355 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_CLOSE_LOCK); }
#line 18775 "parser.c" /* yacc.c:1646  */
    break;

  case 1515:
#line 10360 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("GRAPHICAL WINDOW");
	current_statement->name = "CLOSE WINDOW";
  }
#line 18784 "parser.c" /* yacc.c:1646  */
    break;

  case 1516:
#line 10365 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_close_window ((yyvsp[-1]), (yyvsp[0]));
  }
#line 18792 "parser.c" /* yacc.c:1646  */
    break;

  case 1517:
#line 10371 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 18798 "parser.c" /* yacc.c:1646  */
    break;

  case 1518:
#line 10372 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 18804 "parser.c" /* yacc.c:1646  */
    break;

  case 1519:
#line 10380 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("COMPUTE", TERM_COMPUTE);
  }
#line 18812 "parser.c" /* yacc.c:1646  */
    break;

  case 1521:
#line 10389 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-3]), 0, (yyvsp[-1]));
  }
#line 18820 "parser.c" /* yacc.c:1646  */
    break;

  case 1522:
#line 10396 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), COMPUTE);
  }
#line 18828 "parser.c" /* yacc.c:1646  */
    break;

  case 1523:
#line 10400 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), COMPUTE);
  }
#line 18836 "parser.c" /* yacc.c:1646  */
    break;

  case 1524:
#line 10410 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("COMMIT", 0);
	cb_emit_commit ();
  }
#line 18845 "parser.c" /* yacc.c:1646  */
    break;

  case 1525:
#line 10421 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	size_t	save_unreached;

	/* Do not check unreached for CONTINUE */
	save_unreached = check_unreached;
	check_unreached = 0;
	begin_statement ("CONTINUE", 0);
	cb_emit_continue ();
	check_unreached = (unsigned int) save_unreached;
  }
#line 18860 "parser.c" /* yacc.c:1646  */
    break;

  case 1526:
#line 10438 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("DESTROY", 0);
	CB_PENDING ("GRAPHICAL CONTROL");
  }
#line 18869 "parser.c" /* yacc.c:1646  */
    break;

  case 1528:
#line 10447 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_destroy (NULL);
  }
#line 18877 "parser.c" /* yacc.c:1646  */
    break;

  case 1529:
#line 10454 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_destroy ((yyvsp[0]));
  }
#line 18885 "parser.c" /* yacc.c:1646  */
    break;

  case 1530:
#line 10464 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("DELETE", TERM_DELETE);
  }
#line 18893 "parser.c" /* yacc.c:1646  */
    break;

  case 1532:
#line 10473 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_delete ((yyvsp[-3]));
  }
#line 18901 "parser.c" /* yacc.c:1646  */
    break;

  case 1534:
#line 10481 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	cb_emit_delete_file ((yyvsp[0]));
  }
#line 18910 "parser.c" /* yacc.c:1646  */
    break;

  case 1535:
#line 10486 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	cb_emit_delete_file ((yyvsp[0]));
  }
#line 18919 "parser.c" /* yacc.c:1646  */
    break;

  case 1536:
#line 10494 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), DELETE);
  }
#line 18927 "parser.c" /* yacc.c:1646  */
    break;

  case 1537:
#line 10498 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), DELETE);
  }
#line 18935 "parser.c" /* yacc.c:1646  */
    break;

  case 1538:
#line 10508 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("DISABLE", 0);
  }
#line 18943 "parser.c" /* yacc.c:1646  */
    break;

  case 1542:
#line 10522 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	  /* Add cb_verify for <= COBOL-85 */
  }
#line 18951 "parser.c" /* yacc.c:1646  */
    break;

  case 1548:
#line 10540 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("DISPLAY", TERM_DISPLAY);
	cobc_cs_check = CB_CS_DISPLAY;
	display_type = UNKNOWN_DISPLAY;
	is_first_display_item = 1;
  }
#line 18962 "parser.c" /* yacc.c:1646  */
    break;

  case 1550:
#line 10552 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_env_name ((yyvsp[-2]));
  }
#line 18970 "parser.c" /* yacc.c:1646  */
    break;

  case 1551:
#line 10556 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_env_value ((yyvsp[-2]));
  }
#line 18978 "parser.c" /* yacc.c:1646  */
    break;

  case 1552:
#line 10560 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_arg_number ((yyvsp[-2]));
  }
#line 18986 "parser.c" /* yacc.c:1646  */
    break;

  case 1553:
#line 10564 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_command_line ((yyvsp[-2]));
  }
#line 18994 "parser.c" /* yacc.c:1646  */
    break;

  case 1559:
#line 10576 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0]) != NULL) {
		error_if_different_display_type ((yyvsp[0]), NULL, NULL, NULL);
		cb_emit_display ((yyvsp[0]), NULL, cb_int1, NULL, NULL, 0,
				 display_type);
	}
  }
#line 19006 "parser.c" /* yacc.c:1646  */
    break;

  case 1560:
#line 10584 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	set_display_type ((yyvsp[0]), NULL, NULL, NULL);
	cb_emit_display ((yyvsp[0]), NULL, cb_int1, NULL, NULL, 1,
			 display_type);
  }
#line 19016 "parser.c" /* yacc.c:1646  */
    break;

  case 1563:
#line 10598 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_duplicate = 0;
	check_line_col_duplicate = 0;
	advancing_value = cb_int1;
	upon_value = NULL;
	line_column = NULL;
  }
#line 19028 "parser.c" /* yacc.c:1646  */
    break;

  case 1564:
#line 10606 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-2]) == cb_null) {
		/* Emit DISPLAY OMITTED. */
		CB_UNFINISHED_X (CB_TREE(current_statement), "DISPLAY OMITTED");
		error_if_no_advancing_in_screen_display (advancing_value);
	}

	/* Emit device or screen DISPLAY. */

	/*
	  Check that disp_list does not contain an invalid mix of fields.
	*/
	if (display_type == UNKNOWN_DISPLAY) {
		set_display_type ((yyvsp[-2]), upon_value, line_column,
				  current_statement->attr_ptr);
	} else {
		error_if_different_display_type ((yyvsp[-2]), upon_value,
						 line_column,
						 current_statement->attr_ptr);
	}

	if (display_type == SCREEN_DISPLAY
	    || display_type == FIELD_ON_SCREEN_DISPLAY) {
		error_if_no_advancing_in_screen_display (advancing_value);
	}

	cb_emit_display ((yyvsp[-2]), upon_value, advancing_value, line_column,
			 current_statement->attr_ptr,
			 is_first_display_item, display_type);

	is_first_display_item = 0;
  }
#line 19065 "parser.c" /* yacc.c:1646  */
    break;

  case 1565:
#line 10642 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 19073 "parser.c" /* yacc.c:1646  */
    break;

  case 1566:
#line 10646 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_null;
  }
#line 19081 "parser.c" /* yacc.c:1646  */
    break;

  case 1569:
#line 10658 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("UPON", SYN_CLAUSE_1, &check_duplicate);
  }
#line 19089 "parser.c" /* yacc.c:1646  */
    break;

  case 1570:
#line 10662 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("NO ADVANCING", SYN_CLAUSE_2, &check_duplicate);
	advancing_value = cb_int0;
  }
#line 19098 "parser.c" /* yacc.c:1646  */
    break;

  case 1571:
#line 10667 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("MODE IS BLOCK", SYN_CLAUSE_3, &check_duplicate);
  }
#line 19106 "parser.c" /* yacc.c:1646  */
    break;

  case 1574:
#line 10676 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	upon_value = cb_build_display_mnemonic ((yyvsp[0]));
  }
#line 19114 "parser.c" /* yacc.c:1646  */
    break;

  case 1575:
#line 10680 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	upon_value = cb_build_display_name ((yyvsp[0]));
  }
#line 19122 "parser.c" /* yacc.c:1646  */
    break;

  case 1576:
#line 10684 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	upon_value = cb_int2;
  }
#line 19130 "parser.c" /* yacc.c:1646  */
    break;

  case 1577:
#line 10688 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	upon_value = cb_null;
  }
#line 19138 "parser.c" /* yacc.c:1646  */
    break;

  case 1580:
#line 10700 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_UNFINISHED_X (CB_TREE(current_statement), "DISPLAY MESSAGE");
	upon_value = NULL;
  }
#line 19147 "parser.c" /* yacc.c:1646  */
    break;

  case 1581:
#line 10705 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	/* for now: minimal support for display and prompt only */
	if (upon_value) {
		cb_emit_display (CB_LIST_INIT (upon_value), NULL, NULL, NULL,
				 NULL, 1, FIELD_ON_SCREEN_DISPLAY);
	}
	cb_emit_display ((yyvsp[-2]), NULL, NULL, NULL,
			 NULL, 1, FIELD_ON_SCREEN_DISPLAY);
	cb_emit_accept (cb_null, NULL, NULL);
  }
#line 19162 "parser.c" /* yacc.c:1646  */
    break;

  case 1586:
#line 10729 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	upon_value = (yyvsp[0]);
  }
#line 19170 "parser.c" /* yacc.c:1646  */
    break;

  case 1591:
#line 10740 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("GRAPHICAL WINDOW");
	current_statement->name = "DISPLAY WINDOW";
  }
#line 19179 "parser.c" /* yacc.c:1646  */
    break;

  case 1592:
#line 10745 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_duplicate = 0;
	check_line_col_duplicate = 0;
	line_column = NULL;
	upon_value = NULL; /* Hack: stores the POP-UP AREA */
  }
#line 19190 "parser.c" /* yacc.c:1646  */
    break;

  case 1593:
#line 10752 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_display_window (NULL, upon_value, (yyvsp[-2]), line_column,
			 current_statement->attr_ptr);
  }
#line 19199 "parser.c" /* yacc.c:1646  */
    break;

  case 1596:
#line 10765 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("GRAPHICAL WINDOW");
	current_statement->name = "DISPLAY FLOATING WINDOW";
  }
#line 19208 "parser.c" /* yacc.c:1646  */
    break;

  case 1597:
#line 10770 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_duplicate = 0;
	check_line_col_duplicate = 0;
	line_column = NULL;
	upon_value = NULL; /* Hack: stores the POP-UP AREA */
  }
#line 19219 "parser.c" /* yacc.c:1646  */
    break;

  case 1598:
#line 10777 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-5])) {
		/* TODO: set "CELL WIDTH" and "CELL HEIGHT" to "LABEL FONT" */
		/* if not set already */
	}
	cb_emit_display_window (cb_int0, upon_value, (yyvsp[-2]), line_column,
			 current_statement->attr_ptr);
  }
#line 19232 "parser.c" /* yacc.c:1646  */
    break;

  case 1599:
#line 10789 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("GRAPHICAL WINDOW");
	current_statement->name = "DISPLAY INITIAL WINDOW";
	check_duplicate = 0;
	check_line_col_duplicate = 0;
	line_column = NULL;
	upon_value = NULL; /* Hack: stores the POP-UP AREA */
  }
#line 19245 "parser.c" /* yacc.c:1646  */
    break;

  case 1600:
#line 10798 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-3])) {
		/* TODO: set "CELL WIDTH" and "CELL HEIGHT" to "LABEL FONT" */
		/* if not set already */
	}
	cb_emit_display_window ((yyvsp[-4]), upon_value, NULL, line_column,
			 current_statement->attr_ptr);
  }
#line 19258 "parser.c" /* yacc.c:1646  */
    break;

  case 1601:
#line 10809 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {(yyval) = cb_int1;}
#line 19264 "parser.c" /* yacc.c:1646  */
    break;

  case 1602:
#line 10810 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {(yyval) = cb_int2;}
#line 19270 "parser.c" /* yacc.c:1646  */
    break;

  case 1603:
#line 10811 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {(yyval) = cb_int3;}
#line 19276 "parser.c" /* yacc.c:1646  */
    break;

  case 1604:
#line 10815 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {(yyval) = NULL;}
#line 19282 "parser.c" /* yacc.c:1646  */
    break;

  case 1605:
#line 10816 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {(yyval) = cb_int1;}
#line 19288 "parser.c" /* yacc.c:1646  */
    break;

  case 1606:
#line 10821 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 19296 "parser.c" /* yacc.c:1646  */
    break;

  case 1607:
#line 10825 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 19304 "parser.c" /* yacc.c:1646  */
    break;

  case 1608:
#line 10832 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	struct cb_field	*f;

	if (cb_ref ((yyvsp[0])) != cb_error_node) {
		f = CB_FIELD_PTR ((yyvsp[0]));
		if (f->usage != CB_USAGE_HNDL_WINDOW
		 && f->usage != CB_USAGE_HNDL_SUBWINDOW) {
			cb_error_x ((yyvsp[0]), _("HANDLE must be a %s HANDLE"), "WINDOW");
		}
	}
	(yyval) = (yyvsp[0]);
  }
#line 19321 "parser.c" /* yacc.c:1646  */
    break;

  case 1609:
#line 10845 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	struct cb_field	*f;

	if (cb_ref ((yyvsp[0])) != cb_error_node) {
		f = CB_FIELD_PTR ((yyvsp[0]));
		if (f->usage != CB_USAGE_HNDL) {
			cb_error_x ((yyvsp[0]), _("HANDLE must be a generic HANDLE"));
		}
	}
	(yyval) = (yyvsp[0]);
  }
#line 19337 "parser.c" /* yacc.c:1646  */
    break;

  case 1610:
#line 10857 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_null;
  }
#line 19345 "parser.c" /* yacc.c:1646  */
    break;

  case 1614:
#line 10872 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	/* TODO: store */
  }
#line 19353 "parser.c" /* yacc.c:1646  */
    break;

  case 1626:
#line 10900 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (upon_value) {
		emit_duplicate_clause_message("POP-UP AREA");
	}
	upon_value = (yyvsp[0]);
  }
#line 19364 "parser.c" /* yacc.c:1646  */
    break;

  case 1627:
#line 10910 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (!strcmp (current_statement->name, "DISPLAY WINDOW")) {
		cb_error_x ((yyvsp[0]), _("HANDLE clause invalid for %s"),
			current_statement->name);
		upon_value = cb_error_node;
	} else{
		if (upon_value) {
			emit_duplicate_clause_message("POP-UP AREA / HANDLE IN");
		}
		upon_value = (yyvsp[0]);
	}
  }
#line 19381 "parser.c" /* yacc.c:1646  */
    break;

  case 1628:
#line 10926 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BELL", SYN_CLAUSE_4, &check_duplicate);
	set_dispattr (COB_SCREEN_BELL);
  }
#line 19390 "parser.c" /* yacc.c:1646  */
    break;

  case 1629:
#line 10931 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BLANK LINE", SYN_CLAUSE_5, &check_duplicate);
	set_dispattr_with_conflict ("BLANK LINE", COB_SCREEN_BLANK_LINE,
				    "BLANK SCREEN", COB_SCREEN_BLANK_SCREEN);
  }
#line 19400 "parser.c" /* yacc.c:1646  */
    break;

  case 1630:
#line 10937 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BLANK SCREEN", SYN_CLAUSE_6, &check_duplicate);
	set_dispattr_with_conflict ("BLANK SCREEN", COB_SCREEN_BLANK_SCREEN,
				    "BLANK LINE", COB_SCREEN_BLANK_LINE);
  }
#line 19410 "parser.c" /* yacc.c:1646  */
    break;

  case 1631:
#line 10943 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BLINK", SYN_CLAUSE_7, &check_duplicate);
	set_dispattr (COB_SCREEN_BLINK);
  }
#line 19419 "parser.c" /* yacc.c:1646  */
    break;

  case 1632:
#line 10948 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("CONVERSION", SYN_CLAUSE_8, &check_duplicate);
	cb_warning (COBC_WARN_FILLER, _("ignoring CONVERSION"));
  }
#line 19428 "parser.c" /* yacc.c:1646  */
    break;

  case 1633:
#line 10953 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ERASE EOL", SYN_CLAUSE_9, &check_duplicate);
	set_dispattr_with_conflict ("ERASE EOL", COB_SCREEN_ERASE_EOL,
				    "ERASE EOS", COB_SCREEN_ERASE_EOS);
  }
#line 19438 "parser.c" /* yacc.c:1646  */
    break;

  case 1634:
#line 10959 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("ERASE EOS", SYN_CLAUSE_10, &check_duplicate);
	set_dispattr_with_conflict ("ERASE EOS", COB_SCREEN_ERASE_EOS,
				    "ERASE EOL", COB_SCREEN_ERASE_EOL);
  }
#line 19448 "parser.c" /* yacc.c:1646  */
    break;

  case 1635:
#line 10965 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("HIGHLIGHT", SYN_CLAUSE_11, &check_duplicate);
	set_dispattr_with_conflict ("HIGHLIGHT", COB_SCREEN_HIGHLIGHT,
				    "LOWLIGHT", COB_SCREEN_LOWLIGHT);
  }
#line 19458 "parser.c" /* yacc.c:1646  */
    break;

  case 1636:
#line 10971 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("LOWLIGHT", SYN_CLAUSE_12, &check_duplicate);
	set_dispattr_with_conflict ("LOWLIGHT", COB_SCREEN_LOWLIGHT,
				    "HIGHLIGHT", COB_SCREEN_HIGHLIGHT);
  }
#line 19468 "parser.c" /* yacc.c:1646  */
    break;

  case 1637:
#line 10978 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("SAME phrase");
	/* may not be specified along with the UNDERLINED, BLINK, REVERSED,
	HIGH, LOW, STANDARD, COLOR, FOREGROUND-COLOR, or BACKGROUND-COLOR phrases */
  }
#line 19478 "parser.c" /* yacc.c:1646  */
    break;

  case 1638:
#line 10984 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("STANDARD intensity");
  }
#line 19486 "parser.c" /* yacc.c:1646  */
    break;

  case 1639:
#line 10988 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("BACKGROUND intensity");
  }
#line 19494 "parser.c" /* yacc.c:1646  */
    break;

  case 1640:
#line 10992 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("BACKGROUND intensity");
  }
#line 19502 "parser.c" /* yacc.c:1646  */
    break;

  case 1641:
#line 10996 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING("BACKGROUND intensity");
  }
#line 19510 "parser.c" /* yacc.c:1646  */
    break;

  case 1642:
#line 11000 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("OVERLINE", SYN_CLAUSE_13, &check_duplicate);
	set_dispattr (COB_SCREEN_OVERLINE);
  }
#line 19519 "parser.c" /* yacc.c:1646  */
    break;

  case 1643:
#line 11005 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("REVERSE-VIDEO", SYN_CLAUSE_14, &check_duplicate);
	set_dispattr (COB_SCREEN_REVERSE);
  }
#line 19528 "parser.c" /* yacc.c:1646  */
    break;

  case 1644:
#line 11010 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SIZE", SYN_CLAUSE_15, &check_duplicate);
	set_attribs (NULL, NULL, NULL, NULL, NULL, (yyvsp[0]), 0);
  }
#line 19537 "parser.c" /* yacc.c:1646  */
    break;

  case 1645:
#line 11015 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("UNDERLINE", SYN_CLAUSE_16, &check_duplicate);
	set_dispattr (COB_SCREEN_UNDERLINE);
  }
#line 19546 "parser.c" /* yacc.c:1646  */
    break;

  case 1646:
#line 11020 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FOREGROUND-COLOR", SYN_CLAUSE_17, &check_duplicate);
	check_repeated ("BACKGROUND-COLOR", SYN_CLAUSE_18, &check_duplicate);
	CB_PENDING ("COLOR");
  }
#line 19556 "parser.c" /* yacc.c:1646  */
    break;

  case 1647:
#line 11026 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("FOREGROUND-COLOR", SYN_CLAUSE_17, &check_duplicate);
	set_attribs ((yyvsp[0]), NULL, NULL, NULL, NULL, NULL, 0);
  }
#line 19565 "parser.c" /* yacc.c:1646  */
    break;

  case 1648:
#line 11031 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("BACKGROUND-COLOR", SYN_CLAUSE_18, &check_duplicate);
	set_attribs (NULL, (yyvsp[0]), NULL, NULL, NULL, NULL, 0);
  }
#line 19574 "parser.c" /* yacc.c:1646  */
    break;

  case 1649:
#line 11036 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SCROLL UP", SYN_CLAUSE_19, &check_duplicate);
	set_attribs_with_conflict (NULL, NULL, (yyvsp[0]), NULL, NULL, NULL,
				   "SCROLL UP", COB_SCREEN_SCROLL_UP,
				   "SCROLL DOWN", COB_SCREEN_SCROLL_DOWN);
  }
#line 19585 "parser.c" /* yacc.c:1646  */
    break;

  case 1650:
#line 11043 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_repeated ("SCROLL DOWN", SYN_CLAUSE_20, &check_duplicate);
	set_attribs_with_conflict (NULL, NULL, (yyvsp[0]), NULL, NULL, NULL,
				   "SCROLL DOWN", COB_SCREEN_SCROLL_DOWN,
				   "SCROLL UP", COB_SCREEN_SCROLL_UP);
  }
#line 19596 "parser.c" /* yacc.c:1646  */
    break;

  case 1651:
#line 11053 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), DISPLAY);
  }
#line 19604 "parser.c" /* yacc.c:1646  */
    break;

  case 1652:
#line 11057 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), DISPLAY);
  }
#line 19612 "parser.c" /* yacc.c:1646  */
    break;

  case 1653:
#line 11067 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("DIVIDE", TERM_DIVIDE);
  }
#line 19620 "parser.c" /* yacc.c:1646  */
    break;

  case 1655:
#line 11076 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), '/', (yyvsp[-3]));
  }
#line 19628 "parser.c" /* yacc.c:1646  */
    break;

  case 1656:
#line 11080 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), 0, cb_build_binary_op ((yyvsp[-3]), '/', (yyvsp[-5])));
  }
#line 19636 "parser.c" /* yacc.c:1646  */
    break;

  case 1657:
#line 11084 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), 0, cb_build_binary_op ((yyvsp[-5]), '/', (yyvsp[-3])));
  }
#line 19644 "parser.c" /* yacc.c:1646  */
    break;

  case 1658:
#line 11088 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_divide ((yyvsp[-5]), (yyvsp[-7]), (yyvsp[-3]), (yyvsp[-1]));
  }
#line 19652 "parser.c" /* yacc.c:1646  */
    break;

  case 1659:
#line 11092 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_divide ((yyvsp[-7]), (yyvsp[-5]), (yyvsp[-3]), (yyvsp[-1]));
  }
#line 19660 "parser.c" /* yacc.c:1646  */
    break;

  case 1660:
#line 11099 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), DIVIDE);
  }
#line 19668 "parser.c" /* yacc.c:1646  */
    break;

  case 1661:
#line 11103 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), DIVIDE);
  }
#line 19676 "parser.c" /* yacc.c:1646  */
    break;

  case 1662:
#line 11113 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("ENABLE", 0);
  }
#line 19684 "parser.c" /* yacc.c:1646  */
    break;

  case 1664:
#line 11124 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_unreached = 0;
	begin_statement ("ENTRY", 0);
	backup_current_pos ();
  }
#line 19694 "parser.c" /* yacc.c:1646  */
    break;

  case 1666:
#line 11134 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (current_program->nested_level) {
		cb_error (_("%s is invalid in nested program"), "ENTRY");
	} else if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("%s is invalid in a user FUNCTION"), "ENTRY");
	} else if (cb_verify (cb_entry_statement, "ENTRY")) {
		if (!cobc_check_valid_name ((char *)(CB_LITERAL ((yyvsp[-1]))->data), ENTRY_NAME)) {
			emit_entry ((char *)(CB_LITERAL ((yyvsp[-1]))->data), 1, (yyvsp[0]), (yyvsp[-2]));
		}
	}
  }
#line 19710 "parser.c" /* yacc.c:1646  */
    break;

  case 1667:
#line 11152 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("EVALUATE", TERM_EVALUATE);
	eval_level++;
	if (eval_level >= EVAL_DEPTH) {
		cb_error (_("maximum evaluate depth exceeded (%d)"),
			  EVAL_DEPTH);
		eval_level = 0;
		eval_inc = 0;
		eval_inc2 = 0;
		YYERROR;
	} else {
		for (eval_inc = 0; eval_inc < EVAL_DEPTH; ++eval_inc) {
			eval_check[eval_level][eval_inc] = NULL;
		}
		eval_inc = 0;
		eval_inc2 = 0;
	}
	cb_end_cond (cb_any);
	cb_save_cond ();
	cb_true_side ();
  }
#line 19736 "parser.c" /* yacc.c:1646  */
    break;

  case 1669:
#line 11179 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_evaluate ((yyvsp[-1]), (yyvsp[0]));
	eval_level--;
  }
#line 19745 "parser.c" /* yacc.c:1646  */
    break;

  case 1670:
#line 11186 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 19751 "parser.c" /* yacc.c:1646  */
    break;

  case 1671:
#line 11188 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0])); }
#line 19757 "parser.c" /* yacc.c:1646  */
    break;

  case 1672:
#line 11193 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	eval_check[eval_level][eval_inc++] = (yyvsp[0]);
	if (eval_inc >= EVAL_DEPTH) {
		cb_error (_("maximum evaluate depth exceeded (%d)"),
			  EVAL_DEPTH);
		eval_inc = 0;
		YYERROR;
	}
  }
#line 19772 "parser.c" /* yacc.c:1646  */
    break;

  case 1673:
#line 11204 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_true;
	eval_check[eval_level][eval_inc++] = NULL;
	if (eval_inc >= EVAL_DEPTH) {
		cb_error (_("maximum evaluate depth exceeded (%d)"),
			  EVAL_DEPTH);
		eval_inc = 0;
		YYERROR;
	}
  }
#line 19787 "parser.c" /* yacc.c:1646  */
    break;

  case 1674:
#line 11215 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_false;
	eval_check[eval_level][eval_inc++] = cb_false;
	if (eval_inc >= EVAL_DEPTH) {
		cb_error (_("maximum evaluate depth exceeded (%d)"),
			  EVAL_DEPTH);
		eval_inc = 0;
		YYERROR;
	}
  }
#line 19802 "parser.c" /* yacc.c:1646  */
    break;

  case 1675:
#line 11229 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 19810 "parser.c" /* yacc.c:1646  */
    break;

  case 1676:
#line 11234 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 19818 "parser.c" /* yacc.c:1646  */
    break;

  case 1677:
#line 11240 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 19824 "parser.c" /* yacc.c:1646  */
    break;

  case 1678:
#line 11242 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 19830 "parser.c" /* yacc.c:1646  */
    break;

  case 1679:
#line 11248 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_CHAIN ((yyvsp[0]), (yyvsp[-1]));
	eval_inc2 = 0;
  }
#line 19839 "parser.c" /* yacc.c:1646  */
    break;

  case 1680:
#line 11257 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_CHAIN ((yyvsp[0]), NULL);
	eval_inc2 = 0;
  }
#line 19848 "parser.c" /* yacc.c:1646  */
    break;

  case 1681:
#line 11265 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
	eval_inc2 = 0;
  }
#line 19857 "parser.c" /* yacc.c:1646  */
    break;

  case 1682:
#line 11271 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0]));
	eval_inc2 = 0;
  }
#line 19866 "parser.c" /* yacc.c:1646  */
    break;

  case 1683:
#line 11278 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 19872 "parser.c" /* yacc.c:1646  */
    break;

  case 1684:
#line 11280 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0])); }
#line 19878 "parser.c" /* yacc.c:1646  */
    break;

  case 1685:
#line 11285 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree	not0;
	cb_tree	e1;
	cb_tree	e2;
	cb_tree	x;
	cb_tree	parm1;

	not0 = cb_int0;
	e2 = (yyvsp[0]);
	x = NULL;
	parm1 = (yyvsp[-1]);
	if (eval_check[eval_level][eval_inc2]
	 && eval_check[eval_level][eval_inc2] != cb_false) {
		/* Check if the first token is NOT */
		/* It may belong to the EVALUATE, however see */
		/* below when it may be part of a partial expression */
		if (CB_PURPOSE_INT (parm1) == '!') {
			/* Pop stack if subject not TRUE / FALSE */
			not0 = cb_int1;
			x = parm1;
			parm1 = CB_CHAIN (parm1);
		}
		/* Partial expression handling */
		switch (CB_PURPOSE_INT (parm1)) {
		/* Relational conditions */
		case '<':
		case '>':
		case '[':
		case ']':
		case '~':
		case '=':
		/* Class conditions */
		case '9':
		case 'A':
		case 'L':
		case 'U':
		case 'P':
		case 'N':
		case 'O':
		case 'C':
			if (e2) {
				cb_error_x (e2, _("invalid THROUGH usage"));
				e2 = NULL;
			}
			not0 = CB_PURPOSE (parm1);
			if (x) {
				/* Rebind the NOT to the partial expression */
				parm1 = cb_build_list (cb_int ('!'), NULL, parm1);
			}
			/* Insert subject at head of list */
			parm1 = cb_build_list (cb_int ('x'),
					    eval_check[eval_level][eval_inc2], parm1);
			break;
		}
	}

	/* Build expr now */
	e1 = cb_build_expr (parm1);

	eval_inc2++;
	(yyval) = CB_BUILD_PAIR (not0, CB_BUILD_PAIR (e1, e2));

	if (eval_check[eval_level][eval_inc2-1] == cb_false) {
		/* It was  EVALUATE FALSE; So flip condition */
		if (e1 == cb_true)
			e1 = cb_false;
		else if (e1 == cb_false)
			e1 = cb_true;
	}
	cb_terminate_cond ();
	cb_end_cond (e1);
	cb_save_cond ();
	cb_true_side ();
  }
#line 19957 "parser.c" /* yacc.c:1646  */
    break;

  case 1686:
#line 11359 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_any; eval_inc2++; }
#line 19963 "parser.c" /* yacc.c:1646  */
    break;

  case 1687:
#line 11360 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_true; eval_inc2++; }
#line 19969 "parser.c" /* yacc.c:1646  */
    break;

  case 1688:
#line 11361 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_false; eval_inc2++; }
#line 19975 "parser.c" /* yacc.c:1646  */
    break;

  case 1689:
#line 11365 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 19981 "parser.c" /* yacc.c:1646  */
    break;

  case 1690:
#line 11366 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 19987 "parser.c" /* yacc.c:1646  */
    break;

  case 1691:
#line 11371 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), EVALUATE);
  }
#line 19995 "parser.c" /* yacc.c:1646  */
    break;

  case 1692:
#line 11375 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), EVALUATE);
  }
#line 20003 "parser.c" /* yacc.c:1646  */
    break;

  case 1693:
#line 11385 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("EXIT", 0);
	cobc_cs_check = CB_CS_EXIT;
  }
#line 20012 "parser.c" /* yacc.c:1646  */
    break;

  case 1694:
#line 11390 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 20020 "parser.c" /* yacc.c:1646  */
    break;

  case 1695:
#line 11397 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
  /* TODO: add warning/error if there's another statement in the paragraph */
  }
#line 20028 "parser.c" /* yacc.c:1646  */
    break;

  case 1696:
#line 11401 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (in_declaratives && use_global_ind) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT PROGRAM is not allowed within a USE GLOBAL procedure"));
	}
	if (current_program->prog_type != COB_MODULE_TYPE_PROGRAM) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT PROGRAM not allowed within a FUNCTION"));
	}
	if (current_program->flag_main) {
		check_unreached = 0;
	} else {
		check_unreached = 1;
	}
	if ((yyvsp[0])) {
		if (!current_program->cb_return_code) {
			cb_error_x ((yyvsp[0]), _("RETURNING/GIVING not allowed for non-returning sources"));
		} else {
			cb_emit_move ((yyvsp[0]), CB_LIST_INIT (current_program->cb_return_code));
		}
	}
	current_statement->name = (const char *)"EXIT PROGRAM";
	cb_emit_exit (0);
  }
#line 20057 "parser.c" /* yacc.c:1646  */
    break;

  case 1697:
#line 11426 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (in_declaratives && use_global_ind) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT FUNCTION is not allowed within a USE GLOBAL procedure"));
	}
	if (current_program->prog_type != COB_MODULE_TYPE_FUNCTION) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT FUNCTION only allowed within a FUNCTION"));
	}
	check_unreached = 1;
	current_statement->name = (const char *)"EXIT FUNCTION";
	cb_emit_exit (0);
  }
#line 20075 "parser.c" /* yacc.c:1646  */
    break;

  case 1698:
#line 11440 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	struct cb_perform	*p;
	cb_tree			plabel;
	char			name[64];

	if (!perform_stack) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT PERFORM is only valid with inline PERFORM"));
	} else if (CB_VALUE (perform_stack) != cb_error_node) {
		p = CB_PERFORM (CB_VALUE (perform_stack));
		if (!p->cycle_label) {
			sprintf (name, "EXIT PERFORM CYCLE %d", cb_id);
			p->cycle_label = cb_build_reference (name);
			plabel = cb_build_label (p->cycle_label, NULL);
			CB_LABEL (plabel)->flag_begin = 1;
			CB_LABEL (plabel)->flag_dummy_exit = 1;
		}
		current_statement->name = (const char *)"EXIT PERFORM CYCLE";
		cb_emit_goto (CB_LIST_INIT (p->cycle_label), NULL);
		check_unreached = 1;
	}
  }
#line 20102 "parser.c" /* yacc.c:1646  */
    break;

  case 1699:
#line 11463 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	struct cb_perform	*p;
	cb_tree			plabel;
	char			name[64];

	if (!perform_stack) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT PERFORM is only valid with inline PERFORM"));
	} else if (CB_VALUE (perform_stack) != cb_error_node) {
		p = CB_PERFORM (CB_VALUE (perform_stack));
		if (!p->exit_label) {
			sprintf (name, "EXIT PERFORM %d", cb_id);
			p->exit_label = cb_build_reference (name);
			plabel = cb_build_label (p->exit_label, NULL);
			CB_LABEL (plabel)->flag_begin = 1;
			CB_LABEL (plabel)->flag_dummy_exit = 1;
		}
		current_statement->name = (const char *)"EXIT PERFORM";
		cb_emit_goto (CB_LIST_INIT (p->exit_label), NULL);
		check_unreached = 1;
	}
  }
#line 20129 "parser.c" /* yacc.c:1646  */
    break;

  case 1700:
#line 11486 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree	plabel;
	char	name[64];

	if (!current_section) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT SECTION is only valid with an active SECTION"));
	} else {
		if (!current_section->exit_label) {
			sprintf (name, "EXIT SECTION %d", cb_id);
			current_section->exit_label = cb_build_reference (name);
			plabel = cb_build_label (current_section->exit_label, NULL);
			CB_LABEL (plabel)->flag_begin = 1;
			CB_LABEL (plabel)->flag_dummy_exit = 1;
		}
		current_statement->name = (const char *)"EXIT SECTION";
		cb_emit_goto (CB_LIST_INIT (current_section->exit_label), NULL);
		check_unreached = 1;
	}
  }
#line 20154 "parser.c" /* yacc.c:1646  */
    break;

  case 1701:
#line 11507 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree	plabel;
	char	name[64];

	if (!current_paragraph) {
		cb_error_x (CB_TREE (current_statement),
			    _("EXIT PARAGRAPH is only valid with an active PARAGRAPH"));
	} else {
		if (!current_paragraph->exit_label) {
			sprintf (name, "EXIT PARAGRAPH %d", cb_id);
			current_paragraph->exit_label = cb_build_reference (name);
			plabel = cb_build_label (current_paragraph->exit_label, NULL);
			CB_LABEL (plabel)->flag_begin = 1;
			CB_LABEL (plabel)->flag_dummy_exit = 1;
		}
		current_statement->name = (const char *)"EXIT PARAGRAPH";
		cb_emit_goto (CB_LIST_INIT (current_paragraph->exit_label), NULL);
		check_unreached = 1;
	}
  }
#line 20179 "parser.c" /* yacc.c:1646  */
    break;

  case 1702:
#line 11530 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 20185 "parser.c" /* yacc.c:1646  */
    break;

  case 1703:
#line 11533 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 20191 "parser.c" /* yacc.c:1646  */
    break;

  case 1704:
#line 11541 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("FREE", 0);
	current_statement->flag_no_based = 1;
  }
#line 20200 "parser.c" /* yacc.c:1646  */
    break;

  case 1706:
#line 11550 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_free ((yyvsp[0]));
  }
#line 20208 "parser.c" /* yacc.c:1646  */
    break;

  case 1707:
#line 11560 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("GENERATE", 0);
  }
#line 20216 "parser.c" /* yacc.c:1646  */
    break;

  case 1709:
#line 11569 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	if ((yyvsp[0]) != cb_error_node) {
		cb_emit_generate((yyvsp[0]));
	}
  }
#line 20227 "parser.c" /* yacc.c:1646  */
    break;

  case 1710:
#line 11581 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (!current_paragraph->flag_statement) {
		current_paragraph->flag_first_is_goto = 1;
	}
	begin_statement ("GO TO", 0);
	save_debug = start_debug;
	start_debug = 0;
  }
#line 20240 "parser.c" /* yacc.c:1646  */
    break;

  case 1712:
#line 11594 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_goto ((yyvsp[-1]), (yyvsp[0]));
	start_debug = save_debug;
  }
#line 20249 "parser.c" /* yacc.c:1646  */
    break;

  case 1713:
#line 11602 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_unreached = 1;
	(yyval) = NULL;
  }
#line 20258 "parser.c" /* yacc.c:1646  */
    break;

  case 1714:
#line 11607 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_unreached = 0;
	(yyval) = (yyvsp[0]);
  }
#line 20267 "parser.c" /* yacc.c:1646  */
    break;

  case 1715:
#line 11618 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("GOBACK", 0);
	check_unreached = 1;
	if ((yyvsp[0])) {
		if (!current_program->cb_return_code) {
			cb_error_x ((yyvsp[0]), _("RETURNING/GIVING not allowed for non-returning sources"));
		} else {
			cb_emit_move ((yyvsp[0]), CB_LIST_INIT (current_program->cb_return_code));
		}
	}
	cb_emit_exit (1U);
  }
#line 20284 "parser.c" /* yacc.c:1646  */
    break;

  case 1716:
#line 11637 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("IF", TERM_IF);
  }
#line 20292 "parser.c" /* yacc.c:1646  */
    break;

  case 1718:
#line 11646 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_if ((yyvsp[(-1) - (5)]), (yyvsp[-3]), (yyvsp[0]));
  }
#line 20300 "parser.c" /* yacc.c:1646  */
    break;

  case 1719:
#line 11650 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_if ((yyvsp[(-1) - (3)]), NULL, (yyvsp[0]));
	cb_verify (cb_missing_statement,
		_("IF without imperative statement"));
  }
#line 20310 "parser.c" /* yacc.c:1646  */
    break;

  case 1720:
#line 11656 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_if ((yyvsp[(-1) - (2)]), (yyvsp[0]), NULL);
  }
#line 20318 "parser.c" /* yacc.c:1646  */
    break;

  case 1721:
#line 11662 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_save_cond ();
  }
#line 20326 "parser.c" /* yacc.c:1646  */
    break;

  case 1722:
#line 11666 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_save_cond ();
  }
#line 20334 "parser.c" /* yacc.c:1646  */
    break;

  case 1723:
#line 11672 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	  cb_true_side ();
  }
#line 20342 "parser.c" /* yacc.c:1646  */
    break;

  case 1724:
#line 11678 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	  cb_false_side ();
  }
#line 20350 "parser.c" /* yacc.c:1646  */
    break;

  case 1725:
#line 11685 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-4) - (0)]), IF);
	cb_terminate_cond ();
  }
#line 20359 "parser.c" /* yacc.c:1646  */
    break;

  case 1726:
#line 11690 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-4) - (1)]), IF);
	cb_terminate_cond ();
  }
#line 20368 "parser.c" /* yacc.c:1646  */
    break;

  case 1727:
#line 11701 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("INITIALIZE", 0);
  }
#line 20376 "parser.c" /* yacc.c:1646  */
    break;

  case 1729:
#line 11710 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_initialize ((yyvsp[-4]), (yyvsp[-3]), (yyvsp[-2]), (yyvsp[-1]), (yyvsp[0]));
  }
#line 20384 "parser.c" /* yacc.c:1646  */
    break;

  case 1730:
#line 11716 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 20390 "parser.c" /* yacc.c:1646  */
    break;

  case 1731:
#line 11717 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_true; }
#line 20396 "parser.c" /* yacc.c:1646  */
    break;

  case 1732:
#line 11721 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 20402 "parser.c" /* yacc.c:1646  */
    break;

  case 1733:
#line 11722 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_true; }
#line 20408 "parser.c" /* yacc.c:1646  */
    break;

  case 1734:
#line 11723 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[-2]); }
#line 20414 "parser.c" /* yacc.c:1646  */
    break;

  case 1735:
#line 11728 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 20422 "parser.c" /* yacc.c:1646  */
    break;

  case 1736:
#line 11732 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 20430 "parser.c" /* yacc.c:1646  */
    break;

  case 1737:
#line 11739 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 20438 "parser.c" /* yacc.c:1646  */
    break;

  case 1738:
#line 11744 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0]));
  }
#line 20446 "parser.c" /* yacc.c:1646  */
    break;

  case 1739:
#line 11751 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[-3]), (yyvsp[0]));
  }
#line 20454 "parser.c" /* yacc.c:1646  */
    break;

  case 1740:
#line 11757 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_ALPHABETIC); }
#line 20460 "parser.c" /* yacc.c:1646  */
    break;

  case 1741:
#line 11758 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_ALPHANUMERIC); }
#line 20466 "parser.c" /* yacc.c:1646  */
    break;

  case 1742:
#line 11759 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_NUMERIC); }
#line 20472 "parser.c" /* yacc.c:1646  */
    break;

  case 1743:
#line 11760 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_ALPHANUMERIC_EDITED); }
#line 20478 "parser.c" /* yacc.c:1646  */
    break;

  case 1744:
#line 11761 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_NUMERIC_EDITED); }
#line 20484 "parser.c" /* yacc.c:1646  */
    break;

  case 1745:
#line 11762 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_NATIONAL); }
#line 20490 "parser.c" /* yacc.c:1646  */
    break;

  case 1746:
#line 11763 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (CB_CATEGORY_NATIONAL_EDITED); }
#line 20496 "parser.c" /* yacc.c:1646  */
    break;

  case 1747:
#line 11775 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 20504 "parser.c" /* yacc.c:1646  */
    break;

  case 1748:
#line 11779 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_true;
  }
#line 20512 "parser.c" /* yacc.c:1646  */
    break;

  case 1749:
#line 11788 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("INITIATE", 0);
  }
#line 20520 "parser.c" /* yacc.c:1646  */
    break;

  case 1751:
#line 11796 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	if ((yyvsp[0]) != cb_error_node) {
	    cb_emit_initiate((yyvsp[0]));
	}
  }
#line 20531 "parser.c" /* yacc.c:1646  */
    break;

  case 1752:
#line 11803 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	if ((yyvsp[0]) != cb_error_node) {
	    cb_emit_initiate((yyvsp[0]));
	}
  }
#line 20542 "parser.c" /* yacc.c:1646  */
    break;

  case 1753:
#line 11815 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("INQUIRE", 0);
	cobc_cs_check = CB_CS_INQUIRE_MODIFY;
  }
#line 20551 "parser.c" /* yacc.c:1646  */
    break;

  case 1754:
#line 11820 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 20559 "parser.c" /* yacc.c:1646  */
    break;

  case 1757:
#line 11834 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("INSPECT", 0);
	inspect_keyword = 0;
  }
#line 20568 "parser.c" /* yacc.c:1646  */
    break;

  case 1767:
#line 11862 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	previous_tallying_phrase = NO_PHRASE;
	cb_init_tallying ();
  }
#line 20577 "parser.c" /* yacc.c:1646  */
    break;

  case 1768:
#line 11867 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (!(previous_tallying_phrase == CHARACTERS_PHRASE
	      || previous_tallying_phrase == VALUE_REGION_PHRASE)) {
		cb_error (_("TALLYING clause is incomplete"));
	} else {
		cb_emit_inspect ((yyvsp[-3]), (yyvsp[0]), TALLYING_CLAUSE);
	}

	(yyval) = (yyvsp[-3]);
  }
#line 20592 "parser.c" /* yacc.c:1646  */
    break;

  case 1769:
#line 11883 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_inspect ((yyvsp[-2]), (yyvsp[0]), REPLACING_CLAUSE);
	inspect_keyword = 0;
  }
#line 20601 "parser.c" /* yacc.c:1646  */
    break;

  case 1770:
#line 11893 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;
	x = cb_build_converting ((yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
	cb_emit_inspect ((yyvsp[-5]), x, CONVERTING_CLAUSE);
  }
#line 20611 "parser.c" /* yacc.c:1646  */
    break;

  case 1771:
#line 11902 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 20619 "parser.c" /* yacc.c:1646  */
    break;

  case 1772:
#line 11906 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0]));
  }
#line 20627 "parser.c" /* yacc.c:1646  */
    break;

  case 1773:
#line 11913 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_preceding_tallying_phrases (FOR_PHRASE);
	(yyval) = cb_build_tallying_data ((yyvsp[-1]));
  }
#line 20636 "parser.c" /* yacc.c:1646  */
    break;

  case 1774:
#line 11918 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_preceding_tallying_phrases (CHARACTERS_PHRASE);
	(yyval) = cb_build_tallying_characters ((yyvsp[0]));
  }
#line 20645 "parser.c" /* yacc.c:1646  */
    break;

  case 1775:
#line 11923 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_preceding_tallying_phrases (ALL_LEADING_TRAILING_PHRASES);
	(yyval) = cb_build_tallying_all ();
  }
#line 20654 "parser.c" /* yacc.c:1646  */
    break;

  case 1776:
#line 11928 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_preceding_tallying_phrases (ALL_LEADING_TRAILING_PHRASES);
	(yyval) = cb_build_tallying_leading ();
  }
#line 20663 "parser.c" /* yacc.c:1646  */
    break;

  case 1777:
#line 11933 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_preceding_tallying_phrases (ALL_LEADING_TRAILING_PHRASES);
	(yyval) = cb_build_tallying_trailing ();
  }
#line 20672 "parser.c" /* yacc.c:1646  */
    break;

  case 1778:
#line 11938 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_preceding_tallying_phrases (VALUE_REGION_PHRASE);
	(yyval) = cb_build_tallying_value ((yyvsp[-1]), (yyvsp[0]));
  }
#line 20681 "parser.c" /* yacc.c:1646  */
    break;

  case 1779:
#line 11945 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 20687 "parser.c" /* yacc.c:1646  */
    break;

  case 1780:
#line 11946 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0])); }
#line 20693 "parser.c" /* yacc.c:1646  */
    break;

  case 1781:
#line 11951 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_replacing_characters ((yyvsp[-1]), (yyvsp[0]));
	inspect_keyword = 0;
  }
#line 20702 "parser.c" /* yacc.c:1646  */
    break;

  case 1782:
#line 11956 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 20710 "parser.c" /* yacc.c:1646  */
    break;

  case 1784:
#line 11963 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { inspect_keyword = 1; }
#line 20716 "parser.c" /* yacc.c:1646  */
    break;

  case 1785:
#line 11964 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { inspect_keyword = 2; }
#line 20722 "parser.c" /* yacc.c:1646  */
    break;

  case 1786:
#line 11965 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { inspect_keyword = 3; }
#line 20728 "parser.c" /* yacc.c:1646  */
    break;

  case 1787:
#line 11966 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { inspect_keyword = 4; }
#line 20734 "parser.c" /* yacc.c:1646  */
    break;

  case 1788:
#line 11971 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	switch (inspect_keyword) {
		case 1:
			(yyval) = cb_build_replacing_all ((yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
			break;
		case 2:
			(yyval) = cb_build_replacing_leading ((yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
			break;
		case 3:
			(yyval) = cb_build_replacing_first ((yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
			break;
		case 4:
			(yyval) = cb_build_replacing_trailing ((yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
			break;
		default:
			cb_error_x (CB_TREE (current_statement),
				    _("INSPECT missing ALL/FIRST/LEADING/TRAILING"));
			(yyval) = cb_build_replacing_all ((yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
			break;
	}
  }
#line 20760 "parser.c" /* yacc.c:1646  */
    break;

  case 1789:
#line 11998 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_inspect_region_start ();
  }
#line 20768 "parser.c" /* yacc.c:1646  */
    break;

  case 1790:
#line 12002 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add (cb_build_inspect_region_start (), (yyvsp[0]));
  }
#line 20776 "parser.c" /* yacc.c:1646  */
    break;

  case 1791:
#line 12006 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add (cb_build_inspect_region_start (), (yyvsp[0]));
  }
#line 20784 "parser.c" /* yacc.c:1646  */
    break;

  case 1792:
#line 12010 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add (cb_list_add (cb_build_inspect_region_start (), (yyvsp[-1])), (yyvsp[0]));
  }
#line 20792 "parser.c" /* yacc.c:1646  */
    break;

  case 1793:
#line 12014 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add (cb_list_add (cb_build_inspect_region_start (), (yyvsp[-1])), (yyvsp[0]));
  }
#line 20800 "parser.c" /* yacc.c:1646  */
    break;

  case 1794:
#line 12021 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_FUNCALL_1 ("cob_inspect_before", (yyvsp[0]));
  }
#line 20808 "parser.c" /* yacc.c:1646  */
    break;

  case 1795:
#line 12028 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_FUNCALL_1 ("cob_inspect_after", (yyvsp[0]));
  }
#line 20816 "parser.c" /* yacc.c:1646  */
    break;

  case 1796:
#line 12037 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("MERGE", 0);
	current_statement->flag_merge = 1;
  }
#line 20825 "parser.c" /* yacc.c:1646  */
    break;

  case 1798:
#line 12049 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("MODIFY", TERM_MODIFY);
	cobc_cs_check = CB_CS_INQUIRE_MODIFY;
  }
#line 20834 "parser.c" /* yacc.c:1646  */
    break;

  case 1799:
#line 12055 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 20842 "parser.c" /* yacc.c:1646  */
    break;

  case 1802:
#line 12067 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), MODIFY);
  }
#line 20850 "parser.c" /* yacc.c:1646  */
    break;

  case 1803:
#line 12071 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), MODIFY);
  }
#line 20858 "parser.c" /* yacc.c:1646  */
    break;

  case 1804:
#line 12081 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("MOVE", 0);
  }
#line 20866 "parser.c" /* yacc.c:1646  */
    break;

  case 1806:
#line 12089 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_move ((yyvsp[-2]), (yyvsp[0]));
  }
#line 20874 "parser.c" /* yacc.c:1646  */
    break;

  case 1807:
#line 12093 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_move_corresponding ((yyvsp[-2]), (yyvsp[0]));
  }
#line 20882 "parser.c" /* yacc.c:1646  */
    break;

  case 1808:
#line 12103 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("MULTIPLY", TERM_MULTIPLY);
  }
#line 20890 "parser.c" /* yacc.c:1646  */
    break;

  case 1810:
#line 12112 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), '*', (yyvsp[-3]));
  }
#line 20898 "parser.c" /* yacc.c:1646  */
    break;

  case 1811:
#line 12116 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), 0, cb_build_binary_op ((yyvsp[-5]), '*', (yyvsp[-3])));
  }
#line 20906 "parser.c" /* yacc.c:1646  */
    break;

  case 1812:
#line 12123 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), MULTIPLY);
  }
#line 20914 "parser.c" /* yacc.c:1646  */
    break;

  case 1813:
#line 12127 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), MULTIPLY);
  }
#line 20922 "parser.c" /* yacc.c:1646  */
    break;

  case 1814:
#line 12137 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("OPEN", 0);
  }
#line 20930 "parser.c" /* yacc.c:1646  */
    break;

  case 1818:
#line 12150 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree l;
	cb_tree x;

	if ((yyvsp[-3]) && (yyvsp[0])) {
		cb_error_x (CB_TREE (current_statement),
			    _("%s and %s are mutually exclusive"), "SHARING", _("LOCK clauses"));
	}
	if ((yyvsp[0])) {
		x = (yyvsp[0]);
	} else {
		x = (yyvsp[-3]);
	}

	for (l = (yyvsp[-1]); l; l = CB_CHAIN (l)) {
		if (CB_VALID_TREE (CB_VALUE (l))) {
			begin_implicit_statement ();
			cb_emit_open (CB_VALUE (l), (yyvsp[-4]), x);
		}
	}
  }
#line 20956 "parser.c" /* yacc.c:1646  */
    break;

  case 1819:
#line 12174 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_OPEN_INPUT); }
#line 20962 "parser.c" /* yacc.c:1646  */
    break;

  case 1820:
#line 12175 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_OPEN_OUTPUT); }
#line 20968 "parser.c" /* yacc.c:1646  */
    break;

  case 1821:
#line 12176 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_OPEN_I_O); }
#line 20974 "parser.c" /* yacc.c:1646  */
    break;

  case 1822:
#line 12177 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_OPEN_EXTEND); }
#line 20980 "parser.c" /* yacc.c:1646  */
    break;

  case 1823:
#line 12181 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 20986 "parser.c" /* yacc.c:1646  */
    break;

  case 1824:
#line 12182 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 20992 "parser.c" /* yacc.c:1646  */
    break;

  case 1825:
#line 12186 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 20998 "parser.c" /* yacc.c:1646  */
    break;

  case 1826:
#line 12187 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 21004 "parser.c" /* yacc.c:1646  */
    break;

  case 1827:
#line 12188 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_LOCK_OPEN_EXCLUSIVE); }
#line 21010 "parser.c" /* yacc.c:1646  */
    break;

  case 1828:
#line 12190 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(void)cb_verify (CB_OBSOLETE, "REVERSED");
	(yyval) = NULL;
  }
#line 21019 "parser.c" /* yacc.c:1646  */
    break;

  case 1829:
#line 12201 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("PERFORM", TERM_PERFORM);
	/* Turn off field debug - PERFORM is special */
	save_debug = start_debug;
	start_debug = 0;
	cobc_cs_check = CB_CS_PERFORM;
  }
#line 21031 "parser.c" /* yacc.c:1646  */
    break;

  case 1831:
#line 12216 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_perform ((yyvsp[0]), (yyvsp[-2]), (yyvsp[-3]), (yyvsp[-1]));
	start_debug = save_debug;
	cobc_cs_check = 0;
  }
#line 21041 "parser.c" /* yacc.c:1646  */
    break;

  case 1832:
#line 12224 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_ADD_TO_CHAIN ((yyvsp[-1]), perform_stack);
	/* Restore field debug before inline statements */
	start_debug = save_debug;
	cobc_cs_check = 0;
  }
#line 21052 "parser.c" /* yacc.c:1646  */
    break;

  case 1833:
#line 12231 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	perform_stack = CB_CHAIN (perform_stack);
	cb_emit_perform ((yyvsp[-4]), (yyvsp[-1]), (yyvsp[-5]), (yyvsp[-3]));
  }
#line 21061 "parser.c" /* yacc.c:1646  */
    break;

  case 1834:
#line 12239 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_perform ((yyvsp[-2]), NULL, (yyvsp[-3]), (yyvsp[-1]));
	start_debug = save_debug;
	cobc_cs_check = 0;
  }
#line 21071 "parser.c" /* yacc.c:1646  */
    break;

  case 1835:
#line 12248 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (cb_relaxed_syntax_checks) {
		TERMINATOR_WARNING ((yyvsp[(-6) - (0)]), PERFORM);
	} else {
		TERMINATOR_ERROR ((yyvsp[(-6) - (0)]), PERFORM);
	}
  }
#line 21083 "parser.c" /* yacc.c:1646  */
    break;

  case 1836:
#line 12256 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-6) - (1)]), PERFORM);
  }
#line 21091 "parser.c" /* yacc.c:1646  */
    break;

  case 1837:
#line 12263 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-4) - (1)]), PERFORM);
  }
#line 21099 "parser.c" /* yacc.c:1646  */
    break;

  case 1838:
#line 12267 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (cb_relaxed_syntax_checks) {
		TERMINATOR_WARNING ((yyvsp[(-4) - (1)]), PERFORM);
	} else {
		TERMINATOR_ERROR ((yyvsp[(-4) - (1)]), PERFORM);
	}
	/* Put the dot token back into the stack for reparse */
	cb_unput_dot ();
  }
#line 21113 "parser.c" /* yacc.c:1646  */
    break;

  case 1839:
#line 12280 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	/* Return from $1 */
	CB_REFERENCE ((yyvsp[0]))->length = cb_true;
	CB_REFERENCE ((yyvsp[0]))->flag_decl_ok = 1;
	(yyval) = CB_BUILD_PAIR ((yyvsp[0]), (yyvsp[0]));
  }
#line 21124 "parser.c" /* yacc.c:1646  */
    break;

  case 1840:
#line 12287 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	/* Return from $3 */
	CB_REFERENCE ((yyvsp[0]))->length = cb_true;
	CB_REFERENCE ((yyvsp[-2]))->flag_decl_ok = 1;
	CB_REFERENCE ((yyvsp[0]))->flag_decl_ok = 1;
	(yyval) = CB_BUILD_PAIR ((yyvsp[-2]), (yyvsp[0]));
  }
#line 21136 "parser.c" /* yacc.c:1646  */
    break;

  case 1841:
#line 12298 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_perform_once (NULL);
  }
#line 21144 "parser.c" /* yacc.c:1646  */
    break;

  case 1842:
#line 12302 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_perform_times ((yyvsp[-1]));
	current_program->loop_counter++;
  }
#line 21153 "parser.c" /* yacc.c:1646  */
    break;

  case 1843:
#line 12307 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_perform_forever (NULL);
  }
#line 21161 "parser.c" /* yacc.c:1646  */
    break;

  case 1844:
#line 12311 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree varying;

	if (!(yyvsp[0])) {
		(yyval) = cb_build_perform_forever (NULL);
	} else {
		if ((yyvsp[-2]) == CB_AFTER)
			cb_build_perform_after_until();
		varying = CB_LIST_INIT (cb_build_perform_varying (NULL, NULL, NULL, (yyvsp[0])));
		(yyval) = cb_build_perform_until ((yyvsp[-2]), varying);
	}
  }
#line 21178 "parser.c" /* yacc.c:1646  */
    break;

  case 1845:
#line 12324 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_perform_until ((yyvsp[-2]), (yyvsp[0]));
  }
#line 21186 "parser.c" /* yacc.c:1646  */
    break;

  case 1846:
#line 12330 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = CB_BEFORE; }
#line 21192 "parser.c" /* yacc.c:1646  */
    break;

  case 1847:
#line 12331 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 21198 "parser.c" /* yacc.c:1646  */
    break;

  case 1848:
#line 12335 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 21204 "parser.c" /* yacc.c:1646  */
    break;

  case 1849:
#line 12336 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 21210 "parser.c" /* yacc.c:1646  */
    break;

  case 1850:
#line 12339 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 21216 "parser.c" /* yacc.c:1646  */
    break;

  case 1851:
#line 12341 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0])); }
#line 21222 "parser.c" /* yacc.c:1646  */
    break;

  case 1852:
#line 12346 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;
	int		data_type_ok = 1;

	if ((yyvsp[-5]) != cb_error_node
	 && (yyvsp[-3]) != cb_error_node
	 && (yyvsp[-2]) != cb_error_node) {

		if (cb_tree_category ((yyvsp[-5])) != CB_CATEGORY_NUMERIC) {
			x = cb_ref ((yyvsp[-5]));
			cb_error_x (CB_TREE (current_statement),
				_("PERFORM VARYING '%s' (line %d of %s) is not a numeric field"),
				cb_name (x),x->source_line, x->source_file);
			(yyval) = cb_int1;
			data_type_ok = 0;
		}
		if (cb_tree_category ((yyvsp[-3])) != CB_CATEGORY_NUMERIC) {
			x = cb_ref ((yyvsp[-3]));
			cb_error_x (CB_TREE (current_statement),
				_("PERFORM VARYING '%s' (line %d of %s) is not a numeric field"),
				cb_name (x),x->source_line, x->source_file);
			(yyval) = cb_int1;
			data_type_ok = 0;
		}
		if (cb_tree_category ((yyvsp[-2])) != CB_CATEGORY_NUMERIC) {
			x = cb_ref ((yyvsp[-2]));
			cb_error_x (CB_TREE (current_statement),
				_("PERFORM VARYING '%s' (line %d of %s) is not a numeric field"),
				cb_name (x),x->source_line, x->source_file);
			(yyval) = cb_int1;
			data_type_ok = 0;
		}

		if (data_type_ok) {
			(yyval) = cb_build_perform_varying ((yyvsp[-5]), (yyvsp[-3]), (yyvsp[-2]), (yyvsp[0]));
		}
	}
  }
#line 21265 "parser.c" /* yacc.c:1646  */
    break;

  case 1853:
#line 12388 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_perform_varying_without_by, _ ("PERFORM VARYING without BY phrase"));
	(yyval) = cb_build_numeric_literal (0, "1", 0);
  }
#line 21274 "parser.c" /* yacc.c:1646  */
    break;

  case 1854:
#line 12393 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 21282 "parser.c" /* yacc.c:1646  */
    break;

  case 1855:
#line 12402 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("PURGE", 0);
  }
#line 21290 "parser.c" /* yacc.c:1646  */
    break;

  case 1856:
#line 12406 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
  }
#line 21297 "parser.c" /* yacc.c:1646  */
    break;

  case 1857:
#line 12414 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("READ", TERM_READ);
	cobc_cs_check = CB_CS_READ;
  }
#line 21306 "parser.c" /* yacc.c:1646  */
    break;

  case 1859:
#line 12424 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;

	if (CB_VALID_TREE ((yyvsp[-6]))) {
		struct cb_file	*cf;

		cf = CB_FILE(cb_ref ((yyvsp[-6])));
		if ((yyvsp[-2]) && (cf->lock_mode & COB_LOCK_AUTOMATIC)) {
			cb_error_x (CB_TREE (current_statement),
				    _("LOCK clause invalid with file LOCK AUTOMATIC"));
		} else if ((yyvsp[-1]) &&
		      (cf->organization != COB_ORG_RELATIVE &&
		       cf->organization != COB_ORG_INDEXED)) {
			cb_error_x (CB_TREE (current_statement),
				    _("KEY clause invalid with this file type"));
		} else if (current_statement->handler_type == INVALID_KEY_HANDLER &&
			   (cf->organization != COB_ORG_RELATIVE &&
			    cf->organization != COB_ORG_INDEXED)) {
			cb_error_x (CB_TREE (current_statement),
				    _("INVALID KEY clause invalid with this file type"));
		} else {
			cb_emit_read ((yyvsp[-6]), (yyvsp[-5]), (yyvsp[-3]), (yyvsp[-1]), (yyvsp[-2]));
		}
	}
  }
#line 21336 "parser.c" /* yacc.c:1646  */
    break;

  case 1860:
#line 12452 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 21342 "parser.c" /* yacc.c:1646  */
    break;

  case 1861:
#line 12453 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 21348 "parser.c" /* yacc.c:1646  */
    break;

  case 1862:
#line 12458 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 21356 "parser.c" /* yacc.c:1646  */
    break;

  case 1863:
#line 12462 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int3;
  }
#line 21364 "parser.c" /* yacc.c:1646  */
    break;

  case 1864:
#line 12466 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 21372 "parser.c" /* yacc.c:1646  */
    break;

  case 1865:
#line 12470 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 21380 "parser.c" /* yacc.c:1646  */
    break;

  case 1868:
#line 12482 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("ADVANCING ON LOCK");
  }
#line 21388 "parser.c" /* yacc.c:1646  */
    break;

  case 1872:
#line 12495 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("RETRY");
	cobc_cs_check = 0;
  }
#line 21397 "parser.c" /* yacc.c:1646  */
    break;

  case 1878:
#line 12515 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 21405 "parser.c" /* yacc.c:1646  */
    break;

  case 1879:
#line 12519 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
   (yyval) = cb_int5;
  }
#line 21413 "parser.c" /* yacc.c:1646  */
    break;

  case 1880:
#line 12523 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	/* TO-DO: Merge with RETRY phrase */
	(yyval) = cb_int4;
  }
#line 21422 "parser.c" /* yacc.c:1646  */
    break;

  case 1881:
#line 12530 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 21428 "parser.c" /* yacc.c:1646  */
    break;

  case 1882:
#line 12531 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 21434 "parser.c" /* yacc.c:1646  */
    break;

  case 1885:
#line 12541 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), READ);
  }
#line 21442 "parser.c" /* yacc.c:1646  */
    break;

  case 1886:
#line 12545 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), READ);
  }
#line 21450 "parser.c" /* yacc.c:1646  */
    break;

  case 1887:
#line 12555 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("READY TRACE", 0);
	cb_emit_ready_trace ();
  }
#line 21459 "parser.c" /* yacc.c:1646  */
    break;

  case 1888:
#line 12565 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("RECEIVE", TERM_RECEIVE);
  }
#line 21467 "parser.c" /* yacc.c:1646  */
    break;

  case 1902:
#line 12608 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), RECEIVE);
  }
#line 21475 "parser.c" /* yacc.c:1646  */
    break;

  case 1903:
#line 12612 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), RECEIVE);
  }
#line 21483 "parser.c" /* yacc.c:1646  */
    break;

  case 1904:
#line 12621 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("RELEASE", 0);
  }
#line 21491 "parser.c" /* yacc.c:1646  */
    break;

  case 1906:
#line 12629 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_release ((yyvsp[-1]), (yyvsp[0]));
  }
#line 21499 "parser.c" /* yacc.c:1646  */
    break;

  case 1907:
#line 12639 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("RESET TRACE", 0);
	cb_emit_reset_trace ();
  }
#line 21508 "parser.c" /* yacc.c:1646  */
    break;

  case 1908:
#line 12649 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("RETURN", TERM_RETURN);
  }
#line 21516 "parser.c" /* yacc.c:1646  */
    break;

  case 1910:
#line 12658 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_return ((yyvsp[-3]), (yyvsp[-1]));
  }
#line 21524 "parser.c" /* yacc.c:1646  */
    break;

  case 1911:
#line 12665 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), RETURN);
  }
#line 21532 "parser.c" /* yacc.c:1646  */
    break;

  case 1912:
#line 12669 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), RETURN);
  }
#line 21540 "parser.c" /* yacc.c:1646  */
    break;

  case 1913:
#line 12679 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("REWRITE", TERM_REWRITE);
	/* Special in debugging mode */
	save_debug = start_debug;
	start_debug = 0;
  }
#line 21551 "parser.c" /* yacc.c:1646  */
    break;

  case 1915:
#line 12691 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_rewrite ((yyvsp[-4]), (yyvsp[-3]), (yyvsp[-1]));
	start_debug = save_debug;
  }
#line 21560 "parser.c" /* yacc.c:1646  */
    break;

  case 1916:
#line 12699 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 21568 "parser.c" /* yacc.c:1646  */
    break;

  case 1918:
#line 12707 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 21576 "parser.c" /* yacc.c:1646  */
    break;

  case 1919:
#line 12711 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int2;
  }
#line 21584 "parser.c" /* yacc.c:1646  */
    break;

  case 1920:
#line 12718 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), REWRITE);
  }
#line 21592 "parser.c" /* yacc.c:1646  */
    break;

  case 1921:
#line 12722 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), REWRITE);
  }
#line 21600 "parser.c" /* yacc.c:1646  */
    break;

  case 1922:
#line 12732 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("ROLLBACK", 0);
	cb_emit_rollback ();
  }
#line 21609 "parser.c" /* yacc.c:1646  */
    break;

  case 1923:
#line 12743 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("SEARCH", TERM_SEARCH);
  }
#line 21617 "parser.c" /* yacc.c:1646  */
    break;

  case 1925:
#line 12752 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_search ((yyvsp[-3]), (yyvsp[-2]), (yyvsp[-1]), (yyvsp[0]));
  }
#line 21625 "parser.c" /* yacc.c:1646  */
    break;

  case 1926:
#line 12757 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_statement->name = (const char *)"SEARCH ALL";
	cb_emit_search_all ((yyvsp[-4]), (yyvsp[-3]), (yyvsp[-1]), (yyvsp[0]));
  }
#line 21634 "parser.c" /* yacc.c:1646  */
    break;

  case 1927:
#line 12764 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 21640 "parser.c" /* yacc.c:1646  */
    break;

  case 1928:
#line 12765 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 21646 "parser.c" /* yacc.c:1646  */
    break;

  case 1929:
#line 12770 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 21654 "parser.c" /* yacc.c:1646  */
    break;

  case 1930:
#line 12775 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 21662 "parser.c" /* yacc.c:1646  */
    break;

  case 1931:
#line 12782 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 21670 "parser.c" /* yacc.c:1646  */
    break;

  case 1932:
#line 12786 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[0]), (yyvsp[-1]));
  }
#line 21678 "parser.c" /* yacc.c:1646  */
    break;

  case 1933:
#line 12794 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_if_check_break ((yyvsp[-1]), (yyvsp[0]));
  }
#line 21686 "parser.c" /* yacc.c:1646  */
    break;

  case 1934:
#line 12801 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), SEARCH);
  }
#line 21694 "parser.c" /* yacc.c:1646  */
    break;

  case 1935:
#line 12805 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), SEARCH);
  }
#line 21702 "parser.c" /* yacc.c:1646  */
    break;

  case 1936:
#line 12815 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("SEND", 0);
  }
#line 21710 "parser.c" /* yacc.c:1646  */
    break;

  case 1938:
#line 12823 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
  }
#line 21717 "parser.c" /* yacc.c:1646  */
    break;

  case 1939:
#line 12826 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
  }
#line 21724 "parser.c" /* yacc.c:1646  */
    break;

  case 1942:
#line 12837 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
  }
#line 21731 "parser.c" /* yacc.c:1646  */
    break;

  case 1949:
#line 12857 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("SET", 0);
	set_attr_val_on = 0;
	set_attr_val_off = 0;
	cobc_cs_check = CB_CS_SET;
  }
#line 21742 "parser.c" /* yacc.c:1646  */
    break;

  case 1950:
#line 12864 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_cs_check = 0;
  }
#line 21750 "parser.c" /* yacc.c:1646  */
    break;

  case 1959:
#line 12881 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 21756 "parser.c" /* yacc.c:1646  */
    break;

  case 1960:
#line 12882 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 21762 "parser.c" /* yacc.c:1646  */
    break;

  case 1961:
#line 12886 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 21768 "parser.c" /* yacc.c:1646  */
    break;

  case 1962:
#line 12887 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 21774 "parser.c" /* yacc.c:1646  */
    break;

  case 1963:
#line 12894 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_setenv ((yyvsp[-2]), (yyvsp[0]));
  }
#line 21782 "parser.c" /* yacc.c:1646  */
    break;

  case 1964:
#line 12903 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_attribute ((yyvsp[-2]), set_attr_val_on, set_attr_val_off);
  }
#line 21790 "parser.c" /* yacc.c:1646  */
    break;

  case 1967:
#line 12915 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_BELL);
  }
#line 21798 "parser.c" /* yacc.c:1646  */
    break;

  case 1968:
#line 12919 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_BLINK);
  }
#line 21806 "parser.c" /* yacc.c:1646  */
    break;

  case 1969:
#line 12923 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_HIGHLIGHT);
	check_not_highlight_and_lowlight (set_attr_val_on | set_attr_val_off,
					  COB_SCREEN_HIGHLIGHT);
  }
#line 21816 "parser.c" /* yacc.c:1646  */
    break;

  case 1970:
#line 12929 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_LOWLIGHT);
	check_not_highlight_and_lowlight (set_attr_val_on | set_attr_val_off,
					  COB_SCREEN_LOWLIGHT);
  }
#line 21826 "parser.c" /* yacc.c:1646  */
    break;

  case 1971:
#line 12935 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_REVERSE);
  }
#line 21834 "parser.c" /* yacc.c:1646  */
    break;

  case 1972:
#line 12939 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_UNDERLINE);
  }
#line 21842 "parser.c" /* yacc.c:1646  */
    break;

  case 1973:
#line 12943 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_LEFTLINE);
  }
#line 21850 "parser.c" /* yacc.c:1646  */
    break;

  case 1974:
#line 12947 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	bit_set_attr ((yyvsp[0]), COB_SCREEN_OVERLINE);
  }
#line 21858 "parser.c" /* yacc.c:1646  */
    break;

  case 1975:
#line 12956 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_to ((yyvsp[-3]), cb_build_ppointer ((yyvsp[0])));
  }
#line 21866 "parser.c" /* yacc.c:1646  */
    break;

  case 1976:
#line 12960 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_to ((yyvsp[-2]), (yyvsp[0]));
  }
#line 21874 "parser.c" /* yacc.c:1646  */
    break;

  case 1977:
#line 12964 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_move (cb_build_length ((yyvsp[0])), (yyvsp[-4]));
  }
#line 21882 "parser.c" /* yacc.c:1646  */
    break;

  case 1978:
#line 12973 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_up_down ((yyvsp[-3]), (yyvsp[-2]), (yyvsp[0]));
  }
#line 21890 "parser.c" /* yacc.c:1646  */
    break;

  case 1981:
#line 12987 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_on_off ((yyvsp[-2]), (yyvsp[0]));
  }
#line 21898 "parser.c" /* yacc.c:1646  */
    break;

  case 1984:
#line 13001 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_true ((yyvsp[-2]));
  }
#line 21906 "parser.c" /* yacc.c:1646  */
    break;

  case 1985:
#line 13005 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_false ((yyvsp[-2]));
  }
#line 21914 "parser.c" /* yacc.c:1646  */
    break;

  case 1986:
#line 13014 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	  cb_emit_set_last_exception_to_off ();
  }
#line 21922 "parser.c" /* yacc.c:1646  */
    break;

  case 1987:
#line 13023 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_set_thread_priority ((yyvsp[-3]), (yyvsp[0]));
	CB_PENDING ("THREAD");
  }
#line 21931 "parser.c" /* yacc.c:1646  */
    break;

  case 1988:
#line 13034 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("SORT", 0);
  }
#line 21939 "parser.c" /* yacc.c:1646  */
    break;

  case 1990:
#line 13042 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	x = cb_ref ((yyvsp[-3]));
	if (CB_VALID_TREE (x)) {
		if (CB_INVALID_TREE ((yyvsp[-2]))) {
			if (CB_FILE_P (x)) {
				cb_error (_("file sort requires KEY phrase"));
			} else {
				/* FIXME: use key definition from OCCURS */
				cb_error (_("%s is not implemented"), _("table SORT without keys"));
			}
			(yyval) = NULL;
		} else {
			cb_emit_sort_init ((yyvsp[-3]), (yyvsp[-2]), (yyvsp[0]));
			(yyval)= (yyvsp[-3]);
		}
	} else {
		(yyval) = NULL;
	}
  }
#line 21965 "parser.c" /* yacc.c:1646  */
    break;

  case 1991:
#line 13064 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-2]) && CB_VALID_TREE ((yyvsp[-6]))) {
		cb_emit_sort_finish ((yyvsp[-6]));
	}
  }
#line 21975 "parser.c" /* yacc.c:1646  */
    break;

  case 1992:
#line 13073 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 21983 "parser.c" /* yacc.c:1646  */
    break;

  case 1993:
#line 13078 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree l;
	cb_tree lparm;

	if ((yyvsp[0]) == NULL) {
		l = CB_LIST_INIT (NULL);
	} else {
		l = (yyvsp[0]);
	}
	lparm = l;
	for (; l; l = CB_CHAIN (l)) {
		CB_PURPOSE (l) = (yyvsp[-2]);
	}
	(yyval) = cb_list_append ((yyvsp[-4]), lparm);
  }
#line 22003 "parser.c" /* yacc.c:1646  */
    break;

  case 1994:
#line 13096 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 22009 "parser.c" /* yacc.c:1646  */
    break;

  case 1995:
#line 13097 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 22015 "parser.c" /* yacc.c:1646  */
    break;

  case 1997:
#line 13102 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	/* The OC sort is a stable sort. ie. Dups are per default in order */
	/* Therefore nothing to do here */
  }
#line 22024 "parser.c" /* yacc.c:1646  */
    break;

  case 1998:
#line 13109 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_null; }
#line 22030 "parser.c" /* yacc.c:1646  */
    break;

  case 1999:
#line 13110 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_ref ((yyvsp[0])); }
#line 22036 "parser.c" /* yacc.c:1646  */
    break;

  case 2000:
#line 13115 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0]) && CB_FILE_P (cb_ref ((yyvsp[0])))) {
		cb_error (_("file sort requires USING or INPUT PROCEDURE"));
	}
  }
#line 22046 "parser.c" /* yacc.c:1646  */
    break;

  case 2001:
#line 13121 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-2])) {
		if (!CB_FILE_P (cb_ref ((yyvsp[-2])))) {
			cb_error (_("USING invalid with table SORT"));
		} else {
			cb_emit_sort_using ((yyvsp[-2]), (yyvsp[0]));
		}
	}
  }
#line 22060 "parser.c" /* yacc.c:1646  */
    break;

  case 2002:
#line 13131 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-4])) {
		if (!CB_FILE_P (cb_ref ((yyvsp[-4])))) {
			cb_error (_("INPUT PROCEDURE invalid with table SORT"));
		} else if (current_statement->flag_merge) {
			cb_error (_("INPUT PROCEDURE invalid with MERGE"));
		} else {
			cb_emit_sort_input ((yyvsp[0]));
		}
	}
	cobc_cs_check = 0;
  }
#line 22077 "parser.c" /* yacc.c:1646  */
    break;

  case 2003:
#line 13147 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (0)]) && CB_FILE_P (cb_ref ((yyvsp[(-1) - (0)])))) {
		cb_error (_("file sort requires GIVING or OUTPUT PROCEDURE"));
	}
  }
#line 22087 "parser.c" /* yacc.c:1646  */
    break;

  case 2004:
#line 13153 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (2)])) {
		if (!CB_FILE_P (cb_ref ((yyvsp[(-1) - (2)])))) {
			cb_error (_("GIVING invalid with table SORT"));
		} else {
			cb_emit_sort_giving ((yyvsp[(-1) - (2)]), (yyvsp[0]));
		}
	}
  }
#line 22101 "parser.c" /* yacc.c:1646  */
    break;

  case 2005:
#line 13163 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[(-1) - (4)])) {
		if (!CB_FILE_P (cb_ref ((yyvsp[(-1) - (4)])))) {
			cb_error (_("OUTPUT PROCEDURE invalid with table SORT"));
		} else {
			cb_emit_sort_output ((yyvsp[0]));
		}
	}
	cobc_cs_check = 0;
  }
#line 22116 "parser.c" /* yacc.c:1646  */
    break;

  case 2006:
#line 13180 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("START", TERM_START);
	start_tree = cb_int (COB_EQ);
  }
#line 22125 "parser.c" /* yacc.c:1646  */
    break;

  case 2008:
#line 13190 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[-1]) && !(yyvsp[-2])) {
		cb_error_x (CB_TREE (current_statement),
			    _("SIZE/LENGTH invalid here"));
	} else {
		cb_emit_start ((yyvsp[-3]), start_tree, (yyvsp[-2]), (yyvsp[-1]));
	}
  }
#line 22138 "parser.c" /* yacc.c:1646  */
    break;

  case 2009:
#line 13202 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 22146 "parser.c" /* yacc.c:1646  */
    break;

  case 2010:
#line 13206 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 22154 "parser.c" /* yacc.c:1646  */
    break;

  case 2011:
#line 13213 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 22162 "parser.c" /* yacc.c:1646  */
    break;

  case 2012:
#line 13217 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	start_tree = (yyvsp[-1]);
	(yyval) = (yyvsp[0]);
  }
#line 22171 "parser.c" /* yacc.c:1646  */
    break;

  case 2013:
#line 13222 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	start_tree = cb_int (COB_FI);
	(yyval) = NULL;
  }
#line 22180 "parser.c" /* yacc.c:1646  */
    break;

  case 2014:
#line 13227 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	start_tree = cb_int (COB_LA);
	(yyval) = NULL;
  }
#line 22189 "parser.c" /* yacc.c:1646  */
    break;

  case 2015:
#line 13234 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_EQ); }
#line 22195 "parser.c" /* yacc.c:1646  */
    break;

  case 2016:
#line 13235 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int ((yyvsp[-1]) ? COB_LE : COB_GT); }
#line 22201 "parser.c" /* yacc.c:1646  */
    break;

  case 2017:
#line 13236 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int ((yyvsp[-1]) ? COB_GE : COB_LT); }
#line 22207 "parser.c" /* yacc.c:1646  */
    break;

  case 2018:
#line 13237 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int ((yyvsp[-1]) ? COB_LT : COB_GE); }
#line 22213 "parser.c" /* yacc.c:1646  */
    break;

  case 2019:
#line 13238 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int ((yyvsp[-1]) ? COB_GT : COB_LE); }
#line 22219 "parser.c" /* yacc.c:1646  */
    break;

  case 2020:
#line 13239 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (COB_NE); }
#line 22225 "parser.c" /* yacc.c:1646  */
    break;

  case 2021:
#line 13244 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_error_x (CB_TREE (current_statement),
		    _("NOT EQUAL condition not allowed on START statement"));
  }
#line 22234 "parser.c" /* yacc.c:1646  */
    break;

  case 2024:
#line 13257 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), START);
  }
#line 22242 "parser.c" /* yacc.c:1646  */
    break;

  case 2025:
#line 13261 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), START);
  }
#line 22250 "parser.c" /* yacc.c:1646  */
    break;

  case 2026:
#line 13271 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("STOP RUN", 0);
	cobc_cs_check = CB_CS_STOP;
  }
#line 22259 "parser.c" /* yacc.c:1646  */
    break;

  case 2027:
#line 13276 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_stop_run ((yyvsp[0]));
	check_unreached = 1;
	cobc_cs_check = 0;
  }
#line 22269 "parser.c" /* yacc.c:1646  */
    break;

  case 2028:
#line 13282 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("STOP", 0);
	cb_emit_display (CB_LIST_INIT ((yyvsp[0])), cb_int0, cb_int1, NULL,
			 NULL, 1, DEVICE_DISPLAY);
	cb_emit_accept (cb_null, NULL, NULL);
	cobc_cs_check = 0;
  }
#line 22281 "parser.c" /* yacc.c:1646  */
    break;

  case 2029:
#line 13290 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("STOP THREAD", 0);
	cb_emit_stop_thread ((yyvsp[0]));
	cobc_cs_check = 0;
	cb_warning_x (COBC_WARN_FILLER, (yyvsp[0]), _("%s is replaced by %s"), "STOP THREAD", "STOP RUN");
  }
#line 22292 "parser.c" /* yacc.c:1646  */
    break;

  case 2030:
#line 13300 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (current_program->cb_return_code) {
		(yyval) = current_program->cb_return_code;
	} else {
		(yyval) = cb_int0;
	}
  }
#line 22304 "parser.c" /* yacc.c:1646  */
    break;

  case 2031:
#line 13308 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 22312 "parser.c" /* yacc.c:1646  */
    break;

  case 2032:
#line 13312 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 22320 "parser.c" /* yacc.c:1646  */
    break;

  case 2033:
#line 13316 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		(yyval) = (yyvsp[0]);
	} else {
		(yyval) = cb_int1;
	}
  }
#line 22332 "parser.c" /* yacc.c:1646  */
    break;

  case 2034:
#line 13324 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		(yyval) = (yyvsp[0]);
	} else {
		(yyval) = cb_int0;
	}
  }
#line 22344 "parser.c" /* yacc.c:1646  */
    break;

  case 2035:
#line 13335 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 22352 "parser.c" /* yacc.c:1646  */
    break;

  case 2036:
#line 13339 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 22360 "parser.c" /* yacc.c:1646  */
    break;

  case 2037:
#line 13346 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_stop_literal_statement, _("STOP literal"));
  }
#line 22368 "parser.c" /* yacc.c:1646  */
    break;

  case 2038:
#line 13350 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_stop_identifier_statement, _("STOP identifier"));
  }
#line 22376 "parser.c" /* yacc.c:1646  */
    break;

  case 2039:
#line 13356 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 22382 "parser.c" /* yacc.c:1646  */
    break;

  case 2040:
#line 13357 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_space; }
#line 22388 "parser.c" /* yacc.c:1646  */
    break;

  case 2041:
#line 13358 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_zero; }
#line 22394 "parser.c" /* yacc.c:1646  */
    break;

  case 2042:
#line 13359 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_quote; }
#line 22400 "parser.c" /* yacc.c:1646  */
    break;

  case 2043:
#line 13366 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("STRING", TERM_STRING);
  }
#line 22408 "parser.c" /* yacc.c:1646  */
    break;

  case 2045:
#line 13375 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_string ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[-1]));
  }
#line 22416 "parser.c" /* yacc.c:1646  */
    break;

  case 2046:
#line 13381 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	save_tree = NULL;
  }
#line 22424 "parser.c" /* yacc.c:1646  */
    break;

  case 2047:
#line 13385 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = save_tree;
  }
#line 22432 "parser.c" /* yacc.c:1646  */
    break;

  case 2050:
#line 13397 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (!save_tree) {
		save_tree = CB_LIST_INIT ((yyvsp[-1]));
	} else {
		save_tree = cb_list_add (save_tree, (yyvsp[-1]));
	}
	if ((yyvsp[0])) {
		save_tree = cb_list_add (save_tree, (yyvsp[0]));
	}
  }
#line 22447 "parser.c" /* yacc.c:1646  */
    break;

  case 2051:
#line 13410 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 22453 "parser.c" /* yacc.c:1646  */
    break;

  case 2052:
#line 13412 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 22459 "parser.c" /* yacc.c:1646  */
    break;

  case 2053:
#line 13416 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = CB_BUILD_PAIR (cb_int0, NULL); }
#line 22465 "parser.c" /* yacc.c:1646  */
    break;

  case 2054:
#line 13417 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = CB_BUILD_PAIR ((yyvsp[0]), NULL); }
#line 22471 "parser.c" /* yacc.c:1646  */
    break;

  case 2055:
#line 13421 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 22477 "parser.c" /* yacc.c:1646  */
    break;

  case 2056:
#line 13422 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 22483 "parser.c" /* yacc.c:1646  */
    break;

  case 2057:
#line 13427 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), STRING);
  }
#line 22491 "parser.c" /* yacc.c:1646  */
    break;

  case 2058:
#line 13431 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), STRING);
  }
#line 22499 "parser.c" /* yacc.c:1646  */
    break;

  case 2059:
#line 13441 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("SUBTRACT", TERM_SUBTRACT);
  }
#line 22507 "parser.c" /* yacc.c:1646  */
    break;

  case 2061:
#line 13450 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), '-', cb_build_binary_list ((yyvsp[-3]), '+'));
  }
#line 22515 "parser.c" /* yacc.c:1646  */
    break;

  case 2062:
#line 13454 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_arithmetic ((yyvsp[-1]), 0, cb_build_binary_list (CB_BUILD_CHAIN ((yyvsp[-3]), (yyvsp[-5])), '-'));
  }
#line 22523 "parser.c" /* yacc.c:1646  */
    break;

  case 2063:
#line 13458 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_corresponding (cb_build_sub, (yyvsp[-2]), (yyvsp[-4]), (yyvsp[-1]));
  }
#line 22531 "parser.c" /* yacc.c:1646  */
    break;

  case 2064:
#line 13462 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING ("SUBTRACT TABLE");
	cb_emit_tab_arithmetic (cb_build_sub, (yyvsp[-4]), (yyvsp[-6]), (yyvsp[-3]), (yyvsp[-2]), (yyvsp[-1]));
  }
#line 22540 "parser.c" /* yacc.c:1646  */
    break;

  case 2065:
#line 13470 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), SUBTRACT);
  }
#line 22548 "parser.c" /* yacc.c:1646  */
    break;

  case 2066:
#line 13474 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), SUBTRACT);
  }
#line 22556 "parser.c" /* yacc.c:1646  */
    break;

  case 2067:
#line 13484 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("SUPPRESS", 0);
	if (!in_declaratives) {
		cb_error_x (CB_TREE (current_statement),
			    _("SUPPRESS statement must be within DECLARATIVES"));
	}
	cb_emit_suppress (control_field);
  }
#line 22569 "parser.c" /* yacc.c:1646  */
    break;

  case 2070:
#line 13502 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("TERMINATE", 0);
  }
#line 22577 "parser.c" /* yacc.c:1646  */
    break;

  case 2072:
#line 13510 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	if ((yyvsp[0]) != cb_error_node) {
	    cb_emit_terminate ((yyvsp[0]));
	}
  }
#line 22588 "parser.c" /* yacc.c:1646  */
    break;

  case 2073:
#line 13517 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_implicit_statement ();
	if ((yyvsp[0]) != cb_error_node) {
		cb_emit_terminate ((yyvsp[0]));
	}
  }
#line 22599 "parser.c" /* yacc.c:1646  */
    break;

  case 2074:
#line 13529 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("TRANSFORM", 0);
  }
#line 22607 "parser.c" /* yacc.c:1646  */
    break;

  case 2076:
#line 13537 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	x = cb_build_converting ((yyvsp[-2]), (yyvsp[0]), cb_build_inspect_region_start ());
	cb_emit_inspect ((yyvsp[-4]), x, TRANSFORM_STATEMENT);
  }
#line 22618 "parser.c" /* yacc.c:1646  */
    break;

  case 2077:
#line 13550 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("UNLOCK", 0);
  }
#line 22626 "parser.c" /* yacc.c:1646  */
    break;

  case 2079:
#line 13558 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_VALID_TREE ((yyvsp[-1]))) {
		if (CB_FILE (cb_ref ((yyvsp[-1])))->organization == COB_ORG_SORT) {
			cb_error_x (CB_TREE (current_statement),
				    _("UNLOCK invalid for SORT files"));
		} else {
			cb_emit_unlock ((yyvsp[-1]));
		}
	}
  }
#line 22641 "parser.c" /* yacc.c:1646  */
    break;

  case 2080:
#line 13574 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("UNSTRING", TERM_UNSTRING);
  }
#line 22649 "parser.c" /* yacc.c:1646  */
    break;

  case 2082:
#line 13585 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_emit_unstring ((yyvsp[-5]), (yyvsp[-4]), (yyvsp[-3]), (yyvsp[-2]), (yyvsp[-1]));
  }
#line 22657 "parser.c" /* yacc.c:1646  */
    break;

  case 2083:
#line 13591 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 22663 "parser.c" /* yacc.c:1646  */
    break;

  case 2084:
#line 13593 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 22669 "parser.c" /* yacc.c:1646  */
    break;

  case 2085:
#line 13597 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 22675 "parser.c" /* yacc.c:1646  */
    break;

  case 2086:
#line 13599 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0])); }
#line 22681 "parser.c" /* yacc.c:1646  */
    break;

  case 2087:
#line 13604 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_unstring_delimited ((yyvsp[-1]), (yyvsp[0]));
  }
#line 22689 "parser.c" /* yacc.c:1646  */
    break;

  case 2088:
#line 13610 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 22695 "parser.c" /* yacc.c:1646  */
    break;

  case 2089:
#line 13612 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 22701 "parser.c" /* yacc.c:1646  */
    break;

  case 2090:
#line 13617 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_unstring_into ((yyvsp[-2]), (yyvsp[-1]), (yyvsp[0]));
  }
#line 22709 "parser.c" /* yacc.c:1646  */
    break;

  case 2091:
#line 13623 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 22715 "parser.c" /* yacc.c:1646  */
    break;

  case 2092:
#line 13624 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 22721 "parser.c" /* yacc.c:1646  */
    break;

  case 2093:
#line 13628 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 22727 "parser.c" /* yacc.c:1646  */
    break;

  case 2094:
#line 13629 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 22733 "parser.c" /* yacc.c:1646  */
    break;

  case 2095:
#line 13633 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 22739 "parser.c" /* yacc.c:1646  */
    break;

  case 2096:
#line 13634 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 22745 "parser.c" /* yacc.c:1646  */
    break;

  case 2097:
#line 13639 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), UNSTRING);
  }
#line 22753 "parser.c" /* yacc.c:1646  */
    break;

  case 2098:
#line 13643 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), UNSTRING);
  }
#line 22761 "parser.c" /* yacc.c:1646  */
    break;

  case 2099:
#line 13652 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("VALIDATE", 0);
  }
#line 22769 "parser.c" /* yacc.c:1646  */
    break;

  case 2100:
#line 13656 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
#if 0	/* FIXME: at least add syntax checks here */
	cb_emit_validate ((yyvsp[0]));
#else
	CB_PENDING ("VALIDATE");
#endif
  }
#line 22781 "parser.c" /* yacc.c:1646  */
    break;

  case 2101:
#line 13667 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_validate_item ((yyvsp[0]));
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 22790 "parser.c" /* yacc.c:1646  */
    break;

  case 2102:
#line 13672 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_validate_item ((yyvsp[0]));
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 22799 "parser.c" /* yacc.c:1646  */
    break;

  case 2103:
#line 13683 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	skip_statements = 0;
	in_debugging = 0;
  }
#line 22808 "parser.c" /* yacc.c:1646  */
    break;

  case 2110:
#line 13701 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (!in_declaratives) {
		cb_error (_("USE statement must be within DECLARATIVES"));
	} else if (!current_section) {
		cb_error (_("SECTION header missing before USE statement"));
	} else {
		current_section->flag_begin = 1;
		current_section->flag_return = 1;
		current_section->flag_declarative_exit = 1;
		current_section->flag_real_label = 1;
		current_section->flag_skip_label = 0;
		CB_EXCEPTION_ENABLE (COB_EC_I_O) = 1;
		if (use_global_ind) {
			current_section->flag_global = 1;
			current_program->global_list =
				cb_list_add (current_program->global_list,
					     CB_TREE (current_section));
		}
		emit_statement (cb_build_comment ("USE AFTER ERROR"));
	}
  }
#line 22834 "parser.c" /* yacc.c:1646  */
    break;

  case 2111:
#line 13726 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	use_global_ind = 0;
  }
#line 22842 "parser.c" /* yacc.c:1646  */
    break;

  case 2112:
#line 13730 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("%s is invalid in a user FUNCTION"), "GLOBAL");
	} else {
		use_global_ind = 1;
		current_program->flag_global_use = 1;
	}
  }
#line 22855 "parser.c" /* yacc.c:1646  */
    break;

  case 2113:
#line 13742 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree		l;

	for (l = (yyvsp[0]); l; l = CB_CHAIN (l)) {
		if (CB_VALID_TREE (CB_VALUE (l))) {
			setup_use_file (CB_FILE (cb_ref (CB_VALUE (l))));
		}
	}
  }
#line 22869 "parser.c" /* yacc.c:1646  */
    break;

  case 2114:
#line 13752 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_program->global_handler[COB_OPEN_INPUT].handler_label = current_section;
	current_program->global_handler[COB_OPEN_INPUT].handler_prog = current_program;
  }
#line 22878 "parser.c" /* yacc.c:1646  */
    break;

  case 2115:
#line 13757 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_program->global_handler[COB_OPEN_OUTPUT].handler_label = current_section;
	current_program->global_handler[COB_OPEN_OUTPUT].handler_prog = current_program;
  }
#line 22887 "parser.c" /* yacc.c:1646  */
    break;

  case 2116:
#line 13762 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_program->global_handler[COB_OPEN_I_O].handler_label = current_section;
	current_program->global_handler[COB_OPEN_I_O].handler_prog = current_program;
  }
#line 22896 "parser.c" /* yacc.c:1646  */
    break;

  case 2117:
#line 13767 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_program->global_handler[COB_OPEN_EXTEND].handler_label = current_section;
	current_program->global_handler[COB_OPEN_EXTEND].handler_prog = current_program;
  }
#line 22905 "parser.c" /* yacc.c:1646  */
    break;

  case 2118:
#line 13775 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree		plabel;
	char		name[64];

	cb_verify (cb_use_for_debugging, "USE FOR DEBUGGING");

	if (!in_declaratives) {
		cb_error (_("USE statement must be within DECLARATIVES"));
	} else if (current_program->nested_level) {
		cb_error (_("USE DEBUGGING not supported in contained program"));
	} else {
		in_debugging = 1;
		current_section->flag_begin = 1;
		current_section->flag_return = 1;
		current_section->flag_declarative_exit = 1;
		current_section->flag_real_label = 0;
		current_section->flag_is_debug_sect = 1;
		if (!needs_debug_item) {
			needs_debug_item = 1;
			cb_build_debug_item ();
		}
		if (!current_program->flag_debugging) {
			skip_statements = 1;
			current_section->flag_skip_label = 1;
		} else {
			current_program->flag_gen_debug = 1;
			sprintf (name, "EXIT SECTION %d", cb_id);
			plabel = cb_build_reference (name);
			plabel = cb_build_label (plabel, NULL);
			CB_LABEL (plabel)->flag_begin = 1;
			CB_LABEL (plabel)->flag_dummy_exit = 1;
			current_section->exit_label = plabel;
			emit_statement (cb_build_comment ("USE FOR DEBUGGING"));
		}
	}
  }
#line 22946 "parser.c" /* yacc.c:1646  */
    break;

  case 2121:
#line 13820 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree		l;
	cb_tree		x;
	cb_tree		z;

	if (current_program->flag_debugging) {
		CB_REFERENCE ((yyvsp[0]))->debug_section = current_section;
		CB_REFERENCE ((yyvsp[0]))->flag_debug_code = 1;
		CB_REFERENCE ((yyvsp[0]))->flag_all_debug = 0;

		z = CB_LIST_INIT ((yyvsp[0]));
		current_program->debug_list =
			cb_list_append (current_program->debug_list, z);
		/* Check backward refs to file/data names */
		/* Label refs will be checked later (forward/backward ref) */
		if (CB_WORD_COUNT ((yyvsp[0])) > 0) {
			l = CB_VALUE (CB_WORD_ITEMS ((yyvsp[0])));
			switch (CB_TREE_TAG (l)) {
			case CB_TAG_CD:
				CB_CD (l)->debug_section = current_section;
				CB_CD (l)->flag_field_debug = 1;
				break;
			case CB_TAG_FILE:
				CB_FILE (l)->debug_section = current_section;
				CB_FILE (l)->flag_fl_debug = 1;
				break;
			case CB_TAG_FIELD:
				x = cb_ref ((yyvsp[0]));
				if (CB_INVALID_TREE (x)) {
					break;
				}
				needs_field_debug = 1;
				CB_FIELD (x)->debug_section = current_section;
				CB_FIELD (x)->flag_field_debug = 1;
				CB_PURPOSE (z) = x;
				break;
			default:
				break;
			}
		}
	}
  }
#line 22993 "parser.c" /* yacc.c:1646  */
    break;

  case 2122:
#line 13863 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (current_program->flag_debugging) {
		if (current_program->all_procedure) {
			cb_error (_("duplicate USE DEBUGGING ON ALL PROCEDURES"));
		} else {
			current_program->all_procedure = current_section;
		}
	}
  }
#line 23007 "parser.c" /* yacc.c:1646  */
    break;

  case 2123:
#line 13873 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;

	if (current_program->flag_debugging) {
		/* Reference must be a data item */
		x = cb_ref ((yyvsp[0]));
		if (CB_INVALID_TREE (x) || !CB_FIELD_P (x)) {
			cb_error (_("invalid target for %s"), "DEBUGGING ALL");
		} else {
			needs_field_debug = 1;
			CB_FIELD (x)->debug_section = current_section;
			CB_FIELD (x)->flag_field_debug = 1;
			CB_FIELD (x)->flag_all_debug = 1;
			CB_REFERENCE ((yyvsp[0]))->debug_section = current_section;
			CB_REFERENCE ((yyvsp[0]))->flag_debug_code = 1;
			CB_REFERENCE ((yyvsp[0]))->flag_all_debug = 1;
			CB_CHAIN_PAIR (current_program->debug_list, x, (yyvsp[0]));
		}
	}
  }
#line 23032 "parser.c" /* yacc.c:1646  */
    break;

  case 2128:
#line 13903 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (current_program->nested_level) {
		cb_error (_("%s is invalid in nested program"), "USE AT");
	}
  }
#line 23042 "parser.c" /* yacc.c:1646  */
    break;

  case 2129:
#line 13912 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	emit_statement (cb_build_comment ("USE AT PROGRAM START"));
	backup_current_pos ();
	CB_PENDING ("USE AT PROGRAM START");
	/* emit_entry ("_AT_START", 0, NULL, NULL); */
  }
#line 23053 "parser.c" /* yacc.c:1646  */
    break;

  case 2130:
#line 13919 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	emit_statement (cb_build_comment ("USE AT PROGRAM END"));
	backup_current_pos ();
	CB_PENDING ("USE AT PROGRAM END");
	/* emit_entry ("_AT_END", 0, NULL, NULL); */
  }
#line 23064 "parser.c" /* yacc.c:1646  */
    break;

  case 2131:
#line 13930 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	char wrk[80];
	cb_tree x;
	struct cb_field		*f;
	struct cb_report	*r;

	x = cb_ref ((yyvsp[0]));
	if (!CB_FIELD_P (x)) {
		cb_error_x ((yyvsp[0]), _("'%s' is not a report group"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	} else {
		control_field = f = CB_FIELD (x);
		f->report_decl_id = current_section->id;
		if((r = f->report) != NULL) {
			r->has_declarative = 1;
		}
	}
	sprintf(wrk,"USE BEFORE REPORTING %s is l_%d",cb_name((yyvsp[0])),current_section->id);
	current_section->flag_real_label = 1;
	current_section->flag_declaratives = 1;
	current_section->flag_begin = 1;
	current_section->flag_return = 1;
	current_section->flag_declarative_exit = 1;
	current_section->flag_real_label = 1;
	current_section->flag_skip_label = 0;
	emit_statement (cb_build_comment (strdup(wrk)));
  }
#line 23096 "parser.c" /* yacc.c:1646  */
    break;

  case 2132:
#line 13961 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_section->flag_real_label = 1;
	emit_statement (cb_build_comment ("USE AFTER EXCEPTION CONDITION"));
	CB_PENDING ("USE AFTER EXCEPTION CONDITION");
  }
#line 23106 "parser.c" /* yacc.c:1646  */
    break;

  case 2135:
#line 13977 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	begin_statement ("WRITE", TERM_WRITE);
	/* Special in debugging mode */
	save_debug = start_debug;
	start_debug = 0;
  }
#line 23117 "parser.c" /* yacc.c:1646  */
    break;

  case 2137:
#line 13989 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_VALID_TREE ((yyvsp[-5]))) {
		cb_emit_write ((yyvsp[-5]), (yyvsp[-4]), (yyvsp[-3]), (yyvsp[-1]));
	}
	start_debug = save_debug;
  }
#line 23128 "parser.c" /* yacc.c:1646  */
    break;

  case 2138:
#line 13998 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 23134 "parser.c" /* yacc.c:1646  */
    break;

  case 2139:
#line 13999 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 23140 "parser.c" /* yacc.c:1646  */
    break;

  case 2140:
#line 14004 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int0;
  }
#line 23148 "parser.c" /* yacc.c:1646  */
    break;

  case 2141:
#line 14008 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_write_advancing_lines ((yyvsp[-3]), (yyvsp[-1]));
  }
#line 23156 "parser.c" /* yacc.c:1646  */
    break;

  case 2142:
#line 14012 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_write_advancing_mnemonic ((yyvsp[-2]), (yyvsp[0]));
  }
#line 23164 "parser.c" /* yacc.c:1646  */
    break;

  case 2143:
#line 14016 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_write_advancing_page ((yyvsp[-2]));
  }
#line 23172 "parser.c" /* yacc.c:1646  */
    break;

  case 2144:
#line 14022 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = CB_BEFORE; }
#line 23178 "parser.c" /* yacc.c:1646  */
    break;

  case 2145:
#line 14023 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = CB_AFTER; }
#line 23184 "parser.c" /* yacc.c:1646  */
    break;

  case 2149:
#line 14034 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_WARNING ((yyvsp[(-2) - (0)]), WRITE);
  }
#line 23192 "parser.c" /* yacc.c:1646  */
    break;

  case 2150:
#line 14038 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	TERMINATOR_CLEAR ((yyvsp[(-2) - (1)]), WRITE);
  }
#line 23200 "parser.c" /* yacc.c:1646  */
    break;

  case 2153:
#line 14052 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT EXCEPTION before EXCEPTION"));
	}
  }
#line 23211 "parser.c" /* yacc.c:1646  */
    break;

  case 2154:
#line 14062 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 23219 "parser.c" /* yacc.c:1646  */
    break;

  case 2155:
#line 14066 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 23227 "parser.c" /* yacc.c:1646  */
    break;

  case 2156:
#line 14073 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = ACCEPT_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 23236 "parser.c" /* yacc.c:1646  */
    break;

  case 2161:
#line 14091 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = ACCEPT_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 23245 "parser.c" /* yacc.c:1646  */
    break;

  case 2166:
#line 14107 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT EXCEPTION before EXCEPTION"));
	}
  }
#line 23256 "parser.c" /* yacc.c:1646  */
    break;

  case 2167:
#line 14117 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 23264 "parser.c" /* yacc.c:1646  */
    break;

  case 2168:
#line 14121 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 23272 "parser.c" /* yacc.c:1646  */
    break;

  case 2169:
#line 14128 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = DISPLAY_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 23281 "parser.c" /* yacc.c:1646  */
    break;

  case 2172:
#line 14141 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = DISPLAY_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 23290 "parser.c" /* yacc.c:1646  */
    break;

  case 2175:
#line 14153 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT SIZE ERROR before SIZE ERROR"));
	}
  }
#line 23301 "parser.c" /* yacc.c:1646  */
    break;

  case 2176:
#line 14163 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 23309 "parser.c" /* yacc.c:1646  */
    break;

  case 2177:
#line 14167 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 23317 "parser.c" /* yacc.c:1646  */
    break;

  case 2178:
#line 14174 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = SIZE_ERROR_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 23326 "parser.c" /* yacc.c:1646  */
    break;

  case 2181:
#line 14187 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = SIZE_ERROR_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 23335 "parser.c" /* yacc.c:1646  */
    break;

  case 2184:
#line 14199 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT OVERFLOW before OVERFLOW"));
	}
  }
#line 23346 "parser.c" /* yacc.c:1646  */
    break;

  case 2185:
#line 14209 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 23354 "parser.c" /* yacc.c:1646  */
    break;

  case 2186:
#line 14213 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 23362 "parser.c" /* yacc.c:1646  */
    break;

  case 2187:
#line 14220 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = OVERFLOW_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 23371 "parser.c" /* yacc.c:1646  */
    break;

  case 2190:
#line 14233 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = OVERFLOW_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 23380 "parser.c" /* yacc.c:1646  */
    break;

  case 2192:
#line 14245 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_verify (cb_not_exception_before_exception, "NOT AT END before AT END");
  }
#line 23388 "parser.c" /* yacc.c:1646  */
    break;

  case 2194:
#line 14254 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception, "NOT AT END before AT END");
	}
  }
#line 23398 "parser.c" /* yacc.c:1646  */
    break;

  case 2195:
#line 14263 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 23406 "parser.c" /* yacc.c:1646  */
    break;

  case 2196:
#line 14267 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 23414 "parser.c" /* yacc.c:1646  */
    break;

  case 2197:
#line 14274 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = AT_END_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 23423 "parser.c" /* yacc.c:1646  */
    break;

  case 2200:
#line 14287 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = AT_END_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 23432 "parser.c" /* yacc.c:1646  */
    break;

  case 2202:
#line 14298 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT AT END-OF-PAGE before AT END-OF-PAGE"));
	}
  }
#line 23443 "parser.c" /* yacc.c:1646  */
    break;

  case 2203:
#line 14308 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 23451 "parser.c" /* yacc.c:1646  */
    break;

  case 2204:
#line 14312 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 23459 "parser.c" /* yacc.c:1646  */
    break;

  case 2205:
#line 14319 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = EOP_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 23468 "parser.c" /* yacc.c:1646  */
    break;

  case 2208:
#line 14332 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = EOP_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 23477 "parser.c" /* yacc.c:1646  */
    break;

  case 2212:
#line 14348 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT INVALID KEY before INVALID KEY"));
	}
  }
#line 23488 "parser.c" /* yacc.c:1646  */
    break;

  case 2213:
#line 14358 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 23496 "parser.c" /* yacc.c:1646  */
    break;

  case 2214:
#line 14362 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
  }
#line 23504 "parser.c" /* yacc.c:1646  */
    break;

  case 2215:
#line 14369 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = INVALID_KEY_HANDLER;
	current_statement->ex_handler = (yyvsp[0]);
  }
#line 23513 "parser.c" /* yacc.c:1646  */
    break;

  case 2218:
#line 14382 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_statement->handler_type = INVALID_KEY_HANDLER;
	current_statement->not_ex_handler = (yyvsp[0]);
  }
#line 23522 "parser.c" /* yacc.c:1646  */
    break;

  case 2219:
#line 14392 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 23530 "parser.c" /* yacc.c:1646  */
    break;

  case 2220:
#line 14396 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int1;
	CB_PENDING ("THREAD");
  }
#line 23539 "parser.c" /* yacc.c:1646  */
    break;

  case 2221:
#line 14404 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 23547 "parser.c" /* yacc.c:1646  */
    break;

  case 2222:
#line 14408 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	CB_PENDING ("THREAD");
  }
#line 23556 "parser.c" /* yacc.c:1646  */
    break;

  case 2223:
#line 14416 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 23564 "parser.c" /* yacc.c:1646  */
    break;

  case 2224:
#line 14420 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 23572 "parser.c" /* yacc.c:1646  */
    break;

  case 2225:
#line 14429 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_one;
  }
#line 23580 "parser.c" /* yacc.c:1646  */
    break;

  case 2226:
#line 14433 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
  }
#line 23588 "parser.c" /* yacc.c:1646  */
    break;

  case 2227:
#line 14443 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_cond ((yyvsp[0]));
	cb_end_cond ((yyval));
  }
#line 23597 "parser.c" /* yacc.c:1646  */
    break;

  case 2228:
#line 14451 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_expr ((yyvsp[0]));
  }
#line 23605 "parser.c" /* yacc.c:1646  */
    break;

  case 2229:
#line 14457 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	current_expr = NULL;
	cb_exp_line = cb_source_line;
  }
#line 23614 "parser.c" /* yacc.c:1646  */
    break;

  case 2230:
#line 14462 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_reverse (current_expr);
  }
#line 23622 "parser.c" /* yacc.c:1646  */
    break;

  case 2233:
#line 14473 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr ('x', (yyvsp[0])); }
#line 23628 "parser.c" /* yacc.c:1646  */
    break;

  case 2236:
#line 14478 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr ('x', cb_zero); }
#line 23634 "parser.c" /* yacc.c:1646  */
    break;

  case 2237:
#line 14480 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr ('(', NULL); }
#line 23640 "parser.c" /* yacc.c:1646  */
    break;

  case 2238:
#line 14481 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr (')', NULL); }
#line 23646 "parser.c" /* yacc.c:1646  */
    break;

  case 2239:
#line 14483 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr ('+', NULL); }
#line 23652 "parser.c" /* yacc.c:1646  */
    break;

  case 2240:
#line 14484 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr ('-', NULL); }
#line 23658 "parser.c" /* yacc.c:1646  */
    break;

  case 2241:
#line 14485 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr ('*', NULL); }
#line 23664 "parser.c" /* yacc.c:1646  */
    break;

  case 2242:
#line 14486 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr ('/', NULL); }
#line 23670 "parser.c" /* yacc.c:1646  */
    break;

  case 2243:
#line 14487 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr ('^', NULL); }
#line 23676 "parser.c" /* yacc.c:1646  */
    break;

  case 2245:
#line 14490 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr ('&', NULL); }
#line 23682 "parser.c" /* yacc.c:1646  */
    break;

  case 2246:
#line 14491 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr ('|', NULL); }
#line 23688 "parser.c" /* yacc.c:1646  */
    break;

  case 2249:
#line 14500 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr ('!', NULL); }
#line 23694 "parser.c" /* yacc.c:1646  */
    break;

  case 2250:
#line 14503 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr ('C', (yyvsp[0])); }
#line 23700 "parser.c" /* yacc.c:1646  */
    break;

  case 2251:
#line 14505 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr ('=', NULL); }
#line 23706 "parser.c" /* yacc.c:1646  */
    break;

  case 2252:
#line 14506 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr ('>', NULL); }
#line 23712 "parser.c" /* yacc.c:1646  */
    break;

  case 2253:
#line 14507 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr ('<', NULL); }
#line 23718 "parser.c" /* yacc.c:1646  */
    break;

  case 2254:
#line 14508 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr (']', NULL); }
#line 23724 "parser.c" /* yacc.c:1646  */
    break;

  case 2255:
#line 14509 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr ('[', NULL); }
#line 23730 "parser.c" /* yacc.c:1646  */
    break;

  case 2256:
#line 14510 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr ('~', NULL); }
#line 23736 "parser.c" /* yacc.c:1646  */
    break;

  case 2257:
#line 14512 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr ('O', NULL); }
#line 23742 "parser.c" /* yacc.c:1646  */
    break;

  case 2258:
#line 14513 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr ('9', NULL); }
#line 23748 "parser.c" /* yacc.c:1646  */
    break;

  case 2259:
#line 14514 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr ('A', NULL); }
#line 23754 "parser.c" /* yacc.c:1646  */
    break;

  case 2260:
#line 14515 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr ('L', NULL); }
#line 23760 "parser.c" /* yacc.c:1646  */
    break;

  case 2261:
#line 14516 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr ('U', NULL); }
#line 23766 "parser.c" /* yacc.c:1646  */
    break;

  case 2262:
#line 14519 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr ('P', NULL); }
#line 23772 "parser.c" /* yacc.c:1646  */
    break;

  case 2263:
#line 14520 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { push_expr ('N', NULL); }
#line 23778 "parser.c" /* yacc.c:1646  */
    break;

  case 2272:
#line 14550 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 23786 "parser.c" /* yacc.c:1646  */
    break;

  case 2273:
#line 14554 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-2]), (yyvsp[0]));
  }
#line 23794 "parser.c" /* yacc.c:1646  */
    break;

  case 2277:
#line 14566 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_binary_op ((yyvsp[-2]), '+', (yyvsp[0])); }
#line 23800 "parser.c" /* yacc.c:1646  */
    break;

  case 2278:
#line 14567 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_binary_op ((yyvsp[-2]), '-', (yyvsp[0])); }
#line 23806 "parser.c" /* yacc.c:1646  */
    break;

  case 2279:
#line 14568 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 23812 "parser.c" /* yacc.c:1646  */
    break;

  case 2280:
#line 14572 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_binary_op ((yyvsp[-2]), '*', (yyvsp[0])); }
#line 23818 "parser.c" /* yacc.c:1646  */
    break;

  case 2281:
#line 14573 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_binary_op ((yyvsp[-2]), '/', (yyvsp[0])); }
#line 23824 "parser.c" /* yacc.c:1646  */
    break;

  case 2282:
#line 14574 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 23830 "parser.c" /* yacc.c:1646  */
    break;

  case 2283:
#line 14579 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_binary_op ((yyvsp[-2]), '^', (yyvsp[0]));
  }
#line 23838 "parser.c" /* yacc.c:1646  */
    break;

  case 2284:
#line 14582 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 23844 "parser.c" /* yacc.c:1646  */
    break;

  case 2285:
#line 14586 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 23850 "parser.c" /* yacc.c:1646  */
    break;

  case 2286:
#line 14587 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_binary_op (cb_zero, '-', (yyvsp[0])); }
#line 23856 "parser.c" /* yacc.c:1646  */
    break;

  case 2287:
#line 14588 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 23862 "parser.c" /* yacc.c:1646  */
    break;

  case 2288:
#line 14591 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[-1]); }
#line 23868 "parser.c" /* yacc.c:1646  */
    break;

  case 2289:
#line 14592 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 23874 "parser.c" /* yacc.c:1646  */
    break;

  case 2290:
#line 14603 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (current_linage > 1) {
		cb_error (_("LINAGE-COUNTER must be qualified here"));
		(yyval) = cb_error_node;
	} else if (current_linage == 0) {
		cb_error (_("invalid LINAGE-COUNTER usage"));
		(yyval) = cb_error_node;
	} else {
		(yyval) = linage_file->linage_ctr;
	}
  }
#line 23890 "parser.c" /* yacc.c:1646  */
    break;

  case 2291:
#line 14615 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_FILE_P (cb_ref ((yyvsp[0])))) {
		(yyval) = CB_FILE (cb_ref ((yyvsp[0])))->linage_ctr;
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not a file name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 23903 "parser.c" /* yacc.c:1646  */
    break;

  case 2292:
#line 14624 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (report_count > 1
	&& current_report != NULL) {
		(yyval) = current_report->line_counter;
	} else
	if (report_count > 1) {
		cb_error (_("LINE-COUNTER must be qualified here"));
		(yyval) = cb_error_node;
	} else if (report_count == 0) {
		cb_error (_("invalid LINE-COUNTER usage"));
		(yyval) = cb_error_node;
	} else {
		(yyval) = report_instance->line_counter;
	}
  }
#line 23923 "parser.c" /* yacc.c:1646  */
    break;

  case 2293:
#line 14640 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_REF_OR_REPORT_P ((yyvsp[0]))) {
		(yyval) = CB_REPORT_PTR ((yyvsp[0]))->line_counter;
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not a report name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 23936 "parser.c" /* yacc.c:1646  */
    break;

  case 2294:
#line 14649 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (report_count > 1
	&& current_report != NULL) {
		(yyval) = current_report->page_counter;
	} else
	if (report_count > 1) {
		cb_error (_("PAGE-COUNTER must be qualified here"));
		(yyval) = cb_error_node;
	} else if (report_count == 0) {
		cb_error (_("invalid PAGE-COUNTER usage"));
		(yyval) = cb_error_node;
	} else {
		(yyval) = report_instance->page_counter;
	}
  }
#line 23956 "parser.c" /* yacc.c:1646  */
    break;

  case 2295:
#line 14665 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_REF_OR_REPORT_P ((yyvsp[0]))) {
		(yyval) = CB_REPORT_PTR ((yyvsp[0]))->page_counter;
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not a report name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 23969 "parser.c" /* yacc.c:1646  */
    break;

  case 2296:
#line 14679 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 23975 "parser.c" /* yacc.c:1646  */
    break;

  case 2297:
#line 14681 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_append ((yyvsp[-1]), (yyvsp[0])); }
#line 23981 "parser.c" /* yacc.c:1646  */
    break;

  case 2298:
#line 14686 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[0]), (yyvsp[-1]));
  }
#line 23989 "parser.c" /* yacc.c:1646  */
    break;

  case 2299:
#line 14694 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { cb_build_identifier ((yyvsp[0]), 0); }
#line 23995 "parser.c" /* yacc.c:1646  */
    break;

  case 2300:
#line 14701 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (!CB_FILE_P (cb_ref ((yyvsp[0])))) {
		(yyval) = (yyvsp[0]);
	} else {
		cb_error_x ((yyvsp[0]), _("%s requires a record name as subject"),
			current_statement->name);
		(yyval) = cb_error_node;
	}
  }
#line 24009 "parser.c" /* yacc.c:1646  */
    break;

  case 2301:
#line 14711 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_FILE_P (cb_ref ((yyvsp[0])))) {
		(yyval) = (yyvsp[0]);
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not a file name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 24022 "parser.c" /* yacc.c:1646  */
    break;

  case 2302:
#line 14725 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree x;

	x = cb_ref ((yyvsp[0]));
	if (!CB_FIELD_P (x)) {
		(yyval) = cb_error_node;
	} else if (!CB_FIELD (x)->index_list) {
		cb_error_x ((yyvsp[0]), _("'%s' not indexed"), cb_name ((yyvsp[0])));
		listprint_suppress ();
		cb_error_x (x, _("'%s' defined here"), cb_name (x));
		listprint_restore ();
		(yyval) = cb_error_node;
	} else {
		(yyval) = (yyvsp[0]);
	}
  }
#line 24043 "parser.c" /* yacc.c:1646  */
    break;

  case 2303:
#line 14747 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 24051 "parser.c" /* yacc.c:1646  */
    break;

  case 2304:
#line 14751 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree		l;

	if (CB_VALID_TREE ((yyvsp[0]))) {
		for (l = (yyvsp[-1]); l; l = CB_CHAIN (l)) {
			if (CB_VALID_TREE (CB_VALUE (l)) &&
			    !strcasecmp (CB_NAME ((yyvsp[0])), CB_NAME (CB_VALUE (l)))) {
				cb_error_x ((yyvsp[0]), _("multiple reference to '%s' "),
					    CB_NAME ((yyvsp[0])));
				break;
			}
		}
		if (!l) {
			(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
		}
	}
  }
#line 24073 "parser.c" /* yacc.c:1646  */
    break;

  case 2305:
#line 14772 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_FILE_P (cb_ref ((yyvsp[0])))) {
		(yyval) = (yyvsp[0]);
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not a file name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 24086 "parser.c" /* yacc.c:1646  */
    break;

  case 2306:
#line 14784 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_CD_P (cb_ref ((yyvsp[0])))) {
		(yyval) = (yyvsp[0]);
	} else {
		cb_error_x ((yyvsp[0]), _("'%s' is not a CD name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 24099 "parser.c" /* yacc.c:1646  */
    break;

  case 2307:
#line 14798 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_REF_OR_REPORT_P ((yyvsp[0]))) {
		(yyval) = (yyvsp[0]);
	} else {
		cb_error (_("'%s' is not a valid report name"), CB_NAME ((yyvsp[0])));
		(yyval) = cb_error_node;
	}
  }
#line 24112 "parser.c" /* yacc.c:1646  */
    break;

  case 2308:
#line 14811 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 24118 "parser.c" /* yacc.c:1646  */
    break;

  case 2309:
#line 14813 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 24124 "parser.c" /* yacc.c:1646  */
    break;

  case 2310:
#line 14817 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 24130 "parser.c" /* yacc.c:1646  */
    break;

  case 2311:
#line 14823 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 24136 "parser.c" /* yacc.c:1646  */
    break;

  case 2312:
#line 14825 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 24142 "parser.c" /* yacc.c:1646  */
    break;

  case 2313:
#line 14830 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	struct cb_reference *r = CB_REFERENCE ((yyvsp[0]));

	r->offset = CB_TREE (current_section);
	r->flag_in_decl = !!in_declaratives;
	r->flag_ignored = cb_set_ignore_error (-1);

	(yyval) = (yyvsp[0]);
	CB_ADD_TO_CHAIN ((yyvsp[0]), current_program->label_list);
  }
#line 24157 "parser.c" /* yacc.c:1646  */
    break;

  case 2316:
#line 14846 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_REFERENCE ((yyvsp[-2]))->chain = (yyvsp[0]);
  }
#line 24165 "parser.c" /* yacc.c:1646  */
    break;

  case 2317:
#line 14853 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_reference ((char *)(CB_LITERAL ((yyvsp[0]))->data));
	(yyval)->source_file = (yyvsp[0])->source_file;
	(yyval)->source_line = (yyvsp[0])->source_line;
  }
#line 24175 "parser.c" /* yacc.c:1646  */
    break;

  case 2318:
#line 14863 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = CB_LIST_INIT ((yyvsp[0])); }
#line 24181 "parser.c" /* yacc.c:1646  */
    break;

  case 2319:
#line 14864 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0])); }
#line 24187 "parser.c" /* yacc.c:1646  */
    break;

  case 2320:
#line 14869 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	CB_ADD_TO_CHAIN ((yyval), current_program->reference_list);
  }
#line 24196 "parser.c" /* yacc.c:1646  */
    break;

  case 2321:
#line 14876 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {(yyval) = NULL;}
#line 24202 "parser.c" /* yacc.c:1646  */
    break;

  case 2322:
#line 14877 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {(yyval) = (yyvsp[0]);}
#line 24208 "parser.c" /* yacc.c:1646  */
    break;

  case 2323:
#line 14882 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	CB_ADD_TO_CHAIN ((yyval), current_program->reference_list);
  }
#line 24217 "parser.c" /* yacc.c:1646  */
    break;

  case 2324:
#line 14894 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 24225 "parser.c" /* yacc.c:1646  */
    break;

  case 2325:
#line 14898 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 24233 "parser.c" /* yacc.c:1646  */
    break;

  case 2326:
#line 14905 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	CB_REFERENCE((yyval))->flag_optional = 1;
	CB_ADD_TO_CHAIN ((yyval), current_program->reference_list);
  }
#line 24243 "parser.c" /* yacc.c:1646  */
    break;

  case 2329:
#line 14921 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_WORD_COUNT ((yyvsp[0])) > 0) {
		redefinition_error ((yyvsp[0]));
		(yyval) = cb_error_node;
	} else {
		(yyval) = (yyvsp[0]);
	}
  }
#line 24256 "parser.c" /* yacc.c:1646  */
    break;

  case 2330:
#line 14930 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	yyclearin;
	yyerrok;
	(yyval) = cb_error_node;
  }
#line 24266 "parser.c" /* yacc.c:1646  */
    break;

  case 2331:
#line 14941 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_REFERENCE ((yyvsp[0]))->flag_duped || CB_WORD_COUNT ((yyvsp[0])) > 0) {
		redefinition_error ((yyvsp[0]));
		(yyval) = NULL;
	} else {
		CB_WORD_COUNT ((yyvsp[0]))++;
		(yyval) = (yyvsp[0]);
	}
  }
#line 24280 "parser.c" /* yacc.c:1646  */
    break;

  case 2332:
#line 14958 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 24288 "parser.c" /* yacc.c:1646  */
    break;

  case 2333:
#line 14962 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 24296 "parser.c" /* yacc.c:1646  */
    break;

  case 2336:
#line 14971 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_address ((yyvsp[0]));
  }
#line 24304 "parser.c" /* yacc.c:1646  */
    break;

  case 2337:
#line 14977 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 24310 "parser.c" /* yacc.c:1646  */
    break;

  case 2338:
#line 14978 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 24316 "parser.c" /* yacc.c:1646  */
    break;

  case 2339:
#line 14983 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 24324 "parser.c" /* yacc.c:1646  */
    break;

  case 2340:
#line 14987 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 24332 "parser.c" /* yacc.c:1646  */
    break;

  case 2348:
#line 15007 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 24340 "parser.c" /* yacc.c:1646  */
    break;

  case 2349:
#line 15011 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 24348 "parser.c" /* yacc.c:1646  */
    break;

  case 2350:
#line 15015 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 24356 "parser.c" /* yacc.c:1646  */
    break;

  case 2351:
#line 15019 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_ppointer ((yyvsp[0]));
  }
#line 24364 "parser.c" /* yacc.c:1646  */
    break;

  case 2352:
#line 15023 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_address ((yyvsp[0]));
  }
#line 24372 "parser.c" /* yacc.c:1646  */
    break;

  case 2353:
#line 15027 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree		x;
	cb_tree		switch_id;

	x = cb_ref ((yyvsp[0]));
	if (CB_VALID_TREE (x)) {
		if (CB_SYSTEM_NAME (x)->category != CB_SWITCH_NAME) {
			cb_error_x ((yyvsp[0]), _("invalid mnemonic identifier"));
			(yyval) = cb_error_node;
		} else {
			switch_id = cb_int (CB_SYSTEM_NAME (x)->token);
			(yyval) = CB_BUILD_FUNCALL_1 ("cob_switch_value", switch_id);
		}
	} else {
		(yyval) = cb_error_node;
	}
  }
#line 24394 "parser.c" /* yacc.c:1646  */
    break;

  case 2354:
#line 15048 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 24402 "parser.c" /* yacc.c:1646  */
    break;

  case 2355:
#line 15052 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 24410 "parser.c" /* yacc.c:1646  */
    break;

  case 2363:
#line 15069 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 24418 "parser.c" /* yacc.c:1646  */
    break;

  case 2364:
#line 15073 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 24426 "parser.c" /* yacc.c:1646  */
    break;

  case 2365:
#line 15077 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 24434 "parser.c" /* yacc.c:1646  */
    break;

  case 2369:
#line 15087 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 24442 "parser.c" /* yacc.c:1646  */
    break;

  case 2370:
#line 15091 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 24450 "parser.c" /* yacc.c:1646  */
    break;

  case 2371:
#line 15095 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_length ((yyvsp[0]));
  }
#line 24458 "parser.c" /* yacc.c:1646  */
    break;

  case 2372:
#line 15102 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (CB_TREE_CATEGORY ((yyvsp[0])) == CB_CATEGORY_NUMERIC) {
		cb_error_x ((yyvsp[0]), _("a non-numeric literal is expected here"));
		(yyval) = cb_error_node;
	} else {
		(yyval) = (yyvsp[0]);
	}
  }
#line 24471 "parser.c" /* yacc.c:1646  */
    break;

  case 2373:
#line 15114 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (cb_tree_category ((yyvsp[0])) != CB_CATEGORY_NUMERIC
	    || cb_get_int ((yyvsp[0])) == 0) {
		cb_error (_("non-zero value expected"));
		(yyval) = cb_int1;
	} else {
		(yyval) = (yyvsp[0]);
	}
  }
#line 24485 "parser.c" /* yacc.c:1646  */
    break;

  case 2378:
#line 15138 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	error_if_not_usage_display_or_nonnumeric_lit ((yyvsp[0]));
  }
#line 24493 "parser.c" /* yacc.c:1646  */
    break;

  case 2379:
#line 15145 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	error_if_not_usage_display_or_nonnumeric_lit ((yyvsp[0]));
  }
#line 24501 "parser.c" /* yacc.c:1646  */
    break;

  case 2385:
#line 15163 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));
  }
#line 24509 "parser.c" /* yacc.c:1646  */
    break;

  case 2387:
#line 15171 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));
  }
#line 24517 "parser.c" /* yacc.c:1646  */
    break;

  case 2390:
#line 15180 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));
  }
#line 24525 "parser.c" /* yacc.c:1646  */
    break;

  case 2393:
#line 15189 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));
  }
#line 24533 "parser.c" /* yacc.c:1646  */
    break;

  case 2395:
#line 15194 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_zero;
  }
#line 24541 "parser.c" /* yacc.c:1646  */
    break;

  case 2396:
#line 15203 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));
  }
#line 24549 "parser.c" /* yacc.c:1646  */
    break;

  case 2400:
#line 15219 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));
  }
#line 24557 "parser.c" /* yacc.c:1646  */
    break;

  case 2402:
#line 15227 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	check_not_88_level ((yyvsp[0]));
  }
#line 24565 "parser.c" /* yacc.c:1646  */
    break;

  case 2405:
#line 15237 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_identifier ((yyvsp[0]), 0); }
#line 24571 "parser.c" /* yacc.c:1646  */
    break;

  case 2406:
#line 15241 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_build_identifier ((yyvsp[0]), 1); }
#line 24577 "parser.c" /* yacc.c:1646  */
    break;

  case 2407:
#line 15245 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 24583 "parser.c" /* yacc.c:1646  */
    break;

  case 2408:
#line 15246 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[-1]); }
#line 24589 "parser.c" /* yacc.c:1646  */
    break;

  case 2409:
#line 15251 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	error_if_not_usage_display_or_nonnumeric_lit ((yyvsp[0]));
  }
#line 24597 "parser.c" /* yacc.c:1646  */
    break;

  case 2410:
#line 15258 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0]) != cb_error_node
	    && cb_tree_category ((yyvsp[0])) != CB_CATEGORY_NUMERIC) {
		cb_error_x ((yyvsp[0]), _("'%s' is not numeric"), cb_name ((yyvsp[0])));
	}
  }
#line 24608 "parser.c" /* yacc.c:1646  */
    break;

  case 2411:
#line 15268 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	int     reference_to_existing_object;

	if (CB_REFERENCE_P ((yyvsp[0])) && (CB_FIELD_P (cb_ref ((yyvsp[0])))
				    || CB_FILE_P (cb_ref ((yyvsp[0]))))) {
		(yyval) = cb_build_identifier ((yyvsp[0]), 0);
	} else {
		reference_to_existing_object =
			CB_REFERENCE_P ((yyvsp[0])) && cb_ref ((yyvsp[0])) != cb_error_node;
		if (!CB_REFERENCE_P ((yyvsp[0])) || reference_to_existing_object) {
			cb_error_x ((yyvsp[0]), _("'%s' is not a field or file"), cb_name ((yyvsp[0])));
		}
		(yyval) = cb_error_node;
	}
  }
#line 24628 "parser.c" /* yacc.c:1646  */
    break;

  case 2412:
#line 15287 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	int     reference_to_existing_object;

	if (CB_REFERENCE_P ((yyvsp[0])) && CB_FIELD_P (cb_ref ((yyvsp[0])))) {
		(yyval) = cb_build_identifier ((yyvsp[0]), 0);
	} else {
		reference_to_existing_object =
			CB_REFERENCE_P ((yyvsp[0])) && cb_ref ((yyvsp[0])) != cb_error_node;
		if (!CB_REFERENCE_P ((yyvsp[0])) || reference_to_existing_object) {
			cb_error_x ((yyvsp[0]), _("'%s' is not a field"), cb_name ((yyvsp[0])));
		}
		(yyval) = cb_error_node;
	}
  }
#line 24647 "parser.c" /* yacc.c:1646  */
    break;

  case 2413:
#line 15305 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-2]);
	if (start_debug) {
		cb_check_field_debug ((yyvsp[-2]));
	}
  }
#line 24658 "parser.c" /* yacc.c:1646  */
    break;

  case 2414:
#line 15312 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
	if (start_debug) {
		cb_check_field_debug ((yyvsp[-1]));
	}
  }
#line 24669 "parser.c" /* yacc.c:1646  */
    break;

  case 2415:
#line 15319 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
	if (start_debug) {
		cb_check_field_debug ((yyvsp[-1]));
	}
  }
#line 24680 "parser.c" /* yacc.c:1646  */
    break;

  case 2416:
#line 15326 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	if (start_debug) {
		cb_check_field_debug ((yyvsp[0]));
	}
  }
#line 24691 "parser.c" /* yacc.c:1646  */
    break;

  case 2417:
#line 15336 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_LIST_INIT ((yyvsp[0]));
  }
#line 24699 "parser.c" /* yacc.c:1646  */
    break;

  case 2418:
#line 15340 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[-1]), (yyvsp[0]));
  }
#line 24707 "parser.c" /* yacc.c:1646  */
    break;

  case 2419:
#line 15347 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_identifier ((yyvsp[0]), 0);
  }
#line 24715 "parser.c" /* yacc.c:1646  */
    break;

  case 2420:
#line 15351 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_identifier ((yyvsp[0]), 0);
  }
#line 24723 "parser.c" /* yacc.c:1646  */
    break;

  case 2421:
#line 15358 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-2]);
	if (CB_REFERENCE_P ((yyvsp[-2]))) {
		CB_REFERENCE ((yyvsp[-2]))->flag_target = 1;
	}
	if (start_debug) {
		cb_check_field_debug ((yyvsp[-2]));
	}
  }
#line 24737 "parser.c" /* yacc.c:1646  */
    break;

  case 2422:
#line 15368 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
	if (CB_REFERENCE_P ((yyvsp[-1]))) {
		CB_REFERENCE ((yyvsp[-1]))->flag_target = 1;
	}
	if (start_debug) {
		cb_check_field_debug ((yyvsp[-1]));
	}
  }
#line 24751 "parser.c" /* yacc.c:1646  */
    break;

  case 2423:
#line 15378 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
	if (CB_REFERENCE_P ((yyvsp[-1]))) {
		CB_REFERENCE ((yyvsp[-1]))->flag_target = 1;
	}
	if (start_debug) {
		cb_check_field_debug ((yyvsp[-1]));
	}
  }
#line 24765 "parser.c" /* yacc.c:1646  */
    break;

  case 2424:
#line 15388 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	if (CB_REFERENCE_P ((yyvsp[0]))) {
		CB_REFERENCE ((yyvsp[0]))->flag_target = 1;
	}
	if (start_debug) {
		cb_check_field_debug ((yyvsp[0]));
	}
  }
#line 24779 "parser.c" /* yacc.c:1646  */
    break;

  case 2425:
#line 15401 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 24787 "parser.c" /* yacc.c:1646  */
    break;

  case 2426:
#line 15405 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-2]);
	CB_REFERENCE ((yyvsp[-2]))->chain = (yyvsp[0]);
  }
#line 24796 "parser.c" /* yacc.c:1646  */
    break;

  case 2427:
#line 15413 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-3]);
	CB_REFERENCE ((yyvsp[-3]))->subs = cb_list_reverse ((yyvsp[-1]));
  }
#line 24805 "parser.c" /* yacc.c:1646  */
    break;

  case 2428:
#line 15421 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_REFERENCE ((yyvsp[-4]))->offset = (yyvsp[-2]);
  }
#line 24813 "parser.c" /* yacc.c:1646  */
    break;

  case 2429:
#line 15425 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_REFERENCE ((yyvsp[-5]))->offset = (yyvsp[-3]);
	CB_REFERENCE ((yyvsp[-5]))->length = (yyvsp[-1]);
  }
#line 24822 "parser.c" /* yacc.c:1646  */
    break;

  case 2430:
#line 15435 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (cb_tree_category ((yyvsp[0])) != CB_CATEGORY_NUMERIC
	    || !CB_LITERAL_P((yyvsp[0]))
	    || CB_LITERAL ((yyvsp[0]))->sign < 0
	    || CB_LITERAL ((yyvsp[0]))->scale) {
		cb_error (_("non-negative integer value expected"));
		(yyval) = cb_build_numeric_literal(-1, "1", 0);
	} else {
		(yyval) = (yyvsp[0]);
	}
  }
#line 24838 "parser.c" /* yacc.c:1646  */
    break;

  case 2431:
#line 15450 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	int	n;

	if (cb_tree_category ((yyvsp[0])) != CB_CATEGORY_NUMERIC) {
		cb_error (_("integer value expected"));
		(yyval) = cb_int1;
	} else if (CB_LITERAL_P ((yyvsp[0]))
		&& (CB_LITERAL ((yyvsp[0]))->sign || CB_LITERAL ((yyvsp[0]))->scale)) {
		cb_error (_("integer value expected"));
		(yyval) = cb_int1;
	} else {
		n = cb_get_int ((yyvsp[0]));
		if (n < 1 || n > 256) {
			cb_error (_("invalid symbolic integer"));
			(yyval) = cb_int1;
		} else {
			(yyval) = (yyvsp[0]);
		}
	}
  }
#line 24863 "parser.c" /* yacc.c:1646  */
    break;

  case 2432:
#line 15474 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	int	n;

	if (cb_tree_category ((yyvsp[0])) != CB_CATEGORY_NUMERIC
	    || !CB_LITERAL_P((yyvsp[0]))
	    || CB_LITERAL ((yyvsp[0]))->sign
	    || CB_LITERAL ((yyvsp[0]))->scale) {
		cb_error (_("unsigned positive integer value expected"));
		(yyval) = cb_int1;	} else {
		n = cb_get_int ((yyvsp[0]));
		if (n < 1) {
			cb_error (_("unsigned positive integer value expected"));
			(yyval) = cb_int1;
		} else {
			(yyval) = (yyvsp[0]);
		}
	}
  }
#line 24886 "parser.c" /* yacc.c:1646  */
    break;

  case 2433:
#line 15496 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if (cb_tree_category ((yyvsp[0])) != CB_CATEGORY_NUMERIC) {
		cb_error (_("Integer value expected"));
		(yyval) = cb_int1;
	} else if (!CB_LITERAL_P((yyvsp[0])) ||
	    CB_LITERAL ((yyvsp[0]))->sign < 0 || CB_LITERAL ((yyvsp[0]))->scale) {
		cb_error (_("positive integer value expected"));
		(yyval) = cb_int1;
	} else {
		(yyval) = (yyvsp[0]);
	}
  }
#line 24903 "parser.c" /* yacc.c:1646  */
    break;

  case 2434:
#line 15509 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int0;
  }
#line 24911 "parser.c" /* yacc.c:1646  */
    break;

  case 2435:
#line 15516 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	int	n;

	if (cb_tree_category ((yyvsp[0])) == CB_CATEGORY_NUMERIC) {
		if (CB_LITERAL ((yyvsp[0]))->sign || CB_LITERAL ((yyvsp[0]))->scale) {
			cb_error (_("integer value expected"));
		} else {
			n = cb_get_int ((yyvsp[0]));
			if (n < 1 || n > 256) {
				cb_error (_("invalid CLASS value"));
			}
		}
	}
	(yyval) = (yyvsp[0]);
  }
#line 24931 "parser.c" /* yacc.c:1646  */
    break;

  case 2436:
#line 15531 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_space; }
#line 24937 "parser.c" /* yacc.c:1646  */
    break;

  case 2437:
#line 15532 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_zero; }
#line 24943 "parser.c" /* yacc.c:1646  */
    break;

  case 2438:
#line 15533 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_quote; }
#line 24949 "parser.c" /* yacc.c:1646  */
    break;

  case 2439:
#line 15534 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_high; }
#line 24955 "parser.c" /* yacc.c:1646  */
    break;

  case 2440:
#line 15535 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_low; }
#line 24961 "parser.c" /* yacc.c:1646  */
    break;

  case 2441:
#line 15536 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_null; }
#line 24967 "parser.c" /* yacc.c:1646  */
    break;

  case 2442:
#line 15541 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 24975 "parser.c" /* yacc.c:1646  */
    break;

  case 2443:
#line 15545 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	struct cb_literal	*l;

	if (CB_LITERAL_P ((yyvsp[0]))) {
		/* We must not alter the original definition */
		l = cobc_parse_malloc (sizeof(struct cb_literal));
		*l = *(CB_LITERAL((yyvsp[0])));
		l->all = 1;
		(yyval) = CB_TREE (l);
	} else {
		(yyval) = (yyvsp[0]);
	}
  }
#line 24993 "parser.c" /* yacc.c:1646  */
    break;

  case 2444:
#line 15562 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 25001 "parser.c" /* yacc.c:1646  */
    break;

  case 2445:
#line 15566 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_concat_literals ((yyvsp[-2]), (yyvsp[0]));
  }
#line 25009 "parser.c" /* yacc.c:1646  */
    break;

  case 2446:
#line 15572 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = (yyvsp[0]); }
#line 25015 "parser.c" /* yacc.c:1646  */
    break;

  case 2447:
#line 15573 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_space; }
#line 25021 "parser.c" /* yacc.c:1646  */
    break;

  case 2448:
#line 15574 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_zero; }
#line 25027 "parser.c" /* yacc.c:1646  */
    break;

  case 2449:
#line 15575 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_quote; }
#line 25033 "parser.c" /* yacc.c:1646  */
    break;

  case 2450:
#line 15576 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_high; }
#line 25039 "parser.c" /* yacc.c:1646  */
    break;

  case 2451:
#line 15577 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_low; }
#line 25045 "parser.c" /* yacc.c:1646  */
    break;

  case 2452:
#line 15578 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_null; }
#line 25051 "parser.c" /* yacc.c:1646  */
    break;

  case 2453:
#line 15585 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-1]), NULL, (yyvsp[0]), 0);
  }
#line 25059 "parser.c" /* yacc.c:1646  */
    break;

  case 2454:
#line 15589 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), CB_LIST_INIT ((yyvsp[-2])), (yyvsp[0]), 0);
  }
#line 25067 "parser.c" /* yacc.c:1646  */
    break;

  case 2455:
#line 15593 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 25075 "parser.c" /* yacc.c:1646  */
    break;

  case 2456:
#line 15597 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 25083 "parser.c" /* yacc.c:1646  */
    break;

  case 2457:
#line 15601 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-3]), (yyvsp[-1]), NULL, 0);
  }
#line 25091 "parser.c" /* yacc.c:1646  */
    break;

  case 2458:
#line 15605 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	CB_PENDING (_("PHYSICAL argument for LENGTH functions"));
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), NULL, 0);
  }
#line 25100 "parser.c" /* yacc.c:1646  */
    break;

  case 2459:
#line 15610 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-3]), (yyvsp[-1]), NULL, 0);
  }
#line 25108 "parser.c" /* yacc.c:1646  */
    break;

  case 2460:
#line 15614 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 25116 "parser.c" /* yacc.c:1646  */
    break;

  case 2461:
#line 15618 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 25124 "parser.c" /* yacc.c:1646  */
    break;

  case 2462:
#line 15622 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 25132 "parser.c" /* yacc.c:1646  */
    break;

  case 2463:
#line 15626 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	  (yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 25140 "parser.c" /* yacc.c:1646  */
    break;

  case 2464:
#line 15630 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	  (yyval) = cb_build_intrinsic ((yyvsp[-4]), (yyvsp[-2]), (yyvsp[0]), 0);
  }
#line 25148 "parser.c" /* yacc.c:1646  */
    break;

  case 2465:
#line 15634 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-1]), (yyvsp[0]), NULL, 0);
  }
#line 25156 "parser.c" /* yacc.c:1646  */
    break;

  case 2466:
#line 15638 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_build_intrinsic ((yyvsp[-1]), (yyvsp[0]), NULL, 1);
  }
#line 25164 "parser.c" /* yacc.c:1646  */
    break;

  case 2476:
#line 15663 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 25172 "parser.c" /* yacc.c:1646  */
    break;

  case 2477:
#line 15667 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[-2]), NULL);
  }
#line 25180 "parser.c" /* yacc.c:1646  */
    break;

  case 2478:
#line 15671 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = CB_BUILD_PAIR ((yyvsp[-3]), (yyvsp[-1]));
  }
#line 25188 "parser.c" /* yacc.c:1646  */
    break;

  case 2479:
#line 15678 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 25196 "parser.c" /* yacc.c:1646  */
    break;

  case 2480:
#line 15682 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[-1]);
  }
#line 25204 "parser.c" /* yacc.c:1646  */
    break;

  case 2481:
#line 15686 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 25212 "parser.c" /* yacc.c:1646  */
    break;

  case 2482:
#line 15693 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[0]));
	(yyval) = cb_list_add (x, cb_int0);
  }
#line 25223 "parser.c" /* yacc.c:1646  */
    break;

  case 2483:
#line 15700 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[-2]));
	(yyval) = cb_list_add (x, cb_int1);
  }
#line 25234 "parser.c" /* yacc.c:1646  */
    break;

  case 2484:
#line 15707 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[-2]));
	(yyval) = cb_list_add (x, cb_int2);
  }
#line 25245 "parser.c" /* yacc.c:1646  */
    break;

  case 2485:
#line 15716 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	suppress_data_exceptions = 1;
  }
#line 25253 "parser.c" /* yacc.c:1646  */
    break;

  case 2486:
#line 15720 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	suppress_data_exceptions = 0;
	if (CB_NUMERIC_LITERAL_P((yyvsp[0]))) {
		cb_error_x ((yyvsp[0]), _("a non-numeric literal is expected here"));
		(yyval) = CB_LIST_INIT (cb_error_node);
	} else {
		(yyval) = CB_LIST_INIT ((yyvsp[0]));
	}
  }
#line 25267 "parser.c" /* yacc.c:1646  */
    break;

  case 2487:
#line 15733 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[0]));
	(yyval) = cb_list_add (x, cb_null);
  }
#line 25278 "parser.c" /* yacc.c:1646  */
    break;

  case 2488:
#line 15740 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[-2]));
	(yyval) = cb_list_add (x, (yyvsp[0]));
  }
#line 25289 "parser.c" /* yacc.c:1646  */
    break;

  case 2489:
#line 15750 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[0]));
	(yyval) = cb_list_add (x, cb_null);
  }
#line 25300 "parser.c" /* yacc.c:1646  */
    break;

  case 2490:
#line 15757 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[-2]));
	(yyval) = cb_list_add (x, cb_ref ((yyvsp[0])));
  }
#line 25311 "parser.c" /* yacc.c:1646  */
    break;

  case 2491:
#line 15767 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[0]), cb_int0);
  }
#line 25319 "parser.c" /* yacc.c:1646  */
    break;

  case 2492:
#line 15771 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	const int	num_args = cb_list_length ((yyvsp[-2]));

	if (num_args == 4) {
		cb_error_x ((yyvsp[-2]), _("cannot specify offset and SYSTEM-OFFSET at the same time"));
	}

	(yyval) = cb_list_add ((yyvsp[-2]), cb_int1);
  }
#line 25333 "parser.c" /* yacc.c:1646  */
    break;

  case 2493:
#line 15784 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_list_add ((yyvsp[0]), cb_int0);
  }
#line 25341 "parser.c" /* yacc.c:1646  */
    break;

  case 2494:
#line 15788 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	const int	num_args = cb_list_length ((yyvsp[-2]));

	if (num_args == 3) {
		cb_error_x ((yyvsp[-2]), _("cannot specify offset and SYSTEM-OFFSET at the same time"));
	}

	(yyval) = cb_list_add ((yyvsp[-2]), cb_int1);
  }
#line 25355 "parser.c" /* yacc.c:1646  */
    break;

  case 2495:
#line 15802 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	non_const_word = 1;
  }
#line 25363 "parser.c" /* yacc.c:1646  */
    break;

  case 2496:
#line 15810 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 25369 "parser.c" /* yacc.c:1646  */
    break;

  case 2497:
#line 15811 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 25375 "parser.c" /* yacc.c:1646  */
    break;

  case 2498:
#line 15815 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 25381 "parser.c" /* yacc.c:1646  */
    break;

  case 2499:
#line 15816 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 25387 "parser.c" /* yacc.c:1646  */
    break;

  case 2500:
#line 15820 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 25393 "parser.c" /* yacc.c:1646  */
    break;

  case 2501:
#line 15821 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 25399 "parser.c" /* yacc.c:1646  */
    break;

  case 2502:
#line 15826 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 25407 "parser.c" /* yacc.c:1646  */
    break;

  case 2503:
#line 15830 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 25415 "parser.c" /* yacc.c:1646  */
    break;

  case 2504:
#line 15837 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
  }
#line 25423 "parser.c" /* yacc.c:1646  */
    break;

  case 2505:
#line 15841 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 25431 "parser.c" /* yacc.c:1646  */
    break;

  case 2506:
#line 15848 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 25437 "parser.c" /* yacc.c:1646  */
    break;

  case 2507:
#line 15849 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 25443 "parser.c" /* yacc.c:1646  */
    break;

  case 2508:
#line 15850 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int2; }
#line 25449 "parser.c" /* yacc.c:1646  */
    break;

  case 2509:
#line 15854 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 25455 "parser.c" /* yacc.c:1646  */
    break;

  case 2510:
#line 15855 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_true; }
#line 25461 "parser.c" /* yacc.c:1646  */
    break;

  case 2511:
#line 15859 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int (cb_flag_optional_file); }
#line 25467 "parser.c" /* yacc.c:1646  */
    break;

  case 2512:
#line 15860 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 25473 "parser.c" /* yacc.c:1646  */
    break;

  case 2513:
#line 15861 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int0; }
#line 25479 "parser.c" /* yacc.c:1646  */
    break;

  case 2514:
#line 15866 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int0;
  }
#line 25487 "parser.c" /* yacc.c:1646  */
    break;

  case 2515:
#line 15870 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	if ((yyvsp[0])) {
		(yyval) = (yyvsp[0]);
	} else {
		(yyval) = default_rounded_mode;
	}
	cobc_cs_check = 0;
  }
#line 25500 "parser.c" /* yacc.c:1646  */
    break;

  case 2516:
#line 15882 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = NULL;
	cobc_cs_check = 0;
  }
#line 25509 "parser.c" /* yacc.c:1646  */
    break;

  case 2517:
#line 15887 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
	cobc_cs_check = 0;
  }
#line 25518 "parser.c" /* yacc.c:1646  */
    break;

  case 2518:
#line 15895 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_AWAY_FROM_ZERO);
  }
#line 25526 "parser.c" /* yacc.c:1646  */
    break;

  case 2519:
#line 15899 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_AWAY_FROM_ZERO);
  }
#line 25534 "parser.c" /* yacc.c:1646  */
    break;

  case 2520:
#line 15903 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_EVEN);
  }
#line 25542 "parser.c" /* yacc.c:1646  */
    break;

  case 2521:
#line 15907 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_TOWARD_ZERO);
  }
#line 25550 "parser.c" /* yacc.c:1646  */
    break;

  case 2522:
#line 15911 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_PROHIBITED);
  }
#line 25558 "parser.c" /* yacc.c:1646  */
    break;

  case 2523:
#line 15915 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_TOWARD_GREATER);
  }
#line 25566 "parser.c" /* yacc.c:1646  */
    break;

  case 2524:
#line 15919 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_TOWARD_LESSER);
  }
#line 25574 "parser.c" /* yacc.c:1646  */
    break;

  case 2525:
#line 15923 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = cb_int (COB_STORE_ROUND | COB_STORE_TRUNCATION);
  }
#line 25582 "parser.c" /* yacc.c:1646  */
    break;

  case 2526:
#line 15929 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 25588 "parser.c" /* yacc.c:1646  */
    break;

  case 2527:
#line 15930 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = cb_int1; }
#line 25594 "parser.c" /* yacc.c:1646  */
    break;

  case 2528:
#line 15934 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 25600 "parser.c" /* yacc.c:1646  */
    break;

  case 2529:
#line 15936 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cb_tree	x;

	x = CB_LIST_INIT ((yyvsp[-3]));
	(yyval) = cb_list_add (x, (yyvsp[-1]));
  }
#line 25611 "parser.c" /* yacc.c:1646  */
    break;

  case 2530:
#line 15945 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    { (yyval) = NULL; }
#line 25617 "parser.c" /* yacc.c:1646  */
    break;

  case 2531:
#line 15947 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	(yyval) = (yyvsp[0]);
  }
#line 25625 "parser.c" /* yacc.c:1646  */
    break;

  case 2532:
#line 15956 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_repeat_last_token = 1;
  }
#line 25633 "parser.c" /* yacc.c:1646  */
    break;

  case 2533:
#line 15960 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_repeat_last_token = 1;
  }
#line 25641 "parser.c" /* yacc.c:1646  */
    break;

  case 2534:
#line 15964 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_repeat_last_token = 0;
  }
#line 25649 "parser.c" /* yacc.c:1646  */
    break;

  case 2535:
#line 15968 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1646  */
    {
	cobc_repeat_last_token = 0;
  }
#line 25657 "parser.c" /* yacc.c:1646  */
    break;


#line 25661 "parser.c" /* yacc.c:1646  */
      default: break;
    }
  /* User semantic actions sometimes alter yychar, and that requires
     that yytoken be updated with the new translation.  We take the
     approach of translating immediately before every use of yytoken.
     One alternative is translating here after every semantic action,
     but that translation would be missed if the semantic action invokes
     YYABORT, YYACCEPT, or YYERROR immediately after altering yychar or
     if it invokes YYBACKUP.  In the case of YYABORT or YYACCEPT, an
     incorrect destructor might then be invoked immediately.  In the
     case of YYERROR or YYBACKUP, subsequent parser actions might lead
     to an incorrect destructor call or verbose syntax error message
     before the lookahead is translated.  */
  YY_SYMBOL_PRINT ("-> $$ =", yyr1[yyn], &yyval, &yyloc);

  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);

  *++yyvsp = yyval;

  /* Now 'shift' the result of the reduction.  Determine what state
     that goes to, based on the state we popped back to and the rule
     number reduced by.  */

  yyn = yyr1[yyn];

  yystate = yypgoto[yyn - YYNTOKENS] + *yyssp;
  if (0 <= yystate && yystate <= YYLAST && yycheck[yystate] == *yyssp)
    yystate = yytable[yystate];
  else
    yystate = yydefgoto[yyn - YYNTOKENS];

  goto yynewstate;


/*--------------------------------------.
| yyerrlab -- here on detecting error.  |
`--------------------------------------*/
yyerrlab:
  /* Make sure we have latest lookahead translation.  See comments at
     user semantic actions for why this is necessary.  */
  yytoken = yychar == YYEMPTY ? YYEMPTY : YYTRANSLATE (yychar);

  /* If not already recovering from an error, report this error.  */
  if (!yyerrstatus)
    {
      ++yynerrs;
#if ! YYERROR_VERBOSE
      yyerror (YY_("syntax error"));
#else
# define YYSYNTAX_ERROR yysyntax_error (&yymsg_alloc, &yymsg, \
                                        yyssp, yytoken)
      {
        char const *yymsgp = YY_("syntax error");
        int yysyntax_error_status;
        yysyntax_error_status = YYSYNTAX_ERROR;
        if (yysyntax_error_status == 0)
          yymsgp = yymsg;
        else if (yysyntax_error_status == 1)
          {
            if (yymsg != yymsgbuf)
              YYSTACK_FREE (yymsg);
            yymsg = (char *) YYSTACK_ALLOC (yymsg_alloc);
            if (!yymsg)
              {
                yymsg = yymsgbuf;
                yymsg_alloc = sizeof yymsgbuf;
                yysyntax_error_status = 2;
              }
            else
              {
                yysyntax_error_status = YYSYNTAX_ERROR;
                yymsgp = yymsg;
              }
          }
        yyerror (yymsgp);
        if (yysyntax_error_status == 2)
          goto yyexhaustedlab;
      }
# undef YYSYNTAX_ERROR
#endif
    }



  if (yyerrstatus == 3)
    {
      /* If just tried and failed to reuse lookahead token after an
         error, discard it.  */

      if (yychar <= YYEOF)
        {
          /* Return failure if at end of input.  */
          if (yychar == YYEOF)
            YYABORT;
        }
      else
        {
          yydestruct ("Error: discarding",
                      yytoken, &yylval);
          yychar = YYEMPTY;
        }
    }

  /* Else will try to reuse lookahead token after shifting the error
     token.  */
  goto yyerrlab1;


/*---------------------------------------------------.
| yyerrorlab -- error raised explicitly by YYERROR.  |
`---------------------------------------------------*/
yyerrorlab:

  /* Pacify compilers like GCC when the user code never invokes
     YYERROR and the label yyerrorlab therefore never appears in user
     code.  */
  if (/*CONSTCOND*/ 0)
     goto yyerrorlab;

  /* Do not reclaim the symbols of the rule whose action triggered
     this YYERROR.  */
  YYPOPSTACK (yylen);
  yylen = 0;
  YY_STACK_PRINT (yyss, yyssp);
  yystate = *yyssp;
  goto yyerrlab1;


/*-------------------------------------------------------------.
| yyerrlab1 -- common code for both syntax error and YYERROR.  |
`-------------------------------------------------------------*/
yyerrlab1:
  yyerrstatus = 3;      /* Each real token shifted decrements this.  */

  for (;;)
    {
      yyn = yypact[yystate];
      if (!yypact_value_is_default (yyn))
        {
          yyn += YYTERROR;
          if (0 <= yyn && yyn <= YYLAST && yycheck[yyn] == YYTERROR)
            {
              yyn = yytable[yyn];
              if (0 < yyn)
                break;
            }
        }

      /* Pop the current state because it cannot handle the error token.  */
      if (yyssp == yyss)
        YYABORT;


      yydestruct ("Error: popping",
                  yystos[yystate], yyvsp);
      YYPOPSTACK (1);
      yystate = *yyssp;
      YY_STACK_PRINT (yyss, yyssp);
    }

  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END


  /* Shift the error token.  */
  YY_SYMBOL_PRINT ("Shifting", yystos[yyn], yyvsp, yylsp);

  yystate = yyn;
  goto yynewstate;


/*-------------------------------------.
| yyacceptlab -- YYACCEPT comes here.  |
`-------------------------------------*/
yyacceptlab:
  yyresult = 0;
  goto yyreturn;

/*-----------------------------------.
| yyabortlab -- YYABORT comes here.  |
`-----------------------------------*/
yyabortlab:
  yyresult = 1;
  goto yyreturn;

#if !defined yyoverflow || YYERROR_VERBOSE
/*-------------------------------------------------.
| yyexhaustedlab -- memory exhaustion comes here.  |
`-------------------------------------------------*/
yyexhaustedlab:
  yyerror (YY_("memory exhausted"));
  yyresult = 2;
  /* Fall through.  */
#endif

yyreturn:
  if (yychar != YYEMPTY)
    {
      /* Make sure we have latest lookahead translation.  See comments at
         user semantic actions for why this is necessary.  */
      yytoken = YYTRANSLATE (yychar);
      yydestruct ("Cleanup: discarding lookahead",
                  yytoken, &yylval);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp);
      YYPOPSTACK (1);
    }
#ifndef yyoverflow
  if (yyss != yyssa)
    YYSTACK_FREE (yyss);
#endif
#if YYERROR_VERBOSE
  if (yymsg != yymsgbuf)
    YYSTACK_FREE (yymsg);
#endif
  return yyresult;
}
#line 16161 "/media/sf_dev_desktop/gnucobol-3.x/cobc/parser.y" /* yacc.c:1906  */

