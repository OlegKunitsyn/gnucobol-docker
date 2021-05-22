/* A Bison parser, made by GNU Bison 3.5.1.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2020 Free Software Foundation,
   Inc.

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

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

/* Identify Bison output.  */
#define YYBISON 1

/* Bison version.  */
#define YYBISON_VERSION "3.5.1"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* First part of user prologue.  */
#line 28 "parser.y"

#include <config.h>

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
#define TERM_JSON		10U
#define TERM_MODIFY		11U
#define TERM_MULTIPLY		12U
#define TERM_PERFORM		13U
#define TERM_READ		14U
#define TERM_RECEIVE		15U
#define TERM_RETURN		16U
#define TERM_REWRITE		17U
#define TERM_SEARCH		18U
#define TERM_START		19U
#define TERM_STRING		20U
#define TERM_SUBTRACT		21U
#define TERM_UNSTRING		22U
#define TERM_WRITE		23U
#define TERM_XML		24U
#define TERM_MAX		25U	/* Always last entry, used for array size */

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

struct cb_program		*current_program = NULL;    /* program in parse/syntax check/codegen */
struct cb_statement		*current_statement = NULL;
struct cb_label			*current_section = NULL;
struct cb_label			*current_paragraph = NULL;
struct cb_field		*external_defined_fields_ws;
struct cb_field		*external_defined_fields_global;
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
unsigned int			cobc_in_xml_generate_body = 0;
unsigned int			cobc_in_json_generate_body = 0;

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
static unsigned int		within_typedef_definition;
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

static int			ext_dyn_specified;
static enum cb_assign_device	assign_device;
 
static enum cb_display_type	display_type;
static int			is_first_display_item;
static cb_tree			advancing_value;
static cb_tree			upon_value;
static cb_tree			line_column;

static unsigned int		exhibit_changed;
static unsigned int		exhibit_named;

static cb_tree			ml_suppress_list;
static cb_tree			xml_encoding;
static int			with_xml_dec;
static int			with_attrs;

static cb_tree			alphanumeric_collation;
static cb_tree			national_collation;

static enum cb_ml_suppress_category	ml_suppress_category;

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
}

static void
restore_backup_pos (cb_tree item)
{
	item->source_file = backup_source_file;
	item->source_line = backup_source_line;
}

static void
begin_statement_from_backup_pos (const char *name, const unsigned int term)
{
	current_paragraph->flag_statement = 1;
	current_statement = cb_build_statement (name);
	restore_backup_pos (CB_TREE (current_statement));
	current_statement->flag_in_debug = in_debugging;
	emit_statement (CB_TREE (current_statement));
	if (term) {
		term_array[term]++;
	}
	if (check_unreached) {
		cb_warning_x (cb_warn_unreachable, CB_TREE (current_statement), _("unreachable statement '%s'"), name);
	}
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
	current_statement->body = cb_list_add (current_statement->body,
					    CB_TREE (new_statement));
	current_statement = new_statement;
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
	cb_tree		check_list;
	cb_tree		label;
	cb_tree		x;
	cb_tree		entry_conv;
	struct cb_field	*f, *ret_f;
	int			param_num;
	char		buff[COB_MINI_BUFF];

	snprintf (buff, (size_t)COB_MINI_MAX, "E$%s", name);
	label = cb_build_label (cb_build_reference (buff), NULL);
	if (encode) {
		CB_LABEL (label)->name = cb_encode_program_id (name, 0, cb_fold_call);
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
	check_list = NULL;
	for (l = using_list; l; l = CB_CHAIN (l)) {
		x = CB_VALUE (l);
		if (cb_try_ref (x) != cb_error_node) {
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
				cb_error_x (x, _("'%s' REDEFINES field not allowed here"), f->name);
			}
			/* add a "receiving" entry for the USING parameter */
			if (cb_listing_xref) {
				cobc_xref_link (&f->xref, CB_REFERENCE (x)->common.source_line, 1);
			}
			if (CB_PURPOSE_INT (l) == CB_CALL_BY_REFERENCE) {
				check_list = cb_list_add (check_list, x);
			}
		}
	}

	if (check_list != NULL) {
		for (l = check_list; l; l = CB_CHAIN (l)) {
			cb_tree	l2 = CB_VALUE (l);
			x = cb_ref (l2);
			if (x != cb_error_node) {
				for (l2 = check_list; l2 != l; l2 = CB_CHAIN (l2)) {
					if (cb_ref (CB_VALUE (l2)) == x) {
						cb_error_x (l,
							_("duplicate USING BY REFERENCE item '%s'"),
							cb_name (CB_VALUE (l)));
						CB_VALUE (l) = cb_error_node;
						break;
					}
				}
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
		struct cb_label *check = CB_LABEL (CB_PURPOSE (l));
		if (strcmp (name, check->name) == 0) {
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

static void
emit_entry_goto (const char *name)
{
	cb_tree		l;
	cb_tree		label;
	char		buff[COB_MINI_BUFF];

	snprintf (buff, (size_t)COB_MINI_MAX, "E$%s", name);
	label = cb_build_label (cb_build_reference (buff), NULL);
	CB_LABEL (label)->name = name;
	CB_LABEL (label)->orig_name = name;
	CB_LABEL (label)->flag_begin = 1;
	CB_LABEL (label)->flag_entry = 1;
	CB_LABEL (label)->flag_entry_for_goto = 1;
	label->source_line = backup_source_line;
	emit_statement (label);

	for (l = current_program->entry_list_goto; l; l = CB_CHAIN (l)) {
		struct cb_label *real_label = CB_LABEL (CB_VALUE (l));
		if (strcmp (name, real_label->name) == 0) {
			cb_error_x (CB_TREE (current_statement),
				    _("ENTRY FOR GO TO '%s' duplicated"), name);
		}
	}

	if (current_program->entry_list_goto) {
		current_program->entry_list_goto =
			cb_list_add (current_program->entry_list_goto, label);
	} else {
		current_program->entry_list_goto = CB_LIST_INIT (label);
	}
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

/* note: same message in field.c */
static int
emit_duplicate_clause_message (const char *clause)
{
	/* FIXME: replace by a new warning level that is set
	   to warn/error depending on cb_relaxed_syntax_checks */
	if (cb_relaxed_syntax_checks) {
		cb_warning (COBC_WARN_FILLER, _("duplicate %s clause"), clause);
		return 0;
	}
	cb_error (_("duplicate %s clause"), clause);
	return 1;
}

static int
check_repeated (const char *clause, const cob_flags_t bitval,
			cob_flags_t *already_seen)
{
	if (*already_seen & bitval) {
		return emit_duplicate_clause_message (clause);
	}
	*already_seen |= bitval;
	return 0;
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
		cb_error (_("maximum OCCURS depth exceeded (%d)"),
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
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "OCCURS");
	} else if (current_field->flag_external) {
		cb_error (_("%s and %s are mutually exclusive"), "EXTERNAL", "OCCURS");
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
					cb_warning (COBC_WARN_FILLER, _("TO phrase without DEPENDING phrase"));
					cb_warning (COBC_WARN_FILLER, _("maximum number of occurrences assumed to be exact number"));
					current_field->occurs_min = 1; /* CHECKME: why using 1 ? */
				} else {
					cb_error (_("TO phrase without DEPENDING phrase"));
				}
			}
			if (current_field->occurs_max <= current_field->occurs_min) {
				cb_error (_("OCCURS TO must be greater than OCCURS FROM"));
			}
		} else {
			current_field->occurs_max = 0;
		}
	} else {
		current_field->occurs_min = 1; /* CHECKME: why using 1 ? */
		current_field->occurs_max = cb_get_int (occurs_min);
		if (current_field->depending) {
			cb_verify (cb_odo_without_to, _("OCCURS DEPENDING ON without TO phrase"));
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

static void
program_init_without_program_id (void)
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
build_words_for_nested_programs (void)
{
	cb_tree		x;
	cb_tree		y;

	/* Inherit special name mnemonics from parent */
	for (x = current_program->mnemonic_spec_list; x; x = CB_CHAIN (x)) {
		y = cb_build_reference (cb_name(CB_PURPOSE(x)));
		if (CB_SYSTEM_NAME_P (CB_VALUE(x))) {
			cb_define (y, CB_VALUE(x));
		} else {
			cb_build_constant (y, CB_VALUE(x));
		}
	}

	/* Inherit class names from parent */
	for (x = current_program->class_name_list; x; x = CB_CHAIN(x)) {
		y = cb_build_reference (cb_name(CB_VALUE(x)));
		cb_define (y, CB_VALUE(x));
	}
}

static void
clear_initial_values (void)
{
	perform_stack = NULL;
	current_statement = NULL;
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
	cobc_in_xml_generate_body = 0;
	cobc_in_json_generate_body = 0;
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
			/* The nested_level check is for the pathological case
			   where two nested programs have the same name */
			if (0 == strcmp (program->orig_program_id,
					 CB_PROGRAM (l->value)->orig_program_id)
			    && program->nested_level == CB_PROGRAM (l->value)->nested_level) {
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
		if (depth) {
			build_words_for_nested_programs();
		}
		cb_set_intr_when_compiled ();
		cb_build_registers ();
		cb_add_external_defined_registers ();
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

static int
set_current_field (cb_tree level, cb_tree name)
{
	cb_tree	x  = cb_build_field_tree (level, name, current_field,
					  current_storage, current_file, 0);
	/* Free tree associated with level number */
	cobc_parse_free (level);

	if (CB_INVALID_TREE (x)) {
		return 1;
	} else {
		current_field = CB_FIELD (x);
		check_pic_duplicate = 0;
		if (current_field->level == 1 || current_field->level == 77) {
			within_typedef_definition = 0;
		}
	}

	return 0;
}

static void
setup_external_definition (cb_tree x, const int type)
{
	/* note: syntax checks for conflicting clauses
	         are done in inherit_external_definition */

	if (x != cb_error_node) {
		struct cb_field *f = CB_FIELD (cb_ref (x));

		/* additional checks if the definition isn't provided by type */
		if (type != 1 /* called with SAME AS / LIKE data-name */ ) {
			if (f->level == 88) {
				cb_error (_("condition-name not allowed here: '%s'"), cb_name (x));
				x = cb_error_node;
			}
			/* note: the following are not explicit specified but implied with
			   LIKE as ILE-COBOL does not have those sections */
			if (f->storage == CB_STORAGE_SCREEN) {
				cb_error (_("SCREEN item cannot be used here"));
				x = cb_error_node;
			} else if (f->storage == CB_STORAGE_REPORT) {
				cb_error (_("REPORT item cannot be used here"));
				x = cb_error_node;
			}
			if (type == 0) {
				/* rules that apply only to SAME AS */
				if (f->flag_is_typedef) {
					cb_error (_("TYPEDEF item cannot be used here"));
					x = cb_error_node;
				}
			}
		}

		if (current_field->level == 77) {
			if (type != 2 /* called with LIKE */
			 && f->children) {
				cb_error (_("elementary item expected"));
				x = cb_error_node;
			}
		} else {
			struct cb_field *p;
			for (p = current_field; p; p = p->parent) {
				if (p == f) {
					cb_error (_("item may not reference itself"));
					x = cb_error_node;
					break;
				}
			}
			for (p = f->parent; p; p = p->parent) {
				if (p->usage != CB_USAGE_DISPLAY) {
					cb_error (_("item may not be subordinate to any item with USAGE clause"));
				} else if (p->flag_sign_clause) {
					cb_error (_("item may not be subordinate to any item with SIGN clause"));
				} else {
					continue;
				}
				x = cb_error_node;
				break;
			}
		}
	}

	if (x == cb_error_node) {
		current_field->flag_is_verified = 1;
		current_field->flag_invalid = 1;
		current_field->external_definition = cb_error_node;
	} else {
		current_field->external_definition = cb_ref (x);
	}
}

static void
setup_external_definition_type (cb_tree x)
{
	if (!check_repeated ("TYPE TO", SYN_CLAUSE_31, &check_pic_duplicate)) {
		if (current_field->external_definition) {
			emit_conflicting_clause_message ("SAME AS", "TYPE TO");
		}
		setup_external_definition (x, 1);
	}
}

/* verifies that no conflicting clauses are used and
   inherits the definition of the original field specified
   by SAME AS or by type_name */
static void
inherit_external_definition (cb_tree lvl)
{
	/* note: REDEFINES (clause 1) is allowed with RM/COBOL but not COBOL 2002+ */
	static const cob_flags_t	allowed_clauses =
		SYN_CLAUSE_1 | SYN_CLAUSE_2 | SYN_CLAUSE_3 | SYN_CLAUSE_7 | SYN_CLAUSE_12;
	cob_flags_t	tested = check_pic_duplicate & ~(allowed_clauses);
	if (tested != SYN_CLAUSE_30 && tested != SYN_CLAUSE_31
	 && tested != 0 /* USAGE as TYPE TO */) {
		struct cb_field *fld = CB_FIELD (current_field->external_definition);
		cb_error_x (CB_TREE(current_field), _("illegal combination of %s with other clauses"),
			fld->flag_is_typedef ? "TYPE TO" : "SAME AS");
		current_field->flag_is_verified = 1;
		current_field->flag_invalid = 1;
	} else {
		struct cb_field *fld = CB_FIELD (current_field->external_definition);
		int new_level = lvl ? cb_get_level (lvl) : 0;
		int old_level = current_field->level;
		copy_into_field (fld, current_field);
		if (new_level > 1 && new_level < 66 && new_level > old_level) {
			cb_error_x (lvl, _("entry following %s may not be subordinate to it"),
				fld->flag_is_typedef ? "TYPE TO" : "SAME AS");
		}
	}
}

static cb_tree
get_finalized_description_tree (void)
{
	struct cb_field *p;

	/* finalize last field if target of SAME AS / TYPEDEF */
	if (current_field && !CB_INVALID_TREE (current_field->external_definition)) {
		inherit_external_definition (NULL);
	}

	/* validate the complete current "block" */
	for (p = description_field; p; p = p->sister) {
		cb_validate_field (p);
	}
	return CB_TREE (description_field);
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

	case ALL_LEADING_TRAILING_PHRASES:
		if (previous_tallying_phrase == CHARACTERS_PHRASE
			   || previous_tallying_phrase == ALL_LEADING_TRAILING_PHRASES) {
			cb_error (_("missing value between ALL/LEADING/TRAILING words"));
		}
		/* fall through */
	case CHARACTERS_PHRASE:
		if (previous_tallying_phrase == NO_PHRASE) {
			cb_error (_("missing FOR phrase before CHARACTERS/ALL/LEADING/TRAILING phrase"));
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

static cb_tree
check_not_88_level (cb_tree x)
{
	struct cb_field	*f;

	if (x == cb_error_node) {
		return cb_error_node;
	}
	if (!CB_REF_OR_FIELD_P(x)) {
		return x;
	}

	f = CB_FIELD_PTR (x);

	if (f->level == 88) {
#if 0	/* note: we may consider to support the extension (if existing) to
		         reference a condition-name target by the condition-name */
		if (cb_verify (cb_condition_references_data, _("use of condition-name in place of data-name"))) {
			return CB_TREE (f->parent);
		}
#else
		cb_error (_("condition-name not allowed here: '%s'"), cb_name (x));
		/* invalidate field to prevent same error in typeck.c (validate_one) */
		/* FIXME: If we really need the additional check here then we missed
		          a call to cb_validate_one() somewhere */
		return cb_error_node; 
#endif
	} else {
		return x;
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
		cb_error (_("RENAMES item may not be used here"));
	} else if (f->flag_any_length) {
		cb_error (_("ANY LENGTH item not allowed here"));
	} else if (tree_class == CB_CLASS_INDEX
		|| tree_class == CB_CLASS_OBJECT
		|| tree_class == CB_CLASS_POINTER) {
		cb_error (_("item '%s' has wrong class for VALIDATE"), cb_name (x));
	}
}

static void
error_if_following_every_clause (void)
{
	if (ml_suppress_list
	    && CB_ML_SUPPRESS (CB_VALUE (ml_suppress_list))->target == CB_ML_SUPPRESS_TYPE) {
		cb_error (_("WHEN clause must follow EVERY clause"));
	}
}

static void
prepend_to_ml_suppress_list (cb_tree suppress_entry)
{
	cb_tree	new_list_head = CB_LIST_INIT (suppress_entry);
	cb_list_append (new_list_head, ml_suppress_list);
	ml_suppress_list = new_list_head;
}

static void
add_identifier_to_ml_suppress_conds (cb_tree identifier)
{
	cb_tree suppress_id = cb_build_ml_suppress_clause ();
	CB_ML_SUPPRESS (suppress_id)->target = CB_ML_SUPPRESS_IDENTIFIER;
	CB_ML_SUPPRESS (suppress_id)->identifier = identifier;
	prepend_to_ml_suppress_list (suppress_id);
}

static void
add_when_to_ml_suppress_conds (cb_tree when_list)
{
	struct cb_ml_suppress_clause	*last_suppress_clause;
	cb_tree	suppress_all;

	/*
	  If the preceding clause in SUPPRESS was an identifier, the WHEN
	  belongs to the identifier. If EVERY was preceding, the WHEN belongs to
	  the EVERY. Otherwise, the WHEN acts on the entire record.
	*/
	if (ml_suppress_list) {
		last_suppress_clause = CB_ML_SUPPRESS (CB_VALUE (ml_suppress_list));
		if ((last_suppress_clause->target == CB_ML_SUPPRESS_IDENTIFIER
		     || last_suppress_clause->target == CB_ML_SUPPRESS_TYPE)
		    && !last_suppress_clause->when_list) {
			last_suppress_clause->when_list = when_list;
			return;
		}
	}

	suppress_all = cb_build_ml_suppress_clause ();
	CB_ML_SUPPRESS (suppress_all)->when_list = when_list;
	prepend_to_ml_suppress_list (suppress_all);
}

static void
add_type_to_ml_suppress_conds (enum cb_ml_suppress_category category,
			       enum cb_ml_type ml_type)
{
	cb_tree	suppress_type = cb_build_ml_suppress_clause ();
	CB_ML_SUPPRESS (suppress_type)->target = CB_ML_SUPPRESS_TYPE;
	CB_ML_SUPPRESS (suppress_type)->category = category;
	CB_ML_SUPPRESS (suppress_type)->ml_type = ml_type;
	prepend_to_ml_suppress_list (suppress_type);
}

static void
set_record_size (cb_tree min, cb_tree max)
{
	int record_min, record_max;

	if (min) {
		record_min = cb_get_int (min);
		if (record_min < 0) {
			/* already handled by integer check */
		} else {
			current_file->record_min = record_min;
		}
	} else {
		record_min = 0;
	}
	if (!max) {
		return;
	}

	record_max = cb_get_int (max);
	if (record_max < 0) {
		/* already handled by integer check */
		return;
	} else if (record_max == 0) {
		/* Note: standard COBOL does not allow zero at all, use the related
		         configuration option */
		if (cb_records_mismatch_record_clause >= CB_ERROR) {
			cb_error (_("non-zero value expected"));
		}
		return;
	}
	if (current_file->organization == COB_ORG_INDEXED
	 && record_max > MAX_FD_RECORD_IDX)  {
		cb_error (_("RECORD size (IDX) exceeds maximum allowed (%d)"),
			MAX_FD_RECORD_IDX);
		current_file->record_max = MAX_FD_RECORD_IDX;
	} else if (record_max > MAX_FD_RECORD)  {
		cb_error (_("RECORD size exceeds maximum allowed (%d)"),
			MAX_FD_RECORD);
		current_file->record_max = MAX_FD_RECORD;
	} else {
		if (record_max <= record_min)  {
			cb_error (_("RECORD clause invalid"));
		}
		current_file->record_max = record_max;
	}
}


#line 2301 "parser.c"

# ifndef YY_CAST
#  ifdef __cplusplus
#   define YY_CAST(Type, Val) static_cast<Type> (Val)
#   define YY_REINTERPRET_CAST(Type, Val) reinterpret_cast<Type> (Val)
#  else
#   define YY_CAST(Type, Val) ((Type) (Val))
#   define YY_REINTERPRET_CAST(Type, Val) ((Type) (Val))
#  endif
# endif
# ifndef YY_NULLPTR
#  if defined __cplusplus
#   if 201103L <= __cplusplus
#    define YY_NULLPTR nullptr
#   else
#    define YY_NULLPTR 0
#   endif
#  else
#   define YY_NULLPTR ((void*)0)
#  endif
# endif

/* Enabling verbose error messages.  */
#ifdef YYERROR_VERBOSE
# undef YYERROR_VERBOSE
# define YYERROR_VERBOSE 1
#else
# define YYERROR_VERBOSE 1
#endif

/* Use api.header.include to #include this header
   instead of duplicating it here.  */
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
    ACTUAL = 264,
    ADD = 265,
    ADDRESS = 266,
    ADJUSTABLE_COLUMNS = 267,
    ADVANCING = 268,
    AFTER = 269,
    ALIGNMENT = 270,
    ALL = 271,
    ALLOCATE = 272,
    ALLOWING = 273,
    ALPHABET = 274,
    ALPHABETIC = 275,
    ALPHABETIC_LOWER = 276,
    ALPHABETIC_UPPER = 277,
    ALPHANUMERIC = 278,
    ALPHANUMERIC_EDITED = 279,
    ALSO = 280,
    ALTER = 281,
    ALTERNATE = 282,
    AND = 283,
    ANY = 284,
    APPLY = 285,
    ARE = 286,
    AREA = 287,
    AREAS = 288,
    ARGUMENT_NUMBER = 289,
    ARGUMENT_VALUE = 290,
    ARITHMETIC = 291,
    AS = 292,
    ASCENDING = 293,
    ASCII = 294,
    ASSIGN = 295,
    AT = 296,
    ATTRIBUTE = 297,
    ATTRIBUTES = 298,
    AUTO = 299,
    AUTO_DECIMAL = 300,
    AUTO_SPIN = 301,
    AUTOMATIC = 302,
    AWAY_FROM_ZERO = 303,
    BACKGROUND_COLOR = 304,
    BACKGROUND_HIGH = 305,
    BACKGROUND_LOW = 306,
    BACKGROUND_STANDARD = 307,
    BAR = 308,
    BASED = 309,
    BEFORE = 310,
    BELL = 311,
    BINARY = 312,
    BINARY_C_LONG = 313,
    BINARY_CHAR = 314,
    BINARY_DOUBLE = 315,
    BINARY_LONG = 316,
    BINARY_SEQUENTIAL = 317,
    BINARY_SHORT = 318,
    BIT = 319,
    BITMAP = 320,
    BITMAP_END = 321,
    BITMAP_HANDLE = 322,
    BITMAP_NUMBER = 323,
    BITMAP_START = 324,
    BITMAP_TIMER = 325,
    BITMAP_TRAILING = 326,
    BITMAP_TRANSPARENT_COLOR = 327,
    BITMAP_WIDTH = 328,
    BLANK = 329,
    BLINK = 330,
    BLOCK = 331,
    BOTTOM = 332,
    BOX = 333,
    BOXED = 334,
    BULK_ADDITION = 335,
    BUSY = 336,
    BUTTONS = 337,
    BY = 338,
    BYTE_LENGTH = 339,
    C = 340,
    CALENDAR_FONT = 341,
    CALL = 342,
    CANCEL = 343,
    CANCEL_BUTTON = 344,
    CAPACITY = 345,
    CARD_PUNCH = 346,
    CARD_READER = 347,
    CASSETTE = 348,
    CCOL = 349,
    CD = 350,
    CELL = 351,
    CELL_COLOR = 352,
    CELL_DATA = 353,
    CELL_FONT = 354,
    CELL_PROTECTION = 355,
    CENTER = 356,
    CENTERED = 357,
    CENTERED_HEADINGS = 358,
    CENTURY_DATE = 359,
    CF = 360,
    CH = 361,
    CHAINING = 362,
    CHANGED = 363,
    CHARACTER = 364,
    CHARACTERS = 365,
    CHECK_BOX = 366,
    CLASS = 367,
    CLASSIFICATION = 368,
    CLASS_NAME = 369,
    CLEAR_SELECTION = 370,
    CLINE = 371,
    CLINES = 372,
    CLOSE = 373,
    COBOL = 374,
    CODE = 375,
    CODE_SET = 376,
    COLLATING = 377,
    COL = 378,
    COLOR = 379,
    COLORS = 380,
    COLS = 381,
    COLUMN = 382,
    COLUMN_COLOR = 383,
    COLUMN_DIVIDERS = 384,
    COLUMN_FONT = 385,
    COLUMN_HEADINGS = 386,
    COLUMN_PROTECTION = 387,
    COLUMNS = 388,
    COMBO_BOX = 389,
    COMMA = 390,
    COMMAND_LINE = 391,
    COMMA_DELIM = 392,
    COMMIT = 393,
    COMMON = 394,
    COMMUNICATION = 395,
    COMP = 396,
    COMPUTE = 397,
    COMP_0 = 398,
    COMP_1 = 399,
    COMP_2 = 400,
    COMP_3 = 401,
    COMP_4 = 402,
    COMP_5 = 403,
    COMP_6 = 404,
    COMP_N = 405,
    COMP_X = 406,
    CONCATENATE_FUNC = 407,
    CONDITION = 408,
    CONFIGURATION = 409,
    CONSTANT = 410,
    CONTAINS = 411,
    CONTENT = 412,
    CONTENT_LENGTH_FUNC = 413,
    CONTENT_OF_FUNC = 414,
    CONTINUE = 415,
    CONTROL = 416,
    CONTROLS = 417,
    CONVERSION = 418,
    CONVERTING = 419,
    COPY = 420,
    COPY_SELECTION = 421,
    CORE_INDEX = 422,
    CORRESPONDING = 423,
    COUNT = 424,
    CRT = 425,
    CRT_UNDER = 426,
    CSIZE = 427,
    CURRENCY = 428,
    CURRENT_DATE_FUNC = 429,
    CURSOR = 430,
    CURSOR_COL = 431,
    CURSOR_COLOR = 432,
    CURSOR_FRAME_WIDTH = 433,
    CURSOR_ROW = 434,
    CURSOR_X = 435,
    CURSOR_Y = 436,
    CUSTOM_PRINT_TEMPLATE = 437,
    CYCLE = 438,
    CYL_INDEX = 439,
    CYL_OVERFLOW = 440,
    DASHED = 441,
    DATA = 442,
    DATA_COLUMNS = 443,
    DATA_TYPES = 444,
    DATE = 445,
    DATE_ENTRY = 446,
    DAY = 447,
    DAY_OF_WEEK = 448,
    DE = 449,
    DEBUGGING = 450,
    DECIMAL_POINT = 451,
    DECLARATIVES = 452,
    DEFAULT = 453,
    DEFAULT_BUTTON = 454,
    DEFAULT_FONT = 455,
    DELETE = 456,
    DELIMITED = 457,
    DELIMITER = 458,
    DEPENDING = 459,
    DESCENDING = 460,
    DESTINATION = 461,
    DESTROY = 462,
    DETAIL = 463,
    DISABLE = 464,
    DISC = 465,
    DISK = 466,
    DISP = 467,
    DISPLAY = 468,
    DISPLAY_COLUMNS = 469,
    DISPLAY_FORMAT = 470,
    DISPLAY_OF_FUNC = 471,
    DIVIDE = 472,
    DIVIDERS = 473,
    DIVIDER_COLOR = 474,
    DIVISION = 475,
    DOTDASH = 476,
    DOTTED = 477,
    DRAG_COLOR = 478,
    DROP_DOWN = 479,
    DROP_LIST = 480,
    DOWN = 481,
    DUPLICATES = 482,
    DYNAMIC = 483,
    EBCDIC = 484,
    EC = 485,
    ECHO = 486,
    EGI = 487,
    EIGHTY_EIGHT = 488,
    ENABLE = 489,
    ELEMENT = 490,
    ELSE = 491,
    EMI = 492,
    ENCRYPTION = 493,
    ENCODING = 494,
    END = 495,
    END_ACCEPT = 496,
    END_ADD = 497,
    END_CALL = 498,
    END_COMPUTE = 499,
    END_COLOR = 500,
    END_DELETE = 501,
    END_DISPLAY = 502,
    END_DIVIDE = 503,
    END_EVALUATE = 504,
    END_FUNCTION = 505,
    END_IF = 506,
    END_JSON = 507,
    END_MODIFY = 508,
    END_MULTIPLY = 509,
    END_PERFORM = 510,
    END_PROGRAM = 511,
    END_READ = 512,
    END_RECEIVE = 513,
    END_RETURN = 514,
    END_REWRITE = 515,
    END_SEARCH = 516,
    END_START = 517,
    END_STRING = 518,
    END_SUBTRACT = 519,
    END_UNSTRING = 520,
    END_WRITE = 521,
    END_XML = 522,
    ENGRAVED = 523,
    ENSURE_VISIBLE = 524,
    ENTRY = 525,
    ENTRY_CONVENTION = 526,
    ENTRY_FIELD = 527,
    ENTRY_REASON = 528,
    ENVIRONMENT = 529,
    ENVIRONMENT_NAME = 530,
    ENVIRONMENT_VALUE = 531,
    EOL = 532,
    EOP = 533,
    EOS = 534,
    EQUAL = 535,
    ERASE = 536,
    ERROR = 537,
    ESCAPE = 538,
    ESCAPE_BUTTON = 539,
    ESI = 540,
    EVALUATE = 541,
    EVENT = 542,
    EVENT_LIST = 543,
    EVENT_STATUS = 544,
    EVERY = 545,
    EXCEPTION = 546,
    EXCEPTION_CONDITION = 547,
    EXCEPTION_VALUE = 548,
    EXPAND = 549,
    EXCLUSIVE = 550,
    EXHIBIT = 551,
    EXIT = 552,
    EXPONENTIATION = 553,
    EXTEND = 554,
    EXTENDED_SEARCH = 555,
    EXTERNAL = 556,
    EXTERNAL_FORM = 557,
    F = 558,
    FD = 559,
    FH__FCD = 560,
    FH__KEYDEF = 561,
    FILE_CONTROL = 562,
    FILE_ID = 563,
    FILE_LIMIT = 564,
    FILE_LIMITS = 565,
    FILE_NAME = 566,
    FILE_POS = 567,
    FILL_COLOR = 568,
    FILL_COLOR2 = 569,
    FILL_PERCENT = 570,
    FILLER = 571,
    FINAL = 572,
    FINISH_REASON = 573,
    FIRST = 574,
    FIXED = 575,
    FIXED_FONT = 576,
    FIXED_WIDTH = 577,
    FLAT = 578,
    FLAT_BUTTONS = 579,
    FLOAT_BINARY_128 = 580,
    FLOAT_BINARY_32 = 581,
    FLOAT_BINARY_64 = 582,
    FLOAT_DECIMAL_16 = 583,
    FLOAT_DECIMAL_34 = 584,
    FLOAT_DECIMAL_7 = 585,
    FLOAT_EXTENDED = 586,
    FLOAT_LONG = 587,
    FLOAT_SHORT = 588,
    FLOATING = 589,
    FONT = 590,
    FOOTING = 591,
    FOR = 592,
    FOREGROUND_COLOR = 593,
    FOREVER = 594,
    FORMATTED_DATE_FUNC = 595,
    FORMATTED_DATETIME_FUNC = 596,
    FORMATTED_TIME_FUNC = 597,
    FRAME = 598,
    FRAMED = 599,
    FREE = 600,
    FROM = 601,
    FROM_CRT = 602,
    FULL = 603,
    FULL_HEIGHT = 604,
    FUNCTION = 605,
    FUNCTION_ID = 606,
    FUNCTION_NAME = 607,
    GENERATE = 608,
    GIVING = 609,
    GLOBAL = 610,
    GO = 611,
    GO_BACK = 612,
    GO_FORWARD = 613,
    GO_HOME = 614,
    GO_SEARCH = 615,
    GOBACK = 616,
    GRAPHICAL = 617,
    GREATER = 618,
    GREATER_OR_EQUAL = 619,
    GRID = 620,
    GROUP = 621,
    GROUP_VALUE = 622,
    HANDLE = 623,
    HAS_CHILDREN = 624,
    HEADING = 625,
    HEADING_COLOR = 626,
    HEADING_DIVIDER_COLOR = 627,
    HEADING_FONT = 628,
    HEAVY = 629,
    HEIGHT_IN_CELLS = 630,
    HIDDEN_DATA = 631,
    HIGHLIGHT = 632,
    HIGH_COLOR = 633,
    HIGH_VALUE = 634,
    HOT_TRACK = 635,
    HSCROLL = 636,
    HSCROLL_POS = 637,
    ICON = 638,
    ID = 639,
    IDENTIFIED = 640,
    IDENTIFICATION = 641,
    IF = 642,
    IGNORE = 643,
    IGNORING = 644,
    IN = 645,
    INDEPENDENT = 646,
    INDEX = 647,
    INDEXED = 648,
    INDICATE = 649,
    INITIALIZE = 650,
    INITIALIZED = 651,
    INITIATE = 652,
    INPUT = 653,
    INPUT_OUTPUT = 654,
    INQUIRE = 655,
    INSERTION_INDEX = 656,
    INSERT_ROWS = 657,
    INSPECT = 658,
    INTERMEDIATE = 659,
    INTO = 660,
    INTRINSIC = 661,
    INVALID = 662,
    INVALID_KEY = 663,
    IS = 664,
    ITEM = 665,
    ITEM_TEXT = 666,
    ITEM_TO_ADD = 667,
    ITEM_TO_DELETE = 668,
    ITEM_TO_EMPTY = 669,
    ITEM_VALUE = 670,
    I_O = 671,
    I_O_CONTROL = 672,
    JSON = 673,
    JUSTIFIED = 674,
    KEPT = 675,
    KEY = 676,
    KEYBOARD = 677,
    LABEL = 678,
    LABEL_OFFSET = 679,
    LARGE_FONT = 680,
    LARGE_OFFSET = 681,
    LAST = 682,
    LAST_ROW = 683,
    LAYOUT_DATA = 684,
    LAYOUT_MANAGER = 685,
    LEADING = 686,
    LEADING_SHIFT = 687,
    LEAVE = 688,
    LEFT = 689,
    LEFTLINE = 690,
    LEFT_TEXT = 691,
    LENGTH = 692,
    LENGTH_OF = 693,
    LENGTH_FUNC = 694,
    LESS = 695,
    LESS_OR_EQUAL = 696,
    LEVEL_NUMBER = 697,
    LIKE = 698,
    LIMIT = 699,
    LIMITS = 700,
    LINAGE = 701,
    LINAGE_COUNTER = 702,
    LINE = 703,
    LINE_COUNTER = 704,
    LINE_LIMIT = 705,
    LINE_SEQUENTIAL = 706,
    LINES = 707,
    LINES_AT_ROOT = 708,
    LINKAGE = 709,
    LIST_BOX = 710,
    LITERAL = 711,
    LM_RESIZE = 712,
    LOC = 713,
    LOCALE = 714,
    LOCALE_DATE_FUNC = 715,
    LOCALE_TIME_FUNC = 716,
    LOCALE_TIME_FROM_FUNC = 717,
    LOCAL_STORAGE = 718,
    LOCK = 719,
    LOCK_HOLDING = 720,
    LONG_DATE = 721,
    LOWER = 722,
    LOWERED = 723,
    LOWER_CASE_FUNC = 724,
    LOWLIGHT = 725,
    LOW_COLOR = 726,
    LOW_VALUE = 727,
    MAGNETIC_TAPE = 728,
    MANUAL = 729,
    MASS_UPDATE = 730,
    MASTER_INDEX = 731,
    MAX_LINES = 732,
    MAX_PROGRESS = 733,
    MAX_TEXT = 734,
    MAX_VAL = 735,
    MEMORY = 736,
    MEDIUM_FONT = 737,
    MENU = 738,
    MERGE = 739,
    MESSAGE = 740,
    MINUS = 741,
    MIN_VAL = 742,
    MNEMONIC_NAME = 743,
    MODE = 744,
    MODIFY = 745,
    MODULES = 746,
    MOVE = 747,
    MULTILINE = 748,
    MULTIPLE = 749,
    MULTIPLY = 750,
    NAME = 751,
    NAMED = 752,
    NAMESPACE = 753,
    NAMESPACE_PREFIX = 754,
    NATIONAL = 755,
    NATIONAL_EDITED = 756,
    NATIONAL_OF_FUNC = 757,
    NATIVE = 758,
    NAVIGATE_URL = 759,
    NEAREST_AWAY_FROM_ZERO = 760,
    NEAREST_EVEN = 761,
    NEAREST_TOWARD_ZERO = 762,
    NEGATIVE = 763,
    NESTED = 764,
    NEW = 765,
    NEXT = 766,
    NEXT_ITEM = 767,
    NEXT_GROUP = 768,
    NEXT_PAGE = 769,
    NO = 770,
    NO_ADVANCING = 771,
    NO_AUTOSEL = 772,
    NO_AUTO_DEFAULT = 773,
    NO_BOX = 774,
    NO_DATA = 775,
    NO_DIVIDERS = 776,
    NO_ECHO = 777,
    NO_F4 = 778,
    NO_FOCUS = 779,
    NO_GROUP_TAB = 780,
    NO_KEY_LETTER = 781,
    NOMINAL = 782,
    NO_SEARCH = 783,
    NO_UPDOWN = 784,
    NONNUMERIC = 785,
    NORMAL = 786,
    NOT = 787,
    NOTAB = 788,
    NOTHING = 789,
    NOTIFY = 790,
    NOTIFY_CHANGE = 791,
    NOTIFY_DBLCLICK = 792,
    NOTIFY_SELCHANGE = 793,
    NOT_END = 794,
    NOT_EOP = 795,
    NOT_ESCAPE = 796,
    NOT_EQUAL = 797,
    NOT_EXCEPTION = 798,
    NOT_INVALID_KEY = 799,
    NOT_OVERFLOW = 800,
    NOT_SIZE_ERROR = 801,
    NUM_COL_HEADINGS = 802,
    NUM_ROWS = 803,
    NUMBER = 804,
    NUMBERS = 805,
    NUMERIC = 806,
    NUMERIC_EDITED = 807,
    NUMVALC_FUNC = 808,
    OBJECT = 809,
    OBJECT_COMPUTER = 810,
    OCCURS = 811,
    OF = 812,
    OFF = 813,
    OK_BUTTON = 814,
    OMITTED = 815,
    ON = 816,
    ONLY = 817,
    OPEN = 818,
    OPTIONAL = 819,
    OPTIONS = 820,
    OR = 821,
    ORDER = 822,
    ORGANIZATION = 823,
    OTHER = 824,
    OTHERS = 825,
    OUTPUT = 826,
    OVERLAP_LEFT = 827,
    OVERLAP_TOP = 828,
    OVERLINE = 829,
    PACKED_DECIMAL = 830,
    PADDING = 831,
    PASCAL = 832,
    PAGE = 833,
    PAGE_COUNTER = 834,
    PAGE_SETUP = 835,
    PAGED = 836,
    PARAGRAPH = 837,
    PARENT = 838,
    PARSE = 839,
    PASSWORD = 840,
    PERFORM = 841,
    PERMANENT = 842,
    PH = 843,
    PF = 844,
    PHYSICAL = 845,
    PICTURE = 846,
    PICTURE_SYMBOL = 847,
    PIXEL = 848,
    PLACEMENT = 849,
    PLUS = 850,
    POINTER = 851,
    POP_UP = 852,
    POS = 853,
    POSITION = 854,
    POSITION_SHIFT = 855,
    POSITIVE = 856,
    PRESENT = 857,
    PREVIOUS = 858,
    PRINT = 859,
    PRINT_CONTROL = 860,
    PRINT_NO_PROMPT = 861,
    PRINT_PREVIEW = 862,
    PRINTER = 863,
    PRINTER_1 = 864,
    PRINTING = 865,
    PRIORITY = 866,
    PROCEDURE = 867,
    PROCEDURES = 868,
    PROCEED = 869,
    PROCESSING = 870,
    PROGRAM = 871,
    PROGRAM_ID = 872,
    PROGRAM_NAME = 873,
    PROGRAM_POINTER = 874,
    PROGRESS = 875,
    PROHIBITED = 876,
    PROMPT = 877,
    PROPERTIES = 878,
    PROPERTY = 879,
    PROTECTED = 880,
    PURGE = 881,
    PUSH_BUTTON = 882,
    QUERY_INDEX = 883,
    QUEUE = 884,
    QUOTE = 885,
    RADIO_BUTTON = 886,
    RAISE = 887,
    RAISED = 888,
    RANDOM = 889,
    RD = 890,
    READ = 891,
    READERS = 892,
    READ_ONLY = 893,
    READY_TRACE = 894,
    RECEIVE = 895,
    RECORD = 896,
    RECORD_DATA = 897,
    RECORD_OVERFLOW = 898,
    RECORD_TO_ADD = 899,
    RECORD_TO_DELETE = 900,
    RECORDING = 901,
    RECORDS = 902,
    RECURSIVE = 903,
    REDEFINES = 904,
    REEL = 905,
    REFERENCE = 906,
    REFERENCES = 907,
    REFRESH = 908,
    REGION_COLOR = 909,
    RELATIVE = 910,
    RELEASE = 911,
    REMAINDER = 912,
    REMOVAL = 913,
    RENAMES = 914,
    REORG_CRITERIA = 915,
    REPLACE = 916,
    REPLACING = 917,
    REPORT = 918,
    REPORTING = 919,
    REPORTS = 920,
    REPOSITORY = 921,
    REQUIRED = 922,
    REREAD = 923,
    RERUN = 924,
    RESERVE = 925,
    RESET = 926,
    RESET_TRACE = 927,
    RESET_GRID = 928,
    RESET_LIST = 929,
    RESET_TABS = 930,
    RETRY = 931,
    RETURN = 932,
    RETURNING = 933,
    REVERSE = 934,
    REVERSE_FUNC = 935,
    REVERSE_VIDEO = 936,
    REVERSED = 937,
    REWIND = 938,
    REWRITE = 939,
    RF = 940,
    RH = 941,
    RIGHT = 942,
    RIGHT_ALIGN = 943,
    RIMMED = 944,
    ROLLBACK = 945,
    ROUNDED = 946,
    ROUNDING = 947,
    ROW_COLOR = 948,
    ROW_COLOR_PATTERN = 949,
    ROW_DIVIDERS = 950,
    ROW_FONT = 951,
    ROW_HEADINGS = 952,
    ROW_PROTECTION = 953,
    RUN = 954,
    S = 955,
    SAME = 956,
    SAVE_AS = 957,
    SAVE_AS_NO_PROMPT = 958,
    SCREEN = 959,
    SCREEN_CONTROL = 960,
    SCROLL = 961,
    SCROLL_BAR = 962,
    SD = 963,
    SEARCH = 964,
    SEARCH_OPTIONS = 965,
    SEARCH_TEXT = 966,
    SECONDS = 967,
    SECTION = 968,
    SECURE = 969,
    SEGMENT = 970,
    SEGMENT_LIMIT = 971,
    SELECT = 972,
    SELECTION_INDEX = 973,
    SELECTION_TEXT = 974,
    SELECT_ALL = 975,
    SELF_ACT = 976,
    SEMI_COLON = 977,
    SEND = 978,
    SENTENCE = 979,
    SEPARATE = 980,
    SEPARATION = 981,
    SEQUENCE = 982,
    SEQUENTIAL = 983,
    SET = 984,
    SEVENTY_EIGHT = 985,
    SHADING = 986,
    SHADOW = 987,
    SHARING = 988,
    SHORT_DATE = 989,
    SHOW_LINES = 990,
    SHOW_NONE = 991,
    SHOW_SEL_ALWAYS = 992,
    SIGN = 993,
    SIGNED = 994,
    SIGNED_INT = 995,
    SIGNED_LONG = 996,
    SIGNED_SHORT = 997,
    SIXTY_SIX = 998,
    SIZE = 999,
    SIZE_ERROR = 1000,
    SMALL_FONT = 1001,
    SORT = 1002,
    SORT_MERGE = 1003,
    SORT_ORDER = 1004,
    SOURCE = 1005,
    SOURCE_COMPUTER = 1006,
    SPACE = 1007,
    SPECIAL_NAMES = 1008,
    SPINNER = 1009,
    SQUARE = 1010,
    STANDARD = 1011,
    STANDARD_1 = 1012,
    STANDARD_2 = 1013,
    STANDARD_BINARY = 1014,
    STANDARD_DECIMAL = 1015,
    START = 1016,
    START_X = 1017,
    START_Y = 1018,
    STATIC = 1019,
    STATIC_LIST = 1020,
    STATUS = 1021,
    STATUS_BAR = 1022,
    STATUS_TEXT = 1023,
    STDCALL = 1024,
    STEP = 1025,
    STOP = 1026,
    STRING = 1027,
    STRONG = 1028,
    STYLE = 1029,
    SUB_QUEUE_1 = 1030,
    SUB_QUEUE_2 = 1031,
    SUB_QUEUE_3 = 1032,
    SUBSTITUTE_FUNC = 1033,
    SUBSTITUTE_CASE_FUNC = 1034,
    SUBTRACT = 1035,
    SUBWINDOW = 1036,
    SUM = 1037,
    SUPPRESS = 1038,
    SUPPRESS_XML = 1039,
    SYMBOLIC = 1040,
    SYNCHRONIZED = 1041,
    SYSTEM_DEFAULT = 1042,
    SYSTEM_INFO = 1043,
    SYSTEM_OFFSET = 1044,
    TAB = 1045,
    TAB_TO_ADD = 1046,
    TAB_TO_DELETE = 1047,
    TABLE = 1048,
    TALLYING = 1049,
    TEMPORARY = 1050,
    TAPE = 1051,
    TERMINAL = 1052,
    TERMINATE = 1053,
    TERMINAL_INFO = 1054,
    TERMINATION_VALUE = 1055,
    TEST = 1056,
    TEXT = 1057,
    THAN = 1058,
    THEN = 1059,
    THREAD = 1060,
    THREADS = 1061,
    THRU = 1062,
    THUMB_POSITION = 1063,
    TILED_HEADINGS = 1064,
    TIME = 1065,
    TIME_OUT = 1066,
    TIMES = 1067,
    TITLE = 1068,
    TITLE_POSITION = 1069,
    TO = 1070,
    TOK_AMPER = 1071,
    TOK_CLOSE_PAREN = 1072,
    TOK_COLON = 1073,
    TOK_DIV = 1074,
    TOK_DOT = 1075,
    TOK_EQUAL = 1076,
    TOK_EXTERN = 1077,
    TOK_FALSE = 1078,
    TOK_FILE = 1079,
    TOK_GREATER = 1080,
    TOK_INITIAL = 1081,
    TOK_LESS = 1082,
    TOK_MINUS = 1083,
    TOK_MUL = 1084,
    TOK_NULL = 1085,
    TOK_OVERFLOW = 1086,
    TOK_OPEN_PAREN = 1087,
    TOK_PLUS = 1088,
    TOK_TRUE = 1089,
    TOP = 1090,
    TOWARD_GREATER = 1091,
    TOWARD_LESSER = 1092,
    TRACK = 1093,
    TRACKS = 1094,
    TRACK_AREA = 1095,
    TRACK_LIMIT = 1096,
    TRADITIONAL_FONT = 1097,
    TRAILING = 1098,
    TRAILING_SHIFT = 1099,
    TRANSFORM = 1100,
    TRANSPARENT = 1101,
    TREE_VIEW = 1102,
    TRIM_FUNC = 1103,
    TRUNCATION = 1104,
    TYPE = 1105,
    TYPEDEF = 1106,
    U = 1107,
    UCS_4 = 1108,
    UNBOUNDED = 1109,
    UNDERLINE = 1110,
    UNFRAMED = 1111,
    UNIT = 1112,
    UNLOCK = 1113,
    UNSIGNED = 1114,
    UNSIGNED_INT = 1115,
    UNSIGNED_LONG = 1116,
    UNSIGNED_SHORT = 1117,
    UNSORTED = 1118,
    UNSTRING = 1119,
    UNTIL = 1120,
    UP = 1121,
    UPDATE = 1122,
    UPDATERS = 1123,
    UPON = 1124,
    UPON_ARGUMENT_NUMBER = 1125,
    UPON_COMMAND_LINE = 1126,
    UPON_ENVIRONMENT_NAME = 1127,
    UPON_ENVIRONMENT_VALUE = 1128,
    UPPER = 1129,
    UPPER_CASE_FUNC = 1130,
    USAGE = 1131,
    USE = 1132,
    USE_ALT = 1133,
    USE_RETURN = 1134,
    USE_TAB = 1135,
    USER = 1136,
    USER_DEFAULT = 1137,
    USER_FUNCTION_NAME = 1138,
    USING = 1139,
    UTF_8 = 1140,
    UTF_16 = 1141,
    V = 1142,
    VALIDATE = 1143,
    VALIDATING = 1144,
    VALUE = 1145,
    VALUE_FORMAT = 1146,
    VARIABLE = 1147,
    VARIANT = 1148,
    VARYING = 1149,
    VERTICAL = 1150,
    VERY_HEAVY = 1151,
    VIRTUAL_WIDTH = 1152,
    VOLATILE = 1153,
    VPADDING = 1154,
    VSCROLL = 1155,
    VSCROLL_BAR = 1156,
    VSCROLL_POS = 1157,
    VTOP = 1158,
    WAIT = 1159,
    WEB_BROWSER = 1160,
    WHEN = 1161,
    WHEN_COMPILED_FUNC = 1162,
    WHEN_XML = 1163,
    WIDTH = 1164,
    WIDTH_IN_CELLS = 1165,
    WINDOW = 1166,
    WITH = 1167,
    WORD = 1168,
    WORDS = 1169,
    WORKING_STORAGE = 1170,
    WRAP = 1171,
    WRITE = 1172,
    WRITE_ONLY = 1173,
    WRITE_VERIFY = 1174,
    WRITERS = 1175,
    X = 1176,
    XML = 1177,
    XML_DECLARATION = 1178,
    Y = 1179,
    YYYYDDD = 1180,
    YYYYMMDD = 1181,
    ZERO = 1182,
    SHIFT_PREFER = 1183
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
#define ACTUAL 264
#define ADD 265
#define ADDRESS 266
#define ADJUSTABLE_COLUMNS 267
#define ADVANCING 268
#define AFTER 269
#define ALIGNMENT 270
#define ALL 271
#define ALLOCATE 272
#define ALLOWING 273
#define ALPHABET 274
#define ALPHABETIC 275
#define ALPHABETIC_LOWER 276
#define ALPHABETIC_UPPER 277
#define ALPHANUMERIC 278
#define ALPHANUMERIC_EDITED 279
#define ALSO 280
#define ALTER 281
#define ALTERNATE 282
#define AND 283
#define ANY 284
#define APPLY 285
#define ARE 286
#define AREA 287
#define AREAS 288
#define ARGUMENT_NUMBER 289
#define ARGUMENT_VALUE 290
#define ARITHMETIC 291
#define AS 292
#define ASCENDING 293
#define ASCII 294
#define ASSIGN 295
#define AT 296
#define ATTRIBUTE 297
#define ATTRIBUTES 298
#define AUTO 299
#define AUTO_DECIMAL 300
#define AUTO_SPIN 301
#define AUTOMATIC 302
#define AWAY_FROM_ZERO 303
#define BACKGROUND_COLOR 304
#define BACKGROUND_HIGH 305
#define BACKGROUND_LOW 306
#define BACKGROUND_STANDARD 307
#define BAR 308
#define BASED 309
#define BEFORE 310
#define BELL 311
#define BINARY 312
#define BINARY_C_LONG 313
#define BINARY_CHAR 314
#define BINARY_DOUBLE 315
#define BINARY_LONG 316
#define BINARY_SEQUENTIAL 317
#define BINARY_SHORT 318
#define BIT 319
#define BITMAP 320
#define BITMAP_END 321
#define BITMAP_HANDLE 322
#define BITMAP_NUMBER 323
#define BITMAP_START 324
#define BITMAP_TIMER 325
#define BITMAP_TRAILING 326
#define BITMAP_TRANSPARENT_COLOR 327
#define BITMAP_WIDTH 328
#define BLANK 329
#define BLINK 330
#define BLOCK 331
#define BOTTOM 332
#define BOX 333
#define BOXED 334
#define BULK_ADDITION 335
#define BUSY 336
#define BUTTONS 337
#define BY 338
#define BYTE_LENGTH 339
#define C 340
#define CALENDAR_FONT 341
#define CALL 342
#define CANCEL 343
#define CANCEL_BUTTON 344
#define CAPACITY 345
#define CARD_PUNCH 346
#define CARD_READER 347
#define CASSETTE 348
#define CCOL 349
#define CD 350
#define CELL 351
#define CELL_COLOR 352
#define CELL_DATA 353
#define CELL_FONT 354
#define CELL_PROTECTION 355
#define CENTER 356
#define CENTERED 357
#define CENTERED_HEADINGS 358
#define CENTURY_DATE 359
#define CF 360
#define CH 361
#define CHAINING 362
#define CHANGED 363
#define CHARACTER 364
#define CHARACTERS 365
#define CHECK_BOX 366
#define CLASS 367
#define CLASSIFICATION 368
#define CLASS_NAME 369
#define CLEAR_SELECTION 370
#define CLINE 371
#define CLINES 372
#define CLOSE 373
#define COBOL 374
#define CODE 375
#define CODE_SET 376
#define COLLATING 377
#define COL 378
#define COLOR 379
#define COLORS 380
#define COLS 381
#define COLUMN 382
#define COLUMN_COLOR 383
#define COLUMN_DIVIDERS 384
#define COLUMN_FONT 385
#define COLUMN_HEADINGS 386
#define COLUMN_PROTECTION 387
#define COLUMNS 388
#define COMBO_BOX 389
#define COMMA 390
#define COMMAND_LINE 391
#define COMMA_DELIM 392
#define COMMIT 393
#define COMMON 394
#define COMMUNICATION 395
#define COMP 396
#define COMPUTE 397
#define COMP_0 398
#define COMP_1 399
#define COMP_2 400
#define COMP_3 401
#define COMP_4 402
#define COMP_5 403
#define COMP_6 404
#define COMP_N 405
#define COMP_X 406
#define CONCATENATE_FUNC 407
#define CONDITION 408
#define CONFIGURATION 409
#define CONSTANT 410
#define CONTAINS 411
#define CONTENT 412
#define CONTENT_LENGTH_FUNC 413
#define CONTENT_OF_FUNC 414
#define CONTINUE 415
#define CONTROL 416
#define CONTROLS 417
#define CONVERSION 418
#define CONVERTING 419
#define COPY 420
#define COPY_SELECTION 421
#define CORE_INDEX 422
#define CORRESPONDING 423
#define COUNT 424
#define CRT 425
#define CRT_UNDER 426
#define CSIZE 427
#define CURRENCY 428
#define CURRENT_DATE_FUNC 429
#define CURSOR 430
#define CURSOR_COL 431
#define CURSOR_COLOR 432
#define CURSOR_FRAME_WIDTH 433
#define CURSOR_ROW 434
#define CURSOR_X 435
#define CURSOR_Y 436
#define CUSTOM_PRINT_TEMPLATE 437
#define CYCLE 438
#define CYL_INDEX 439
#define CYL_OVERFLOW 440
#define DASHED 441
#define DATA 442
#define DATA_COLUMNS 443
#define DATA_TYPES 444
#define DATE 445
#define DATE_ENTRY 446
#define DAY 447
#define DAY_OF_WEEK 448
#define DE 449
#define DEBUGGING 450
#define DECIMAL_POINT 451
#define DECLARATIVES 452
#define DEFAULT 453
#define DEFAULT_BUTTON 454
#define DEFAULT_FONT 455
#define DELETE 456
#define DELIMITED 457
#define DELIMITER 458
#define DEPENDING 459
#define DESCENDING 460
#define DESTINATION 461
#define DESTROY 462
#define DETAIL 463
#define DISABLE 464
#define DISC 465
#define DISK 466
#define DISP 467
#define DISPLAY 468
#define DISPLAY_COLUMNS 469
#define DISPLAY_FORMAT 470
#define DISPLAY_OF_FUNC 471
#define DIVIDE 472
#define DIVIDERS 473
#define DIVIDER_COLOR 474
#define DIVISION 475
#define DOTDASH 476
#define DOTTED 477
#define DRAG_COLOR 478
#define DROP_DOWN 479
#define DROP_LIST 480
#define DOWN 481
#define DUPLICATES 482
#define DYNAMIC 483
#define EBCDIC 484
#define EC 485
#define ECHO 486
#define EGI 487
#define EIGHTY_EIGHT 488
#define ENABLE 489
#define ELEMENT 490
#define ELSE 491
#define EMI 492
#define ENCRYPTION 493
#define ENCODING 494
#define END 495
#define END_ACCEPT 496
#define END_ADD 497
#define END_CALL 498
#define END_COMPUTE 499
#define END_COLOR 500
#define END_DELETE 501
#define END_DISPLAY 502
#define END_DIVIDE 503
#define END_EVALUATE 504
#define END_FUNCTION 505
#define END_IF 506
#define END_JSON 507
#define END_MODIFY 508
#define END_MULTIPLY 509
#define END_PERFORM 510
#define END_PROGRAM 511
#define END_READ 512
#define END_RECEIVE 513
#define END_RETURN 514
#define END_REWRITE 515
#define END_SEARCH 516
#define END_START 517
#define END_STRING 518
#define END_SUBTRACT 519
#define END_UNSTRING 520
#define END_WRITE 521
#define END_XML 522
#define ENGRAVED 523
#define ENSURE_VISIBLE 524
#define ENTRY 525
#define ENTRY_CONVENTION 526
#define ENTRY_FIELD 527
#define ENTRY_REASON 528
#define ENVIRONMENT 529
#define ENVIRONMENT_NAME 530
#define ENVIRONMENT_VALUE 531
#define EOL 532
#define EOP 533
#define EOS 534
#define EQUAL 535
#define ERASE 536
#define ERROR 537
#define ESCAPE 538
#define ESCAPE_BUTTON 539
#define ESI 540
#define EVALUATE 541
#define EVENT 542
#define EVENT_LIST 543
#define EVENT_STATUS 544
#define EVERY 545
#define EXCEPTION 546
#define EXCEPTION_CONDITION 547
#define EXCEPTION_VALUE 548
#define EXPAND 549
#define EXCLUSIVE 550
#define EXHIBIT 551
#define EXIT 552
#define EXPONENTIATION 553
#define EXTEND 554
#define EXTENDED_SEARCH 555
#define EXTERNAL 556
#define EXTERNAL_FORM 557
#define F 558
#define FD 559
#define FH__FCD 560
#define FH__KEYDEF 561
#define FILE_CONTROL 562
#define FILE_ID 563
#define FILE_LIMIT 564
#define FILE_LIMITS 565
#define FILE_NAME 566
#define FILE_POS 567
#define FILL_COLOR 568
#define FILL_COLOR2 569
#define FILL_PERCENT 570
#define FILLER 571
#define FINAL 572
#define FINISH_REASON 573
#define FIRST 574
#define FIXED 575
#define FIXED_FONT 576
#define FIXED_WIDTH 577
#define FLAT 578
#define FLAT_BUTTONS 579
#define FLOAT_BINARY_128 580
#define FLOAT_BINARY_32 581
#define FLOAT_BINARY_64 582
#define FLOAT_DECIMAL_16 583
#define FLOAT_DECIMAL_34 584
#define FLOAT_DECIMAL_7 585
#define FLOAT_EXTENDED 586
#define FLOAT_LONG 587
#define FLOAT_SHORT 588
#define FLOATING 589
#define FONT 590
#define FOOTING 591
#define FOR 592
#define FOREGROUND_COLOR 593
#define FOREVER 594
#define FORMATTED_DATE_FUNC 595
#define FORMATTED_DATETIME_FUNC 596
#define FORMATTED_TIME_FUNC 597
#define FRAME 598
#define FRAMED 599
#define FREE 600
#define FROM 601
#define FROM_CRT 602
#define FULL 603
#define FULL_HEIGHT 604
#define FUNCTION 605
#define FUNCTION_ID 606
#define FUNCTION_NAME 607
#define GENERATE 608
#define GIVING 609
#define GLOBAL 610
#define GO 611
#define GO_BACK 612
#define GO_FORWARD 613
#define GO_HOME 614
#define GO_SEARCH 615
#define GOBACK 616
#define GRAPHICAL 617
#define GREATER 618
#define GREATER_OR_EQUAL 619
#define GRID 620
#define GROUP 621
#define GROUP_VALUE 622
#define HANDLE 623
#define HAS_CHILDREN 624
#define HEADING 625
#define HEADING_COLOR 626
#define HEADING_DIVIDER_COLOR 627
#define HEADING_FONT 628
#define HEAVY 629
#define HEIGHT_IN_CELLS 630
#define HIDDEN_DATA 631
#define HIGHLIGHT 632
#define HIGH_COLOR 633
#define HIGH_VALUE 634
#define HOT_TRACK 635
#define HSCROLL 636
#define HSCROLL_POS 637
#define ICON 638
#define ID 639
#define IDENTIFIED 640
#define IDENTIFICATION 641
#define IF 642
#define IGNORE 643
#define IGNORING 644
#define IN 645
#define INDEPENDENT 646
#define INDEX 647
#define INDEXED 648
#define INDICATE 649
#define INITIALIZE 650
#define INITIALIZED 651
#define INITIATE 652
#define INPUT 653
#define INPUT_OUTPUT 654
#define INQUIRE 655
#define INSERTION_INDEX 656
#define INSERT_ROWS 657
#define INSPECT 658
#define INTERMEDIATE 659
#define INTO 660
#define INTRINSIC 661
#define INVALID 662
#define INVALID_KEY 663
#define IS 664
#define ITEM 665
#define ITEM_TEXT 666
#define ITEM_TO_ADD 667
#define ITEM_TO_DELETE 668
#define ITEM_TO_EMPTY 669
#define ITEM_VALUE 670
#define I_O 671
#define I_O_CONTROL 672
#define JSON 673
#define JUSTIFIED 674
#define KEPT 675
#define KEY 676
#define KEYBOARD 677
#define LABEL 678
#define LABEL_OFFSET 679
#define LARGE_FONT 680
#define LARGE_OFFSET 681
#define LAST 682
#define LAST_ROW 683
#define LAYOUT_DATA 684
#define LAYOUT_MANAGER 685
#define LEADING 686
#define LEADING_SHIFT 687
#define LEAVE 688
#define LEFT 689
#define LEFTLINE 690
#define LEFT_TEXT 691
#define LENGTH 692
#define LENGTH_OF 693
#define LENGTH_FUNC 694
#define LESS 695
#define LESS_OR_EQUAL 696
#define LEVEL_NUMBER 697
#define LIKE 698
#define LIMIT 699
#define LIMITS 700
#define LINAGE 701
#define LINAGE_COUNTER 702
#define LINE 703
#define LINE_COUNTER 704
#define LINE_LIMIT 705
#define LINE_SEQUENTIAL 706
#define LINES 707
#define LINES_AT_ROOT 708
#define LINKAGE 709
#define LIST_BOX 710
#define LITERAL 711
#define LM_RESIZE 712
#define LOC 713
#define LOCALE 714
#define LOCALE_DATE_FUNC 715
#define LOCALE_TIME_FUNC 716
#define LOCALE_TIME_FROM_FUNC 717
#define LOCAL_STORAGE 718
#define LOCK 719
#define LOCK_HOLDING 720
#define LONG_DATE 721
#define LOWER 722
#define LOWERED 723
#define LOWER_CASE_FUNC 724
#define LOWLIGHT 725
#define LOW_COLOR 726
#define LOW_VALUE 727
#define MAGNETIC_TAPE 728
#define MANUAL 729
#define MASS_UPDATE 730
#define MASTER_INDEX 731
#define MAX_LINES 732
#define MAX_PROGRESS 733
#define MAX_TEXT 734
#define MAX_VAL 735
#define MEMORY 736
#define MEDIUM_FONT 737
#define MENU 738
#define MERGE 739
#define MESSAGE 740
#define MINUS 741
#define MIN_VAL 742
#define MNEMONIC_NAME 743
#define MODE 744
#define MODIFY 745
#define MODULES 746
#define MOVE 747
#define MULTILINE 748
#define MULTIPLE 749
#define MULTIPLY 750
#define NAME 751
#define NAMED 752
#define NAMESPACE 753
#define NAMESPACE_PREFIX 754
#define NATIONAL 755
#define NATIONAL_EDITED 756
#define NATIONAL_OF_FUNC 757
#define NATIVE 758
#define NAVIGATE_URL 759
#define NEAREST_AWAY_FROM_ZERO 760
#define NEAREST_EVEN 761
#define NEAREST_TOWARD_ZERO 762
#define NEGATIVE 763
#define NESTED 764
#define NEW 765
#define NEXT 766
#define NEXT_ITEM 767
#define NEXT_GROUP 768
#define NEXT_PAGE 769
#define NO 770
#define NO_ADVANCING 771
#define NO_AUTOSEL 772
#define NO_AUTO_DEFAULT 773
#define NO_BOX 774
#define NO_DATA 775
#define NO_DIVIDERS 776
#define NO_ECHO 777
#define NO_F4 778
#define NO_FOCUS 779
#define NO_GROUP_TAB 780
#define NO_KEY_LETTER 781
#define NOMINAL 782
#define NO_SEARCH 783
#define NO_UPDOWN 784
#define NONNUMERIC 785
#define NORMAL 786
#define NOT 787
#define NOTAB 788
#define NOTHING 789
#define NOTIFY 790
#define NOTIFY_CHANGE 791
#define NOTIFY_DBLCLICK 792
#define NOTIFY_SELCHANGE 793
#define NOT_END 794
#define NOT_EOP 795
#define NOT_ESCAPE 796
#define NOT_EQUAL 797
#define NOT_EXCEPTION 798
#define NOT_INVALID_KEY 799
#define NOT_OVERFLOW 800
#define NOT_SIZE_ERROR 801
#define NUM_COL_HEADINGS 802
#define NUM_ROWS 803
#define NUMBER 804
#define NUMBERS 805
#define NUMERIC 806
#define NUMERIC_EDITED 807
#define NUMVALC_FUNC 808
#define OBJECT 809
#define OBJECT_COMPUTER 810
#define OCCURS 811
#define OF 812
#define OFF 813
#define OK_BUTTON 814
#define OMITTED 815
#define ON 816
#define ONLY 817
#define OPEN 818
#define OPTIONAL 819
#define OPTIONS 820
#define OR 821
#define ORDER 822
#define ORGANIZATION 823
#define OTHER 824
#define OTHERS 825
#define OUTPUT 826
#define OVERLAP_LEFT 827
#define OVERLAP_TOP 828
#define OVERLINE 829
#define PACKED_DECIMAL 830
#define PADDING 831
#define PASCAL 832
#define PAGE 833
#define PAGE_COUNTER 834
#define PAGE_SETUP 835
#define PAGED 836
#define PARAGRAPH 837
#define PARENT 838
#define PARSE 839
#define PASSWORD 840
#define PERFORM 841
#define PERMANENT 842
#define PH 843
#define PF 844
#define PHYSICAL 845
#define PICTURE 846
#define PICTURE_SYMBOL 847
#define PIXEL 848
#define PLACEMENT 849
#define PLUS 850
#define POINTER 851
#define POP_UP 852
#define POS 853
#define POSITION 854
#define POSITION_SHIFT 855
#define POSITIVE 856
#define PRESENT 857
#define PREVIOUS 858
#define PRINT 859
#define PRINT_CONTROL 860
#define PRINT_NO_PROMPT 861
#define PRINT_PREVIEW 862
#define PRINTER 863
#define PRINTER_1 864
#define PRINTING 865
#define PRIORITY 866
#define PROCEDURE 867
#define PROCEDURES 868
#define PROCEED 869
#define PROCESSING 870
#define PROGRAM 871
#define PROGRAM_ID 872
#define PROGRAM_NAME 873
#define PROGRAM_POINTER 874
#define PROGRESS 875
#define PROHIBITED 876
#define PROMPT 877
#define PROPERTIES 878
#define PROPERTY 879
#define PROTECTED 880
#define PURGE 881
#define PUSH_BUTTON 882
#define QUERY_INDEX 883
#define QUEUE 884
#define QUOTE 885
#define RADIO_BUTTON 886
#define RAISE 887
#define RAISED 888
#define RANDOM 889
#define RD 890
#define READ 891
#define READERS 892
#define READ_ONLY 893
#define READY_TRACE 894
#define RECEIVE 895
#define RECORD 896
#define RECORD_DATA 897
#define RECORD_OVERFLOW 898
#define RECORD_TO_ADD 899
#define RECORD_TO_DELETE 900
#define RECORDING 901
#define RECORDS 902
#define RECURSIVE 903
#define REDEFINES 904
#define REEL 905
#define REFERENCE 906
#define REFERENCES 907
#define REFRESH 908
#define REGION_COLOR 909
#define RELATIVE 910
#define RELEASE 911
#define REMAINDER 912
#define REMOVAL 913
#define RENAMES 914
#define REORG_CRITERIA 915
#define REPLACE 916
#define REPLACING 917
#define REPORT 918
#define REPORTING 919
#define REPORTS 920
#define REPOSITORY 921
#define REQUIRED 922
#define REREAD 923
#define RERUN 924
#define RESERVE 925
#define RESET 926
#define RESET_TRACE 927
#define RESET_GRID 928
#define RESET_LIST 929
#define RESET_TABS 930
#define RETRY 931
#define RETURN 932
#define RETURNING 933
#define REVERSE 934
#define REVERSE_FUNC 935
#define REVERSE_VIDEO 936
#define REVERSED 937
#define REWIND 938
#define REWRITE 939
#define RF 940
#define RH 941
#define RIGHT 942
#define RIGHT_ALIGN 943
#define RIMMED 944
#define ROLLBACK 945
#define ROUNDED 946
#define ROUNDING 947
#define ROW_COLOR 948
#define ROW_COLOR_PATTERN 949
#define ROW_DIVIDERS 950
#define ROW_FONT 951
#define ROW_HEADINGS 952
#define ROW_PROTECTION 953
#define RUN 954
#define S 955
#define SAME 956
#define SAVE_AS 957
#define SAVE_AS_NO_PROMPT 958
#define SCREEN 959
#define SCREEN_CONTROL 960
#define SCROLL 961
#define SCROLL_BAR 962
#define SD 963
#define SEARCH 964
#define SEARCH_OPTIONS 965
#define SEARCH_TEXT 966
#define SECONDS 967
#define SECTION 968
#define SECURE 969
#define SEGMENT 970
#define SEGMENT_LIMIT 971
#define SELECT 972
#define SELECTION_INDEX 973
#define SELECTION_TEXT 974
#define SELECT_ALL 975
#define SELF_ACT 976
#define SEMI_COLON 977
#define SEND 978
#define SENTENCE 979
#define SEPARATE 980
#define SEPARATION 981
#define SEQUENCE 982
#define SEQUENTIAL 983
#define SET 984
#define SEVENTY_EIGHT 985
#define SHADING 986
#define SHADOW 987
#define SHARING 988
#define SHORT_DATE 989
#define SHOW_LINES 990
#define SHOW_NONE 991
#define SHOW_SEL_ALWAYS 992
#define SIGN 993
#define SIGNED 994
#define SIGNED_INT 995
#define SIGNED_LONG 996
#define SIGNED_SHORT 997
#define SIXTY_SIX 998
#define SIZE 999
#define SIZE_ERROR 1000
#define SMALL_FONT 1001
#define SORT 1002
#define SORT_MERGE 1003
#define SORT_ORDER 1004
#define SOURCE 1005
#define SOURCE_COMPUTER 1006
#define SPACE 1007
#define SPECIAL_NAMES 1008
#define SPINNER 1009
#define SQUARE 1010
#define STANDARD 1011
#define STANDARD_1 1012
#define STANDARD_2 1013
#define STANDARD_BINARY 1014
#define STANDARD_DECIMAL 1015
#define START 1016
#define START_X 1017
#define START_Y 1018
#define STATIC 1019
#define STATIC_LIST 1020
#define STATUS 1021
#define STATUS_BAR 1022
#define STATUS_TEXT 1023
#define STDCALL 1024
#define STEP 1025
#define STOP 1026
#define STRING 1027
#define STRONG 1028
#define STYLE 1029
#define SUB_QUEUE_1 1030
#define SUB_QUEUE_2 1031
#define SUB_QUEUE_3 1032
#define SUBSTITUTE_FUNC 1033
#define SUBSTITUTE_CASE_FUNC 1034
#define SUBTRACT 1035
#define SUBWINDOW 1036
#define SUM 1037
#define SUPPRESS 1038
#define SUPPRESS_XML 1039
#define SYMBOLIC 1040
#define SYNCHRONIZED 1041
#define SYSTEM_DEFAULT 1042
#define SYSTEM_INFO 1043
#define SYSTEM_OFFSET 1044
#define TAB 1045
#define TAB_TO_ADD 1046
#define TAB_TO_DELETE 1047
#define TABLE 1048
#define TALLYING 1049
#define TEMPORARY 1050
#define TAPE 1051
#define TERMINAL 1052
#define TERMINATE 1053
#define TERMINAL_INFO 1054
#define TERMINATION_VALUE 1055
#define TEST 1056
#define TEXT 1057
#define THAN 1058
#define THEN 1059
#define THREAD 1060
#define THREADS 1061
#define THRU 1062
#define THUMB_POSITION 1063
#define TILED_HEADINGS 1064
#define TIME 1065
#define TIME_OUT 1066
#define TIMES 1067
#define TITLE 1068
#define TITLE_POSITION 1069
#define TO 1070
#define TOK_AMPER 1071
#define TOK_CLOSE_PAREN 1072
#define TOK_COLON 1073
#define TOK_DIV 1074
#define TOK_DOT 1075
#define TOK_EQUAL 1076
#define TOK_EXTERN 1077
#define TOK_FALSE 1078
#define TOK_FILE 1079
#define TOK_GREATER 1080
#define TOK_INITIAL 1081
#define TOK_LESS 1082
#define TOK_MINUS 1083
#define TOK_MUL 1084
#define TOK_NULL 1085
#define TOK_OVERFLOW 1086
#define TOK_OPEN_PAREN 1087
#define TOK_PLUS 1088
#define TOK_TRUE 1089
#define TOP 1090
#define TOWARD_GREATER 1091
#define TOWARD_LESSER 1092
#define TRACK 1093
#define TRACKS 1094
#define TRACK_AREA 1095
#define TRACK_LIMIT 1096
#define TRADITIONAL_FONT 1097
#define TRAILING 1098
#define TRAILING_SHIFT 1099
#define TRANSFORM 1100
#define TRANSPARENT 1101
#define TREE_VIEW 1102
#define TRIM_FUNC 1103
#define TRUNCATION 1104
#define TYPE 1105
#define TYPEDEF 1106
#define U 1107
#define UCS_4 1108
#define UNBOUNDED 1109
#define UNDERLINE 1110
#define UNFRAMED 1111
#define UNIT 1112
#define UNLOCK 1113
#define UNSIGNED 1114
#define UNSIGNED_INT 1115
#define UNSIGNED_LONG 1116
#define UNSIGNED_SHORT 1117
#define UNSORTED 1118
#define UNSTRING 1119
#define UNTIL 1120
#define UP 1121
#define UPDATE 1122
#define UPDATERS 1123
#define UPON 1124
#define UPON_ARGUMENT_NUMBER 1125
#define UPON_COMMAND_LINE 1126
#define UPON_ENVIRONMENT_NAME 1127
#define UPON_ENVIRONMENT_VALUE 1128
#define UPPER 1129
#define UPPER_CASE_FUNC 1130
#define USAGE 1131
#define USE 1132
#define USE_ALT 1133
#define USE_RETURN 1134
#define USE_TAB 1135
#define USER 1136
#define USER_DEFAULT 1137
#define USER_FUNCTION_NAME 1138
#define USING 1139
#define UTF_8 1140
#define UTF_16 1141
#define V 1142
#define VALIDATE 1143
#define VALIDATING 1144
#define VALUE 1145
#define VALUE_FORMAT 1146
#define VARIABLE 1147
#define VARIANT 1148
#define VARYING 1149
#define VERTICAL 1150
#define VERY_HEAVY 1151
#define VIRTUAL_WIDTH 1152
#define VOLATILE 1153
#define VPADDING 1154
#define VSCROLL 1155
#define VSCROLL_BAR 1156
#define VSCROLL_POS 1157
#define VTOP 1158
#define WAIT 1159
#define WEB_BROWSER 1160
#define WHEN 1161
#define WHEN_COMPILED_FUNC 1162
#define WHEN_XML 1163
#define WIDTH 1164
#define WIDTH_IN_CELLS 1165
#define WINDOW 1166
#define WITH 1167
#define WORD 1168
#define WORDS 1169
#define WORKING_STORAGE 1170
#define WRAP 1171
#define WRITE 1172
#define WRITE_ONLY 1173
#define WRITE_VERIFY 1174
#define WRITERS 1175
#define X 1176
#define XML 1177
#define XML_DECLARATION 1178
#define Y 1179
#define YYYYDDD 1180
#define YYYYMMDD 1181
#define ZERO 1182
#define SHIFT_PREFER 1183

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef int YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (void);

#endif /* !YY_YY_PARSER_H_INCLUDED  */



#ifdef short
# undef short
#endif

/* On compilers that do not define __PTRDIFF_MAX__ etc., make sure
   <limits.h> and (if available) <stdint.h> are included
   so that the code can choose integer types of a good width.  */

#ifndef __PTRDIFF_MAX__
# include <limits.h> /* INFRINGES ON USER NAME SPACE */
# if defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stdint.h> /* INFRINGES ON USER NAME SPACE */
#  define YY_STDINT_H
# endif
#endif

/* Narrow types that promote to a signed type and that can represent a
   signed or unsigned integer of at least N bits.  In tables they can
   save space and decrease cache pressure.  Promoting to a signed type
   helps avoid bugs in integer arithmetic.  */

#ifdef __INT_LEAST8_MAX__
typedef __INT_LEAST8_TYPE__ yytype_int8;
#elif defined YY_STDINT_H
typedef int_least8_t yytype_int8;
#else
typedef signed char yytype_int8;
#endif

#ifdef __INT_LEAST16_MAX__
typedef __INT_LEAST16_TYPE__ yytype_int16;
#elif defined YY_STDINT_H
typedef int_least16_t yytype_int16;
#else
typedef short yytype_int16;
#endif

#if defined __UINT_LEAST8_MAX__ && __UINT_LEAST8_MAX__ <= __INT_MAX__
typedef __UINT_LEAST8_TYPE__ yytype_uint8;
#elif (!defined __UINT_LEAST8_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST8_MAX <= INT_MAX)
typedef uint_least8_t yytype_uint8;
#elif !defined __UINT_LEAST8_MAX__ && UCHAR_MAX <= INT_MAX
typedef unsigned char yytype_uint8;
#else
typedef short yytype_uint8;
#endif

#if defined __UINT_LEAST16_MAX__ && __UINT_LEAST16_MAX__ <= __INT_MAX__
typedef __UINT_LEAST16_TYPE__ yytype_uint16;
#elif (!defined __UINT_LEAST16_MAX__ && defined YY_STDINT_H \
       && UINT_LEAST16_MAX <= INT_MAX)
typedef uint_least16_t yytype_uint16;
#elif !defined __UINT_LEAST16_MAX__ && USHRT_MAX <= INT_MAX
typedef unsigned short yytype_uint16;
#else
typedef int yytype_uint16;
#endif

#ifndef YYPTRDIFF_T
# if defined __PTRDIFF_TYPE__ && defined __PTRDIFF_MAX__
#  define YYPTRDIFF_T __PTRDIFF_TYPE__
#  define YYPTRDIFF_MAXIMUM __PTRDIFF_MAX__
# elif defined PTRDIFF_MAX
#  ifndef ptrdiff_t
#   include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  endif
#  define YYPTRDIFF_T ptrdiff_t
#  define YYPTRDIFF_MAXIMUM PTRDIFF_MAX
# else
#  define YYPTRDIFF_T long
#  define YYPTRDIFF_MAXIMUM LONG_MAX
# endif
#endif

#ifndef YYSIZE_T
# ifdef __SIZE_TYPE__
#  define YYSIZE_T __SIZE_TYPE__
# elif defined size_t
#  define YYSIZE_T size_t
# elif defined __STDC_VERSION__ && 199901 <= __STDC_VERSION__
#  include <stddef.h> /* INFRINGES ON USER NAME SPACE */
#  define YYSIZE_T size_t
# else
#  define YYSIZE_T unsigned
# endif
#endif

#define YYSIZE_MAXIMUM                                  \
  YY_CAST (YYPTRDIFF_T,                                 \
           (YYPTRDIFF_MAXIMUM < YY_CAST (YYSIZE_T, -1)  \
            ? YYPTRDIFF_MAXIMUM                         \
            : YY_CAST (YYSIZE_T, -1)))

#define YYSIZEOF(X) YY_CAST (YYPTRDIFF_T, sizeof (X))

/* Stored state numbers (used for stacks). */
typedef yytype_int16 yy_state_t;

/* State numbers in computations.  */
typedef int yy_state_fast_t;

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

#ifndef YY_ATTRIBUTE_PURE
# if defined __GNUC__ && 2 < __GNUC__ + (96 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_PURE __attribute__ ((__pure__))
# else
#  define YY_ATTRIBUTE_PURE
# endif
#endif

#ifndef YY_ATTRIBUTE_UNUSED
# if defined __GNUC__ && 2 < __GNUC__ + (7 <= __GNUC_MINOR__)
#  define YY_ATTRIBUTE_UNUSED __attribute__ ((__unused__))
# else
#  define YY_ATTRIBUTE_UNUSED
# endif
#endif

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
/* Suppress an incorrect diagnostic about yylval being uninitialized.  */
# define YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN                            \
    _Pragma ("GCC diagnostic push")                                     \
    _Pragma ("GCC diagnostic ignored \"-Wuninitialized\"")              \
    _Pragma ("GCC diagnostic ignored \"-Wmaybe-uninitialized\"")
# define YY_IGNORE_MAYBE_UNINITIALIZED_END      \
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

#if defined __cplusplus && defined __GNUC__ && ! defined __ICC && 6 <= __GNUC__
# define YY_IGNORE_USELESS_CAST_BEGIN                          \
    _Pragma ("GCC diagnostic push")                            \
    _Pragma ("GCC diagnostic ignored \"-Wuseless-cast\"")
# define YY_IGNORE_USELESS_CAST_END            \
    _Pragma ("GCC diagnostic pop")
#endif
#ifndef YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_BEGIN
# define YY_IGNORE_USELESS_CAST_END
#endif


#define YY_ASSERT(E) ((void) (0 && (E)))

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
  yy_state_t yyss_alloc;
  YYSTYPE yyvs_alloc;
};

/* The size of the maximum gap between one aligned stack and the next.  */
# define YYSTACK_GAP_MAXIMUM (YYSIZEOF (union yyalloc) - 1)

/* The size of an array large to enough to hold all stacks, each with
   N elements.  */
# define YYSTACK_BYTES(N) \
     ((N) * (YYSIZEOF (yy_state_t) + YYSIZEOF (YYSTYPE)) \
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
        YYPTRDIFF_T yynewbytes;                                         \
        YYCOPY (&yyptr->Stack_alloc, Stack, yysize);                    \
        Stack = &yyptr->Stack_alloc;                                    \
        yynewbytes = yystacksize * YYSIZEOF (*Stack) + YYSTACK_GAP_MAXIMUM; \
        yyptr += yynewbytes / YYSIZEOF (*yyptr);                        \
      }                                                                 \
    while (0)

#endif

#if defined YYCOPY_NEEDED && YYCOPY_NEEDED
/* Copy COUNT objects from SRC to DST.  The source and destination do
   not overlap.  */
# ifndef YYCOPY
#  if defined __GNUC__ && 1 < __GNUC__
#   define YYCOPY(Dst, Src, Count) \
      __builtin_memcpy (Dst, Src, YY_CAST (YYSIZE_T, (Count)) * sizeof (*(Src)))
#  else
#   define YYCOPY(Dst, Src, Count)              \
      do                                        \
        {                                       \
          YYPTRDIFF_T yyi;                      \
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
#define YYLAST   18113

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  929
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  1321
/* YYNRULES -- Number of rules.  */
#define YYNRULES  3192
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  4515

#define YYUNDEFTOK  2
#define YYMAXUTOK   1183


/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                                \
  (0 <= (YYX) && (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_int16 yytranslate[] =
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
     855,   856,   857,   858,   859,   860,   861,   862,   863,   864,
     865,   866,   867,   868,   869,   870,   871,   872,   873,   874,
     875,   876,   877,   878,   879,   880,   881,   882,   883,   884,
     885,   886,   887,   888,   889,   890,   891,   892,   893,   894,
     895,   896,   897,   898,   899,   900,   901,   902,   903,   904,
     905,   906,   907,   908,   909,   910,   911,   912,   913,   914,
     915,   916,   917,   918,   919,   920,   921,   922,   923,   924,
     925,   926,   927,   928
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_int16 yyrline[] =
{
       0,  3311,  3311,  3311,  3345,  3346,  3350,  3350,  3359,  3360,
    3364,  3365,  3369,  3369,  3381,  3392,  3400,  3404,  3408,  3409,
    3414,  3413,  3426,  3425,  3440,  3444,  3438,  3456,  3457,  3461,
    3470,  3470,  3475,  3479,  3474,  3495,  3494,  3510,  3521,  3528,
    3529,  3536,  3537,  3540,  3541,  3545,  3554,  3563,  3564,  3571,
    3572,  3576,  3580,  3586,  3588,  3596,  3603,  3605,  3609,  3616,
    3620,  3624,  3640,  3643,  3653,  3655,  3662,  3666,  3670,  3676,
    3678,  3685,  3689,  3693,  3697,  3706,  3711,  3712,  3721,  3725,
    3726,  3736,  3738,  3742,  3743,  3747,  3748,  3749,  3750,  3751,
    3758,  3757,  3768,  3769,  3772,  3773,  3786,  3785,  3799,  3800,
    3801,  3802,  3806,  3807,  3811,  3812,  3813,  3814,  3818,  3826,
    3835,  3834,  3842,  3846,  3852,  3856,  3861,  3868,  3878,  3892,
    3903,  3907,  3911,  3915,  3922,  3923,  3930,  3929,  3942,  3944,
    3945,  3952,  3953,  3957,  3961,  3967,  3968,  3975,  3982,  3987,
    3998,  4012,  4015,  4016,  4019,  4023,  4024,  4025,  4026,  4027,
    4028,  4029,  4030,  4031,  4032,  4033,  4034,  4035,  4036,  4044,
    4043,  4062,  4073,  4094,  4102,  4105,  4106,  4110,  4117,  4132,
    4153,  4152,  4177,  4176,  4185,  4184,  4194,  4196,  4200,  4204,
    4205,  4211,  4217,  4223,  4232,  4233,  4240,  4247,  4257,  4263,
    4271,  4281,  4285,  4292,  4296,  4301,  4300,  4311,  4315,  4322,
    4323,  4324,  4325,  4326,  4327,  4331,  4332,  4339,  4354,  4357,
    4364,  4372,  4376,  4387,  4407,  4415,  4426,  4427,  4434,  4448,
    4449,  4453,  4474,  4495,  4496,  4500,  4504,  4522,  4524,  4528,
    4535,  4537,  4547,  4568,  4635,  4638,  4647,  4666,  4682,  4700,
    4718,  4735,  4753,  4752,  4780,  4786,  4787,  4796,  4797,  4805,
    4806,  4811,  4810,  4857,  4858,  4864,  4865,  4874,  4875,  4876,
    4877,  4878,  4879,  4880,  4881,  4882,  4883,  4884,  4885,  4886,
    4887,  4888,  4889,  4890,  4891,  4892,  4918,  4928,  4938,  4949,
    4960,  4991,  4994,  4998,  5002,  5006,  5011,  5015,  5023,  5027,
    5031,  5039,  5040,  5041,  5042,  5046,  5047,  5048,  5049,  5050,
    5051,  5052,  5056,  5064,  5068,  5076,  5080,  5087,  5088,  5094,
    5101,  5102,  5103,  5110,  5165,  5168,  5173,  5172,  5198,  5201,
    5205,  5215,  5226,  5225,  5233,  5237,  5243,  5247,  5252,  5259,
    5269,  5280,  5295,  5306,  5308,  5309,  5315,  5315,  5322,  5326,
    5330,  5337,  5338,  5339,  5343,  5349,  5350,  5354,  5360,  5361,
    5377,  5378,  5382,  5388,  5394,  5400,  5413,  5424,  5423,  5432,
    5443,  5457,  5470,  5486,  5525,  5528,  5535,  5536,  5540,  5540,
    5544,  5549,  5567,  5578,  5585,  5586,  5592,  5605,  5606,  5607,
    5613,  5621,  5622,  5628,  5638,  5648,  5658,  5668,  5669,  5676,
    5684,  5685,  5686,  5693,  5694,  5698,  5699,  5700,  5701,  5707,
    5735,  5736,  5737,  5738,  5744,  5749,  5753,  5757,  5758,  5765,
    5766,  5767,  5768,  5769,  5770,  5771,  5772,  5779,  5778,  5794,
    5795,  5799,  5802,  5803,  5809,  5813,  5817,  5818,  5827,  5824,
    5838,  5839,  5843,  5851,  5852,  5860,  5861,  5865,  5885,  5884,
    5908,  5915,  5919,  5925,  5926,  5930,  5940,  5955,  5956,  5957,
    5958,  5959,  5960,  5961,  5962,  5963,  5970,  5977,  5977,  5977,
    5983,  5992,  6001,  6011,  6012,  6019,  6020,  6024,  6025,  6032,
    6043,  6048,  6059,  6060,  6064,  6065,  6071,  6082,  6100,  6101,
    6105,  6106,  6107,  6111,  6118,  6125,  6134,  6143,  6144,  6145,
    6146,  6147,  6156,  6157,  6163,  6200,  6201,  6211,  6226,  6227,
    6231,  6245,  6263,  6265,  6264,  6282,  6283,  6287,  6304,  6303,
    6324,  6325,  6329,  6330,  6331,  6334,  6336,  6337,  6341,  6342,
    6346,  6347,  6348,  6349,  6350,  6351,  6352,  6353,  6354,  6355,
    6356,  6360,  6364,  6366,  6370,  6371,  6375,  6376,  6377,  6378,
    6379,  6380,  6381,  6384,  6386,  6387,  6391,  6392,  6396,  6397,
    6398,  6399,  6400,  6401,  6405,  6410,  6412,  6411,  6427,  6431,
    6431,  6444,  6445,  6449,  6450,  6451,  6453,  6452,  6472,  6489,
    6495,  6497,  6501,  6508,  6512,  6523,  6526,  6538,  6539,  6541,
    6545,  6549,  6555,  6559,  6563,  6567,  6571,  6575,  6579,  6587,
    6591,  6595,  6599,  6603,  6607,  6618,  6619,  6623,  6624,  6628,
    6629,  6630,  6634,  6635,  6639,  6683,  6686,  6694,  6693,  6706,
    6734,  6733,  6748,  6752,  6759,  6765,  6769,  6776,  6777,  6781,
    6782,  6783,  6784,  6785,  6786,  6787,  6788,  6789,  6790,  6793,
    6795,  6799,  6803,  6807,  6808,  6809,  6810,  6811,  6812,  6813,
    6814,  6815,  6816,  6817,  6818,  6819,  6820,  6821,  6822,  6823,
    6824,  6825,  6826,  6827,  6834,  6855,  6869,  6870,  6873,  6882,
    6900,  6924,  6925,  6935,  6963,  6966,  6974,  6975,  6979,  7004,
    7003,  7015,  7023,  7040,  7052,  7069,  7086,  7107,  7108,  7115,
    7117,  7125,  7140,  7151,  7152,  7153,  7184,  7191,  7195,  7200,
    7204,  7209,  7218,  7222,  7226,  7230,  7234,  7238,  7242,  7246,
    7250,  7254,  7258,  7262,  7267,  7272,  7276,  7280,  7284,  7289,
    7293,  7298,  7302,  7307,  7312,  7317,  7321,  7325,  7333,  7337,
    7341,  7349,  7353,  7357,  7361,  7365,  7369,  7373,  7377,  7381,
    7389,  7397,  7401,  7405,  7409,  7413,  7417,  7425,  7426,  7429,
    7431,  7432,  7433,  7434,  7435,  7436,  7439,  7441,  7447,  7454,
    7467,  7476,  7477,  7486,  7493,  7505,  7523,  7524,  7528,  7529,
    7533,  7534,  7537,  7538,  7543,  7544,  7551,  7552,  7558,  7560,
    7562,  7561,  7570,  7571,  7575,  7597,  7598,  7602,  7627,  7628,
    7631,  7633,  7636,  7643,  7644,  7649,  7660,  7671,  7680,  7682,
    7683,  7693,  7704,  7731,  7730,  7739,  7740,  7744,  7745,  7748,
    7750,  7762,  7771,  7786,  7809,  7828,  7830,  7829,  7851,  7853,
    7852,  7868,  7870,  7869,  7880,  7881,  7888,  7887,  7913,  7914,
    7915,  7922,  7928,  7933,  7934,  7940,  7947,  7948,  7949,  7953,
    7960,  7961,  7965,  7975,  8014,  8025,  8026,  8040,  8053,  8054,
    8057,  8058,  8063,  8064,  8065,  8066,  8067,  8068,  8080,  8094,
    8108,  8122,  8136,  8149,  8150,  8155,  8154,  8164,  8176,  8177,
    8181,  8182,  8183,  8184,  8185,  8186,  8187,  8188,  8189,  8190,
    8191,  8192,  8193,  8194,  8195,  8196,  8197,  8201,  8208,  8212,
    8216,  8217,  8218,  8225,  8229,  8237,  8240,  8248,  8258,  8259,
    8264,  8267,  8272,  8276,  8284,  8291,  8300,  8305,  8312,  8313,
    8314,  8318,  8326,  8327,  8328,  8335,  8339,  8346,  8351,  8357,
    8364,  8370,  8380,  8384,  8391,  8393,  8397,  8401,  8405,  8409,
    8416,  8424,  8425,  8428,  8430,  8434,  8438,  8452,  8467,  8470,
    8472,  8476,  8480,  8484,  8491,  8512,  8516,  8517,  8521,  8542,
    8550,  8559,  8561,  8560,  8579,  8580,  8584,  8585,  8589,  8592,
    8591,  8642,  8654,  8641,  8698,  8718,  8720,  8724,  8729,  8734,
    8738,  8742,  8747,  8752,  8757,  8762,  8771,  8775,  8779,  8783,
    8787,  8793,  8797,  8802,  8808,  8812,  8817,  8822,  8827,  8832,
    8837,  8842,  8851,  8855,  8859,  8865,  8869,  8873,  8877,  8881,
    8885,  8889,  8893,  8904,  8909,  8914,  8918,  8919,  8920,  8921,
    8922,  8923,  8924,  8925,  8934,  8939,  8950,  8951,  8958,  8959,
    8960,  8961,  8962,  8963,  8964,  8965,  8966,  8969,  8972,  8973,
    8974,  8975,  8976,  8977,  8984,  8985,  8990,  8991,  8994,  8996,
    9000,  9001,  9005,  9006,  9010,  9011,  9015,  9016,  9020,  9021,
    9022,  9023,  9024,  9027,  9028,  9029,  9030,  9031,  9033,  9034,
    9036,  9037,  9041,  9042,  9043,  9044,  9046,  9048,  9050,  9051,
    9052,  9053,  9054,  9055,  9056,  9057,  9058,  9064,  9065,  9066,
    9067,  9068,  9069,  9070,  9071,  9072,  9073,  9077,  9078,  9083,
    9084,  9085,  9086,  9087,  9091,  9099,  9100,  9101,  9102,  9103,
    9104,  9105,  9106,  9107,  9108,  9109,  9111,  9113,  9114,  9115,
    9119,  9120,  9121,  9122,  9123,  9124,  9125,  9126,  9127,  9128,
    9133,  9134,  9135,  9136,  9137,  9138,  9139,  9140,  9141,  9142,
    9147,  9148,  9159,  9160,  9184,  9185,  9202,  9205,  9206,  9207,
    9210,  9214,  9215,  9216,  9217,  9218,  9219,  9220,  9221,  9222,
    9223,  9224,  9225,  9226,  9227,  9228,  9229,  9235,  9236,  9237,
    9257,  9258,  9259,  9260,  9261,  9262,  9263,  9264,  9268,  9269,
    9270,  9271,  9272,  9273,  9279,  9280,  9281,  9282,  9283,  9284,
    9285,  9286,  9291,  9293,  9294,  9295,  9300,  9301,  9302,  9306,
    9307,  9308,  9309,  9310,  9311,  9322,  9323,  9324,  9325,  9330,
    9333,  9334,  9335,  9336,  9337,  9339,  9344,  9345,  9346,  9352,
    9353,  9354,  9355,  9356,  9357,  9358,  9359,  9360,  9361,  9365,
    9366,  9367,  9368,  9369,  9370,  9371,  9372,  9373,  9374,  9375,
    9376,  9377,  9378,  9380,  9381,  9382,  9383,  9384,  9385,  9386,
    9387,  9388,  9389,  9390,  9391,  9392,  9393,  9394,  9395,  9396,
    9399,  9400,  9401,  9409,  9410,  9411,  9415,  9416,  9417,  9421,
    9422,  9425,  9426,  9427,  9430,  9439,  9440,  9441,  9442,  9443,
    9444,  9445,  9446,  9447,  9448,  9449,  9450,  9451,  9453,  9454,
    9455,  9456,  9457,  9458,  9459,  9460,  9461,  9462,  9469,  9473,
    9477,  9478,  9479,  9480,  9481,  9482,  9483,  9484,  9490,  9491,
    9492,  9497,  9498,  9503,  9508,  9509,  9513,  9514,  9519,  9520,
    9524,  9525,  9526,  9531,  9532,  9536,  9537,  9541,  9542,  9546,
    9550,  9550,  9554,  9558,  9558,  9562,  9566,  9567,  9571,  9572,
    9576,  9584,  9586,  9590,  9597,  9607,  9610,  9614,  9621,  9633,
    9643,  9652,  9657,  9667,  9689,  9656,  9717,  9717,  9751,  9755,
    9754,  9768,  9767,  9787,  9788,  9793,  9815,  9817,  9821,  9832,
    9834,  9842,  9850,  9858,  9864,  9868,  9904,  9906,  9914,  9917,
    9930,  9935,  9945,  9980,  9982,  9981, 10018, 10019, 10023, 10024,
   10025, 10043, 10044, 10056, 10055, 10101, 10102, 10106, 10151, 10171,
   10174, 10203, 10209, 10202, 10225, 10225, 10265, 10273, 10274, 10275,
   10276, 10277, 10278, 10279, 10280, 10281, 10282, 10283, 10284, 10285,
   10286, 10287, 10288, 10289, 10290, 10291, 10292, 10293, 10294, 10295,
   10296, 10297, 10298, 10299, 10300, 10301, 10303, 10304, 10305, 10306,
   10307, 10308, 10309, 10310, 10311, 10312, 10313, 10314, 10315, 10316,
   10317, 10319, 10320, 10321, 10322, 10323, 10324, 10325, 10326, 10327,
   10328, 10329, 10330, 10331, 10332, 10333, 10334, 10335, 10336, 10337,
   10338, 10339, 10354, 10366, 10365, 10376, 10375, 10410, 10409, 10420,
   10424, 10428, 10434, 10440, 10445, 10450, 10455, 10460, 10466, 10472,
   10476, 10482, 10486, 10491, 10495, 10499, 10503, 10507, 10511, 10515,
   10519, 10533, 10540, 10541, 10548, 10548, 10559, 10560, 10564, 10568,
   10572, 10579, 10583, 10587, 10594, 10595, 10599, 10601, 10605, 10606,
   10610, 10611, 10615, 10619, 10620, 10629, 10630, 10635, 10636, 10640,
   10641, 10645, 10661, 10677, 10690, 10698, 10706, 10713, 10719, 10725,
   10730, 10736, 10741, 10746, 10759, 10764, 10769, 10775, 10781, 10787,
   10794, 10798, 10802, 10806, 10810, 10821, 10826, 10832, 10837, 10842,
   10847, 10853, 10859, 10864, 10870, 10876, 10882, 10889, 10894, 10899,
   10906, 10913, 10919, 10922, 10922, 10926, 10937, 10938, 10939, 10943,
   10944, 10945, 10949, 10950, 10954, 10958, 10977, 10976, 10985, 10989,
   10996, 11000, 11008, 11009, 11013, 11017, 11028, 11027, 11037, 11041,
   11052, 11054, 11067, 11068, 11076, 11075, 11084, 11085, 11089, 11095,
   11095, 11102, 11101, 11118, 11117, 11186, 11190, 11189, 11205, 11209,
   11213, 11221, 11224, 11232, 11240, 11244, 11248, 11252, 11256, 11275,
   11281, 11301, 11305, 11315, 11319, 11324, 11328, 11327, 11344, 11345,
   11350, 11358, 11392, 11394, 11398, 11407, 11420, 11423, 11427, 11431,
   11436, 11459, 11460, 11464, 11465, 11469, 11473, 11477, 11488, 11492,
   11499, 11503, 11511, 11515, 11522, 11529, 11533, 11544, 11543, 11555,
   11559, 11566, 11567, 11577, 11576, 11584, 11585, 11589, 11596, 11604,
   11605, 11606, 11607, 11608, 11613, 11612, 11624, 11625, 11633, 11632,
   11641, 11648, 11652, 11662, 11674, 11673, 11694, 11695, 11695, 11710,
   11709, 11718, 11725, 11736, 11735, 11744, 11748, 11752, 11759, 11767,
   11771, 11782, 11781, 11790, 11793, 11795, 11801, 11803, 11804, 11805,
   11806, 11814, 11813, 11825, 11829, 11833, 11837, 11841, 11842, 11843,
   11844, 11845, 11846, 11847, 11851, 11859, 11868, 11869, 11874, 11873,
   11917, 11921, 11927, 11929, 11933, 11934, 11938, 11939, 11943, 11947,
   11952, 11956, 11957, 11962, 11965, 11969, 11973, 11977, 11981, 11988,
   11989, 11994, 11993, 12010, 12017, 12017, 12029, 12033, 12041, 12042,
   12043, 12054, 12053, 12071, 12073, 12077, 12078, 12082, 12086, 12087,
   12088, 12089, 12094, 12099, 12093, 12113, 12114, 12119, 12124, 12118,
   12143, 12142, 12164, 12165, 12166, 12170, 12171, 12176, 12179, 12186,
   12199, 12211, 12218, 12219, 12225, 12226, 12230, 12231, 12232, 12233,
   12234, 12235, 12239, 12242, 12246, 12247, 12248, 12252, 12253, 12254,
   12255, 12259, 12260, 12265, 12266, 12270, 12280, 12296, 12301, 12307,
   12313, 12318, 12323, 12329, 12335, 12341, 12347, 12354, 12358, 12362,
   12366, 12370, 12375, 12380, 12385, 12390, 12396, 12401, 12406, 12413,
   12423, 12427, 12438, 12437, 12446, 12450, 12454, 12458, 12462, 12469,
   12473, 12484, 12483, 12495, 12494, 12502, 12501, 12511, 12535, 12548,
   12547, 12574, 12584, 12585, 12590, 12601, 12612, 12626, 12634, 12642,
   12643, 12648, 12654, 12664, 12676, 12682, 12692, 12705, 12704, 12716,
   12714, 12728, 12729, 12734, 12809, 12810, 12811, 12812, 12816, 12817,
   12821, 12825, 12835, 12834, 12848, 12847, 12888, 12888, 12889, 12889,
   12892, 12901, 12912, 12913, 12921, 12920, 12932, 12936, 12961, 12975,
   12998, 13021, 13042, 13066, 13069, 13077, 13076, 13085, 13096, 13095,
   13104, 13119, 13118, 13131, 13136, 13147, 13151, 13162, 13182, 13181,
   13190, 13194, 13200, 13207, 13210, 13217, 13223, 13229, 13234, 13246,
   13245, 13253, 13261, 13262, 13266, 13267, 13268, 13273, 13276, 13283,
   13287, 13295, 13302, 13303, 13304, 13305, 13306, 13307, 13308, 13320,
   13323, 13333, 13332, 13340, 13349, 13362, 13361, 13373, 13374, 13381,
   13380, 13389, 13393, 13394, 13395, 13399, 13400, 13401, 13402, 13409,
   13408, 13429, 13439, 13447, 13451, 13458, 13463, 13468, 13473, 13478,
   13483, 13491, 13492, 13496, 13501, 13507, 13509, 13510, 13511, 13512,
   13516, 13544, 13547, 13551, 13555, 13559, 13566, 13573, 13583, 13582,
   13595, 13600, 13593, 13612, 13615, 13622, 13623, 13627, 13635, 13639,
   13649, 13648, 13658, 13665, 13667, 13674, 13673, 13686, 13685, 13698,
   13699, 13703, 13707, 13718, 13717, 13725, 13729, 13740, 13739, 13748,
   13752, 13759, 13763, 13774, 13773, 13782, 13783, 13787, 13815, 13816,
   13820, 13821, 13822, 13823, 13827, 13828, 13832, 13833, 13834, 13838,
   13839, 13847, 13848, 13852, 13853, 13859, 13868, 13869, 13870, 13875,
   13876, 13877, 13881, 13888, 13904, 13905, 13906, 13912, 13911, 13923,
   13935, 13932, 13949, 13946, 13962, 13970, 13977, 13981, 13994, 14001,
   14013, 14016, 14021, 14025, 14038, 14045, 14046, 14050, 14051, 14054,
   14055, 14060, 14103, 14107, 14117, 14116, 14129, 14128, 14136, 14141,
   14151, 14166, 14165, 14175, 14204, 14205, 14209, 14213, 14217, 14221,
   14228, 14229, 14233, 14237, 14240, 14242, 14246, 14255, 14256, 14257,
   14260, 14262, 14266, 14270, 14274, 14282, 14283, 14287, 14288, 14292,
   14296, 14306, 14317, 14316, 14325, 14330, 14331, 14335, 14336, 14337,
   14341, 14342, 14346, 14350, 14351, 14355, 14359, 14363, 14373, 14372,
   14380, 14390, 14401, 14400, 14409, 14416, 14420, 14431, 14430, 14442,
   14451, 14454, 14458, 14462, 14469, 14473, 14483, 14495, 14494, 14503,
   14507, 14516, 14517, 14522, 14525, 14533, 14537, 14544, 14552, 14556,
   14567, 14566, 14574, 14577, 14582, 14584, 14588, 14594, 14595, 14596,
   14597, 14600, 14602, 14609, 14608, 14622, 14623, 14624, 14625, 14626,
   14627, 14628, 14629, 14633, 14634, 14638, 14639, 14645, 14654, 14661,
   14662, 14666, 14670, 14674, 14680, 14686, 14690, 14694, 14698, 14707,
   14711, 14715, 14724, 14733, 14734, 14738, 14747, 14748, 14752, 14756,
   14765, 14774, 14786, 14785, 14794, 14793, 14845, 14846, 14863, 14864,
   14867, 14868, 14877, 14880, 14885, 14890, 14900, 14917, 14922, 14932,
   14950, 14949, 14959, 14972, 14975, 14983, 14986, 14991, 14996, 15004,
   15005, 15006, 15007, 15008, 15009, 15013, 15021, 15022, 15026, 15030,
   15041, 15040, 15051, 15059, 15070, 15077, 15081, 15085, 15093, 15105,
   15108, 15115, 15119, 15126, 15127, 15128, 15129, 15136, 15135, 15144,
   15151, 15151, 15161, 15162, 15166, 15180, 15181, 15186, 15187, 15191,
   15192, 15196, 15200, 15211, 15210, 15219, 15223, 15227, 15231, 15239,
   15243, 15253, 15264, 15265, 15272, 15271, 15279, 15288, 15301, 15300,
   15308, 15322, 15321, 15329, 15346, 15345, 15355, 15363, 15364, 15369,
   15370, 15375, 15382, 15383, 15388, 15395, 15396, 15400, 15401, 15405,
   15409, 15419, 15418, 15433, 15438, 15450, 15449, 15458, 15459, 15460,
   15461, 15462, 15466, 15494, 15497, 15509, 15519, 15524, 15529, 15534,
   15542, 15582, 15583, 15587, 15647, 15657, 15680, 15681, 15682, 15683,
   15687, 15696, 15703, 15714, 15740, 15741, 15745, 15751, 15767, 15768,
   15775, 15774, 15786, 15796, 15797, 15802, 15805, 15809, 15813, 15820,
   15821, 15825, 15826, 15827, 15831, 15835, 15845, 15844, 15857, 15868,
   15855, 15879, 15881, 15885, 15886, 15890, 15894, 15906, 15915, 15925,
   15928, 15938, 15941, 15949, 15952, 15961, 15965, 15972, 15980, 15983,
   15990, 15994, 16001, 16005, 16013, 16016, 16025, 16029, 16036, 16044,
   16047, 16051, 16052, 16053, 16056, 16058, 16066, 16067, 16071, 16076,
   16081, 16088, 16093, 16098, 16106, 16110, 16117, 16121, 16132, 16131,
   16148, 16143, 16154, 16156, 16159, 16161, 16164, 16166, 16170, 16171,
   16187, 16188, 16189, 16199, 16203, 16210, 16218, 16219, 16223, 16224,
   16228, 16236, 16237, 16242, 16243, 16244, 16254, 16258, 16265, 16273,
   16274, 16278, 16286, 16287, 16288, 16298, 16302, 16309, 16317, 16318,
   16322, 16330, 16331, 16332, 16342, 16346, 16353, 16361, 16362, 16366,
   16376, 16377, 16378, 16388, 16392, 16399, 16407, 16408, 16412, 16422,
   16423, 16424, 16434, 16438, 16445, 16453, 16454, 16458, 16469, 16470,
   16477, 16479, 16488, 16492, 16499, 16507, 16508, 16512, 16522, 16523,
   16533, 16537, 16544, 16552, 16553, 16557, 16567, 16568, 16572, 16573,
   16583, 16587, 16594, 16602, 16603, 16607, 16618, 16621, 16630, 16633,
   16641, 16645, 16654, 16658, 16665, 16666, 16672, 16677, 16685, 16692,
   16692, 16703, 16704, 16708, 16709, 16711, 16713, 16715, 16716, 16718,
   16719, 16720, 16721, 16722, 16724, 16725, 16726, 16729, 16731, 16735,
   16738, 16740, 16741, 16742, 16743, 16744, 16745, 16747, 16748, 16749,
   16750, 16751, 16754, 16755, 16759, 16760, 16764, 16765, 16769, 16770,
   16774, 16778, 16784, 16788, 16794, 16796, 16797, 16801, 16802, 16803,
   16807, 16808, 16809, 16813, 16817, 16821, 16822, 16823, 16826, 16827,
   16837, 16849, 16858, 16874, 16883, 16899, 16914, 16915, 16920, 16929,
   16935, 16945, 16959, 16979, 16983, 17004, 17008, 17029, 17041, 17055,
   17069, 17070, 17075, 17081, 17082, 17087, 17096, 17098, 17103, 17117,
   17118, 17119, 17126, 17137, 17138, 17142, 17150, 17151, 17155, 17156,
   17160, 17172, 17176, 17183, 17192, 17193, 17199, 17208, 17219, 17236,
   17240, 17247, 17248, 17249, 17256, 17257, 17261, 17265, 17272, 17273,
   17277, 17278, 17282, 17283, 17284, 17285, 17289, 17293, 17297, 17301,
   17305, 17309, 17313, 17334, 17344, 17348, 17355, 17356, 17357, 17361,
   17362, 17363, 17364, 17365, 17369, 17373, 17380, 17381, 17382, 17383,
   17387, 17391, 17398, 17410, 17422, 17436, 17437, 17441, 17442, 17446,
   17453, 17460, 17461, 17468, 17469, 17476, 17477, 17478, 17482, 17483,
   17487, 17491, 17495, 17499, 17500, 17504, 17508, 17509, 17513, 17517,
   17518, 17527, 17531, 17536, 17537, 17543, 17547, 17551, 17555, 17556,
   17562, 17566, 17570, 17571, 17575, 17582, 17592, 17611, 17632, 17651,
   17669, 17676, 17683, 17690, 17700, 17704, 17711, 17715, 17722, 17732,
   17742, 17752, 17765, 17791, 17795, 17803, 17803, 17816, 17821, 17829,
   17837, 17841, 17851, 17866, 17888, 17908, 17912, 17919, 17933, 17934,
   17935, 17936, 17937, 17938, 17942, 17946, 17963, 17967, 17974, 17975,
   17976, 17977, 17978, 17979, 17980, 17984, 17985, 17986, 17987, 17993,
   17997, 18001, 18005, 18009, 18013, 18018, 18022, 18026, 18030, 18034,
   18038, 18042, 18046, 18053, 18054, 18058, 18059, 18060, 18061, 18065,
   18066, 18067, 18068, 18069, 18073, 18077, 18081, 18088, 18092, 18096,
   18103, 18110, 18117, 18127, 18127, 18143, 18150, 18160, 18167, 18177,
   18181, 18194, 18198, 18213, 18221, 18222, 18226, 18227, 18228, 18232,
   18233, 18238, 18241, 18249, 18252, 18259, 18261, 18262, 18266, 18267,
   18271, 18272, 18273, 18278, 18281, 18294, 18298, 18306, 18310, 18314,
   18318, 18322, 18326, 18330, 18334, 18341, 18342, 18346, 18347, 18357,
   18358, 18367, 18371, 18375, 18379, 18386, 18387, 18388, 18389, 18390,
   18391, 18392, 18393, 18394, 18395, 18396, 18397, 18398, 18399, 18400,
   18401, 18402, 18403, 18404, 18405, 18406, 18407, 18408, 18409, 18410,
   18411, 18412, 18413, 18414, 18415, 18416, 18417, 18418, 18419, 18420,
   18421, 18422, 18423, 18424, 18425, 18426, 18427, 18428, 18429, 18430,
   18431, 18432, 18433, 18434, 18435, 18436, 18437, 18441, 18442, 18443,
   18444, 18445, 18446, 18447, 18448, 18449, 18450, 18451, 18452, 18453,
   18454, 18455, 18456, 18457, 18458, 18459, 18460, 18461, 18462, 18463,
   18470, 18470, 18471, 18471, 18472, 18472, 18473, 18473, 18474, 18474,
   18474, 18475, 18475, 18476, 18476, 18477, 18477, 18478, 18478, 18479,
   18479, 18480, 18480, 18481, 18481, 18482, 18482, 18483, 18483, 18484,
   18484, 18485, 18485, 18486, 18486, 18487, 18487, 18488, 18488, 18489,
   18489, 18490, 18490, 18491, 18491, 18492, 18492, 18493, 18493, 18494,
   18494, 18495, 18495, 18495, 18496, 18496, 18496, 18497, 18497, 18498,
   18498, 18499, 18499, 18500, 18500, 18501, 18501, 18502, 18502, 18502,
   18503, 18503, 18503, 18504, 18504, 18504, 18504, 18505, 18505, 18505,
   18506, 18506, 18507, 18507, 18508, 18508, 18508, 18509, 18509, 18509,
   18510, 18510, 18511, 18511, 18512, 18512, 18513, 18513, 18514, 18514,
   18515, 18515, 18516, 18516, 18517, 18517, 18518, 18518, 18519, 18519,
   18519, 18520, 18520, 18520, 18520, 18521, 18521, 18522, 18522, 18523,
   18523, 18524, 18524, 18525, 18525, 18526, 18526, 18527, 18527, 18527,
   18528, 18528, 18529, 18529, 18530, 18530, 18531, 18531, 18531, 18532,
   18532, 18533, 18533, 18534, 18534, 18535, 18535, 18536, 18536, 18537,
   18537, 18538, 18538, 18539, 18539, 18540, 18540, 18541, 18541, 18542,
   18542, 18543, 18543, 18544, 18544, 18545, 18545, 18545, 18549, 18549,
   18550, 18550, 18551, 18551, 18552, 18552, 18552, 18552, 18553, 18553,
   18554, 18554, 18555, 18555, 18556, 18556, 18557, 18557, 18558, 18558,
   18559, 18559, 18560, 18560, 18560, 18561, 18561, 18562, 18562, 18563,
   18563, 18564, 18564, 18565, 18565, 18566, 18566, 18569, 18569, 18570,
   18570, 18571, 18571, 18572, 18572, 18573, 18573, 18574, 18574, 18575,
   18575, 18576, 18576
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "$undefined", "\"3D\"", "ABSENT", "ACCEPT",
  "ACCESS", "\"ACTIVE-X\"", "ACTION", "ACTUAL", "ADD", "ADDRESS",
  "\"ADJUSTABLE-COLUMNS\"", "ADVANCING", "AFTER", "ALIGNMENT", "ALL",
  "ALLOCATE", "ALLOWING", "ALPHABET", "ALPHABETIC", "\"ALPHABETIC-LOWER\"",
  "\"ALPHABETIC-UPPER\"", "ALPHANUMERIC", "\"ALPHANUMERIC-EDITED\"",
  "ALSO", "ALTER", "ALTERNATE", "AND", "ANY", "APPLY", "ARE", "AREA",
  "AREAS", "\"ARGUMENT-NUMBER\"", "\"ARGUMENT-VALUE\"", "ARITHMETIC", "AS",
  "ASCENDING", "ASCII", "ASSIGN", "AT", "ATTRIBUTE", "ATTRIBUTES", "AUTO",
  "\"AUTO-DECIMAL\"", "\"AUTO-SPIN\"", "AUTOMATIC", "\"AWAY-FROM-ZERO\"",
  "\"BACKGROUND-COLOR\"", "\"BACKGROUND-HIGH\"", "\"BACKGROUND-LOW\"",
  "\"BACKGROUND-STANDARD\"", "BAR", "BASED", "BEFORE", "BELL", "BINARY",
  "\"BINARY-C-LONG\"", "\"BINARY-CHAR\"", "\"BINARY-DOUBLE\"",
  "\"BINARY-LONG\"", "\"BINARY-SEQUENTIAL\"", "\"BINARY-SHORT\"", "BIT",
  "BITMAP", "\"BITMAP-END\"", "\"BITMAP-HANDLE\"", "\"BITMAP-NUMBER\"",
  "\"BITMAP-START\"", "\"BITMAP-TIMER\"", "\"BITMAP-TRAILING\"",
  "\"BITMAP-TRANSPARENT-COLOR\"", "\"BITMAP-WIDTH\"", "BLANK", "BLINK",
  "BLOCK", "BOTTOM", "BOX", "BOXED", "\"BULK-ADDITION\"", "BUSY",
  "BUTTONS", "BY", "\"BYTE-LENGTH\"", "C", "\"CALENDAR-FONT\"", "CALL",
  "CANCEL", "\"CANCEL-BUTTON\"", "CAPACITY", "\"CARD-PUNCH\"",
  "\"CARD-READER\"", "CASSETTE", "CCOL", "CD", "CELL", "\"CELL-COLOR\"",
  "\"CELL-DATA\"", "\"CELL-FONT\"", "\"CELL-PROTECTION\"", "CENTER",
  "CENTERED", "\"CENTERED-HEADINGS\"", "\"CENTURY-DATE\"", "CF", "CH",
  "CHAINING", "CHANGED", "CHARACTER", "CHARACTERS", "\"CHECK-BOX\"",
  "CLASS", "CLASSIFICATION", "\"class-name\"", "\"CLEAR-SELECTION\"",
  "CLINE", "CLINES", "CLOSE", "COBOL", "CODE", "\"CODE-SET\"", "COLLATING",
  "COL", "COLOR", "COLORS", "COLS", "COLUMN", "\"COLUMN-COLOR\"",
  "\"COLUMN-DIVIDERS\"", "\"COLUMN-FONT\"", "\"COLUMN-HEADINGS\"",
  "\"COLUMN-PROTECTION\"", "COLUMNS", "\"COMBO-BOX\"", "COMMA",
  "\"COMMAND-LINE\"", "\"comma delimiter\"", "COMMIT", "COMMON",
  "COMMUNICATION", "COMP", "COMPUTE", "\"COMP-0\"", "\"COMP-1\"",
  "\"COMP-2\"", "\"COMP-3\"", "\"COMP-4\"", "\"COMP-5\"", "\"COMP-6\"",
  "\"COMP-N\"", "\"COMP-X\"", "\"FUNCTION CONCATENATE\"", "CONDITION",
  "CONFIGURATION", "CONSTANT", "CONTAINS", "CONTENT",
  "\"FUNCTION CONTENT-LENGTH\"", "\"FUNCTION CONTENT-OF\"", "CONTINUE",
  "CONTROL", "CONTROLS", "CONVERSION", "CONVERTING", "COPY",
  "\"COPY-SELECTION\"", "\"CORE-INDEX\"", "CORRESPONDING", "COUNT", "CRT",
  "\"CRT-UNDER\"", "CSIZE", "CURRENCY", "\"FUNCTION CURRENT-DATE\"",
  "CURSOR", "\"CURSOR-COL\"", "\"CURSOR-COLOR\"", "\"CURSOR-FRAME-WIDTH\"",
  "\"CURSOR-ROW\"", "\"CURSOR-X\"", "\"CURSOR-Y\"",
  "\"CUSTOM-PRINT-TEMPLATE\"", "CYCLE", "\"CYL-INDEX\"",
  "\"CYL-OVERFLOW\"", "DASHED", "DATA", "\"DATA-COLUMNS\"",
  "\"DATA-TYPES\"", "DATE", "\"DATE-ENTRY\"", "DAY", "\"DAY-OF-WEEK\"",
  "DE", "DEBUGGING", "\"DECIMAL-POINT\"", "DECLARATIVES", "DEFAULT",
  "\"DEFAULT-BUTTON\"", "\"DEFAULT-FONT\"", "DELETE", "DELIMITED",
  "DELIMITER", "DEPENDING", "DESCENDING", "DESTINATION", "DESTROY",
  "DETAIL", "DISABLE", "DISC", "DISK", "DISP", "DISPLAY",
  "\"DISPLAY-COLUMNS\"", "\"DISPLAY-FORMAT\"", "\"FUNCTION DISPLAY-OF\"",
  "DIVIDE", "DIVIDERS", "\"DIVIDER-COLOR\"", "DIVISION", "DOTDASH",
  "DOTTED", "\"DRAG-COLOR\"", "\"DROP-DOWN\"", "\"DROP-LIST\"", "DOWN",
  "DUPLICATES", "DYNAMIC", "EBCDIC", "EC", "ECHO", "EGI",
  "\"level-number 88\"", "ENABLE", "ELEMENT", "ELSE", "EMI", "ENCRYPTION",
  "ENCODING", "END", "\"END-ACCEPT\"", "\"END-ADD\"", "\"END-CALL\"",
  "\"END-COMPUTE\"", "\"END-COLOR\"", "\"END-DELETE\"", "\"END-DISPLAY\"",
  "\"END-DIVIDE\"", "\"END-EVALUATE\"", "\"END FUNCTION\"", "\"END-IF\"",
  "\"END-JSON\"", "\"END-MODIFY\"", "\"END-MULTIPLY\"", "\"END-PERFORM\"",
  "\"END PROGRAM\"", "\"END-READ\"", "\"END-RECEIVE\"", "\"END-RETURN\"",
  "\"END-REWRITE\"", "\"END-SEARCH\"", "\"END-START\"", "\"END-STRING\"",
  "\"END-SUBTRACT\"", "\"END-UNSTRING\"", "\"END-WRITE\"", "\"END-XML\"",
  "ENGRAVED", "\"ENSURE-VISIBLE\"", "ENTRY", "\"ENTRY-CONVENTION\"",
  "\"ENTRY-FIELD\"", "\"ENTRY-REASON\"", "ENVIRONMENT",
  "\"ENVIRONMENT-NAME\"", "\"ENVIRONMENT-VALUE\"", "EOL", "EOP", "EOS",
  "EQUAL", "ERASE", "ERROR", "ESCAPE", "\"ESCAPE-BUTTON\"", "ESI",
  "EVALUATE", "EVENT", "\"EVENT-LIST\"", "\"EVENT STATUS\"", "EVERY",
  "EXCEPTION", "\"EXCEPTION CONDITION\"", "\"EXCEPTION-VALUE\"", "EXPAND",
  "EXCLUSIVE", "EXHIBIT", "EXIT", "\"exponentiation operator\"", "EXTEND",
  "\"EXTENDED-SEARCH\"", "EXTERNAL", "\"EXTERNAL-FORM\"", "F", "FD",
  "\"FH--FCD\"", "\"FH--KEYDEF\"", "\"FILE-CONTROL\"", "\"FILE-ID\"",
  "\"FILE-LIMIT\"", "\"FILE-LIMITS\"", "\"FILE-NAME\"", "\"FILE-POS\"",
  "\"FILL-COLOR\"", "\"FILL-COLOR2\"", "\"FILL-PERCENT\"", "FILLER",
  "FINAL", "\"FINISH-REASON\"", "FIRST", "FIXED", "\"FIXED-FONT\"",
  "\"FIXED-WIDTH\"", "FLAT", "\"FLAT-BUTTONS\"", "\"FLOAT-BINARY-128\"",
  "\"FLOAT-BINARY-32\"", "\"FLOAT-BINARY-64\"", "\"FLOAT-DECIMAL-16\"",
  "\"FLOAT-DECIMAL-34\"", "\"FLOAT-DECIMAL-7\"", "\"FLOAT-EXTENDED\"",
  "\"FLOAT-LONG\"", "\"FLOAT-SHORT\"", "FLOATING", "FONT", "FOOTING",
  "FOR", "\"FOREGROUND-COLOR\"", "FOREVER", "\"FUNCTION FORMATTED-DATE\"",
  "\"FUNCTION FORMATTED-DATETIME\"", "\"FUNCTION FORMATTED-TIME\"",
  "FRAME", "FRAMED", "FREE", "FROM", "\"FROM CRT\"", "FULL",
  "\"FULL-HEIGHT\"", "FUNCTION", "\"FUNCTION-ID\"",
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
  "\"I-O-CONTROL\"", "JSON", "JUSTIFIED", "KEPT", "KEY", "KEYBOARD",
  "LABEL", "\"LABEL-OFFSET\"", "\"LARGE-FONT\"", "\"LARGE-OFFSET\"",
  "LAST", "\"LAST-ROW\"", "\"LAYOUT-DATA\"", "\"LAYOUT-MANAGER\"",
  "LEADING", "\"LEADING-SHIFT\"", "LEAVE", "LEFT", "LEFTLINE",
  "\"LEFT-TEXT\"", "LENGTH", "\"LENGTH OF\"",
  "\"FUNCTION LENGTH/BYTE-LENGTH\"", "LESS", "\"LESS OR EQUAL\"",
  "\"level-number\"", "LIKE", "LIMIT", "LIMITS", "LINAGE",
  "\"LINAGE-COUNTER\"", "LINE", "\"LINE-COUNTER\"", "\"LINE LIMIT\"",
  "\"LINE-SEQUENTIAL\"", "LINES", "\"LINES-AT-ROOT\"", "LINKAGE",
  "\"LIST-BOX\"", "\"Literal\"", "\"LM-RESIZE\"", "LOC", "LOCALE",
  "\"FUNCTION LOCALE-DATE\"", "\"FUNCTION LOCALE-TIME\"",
  "\"FUNCTION LOCALE-TIME-FROM-SECONDS\"", "\"LOCAL-STORAGE\"", "LOCK",
  "\"LOCK-HOLDING\"", "\"LONG-DATE\"", "LOWER", "LOWERED",
  "\"FUNCTION LOWER-CASE\"", "LOWLIGHT", "\"LOW-COLOR\"", "\"LOW-VALUE\"",
  "\"MAGNETIC-TAPE\"", "MANUAL", "\"MASS-UPDATE\"", "\"MASTER-INDEX\"",
  "\"MAX-LINES\"", "\"MAX-PROGRESS\"", "\"MAX-TEXT\"", "\"MAX-VAL\"",
  "MEMORY", "\"MEDIUM-FONT\"", "MENU", "MERGE", "MESSAGE", "MINUS",
  "\"MIN-VAL\"", "\"Mnemonic name\"", "MODE", "MODIFY", "MODULES", "MOVE",
  "MULTILINE", "MULTIPLE", "MULTIPLY", "NAME", "NAMED", "NAMESPACE",
  "\"NAMESPACE-PREFIX\"", "NATIONAL", "\"NATIONAL-EDITED\"",
  "\"FUNCTION NATIONAL-OF\"", "NATIVE", "\"NAVIGATE-URL\"",
  "\"NEAREST-AWAY-FROM-ZERO\"", "\"NEAREST-EVEN\"",
  "\"NEAREST-TOWARD-ZERO\"", "NEGATIVE", "NESTED", "NEW", "NEXT",
  "\"NEXT-ITEM\"", "\"NEXT GROUP\"", "\"NEXT PAGE\"", "NO",
  "\"NO ADVANCING\"", "\"NO-AUTOSEL\"", "\"NO-AUTO-DEFAULT\"",
  "\"NO-BOX\"", "\"NO DATA\"", "\"NO-DIVIDERS\"", "\"NO-ECHO\"",
  "\"NO-F4\"", "\"NO-FOCUS\"", "\"NO-GROUP-TAB\"", "\"NO-KEY-LETTER\"",
  "NOMINAL", "\"NO-SEARCH\"", "\"NO-UPDOWN\"", "NONNUMERIC", "NORMAL",
  "NOT", "NOTAB", "NOTHING", "NOTIFY", "\"NOTIFY-CHANGE\"",
  "\"NOTIFY-DBLCLICK\"", "\"NOTIFY-SELCHANGE\"", "\"NOT END\"",
  "\"NOT EOP\"", "\"NOT ESCAPE\"", "\"NOT EQUAL\"", "\"NOT EXCEPTION\"",
  "\"NOT INVALID KEY\"", "\"NOT OVERFLOW\"", "\"NOT SIZE ERROR\"",
  "\"NUM-COL-HEADINGS\"", "\"NUM-ROWS\"", "NUMBER", "NUMBERS", "NUMERIC",
  "\"NUMERIC-EDITED\"", "\"FUNCTION NUMVAL-C\"", "OBJECT",
  "\"OBJECT-COMPUTER\"", "OCCURS", "OF", "OFF", "\"OK-BUTTON\"", "OMITTED",
  "ON", "ONLY", "OPEN", "OPTIONAL", "OPTIONS", "OR", "ORDER",
  "ORGANIZATION", "OTHER", "OTHERS", "OUTPUT", "\"OVERLAP-LEFT\"",
  "\"OVERLAP-TOP\"", "OVERLINE", "\"PACKED-DECIMAL\"", "PADDING", "PASCAL",
  "PAGE", "\"PAGE-COUNTER\"", "\"PAGE-SETUP\"", "PAGED", "PARAGRAPH",
  "PARENT", "PARSE", "PASSWORD", "PERFORM", "PERMANENT", "PH", "PF",
  "PHYSICAL", "PICTURE", "\"PICTURE SYMBOL\"", "PIXEL", "PLACEMENT",
  "PLUS", "POINTER", "\"POP-UP\"", "POS", "POSITION", "\"POSITION-SHIFT\"",
  "POSITIVE", "PRESENT", "PREVIOUS", "PRINT", "\"PRINT-CONTROL\"",
  "\"PRINT-NO-PROMPT\"", "\"PRINT-PREVIEW\"", "PRINTER", "\"PRINTER-1\"",
  "PRINTING", "PRIORITY", "PROCEDURE", "PROCEDURES", "PROCEED",
  "PROCESSING", "PROGRAM", "\"PROGRAM-ID\"", "\"program name\"",
  "\"PROGRAM-POINTER\"", "PROGRESS", "PROHIBITED", "PROMPT", "PROPERTIES",
  "PROPERTY", "PROTECTED", "PURGE", "\"PUSH-BUTTON\"", "\"QUERY-INDEX\"",
  "QUEUE", "QUOTE", "\"RADIO-BUTTON\"", "RAISE", "RAISED", "RANDOM", "RD",
  "READ", "READERS", "\"READ-ONLY\"", "\"READY TRACE\"", "RECEIVE",
  "RECORD", "\"RECORD-DATA\"", "\"RECORD-OVERFLOW\"", "\"RECORD-TO-ADD\"",
  "\"RECORD-TO-DELETE\"", "RECORDING", "RECORDS", "RECURSIVE", "REDEFINES",
  "REEL", "REFERENCE", "REFERENCES", "REFRESH", "\"REGION-COLOR\"",
  "RELATIVE", "RELEASE", "REMAINDER", "REMOVAL", "RENAMES",
  "\"REORG-CRITERIA\"", "REPLACE", "REPLACING", "REPORT", "REPORTING",
  "REPORTS", "REPOSITORY", "REQUIRED", "REREAD", "RERUN", "RESERVE",
  "RESET", "\"RESET TRACE\"", "\"RESET-GRID\"", "\"RESET-LIST\"",
  "\"RESET-TABS\"", "RETRY", "RETURN", "RETURNING", "REVERSE",
  "\"FUNCTION REVERSE\"", "\"REVERSE-VIDEO\"", "REVERSED", "REWIND",
  "REWRITE", "RF", "RH", "RIGHT", "\"RIGHT-ALIGN\"", "RIMMED", "ROLLBACK",
  "ROUNDED", "ROUNDING", "\"ROW-COLOR\"", "\"ROW-COLOR-PATTERN\"",
  "\"ROW-DIVIDERS\"", "\"ROW-FONT\"", "\"ROW-HEADINGS\"",
  "\"ROW-PROTECTION\"", "RUN", "S", "SAME", "\"SAVE-AS\"",
  "\"SAVE-AS-NO-PROMPT\"", "SCREEN", "\"SCREEN CONTROL\"", "SCROLL",
  "\"SCROLL-BAR\"", "SD", "SEARCH", "\"SEARCH-OPTIONS\"",
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
  "STRONG", "STYLE", "\"SUB-QUEUE-1\"", "\"SUB-QUEUE-2\"",
  "\"SUB-QUEUE-3\"", "\"FUNCTION SUBSTITUTE\"",
  "\"FUNCTION SUBSTITUTE-CASE\"", "SUBTRACT", "SUBWINDOW", "SUM",
  "SUPPRESS", "\"SUPPRESS\"", "SYMBOLIC", "SYNCHRONIZED",
  "\"SYSTEM-DEFAULT\"", "\"SYSTEM-INFO\"", "\"SYSTEM-OFFSET\"", "TAB",
  "\"TAB-TO-ADD\"", "\"TAB-TO-DELETE\"", "TABLE", "TALLYING", "TEMPORARY",
  "TAPE", "TERMINAL", "TERMINATE", "\"TERMINAL-INFO\"",
  "\"TERMINATION-VALUE\"", "TEST", "TEXT", "THAN", "THEN", "THREAD",
  "THREADS", "THRU", "\"THUMB-POSITION\"", "\"TILED-HEADINGS\"", "TIME",
  "\"TIME-OUT\"", "TIMES", "TITLE", "\"TITLE-POSITION\"", "TO", "\"&\"",
  "\")\"", "\":\"", "\"/\"", "\".\"", "\"=\"", "\"EXTERN\"", "\"FALSE\"",
  "\"FILE\"", "\">\"", "\"INITIAL\"", "\"<\"", "\"-\"", "\"*\"",
  "\"NULL\"", "\"OVERFLOW\"", "\"(\"", "\"+\"", "\"TRUE\"", "TOP",
  "\"TOWARD-GREATER\"", "\"TOWARD-LESSER\"", "TRACK", "TRACKS",
  "\"TRACK-AREA\"", "\"TRACK-LIMIT\"", "\"TRADITIONAL-FONT\"", "TRAILING",
  "\"TRAILING-SHIFT\"", "TRANSFORM", "TRANSPARENT", "\"TREE-VIEW\"",
  "\"FUNCTION TRIM\"", "TRUNCATION", "TYPE", "TYPEDEF", "U", "\"UCS-4\"",
  "UNBOUNDED", "UNDERLINE", "UNFRAMED", "UNIT", "UNLOCK", "UNSIGNED",
  "\"UNSIGNED-INT\"", "\"UNSIGNED-LONG\"", "\"UNSIGNED-SHORT\"",
  "UNSORTED", "UNSTRING", "UNTIL", "UP", "UPDATE", "UPDATERS", "UPON",
  "\"UPON ARGUMENT-NUMBER\"", "\"UPON COMMAND-LINE\"",
  "\"UPON ENVIRONMENT-NAME\"", "\"UPON ENVIRONMENT-VALUE\"", "UPPER",
  "\"FUNCTION UPPER-CASE\"", "USAGE", "USE", "\"USE-ALT\"",
  "\"USE-RETURN\"", "\"USE-TAB\"", "USER", "\"USER-DEFAULT\"",
  "\"user function name\"", "USING", "\"UTF-8\"", "\"UTF-16\"", "V",
  "VALIDATE", "VALIDATING", "VALUE", "\"VALUE-FORMAT\"", "VARIABLE",
  "VARIANT", "VARYING", "VERTICAL", "\"VERY-HEAVY\"", "\"VIRTUAL-WIDTH\"",
  "VOLATILE", "VPADDING", "VSCROLL", "\"VSCROLL-BAR\"", "\"VSCROLL-POS\"",
  "VTOP", "WAIT", "\"WEB-BROWSER\"", "WHEN", "\"FUNCTION WHEN-COMPILED\"",
  "\"WHEN\"", "WIDTH", "\"WIDTH-IN-CELLS\"", "WINDOW", "WITH",
  "\"Identifier\"", "WORDS", "\"WORKING-STORAGE\"", "WRAP", "WRITE",
  "\"WRITE-ONLY\"", "\"WRITE-VERIFY\"", "WRITERS", "X", "XML",
  "\"XML-DECLARATION\"", "Y", "YYYYDDD", "YYYYMMDD", "ZERO",
  "SHIFT_PREFER", "$accept", "start", "$@1", "compilation_group",
  "nested_list", "$@2", "source_element_list", "source_element",
  "simple_prog", "$@3", "program_definition", "function_definition",
  "_end_program_list", "end_program_list", "end_program", "$@4",
  "end_function", "$@5", "_program_body", "$@6", "$@7",
  "_identification_header", "identification_header",
  "identification_or_id", "program_id_paragraph", "$@8", "$@9",
  "function_id_paragraph", "$@10", "program_id_name", "end_program_name",
  "_as_literal", "_program_type", "program_type_clause",
  "init_or_recurse_and_common", "init_or_recurse", "_options_paragraph",
  "_options_clauses", "_arithmetic_clause", "arithmetic_choice",
  "_default_rounded_clause", "_entry_convention_clause", "convention_type",
  "_intermediate_rounding_clause", "intermediate_rounding_choice",
  "_environment_division", "_environment_header", "_configuration_section",
  "_configuration_header", "_configuration_paragraphs",
  "configuration_paragraphs", "configuration_paragraph",
  "source_computer_paragraph", "$@11", "_source_computer_entry",
  "_with_debugging_mode", "object_computer_paragraph", "$@12",
  "_object_computer_entry", "object_clauses_list", "object_clauses",
  "object_computer_memory", "object_computer_sequence",
  "program_collating_sequence", "$@13", "program_coll_sequence_values",
  "object_computer_segment", "object_computer_class", "locale_class",
  "computer_words", "repository_paragraph", "$@14", "_repository_entry",
  "repository_list", "repository_name", "repository_name_list",
  "special_names_header", "special_names_sentence", "special_name_list",
  "special_name", "mnemonic_name_clause", "$@15", "mnemonic_choices",
  "_special_name_mnemonic_on_off", "on_off_clauses", "on_off_clauses_1",
  "alphabet_name_clause", "@16", "alphabet_definition", "@17", "@18",
  "alphabet_target_alphanumeric", "alphabet_target_national",
  "alphabet_type_alphanumeric", "alphabet_type_national",
  "alphabet_type_common", "alphabet_literal_list", "alphabet_literal",
  "@19", "alphabet_also_sequence", "alphabet_lits", "space_or_zero",
  "symbolic_characters_clause", "_sym_in_word", "symbolic_collection",
  "symbolic_chars_list", "symbolic_chars_phrase", "char_list",
  "integer_list", "symbolic_constant_clause", "symbolic_constant_list",
  "symbolic_constant", "class_name_clause", "class_item_list",
  "class_item", "_class_type", "_in_alphabet", "locale_clause",
  "currency_sign_clause", "_with_pic_symbol", "decimal_point_clause",
  "numeric_sign_clause", "cursor_clause", "crt_status_clause",
  "screen_control", "event_status", "top_clause", "$@20",
  "_input_output_section", "_input_output_header", "_file_control_header",
  "_file_control_sequence", "file_control_entry", "$@21",
  "_select_clauses_or_error", "_select_clause_sequence", "select_clause",
  "assign_clause", "_assign_device_or_line_adv_file", "assign_device",
  "general_device_name", "line_seq_device_name", "line_adv_file",
  "_ext_clause", "ext_clause", "assignment_name", "access_mode_clause",
  "access_mode", "alternative_record_key_clause", "_password_clause",
  "password_clause", "$@22", "_suppress_clause",
  "collating_sequence_clause", "collating_sequence", "$@23",
  "coll_sequence_values", "collating_sequence_clause_key", "alphabet_name",
  "file_status_clause", "_file_or_sort", "lock_mode_clause", "$@24",
  "lock_mode", "_lock_with", "_with_rollback", "with_rollback",
  "_with_mass_update", "organization_clause", "organization",
  "padding_character_clause", "record_delimiter_clause", "$@25",
  "record_delimiter_option", "record_key_clause", "_split_keys",
  "source_is", "split_key_list", "$@26", "split_key",
  "relative_key_clause", "reserve_clause", "no_or_integer",
  "sharing_clause", "sharing_option", "file_limit_clause", "thru_list",
  "actual_key_clause", "nominal_key_clause", "track_area_clause",
  "track_limit_clause", "_i_o_control", "i_o_control_header",
  "_i_o_control_entries", "i_o_control_list", "i_o_control_clause",
  "same_clause", "_same_option", "apply_clause",
  "obsolete_dos_vs_apply_phrase", "multiple_file_tape_clause", "$@27",
  "multiple_file_list", "multiple_file", "_multiple_file_position",
  "rerun_clause", "_on_assignment", "rerun_event", "_data_division",
  "$@28", "_data_division_header", "data_division_header",
  "_file_section_header", "_file_description_sequence", "file_description",
  "file_description_entry", "$@29", "file_type",
  "_file_description_clause_sequence", "file_description_clause",
  "block_contains_clause", "_records_or_characters", "record_clause",
  "_record_depending", "_from_integer", "_to_integer",
  "label_records_clause", "value_of_clause", "file_id", "valueof_name",
  "data_records_clause", "linage_clause", "_linage_sequence",
  "linage_lines", "linage_footing", "linage_top", "linage_bottom",
  "recording_mode_clause", "recording_mode", "u_or_s", "code_set_clause",
  "_for_sub_records_clause", "report_clause", "report_keyword",
  "rep_name_list", "_communication_section", "$@30",
  "_communication_description_sequence", "communication_description",
  "communication_description_entry", "$@31",
  "_communication_description_clause_sequence",
  "communication_description_clause", "_input_cd_clauses",
  "named_input_cd_clauses", "named_input_cd_clause",
  "unnamed_input_cd_clauses", "_output_cd_clauses", "output_cd_clauses",
  "output_cd_clause", "_i_o_cd_clauses", "named_i_o_cd_clauses",
  "named_i_o_cd_clause", "unnamed_i_o_cd_clauses",
  "_working_storage_section", "$@32", "_record_description_list", "$@33",
  "record_description_list", "data_description", "$@34", "level_number",
  "_filler", "_entry_name", "user_entry_name", "_const_global",
  "lit_or_length", "con_source", "fp32_usage", "fp64_usage", "fp128_usage",
  "pointer_len", "renames_entry", "_renames_thru", "condition_name_entry",
  "$@35", "constant_entry", "$@36", "constant_source",
  "constant_78_source", "constant_expression_list", "constant_expression",
  "_data_description_clause_sequence", "data_description_clause_sequence",
  "data_description_clause", "redefines_clause", "like_clause",
  "_length_modifier", "length_modifier", "same_as_clause",
  "typedef_clause", "_strong", "external_clause", "_as_extname",
  "_global_clause", "global_clause", "special_names_clause", "$@37",
  "special_names_target", "volatile_clause", "picture_clause",
  "_pic_locale_format", "_is_locale_name", "locale_name", "type_to_clause",
  "usage_clause", "usage", "double_usage", "_font_name", "_layout_name",
  "sign_clause", "report_occurs_clause", "_occurs_step", "occurs_clause",
  "_occurs_to_integer", "_occurs_from_integer", "_occurs_integer_to",
  "_occurs_depending", "_capacity_in", "_occurs_initialized",
  "_occurs_keys_and_indexed", "$@38", "occurs_keys", "occurs_key_list",
  "occurs_key_field", "ascending_or_descending", "_occurs_indexed",
  "occurs_indexed", "occurs_index_list", "occurs_index",
  "justified_clause", "synchronized_clause", "_left_or_right",
  "blank_clause", "based_clause", "value_clause", "$@39",
  "value_item_list", "value_item", "_false_is", "any_length_clause",
  "external_form_clause", "identified_by_clause", "_local_storage_section",
  "$@40", "_linkage_section", "$@41", "_report_section", "$@42",
  "_report_description_sequence", "report_description", "$@43",
  "_report_description_options", "report_description_option",
  "control_clause", "control_field_list", "control_final_tag",
  "control_identifier_list", "control_identifier", "page_limit_clause",
  "page_line_column", "page_limit_cols", "integer_or_zero_or_ident",
  "_page_heading_list", "page_detail", "heading_clause", "first_detail",
  "last_heading", "last_detail", "footing_clause",
  "_report_group_description_list", "report_group_description_entry",
  "$@44", "_report_group_options", "report_group_option", "type_is_clause",
  "type_option", "_control_heading_final", "_or_page",
  "_control_footing_final", "next_group_clause", "next_group_plus",
  "next_page", "sum_clause_list", "_reset_clause", "data_or_final",
  "present_when_condition", "present_absent", "_page_or_id", "page_or_ids",
  "report_varying_clause", "line_clause", "line_keyword_clause",
  "_line_clause_options", "line_clause_option", "column_clause",
  "col_keyword_clause", "_orientation", "_left_right_center",
  "col_or_plus", "column_integer_list", "column_integer", "source_clause",
  "group_indicate_clause", "_screen_section", "$@45",
  "_screen_description_list", "screen_description_list",
  "screen_description", "$@46", "$@47", "$@48", "_screen_options",
  "screen_option", "control_definition", "control_type_name",
  "control_type", "control_item", "_control_attributes",
  "control_attributes", "control_attribute", "control_style",
  "control_property", "control_style_name", "control_property_name",
  "control_style_name_generic", "control_property_name_generic",
  "control_style_name_label", "control_property_name_label",
  "control_style_name_entry_field", "control_property_name_entry_field",
  "control_style_name_push_button", "control_property_name_push_button",
  "control_style_name_check_box", "control_property_name_radio_button",
  "control_style_name_list_box", "control_property_name_list_box",
  "control_style_name_combo_box", "control_style_name_frame",
  "control_property_name_frame", "control_style_name_tab_control",
  "control_property_name_tab_control", "control_style_name_bar",
  "control_property_name_bar", "control_property_name_bitmap",
  "control_style_name_grid", "control_property_name_grid",
  "control_style_name_tree_view", "control_property_name_tree_view",
  "control_property_name_web_browser", "control_style_name_activex",
  "control_property_name_activex", "control_style_name_date_entry",
  "control_property_name_date_entry", "control_style_type",
  "control_property_type", "changeable_control_properties",
  "changeable_control_property", "changeable_window_properties",
  "changeable_window_property", "eol", "eos", "_plus", "plus",
  "plus_tokens", "minus", "minus_tokens", "control_size",
  "control_size_unit", "_cell", "screen_line_number",
  "_screen_line_plus_minus", "screen_col_number", "_screen_col_plus_minus",
  "screen_occurs_clause", "screen_global_clause", "_procedure_division",
  "procedure_division", "$@49", "$@50", "$@51", "$@52",
  "_procedure_using_chaining", "$@53", "$@54", "procedure_param_list",
  "procedure_param", "_procedure_type", "_size_optional",
  "size_is_integer", "_acu_size", "_procedure_optional",
  "_procedure_returning", "_procedure_declaratives", "$@55",
  "_procedure_list", "procedure", "section_header", "$@56",
  "_use_statement", "paragraph_header", "invalid_statement", "_segment",
  "statement_list", "@57", "@58", "statements", "$@59", "statement",
  "accept_statement", "$@60", "accept_body", "$@61", "$@62",
  "accp_identifier", "field_with_pos_specifier", "$@63", "_pos_specifier",
  "pos_specifier", "pos_specifier_value", "identifier_or_numeric_literal",
  "_accept_clauses", "accept_clauses", "accept_clause",
  "accept_from_screen_clauses", "accept_from_screen_clause",
  "lines_or_number", "at_line_column", "line_number", "column_number",
  "mode_is_block", "accp_attr", "_key_dest", "key_dest", "no_echo",
  "reverse_video", "update_default", "_end_accept", "add_statement",
  "$@64", "add_body", "_add_to", "_end_add", "allocate_statement", "$@65",
  "allocate_body", "_loc", "allocate_returning", "alter_statement", "$@66",
  "alter_body", "alter_entry", "_proceed_to", "call_statement", "$@67",
  "call_body", "$@68", "_conv_linkage", "$@69", "conv_linkage_option",
  "_mnemonic_conv", "mnemonic_conv", "program_or_prototype",
  "_id_or_lit_or_func_as", "nested_or_prototype", "call_using", "$@70",
  "call_param_list", "call_param", "_call_type", "call_returning",
  "return_give", "null_or_omitted", "call_exception_phrases",
  "_call_on_exception", "call_on_exception", "_call_not_on_exception",
  "call_not_on_exception", "_end_call", "cancel_statement", "$@71",
  "cancel_body", "id_or_lit_or_program_name", "close_statement", "$@72",
  "close_body", "close_files", "_close_option", "close_window", "$@73",
  "_close_display_option", "compute_statement", "$@74", "compute_body",
  "_end_compute", "commit_statement", "continue_statement", "$@75",
  "_continue_after_phrase", "$@76", "destroy_statement", "$@77",
  "destroy_body", "delete_statement", "$@78", "delete_body",
  "delete_file_list", "_end_delete", "disable_statement", "$@79",
  "enable_disable_handling", "_enable_disable_key", "communication_mode",
  "display_statement", "$@80", "display_body", "screen_or_device_display",
  "display_list", "display_atom", "$@81", "disp_list",
  "_with_display_attr", "display_attrs", "display_clauses",
  "display_clause", "_display_upon", "display_upon", "crt_under",
  "display_erase", "$@82", "display_pos_specifier",
  "field_or_literal_or_erase_with_pos_specifier", "$@83",
  "field_or_literal_or_erase_list", "field_or_literal_or_erase",
  "display_message_box", "$@84", "_display_message_clauses",
  "display_message_clauses", "display_message_clause", "display_window",
  "$@85", "$@86", "sub_or_window", "display_floating_window", "$@87",
  "$@88", "display_initial_window", "$@89", "initial_type", "_graphical",
  "_upon_window_handle", "window_handle", "display_window_clauses",
  "display_window_clause", "shadow", "boxed", "_top_or_bottom",
  "_left_or_centered_or_right", "no_scroll_wrap", "pop_up_or_handle",
  "pop_up_area", "handle_is_in", "disp_attr", "_end_display",
  "divide_statement", "$@90", "divide_body", "_end_divide",
  "enable_statement", "$@91", "entry_statement", "$@92", "$@93",
  "entry_body", "entry_goto_body", "evaluate_statement", "$@94",
  "evaluate_body", "evaluate_subject_list", "evaluate_subject",
  "evaluate_condition_list", "evaluate_case_list", "evaluate_case",
  "evaluate_other", "evaluate_when_list", "$@95", "$@96",
  "evaluate_object_list", "evaluate_object", "_evaluate_thru_expr",
  "_end_evaluate", "exhibit_statement", "$@97", "exhibit_body", "$@98",
  "_changed", "_named", "exhibit_target_list", "exhibit_target",
  "exit_statement", "$@99", "exit_body", "exit_program_returning",
  "free_statement", "$@100", "free_body", "generate_statement", "$@101",
  "generate_body", "goto_statement", "$@102", "go_body", "goto_depending",
  "goback_statement", "if_statement", "$@103", "if_else_statements",
  "_if_then", "if_true", "if_false", "_end_if", "initialize_statement",
  "$@104", "initialize_body", "_initialize_filler", "_initialize_value",
  "_initialize_replacing", "initialize_replacing_list",
  "initialize_replacing_item", "initialize_category",
  "_initialize_default", "initiate_statement", "$@105", "initiate_body",
  "inquire_statement", "$@106", "inquire_body", "inspect_statement",
  "$@107", "inspect_body", "send_identifier", "inspect_list",
  "inspect_tallying", "$@108", "inspect_replacing", "inspect_converting",
  "tallying_list", "tallying_item", "replacing_list", "replacing_item",
  "rep_keyword", "replacing_region", "inspect_region", "inspect_before",
  "inspect_after", "json_generate_statement", "$@109",
  "json_generate_body", "$@110", "$@111", "_json_suppress",
  "json_suppress_list", "json_suppress_entry", "_end_json",
  "json_parse_statement", "$@112", "json_parse_body", "_with_detail",
  "merge_statement", "$@113", "modify_statement", "$@114", "modify_body",
  "_end_modify", "move_statement", "$@115", "move_body",
  "multiply_statement", "$@116", "multiply_body", "_end_multiply",
  "open_statement", "$@117", "open_body", "open_file_entry",
  "_open_exclusive", "open_mode", "_open_sharing", "_open_option",
  "lock_allowing", "open_lock_option", "allowing_option", "allowing_all",
  "open_option_sequential", "osvs_input_mode", "perform_statement",
  "$@118", "perform_body", "$@119", "$@120", "_end_perform",
  "end_perform_or_dot", "perform_procedure", "_perform_option",
  "perform_test", "cond_or_exit", "perform_varying_list",
  "perform_varying", "_by_phrase", "purge_statement", "$@121",
  "raise_statement", "$@122", "raise_body", "exception_name",
  "read_statement", "$@123", "read_body", "_read_into", "_lock_phrases",
  "ignoring_lock", "advancing_lock_or_retry", "_retry_phrase",
  "retry_phrase", "retry_options", "_extended_with_lock",
  "extended_with_lock", "_read_key", "read_handler", "_end_read",
  "ready_statement", "receive_statement", "$@124", "receive_body",
  "message_or_segment", "_data_sentence_phrases", "_no_data_sentence",
  "no_data_sentence", "_with_data_sentence", "with_data_sentence",
  "_end_receive", "release_statement", "$@125", "release_body",
  "reset_statement", "return_statement", "$@126", "return_body",
  "_end_return", "rewrite_statement", "$@127", "rewrite_body",
  "_with_lock", "with_lock", "_end_rewrite", "rollback_statement",
  "search_statement", "$@128", "search_body", "search_varying",
  "search_at_end", "search_whens", "search_when", "_end_search",
  "send_statement", "$@129", "send_body", "_from_identifier",
  "from_identifier", "with_indicator", "_replacing_line", "set_statement",
  "$@130", "set_body", "on_or_off", "up_or_down", "set_environment",
  "set_attr", "set_attr_clause", "set_attr_one", "set_to", "set_up_down",
  "set_to_on_off_sequence", "set_to_on_off", "set_to_true_false_sequence",
  "set_to_true_false", "set_last_exception_to_off", "set_thread_priority",
  "sort_statement", "$@131", "sort_body", "@132", "_sort_key_list",
  "_key_sort_list", "_sort_duplicates", "_sort_collating", "sort_input",
  "sort_output", "start_statement", "$@133", "start_body",
  "_sizelen_clause", "_start_key", "start_op", "disallowed_op",
  "not_equal_op", "_end_start", "stop_statement", "$@134",
  "stop_returning", "_status_x", "stop_argument", "stop_literal",
  "string_statement", "$@135", "string_body", "string_items", "$@136",
  "string_item_list", "string_item", "_string_delimited",
  "string_delimiter", "_with_pointer", "_end_string", "subtract_statement",
  "$@137", "subtract_body", "_end_subtract", "suppress_statement",
  "_printing", "terminate_statement", "$@138", "terminate_body",
  "transform_statement", "$@139", "transform_body", "unlock_statement",
  "$@140", "unlock_body", "unstring_statement", "$@141", "unstring_body",
  "_unstring_delimited", "unstring_delimited_list",
  "unstring_delimited_item", "unstring_into", "unstring_into_item",
  "_unstring_into_delimiter", "_unstring_tallying", "_end_unstring",
  "validate_statement", "$@142", "validate_fields", "use_statement",
  "$@143", "use_phrase", "use_file_exception", "use_global",
  "use_file_exception_target", "use_debugging", "debugging_list",
  "debugging_target", "_all_refs", "use_start_end", "program_start_end",
  "use_reporting", "use_exception_list", "use_exception", "use_ex_keyw",
  "write_statement", "$@144", "write_body", "from_option", "write_option",
  "before_or_after", "write_handler", "_end_write",
  "xml_generate_statement", "$@145", "xml_generate_body", "$@146", "$@147",
  "_with_encoding_xml_dec_and_attrs", "with_encoding_xml_dec_and_attrs",
  "with_encoding_xml_dec_and_attr", "encoding_xml_dec_and_attr",
  "_xml_gen_namespace", "_xml_gen_namespace_prefix", "_xml_name_of",
  "identifier_name_list", "identifier_is_name", "_json_name_of",
  "json_identifier_name_list", "json_identifier_is_name", "_type_of",
  "identifier_type_list", "identifier_is_type", "_xml_type", "ml_type",
  "_xml_gen_suppress", "xml_suppress_list", "xml_suppress_entry",
  "xml_suppress_generic_opt", "xml_suppress_when_list", "_end_xml",
  "xml_parse_statement", "$@148", "xml_parse_body", "$@149",
  "_with_encoding", "_returning_national", "_validating_with",
  "schema_file_or_record_name", "_accept_exception_phrases",
  "_accp_on_exception", "accp_on_exception", "escape_or_exception",
  "_accp_not_on_exception", "accp_not_on_exception",
  "not_escape_or_not_exception", "_display_exception_phrases",
  "_disp_on_exception", "disp_on_exception", "_disp_not_on_exception",
  "disp_not_on_exception", "_xml_exception_phrases", "_xml_on_exception",
  "xml_on_exception", "_xml_not_on_exception", "xml_not_on_exception",
  "_json_exception_phrases", "_json_on_exception", "json_on_exception",
  "_json_not_on_exception", "json_not_on_exception",
  "on_size_error_phrases", "_on_size_error", "on_size_error",
  "_not_on_size_error", "not_on_size_error", "_on_overflow_phrases",
  "_on_overflow", "on_overflow", "_not_on_overflow", "not_on_overflow",
  "return_at_end", "at_end", "_at_end_clause", "at_end_clause",
  "_not_at_end_clause", "not_at_end_clause", "at_eop_clauses",
  "_at_eop_clause", "at_eop_clause", "_not_at_eop_clause",
  "not_at_eop_clause", "_invalid_key_phrases", "invalid_key_phrases",
  "_invalid_key_sentence", "invalid_key_sentence",
  "_not_invalid_key_sentence", "not_invalid_key_sentence", "_thread_start",
  "_thread_handle", "thread_reference_optional", "_scroll_lines",
  "_count_in", "condition", "expr", "partial_expr", "$@150", "expr_tokens",
  "expr_token", "_not_expr", "not_expr", "condition_or_class", "eq", "gt",
  "lt", "ge", "le", "exp_list", "_e_sep", "exp", "exp_term", "exp_factor",
  "exp_unary", "exp_atom", "line_linage_page_counter", "arithmetic_x_list",
  "arithmetic_x", "record_name", "file_or_record_name", "table_name",
  "file_name_list", "file_file_name_list", "file_name", "cd_name",
  "report_name", "mnemonic_name_list", "mnemonic_name", "entry_name_list",
  "entry_name", "procedure_name_list", "procedure_name", "label",
  "integer_label", "reference_list", "reference", "_reference",
  "single_reference_list", "single_reference", "optional_reference_list",
  "optional_reference", "reference_or_literal", "undefined_word",
  "unique_word", "target_x_list", "target_x", "_x_list", "x_list", "x",
  "call_x", "x_common", "length_of_register", "report_x_list", "expr_x",
  "arith_x", "arith_nonzero_x", "numeric_literal", "non_numeric_literal",
  "nonzero_numeric_literal", "prog_or_entry", "alnum_or_id",
  "simple_display_value", "simple_display_all_value", "inspect_from",
  "inspect_to", "simple_value", "simple_all_value", "id_or_lit",
  "id_or_lit_or_func", "id_or_lit_or_length_or_func", "num_id_or_lit",
  "positive_id_or_lit", "pos_num_id_or_lit_or_zero", "pos_num_id_or_lit",
  "from_parameter", "sub_identifier", "table_identifier",
  "sub_identifier_1", "display_identifier", "numeric_identifier",
  "identifier_or_file_name", "identifier_field", "type_name", "identifier",
  "identifier_1", "identifier_list", "target_identifier",
  "target_identifier_1", "display_identifier_or_alphabet_name",
  "qualified_word", "unqualified_word", "$@151", "unqualified_word_check",
  "subref", "refmod", "integer", "symbolic_integer",
  "unsigned_pos_integer", "integer_or_zero", "class_value", "literal",
  "basic_literal", "basic_value", "zero_spaces_high_low_values",
  "function", "func_no_parm", "func_one_parm", "func_multi_parm",
  "func_refmod", "func_args", "trim_args", "length_arg", "$@152",
  "numvalc_args", "locale_dt_args", "formatted_datetime_args",
  "formatted_time_args", "not_const_word", "flag_all", "flag_duplicates",
  "flag_initialized", "flag_initialized_to", "to_init_val", "_flag_next",
  "_flag_not", "flag_optional", "flag_rounded", "round_mode",
  "round_choice", "flag_separate", "_from_idx_to_idx", "_dest_index",
  "error_stmt_recover", "verb", "scope_terminator", "_advancing", "_after",
  "_are", "_area", "_areas", "_as", "_at", "_before", "_binary", "_box",
  "_by", "_character", "_characters", "_collating", "_contains",
  "_controls", "_control", "_data", "_end_of", "_erase", "_every", "_file",
  "_for", "_from", "_in", "_in_equal", "_in_order", "_index", "_indicate",
  "_initial", "_into", "_is", "_is_equal", "_is_are", "_is_are_equal",
  "_is_in", "_key", "_line", "_line_or_lines", "_limits", "_lines",
  "_lock", "_message", "_mode", "_new", "_number", "_number_or_numbers",
  "_of", "_on", "_on_for", "_onoff_status", "_other", "_others",
  "_procedure", "_program", "_protected", "_record", "_records", "_right",
  "_sign", "_signed", "_sign_is", "_size", "_standard", "_status",
  "_symbolic", "_tape", "_terminal", "_then", "_times", "_to", "_up",
  "_when", "_when_set_to", "_with", "_with_for", "column_or_col",
  "columns_or_cols", "column_or_cols", "column_or_col_or_position_or_pos",
  "comp_equal", "exception_or_error", "file_limit_or_limits", "in_of",
  "label_option", "line_or_lines", "lock_records",
  "object_char_or_word_or_modules", "records", "reel_or_unit",
  "size_or_length", "length_of", "track_or_tracks", "using_or_varying",
  "detail_keyword", "ch_keyword", "cf_keyword", "ph_keyword", "pf_keyword",
  "rh_keyword", "rf_keyword", "control_keyword", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_int16 yytoknum[] =
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
    1105,  1106,  1107,  1108,  1109,  1110,  1111,  1112,  1113,  1114,
    1115,  1116,  1117,  1118,  1119,  1120,  1121,  1122,  1123,  1124,
    1125,  1126,  1127,  1128,  1129,  1130,  1131,  1132,  1133,  1134,
    1135,  1136,  1137,  1138,  1139,  1140,  1141,  1142,  1143,  1144,
    1145,  1146,  1147,  1148,  1149,  1150,  1151,  1152,  1153,  1154,
    1155,  1156,  1157,  1158,  1159,  1160,  1161,  1162,  1163,  1164,
    1165,  1166,  1167,  1168,  1169,  1170,  1171,  1172,  1173,  1174,
    1175,  1176,  1177,  1178,  1179,  1180,  1181,  1182,  1183
};
# endif

#define YYPACT_NINF (-3989)

#define yypact_value_is_default(Yyn) \
  ((Yyn) == YYPACT_NINF)

#define YYTABLE_NINF (-3160)

#define yytable_value_is_error(Yyn) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
   -3989,   471,    -1, -3989, -3989, -3989,  1034, -3989,   745, -3989,
   -3989,  1441, -3989, -3989, -3989,    -7, -3989,   267,   567, -3989,
    1179, -3989, -3989, -3989,   745,   745,   613,  1530,  1373, -3989,
    1677,  1020,  1064,  1744,  1812, -3989,  1676, -3989,  1896,  1302,
    1951,  1461,  1783,  2433,   -51,   -51, -3989, -3989,  1744, -3989,
   -3989, -3989, -3989,  1374,  1505,  1946, -3989,  2000, -3989,  1408,
   -3989,  1420,  1576, -3989,  1936,   108,   108,  1535,  1573,  1676,
    1676,  1676,   108,  1579,  1508,  1512,  1676,  1515,  1518,  1841,
   -3989, -3989, -3989,  2433, -3989, -3989, -3989, -3989, -3989, -3989,
     368, -3989, -3989, -3989, -3989,  1953, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,  2322,
    2322,  1047, -3989,  1047, -3989, -3989, -3989, -3989, -3989,  1886,
    1676,  1976,  1557,  1876,  1669, -3989, -3989,  1581,  1583, -3989,
   -3989, -3989, -3989,   913,  1676, -3989,  1676,  1486,  2275,  1486,
    1676,  1676, -3989, -3989,  1486, -3989, -3989, -3989,  1500,  1509,
    1676,  1712, -3989, -3989, -3989, -3989,  1517, -3989,  1962,  1612,
   -3989, -3989, -3989,  1617,  1621, -3989,  1676,    89,  1752,  1629,
   -3989,  2233, -3989, -3989, -3989,  1646,    25, -3989, -3989,   -35,
     913, -3989,  1676,   557,  1486,  2002,    32, -3989, -3989, -3989,
   -3989,  2017,  1648,   102,    14, -3989,  1568, -3989,  1500, -3989,
    1676, -3989,  1509, -3989,   101, -3989,   108, -3989, -3989, -3989,
   -3989, -3989,  1586,   -93,  1676,   105, -3989, -3989, -3989,  -129,
   -3989, -3989,   919, -3989, -3989, -3989, -3989,  1676, -3989, -3989,
    7768,  8348, -3989, -3989, -3989,  1574, -3989,   869,   116,  1672,
     -56, -3989, -3989,   316, -3989, -3989, -3989,   572,  1109, -3989,
   -3989, -3989,   586, -3989, -3989,  1486, -3989,  1772, -3989,  1751,
   -3989,  1676, -3989, -3989,   391, -3989, -3989, -3989, -3989, -3989,
     691,  2385,  2378,   125,  1589, -3989,   189, -3989, -3989,    71,
   -3989,   200, -3989, -3989, -3989, -3989,  2047, -3989,   -93, -3989,
    2095,   108,   108, -3989,  1586,  1686,   149, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989,  1381,   500,  8913, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989,  2179, -3989, -3989, -3989, -3989,
   -3989, -3989,     3, -3989, -3989, -3989, -3989, -3989,    19, -3989,
   -3989, -3989, -3989,  1793, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989,  1656, -3989, -3989,  1909, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989,   921, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,  1807,
    2381, -3989,   514,  1710, -3989, -3989, -3989,  1969, -3989,   108,
    1532, -3989,  1974,  1154, -3989,   301, -3989, -3989, -3989, -3989,
   -3989, -3989,  1676,  1676, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989,  1277, -3989,  1727, -3989, -3989,  1954, -3989,
   -3989, -3989,  1676,  2092, -3989, -3989, -3989, -3989,   743,  1676,
   -3989, -3989,  1827,  2156, -3989,  2322,  1212,  2322,  1743, -3989,
   -3989,  1745,  2375,  1494, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989,  1755, -3989, -3989,  2047, -3989,   108, -3989, -3989,
   -3989, -3989, -3989, -3989,  -250, -3989, -3989, -3989,  1952, -3989,
    2434, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989,  1660, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989,  -223,  6183, 16707,   248,   500,   669,
     782,  1022,  2560,  -397,   111,  1098,  5066, 12134,  1098,  2219,
     500,  1317,  2468,  1312,  1022,  1486,  1763, -3989, -3989, 12134,
   -3989,  5245,  1022,  1664,     4,  8020, -3989, -3989,  1486,     4,
   11277, 12134, -3989,  2284,   -60,  1667,  -104,  1668,  1667,  1486,
    1668,   773,   131,  1667,  1394,  1486,  1668, -3989, -3989, -3989,
   -3989,  1486, -3989, -3989, -3989, -3989, -3989, -3989,  1750, -3989,
   10298, -3989, -3989,  1664,   123,  1486,  1668,  5497,  1486,   773,
   -3989, -3989,  1764,  1873,  2126,  1500,  1500,  1500,   906,  1771,
   13168, -3989, -3989, -3989,  2148, -3989, -3989, -3989, -3989,  1974,
    1763,  1763,  2037,  1974,  1974,  1763,  1974,  1974,  1763,  1974,
    1974,  1773, -3989,  2305,   296, -3989, -3989, -3989,  2564,  1778,
   -3989, -3989,   419,  2359,  1688, -3989, -3989,  1109,  2146,  2092,
   -3989, -3989,  -172, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989,  2116, -3989,  1494, -3989, -3989, -3989, -3989, -3989,
   -3989,    50, -3989,  1016,  2366, -3989, 14304,  1779,  2125,  2268,
    2037, -3989, -3989, -3989,  1486, -3989, -3989,  1784,  1787,  1788,
   -3989, -3989,  1792,    32,    32,  1794,  1798,  1799, -3989, -3989,
    1800,    32, -3989, -3989, -3989,  1486,  1801, -3989,  1788, -3989,
    2376, -3989, 11598, -3989, -3989, 10060, -3989, -3989, -3989,  1802,
    1803,  1811, -3989, 17115, 16707, 17115, -3989,   145,  1252, -3989,
    2317, -3989, -3989, 10060, -3989,   307,  1755, -3989, -3989,   248,
   -3989,  1810, -3989,    32, -3989,  2401,   -60, -3989, -3989,   669,
   -3989, -3989, -3989, -3989, -3989,  1668, -3989,  1410,  2037,  2402,
   -3989,   437, -3989,  1957, -3989, -3989,  1750,  1755, -3989, -3989,
    1668,  2404,  2011,  2491, -3989, -3989,  1486,  1858,  1859, -3989,
   -3989, -3989,  1667, -3989,  2296, -3989,  1638,  2582, -3989, -3989,
   -3989, -3989, -3989,  2414,    86, 11736, -3989, -3989, -3989, -3989,
    1753,  1779, -3989, -3989, -3989, -3989, -3989,  2296, 10669,  1642,
    1653,  2415,   291, -3989,  1847,  2208, -3989, -3989, -3989,  2417,
     104, -3989, -3989, -3989, 11166, -3989, -3989,  2170, -3989, -3989,
    2485,     3, -3989, -3989, -3989,  1022, -3989, -3989, -3989, -3989,
   -3989,  2406, -3989, -3989,  1870, -3989, -3989,   440, -3989,  1664,
   -3989, -3989,  1486, 10191,  1114, -3989,  1145, -3989,    69, -3989,
   -3989, -3989,  1486,  1486, -3989, -3989, -3989,  1840,  9435,  1114,
    2424, 12134, -3989,  1863,  2427,  2599, -3989,  1868, -3989,  1430,
   -3989, -3989, 13204,  1879, -3989, -3989,  1776, -3989, -3989,  2429,
     -78,  2432,   844, -3989,  2345, -3989,  2435,  2011,  1782,  2436,
   -3989,  2345,  1486,  2437,  1805, -3989, -3989,  2347, 10060,  2409,
   -3989, -3989, -3989, -3989, -3989, -3989,  2215, -3989,  1022, -3989,
   -3989, -3989,  2096,  -165, -3989,   393,  2666, -3989,   118, -3989,
    2447,  1415,  5545, -3989, 16707,  1878, -3989,  2448,  2307, 12134,
    1486,  1486,  2449, 11778,  1664, -3989, -3989,   603, -3989, -3989,
   -3989, -3989,  8101, -3989,  2368, -3989, -3989,  1446, -3989,  2450,
    2514, -3989, -3989,  1486, -3989,  2451,  2345,  1486,  1486, -3989,
    1898,  2007,  2269, -3989, -3989,  2062,  1905, -3989,  1906, -3989,
   -3989, -3989,  2574, -3989,  2008,  5491,  1486,  1486,  2092, -3989,
    2092,  1668,  1668,  2092,  1668,  1668,  1486,  1668,  1668, -3989,
    1934, -3989,   140, -3989, -3989, -3989, -3989,  2397, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989,  1159, -3989,   146, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989,    49, -3989, -3989, -3989, -3989,
   -3989, -3989,  1822,   -87, -3989, -3989,   128, -3989, -3989,  2058,
   -3989, -3989, 14304,   253, -3989, -3989,  1676, -3989, 14304, -3989,
   -3989, -3989,  1207,  1932, 14872,    33,  1486, -3989,  2578,  2253,
      20,  1933, 16707, 16707, 15806, -3989, -3989,  1836,  1837, 16707,
   16707, 16707, 10060,  1839,  1938, 10060, -3989, -3989, -3989, 12237,
    2400, -3989, -3989,  1755, -3989, 16707, -3989, 10060, 16707, -3989,
   -3989,  1273, -3989,  2355, 16707, 16707, 16707, 16707, 16707, -3989,
    1755, -3989, -3989,  2297, -3989,  2142,  2301, -3989, -3989,  5497,
   -3989,  1486,  1410, -3989, -3989, -3989,  1411,   989,  1486, -3989,
   -3989, -3989, -3989, -3989, 16707,  2270, -3989,  1878, -3989, 16707,
    1668, -3989, -3989, -3989, -3989,  2082, -3989, -3989, -3989, -3989,
   -3989, -3989,  -169,  1753, -3989,  1849, -3989, 12134, -3989, -3989,
   -3989, -3989, -3989,  2218,  2471, -3989, -3989, 10669,   565,  6514,
   -3989,  1489,  1894,  1853,    86,    86,    86,    86, -3989, -3989,
   12134, 12237, -3989,  1660, -3989, -3989,  1317, -3989, -3989,  1860,
   -3989,   -47, -3989, -3989,  -198, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, 11041, -3989, -3989, -3989,  1549, -3989, -3989,
   -3989, -3989, -3989,  2311,   -36, -3989,  2532,  1319,  2453, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989,   -89, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989,  2092, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,   -89,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, 10191, -3989,   -89,   -89,
     -89,  1114, -3989,  1506,   126, -3989, -3989,  2108, -3989, -3989,
    2519,  2428,  2519,  2370,   166, 16707, -3989, -3989,   213,  6704,
   -3989, -3989,   119, 14081,  1114, -3989, -3989,  1961,  1022, -3989,
   -3989, 12237, -3989, -3989, -3989, -3989, -3989,  2044,  2037, -3989,
    1390, -3989,  2412,  2412,   562,  1975,  1922,  1971, -3989,  -306,
   -3989, -3989,  1980, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
    2011, -3989, -3989, -3989, -3989,  2380,  8020, -3989, -3989, -3989,
    2386, -3989, -3989, -3989,  2082,  2552, -3989, -3989,  1486,  2552,
    1486,  1881,    81,  1985, -3989, -3989,  1755, -3989,  1986, -3989,
   -3989,   633,  1988,  1586, -3989, -3989,  4196, -3989,  2712,  1209,
     144, -3989, -3989, -3989,  1676, -3989,   442, 12134, -3989, -3989,
    1010,   226,  1352, 16707, -3989, -3989, -3989,  1486, 12134, -3989,
    2602,  2459,  2463, -3989, -3989, 12237, -3989, -3989, -3989, -3989,
   10060, -3989, -3989, -3989, -3989, -3989,  2727,  2407, -3989, -3989,
   -3989,   465,  2547,  2470,  2547,   -11,   869, -3989,  1997,  2106,
    2157,  1931,  1405, -3989, -3989, -3989, 15046,  1405,  2669,  1676,
    1469,  1469,  1676,     5,  1886,  1676,  2791, -3989,  2276, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,   108,
    1553,  2006, -3989, 10107,  1486, -3989,  1974,  1974,  1418,  1668,
   -3989,  1668,  1974,  1668,  1668,  1974,  1668,  1668, -3989,  2669,
    2037,  2037,  2180,  1668, -3989, -3989, -3989,  1159, -3989, -3989,
   -3989, -3989,    32, -3989, -3989,   590, -3989, -3989, -3989, -3989,
    2382,    84,    84,  -211,  2012,  1720, -3989, -3989, -3989, -3989,
    2758, -3989, -3989, -3989,  2289, -3989, -3989, -3989, -3989,  2289,
     677, -3989,  1676, -3989, -3989, -3989, -3989, -3989,  1676, -3989,
   -3989,  1676, -3989,  1676, -3989, -3989, -3989, -3989, -3989,   110,
   -3989, -3989, -3989,    34, -3989, -3989, -3989, -3989, -3989, -3989,
     -23, -3989, -3989, -3989,  2825, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989,  2419,  2097,   765, -3989,  2705,  1108, -3989, -3989,
   -3989, -3989, -3989,  1720, -3989, -3989, -3989,  1923,  1918, -3989,
   10060,  1720,  2426,  2084,  2085,  2309, -3989, -3989, -3989, -3989,
   -3989,  2358, -3989, -3989, -3989, -3989, -3989,  2037,  2037, -3989,
     815, -3989,  1486,   330,  1151,  2046,   365,  2050, -3989,   387,
     649, 10060, -3989, -3989,   382,  2052,  2056,  2063,   416, -3989,
    1755, -3989,  2065, -3989,  1486,   421,  2067,  2037,  2531,   853,
   -3989,   -62,    64,  1022,  1447,  2069,   426, -3989,  2073,  2297,
    1252,  1252, -3989, -3989, -3989,  2092,  2211,  2079,   248, -3989,
   -3989,   901,  2858,  -187, -3989, -3989,  2214,  2241, -3989,  1360,
    1676, -3989, -3989,   653, -3989,  1892,   971, -3989, -3989, -3989,
    2479, -3989, -3989, 12134, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989,    78, -3989, -3989,  9413, -3989, -3989,  4262,   213, -3989,
   -3989, -3989, -3989,   -81, -3989,  1676, -3989,    66,   213, -3989,
   -3989, -3989, -3989,    -4,  1676, -3989, -3989, -3989,  6514, -3989,
   -3989,  1489, -3989, -3989,  1755,  1486, -3989, -3989, -3989, -3989,
   -3989, -3989,  2548,   853,  2551,  2452,  2022, -3989,  4356,  2338,
   -3989, -3989, -3989, -3989, -3989, -3989,  1983,  1549, -3989, -3989,
   -3989, -3989, -3989,  1763, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989,  1779, -3989,  1250, -3989,  1974, -3989, -3989, -3989,
    2658, -3989,  1763, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
    2249,  1763, -3989, -3989, -3989,  1486, -3989, -3989,  1486, -3989,
    1486,  1486,  1486, -3989,  2098, -3989, -3989,  1755, -3989,  2829,
   -3989, -3989, -3989,  1770, -3989,  1506,  3114, -3989, -3989, -3989,
    1486, -3989,  1486,   142,   383,  2687, -3989, -3989,   827, -3989,
   -3989, -3989, -3989, 12134, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989,  1022,  1022,   853,
    2561,  1881,  2082,  1922,  2526, 13611,    15,  4541,  1486,   248,
    2099, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,   465,
    2386,  1486, -3989, -3989, -3989, -3989,  1486,  1104,   229, -3989,
    2014, -3989,  2018, -3989,   465,    10, 10060,  2360,  1355,   462,
   -3989,   815,  2371, -3989, -3989, -3989, 12134,  1586,  1586,  1586,
    1586,  1586,  1586,  1586,  1586,  1209, -3989,    82,   971,   940,
   -3989,  2151,  2151, -3989, -3989, -3989, 16707, 16181,  1352,  -260,
   -3989,  2727, -3989,  1486,  1486,   853,  2569,  2092,  2110, -3989,
    2911,  1486,   835, -3989, -3989,  2082,  2917, -3989, -3989,  1486,
   -3989,  2254,  2692, -3989, -3989, -3989,  2114,  2225,  2236,   269,
   -3989,  2051, -3989,  2587,  1486,  1234, -3989, -3989,  -327,  -286,
     651,   683,   741, -3989,  2040, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989,  7300, -3989,  2260,  1486, -3989,   224, -3989,  2492, -3989,
   -3989,  1486,  2913,  2543, -3989, -3989, -3989,   -28,  1763, -3989,
   -3989, -3989,  1676, -3989, -3989, 15215, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
    1135,   718,   275,  2598, -3989,  2092,  1688,   269,   269,  2042,
    -164,   865,  2092,  2064,  1676, -3989, -3989, -3989,   -37,  1958,
   -3989, -3989, -3989, -3989,  1886,  2426,  2011,  1763, -3989, -3989,
   -3989,  2228,  2426,  1676,  2850,   221,    52,    12,  1881, -3989,
   -3989, -3989,  1676,  1676, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989,  2195, -3989,  2502, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,  2245,  2916,
     269, -3989,  1668,  1668, -3989, -3989,  1974, -3989,  1668,  1668,
    1668,  -256,  1668, -3989,  1668,  1159, -3989,  1822,  1676,  1676,
    2055, -3989,    68, -3989,   654,  -265,   459, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989,  1740,  1486,   201, -3989, -3989,
   -3989,   253,   253, -3989, -3989, -3989, -3989,   253,   253,   677,
     253, -3989, -3989, -3989,  1676,   827, -3989,   827, -3989,   677,
    2526,  1676,  2158,   690,  2518,  2518, -3989, -3989, -3989,  1720,
   -3989, -3989, -3989, -3989, -3989, -3989,   148, -3989,  1668,  1668,
   -3989, -3989, -3989,  1957, 16325,  1802, 16436,  1802, -3989,  2159,
   -3989, -3989,  1486,  1802,  1802,  1802, 10060, -3989,  1957,   729,
    1802,    20, -3989, -3989, -3989,  2431,  2235,   -45,  2624,   853,
   16583,  1802,  1802,   489, -3989,  2211, -3989,  1022, -3989, -3989,
   -3989,  2412, -3989, -3989, -3989, -3989, -3989,  2466, -3989, -3989,
   -3989,   919, -3989, -3989, 16707, -3989, -3989, -3989, -3989,  2438,
    2575,   884,  1894,   931, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989,   253, -3989, -3989,   253, -3989, -3989,
   -3989, -3989,   808,  2745,   253,   827,   827,   253, -3989, -3989,
   -3989,  2926,  2926,  1022, -3989,  1022, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989,  2961, -3989,  2183,    -5,  4356, -3989,
   -3989, -3989,  2710, -3989, -3989, -3989,  1486, -3989, -3989, -3989,
    2757,  2104,  1348,   193,  2105, -3989, -3989, -3989, -3989, -3989,
     489, 10060, -3989, -3989,  2914, -3989,  1437, -3989, -3989,  3114,
   -3989,  1437,  2662,  2664,  2833,   -29, -3989, -3989,  2277, -3989,
   -3989,  2426,    23, -3989, -3989, -3989, 12134,  1022, -3989,  1022,
     198,  1668, -3989,  1486, -3989, -3989,    31, -3989, -3989, -3989,
    2992, -3989,  2661, -3989, -3989, -3989,   286,   203, -3989, -3989,
   -3989, -3989,  2469,  2769,   971, -3989,  1432, -3989, -3989,  5245,
   -3989,  2018,  2348, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, 12134, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989,     1, -3989,  1486, -3989,
   -3989, -3989,  1569, -3989, -3989, -3989, 16707, -3989, 12134, 12134,
    1151, -3989,  1336,  -186,  2418, 11807,  1957,  1957, -3989,  1022,
    2191, -3989,   489, -3989,  2454, -3989, 10060, -3989,  2810,  2222,
   -3989,   229, -3989,  1281,  2833,  2517,  2129, 10060,  2924,   869,
   -3989,  2201,  2310, -3989,  1285,  1676, -3989,  2217, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989,  2100,  8200, -3989, -3989,  2190, -3989,  2944,  2181,
    2221,  -150, -3989, -3989, -3989,  1486, -3989, -3989, -3989, -3989,
    2124,  9186, -3989,  3001, -3989,  2727, -3989,  2266,  2318,  2318,
    2131, -3989,  1285, -3989,  2230,  2709, -3989, -3989, -3989,  2042,
   -3989, -3989, -3989, -3989, -3989, -3989,  2595,    59,  2526,   839,
    1676, -3989, -3989,  1676, -3989,  1676,  1676,  2426,  1725, -3989,
    1676,   -73,  1676,  1676,  1676,  1676, -3989,  2316, -3989,   198,
     865,  2092,  1676,  1886,  2497, -3989,  2320,   865,  1668,  1668,
    1668,  1668,  1668,  1668, -3989,  2456, -3989, -3989,  3033, -3989,
   -3989, -3989, -3989, -3989, -3989,  1676,  2321,  2503, -3989, -3989,
   -3989,  2864, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989,   884, -3989,  1762,
   -3989, -3989,  1486,   462, -3989, -3989,  2251, -3989, -3989, -3989,
    1676,   783, -3989, -3989, -3989, -3989,  -158, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989,  2720, -3989, -3989,
   -3989,  2715, -3989, -3989, -3989, -3989, -3989, -3989,  2716, -3989,
   -3989,  1457, -3989, -3989, -3989, -3989, -3989, -3989, -3989,  1660,
    2860, -3989,  1353, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989,   213,   213,   213,   213, 12134, -3989,   931, -3989,  7451,
   -3989, -3989, -3989, -3989,  2037, -3989, -3989, -3989, -3989, -3989,
   -3989,  1334,   253,  2564, -3989, -3989, -3989, 14649, -3989, -3989,
   -3989,   397, -3989, -3989, -3989, -3989,  3886, 14649,   853,  2420,
     853,  2421,    70,  4356, -3989, -3989, -3989, -3989, -3989,  2961,
   -3989,   489, -3989, -3989, -3989, -3989,  1348, -3989,  2884, -3989,
   -3989,  1763, -3989,  1437, -3989, -3989,  1437,   489,  2248,  2248,
   -3989,  3061,  3021, -3989, -3989, -3989,  2526, -3989,  2584,  2875,
     272, -3989, -3989,  2521, -3989, -3989,   853,  2515,  2515,  2523,
   -3989,  1093, -3989,  2831, -3989, -3989, -3989,  1486, 12134,  1974,
    2629,  2673, -3989,   897, -3989, -3989, -3989,  -126, -3989, -3989,
   -3989,  2908,  2576, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
    2633, -3989, -3989, -3989,  2650, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989,  1151, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989,  2555,  2272,  1676, -3989, -3989, -3989,  -158,  2720,   853,
    2224, -3989, -3989,  2911, -3989,  2526,  2833,  2526,  -186,  1534,
   -3989, -3989,  1820, -3989, -3989,  1881,  2489, -3989,   108, -3989,
     869, -3989,   869, -3989,  2292, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989,  2302, -3989,  1285, -3989,  2027,  1486,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,   975,
   -3989, -3989,  2656, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989,  2659, -3989, -3989,
    2526,  2777,  2312,  2092,  2312,  2387,  2209, -3989, -3989, -3989,
   -3989, -3989, -3989,  2670, -3989,   884,   395, -3989, -3989,  2850,
   -3989, -3989, -3989,  1285,  2092,   251,  1486, -3989, -3989, -3989,
   -3989,  2092, -3989,  1751, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989,   891,   891,   520,  1486,  1676, -3989, -3989,  3696,
   -3989,  1486, -3989, -3989,   865, -3989,  1486,  1486, -3989, -3989,
   -3989, -3989,  3015,  1418,  1486,  1676,  1486,   -87, -3989,   865,
    2326,  1668, -3989,  2092, -3989,  1159,    93,  1199,  1264,  1676,
   -3989, -3989,  2223,  2315, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989,   462, -3989, -3989,  2747,  2934, -3989,  2022,
   -3989, -3989, -3989,  2926, 12134, 12134, 12134, 12134, -3989, -3989,
   -3989, -3989, -3989,  1486, -3989,   213, -3989, -3989, -3989, -3989,
    2328,  -325, -3989, -3989,  1022, -3989,  1022,  5301, -3989,  1224,
      26, -3989, -3989,   482, -3989, -3989, -3989, -3989, -3989, -3989,
    3059,  2946, -3989, -3989,  1437, -3989, 12134, 12134, -3989, -3989,
    1486,  2584,  2037,  2364, -3989,  2538,  1668,  1003,  1486, -3989,
   -3989, -3989, -3989, -3989, -3989,   112, -3989, -3989, -3989, -3989,
   -3989,  2636, -3989,   505, -3989, -3989,  2637,  1349, -3989, -3989,
   -3989,  3070,  2690, -3989,  1676,  1510, -3989, -3989,   826,  2691,
    2693, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,  1486,
   -3989,  2934, -3989, -3989, -3989,  2340, -3989,  1486, -3989,  1486,
   -3989, -3989, -3989, -3989, -3989,  2618,  2883, -3989, -3989, -3989,
   -3989,   139,   966,  2550, -3989, -3989, -3989, -3989, -3989,   376,
     977, -3989, -3989,  2037, -3989, -3989,  1285, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,  2346,
    2252,  2092,  2349, -3989,  2963, -3989,  2964,  1676, -3989, -3989,
   -3989, -3989,  2403, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989,  1486,    67,  3015,    94, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989,  1486, -3989, -3989, -3989, -3989,
    2824, -3989, -3989, -3989, -3989,  3159, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989,   207,   410, -3989, -3989, -3989, -3989, -3989,
     175,   646, -3989, -3989, -3989,  1486,  1200,  1676, -3989,   595,
    1688,  2367,   865, -3989, -3989,  2676,  3154, -3989, -3989, -3989,
    1124,  -178, -3989, -3989, -3989, -3989,   462,  2747,  -158,     3,
   14649, -3989, -3989, -3989, -3989, -3989,  1486,   213, -3989, -3989,
    -158,  -158, -3989, -3989, 12134, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, 12134, -3989, -3989, -3989, -3989, -3989,  2364,  1486,
    1486,  1167,  1676,  1668,  1668,  2566, -3989, -3989, -3989,  2611,
   -3989, -3989, -3989, -3989, -3989, -3989,  2499, -3989, -3989, -3989,
   -3989,  4699,  2323, -3989,  1486, -3989, -3989,  2469,  2769, -3989,
   -3989, -3989, -3989,  -158,  1287, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989,  2685,   139, -3989,   163,  2278, -3989,
   -3989,  1676, -3989,  2557,   755, -3989,  2464, -3989,  2362,  1486,
   -3989, -3989, -3989, -3989,  2092,  2798,  1974,  1974,  1237,  2092,
   -3989,   865,   865, -3989, -3989, -3989, -3989,  2859, -3989, -3989,
    2230,  2092,   646,  1486,  1773, -3989, -3989, -3989, -3989,  1486,
   -3989, -3989, -3989, -3989, -3989,  1676, -3989,  2612, -3989, -3989,
   -3989,   665,  -182,   665, -3989,  1688,  1676,  1676,  1688,   865,
   -3989,  1676,  1676, -3989, -3989,  2717, -3989,   185,  2388,   462,
   -3989,   -46,   151, -3989, 12134, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989,  1486, -3989,  1676,  1486, -3989, -3989, -3989,
   -3989, -3989,  2653,  2909,   248,  1668,  1676, -3989, -3989, -3989,
   10060, -3989, -3989, -3989, -3989,  5245, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989,  1917, -3989, -3989,
   -3989,  1662, -3989,  1776,  2586,  3010,  1676,  2711, -3989, -3989,
   10060, -3989, -3989, -3989,   248,   -83,  1664, -3989, 12748, -3989,
   -3989,   755,  2389,  2391,  1676, -3989, -3989,  1237,  1486,  1486,
    2727, -3989,  2815, -3989,   383,  2426,  1332, -3989, -3989, -3989,
    3170,  3015, -3989,     2, -3989, -3989, -3989, -3989, -3989,     2,
   -3989,  1486,  1486,  -109, -3989, -3989,  2524, -3989,  2740, -3989,
   -3989,  1688,  1688, -3989, -3989, -3989, -3989,  2473,    -9,   462,
   -3989, -3989, -3989, -3989, -3989,  2677,   232,  2037, -3989, -3989,
   -3989, -3989, -3989,  1486, -3989,  1167, -3989,   156, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989,   248, -3989,  1755, -3989,
   -3989,  2562,  2472, -3989,  2408,   274,  1974, 10060,  2037,  2377,
   -3989, -3989, -3989, -3989,   709, -3989,  2405,  1780,  2413, -3989,
   -3989,   376, -3989,  1237, -3989, -3989, -3989, -3989,  1676,   383,
     865,  3030,  2612,  1166,  1676, -3989, -3989, -3989, -3989, -3989,
    2674, -3989, -3989,    93,  1199, -3989, -3989,  1676,  3039, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989,  1486, -3989, -3989,
   -3989, -3989, -3989,  1486, -3989,  1786,  1668,  2416, -3989, -3989,
   -3989,   133,  2738,  1486,  2037,  2455,  1215,    51,  1440,   136,
   -3989, -3989,  3301, -3989, -3989, -3989,  2055, -3989, -3989, -3989,
   -3989, -3989,  1974, -3989,  2460, -3989,  3011,  1486,   -95,  2742,
    3223,   677,  2439, -3989, -3989, -3989, -3989,  2639, -3989,  1668,
    1456,   133, -3989, -3989,  1676, -3989,  1486, -3989,  1676,  1486,
     -80, -3989, -3989, -3989, -3989,  2713,  2957,    77,  2834,  2836,
    3048,  2821, -3989,    51, -3989,  2102,   980,  2441,   191, 15650,
     827, -3989, -3989, -3989,  2055, -3989,  1486,  2357, -3989, -3989,
   -3989,  1484, -3989, -3989,   666,  1676,  1676, -3989, -3989, -3989,
    1974, -3989, -3989, -3989,  2707,  1486, -3989, 10060, -3989,   200,
    1486, -3989,  1676,  1386,  -121,   -80, -3989, -3989,  1215, -3989,
   -3989, -3989, -3989, -3989, -3989,  1676,  2718,  1486,  1676,  1676,
    1676,  1676, -3989,  2844,    29,  2846, -3989,  2832, -3989,  2194,
   -3989, -3989,  1486,  3102,  1519,  2856,    61,  2857,  2842, -3989,
    1033, -3989, -3989,  1486,  2483, -3989,   269,   269,  1921, -3989,
   -3989, -3989, -3989,  2927,  3161,   -40, -3989,  1676, -3989, -3989,
   -3989, -3989,   670, -3989,  2289,  2289,  1676,   213,    66,  1676,
    8020, -3989, -3989, -3989, -3989,  2289,   213, -3989,  3052,  2092,
   -3989,  3175, -3989, -3989,   213, -3989, -3989,  1486, -3989, -3989,
    1486, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989,  2930,  2289, -3989, -3989, -3989,  6704, -3989, -3989,
      95,   666, -3989, -3989,  1688,  1688,   -58, -3989, -3989, -3989,
   -3989, -3989,  1289, -3989, -3989, -3989,  1289,  1289, -3989, -3989,
   -3989, -3989, -3989, -3989,  2721, -3989, -3989, -3989,  1486,  2092,
   -3989,  1486,  1486,  1486,  1486,  1676,  1676,  1676,  1676,  1676,
   -3989,  1486,  1676,  1676,  1676,  1676,  1676,  1676,  1676,  1676,
    1676,  1676,  1676, -3989,  1486,  1676, -3989, -3989,   269,   269,
     727,  2847, -3989,  1676, -3989, -3989,  1486,  1486, -3989, -3989,
     253, -3989, -3989, -3989,  1676, -3989,  1676,   253,   827, -3989,
   -3989,   253, -3989, -3989,   253,  2312,  1676,   827, -3989, -3989,
   -3989, -3989, -3989,  6704,  1494, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989,  1668, -3989, -3989, -3989,
   -3989,  -121, -3989,  2312, -3989, -3989, -3989, -3989,  1486,  1486,
    1486,  1486,  1486,  1486,  1486,  1486,  1486,  1486,  1486,  1486,
    1486,  1486,  1486,  1486,  1486,  1486,  1486, -3989, -3989, -3989,
   -3989, -3989,  1540, -3989, -3989, -3989, 13383, -3989,   884,  1486,
   -3989, -3989,   813,   813, -3989, -3989, -3989, -3989, -3989,   195,
   -3989,   884, -3989, -3989, 15980, -3989, -3989,  2815, -3989, -3989,
   -3989, -3989, -3989,  1486, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989,  1486, -3989,  1864,   798,   832,
   -3989, -3989,   727,  2476, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989,   253, -3989, -3989,   253, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989,  1486,  1486,  1766,  1676,
    1676,  1982,  1676, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
    1914, -3989, -3989, -3989, -3989,  1486, -3989, -3989, -3989,  1676,
     727,   727, -3989,  2921,  1676,  1676,   727, 14432,  1486,   727,
   -3989, -3989, -3989,   727,   727, -3989, -3989, -3989, -3989,  2898,
    1838,  2791,  1676,  2092, -3989,  1676,  2037, -3989,   329,  1486,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989,   141, -3989, -3989,   938, -3989,   789, -3989,
   -3989, -3989, -3989,  1838,  1486, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989,   269, -3989,  1704,  2349, 17186, 17186,  1130,  2947,
    2787,  2787,  1973,  5245, -3989, -3989,   938,   179, -3989, -3989,
   -3989,  2092,   179, -3989,   173,  1486, -3989, -3989, -3989, -3989,
   -3989, -3989,  2092, -3989,  2312,  1957, 16843, -3989, -3989,  1691,
    1703, -3989, -3989,  1705, -3989, -3989, -3989, -3989,  1041,  1041,
   -3989, -3989, -3989, -3989, 17186, -3989,   984,   984,  2787,  2787,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,   269, -3989,
    1486, -3989,  2964, -3989,  1974,  1486, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989,     7,   132,  3215, -3989, -3989,
   -3989,   984, -3989, -3989,  2722,  2724, -3989, -3989,  2533,    22,
   -3989,  2739, -3989,  2739, -3989,  2739, -3989,  2739, 17186, -3989,
   -3989, -3989,  2092, -3989, -3989, -3989, -3989,  2726, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_int16 yydefact[] =
{
       2,     0,    12,     1,     3,     5,    27,     4,    53,    31,
      30,    27,     8,    10,    11,     0,    28,     0,     0,    13,
      76,     9,    35,    32,    53,    53,     0,    56,     0,    24,
      79,     0,     0,    16,     0,    29,  3035,    54,    62,     0,
     430,     0,   245,    81,     0,     0,    20,    14,    17,    18,
      22,    15,  3036,     0,     0,    64,    77,     0,    25,   433,
     431,     0,     0,    75,   247,     0,     0,     0,  3102,  3035,
    3035,  3035,     0,     0,     0,     0,  3035,     0,     0,  2995,
     242,   159,    78,    82,    83,    85,    86,    89,    87,    88,
       0,   142,   145,   146,   147,   208,   148,   150,   149,   151,
     152,   153,   154,   155,   156,   157,   158,    38,    37,    41,
      41,     0,    19,     0,    58,    59,    60,    61,    57,  3066,
    3035,    69,     0,  1336,     0,   435,    80,     0,     0,   249,
    2667,  2666,   170,   227,  3035,  3103,  3035,     0,     0,     0,
    3035,  3035,    96,   126,     0,    90,   140,  2996,     0,     0,
    3035,  3035,    84,   144,   141,   143,     0,   207,     0,     0,
      33,    40,    39,     0,     0,  3067,  3035,     0,     0,     0,
     432,     0,    26,  1331,  1384,     0,   428,   246,   248,   387,
     176,  3016,  3035,     0,     0,     0,  2773,   238,  2655,   236,
     241,     0,     0,    98,   128,   240,    92,   574,   218,   219,
    3035,  2668,   210,   211,  3040,   214,     0,  2782,  2204,  2203,
     160,   164,   167,  3081,  3035,     0,   209,    42,    36,  3035,
      21,    23,     0,    66,    68,    67,    65,  3035,    55,  1332,
       0,     0,   434,   441,   442,   555,   436,   558,     0,     0,
    2870,   250,   244,   390,   171,   172,   174,     0,     0,   228,
     229,   239,   234,  3154,  3155,     0,   232,     0,  2994,  3109,
    3092,  3035,   124,    97,  3091,   102,   104,   105,   106,   107,
    3091,     0,  2997,     0,     0,   127,     0,   131,    91,    94,
     220,     0,   212,  3042,  3041,   215,     0,   243,  3081,  3084,
    3083,     0,     0,   161,   165,     0,     0,  2877,  2878,  2879,
    2880,  2881,  2882,  2883,  2884,    63,     0,  1601,     0,  1453,
    1566,  1576,  1584,  1591,  1647,  1653,  1673,  1668,  1674,  1683,
    1679,  1691,  1701,  1832,  1841,  1843,  1849,  1882,  1894,  1905,
    1908,  1911,  1903,  1918,  1929,  1951,  1955,  1959,     0,  2015,
    2017,  2023,  2027,     0,  2033,  2067,  2094,  2096,  2101,  2131,
    2132,  2148,  2151,  2152,  2157,  2166,  2167,  2180,  2193,  2232,
    2250,     0,  2287,  2303,  2312,  2314,  1366,  2318,  2321,  2324,
    2341,  2380,     0,  1386,  1387,  1388,  1389,  1390,  1391,  1392,
    1393,  1395,  1394,  1396,  1398,  1397,  1399,  1400,  1401,  1402,
    1403,  1404,  1405,  1406,  1407,  1408,  1409,  1410,  1411,  1412,
    1413,  1414,  1415,  1416,  1417,  1418,  1419,  1420,  1421,  1422,
    1423,  1424,  1425,  1426,  1427,  1428,  1429,  1430,  1431,  1432,
    1433,  1434,  1435,  1436,  1437,  1438,  1439,  1440,  1441,  1442,
    1443,  1444,  1445,  1446,  1447,  1448,  1449,  1450,  1385,     0,
     502,   437,  2853,     0,  2637,   438,   389,     0,  2871,     0,
       0,   417,  3076,   400,   388,     0,   393,   395,   396,   408,
     397,   398,  3035,  3035,   177,   178,  2791,  2787,  2792,  2790,
    2788,  2793,  2789,   230,   223,   225,  3134,   233,     0,  2774,
     237,  3110,  3035,     0,   101,   103,    99,   125,  3091,  3035,
    2998,   109,     0,     0,   138,    41,     0,    41,     0,   129,
     132,     0,     0,     0,  2802,  2798,  2803,  2801,  2799,  2804,
    2800,   221,  2794,  2796,  2783,   213,   216,     0,  3082,   168,
     162,   163,   166,    34,    45,    48,    52,    51,  3091,    46,
      47,    71,    72,    73,    74,    70,  1605,  1608,  1607,  1603,
    1604,  1606,  1595,  1602,  2895,  2896,  2897,  2898,  2899,  2900,
    2901,  2902,  2903,  2904,  2905,  2906,  2907,  2893,  2947,  2948,
    2949,  2950,  2951,  2952,  2953,  2954,  2955,  2956,  2957,  2958,
    2959,  2960,  2961,  2962,  2963,  2964,  2965,  2966,  2967,  2968,
    2969,  2908,  2909,  2911,  2910,  2912,  2913,  2914,  2915,  2916,
    2917,  2918,  2920,  2919,  2921,  2922,  2923,  2924,  2925,  2926,
    2927,  2928,  2929,  2930,  2931,  2932,  2933,  2934,  2935,  2936,
    2937,  2938,  2939,  2940,  2941,  2891,  2942,  2943,  2944,  2945,
    2946,  1452,  2892,  2894,  1484,     0,     0,     0,  1601,     0,
       0,     0,  1676,     0,     0,  1696,  1744,     0,  1696,     0,
    1601,  2559,  1886,  1896,     0,     0,  3125,  1632,  1631,     0,
    1917,     0,     0,     0,     0,     0,  1998,  2010,     0,     0,
       0,     0,  1451,  2038,  2546,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2283,  2286,  2270,
    2284,  2551,  2285,  2272,  2281,  2273,  2282,  2759,  2763,  2290,
       0,  2313,  2311,     0,  1384,     0,     0,     0,     0,     0,
    2396,  2448,     0,     0,   805,     0,     0,     0,   560,     0,
       0,   564,   565,   563,     0,   440,   443,  2872,   251,  3076,
    3125,  3125,  3074,  3076,  3076,  3125,  3076,  3076,  3125,  3076,
    3076,  3013,  3077,  3011,     0,   401,   402,   403,  2976,     0,
     391,   394,     0,     0,     0,   224,   222,     0,     0,     0,
     118,   100,  2775,   110,   133,   134,   137,   139,   135,   136,
     130,    93,     0,  2795,     0,   217,   169,    50,    44,    49,
    1596,  1338,  1483,  2773,  1564,  1455,  1496,     0,  3064,  1482,
    3074,  2829,  2827,  2830,     0,  2823,  2831,     0,     0,  2837,
    3171,  3172,     0,  2620,  2622,     0,     0,     0,  2826,  2692,
       0,  2624,  2828,  2832,  2833,     0,     0,  2825,  2837,  2824,
    1574,  2684,  1572,  2676,  2679,     0,  2678,  2682,  2683,  2834,
       0,     0,  2693,     0,     0,     0,  1577,     0,  2609,  2612,
    2614,  2617,  2702,     0,  2619,  2859,  2700,  2701,  2652,  1585,
    1586,     0,  2648,  2650,  2649,  1645,  2546,  2731,  1652,  1648,
    1649,  1651,  2730,  1664,  1654,  1655,  1656,  1659,  3074,  1671,
    2767,     0,  2626,  2873,  2671,  2766,  2771,  2672,  1677,  1675,
       0,  1689,  3095,  3001,  1680,  2764,  1682,  3119,     0,  1698,
    1700,  1692,     0,  1741,  1775,  1774,  2798,  2989,  1721,  1773,
    1766,  1772,  1765,  1830,  2473,  2674,  1716,  1718,  1708,  1709,
    1722,     0,  1710,  1711,  1762,  1712,  1713,  1775,  1715,     0,
    2678,  1839,     0,  1842,     0,     0,  1844,  1856,  1855,  1880,
       0,  1852,  1854,  2558,  3035,  1887,  1883,  1888,  1898,  1902,
    1900,  1903,  1901,  1895,  1906,  1907,  2669,  1909,  1910,  3126,
    1912,  2646,  1904,  2557,  1923,  2556,  1930,  1932,  2639,  1952,
    1953,  1027,  1781,     0,     0,  1956,  1026,  1960,     0,  1962,
    1963,  1964,     0,     0,  2016,  2236,  2751,  2752,  2868,     0,
    2021,     0,  2024,     0,  2031,     0,  2039,  2034,  2035,     0,
    3020,  2068,  2080,     0,  2638,  2095,     0,  2097,  2099,  2129,
    2865,  2146,     0,  2149,  2383,  2629,  2155,  3095,     0,  2164,
    2630,  2383,     0,  2178,  2171,  2632,  2181,  2184,     0,     0,
    2642,  2194,  2195,  2196,  2197,  2198,  2199,  2223,  2200,  2226,
    2201,  2202,     0,     0,  2640,     0,     0,  2750,  2771,  2233,
    2268,  2255,  2274,  2550,     0,  2761,  2762,  2301,     0,     0,
       0,     0,  2309,     0,  2315,  2316,  1372,  1378,  1367,  1368,
    1369,  1371,     0,  2319,     0,  2754,  2322,  3097,  2733,  2339,
    2327,  2732,  2734,  2342,  2343,  2394,  2383,     0,     0,   556,
       0,     0,   808,   607,   610,     0,     0,   561,     0,   571,
     572,   566,   573,   569,  3035,     0,     0,     0,     0,  3075,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  3014,
    3117,  3012,     0,   307,   425,   308,  2977,  3015,   392,   183,
     182,   203,   199,  2775,   204,   188,   202,   200,   180,   181,
     201,   173,   179,   190,   191,   193,   185,   186,   187,   175,
     184,   331,   231,   226,   235,     0,   121,   123,   122,   119,
     120,  2660,     0,  3015,    95,  2797,     0,  1341,  1339,  1360,
    1565,  1454,  1496,  2984,  2986,  1501,  3035,  1480,  1497,  1498,
    1500,  1502,     0,     0,  3003,     0,     0,  3065,     0,     0,
       0,     0,     0,     0,     0,  2821,  2843,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2822,  1575,  1567,     0,
       0,  2677,  2685,  2686,  2687,     0,  2809,     0,     0,  2616,
    2699,     0,  2615,  2861,     0,     0,     0,     0,     0,  2703,
    2704,  2705,  2860,  1580,  1587,  1589,     0,  1646,  1592,  1611,
    1650,     0,  1659,  3167,  3168,  1657,     0,  1660,     0,  1672,
    1669,  3149,  3148,  2627,     0,  2875,  2628,  2769,  2770,     0,
    1686,  1687,  1690,  1684,  3096,  2114,  3002,  1681,  2765,  3120,
    1697,  1699,  1694,  1722,  1776,     0,  2990,     0,  1831,  1702,
    1381,  1381,  1707,  2479,  2476,  1717,  1714,  2675,  3133,     0,
    1743,     0,  1777,     0,  2473,  2473,  2473,  2473,  1840,  1833,
       0,     0,  1845,  1595,  1881,  1850,  2559,  1867,  1851,  1858,
    1859,  1381,  2575,  2573,  3036,  2579,  2576,  2568,  2572,  2570,
    2571,  2567,  2569,  2560,  2561,  2574,  2563,     0,  1889,  1884,
    1899,  1897,  2670,     0,  1915,  1924,  1925,  1934,     0,  1954,
    1780,  1107,  1136,  1104,  1186,  1121,  1120,  1185,  1187,  1209,
    1188,  1172,  1255,  1289,  1205,  1234,  1208,  1231,  1277,  1180,
    1203,  1199,  1206,  1229,  1275,  1106,  1109,  1216,  1213,  1105,
    1212,  1211,  1261,  1133,  1215,  1134,  1290,  1138,  1198,  1227,
    1224,  1251,  1242,  1279,  1078,  1252,  1262,  1225,  1160,  1162,
    1161,  1228,  1263,  1264,  1265,  1266,  1124,  1125,  1254,  1217,
    1219,  1218,  1223,  1158,  1239,  1132,  1241,  1248,  1249,  1140,
    1142,  1253,  1145,  1084,  1237,  3021,  1183,  1159,  1131,  1101,
    1260,  1100,  1103,  1102,  1258,  1250,  1226,  1210,  1271,  1246,
    1247,  1182,  1268,  1269,  1270,  1259,  1274,     0,  1135,  1236,
    1232,  1235,  1267,  1222,  1233,  1141,  1174,  1204,  1200,  1196,
    1207,  1230,  1272,  1273,  1240,  1143,  1144,  1108,  1276,  1137,
    1181,  1139,  1220,  1221,  1257,  1173,  1175,  1077,  1146,  1163,
    1184,  1256,  1288,  1214,  1197,  1238,  1179,  1202,  1201,  3021,
    1037,  1052,  1053,  1054,  1055,  1056,  1057,  1058,  1059,  1060,
    1061,  1062,  1063,  1064,  1065,  1066,  1957,  1294,  3021,  3021,
    3021,  1958,  1298,     0,  1985,  1969,  1961,  1966,  1967,  1968,
    2008,     0,  2008,     0,  2240,     0,  2753,  2869,  3037,  2019,
    1030,  1032,  3043,     0,  2020,  2022,  2018,     0,     0,  2032,
    2028,     0,  2036,  2043,  2040,  2042,  2041,  2044,  3074,  2082,
    2652,  2736,  2548,  2548,     0,  2078,     0,     0,  2735,  2649,
     577,  2737,     0,  2547,  2100,  2098,  2130,  2102,  2866,  2867,
    3095,  2147,  2133,  2135,  2136,     0,     0,  2150,  2156,  2153,
    2104,  2631,  2165,  2158,  2114,  2173,  2179,  2168,     0,  2173,
       0,  3133,  2182,     0,  2719,  2725,  2726,  2727,     0,  2224,
    2227,     0,     0,     0,  2641,  2206,     0,  2205,     0,     0,
    2769,  2269,  2251,  2257,  3035,  2258,  2253,     0,  2271,  2276,
       0,  2604,  2602,     0,  2760,  2302,  2288,     0,  2291,  2292,
    2295,     0,     0,  2310,  2304,     0,  2317,  1373,  1377,  1370,
       0,  3098,  3099,  2323,  2340,  2325,  2991,     0,  2344,  2395,
    2381,  2385,  2446,     0,  2446,  2452,   558,   503,     0,     0,
     811,     0,   666,  2853,   562,   568,  3035,   575,  2999,  3035,
       0,     0,  3035,  2999,  3066,  3035,  2974,   439,     0,   444,
     447,   448,   449,   450,   451,   452,   453,   454,   455,     0,
       0,     0,   252,  3095,   404,  2653,  3076,  3076,     0,   412,
    2633,   405,  3076,   406,   414,  3076,   407,   416,  3118,  2999,
    3074,  3074,     0,     0,   189,   192,   195,     0,  3162,  3164,
    3163,   108,  2777,  2776,   111,     0,  2775,  1599,  1600,  1598,
       0,  1346,  1346,     0,     0,  2460,  2740,  1513,  2738,  2739,
       0,  1499,  3145,  3144,  3070,  3147,  3146,  1511,  1512,  3070,
       0,  1517,  3035,  1531,  1532,  1533,  1519,  1521,  3035,  3004,
    1522,  3035,  1563,  3035,  1524,  1527,  1525,  1526,  1528,     0,
    1557,  1558,  1535,  1537,  3094,  1538,  1561,  1559,  1560,  1529,
    3127,  1540,  1530,  1518,  2972,  1542,  1562,  1545,  1503,  1534,
    1539,  1544,     0,     0,     0,  2712,     0,  1491,  1495,  1494,
    1485,  1481,  1476,  2460,  3141,  3140,  1473,  1464,  1466,  1467,
       0,  2460,  3050,     0,     0,     0,  1509,  1457,  1462,  1461,
    1471,     0,  1479,  1459,  1478,  1460,  2716,  3074,  3074,  2715,
       0,  2689,     0,  2604,  2602,     0,  2604,     0,  2839,  2604,
       0,     0,  2621,  2623,  2604,     0,     0,     0,  2604,  2696,
    2697,  2698,     0,  2625,     0,  2604,     0,  3074,  2767,  2500,
    1573,  2771,  2672,     0,     0,     0,  2604,  2618,  2863,  1580,
    2608,  2607,  2611,  2610,  2613,     0,  1582,     0,     0,  2651,
    1593,     0,  1609,  1666,  1658,  1663,     0,     0,  2673,  2500,
    3035,  2874,  2768,     0,  1688,  3015,  2536,  2115,  2116,  1693,
       0,  1742,  1767,  1751,  2478,  1382,  2481,  2474,  2480,  2475,
    2477,     0,  1731,  1730,  1719,  1726,  1728,     0,  3037,  1818,
    1819,  1820,  1807,     0,  1810,  3035,  1811,  3007,  3037,  1814,
    1815,  1821,  1816,  3127,  3035,  1817,  1824,  1822,  1723,  1724,
    1750,  1745,  1746,  1748,  1749,     0,  1763,  1770,  1705,  1706,
    1703,  1704,     0,  2500,     0,     0,  1615,  1853,     0,  1867,
    1860,  1857,  1862,  1863,  1869,  1861,     0,  2578,  2562,  2589,
    2590,  2591,  2580,  3125,  2597,  2600,  2599,  2601,  2593,  2586,
    2588,  2587,  2592,  2594,  2596,  2598,  2564,  2581,  2582,  2583,
    2584,  2585,  1486,  2645,  1915,  2643,  3076,  1913,  2647,  1926,
    1927,  1381,  3125,  1942,  1943,  1945,  1947,  1948,  1944,  1946,
    1937,  3125,  1933,  3022,  3023,     0,  1036,  1293,     0,  1295,
       0,     0,     0,  1299,     0,  2772,  2721,  2722,  1986,     0,
    1988,  1987,  1989,  1971,  1981,     0,     0,  1965,  2009,  1999,
       0,  2011,     0,  2242,     0,     0,  3038,  3039,     0,  1031,
    3045,  3044,  3046,     0,  1072,  1195,  1156,  1099,  1115,  1166,
    1085,  1189,  1164,  1113,  1081,  1194,  1281,  1191,  1177,  1110,
    1178,  1176,  1147,  1149,  1152,  1111,  1165,  1119,  1168,  1117,
    1157,  1154,  1070,  1169,  1190,  1079,  1123,  1244,  1282,  1093,
    1151,  1087,  1094,  1114,  1086,  1170,  1284,  1171,  1082,  1130,
    1285,  1069,  1076,  1096,  1127,  1128,  1097,  1112,  1073,  1074,
    1129,  1067,  1150,  1095,  1080,  1286,  1153,  1192,  1075,  1280,
    1243,  1287,  1245,  1098,  1116,  1148,  1068,  1193,  1283,  1083,
    1118,  1126,  1092,  1278,  1090,  1091,  1167,  1155,  1088,  1089,
    1122,  1071,  1035,  1038,  1039,  1040,  1041,  1042,  1043,  1044,
    1045,  1046,  1047,  1048,  1049,  1050,  1051,     0,  2025,  2500,
       0,  3133,  2114,     0,  3019,  2080,  2070,     0,     0,     0,
       0,   586,   582,   585,   584,   583,   588,   737,   599,   595,
     597,   598,   600,   596,   601,   738,   589,  2713,   602,   603,
     578,   591,   592,   593,   587,   590,   581,   580,  2081,     0,
    2104,     0,  2384,  2747,  2748,  2749,     0,     0,  2160,  1381,
       0,  2172,     0,  2186,  2385,     0,     0,     0,     0,     0,
    2225,     0,     0,  2229,  2228,  2220,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2208,  2209,  2868,  2536,     0,
    2275,  3113,  3113,  2605,  2606,  2779,     0,     0,     0,  2299,
    2293,  2991,  2294,     0,     0,  2500,     0,  1379,     0,  2992,
    2854,     0,  2299,  2390,  2389,  2114,  2970,  2447,  2397,     0,
    2449,  2454,     0,   557,   505,   806,     0,     0,   941,  3040,
     608,     0,   667,     0,     0,     0,   792,   687,  3104,  3104,
    3104,  3104,  3104,   688,  3129,   689,   690,   691,   693,   694,
     695,   696,   698,   697,   700,   733,   731,   732,   734,   735,
     699,   705,   701,  3100,     0,   736,   760,   702,   677,   703,
     704,     0,  2981,  3107,   716,   717,   715,   788,  3125,   719,
     720,   718,  3035,   675,   567,  3035,   631,   633,   636,   634,
     635,   637,   639,   638,   653,   640,   642,   641,   683,   692,
     643,   644,   645,   646,   647,   648,   649,   650,   651,   652,
       0,     0,  2981,     0,  3000,     0,     0,  3040,  3040,     0,
       0,     0,     0,     0,  3035,   498,  2975,   499,     0,     0,
     500,   445,   446,   254,  3066,  3050,  3095,  3125,  3152,  3153,
     352,     0,  3050,  3035,  2993,  3050,   354,     0,  3133,   335,
     253,   334,  3035,  3035,   256,   257,   262,   267,   264,   321,
     265,   268,     0,   269,     0,   259,   351,   260,   261,   266,
     263,   258,   270,   271,   272,   273,   274,   275,     0,  2987,
    3040,  2654,     0,     0,  3173,  3174,  3076,  2634,     0,     0,
       0,     0,     0,   426,   399,     0,   194,     0,  3035,  3035,
     112,  1597,  1346,  1343,  1349,     0,  1346,  1361,  1362,  1333,
    2466,  2467,  2471,  2472,  1456,  2468,  1553,  2463,  1381,  1516,
    3071,     0,     0,  2784,  1504,  2741,  2742,     0,     0,     0,
       0,  1520,  1556,  1543,  3035,  2552,  3128,  2552,  2973,     0,
    3019,  3035,     0,     0,     0,     0,  1477,  1463,  1465,  2460,
    1474,  3051,  1468,  1469,  1470,  1510,  2983,  1472,     0,     0,
    2718,  2688,  2717,  2873,     0,  2834,     0,  2834,  2838,     0,
    2813,  2844,     0,  2834,  2834,  2834,     0,  2815,  2873,     0,
    2834,     0,  1381,  1381,  1568,  2506,  2503,  2769,  2770,  2500,
       0,  2834,  2834,     0,  2862,  1582,  1581,     0,  1578,  1590,
    1588,  2548,  1613,  1614,  1610,  1612,  1665,     0,  1662,  1661,
    1670,     0,  1678,  2119,     0,  1381,  1381,  1685,  2537,  2543,
    2540,     0,  1777,  1753,  1384,  1739,  1740,  1737,  1736,  1738,
    1735,  1727,  1729,  1732,     0,  1808,  1809,     0,  1303,  1305,
    1812,  1813,     0,     0,     0,  2552,  2552,     0,  1725,  1747,
    1778,  3133,  3133,     0,  1834,     0,  1848,  1846,  1616,  1847,
    1877,  1874,  1876,  1875,  1868,  1871,  1878,  1381,     0,  2566,
    2565,  2595,  3009,  1487,  1914,  2644,     0,  1381,  1928,  1919,
    1922,     0,     0,  1949,     0,  1297,  1296,  1302,  1301,  1300,
       0,     0,  1982,  1984,     0,  1977,  1991,  1978,  1979,  1970,
    1973,  1991,     0,  2725,  2554,  2013,  2243,  2234,     0,   778,
     779,  3050,  3024,  1034,  1292,  1291,  1033,  2026,  2029,     0,
       0,     0,   579,     0,  2069,  1381,     0,  2087,  2083,  2088,
    2084,  2089,     0,  2079,   594,  2086,  2106,  2137,  2105,  1381,
    1381,  2154,  2525,     0,  2536,  2161,     0,  2174,  2559,     0,
    2169,  2175,  2191,  2190,  2189,  2188,  2187,  2207,  2230,  2744,
    2231,  2743,  2745,  2746,  2219,     0,  2222,  2211,  2212,  2213,
    2217,  2214,  2218,  2215,  2216,  2210,  2869,  2267,     0,  2264,
    2265,  2259,     0,  2252,  3170,  3169,     0,  3114,  2279,  2279,
    2603,  2780,     0,  2509,     0,     0,  2873,  2873,  2305,     0,
       0,  1380,     0,  2855,  2328,  2329,     0,  2332,  2335,  2337,
    2333,  2160,  2971,     0,  2554,     0,  2456,     0,   504,   558,
     809,     0,     0,   429,     0,  3035,   668,   605,   801,   802,
    3105,   730,   729,   722,   721,   728,   727,   726,   725,   724,
     723,  3130,     0,     0,  3101,   786,   656,  2757,   764,     0,
     756,   679,   676,   654,  2982,     0,  3108,   789,   790,   787,
       0,     0,   632,   664,   803,  2991,   669,   661,  2885,  2885,
       0,   609,     0,   576,   467,   495,  3165,  3166,  2663,   476,
    2661,  3157,  3156,   469,  2665,  2664,  3060,  2995,  3019,     0,
    3035,   473,   472,  3035,   501,  3035,  3035,  3050,   303,   355,
    3035,  3095,  3035,  3035,  3035,  3035,   374,  2978,   375,     0,
       0,     0,  3035,  3066,   322,  2988,     0,     0,   409,   410,
       0,   413,   415,   418,   419,   422,   427,   424,   196,   197,
    2778,  2775,  2775,   113,  1344,  3035,     0,  1358,  1354,  1347,
    1348,  1363,  2461,  2469,  1381,  1554,  1555,  2755,  2462,  2464,
    2470,  1514,  1515,  1548,  1546,  1523,  1547,     0,  1550,     0,
    1549,  1551,     0,     0,  1490,  1489,     0,  1493,  1492,  1475,
    3035,  1458,  1505,  1507,  2690,  2691,  2500,  2850,  2819,  2852,
    2820,  2814,  2848,  2816,  2817,  2818,  2846,  2887,  2841,  2842,
    2812,  2673,  2508,  2505,  2501,  2507,  2502,  2504,  2768,  1569,
    2835,     0,  2810,  2811,  2864,  2728,  2729,  1579,  1583,  1595,
       0,  2876,     0,  2542,  2545,  2538,  2544,  2539,  2541,  1695,
    1768,  3037,  3037,  3037,  3037,     0,  1752,  1754,  1755,     0,
    1827,  1825,  1304,  1306,  3074,  1826,  1829,  1828,  1823,  1796,
    1793,  3047,     0,  2976,  1792,  1795,  1786,  1764,  1782,  1788,
    1789,  1799,  1790,  1784,  1803,  1804,     0,  1771,  2500,  2626,
    2500,  2626,  1622,     0,  2559,  1873,  1865,  1866,  1864,  1870,
    3010,     0,  1916,  1921,  1926,  1935,  1938,  1939,  3005,  3122,
    1931,  3125,  1936,  1991,  2723,  2724,  1991,     0,  3031,  3031,
    1976,  1992,  1993,  1974,  1980,  1975,  3019,  2000,  2418,     0,
    2244,   322,  2238,     0,  3025,  2241,  2500,  3085,  3085,     0,
    2045,  2046,  2549,  2074,  2076,  2077,  2073,     0,     0,  3076,
       0,  2125,  2107,  2120,  2113,  2109,  2122,     0,  1381,  1381,
    2134,  2143,  2140,  2524,  2527,  2518,  2526,  2519,  2159,  2162,
       0,  1381,  1381,  2176,  3052,  2183,  2221,  2266,  2256,  2260,
    2261,  2262,  2263,  2254,  2277,  2280,  2278,  2781,  1381,  1381,
    2289,  2515,  2512,  3035,  2297,  2296,  2298,  2500,  2887,  2500,
    1375,  2320,  2720,  2854,  2331,  3019,  2554,  3019,  2509,  2391,
    2388,  2387,  3054,  2398,  2455,  3133,     0,  2453,     0,   506,
     558,   807,   558,   812,     0,   626,   628,   627,   621,   625,
     623,   624,   620,   622,   619,   798,   793,   795,     0,     0,
     604,   791,  1021,  1017,  1018,  1011,  1015,  1023,  1009,   739,
    1016,  1008,   746,  1014,   710,  1010,  1012,  1013,  1022,   707,
     709,  1019,   711,  1020,   706,   714,   713,     0,   655,   657,
    3019,   758,  3123,   761,  3123,     0,     0,   659,  2758,   682,
     686,   685,   684,     0,   663,     0,     0,   662,   660,  2993,
     748,   749,   613,   612,     0,   457,     0,   494,  2662,  3061,
     478,     0,   460,  3109,   487,   489,   493,   492,   488,   490,
     486,   491,     0,     0,     0,     0,  3035,   306,   305,   281,
     304,     0,   354,   350,     0,   357,     0,     0,  2979,  2980,
     373,   376,  2995,     0,     0,  3035,     0,  3015,   353,   380,
       0,   411,   420,     0,   421,     0,   114,   115,     0,  3035,
    1353,  1359,     0,     0,  1334,  2465,  1536,  3158,  3159,  2553,
    1552,  1541,  1488,     0,  1506,  1570,  3027,  2889,  2836,  1615,
    1667,  2118,  2117,  3133,     0,     0,     0,     0,  1761,  1756,
    3008,  3049,  3048,     0,  1785,  3037,  1783,  1798,  1797,  1800,
       0,     0,  1791,  1836,     0,  1835,     0,  1617,  1618,  1349,
       0,  1872,  1879,  1733,  1890,  1892,  1893,  1381,  1940,  3006,
       0,     0,  1972,  1983,  1991,  3032,     0,     0,  1994,  1995,
       0,  2418,  3074,  2003,  2014,     0,     0,  2247,  2237,  3026,
    2030,  3086,   377,   378,   379,     0,  2066,  3137,  2064,  2065,
    2063,  3136,  2037,  2047,  2048,  2050,     0,     0,  2075,  2071,
    2090,  2092,     0,  2110,  3035,  2536,  2108,  2121,     0,     0,
       0,  2124,  2145,  2142,  2138,  2144,  2139,  2141,  2163,  2170,
    2177,  3053,  2192,  2517,  2514,  2510,  2516,  2511,  2513,     0,
    2307,  2889,  2306,  2345,  1374,     0,  2330,     0,  2334,     0,
    2326,  1381,  1381,  2382,  2393,  2533,  2530,  2392,  3055,  3056,
    2386,  2401,     0,     0,   508,   507,   810,   814,   942,     0,
     799,   796,   616,  3074,   619,   611,   614,   617,   606,   740,
     741,   745,   744,   743,   742,   708,   747,   712,  2714,     0,
       0,     0,   756,  3124,     0,   757,   762,  3035,   681,   680,
     665,   804,     0,   671,   674,   673,   670,  2886,   468,   459,
     458,   456,   496,   477,  2995,   465,   474,   471,   475,   470,
     311,   312,   310,   309,   383,     0,   295,   296,   297,   291,
     292,   286,   298,   299,   287,     0,   300,   301,   290,   288,
     289,   294,   293,     0,   280,   284,   285,   282,   384,   356,
       0,   364,   372,   385,   386,  2656,     0,  3035,   323,     0,
       0,     0,     0,   423,   198,     0,     0,  1350,  1351,  1355,
       0,  1356,  1364,  1366,  1508,  3028,     0,  3027,  2500,  1626,
    1769,  1760,  1759,  1757,  1758,  1806,     0,  3037,  1801,  1802,
    2500,  2500,  1619,  1620,     0,  1624,  1623,  1625,  1885,  1734,
    1891,  1920,     0,  1950,  1990,  1997,  1996,  2555,  2003,     0,
       0,  2491,  3035,  2245,     0,     0,  2235,  2239,  2061,  3087,
    2058,  2060,  2059,  2052,  2057,  2049,     0,  2055,  2053,  2054,
    2051,     0,     0,  2112,     0,  2103,  2128,  2525,  2522,  2127,
    2111,  2123,  2300,  2500,  2353,  1376,  2336,  2338,  2532,  2535,
    2528,  2534,  2529,  2531,  2409,  2402,  2403,     0,     0,  2457,
    2458,  3035,   510,   813,   944,   797,     0,   794,     0,     0,
     618,   658,   765,   759,     0,   766,  3076,  3076,   768,     0,
     672,     0,     0,   479,   480,   481,   482,     0,   461,  3018,
     467,     0,   364,     0,  3013,  3175,  3176,   277,   276,     0,
     361,   360,   359,   362,   358,  3035,   366,   314,   368,  2657,
     332,   341,   348,   341,   337,     0,  3035,  3035,   324,     0,
     381,  3035,  3035,  1352,  1345,     0,  1366,  1384,     0,     0,
    1571,  1635,  3033,  1805,     0,  1838,  1837,  1621,  2681,  2680,
    2756,  1941,  2001,  2419,  2420,  3035,  2004,  2005,  2007,  1381,
    1381,  2012,  2497,  2494,     0,  2248,  3035,  3088,  2056,  2062,
       0,  2093,  2707,  2706,  2708,     0,  2126,  2520,  2521,  2523,
    2308,  2984,  2379,  2378,  2354,  2346,  2347,  2972,  2348,  2349,
    2350,  2351,  2374,     0,     0,     0,  3035,  2413,  2404,  2408,
       0,  2407,  2405,  2459,     0,  3015,     0,   815,     0,   948,
     943,   945,     0,     0,  3035,   615,   767,   768,     0,     0,
    2991,   753,   773,   774,   775,  3050,   772,   678,   485,   484,
    2983,  2995,   466,  2856,   279,   302,   278,   367,   316,  2856,
     315,   365,     0,  3062,   339,   344,     0,   340,     0,   338,
     330,     0,     0,   325,   382,  2775,  2775,     0,  1384,     0,
    2890,  1381,  1381,  1381,  1594,  1642,  1638,  3074,  3034,  1629,
    1634,  1633,  1628,     0,  1787,  2491,  2421,     0,  2006,  2496,
    2499,  2492,  2498,  2493,  2495,  2246,     0,  2709,  2710,  2711,
    2091,     0,  3111,  2375,  2376,     0,  3076,     0,  3074,  2424,
    2406,  2450,   509,   511,  3031,   816,     0,   949,     0,   946,
    3132,     0,   755,   768,   763,  2775,   769,   776,  3035,     0,
       0,   463,   314,     0,  3035,   363,   370,   371,   369,  3063,
       0,   347,   349,   326,   327,   116,   117,  3035,     0,  2888,
    1640,  1644,  1641,  1636,  1643,  1637,  1639,     0,  1627,  2002,
    2423,  2422,  2249,     0,  3112,     0,     0,  2377,  2372,  2371,
    2370,     0,  2411,     0,  3074,  2434,  2482,   532,     0,     0,
     954,   955,     0,   947,   800,   754,   782,   783,   785,  2775,
     771,   483,  3076,   462,   318,  2858,     0,     0,     0,     0,
       0,     0,     0,  1630,  2373,  3151,  3150,  3089,  2635,     0,
    2366,  2360,  2361,  2363,  3035,  2410,  2414,  2415,  3035,     0,
       0,  2399,  1381,  1381,  2451,  2488,  2485,  3035,     0,     0,
       0,     0,   513,   533,   534,   515,   543,     0,  3035,  3106,
       0,   952,  1006,   784,   777,  2658,     0,     0,   313,  2857,
     317,     0,  3160,  3161,   345,  3035,  3035,  1357,  1365,  3090,
    3076,  2636,  2369,  2364,  2367,     0,  2362,     0,  2416,     0,
    2425,  2426,  3035,     0,     0,  2435,  2436,  2438,  2482,  2487,
    2490,  2483,  2489,  2484,  2486,  3035,     0,     0,  3035,  3035,
    3035,  3035,   535,     0,  3065,     0,  3116,     0,   512,   516,
     518,   517,     0,     0,     0,     0,     0,     0,     0,   514,
     544,   546,   545,     0,     0,   820,  3040,  3040,  3057,   853,
     819,   823,   824,     0,     0,     0,   978,  3035,   966,   967,
     968,   959,  3129,   960,  3070,  3070,  3035,  3037,  3007,  3035,
       0,   983,   976,   963,   977,  3070,  3037,   964,     0,     0,
     975,   985,   982,   980,  3037,   965,   979,     0,   986,   974,
       0,  1001,   995,   999,   998,   996,  1000,   956,  1002,   997,
     981,   969,     0,  3070,  1007,  1025,  1024,  1028,  2659,   464,
       0,   345,   342,   346,     0,     0,     0,  2368,  2365,  2412,
    2417,  2427,     0,  2431,  2433,  2432,  2429,  2429,  2443,  2439,
    2807,  2808,  2805,  2806,  2440,  2444,  2437,  2400,     0,     0,
     541,     0,     0,     0,     0,  3035,  3035,  3035,  3035,  3035,
     519,     0,  3035,  3035,  3035,  3035,  3035,  3035,  3035,  3035,
    3035,  3035,  3035,   547,     0,  3035,  3191,  3192,  3040,  3040,
       0,   817,   821,  3035,   829,   825,   827,   828,   830,   832,
       0,   957,   958,   991,  3035,   989,  3035,     0,     0,   961,
     962,     0,  1004,   987,     0,  3123,  3035,     0,  1005,  1003,
    1329,   990,   955,  1029,     0,   205,   206,   320,   343,   328,
     329,  2359,  2356,  2358,  2357,  2352,  2355,  2428,  2442,  2430,
    2441,     0,   536,  3123,   540,   538,   542,   537,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  3058,  3059,  2786,
     840,   835,  3054,   839,  2785,   838,     0,   854,     0,   826,
     831,   994,  1325,  1321,   992,   972,   973,   993,   988,  1318,
    1328,     0,   971,   970,  3106,   319,  2445,   780,   528,   524,
     525,   529,   527,     0,   530,   520,   526,   521,   522,   523,
     552,   548,   549,   553,   551,     0,   550,   833,  3055,  3056,
     834,   837,     0,     0,   855,   573,   822,  1313,  1310,  1314,
    1311,  1326,  1309,  1327,  1312,     0,  1322,  1323,     0,  1319,
    1317,  1315,  1316,   984,   539,   781,     0,     0,     0,  3035,
    3035,     0,  3035,   841,   842,   843,   844,   845,   846,   836,
       0,   857,   858,  1324,  1320,     0,   554,  3178,  3177,  3035,
       0,     0,  3180,     0,  3035,  3035,     0,  3106,     0,     0,
     852,   848,  3179,     0,     0,   847,   913,  3139,  3138,  3029,
    3070,  2974,  3035,     0,   912,  3035,  3074,   856,  3035,     0,
     863,   865,   864,   866,   875,   867,   869,   872,   859,   860,
     861,   871,   873,     0,   876,   862,   923,   868,     0,   870,
     874,  3142,  3143,  3070,     0,   849,   851,   850,  3030,   940,
    3073,  3072,  3040,   922,     0,   756,     0,     0,     0,     0,
    3068,  3068,     0,     0,   925,   920,   923,     0,  1308,   927,
     935,   936,     0,   938,   929,     0,   921,   900,   898,   899,
     894,   897,     0,   895,  3123,  2873,   902,  2694,  3182,     0,
       0,  3184,  3186,     0,  3190,  3188,   877,   882,  3078,  3078,
     879,   883,   878,   884,     0,  3069,   914,   914,  3068,  3068,
     907,   924,   926,   937,   934,   933,   931,   932,  3040,   930,
       0,   896,   762,   939,  3076,     0,   901,  2695,  3181,  3185,
    3183,  3189,  3187,  3080,  3079,   885,   890,     0,   918,   916,
     908,   914,   917,   910,     0,     0,   928,   531,   751,     0,
     904,   888,   880,   888,   893,   888,   881,   888,     0,   915,
     909,   911,     0,   750,   906,   903,   905,     0,   887,   886,
     892,   891,   919,   752,   889
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
   -3989, -3989, -3989, -3989, -3989, -3989, -3989,  3298, -3989, -3989,
   -3989, -3989, -3989, -3989,  3262, -3989, -3989, -3989,  2373, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,  3266,
    3199,  1059, -3989, -3989, -3989,  2789, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989,  3231, -3989, -3989, -3989, -3989, -3989, -3989, -3989,  3045,
    1157, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,  3120,
   -3989, -3989, -3989, -3989,  3041, -3989, -3989, -3989, -3989,  3229,
   -3989, -3989, -3989, -3989,  3026, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989,  2579, -3989,  2198, -3989, -3989,
   -1641, -3989, -3989, -3989, -3989, -3989,  3121, -3989, -3989, -3989,
   -3989,  3126, -3989, -3989,  2852, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,  -516,
   -3989, -3989, -3989, -3989,  1314, -3989, -3989, -3989, -2272, -3989,
   -3989, -3989, -3989, -3989,  -315,  -762, -2602, -3989, -3989,   510,
   -3989, -3989, -3989, -3989, -3989,  -290, -3989, -3989, -3989,  -429,
   -3989, -3989, -3989, -3989,   507, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989,  2882, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989,   495, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989,  -281, -3989, -3989, -3989,   137,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989,  -668, -3989, -3989, -3989,  -600,
   -3989, -3989,  -676, -3989, -3989, -3989, -1565, -3989, -3989,  2640,
   -3989, -3169, -3989, -3485,  -699, -3989,  -970,  1214, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3132, -3989, -3989, -3989,
   -3989, -2809, -3989, -3989,  1045, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989,  1719, -3989, -3989, -3989, -3989,
   -3601, -3989, -3989, -3989,  -985, -2634,   575, -1292, -3989, -3989,
   -2415, -3989, -3989, -3989, -3319, -3989, -3989, -1109, -3989, -3989,
   -3321, -3989,  -482,  -385, -3989,  1347, -3989, -3481, -3989,  -534,
   -2397, -3989, -3989, -2394, -3989, -1609, -3989,   571, -1738, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989,  -792, -2718, -3989,
   -3989,  -917, -1859, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -2164, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3988, -3989,
   -3989, -3989, -3989, -1039, -3989, -3989, -3989, -3989, -3989, -3989,
   -1043, -3989, -3989, -3989, -3989, -3989, -3989, -3989,  -362, -3989,
   -3989, -3989,  -812, -3989, -3989,  -521, -3989,  2714, -3989,  -715,
   -1481, -3989,  -680, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989,  1899,  2410,  -993,
    -667,  -666, -3989, -2380, -3989,  -866, -3989, -3989, -3989, -3989,
    -675, -3989,  -700, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989,  1682,  -106, -3989,    96,   530,
   -3989, -3989, -3989, -3989, -3989, -3039, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989,  -907, -3989, -3989,  -153, -3989,  3156, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,  -811, -1569,
   -3989,  2237, -3989,  2232, -3989,   501, -3989,  -714, -3989, -3989,
   -1063, -3989, -3989,   509,  2229, -1147,  1655, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989,  1559,   881, -3989, -3989,
   -3989,  2563, -3989, -3989, -3989, -3989, -3989, -1254, -3989, -3989,
    1416, -3989, -3989, -3989, -3989,   147, -3989, -3989,   113, -3989,
   -3989, -1013, -3989, -3989, -3989,  -387, -3989,  -381, -3989, -3989,
   -3989, -3989,  2556, -3989, -3989, -3989, -3989,  2185, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
    2770, -3989, -3989, -3989, -3989, -3989, -3989, -3989,  2516, -3989,
   -3989,  2161, -3989, -3989,  1526, -3989,   122, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989,  1507, -3989, -3989, -3989, -3989,   469,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
    2510,   877,  2762, -2363, -2698, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -1795, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
    2136, -3989, -3989,  2134, -3989, -3989, -3989, -3989,   836,   443,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,   135,
   -3989, -3989, -3989,  2498, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989,  1466, -3989, -3989, -3989, -3989, -3989, -3989,
     448, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,   435,
    2117, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989,  1956, -3989, -3989,   825, -3989,
    1442, -3989, -3989, -2306,   438,   446, -3989, -3989, -3989, -3989,
   -3989,   -79, -3989,  -225,  1968, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989,  2486, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989,   134, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3269,  1333, -3989, -3989, -3989,   439, -3989,
   -3989, -3989, -3989, -3989, -3989,  -236, -3989, -3989, -3989,  1309,
   -3989, -3989, -3989, -1278,   824, -3989, -3989,   444, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,   429, -3989,
     431, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989,   752, -2321, -3989, -3989, -3989, -3989, -3989,
   -3989,  1926,   818, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989,  -181, -3989, -3989, -3989, -3989,
    1290, -3989, -3989, -3989,  2474, -3989,  2478, -3989, -3989, -3989,
   -3989,  2819, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989,   788, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
    1900, -3989, -3989,  1267, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989,   407, -3989,  1270, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989, -3989,
   -3989, -3989,  -418, -3989, -3989, -3989, -3989, -3989,  -207, -3989,
   -3989, -3989, -3989,  -544,  1321,  1338, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -3989,   -76, -3989, -3989, -3989, -3989,
   -3989,  -416,   202, -3989,  -162, -3989, -3989,  -468,  -590, -2580,
   -3989, -3989,  -467, -3989, -3989,  1895, -3989, -3989, -3989, -3989,
   -3989, -3989, -3989, -3989, -1574, -3989,  1083, -3989, -3989,  1087,
   -3989,  1276, -3989,  2263, -3989,  2265,  -459, -3989,  -406, -3989,
    -404,  -262, -3989,  -149, -3989,  -147, -1729, -3989,  1030, -3989,
    1035,   441, -3989,   460, -3989,   463, -3989, -3989, -3989, -2542,
     -24, -2093, -3989, -3989,   155, -3989,   161, -2115,   450, -3989,
    1007, -3989,  1011,  2708, -1422,  2877,  -610, -2445, -2110,  -615,
   -1831, -3989, -3989,  2255, -3989,  2261,  1619, -1935,   855,   857,
     858,   859,  1211,   676,  -519,  1243,  1165, -3989,  1516,  -540,
   -1135,  -580,  -653,  2865,  2565,  -959, -3989,  -201,  1339,  -684,
   -3989,  -991, -3989,  1602, -3989,  -586, -3989,  2352,   385,    36,
   -3989, -3989, -1088, -3989,   770, -2685,   -52,  3368,  -595,  -579,
   -3989,  -568,  2776, -3989,    53,    21, -3989, -1116, -2346, -3989,
      -2, -3989,    17, -3989,  1388,  -972, -3989,  1575,   574, -2659,
     862,  -632,  2372, -3989, -2250, -2382, -2752,  -602,  -465,  -661,
    -731,  -587, -3989, -2401, -3989, -2693, -3989,  2267,  -800, -3989,
   -3989, -3989, -2268,   -97, -3393, -3989,  1172,  -746,  -803,  -151,
    3075, -3989, -2109,  2845,  -183,  -414,  -501,  -608,   453, -3989,
   -3989, -3989,  -185,  2786, -3989, -3989, -3989, -3989,  1275, -3989,
   -3989,  1963, -3989,  -161, -3989, -3989, -3989, -3989,  1392, -3989,
   -2449, -3989,  1066,   806,   513,   231, -3989, -3989, -3989, -3989,
    -103,  -748,   642, -3989,  1278, -3166, -3989, -3989, -3989, -1578,
   -1677, -2691, -1367, -1161, -3989, -3989, -3989, -3989, -3989, -3989,
     -17,  -177, -3989, -2029,   946, -3989,   109, -3989, -2883, -3989,
     -26, -1828, -2219, -3989, -3989, -2266, -3989,  -624, -3989, -3989,
   -3989,  2837, -1572, -2529, -1691,  -772,  -756,  -671,  -827,  3329,
     594, -3989, -3989,  3090, -3989,  -833, -3989, -3989, -3989,   298,
   -3989,   430, -3989,  1412, -2560, -3989, -3989, -3989, -3079,  -645,
    1726, -3989, -3989,  1585, -3989, -3989, -1164, -3989,  2475, -3989,
   -3989, -3989,  -617, -3989,  -601,  -331, -3989,  1991,  1222, -3989,
   -3989,   411, -3989, -2847,  -763, -3989, -3989, -3989, -3989, -3989,
   -3989
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,     2,     4,     5,     6,    11,    12,     7,     8,
      13,    14,    47,    48,    49,   111,    51,   113,    19,    40,
     123,    15,    16,    17,    24,    32,   219,    25,    31,   109,
     163,   159,   295,   528,   529,   530,    20,    37,    38,   118,
      55,   121,   226,   169,   535,    29,    30,    42,    43,    82,
      83,    84,    85,   196,   278,   501,    86,   193,   263,   264,
     265,   266,   267,   491,  1143,  1694,   268,   269,  1139,   270,
      87,   194,   275,   276,   277,   496,    88,    89,    90,    91,
      92,   151,   210,   521,   211,   212,    93,   180,   244,   462,
     463,   245,   246,  1121,  1129,  1122,  1123,  1124,  2415,  2848,
    1125,  4187,    94,   157,    95,   202,   203,   204,   515,    96,
     198,   199,    97,   473,   474,   182,   746,    98,    99,   477,
     100,   101,   102,   103,   104,   105,   106,   150,    63,    64,
     129,   179,   241,  1085,  1662,  1663,  2374,  2375,  3473,  3474,
    3475,  3476,  3477,  3209,  3210,  1104,  2376,  3453,  2377,  3759,
    3760,  3844,  3958,  2378,  2379,  3227,  3488,  2380,  1132,  2381,
    2382,  2383,  2384,  3644,  3764,  4092,  3765,  3767,  2385,  2386,
    2387,  2388,  3480,  3634,  2389,  3637,  3638,  3761,  3762,  3846,
    2390,  2391,  2827,  2392,  3030,  2393,  3229,  2394,  2395,  2396,
    2397,   242,   243,   454,   455,   456,   457,   738,   458,   459,
     460,   731,  2843,  2844,  3234,   461,   733,  1681,    58,   235,
      59,    60,   125,   176,   236,   237,   716,   238,  1084,  1649,
    1650,  3441,  1651,  3903,  3620,  3185,  1652,  1653,  2813,  3447,
    1654,  1655,  3443,  3613,  3614,  3615,  3616,  1656,  3200,  3201,
    1657,  3187,  1658,  1659,  2349,   704,  2244,  2738,  3109,  3110,
    3592,  3725,  3823,  4008,  4009,  4010,  4011,  3942,  3943,  3944,
    4019,  4020,  4021,  4022,   440,  1626,   441,   442,   708,   709,
    1636,   710,  1080,  1081,   200,  2332,  3124,  2160,  2161,  2162,
    2163,  2164,   711,  3130,   712,  1631,   713,  1632,  2791,  3405,
    3406,  3125,  2304,  2305,  2306,  2307,  2308,  3158,  3159,  2309,
    2310,  3178,  2311,  3174,  2251,  2312,  2313,  3176,  3436,  2314,
    2315,  2772,  3165,  3429,  2316,  2317,  2318,  2319,  3415,  3417,
    2320,  4364,  4503,  2321,  3164,  3422,  2769,  3608,  3161,  3737,
    3741,  3839,  3742,  3743,  3744,  3745,  4304,  3746,  3896,  3897,
    2322,  2323,  2779,  2324,  2325,  2326,  3400,  3126,  3127,  3597,
    2327,  2328,  2329,  1072,  2739,  1630,  3112,  2248,  3397,  3593,
    3727,  3889,  3948,  4030,  4031,  4155,  4156,  4157,  4158,  4032,
    4230,  4231,  4232,  4277,  4313,  4314,  4315,  4316,  4317,  4318,
    4151,  4237,  4322,  4337,  4368,  4369,  4436,  4492,  4508,  4496,
    4370,  4420,  4421,  4371,  4466,  4505,  4372,  4373,  4480,  4481,
    4374,  4375,  4376,  4405,  4406,  4377,  4378,  4458,  4459,  4409,
    4410,  4411,  4379,  4380,  2743,  3594,  3730,  3731,  3732,  3891,
    3892,  4087,  3949,  4077,  3951,  3156,  4084,   953,  4182,  1499,
    1500,  1501,  1502,  2112,  1460,  2113,  1461,  2114,  1462,  2115,
    1463,  2116,  1464,  2117,  1465,  2118,  1466,  2119,  2120,  1467,
    2121,  1468,  2122,  1469,  1470,  2123,  1471,  2124,  1472,  1473,
    2125,  1474,  2126,  1475,  2633,  1986,  1476,  1477,  1481,  1482,
    2560,  2561,  4407,  4408,  4292,  4293,  4294,  4248,  4301,  4302,
    4165,  4298,  4163,  4295,  4078,  4079,   172,   173,   307,  2861,
    3503,   174,  1149,  1702,  1701,  2422,  2423,  2424,  2857,  2858,
    3654,  3242,  1704,  3244,  3656,   694,  1048,  1049,  2227,  3374,
    1050,  1051,  2720,  1874,  1875,  2544,  1052,   231,   373,   374,
     624,   774,  1152,  2476,   775,   776,   777,  2592,  1166,  1766,
    1767,  1157,  1158,  1159,  2891,  2892,  1793,  2966,  1717,  1718,
    1161,  1758,  2864,  2865,  4080,  1907,  1761,  1151,   375,   625,
     810,  1190,  1188,   376,   626,   826,  1846,  2518,   377,   627,
     839,   840,  1848,   378,   628,   845,  2521,   771,  1146,  1700,
     542,   543,  1850,  1851,  2524,  2579,  2982,  3287,  3288,  3289,
    3661,   649,  3792,  3784,  3865,  3785,  3863,  3786,  1218,   379,
     629,   849,   850,   380,   630,   854,   855,  1225,   856,  1221,
    2526,   381,   631,   859,  1230,   382,   383,   632,   869,  1239,
     384,   634,   874,   385,   633,   871,  1240,  1243,   386,   635,
     881,  1869,   882,   387,   636,   893,   894,   895,   896,  1268,
     897,  1270,  1908,  1884,  1885,  3528,  1886,  2549,   898,  1253,
     899,   900,   901,  1911,  1912,   902,  2543,  2946,  2947,  2948,
     903,  1272,  2571,   904,   905,  2542,  3263,   906,  2572,   907,
    1255,  1916,   954,  2967,  2968,  2969,  2970,  2971,  3280,  2972,
    2973,  2974,  2975,  1909,  1259,   388,   637,   911,  1279,   389,
     638,   390,   640,  1925,   916,  2577,   391,   641,   919,   920,
     921,  1288,  1289,  1290,  1931,  1291,  1928,  2588,  2584,  2585,
    2985,  1285,   392,   642,   926,  1962,   927,  1309,  3293,  3294,
     393,   643,   933,   650,   394,   644,   934,   395,   645,   937,
     396,   646,   940,  1967,   397,   398,   651,  1970,  1316,  1971,
    2597,  2599,   399,   652,   946,  1317,  1980,  2603,  2996,  2997,
    2998,  3000,   400,   653,   949,   401,   654,   955,   402,   655,
     957,   958,  1486,  1487,  2006,  1488,  1489,  2619,  2620,  2003,
    2004,  2005,  2613,  3010,  3011,  3012,   403,   962,  1490,  3311,
    3795,  3541,  3676,  3677,  2009,   404,   963,  1492,  3018,   405,
     658,   406,   659,   970,  1506,   407,   660,   972,   408,   661,
     974,  1510,   409,   663,   977,   978,   979,  1517,  2132,  3332,
    3333,  3560,  3553,  3554,  3334,  3335,   410,   664,   981,  2645,
    2646,  3339,  3036,  1522,  1523,  1524,  2648,  2650,  2651,  3562,
     411,   665,   412,   666,   987,  1535,   413,   667,   989,  2177,
    3041,  3042,  3043,  1866,  1867,  1868,  3346,  3045,  3345,  3565,
    1537,   414,   415,   668,   991,  1545,  3050,  3356,  3051,  3354,
    3052,  1542,   416,   669,   993,   417,   418,   670,   996,  1549,
     419,   671,   999,  2664,  2665,  1553,   420,   421,   672,  1003,
    1559,  2180,  2670,  2671,  1557,   422,   673,  1006,  1561,  1562,
    2184,  3065,   423,   674,  1011,   213,  1578,  1012,  1013,  2205,
    2206,  1014,  1015,  1016,  1017,  1018,  1019,  1020,  1021,   424,
     675,   964,  3020,  1494,  3318,  2013,  2627,  3317,  3546,   425,
     676,  1030,  2208,  1586,  2698,  2699,  2700,  1582,   426,  1032,
    1588,  3074,   683,   684,   427,   689,  1037,  1038,  1039,  1598,
    1599,  2222,  3085,  2713,  1596,   428,   690,  1042,  1604,   429,
     692,   430,   693,  1044,   431,   695,  1053,   432,   696,  1056,
     433,   697,  1059,  1617,  2724,  2725,  2232,  2727,  3096,  3098,
    1615,   434,   698,  1063,  3375,  3574,  3705,  3706,  3707,  4195,
    3708,  3921,  3922,  3975,  3709,  3880,  3710,  3711,  3712,  3713,
     435,   699,  1065,  1547,  2235,  2236,  3383,  1620,   436,  1067,
    1622,  3391,  3988,  3584,  3585,  3586,  3722,  3717,  3925,  3819,
    3926,  3927,  3313,  3673,  3674,  3885,  3980,  3981,  4198,  4199,
    3931,  3985,  3986,  4109,  4114,  2238,   437,  1068,  1624,  3886,
    2241,  2736,  3106,  3589,  2434,  2868,  2435,  2436,  2862,  2437,
    2438,  1262,  1879,  1263,  1877,  1264,  3934,  3993,  3935,  3991,
    3936,  3681,  3803,  3682,  3801,  3683,  2504,  2916,  2505,  2914,
    2506,  3080,  3367,  3081,  3365,  3082,  2661,  3566,  3698,  2662,
    3055,  3056,  3384,  3582,  3385,  3580,  3386,  2537,  2538,  2937,
    2539,  2935,  2540,   982,  2135,   685,  2878,  3017,   944,   945,
     923,   924,  1303,  1304,  1936,  1305,  1956,  1957,  1958,  1959,
    1960,  1961,  1591,  2216,  1804,   828,   829,   830,   831,   811,
     861,  1233,  1000,  1001,  1004,  1669,  3877,  1670,   778,   950,
    1023,  1024,  1964,  1965,  1314,  1525,   842,   843,  1664,  2805,
    3640,  3954,  1140,  2799,  2800,  2806,   132,   205,   935,   863,
    1266,   812,   813,  3667,   814,   815,  4426,  1818,   834,  3691,
    1768,  2166,  3419,  1800,  2481,  2621,  3091,  1994,  3003,  1564,
    2924,   851,  1060,  1527,  1707,  2444,  2680,  2681,  2172,  1708,
     965,  1027,  1054,  2622,  3669,  2766,  3169,   816,   687,   876,
     864,   865,  1996,   688,  1141,  1142,  1693,  1035,  1036,  1709,
     516,  2446,  4235,   475,   817,   512,   513,  4115,   818,   819,
     820,   821,  1196,  1175,  1826,  1810,  1811,  1822,  1815,  1805,
    1807,   714,  2726,  3842,  1213,  1839,  2514,  1540,  1503,   449,
    1236,  1861,   305,  3180,  3257,  3508,   621,   622,   623,  2733,
    2459,  2347,  1107,  3220,  2775,  1162,  1163,  2836,  1257,  2425,
     271,   149,   492,  2335,  1247,  1762,  3300,  2562,  2991,  1102,
    1100,   183,  3621,   983,  1985,  3025,  3506,  4389,  3306,  3793,
    1307,  2018,   286,  2023,  3273,  2472,  3362,  3390,  4150,  3190,
    3850,  4013,   166,  4446,  4164,  4392,  1090,   734,  4475,   291,
    3322,  3688,  3970,   272,  1763,  2399,  1613,  2765,   136,  2752,
    2331,   482,  3875,  2708,  4014,  1679,  1250,  3001,  3424,  2563,
    2457,  2762,  3598,  2976,  3337,  4381,  4281,  4383,  4083,  1234,
    3917,  2400,   255,  2803,  3249,  3964,  1691,  2339,  1227,  2706,
     822,  2406,  3629,  4329,  4335,  4439,  4440,  4441,  4442,  4443,
    4035
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     214,   941,   763,   247,   909,  1795,  1073,  1074,  1075,  1045,
      53,  1082,  1521,  1026,   133,  1192,   994,  1760,  2019,  1587,
     140,   230,  2250,  2441,  1170,  1684,   922,  2649,  2442,  1926,
    2744,   288,  1574,  1209,  2896,  2866,  1563,   445,  2230,  1245,
     188,   841,   188,   137,   138,   139,  2416,   188,  1086,  2907,
     144,   862,  1091,  1092,  1829,  1094,  1095,   947,  1097,  1098,
    2554,  2243,  1160,  1238,  2795,   936,  2454,  2875,   908,  1825,
    2564,   966,  2344,   936,  1184,  1087,  1088,  2881,  3107,  1025,
    1093,  1835,  3167,  1096,  2663,  3426,   832,   188,   966,  2816,
    1271,   860,  2553,  2703,   167,   936,  2820,  2586,   511,  2824,
    2825,  2136,  1228,  3605,   860,  2643,   130,   827,   184,   130,
     185,  4184,   860,  2568,   191,   192,  3192,   443,  2796,  2797,
    1237,  3057,  1043, -1337,   206,   215,  3307,   873,  3548,  1286,
    2530,  3251,   283,  1671,   860,  1673,  1674,  3947,  1676,  1677,
     222,   493,  1998,   258,  3611,  3222,  1923,  1002,  4494,  3920,
    2020,  2229,  3230,  2229,   287,  4400,   248,  1147,   479,  1688,
   -2752,  2334,  3787,   294,  1550,   951,  2451,  2229,  1966,   147,
    1764,  1686,   503,   187,   281,   190,  1177,  1178,  1794, -3133,
     195,  2837, -3133,  3525,  1183, -1335, -2753,   986,   292,  1153,
     498,  2871,  2872,   296,  2574,  2462,  4401,  2873,  2874,  2466,
    2876,   306,  1932,  2455, -3076,  1883,  3719,  2470,   223,  2977,
    3983,   258,   836,  1697,  3027, -2993,   503,   867,   966,  4126,
     251,  1496,  2565,   503, -3091,  1238,  1216, -2991, -3133, -3133,
     867,  3858,  1594,  1483,  2165,   483,  1999,  3630,   867,   519,
     520,  4191,  2673,  3827,  2986,  3781,  3995,  2674,  2545,  2546,
     181,  4139, -3133,   841,   181,  1203,  -818,  3937,  4110,    52,
     867,  3836,  3349,  1145,   490,  1319, -3133,  3087,  3088,  3276,
   -2072,  2810,  2701,  1459,  4455,   293,  2178,  4154, -3096,  3276,
      52,  1943,  1580,   832,   832,   832,  3034,  1136,   524,  3103,
    1796,  4299, -2763, -3133,  3350,  2675,  2398,  -818,  -818,  3039,
     283,  1983,   739, -3133,  2950,  1201, -3125,  2951,  1732, -2761,
    1602, -3133,  2774, -2185,  2955,  3014,   289,  2958, -2185,  3407,
    2360,   860,    52,  1010,  4491,  1797,  1798,  1267, -3133,   233,
     980,   450,   750,  3938,  1295,  3046, -3133,   772,  3059,  4504,
    4192,  2452,  3004,  2558,    22,  2559,   450,  2811,  4071,  2427,
      -6,  4111,  4026,  4027,  1876,  3849,  1312,   647,  4193,  3078,
    1606,  3439,  1943,  2213,   273,  3022, -2185,  2555,  1312,   153,
    1801, -3076,   656, -1337,  1280,  2361,  2129,  1260, -3133, -1337,
    1680,  3518,   239,    -6,  1935,    -6,  2859,    65,  2502,  3060,
    3048, -3121,   503, -3133,  1223,   860,  2801,   718,   526,  3961,
    2638,  1193,  3720, -3133,   858,   107,  2777,   860,  3714,   836,
     836,   836,  2750,  3023,    52,  3805,  3832, -2699, -2794,  1210,
     838,  2629,   253,  1571,  2823,  3728,  -283,   870,  1858,   857,
     181,  2882,   872,  1538,  1862, -1335,   742,   743,  1160,   936,
    3619, -1335,  3898, -2993,  1160,  2000,  1312,   867,   858,  4495,
     525,   858,  2768,  2750,   966,  3821,   749,  1554,  1109,  4483,
    1518, -3050,  3729,   752,  3657,   766,   990,  2213,   207,   997,
    2225,     3,   960,  2471, -1337,  1031,   447,   494,   860,  2233,
      66,  2866,  2342,  3102,  2430,   860,    52,    26,  1993,  1765,
   -2993,  -818,  2431,  4499,   832,  1057,  2718,  3782,   503,  3277,
     258,  3504,  2213,  3898, -2993,   503, -2763, -1337,   448, -1337,
     284,  1993,  3895,  4194,  3878,  1592,   444, -3133,  2410,  2213,
    2234,   867,  1621,  3781,  2213,  1539,  1034,  2826,  2021,  3479,
     844,  3483,  2751,   867,   866,   504, -1335,  3872,    67,   273,
    1689,    68,  2229,    69,  3491,  -818,  3962,   866,   938,  3058,
   -3017,  3206,  3963,  2213,  1882,   866,  3788,  2001,  2213,  3062,
    4402,   967,  3728,  2213,    70,  3432,  1010,   108,  1244, -1335,
    3433, -1335,   995,  2753,   995,  1005,   527,  1028,   967,   504,
     249, -3134,  3212,   259,  3840,   536,   504,  2503,  2630,   254,
    3024,  3519,  2802, -3015,  1566,   464,   207,  3600,  1135,  3729,
      52,  1224,   995,   657,   867, -2699,  1153,  4456,  2420,   862,
      23,   867,   505,  2418,  2696,  1137,    -6,  3778,  3646,  1575,
     836,  2790,  2016,  2556,  2697,  2860,  3631,  3549,   506,  1261,
     274,  4112,   832,   832,   832,   207,  1799,  1105,  2228,   832,
     832,   832,  2471,  2715,   858,  3079,  2628,   833,  1110,  1828,
    1573,  3378,   476,  4071,  1222,   832,   505,    71,   832,  2778,
    1814,  1814,  1814,   505,   832,   832,   832,   832,   832,  1241,
    3315,   922,   506,   290, -3133,  3040,  1834,  3526,   284,   506,
     207,   648,   240,  1995,  3434,  3789,  2547,  2822, -2983,  1873,
     773,   -43, -2983, -3133,   832,  2491,  1281,  3302,  2509,   832,
    3303,   862,  2428,  1212,   838,  1698, -3133,  2170,   967,   207,
    1138,  3790,  3274,  3028,  -818,  1859,  3870,  1231,   260, -2991,
    1863, -2991,  3046,  3049,  2414,   476,  2167,   732,  1968, -2577,
     476,  1484,  1984,   234,   476, -2991,  4360,  3822,    52,  3004,
   -1337,  1828,   844,   476, -3133, -3019, -1342,   705,  3450,  3550,
   -3133,  4284,  1103,  3618,  3658,   504, -3133,  2586,   836,   836,
     836,  3067,  2133, -3035,   866,   836,   836,   836,  1820,  4028,
    1034,  1820,   504,  1933,  2849,  1832,  4305,  1862,  3351,  3193,
    2919,   836,  2815,  1820,   836,  3783,   507,  1593,  4300,  -283,
     836,   836,   836,   836,   836,   451,  1459,   526,  1111,  1530,
     258, -3133, -1335,  3567, -2993,   274,  4113,  3650,  2469,   186,
     451,  1046,  3276,   790,   791,  2987,   504,  3939,   261,   504,
     836,  1883,  1953,  2014,  1153,   836, -3035,    72,  3984, -3091,
     507,  3278,   505,   186,  3029, -2072,  3940,   507,   866,  4127,
     793,  2165,   794,  2456,   833,   833,   833,  4185,   506,   505,
     866,  3035,   258,  3941,  2641,   444, -2993,  1914,   224,  1934,
    4457,   504,  2456,  1485,   858,   506,  -283,  1832,   504, -2997,
    3996,  4140,   259,   186,  3191,  1112,  2812,   186,  1113, -3133,
     764,   939,  -283,   476,   793,  1529,   794,   793, -1342,   794,
    2550,  1114,  2890,   505,  2886,  2889,   505,  1699,  3440, -3035,
    3510,   476,  3612,  1953,  1047,  1005, -2983,  3780,   508,   506,
   -2983,   225,   506,  2128,   476,   952,  3527,   186,   207,    73,
     186,   866,  1115,   186,  2411,  2412,  3748,  3749,   866,   936,
    1683,   862,  3632,   186,  1148,   186, -3015,  1667,   505,  1668,
    2022,  3888,  1672,  1046,   967,   505,   186, -2993,  2214, -3096,
    1034,  1682,   508,  1687,   506,   832,  -818,  2731, -2991,   508,
   -2991,   506,  1865,  1690,  3774, -3050,  1695,   297,   860,  2002,
     452,  1828,   801,  1204, -2991,   527,  1593,  1756,  1205,   476,
    3551,  3791,  4236,   476,   487,   452,   509,  3310,   537,   188,
     188,  2548,  1312, -2185, -2185,  2402,  2403,  2999,  3534,   188,
    2482,  2408,   453,  1526,  2409,  1046,   507,   260, -3121,   499,
    1287,  4029,   504, -2983,   201,   262,   801,   453,   131,   801,
   -3133,   131,  4186,   507,   186,   862,  3699,  3859,  2508,   444,
     509,   860,  3552,   858,  2017,  3879,  1047,   509,   495,  1864,
    -283,  2478,  2479,  2215,   186,   186,   186,  4403,  -818,  1116,
    1857,   476,  2214,   832,  1156,   833,   967,   250,  1660,  2445,
    3841,  3820, -3133,  3783,  2600,  1828,  3377,   507,  3379,  1997,
     507,  2501,   465,    76,  2218,  2417,  4424,   538,   476,   837,
     793,   836,   794,   510,  3279,  2507,  3721,  2214,  3633,   505,
    2419,  3625,  1831,  2498,   867,  3647,  4250,  1832,  1047,  2929,
    3435,  3626,  -559,   476,  2214,   506,  4229,   261,   961,  2214,
   -2991,  3325,   507,  3264,  3265,  3266,  3267,  1696,  4161,   507,
     186,   740,  1665,  1666,  4257,   847, -2699,   510,   508,  2941,
    1710,  3420,  1675,  2443,   510, -2699, -2699, -1340,  2214,   705,
   -2699,   476,  3194,  2214,   939,   508,  1765, -2849,  2214,  2788,
    1062,  2593,  2586,    79,  3451,  3901,  3316,   867,  3882,  3195,
    2908,   186,  -283, -2983, -2983,  1521,   186,  3255,  3653,   160,
    1882,  1117,   259, -3135,  3111,  3409,  1118,  1119, -3133,   836,
    1706,  3282, -2851,   207,  1831, -2638, -3133,  3330,   154,   508,
   -3133,  1832,   508,   833,   833,   833,  1566, -2853,   476, -2847,
     833,   833,   833,    80,  2488,  1995,   509,  3175,  1576,   186,
    1204,   484,   801,   756,  2677,  1205,   833,   844,  3648,   833,
    4015,  1765, -3015,   509,   259,   833,   833,   833,   833,   833,
    3569, -2983,   504, -2845,   508,  4428,  4332,   966, -2840,  2489,
    -283,   508, -3125,  2512,   706,   207,  3350,  3641,  3452,  3283,
     181,  3285,  3568,   862, -3158,   833,  2952,   707,  1232,  1577,
     833,  3225,  2520,   507,   539,  2197,  1987,   509,  1194,   540,
     509,  2480,  2667,  4015,   700,  2629,   837,   837,   837, -1340,
    3887,    81,  4098,   207,  2198,   647,  1211,   848, -3159,   860,
    3059,  4429,  2211,   860,  -283,  2596,  3410,  3320,  2591,  4287,
     793,   503,   794,   510,  -283,  3326,   186,   260,  3497,   505,
      18,  -559,   509,  3115,  2942,  4072,  1607, -3133,  4099,   509,
     510,  2804,   541,  -283,  4327,   506,   181,  2601,  3701,  1543,
    1712,  4103,  2853,  2167,  1713,  1972,  2604,  -283,  4328,  1973,
     847,  3060,  1974,  1975,  2659,  4462,  1120,  3446, -2853, -2991,
     186,  1881,   476,   186,   476, -3133, -3133,  3544,  3370,   260,
    3372, -3133,  4093,  2174,   510,  2532,  1566,   510,  1973,  1518,
    -770,  1974,  1975,  3770,  4162,   186,  3773,  2704,  4473,  2535,
    2906, -2983, -2983,   860,  4288,   508,  4024,    27,  3401,  2679,
    2750,  2482,  2190,  3905, -1307,   186,  3635,  1820,  2855,  2502,
    3411,   504,   186,  4108,  3290,   858,   253,   261,  4288,   510,
    2522,   866, -3133, -3015,  1831,   867,   510,   476,     9,   867,
      10,   485,  2750,  1608,   298,   299,   300,  2137,  4103,  3557,
    3327, -3133,   801,    35,  1881,  1531,  2783,  2784,  2978,  4240,
    2980,    -7,  2630,  2838,  2839,  3401,  4104,  3516,  2188,  2841,
    2842,  3008,  4404,    28,  1966,  2636,  2138,  3412,  3679,   261,
    4024,  1567,  3498,   509,  4334,  4016,  2490,  3636,  2407,   793,
    2407,   794,  2407,  2407,   866,  2407,  2407,   476,   505,  3838,
    2750,  1204, -3015,   507,  2767,   706,  1205,   837,   466,  4093,
    2746,  2773,  3009,  2717,   506,  3642,   877,  1914,   476,  3853,
    3854, -2638,  3437,   161,  3026,   701,  3932,  2885,  1831,   867,
    2755,   486,  2953,  2856,   878,  2536,   833,  3702,  4016,  2523,
    2785,  4240,  4197,  2817,  4105,  2763,  3328,  2890,  2682,  3967,
    2945,  2870,  2637,  4288,  4073,  3305,   181,  -770,  1111,  3196,
     301,  2212,  2757,  4104,  1478,  3395,   186,  3396,   936,  1312,
    4478,  4437,  4074,  2653,   755,  4075,   759, -3135,  2207,  1544,
     510,  2789,  4479,   751,   757,   467,    36,   188, -3135,  1939,
    1940,  1941,  2909,   254,  3545, -1779,  2761,  3763,   476,  3703,
     207,   468,   186,  3116,  3089,  3810,  2199,   860,   860,   860,
     186,  1997,  1566,    39,  3494,  2912,  2913,   998,  2503,  -559,
    2759,   801,  4474,   186,   487,   508,  2253,  2350, -3133,   648,
    2330,  2333,  -559,  2336,   833,  1112,  2341,  2343,   758,  2345,
    4072,  4105,  4290,   967,   181,   837,   837,   837,  2933,  2934,
    3740,  1114,   837,   837,   837,  1821,   706,   966,  1821,  3006,
     186,  4289,  3704,  2660,  2200,   485,  4290, -3015,   837,   707,
    1821,   837,   507,  2780,  4229,  1714,   466,   837,   837,   837,
     837,   837,   928,  1942,   504,   162,  1774,   744,  1008,   879,
     719,  2748,  1062,  1775,  3643,  3259,   832,   832,   186,  2201,
    2988,  3906,  3031,   509,  2705,   860,   186,   837,  2534,  3664,
    2993,  3197,   837,   853,  2516,   444,  2447,  2710,  2712,   720,
    2401,  2911,  2448,  4362,  2918,  2449,  1963,  2450,  4430,  3290,
    3680, -3158,  2818,   867,   867,   867,   721,   722,  4431,  4432,
     207,  3413,   790,   791,  3271, -3158,  3198,   967,   186,  1513,
    3351,  3199,   866,   467,  1583,  2840,   866,   207,  3033,   469,
     186,   505,  3101,  3272,  2943, -3159,  4017,   476,   186,   468,
    2659,   844,  3053,  3054,  3094,   302,   303,   506,  3933, -3159,
   -3035,  3329,  2893,  3236,  3237,  4006,   186,  4245,   304,  1010,
    1910,  4290,  1566,   504,   508,  3330,  4252, -3115,   186,  3660,
     510,  2944,  4018,  2202,  3523,  2749,  1998,  -951,  2445,  1116,
    3588,  3665,  3666,  4433,  2682,   735,  2682,   186,  2445,  4017,
   -3131, -3133,   836,   836,   186,  1715,  1716,  4146,  4147,   476,
    1995,   867,  3381,  3558,    52,  4434,  4435,  3414,  4006,  1976,
    1977,  1009,  4189,  4190,  3559,     9,   866,    10,  1514,  1943,
   -3115,    41,   723,  -951,  2531,  4018,  1584,   478,  3945,  4073,
      44,   793,  1585,   794,  3700,  -951,  1515,  2880,  1976,  1977,
     505,  3117,   509,  2879,  2351,  2879,  3946,  4074,  1479,  3100,
    4075,   470,  4291,  4296,   502, -1307,   506,  2635,   504,  2557,
    1978,  1979,  4447,   504, -3133,  1855, -1330,   114,  2567,   186,
    1999,  3231,  1010,  3596,    45,   504,   531,   532,  2786, -1779,
    2203,  -951,  3702,   186,   929,   880,  3059,   186,   930,  1978,
    1979,   736,   737, -2983,  2682,  2682,  2502,   469,  2352,  2939,
    4241,  1117,  1944,  1945,  -951,   507,  4106,  4244,  2535,  4484,
    4485,  4247,  4363,  2140,  4249, -3133,  1856,  1480,   931,  4227,
    4228,  2458,  1944,  1945,  2945,   186,  2464,  4107,  2928,   471,
    4365,  2465,  2535,  4366,   832,   505,   832,  3060,   837,   510,
     505,   147,  1530,  3207,  3703,  2956,  2957,  3245, -1779,   130,
    4327,   506,   505,  2879,  2879,  2710,   506,  2710,  2855,   860,
     832,  -951,  3811,   801,  4328,  2767,  4463,   860,   506,  1204,
    2141,  2142,  2143,  2144,  1205,  2145,  2787,  4448,  4278,  1946,
    1947,  2921,  4279,  2979,   832,  2981,   148,   724,  4412,  2175,
      46,  1516,   533,  2430,   985,  3331,   444,   992,   725,  1946,
    1947,  2431,  1007,  3972,  4422,  2932,  2687,  2688,  2689,  2690,
    2691,  2692,  2693,  2694,   507,   932,  3208,  4468,  4449,   470,
     866,   866,   866,   860,  -497,   860,   472,   508,  2683,  4469,
    1774,  4471,   844,   503,   846,  4323,   837,  1775,  4324,  2660,
    4425,  4427,  -951,  3061,  2536,  3115,   915,  1948,  1312,   862,
    1223,  4342,    50,  1567,  2204,  3261,  2146,  2147,  3915,  3973,
     836,  1206,   836,  4470,  3382,  4472,  2721,  3916,  2536,  -497,
    4467,  1207,  1820,  2856,  1638,    52,  1120,  1611,  4332,  2000,
    1837,  1949,   967,  1612,    54,   867,   836,   860,  4477,   860,
    1950,  1204,  3118,   867,  3119,  2503,  1205,   471,  3974,  1951,
    2337,  1518,   677,  3120,  3121,   509,  2338,  3122,  3123,   507,
     836,    52,    56,  -951,   507,  3962, -1330,   967,   866,  1639,
     115,  3963, -1330,   116,   117,  2770,   507,   726,    57,   862,
     917,  3352,  3353,  4333,   208,  -497,   508,   209,  4133, -1985,
    1952,   918,  4512,  3077,  3359,  3360,  1526,  2747,  3404,   867,
     207,   867,  3835,   976,  1204,  3262,   832, -2038,   207,  1205,
    2217,  3363,  3364,  4416,    61,   727,  4327,  2893,  2193,   860,
    1204,  1204,    62,  4308,  2794,  1205,  1205,  3073,  1204,  2194,
    4328,  2807,   728,  1205,   186,  1640,   119,  1566,  3270,   681,
    4309,  2001, -2798,  -951,   472,  1566, -2798,  1995,  1706,  2845,
    3247,  2847,   510,  2407,  3248,  4417,  2828,   120,  4418,  4168,
     122,  1252,  2682,   867,   509,   867, -1985, -1330,  4174,   181,
     534,  2533,   124,  1567,  4310,  -951,  4177,   833,   833,  4486,
     126,   508, -1985,   128,   188,  3246,   508,  2148,  2149,  2150,
    2151,  2152,  2153,  2154,  2155,  2156,  2404,  2405,   508,  -497,
   -1330,  2432, -1330,  2433,  1821,  2510, -2038,  1224,  3388,  4134,
     208, -3064,  3389,   209,  3258,  1204,  2781,  2894,  2895,  2330,
    1205,  2432,  4419,  2433, -2038,  1204,   678,  1772,  1773,   127,
    1205,  4311,   836,  4450,  4135,  4136,  4137,  2814,  4452,  4288,
    2898,   134,  2900,  4454,  2683,   867,  2683,   186,  2903,  2904,
    2905,   135,  1566,  -497,  4312,  2910,  2854,   141,  2809,   509,
    2854,   510,   476,  1566,   509,  3116,  2922,  2923,   142,  4509,
    2926,  4510,   143,  4511,  -951,   145,   509,  2821,   146,  1199,
    4076,  1202,  4003,   156,   967,   967,  2830,  2831,  3218,  3219,
     967,   967,   967,   967,  4166,   679,  3301,  3543,   967,   158,
     967,  1164,   967, -3064,  4166,  4148,  4149,  -497,  3342,  3292,
    1953,  1842,  1843,  1844,  1954,   165,  1955,   170,  2157,  1774,
     168,  -497,   175,  1803,  1806,  1809,  1775,  2440,  4390,  1776,
    3531,  2949,  2851,  2852,  1954,   188,  1955,    33,    34,   186,
   -1985,   177,   186,   178,  -497,  1988,   504,  -951,   680,  1836,
     189,  -951,   866,   197,  2683,  2683,   510,    52,   217,   186,
     866,   510,   201,  4320,  1990,  1991,  1992,  3005,  2877,  3595,
     216,  1641,   218,   510,  4003,  2883,  3404,   220,   860, -2038,
     860,   221,  1226,  1777,   227,  1778,  1779,  1840,  1841,   228,
     729,   730,    65,   229,  1642,  1816,  1817,   967,   252,  1567,
     967,   681,  2887,  2888,   790,   791,   232,   967,   967,   967,
     967,  4340,  4341,   256,  3578,  3579,   866,  4345,   866,  2484,
    4385,   262,  2486,   505,  4386,  4387,   860,  -951,   171,   439,
    2492,   257,   446, -1330,  2496,   481,  3662,   480,   489,   506,
     490,  2499,   497,   514,   518,   833,   523,   833, -2731, -2731,
   -2731, -2731,  1274,  1275,  1276,  1277,   639,   662,  2158,   691,
     702,   703, -1985, -2730, -2730, -2730, -2730,  1780,  2902,  1781,
     715,   833,  1318,   717,   747,   732,  1782,  4290,  3402,  2926,
     866,  2159,   866,  3431,  1783,    66,   748,  -951,   207,   860,
    1918,  1919,  1920,  1921,   753,   833,  3539,  2754,  2756,  2758,
    2760,  1530,   754,   760,   867,   761,   867,  1532,   260,   186,
     762,   764,   770,   769,   868,   914,   925,   948,   939,   976,
     984,   444,  1034,   682,  1069,  3685,  1070,  4004,  1531,  1071,
    1083,  1077,  2682,  3117,  1089,  1101,  1106,  1099,  1108,  -497,
   -1985,  1131,  1134,    67,  -497,  1144,    68,  1150,    69,  1530,
    1167,  1165,   867,  2002,  1169,  1208,  1172,  1590,  1187,  1173,
    1174,  -497,   866,  -497,  1176,  1215,  1179,  -951,   686,    70,
    1180,  1181,  1182,  1185,  1195,  1197,   967,  2407,  2407,  1567,
    2407,  2407,  2845,  1198,  1217,  4076,  1229,  3599,  1235,  1643,
    1242,  1784,  1244,  1246,  1644,  1249,  1251,   507,  1254,  4391,
    1256,  1258,  1282,  1278,  1283,  1269,  1284,  1308,  1310,   837,
     837,  1645,  1495,  1646,  1315,   867,  1313,  1505,  1508,  4004,
    3223,  1509,  1511, -1985,  1533,  -951,  1536,  3855,  3856,  1534,
    1541,  1546,  4391,  1560,  1548,  1551,  1552, -1985,  1556,  1558,
    1568,  1785,  2019,  1010,  3520,  1786,  3521,  1572,  1579,  1581,
    1593,  1595,  1597,  1603,  1610,  1614,  1616,  1619,  1627,  3128,
    1628,  1633,    71,  1629,  3670,  1634,  1635,   833,  4367,  1637,
    1678, -3115,  2683,   188,   181,  1692,  1703,  1164,  1111,  3590,
     188,  1010,  1720,  1164,   860,  3166,   860,  1771,  1802,  1812,
    1813,  1838,  1823,  1824,  1833,  1845,  1847,   838,  1865,  1860,
    1872,  1261,  1260,  1915,  1917,  1526,  1929,  1963,  1969,  1982,
    1484,  2008,  3799,  3800,  2010,  2012,  2127,  2131,  -497,   508,
    2134,  2169,  2139,  2168,  3202,  2171,   967,  3203,  3403,  3204,
    3205,  2176,  2179,   476,  3211,  2196,  3214,  3215,  3216,  3217,
    2186,  2187,  4081,  2189,  2221,  2223,  3224,  1226,  3296,  2224,
    2229,  3955,  2231,  1526,  2237,  1112,  2239,  2245,  1113,  2246,
    2247,  2249,  2346, -3115,  3005,  2334,  2353,  2413,  1647,  3238,
    2407,  1114,  2429,  2348,  2439,   186,  2421,  1870,  2440,  2458,
    2460,  2461,  2463,  2468,  3118,  2682,  3119,  2471,  -497,  2467,
    2473,  2474, -3115,  1887,  2477,  3120,  3121,   509,  2475,  3122,
    3123,  3894,  1115,  2485,  3253,   967,  4088,  2487,  4005,  2493,
     867,   131,   867,  2494,  3860,  3861,  3862, -3115, -3115, -3115,
    2495,   866,  2497,   866,  2500, -2684,  2511,  4006,  2513,  2517,
    3807,   779,    72,   835,  2519,  2525,   852,  2528,  1648,  2529,
    2541,   875,  2573,   910,  4007,  2575,  2578,  2587,  2576,  2598,
    2589,  2602,  2611,  2610,  2632,  2639,   980,  2707,  2678,  2654,
    2668,   956,   959,  2719,  2669,  2722,   956,  2723,  2685,   866,
    2732,  2737,  2735,   988,  2740,  3738,  3739,   837,  2741,   837,
    2742,  2745,  2746,  1530, -3115,  1530,  2761,  2764,  1033,  1821,
    2774,  2771,  2776,  2793,   510,  2798,  2819,  1787,  2808,   258,
    4005,  2832,  1055,   837,  1061,  1064,  2833,  1153, -2775, -3115,
   -3115, -3115,  2834,  2835,  1765,  2884,  2901,  2502, -2762,  4006,
    2503,  2930,  2536,  2535,    73,  2954,  2983,   837,    74,  1116,
    2984,  2990,   866,  2994,  2995,  3002,  4007,  3007,  2682,  3015,
    1530, -2755,  3016,  2959,  3021,  2960,  3037,  3038,  2660,  2659,
    3064,  3090,  3425,  3095,  3083,   186,  3097,  3104,  3105,  3108,
    3093,  3113,  3157,  3114,  3129,  3989,  3990,  3131, -1794,  1082,
    2407,  3867,  3408,  3438,  3160,  3162,  3163,  3168,  3173,  3177,
    3444,  1788,  3825,  3179,  3182,  3184,  3186,  3189,  3228, -2983,
    3489,  1171,  1789, -2983,  3226,  3233,  3394,  3369,  3235,  3495,
    3496,  3243,  3883,  1790,  1567,  3239,  3256,  3241,  3252, -2689,
   -2760,  3299,  1567,  3260,  3305,  3008,  3009,  3284,  3286,  2015,
    3312,  3923,  3493,  3314,  3321,  3324,  3338,  3499,  3319,   188,
    1200,  1200,  1200,  3343,  3344,  3048,  3049,  3358,  3361,    75,
    3078,  3373,  2683,  3079,  3393,  3448,  3448,  4081,   188,  3399,
    3296,  1117,  3398,  3416,   188,  3418,   852,   188,  2682,   188,
     188,  3923,  3428,  3421,  3423,   147,  3430,   188,  3929,   188,
    2615,  3427,   188,  3492,  1791,  3502,  3501,  4196,    76,  3505,
    3507,  3517,  3532,  1248,  3533,  3881,  2185,  1526,  3540,  1526,
    3542, -3134,  3556,  3561,  3563,  3570,   967,  3571,  3382,   837,
    3575,  3381,  3591,  3601,  3604,  3602,  1792,  3606,  3607,  3610,
    3623,  2209,  3624,  4382,  3649,  2767,  3651,  3652,  3686,  1567,
    3455,  3687,  3689,  3716,    77,  3734,    78,   866,  3695,   866,
    1567,  3723,  3726,  3733,  3736,  3750,  3680,  3758,  3777,  3486,
    3679,  3490,  3815,  3779,  1526,  3816,  3830,  3818,  3740,  3829,
    2242,  3701,  1126,  3500,  3851,  3852,  1530,  3857,    79,  1320,
    3782,  3547,  1665,  1530,  2616,  3890,  3873,  3884,  3874,  1491,
    1493,  3956,  3876,  3893,  3902,  3908,  3912,  3924,  3959,  3930,
    3919,  3454,  3965,  3957,  1127,  1128,  3966,  3478,  3932,  1528,
    2445,  3969,  3481,  3482,  4000,  3998,  3933,  3999,  4001,  3968,
    3485,  4025,  3487,  4090,  4097,  4125,   781,  4128,    80,  4129,
    3603,  4132,   782,   783,  4119,  1565,  3808,  4138,  4141,  4142,
    4145,  4153,  4152,  2452,  4176,  4180,  1120,  4201,   785, -2853,
    3628,  4342,  4388,  4444,  2961,   995,  4321,  4445,  4498,  4096,
    4500,  1200,  4501,  4502,  4514,  4507,  1566,  1601,  3132,    21,
     112,   110,   164,   767,   152,   488,   279,   500,  3564,   155,
     522,  1685,  1130,   282,   280,   745,  3904,  2626,  3769,  4188,
    1618,  3213,  3753,  3848,  1623,  1625,  3221,   741,  3232,  3751,
    3449,  4130,  2407,  4002,  4143,   188,    81,  2642,  1076,  3499,
    2782,  2252,  4361,  4488,  3133,  2683,  3172,  3900,   188,  3837,
   -1794,  2631,  3953,  3183,  4239,  4319,  3134,  4451,  4453,  3828,
    4254,  3952,  4183,   968, -2983,  1989,  3627,  4297,  2962,  1504,
    4173,  4169,  4170,  4181,  2426,  3524,  3240,   438,   188,  1705,
    1711,  3250,  3254,  1759,  2453,   188,  2927,  3715,  2515,  3866,
    3522,  3609,  1214,  1566,  3864,  1220,  3509,  1854,   913,   967,
    2551,  1265,  3135,   912,  1871,  3529,  3269,  1273,  2569,  2940,
    1526,   969,  1927,  1930,  2989,   942,  3291,  1526,  3530,  1311,
    2594,  3298,  1769,  1770,  1981,  3136,   973,   975,  2527,  1200,
    1200,  1200,  3297,  2007,  3013,  2612,  1200,  1200,  1200,  1819,
    3309,  3798,  1819,  3425,   786,   787,   788,  3308,  3747,  3672,
    2011,  3645,  1200,  1512,  1819,  1200,   789,  3555,  2644,  1887,
    3752,  1200,  1200,  1200,  1200,  1200,  3340,  3814,  2401,  2656,
    3044,  3357,  3355,  3099,  2407,  2182,  1061,  3347,  1853,  3063,
    1569,  3622,  3137,   504,  1029,  2695,  1570,  3076,  2220,  2729,
    3376,  1200,  2730,  3976,  3813,  2672,  1200,  2655,  2683,  3718,
    3978,  3796,  4101,  3538,   188,   188,  3684,  4200,  4116,  2240,
    2869,  3639,  2863,  2963, -2983, -2983,  3754,  1880,  1878,  4117,
    3994,  3992,  3756,  3869,  3804,  3802,  2917,  4285,  1913,  3380,
    2915,  3583,  3368,  3697,  3366,  2617,  3581,  2938,  3824,  3387,
    2936,  1022,   188,   792,  1219,  1937,  2590,  3069,  1938,  3070,
    3071,  3072,   967,  1566,  1066,  3724,  2595,  1555,  1849,  3188,
     505,  3442,   285,  3138,   795,   796,   797,  3668,  3692,  2684,
    2614,  3304,  3690,   798,  3092,  4172,   506,   844,  1191,  2850,
     765,  1852,  1133,  4256,  1186,  3181,  2254,  2931,  3845,  2702,
    4397,  3371,  3573,  4393,  3812,  3275,  4286,  3755,  4280,  3757,
    2792,  4414,  4476, -1794,  3871,  1168,  3659,   517,   768,  4303,
    3771,  3772,  3323,  3445,  2709,  3775,  3776,   844,  2683,  2566,
    4091,  4282,  2340,  2846,  3484,  4438,     0,  1719,     0,     0,
       0,   188,   188,     0,  3140,     0,     0,     0,  1530,  3797,
       0,     0,     0,     0,     0,     0,     0,     0,  2964,     0,
    3806,     0,     0,     0,   188,   188,     0,   800,     0,     0,
       0,     0,     0,     0,     0,  3918,  3909,  3910,     0,     0,
       0,     0,   967,  4255,  1191,     0,     0,     0,     0,     0,
    3817,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1306,     0,     0,     0,     0,     0,     0,     0,  3831,   844,
       0,     0,     0,  2780,     0,     0,  2640,     0,  3971,     0,
    1532,     0,     0,     0,  3141,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0, -1794,
       0,     0,     0,   188,   507,     0,     0,  1507,     0,     0,
       0,     0,     0,     0,     0,     0,  3143,     0,     0,     0,
       0,  2965,  1200,  2666,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  3833,  3834,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  3456,  3457,  3458,
       0,     0,     0,  4489,   802,     0,  4100,  3847,  3847,  4086,
       0,     0,     0,     0,  2714,     0,     0,     0,  1589,     0,
     188,     0,  3899,  2173,   967,  1600,     0,  2714,  3907,  1191,
       0,     0,     0,     0,     0,  2181,     0,  2183,     0,     0,
       0,  3911,     0,     0,     0,     0,     0,     0,   476,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1526,     0,     0,  3950,   832,   832,     0,   188,
    1200,     0,     0,     0,  2219,     0,   508,  2174,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1565,     0,     0,
       0,     0,     0,     0,     0,     0,   832,     0,     0,     0,
       0,     0,   803,   804,     0,     0,     0,     0,  3977,     0,
       0,     0,  3979,     0,   832,     0,  3459,  3460,  4175,  3461,
       0,  3997,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  4033,  4082,     0,     0,     0,     0,  3145,     0,
       0,     0,  3146,     0,     0,  1888,  1889,  1890,  1891,  4094,
    4095,     0,  1892,  3960,   509,     0,     0,     0,     0,     0,
       0,     0,     0,  2829,     0,     0,  4102,  2618,   832,     0,
    1893,  1894,   806,     0,     0,  1830,     0,     0,  4203,  4118,
       0,     0,  4121,  4122,  4123,  4124,     0,     0,     0,     0,
       0,     0,   836,   836,     0,     0,     0,     0,     0,   807,
       0,     0,  4089,     0,     0,  2407,     0,   808,     0,  4234,
       0,     0,     0,     0,     0,     0,     0,     0,  3147,     0,
    1895,  4160,   836,     0,  3694,     0,     0,  4246,     0,     0,
    4167,   809,     0,  4171,     0,     0,  4253,   186,     0,     0,
     836,  1769,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   510,     0,  1191,     0,     0,     0,  1565,     0,  1896,
       0,     0,     0,     0,     0,     0,  1922,  1924,     0,     0,
       0,     0,     0,   967,     0,     0,     0,     0,  3148,  2483,
     967,     0,     0,     0,   967,     0,     0,   967,  1819,  1306,
       0,     0,     0,     0,   836,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  3462,  3463,     0,     0,     0,  4208,
    4209,  4210,  4211,  4212,     0,     0,  4214,  4215,  4216,  4217,
    4218,  4219,  4220,  4221,  4222,  4223,  4224,     0,  3464,  4226,
       0,     0,     0,     0,     0,     0,     0,  4238,     0,     0,
       0,  4234,     0,     0,     0,     0,     0,     0,  4242,     0,
    4243,     0,     0,  3809,  3465,     0,     0,     0,  3151,     0,
    4251,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1897,     0,  3466,
       0,     0,     0,  1567,     0,     0,     0,     0,  1913,  4234,
    4234,     0,  2570,     0,     0,  4234,     0,     0,  4234,     0,
       0,     0,  4234,  4234,     0,     0,     0,     0,   967,     0,
       0,   967,  4395,     0,     0,     0,  3153,   780,     0,     0,
    3019,     0,   503,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1898,     0,     0,  4413,  4082,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  3047,     0,  4423,     0,     0,     0,     0,     0,     0,
       0,     0,  2605,     0,     0,  2606,  4234,  2607,  2608,  2609,
    4413,  4234,     0,  1899,     0,     0,     0,  3467,     0,     0,
    1567,  4461,     0,  2623,     0,     0,     0,  2624,     0,  2625,
       0,     0,     0,  4330,  4331,  2634,  4336,  2130,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    3468,     0,     0,  4339,  3469,  3470,     0,     0,  4343,  4344,
       0,  1888,  1889,  1890,  1891,     0,  2666,     0,  1892,     0,
       0,     0,     0,     0,     0,     0,  4394,     0,     0,  4396,
    3471,     0,  4398,     0,     0,     0,  1893,  1894,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   781,     0,
       0,  4513,  2195,     0,   782,   783,  1900,  2580,     0,     0,
       0,     0,     0,  2210,     0,     0,     0, -2559,     0,     0,
     785,     0, -2559,     0,  1600,     0, -2559, -2559, -2559,     0,
       0,  2226,     0,     0, -2559,  2581,  1895,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  3281,  1528,     0,     0,  2652,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   833,   833,     0,
       0,     0,     0,     0,     0,  1896,     0,     0,     0,     0,
    1567,     0,     0,     0,     0,     0,     0,     0,  2657,     0,
       0,     0,     0,  2658,     0,     0,     0,   833,     0,     0,
       0,     0,  2676,  1565,     0,     0,     0,     0,     0,     0,
    1901,     0,     0,     0,     0,   833,  2191,     0,     0,     0,
   -2559,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1200,  1200,     0,     0,     0,     0,     0,
    2716,     0,  3472,     0,     0,     0,     0,     0,  2728,  2728,
       0,     0,     0,  2175,     0,     0,  2734,     0, -2559,     0,
       0,     0,     0,     0, -2559, -2559,     0,     0,     0,   833,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   -2559,     0,     0,     0,     0,     0,   786,   787,   788,     0,
       0,     0,   943,  1897,     0,     0,     0,     0,   789,     0,
       0,     0, -2559,     0,     0,     0,     0, -2559,     0,     0,
       0, -2559, -2559, -2559,     0,  1746,     0,  1747,  1748, -2559,
       0,     0,     0,     0,     0,   504,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1902,     0,     0,
       0,     0,  1903,     0,     0,     0,     0,     0,     0,     0,
    1898,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  3336,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  3348,     0,
    1904,     0,     0,   790,   791,   792, -2559,     0,     0,  1899,
       0,     0,  1905,   793,     0,   794,     0,     0,     0,  1191,
       0,     0,   505,     0, -2559, -2559,   795,   796,   797,     0,
       0,     0,     0,     0,     0,   798,     0,     0,   506,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   799,     0,     0,     0,     0,     0,
    3392,     0,     0, -2559,     0,     0, -2559, -2559, -2559, -2559,
   -2559,     0,     0,  2867,     0,     0,     0,     0, -2559,     0,
       0,     0,     0,     0,     0, -2559,     0,     0,     0, -2559,
   -2559,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1769,     0,  1900,     0,     0, -2559,     0,     0,     0,     0,
       0,  1906,     0,     0,     0,     0,     0,     0,     0,   800,
       0,  1200,     0,  1200,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1819,     0, -2559,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   801,     0,  1200,  2552,     0,
    2925,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -2559, -2559, -2559, -2559, -2559,     0,     0,
       0,  1200,     0, -2559,     0, -2559,     0,     0,   852,     0,
       0,     0, -2559,     0,     0,     0, -2559, -2559, -2559,     0,
       0, -2559,     0,     0,     0, -2559,   507,     0, -2559,     0,
       0,     0,     0,     0,     0,     0,  1901,     0,  2647, -2559,
       0,     0,     0,     0, -2559,     0,     0,     0,     0,   837,
     837,   781,     0,     0,     0,     0,     0,   782,   783,     0,
       0,     0,     0,  2992, -2559,     0,     0,     0,     0,     0,
       0,     0,     0,   785,     0,     0,   802,     0,  1565,   837,
       0, -2559, -2559, -2559,     0,     0,  2623,     0, -2559,     0,
       0,     0,     0, -2559,     0,     0,     0,   837, -2559,     0,
       0,     0,     0,     0, -2559, -2559,     0, -2559,     0, -2559,
    3032,     0,     0,     0,     0,     0, -2559,     0,  3336,     0,
   -2559,     0, -2559,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0, -2559,     0,     0,     0,     0,
    2192,  1746,     0,  1747,  1748,     0,     0,     0,   508,     0,
   -2559,   837,     0,     0,     0,     0,     0, -2559,     0,     0,
       0,     0,     0,  1902,     0,  3068,     0,     0,  1903,     0,
       0,     0,  2686,  1200,   803,   804,  3587,     0, -2559, -2559,
   -2559, -2559, -2559,     0,     0,     0, -2559,     0, -2559,  2925,
   -2559,     0,     0,  1565,     0,     0,     0, -2559,     0,     0,
       0, -2559, -2559, -2559,  1565,     0,  1904,     0,     0,     0,
   -2559,     0,     0, -2559,     0,     0,     0,     0,  1905,  2193,
       0,     0,     0,     0,     0,     0,   509,     0,  3617, -2559,
    2194,     0,     0,     0,     0,     0, -2559,     0,     0,   786,
     787,   788,     0,     0,   806,     0,     0,     0,     0, -2559,
       0,   789,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   807,     0, -2559,     0,     0,     0,   780,     0,   808,
       0,     0,   503, -2559,     0,     0,  3655,     0,     0,     0,
       0,     0, -2559,     0, -2559,     0,     0,     0,     0,     0,
       0, -2559,     0,   809,     0,     0,     0, -2559, -2559,   186,
       0,     0,     0,     0,     0,     0,     0,  1906,     0,     0,
   -2559,     0,     0,   510,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0, -2559, -2559,   790,   791,   792,     0,
       0,     0, -2559,     0,   852,     0,     0,     0,     0,  2867,
       0,     0,     0,     0,     0,  3418,     0,     0,     0,   795,
     796,   797,     0,     0,     0,     0,     0,     0,   798,     0,
    3587, -2559,     0, -2559,     0, -2559,     0, -2559,     0,  2582,
       0, -2559,     0, -2559, -2559, -2559, -2559,     0, -2559, -2559,
    2583,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0, -2559,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   781,     0,
       0, -2559,     0,     0,   782,   783,  3766,  3768,  3766,     0,
       0, -2559,     0,     0,     0,     0,     0,     0,     0, -2559,
     785,     0,     0,     0,     0,     0,   943,     0,     0,     0,
       0,     0,   800,     0,     0,     0, -2559,     0,  3295,     0,
       0, -2559,     0, -2559,     0, -2559, -2559, -2559,     0, -2559,
       0,     0,     0, -2559,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -2559,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -2559,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2652,     0,     0,     0,     0,     0,
       0,     0, -1622,     0,     0,     0,     0, -1622,     0, -2559,
   -2559,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  3843,     0,
       0,     0,     0,     0,  3843,     0,     0,   883,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0, -2559, -2559,
   -2559,     0, -2559,     0,     0,     0, -2559,     0, -2559, -2559,
   -2559, -2559,     0, -2559, -2559,     0,     0,     0,     0,   802,
       0,     0,     0,     0,  2229,     0,     0,     0,     0, -2559,
       0,     0,     0,     0,     0,     0,     0, -2559,     0,     0,
     884,     0,     0, -2559, -2559,     0,   786,   787,   788,     0,
       0,     0,  1191,     0,     0,     0, -2559,     0,   789, -2559,
       0,     0,     0,     0, -2559,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   852,     0,     0,   504,     0,     0, -2559,     0,
       0,     0,     0, -1622, -2559,     0,     0,   885, -2991, -1622,
   -1622,  3066,     0,     0,     0,     0,     0,     0, -2559,     0,
       0,     0,     0,     0,     0, -1622,     0,   803,   804,     0,
       0,     0,     0,     0,  3075,  3075,     0,     0,     0,     0,
       0,  3086,  1661,     0,     0,     0,     0,  -255,     0,     0,
    -255,     0,     0,   790,   791,   792,     0,     0,     0,     0,
       0,     0,     0,   793,     0,   794,     0,     0,  -255,     0,
       0,     0,   886,     0,     0, -2559,   795,   796,   797,     0,
       0,  -255,     0,  4034,     0,   798,     0,     0,   506,     0,
    3515,     0,     0, -2559,     0,     0,     0,   806,  -255,  3766,
       0,   887,     0,     0,   799,     0,   780,     0,     0,     0,
    3295,   503,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   807,     0,     0,  3537,     0,     0,
       0,     0,   808,     0,     0, -2559, -2559, -2559,     0,     0,
       0,     0,     0,     0,     0,     0,     0, -2559,     0,     0,
       0,     0,     0,     0,     0,     0,   809,     0, -2559, -2559,
       0,     0,   186,  -255,     0,     0,     0,     0,     0,   800,
       0,     0,     0,     0, -2559,     0,   888,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  3572,     0,     0,     0,
       0, -1622, -1622, -1622,  3576,   801,  3577,     0,     0,   781,
       0,     0,     0, -1622, -2559,   782,   783,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   785,     0,     0,     0,     0,  3766,     0,     0,     0,
   -1622,     0, -2559, -2559, -2559, -2559, -2559,     0,     0,     0,
       0,     0, -2559,     0, -2559,     0,   507,   781,     0,     0,
       0, -2559,     0,   782,   783, -2559, -2559, -2559,     0,     0,
       0,     0,     0,     0, -2559,     0,     0, -2559,     0,   785,
       0,  3268,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -2559,     0,     0,     0,     0, -1622, -1622,
   -1622,     0,     0,     0,     0,     0,   802,     0, -1622,     0,
   -1622,     0,     0, -2559,     0,     0,     0, -1622,     0,     0,
       0, -1622, -1622, -1622,     0,     0,     0,     0,     0,     0,
   -1622,     0,     0, -1622,     0,     0,     0, -2559,     0,     0,
       0,     0,     0,  3663,     0,     0,     0, -2559,     0, -1622,
       0,     0,     0,     0,     0,     0, -2559,     0, -2559,     0,
    -255,  -255,     0,     0,     0, -2559,  3675,  3678,     0,     0,
       0, -2559,     0,     0,  3341,     0,     0,     0,   508,     0,
       0,     0,   889,     0, -2559,     0,     0, -3133,  3693,     0,
       0,  3696,     0,     0,     0,     0,     0,   786,   787,   788,
       0,     0,     0,     0,   803,   804, -2559,   890,     0,   789,
       0,     0,     0,     0, -1622,     0,     0,     0,     0,     0,
       0, -1622,     0,     0,     0,     0,  3735,     0,     0,     0,
       0,     0,     0,     0,     0, -2559,     0,     0,     0,     0,
   -1622,     0,     0,     0,  -255,   786,   787,   788,     0,     0,
       0,     0,   891,     0,     0,     0,   509,   789,     0,   647,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   806,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   504, -2559,     0,     0,     0,     0,
       0, -1622,     0,     0,     0,     0,   792,     0,     0,  -255,
    3675,   807,     0,  3678,     0,     0,     0,     0,     0,   808,
       0,     0, -2991,  1058,     0,  -255,     0,   795,   796,   797,
       0,     0,     0,     0,     0,     0,   798,     0,     0,     0,
       0,     0,     0,   809,     0,     0,     0,   892,     0,   186,
       0, -1622,   790,   791,   792,     0,     0,  1565,     0,     0,
       0,     0,   793,   510,   794,     0,     0, -2559,     0,     0,
       0,   505,     0,     0,     0,   795,   796,   797,     0,     0,
       0,     0,     0,     0,   798,     0,     0,   506,  -255,     0,
       0,     0,     0, -2559, -2559,     0,     0,     0,     0,     0,
       0,     0,     0,   799,     0,     0,     0,     0,     0,     0,
    3511,  3512,  3513,  3514,     0, -1622,     0,     0,     0,     0,
     800,     0,     0, -1622,     0,     0,     0,     0,     0,  -255,
    3868,     0, -2559,     0, -2559,     0, -2559,  -255,     0,     0,
   -2559,     0, -2559, -2559, -2559, -2559, -3133, -2559, -2559, -1622,
   -1622,     0,  3535,  3536,  1565,     0,     0,     0,     0,     0,
       0,     0,     0, -2559,     0,     0,     0,     0,   800,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   -2559,     0,     0,     0,   801,     0,     0,     0, -2559,     0,
       0, -1622,  -255,     0,  3913,     0,     0,     0,     0,     0,
    3914,     0,     0,     0,     0,     0,  -255,     0,     0, -1622,
    3928,     0, -2559,     0,     0,     0,     0,     0, -2559,     0,
   -1622,  -255,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0, -2559,     0,     0,   507, -1622,   802,     0,     0,
       0,     0,     0,     0, -1622,     0,     0,     0,     0,     0,
       0, -2991,     0,  3928,   780,     0,  3982,  3987,     0,   503,
       0,     0,     0,     0,     0,     0,     0,     0, -1622,     0,
       0,     0,  4012,  4023, -1622,     0,     0,  4085,  -255,  -255,
       0,     0,     0,   648,  -255,   802,     0,     0, -1622,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -255,     0,
       0,     0,     0,     0,  1565,     0,     0,  3982,     0,     0,
       0,     0,  3987,     0,     0,     0,     0,  -255,     0,     0,
       0,     0,     0,     0,  4120,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   803,   804,     0,     0,  4131,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    4144,     0,     0,     0,     0,     0,     0,   508,     0,     0,
       0,     0,  4159,     0,     0,     0,     0,     0,  3671,     0,
       0,  -255,     0,     0,     0,  -255,     0,  2173,     0,     0,
       0,     0,     0,   803,   804,     0,     0,     0,     0,     0,
       0,  -255,  -255,     0,  4178,   781,     0,  4179,     0,     0,
       0,   782,   783,     0,     0,   806,     0,     0,     0,     0,
       0,   784,     0,     0,     0,     0,     0,   785,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   807,     0,     0,   509,     0,     0,     0,     0,
     808,     0,     0,     0,     0,  4202,     0,     0,  4204,  4205,
    4206,  4207,     0,   806,     0,     0,     0,     0,  4213,     0,
       0,     0,     0,     0,   809,     0,     0,     0,     0,     0,
     186,  4225,     0,     0,     0,     0,     0,  4233,     0,     0,
     807,     0,     0,  4159,  4159,     0,     0,     0,   808,     0,
       0,     0,     0,     0,     0,  2867,     0,     0,     0,     0,
    3794,     0,     0,     0,  2867,     0,     0,     0,     0,     0,
       0,     0,   809,     0,     0,     0,     0,   476,   186,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   510,     0,     0,  4258,  4259,  4260,  4261,  4262,
    4263,  4264,  4265,  4266,  4267,  4268,  4269,  4270,  4271,  4272,
    4273,  4274,  4275,  4276,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   852,  4159,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   852,     0,
       0,     0,     0,   786,   787,   788,     0,     0,     0,     0,
    4306,     0,     0,     0,     0,   789,     0,     0,     0,     0,
       0,     0,  4307,     0,     0,     0,     0,     0,     0,  4233,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   504,  1888,  1889,  1890,  1891,     0,     0,     0,
    1892,     0,     0,  4325,  4326,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1893,  1894,
       0,     0,  4338,     0,     0,     0,     0,  4233,  4233,     0,
       0,     0,     0,  4233,     0,  4384,  4233,     0,     0,     0,
    4233,  4233,     0,     0,     0,     0,     0,     0,     0,     0,
     790,   791,   792,     0,     0,     0,  4399,     0,     0,     0,
     793,     0,   794,     0,     0,     0,     0,     0,  1895,   505,
       0,     0,     0,   795,   796,   797,     0,     0,     0,     0,
       0,  4415,   798,     0,     0,   506,     0,     0,     0,     0,
       0,     0,     0,  1200,  1200,     0,     0,     0,     0,     0,
       0,   799,     0,     0,     0,     0,     0,  1896,     0,     0,
       0,     0,  4460,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1200,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0, -2868,     0,     0,
       0,  1200,  1321,  4482,  4482,     0, -2868,     0,     0,  1322,
       0,     0,     0,     0,     0,     0,     0,  4487,     0,     0,
       0, -2868,  4490,     0,     0,     0,   800,     0,     0,     0,
       0,     0,  4493,  4497,     0,     0,     0,     0,  4482,  1323,
   -2868,     0,     0,     0,     0,     0,  4506,     0,     0,     0,
       0,     0,   801,     0,     0,  1200,     0,     0,     0, -2868,
    1324,  1325,  1326,  1327,  1328,  1329,  1330,  1331,     0,     0,
       0, -2868, -2868, -2868,     0,  1332, -2868,     0,     0,     0,
    1333,     0,     0, -2868,     0,  1897,     0,     0,     0,     0,
       0,  1334,  1335,  1336,  1337, -2868,     0, -2868, -2868,     0,
       0,     0,     0,   507,     0,     0,     0,     0,     0,  1338,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1339,
       0,     0,  1340,  1341,  1342, -2868,  1343,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1898,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   802,     0,     0,     0,     0,     0,     0,
    1344,     0,     0,     0,     0,     0,     0,     0,     0,  1345,
    1346,  1347,  1348,  1349,  1350,  1351,  1352,     0,     0,     0,
   -2868,  1899,  1353,  1354,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -2868,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1355,  1356,
       0,     0,  1357,  1358,     0, -2868, -2868,  1359, -2868, -2868,
       0,     0,     0,     0,     0,   508,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1360,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   803,   804,     0,     0,     0,     0,     0,     0,     0,
       0,     0, -2868,  1361,     0,     0,   805,  1362,     0,     0,
       0,     0,     0,     0,  1900,     0,     0,     0, -2868,     0,
       0,     0,  1363,     0,     0,     0,     0,  1364,  1365,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   509,     0,  1366,  1367,  1368,  1369,  1370,
       0,     0,  1371,     0,     0,     0, -2868, -2868, -2868,     0,
       0,   806,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0, -2868,     0,
       0,     0,     0, -2868,     0,     0,     0,     0,   807,     0,
       0,  1372,  1373,  1374,  1375,     0,   808,     0,     0,     0,
    1376,  1377,     0,  1378,     0,  1379,  1380,  1381, -2868, -2868,
    1382,     0,  1383,     0, -2868, -2868,  1384,     0,  1901,     0,
     809,     0,     0,     0,     0,     0,   186,     0,     0,     0,
       0,     0,     0,     0,     0,  1385,  1386,     0,     0,     0,
     510,     0,     0,     0,  1387,  1388,  1389,  1390,  1391,  1392,
       0,     0,     0,     0,     0,     0,     0,     0,  1393,     0,
       0,     0,  1394,     0,     0,     0,  1396,     0, -2868,     0,
   -2868,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0, -2868,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   -2868, -2868, -2868,     0,     0,  1397,     0,     0,     0,  1398,
       0,  1399,  1400,  1401,  1402,     0,     0,     0,     0,     0,
       0,  1403,     0,  1746,     0,  1747,  1748, -2868,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1404,     0,
       0,     0,     0,     0,     0,  1902,  1405,     0,     0,     0,
    1903, -2868, -2868, -2868,     0, -2868,     0, -2868, -2868,     0,
   -2868,     0, -2868, -2868,     0,     0,  1497, -2868,     0, -2868,
   -2868, -2868, -2868,     0,     0,     0,     0,     0,     0,     0,
       0,  1406,  1407,     0,     0, -2868,     0,     0,  1904,     0,
       0,     0,     0, -2868,     0,     0,     0,     0,     0,     0,
    1905,     0,     0,     0,     0,     0, -2868, -2868,     0,     0,
       0,     0,     0,     0,  1408, -2868,     0,  1409,     0,     0,
       0, -2868,     0,     0,     0,     0,     0,     0,  1410,     0,
       0,     0,     0,     0,  1411,     0,     0, -3074,  1412,     0,
    1413,  1414,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1415,     0,     0,  1416,  1417,     0,
       0,     0,  1418,     0,     0,     0,     0, -2868,     0,     0,
       0,     0, -2868,     0,     0,     0,  1419,     0,  1420,  1421,
       0,     0,     0, -3074,     0,     0,     0,  1422,  1423,     0,
       0,     0,     0,     0,     0, -3074,     0,     0,     0,  1906,
       0,     0,     0,     0,     0,     0,     0,  1424,  1425,  1426,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0, -2868, -2868, -2868,     0,     0,     0,  1427,  1428,  1429,
    1430, -2868,  1431,     0,     0,     0,  1432,  1433,     0,     0,
       0, -3074,     0,     0,  1434,  1435,     0,     0,     0,     0,
       0,     0,  1436,  1437,  1438, -2868,     0,     0,     0,     0,
    1439,     0,     0,     0, -3074,  1440,     0,     0, -2868, -2868,
   -2868, -2868,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   308,  1441,     0,     0,   309,     0, -2868, -2868,
       0,   310,     0,     0,     0,     0,  1442,  1443,   311, -2868,
       0,     0,  1444,     0,     0,     0,     0,   312,  1498,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0, -3074,     0,     0,     0,  1445,  1446,     0,     0, -2868,
       0,     0,     0,     0,  1447,     0,     0,     0,     0,     0,
       0,     0,  1448, -2868, -2868,     0,     0,     0,  1449,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   313,   314,
       0,     0,     0,     0,     0,     0,     0,     0,  1450,     0,
   -2868,     0,     0,     0,  1451,     0,     0,     0,     0,     0,
   -2868,     0,     0,     0,     0,     0,     0, -2868,     0,   315,
       0,     0, -3074,     0,     0,     0,     0,     0, -2868,     0,
       0,     0, -2868, -2868, -2868,     0,     0,     0,     0,   316,
       0,     0,     0,   317,     0,  1452,     0,     0,     0, -2868,
   -2868,  1453,     0,  1454, -2868, -2868,  1455, -2868,     0,     0,
       0,   318,     0,  1456, -2868,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1457,     0,     0,  1458,     0,
       0,     0,     0,     0,     0, -3074,     0,     0, -1383,     0,
       0,     0,     0, -3074,     0,     0,     0,     0,     0,     0,
       0,     0,   319,     0,     0,     0,     0,     0,   320,     0,
     321,     0,     0,     0,   322,     0,     0,     0,   323,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   324,     0, -1383,     0,     0,
       0, -1383, -1383, -1383, -1383, -1383,     0, -1383, -1383, -1383,
   -1383,     0, -1383, -1383,     0, -1383, -1383,     0, -1383, -1383,
   -1383, -1383, -1383, -1383, -1383, -1383, -1383, -1383, -1383,     0,
       0,   325,     0, -3074,     0,     0,     0,     0,     0, -1383,
   -3074,     0,     0,     0, -1383,     0,     0,   326,     0,     0,
       0,     0, -1383,     0,     0,     0,     0,   327,   328,     0,
       0,     0,     0,     0,     0, -3074,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   308,
       0,     0,     0,   309,     0,     0,     0,     0,   310,     0,
       0,     0,     0, -3074,     0,   311,     0,     0,     0,     0,
       0,     0,     0,     0,   312,     0,   329,     0,     0,     0,
       0,     0,     0,     0,   330,     0,     0,   331,     0,     0,
       0,     0,   332,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   333,     0,
       0,     0,     0,     0,     0,     0,   334,     0,   335,     0,
       0,   336,     0,     0,   337,   313,   314,  1089,     0, -1383,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   338,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   315,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   316,     0,     0,     0,
     317,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0, -3074,   318,     0,
       0, -3074,     0,     0,     0,   339,     0,     0,     0,     0,
       0,   340,     0,   341,     0,     0,   342,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   343,     0,     0,     0,     0,     0,     0,   319,
       0, -1383,     0,     0,     0,   320,     0,   321,     0,     0,
       0,   322,     0,     0,     0,   323,     0,     0,     0,     0,
   -1383, -1383, -1383,     0, -1383, -1383, -1383, -1383,     0,     0,
       0,     0,   324,     0,     0,     0,     0, -3074,     0,     0,
       0,     0,     0,     0,   344,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   503,   345,   325,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   326,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   327,   328,     0, -3074,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   346,     0,     0,
       0, -3074,     0,   347,     0,     0,     0,   348,     0,     0,
     349,   350,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   308,     0,     0, -3074,   309,   351,     0,     0,
       0,   310,     0,   329,     0,     0,     0,     0,   311,     0,
       0,   330,     0,   352,   331,     0,     0,   312,   353,   332,
       0,     0,     0,     0,     0,   354,     0,     0,     0,     0,
       0,   355,     0,     0,     0,     0,     0, -3074,     0,     0,
       0,     0,     0,     0,     0,   333,     0,     0,     0,     0,
     356,     0,     0,   334,     0,   335,     0,     0,   336,     0,
       0,   337,   781,     0,   357,     0,     0,     0,   782,   783,
     358,     0,     0,     0,     0,     0,   338,     0,   313,   314,
       0,     0,     0, -3074,   785,     0, -1383,     0,   359,     0,
       0,     0,     0,     0,     0, -3074,     0,  3132,     0,     0,
       0, -3074,   360, -3074,     0,     0,     0,     0,     0,   315,
       0,     0,   361,   362,     0,     0,     0,     0,     0,     0,
       0,   363,     0,     0,   364,     0,     0,     0,     0,   316,
       0,     0,     0,   317,     0,     0,     0,     0,     0,   365,
       0,     0,   339,  3133,     0,     0,     0,     0,   340,     0,
     341,   318,     0,   342,     0,  3134,     0,     0,     0,     0,
       0, -1383,     0,     0,     0,     0,     0,     0,     0,   343,
       0,     0, -1383,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   367,     0,     0,     0,
       0,     0,   319,     0,     0,     0,     0,     0,   320,   368,
     321,  3135,     0,     0,   322,   369,     0,     0,   323,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   344,     0,     0,  3136,   324,     0,     0,     0,   370,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   308,
       0,     0,     0,   309,   345,     0,     0, -1383,   310,     0,
     786,   787,   788,     0,     0,   311,     0,     0,   371,     0,
       0,   325,   789,   372,   312,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   326,     0,     0,
       0,  3137,     0,     0,   346,     0,     0,   327,   328,   504,
     347,     0,     0,     0,   348,     0,     0,   349,   350,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   351,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   313,   314,     0,     0,     0,
     352,     0,     0,     0,     0,   353,   329,     0,     0,     0,
       0,     0,   354,     0,   330,     0,     0,   331,   355,   792,
       0,     0,   332,     0,     0,     0,   315,     0,     0,     0,
       0,     0,  3138,     0,     0,     0,   505,   356,     0,     0,
     795,   796,   797,     0,     0,     0,   316,     0,   333,   798,
     317,   357,   506,     0,     0,     0,   334,   358,   335,     0,
       0,   336,     0,     0,   337,     0,     0,     0,   318,     0,
       0,     0,     0,     0,     0,   359,     0,     0,     0,   338,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   360,
       0,     0,     0,     0,     0,  3139,     0,     0,     0,   361,
     362,     0,     0,  3140,     0,     0,     0,     0,   363,   319,
       0,   364,     0,     0,     0,   320,     0,   321,     0,     0,
       0,   322,     0,     0,     0,   323,   365,     0,     0,     0,
       0,     0,     0,   800,     0,     0,     0,     0,     0,     0,
       0,     0,   324,     0,     0,   339,     0,     0,   366,     0,
       0,   340,     0,   341,     0,     0,   342,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   343,   367,     0,     0,     0,     0,   325,     0,
       0,     0,     0,  3141,     0,     0,   368,     0,     0,     0,
    3142,     0,   369,     0,   326,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   327,   328,     0,     0,     0,     0,
     507,     0,     0,     0,     0,  3143,   370,     0,     0,     0,
       0,     0,     0,     0,   344,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  3144,     0,   371,     0,   345,     0,     0,
     372,     0,     0,   329,     0,     0,     0,     0,     0,     0,
     802,   330,     0,     0,   331,     0,     0,     0,     0,   332,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   346,     0,     0,
       0,     0,     0,   347,     0,   333,     0,   348,     0,     0,
     349,   350,     0,   334,     0,   335,     0,     0,   336,     0,
       0,   337,     0,     0,     0,     0,     0,   351,     0,     0,
       0,     0,     0,     0,     0,     0,   338,     0,     0,     0,
       0,     0,   508,   352,     0,     0,     0,     0,   353,     0,
       0,     0,     0,     0,     0,   354,     0,     0,     0,     0,
       0,   355,     0,     0,     0,     0,     0,     0,   803,   804,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     356,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   357,     0,     0,  3145,     0,     0,
     358,  3146,   339,     0,     0,     0,     0,     0,   340,     0,
     341,     0,     0,   342,     0,     0,     0,     0,   359,     0,
     509,     0,     0,     0,     0,     0,     0,     0,     0,   343,
       0,     0,   360,     0,     0,     0,     0,     0,   806,     0,
       0,     0,   361,   362,     0,     0,     0,     0,     0,     0,
       0,   363,     0,     0,   364,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   807,     0,     0,     0,   365,
       0,     0,     0,   808,     0,     0,     0,  3147,     0,     0,
       0,   344,     0,     0,     0,     0,     0,     0,   544,     0,
       0,  1609,     0,   545,     0,     0,     0,   809,     0,     0,
     546,     0,     0,   186,   345,     0,     0,     0,     0,   547,
       0,     0,     0,     0,     0,     0,   367,   510,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   368,
       0,     0,     0,     0,     0,   369,     0,  3148,     0,     0,
       0,     0,     0,     0,   346,     0,     0,     0,     0,     0,
     347,  3149,     0,     0,   348,     0,     0,   349,   350,   370,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     548,   549,     0,     0,   351,  3150,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   371,     0,
     352,     0,     0,   372,     0,   353,     0,     0,     0,     0,
       0,   550,   354,     0,     0,     0,     0,     0,   355,     0,
       0,     0,     0,     0,     0,     0,     0,  3151,     0,     0,
       0,   551,     0,     0,     0,   552,     0,   356,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   357,     0,   553,     0,     0,     0,   358,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  3152,     0,   359,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  3153,     0,     0,     0,   360,
       0,  3154,     0,  3155,   554,     0,     0,     0,     0,   361,
     362,     0,     0,     0,     0,     0,   555,     0,   363,     0,
     556,   364,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   365,     0,     0,   557,
       0,     0,     0,     0,   558,   559,   560,   561,     0,   562,
     563,   564,   565,     0,   566,     0,   567,   568,   569,     0,
     570,   571,   572,   573,   574,   575,   576,   577,   578,   579,
     580,     0,     0,   581,     0,     0,     0,  3170,     0,     0,
       0,     0,     0,   367,     0,     0,     0,     0,     0,   582,
       0,     0,     0,     0,     0,     0,   368,     0,     0,   583,
     584,     0,   369,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   370,     0,     0,     0,
       0,     0,     0,  2257,  2258,  2259,  2260,  2261,     0,  2262,
    2263,     0,     0,     0,     0,     0,     0,     0,   585,     0,
       0,     0,     0,     0,     0,   371,   586,     0,     0,   587,
     372,     0,     0,     0,   588,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     589,     0,     0,     0,     0,     0,     0,     0,   590,     0,
     591,     0,     0,   592,     0,     0,   593,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  2265,     0,  2266,
    2267,  2147,  2268,  2269,  2270,  2271,  2272,  2273,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   594,     0,  2274,
       0,     0,     0,   595,     0,   596,     0,     0,   597,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   598,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1321,     0,     0,     0,     0,     0,     0,
    1322,     0,     0,     0,  1153,     0,     0,     0,     0,     0,
       0,     0, -3133, -3133, -3133, -3133,     0,     0,     0, -3133,
       0,     0,     0,     0,     0,     0,   599,     0,     0,     0,
    1323,     0,     0,     0,     0,     0,     0, -3133, -3133,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   600,
       0,  1324,  1325,  1326,  1327,  1328,  1329,  1330,  1331,     0,
       0,  2275,  2276,  2277,  2278,  2279,  1332,     0,  2155,  2280,
       0,  1333,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1334,  1335,  1336,  1337, -2983, -3133,     0,     0,
   -2983,     0,     0,     0,     0,     0,     0,     0,     0,   601,
    1338,     0,     0,     0,  2281,     0,     0,     0,     0,     0,
    1339,     0,     0,  1340,  1341,  1342,     0,  1343,     0,   602,
       0,     0,     0,     0,     0,     0, -3133,     0,  2282,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     603,     0,     0,     0,     0,     0,     0,   604,     0,     0,
       0,  1344,     0,   605,     0,     0,     0,     0,     0,     0,
    1345,  1346,  1347,  1348,  1349,  1350,  1351,  1352,     0,     0,
       0,     0,   606,  1353,  1354,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   607,     0,     0,     0,     0,     0,     0,  1355,
    1356,     0,     0,  1357,  1358,     0,     0,     0,  1359,     0,
     608,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   609,     0,     0,     0,     0,     0,
    1360,     0,     0,     0,   610,   611,  2285,     0,     0,     0,
       0,     0,     0,   612, -3133,     0,   613,     0,     0,     0,
       0,     0,     0,     0,  1361,     0,     0,     0,  1362,     0,
       0,   614,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1363,     0,     0,     0,     0,  1364,  1365,
       0,     0,     0,   615,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1366,  1367,  1368,  1369,
    1370, -3133,     0,  1371,     0,     0,     0,     0,   616,     0,
       0,  2287,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   617,     0,     0,     0,     0,     0,   618,     0,     0,
       0,     0,  2289,     0,     0,     0,     0,     0,     0,     0,
   -3133,     0,  1372,  1373,  1374,  1375,     0,     0,     0,     0,
       0,  1376,  1377,     0,  1378,  2290,  1379,  1380,  1381,     0,
       0,  1382,     0,  1383,     0,     0,     0,  1384,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     619,     0,     0,     0,     0,   620,  1385,  1386,     0,     0,
       0,     0,     0,     0,     0,  1387,  1388,  1389,  1390,  1391,
    1392,     0,     0,     0,     0,     0,     0,     0,     0,  1393,
       0, -2983,     0,  1394,     0,     0,     0,  1396,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -3133,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1156,     0,     0,     0,  1397,     0,     0,     0,
    1398,     0,  1399,  1400,  1401,  1402,     0,     0,     0,     0,
       0,     0,  1403,     0,     0,     0,  2294,  2295,  2296, -3133,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1404,
       0,     0,     0,     0,     0,     0,     0,  1405,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1497,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1406,  1407,     0,     0,     0, -3133,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0, -2983, -2983,     0,     0,  1408,     0,     0,  1409,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1410,
       0,     0,     0,     0,     0,  1411,     0,     0,     0,  1412,
       0,  1413,  1414,     0,     0,     0,  2299,  2300,  2301,     0,
       0,     0,     0,     0,     0,  1415,     0,     0,  1416,  1417,
       0,     0,     0,  1418,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1419,     0,  1420,
    1421,     0,     0,     0,     0,     0,     0,     0,  1422,  1423,
       0,     0, -3133,     0, -3133, -3133,     0,     0,     0,  3171,
       0,     0,     0,     0,     0,     0,     0,     0,  1424,  1425,
    1426,     0,     0,  2354, -3133,     0,  2355,     0,     0, -3133,
       0,     0,     0,     0,     0,     0,     0,     0,  1427,  1428,
    1429,  1430,     0,  1431,  2356,     0,     0,  1432,  1433,     0,
       0,     0,     0,     0,     0,  1434,  1435,  2357,     0,     0,
       0,     0,     0,  1436,  1437,  1438,     0, -3133,     0,     0,
       0,  1439,     0,     0,     0,     0,  1440,     0,     0, -3133,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1441,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1442,  1443,  1321,
       0,     0,     0,  1444,     0,     0,  1322,     0,     0,  1498,
       0,     0,   781,     0,     0,     0,     0,     0,   782,   783,
       0,     0,     0,     0,     0,     0,  1445,  1446,     0,   490,
       0,     0,     0,     0,   785,  1447,  1323,     0,     0,     0,
       0,     0,     0,  1448,     0,     0,     0,     0,     0,  1449,
       0,     0,     0,     0,     0,     0,     0,  1324,  1325,  1326,
    1327,  1328,  1329,  1330,  1331,     0,     0,     0, -3133,     0,
       0,     0,  1332,     0,     0,     0,     0,  1333,     0,  1450,
       0,     0,  1881,     0,     0,  1451,     0,     0,  1334,  1335,
    1336,  1337,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1338,     0,     0,   780,
       0,     0,     0,     0,   503,     0,  1339,     0,     0,  1340,
    1341,  1342,     0,  1343,     0,   476,  1452,     0,     0,     0,
       0,     0,  1453,     0,  1454,     0,     0,  1455,     0,     0,
       0,     0,     0,     0,  1456,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1457,  1344,     0,  1458,
       0,     0,     0,     0,     0,     0,  1345,  1346,  1347,  1348,
    1349,  1350,  1351,  1352,     0,     0,     0,     0,     0,  1353,
    1354,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     786,   787,   788,     0,     0,  1355,  1356,     0,     0,  1357,
    1358,     0,   789,     0,  1359,     0,  2358,  2359,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1360,     0,     0,   504,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     781,     0,     0,     0,     0,     0,   782,   783,     0,     0,
    1361,     0,     0,     0,  1362,     0,  1040,     0,     0,     0,
       0,     0,   785,     0,     0,     0,     0,     0,     0,  1363,
       0,     0,     0,     0,  1364,  1365,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   792,
    2360,     0,  1366,  1367,  1368,  1369,  1370,     0,     0,  1371,
       0,     0,     0,     0,     0,     0,   505,     0,     0,     0,
     795,   796,   797,     0,     0,     0,     0,     0,     0,   798,
       0,     0,   506,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1372,  1373,
    1374,  1375,     0,     0,     0,  2361,     0,  1376,  1377,     0,
    1378,     0,  1379,  1380,  1381,     0,     0,  1382,     0,  1383,
       0,  -336,     0,  1384,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1385,  1386,     0,     0,     0,     0,     0,     0,
       0,  1387,  1388,  1389,  1390,  1391,  1392,     0,     0,     0,
       0,     0,     0,   800,     0,  1393,     0,     0,     0,  1394,
    1395,     0,     0,  1396,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2362,     0,     0,     0,   786,   787,
     788,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     789,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  1397,     0,     0,     0,  1398,     0,  1399,  1400,
    1401,  1402,     0,     0,     0,  2363,     0,   504,  1403,     0,
     780,     0,     0,  2364,     0,   503,     0,     0,     0,     0,
     507,     0,     0,     0,     0,  1404,     0,     0,     0,     0,
       0,     0,     0,  1405,     0,     0,     0,     0,     0,     0,
   -1720,     0,     0,     0,     0,     0,     0,     0, -1720, -1720,
   -1720, -1720,     0,     0,     0, -1720,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   790,   791,   792,  1406,  1407,
     802,     0,     0, -1720, -1720,   793,     0,   794,  2365,     0,
       0,     0,     0,     0,   505,     0,     0,     0,   795,   796,
     797,     0,  2366,     0,     0,     0,     0,   798,     0,     0,
     506,  1408,     0,     0,  1409,     0,     0,  2367,     0,     0,
       0,     0,     0,     0,     0,  1410,   799,     0,     0,     0,
       0,  1411, -1720, -1720,     0,  1412, -1720,  1413,  1414,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  1415,   508,     0,  1416,  1417,     0,     0,     0,  1418,
       0,   781,     0,     0,     0,     0,     0,   782,   783,     0,
       0,     0, -1720,  1419, -2997,  1420,  1421,     0,   803,   804,
    2368,     0,     0,   785,  1422,  1423,     0,     0,     0,     0,
       0,   800,     0,     0,  2369,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1424,  1425,  1426,     0,     0,     0,
       0,     0,     0,  -333,     0,     0,     0,   801,     0,     0,
       0,     0,     0,     0,  1427,  1428,  1429,  1430,     0,  1431,
     509,     0,     0,  1432,  1433,     0,     0,     0,     0,     0,
       0,  1434,  1435,     0,     0,     0,     0,     0,   806,  1436,
    1437,  1438,     0,     0,     0,     0,     0,  1439,     0,     0,
       0,     0,  1440,     0,     0,     0,     0,  2370,   507,     0,
       0,  2371,     0,     0,     0,   807,     0,     0,     0,     0,
    1441,     0,     0,   808,     0,     0,     0,  2372,  2373,     0,
   -1720,     0,     0,  1442,  1443,     0,     0,     0,     0,  1444,
       0,     0,     0,     0,     0,     0,     0,   809,     0,     0,
       0,     0,     0,   186,     0,     0,     0,     0,   802,     0,
       0,     0,  1445,  1446,     0,     0,     0,   510,     0,     0,
       0,  1447,     0,     0,     0,     0,     0,     0,     0,  1448,
       0,     0,     0,     0,     0,  1449,     0, -1720,     0,   786,
     787,   788,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   789,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1450,     0,     0,     0,     0,
       0,  1451,     0,     0,     0,     0, -1720,     0,   504,     0,
     508,     0,   780,     0,     0,     0,     0,   503,     0,     0,
       0, -3035, -3035, -3035,     0,     0,     0,     0,     0,  1292,
       0,     0,     0,     0,     0,     0,   803,   804,     0,     0,
       0,     0,  1452,     0,     0,     0,     0,     0,  1453,     0,
    1454,  1041,     0,  1455,     0,     0,     0,     0,     0,     0,
    1456,     0,     0,     0,     0,     0,   790,   791,   792,     0,
       0,     0,  1457,     0,     0,  1458,   793, -1720,   794,     0,
       0,     0,     0,     0,     0,   505,     0,     0,   509,   795,
     796,   797,     0,     0,     0,     0,     0,     0,   798, -1720,
       0,   506,     0,     0,     0,     0,   806,     0,     0,     0,
       0,     0,     0,     0,     0, -3035,     0,   799, -1720,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   807,     0,     0,     0,   780,     0,     0,
       0,   808,   503,     0,     0, -1720,     0,     0,     0,     0,
       0,     0,     0,   781,  1292,     0,     0,     0,     0,   782,
     783,     0,     0,     0,     0,   809,     0,     0,     0,     0,
       0,   186,     0,     0,     0,   785,     0,     0,     0,     0,
       0,     0,   800,     0,     0,   510,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0, -1720,     0,     0,     0,     0,   801,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0, -1720, -1720,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   780,     0,
       0,     0,     0,   503,     0,     0,     0,     0,     0,   507,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   781,     0,
       0, -3035,     0,     0,   782,   783,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1293,
     785,     0,     0,     0,     0,     0,     0,     0, -1720,   802,
   -1720, -1720,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
   -1720,     0,     0,     0,     0, -1720,     0,     0,     0,     0,
       0,   786,   787,   788,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   789,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0, -3035, -3035,     0,     0,     0,     0,
       0,     0,     0, -1720,     0,     0,     0,     0,     0,     0,
     504,   508,     0,     0,     0, -1720,     0,     0,     0,   781,
       0,     0,     0,     0,     0,   782,   783,     0,     0,     0,
       0,     0,     0,     0,     0,   971,     0,   803,   804,     0,
    1294,   785,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1293,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   790,   791,
     792, -3035, -3035,     0,     0,     0,     0,     0,   793,     0,
     794,     0,     0,     0,     0,     0,     0,   505,     0,   509,
       0,   795,   796,   797,     0,     0,   786,   787,   788,     0,
     798,     0,     0,   506,     0,     0,     0,   806,   789,     0,
       0,     0,     0,     0, -1720,     0,     0,     0,     0,   799,
       0,     0,     0,     0,     0,     0,     0,     0, -1720,     0,
       0,     0,     0,     0,   807,   504,     0,     0,     0, -3035,
       0,     0,   808,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1295,     0,  1294,   809,     0,     0,     0,
       0, -1720,   186, -3035,     0,     0,     0,     0,     0,     0,
       0,     0, -3035,     0,   800,     0,   510,     0,     0,     0,
       0, -3035,     0,   790,   791,   792,     0,  1296,     0,   780,
       0,     0,     0,   793,   503,   794,     0,   786,   787,   788,
     801,     0,   505,     0,     0,     0,   795,   796,   797,   789,
       0,     0,     0,     0,     0,   798,     0,     0,   506,     0,
       0,     0, -3035,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   799,     0,   504,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   507,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1295,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   790,   791,   792,     0,     0,   800,
       0,   802,     0,     0,   793,     0,   794,     0,     0,     0,
       0,     0,  1296,   505,     0,     0,     0,   795,   796,   797,
       0,     0,     0,     0,     0,   801,   798,   780,     0,   506,
     781,     0,   503,     0,     0,     0,   782,   783,     0,     0,
       0,     0,     0,     0,     0,   799,     0,     0,     0,     0,
       0,     0,   785,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   780,
       0,     0,     0,   508,   503,     0,   507,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   780,   803,
     804,     0,     0,   503,     0,     0,     0,     0,     0,     0,
     800,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   802,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   801,     0,  1297,     0,
    1298,     0, -3035,     0,     0,     0, -3035,     0, -3035,  1299,
    1300,   509,     0,  1301,  1302,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   781,   806,
       0,     0,     0,     0,   782,   783,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   507,     0,     0,
     785,     0,     0,     0,     0,     0,   807,     0,   508,     0,
       0,     0,     0,     0,   808,     0,     0,     0,     0,     0,
     781,     0,     0,     0,     0,     0,   782,   783,   786,   787,
     788,     0,     0,     0,   803,   804,     0,     0,   809,     0,
     789,     0,   785,     0,   186,     0,     0,   802,     0,   781,
       0,     0,     0,     0,     0,   782,   783,     0,   510,     0,
       0,     0,     0,     0,     0,     0,     0,   504,     0,     0,
       0,   785,     0,  1297,     0,  1298,     0,     0,     0,     0,
       0,     0,     0,     0,  1299,  1300,   509,     0,  1301,  1302,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   806,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   508,
       0,     0,     0,     0,     0,   790,   791,   792,     0,     0,
       0,   807,     0,     0,     0,   793,     0,   794,     0,   808,
       0,     0,     0,     0,   505,   803,   804,     0,   795,   796,
     797,     0,     0,     0,     0,     0,     0,   798,     0,     0,
     506,     0,     0,   809,     0,     0,   786,   787,   788,   186,
       0,     0,     0,     0,     0,     0,   799,     0,   789,     0,
       0,     0,     0,   510,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   509,     0,     0,
       0,     0,     0,     0,     0,   504,     0,     0,   786,   787,
     788,     0,     0,     0,  1605,   806,     0,     0,     0,     0,
     789,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   780,     0,   786,   787,   788,
     503,   800,   807,     0,     0,     0,     0,   504,     0,   789,
     808,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   790,   791,   792,     0,   801,     0,     0,
       0,     0,     0,   793,   809,   794,   504,     0,     0,     0,
     186,     0,   505,     0,     0,     0,   795,   796,   797,     0,
       0,     0,     0,     0,   510,   798,     0,     0,   506,     0,
       0,     0,     0,     0,     0,   790,   791,   792,     0,     0,
       0,     0,     0,     0,   799,   793,     0,   794,   507,     0,
       0,     0,     0,     0,   505,     0,     0,     0,   795,   796,
     797,     0,     0,     0,   790,   791,   792,   798,  1827,     0,
     506,     0,     0,   503,   793,     0,   794,     0,     0,     0,
       0,     0,     0,   505,     0,     0,   799,   795,   796,   797,
       0,     0,     0,     0,     0,     0,   798,     0,   802,   506,
       0,     0,     0,     0,     0,     0,   781,     0,     0,   800,
       0,     0,   782,   783,     0,   799,   888,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   785,     0,
       0,     0,     0,     0,     0,   801,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   800,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     508,     0,     0,     0,     0,     0,     0,   801,     0,     0,
     800,     0,     0,     0,     0,     0,   507,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   803,   804,     0,     0,
       0,     0,     0,     0,     0,     0,   801,     0,     0,   781,
       0,     0,     0,     0,     0,   782,   783,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   507,     0,
       0,   785,     0,  1189,     0,     0,   802,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   509,     0,
       0,     0,     0,     0,     0,     0,     0,   507,     0,     0,
       0,     0,     0,     0,     0,     0,   806,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   802,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   807,   786,   787,   788,     0,     0,     0,
       0,   808,     0,     0,     0,     0,   789,   802,   508,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   809,     0,     0,     0,     0,
       0,   186,     0,   504,   803,   804,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   510,     0,     0,     0,     0,
     508,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  3084,     0,     0,     0,     0,   803,   804,     0,   508,
       0,     0,     0,     0,     0,     0,   509,     0,     0,     0,
       0,   790,   791,   792,     0,     0,     0,   786,   787,   788,
       0,   793,     0,   794,   806,   803,   804,     0,     0,   789,
     505,     0,     0,     0,   795,   796,   797,     0,     0,     0,
       0,     0,     0,   798,     0,     0,   506,     0,   509,     0,
       0,   807,     0,     0,     0,     0,   504,     0,     0,   808,
       0,     0,   799,     0,     0,     0,   806,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   509,     0,     0,
       0,     0,     0,   809,     0,     0,     0,     0,     0,   186,
       0,     0,     0,   807,     0,   806,     0,     0,     0,     0,
       0,   808,     0,   510,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   790,   791,   792,     0,     0,     0,
       0,     0,   807,     0,   793,   809,   794,   800,     0,     0,
     808,   186,     0,   505,     0,     0,     0,   795,   796,   797,
       0,     0,     0,     0,     0,   510,   798,     0,     0,   506,
       0,     0,     0,   801,   809,     0,     0,     0,     0,     0,
     186,     0,     0,     0,     0,   799,     0,     0,     0,     0,
       0,     0,     0,     0,   510,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  3826,
       0,     0,     0,     0,     0,  -570,     0,     0,     0,     0,
       0,     0,     0,     0,   507,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     800,     0,  -570,     0,     0,     0,     0,  -570,  -570,  -570,
    -570,  -570,     0,     0,  -570,  -570,  -570,  -570,  -570,  -570,
       0,  -570,  -570,  -570,   802,     0,   801,     0,     0,     0,
       0,     0,  -570,  -570,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -570,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -570,
       0,     0,     0,     0,  -570,     0,     0,   507,     0,     0,
       0,  -570,  -570,     0,     0,  -570,     0,     0,     0,     0,
       0,     0,  -570,     0,     0,     0,   508,     0,     0,  -570,
       0,  -570,  -570,  -570,  -570,  -570,  -570,  -570,  -570,  -570,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   803,   804,     0,     0,     0,   802,     0,     0,
    -570,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -570,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -570,     0,     0,   509,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   806,     0,     0,     0,     0,     0,     0,   508,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   807,
       0,     0,     0,     0,     0,   803,   804,   808,     0,     0,
    -570,     0,     0,     0,     0,     0,     0,     0,     0,  -570,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   809,     0,     0,     0,     0,     0,   186,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   510,     0,     0,  1079,     0,     0,   509,     0,     0,
       0,     0,     0,  -570,  -570,  -570,  -570,  -570,     0,     0,
    -570,  -570,     0,     0,     0,   806,  -570,     0,     0,     0,
       0,  -570,     0,     0,  -570,     0,  -570,     0,     0,     0,
       0,     0,     0,  -570,     0,     0,     0,     0,     0,     0,
       0,     0,   807,  -570,     0,     0,  -570,     0,     0,     0,
     808,     0,     0,     0,     0,  -570,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -570,     0,     0,     0,   809,     0,     0,     0,     0,     0,
     186,     0,     0,     0,     0,     0,     0,  -570,     0,     0,
       0,     0,     0,     0,   510,     0,     0,  -570,     0,  1078,
       0,  -570,     0,     0,     0,     0,     0,     0,     0,  -570,
       0,     0,     0,  -570,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -570,  -570,     0,     0,
    -570,     0,     0,  -570,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  -570,     0,
     503,     0,  -570,     0,     0,  -570,  -570,  -570,  -570,  -570,
       0,  -570,  -570,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -570,     0,     0,     0,     0,     0,  -570,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -570,     0,     0,     0,     0,     0,     0,
    -570,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  1518,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -570,     0,  -570,     0,  -570,     0,     0,  -570,
       0,  -570,  -570,  -570,  -570,  -570,  -570,  -570,  -570,  -570,
       0,     0,  -570,  -570,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -570,
       0,     0,     0,     0,  -570,     0,  -570,  -570,     0,     0,
       0,     0,     0,     0,     0,     0,   781,     0,     0,     0,
       0,     0,   782,   783,     0,     0,     0,  -570,     0,     0,
    -570,     0,     0,     0,     0,  -570,     0,     0,   785,  -570,
       0,  -570,     0,     0,  4283,     0,     0,  -570,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  -570,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  -570,     0,  -570,
    -570,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    -570,  -570,  -570,  -570,  -570,     0,  -570,  -570,     0,     0,
       0,     0,     0,     0,     0,  -570,     0,  -570,     0,     0,
       0,     0,  -570,     0,     0,     0,     0,     0,     0,  -570,
    -570,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  1079,     0,  -570,     0,  -570,  -570,
    -570,     0,  -570,  -570,  -570,  -570,  -570,  -570,     0,     0,
    -570,  -570,     0,     0,  -570,     0,  -570,     0,     0,  -570,
    -570,     0,     0,     0,     0,  -570,  -570,     0,     0,     0,
       0,     0,     0,  -570,  -570,     0,  -570,  -570,  -570,  -570,
    -570,  -570,  -570,  -570,  -570,     0,  -570,     0,  -570,     0,
       0,     0,     0,  1519,   786,   787,   788,     0,     0,     0,
       0,     0,     0,  -570,     0,     0,   789,     0,     0,     0,
    -570,     0,     0,  -570,     0,     0,     0,     0,  -570,     0,
       0,     0,     0,     0,  -570,     0,     0,  -570,     0,     0,
       0,     0,     0,   504,     0,     0,     0,  -570,     0,     0,
       0,  -570,     0,     0,     0,  -570,  -570,     0,     0,  -570,
       0,     0,     0,  -570,     0,     0,     0,     0,  -570,  -570,
    -570,  -570,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -570,     0,     0,   503,     0,     0,
       0,     0,  -570,     0,     0,     0,     0,     0,  -570,     0,
       0,   790,   791,   792,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -570,     0,     0,     0,     0,     0,     0,
    1520,   197,     0,     0,   795,   796,   797,     0,  -570,     0,
       0,     0,     0,   798,     0,     0,   506,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1518,     0,     0,     0,  1079,
       0,     0,     0,     0,     0,     0,     0,     0,  -570,  -570,
    -570,  -570,  -570,     0,     0,  -570,  -570,     0,     0,     0,
       0,     0,     0,     0,  -570,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -570,     0,     0,     0,     0,     0,  -570,
       0,  -570,     0,     0,     0,     0,     0,   800,     0,  -570,
       0,     0,     0,   781,  -570,     0,     0,     0,     0,   782,
     783,     0,     0,     0,     0,  -570,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   785,     0,  -570,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -570,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -570,     0,     0,  -570,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -570,     0,     0,   507,  -570,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -570,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -570,   802,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -570,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -570,     0,  -570,  -570,
    -570,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  -570,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -570,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1519,   786,   787,   788,  -570,     0,   508,     0,  -570,     0,
       0,     0,     0,   789,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -570,     0,     0,     0,     0,  -570,
       0,     0,   803,   804,     0,  -570,     0,     0,  -570,     0,
     504,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  -570,     0,     0, -3133,     0,     0,     0,     0,
       0,  -570,     0,     0,     0,     0,     0,     0,  -570,  -570,
       0,     0,     0,     0,     0,     0,     0,     0,  -570,  -570,
    -570,     0,     0,     0,   509,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  -570,     0,     0,     0,   790,   791,
     792,     0,   806,     0,     0,     0,     0,     0,  -570,     0,
       0,     0,     0,     0,     0,     0,  -570,   505,     0, -2085,
       0,   795,   796,   797,     0,     0,     0,     0,     0,   807,
     798,   197,     0,   506,  2024,     0,     0,   808,     0,     0,
       0,     0,     0,  2025,     0,     0,     0,     0, -2085,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2026,     0,
       0,   809,     0,     0,     0,     0,   476,   186,     0,     0,
       0,  -570,     0,  -570,  -570,  -570,     0,  2027,     0,     0,
       0,   510,     0,  -570,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2028,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2029,  2030,
    2031,     0,     0,  2032,   800,  -570,     0,     0,     0,     0,
    2033,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2034,     0,  2035,  2036,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  -570,     0,     0,     0,     0,     0,     0,
       0,     0,  2037,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  -570,     0,     0,     0,
       0,     0,     0,  -570,     0,     0,     0,     0,     0,     0,
       0,   507,     0,  -570,  -570,  -570,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  -570,
       0,     0,     0,     0,     0,     0,     0,  2038,     0,     0,
       0,     0,     0,  -570,     0,     0,     0,  -570,     0,     0,
    2039,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   802,     0,     0,     0,     0,   197,     0,     0,     0,
       0,     0,  2040,  2041,     0,  2042,  2043,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  1153,     0,     0, -3133,  2044,
       0,     0,     0, -3133, -3133, -3133, -3133,     0,     0,  1154,
   -3133,     0,     0,   508,     0,  2045,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0, -3133,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   803,
     804,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  2046,  2047,  2048,     0,     0,     0,     0,
       0,     0, -3133,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2049,     0, -2983, -3133,     0,
    2050, -2983,     0,     0,     0,     0,  4346,     0,     0,     0,
       0,   509,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2051,  2052,     0,     0,   806,
       0,  2053,  2054,     0,     0, -3133,     0, -3133,     0,     0,
       0,     0,     0,     0,     0,     0, -2085,     0,     0, -3133,
       0,     0,     0,     0,     0,     0,   807,     0,     0,  2257,
    2258,  2259,  2260,  2261,   808,  2262,  2263,     0,     0,     0,
       0,     0, -3133,     0,     0, -2085,  2264,     0,     0,     0,
       0,     0,     0,     0,     0,  2055,     0,  2056,   809,     0,
       0,     0,     0,   476,   186,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2057,     0,     0,     0,   510,     0,
       0,     0,     0,     0,     0,     0,     0,  2058,  2059,  2060,
       0,     0,     0,     0,     0,  4347,     0,     0,  1774,  4348,
       0,     0,     0,     0,     0,  1775,     0,     0,     0,     0,
       0,     0,     0,  2265,  2061,  2266,  2267,  2147,  2268,  2269,
    2270,  2271,  2272,  2273,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2062,  2063,
    2064,     0,  2065,     0,  2066,  2067,     0,  2068,     0,  2069,
    2070,     0,     0,     0,  2071,     0,  2072,  2073,  2074,  2075,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2076,     0,     0,     0,     0,     0,     0,     0,
    2077,     0, -3133,     0,     0,  2274,     0,     0,     0,     0,
       0,  1155, -3133,  2078,  2079,     0,     0,     0,     0,     0,
       0,     0,  2080,     0,     0,     0,     0,     0,  2081,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0, -3133,     0,     0,     0,     0,     0,     0,     0,     0,
    1153,     0,     0,     0,     0,     0,     0,     0, -3133, -3133,
   -3133, -3133,     0,     0,     0, -3133,     0,     0,     0,     0,
       0,     0,     0,     0,  2082,     0,     0,     0,     0,  2083,
       0,     0,     0, -3133, -3133, -3133,  2959,     0,  2960,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0, -3133,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0, -1794, -2983,     0,     0,     0,     0,  2275,  2276,  2277,
    2278,  2279,     0,     0,  2155,  2280,     0,     0,  2084,  2085,
    2086, -3133, -2983, -3133, -3133,     0, -2983,     0,  2087,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1156,     0,     0,     0,     0,  4349,     0,
    2281,     0,  2088,     0,     0,     0,     0,     0,     0,     0,
       0,     0, -3133,     0,     0,  2089,  2090,  2091,  2092, -3133,
       0,     0,     0,     0,  2282,     0, -3133,     0,     0,     0,
       0,     0,     0,     0,     0,  2093,  2094,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2095,     0,     0,     0,
       0,  2283,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0, -3133,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2096,     0, -3133,     0,
    4350,     0,     0,     0,  4351,     0,     0,     0,     0,     0,
    2097,  2098,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0, -2983, -2983,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  1721,     0,     0,     0,
       0,  1722,  1723,  1724,  1725,     0, -3133,  2099,  1726, -3133,
   -3133,     0,  2285,     0,     0,     0,     0,  2100,     0,     0,
       0,     0,     0,     0,  2101,  4352,     0,  1727,     0,     0,
       0,     0,     0,     0,     0,  2102,     0,     0,     0,  2103,
    2104,  2105,     0,     0,     0,     0,     0,     0,     0,     0,
       0, -3133,     0,     0,     0,     0,  2106,  2107,     0,     0,
       0,  2108,  2109, -3133,  2110, -3133, -3133, -3133,  4353,     0,
       0,  2111,     0,     0,     0,     0,  1728,     0,     0,     0,
       0,     0,     0,     0,     0, -3133,     0,  2287,     0,     0,
   -3133,     0,     0,     0,     0,     0,     0,  2961, -3133,     0,
       0,     0,     0,  2288,     0,     0, -3133,     0,  2289,     0,
       0,     0,     0,  1729,  4354,  1730,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1731, -3133,     0,
       0,  2290,     0,     0,     0,     0,     0,     0,     0,     0,
   -3133,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1732,     0,     0,     0,     0,  2255,     0,     0,     0,     0,
       0,     0,     0, -1794,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0, -3133,     0,     0, -2983,     0,     0,
    2256,  2962,     0,  2257,  2258,  2259,  2260,  2261,     0,  2262,
    2263,     0,     0,     0, -2985, -3133,     0,     0,     0, -3133,
    2264,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0, -3133,
       0,     0,     0,     0, -3133,     0,     0,     0,     0,     0,
    2293, -3133,  2294,  2295,  2296,     0,     0,     0, -3133,     0,
       0,     0,  4355,     0,     0,     0,     0,  2265,     0,  2266,
    2267,  2147,  2268,  2269,  2270,  2271,  2272,  2273,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    1733,     0,     0,     0,  4356,     0,   476,     0,     0,     0,
    1734,     0,     0, -3133,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  2255,     0,  2963, -2983, -2983,  1735,
       0,     0,  4357,     0,     0,     0,     0,     0,     0,  2274,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  2256,
       0,     0,  2257,  2258,  2259,  2260,  2261,     0,  2262,  2263,
       0,     0,  4358,     0,     0,     0,     0,     0,     0,  2264,
       0,     0,  2299,  2300,  2301,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,  1736,  2302,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2249,     0,     0,     0,  4359,     0, -3133,     0,
   -3133, -3133,     0,     0,     0,     0, -1794,     0,     0,  1737,
       0,     0,  1738,     0,     0,     0,     0,     0,     0,     0,
   -3133,     0,     0,     0,     0, -3133,  2265,     0,  2266,  2267,
    2147,  2268,  2269,  2270,  2271,  2272,  2273,     0,     0,     0,
       0,  2275,  2276,  2277,  2278,  2279,     0,     0,  2155,  2280,
       0,  2964,     0,     0,     0,     0,     0,  1739,     0,     0,
       0,     0,     0, -3133,  1740,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0, -3133,     0,     0,     0,     0,
       0,     0,     0,     0,  2281,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2274,     0,
    1741,     0,     0,     0,     0,     0,     0,     0,  2282,     0,
       0,     0,     0,     0,     0,     0,  1742,     0,     0,     0,
       0,     0,     0,     0,     0,    52,     0,     0,     0,     0,
       0,     0, -1794,     0,     0,  2283,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0, -3106,     0,     0,
       0,     0,     0,     0,  2965,     0,     0,     0,     0,  2284,
       0,     0,     0,     0,  1743,     0,     0,  1744,     0,     0,
       0,     0,     0,     0, -3133,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1745,
    2275,  2276,  2277,  2278,  2279,     0,  2285,  2155,  2280,     0,
       0,  1746,     0,  1747,  1748,     0,     0,     0,     0,     0,
       0,   476,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1749,     0,     0,     0,     0,  1750,     0,
       0,     0,     0,  2281,     0,     0,  1751,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2286,     0,     0,     0,     0,  2282,     0,     0,
       0,     0,     0,     0,     0,     0, -3093,     0,     0,     0,
       0,  2287,     0,     0,    52,     0,     0,     0,  1752,     0,
       0,     0,     0,     0,  2283,     0,     0,  2288,     0,     0,
       0,     0,  2289,     0,     0,     0, -3106,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  2284,     0,
       0,     0,  1753,     0,     0,  2290,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  1754,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  4036,  2291,     0,     0,     0,  4037,
    4038,  4039,  4040,     0,     0,     0,  4041,  2257,  2258,  2259,
    2260,  2261,     0,  2262,  2263,  2285,     0,     0,     0,     0,
       0,     0,     0,     0,  4042,  4043,     0,  1755,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,  1756,
       0,     0,     0,     0,  4044,     0,  1757,  2292,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  4045,     0,     0,     0,
       0,  2286,     0,  1712,  4046,     0,     0,  1713,     0,     0,
       0,     0,     0,     0,  2293,     0,  2294,  2295,  2296,     0,
    2287,  2265,     0,  2266,  2267,  2147,  2268,  2269,  2270,  2271,
    2272,  2273,     0,     0,     0,     0,  2288,     0,     0,     0,
       0,  2289,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  4047,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  2297,     0,  2290,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,  2274,  2291,     0,  -629,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0, -3106,
       0,     0,     0,     0,     0,     0,  2298,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2299,  2300,  2301,     0,
       0,     0,     0,     0,     0,     0,  2292,     0,     0,     0,
       0,     0,  2302,     0,     0,     0,     0,     0,     0,     0,
       0,  4048,     0,     0,     0,     0,  2249,     0,     0,     0,
       0,     0,     0,     0,  2303,     0,     0,     0,     0,     0,
       0,     0,     0,  2293,     0,  2294,  2295,  2296,   781,     0,
       0,     0,     0,     0,   782,   783,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2275,  2276,  2277,  2278,  2279,
     785,     0,  2155,  2280,     0,     0,     0,     0,  4049,     0,
       0,     0,     0,     0,     0,     0,  4050,     0,  4051,     0,
       0,  2297,     0,     0,     0, -3035,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  4052,     0,     0,  2281,     0,
       0,     0,     0,     0,  4036,     0,     0,  4053,     0,  4037,
    4038,  4039,  4040,     0,     0,  -630,  4041,  2257,  2258,  2259,
    2260,  2261,  2282,  2262,  2263,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  4042,  4043,     0,     0, -3106,    52,
       0,     0,     0,     0,     0,  2298,     0,     0,     0,  2283,
       0,     0,     0,     0,  4044,  2299,  2300,  2301,     0,     0,
       0,     0,     0,     0,     0,  4054,     0,     0,     0,     0,
       0,  2302,     0,     0,     0,     0,  4045,     0,  4055,     0,
       0,     0,  4056,  1712,  4046,  2249,     0,  1713,     0,     0,
       0,     0,     0,  2303,     0,     0,     0,     0,     0,     0,
    4057,  2265,     0,  2266,  2267,  2147,  2268,  2269,  2270,  2271,
    2272,  2273,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   786,   787,   788,     0,
    2285,     0,  4047,     0,     0,     0,     0,     0,   789,     0,
       0,     0,     0,     0,     0,  4058,     0,     0,     0,     0,
       0,     0,  1740,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   504,     0,     0,     0,     0,
       0,     0,     0,  2274,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  4059,     0,  1741,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  4060,  2287,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2288,     0,   790,   791,   792,  2289,     0,  1715,  1716,
       0,     0,     0,   793,     0,   794,     0,     0,     0,     0,
       0,  4048,   505,     0,     0,     0,   795,   796,   797,  2290,
       0,     0,  4061,     0,     0,   798,     0,     0,   506,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  2275,  2276,  2277,  2278,  2279,
       0,     0,  2155,  2280,     0,     0,     0,  4062,  4049,     0,
       0,     0,     0,     0,     0,     0,  4050,     0,  4051,  1746,
       0,  1747,  1748,   781,     0, -3035,     0,     0,     0,   782,
     783,     0,     0,     0,     0,  4052,     0,     0,  2281,     0,
       0,     0,     0,     0,     0,   785,     0,  4053,     0,   800,
       0,     0,     0,     0,  4063,     0,     0,     0,     0,     0,
       0,     0,  2282,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   801,     0,     0,  2293,    52,
    2294,  2295,  2296,     0,  4064,     0,     0,     0,     0,  2283,
       0,     0,     0,     0,     0,     0,  4065,     0,     0,     0,
       0,     0,     0,     0,     0,  4054,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,  4055,     0,
       0,     0,  4056,     0,     0,     0,   507,     0,     0,     0,
    4066,     0,     0,     0,     0,     0,     0,     0,     0,     0,
    4057,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  4067,     0,     0,     0,     0,
    -950,     0,     0,     0,     0,     0,  4068,   781,     0,     0,
    2285,     0,     0,   782,   783,     0,   802,     0,     0,     0,
       0,     0,     0,     0,     0,  4058,     0,     0,     0,   785,
       0,     0,  1740,     0,     0,  4069,     0,     0,     0,     0,
    2299,  2300,  2301,     0,     0,     0,     0,     0,     0,     0,
       0,   786,   787,   788,     0,     0,  2302,     0,     0,     0,
       0,     0,     0,   789,  4070,     0,  4059,     0,  1741,     0,
    2249,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,  4060,  2287,     0,     0,   508,     0,
     504,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,  2288,     0,     0,     0,     0,  2289,     0,  1715,  1716,
       0,     0,     0,     0,   803,   804,     0,     0,   781,     0,
       0,     0,     0,     0,   782,   783,     0,     0,     0,  2290,
       0,     0,  4061,     0,     0,     0,     0,     0,     0,     0,
     785,     0,     0,     0,     0,     0,     0,     0,   790,   791,
     792,     0,     0,  1808,     0,     0,     0,     0,   793,     0,
     794,     0,     0,     0,   823,     0,   509,   505,   824,   825,
       0,   795,   796,   797,     0,     0,     0,  4062,     0,     0,
     798,     0,     0,   506,   806,     0,     0,     0,     0,  1746,
       0,  1747,  1748,     0,     0,   786,   787,   788,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   789,     0,     0,
       0,   807,     0,     0,     0,     0,     0,     0,     0,   808,
       0,     0,     0,     0,  4063,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   504,     0,     0,     0,     0,     0,
       0,     0,     0,   809,     0,     0,     0,     0,  2293,   186,
    2294,  2295,  2296,     0,  4064,     0,     0,     0,     0,     0,
       0,     0,     0,   510,   800,   781,  4065,     0,     0,     0,
       0,   782,   783,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   785,     0,     0,
     801,     0,   790,   791,   792,     0,     0,     0,     0,     0,
    4066,     0,   793,     0,   794,     0,   786,   787,   788,     0,
       0,   505,     0,     0,     0,   795,   796,   797,   789,     0,
       0,     0,     0,     0,   798,  4067,     0,   506,     0,     0,
    -953,     0,     0,     0,     0,     0,  4068,     0,     0,     0,
       0,   507,     0,     0,     0,   504,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,  4069,     0,     0,     0,     0,
    2299,  2300,  2301,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,  2302,     0,     0,   781,
       0,   802,     0,     0,  4070,   782,   783,     0,     0,     0,
    2249,     0,     0,   790,   791,   792,     0,     0,   800,     0,
       0,   785,     0,   793,     0,   794,     0,     0,     0,     0,
       0,     0,   505,     0,     0,     0,   795,   796,   797,     0,
       0,     0,     0,     0,   801,   798,     0,     0,   506,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   786,   787,   788,     0,     0,     0,     0,
       0,     0,     0,   508,     0,   789,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   507,     0,     0,     0,   803,
     804,     0,   504,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   800,
       0,     0,     0,     0,     0,   781,     0,     0,  2711,     0,
       0,   782,   783,     0,     0,   802,     0,     0,     0,   823,
       0,   509,     0,   824,   825,   801,     0,   785,     0,     0,
     790,   791,   792,     0,     0,     0,     0,     0,     0,   806,
     793,     0,   794,     0,     0,     0,     0,     0,     0,   505,
       0,     0,     0,   795,   796,   797,     0,   786,   787,   788,
       0,     0,   798,     0,     0,   506,   807,     0,     0,   789,
       0,     0,     0,     0,   808,     0,   507,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   508,     0,     0,
       0,     0,     0,     0,     0,     0,   504,     0,   809,     0,
       0,     0,     0,     0,   186,     0,     0,     0,     0,     0,
       0,     0,     0,   803,   804,     0,     0,     0,   510,     0,
       0,     0,     0,     0,  2897,     0,   802,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   800,     0,     0,     0,
       0,     0,     0,     0,   790,   791,   792,     0,     0,     0,
       0,     0,     0,   823,   793,   509,   794,   824,   825,     0,
       0,     0,   801,   505,     0,     0,     0,   795,   796,   797,
       0,     0,     0,   806,     0,     0,   798,     0,     0,   506,
       0,     0,     0,   786,   787,   788,     0,     0,   508,     0,
       0,     0,     0,     0,     0,   789,     0,     0,     0,     0,
     807,     0,     0,     0,     0,     0,     0,     0,   808,     0,
       0,     0,     0,   507,   803,   804,     0,     0,     0,     0,
       0,     0,   504,     0,     0,  2899,     0,     0,     0,     0,
       0,     0,   809,     0,     0,     0,     0,     0,   186,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   510,     0,     0,     0,     0,     0,     0,     0,
     800,     0,     0,   802,   823,     0,   509,   781,   824,   825,
       0,     0,     0,   782,   783,     0,     0,     0,     0,     0,
     790,   791,   792,     0,   806,     0,   801,     0,     0,   785,
     793,     0,   794,     0,     0,     0,     0,     0,     0,   505,
       0,     0,     0,   795,   796,   797,     0,     0,     0,     0,
       0,   807,   798,     0,     0,   506,     0,     0,     0,   808,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   508,     0,   507,   781,     0,
       0,     0,     0,   809,   782,   783,     0,     0,     0,   186,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     785,   803,   804,   510,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,   802,     0,     0,
       0,     0,     0,     0,     0,     0,   800,     0,     0,     0,
    2920,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   823,     0,   509,     0,   824,   825,     0,     0,     0,
       0,     0,   801,     0,     0,     0,     0,     0,     0,     0,
       0,   806,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   786,   787,   788,   807,   508,
       0,     0,     0,     0,     0,     0,   808,   789,     0,     0,
       0,     0,     0,   507,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   803,   804,     0,     0,     0,
     809,     0,     0,     0,   504,     0,   186,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     510,     0,     0,     0,  4464,     0,     0,     0,     0,     0,
       0,     0,     0,   802,     0,     0,   786,   787,   788,     0,
       0,     0,     0,     0,     0,   823,     0,   509,   789,   824,
     825,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,   790,   791,   792,   806,     0,     0,     0,     0,
       0,     0,   793,     0,   794,   504,     0,     0,     0,     0,
       0,   505,     0,     0,     0,   795,   796,   797,     0,     0,
       0,     0,   807,     0,   798,     0,     0,   506,     0,     0,
     808,     0,     0,     0,     0,   508,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,   809,     0,     0,     0,     0,     0,
     186,   803,   804,   790,   791,   792,     0,     0,     0,     0,
       0,     0,     0,   793,   510,   794,     0,     0,     0,     0,
       0,     0,   505,     0,     0,     0,   795,   796,   797,     0,
       0,     0,     0,     0,     0,   798,     0,     0,   506,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   800,     0,
       0,     0,     0,   509,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   806,     0,     0,   801,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,  4465,     0,     0,     0,     0,     0,   807,     0,
       0,     0,     0,     0,     0,     0,   808,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,   800,
       0,     0,     0,     0,     0,   507,     0,     0,     0,     0,
     809,     0,     0,     0,     0,     0,   186,     0,     0,     0,
       0,     0,     0,     0,     0,   801,     0,     0,     0,     0,
     510,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,   802,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   507,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   802,   508,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   803,   804,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,   508,     0,
       0,     0,     0,     0,     0,   509,     0,   824,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   806,   803,   804,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
     807,     0,     0,     0,     0,     0,     0,     0,   808,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,   509,     0,     0,     0,
       0,     0,   809,     0,     0,     0,     0,     0,   186,     0,
       0,     0,     0,     0,   806,     0,     0,     0,     0,     0,
       0,     0,   510,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,   807,     0,     0,     0,     0,     0,     0,     0,   808,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   809,     0,     0,     0,     0,     0,   186,
       0,     0,     0,     0,     0,     0,     0,     0,     0,     0,
       0,     0,     0,   510
};

static const yytype_int16 yycheck[] =
{
     151,   646,   503,   180,   636,  1169,   705,   706,   707,   693,
      36,   710,   982,   674,    66,   815,   669,  1164,  1499,  1032,
      72,   174,  1631,  1714,   780,  1113,   641,  2137,  1719,  1283,
    2249,   212,  1023,   833,  2483,  2436,  1008,   238,  1616,   872,
     137,   627,   139,    69,    70,    71,  1687,   144,   719,  2498,
      76,   631,   723,   724,  1189,   726,   727,   652,   729,   730,
    1888,  1626,   776,   866,  2336,   644,  1743,  2449,   636,  1185,
    1898,   658,  1644,   652,   805,   720,   721,  2459,  2737,   674,
     725,  1197,  2775,   728,  2177,  3164,   626,   184,   675,  2355,
     901,   631,  1887,  2208,   120,   674,  2362,  1928,   281,  2365,
    2366,  1523,   858,  3422,   644,  2134,     1,   626,   134,     1,
     136,    16,   652,  1908,   140,   141,  2807,     1,  2337,  2338,
     866,  2663,   690,     0,   150,   151,  3009,    16,    16,    25,
    1859,  2883,    31,  1092,   674,  1094,  1095,     1,  1097,  1098,
     166,    16,    16,   109,    77,  2830,  1281,    16,    16,    16,
      31,    83,  2837,    83,   206,    14,   182,   107,   255,   110,
      42,   156,    11,   215,   997,   161,    56,    83,   204,   110,
     137,    25,    16,   137,   200,   139,   793,   794,  1169,   208,
     144,  2400,    43,   157,   801,     0,    42,   291,   214,    41,
       1,  2441,  2442,   219,  1923,  1764,    55,  2447,  2448,  1773,
    2450,   227,   249,   226,    38,  1268,    43,  1781,   119,  2572,
     290,   109,   626,    85,    16,   113,    16,   631,   805,   190,
     184,   967,   226,    16,   122,  1028,   843,   157,   239,   227,
     644,   240,  1035,   164,  1526,   261,   110,    62,   652,   291,
     292,   299,   232,  3728,   249,   291,   169,   237,   170,   171,
     337,   190,   421,   839,   337,   110,   120,   206,   379,   409,
     674,  3742,   388,   764,   122,   949,   195,  2716,  2717,  2967,
     255,   308,  2207,   953,   101,   170,  1554,   317,    57,  2977,
     409,   280,  1028,   823,   824,   825,   255,   459,   139,  2734,
     270,    96,   354,   475,   420,   285,  1663,   161,   162,    13,
      31,   390,     1,   481,  2554,   824,   240,  2557,   198,   354,
    1041,   120,    37,   232,  2564,  2621,   409,  2567,   237,  3128,
     393,   861,   409,   488,   317,   305,   306,   895,   515,   304,
     390,    30,   483,   282,   532,  2656,   596,   560,   464,   317,
     398,   231,  2610,   277,   351,   279,    30,   384,  3949,   560,
     351,   472,   161,   162,  1261,   464,   935,   354,   416,   545,
    1044,   110,   280,   137,   350,  2631,   285,   448,   947,     1,
    1170,   205,   353,   250,    83,   448,  1511,   291,   239,   256,
     240,   706,   417,   384,  1291,   386,   651,    19,   546,   515,
     187,   198,    16,   227,   650,   935,   560,   449,   648,   494,
    2129,   815,   239,   336,    11,   456,   434,   947,  3574,   823,
     824,   825,   739,   390,   409,  3684,  3737,   110,   354,   833,
     456,    38,   390,  1018,   203,  3594,    16,   824,  1228,   630,
     337,  2460,   633,   511,  1237,   250,   462,   463,  1152,  1018,
     346,   256,  3835,   409,  1158,   319,  1025,   861,    11,   317,
     301,    11,   228,   739,  1041,  3724,   482,  1001,    39,  4447,
      84,   409,  3594,   489,  3503,   517,   667,   137,   456,   670,
    1605,     0,   655,   421,   351,   676,   532,   352,  1018,    14,
     112,  2882,  1643,  2733,   283,  1025,   409,   220,  1481,   456,
     456,   355,   291,  4481,  1034,   696,  2225,   543,    16,   102,
     109,  3253,   137,  3896,   113,    16,   812,   384,   564,   386,
     409,  1504,  3833,   571,   240,  1034,   913,   515,  1679,   137,
      55,   935,  1066,   291,   137,   603,   832,   515,   409,  3214,
     627,  3222,   859,   947,   631,   379,   351,  3806,   170,   350,
     491,   173,    83,   175,  3229,   409,   641,   644,   645,  2664,
     456,  2817,   647,   137,  1268,   652,   405,   431,   137,  2669,
     419,   658,  3731,   137,   196,   170,   488,   618,   641,   384,
     175,   386,   669,   859,   671,   672,   826,   674,   675,   379,
      23,   690,   655,   481,  3750,    85,   379,   745,   205,   557,
     567,   916,   756,   500,  1008,    23,   456,  3406,   749,  3731,
     409,   857,   699,   584,  1018,   298,    41,   434,  1696,  1189,
     617,  1025,   456,    23,   532,   787,   617,  3656,    23,   226,
    1034,   346,   409,   704,   542,   890,   451,   515,   472,   543,
     616,   752,  1172,  1173,  1174,   456,   616,   734,  1610,  1179,
    1180,  1181,   421,  2221,    11,   831,  2013,   626,   229,  1189,
     815,  3096,   912,  4254,   855,  1195,   456,   289,  1198,   687,
    1179,  1180,  1181,   456,  1204,  1205,  1206,  1207,  1208,   870,
     398,  1286,   472,   766,   388,   389,  1195,   651,   409,   472,
     456,   678,   717,  1483,   289,   534,   608,  2364,   123,  1257,
     913,   820,   127,   464,  1234,  1811,   405,  3003,  1833,  1239,
    3006,  1281,   913,   396,   456,   577,   420,  1540,   805,   456,
     882,   560,  2962,   515,   578,  1234,   560,   280,   616,   651,
    1239,   651,  3043,   520,  1683,   912,  1526,   561,  1314,   927,
     912,   662,   821,   708,   912,   651,  4337,   820,   409,  3007,
     617,  1281,   839,   912,   515,   805,   678,   233,   228,   637,
     464,  4236,   456,  3444,  3506,   379,   316,  2588,  1172,  1173,
    1174,  2696,  1518,   913,   861,  1179,  1180,  1181,  1182,   578,
     832,  1185,   379,   820,  2415,  1189,  4257,  1580,   904,  2808,
    2509,  1195,  2354,  1197,  1198,   831,   630,   832,   593,   379,
    1204,  1205,  1206,  1207,  1208,   494,  1476,   648,   379,   982,
     109,   515,   617,  3345,   113,   616,   927,  3492,  1780,   913,
     494,   820,  3510,   437,   438,   820,   379,   766,   716,   379,
    1234,  1884,   821,  1494,    41,  1239,   913,   459,   908,   727,
     630,   434,   456,   913,   636,   820,   785,   630,   935,   810,
     447,  2133,   449,   866,   823,   824,   825,   752,   472,   456,
     947,   820,   109,   802,  2132,   913,   113,  1271,   769,   906,
     687,   379,   866,   794,    11,   472,   456,  1281,   379,   727,
     793,   810,   481,   913,   815,   456,   913,   913,   459,   437,
     816,   815,   472,   912,   447,   982,   449,   447,   820,   449,
    1881,   472,   744,   456,  2463,  2469,   456,   769,   647,   894,
    3263,   912,   835,   821,   913,  1002,   123,  3659,   752,   472,
     127,   822,   472,  1508,   912,   911,   890,   913,   456,   551,
     913,  1018,   503,   913,  1680,  1681,  3611,  3612,  1025,  1508,
    1107,  1511,   757,   913,   884,   913,    23,  1088,   456,  1090,
     821,  3824,  1093,   820,  1041,   456,   913,   913,   722,   728,
     832,  1102,   752,   807,   472,  1495,   820,  2235,   890,   752,
     890,   472,   676,   914,  3649,   913,  1143,    48,  1508,   843,
     669,  1511,   579,   828,   890,   826,   832,   867,   833,   912,
     868,   830,  4151,   912,   913,   669,   830,  3016,   488,  1086,
    1087,   913,  1571,   912,   913,  1666,  1667,   804,  3304,  1096,
    1800,  1672,   701,   982,  1675,   820,   630,   616,   815,   820,
     906,   820,   379,   448,   913,   913,   579,   701,   913,   579,
     515,   913,   927,   630,   913,  1605,  3568,  3779,  1831,   913,
     830,  1571,   920,    11,   821,   761,   913,   830,   913,  1240,
     630,  1797,  1798,   817,   913,   913,   913,   906,   912,   630,
    1227,   912,   722,  1593,   489,  1034,  1153,   500,  1084,  1720,
    3751,  3720,   923,   831,  1971,  1605,  3095,   630,  3097,  1483,
     630,  1827,   500,   705,  1593,  1692,  4395,   577,   912,   626,
     447,  1495,   449,   927,   687,  1831,   923,   722,   913,   456,
     500,   884,  1189,  1824,  1508,   500,  4175,  1511,   913,  2521,
     705,   894,   233,   912,   722,   472,   927,   716,   655,   722,
     651,    18,   630,  2941,  2942,  2943,  2944,  1143,   448,   630,
     913,   820,  1086,  1087,  4203,   456,   819,   927,   752,   198,
    1156,  3160,  1096,   456,   927,   828,   829,   678,   722,   233,
     833,   912,   303,   722,   815,   752,   456,   817,   722,   431,
     697,  1962,  2983,   785,   634,  3840,   884,  1571,  3817,   320,
     431,   913,   752,   598,   599,  2135,   913,  2896,    44,   110,
    1884,   752,   481,    80,  2739,   200,   757,   758,   592,  1593,
     927,  2976,   817,   456,  1281,   169,   744,   682,   820,   752,
     904,  1605,   752,  1172,  1173,  1174,  1610,   442,   912,   817,
    1179,  1180,  1181,   835,   817,  2005,   830,  2785,   815,   913,
     828,   820,   579,     1,  2186,   833,  1195,  1314,  3490,  1198,
     240,   456,    23,   830,   481,  1204,  1205,  1206,  1207,  1208,
    3345,   448,   379,   817,   752,   105,   106,  1824,   817,   590,
     830,   752,   913,   817,   730,   456,   420,    47,   728,  2978,
     337,  2980,  3345,  1833,   456,  1234,   448,   743,   821,   866,
    1239,  2833,  1848,   630,   764,    56,  1417,   830,   815,   769,
     830,   456,  2179,   240,   353,    38,   823,   824,   825,   820,
     571,   913,  3975,   456,    75,   354,   833,   618,   456,  1829,
     464,   161,   282,  1833,   884,  1966,   321,  3026,  1943,   486,
     447,    16,   449,   927,   894,   212,   913,   616,    44,   456,
     565,   442,   830,    28,   383,  3949,   713,   420,  3977,   830,
     927,   456,   822,   913,   194,   472,   337,  1972,    41,   485,
     123,    42,  2420,  2133,   127,    16,  1981,   927,   208,    20,
     456,   515,    23,    24,   240,  4424,   927,   456,   442,   890,
     913,   869,   912,   913,   912,   690,   690,   354,  3087,   616,
    3089,   464,  3964,  1546,   927,   712,  1780,   927,    20,    84,
      38,    23,    24,  3645,   704,   913,  3648,   437,   337,   408,
    2496,   598,   599,  1923,   595,   752,  3946,   820,  3126,   927,
     739,  2191,  1573,   227,   456,   913,   750,  1811,   744,   546,
     425,   379,   913,  3983,  2982,    11,   390,   716,   595,   927,
     509,  1508,   515,   500,  1511,  1829,   927,   912,   384,  1833,
     386,   264,   739,   820,   505,   506,   507,   865,    42,    80,
     337,   596,   579,   820,   869,   982,   301,   302,  2573,  4157,
    2575,     0,   205,  2402,  2403,  3183,   157,  3275,   815,  2408,
    2409,    14,   514,   274,   204,  2023,   894,   482,   291,   716,
    4020,  1008,   198,   830,  4311,   485,   817,   821,  1669,   447,
    1671,   449,  1673,  1674,  1571,  1676,  1677,   912,   456,  3745,
     739,   828,   195,   630,  2284,   730,   833,  1034,   379,  4091,
     355,  2291,    55,  2224,   472,   295,   398,  1911,   912,  3771,
    3772,   485,  3179,   456,  2639,   584,   291,   817,  1605,  1923,
     859,   820,   704,   859,   416,   544,  1495,   230,   485,   618,
     385,  4239,  4102,  2356,   235,  2281,   433,   744,  2189,  3911,
    2543,  2438,  2127,   595,  3949,   826,   337,   205,   379,   700,
     621,   531,   859,   157,   430,  3110,   913,  3112,  2127,  2128,
     566,  4398,  3949,  2139,   495,  3949,   497,   464,  1584,   715,
     927,   843,   578,   820,   352,   456,    36,  1664,   475,    20,
      21,    22,   843,   557,   571,   430,   906,   912,   912,   292,
     456,   472,   913,   298,  2719,  3695,   377,  2127,  2128,  2129,
     913,  2005,  2006,   220,  3235,  2502,  2503,   824,   745,   730,
     859,   579,   561,   913,   913,   752,  1632,  1659,   515,   678,
    1636,  1637,   743,  1639,  1593,   456,  1642,  1643,   406,  1645,
    4254,   235,   833,  1720,   337,  1172,  1173,  1174,  2535,  2536,
     393,   472,  1179,  1180,  1181,  1182,   730,  2224,  1185,  2611,
     913,   828,   355,   539,   435,   488,   833,   658,  1195,   743,
    1197,  1198,   630,  2298,   927,   448,   379,  1204,  1205,  1206,
    1207,  1208,   350,   114,   379,   618,   126,   390,   274,   571,
     138,   437,  1219,   133,   474,  2929,  2216,  2217,   913,   470,
    2587,   515,  2641,   830,   744,  2225,   913,  1234,  1865,  3517,
    2597,   852,  1239,   911,  1845,   913,  1722,  2216,  2217,   167,
    1664,  2501,  1728,  4337,  2507,  1731,   456,  1733,   578,  3287,
     543,   913,  2357,  2127,  2128,  2129,   184,   185,   588,   589,
     456,   746,   437,   438,   390,   927,   887,  1824,   913,   299,
     904,   892,  1829,   456,   319,  2406,  1833,   456,  2645,   630,
     913,   456,  2733,   409,   813,   913,   766,   912,   913,   472,
     240,  1848,  2659,  2660,  2726,   836,   837,   472,   543,   927,
     355,   668,  2476,  2851,  2852,   785,   913,  4168,   849,   488,
     281,   833,  2186,   379,   752,   682,  4177,   797,   913,  3508,
     927,   850,   802,   574,   560,   551,    16,     7,  2449,   630,
     824,  3520,  3521,   663,  2455,   641,  2457,   913,  2459,   766,
     823,   904,  2216,  2217,   913,   598,   599,  4026,  4027,   912,
    2610,  2225,   278,   464,   409,   685,   686,   842,   785,   500,
     501,   427,  4094,  4095,   475,   384,  1923,   386,   398,   280,
     797,   154,   300,    53,  1860,   802,   421,   252,   398,  4254,
     820,   447,   427,   449,  3573,    65,   416,  2457,   500,   501,
     456,   566,   830,  2455,   301,  2457,   416,  4254,   744,   578,
    4254,   752,  4242,  4243,   279,   927,   472,  2018,   379,  1895,
     551,   552,  4401,   379,   464,   464,     0,   503,  1904,   913,
     110,  2840,   488,   906,   820,   379,   505,   506,   753,   744,
     681,   111,   230,   913,   582,   797,   464,   913,   586,   551,
     552,   747,   748,   616,  2565,  2566,   546,   630,   355,  2541,
    4160,   752,   363,   364,   134,   630,   530,  4167,   408,  4448,
    4449,  4171,  4337,     1,  4174,   515,   515,   813,   616,  4148,
    4149,    14,   363,   364,  2947,   913,   828,   551,  2517,   830,
    4337,   833,   408,  4337,  2484,   456,  2486,   515,  1495,   927,
     456,   110,  2135,   228,   292,  2565,  2566,  2864,   813,     1,
     194,   472,   456,  2565,  2566,  2484,   472,  2486,   744,  2509,
    2510,   191,    55,   579,   208,  2775,  4425,  2517,   472,   828,
      58,    59,    60,    61,   833,    63,   851,    14,   448,   440,
     441,  2510,   452,  2573,  2534,  2575,   155,   465,  4378,  1546,
     256,   571,   621,   283,   665,   912,   913,   668,   476,   440,
     441,   291,   673,   557,  4394,  2534,  2197,  2198,  2199,  2200,
    2201,  2202,  2203,  2204,   630,   713,   301,   336,    55,   752,
    2127,  2128,  2129,  2573,    76,  2575,   927,   752,  2189,   336,
     126,   336,  2139,    16,   628,  4295,  1593,   133,  4298,   539,
    4396,  4397,   272,  2668,   544,    28,   640,   508,  2637,  2639,
     650,   370,   250,  1610,   855,   712,   144,   145,   282,   613,
    2484,   819,  2486,   370,   540,   370,  2227,   291,   544,   121,
    4426,   829,  2496,   859,    76,   409,   927,   641,   106,   319,
     817,   542,  2189,   647,   198,  2509,  2510,  2637,  4444,  2639,
     551,   828,   817,  2517,   819,   745,   833,   830,   652,   560,
     641,    84,   456,   828,   829,   830,   647,   832,   833,   630,
    2534,   409,   820,   343,   630,   641,   250,  2224,  2225,   121,
     756,   647,   256,   759,   760,  2286,   630,   605,   187,  2719,
     823,  3048,  3049,   161,   558,   187,   752,   561,   629,   379,
     601,   834,  4498,   817,  3061,  3062,  2135,  2254,  3128,  2573,
     456,  2575,  3740,   295,   828,   812,  2706,   299,   456,   833,
     818,  3078,  3079,  4392,   713,   643,   194,  2891,   823,  2719,
     828,   828,   399,   319,  2335,   833,   833,  2706,   828,   834,
     208,  2342,   660,   833,   913,   187,   691,  2611,  2954,   805,
     336,   431,   812,   423,   927,  2619,   816,  3007,   927,  2410,
     448,  2412,   927,  2414,   452,   511,  2367,   271,   514,  4047,
     220,   882,  2883,  2637,   830,  2639,   456,   351,  4056,   337,
     849,   339,   824,  1780,   370,   455,  4064,  2216,  2217,  4458,
     820,   752,   472,   307,  2341,  2877,   752,   325,   326,   327,
     328,   329,   330,   331,   332,   333,   838,   839,   752,   301,
     384,   541,   386,   543,  1811,   818,   398,   857,   448,   750,
     558,   169,   452,   561,   817,   828,  2302,  2478,  2479,  2305,
     833,   541,   578,   543,   416,   828,   630,    34,    35,   713,
     833,   427,  2706,  4403,   775,   776,   777,  2349,  4407,   595,
    2485,   766,  2487,  4412,  2455,  2719,  2457,   913,  2493,  2494,
    2495,   738,  2726,   355,   450,  2500,  2422,   738,  2344,   830,
    2426,   927,   912,  2737,   830,   298,  2511,  2512,   820,  4493,
    2513,  4495,   820,  4497,   554,   820,   830,  2363,   820,   823,
    3949,   825,   240,   390,  2441,  2442,  2372,  2373,    32,    33,
    2447,  2448,  2449,  2450,  4045,   699,  3001,  3316,  2455,    37,
    2457,   776,  2459,   169,  4055,   444,   445,   409,  3039,  2984,
     821,  1206,  1207,  1208,   825,   489,   827,   820,   456,   126,
     404,   423,   713,  1172,  1173,  1174,   133,   549,   550,   136,
    3297,  2544,  2418,  2419,   825,  2492,   827,    24,    25,   913,
     630,   820,   913,   820,   446,  1459,   379,   627,   752,  1198,
     135,   631,  2509,   913,  2565,  2566,   927,   409,   456,   913,
    2517,   927,   913,  4282,  1478,  1479,  1480,  2610,  2454,  3399,
     913,   423,   820,   927,   240,  2461,  3406,   820,  2978,   571,
    2980,   820,   857,   190,   692,   192,   193,  1204,  1205,   820,
     918,   919,    19,   220,   446,  1180,  1181,  2554,   456,  2006,
    2557,   805,  2464,  2465,   437,   438,   820,  2564,  2565,  2566,
    2567,  4330,  4331,   456,  3381,  3382,  2573,  4336,  2575,  1803,
    4339,   913,  1806,   456,  4343,  4344,  3026,   707,   612,   915,
    1814,   843,   820,   617,  1818,   744,  3509,   725,   113,   472,
     122,  1825,   913,   456,   409,  2484,   820,  2486,   870,   871,
     872,   873,   870,   871,   872,   873,   337,   724,   596,   610,
     713,   140,   752,   870,   871,   872,   873,   274,  2492,   276,
     820,  2510,   947,   564,   807,   561,   283,   833,   511,  2722,
    2637,   619,  2639,  3175,   291,   112,   592,   767,   456,  3089,
    1274,  1275,  1276,  1277,   727,  2534,  3312,  2259,  2260,  2261,
    2262,  2744,   406,   820,  2978,   820,  2980,   982,   616,   913,
     195,   816,   912,   139,    14,   356,   108,   913,   815,   295,
     913,   913,   832,   927,   820,  3544,   713,   485,  2135,   463,
     442,   820,  3253,   566,   557,   290,    32,   824,   820,   641,
     830,   913,   456,   170,   646,   489,   173,   241,   175,  2792,
     485,   832,  3026,   843,   346,   298,   832,  1032,   242,   832,
     832,   663,  2719,   665,   832,   815,   832,   847,   361,   196,
     832,   832,   832,   832,   832,   832,  2733,  2838,  2839,  2186,
    2841,  2842,  2843,   832,   243,  4254,   244,  3403,   691,   641,
     246,   398,   641,   162,   646,   797,   797,   630,   362,  4350,
      78,   247,   815,   248,   456,   912,   249,   497,   183,  2216,
    2217,   663,   832,   665,   804,  3089,   270,   253,   815,   485,
    2831,   254,    83,   913,   805,   905,   257,  3775,  3776,   913,
     258,   346,  4383,   346,   259,   913,   260,   927,   261,   894,
     291,   448,  4183,   488,  3284,   452,  3286,   611,    42,   262,
     832,   263,   405,   264,   346,   265,   202,   266,   820,  2745,
     713,   659,   289,   454,  3524,   820,   820,  2706,  4337,   155,
     796,   629,  2883,  2830,   337,   913,   678,  1152,   379,  3392,
    2837,   488,   810,  1158,  3284,  2771,  3286,   169,   815,   913,
     913,   396,   913,   815,   354,   458,   614,   456,   676,   489,
     911,   543,   291,   869,   911,  2744,   906,   456,   236,   316,
     662,   252,  3679,  3680,   346,   405,   815,   733,   820,   752,
     368,   801,   807,   812,  2810,   405,  2883,  2813,   761,  2815,
    2816,   405,   240,   912,  2820,    83,  2822,  2823,  2824,  2825,
     815,   815,  3949,   815,   202,   346,  2832,  1222,  2991,   346,
      83,  3899,   405,  2792,   267,   456,   346,   820,   459,   713,
     663,   890,    31,   629,  3007,   156,   820,   647,   820,  2855,
    3031,   472,   820,   557,    76,   913,   454,  1252,   549,    14,
     421,   744,   137,   925,   817,  3506,   819,   421,   890,   926,
     766,   766,   750,  1268,   496,   828,   829,   830,   549,   832,
     833,  3831,   503,   817,  2890,  2962,  3954,   817,   766,   817,
    3284,   913,  3286,   817,  3781,  3782,  3783,   775,   776,   777,
     817,  2978,   817,  2980,   817,   354,   817,   785,   815,   678,
    3690,   624,   459,   626,   815,    37,   629,   683,   890,   658,
     421,   634,   354,   636,   802,   354,   884,   569,   456,   251,
     927,   662,    83,   815,   227,   354,   390,   766,   558,   820,
     906,   654,   655,   354,   906,   815,   659,    16,   557,  3026,
      13,   239,   678,   666,   820,  3606,  3607,  2484,   713,  2486,
     704,   890,   355,  3126,   750,  3128,   906,   687,   681,  2496,
      37,   459,   409,   355,   927,   913,   728,   704,   894,   109,
     766,   766,   695,  2510,   697,   698,   464,    41,   913,   775,
     776,   777,   727,    57,   456,   817,   817,   546,   354,   785,
     745,   515,   544,   408,   551,   240,    25,  2534,   555,   630,
     807,   281,  3089,   236,   890,   890,   802,    83,  3659,   337,
    3183,   337,   169,    77,   727,    79,    14,   346,   539,   240,
     662,   820,  3163,   203,   596,   913,   794,   500,   889,    95,
     566,   820,   832,   713,   807,  3932,  3933,   927,   102,  3728,
    3231,  3787,  3129,  3184,    90,   854,   815,   913,    37,   773,
    3191,   788,  3726,   725,   913,   815,   337,   452,   728,   123,
    3227,   784,   799,   127,   557,   599,  3108,  3083,    25,  3236,
    3237,   197,  3818,   810,  2611,   744,   346,   564,   817,   354,
     354,   187,  2619,   213,   826,    14,    55,   657,   657,  1494,
     496,  3881,  3233,   208,   569,   562,   255,  3238,   567,  3186,
     823,   824,   825,   464,   421,   187,   520,   464,   448,   666,
     545,   877,  3253,   831,   615,  3202,  3203,  4254,  3205,   807,
    3293,   752,   820,   457,  3211,   456,   849,  3214,  3779,  3216,
    3217,  3921,   913,   346,   812,   110,   456,  3224,  3884,  3226,
      16,   744,  3229,   807,   881,   820,   913,  4096,   705,   392,
     206,   813,    83,   876,   198,  3816,  1561,  3126,   784,  3128,
     612,   515,   515,    83,   464,   464,  3253,   464,   540,  2706,
     820,   278,   612,   817,   815,   913,   913,   204,   204,   766,
     346,  1586,    13,  4337,   807,  3975,   500,    23,   612,  2726,
    3206,   570,   683,   498,   751,   823,   753,  3284,   865,  3286,
    2737,   913,   635,   729,   396,   336,   543,   585,   481,  3225,
     291,  3227,   616,   815,  3183,   195,   815,   496,   393,   820,
    1625,    41,   853,  3239,   690,   475,  3399,   744,   785,   952,
     543,  3318,  3186,  3406,   110,   820,   664,   850,   756,   962,
     963,  3902,   824,   820,   204,   561,   197,   499,   227,   784,
     824,  3205,   500,   783,   885,   886,    23,  3211,   291,   982,
    3911,   612,  3216,  3217,   206,   421,   543,   421,   437,   820,
    3224,   820,  3226,   906,   557,   421,   152,   421,   835,   437,
    3421,   169,   158,   159,   556,  1008,  3690,   421,   421,   437,
     797,   120,   355,   231,   109,   355,   927,   566,   174,   442,
    3473,   370,   394,   346,   368,  3392,   820,   510,    83,  3970,
     578,  1034,   578,   770,   578,   566,  3720,  1040,     7,    11,
      48,    45,   113,   524,    83,   270,   196,   276,  3344,    90,
     294,  1123,   743,   202,   198,   473,  3842,  2013,  3643,  4091,
    1063,  2821,  3622,  3762,  1067,  1068,  2829,   455,  2843,  3620,
    3203,  4009,  3543,  3943,  4020,  3442,   913,  2133,   708,  3500,
    2305,  1632,  4337,  4462,    53,  3506,  2781,  3839,  3455,  3744,
     434,  2014,  3896,  2792,  4156,  4282,    65,  4406,  4411,  3731,
    4182,  3892,  4087,   659,   448,  1476,  3473,  4243,   452,   969,
    4055,  4048,  4048,  4083,  1702,  3289,  2856,   231,  3485,  1152,
    1158,  2882,  2891,  1164,  1739,  3492,  2515,  3574,  1839,  3786,
    3287,  3427,   839,  3817,  3785,   849,  3259,  1222,   638,  3506,
    1884,   895,   111,   637,  1253,  3293,  2947,   907,  1911,  2542,
    3399,   659,  1286,  1289,  2588,   649,  2983,  3406,  3293,   931,
    1964,  2996,  1165,  1166,  1317,   134,   660,   661,  1853,  1172,
    1173,  1174,  2994,  1487,  2619,  2003,  1179,  1180,  1181,  1182,
    3012,  3676,  1185,  3604,   340,   341,   342,  3011,  3609,  3538,
    1492,  3487,  1195,   977,  1197,  1198,   352,  3333,  2135,  1884,
    3621,  1204,  1205,  1206,  1207,  1208,  3037,  3713,  3442,  2170,
    2656,  3052,  3051,  2731,  3685,  1559,  1219,  3043,  1221,  2671,
    1016,  3455,   191,   379,   675,  2205,  1018,  2709,  1598,  2232,
    3093,  1234,  2232,  3921,  3711,  2184,  1239,  2169,  3659,  3585,
    3926,  3673,  3980,  3311,  3611,  3612,  3542,  4107,  3985,  1624,
    2437,  3485,  2435,   597,   598,   599,  3623,  1264,  1263,  3988,
    3936,  3935,  3629,  3795,  3683,  3682,  2506,  4236,  1271,  3098,
    2505,  3386,  3082,  3567,  3081,   431,  3385,  2540,  3725,  3099,
    2539,   674,  3649,   439,   846,  1294,  1937,  2702,  1303,  2702,
    2702,  2702,  3659,  3977,   699,  3591,  1964,  1002,  1216,  2799,
     456,  3186,   204,   272,   460,   461,   462,  3524,  3561,  2191,
    2005,  3007,  3561,   469,  2722,  4050,   472,  3684,   812,  2417,
     515,  1219,   747,  4201,   808,  2789,  1633,  2531,  3759,  2207,
    4356,  3088,  3371,  4351,  3707,  2963,  4238,  3624,  4232,  3635,
    2332,  4383,  4439,   687,  3797,   778,  3507,   288,   528,  4251,
    3646,  3647,  3028,  3193,  2212,  3651,  3652,  3724,  3779,  1903,
    3961,  4232,  1641,  2411,  3223,  4398,    -1,  1162,    -1,    -1,
      -1,  3738,  3739,    -1,   343,    -1,    -1,    -1,  3831,  3675,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   732,    -1,
    3686,    -1,    -1,    -1,  3761,  3762,    -1,   553,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  3876,  3853,  3854,    -1,    -1,
      -1,    -1,  3779,  4184,   908,    -1,    -1,    -1,    -1,    -1,
    3716,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     924,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  3734,  3806,
      -1,    -1,    -1,  4358,    -1,    -1,  2131,    -1,  3919,    -1,
    2135,    -1,    -1,    -1,   423,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   813,
      -1,    -1,    -1,  3840,   630,    -1,    -1,   971,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   455,    -1,    -1,    -1,
      -1,   835,  1495,  2178,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  3738,  3739,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    91,    92,    93,
      -1,    -1,    -1,  4464,   680,    -1,  3979,  3761,  3762,  3950,
      -1,    -1,    -1,    -1,  2219,    -1,    -1,    -1,  1032,    -1,
    3907,    -1,  3838,  1546,  3911,  1039,    -1,  2232,  3844,  1043,
      -1,    -1,    -1,    -1,    -1,  1558,    -1,  1560,    -1,    -1,
      -1,  3857,    -1,    -1,    -1,    -1,    -1,    -1,   912,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  3831,    -1,    -1,   554,  4396,  4397,    -1,  3956,
    1593,    -1,    -1,    -1,  1597,    -1,   752,  4050,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  1610,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  4426,    -1,    -1,    -1,
      -1,    -1,   778,   779,    -1,    -1,    -1,    -1,  3924,    -1,
      -1,    -1,  3928,    -1,  4444,    -1,   210,   211,  4059,   213,
      -1,  3937,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  3948,  3949,    -1,    -1,    -1,    -1,   627,    -1,
      -1,    -1,   631,    -1,    -1,    49,    50,    51,    52,  3965,
    3966,    -1,    56,  3907,   830,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2368,    -1,    -1,  3982,   843,  4498,    -1,
      74,    75,   848,    -1,    -1,  1189,    -1,    -1,  4119,  3995,
      -1,    -1,  3998,  3999,  4000,  4001,    -1,    -1,    -1,    -1,
      -1,    -1,  4396,  4397,    -1,    -1,    -1,    -1,    -1,   875,
      -1,    -1,  3956,    -1,    -1,  4196,    -1,   883,    -1,  4150,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   707,    -1,
     124,  4037,  4426,    -1,  3561,    -1,    -1,  4168,    -1,    -1,
    4046,   907,    -1,  4049,    -1,    -1,  4177,   913,    -1,    -1,
    4444,  1764,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   927,    -1,  1267,    -1,    -1,    -1,  1780,    -1,   163,
      -1,    -1,    -1,    -1,    -1,    -1,  1280,  1281,    -1,    -1,
      -1,    -1,    -1,  4160,    -1,    -1,    -1,    -1,   767,  1802,
    4167,    -1,    -1,    -1,  4171,    -1,    -1,  4174,  1811,  1303,
      -1,    -1,    -1,    -1,  4498,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   398,   399,    -1,    -1,    -1,  4125,
    4126,  4127,  4128,  4129,    -1,    -1,  4132,  4133,  4134,  4135,
    4136,  4137,  4138,  4139,  4140,  4141,  4142,    -1,   422,  4145,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  4153,    -1,    -1,
      -1,  4282,    -1,    -1,    -1,    -1,    -1,    -1,  4164,    -1,
    4166,    -1,    -1,  3690,   448,    -1,    -1,    -1,   847,    -1,
    4176,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   281,    -1,   473,
      -1,    -1,    -1,  3720,    -1,    -1,    -1,    -1,  1911,  4330,
    4331,    -1,  1915,    -1,    -1,  4336,    -1,    -1,  4339,    -1,
      -1,    -1,  4343,  4344,    -1,    -1,    -1,    -1,  4295,    -1,
      -1,  4298,  4353,    -1,    -1,    -1,   905,    11,    -1,    -1,
    2625,    -1,    16,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   338,    -1,    -1,  4378,  4254,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,  2656,    -1,  4394,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  1985,    -1,    -1,  1988,  4407,  1990,  1991,  1992,
    4411,  4412,    -1,   377,    -1,    -1,    -1,   571,    -1,    -1,
    3817,  4422,    -1,  2006,    -1,    -1,    -1,  2010,    -1,  2012,
      -1,    -1,    -1,  4309,  4310,  2018,  4312,  1511,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     604,    -1,    -1,  4329,   608,   609,    -1,    -1,  4334,  4335,
      -1,    49,    50,    51,    52,    -1,  2731,    -1,    56,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  4352,    -1,    -1,  4355,
     634,    -1,  4358,    -1,    -1,    -1,    74,    75,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,
      -1,  4502,  1576,    -1,   158,   159,   470,     1,    -1,    -1,
      -1,    -1,    -1,  1587,    -1,    -1,    -1,    11,    -1,    -1,
     174,    -1,    16,    -1,  1598,    -1,    20,    21,    22,    -1,
      -1,  1605,    -1,    -1,    28,    29,   124,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   515,  2135,    -1,    -1,  2138,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  4396,  4397,    -1,
      -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,    -1,    -1,
    3977,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  2171,    -1,
      -1,    -1,    -1,  2176,    -1,    -1,    -1,  4426,    -1,    -1,
      -1,    -1,  2185,  2186,    -1,    -1,    -1,    -1,    -1,    -1,
     574,    -1,    -1,    -1,    -1,  4444,   270,    -1,    -1,    -1,
     114,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2216,  2217,    -1,    -1,    -1,    -1,    -1,
    2223,    -1,   796,    -1,    -1,    -1,    -1,    -1,  2231,  2232,
      -1,    -1,    -1,  4050,    -1,    -1,  2239,    -1,   152,    -1,
      -1,    -1,    -1,    -1,   158,   159,    -1,    -1,    -1,  4498,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     174,    -1,    -1,    -1,    -1,    -1,   340,   341,   342,    -1,
      -1,    -1,     1,   281,    -1,    -1,    -1,    -1,   352,    -1,
      -1,    -1,    11,    -1,    -1,    -1,    -1,    16,    -1,    -1,
      -1,    20,    21,    22,    -1,   679,    -1,   681,   682,    28,
      -1,    -1,    -1,    -1,    -1,   379,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   701,    -1,    -1,
      -1,    -1,   706,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     338,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  3031,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  3043,    -1,
     744,    -1,    -1,   437,   438,   439,   280,    -1,    -1,   377,
      -1,    -1,   756,   447,    -1,   449,    -1,    -1,    -1,  1873,
      -1,    -1,   456,    -1,   298,   114,   460,   461,   462,    -1,
      -1,    -1,    -1,    -1,    -1,   469,    -1,    -1,   472,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   488,    -1,    -1,    -1,    -1,    -1,
    3105,    -1,    -1,   152,    -1,    -1,   340,   341,   342,   158,
     159,    -1,    -1,  2436,    -1,    -1,    -1,    -1,   352,    -1,
      -1,    -1,    -1,    -1,    -1,   174,    -1,    -1,    -1,   363,
     364,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    2463,    -1,   470,    -1,    -1,   379,    -1,    -1,    -1,    -1,
      -1,   855,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   553,
      -1,  2484,    -1,  2486,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  2496,    -1,   409,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   579,    -1,  2510,   516,    -1,
    2513,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   437,   438,   439,   440,   441,    -1,    -1,
      -1,  2534,    -1,   447,    -1,   449,    -1,    -1,  2541,    -1,
      -1,    -1,   456,    -1,    -1,    -1,   460,   461,   462,    -1,
      -1,   280,    -1,    -1,    -1,   469,   630,    -1,   472,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   574,    -1,   297,   298,
      -1,    -1,    -1,    -1,   488,    -1,    -1,    -1,    -1,  4396,
    4397,   152,    -1,    -1,    -1,    -1,    -1,   158,   159,    -1,
      -1,    -1,    -1,  2596,   508,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   174,    -1,    -1,   680,    -1,  2611,  4426,
      -1,   340,   341,   342,    -1,    -1,  2619,    -1,   532,    -1,
      -1,    -1,    -1,   352,    -1,    -1,    -1,  4444,   542,    -1,
      -1,    -1,    -1,    -1,   363,   364,    -1,   551,    -1,   553,
    2643,    -1,    -1,    -1,    -1,    -1,   560,    -1,  3333,    -1,
     379,    -1,   566,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   579,    -1,    -1,    -1,    -1,
     744,   679,    -1,   681,   682,    -1,    -1,    -1,   752,    -1,
     409,  4498,    -1,    -1,    -1,    -1,    -1,   601,    -1,    -1,
      -1,    -1,    -1,   701,    -1,  2698,    -1,    -1,   706,    -1,
      -1,    -1,  2196,  2706,   778,   779,  3391,    -1,   437,   438,
     439,   440,   441,    -1,    -1,    -1,   630,    -1,   447,  2722,
     449,    -1,    -1,  2726,    -1,    -1,    -1,   456,    -1,    -1,
      -1,   460,   461,   462,  2737,    -1,   744,    -1,    -1,    -1,
     469,    -1,    -1,   472,    -1,    -1,    -1,    -1,   756,   823,
      -1,    -1,    -1,    -1,    -1,    -1,   830,    -1,  3443,   488,
     834,    -1,    -1,    -1,    -1,    -1,   680,    -1,    -1,   340,
     341,   342,    -1,    -1,   848,    -1,    -1,    -1,    -1,   508,
      -1,   352,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   875,    -1,   532,    -1,    -1,    -1,    11,    -1,   883,
      -1,    -1,    16,   542,    -1,    -1,  3501,    -1,    -1,    -1,
      -1,    -1,   551,    -1,   553,    -1,    -1,    -1,    -1,    -1,
      -1,   560,    -1,   907,    -1,    -1,    -1,   566,   752,   913,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   855,    -1,    -1,
     579,    -1,    -1,   927,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   778,   779,   437,   438,   439,    -1,
      -1,    -1,   601,    -1,  2877,    -1,    -1,    -1,    -1,  2882,
      -1,    -1,    -1,    -1,    -1,   456,    -1,    -1,    -1,   460,
     461,   462,    -1,    -1,    -1,    -1,    -1,    -1,   469,    -1,
    3585,   630,    -1,   817,    -1,   819,    -1,   821,    -1,   823,
      -1,   825,    -1,   827,   828,   829,   830,    -1,   832,   833,
     834,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   848,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,
      -1,   680,    -1,    -1,   158,   159,  3641,  3642,  3643,    -1,
      -1,   875,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   883,
     174,    -1,    -1,    -1,    -1,    -1,     1,    -1,    -1,    -1,
      -1,    -1,   553,    -1,    -1,    -1,    11,    -1,  2991,    -1,
      -1,    16,    -1,   907,    -1,    20,    21,    22,    -1,   913,
      -1,    -1,    -1,    28,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   927,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   752,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,  3037,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    11,    -1,    -1,    -1,    -1,    16,    -1,   778,
     779,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  3753,    -1,
      -1,    -1,    -1,    -1,  3759,    -1,    -1,   281,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   817,   114,
     819,    -1,   821,    -1,    -1,    -1,   825,    -1,   827,   828,
     829,   830,    -1,   832,   833,    -1,    -1,    -1,    -1,   680,
      -1,    -1,    -1,    -1,    83,    -1,    -1,    -1,    -1,   848,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,
     334,    -1,    -1,   158,   159,    -1,   340,   341,   342,    -1,
      -1,    -1,  2636,    -1,    -1,    -1,   875,    -1,   352,   174,
      -1,    -1,    -1,    -1,   883,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,  3175,    -1,    -1,   379,    -1,    -1,   907,    -1,
      -1,    -1,    -1,   152,   913,    -1,    -1,   391,   157,   158,
     159,  2685,    -1,    -1,    -1,    -1,    -1,    -1,   927,    -1,
      -1,    -1,    -1,    -1,    -1,   174,    -1,   778,   779,    -1,
      -1,    -1,    -1,    -1,  2708,  2709,    -1,    -1,    -1,    -1,
      -1,  2715,     1,    -1,    -1,    -1,    -1,     6,    -1,    -1,
       9,    -1,    -1,   437,   438,   439,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   447,    -1,   449,    -1,    -1,    27,    -1,
      -1,    -1,   456,    -1,    -1,   280,   460,   461,   462,    -1,
      -1,    40,    -1,  3948,    -1,   469,    -1,    -1,   472,    -1,
    3273,    -1,    -1,   298,    -1,    -1,    -1,   848,    57,  3964,
      -1,   485,    -1,    -1,   488,    -1,    11,    -1,    -1,    -1,
    3293,    16,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   875,    -1,    -1,  3310,    -1,    -1,
      -1,    -1,   883,    -1,    -1,   340,   341,   342,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   352,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   907,    -1,   363,   364,
      -1,    -1,   913,   122,    -1,    -1,    -1,    -1,    -1,   553,
      -1,    -1,    -1,    -1,   379,    -1,   560,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,  3369,    -1,    -1,    -1,
      -1,   340,   341,   342,  3377,   579,  3379,    -1,    -1,   152,
      -1,    -1,    -1,   352,   409,   158,   159,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   174,    -1,    -1,    -1,    -1,  4091,    -1,    -1,    -1,
     379,    -1,   437,   438,   439,   440,   441,    -1,    -1,    -1,
      -1,    -1,   447,    -1,   449,    -1,   630,   152,    -1,    -1,
      -1,   456,    -1,   158,   159,   460,   461,   462,    -1,    -1,
      -1,    -1,    -1,    -1,   469,    -1,    -1,   472,    -1,   174,
      -1,  2945,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   488,    -1,    -1,    -1,    -1,   437,   438,
     439,    -1,    -1,    -1,    -1,    -1,   680,    -1,   447,    -1,
     449,    -1,    -1,   508,    -1,    -1,    -1,   456,    -1,    -1,
      -1,   460,   461,   462,    -1,    -1,    -1,    -1,    -1,    -1,
     469,    -1,    -1,   472,    -1,    -1,    -1,   532,    -1,    -1,
      -1,    -1,    -1,  3516,    -1,    -1,    -1,   542,    -1,   488,
      -1,    -1,    -1,    -1,    -1,    -1,   551,    -1,   553,    -1,
     309,   310,    -1,    -1,    -1,   560,  3539,  3540,    -1,    -1,
      -1,   566,    -1,    -1,  3038,    -1,    -1,    -1,   752,    -1,
      -1,    -1,   756,    -1,   579,    -1,    -1,   282,  3561,    -1,
      -1,  3564,    -1,    -1,    -1,    -1,    -1,   340,   341,   342,
      -1,    -1,    -1,    -1,   778,   779,   601,   781,    -1,   352,
      -1,    -1,    -1,    -1,   553,    -1,    -1,    -1,    -1,    -1,
      -1,   560,    -1,    -1,    -1,    -1,  3599,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   630,    -1,    -1,    -1,    -1,
     579,    -1,    -1,    -1,   393,   340,   341,   342,    -1,    -1,
      -1,    -1,   826,    -1,    -1,    -1,   830,   352,    -1,   354,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   848,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   379,   680,    -1,    -1,    -1,    -1,
      -1,   630,    -1,    -1,    -1,    -1,   439,    -1,    -1,   448,
    3673,   875,    -1,  3676,    -1,    -1,    -1,    -1,    -1,   883,
      -1,    -1,   651,   456,    -1,   464,    -1,   460,   461,   462,
      -1,    -1,    -1,    -1,    -1,    -1,   469,    -1,    -1,    -1,
      -1,    -1,    -1,   907,    -1,    -1,    -1,   911,    -1,   913,
      -1,   680,   437,   438,   439,    -1,    -1,  3720,    -1,    -1,
      -1,    -1,   447,   927,   449,    -1,    -1,   752,    -1,    -1,
      -1,   456,    -1,    -1,    -1,   460,   461,   462,    -1,    -1,
      -1,    -1,    -1,    -1,   469,    -1,    -1,   472,   527,    -1,
      -1,    -1,    -1,   778,   779,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   488,    -1,    -1,    -1,    -1,    -1,    -1,
    3264,  3265,  3266,  3267,    -1,   744,    -1,    -1,    -1,    -1,
     553,    -1,    -1,   752,    -1,    -1,    -1,    -1,    -1,   568,
    3793,    -1,   817,    -1,   819,    -1,   821,   576,    -1,    -1,
     825,    -1,   827,   828,   829,   830,   531,   832,   833,   778,
     779,    -1,  3306,  3307,  3817,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   848,    -1,    -1,    -1,    -1,   553,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     875,    -1,    -1,    -1,   579,    -1,    -1,    -1,   883,    -1,
      -1,   830,   641,    -1,  3867,    -1,    -1,    -1,    -1,    -1,
    3873,    -1,    -1,    -1,    -1,    -1,   655,    -1,    -1,   848,
    3883,    -1,   907,    -1,    -1,    -1,    -1,    -1,   913,    -1,
     859,   670,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   927,    -1,    -1,   630,   875,   680,    -1,    -1,
      -1,    -1,    -1,    -1,   883,    -1,    -1,    -1,    -1,    -1,
      -1,   890,    -1,  3926,    11,    -1,  3929,  3930,    -1,    16,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   907,    -1,
      -1,    -1,  3945,  3946,   913,    -1,    -1,  3950,   727,   728,
      -1,    -1,    -1,   678,   733,   680,    -1,    -1,   927,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   747,    -1,
      -1,    -1,    -1,    -1,  3977,    -1,    -1,  3980,    -1,    -1,
      -1,    -1,  3985,    -1,    -1,    -1,    -1,   766,    -1,    -1,
      -1,    -1,    -1,    -1,  3997,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   778,   779,    -1,    -1,  4012,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
    4023,    -1,    -1,    -1,    -1,    -1,    -1,   752,    -1,    -1,
      -1,    -1,  4035,    -1,    -1,    -1,    -1,    -1,  3532,    -1,
      -1,   820,    -1,    -1,    -1,   824,    -1,  4050,    -1,    -1,
      -1,    -1,    -1,   778,   779,    -1,    -1,    -1,    -1,    -1,
      -1,   840,   841,    -1,  4067,   152,    -1,  4070,    -1,    -1,
      -1,   158,   159,    -1,    -1,   848,    -1,    -1,    -1,    -1,
      -1,   168,    -1,    -1,    -1,    -1,    -1,   174,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   875,    -1,    -1,   830,    -1,    -1,    -1,    -1,
     883,    -1,    -1,    -1,    -1,  4118,    -1,    -1,  4121,  4122,
    4123,  4124,    -1,   848,    -1,    -1,    -1,    -1,  4131,    -1,
      -1,    -1,    -1,    -1,   907,    -1,    -1,    -1,    -1,    -1,
     913,  4144,    -1,    -1,    -1,    -1,    -1,  4150,    -1,    -1,
     875,    -1,    -1,  4156,  4157,    -1,    -1,    -1,   883,    -1,
      -1,    -1,    -1,    -1,    -1,  4168,    -1,    -1,    -1,    -1,
    3664,    -1,    -1,    -1,  4177,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   907,    -1,    -1,    -1,    -1,   912,   913,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   927,    -1,    -1,  4208,  4209,  4210,  4211,  4212,
    4213,  4214,  4215,  4216,  4217,  4218,  4219,  4220,  4221,  4222,
    4223,  4224,  4225,  4226,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,  4238,  4239,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,  4251,    -1,
      -1,    -1,    -1,   340,   341,   342,    -1,    -1,    -1,    -1,
    4263,    -1,    -1,    -1,    -1,   352,    -1,    -1,    -1,    -1,
      -1,    -1,  4275,    -1,    -1,    -1,    -1,    -1,    -1,  4282,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   379,    49,    50,    51,    52,    -1,    -1,    -1,
      56,    -1,    -1,  4306,  4307,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    74,    75,
      -1,    -1,  4325,    -1,    -1,    -1,    -1,  4330,  4331,    -1,
      -1,    -1,    -1,  4336,    -1,  4338,  4339,    -1,    -1,    -1,
    4343,  4344,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     437,   438,   439,    -1,    -1,    -1,  4359,    -1,    -1,    -1,
     447,    -1,   449,    -1,    -1,    -1,    -1,    -1,   124,   456,
      -1,    -1,    -1,   460,   461,   462,    -1,    -1,    -1,    -1,
      -1,  4384,   469,    -1,    -1,   472,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  4396,  4397,    -1,    -1,    -1,    -1,    -1,
      -1,   488,    -1,    -1,    -1,    -1,    -1,   163,    -1,    -1,
      -1,    -1,  4415,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,  4426,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,     3,    -1,    -1,
      -1,  4444,     8,  4446,  4447,    -1,    12,    -1,    -1,    15,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,  4460,    -1,    -1,
      -1,    27,  4465,    -1,    -1,    -1,   553,    -1,    -1,    -1,
      -1,    -1,  4475,  4476,    -1,    -1,    -1,    -1,  4481,    45,
      46,    -1,    -1,    -1,    -1,    -1,  4489,    -1,    -1,    -1,
      -1,    -1,   579,    -1,    -1,  4498,    -1,    -1,    -1,    65,
      66,    67,    68,    69,    70,    71,    72,    73,    -1,    -1,
      -1,    77,    78,    79,    -1,    81,    82,    -1,    -1,    -1,
      86,    -1,    -1,    89,    -1,   281,    -1,    -1,    -1,    -1,
      -1,    97,    98,    99,   100,   101,    -1,   103,   104,    -1,
      -1,    -1,    -1,   630,    -1,    -1,    -1,    -1,    -1,   115,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   125,
      -1,    -1,   128,   129,   130,   131,   132,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   338,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   680,    -1,    -1,    -1,    -1,    -1,    -1,
     166,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   175,
     176,   177,   178,   179,   180,   181,   182,    -1,    -1,    -1,
     186,   377,   188,   189,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   199,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   214,   215,
      -1,    -1,   218,   219,    -1,   221,   222,   223,   224,   225,
      -1,    -1,    -1,    -1,    -1,   752,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   245,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   778,   779,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   268,   269,    -1,    -1,   793,   273,    -1,    -1,
      -1,    -1,    -1,    -1,   470,    -1,    -1,    -1,   284,    -1,
      -1,    -1,   288,    -1,    -1,    -1,    -1,   293,   294,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   830,    -1,   311,   312,   313,   314,   315,
      -1,    -1,   318,    -1,    -1,    -1,   322,   323,   324,    -1,
      -1,   848,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   344,    -1,
      -1,    -1,    -1,   349,    -1,    -1,    -1,    -1,   875,    -1,
      -1,   357,   358,   359,   360,    -1,   883,    -1,    -1,    -1,
     366,   367,    -1,   369,    -1,   371,   372,   373,   374,   375,
     376,    -1,   378,    -1,   380,   381,   382,    -1,   574,    -1,
     907,    -1,    -1,    -1,    -1,    -1,   913,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   401,   402,    -1,    -1,    -1,
     927,    -1,    -1,    -1,   410,   411,   412,   413,   414,   415,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   424,    -1,
      -1,    -1,   428,    -1,    -1,    -1,   432,    -1,   434,    -1,
     436,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   453,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     466,   467,   468,    -1,    -1,   471,    -1,    -1,    -1,   475,
      -1,   477,   478,   479,   480,    -1,    -1,    -1,    -1,    -1,
      -1,   487,    -1,   679,    -1,   681,   682,   493,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   504,    -1,
      -1,    -1,    -1,    -1,    -1,   701,   512,    -1,    -1,    -1,
     706,   517,   518,   519,    -1,   521,    -1,   523,   524,    -1,
     526,    -1,   528,   529,    -1,    -1,   532,   533,    -1,   535,
     536,   537,   538,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   547,   548,    -1,    -1,   551,    -1,    -1,   744,    -1,
      -1,    -1,    -1,   559,    -1,    -1,    -1,    -1,    -1,    -1,
     756,    -1,    -1,    -1,    -1,    -1,   572,   573,    -1,    -1,
      -1,    -1,    -1,    -1,   580,   581,    -1,   583,    -1,    -1,
      -1,   587,    -1,    -1,    -1,    -1,    -1,    -1,   594,    -1,
      -1,    -1,    -1,    -1,   600,    -1,    -1,     7,   604,    -1,
     606,   607,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   620,    -1,    -1,   623,   624,    -1,
      -1,    -1,   628,    -1,    -1,    -1,    -1,   633,    -1,    -1,
      -1,    -1,   638,    -1,    -1,    -1,   642,    -1,   644,   645,
      -1,    -1,    -1,    53,    -1,    -1,    -1,   653,   654,    -1,
      -1,    -1,    -1,    -1,    -1,    65,    -1,    -1,    -1,   855,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   673,   674,   675,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   687,   688,   689,    -1,    -1,    -1,   693,   694,   695,
     696,   697,   698,    -1,    -1,    -1,   702,   703,    -1,    -1,
      -1,   111,    -1,    -1,   710,   711,    -1,    -1,    -1,    -1,
      -1,    -1,   718,   719,   720,   721,    -1,    -1,    -1,    -1,
     726,    -1,    -1,    -1,   134,   731,    -1,    -1,   734,   735,
     736,   737,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,   749,    -1,    -1,     5,    -1,   754,   755,
      -1,    10,    -1,    -1,    -1,    -1,   762,   763,    17,   765,
      -1,    -1,   768,    -1,    -1,    -1,    -1,    26,   774,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   191,    -1,    -1,    -1,   791,   792,    -1,    -1,   795,
      -1,    -1,    -1,    -1,   800,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   808,   809,   810,    -1,    -1,    -1,   814,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    87,    88,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   844,    -1,
     846,    -1,    -1,    -1,   850,    -1,    -1,    -1,    -1,    -1,
     856,    -1,    -1,    -1,    -1,    -1,    -1,   863,    -1,   118,
      -1,    -1,   272,    -1,    -1,    -1,    -1,    -1,   874,    -1,
      -1,    -1,   878,   879,   880,    -1,    -1,    -1,    -1,   138,
      -1,    -1,    -1,   142,    -1,   891,    -1,    -1,    -1,   895,
     896,   897,    -1,   899,   900,   901,   902,   903,    -1,    -1,
      -1,   160,    -1,   909,   910,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   921,    -1,    -1,   924,    -1,
      -1,    -1,    -1,    -1,    -1,   335,    -1,    -1,   187,    -1,
      -1,    -1,    -1,   343,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   201,    -1,    -1,    -1,    -1,    -1,   207,    -1,
     209,    -1,    -1,    -1,   213,    -1,    -1,    -1,   217,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   234,    -1,   236,    -1,    -1,
      -1,   240,   241,   242,   243,   244,    -1,   246,   247,   248,
     249,    -1,   251,   252,    -1,   254,   255,    -1,   257,   258,
     259,   260,   261,   262,   263,   264,   265,   266,   267,    -1,
      -1,   270,    -1,   423,    -1,    -1,    -1,    -1,    -1,   278,
     430,    -1,    -1,    -1,   283,    -1,    -1,   286,    -1,    -1,
      -1,    -1,   291,    -1,    -1,    -1,    -1,   296,   297,    -1,
      -1,    -1,    -1,    -1,    -1,   455,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,
      -1,    -1,    -1,     5,    -1,    -1,    -1,    -1,    10,    -1,
      -1,    -1,    -1,   483,    -1,    17,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    26,    -1,   345,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   353,    -1,    -1,   356,    -1,    -1,
      -1,    -1,   361,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   387,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   395,    -1,   397,    -1,
      -1,   400,    -1,    -1,   403,    87,    88,   557,    -1,   408,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   418,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   118,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   138,    -1,    -1,    -1,
     142,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   627,   160,    -1,
      -1,   631,    -1,    -1,    -1,   484,    -1,    -1,    -1,    -1,
      -1,   490,    -1,   492,    -1,    -1,   495,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   511,    -1,    -1,    -1,    -1,    -1,    -1,   201,
      -1,   520,    -1,    -1,    -1,   207,    -1,   209,    -1,    -1,
      -1,   213,    -1,    -1,    -1,   217,    -1,    -1,    -1,    -1,
     539,   540,   541,    -1,   543,   544,   545,   546,    -1,    -1,
      -1,    -1,   234,    -1,    -1,    -1,    -1,   707,    -1,    -1,
      -1,    -1,    -1,    -1,   563,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    16,   586,   270,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   286,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   296,   297,    -1,   767,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   626,    -1,    -1,
      -1,   781,    -1,   632,    -1,    -1,    -1,   636,    -1,    -1,
     639,   640,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,     1,    -1,    -1,   805,     5,   656,    -1,    -1,
      -1,    10,    -1,   345,    -1,    -1,    -1,    -1,    17,    -1,
      -1,   353,    -1,   672,   356,    -1,    -1,    26,   677,   361,
      -1,    -1,    -1,    -1,    -1,   684,    -1,    -1,    -1,    -1,
      -1,   690,    -1,    -1,    -1,    -1,    -1,   847,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   387,    -1,    -1,    -1,    -1,
     709,    -1,    -1,   395,    -1,   397,    -1,    -1,   400,    -1,
      -1,   403,   152,    -1,   723,    -1,    -1,    -1,   158,   159,
     729,    -1,    -1,    -1,    -1,    -1,   418,    -1,    87,    88,
      -1,    -1,    -1,   893,   174,    -1,   745,    -1,   747,    -1,
      -1,    -1,    -1,    -1,    -1,   905,    -1,     7,    -1,    -1,
      -1,   911,   761,   913,    -1,    -1,    -1,    -1,    -1,   118,
      -1,    -1,   771,   772,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   780,    -1,    -1,   783,    -1,    -1,    -1,    -1,   138,
      -1,    -1,    -1,   142,    -1,    -1,    -1,    -1,    -1,   798,
      -1,    -1,   484,    53,    -1,    -1,    -1,    -1,   490,    -1,
     492,   160,    -1,   495,    -1,    65,    -1,    -1,    -1,    -1,
      -1,   820,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   511,
      -1,    -1,   831,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   845,    -1,    -1,    -1,
      -1,    -1,   201,    -1,    -1,    -1,    -1,    -1,   207,   858,
     209,   111,    -1,    -1,   213,   864,    -1,    -1,   217,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   563,    -1,    -1,   134,   234,    -1,    -1,    -1,   888,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,
      -1,    -1,    -1,     5,   586,    -1,    -1,   906,    10,    -1,
     340,   341,   342,    -1,    -1,    17,    -1,    -1,   917,    -1,
      -1,   270,   352,   922,    26,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   286,    -1,    -1,
      -1,   191,    -1,    -1,   626,    -1,    -1,   296,   297,   379,
     632,    -1,    -1,    -1,   636,    -1,    -1,   639,   640,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   656,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    87,    88,    -1,    -1,    -1,
     672,    -1,    -1,    -1,    -1,   677,   345,    -1,    -1,    -1,
      -1,    -1,   684,    -1,   353,    -1,    -1,   356,   690,   439,
      -1,    -1,   361,    -1,    -1,    -1,   118,    -1,    -1,    -1,
      -1,    -1,   272,    -1,    -1,    -1,   456,   709,    -1,    -1,
     460,   461,   462,    -1,    -1,    -1,   138,    -1,   387,   469,
     142,   723,   472,    -1,    -1,    -1,   395,   729,   397,    -1,
      -1,   400,    -1,    -1,   403,    -1,    -1,    -1,   160,    -1,
      -1,    -1,    -1,    -1,    -1,   747,    -1,    -1,    -1,   418,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   761,
      -1,    -1,    -1,    -1,    -1,   335,    -1,    -1,    -1,   771,
     772,    -1,    -1,   343,    -1,    -1,    -1,    -1,   780,   201,
      -1,   783,    -1,    -1,    -1,   207,    -1,   209,    -1,    -1,
      -1,   213,    -1,    -1,    -1,   217,   798,    -1,    -1,    -1,
      -1,    -1,    -1,   553,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   234,    -1,    -1,   484,    -1,    -1,   820,    -1,
      -1,   490,    -1,   492,    -1,    -1,   495,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   511,   845,    -1,    -1,    -1,    -1,   270,    -1,
      -1,    -1,    -1,   423,    -1,    -1,   858,    -1,    -1,    -1,
     430,    -1,   864,    -1,   286,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   296,   297,    -1,    -1,    -1,    -1,
     630,    -1,    -1,    -1,    -1,   455,   888,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   563,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   483,    -1,   917,    -1,   586,    -1,    -1,
     922,    -1,    -1,   345,    -1,    -1,    -1,    -1,    -1,    -1,
     680,   353,    -1,    -1,   356,    -1,    -1,    -1,    -1,   361,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   626,    -1,    -1,
      -1,    -1,    -1,   632,    -1,   387,    -1,   636,    -1,    -1,
     639,   640,    -1,   395,    -1,   397,    -1,    -1,   400,    -1,
      -1,   403,    -1,    -1,    -1,    -1,    -1,   656,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   418,    -1,    -1,    -1,
      -1,    -1,   752,   672,    -1,    -1,    -1,    -1,   677,    -1,
      -1,    -1,    -1,    -1,    -1,   684,    -1,    -1,    -1,    -1,
      -1,   690,    -1,    -1,    -1,    -1,    -1,    -1,   778,   779,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     709,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   723,    -1,    -1,   627,    -1,    -1,
     729,   631,   484,    -1,    -1,    -1,    -1,    -1,   490,    -1,
     492,    -1,    -1,   495,    -1,    -1,    -1,    -1,   747,    -1,
     830,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   511,
      -1,    -1,   761,    -1,    -1,    -1,    -1,    -1,   848,    -1,
      -1,    -1,   771,   772,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   780,    -1,    -1,   783,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   875,    -1,    -1,    -1,   798,
      -1,    -1,    -1,   883,    -1,    -1,    -1,   707,    -1,    -1,
      -1,   563,    -1,    -1,    -1,    -1,    -1,    -1,     5,    -1,
      -1,   820,    -1,    10,    -1,    -1,    -1,   907,    -1,    -1,
      17,    -1,    -1,   913,   586,    -1,    -1,    -1,    -1,    26,
      -1,    -1,    -1,    -1,    -1,    -1,   845,   927,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   858,
      -1,    -1,    -1,    -1,    -1,   864,    -1,   767,    -1,    -1,
      -1,    -1,    -1,    -1,   626,    -1,    -1,    -1,    -1,    -1,
     632,   781,    -1,    -1,   636,    -1,    -1,   639,   640,   888,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      87,    88,    -1,    -1,   656,   805,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   917,    -1,
     672,    -1,    -1,   922,    -1,   677,    -1,    -1,    -1,    -1,
      -1,   118,   684,    -1,    -1,    -1,    -1,    -1,   690,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   847,    -1,    -1,
      -1,   138,    -1,    -1,    -1,   142,    -1,   709,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   723,    -1,   160,    -1,    -1,    -1,   729,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   893,    -1,   747,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   905,    -1,    -1,    -1,   761,
      -1,   911,    -1,   913,   201,    -1,    -1,    -1,    -1,   771,
     772,    -1,    -1,    -1,    -1,    -1,   213,    -1,   780,    -1,
     217,   783,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   798,    -1,    -1,   236,
      -1,    -1,    -1,    -1,   241,   242,   243,   244,    -1,   246,
     247,   248,   249,    -1,   251,    -1,   253,   254,   255,    -1,
     257,   258,   259,   260,   261,   262,   263,   264,   265,   266,
     267,    -1,    -1,   270,    -1,    -1,    -1,     1,    -1,    -1,
      -1,    -1,    -1,   845,    -1,    -1,    -1,    -1,    -1,   286,
      -1,    -1,    -1,    -1,    -1,    -1,   858,    -1,    -1,   296,
     297,    -1,   864,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   888,    -1,    -1,    -1,
      -1,    -1,    -1,    57,    58,    59,    60,    61,    -1,    63,
      64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   345,    -1,
      -1,    -1,    -1,    -1,    -1,   917,   353,    -1,    -1,   356,
     922,    -1,    -1,    -1,   361,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     387,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   395,    -1,
     397,    -1,    -1,   400,    -1,    -1,   403,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   141,    -1,   143,
     144,   145,   146,   147,   148,   149,   150,   151,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   484,    -1,   213,
      -1,    -1,    -1,   490,    -1,   492,    -1,    -1,   495,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   511,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,     8,    -1,    -1,    -1,    -1,    -1,    -1,
      15,    -1,    -1,    -1,    41,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    49,    50,    51,    52,    -1,    -1,    -1,    56,
      -1,    -1,    -1,    -1,    -1,    -1,   563,    -1,    -1,    -1,
      45,    -1,    -1,    -1,    -1,    -1,    -1,    74,    75,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   586,
      -1,    66,    67,    68,    69,    70,    71,    72,    73,    -1,
      -1,   325,   326,   327,   328,   329,    81,    -1,   332,   333,
      -1,    86,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    97,    98,    99,   100,   123,   124,    -1,    -1,
     127,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   636,
     115,    -1,    -1,    -1,   368,    -1,    -1,    -1,    -1,    -1,
     125,    -1,    -1,   128,   129,   130,    -1,   132,    -1,   656,
      -1,    -1,    -1,    -1,    -1,    -1,   163,    -1,   392,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     677,    -1,    -1,    -1,    -1,    -1,    -1,   684,    -1,    -1,
      -1,   166,    -1,   690,    -1,    -1,    -1,    -1,    -1,    -1,
     175,   176,   177,   178,   179,   180,   181,   182,    -1,    -1,
      -1,    -1,   709,   188,   189,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   729,    -1,    -1,    -1,    -1,    -1,    -1,   214,
     215,    -1,    -1,   218,   219,    -1,    -1,    -1,   223,    -1,
     747,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   761,    -1,    -1,    -1,    -1,    -1,
     245,    -1,    -1,    -1,   771,   772,   500,    -1,    -1,    -1,
      -1,    -1,    -1,   780,   281,    -1,   783,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   269,    -1,    -1,    -1,   273,    -1,
      -1,   798,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   288,    -1,    -1,    -1,    -1,   293,   294,
      -1,    -1,    -1,   820,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   311,   312,   313,   314,
     315,   338,    -1,   318,    -1,    -1,    -1,    -1,   845,    -1,
      -1,   575,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   858,    -1,    -1,    -1,    -1,    -1,   864,    -1,    -1,
      -1,    -1,   596,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     377,    -1,   357,   358,   359,   360,    -1,    -1,    -1,    -1,
      -1,   366,   367,    -1,   369,   619,   371,   372,   373,    -1,
      -1,   376,    -1,   378,    -1,    -1,    -1,   382,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     917,    -1,    -1,    -1,    -1,   922,   401,   402,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   410,   411,   412,   413,   414,
     415,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   424,
      -1,   448,    -1,   428,    -1,    -1,    -1,   432,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   470,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   489,    -1,    -1,    -1,   471,    -1,    -1,    -1,
     475,    -1,   477,   478,   479,   480,    -1,    -1,    -1,    -1,
      -1,    -1,   487,    -1,    -1,    -1,   740,   741,   742,   516,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   504,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   512,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   532,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   547,   548,    -1,    -1,    -1,   574,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   598,   599,    -1,    -1,   580,    -1,    -1,   583,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   594,
      -1,    -1,    -1,    -1,    -1,   600,    -1,    -1,    -1,   604,
      -1,   606,   607,    -1,    -1,    -1,   860,   861,   862,    -1,
      -1,    -1,    -1,    -1,    -1,   620,    -1,    -1,   623,   624,
      -1,    -1,    -1,   628,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   642,    -1,   644,
     645,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   653,   654,
      -1,    -1,   679,    -1,   681,   682,    -1,    -1,    -1,   913,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   673,   674,
     675,    -1,    -1,     6,   701,    -1,     9,    -1,    -1,   706,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   693,   694,
     695,   696,    -1,   698,    27,    -1,    -1,   702,   703,    -1,
      -1,    -1,    -1,    -1,    -1,   710,   711,    40,    -1,    -1,
      -1,    -1,    -1,   718,   719,   720,    -1,   744,    -1,    -1,
      -1,   726,    -1,    -1,    -1,    -1,   731,    -1,    -1,   756,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   749,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   762,   763,     8,
      -1,    -1,    -1,   768,    -1,    -1,    15,    -1,    -1,   774,
      -1,    -1,   152,    -1,    -1,    -1,    -1,    -1,   158,   159,
      -1,    -1,    -1,    -1,    -1,    -1,   791,   792,    -1,   122,
      -1,    -1,    -1,    -1,   174,   800,    45,    -1,    -1,    -1,
      -1,    -1,    -1,   808,    -1,    -1,    -1,    -1,    -1,   814,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    66,    67,    68,
      69,    70,    71,    72,    73,    -1,    -1,    -1,   855,    -1,
      -1,    -1,    81,    -1,    -1,    -1,    -1,    86,    -1,   844,
      -1,    -1,   869,    -1,    -1,   850,    -1,    -1,    97,    98,
      99,   100,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   115,    -1,    -1,    11,
      -1,    -1,    -1,    -1,    16,    -1,   125,    -1,    -1,   128,
     129,   130,    -1,   132,    -1,   912,   891,    -1,    -1,    -1,
      -1,    -1,   897,    -1,   899,    -1,    -1,   902,    -1,    -1,
      -1,    -1,    -1,    -1,   909,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   921,   166,    -1,   924,
      -1,    -1,    -1,    -1,    -1,    -1,   175,   176,   177,   178,
     179,   180,   181,   182,    -1,    -1,    -1,    -1,    -1,   188,
     189,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     340,   341,   342,    -1,    -1,   214,   215,    -1,    -1,   218,
     219,    -1,   352,    -1,   223,    -1,   309,   310,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   245,    -1,    -1,   379,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     152,    -1,    -1,    -1,    -1,    -1,   158,   159,    -1,    -1,
     269,    -1,    -1,    -1,   273,    -1,   168,    -1,    -1,    -1,
      -1,    -1,   174,    -1,    -1,    -1,    -1,    -1,    -1,   288,
      -1,    -1,    -1,    -1,   293,   294,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   439,
     393,    -1,   311,   312,   313,   314,   315,    -1,    -1,   318,
      -1,    -1,    -1,    -1,    -1,    -1,   456,    -1,    -1,    -1,
     460,   461,   462,    -1,    -1,    -1,    -1,    -1,    -1,   469,
      -1,    -1,   472,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   357,   358,
     359,   360,    -1,    -1,    -1,   448,    -1,   366,   367,    -1,
     369,    -1,   371,   372,   373,    -1,    -1,   376,    -1,   378,
      -1,   464,    -1,   382,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   401,   402,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   410,   411,   412,   413,   414,   415,    -1,    -1,    -1,
      -1,    -1,    -1,   553,    -1,   424,    -1,    -1,    -1,   428,
     429,    -1,    -1,   432,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   527,    -1,    -1,    -1,   340,   341,
     342,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     352,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   471,    -1,    -1,    -1,   475,    -1,   477,   478,
     479,   480,    -1,    -1,    -1,   568,    -1,   379,   487,    -1,
      11,    -1,    -1,   576,    -1,    16,    -1,    -1,    -1,    -1,
     630,    -1,    -1,    -1,    -1,   504,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   512,    -1,    -1,    -1,    -1,    -1,    -1,
      41,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    50,
      51,    52,    -1,    -1,    -1,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   437,   438,   439,   547,   548,
     680,    -1,    -1,    74,    75,   447,    -1,   449,   641,    -1,
      -1,    -1,    -1,    -1,   456,    -1,    -1,    -1,   460,   461,
     462,    -1,   655,    -1,    -1,    -1,    -1,   469,    -1,    -1,
     472,   580,    -1,    -1,   583,    -1,    -1,   670,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   594,   488,    -1,    -1,    -1,
      -1,   600,   123,   124,    -1,   604,   127,   606,   607,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   620,   752,    -1,   623,   624,    -1,    -1,    -1,   628,
      -1,   152,    -1,    -1,    -1,    -1,    -1,   158,   159,    -1,
      -1,    -1,   163,   642,   727,   644,   645,    -1,   778,   779,
     733,    -1,    -1,   174,   653,   654,    -1,    -1,    -1,    -1,
      -1,   553,    -1,    -1,   747,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   673,   674,   675,    -1,    -1,    -1,
      -1,    -1,    -1,   766,    -1,    -1,    -1,   579,    -1,    -1,
      -1,    -1,    -1,    -1,   693,   694,   695,   696,    -1,   698,
     830,    -1,    -1,   702,   703,    -1,    -1,    -1,    -1,    -1,
      -1,   710,   711,    -1,    -1,    -1,    -1,    -1,   848,   718,
     719,   720,    -1,    -1,    -1,    -1,    -1,   726,    -1,    -1,
      -1,    -1,   731,    -1,    -1,    -1,    -1,   820,   630,    -1,
      -1,   824,    -1,    -1,    -1,   875,    -1,    -1,    -1,    -1,
     749,    -1,    -1,   883,    -1,    -1,    -1,   840,   841,    -1,
     281,    -1,    -1,   762,   763,    -1,    -1,    -1,    -1,   768,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   907,    -1,    -1,
      -1,    -1,    -1,   913,    -1,    -1,    -1,    -1,   680,    -1,
      -1,    -1,   791,   792,    -1,    -1,    -1,   927,    -1,    -1,
      -1,   800,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   808,
      -1,    -1,    -1,    -1,    -1,   814,    -1,   338,    -1,   340,
     341,   342,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   352,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   844,    -1,    -1,    -1,    -1,
      -1,   850,    -1,    -1,    -1,    -1,   377,    -1,   379,    -1,
     752,    -1,    11,    -1,    -1,    -1,    -1,    16,    -1,    -1,
      -1,    20,    21,    22,    -1,    -1,    -1,    -1,    -1,    28,
      -1,    -1,    -1,    -1,    -1,    -1,   778,   779,    -1,    -1,
      -1,    -1,   891,    -1,    -1,    -1,    -1,    -1,   897,    -1,
     899,   793,    -1,   902,    -1,    -1,    -1,    -1,    -1,    -1,
     909,    -1,    -1,    -1,    -1,    -1,   437,   438,   439,    -1,
      -1,    -1,   921,    -1,    -1,   924,   447,   448,   449,    -1,
      -1,    -1,    -1,    -1,    -1,   456,    -1,    -1,   830,   460,
     461,   462,    -1,    -1,    -1,    -1,    -1,    -1,   469,   470,
      -1,   472,    -1,    -1,    -1,    -1,   848,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   114,    -1,   488,   489,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   875,    -1,    -1,    -1,    11,    -1,    -1,
      -1,   883,    16,    -1,    -1,   516,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   152,    28,    -1,    -1,    -1,    -1,   158,
     159,    -1,    -1,    -1,    -1,   907,    -1,    -1,    -1,    -1,
      -1,   913,    -1,    -1,    -1,   174,    -1,    -1,    -1,    -1,
      -1,    -1,   553,    -1,    -1,   927,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   574,    -1,    -1,    -1,    -1,   579,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   598,   599,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    11,    -1,
      -1,    -1,    -1,    16,    -1,    -1,    -1,    -1,    -1,   630,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,
      -1,   280,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   298,
     174,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   679,   680,
     681,   682,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     701,    -1,    -1,    -1,    -1,   706,    -1,    -1,    -1,    -1,
      -1,   340,   341,   342,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   352,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   363,   364,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   744,    -1,    -1,    -1,    -1,    -1,    -1,
     379,   752,    -1,    -1,    -1,   756,    -1,    -1,    -1,   152,
      -1,    -1,    -1,    -1,    -1,   158,   159,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   168,    -1,   778,   779,    -1,
     409,   174,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   298,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   437,   438,
     439,   440,   441,    -1,    -1,    -1,    -1,    -1,   447,    -1,
     449,    -1,    -1,    -1,    -1,    -1,    -1,   456,    -1,   830,
      -1,   460,   461,   462,    -1,    -1,   340,   341,   342,    -1,
     469,    -1,    -1,   472,    -1,    -1,    -1,   848,   352,    -1,
      -1,    -1,    -1,    -1,   855,    -1,    -1,    -1,    -1,   488,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   869,    -1,
      -1,    -1,    -1,    -1,   875,   379,    -1,    -1,    -1,   508,
      -1,    -1,   883,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   532,    -1,   409,   907,    -1,    -1,    -1,
      -1,   912,   913,   542,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   551,    -1,   553,    -1,   927,    -1,    -1,    -1,
      -1,   560,    -1,   437,   438,   439,    -1,   566,    -1,    11,
      -1,    -1,    -1,   447,    16,   449,    -1,   340,   341,   342,
     579,    -1,   456,    -1,    -1,    -1,   460,   461,   462,   352,
      -1,    -1,    -1,    -1,    -1,   469,    -1,    -1,   472,    -1,
      -1,    -1,   601,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   488,    -1,   379,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   630,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   532,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   437,   438,   439,    -1,    -1,   553,
      -1,   680,    -1,    -1,   447,    -1,   449,    -1,    -1,    -1,
      -1,    -1,   566,   456,    -1,    -1,    -1,   460,   461,   462,
      -1,    -1,    -1,    -1,    -1,   579,   469,    11,    -1,   472,
     152,    -1,    16,    -1,    -1,    -1,   158,   159,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   488,    -1,    -1,    -1,    -1,
      -1,    -1,   174,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    11,
      -1,    -1,    -1,   752,    16,    -1,   630,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    11,   778,
     779,    -1,    -1,    16,    -1,    -1,    -1,    -1,    -1,    -1,
     553,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   680,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   579,    -1,   817,    -1,
     819,    -1,   821,    -1,    -1,    -1,   825,    -1,   827,   828,
     829,   830,    -1,   832,   833,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   152,   848,
      -1,    -1,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   630,    -1,    -1,
     174,    -1,    -1,    -1,    -1,    -1,   875,    -1,   752,    -1,
      -1,    -1,    -1,    -1,   883,    -1,    -1,    -1,    -1,    -1,
     152,    -1,    -1,    -1,    -1,    -1,   158,   159,   340,   341,
     342,    -1,    -1,    -1,   778,   779,    -1,    -1,   907,    -1,
     352,    -1,   174,    -1,   913,    -1,    -1,   680,    -1,   152,
      -1,    -1,    -1,    -1,    -1,   158,   159,    -1,   927,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   379,    -1,    -1,
      -1,   174,    -1,   817,    -1,   819,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   828,   829,   830,    -1,   832,   833,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   848,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   752,
      -1,    -1,    -1,    -1,    -1,   437,   438,   439,    -1,    -1,
      -1,   875,    -1,    -1,    -1,   447,    -1,   449,    -1,   883,
      -1,    -1,    -1,    -1,   456,   778,   779,    -1,   460,   461,
     462,    -1,    -1,    -1,    -1,    -1,    -1,   469,    -1,    -1,
     472,    -1,    -1,   907,    -1,    -1,   340,   341,   342,   913,
      -1,    -1,    -1,    -1,    -1,    -1,   488,    -1,   352,    -1,
      -1,    -1,    -1,   927,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   830,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   379,    -1,    -1,   340,   341,
     342,    -1,    -1,    -1,   346,   848,    -1,    -1,    -1,    -1,
     352,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    11,    -1,   340,   341,   342,
      16,   553,   875,    -1,    -1,    -1,    -1,   379,    -1,   352,
     883,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   437,   438,   439,    -1,   579,    -1,    -1,
      -1,    -1,    -1,   447,   907,   449,   379,    -1,    -1,    -1,
     913,    -1,   456,    -1,    -1,    -1,   460,   461,   462,    -1,
      -1,    -1,    -1,    -1,   927,   469,    -1,    -1,   472,    -1,
      -1,    -1,    -1,    -1,    -1,   437,   438,   439,    -1,    -1,
      -1,    -1,    -1,    -1,   488,   447,    -1,   449,   630,    -1,
      -1,    -1,    -1,    -1,   456,    -1,    -1,    -1,   460,   461,
     462,    -1,    -1,    -1,   437,   438,   439,   469,    11,    -1,
     472,    -1,    -1,    16,   447,    -1,   449,    -1,    -1,    -1,
      -1,    -1,    -1,   456,    -1,    -1,   488,   460,   461,   462,
      -1,    -1,    -1,    -1,    -1,    -1,   469,    -1,   680,   472,
      -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,   553,
      -1,    -1,   158,   159,    -1,   488,   560,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   174,    -1,
      -1,    -1,    -1,    -1,    -1,   579,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   553,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     752,    -1,    -1,    -1,    -1,    -1,    -1,   579,    -1,    -1,
     553,    -1,    -1,    -1,    -1,    -1,   630,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   778,   779,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   579,    -1,    -1,   152,
      -1,    -1,    -1,    -1,    -1,   158,   159,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   630,    -1,
      -1,   174,    -1,   815,    -1,    -1,   680,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   830,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   630,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   848,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   680,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   875,   340,   341,   342,    -1,    -1,    -1,
      -1,   883,    -1,    -1,    -1,    -1,   352,   680,   752,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   907,    -1,    -1,    -1,    -1,
      -1,   913,    -1,   379,   778,   779,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   927,    -1,    -1,    -1,    -1,
     752,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   744,    -1,    -1,    -1,    -1,   778,   779,    -1,   752,
      -1,    -1,    -1,    -1,    -1,    -1,   830,    -1,    -1,    -1,
      -1,   437,   438,   439,    -1,    -1,    -1,   340,   341,   342,
      -1,   447,    -1,   449,   848,   778,   779,    -1,    -1,   352,
     456,    -1,    -1,    -1,   460,   461,   462,    -1,    -1,    -1,
      -1,    -1,    -1,   469,    -1,    -1,   472,    -1,   830,    -1,
      -1,   875,    -1,    -1,    -1,    -1,   379,    -1,    -1,   883,
      -1,    -1,   488,    -1,    -1,    -1,   848,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   830,    -1,    -1,
      -1,    -1,    -1,   907,    -1,    -1,    -1,    -1,    -1,   913,
      -1,    -1,    -1,   875,    -1,   848,    -1,    -1,    -1,    -1,
      -1,   883,    -1,   927,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   437,   438,   439,    -1,    -1,    -1,
      -1,    -1,   875,    -1,   447,   907,   449,   553,    -1,    -1,
     883,   913,    -1,   456,    -1,    -1,    -1,   460,   461,   462,
      -1,    -1,    -1,    -1,    -1,   927,   469,    -1,    -1,   472,
      -1,    -1,    -1,   579,   907,    -1,    -1,    -1,    -1,    -1,
     913,    -1,    -1,    -1,    -1,   488,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   927,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,     1,
      -1,    -1,    -1,    -1,    -1,     7,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   630,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     553,    -1,    44,    -1,    -1,    -1,    -1,    49,    50,    51,
      52,    53,    -1,    -1,    56,    57,    58,    59,    60,    61,
      -1,    63,    64,    65,   680,    -1,   579,    -1,    -1,    -1,
      -1,    -1,    74,    75,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    94,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   111,
      -1,    -1,    -1,    -1,   116,    -1,    -1,   630,    -1,    -1,
      -1,   123,   124,    -1,    -1,   127,    -1,    -1,    -1,    -1,
      -1,    -1,   134,    -1,    -1,    -1,   752,    -1,    -1,   141,
      -1,   143,   144,   145,   146,   147,   148,   149,   150,   151,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   778,   779,    -1,    -1,    -1,   680,    -1,    -1,
     172,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   191,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   213,    -1,    -1,   830,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   848,    -1,    -1,    -1,    -1,    -1,    -1,   752,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   875,
      -1,    -1,    -1,    -1,    -1,   778,   779,   883,    -1,    -1,
     272,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   281,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   907,    -1,    -1,    -1,    -1,    -1,   913,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   927,    -1,    -1,   316,    -1,    -1,   830,    -1,    -1,
      -1,    -1,    -1,   325,   326,   327,   328,   329,    -1,    -1,
     332,   333,    -1,    -1,    -1,   848,   338,    -1,    -1,    -1,
      -1,   343,    -1,    -1,   346,    -1,   348,    -1,    -1,    -1,
      -1,    -1,    -1,   355,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   875,   365,    -1,    -1,   368,    -1,    -1,    -1,
     883,    -1,    -1,    -1,    -1,   377,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     392,    -1,    -1,    -1,   907,    -1,    -1,    -1,    -1,    -1,
     913,    -1,    -1,    -1,    -1,    -1,    -1,   409,    -1,    -1,
      -1,    -1,    -1,    -1,   927,    -1,    -1,   419,    -1,     1,
      -1,   423,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   431,
      -1,    -1,    -1,   435,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   448,    29,    -1,    -1,
     452,    -1,    -1,   455,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   470,    -1,
      16,    -1,    54,    -1,    -1,    57,    58,    59,    60,    61,
      -1,    63,    64,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    74,    -1,    -1,    -1,    -1,    -1,   500,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   515,    -1,    -1,    -1,    -1,    -1,    -1,
     522,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    84,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   554,    -1,   556,    -1,   558,    -1,    -1,   141,
      -1,   143,   144,   145,   146,   147,   148,   149,   150,   151,
      -1,    -1,   574,   575,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   591,
      -1,    -1,    -1,    -1,   596,    -1,   598,   599,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,    -1,
      -1,    -1,   158,   159,    -1,    -1,    -1,   619,    -1,    -1,
     622,    -1,    -1,    -1,    -1,   627,    -1,    -1,   174,   631,
      -1,   213,    -1,    -1,     1,    -1,    -1,     4,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   667,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   679,    -1,   681,
     682,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      57,    58,    59,    60,    61,    -1,    63,    64,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   707,    -1,    74,    -1,    -1,
      -1,    -1,   714,    -1,    -1,    -1,    -1,    -1,    -1,   301,
     302,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   316,    -1,   738,    -1,   740,   741,
     742,    -1,   744,   325,   326,   327,   328,   329,    -1,    -1,
     332,   333,    -1,    -1,   756,    -1,   123,    -1,    -1,   126,
     127,    -1,    -1,    -1,    -1,   767,   133,    -1,    -1,    -1,
      -1,    -1,    -1,   355,   141,    -1,   143,   144,   145,   146,
     147,   148,   149,   150,   151,    -1,   368,    -1,   790,    -1,
      -1,    -1,    -1,   339,   340,   341,   342,    -1,    -1,    -1,
      -1,    -1,    -1,   385,    -1,    -1,   352,    -1,    -1,    -1,
     392,    -1,    -1,   815,    -1,    -1,    -1,    -1,   820,    -1,
      -1,    -1,    -1,    -1,   826,    -1,    -1,   409,    -1,    -1,
      -1,    -1,    -1,   379,    -1,    -1,    -1,   419,    -1,    -1,
      -1,   843,    -1,    -1,    -1,   847,   213,    -1,    -1,   431,
      -1,    -1,    -1,   855,    -1,    -1,    -1,    -1,   860,   861,
     862,   443,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   876,    -1,    -1,    16,    -1,    -1,
      -1,    -1,   884,    -1,    -1,    -1,    -1,    -1,   890,    -1,
      -1,   437,   438,   439,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   905,    -1,    -1,    -1,    -1,    -1,    -1,
     456,   913,    -1,    -1,   460,   461,   462,    -1,   500,    -1,
      -1,    -1,    -1,   469,    -1,    -1,   472,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    84,    -1,    -1,    -1,   316,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   325,   326,
     327,   328,   329,    -1,    -1,   332,   333,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   556,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   575,    -1,    -1,    -1,    -1,    -1,   366,
      -1,   368,    -1,    -1,    -1,    -1,    -1,   553,    -1,   591,
      -1,    -1,    -1,   152,   596,    -1,    -1,    -1,    -1,   158,
     159,    -1,    -1,    -1,    -1,   392,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   174,    -1,   619,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   419,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   431,    -1,    -1,   649,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   448,    -1,    -1,   630,   452,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   701,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   500,   680,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   513,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   738,    -1,   740,   741,
     742,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   753,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   556,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     339,   340,   341,   342,   786,    -1,   752,    -1,   575,    -1,
      -1,    -1,    -1,   352,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   591,    -1,    -1,    -1,    -1,   596,
      -1,    -1,   778,   779,    -1,   602,    -1,    -1,   820,    -1,
     379,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   619,    -1,    -1,   801,    -1,    -1,    -1,    -1,
      -1,   843,    -1,    -1,    -1,    -1,    -1,    -1,   850,   851,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   860,   861,
     862,    -1,    -1,    -1,   830,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   876,    -1,    -1,    -1,   437,   438,
     439,    -1,   848,    -1,    -1,    -1,    -1,    -1,   890,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   898,   456,    -1,   865,
      -1,   460,   461,   462,    -1,    -1,    -1,    -1,    -1,   875,
     469,   913,    -1,   472,     3,    -1,    -1,   883,    -1,    -1,
      -1,    -1,    -1,    12,    -1,    -1,    -1,    -1,   894,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    27,    -1,
      -1,   907,    -1,    -1,    -1,    -1,   912,   913,    -1,    -1,
      -1,   738,    -1,   740,   741,   742,    -1,    46,    -1,    -1,
      -1,   927,    -1,   750,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    65,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    77,    78,
      79,    -1,    -1,    82,   553,   782,    -1,    -1,    -1,    -1,
      89,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   101,    -1,   103,   104,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   820,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   131,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   843,    -1,    -1,    -1,
      -1,    -1,    -1,   850,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   630,    -1,   860,   861,   862,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   876,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   186,    -1,    -1,
      -1,    -1,    -1,   890,    -1,    -1,    -1,   894,    -1,    -1,
     199,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   680,    -1,    -1,    -1,    -1,   913,    -1,    -1,    -1,
      -1,    -1,   221,   222,    -1,   224,   225,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    41,    -1,    -1,    44,   268,
      -1,    -1,    -1,    49,    50,    51,    52,    -1,    -1,    55,
      56,    -1,    -1,   752,    -1,   284,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    75,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   778,
     779,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   322,   323,   324,    -1,    -1,    -1,    -1,
      -1,    -1,   801,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   344,    -1,   123,   124,    -1,
     349,   127,    -1,    -1,    -1,    -1,     4,    -1,    -1,    -1,
      -1,   830,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   374,   375,    -1,    -1,   848,
      -1,   380,   381,    -1,    -1,   161,    -1,   163,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   865,    -1,    -1,   175,
      -1,    -1,    -1,    -1,    -1,    -1,   875,    -1,    -1,    57,
      58,    59,    60,    61,   883,    63,    64,    -1,    -1,    -1,
      -1,    -1,   198,    -1,    -1,   894,    74,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   434,    -1,   436,   907,    -1,
      -1,    -1,    -1,   912,   913,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   453,    -1,    -1,    -1,   927,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   466,   467,   468,
      -1,    -1,    -1,    -1,    -1,   123,    -1,    -1,   126,   127,
      -1,    -1,    -1,    -1,    -1,   133,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   141,   493,   143,   144,   145,   146,   147,
     148,   149,   150,   151,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   517,   518,
     519,    -1,   521,    -1,   523,   524,    -1,   526,    -1,   528,
     529,    -1,    -1,    -1,   533,    -1,   535,   536,   537,   538,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   551,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     559,    -1,   338,    -1,    -1,   213,    -1,    -1,    -1,    -1,
      -1,   347,   348,   572,   573,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   581,    -1,    -1,    -1,    -1,    -1,   587,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   377,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      41,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    49,    50,
      51,    52,    -1,    -1,    -1,    56,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   633,    -1,    -1,    -1,    -1,   638,
      -1,    -1,    -1,    74,    75,   421,    77,    -1,    79,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   435,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   102,   448,    -1,    -1,    -1,    -1,   325,   326,   327,
     328,   329,    -1,    -1,   332,   333,    -1,    -1,   687,   688,
     689,   467,   123,   124,   470,    -1,   127,    -1,   697,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   489,    -1,    -1,    -1,    -1,   366,    -1,
     368,    -1,   721,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   163,    -1,    -1,   734,   735,   736,   737,   515,
      -1,    -1,    -1,    -1,   392,    -1,   522,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   754,   755,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   765,    -1,    -1,    -1,
      -1,   419,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   558,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   795,    -1,   574,    -1,
     448,    -1,    -1,    -1,   452,    -1,    -1,    -1,    -1,    -1,
     809,   810,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   598,   599,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    44,    -1,    -1,    -1,
      -1,    49,    50,    51,    52,    -1,   622,   846,    56,   625,
     281,    -1,   500,    -1,    -1,    -1,    -1,   856,    -1,    -1,
      -1,    -1,    -1,    -1,   863,   513,    -1,    75,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   874,    -1,    -1,    -1,   878,
     879,   880,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   667,    -1,    -1,    -1,    -1,   895,   896,    -1,    -1,
      -1,   900,   901,   679,   903,   681,   682,   338,   556,    -1,
      -1,   910,    -1,    -1,    -1,    -1,   124,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   701,    -1,   575,    -1,    -1,
     706,    -1,    -1,    -1,    -1,    -1,    -1,   368,   714,    -1,
      -1,    -1,    -1,   591,    -1,    -1,   377,    -1,   596,    -1,
      -1,    -1,    -1,   161,   602,   163,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   175,   744,    -1,
      -1,   619,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     756,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     198,    -1,    -1,    -1,    -1,    29,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   434,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   790,    -1,    -1,   448,    -1,    -1,
      54,   452,    -1,    57,    58,    59,    60,    61,    -1,    63,
      64,    -1,    -1,    -1,   810,   811,    -1,    -1,    -1,   470,
      74,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   855,
      -1,    -1,    -1,    -1,   515,    -1,    -1,    -1,    -1,    -1,
     738,   867,   740,   741,   742,    -1,    -1,    -1,   874,    -1,
      -1,    -1,   750,    -1,    -1,    -1,    -1,   141,    -1,   143,
     144,   145,   146,   147,   148,   149,   150,   151,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     338,    -1,    -1,    -1,   782,    -1,   912,    -1,    -1,    -1,
     348,    -1,    -1,   574,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    29,    -1,   597,   598,   599,   377,
      -1,    -1,   820,    -1,    -1,    -1,    -1,    -1,    -1,   213,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    54,
      -1,    -1,    57,    58,    59,    60,    61,    -1,    63,    64,
      -1,    -1,   850,    -1,    -1,    -1,    -1,    -1,    -1,    74,
      -1,    -1,   860,   861,   862,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   435,   876,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   890,    -1,    -1,    -1,   894,    -1,   679,    -1,
     681,   682,    -1,    -1,    -1,    -1,   687,    -1,    -1,   467,
      -1,    -1,   470,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     701,    -1,    -1,    -1,    -1,   706,   141,    -1,   143,   144,
     145,   146,   147,   148,   149,   150,   151,    -1,    -1,    -1,
      -1,   325,   326,   327,   328,   329,    -1,    -1,   332,   333,
      -1,   732,    -1,    -1,    -1,    -1,    -1,   515,    -1,    -1,
      -1,    -1,    -1,   744,   522,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   756,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   368,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   213,    -1,
     558,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   392,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   574,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   409,    -1,    -1,    -1,    -1,
      -1,    -1,   813,    -1,    -1,   419,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   431,    -1,    -1,
      -1,    -1,    -1,    -1,   835,    -1,    -1,    -1,    -1,   443,
      -1,    -1,    -1,    -1,   622,    -1,    -1,   625,    -1,    -1,
      -1,    -1,    -1,    -1,   855,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   667,
     325,   326,   327,   328,   329,    -1,   500,   332,   333,    -1,
      -1,   679,    -1,   681,   682,    -1,    -1,    -1,    -1,    -1,
      -1,   912,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   701,    -1,    -1,    -1,    -1,   706,    -1,
      -1,    -1,    -1,   368,    -1,    -1,   714,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   556,    -1,    -1,    -1,    -1,   392,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   744,    -1,    -1,    -1,
      -1,   575,    -1,    -1,   409,    -1,    -1,    -1,   756,    -1,
      -1,    -1,    -1,    -1,   419,    -1,    -1,   591,    -1,    -1,
      -1,    -1,   596,    -1,    -1,    -1,   431,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   443,    -1,
      -1,    -1,   790,    -1,    -1,   619,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   811,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    44,   649,    -1,    -1,    -1,    49,
      50,    51,    52,    -1,    -1,    -1,    56,    57,    58,    59,
      60,    61,    -1,    63,    64,   500,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    74,    75,    -1,   855,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   867,
      -1,    -1,    -1,    -1,    94,    -1,   874,   701,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   116,    -1,    -1,    -1,
      -1,   556,    -1,   123,   124,    -1,    -1,   127,    -1,    -1,
      -1,    -1,    -1,    -1,   738,    -1,   740,   741,   742,    -1,
     575,   141,    -1,   143,   144,   145,   146,   147,   148,   149,
     150,   151,    -1,    -1,    -1,    -1,   591,    -1,    -1,    -1,
      -1,   596,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   172,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   786,    -1,   619,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   213,   649,    -1,   820,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   843,
      -1,    -1,    -1,    -1,    -1,    -1,   850,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   860,   861,   862,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   701,    -1,    -1,    -1,
      -1,    -1,   876,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   281,    -1,    -1,    -1,    -1,   890,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   898,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   738,    -1,   740,   741,   742,   152,    -1,
      -1,    -1,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   325,   326,   327,   328,   329,
     174,    -1,   332,   333,    -1,    -1,    -1,    -1,   338,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   346,    -1,   348,    -1,
      -1,   786,    -1,    -1,    -1,   355,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   365,    -1,    -1,   368,    -1,
      -1,    -1,    -1,    -1,    44,    -1,    -1,   377,    -1,    49,
      50,    51,    52,    -1,    -1,   820,    56,    57,    58,    59,
      60,    61,   392,    63,    64,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    74,    75,    -1,    -1,   843,   409,
      -1,    -1,    -1,    -1,    -1,   850,    -1,    -1,    -1,   419,
      -1,    -1,    -1,    -1,    94,   860,   861,   862,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   435,    -1,    -1,    -1,    -1,
      -1,   876,    -1,    -1,    -1,    -1,   116,    -1,   448,    -1,
      -1,    -1,   452,   123,   124,   890,    -1,   127,    -1,    -1,
      -1,    -1,    -1,   898,    -1,    -1,    -1,    -1,    -1,    -1,
     470,   141,    -1,   143,   144,   145,   146,   147,   148,   149,
     150,   151,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   340,   341,   342,    -1,
     500,    -1,   172,    -1,    -1,    -1,    -1,    -1,   352,    -1,
      -1,    -1,    -1,    -1,    -1,   515,    -1,    -1,    -1,    -1,
      -1,    -1,   522,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   379,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   213,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   556,    -1,   558,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   574,   575,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   591,    -1,   437,   438,   439,   596,    -1,   598,   599,
      -1,    -1,    -1,   447,    -1,   449,    -1,    -1,    -1,    -1,
      -1,   281,   456,    -1,    -1,    -1,   460,   461,   462,   619,
      -1,    -1,   622,    -1,    -1,   469,    -1,    -1,   472,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   325,   326,   327,   328,   329,
      -1,    -1,   332,   333,    -1,    -1,    -1,   667,   338,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   346,    -1,   348,   679,
      -1,   681,   682,   152,    -1,   355,    -1,    -1,    -1,   158,
     159,    -1,    -1,    -1,    -1,   365,    -1,    -1,   368,    -1,
      -1,    -1,    -1,    -1,    -1,   174,    -1,   377,    -1,   553,
      -1,    -1,    -1,    -1,   714,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   392,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   579,    -1,    -1,   738,   409,
     740,   741,   742,    -1,   744,    -1,    -1,    -1,    -1,   419,
      -1,    -1,    -1,    -1,    -1,    -1,   756,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   435,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   448,    -1,
      -1,    -1,   452,    -1,    -1,    -1,   630,    -1,    -1,    -1,
     790,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     470,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   815,    -1,    -1,    -1,    -1,
     820,    -1,    -1,    -1,    -1,    -1,   826,   152,    -1,    -1,
     500,    -1,    -1,   158,   159,    -1,   680,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   515,    -1,    -1,    -1,   174,
      -1,    -1,   522,    -1,    -1,   855,    -1,    -1,    -1,    -1,
     860,   861,   862,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   340,   341,   342,    -1,    -1,   876,    -1,    -1,    -1,
      -1,    -1,    -1,   352,   884,    -1,   556,    -1,   558,    -1,
     890,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   574,   575,    -1,    -1,   752,    -1,
     379,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   591,    -1,    -1,    -1,    -1,   596,    -1,   598,   599,
      -1,    -1,    -1,    -1,   778,   779,    -1,    -1,   152,    -1,
      -1,    -1,    -1,    -1,   158,   159,    -1,    -1,    -1,   619,
      -1,    -1,   622,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     174,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   437,   438,
     439,    -1,    -1,   817,    -1,    -1,    -1,    -1,   447,    -1,
     449,    -1,    -1,    -1,   828,    -1,   830,   456,   832,   833,
      -1,   460,   461,   462,    -1,    -1,    -1,   667,    -1,    -1,
     469,    -1,    -1,   472,   848,    -1,    -1,    -1,    -1,   679,
      -1,   681,   682,    -1,    -1,   340,   341,   342,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   352,    -1,    -1,
      -1,   875,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   883,
      -1,    -1,    -1,    -1,   714,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   379,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   907,    -1,    -1,    -1,    -1,   738,   913,
     740,   741,   742,    -1,   744,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   927,   553,   152,   756,    -1,    -1,    -1,
      -1,   158,   159,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   174,    -1,    -1,
     579,    -1,   437,   438,   439,    -1,    -1,    -1,    -1,    -1,
     790,    -1,   447,    -1,   449,    -1,   340,   341,   342,    -1,
      -1,   456,    -1,    -1,    -1,   460,   461,   462,   352,    -1,
      -1,    -1,    -1,    -1,   469,   815,    -1,   472,    -1,    -1,
     820,    -1,    -1,    -1,    -1,    -1,   826,    -1,    -1,    -1,
      -1,   630,    -1,    -1,    -1,   379,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   855,    -1,    -1,    -1,    -1,
     860,   861,   862,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   876,    -1,    -1,   152,
      -1,   680,    -1,    -1,   884,   158,   159,    -1,    -1,    -1,
     890,    -1,    -1,   437,   438,   439,    -1,    -1,   553,    -1,
      -1,   174,    -1,   447,    -1,   449,    -1,    -1,    -1,    -1,
      -1,    -1,   456,    -1,    -1,    -1,   460,   461,   462,    -1,
      -1,    -1,    -1,    -1,   579,   469,    -1,    -1,   472,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   340,   341,   342,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   752,    -1,   352,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   630,    -1,    -1,    -1,   778,
     779,    -1,   379,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   553,
      -1,    -1,    -1,    -1,    -1,   152,    -1,    -1,   817,    -1,
      -1,   158,   159,    -1,    -1,   680,    -1,    -1,    -1,   828,
      -1,   830,    -1,   832,   833,   579,    -1,   174,    -1,    -1,
     437,   438,   439,    -1,    -1,    -1,    -1,    -1,    -1,   848,
     447,    -1,   449,    -1,    -1,    -1,    -1,    -1,    -1,   456,
      -1,    -1,    -1,   460,   461,   462,    -1,   340,   341,   342,
      -1,    -1,   469,    -1,    -1,   472,   875,    -1,    -1,   352,
      -1,    -1,    -1,    -1,   883,    -1,   630,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   752,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   379,    -1,   907,    -1,
      -1,    -1,    -1,    -1,   913,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   778,   779,    -1,    -1,    -1,   927,    -1,
      -1,    -1,    -1,    -1,   789,    -1,   680,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   553,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   437,   438,   439,    -1,    -1,    -1,
      -1,    -1,    -1,   828,   447,   830,   449,   832,   833,    -1,
      -1,    -1,   579,   456,    -1,    -1,    -1,   460,   461,   462,
      -1,    -1,    -1,   848,    -1,    -1,   469,    -1,    -1,   472,
      -1,    -1,    -1,   340,   341,   342,    -1,    -1,   752,    -1,
      -1,    -1,    -1,    -1,    -1,   352,    -1,    -1,    -1,    -1,
     875,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   883,    -1,
      -1,    -1,    -1,   630,   778,   779,    -1,    -1,    -1,    -1,
      -1,    -1,   379,    -1,    -1,   789,    -1,    -1,    -1,    -1,
      -1,    -1,   907,    -1,    -1,    -1,    -1,    -1,   913,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   927,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     553,    -1,    -1,   680,   828,    -1,   830,   152,   832,   833,
      -1,    -1,    -1,   158,   159,    -1,    -1,    -1,    -1,    -1,
     437,   438,   439,    -1,   848,    -1,   579,    -1,    -1,   174,
     447,    -1,   449,    -1,    -1,    -1,    -1,    -1,    -1,   456,
      -1,    -1,    -1,   460,   461,   462,    -1,    -1,    -1,    -1,
      -1,   875,   469,    -1,    -1,   472,    -1,    -1,    -1,   883,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   752,    -1,   630,   152,    -1,
      -1,    -1,    -1,   907,   158,   159,    -1,    -1,    -1,   913,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     174,   778,   779,   927,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,   680,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   553,    -1,    -1,    -1,
     817,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   828,    -1,   830,    -1,   832,   833,    -1,    -1,    -1,
      -1,    -1,   579,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   848,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   340,   341,   342,   875,   752,
      -1,    -1,    -1,    -1,    -1,    -1,   883,   352,    -1,    -1,
      -1,    -1,    -1,   630,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   778,   779,    -1,    -1,    -1,
     907,    -1,    -1,    -1,   379,    -1,   913,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     927,    -1,    -1,    -1,   671,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   680,    -1,    -1,   340,   341,   342,    -1,
      -1,    -1,    -1,    -1,    -1,   828,    -1,   830,   352,   832,
     833,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   437,   438,   439,   848,    -1,    -1,    -1,    -1,
      -1,    -1,   447,    -1,   449,   379,    -1,    -1,    -1,    -1,
      -1,   456,    -1,    -1,    -1,   460,   461,   462,    -1,    -1,
      -1,    -1,   875,    -1,   469,    -1,    -1,   472,    -1,    -1,
     883,    -1,    -1,    -1,    -1,   752,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,   907,    -1,    -1,    -1,    -1,    -1,
     913,   778,   779,   437,   438,   439,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   447,   927,   449,    -1,    -1,    -1,    -1,
      -1,    -1,   456,    -1,    -1,    -1,   460,   461,   462,    -1,
      -1,    -1,    -1,    -1,    -1,   469,    -1,    -1,   472,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   553,    -1,
      -1,    -1,    -1,   830,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   848,    -1,    -1,   579,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   869,    -1,    -1,    -1,    -1,    -1,   875,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   883,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   553,
      -1,    -1,    -1,    -1,    -1,   630,    -1,    -1,    -1,    -1,
     907,    -1,    -1,    -1,    -1,    -1,   913,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   579,    -1,    -1,    -1,    -1,
     927,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,   680,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   630,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   680,   752,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   778,   779,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   752,    -1,
      -1,    -1,    -1,    -1,    -1,   830,    -1,   832,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   848,   778,   779,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
     875,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   883,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,   830,    -1,    -1,    -1,
      -1,    -1,   907,    -1,    -1,    -1,    -1,    -1,   913,    -1,
      -1,    -1,    -1,    -1,   848,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,   927,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,   875,    -1,    -1,    -1,    -1,    -1,    -1,    -1,   883,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   907,    -1,    -1,    -1,    -1,    -1,   913,
      -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    -1,
      -1,    -1,    -1,   927
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_int16 yystos[] =
{
       0,   930,   931,     0,   932,   933,   934,   937,   938,   384,
     386,   935,   936,   939,   940,   950,   951,   952,   565,   947,
     965,   936,   351,   617,   953,   956,   220,   820,   274,   974,
     975,   957,   954,   947,   947,   820,    36,   966,   967,   220,
     948,   154,   976,   977,   820,   820,   256,   941,   942,   943,
     250,   945,   409,  2179,   198,   969,   820,   187,  1137,  1139,
    1140,   713,   399,  1057,  1058,    19,   112,   170,   173,   175,
     196,   289,   459,   551,   555,   666,   705,   751,   753,   785,
     835,   913,   978,   979,   980,   981,   985,   999,  1005,  1006,
    1007,  1008,  1009,  1015,  1031,  1033,  1038,  1041,  1046,  1047,
    1049,  1050,  1051,  1052,  1053,  1054,  1055,   456,   618,   958,
     958,   944,   943,   946,   503,   756,   759,   760,   968,   691,
     271,   970,   220,   949,   824,  1141,   820,   713,   307,  1059,
       1,   913,  2055,  2055,   766,   738,  2207,  2179,  2179,  2179,
    2055,   738,   820,   820,  2179,   820,   820,   110,   155,  2160,
    1056,  1010,   980,     1,   820,  1008,   390,  1032,    37,   960,
     960,   456,   618,   959,   959,   489,  2191,  2179,   404,   972,
     820,   612,  1415,  1416,  1420,   713,  1142,   820,   820,  1060,
    1016,   337,  1044,  2170,  2179,  2179,   913,  2048,  2102,   135,
    2048,  2179,  2179,   986,  1000,  2048,   982,   913,  1039,  1040,
    1203,   913,  1034,  1035,  1036,  2056,  2179,   456,   558,   561,
    1011,  1013,  1014,  1814,  2108,  2179,   913,   456,   820,   955,
     820,   820,  2179,   119,   769,   822,   971,   692,   820,   220,
    1445,  1446,   820,   304,   708,  1138,  1143,  1144,  1146,   417,
     717,  1061,  1120,  1121,  1017,  1020,  1021,  2170,  2179,    23,
     500,  2048,   456,   390,   557,  2231,   456,   843,   109,   481,
     616,   716,   913,   987,   988,   989,   990,   991,   995,   996,
     998,  2159,  2202,   350,   616,  1001,  1002,  1003,   983,   998,
    1040,  2179,  1035,    31,   409,  2056,  2181,  2055,  1814,   409,
     766,  2198,  2179,   170,  2055,   961,  2179,    48,   505,   506,
     507,   621,   836,   837,   849,  2141,  2179,  1417,     1,     5,
      10,    17,    26,    87,    88,   118,   138,   142,   160,   201,
     207,   209,   213,   217,   234,   270,   286,   296,   297,   345,
     353,   356,   361,   387,   395,   397,   400,   403,   418,   484,
     490,   492,   495,   511,   563,   586,   626,   632,   636,   639,
     640,   656,   672,   677,   684,   690,   709,   723,   729,   747,
     761,   771,   772,   780,   783,   798,   820,   845,   858,   864,
     888,   917,   922,  1447,  1448,  1477,  1482,  1487,  1492,  1518,
    1522,  1530,  1534,  1535,  1539,  1542,  1547,  1552,  1604,  1608,
    1610,  1615,  1631,  1639,  1643,  1646,  1649,  1653,  1654,  1661,
    1671,  1674,  1677,  1695,  1704,  1708,  1710,  1714,  1717,  1721,
    1735,  1749,  1751,  1755,  1770,  1771,  1781,  1784,  1785,  1789,
    1795,  1796,  1804,  1811,  1828,  1838,  1847,  1853,  1864,  1868,
    1870,  1873,  1876,  1879,  1890,  1909,  1917,  1945,  1447,   915,
    1193,  1195,  1196,     1,   913,  2036,   820,   532,   564,  2138,
      30,   494,   669,   701,  1122,  1123,  1124,  1125,  1127,  1128,
    1129,  1134,  1018,  1019,    23,   500,   379,   456,   472,   630,
     752,   830,   927,  1042,  1043,  2112,   912,  1048,  2222,  2102,
     725,   744,  2210,  2179,   820,   989,   820,   913,   988,   113,
     122,   992,  2161,    16,   352,   913,  1004,   913,     1,   820,
    1003,   984,  2222,    16,   379,   456,   472,   630,   752,   830,
     927,  2113,  2114,  2115,   456,  1037,  2109,  2198,   409,  2055,
    2055,  1012,  1013,   820,   139,   301,   648,   826,   962,   963,
     964,   505,   506,   621,   849,   973,    85,   488,   577,   764,
     769,   822,  1499,  1500,     5,    10,    17,    26,    87,    88,
     118,   138,   142,   160,   201,   213,   217,   236,   241,   242,
     243,   244,   246,   247,   248,   249,   251,   253,   254,   255,
     257,   258,   259,   260,   261,   262,   263,   264,   265,   266,
     267,   270,   286,   296,   297,   345,   353,   356,   361,   387,
     395,   397,   400,   403,   484,   490,   492,   495,   511,   563,
     586,   636,   656,   677,   684,   690,   709,   729,   747,   761,
     771,   772,   780,   783,   798,   820,   845,   858,   864,   917,
     922,  2145,  2146,  2147,  1449,  1478,  1483,  1488,  1493,  1519,
    1523,  1531,  1536,  1543,  1540,  1548,  1553,  1605,  1609,   337,
    1611,  1616,  1632,  1640,  1644,  1647,  1650,   354,   678,  1510,
    1642,  1655,  1662,  1672,  1675,  1678,   353,   584,  1709,  1711,
    1715,  1718,   724,  1722,  1736,  1750,  1752,  1756,  1772,  1782,
    1786,  1790,  1797,  1805,  1812,  1829,  1839,   456,   630,   699,
     752,   805,   927,  1851,  1852,  2004,  2096,  2097,  2102,  1854,
    1865,   610,  1869,  1871,  1434,  1874,  1877,  1880,  1891,  1910,
     353,   584,   713,   140,  1174,   233,   730,   743,  1197,  1198,
    1200,  1211,  1213,  1215,  2130,   820,  1145,   564,  2055,   138,
     167,   184,   185,   300,   465,   476,   605,   643,   660,   918,
     919,  1130,   561,  1135,  2196,   641,   747,   748,  1126,     1,
     820,  1124,  2179,  2179,   390,  1043,  1045,   807,   592,  2179,
    2108,   820,  2179,   727,   406,   960,     1,   352,   406,   960,
     820,   820,   195,  2115,   816,  2109,  2055,   964,  2202,   139,
     912,  1496,   560,   913,  1450,  1453,  1454,  1455,  2037,  2096,
      11,   152,   158,   159,   168,   174,   340,   341,   342,   352,
     437,   438,   439,   447,   449,   460,   461,   462,   469,   488,
     553,   579,   680,   778,   779,   793,   848,   875,   883,   907,
    1479,  2028,  2060,  2061,  2063,  2064,  2096,  2113,  2117,  2118,
    2119,  2120,  2239,   828,   832,   833,  1484,  2023,  2024,  2025,
    2026,  2027,  2028,  2064,  2067,  2096,  2114,  2117,   456,  1489,
    1490,  2044,  2045,  2046,  2102,  1494,  1499,   456,   618,  1520,
    1521,  2080,  2096,   911,  1524,  1525,  1527,  2036,    11,  1532,
    2028,  2029,  2030,  2058,  2099,  2100,  2102,  2114,    14,  1537,
     824,  1544,  2036,    16,  1541,  2096,  2098,   398,   416,   571,
     797,  1549,  1551,   281,   334,   391,   456,   485,   560,   756,
     781,   826,   911,  1554,  1555,  1556,  1557,  1559,  1567,  1569,
    1570,  1571,  1574,  1579,  1582,  1583,  1586,  1588,  2060,  2080,
    2096,  1606,  2061,  1549,   356,  1499,  1613,   823,   834,  1617,
    1618,  1619,  2008,  2009,  2010,   108,  1633,  1635,   350,   582,
     586,   616,   713,  1641,  1645,  2057,  2058,  1648,  2102,   815,
    1651,  2218,  2061,     1,  2007,  2008,  1663,  2057,   913,  1673,
    2038,   161,   911,  1356,  1591,  1676,  2096,  1679,  1680,  2096,
    2113,  2117,  1696,  1705,  1830,  2089,  2090,  2102,  1356,  1591,
    1712,   168,  1716,  2061,  1719,  2061,   295,  1723,  1724,  1725,
     390,  1737,  2002,  2172,   913,  2037,   291,  1753,  2096,  1757,
    2036,  1773,  2037,  1783,  2031,  2102,  1787,  2036,   824,  1791,
    2031,  2032,    16,  1798,  2033,  2102,  1806,  2037,   274,   427,
     488,  1813,  1816,  1817,  1820,  1821,  1822,  1823,  1824,  1825,
    1826,  1827,  2004,  2039,  2040,  2057,  2088,  2090,  2102,  1830,
    1840,  2036,  1848,  2096,   832,  2106,  2107,  1855,  1856,  1857,
     168,   793,  1866,  2060,  1872,  2038,   820,   913,  1435,  1436,
    1439,  1440,  1445,  1875,  2091,  2096,  1878,  2036,   456,  1881,
    2081,  2096,  2117,  1892,  2096,  1911,  2032,  1918,  1946,   820,
     713,   463,  1282,  1203,  1203,  1203,  1198,   820,     1,   316,
    1201,  1202,  1203,   442,  1147,  1062,  2196,  2218,  2218,   557,
    2195,  2196,  2196,  2218,  2196,  2196,  2218,  2196,  2196,   824,
    2169,   290,  2168,   456,  1074,  2102,    32,  2151,   820,    39,
     229,   379,   456,   459,   472,   503,   630,   752,   757,   758,
     927,  1022,  1024,  1025,  1026,  1029,   853,   885,   886,  1023,
    1024,   913,  1087,  2112,   456,  2108,   459,   787,   882,   997,
    2051,  2103,  2104,   993,   489,  2115,  1497,   107,   884,  1421,
     241,  1476,  1451,    41,    55,   347,   489,  1460,  1461,  1462,
    1466,  1469,  2154,  2155,  2222,   832,  1457,   485,  2190,   346,
    2195,  2096,   832,   832,   832,  2122,   832,  2231,  2231,   832,
     832,   832,   832,  2231,  2089,   832,  2122,   242,  1481,   815,
    1480,  2061,  2097,  2114,  2117,   832,  2121,   832,   832,  2027,
    2096,  2023,  2027,   110,   828,   833,   819,   829,   298,  2097,
    2114,  2117,   396,  2133,  1490,   815,  2231,   243,  1517,  2002,
    1521,  1528,  2036,   650,   857,  1526,  2222,  2237,  2195,   244,
    1533,   280,   821,  2030,  2228,   691,  2139,  2106,  2107,  1538,
    1545,  2036,   246,  1546,   641,  2204,   162,  2163,  2096,   797,
    2215,   797,  2037,  1568,   362,  1589,    78,  2157,   247,  1603,
     291,   543,  1960,  1962,  1964,  1557,  2059,  2060,  1558,   912,
    1560,  1457,  1580,  1589,   870,   871,   872,   873,   248,  1607,
      83,   405,   815,   456,   249,  1630,    25,   906,  1620,  1621,
    1622,  1624,    28,   298,   409,   532,   566,   817,   819,   828,
     829,   832,   833,  2011,  2012,  2014,  2061,  2179,   497,  1636,
     183,  1642,  2058,   270,  2043,   804,  1657,  1664,  2222,  2038,
    2096,     8,    15,    45,    66,    67,    68,    69,    70,    71,
      72,    73,    81,    86,    97,    98,    99,   100,   115,   125,
     128,   129,   130,   132,   166,   175,   176,   177,   178,   179,
     180,   181,   182,   188,   189,   214,   215,   218,   219,   223,
     245,   269,   273,   288,   293,   294,   311,   312,   313,   314,
     315,   318,   357,   358,   359,   360,   366,   367,   369,   371,
     372,   373,   376,   378,   382,   401,   402,   410,   411,   412,
     413,   414,   415,   424,   428,   429,   432,   471,   475,   477,
     478,   479,   480,   487,   504,   512,   547,   548,   580,   583,
     594,   600,   604,   606,   607,   620,   623,   624,   628,   642,
     644,   645,   653,   654,   673,   674,   675,   693,   694,   695,
     696,   698,   702,   703,   710,   711,   718,   719,   720,   726,
     731,   749,   762,   763,   768,   791,   792,   800,   808,   814,
     844,   850,   891,   897,   899,   902,   909,   921,   924,  1361,
    1363,  1365,  1367,  1369,  1371,  1373,  1375,  1378,  1380,  1382,
    1383,  1385,  1387,  1388,  1390,  1392,  1395,  1396,   430,   744,
     813,  1397,  1398,   164,   662,   794,  1681,  1682,  1684,  1685,
    1697,  2096,  1706,  2096,  1832,   832,  2106,   532,   774,  1358,
    1359,  1360,  1361,  2137,  1397,   253,  1713,  2061,   815,   254,
    1720,    83,  1724,   299,   398,   416,   571,  1726,    84,   339,
     456,  1205,  1742,  1743,  1744,  2044,  2064,  2082,  2096,  2102,
    2113,  2117,  2222,   805,   913,  1754,   257,  1769,   511,   603,
    2136,   258,  1780,   485,   715,  1774,   346,  1912,   259,  1788,
    2204,   913,   260,  1794,  1912,  2033,   261,  1803,   894,  1799,
     346,  1807,  1808,  2074,  2078,  2096,  2114,  2117,   291,  1823,
    1825,  2057,   611,   815,  2040,   226,   815,   866,  1815,    42,
    2106,   262,  1846,   319,   421,   427,  1842,  1510,  1849,  2061,
    2222,  2021,  2023,   832,  2107,   263,  1863,   405,  1858,  1859,
    2061,  2096,  2089,   264,  1867,   346,  2038,   713,   820,   820,
     346,   641,   647,  2205,   265,  1889,   202,  1882,  2096,   266,
    1916,  1912,  1919,  2096,  1947,  2096,  1194,   820,   713,   454,
    1284,  1214,  1216,   659,   820,   820,  1199,   155,    76,   121,
     187,   423,   446,   641,   646,   663,   665,   820,   890,  1148,
    1149,  1151,  1155,  1156,  1159,  1160,  1166,  1169,  1171,  1172,
    2179,     1,  1063,  1064,  2047,  2048,  2048,  2108,  2108,  2034,
    2036,  2034,  2108,  2034,  2034,  2048,  2034,  2034,   796,  2214,
     240,  1136,  2108,  2170,  2051,  1026,    25,   807,   110,   491,
     914,  2235,   913,  2105,   994,  2170,  2179,    85,   577,   769,
    1498,  1423,  1422,   678,  1431,  1460,   927,  2083,  2088,  2108,
    2179,  1462,   123,   127,   448,   598,   599,  1467,  1468,  2227,
     810,    44,    49,    50,    51,    52,    56,    75,   124,   161,
     163,   175,   198,   338,   348,   377,   435,   467,   470,   515,
     522,   558,   574,   622,   625,   667,   679,   681,   682,   701,
     706,   714,   756,   790,   811,   855,   867,   874,  1470,  1473,
    1474,  1475,  2164,  2203,   137,   456,  1458,  1459,  2069,  2096,
    2096,   169,    34,    35,   126,   133,   136,   190,   192,   193,
     274,   276,   283,   291,   398,   448,   452,   704,   788,   799,
     810,   881,   913,  1465,  2040,  2225,   270,   305,   306,   616,
    2072,  2097,   815,  2021,  2023,  2128,  2021,  2129,   817,  2021,
    2124,  2125,   913,   913,  2023,  2127,  2127,  2127,  2066,  2096,
    2114,  2117,  2126,   913,   815,  2066,  2123,    11,  2028,  2029,
    2061,  2102,  2114,   354,  2023,  2066,  2021,   817,   396,  2134,
    2024,  2024,  2025,  2025,  2025,   458,  1485,   614,  1491,  2046,
    1501,  1502,  2081,  2096,  1526,   464,   515,  2170,  2097,  2023,
     489,  2140,  2107,  2023,  2036,   676,  1762,  1763,  1764,  1550,
    2222,  1560,   911,  2060,  1442,  1443,  1442,  1963,  1964,  1961,
    1962,   869,  1466,  1469,  1562,  1563,  1565,  2222,    49,    50,
      51,    52,    56,    74,    75,   124,   163,   281,   338,   377,
     470,   574,   701,   706,   744,   756,   855,  1474,  1561,  1602,
     281,  1572,  1573,  2096,  2114,   869,  1590,   911,  1960,  1960,
    1960,  1960,  2061,  2029,  2061,  1612,  1496,  1619,  1625,   906,
    1622,  1623,   249,   820,   906,  1442,  2013,  2014,  2012,    20,
      21,    22,   114,   280,   363,   364,   440,   441,   508,   542,
     551,   560,   601,   821,   825,   827,  2015,  2016,  2017,  2018,
    2019,  2020,  1634,   456,  2041,  2042,   204,  1652,  2044,   236,
    1656,  1658,    16,    20,    23,    24,   500,   501,   551,   552,
    1665,  1669,   316,   390,   821,  2173,  1394,  2108,  2173,  1396,
    2173,  2173,  2173,  1398,  2076,  2097,  2101,  2114,    16,   110,
     319,   431,   843,  1688,  1689,  1690,  1683,  1684,   252,  1703,
     346,  1703,   405,  1834,  2196,  2222,   409,   821,  2180,  1359,
      31,   409,   821,  2182,     3,    12,    27,    46,    65,    77,
      78,    79,    82,    89,   101,   103,   104,   131,   186,   199,
     221,   222,   224,   225,   268,   284,   322,   323,   324,   344,
     349,   374,   375,   380,   381,   434,   436,   453,   466,   467,
     468,   493,   517,   518,   519,   521,   523,   524,   526,   528,
     529,   533,   535,   536,   537,   538,   551,   559,   572,   573,
     581,   587,   633,   638,   687,   688,   689,   697,   721,   734,
     735,   736,   737,   754,   755,   765,   795,   809,   810,   846,
     856,   863,   874,   878,   879,   880,   895,   896,   900,   901,
     903,   910,  1362,  1364,  1366,  1368,  1370,  1372,  1374,  1376,
    1377,  1379,  1381,  1384,  1386,  1389,  1391,   815,  2057,  2029,
    2061,   733,  1727,  2195,   368,  2003,  2003,   865,   894,   807,
       1,    58,    59,    60,    61,    63,   144,   145,   325,   326,
     327,   328,   329,   330,   331,   332,   333,   456,   596,   619,
    1206,  1207,  1208,  1209,  1210,  1246,  2070,  2097,   812,   801,
    2204,   405,  2087,  2096,  2113,  2117,   405,  1758,  1762,   240,
    1800,  2096,  1800,  2096,  1809,  2222,   815,   815,   815,   815,
    1814,   270,   744,   823,   834,  2061,    83,    56,    75,   377,
     435,   470,   574,   681,   855,  1818,  1819,  2179,  1841,  2222,
    2061,   282,   531,   137,   722,   817,  2022,   818,  2023,  2096,
    1859,   202,  1860,   346,   346,  2029,  2061,  1437,  2074,    83,
    2158,   405,  1885,    14,    55,  1913,  1914,   267,  1944,   346,
    1944,  1949,  2222,  1195,  1175,   820,   713,   663,  1286,   890,
    1274,  1233,  1234,  2179,  2130,    29,    54,    57,    58,    59,
      60,    61,    63,    64,    74,   141,   143,   144,   146,   147,
     148,   149,   150,   151,   213,   325,   326,   327,   328,   329,
     333,   368,   392,   419,   443,   500,   556,   575,   591,   596,
     619,   649,   701,   738,   740,   741,   742,   786,   850,   860,
     861,   862,   876,   898,  1221,  1222,  1223,  1224,  1225,  1228,
    1229,  1231,  1234,  1235,  1238,  1239,  1243,  1244,  1245,  1246,
    1249,  1252,  1269,  1270,  1272,  1273,  1274,  1279,  1280,  1281,
    2179,  2209,  1204,  2179,   156,  2162,  2179,   641,   647,  2236,
    2236,  2179,  2162,  2179,  2191,  2179,    31,  2150,   557,  1173,
    2055,   301,   355,   820,     6,     9,    27,    40,   309,   310,
     393,   448,   527,   568,   576,   641,   655,   670,   733,   747,
     820,   824,   840,   841,  1065,  1066,  1075,  1077,  1082,  1083,
    1086,  1088,  1089,  1090,  1091,  1097,  1098,  1099,  1100,  1103,
    1109,  1110,  1112,  1114,  1116,  1117,  1118,  1119,  2161,  2204,
    2230,  2048,  2196,  2196,   838,   839,  2240,  2036,  2196,  2196,
    2162,  2195,  2195,   647,  2034,  1027,  1029,  2231,    23,   500,
    2051,   454,  1424,  1425,  1426,  2158,  1424,   560,   913,   820,
     283,   291,   541,   543,  1953,  1955,  1956,  1958,  1959,    76,
     549,  2193,  2193,   456,  2084,  2088,  2110,  2179,  2179,  2179,
    2179,    56,   231,  1475,  2159,   226,   866,  2219,    14,  2149,
     421,   744,  1458,   137,   828,   833,  1953,   926,   925,  2074,
    1953,   421,  2184,   766,   766,   549,  1452,   496,  2195,  2195,
     456,  2073,  2097,  2096,  2022,   817,  2022,   817,   817,   590,
     817,  2066,  2022,   817,   817,   817,  2022,   817,  2089,  2022,
     817,  2195,   546,   745,  1975,  1977,  1979,  2106,  2107,  2029,
     818,   817,   817,   815,  2135,  1485,  2108,   678,  1486,   815,
    2044,  1495,   509,   618,  1503,    37,  1529,  2222,   683,   658,
    1975,  2179,   712,   339,  2170,   408,   544,  1996,  1997,  1999,
    2001,   421,  1584,  1575,  1444,   170,   171,   608,   913,  1566,
    2040,  1563,   516,  1602,  2180,   448,   704,  2179,   277,   279,
    1399,  1400,  2166,  2218,  2180,   226,  2219,  2179,  1602,  1573,
    2096,  1581,  1587,   354,  1975,   354,   456,  1614,   884,  1504,
       1,    29,   823,   834,  1627,  1628,  2009,   569,  1626,   927,
    2015,  2218,  1456,  1457,  1652,  2042,  2196,  1659,   251,  1660,
    1442,  2218,   662,  1666,  2218,  2096,  2096,  2096,  2096,  2096,
     815,    83,  1689,  1691,  2076,    16,   110,   431,   843,  1686,
    1687,  2074,  2092,  2096,  2096,  2096,  1083,  1835,  2161,    38,
     205,  1264,   227,  1393,  2096,  2108,  2060,  2057,  1975,   354,
    2222,  1762,  1206,  2172,  1743,  1738,  1739,   297,  1745,  2007,
    1746,  1747,  2096,  2044,   820,  1914,  1758,  2096,  2096,   240,
     539,  1985,  1988,  1990,  1792,  1793,  2222,  1442,   906,   906,
    1801,  1802,  1913,   232,   237,   285,  2096,  2074,   558,   927,
    2085,  2086,  2088,  2108,  2073,   557,  2061,  1814,  1814,  1814,
    1814,  1814,  1814,  1814,  1814,  1819,   532,   542,  1843,  1844,
    1845,  2016,  2137,  1996,   437,   744,  2238,   766,  2212,  2212,
    2023,   817,  2023,  1862,  2222,  2158,  2096,  2089,  1975,   354,
    1441,  2108,   815,    16,  1883,  1884,  2131,  1886,  2096,  1862,
    1886,  1762,    13,  2148,  2096,   678,  1950,   239,  1176,  1283,
     820,   713,   704,  1343,  2181,   890,   355,  2102,   437,   551,
     739,   859,  2208,   859,  2208,   859,  2208,   859,  2208,   859,
    2208,   906,  2220,  2195,   687,  2206,  2094,  2097,   228,  1255,
    2108,   459,  1240,  2097,    37,  2153,   409,   434,   687,  1271,
    2218,  2179,  1223,   301,   302,   385,   753,   851,   431,   843,
     346,  1217,  2153,   355,  2108,  1087,  2181,  2181,   913,  2052,
    2053,   560,   756,  2232,   456,  2048,  2054,  2108,   894,  2179,
     308,   384,   913,  1157,  2055,  2191,  2184,  2204,  2218,   728,
    2184,  2179,  2159,   203,  2184,  2184,   515,  1111,  2108,  2222,
    2179,  2179,   766,   464,   727,    57,  2156,  2181,  2034,  2034,
    2196,  2034,  2034,  1131,  1132,  2036,  2237,  2036,  1028,  1029,
    2105,  2179,  2179,  2051,  1425,   744,   859,  1427,  1428,   651,
     890,  1418,  1957,  1958,  1471,  1472,  2092,  2096,  1954,  1955,
    1442,  2083,  2083,  2083,  2083,  2084,  2083,  2179,  2005,  2086,
    2005,  2084,  2172,  2179,   817,   817,  1458,  2069,  2069,  1953,
     744,  1463,  1464,  1466,  2036,  2036,  2139,   789,  2121,   789,
    2121,   817,  2048,  2121,  2121,  2121,  2066,  2139,   431,   843,
    2121,  2097,  1442,  1442,  1978,  1979,  1976,  1977,  2107,  1975,
     817,  2023,  2121,  2121,  2079,  2096,  2113,  1486,  2058,  2003,
     515,  2141,  2023,  1442,  1442,  2000,  2001,  1998,  1999,  2080,
    1590,   198,   383,   813,   850,  1510,  1576,  1577,  1578,  1445,
    2083,  2083,   448,   704,   240,  2083,  2005,  2005,  2083,    77,
      79,   368,   452,   597,   732,   835,  1466,  1592,  1593,  1594,
    1595,  1596,  1598,  1599,  1600,  1601,  2222,  1592,  2029,  2030,
    2029,  2030,  1505,    25,   807,  1629,   249,   820,  1442,  1627,
     281,  2167,  2096,  1442,   236,   890,  1667,  1668,  1669,   804,
    1670,  2216,   890,  2077,  2101,  2113,  2074,    83,    14,    55,
    1692,  1693,  1694,  1687,  1692,   337,   169,  2006,  1707,  2222,
    1831,   727,  2184,   390,   567,  2174,  2029,    16,   515,   636,
    1113,  2034,  2096,  1442,   255,   820,  1741,    14,   346,    13,
     389,  1759,  1760,  1761,  1763,  1766,  1793,  2222,   187,   520,
    1775,  1777,  1779,  1442,  1442,  1989,  1990,  1988,  1996,   464,
     515,  2008,  2007,  1801,   662,  1810,  2061,  2016,  2096,  2017,
    2018,  2019,  2020,  2023,  1850,  2061,  1850,   817,   545,   831,
    1980,  1982,  1984,   596,   744,  1861,  2061,  2139,  2139,  2029,
     820,  2075,  2079,   566,  2074,   203,  1887,   794,  1888,  1792,
     578,  2040,  2083,  2006,   500,   889,  1951,  2078,    95,  1177,
    1178,  1195,  1285,   820,   713,    28,   298,   566,   817,   819,
     828,   829,   832,   833,  1205,  1220,  1276,  1277,  2179,   807,
    1212,   927,     7,    53,    65,   111,   134,   191,   272,   335,
     343,   423,   430,   455,   483,   627,   631,   707,   767,   781,
     805,   847,   893,   905,   911,   913,  1354,   832,  1226,  1227,
      90,  1257,   854,   815,  1253,  1241,  2179,  2094,   913,  2095,
       1,   913,  1245,    37,  1232,  2158,  1236,   773,  1230,   725,
    2142,  2142,   913,  1276,   815,  1154,   337,  1170,  2053,   452,
    2188,   815,  2160,  2172,   303,   320,   700,   852,   887,   892,
    1167,  1168,  2179,  2179,  2179,  2179,  2184,   228,   301,  1072,
    1073,  2179,   655,  1098,  2179,  2179,  2179,  2179,    32,    33,
    2152,  1113,  2054,  2108,  2179,  2191,   557,  1084,   728,  1115,
    2054,  2034,  1132,   599,  1133,    25,  2051,  2051,  2179,   744,
    1428,   564,  1430,   197,  1432,  1442,  2080,   448,   452,  2233,
    1472,  2085,   817,  2179,  1464,  1975,   346,  2143,   817,  1496,
     213,   712,   812,  1585,  2180,  2180,  2180,  2180,  2061,  1578,
    2195,   390,   409,  2183,  2083,  2151,  1593,   102,   434,   687,
    1597,   515,  1602,  1975,   657,  1975,   657,  1506,  1507,  1508,
    2158,  1628,  2008,  1637,  1638,  2096,  2113,  1659,  1668,   187,
    2165,  2218,  1692,  1692,  2077,   826,  2177,  2177,  1694,  1693,
    2172,  1698,   496,  1931,   208,   398,   884,  1836,  1833,   567,
    1975,   569,  2199,  2199,   562,    18,   212,   337,   433,   668,
     682,   912,  1728,  1729,  1733,  1734,  2222,  2223,   255,  1740,
    1747,  2061,  2196,   464,   421,  1767,  1765,  1766,  2222,   388,
     420,   904,  1442,  1442,  1778,  1779,  1776,  1777,   464,  1442,
    1442,   448,  2185,  1442,  1442,  1983,  1984,  1981,  1982,  2179,
    1975,  2143,  1975,   877,  1438,  1893,  1884,  2172,  2006,  2172,
    1980,   278,   540,  1915,  1991,  1993,  1995,  1997,   448,   452,
    2186,  1920,  2222,   615,  2055,  1195,  1195,  1287,   820,   807,
    1275,  1277,   511,   761,  1205,  1218,  1219,  1220,  2102,   200,
     321,   425,   482,   746,   842,  1247,   457,  1248,   456,  2071,
    2172,   346,  1254,   812,  2217,  2108,  2217,   744,   913,  1242,
     456,  2080,   170,   175,   289,   705,  1237,  2159,  2108,   110,
     647,  1150,  2047,  1161,  2108,  2210,   456,  1158,  2102,  1158,
     228,   634,   728,  1076,  2048,  2179,    91,    92,    93,   210,
     211,   213,   398,   399,   422,   448,   473,   571,   604,   608,
     609,   634,   796,  1067,  1068,  1069,  1070,  1071,  2048,  2054,
    1101,  2048,  2048,  2160,  2240,  2048,  2179,  2048,  1085,  2170,
    2179,  2054,   807,  2108,  1029,  2170,  2170,    44,   198,  2108,
    2179,   913,   820,  1419,  2085,   392,  2175,   206,  2144,  1504,
    1592,  2061,  2061,  2061,  2061,  2096,  2180,   813,   706,   916,
    2030,  2030,  1507,   560,  1427,   157,   651,   890,  1564,  1565,
    1638,  1442,    83,   198,  1692,  2061,  2061,  2096,  1931,  2195,
     784,  1700,   612,  2034,   354,   571,  1837,  2102,    16,   515,
     637,   868,   920,  1731,  1732,  1733,   515,    80,   464,   475,
    1730,    83,  1748,   464,  2179,  1768,  1986,  1988,  1990,  1996,
     464,   464,  2096,  2144,  1894,   820,  2096,  2096,  1442,  1442,
    1994,  1995,  1992,  1993,  1922,  1923,  1924,  2222,   824,  1952,
    2031,   612,  1179,  1288,  1344,  1205,   906,  1278,  2221,  2195,
    1220,   817,   913,  2108,   815,  1253,   204,   204,  1256,  2179,
     766,    77,   835,  1162,  1163,  1164,  1165,  2222,  2160,   346,
    1153,  2171,  2048,   346,    13,   884,   894,  2102,  2113,  2241,
      62,   451,   757,   913,  1102,   750,   821,  1104,  1105,  2048,
    2049,    47,   295,   474,  1092,  2179,    23,   500,  1087,   807,
    2054,   500,    23,    44,  1429,  2222,  1433,  1434,  2085,  2175,
    1975,  1509,  1510,  2096,  2180,  1975,  1975,  2062,  2063,  2093,
    2097,  2061,  1700,  1932,  1933,  2096,  1701,  1702,  2096,   291,
     543,  1970,  1972,  1974,  2179,  2034,   612,   570,  2200,   683,
    2064,  2068,  2071,  2096,  2117,   865,  2096,  1989,  1987,  1988,
    1975,    41,   230,   292,   355,  1895,  1896,  1897,  1899,  1903,
    1905,  1906,  1907,  1908,  2154,  2170,   498,  1926,  1924,    43,
     239,   923,  1925,   913,  2179,  1180,   635,  1289,  1200,  1215,
    1345,  1346,  1347,   729,   823,  2096,   396,  1258,  2196,  2196,
     393,  1259,  1261,  1262,  1263,  1264,  1266,  2108,  2054,  2054,
     336,  1154,  2108,  1104,  2102,  2169,  2102,  2179,   585,  1078,
    1079,  1106,  1107,   912,  1093,  1095,  2222,  1096,  2222,  1093,
    1087,  2179,  2179,  1087,  2054,  2179,  2179,   481,  1434,   815,
    2085,   291,   543,   831,  1512,  1514,  1516,    11,   405,   534,
     560,   830,  1511,  2178,  2061,  1699,  1933,  2179,  1702,  1442,
    1442,  1973,  1974,  1971,  1972,  1742,  2179,  2097,  2114,  2117,
    2007,    55,  2149,  1907,  1754,   616,   195,  2179,   496,  1928,
    2078,  1742,   820,  1181,  2170,  2038,     1,  1202,  1347,   820,
     815,  2179,  1259,  2048,  2048,  2158,  1266,  1262,  2184,  1260,
    2154,  2160,  2132,  2222,  1080,  2132,  1108,  2048,  1108,   464,
    2189,   690,   475,  1087,  1087,  2051,  2051,   744,   240,  2085,
    1442,  1442,  1442,  1515,  1516,  1513,  1514,  2195,  2096,  1970,
     560,  2113,  1742,   664,   756,  2211,   824,  2035,   240,   761,
    1904,  2196,  2078,  2195,   850,  1934,  1948,   571,  2177,  1290,
     820,  1348,  1349,   820,  1205,  1259,  1267,  1268,  2103,  2179,
    1261,  2054,   204,  1152,  1078,   227,   515,  2179,   561,  2170,
    2170,  2179,   197,  2096,  2096,   282,   291,  2229,  2036,   824,
      16,  1900,  1901,  2097,   499,  1927,  1929,  1930,  2096,  2195,
     784,  1939,   291,   543,  1965,  1967,  1969,   206,   282,   766,
     785,   802,  1186,  1187,  1188,   398,   416,     1,  1291,  1351,
     554,  1353,  1354,  1268,  2050,  2051,  2196,   783,  1081,   227,
    2048,   494,   641,   647,  2234,   500,    23,  2084,   820,   612,
    2201,  2036,   557,   613,   652,  1902,  1901,  2179,  1930,  2179,
    1935,  1936,  2096,   290,   908,  1940,  1941,  2096,  1921,  1442,
    1442,  1968,  1969,  1966,  1967,   169,   793,  2179,   421,   421,
     206,   437,  1188,   240,   485,   766,   785,   802,  1182,  1183,
    1184,  1185,  2096,  2190,  2213,   240,   485,   766,   802,  1189,
    1190,  1191,  1192,  2096,  2213,   820,   161,   162,   578,   820,
    1292,  1293,  1298,  2179,  2222,  2249,    44,    49,    50,    51,
      52,    56,    74,    75,    94,   116,   124,   172,   281,   338,
     346,   348,   365,   377,   435,   448,   452,   470,   515,   556,
     574,   622,   667,   714,   744,   756,   790,   815,   826,   855,
     884,  1239,  1244,  1249,  1269,  1272,  1274,  1352,  1413,  1414,
    1473,  1474,  2179,  2227,  1355,  2096,  2108,  1350,  2051,  2048,
     906,  2234,  1094,  1095,  2179,  2179,  2196,   557,  2094,  2078,
    2113,  1936,  2179,    42,   157,   235,   530,   551,  1938,  1942,
     379,   472,   752,   927,  1943,  2116,  1941,  1965,  2179,   556,
    2096,  2179,  2179,  2179,  2179,   421,   190,   810,   421,   437,
    1184,  2096,   169,   629,   750,   775,   776,   777,   421,   190,
     810,   421,   437,  1191,  2096,   797,  2181,  2181,   444,   445,
    2187,  1309,   355,   120,   317,  1294,  1295,  1296,  1297,  2096,
    2179,   448,   704,  1411,  2193,  1409,  2193,  2179,  2180,  1399,
    1400,  2179,  2087,  1409,  2180,  2108,   109,  2180,  2096,  2096,
     355,  1411,  1357,  1358,    16,   752,   927,  1030,  1094,  1087,
    1087,   299,   398,   416,   571,  1898,  2034,  1938,  1937,  1938,
    1937,   566,  2096,  2108,  2096,  2096,  2096,  2096,  2179,  2179,
    2179,  2179,  2179,  2096,  2179,  2179,  2179,  2179,  2179,  2179,
    2179,  2179,  2179,  2179,  2179,  2096,  2179,  2181,  2181,   927,
    1299,  1300,  1301,  2096,  2108,  2111,  1200,  1310,  2179,  1296,
    1297,  2083,  2179,  2179,  2083,  2092,  2108,  2083,  1406,  2083,
    2217,  2179,  2092,  2108,  1351,  2115,  2116,  2217,  2096,  2096,
    2096,  2096,  2096,  2096,  2096,  2096,  2096,  2096,  2096,  2096,
    2096,  2096,  2096,  2096,  2096,  2096,  2096,  1302,   448,   452,
    2186,  2225,  2233,     1,  1202,  1203,  2080,   486,   595,   828,
     833,  1402,  1403,  1404,  1405,  1412,  1402,  1404,  1410,    96,
     593,  1407,  1408,  2080,  1265,  1266,  2096,  2096,   319,   336,
     370,   427,   450,  1303,  1304,  1305,  1306,  1307,  1308,  1300,
    1301,   820,  1311,  2083,  2083,  2096,  2096,   194,   208,  2242,
    2179,  2179,   106,   161,  2242,  2243,  2179,  1312,  2096,  2179,
    1301,  1301,   370,  2179,  2179,  1301,     4,   123,   127,   366,
     448,   452,   513,   556,   602,   750,   782,   820,   850,   894,
    1239,  1243,  1244,  1249,  1250,  1269,  1272,  1274,  1313,  1314,
    1319,  1322,  1325,  1326,  1329,  1330,  1331,  1334,  1335,  1341,
    1342,  2224,  2225,  2226,  2096,  1301,  1301,  1301,   394,  2176,
     550,  2193,  2194,  2150,  2179,  2108,  2179,  2195,  2179,  2096,
      14,    55,   419,   906,   514,  1332,  1333,  1401,  1402,  1338,
    1339,  1340,  1402,  2108,  2194,  2096,  2181,   511,   514,   578,
    1320,  1321,  1402,  2108,  1253,  2067,  2065,  2067,   105,   161,
     578,   588,   589,   663,   685,   686,  1315,  2242,  2243,  2244,
    2245,  2246,  2247,  2248,   346,   510,  2192,  2192,    14,    55,
    2007,  1332,  2111,  1339,  2111,   101,   434,   687,  1336,  1337,
    2096,  2108,  2217,  2139,   671,   869,  1323,  2067,   336,   336,
     370,   336,   370,   337,   561,  2197,  2197,  2067,   566,   578,
    1327,  1328,  2096,  1327,  2192,  2192,  2181,  2096,  1256,  2196,
    2096,   317,  1316,  2096,    16,   317,  1318,  2096,    83,  1327,
     578,   578,   770,  1251,   317,  1324,  2096,   566,  1317,  1317,
    1317,  1317,  2067,  2108,   578
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_int16 yyr1[] =
{
       0,   929,   931,   930,   932,   932,   934,   933,   935,   935,
     936,   936,   938,   937,   939,   940,   941,   941,   942,   942,
     944,   943,   946,   945,   948,   949,   947,   950,   950,   951,
     952,   952,   954,   955,   953,   957,   956,   958,   958,   959,
     959,   960,   960,   961,   961,   962,   962,   962,   962,   963,
     963,   964,   964,   965,   965,   966,   967,   967,   968,   968,
     968,   968,   969,   969,   970,   970,   971,   971,   971,   972,
     972,   973,   973,   973,   973,   974,   975,   975,   976,   977,
     977,   978,   978,   979,   979,   980,   980,   980,   980,   980,
     982,   981,   983,   983,   984,   984,   986,   985,   987,   987,
     987,   987,   988,   988,   989,   989,   989,   989,   990,   991,
     993,   992,   994,   994,   994,   994,   994,   994,   995,   996,
     997,   997,   997,   997,   998,   998,  1000,   999,  1001,  1001,
    1001,  1002,  1002,  1003,  1003,  1003,  1003,  1003,  1004,  1004,
    1005,  1006,  1007,  1007,  1007,  1008,  1008,  1008,  1008,  1008,
    1008,  1008,  1008,  1008,  1008,  1008,  1008,  1008,  1008,  1010,
    1009,  1011,  1011,  1011,  1011,  1012,  1012,  1013,  1014,  1014,
    1016,  1015,  1018,  1017,  1019,  1017,  1020,  1020,  1021,  1022,
    1022,  1022,  1022,  1022,  1023,  1023,  1023,  1023,  1024,  1024,
    1024,  1025,  1025,  1026,  1026,  1027,  1026,  1028,  1028,  1029,
    1029,  1029,  1029,  1029,  1029,  1030,  1030,  1031,  1032,  1032,
    1033,  1034,  1034,  1035,  1036,  1036,  1037,  1037,  1038,  1039,
    1039,  1040,  1041,  1042,  1042,  1043,  1043,  1044,  1044,  1044,
    1045,  1045,  1046,  1047,  1048,  1048,  1049,  1050,  1051,  1052,
    1053,  1054,  1056,  1055,  1057,  1058,  1058,  1059,  1059,  1060,
    1060,  1062,  1061,  1063,  1063,  1064,  1064,  1065,  1065,  1065,
    1065,  1065,  1065,  1065,  1065,  1065,  1065,  1065,  1065,  1065,
    1065,  1065,  1065,  1065,  1065,  1065,  1066,  1066,  1066,  1066,
    1066,  1067,  1067,  1067,  1068,  1068,  1068,  1068,  1068,  1068,
    1068,  1069,  1069,  1069,  1069,  1070,  1070,  1070,  1070,  1070,
    1070,  1070,  1071,  1072,  1072,  1073,  1073,  1074,  1074,  1075,
    1076,  1076,  1076,  1077,  1078,  1078,  1080,  1079,  1081,  1081,
    1081,  1082,  1084,  1083,  1085,  1085,  1085,  1085,  1085,  1085,
    1086,  1087,  1088,  1089,  1089,  1089,  1091,  1090,  1092,  1092,
    1092,  1093,  1093,  1093,  1093,  1094,  1094,  1095,  1096,  1096,
    1097,  1097,  1098,  1098,  1098,  1098,  1099,  1101,  1100,  1102,
    1102,  1102,  1102,  1103,  1104,  1104,  1105,  1105,  1107,  1106,
    1106,  1108,  1109,  1110,  1111,  1111,  1112,  1113,  1113,  1113,
    1114,  1115,  1115,  1116,  1117,  1118,  1119,  1120,  1120,  1121,
    1122,  1122,  1122,  1123,  1123,  1124,  1124,  1124,  1124,  1125,
    1126,  1126,  1126,  1126,  1127,  1127,  1127,  1127,  1127,  1128,
    1128,  1128,  1128,  1128,  1128,  1128,  1128,  1130,  1129,  1131,
    1131,  1132,  1133,  1133,  1134,  1135,  1136,  1136,  1138,  1137,
    1139,  1139,  1140,  1141,  1141,  1142,  1142,  1143,  1145,  1144,
    1144,  1146,  1146,  1147,  1147,  1148,  1148,  1148,  1148,  1148,
    1148,  1148,  1148,  1148,  1148,  1148,  1149,  1150,  1150,  1150,
    1151,  1151,  1151,  1152,  1152,  1153,  1153,  1154,  1154,  1155,
    1156,  1156,  1157,  1157,  1158,  1158,  1159,  1160,  1161,  1161,
    1162,  1162,  1162,  1163,  1164,  1165,  1166,  1167,  1167,  1167,
    1167,  1167,  1168,  1168,  1169,  1170,  1170,  1171,  1172,  1172,
    1173,  1173,  1174,  1175,  1174,  1176,  1176,  1177,  1179,  1178,
    1180,  1180,  1181,  1181,  1181,  1182,  1182,  1182,  1183,  1183,
    1184,  1184,  1184,  1184,  1184,  1184,  1184,  1184,  1184,  1184,
    1184,  1185,  1186,  1186,  1187,  1187,  1188,  1188,  1188,  1188,
    1188,  1188,  1188,  1189,  1189,  1189,  1190,  1190,  1191,  1191,
    1191,  1191,  1191,  1191,  1192,  1193,  1194,  1193,  1195,  1196,
    1195,  1197,  1197,  1198,  1198,  1198,  1199,  1198,  1198,  1200,
    1201,  1201,  1202,  1202,  1203,  1204,  1204,  1205,  1205,  1205,
    1206,  1206,  1206,  1206,  1206,  1206,  1206,  1206,  1206,  1206,
    1206,  1206,  1206,  1206,  1206,  1207,  1207,  1208,  1208,  1209,
    1209,  1209,  1210,  1210,  1211,  1212,  1212,  1214,  1213,  1215,
    1216,  1215,  1217,  1217,  1218,  1218,  1218,  1219,  1219,  1220,
    1220,  1220,  1220,  1220,  1220,  1220,  1220,  1220,  1220,  1221,
    1221,  1222,  1222,  1223,  1223,  1223,  1223,  1223,  1223,  1223,
    1223,  1223,  1223,  1223,  1223,  1223,  1223,  1223,  1223,  1223,
    1223,  1223,  1223,  1223,  1224,  1225,  1226,  1226,  1227,  1228,
    1229,  1230,  1230,  1231,  1232,  1232,  1233,  1233,  1234,  1236,
    1235,  1237,  1237,  1237,  1237,  1238,  1239,  1240,  1240,  1241,
    1241,  1242,  1243,  1244,  1244,  1244,  1244,  1245,  1245,  1245,
    1245,  1245,  1245,  1245,  1245,  1245,  1245,  1245,  1245,  1245,
    1245,  1245,  1245,  1245,  1245,  1245,  1245,  1245,  1245,  1245,
    1245,  1245,  1245,  1245,  1245,  1245,  1245,  1245,  1245,  1245,
    1245,  1245,  1245,  1245,  1245,  1245,  1245,  1245,  1245,  1245,
    1245,  1245,  1245,  1245,  1245,  1245,  1245,  1246,  1246,  1247,
    1247,  1247,  1247,  1247,  1247,  1247,  1248,  1248,  1249,  1249,
    1250,  1251,  1251,  1252,  1252,  1252,  1253,  1253,  1254,  1254,
    1255,  1255,  1256,  1256,  1257,  1257,  1258,  1258,  1259,  1259,
    1260,  1259,  1259,  1259,  1261,  1262,  1262,  1263,  1264,  1264,
    1265,  1265,  1266,  1267,  1267,  1268,  1269,  1270,  1271,  1271,
    1271,  1272,  1273,  1275,  1274,  1276,  1276,  1277,  1277,  1278,
    1278,  1279,  1279,  1280,  1281,  1282,  1283,  1282,  1284,  1285,
    1284,  1286,  1287,  1286,  1288,  1288,  1290,  1289,  1291,  1291,
    1291,  1292,  1292,  1292,  1292,  1293,  1294,  1294,  1294,  1295,
    1296,  1296,  1297,  1298,  1299,  1299,  1299,  1300,  1301,  1301,
    1302,  1302,  1303,  1303,  1303,  1303,  1303,  1303,  1304,  1305,
    1306,  1307,  1308,  1309,  1309,  1311,  1310,  1310,  1312,  1312,
    1313,  1313,  1313,  1313,  1313,  1313,  1313,  1313,  1313,  1313,
    1313,  1313,  1313,  1313,  1313,  1313,  1313,  1314,  1315,  1315,
    1315,  1315,  1315,  1315,  1315,  1316,  1316,  1316,  1317,  1317,
    1318,  1318,  1318,  1318,  1319,  1320,  1320,  1320,  1321,  1321,
    1321,  1322,  1323,  1323,  1323,  1324,  1324,  1325,  1325,  1325,
    1325,  1325,  1326,  1326,  1327,  1327,  1328,  1328,  1328,  1329,
    1330,  1331,  1331,  1332,  1332,  1333,  1333,  1334,  1335,  1336,
    1336,  1337,  1337,  1337,  1338,  1338,  1339,  1339,  1340,  1341,
    1342,  1343,  1344,  1343,  1345,  1345,  1346,  1346,  1347,  1348,
    1347,  1349,  1350,  1347,  1347,  1351,  1351,  1352,  1352,  1352,
    1352,  1352,  1352,  1352,  1352,  1352,  1352,  1352,  1352,  1352,
    1352,  1352,  1352,  1352,  1352,  1352,  1352,  1352,  1352,  1352,
    1352,  1352,  1352,  1352,  1352,  1352,  1352,  1352,  1352,  1352,
    1352,  1352,  1352,  1352,  1352,  1352,  1352,  1352,  1352,  1352,
    1352,  1352,  1352,  1352,  1352,  1352,  1353,  1353,  1354,  1354,
    1354,  1354,  1354,  1354,  1354,  1354,  1354,  1354,  1354,  1354,
    1354,  1354,  1354,  1354,  1355,  1355,  1356,  1356,  1357,  1357,
    1358,  1358,  1359,  1359,  1360,  1360,  1361,  1361,  1362,  1362,
    1362,  1362,  1362,  1362,  1362,  1362,  1362,  1362,  1362,  1362,
    1362,  1362,  1363,  1363,  1363,  1363,  1363,  1363,  1363,  1363,
    1363,  1363,  1363,  1363,  1363,  1363,  1363,  1364,  1364,  1364,
    1364,  1364,  1364,  1364,  1364,  1364,  1364,  1365,  1365,  1366,
    1366,  1366,  1366,  1366,  1367,  1368,  1368,  1368,  1368,  1368,
    1368,  1368,  1368,  1368,  1368,  1368,  1368,  1368,  1368,  1368,
    1369,  1369,  1369,  1369,  1369,  1369,  1369,  1369,  1369,  1369,
    1370,  1370,  1370,  1370,  1370,  1370,  1370,  1370,  1370,  1370,
    1371,  1371,  1372,  1372,  1373,  1373,  1374,  1374,  1374,  1374,
    1374,  1375,  1375,  1375,  1375,  1375,  1375,  1375,  1375,  1375,
    1375,  1375,  1375,  1375,  1375,  1375,  1375,  1376,  1376,  1376,
    1377,  1377,  1377,  1377,  1377,  1377,  1377,  1377,  1378,  1378,
    1378,  1378,  1378,  1378,  1379,  1379,  1379,  1379,  1379,  1379,
    1379,  1379,  1380,  1380,  1380,  1380,  1381,  1381,  1381,  1382,
    1382,  1382,  1382,  1382,  1382,  1383,  1383,  1383,  1383,  1384,
    1384,  1384,  1384,  1384,  1384,  1384,  1385,  1385,  1385,  1385,
    1385,  1385,  1385,  1385,  1385,  1385,  1385,  1385,  1385,  1385,
    1385,  1385,  1385,  1385,  1385,  1385,  1385,  1385,  1385,  1385,
    1385,  1385,  1385,  1385,  1385,  1385,  1385,  1385,  1385,  1385,
    1385,  1385,  1385,  1385,  1385,  1385,  1385,  1385,  1385,  1385,
    1385,  1385,  1385,  1386,  1386,  1386,  1387,  1387,  1387,  1387,
    1387,  1387,  1387,  1387,  1387,  1388,  1388,  1388,  1388,  1388,
    1388,  1388,  1388,  1388,  1388,  1388,  1388,  1388,  1388,  1388,
    1388,  1388,  1388,  1388,  1388,  1388,  1388,  1388,  1389,  1390,
    1391,  1391,  1391,  1391,  1391,  1391,  1391,  1391,  1392,  1392,
    1392,  1393,  1393,  1394,  1395,  1395,  1396,  1396,  1397,  1397,
    1398,  1398,  1398,  1399,  1399,  1400,  1400,  1401,  1401,  1402,
    1403,  1403,  1404,  1405,  1405,  1406,  1407,  1407,  1408,  1408,
    1409,  1410,  1410,  1410,  1411,  1412,  1412,  1412,  1413,  1414,
    1415,  1415,  1417,  1418,  1419,  1416,  1420,  1416,  1421,  1422,
    1421,  1423,  1421,  1424,  1424,  1425,  1426,  1426,  1426,  1427,
    1427,  1427,  1427,  1427,  1427,  1428,  1429,  1429,  1430,  1430,
    1431,  1431,  1431,  1432,  1433,  1432,  1434,  1434,  1435,  1435,
    1435,  1435,  1435,  1437,  1436,  1438,  1438,  1439,  1440,  1441,
    1441,  1443,  1444,  1442,  1446,  1445,  1445,  1447,  1447,  1447,
    1447,  1447,  1447,  1447,  1447,  1447,  1447,  1447,  1447,  1447,
    1447,  1447,  1447,  1447,  1447,  1447,  1447,  1447,  1447,  1447,
    1447,  1447,  1447,  1447,  1447,  1447,  1447,  1447,  1447,  1447,
    1447,  1447,  1447,  1447,  1447,  1447,  1447,  1447,  1447,  1447,
    1447,  1447,  1447,  1447,  1447,  1447,  1447,  1447,  1447,  1447,
    1447,  1447,  1447,  1447,  1447,  1447,  1447,  1447,  1447,  1447,
    1447,  1447,  1447,  1449,  1448,  1451,  1450,  1452,  1450,  1450,
    1450,  1450,  1450,  1450,  1450,  1450,  1450,  1450,  1450,  1450,
    1450,  1450,  1450,  1450,  1450,  1450,  1450,  1450,  1450,  1450,
    1450,  1450,  1453,  1453,  1455,  1454,  1456,  1456,  1457,  1457,
    1457,  1458,  1458,  1458,  1459,  1459,  1460,  1460,  1461,  1461,
    1462,  1462,  1462,  1462,  1462,  1463,  1463,  1464,  1464,  1465,
    1465,  1466,  1466,  1466,  1467,  1468,  1469,  1470,  1470,  1470,
    1470,  1470,  1470,  1470,  1470,  1470,  1470,  1470,  1470,  1470,
    1470,  1470,  1470,  1470,  1470,  1470,  1470,  1470,  1470,  1470,
    1470,  1470,  1470,  1470,  1470,  1470,  1470,  1470,  1470,  1470,
    1470,  1470,  1470,  1471,  1471,  1472,  1473,  1473,  1473,  1474,
    1474,  1474,  1475,  1475,  1476,  1476,  1478,  1477,  1479,  1479,
    1479,  1479,  1480,  1480,  1481,  1481,  1483,  1482,  1484,  1484,
    1485,  1485,  1486,  1486,  1488,  1487,  1489,  1489,  1490,  1491,
    1491,  1493,  1492,  1495,  1494,  1496,  1497,  1496,  1498,  1498,
    1498,  1499,  1499,  1500,  1500,  1500,  1500,  1500,  1500,  1501,
    1501,  1502,  1502,  1503,  1503,  1504,  1505,  1504,  1506,  1506,
    1507,  1507,  1508,  1508,  1508,  1508,  1509,  1509,  1509,  1509,
    1509,  1510,  1510,  1511,  1511,  1512,  1512,  1512,  1513,  1513,
    1514,  1514,  1515,  1515,  1516,  1517,  1517,  1519,  1518,  1520,
    1520,  1521,  1521,  1523,  1522,  1524,  1524,  1525,  1525,  1526,
    1526,  1526,  1526,  1526,  1528,  1527,  1529,  1529,  1531,  1530,
    1532,  1533,  1533,  1534,  1536,  1535,  1537,  1538,  1537,  1540,
    1539,  1541,  1541,  1543,  1542,  1544,  1544,  1545,  1545,  1546,
    1546,  1548,  1547,  1549,  1550,  1550,  1551,  1551,  1551,  1551,
    1551,  1553,  1552,  1554,  1554,  1554,  1554,  1554,  1554,  1554,
    1554,  1554,  1554,  1554,  1555,  1555,  1556,  1556,  1558,  1557,
    1559,  1559,  1560,  1560,  1561,  1561,  1562,  1562,  1563,  1563,
    1563,  1563,  1563,  1564,  1564,  1565,  1565,  1565,  1565,  1566,
    1566,  1568,  1567,  1569,  1571,  1570,  1572,  1572,  1573,  1573,
    1573,  1575,  1574,  1576,  1576,  1577,  1577,  1578,  1578,  1578,
    1578,  1578,  1580,  1581,  1579,  1582,  1582,  1584,  1585,  1583,
    1587,  1586,  1588,  1588,  1588,  1589,  1589,  1590,  1590,  1591,
    1591,  1591,  1592,  1592,  1593,  1593,  1593,  1593,  1593,  1593,
    1593,  1593,  1594,  1595,  1596,  1596,  1596,  1597,  1597,  1597,
    1597,  1598,  1598,  1599,  1599,  1600,  1601,  1602,  1602,  1602,
    1602,  1602,  1602,  1602,  1602,  1602,  1602,  1602,  1602,  1602,
    1602,  1602,  1602,  1602,  1602,  1602,  1602,  1602,  1602,  1602,
    1603,  1603,  1605,  1604,  1606,  1606,  1606,  1606,  1606,  1607,
    1607,  1609,  1608,  1611,  1610,  1612,  1610,  1613,  1614,  1616,
    1615,  1617,  1618,  1618,  1619,  1619,  1619,  1620,  1620,  1621,
    1621,  1622,  1622,  1622,  1623,  1623,  1623,  1625,  1624,  1626,
    1624,  1627,  1627,  1628,  1628,  1628,  1628,  1628,  1629,  1629,
    1630,  1630,  1632,  1631,  1634,  1633,  1635,  1635,  1636,  1636,
    1637,  1637,  1638,  1638,  1640,  1639,  1641,  1641,  1641,  1641,
    1641,  1641,  1641,  1642,  1642,  1644,  1643,  1645,  1647,  1646,
    1648,  1650,  1649,  1651,  1651,  1652,  1652,  1653,  1655,  1654,
    1656,  1656,  1656,  1657,  1657,  1658,  1659,  1660,  1660,  1662,
    1661,  1663,  1664,  1664,  1665,  1665,  1665,  1666,  1666,  1667,
    1667,  1668,  1669,  1669,  1669,  1669,  1669,  1669,  1669,  1670,
    1670,  1672,  1671,  1673,  1673,  1675,  1674,  1676,  1676,  1678,
    1677,  1679,  1680,  1680,  1680,  1681,  1681,  1681,  1681,  1683,
    1682,  1684,  1685,  1686,  1686,  1687,  1687,  1687,  1687,  1687,
    1687,  1688,  1688,  1689,  1689,  1690,  1690,  1690,  1690,  1690,
    1691,  1692,  1692,  1692,  1692,  1692,  1693,  1694,  1696,  1695,
    1698,  1699,  1697,  1700,  1700,  1701,  1701,  1702,  1703,  1703,
    1705,  1704,  1706,  1707,  1707,  1709,  1708,  1711,  1710,  1712,
    1712,  1713,  1713,  1715,  1714,  1716,  1716,  1718,  1717,  1719,
    1719,  1720,  1720,  1722,  1721,  1723,  1723,  1724,  1725,  1725,
    1726,  1726,  1726,  1726,  1727,  1727,  1728,  1728,  1728,  1728,
    1728,  1729,  1729,  1730,  1730,  1730,  1731,  1731,  1731,  1732,
    1732,  1732,  1733,  1733,  1734,  1734,  1734,  1736,  1735,  1737,
    1738,  1737,  1739,  1737,  1740,  1740,  1741,  1741,  1742,  1742,
    1743,  1743,  1743,  1743,  1743,  1744,  1744,  1745,  1745,  1746,
    1746,  1747,  1748,  1748,  1750,  1749,  1752,  1751,  1753,  1753,
    1754,  1756,  1755,  1757,  1758,  1758,  1759,  1759,  1759,  1759,
    1760,  1760,  1761,  1761,  1762,  1762,  1763,  1764,  1764,  1764,
    1765,  1765,  1766,  1766,  1766,  1767,  1767,  1768,  1768,  1769,
    1769,  1770,  1772,  1771,  1773,  1774,  1774,  1775,  1775,  1775,
    1776,  1776,  1777,  1778,  1778,  1779,  1780,  1780,  1782,  1781,
    1783,  1784,  1786,  1785,  1787,  1788,  1788,  1790,  1789,  1791,
    1792,  1792,  1793,  1793,  1794,  1794,  1795,  1797,  1796,  1798,
    1798,  1799,  1799,  1800,  1800,  1801,  1801,  1802,  1803,  1803,
    1805,  1804,  1806,  1806,  1807,  1807,  1808,  1809,  1809,  1809,
    1809,  1810,  1810,  1812,  1811,  1813,  1813,  1813,  1813,  1813,
    1813,  1813,  1813,  1814,  1814,  1815,  1815,  1816,  1817,  1818,
    1818,  1819,  1819,  1819,  1819,  1819,  1819,  1819,  1819,  1820,
    1820,  1820,  1821,  1822,  1822,  1823,  1824,  1824,  1825,  1825,
    1826,  1827,  1829,  1828,  1831,  1830,  1832,  1832,  1833,  1833,
    1834,  1834,  1835,  1835,  1836,  1836,  1836,  1837,  1837,  1837,
    1839,  1838,  1840,  1841,  1841,  1842,  1842,  1842,  1842,  1843,
    1843,  1843,  1843,  1843,  1843,  1844,  1845,  1845,  1846,  1846,
    1848,  1847,  1847,  1847,  1849,  1849,  1849,  1849,  1849,  1850,
    1850,  1851,  1851,  1852,  1852,  1852,  1852,  1854,  1853,  1855,
    1857,  1856,  1858,  1858,  1859,  1860,  1860,  1861,  1861,  1862,
    1862,  1863,  1863,  1865,  1864,  1866,  1866,  1866,  1866,  1867,
    1867,  1868,  1869,  1869,  1871,  1870,  1872,  1872,  1874,  1873,
    1875,  1877,  1876,  1878,  1880,  1879,  1881,  1882,  1882,  1883,
    1883,  1884,  1885,  1885,  1886,  1887,  1887,  1888,  1888,  1889,
    1889,  1891,  1890,  1892,  1892,  1894,  1893,  1895,  1895,  1895,
    1895,  1895,  1896,  1897,  1897,  1898,  1898,  1898,  1898,  1898,
    1899,  1900,  1900,  1901,  1901,  1901,  1902,  1902,  1902,  1902,
    1903,  1904,  1904,  1905,  1906,  1906,  1907,  1907,  1908,  1908,
    1910,  1909,  1911,  1912,  1912,  1913,  1913,  1913,  1913,  1914,
    1914,  1915,  1915,  1915,  1916,  1916,  1918,  1917,  1920,  1921,
    1919,  1922,  1922,  1923,  1923,  1924,  1925,  1925,  1925,  1926,
    1926,  1927,  1927,  1928,  1928,  1929,  1929,  1930,  1931,  1931,
    1932,  1932,  1933,  1933,  1934,  1934,  1935,  1935,  1936,  1937,
    1937,  1938,  1938,  1938,  1939,  1939,  1940,  1940,  1941,  1941,
    1941,  1942,  1942,  1942,  1943,  1943,  1944,  1944,  1946,  1945,
    1948,  1947,  1949,  1949,  1950,  1950,  1951,  1951,  1952,  1952,
    1953,  1953,  1953,  1954,  1954,  1955,  1956,  1956,  1957,  1957,
    1958,  1959,  1959,  1960,  1960,  1960,  1961,  1961,  1962,  1963,
    1963,  1964,  1965,  1965,  1965,  1966,  1966,  1967,  1968,  1968,
    1969,  1970,  1970,  1970,  1971,  1971,  1972,  1973,  1973,  1974,
    1975,  1975,  1975,  1976,  1976,  1977,  1978,  1978,  1979,  1980,
    1980,  1980,  1981,  1981,  1982,  1983,  1983,  1984,  1985,  1985,
    1986,  1986,  1987,  1987,  1988,  1989,  1989,  1990,  1991,  1991,
    1992,  1992,  1993,  1994,  1994,  1995,  1996,  1996,  1997,  1997,
    1998,  1998,  1999,  2000,  2000,  2001,  2002,  2002,  2003,  2003,
    2004,  2004,  2005,  2005,  2006,  2006,  2007,  2007,  2008,  2010,
    2009,  2011,  2011,  2012,  2012,  2012,  2012,  2012,  2012,  2012,
    2012,  2012,  2012,  2012,  2012,  2012,  2012,  2013,  2013,  2014,
    2015,  2015,  2015,  2015,  2015,  2015,  2015,  2015,  2015,  2015,
    2015,  2015,  2015,  2015,  2016,  2016,  2017,  2017,  2018,  2018,
    2019,  2020,  2021,  2021,  2022,  2022,  2022,  2023,  2023,  2023,
    2024,  2024,  2024,  2025,  2025,  2026,  2026,  2026,  2027,  2027,
    2028,  2028,  2028,  2028,  2028,  2028,  2029,  2029,  2030,  2031,
    2032,  2032,  2033,  2034,  2034,  2035,  2035,  2036,  2037,  2038,
    2039,  2039,  2040,  2041,  2041,  2042,  2043,  2043,  2044,  2045,
    2045,  2045,  2046,  2047,  2047,  2048,  2049,  2049,  2050,  2050,
    2051,  2052,  2052,  2053,  2054,  2054,  2055,  2055,  2056,  2057,
    2057,  2058,  2058,  2058,  2059,  2059,  2060,  2060,  2061,  2061,
    2062,  2062,  2063,  2063,  2063,  2063,  2063,  2063,  2063,  2063,
    2063,  2063,  2063,  2064,  2065,  2065,  2066,  2066,  2066,  2067,
    2067,  2067,  2067,  2067,  2067,  2067,  2068,  2068,  2068,  2068,
    2068,  2068,  2069,  2070,  2071,  2072,  2072,  2073,  2073,  2074,
    2075,  2076,  2076,  2077,  2077,  2078,  2078,  2078,  2079,  2079,
    2080,  2080,  2081,  2081,  2081,  2082,  2082,  2082,  2083,  2083,
    2083,  2084,  2084,  2085,  2085,  2086,  2086,  2087,  2087,  2087,
    2088,  2089,  2090,  2090,  2091,  2092,  2093,  2094,  2095,  2096,
    2097,  2097,  2097,  2097,  2098,  2098,  2099,  2099,  2100,  2100,
    2100,  2100,  2101,  2102,  2102,  2104,  2103,  2105,  2105,  2106,
    2107,  2107,  2108,  2109,  2110,  2111,  2111,  2112,  2112,  2112,
    2112,  2112,  2112,  2112,  2113,  2113,  2114,  2114,  2115,  2115,
    2115,  2115,  2115,  2115,  2115,  2116,  2116,  2116,  2116,  2117,
    2117,  2117,  2117,  2117,  2117,  2117,  2117,  2117,  2117,  2117,
    2117,  2117,  2117,  2118,  2118,  2119,  2119,  2119,  2119,  2120,
    2120,  2120,  2120,  2120,  2121,  2121,  2121,  2122,  2122,  2122,
    2123,  2123,  2123,  2125,  2124,  2126,  2126,  2127,  2127,  2128,
    2128,  2129,  2129,  2130,  2131,  2131,  2132,  2132,  2132,  2133,
    2133,  2134,  2134,  2135,  2135,  2136,  2136,  2136,  2137,  2137,
    2138,  2138,  2138,  2139,  2139,  2140,  2140,  2141,  2141,  2141,
    2141,  2141,  2141,  2141,  2141,  2142,  2142,  2143,  2143,  2144,
    2144,  2145,  2145,  2145,  2145,  2146,  2146,  2146,  2146,  2146,
    2146,  2146,  2146,  2146,  2146,  2146,  2146,  2146,  2146,  2146,
    2146,  2146,  2146,  2146,  2146,  2146,  2146,  2146,  2146,  2146,
    2146,  2146,  2146,  2146,  2146,  2146,  2146,  2146,  2146,  2146,
    2146,  2146,  2146,  2146,  2146,  2146,  2146,  2146,  2146,  2146,
    2146,  2146,  2146,  2146,  2146,  2146,  2146,  2147,  2147,  2147,
    2147,  2147,  2147,  2147,  2147,  2147,  2147,  2147,  2147,  2147,
    2147,  2147,  2147,  2147,  2147,  2147,  2147,  2147,  2147,  2147,
    2148,  2148,  2149,  2149,  2150,  2150,  2151,  2151,  2152,  2152,
    2152,  2153,  2153,  2154,  2154,  2155,  2155,  2156,  2156,  2157,
    2157,  2158,  2158,  2159,  2159,  2160,  2160,  2161,  2161,  2162,
    2162,  2163,  2163,  2164,  2164,  2165,  2165,  2166,  2166,  2167,
    2167,  2168,  2168,  2169,  2169,  2170,  2170,  2171,  2171,  2172,
    2172,  2173,  2173,  2173,  2174,  2174,  2174,  2175,  2175,  2176,
    2176,  2177,  2177,  2178,  2178,  2179,  2179,  2180,  2180,  2180,
    2181,  2181,  2181,  2182,  2182,  2182,  2182,  2183,  2183,  2183,
    2184,  2184,  2185,  2185,  2186,  2186,  2186,  2187,  2187,  2187,
    2188,  2188,  2189,  2189,  2190,  2190,  2191,  2191,  2192,  2192,
    2193,  2193,  2194,  2194,  2195,  2195,  2196,  2196,  2197,  2197,
    2197,  2198,  2198,  2198,  2198,  2199,  2199,  2200,  2200,  2201,
    2201,  2202,  2202,  2203,  2203,  2204,  2204,  2205,  2205,  2205,
    2206,  2206,  2207,  2207,  2208,  2208,  2209,  2209,  2209,  2210,
    2210,  2211,  2211,  2212,  2212,  2213,  2213,  2214,  2214,  2215,
    2215,  2216,  2216,  2217,  2217,  2218,  2218,  2219,  2219,  2220,
    2220,  2221,  2221,  2222,  2222,  2223,  2223,  2223,  2224,  2224,
    2225,  2225,  2226,  2226,  2227,  2227,  2227,  2227,  2228,  2228,
    2229,  2229,  2230,  2230,  2231,  2231,  2232,  2232,  2233,  2233,
    2234,  2234,  2235,  2235,  2235,  2236,  2236,  2237,  2237,  2238,
    2238,  2239,  2239,  2240,  2240,  2241,  2241,  2242,  2242,  2243,
    2243,  2244,  2244,  2245,  2245,  2246,  2246,  2247,  2247,  2248,
    2248,  2249,  2249
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_int8 yyr2[] =
{
       0,     2,     0,     2,     1,     1,     0,     2,     1,     2,
       1,     1,     0,     2,     4,     4,     0,     1,     1,     2,
       0,     4,     0,     4,     0,     0,     6,     0,     1,     3,
       1,     1,     0,     0,     8,     0,     6,     1,     1,     1,
       1,     0,     2,     0,     3,     1,     1,     1,     1,     2,
       2,     1,     1,     0,     3,     5,     0,     3,     1,     1,
       1,     1,     0,     5,     0,     3,     1,     1,     1,     0,
       4,     1,     1,     1,     1,     3,     0,     3,     2,     0,
       3,     0,     1,     1,     2,     1,     1,     1,     1,     1,
       0,     4,     0,     3,     0,     3,     0,     4,     0,     2,
       3,     2,     1,     2,     1,     1,     1,     1,     5,     2,
       0,     4,     2,     3,     4,     4,     8,     8,     3,     4,
       1,     1,     1,     1,     1,     2,     0,     4,     0,     2,
       3,     1,     2,     3,     3,     3,     3,     3,     1,     2,
       2,     2,     1,     2,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       3,     2,     3,     3,     1,     0,     1,     1,     3,     4,
       0,     4,     0,     4,     0,     4,     0,     2,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       1,     1,     2,     1,     3,     0,     4,     1,     3,     1,
       1,     1,     1,     1,     1,     1,     1,     2,     0,     2,
       3,     1,     2,     3,     1,     2,     1,     2,     3,     1,
       2,     3,     6,     1,     2,     1,     3,     0,     2,     2,
       0,     2,     4,     5,     0,     3,     3,     5,     3,     4,
       3,     3,     0,     4,     4,     0,     3,     0,     2,     0,
       2,     0,     5,     2,     2,     0,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     5,     5,     6,     6,
       4,     0,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     3,     0,     1,     1,     1,     1,     1,     4,
       1,     1,     1,     9,     0,     1,     0,     4,     0,     4,
       3,     1,     0,     4,     2,     3,     4,     4,     8,     8,
       6,     1,     5,     0,     1,     1,     0,     5,     2,     2,
       2,     0,     5,     6,     1,     0,     1,     2,     0,     2,
       3,     1,     1,     3,     1,     2,     4,     0,     5,     1,
       1,     1,     1,     7,     0,     2,     1,     2,     0,     2,
       2,     1,     4,     3,     1,     1,     3,     2,     2,     2,
       3,     3,     4,     4,     4,     4,     4,     0,     2,     2,
       0,     2,     3,     1,     2,     1,     1,     1,     1,     5,
       0,     1,     1,     1,     4,     4,     4,     4,     1,     6,
       6,     7,     4,     6,     4,     6,     4,     0,     6,     1,
       2,     2,     0,     2,     6,     2,     2,     3,     0,    10,
       0,     1,     3,     0,     3,     0,     2,     2,     0,     5,
       3,     1,     1,     0,     2,     2,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     5,     0,     1,     1,
       4,     6,     9,     0,     3,     0,     2,     0,     2,     3,
       5,     5,     1,     1,     1,     1,     3,     5,     0,     2,
       1,     1,     1,     4,     2,     2,     4,     1,     1,     1,
       1,     1,     1,     1,     4,     0,     2,     2,     2,     2,
       1,     2,     0,     0,     5,     0,     2,     2,     0,     5,
       0,     2,     4,     3,     4,     0,     1,     1,     1,     2,
       4,     4,     4,     4,     4,     4,     4,     4,     4,     4,
       4,    11,     0,     1,     1,     2,     4,     4,     4,     6,
       4,     3,     4,     0,     1,     1,     1,     2,     4,     4,
       4,     4,     4,     4,     6,     0,     0,     5,     0,     0,
       2,     2,     3,     1,     1,     1,     0,     4,     3,     2,
       0,     1,     1,     1,     1,     0,     2,     1,     2,     3,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     6,     0,     2,     0,     4,     5,
       0,     7,     2,     2,     1,     3,     1,     1,     2,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     0,
       1,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     2,     3,     0,     1,     3,     3,
       3,     0,     1,     3,     0,     2,     0,     1,     2,     0,
       4,     1,     2,     1,     1,     1,     2,     0,     5,     0,
       2,     1,     3,     1,     3,     3,     3,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     3,     3,     4,     3,
       3,     3,     4,     3,     3,     1,     1,     1,     1,     1,
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
       1,     1,     1,     1,     1,     1,     1,     3,     1,     1,
       3,     3,     1,     1,     1,     0,     2,     2,     0,     2,
       0,     2,     2,     1,     3,     1,     2,     1,     1,     1,
       1,     4,     0,     3,     2,     1,     1,     3,     4,     5,
       4,     5,     1,     1,     0,     2,     1,     1,     1,     6,
       2,     3,     2,     0,     2,     1,     2,     2,     4,     0,
       1,     1,     1,     1,     2,     1,     1,     2,     1,     4,
       2,     0,     0,     5,     0,     1,     2,     3,     1,     0,
       4,     0,     0,     7,     3,     0,     2,     2,     2,     1,
       1,     2,     2,     1,     1,     1,     1,     1,     1,     1,
       3,     3,     3,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     4,     1,     1,     2,     3,     2,
       2,     2,     3,     3,     3,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     2,     2,     1,     2,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     0,     1,
       1,     2,     1,     3,     3,     2,     2,     1,     1,     1,
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
       1,     1,     1,     1,     1,     2,     3,     3,     1,     2,
       3,     3,     3,     1,     2,     1,     2,     0,     1,     1,
       1,     1,     1,     1,     1,     2,     1,     1,     0,     1,
       4,     0,     1,     1,     4,     0,     1,     1,     3,     2,
       0,     1,     0,     0,     0,    12,     0,     4,     0,     0,
       3,     0,     3,     1,     2,     5,     0,     2,     2,     0,
       3,     3,     4,     2,     1,     3,     0,     5,     0,     1,
       0,     2,     2,     0,     0,     7,     0,     2,     1,     1,
       2,     1,     1,     0,     6,     0,     2,     2,     1,     0,
       1,     0,     0,     3,     0,     2,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     2,     2,     0,     4,     0,     4,     0,     5,     3,
       3,     3,     3,     4,     3,     4,     3,     3,     4,     4,
       4,     3,     4,     3,     4,     5,     3,     4,     3,     3,
       2,     3,     1,     1,     0,     3,     0,     1,     5,     4,
       4,     1,     3,     3,     1,     1,     0,     1,     1,     2,
       1,     1,     1,     2,     3,     1,     2,     1,     3,     1,
       2,     2,     2,     2,     3,     3,     3,     1,     1,     1,
       2,     1,     1,     3,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     4,     1,     1,     1,
       1,     4,     1,     2,     1,     1,     3,     3,     3,     3,
       3,     3,     4,     0,     1,     1,     2,     1,     1,     1,
       1,     1,     1,     1,     0,     1,     0,     4,     4,     5,
       6,     8,     0,     2,     0,     1,     0,     3,     4,     5,
       0,     2,     0,     2,     0,     3,     1,     2,     4,     0,
       2,     0,     4,     0,     9,     0,     0,     4,     1,     1,
       1,     0,     1,     1,     1,     1,     1,     1,     1,     1,
       2,     0,     2,     1,     1,     0,     0,     3,     1,     2,
       2,     3,     0,     2,     2,     2,     0,     3,     2,     2,
       4,     1,     1,     1,     1,     0,     2,     2,     0,     1,
       2,     2,     0,     1,     2,     0,     1,     0,     3,     1,
       2,     1,     1,     0,     3,     1,     1,     2,     3,     0,
       1,     3,     3,     2,     0,     4,     0,     3,     0,     4,
       4,     0,     1,     1,     0,     3,     0,     0,     4,     0,
       3,     2,     1,     0,     4,     4,     2,     1,     2,     0,
       1,     0,     3,     3,     0,     3,     0,     2,     1,     2,
       1,     0,     4,     3,     3,     3,     3,     2,     1,     1,
       1,     1,     1,     1,     2,     1,     1,     2,     0,     3,
       1,     1,     0,     2,     1,     2,     1,     2,     1,     2,
       1,     1,     2,     0,     1,     2,     2,     2,     2,     1,
       1,     0,     3,     2,     0,     3,     1,     2,     1,     1,
       1,     0,     5,     0,     1,     1,     2,     3,     3,     3,
       3,     2,     0,     0,     5,     1,     1,     0,     0,     7,
       0,     5,     1,     1,     1,     0,     1,     0,     2,     1,
       2,     1,     1,     2,     1,     2,     1,     5,     1,     1,
       1,     2,     1,     1,     0,     1,     1,     1,     1,     0,
       1,     3,     3,     1,     1,     4,     3,     1,     2,     2,
       1,     1,     2,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     3,     1,     3,     3,     3,     3,     3,
       0,     1,     0,     4,     4,     6,     6,     8,     8,     0,
       1,     0,     3,     0,     3,     0,     6,     4,     1,     0,
       4,     2,     1,     3,     1,     1,     1,     2,     1,     1,
       2,     2,     2,     2,     3,     3,     3,     0,     3,     0,
       4,     1,     3,     2,     1,     1,     1,     1,     0,     2,
       0,     1,     0,     3,     0,     7,     0,     1,     0,     1,
       1,     2,     1,     1,     0,     3,     0,     2,     1,     2,
       1,     1,     1,     0,     2,     0,     3,     1,     0,     3,
       1,     0,     3,     3,     4,     0,     3,     2,     0,     6,
       5,     3,     2,     0,     1,     0,     0,     0,     1,     0,
       3,     5,     0,     2,     0,     3,     3,     0,     2,     1,
       2,     4,     1,     1,     1,     1,     1,     1,     1,     0,
       3,     0,     3,     1,     2,     0,     3,     2,     2,     0,
       3,     2,     1,     1,     1,     2,     1,     1,     1,     0,
       3,     2,     5,     1,     2,     2,     2,     1,     1,     1,
       2,     1,     2,     4,     2,     0,     1,     1,     1,     1,
       4,     0,     1,     1,     2,     2,     3,     3,     0,     5,
       0,     0,     9,     0,     2,     1,     2,     1,     0,     1,
       0,     5,     7,     0,     2,     0,     3,     0,     4,     2,
       2,     0,     1,     0,     3,     3,     4,     0,     4,     4,
       6,     0,     1,     0,     3,     1,     2,     6,     0,     1,
       1,     1,     1,     1,     0,     3,     0,     1,     1,     2,
       1,     2,     2,     1,     1,     1,     2,     1,     1,     1,
       1,     1,     3,     1,     1,     1,     1,     0,     3,     4,
       0,     6,     0,     5,     0,     1,     1,     1,     1,     3,
       0,     2,     1,     3,     3,     0,     3,     1,     1,     1,
       3,     6,     0,     2,     0,     3,     0,     3,     2,     1,
       1,     0,     4,     7,     0,     2,     0,     1,     2,     1,
       2,     3,     3,     1,     0,     1,     1,     4,     4,     2,
       0,     1,     1,     3,     2,     0,     3,     1,     1,     0,
       1,     1,     0,     4,     5,     1,     1,     0,     2,     2,
       0,     1,     2,     0,     1,     2,     0,     1,     0,     3,
       2,     1,     0,     4,     4,     0,     1,     0,     4,     5,
       0,     1,     2,     3,     0,     1,     1,     0,     4,     4,
       6,     0,     2,     0,     2,     1,     2,     3,     0,     1,
       0,     3,     2,     5,     0,     1,     2,     2,     2,     2,
       2,     0,     2,     0,     3,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     4,     3,     1,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     4,
       3,     5,     4,     1,     2,     3,     1,     2,     3,     3,
       4,     4,     0,     3,     0,     7,     0,     5,     0,     2,
       0,     3,     0,     1,     0,     2,     4,     0,     2,     4,
       0,     4,     4,     0,     3,     0,     4,     1,     1,     1,
       2,     2,     2,     2,     1,     1,     2,     1,     0,     1,
       0,     4,     2,     2,     0,     2,     1,     4,     4,     0,
       1,     1,     1,     1,     1,     1,     1,     0,     4,     5,
       0,     2,     1,     2,     2,     0,     3,     1,     1,     0,
       4,     0,     1,     0,     4,     4,     6,     6,     8,     0,
       1,     2,     0,     1,     0,     3,     1,     2,     0,     3,
       5,     0,     3,     2,     0,     4,     6,     0,     3,     1,
       3,     2,     2,     2,     3,     0,     3,     0,     3,     0,
       1,     0,     3,     1,     2,     0,     3,     1,     1,     1,
       1,     1,     7,     0,     1,     1,     1,     1,     1,     1,
       4,     1,     2,     1,     2,     3,     0,     1,     2,     1,
       3,     1,     1,     4,     1,     2,     2,     3,     1,     1,
       0,     4,     6,     0,     2,     0,     4,     3,     3,     1,
       1,     0,     1,     1,     0,     1,     0,     5,     0,     0,
      12,     0,     1,     1,     2,     2,     2,     1,     1,     0,
       4,     0,     3,     0,     3,     1,     2,     3,     0,     3,
       1,     2,     3,     3,     0,     3,     1,     2,     3,     0,
       1,     1,     1,     1,     0,     2,     1,     2,     1,     2,
       2,     2,     2,     1,     1,     3,     0,     1,     0,     5,
       0,    10,     0,     3,     0,     2,     0,     3,     1,     2,
       0,     2,     2,     0,     1,     3,     1,     1,     0,     1,
       2,     1,     1,     0,     2,     2,     0,     1,     2,     0,
       1,     2,     0,     2,     2,     0,     1,     2,     0,     1,
       2,     0,     2,     2,     0,     1,     2,     0,     1,     2,
       0,     2,     2,     0,     1,     2,     0,     1,     2,     0,
       2,     2,     0,     1,     2,     0,     1,     2,     2,     2,
       2,     2,     0,     1,     2,     0,     1,     2,     2,     2,
       0,     1,     2,     0,     1,     2,     0,     1,     2,     2,
       0,     1,     2,     0,     1,     2,     0,     2,     0,     3,
       2,     1,     0,     2,     0,     3,     1,     1,     1,     0,
       2,     1,     2,     1,     2,     3,     3,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     0,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     1,     1,     1,     1,
       1,     1,     1,     3,     0,     1,     1,     3,     3,     1,
       3,     3,     1,     3,     1,     2,     2,     1,     3,     1,
       1,     3,     1,     3,     1,     3,     1,     2,     2,     1,
       1,     2,     1,     1,     2,     2,     3,     1,     1,     1,
       1,     2,     1,     1,     2,     1,     0,     2,     1,     1,
       1,     3,     1,     1,     2,     1,     0,     1,     1,     2,
       1,     1,     2,     1,     1,     1,     1,     1,     1,     1,
       2,     1,     1,     3,     0,     1,     1,     2,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     2,     4,     3,
       5,     5,     1,     1,     1,     2,     1,     1,     1,     1,
       1,     1,     1,     2,     2,     2,     1,     1,     1,     2,
       2,     2,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     1,     1,     1,     1,     1,     1,
       3,     2,     2,     1,     1,     2,     1,     1,     3,     2,
       2,     1,     1,     1,     3,     0,     2,     1,     3,     3,
       4,     5,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     1,     3,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       5,     5,     5,     4,     5,     4,     5,     5,     5,     5,
       5,     2,     2,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     0,     4,     5,     0,     3,     2,
       1,     3,     3,     0,     2,     1,     3,     1,     3,     1,
       3,     1,     3,     0,     0,     1,     0,     3,     2,     0,
       1,     0,     2,     0,     2,     0,     1,     1,     0,     1,
       0,     1,     2,     0,     2,     0,     3,     1,     1,     1,
       1,     1,     1,     1,     1,     0,     2,     0,     5,     0,
       3,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       0,     1,     0,     1,     0,     1,     0,     1,     0,     1,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     3,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     1,     1,     0,     1,     2,     0,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     1,
       0,     1,     1,     0,     1,     1,     1,     0,     1,     1,
       0,     1,     0,     1,     0,     1,     1,     0,     2,     2,
       0,     1,     0,     1,     0,     1,     0,     1,     0,     1,
       0,     1,     1,     1,     0,     1,     0,     1,     0,     1,
       1,     0,     2,     1,     1,     0,     1,     0,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     1,
       0,     1,     0,     1,     0,     1,     0,     1,     2,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     3,     0,     1,     0,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     2,     2,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     2,
       1,     2,     1,     2,     1,     2,     1,     2,     1,     2,
       1,     2,     2
};


#define yyerrok         (yyerrstatus = 0)
#define yyclearin       (yychar = YYEMPTY)
#define YYEMPTY         (-2)
#define YYEOF           0

#define YYACCEPT        goto yyacceptlab
#define YYABORT         goto yyabortlab
#define YYERROR         goto yyerrorlab


#define YYRECOVERING()  (!!yyerrstatus)

#define YYBACKUP(Token, Value)                                    \
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


/*-----------------------------------.
| Print this symbol's value on YYO.  |
`-----------------------------------*/

static void
yy_symbol_value_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep)
{
  FILE *yyoutput = yyo;
  YYUSE (yyoutput);
  if (!yyvaluep)
    return;
# ifdef YYPRINT
  if (yytype < YYNTOKENS)
    YYPRINT (yyo, yytoknum[yytype], *yyvaluep);
# endif
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  YYUSE (yytype);
  YY_IGNORE_MAYBE_UNINITIALIZED_END
}


/*---------------------------.
| Print this symbol on YYO.  |
`---------------------------*/

static void
yy_symbol_print (FILE *yyo, int yytype, YYSTYPE const * const yyvaluep)
{
  YYFPRINTF (yyo, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyo, yytype, yyvaluep);
  YYFPRINTF (yyo, ")");
}

/*------------------------------------------------------------------.
| yy_stack_print -- Print the state stack from its BOTTOM up to its |
| TOP (included).                                                   |
`------------------------------------------------------------------*/

static void
yy_stack_print (yy_state_t *yybottom, yy_state_t *yytop)
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
yy_reduce_print (yy_state_t *yyssp, YYSTYPE *yyvsp, int yyrule)
{
  int yylno = yyrline[yyrule];
  int yynrhs = yyr2[yyrule];
  int yyi;
  YYFPRINTF (stderr, "Reducing stack by rule %d (line %d):\n",
             yyrule - 1, yylno);
  /* The symbols being reduced.  */
  for (yyi = 0; yyi < yynrhs; yyi++)
    {
      YYFPRINTF (stderr, "   $%d = ", yyi + 1);
      yy_symbol_print (stderr,
                       yystos[+yyssp[yyi + 1 - yynrhs]],
                       &yyvsp[(yyi + 1) - (yynrhs)]
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
#   define yystrlen(S) (YY_CAST (YYPTRDIFF_T, strlen (S)))
#  else
/* Return the length of YYSTR.  */
static YYPTRDIFF_T
yystrlen (const char *yystr)
{
  YYPTRDIFF_T yylen;
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
static YYPTRDIFF_T
yytnamerr (char *yyres, const char *yystr)
{
  if (*yystr == '"')
    {
      YYPTRDIFF_T yyn = 0;
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
            else
              goto append;

          append:
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

  if (yyres)
    return yystpcpy (yyres, yystr) - yyres;
  else
    return yystrlen (yystr);
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
yysyntax_error (YYPTRDIFF_T *yymsg_alloc, char **yymsg,
                yy_state_t *yyssp, int yytoken)
{
  enum { YYERROR_VERBOSE_ARGS_MAXIMUM = 5 };
  /* Internationalized format string. */
  const char *yyformat = YY_NULLPTR;
  /* Arguments of yyformat: reported tokens (one for the "unexpected",
     one per "expected"). */
  char const *yyarg[YYERROR_VERBOSE_ARGS_MAXIMUM];
  /* Actual size of YYARG. */
  int yycount = 0;
  /* Cumulated lengths of YYARG.  */
  YYPTRDIFF_T yysize = 0;

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
      int yyn = yypact[+*yyssp];
      YYPTRDIFF_T yysize0 = yytnamerr (YY_NULLPTR, yytname[yytoken]);
      yysize = yysize0;
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
                  YYPTRDIFF_T yysize1
                    = yysize + yytnamerr (YY_NULLPTR, yytname[yyx]);
                  if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
                    yysize = yysize1;
                  else
                    return 2;
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
    default: /* Avoid compiler warnings. */
      YYCASE_(0, YY_("syntax error"));
      YYCASE_(1, YY_("syntax error, unexpected %s"));
      YYCASE_(2, YY_("syntax error, unexpected %s, expecting %s"));
      YYCASE_(3, YY_("syntax error, unexpected %s, expecting %s or %s"));
      YYCASE_(4, YY_("syntax error, unexpected %s, expecting %s or %s or %s"));
      YYCASE_(5, YY_("syntax error, unexpected %s, expecting %s or %s or %s or %s"));
# undef YYCASE_
    }

  {
    /* Don't count the "%s"s in the final size, but reserve room for
       the terminator.  */
    YYPTRDIFF_T yysize1 = yysize + (yystrlen (yyformat) - 2 * yycount) + 1;
    if (yysize <= yysize1 && yysize1 <= YYSTACK_ALLOC_MAXIMUM)
      yysize = yysize1;
    else
      return 2;
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
          ++yyp;
          ++yyformat;
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
    yy_state_fast_t yystate;
    /* Number of tokens to shift before error messages enabled.  */
    int yyerrstatus;

    /* The stacks and their tools:
       'yyss': related to states.
       'yyvs': related to semantic values.

       Refer to the stacks through separate pointers, to allow yyoverflow
       to reallocate them elsewhere.  */

    /* The state stack.  */
    yy_state_t yyssa[YYINITDEPTH];
    yy_state_t *yyss;
    yy_state_t *yyssp;

    /* The semantic value stack.  */
    YYSTYPE yyvsa[YYINITDEPTH];
    YYSTYPE *yyvs;
    YYSTYPE *yyvsp;

    YYPTRDIFF_T yystacksize;

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
  YYPTRDIFF_T yymsg_alloc = sizeof yymsgbuf;
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
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yysetstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  YYDPRINTF ((stderr, "Entering state %d\n", yystate));
  YY_ASSERT (0 <= yystate && yystate < YYNSTATES);
  YY_IGNORE_USELESS_CAST_BEGIN
  *yyssp = YY_CAST (yy_state_t, yystate);
  YY_IGNORE_USELESS_CAST_END

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYPTRDIFF_T yysize = yyssp - yyss + 1;

# if defined yyoverflow
      {
        /* Give user a chance to reallocate the stack.  Use copies of
           these so that the &'s don't force the real ones into
           memory.  */
        yy_state_t *yyss1 = yyss;
        YYSTYPE *yyvs1 = yyvs;

        /* Each stack pointer address is followed by the size of the
           data in use in that stack, in bytes.  This used to be a
           conditional around just the two extra args, but that might
           be undefined if yyoverflow is a macro.  */
        yyoverflow (YY_("memory exhausted"),
                    &yyss1, yysize * YYSIZEOF (*yyssp),
                    &yyvs1, yysize * YYSIZEOF (*yyvsp),
                    &yystacksize);
        yyss = yyss1;
        yyvs = yyvs1;
      }
# else /* defined YYSTACK_RELOCATE */
      /* Extend the stack our own way.  */
      if (YYMAXDEPTH <= yystacksize)
        goto yyexhaustedlab;
      yystacksize *= 2;
      if (YYMAXDEPTH < yystacksize)
        yystacksize = YYMAXDEPTH;

      {
        yy_state_t *yyss1 = yyss;
        union yyalloc *yyptr =
          YY_CAST (union yyalloc *,
                   YYSTACK_ALLOC (YY_CAST (YYSIZE_T, YYSTACK_BYTES (yystacksize))));
        if (! yyptr)
          goto yyexhaustedlab;
        YYSTACK_RELOCATE (yyss_alloc, yyss);
        YYSTACK_RELOCATE (yyvs_alloc, yyvs);
# undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YY_IGNORE_USELESS_CAST_BEGIN
      YYDPRINTF ((stderr, "Stack size increased to %ld\n",
                  YY_CAST (long, yystacksize)));
      YY_IGNORE_USELESS_CAST_END

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */

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
  yystate = yyn;
  YY_IGNORE_MAYBE_UNINITIALIZED_BEGIN
  *++yyvsp = yylval;
  YY_IGNORE_MAYBE_UNINITIALIZED_END

  /* Discard the shifted token.  */
  yychar = YYEMPTY;
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
| yyreduce -- do a reduction.  |
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
#line 3311 "parser.y"
  {
	clear_initial_values ();
	current_program = NULL;
	defined_prog_list = NULL;
	cobc_cs_check = 0;
	main_flag_set = 0;
	current_program = cb_build_program (NULL, 0);
	cb_set_intr_when_compiled ();
	cb_build_registers ();
	cb_add_external_defined_registers ();
  }
#line 12333 "parser.c"
    break;

  case 3:
#line 3323 "parser.y"
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
#line 12357 "parser.c"
    break;

  case 6:
#line 3350 "parser.y"
  {
	first_prog = 1;
	depth = 0;
	setup_from_identification = 0;
  }
#line 12367 "parser.c"
    break;

  case 12:
#line 3369 "parser.y"
  {
	program_init_without_program_id ();
  }
#line 12375 "parser.c"
    break;

  case 13:
#line 3374 "parser.y"
  {
	backup_current_pos ();
	clean_up_program (NULL, COB_MODULE_TYPE_PROGRAM);
  }
#line 12384 "parser.c"
    break;

  case 16:
#line 3400 "parser.y"
  {
	backup_current_pos ();
	clean_up_program (NULL, COB_MODULE_TYPE_PROGRAM);
  }
#line 12393 "parser.c"
    break;

  case 20:
#line 3414 "parser.y"
  {
	backup_current_pos ();
  }
#line 12401 "parser.c"
    break;

  case 21:
#line 3418 "parser.y"
  {
	first_nested_program = 0;
	clean_up_program (yyvsp[-1], COB_MODULE_TYPE_PROGRAM);
  }
#line 12410 "parser.c"
    break;

  case 22:
#line 3426 "parser.y"
  {
	backup_current_pos ();
  }
#line 12418 "parser.c"
    break;

  case 23:
#line 3430 "parser.y"
  {
	clean_up_program (yyvsp[-1], COB_MODULE_TYPE_FUNCTION);
  }
#line 12426 "parser.c"
    break;

  case 24:
#line 3440 "parser.y"
  {
	cb_validate_program_environment (current_program);
  }
#line 12434 "parser.c"
    break;

  case 25:
#line 3444 "parser.y"
  {
	/* note:
	   we also validate all references we found so far here */
	cb_validate_program_data (current_program);
	within_typedef_definition = 0;
  }
#line 12445 "parser.c"
    break;

  case 29:
#line 3462 "parser.y"
  {
	setup_program_start ();
	setup_from_identification = 1;
  }
#line 12454 "parser.c"
    break;

  case 32:
#line 3475 "parser.y"
  {
	cobc_in_id = 1;
  }
#line 12462 "parser.c"
    break;

  case 33:
#line 3479 "parser.y"
  {
	if (setup_program (yyvsp[-1], yyvsp[0], COB_MODULE_TYPE_PROGRAM)) {
		YYABORT;
	}

	setup_prototype (yyvsp[-1], yyvsp[0], COB_MODULE_TYPE_PROGRAM, 1);
  }
#line 12474 "parser.c"
    break;

  case 34:
#line 3487 "parser.y"
  {
	cobc_cs_check = 0;
	cobc_in_id = 0;
  }
#line 12483 "parser.c"
    break;

  case 35:
#line 3495 "parser.y"
  {
	cobc_in_id = 1;
  }
#line 12491 "parser.c"
    break;

  case 36:
#line 3499 "parser.y"
  {
	if (setup_program (yyvsp[-2], yyvsp[-1], COB_MODULE_TYPE_FUNCTION)) {
		YYABORT;
	}
	setup_prototype (yyvsp[-2], yyvsp[-1], COB_MODULE_TYPE_FUNCTION, 1);
	cobc_cs_check = 0;
	cobc_in_id = 0;
  }
#line 12504 "parser.c"
    break;

  case 37:
#line 3511 "parser.y"
  {
	if (CB_REFERENCE_P (yyvsp[0]) && CB_WORD_COUNT (yyvsp[0]) > 0) {
		redefinition_error (yyvsp[0]);
	}
	/*
	  The program name is a key part of defining the current_program, so we
	  mustn't lose it (unlike in undefined_word).
	*/
	yyval = yyvsp[0];
  }
#line 12519 "parser.c"
    break;

  case 38:
#line 3522 "parser.y"
  {
	cb_trim_program_id (yyvsp[0]);
  }
#line 12527 "parser.c"
    break;

  case 40:
#line 3530 "parser.y"
  {
	cb_trim_program_id (yyvsp[0]);
  }
#line 12535 "parser.c"
    break;

  case 41:
#line 3536 "parser.y"
                                { yyval = NULL; }
#line 12541 "parser.c"
    break;

  case 42:
#line 3537 "parser.y"
                                { yyval = yyvsp[0]; }
#line 12547 "parser.c"
    break;

  case 45:
#line 3546 "parser.y"
  {
	if (!current_program->nested_level) {
		cb_error (_("COMMON may only be used in a contained program"));
	} else {
		current_program->flag_common = 1;
		cb_add_common_prog (current_program);
	}
  }
#line 12560 "parser.c"
    break;

  case 46:
#line 3555 "parser.y"
  {
	if (!current_program->nested_level) {
		cb_error (_("COMMON may only be used in a contained program"));
	} else {
		current_program->flag_common = 1;
		cb_add_common_prog (current_program);
	}
  }
#line 12573 "parser.c"
    break;

  case 48:
#line 3565 "parser.y"
  {
	CB_PENDING (_("CALL prototypes"));
  }
#line 12581 "parser.c"
    break;

  case 51:
#line 3577 "parser.y"
  {
	current_program->flag_initial = 1;
  }
#line 12589 "parser.c"
    break;

  case 52:
#line 3581 "parser.y"
  {
	current_program->flag_recursive = 1;
  }
#line 12597 "parser.c"
    break;

  case 54:
#line 3590 "parser.y"
  {
	cobc_cs_check = 0;
  }
#line 12605 "parser.c"
    break;

  case 58:
#line 3610 "parser.y"
  {
/* FIXME: the IBM-compatible ARITHMETIC should only be disabled
          for the specified program (and its nested programs)
   note: ibm-strict.conf has no OPTIONS paragraph, but ibm.conf does */
	cb_arithmetic_osvs = 0;
  }
#line 12616 "parser.c"
    break;

  case 59:
#line 3617 "parser.y"
  {
	CB_PENDING ("STANDARD ARITHMETIC");
  }
#line 12624 "parser.c"
    break;

  case 60:
#line 3621 "parser.y"
  {
	CB_PENDING ("STANDARD-BINARY ARITHMETIC");
  }
#line 12632 "parser.c"
    break;

  case 61:
#line 3625 "parser.y"
  {
	CB_PENDING ("STANDARD-DECIMAL ARITHMETIC");
  }
#line 12640 "parser.c"
    break;

  case 62:
#line 3640 "parser.y"
  {
	default_rounded_mode = cb_int (COB_STORE_ROUND);
  }
#line 12648 "parser.c"
    break;

  case 63:
#line 3644 "parser.y"
  {
	if (yyvsp[0]) {
		default_rounded_mode = yyvsp[0];
	} else {
		default_rounded_mode = cb_int (COB_STORE_ROUND);
	}
  }
#line 12660 "parser.c"
    break;

  case 65:
#line 3656 "parser.y"
  {
	current_program->entry_convention = yyvsp[0];
  }
#line 12668 "parser.c"
    break;

  case 66:
#line 3663 "parser.y"
  {
	yyval = cb_int (CB_CONV_COBOL);
  }
#line 12676 "parser.c"
    break;

  case 67:
#line 3667 "parser.y"
  {
	yyval = cb_int0;
  }
#line 12684 "parser.c"
    break;

  case 68:
#line 3671 "parser.y"
  {
	yyval = cb_int (CB_CONV_STDCALL);
  }
#line 12692 "parser.c"
    break;

  case 70:
#line 3679 "parser.y"
  {
	CB_PENDING ("INTERMEDIATE ROUNDING");
  }
#line 12700 "parser.c"
    break;

  case 71:
#line 3686 "parser.y"
  {
	yyval = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_AWAY_FROM_ZERO);
  }
#line 12708 "parser.c"
    break;

  case 72:
#line 3690 "parser.y"
  {
	yyval = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_EVEN);
  }
#line 12716 "parser.c"
    break;

  case 73:
#line 3694 "parser.y"
  {
	yyval = cb_int (COB_STORE_ROUND | COB_STORE_PROHIBITED);
  }
#line 12724 "parser.c"
    break;

  case 74:
#line 3698 "parser.y"
  {
	yyval = cb_int (COB_STORE_ROUND | COB_STORE_TRUNCATION);
  }
#line 12732 "parser.c"
    break;

  case 77:
#line 3713 "parser.y"
  {
	header_check |= COBC_HD_ENVIRONMENT_DIVISION;
  }
#line 12740 "parser.c"
    break;

  case 80:
#line 3727 "parser.y"
  {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_CONFIGURATION_SECTION;
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "CONFIGURATION SECTION");
	}
  }
#line 12752 "parser.c"
    break;

  case 90:
#line 3758 "parser.y"
  {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION, 0, 0);
	check_conf_section_order (COBC_HD_SOURCE_COMPUTER);
	set_conf_section_part (COBC_HD_SOURCE_COMPUTER);
  }
#line 12763 "parser.c"
    break;

  case 95:
#line 3774 "parser.y"
  {
	current_program->flag_debugging = 1;
	needs_debug_item = 1;
	cobc_cs_check = 0;
	cb_build_debug_item ();
  }
#line 12774 "parser.c"
    break;

  case 96:
#line 3786 "parser.y"
  {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION, 0, 0);
	check_conf_section_order (COBC_HD_OBJECT_COMPUTER);
	set_conf_section_part (COBC_HD_OBJECT_COMPUTER);
  }
#line 12785 "parser.c"
    break;

  case 97:
#line 3793 "parser.y"
  {
	cobc_cs_check = 0;
  }
#line 12793 "parser.c"
    break;

  case 108:
#line 3819 "parser.y"
  {
	cb_verify (cb_memory_size_clause, "MEMORY SIZE");
  }
#line 12801 "parser.c"
    break;

  case 109:
#line 3827 "parser.y"
  {
	current_program->collating_sequence = alphanumeric_collation;
	current_program->collating_sequence_n = national_collation;
  }
#line 12810 "parser.c"
    break;

  case 110:
#line 3835 "parser.y"
  {
	alphanumeric_collation = national_collation = NULL;
  }
#line 12818 "parser.c"
    break;

  case 112:
#line 3843 "parser.y"
  {
	alphanumeric_collation = yyvsp[0];
  }
#line 12826 "parser.c"
    break;

  case 113:
#line 3847 "parser.y"
  {
	alphanumeric_collation = yyvsp[-1];
	CB_PENDING_X (yyvsp[0], "NATIONAL COLLATING SEQUENCE");
	national_collation = yyvsp[0];
  }
#line 12836 "parser.c"
    break;

  case 114:
#line 3853 "parser.y"
  {
	alphanumeric_collation = yyvsp[0];
  }
#line 12844 "parser.c"
    break;

  case 115:
#line 3857 "parser.y"
  {
	CB_PENDING_X (yyvsp[0], "NATIONAL COLLATING SEQUENCE");
	national_collation = yyvsp[0];
  }
#line 12853 "parser.c"
    break;

  case 116:
#line 3863 "parser.y"
  {
	alphanumeric_collation = yyvsp[-4];
	CB_PENDING_X (yyvsp[0], "NATIONAL COLLATING SEQUENCE");
	national_collation = yyvsp[0];
  }
#line 12863 "parser.c"
    break;

  case 117:
#line 3870 "parser.y"
  {
	CB_PENDING_X (yyvsp[-4], "NATIONAL COLLATING SEQUENCE");
	national_collation = yyvsp[-4];
	alphanumeric_collation = yyvsp[0];
  }
#line 12873 "parser.c"
    break;

  case 118:
#line 3879 "parser.y"
  {
	if (cb_verify (cb_section_segments, "SEGMENT LIMIT")) {
		int segnum = cb_get_int (yyvsp[0]);
		if (segnum == 0 || segnum > 49) {
			cb_error (_("segment-number must be in range of values 1 to 49"));
			yyval = NULL;
		}
	}
	/* Ignore */
  }
#line 12888 "parser.c"
    break;

  case 119:
#line 3893 "parser.y"
  {
	if (current_program->classification) {
		cb_error (_("duplicate CLASSIFICATION clause"));
	} else {
		current_program->classification = yyvsp[0];
	}
  }
#line 12900 "parser.c"
    break;

  case 120:
#line 3904 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 12908 "parser.c"
    break;

  case 121:
#line 3908 "parser.y"
  {
	yyval = NULL;
  }
#line 12916 "parser.c"
    break;

  case 122:
#line 3912 "parser.y"
  {
	yyval = cb_int1;
  }
#line 12924 "parser.c"
    break;

  case 123:
#line 3916 "parser.y"
  {
	yyval = cb_int1;
  }
#line 12932 "parser.c"
    break;

  case 126:
#line 3930 "parser.y"
  {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION, 0, 0);
	check_conf_section_order (COBC_HD_REPOSITORY);
	set_conf_section_part (COBC_HD_REPOSITORY);
  }
#line 12943 "parser.c"
    break;

  case 127:
#line 3937 "parser.y"
  {
	cobc_in_repository = 0;
  }
#line 12951 "parser.c"
    break;

  case 130:
#line 3946 "parser.y"
  {
	yyerrok;
  }
#line 12959 "parser.c"
    break;

  case 133:
#line 3958 "parser.y"
  {
	functions_are_all = 1;
  }
#line 12967 "parser.c"
    break;

  case 134:
#line 3962 "parser.y"
  {
	if (yyvsp[-1] != cb_error_node) {
		setup_prototype (yyvsp[-1], yyvsp[0], COB_MODULE_TYPE_FUNCTION, 0);
	}
  }
#line 12977 "parser.c"
    break;

  case 136:
#line 3969 "parser.y"
  {
	  if (yyvsp[-1] != cb_error_node
	      && cb_verify (cb_program_prototypes, _("PROGRAM phrase"))) {
		setup_prototype (yyvsp[-1], yyvsp[0], COB_MODULE_TYPE_PROGRAM, 0);
	}
  }
#line 12988 "parser.c"
    break;

  case 137:
#line 3976 "parser.y"
  {
	yyerrok;
  }
#line 12996 "parser.c"
    break;

  case 138:
#line 3983 "parser.y"
  {
	current_program->function_spec_list =
		cb_list_add (current_program->function_spec_list, yyvsp[0]);
  }
#line 13005 "parser.c"
    break;

  case 139:
#line 3988 "parser.y"
  {
	current_program->function_spec_list =
		cb_list_add (current_program->function_spec_list, yyvsp[0]);
  }
#line 13014 "parser.c"
    break;

  case 140:
#line 3999 "parser.y"
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
#line 13029 "parser.c"
    break;

  case 159:
#line 4044 "parser.y"
  {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	check_duplicate = 0;
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
		save_tree = NULL;
	} else {
		/* lookup system name with special translation
		   note: result in NULL + raised error if not found */
		save_tree = get_system_name_translated (yyvsp[0]);
	}
  }
#line 13048 "parser.c"
    break;

  case 161:
#line 4063 "parser.y"
  {
	if (save_tree) {
		if (CB_SYSTEM_NAME(save_tree)->token != CB_DEVICE_CONSOLE) {
			cb_error_x (save_tree, _("invalid %s clause"), "");
		} else {
			current_program->flag_console_is_crt = 1;
		}
	}
  }
#line 13062 "parser.c"
    break;

  case 162:
#line 4074 "parser.y"
  {
	if (save_tree) {
		if (CB_SYSTEM_NAME(save_tree)->token != CB_FEATURE_CONVENTION) {
			cb_error_x (save_tree, _("invalid %s clause"), "SPECIAL NAMES");
		} else if (CB_VALID_TREE (yyvsp[0])) {
			CB_SYSTEM_NAME(save_tree)->value = yyvsp[-2];
			cb_define (yyvsp[0], save_tree);
			CB_CHAIN_PAIR (current_program->mnemonic_spec_list,
					yyvsp[0], save_tree);
			/* remove non-standard context-sensitive words when identical to mnemonic */
			if (strcasecmp (CB_NAME(yyvsp[0]), "EXTERN" ) == 0 ||
			    strcasecmp (CB_NAME(yyvsp[0]), "STDCALL") == 0 ||
			    strcasecmp (CB_NAME(yyvsp[0]), "STATIC" ) == 0 ||
			    strcasecmp (CB_NAME(yyvsp[0]), "C"      ) == 0 ||
			    strcasecmp (CB_NAME(yyvsp[0]), "PASCAL" ) == 0) {
				remove_context_sensitivity (CB_NAME(yyvsp[0]), CB_CS_CALL);
			}
		}
	}
  }
#line 13087 "parser.c"
    break;

  case 163:
#line 4095 "parser.y"
  {
	if (save_tree && CB_VALID_TREE (yyvsp[-1])) {
		cb_define (yyvsp[-1], save_tree);
		CB_CHAIN_PAIR (current_program->mnemonic_spec_list,
				yyvsp[-1], save_tree);
	}
  }
#line 13099 "parser.c"
    break;

  case 167:
#line 4111 "parser.y"
  {
	  check_on_off_duplicate = 0;
  }
#line 13107 "parser.c"
    break;

  case 168:
#line 4118 "parser.y"
  {
	cb_tree		x;

	/* cb_define_switch_name checks param validity */
	x = cb_define_switch_name (yyvsp[0], save_tree, yyvsp[-2] == cb_int1);
	if (x) {
		if (yyvsp[-2] == cb_int1) {
			check_repeated ("ON", SYN_CLAUSE_1, &check_on_off_duplicate);
		} else {
			check_repeated ("OFF", SYN_CLAUSE_2, &check_on_off_duplicate);
		}
		CB_CHAIN_PAIR (current_program->mnemonic_spec_list, yyvsp[0], x);
	}
  }
#line 13126 "parser.c"
    break;

  case 169:
#line 4133 "parser.y"
  {
	cb_tree		x;

	/* cb_define_switch_name checks param validity */
	x = cb_define_switch_name (yyvsp[0], save_tree, yyvsp[-2] == cb_int1);
	if (x) {
		if (yyvsp[-2] == cb_int1) {
			check_repeated ("ON", SYN_CLAUSE_1, &check_on_off_duplicate);
		} else {
			check_repeated ("OFF", SYN_CLAUSE_2, &check_on_off_duplicate);
		}
		CB_CHAIN_PAIR (current_program->mnemonic_spec_list, yyvsp[0], x);
	}
  }
#line 13145 "parser.c"
    break;

  case 170:
#line 4153 "parser.y"
  {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
		yyval = NULL;
	} else {
		/* Returns null on error */
		yyval = cb_build_alphabet_name (yyvsp[0]);
	}
  }
#line 13162 "parser.c"
    break;

  case 171:
#line 4166 "parser.y"
  {
	if (yyvsp[-1]) {
		current_program->alphabet_name_list =
			cb_list_add (current_program->alphabet_name_list, yyvsp[-1]);
	}
	cobc_cs_check = 0;
  }
#line 13174 "parser.c"
    break;

  case 172:
#line 4177 "parser.y"
  {
	yyval = yyvsp[-1];
	if (yyvsp[-1]) {
		CB_ALPHABET_NAME (yyvsp[-1])->alphabet_target = CB_ALPHABET_ALPHANUMERIC;
	}
  }
#line 13185 "parser.c"
    break;

  case 174:
#line 4185 "parser.y"
  {
	yyval = yyvsp[-1];
	if (yyvsp[-1]) {
		CB_ALPHABET_NAME(yyvsp[-1])->alphabet_target = CB_ALPHABET_NATIONAL;
	}
  }
#line 13196 "parser.c"
    break;

  case 180:
#line 4206 "parser.y"
  {
	if (yyvsp[(-1) - (1)]) {
		CB_ALPHABET_NAME (yyvsp[(-1) - (1)])->alphabet_type = CB_ALPHABET_ASCII;
	}
  }
#line 13206 "parser.c"
    break;

  case 181:
#line 4212 "parser.y"
  {
	if (yyvsp[(-1) - (1)]) {
		CB_ALPHABET_NAME (yyvsp[(-1) - (1)])->alphabet_type = CB_ALPHABET_ASCII;
	}
  }
#line 13216 "parser.c"
    break;

  case 182:
#line 4218 "parser.y"
  {
	if (yyvsp[(-1) - (1)]) {
		CB_ALPHABET_NAME (yyvsp[(-1) - (1)])->alphabet_type = CB_ALPHABET_EBCDIC;
	}
  }
#line 13226 "parser.c"
    break;

  case 183:
#line 4224 "parser.y"
  {
	if (yyvsp[(-1) - (1)]) {
		CB_ALPHABET_NAME (yyvsp[(-1) - (1)])->alphabet_type = CB_ALPHABET_ASCII;
	}
  }
#line 13236 "parser.c"
    break;

  case 185:
#line 4234 "parser.y"
  {
	if (yyvsp[(-1) - (1)]) {
		CB_PENDING_X (yyvsp[(-1) - (1)], "ALPHABET UCS-4");
		CB_ALPHABET_NAME (yyvsp[(-1) - (1)])->alphabet_type = CB_ALPHABET_UCS_4;
	}
  }
#line 13247 "parser.c"
    break;

  case 186:
#line 4241 "parser.y"
  {
	if (yyvsp[(-1) - (1)]) {
		CB_PENDING_X (yyvsp[(-1) - (1)], "ALPHABET UTF-8");
		CB_ALPHABET_NAME (yyvsp[(-1) - (1)])->alphabet_type = CB_ALPHABET_UTF_8;
	}
  }
#line 13258 "parser.c"
    break;

  case 187:
#line 4248 "parser.y"
  {
	if (yyvsp[(-1) - (1)]) {
		CB_PENDING_X (yyvsp[(-1) - (1)], "ALPHABET UTF-16");
		CB_ALPHABET_NAME (yyvsp[(-1) - (1)])->alphabet_type = CB_ALPHABET_UTF_16;
	}
  }
#line 13269 "parser.c"
    break;

  case 188:
#line 4258 "parser.y"
  {
	if (yyvsp[(-1) - (1)]) {
		CB_ALPHABET_NAME (yyvsp[(-1) - (1)])->alphabet_type = CB_ALPHABET_NATIVE;
	}
  }
#line 13279 "parser.c"
    break;

  case 189:
#line 4264 "parser.y"
  {
	if (yyvsp[(-1) - (2)]) {
		CB_ALPHABET_NAME (yyvsp[(-1) - (2)])->alphabet_type = CB_ALPHABET_LOCALE;
		CB_ALPHABET_NAME (yyvsp[(-1) - (2)])->custom_list = yyvsp[0];
		CB_PENDING_X (yyvsp[(-1) - (2)], "LOCALE ALPHABET");
	}
  }
#line 13291 "parser.c"
    break;

  case 190:
#line 4272 "parser.y"
  {
	if (yyvsp[(-1) - (1)]) {
		CB_ALPHABET_NAME (yyvsp[(-1) - (1)])->alphabet_type = CB_ALPHABET_CUSTOM;
		CB_ALPHABET_NAME (yyvsp[(-1) - (1)])->custom_list = yyvsp[0];
	}
  }
#line 13302 "parser.c"
    break;

  case 191:
#line 4282 "parser.y"
  {
	yyval = CB_LIST_INIT (yyvsp[0]);
  }
#line 13310 "parser.c"
    break;

  case 192:
#line 4286 "parser.y"
  {
	yyval = cb_list_add (yyvsp[-1], yyvsp[0]);
  }
#line 13318 "parser.c"
    break;

  case 193:
#line 4293 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 13326 "parser.c"
    break;

  case 194:
#line 4297 "parser.y"
  {
	yyval = CB_BUILD_PAIR (yyvsp[-2], yyvsp[0]);
  }
#line 13334 "parser.c"
    break;

  case 195:
#line 4301 "parser.y"
  {
	yyval = CB_LIST_INIT (yyvsp[-1]);
  }
#line 13342 "parser.c"
    break;

  case 196:
#line 4305 "parser.y"
  {
	yyval = yyvsp[-1];
  }
#line 13350 "parser.c"
    break;

  case 197:
#line 4312 "parser.y"
  {
	cb_list_add (yyvsp[-1], yyvsp[0]);
  }
#line 13358 "parser.c"
    break;

  case 198:
#line 4316 "parser.y"
  {
	cb_list_add (yyvsp[-3], yyvsp[0]);
  }
#line 13366 "parser.c"
    break;

  case 199:
#line 4322 "parser.y"
                                { yyval = yyvsp[0]; }
#line 13372 "parser.c"
    break;

  case 200:
#line 4323 "parser.y"
                                { yyval = cb_space; }
#line 13378 "parser.c"
    break;

  case 201:
#line 4324 "parser.y"
                                { yyval = cb_zero; }
#line 13384 "parser.c"
    break;

  case 202:
#line 4325 "parser.y"
                                { yyval = cb_quote; }
#line 13390 "parser.c"
    break;

  case 203:
#line 4326 "parser.y"
                                { yyval = cb_norm_high; }
#line 13396 "parser.c"
    break;

  case 204:
#line 4327 "parser.y"
                                { yyval = cb_norm_low; }
#line 13402 "parser.c"
    break;

  case 205:
#line 4331 "parser.y"
                                { yyval = cb_space; }
#line 13408 "parser.c"
    break;

  case 206:
#line 4332 "parser.y"
                                { yyval = cb_zero; }
#line 13414 "parser.c"
    break;

  case 207:
#line 4340 "parser.y"
  {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else if (yyvsp[-1]) {
		CB_CHAIN_PAIR (current_program->symbolic_char_list, yyvsp[-1], yyvsp[0]);
	}
  }
#line 13429 "parser.c"
    break;

  case 208:
#line 4354 "parser.y"
  {
	yyval = NULL;
  }
#line 13437 "parser.c"
    break;

  case 209:
#line 4358 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 13445 "parser.c"
    break;

  case 210:
#line 4366 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 13453 "parser.c"
    break;

  case 211:
#line 4373 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 13461 "parser.c"
    break;

  case 212:
#line 4377 "parser.y"
  {
	if (yyvsp[0]) {
		yyval = cb_list_append (yyvsp[-1], yyvsp[0]);
	} else {
		yyval = yyvsp[-1];
	}
  }
#line 13473 "parser.c"
    break;

  case 213:
#line 4388 "parser.y"
  {
	cb_tree		l1;
	cb_tree		l2;

	if (cb_list_length (yyvsp[-2]) != cb_list_length (yyvsp[0])) {
		cb_error (_("invalid %s clause"), "SYMBOLIC");
		yyval = NULL;
	} else {
		l1 = yyvsp[-2];
		l2 = yyvsp[0];
		for (; l1; l1 = CB_CHAIN (l1), l2 = CB_CHAIN (l2)) {
			CB_PURPOSE (l1) = CB_VALUE (l2);
		}
		yyval = yyvsp[-2];
	}
  }
#line 13494 "parser.c"
    break;

  case 214:
#line 4408 "parser.y"
  {
	if (yyvsp[0] == NULL) {
		yyval = NULL;
	} else {
		yyval = CB_LIST_INIT (yyvsp[0]);
	}
  }
#line 13506 "parser.c"
    break;

  case 215:
#line 4416 "parser.y"
  {
	if (yyvsp[0] == NULL) {
		yyval = yyvsp[-1];
	} else {
		yyval = cb_list_add (yyvsp[-1], yyvsp[0]);
	}
  }
#line 13518 "parser.c"
    break;

  case 216:
#line 4426 "parser.y"
                                { yyval = CB_LIST_INIT (yyvsp[0]); }
#line 13524 "parser.c"
    break;

  case 217:
#line 4427 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 13530 "parser.c"
    break;

  case 218:
#line 4436 "parser.y"
  {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	}
	(void)cb_verify (cb_symbolic_constant, "SYMBOLIC CONSTANT");
  }
#line 13544 "parser.c"
    break;

  case 221:
#line 4454 "parser.y"
  {
	struct cb_field *f;
	cb_tree v;

	v = CB_LIST_INIT (yyvsp[0]);
	f = CB_FIELD (cb_build_constant (yyvsp[-2], v));
	f->flag_item_78 = 1;
	f->flag_constant = 1;
	f->flag_is_global = 1;
	f->level = 1;
	f->values = v;
	cb_needs_01 = 1;
	/* Ignore return value */
	(void)cb_validate_78_item (f, 0);
  }
#line 13564 "parser.c"
    break;

  case 222:
#line 4475 "parser.y"
  {
	cb_tree		x;

	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		/* Returns null on error */
		x = cb_build_class_name (yyvsp[-4], yyvsp[-1]);
		if (x) {
			current_program->class_name_list =
				cb_list_add (current_program->class_name_list, x);
		}
	}
  }
#line 13586 "parser.c"
    break;

  case 223:
#line 4495 "parser.y"
                                { yyval = CB_LIST_INIT (yyvsp[0]); }
#line 13592 "parser.c"
    break;

  case 224:
#line 4496 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 13598 "parser.c"
    break;

  case 225:
#line 4501 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 13606 "parser.c"
    break;

  case 226:
#line 4505 "parser.y"
  {
	if (CB_TREE_CLASS (yyvsp[-2]) != CB_CLASS_NUMERIC &&
	    CB_LITERAL_P (yyvsp[-2]) && CB_LITERAL (yyvsp[-2])->size != 1) {
		cb_error (_("CLASS literal with THRU must have size 1"));
	}
	if (CB_TREE_CLASS (yyvsp[0]) != CB_CLASS_NUMERIC &&
	    CB_LITERAL_P (yyvsp[0]) && CB_LITERAL (yyvsp[0])->size != 1) {
		cb_error (_("CLASS literal with THRU must have size 1"));
	}
	if (literal_value (yyvsp[-2]) <= literal_value (yyvsp[0])) {
		yyval = CB_BUILD_PAIR (yyvsp[-2], yyvsp[0]);
	} else {
		yyval = CB_BUILD_PAIR (yyvsp[0], yyvsp[-2]);
	}
  }
#line 13626 "parser.c"
    break;

  case 228:
#line 4525 "parser.y"
  {
	yyval = NULL;
  }
#line 13634 "parser.c"
    break;

  case 229:
#line 4529 "parser.y"
  {
	CB_PENDING_X (yyvsp[0], "NATIONAL CLASS");
	yyval = cb_int0;
  }
#line 13643 "parser.c"
    break;

  case 231:
#line 4538 "parser.y"
  {
	CB_PENDING_X (yyvsp[0], _("CLASS IS integer IN alphabet-name"));
	yyval = yyvsp[0];
  }
#line 13652 "parser.c"
    break;

  case 232:
#line 4548 "parser.y"
  {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		/* Returns null on error */
		cb_tree	l = cb_build_locale_name (yyvsp[-2], yyvsp[0]);
		if (l) {
			current_program->locale_list =
				cb_list_add (current_program->locale_list, l);
		}
	}
  }
#line 13672 "parser.c"
    break;

  case 233:
#line 4569 "parser.y"
  {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		unsigned int	error_ind = 0;

		/* FIXME: actual allowed (depending on dialect), see FR #246 */
		check_repeated ("CURRENCY", SYN_CLAUSE_1, &check_duplicate);

		/* checks of CURRENCY SIGN (being currency string) when separate */
		if (yyvsp[0]) {
			unsigned int	char_seen = 0;
			unsigned char	*s = CB_LITERAL (yyvsp[-1])->data;

			CB_PENDING_X (yyvsp[-1], _("separate currency symbol and currency string"));
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
			cb_error_x (yyvsp[-1], _("invalid CURRENCY SIGN '%s'"), (char*)CB_LITERAL (yyvsp[-1])->data);
		}
		if (yyvsp[0]) {
			set_currency_picture_symbol (yyvsp[0]);
		} else {
			if (!error_ind) {
				set_currency_picture_symbol (yyvsp[-1]);
			}
		}
	}
  }
#line 13738 "parser.c"
    break;

  case 234:
#line 4635 "parser.y"
  {
	yyval = NULL;
  }
#line 13746 "parser.c"
    break;

  case 235:
#line 4639 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 13754 "parser.c"
    break;

  case 236:
#line 4648 "parser.y"
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
#line 13771 "parser.c"
    break;

  case 237:
#line 4667 "parser.y"
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
#line 13786 "parser.c"
    break;

  case 238:
#line 4683 "parser.y"
  {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("CURSOR", SYN_CLAUSE_3, &check_duplicate);
		current_program->cursor_pos = yyvsp[0];
	}
  }
#line 13802 "parser.c"
    break;

  case 239:
#line 4701 "parser.y"
  {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		check_repeated ("CRT STATUS", SYN_CLAUSE_4, &check_duplicate);
		current_program->crt_status = yyvsp[0];
	}
  }
#line 13818 "parser.c"
    break;

  case 240:
#line 4719 "parser.y"
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
#line 13834 "parser.c"
    break;

  case 241:
#line 4736 "parser.y"
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
#line 13850 "parser.c"
    break;

  case 242:
#line 4753 "parser.y"
  {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_CONFIGURATION_SECTION,
			       COBC_HD_SPECIAL_NAMES, 0);
	check_duplicate = 0;
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
		save_tree = NULL;
	} else {
		/* lookup system name
		   note: result in NULL + raised error if not found */
		save_tree = get_system_name ("TOP");
	}
  }
#line 13869 "parser.c"
    break;

  case 243:
#line 4768 "parser.y"
  {
	if (save_tree && CB_VALID_TREE (yyvsp[0])) {
		cb_define (yyvsp[0], save_tree);
		CB_CHAIN_PAIR (current_program->mnemonic_spec_list,
				yyvsp[0], save_tree);
	}
  }
#line 13881 "parser.c"
    break;

  case 246:
#line 4788 "parser.y"
  {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_INPUT_OUTPUT_SECTION;
  }
#line 13890 "parser.c"
    break;

  case 248:
#line 4798 "parser.y"
  {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_INPUT_OUTPUT_SECTION, 0, 0);
	header_check |= COBC_HD_FILE_CONTROL;
  }
#line 13900 "parser.c"
    break;

  case 251:
#line 4811 "parser.y"
  {
	char	buff[COB_MINI_BUFF];
	  
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_INPUT_OUTPUT_SECTION,
			       COBC_HD_FILE_CONTROL, 0);
	check_duplicate = 0;
	if (CB_VALID_TREE (yyvsp[0])) {
		/* Build new file */
		current_file = build_file (yyvsp[0]);
		current_file->optional = CB_INTEGER (yyvsp[-1])->val;

		/* Add file to current program list */
		CB_ADD_TO_CHAIN (CB_TREE (current_file),
				 current_program->file_list);
	} else {
		/* Create dummy file */
		snprintf (buff, COB_MINI_BUFF, "SELECT on line %d",
			  cb_source_line);
		current_file = build_file (cb_build_reference (buff));
		CB_ADD_TO_CHAIN (CB_TREE (current_file),
				 current_program->file_list);

	}
	key_type = NO_KEY;
  }
#line 13931 "parser.c"
    break;

  case 252:
#line 4838 "parser.y"
  {
	cobc_cs_check = 0;
	if (CB_VALID_TREE (yyvsp[-2])) {
		if (current_file->organization == COB_ORG_INDEXED
		    && key_type == RELATIVE_KEY) {
			cb_error_x (current_file->key,
				    _("cannot use RELATIVE KEY clause on INDEXED files"));
		} else if (current_file->organization == COB_ORG_RELATIVE
			   && key_type == RECORD_KEY) {
			cb_error_x (current_file->key,
				    _("cannot use RECORD KEY clause on RELATIVE files"));
		}

		validate_file (current_file, yyvsp[-2]);
	}
  }
#line 13952 "parser.c"
    break;

  case 254:
#line 4859 "parser.y"
  {
	yyerrok;
  }
#line 13960 "parser.c"
    break;

  case 256:
#line 4866 "parser.y"
  {
	/* reset context-sensitive words for next clauses */
	cobc_cs_check = CB_CS_SELECT;
  }
#line 13969 "parser.c"
    break;

  case 276:
#line 4919 "parser.y"
  {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	if (ext_dyn_specified) {
		cb_error (_("EXTERNAL/DYNAMIC cannot be used with literals"));
	}

	current_file->assign_type = CB_ASSIGN_EXT_FILE_NAME_REQUIRED;
	current_file->assign = cb_build_assignment_name (current_file, yyvsp[0]);
  }
#line 13983 "parser.c"
    break;

  case 277:
#line 4929 "parser.y"
  {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);

	/* current_file->assign_type is set by _ext_clause */
	if (!ext_dyn_specified) {
		current_file->flag_assign_no_keyword = 1;
	}
	current_file->assign = cb_build_assignment_name (current_file, yyvsp[0]);
  }
#line 13997 "parser.c"
    break;

  case 278:
#line 4939 "parser.y"
  {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	if (ext_dyn_specified) {
		cb_error (_("EXTERNAL/DYNAMIC cannot be used with USING/VARYING"));
	}
        cb_verify (cb_assign_using_variable, "ASSIGN USING/VARYING variable");

	current_file->assign_type = CB_ASSIGN_VARIABLE_REQUIRED;
	current_file->assign = cb_build_assignment_name (current_file, yyvsp[0]);
  }
#line 14012 "parser.c"
    break;

  case 279:
#line 4950 "parser.y"
  {
	check_repeated ("ASSIGN", SYN_CLAUSE_1, &check_duplicate);
	if (ext_dyn_specified) {
		cb_error (_("EXTERNAL/DYNAMIC cannot be used with DISK FROM"));
	}
	cb_verify (cb_assign_disk_from, _("ASSIGN DISK FROM"));

	current_file->assign_type = CB_ASSIGN_VARIABLE_REQUIRED;
	current_file->assign = cb_build_assignment_name (current_file, yyvsp[0]);
  }
#line 14027 "parser.c"
    break;

  case 280:
#line 4961 "parser.y"
  {
	if (assign_device == CB_ASSIGN_DISPLAY_DEVICE) {
		current_file->assign =
			cb_build_alphanumeric_literal ("stdout", (size_t)6);
		current_file->special = COB_SELECT_STDOUT;
	} else if (assign_device == CB_ASSIGN_KEYBOARD_DEVICE) {
		current_file->assign =
			cb_build_alphanumeric_literal ("stdin", (size_t)5);
		current_file->special = COB_SELECT_STDIN;
	} else if (assign_device == CB_ASSIGN_PRINTER_DEVICE) {
		current_file->organization = COB_ORG_LINE_SEQUENTIAL;
		current_file->assign =
			cb_build_alphanumeric_literal ("PRINTER", (size_t)7);
	} else if (assign_device == CB_ASSIGN_PRINTER_1_DEVICE) {
		current_file->organization = COB_ORG_LINE_SEQUENTIAL;
		current_file->assign =
			cb_build_alphanumeric_literal ("PRINTER-1", (size_t)9);
	} else if (assign_device == CB_ASSIGN_PRINT_DEVICE) {
		current_file->organization = COB_ORG_LINE_SEQUENTIAL;
		current_file->assign =
			cb_build_alphanumeric_literal ("LPT1", (size_t)4);
	} else if (assign_device == CB_ASSIGN_LINE_SEQ_DEVICE
		   || assign_device == CB_ASSIGN_GENERAL_DEVICE) {
		current_file->flag_fileid = 1;
	}
  }
#line 14058 "parser.c"
    break;

  case 281:
#line 4991 "parser.y"
  {
	assign_device = CB_ASSIGN_NO_DEVICE;
  }
#line 14066 "parser.c"
    break;

  case 282:
#line 4995 "parser.y"
  {
	assign_device = CB_ASSIGN_NO_DEVICE;
  }
#line 14074 "parser.c"
    break;

  case 284:
#line 5003 "parser.y"
  {
	assign_device = CB_ASSIGN_GENERAL_DEVICE;
  }
#line 14082 "parser.c"
    break;

  case 285:
#line 5007 "parser.y"
  {
	current_file->organization = COB_ORG_LINE_SEQUENTIAL;
	assign_device = CB_ASSIGN_LINE_SEQ_DEVICE;
  }
#line 14091 "parser.c"
    break;

  case 286:
#line 5012 "parser.y"
  {
	assign_device = CB_ASSIGN_DISPLAY_DEVICE;
  }
#line 14099 "parser.c"
    break;

  case 287:
#line 5016 "parser.y"
  {
	assign_device = CB_ASSIGN_KEYBOARD_DEVICE;
  }
#line 14107 "parser.c"
    break;

  case 288:
#line 5024 "parser.y"
  {
	assign_device = CB_ASSIGN_PRINTER_DEVICE;
  }
#line 14115 "parser.c"
    break;

  case 289:
#line 5028 "parser.y"
  {
	assign_device = CB_ASSIGN_PRINTER_1_DEVICE;
  }
#line 14123 "parser.c"
    break;

  case 290:
#line 5032 "parser.y"
  {
	assign_device = CB_ASSIGN_PRINT_DEVICE;
  }
#line 14131 "parser.c"
    break;

  case 302:
#line 5057 "parser.y"
  {
	current_file->flag_line_adv = 1;
  }
#line 14139 "parser.c"
    break;

  case 303:
#line 5064 "parser.y"
  {
	ext_dyn_specified = 0;
	current_file->assign_type = cb_assign_type_default;
  }
#line 14148 "parser.c"
    break;

  case 304:
#line 5069 "parser.y"
  {
	ext_dyn_specified = 1;
	cb_verify (cb_assign_ext_dyn, _("ASSIGN EXTERNAL/DYNAMIC"));
  }
#line 14157 "parser.c"
    break;

  case 305:
#line 5077 "parser.y"
  {
	current_file->assign_type = CB_ASSIGN_EXT_FILE_NAME_REQUIRED;
  }
#line 14165 "parser.c"
    break;

  case 306:
#line 5081 "parser.y"
  {
	current_file->assign_type = CB_ASSIGN_VARIABLE_REQUIRED;
  }
#line 14173 "parser.c"
    break;

  case 309:
#line 5095 "parser.y"
  {
	check_repeated ("ACCESS", SYN_CLAUSE_2, &check_duplicate);
  }
#line 14181 "parser.c"
    break;

  case 310:
#line 5101 "parser.y"
                        { current_file->access_mode = COB_ACCESS_SEQUENTIAL; }
#line 14187 "parser.c"
    break;

  case 311:
#line 5102 "parser.y"
                        { current_file->access_mode = COB_ACCESS_DYNAMIC; }
#line 14193 "parser.c"
    break;

  case 312:
#line 5103 "parser.y"
                        { current_file->access_mode = COB_ACCESS_RANDOM; }
#line 14199 "parser.c"
    break;

  case 313:
#line 5111 "parser.y"
  {
	struct cb_alt_key *p;
	struct cb_alt_key *l;

	cb_tree composite_key;

	p = cobc_parse_malloc (sizeof (struct cb_alt_key));
	p->key = yyvsp[-4];
	p->component_list = NULL;
	if (yyvsp[-2]) {
		p->duplicates = CB_INTEGER (yyvsp[-2])->val;
	} else {
		/* note: we may add a compiler configuration here,
		         as at least ICOBOL defaults to WITH DUPLICATES
		         for ALTERNATE keys if not explicit deactivated
		*/
		p->duplicates = 0;
	}
	p->password = yyvsp[-1];
	if (yyvsp[0]) {
		p->tf_suppress = 1;
		p->char_suppress = CB_INTEGER (yyvsp[0])->val;
	} else {
		p->tf_suppress = 0;
	}
	p->next = NULL;

	/* handle split keys */
	if (yyvsp[-3]) {
		/* generate field (in w-s) for composite-key */
		composite_key = cb_build_field(yyvsp[-4]);
		if (composite_key == cb_error_node) {
			YYERROR;
		} else {
			composite_key->category = CB_CATEGORY_ALPHANUMERIC;
			((struct cb_field *)composite_key)->count = 1;
			p->key = cb_build_field_reference((struct cb_field *)composite_key, NULL);
			p->component_list = key_component_list;
		}
	}

	/* Add to the end of list */
	if (current_file->alt_key_list == NULL) {
		current_file->alt_key_list = p;
	} else {
		l = current_file->alt_key_list;
		for (; l->next; l = l->next) { ; }
		l->next = p;
	}
  }
#line 14254 "parser.c"
    break;

  case 314:
#line 5165 "parser.y"
  {
	yyval = NULL;
  }
#line 14262 "parser.c"
    break;

  case 316:
#line 5173 "parser.y"
  {
	CB_PENDING ("PASSWORD clause");
  }
#line 14270 "parser.c"
    break;

  case 317:
#line 5177 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 14278 "parser.c"
    break;

  case 318:
#line 5198 "parser.y"
  {
	yyval = NULL;
  }
#line 14286 "parser.c"
    break;

  case 319:
#line 5202 "parser.y"
  {
	yyval = cb_int (literal_value (yyvsp[0]));
  }
#line 14294 "parser.c"
    break;

  case 320:
#line 5206 "parser.y"
  {
	yyval = cb_int (literal_value (yyvsp[0]));
  }
#line 14302 "parser.c"
    break;

  case 321:
#line 5216 "parser.y"
  {
	check_repeated ("COLLATING", SYN_CLAUSE_3, &check_duplicate);
	current_file->collating_sequence = alphanumeric_collation;
	current_file->collating_sequence_n = national_collation;
	CB_PENDING ("FILE COLLATING SEQUENCE");
  }
#line 14313 "parser.c"
    break;

  case 322:
#line 5226 "parser.y"
  {
	alphanumeric_collation = national_collation = NULL;
  }
#line 14321 "parser.c"
    break;

  case 324:
#line 5234 "parser.y"
  {
	alphanumeric_collation = yyvsp[0];
  }
#line 14329 "parser.c"
    break;

  case 325:
#line 5238 "parser.y"
  {
	alphanumeric_collation = yyvsp[-1];
	CB_PENDING_X (yyvsp[0], "NATIONAL COLLATING SEQUENCE");
	national_collation = yyvsp[0];
  }
#line 14339 "parser.c"
    break;

  case 326:
#line 5244 "parser.y"
  {
	alphanumeric_collation = yyvsp[0];
  }
#line 14347 "parser.c"
    break;

  case 327:
#line 5248 "parser.y"
  {
	CB_PENDING_X (yyvsp[0], "NATIONAL COLLATING SEQUENCE");
	national_collation = yyvsp[0];
  }
#line 14356 "parser.c"
    break;

  case 328:
#line 5254 "parser.y"
  {
	alphanumeric_collation = yyvsp[-4];
	CB_PENDING_X (yyvsp[0], "NATIONAL COLLATING SEQUENCE");
	national_collation = yyvsp[0];
  }
#line 14366 "parser.c"
    break;

  case 329:
#line 5261 "parser.y"
  {
	CB_PENDING_X (yyvsp[-4], "NATIONAL COLLATING SEQUENCE");
	national_collation = yyvsp[-4];
	alphanumeric_collation = yyvsp[0];
  }
#line 14376 "parser.c"
    break;

  case 330:
#line 5270 "parser.y"
  {
	/* note: both entries must be resolved later on
	   and also attached to the correct key later, so just store in a list here: */
	current_file->collating_sequence_keys =
		cb_list_add(current_file->collating_sequence_keys, CB_BUILD_PAIR (yyvsp[0], yyvsp[-2]));
	CB_PENDING ("KEY COLLATING SEQUENCE");
  }
#line 14388 "parser.c"
    break;

  case 331:
#line 5281 "parser.y"
  {
	if (CB_ALPHABET_NAME_P (cb_ref (yyvsp[0]))) {
		yyval = yyvsp[0];
	} else {
		cb_error_x (yyvsp[0], _("'%s' is not an alphabet-name"),
			cb_name (yyvsp[0]));
		yyval = cb_error_node;
	}
  }
#line 14402 "parser.c"
    break;

  case 332:
#line 5296 "parser.y"
  {
	check_repeated ("STATUS", SYN_CLAUSE_4, &check_duplicate);
	current_file->file_status = yyvsp[-1];
	if (yyvsp[0]) {
		/* Ignore VSAM STATUS field */
		cb_verify (cb_vsam_status, _("VSAM status"));
	}
  }
#line 14415 "parser.c"
    break;

  case 336:
#line 5315 "parser.y"
  {
	check_repeated ("LOCK", SYN_CLAUSE_5, &check_duplicate);
  }
#line 14423 "parser.c"
    break;

  case 338:
#line 5323 "parser.y"
  {
	current_file->lock_mode |= COB_LOCK_MANUAL;
  }
#line 14431 "parser.c"
    break;

  case 339:
#line 5327 "parser.y"
  {
	current_file->lock_mode |= COB_LOCK_AUTOMATIC;
  }
#line 14439 "parser.c"
    break;

  case 340:
#line 5331 "parser.y"
  {
	current_file->lock_mode |= COB_LOCK_EXCLUSIVE;
  }
#line 14447 "parser.c"
    break;

  case 343:
#line 5340 "parser.y"
  {
	current_file->lock_mode |= COB_LOCK_MULTIPLE;
  }
#line 14455 "parser.c"
    break;

  case 344:
#line 5344 "parser.y"
  {
	current_file->lock_mode |= COB_LOCK_MULTIPLE;
  }
#line 14463 "parser.c"
    break;

  case 347:
#line 5355 "parser.y"
  {
	CB_PENDING ("WITH ROLLBACK");
  }
#line 14471 "parser.c"
    break;

  case 349:
#line 5362 "parser.y"
  {
	if (current_file->organization == COB_ORG_INDEXED) {
		current_file->lock_mode |= COB_LOCK_EXCLUSIVE;
		/* TODO: pass extra flag to fileio */
		CB_PENDING ("WITH MASS-UPDATE");
	} else {
		cb_error (_("%s only valid with ORGANIZATION %s"), "MASS-UPDATE", "INDEXED");
	}
  }
#line 14485 "parser.c"
    break;

  case 352:
#line 5383 "parser.y"
  {
	check_repeated ("ORGANIZATION", SYN_CLAUSE_6, &check_duplicate);
	error_if_record_delimiter_incompatible (COB_ORG_INDEXED, "INDEXED");
	current_file->organization = COB_ORG_INDEXED;
  }
#line 14495 "parser.c"
    break;

  case 353:
#line 5389 "parser.y"
  {
	check_repeated ("ORGANIZATION", SYN_CLAUSE_6, &check_duplicate);
	error_if_record_delimiter_incompatible (COB_ORG_SEQUENTIAL, "SEQUENTIAL");
	current_file->organization = COB_ORG_SEQUENTIAL;
  }
#line 14505 "parser.c"
    break;

  case 354:
#line 5395 "parser.y"
  {
	check_repeated ("ORGANIZATION", SYN_CLAUSE_6, &check_duplicate);
	error_if_record_delimiter_incompatible (COB_ORG_RELATIVE, "RELATIVE");
	current_file->organization = COB_ORG_RELATIVE;
  }
#line 14515 "parser.c"
    break;

  case 355:
#line 5401 "parser.y"
  {
	check_repeated ("ORGANIZATION", SYN_CLAUSE_6, &check_duplicate);
	error_if_record_delimiter_incompatible (COB_ORG_LINE_SEQUENTIAL,
						"LINE SEQUENTIAL");
	current_file->organization = COB_ORG_LINE_SEQUENTIAL;
  }
#line 14526 "parser.c"
    break;

  case 356:
#line 5414 "parser.y"
  {
	check_repeated ("PADDING", SYN_CLAUSE_7, &check_duplicate);
	cb_verify (cb_padding_character_clause, "PADDING CHARACTER");
  }
#line 14535 "parser.c"
    break;

  case 357:
#line 5424 "parser.y"
  {
	check_repeated ("RECORD DELIMITER", SYN_CLAUSE_8, &check_duplicate);
	current_file->flag_delimiter = 1;
  }
#line 14544 "parser.c"
    break;

  case 359:
#line 5433 "parser.y"
  {
	if (current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("RECORD DELIMITER %s only allowed with SEQUENTIAL files"),
			  "STANDARD-1");
		current_file->flag_delimiter = 0;
	} else if (cb_verify (cb_record_delimiter, _("RECORD DELIMITER clause"))) {
		cb_warning (cb_warn_additional,
			    _("%s ignored"), "RECORD DELIMITER STANDARD-1");
	}
  }
#line 14559 "parser.c"
    break;

  case 360:
#line 5444 "parser.y"
  {
	if (current_file->organization != COB_ORG_SEQUENTIAL
	 && current_file->organization != COB_ORG_LINE_SEQUENTIAL) {
		cb_error (_("RECORD DELIMITER %s only allowed with (LINE) SEQUENTIAL files"),
			  "LINE-SEQUENTIAL");
		current_file->flag_delimiter = 0;
	}

	if (cb_verify (cb_record_delimiter, _("RECORD DELIMITER clause"))
	 && cb_verify (cb_sequential_delimiters, _("LINE-SEQUENTIAL phrase"))) {
		current_file->organization = COB_ORG_LINE_SEQUENTIAL;
	}
  }
#line 14577 "parser.c"
    break;

  case 361:
#line 5458 "parser.y"
  {
	if (current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("RECORD DELIMITER %s only allowed with SEQUENTIAL files"),
			  "BINARY-SEQUENTIAL");
		current_file->flag_delimiter = 0;
	}

	if (cb_verify (cb_record_delimiter, _("RECORD DELIMITER clause"))
	 && cb_verify (cb_sequential_delimiters, _("BINARY-SEQUENTIAL phrase"))) {
		current_file->organization = COB_ORG_SEQUENTIAL;
	}
  }
#line 14594 "parser.c"
    break;

  case 362:
#line 5471 "parser.y"
  {
	if (current_file->organization != COB_ORG_SEQUENTIAL
	 && current_file->organization != COB_ORG_LINE_SEQUENTIAL) {
		cb_error (_("RECORD DELIMITER clause only allowed with (LINE) SEQUENTIAL files"));
		current_file->flag_delimiter = 0;
	} else if (cb_verify (cb_record_delimiter, _("RECORD DELIMITER clause"))) {
		cb_warning (cb_warn_additional,
			    _("RECORD DELIMITER %s not recognized; will be ignored"), cb_name (yyvsp[0]));
	}
  }
#line 14609 "parser.c"
    break;

  case 363:
#line 5487 "parser.y"
  {
	cb_tree composite_key;

	check_repeated ("RECORD KEY", SYN_CLAUSE_9, &check_duplicate);
	current_file->key = yyvsp[-3];
	key_type = RECORD_KEY;

	/* handle split keys */
	if (yyvsp[-2]) {
		/* generate field (in w-s) for composite-key */
		composite_key = cb_build_field (yyvsp[-3]);
		if (composite_key == cb_error_node) {
			YYERROR;
		} else {
			composite_key->category = CB_CATEGORY_ALPHANUMERIC;
			((struct cb_field *)composite_key)->count = 1;
			current_file->key = cb_build_field_reference ((struct cb_field *)composite_key, NULL);
			current_file->component_list = key_component_list;
		}
	}
	current_file->password = yyvsp[-1];
	if (yyvsp[0]) {
		/* note: we *may* add a compiler configuration here,
		         as most dialects do not allow this clause
		         on primary keys */
		if (CB_INTEGER (yyvsp[0])->val) {
			/* note: see ACUCOBOL docs for implementation notes, including [RE]WRITE rules
			         and "if the underlying (file) system does not support them OPEN
					 result in (sucessfull) io-status 0M" */
			CB_PENDING (_("DUPLICATES for primary keys"));
		};

	}
  }
#line 14648 "parser.c"
    break;

  case 364:
#line 5525 "parser.y"
  {
  	yyval = NULL;
  }
#line 14656 "parser.c"
    break;

  case 365:
#line 5529 "parser.y"
  {
  	yyval = cb_int0;
  }
#line 14664 "parser.c"
    break;

  case 368:
#line 5540 "parser.y"
  {
	key_component_list = NULL;
  }
#line 14672 "parser.c"
    break;

  case 371:
#line 5550 "parser.y"
  {
	struct cb_key_component *c;
	struct cb_key_component *comp = cobc_main_malloc (sizeof(struct cb_key_component));
	comp->next = NULL;
	comp->component = yyvsp[0];
	if (key_component_list == NULL) {
		key_component_list = comp;
	} else {
		for (c = key_component_list; c->next != NULL; c = c->next);
		c->next = comp;
	}
  }
#line 14689 "parser.c"
    break;

  case 372:
#line 5568 "parser.y"
  {
	check_repeated ("RELATIVE KEY", SYN_CLAUSE_10, &check_duplicate);
	current_file->key = yyvsp[0];
	key_type = RELATIVE_KEY;
  }
#line 14699 "parser.c"
    break;

  case 373:
#line 5579 "parser.y"
  {
	check_repeated ("RESERVE", SYN_CLAUSE_11, &check_duplicate);
  }
#line 14707 "parser.c"
    break;

  case 376:
#line 5593 "parser.y"
  {
	check_repeated ("SHARING", SYN_CLAUSE_12, &check_duplicate);
	current_file->sharing = yyvsp[0];
  }
#line 14716 "parser.c"
    break;

  case 377:
#line 5605 "parser.y"
                                { yyval = NULL; }
#line 14722 "parser.c"
    break;

  case 378:
#line 5606 "parser.y"
                                { yyval = cb_int (COB_LOCK_OPEN_EXCLUSIVE); }
#line 14728 "parser.c"
    break;

  case 379:
#line 5607 "parser.y"
                                { yyval = NULL; }
#line 14734 "parser.c"
    break;

  case 380:
#line 5614 "parser.y"
  {
	(void)cb_verify (CB_OBSOLETE, "FILE-LIMIT");
	check_repeated ("FILE-LIMIT", SYN_CLAUSE_13, &check_duplicate);
  }
#line 14743 "parser.c"
    break;

  case 383:
#line 5629 "parser.y"
  {
	(void)cb_verify (CB_OBSOLETE, "ACTUAL KEY");
	check_repeated ("ACTUAL KEY", SYN_CLAUSE_14, &check_duplicate);
  }
#line 14752 "parser.c"
    break;

  case 384:
#line 5639 "parser.y"
  {
	(void)cb_verify (CB_OBSOLETE, "NOMINAL KEY");
	check_repeated ("NOMINAL KEY", SYN_CLAUSE_15, &check_duplicate);
  }
#line 14761 "parser.c"
    break;

  case 385:
#line 5649 "parser.y"
  {
	(void)cb_verify (CB_OBSOLETE, "TRACK-AREA");
	check_repeated ("TRACK-AREA", SYN_CLAUSE_16, &check_duplicate);
  }
#line 14770 "parser.c"
    break;

  case 386:
#line 5659 "parser.y"
  {
	(void)cb_verify (CB_OBSOLETE, "TRACK-LIMIT");
	check_repeated ("TRACK-LIMIT", SYN_CLAUSE_17, &check_duplicate);
  }
#line 14779 "parser.c"
    break;

  case 388:
#line 5670 "parser.y"
  {
	cobc_cs_check = 0;
  }
#line 14787 "parser.c"
    break;

  case 389:
#line 5677 "parser.y"
{
	check_headers_present(COBC_HD_ENVIRONMENT_DIVISION,
				 COBC_HD_INPUT_OUTPUT_SECTION, 0, 0);
	header_check |= COBC_HD_I_O_CONTROL;
}
#line 14797 "parser.c"
    break;

  case 392:
#line 5687 "parser.y"
  {
	yyerrok;
  }
#line 14805 "parser.c"
    break;

  case 399:
#line 5708 "parser.y"
  {
	cb_tree l;

	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_INPUT_OUTPUT_SECTION,
			       COBC_HD_I_O_CONTROL, 0);
	switch (CB_INTEGER (yyvsp[-3])->val) {
	case 0:
		/* SAME AREA */
		break;
	case 1:
		/* SAME RECORD */
		for (l = yyvsp[0]; l; l = CB_CHAIN (l)) {
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
#line 14834 "parser.c"
    break;

  case 400:
#line 5735 "parser.y"
                                { yyval = cb_int0; }
#line 14840 "parser.c"
    break;

  case 401:
#line 5736 "parser.y"
                                { yyval = cb_int1; }
#line 14846 "parser.c"
    break;

  case 402:
#line 5737 "parser.y"
                                { yyval = cb_int2; }
#line 14852 "parser.c"
    break;

  case 403:
#line 5738 "parser.y"
                                { yyval = cb_int2; }
#line 14858 "parser.c"
    break;

  case 404:
#line 5745 "parser.y"
  {
	current_program->apply_commit = yyvsp[0];
	CB_PENDING("APPLY COMMIT");
  }
#line 14867 "parser.c"
    break;

  case 405:
#line 5750 "parser.y"
  {
	CB_PENDING ("APPLY LOCK-HOLDING");
  }
#line 14875 "parser.c"
    break;

  case 406:
#line 5754 "parser.y"
  {
	CB_PENDING ("APPLY PRINT-CONTROL");
  }
#line 14883 "parser.c"
    break;

  case 408:
#line 5759 "parser.y"
  {
	cb_verify (CB_OBSOLETE, _("DOS/VS APPLY phrase"));
  }
#line 14891 "parser.c"
    break;

  case 417:
#line 5779 "parser.y"
  {
	/* Fake for TAPE */
	cobc_cs_check = CB_CS_ASSIGN;
  }
#line 14900 "parser.c"
    break;

  case 418:
#line 5784 "parser.y"
  {
	check_headers_present (COBC_HD_ENVIRONMENT_DIVISION,
			       COBC_HD_INPUT_OUTPUT_SECTION,
			       COBC_HD_I_O_CONTROL, 0);
	cb_verify (cb_multiple_file_tape_clause, "MULTIPLE FILE TAPE");
	cobc_cs_check = 0;
  }
#line 14912 "parser.c"
    break;

  case 428:
#line 5827 "parser.y"
  {
	current_storage = CB_STORAGE_WORKING;
  }
#line 14920 "parser.c"
    break;

  case 432:
#line 5844 "parser.y"
  {
	header_check |= COBC_HD_DATA_DIVISION;
  }
#line 14928 "parser.c"
    break;

  case 434:
#line 5853 "parser.y"
  {
	current_storage = CB_STORAGE_FILE;
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_FILE_SECTION;
  }
#line 14938 "parser.c"
    break;

  case 437:
#line 5867 "parser.y"
  {
	if (CB_VALID_TREE (current_file)) {
		if (CB_VALID_TREE (yyvsp[0])) {
			/* Do not keep Record if this is really a report */
			if (!current_file->reports) {
				finalize_file (current_file, CB_FIELD (yyvsp[0]));
			}
		} else if (!current_file->reports) {
			cb_error (_("RECORD description missing or invalid"));
		}
	}
  }
#line 14955 "parser.c"
    break;

  case 438:
#line 5885 "parser.y"
  {
	current_storage = CB_STORAGE_FILE;
	check_headers_present (COBC_HD_DATA_DIVISION,
			       COBC_HD_FILE_SECTION, 0, 0);
	check_duplicate = 0;
	if (CB_INVALID_TREE (yyvsp[0])) {
		current_file = NULL;
		YYERROR;
	}
	current_file = CB_FILE (cb_ref (yyvsp[0]));
	current_file->description_entry = yyvsp[0];
	if (CB_VALID_TREE (current_file)) {
		if (yyvsp[-1] == cb_int1) {
			current_file->organization = COB_ORG_SORT;
		}
		/* note: this is a HACK and should be moved */
		if (current_file->flag_finalized) {
			cb_error_x (yyvsp[0], _("duplicate file description for %s"),
				cb_name (CB_TREE (current_file)));
		}
	}
  }
#line 14982 "parser.c"
    break;

  case 440:
#line 5909 "parser.y"
  {
	yyerrok;
  }
#line 14990 "parser.c"
    break;

  case 441:
#line 5916 "parser.y"
  {
	yyval = cb_int0;
  }
#line 14998 "parser.c"
    break;

  case 442:
#line 5920 "parser.y"
  {
	yyval = cb_int1;
  }
#line 15006 "parser.c"
    break;

  case 445:
#line 5931 "parser.y"
  {
	check_repeated ("EXTERNAL", SYN_CLAUSE_1, &check_duplicate);
#if	0	/* RXWRXW - Global/External */
	if (current_file->flag_global) {
		cb_error (_("file cannot have both EXTERNAL and GLOBAL clauses"));
	}
#endif
	current_file->flag_external = 1;
  }
#line 15020 "parser.c"
    break;

  case 446:
#line 5941 "parser.y"
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
#line 15039 "parser.c"
    break;

  case 456:
#line 5971 "parser.y"
  {
	check_repeated ("BLOCK", SYN_CLAUSE_3, &check_duplicate);
	/* ignore */
  }
#line 15048 "parser.c"
    break;

  case 460:
#line 5984 "parser.y"
  {
	check_repeated ("RECORD", SYN_CLAUSE_4, &check_duplicate);
	if (current_file->organization == COB_ORG_LINE_SEQUENTIAL) {
		cb_warning (cb_warn_additional, _("RECORD clause ignored for LINE SEQUENTIAL"));
	} else {
		set_record_size (NULL, yyvsp[-1]);
	}
  }
#line 15061 "parser.c"
    break;

  case 461:
#line 5993 "parser.y"
  {
	check_repeated ("RECORD", SYN_CLAUSE_4, &check_duplicate);
	if (current_file->organization == COB_ORG_LINE_SEQUENTIAL) {
		cb_warning (cb_warn_additional, _("RECORD clause ignored for LINE SEQUENTIAL"));
	} else {
		set_record_size (yyvsp[-3], yyvsp[-1]);
	}
  }
#line 15074 "parser.c"
    break;

  case 462:
#line 6003 "parser.y"
  {
	check_repeated ("RECORD", SYN_CLAUSE_4, &check_duplicate);
	set_record_size (yyvsp[-3], yyvsp[-2]);
	current_file->flag_check_record_varying_limits =
		current_file->record_min == 0 || current_file->record_max == 0;
  }
#line 15085 "parser.c"
    break;

  case 464:
#line 6013 "parser.y"
  {
	current_file->record_depending = yyvsp[0];
  }
#line 15093 "parser.c"
    break;

  case 465:
#line 6019 "parser.y"
                                { yyval = NULL; }
#line 15099 "parser.c"
    break;

  case 466:
#line 6020 "parser.y"
                                { yyval = yyvsp[0]; }
#line 15105 "parser.c"
    break;

  case 467:
#line 6024 "parser.y"
                                { yyval = NULL; }
#line 15111 "parser.c"
    break;

  case 468:
#line 6025 "parser.y"
                                { yyval = yyvsp[0]; }
#line 15117 "parser.c"
    break;

  case 469:
#line 6033 "parser.y"
  {
	check_repeated ("LABEL", SYN_CLAUSE_5, &check_duplicate);
	cb_verify (cb_label_records_clause, "LABEL RECORDS");
  }
#line 15126 "parser.c"
    break;

  case 470:
#line 6044 "parser.y"
  {
	check_repeated ("VALUE OF", SYN_CLAUSE_6, &check_duplicate);
	cb_verify (cb_value_of_clause, "VALUE OF");
  }
#line 15135 "parser.c"
    break;

  case 471:
#line 6049 "parser.y"
  {
	check_repeated ("VALUE OF", SYN_CLAUSE_6, &check_duplicate);
	cb_verify (cb_value_of_clause, "VALUE OF");
	if (!current_file->assign) {
		current_file->assign = cb_build_assignment_name (current_file, yyvsp[0]);
	}
  }
#line 15147 "parser.c"
    break;

  case 476:
#line 6072 "parser.y"
  {
	check_repeated ("DATA", SYN_CLAUSE_7, &check_duplicate);
	cb_verify (cb_data_records_clause, "DATA RECORDS");
  }
#line 15156 "parser.c"
    break;

  case 477:
#line 6084 "parser.y"
  {
	check_repeated ("LINAGE", SYN_CLAUSE_8, &check_duplicate);
	if (current_file->organization != COB_ORG_LINE_SEQUENTIAL &&
	    current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("LINAGE clause with wrong file type"));
	} else {
		current_file->linage = yyvsp[-2];
		current_file->organization = COB_ORG_LINE_SEQUENTIAL;
		if (current_linage == 0) {
			linage_file = current_file;
		}
		current_linage++;
	}
  }
#line 15175 "parser.c"
    break;

  case 483:
#line 6112 "parser.y"
  {
	current_file->latfoot = yyvsp[0];
  }
#line 15183 "parser.c"
    break;

  case 484:
#line 6119 "parser.y"
  {
	current_file->lattop = yyvsp[0];
  }
#line 15191 "parser.c"
    break;

  case 485:
#line 6126 "parser.y"
  {
	current_file->latbot = yyvsp[0];
  }
#line 15199 "parser.c"
    break;

  case 486:
#line 6135 "parser.y"
  {
	cobc_cs_check ^= CB_CS_RECORDING;
	check_repeated ("RECORDING", SYN_CLAUSE_9, &check_duplicate);
	/* ignore */
  }
#line 15209 "parser.c"
    break;

  case 491:
#line 6148 "parser.y"
  {
	if (current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("RECORDING MODE U or S can only be used with RECORD SEQUENTIAL files"));
	}
  }
#line 15219 "parser.c"
    break;

  case 494:
#line 6164 "parser.y"
  {
	struct cb_alphabet_name	*al;

	check_repeated ("CODE SET", SYN_CLAUSE_10, &check_duplicate);

	if (CB_VALID_TREE (yyvsp[-1])) {
		al = CB_ALPHABET_NAME (cb_ref (yyvsp[-1]));
		switch (al->alphabet_type) {
#ifdef	COB_EBCDIC_MACHINE
		case CB_ALPHABET_ASCII:
#else
		case CB_ALPHABET_EBCDIC:
#endif
		case CB_ALPHABET_CUSTOM:
			current_file->code_set = al;
			CB_PENDING ("CODE-SET");
			break;
		default:
			if (cb_warn_opt_val[cb_warn_additional] != COBC_WARN_DISABLED) {
				cb_warning_x (cb_warn_additional, yyvsp[-1], _("ignoring CODE-SET '%s'"),
						  cb_name (yyvsp[-1]));
			} else {
				CB_PENDING ("CODE-SET");
			}
			break;
		}
	}

	if (current_file->organization != COB_ORG_LINE_SEQUENTIAL &&
	    current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("CODE-SET clause invalid for file type"));
	}

  }
#line 15258 "parser.c"
    break;

  case 496:
#line 6202 "parser.y"
  {
	  CB_PENDING ("FOR sub-records");
	  current_file->code_set_items = CB_LIST (yyvsp[0]);
  }
#line 15267 "parser.c"
    break;

  case 497:
#line 6212 "parser.y"
  {
	check_repeated ("REPORT", SYN_CLAUSE_11, &check_duplicate);
	if (current_file->organization != COB_ORG_LINE_SEQUENTIAL &&
	    current_file->organization != COB_ORG_SEQUENTIAL) {
		cb_error (_("REPORT clause with wrong file type"));
	} else {
		current_file->reports = yyvsp[0];
		current_file->organization = COB_ORG_LINE_SEQUENTIAL;
		current_file->flag_line_adv = 1;
	}
  }
#line 15283 "parser.c"
    break;

  case 500:
#line 6232 "parser.y"
  {
	if (CB_VALID_TREE (yyvsp[0])) {
		current_report = build_report (yyvsp[0]);
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
#line 15301 "parser.c"
    break;

  case 501:
#line 6246 "parser.y"
  {
	if (CB_VALID_TREE (yyvsp[0])) {
		current_report = build_report (yyvsp[0]);
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
#line 15319 "parser.c"
    break;

  case 503:
#line 6265 "parser.y"
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
#line 15338 "parser.c"
    break;

  case 507:
#line 6289 "parser.y"
  {
	if (CB_VALID_TREE (current_cd)) {
		if (CB_VALID_TREE (yyvsp[0])) {
			cb_finalize_cd (current_cd, CB_FIELD (yyvsp[0]));
		} else if (!current_cd->record) {
			cb_error (_("CD record missing"));
		}
	}
  }
#line 15352 "parser.c"
    break;

  case 508:
#line 6304 "parser.y"
  {
	/* CD internally defines a new file */
	if (CB_VALID_TREE (yyvsp[0])) {
		current_cd = cb_build_cd (yyvsp[0]);

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
#line 15374 "parser.c"
    break;

  case 556:
#line 6412 "parser.y"
  {
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_WORKING_STORAGE_SECTION;
	current_storage = CB_STORAGE_WORKING;
  }
#line 15384 "parser.c"
    break;

  case 557:
#line 6418 "parser.y"
  {
	if (yyvsp[0]) {
		CB_FIELD_ADD (current_program->working_storage, CB_FIELD (yyvsp[0]));
	}
  }
#line 15394 "parser.c"
    break;

  case 558:
#line 6427 "parser.y"
  {
	yyval = NULL;
  }
#line 15402 "parser.c"
    break;

  case 559:
#line 6431 "parser.y"
  {
	current_field = NULL;
	control_field = NULL;
	description_field = NULL;
	cb_clear_real_field ();
  }
#line 15413 "parser.c"
    break;

  case 560:
#line 6438 "parser.y"
  {
	yyval = get_finalized_description_tree ();
  }
#line 15421 "parser.c"
    break;

  case 566:
#line 6453 "parser.y"
  {
	if (current_field && !CB_INVALID_TREE (current_field->external_definition)) {
		/* finalize last field if target of SAME AS / type-name */
		inherit_external_definition (yyvsp[-1]);
	}
	if (set_current_field (yyvsp[-1], yyvsp[0])) {
		YYERROR;
	}
	save_tree = NULL;
  }
#line 15436 "parser.c"
    break;

  case 567:
#line 6464 "parser.y"
  {
	if (!qualifier) {
		current_field->flag_filler = 1;
	}
	if (!description_field) {
		description_field = current_field;
	}
  }
#line 15449 "parser.c"
    break;

  case 568:
#line 6473 "parser.y"
  {
#if 0 /* works fine without, leads to invalid free otherwise [COB_TREE_DEBUG] */
	/* Free tree associated with level number */
	cobc_parse_free (yyvsp[-2]);
#endif
	yyerrok;
	cb_unput_dot ();
	check_pic_duplicate = 0;
	check_duplicate = 0;
#if 0 /* CHECKME - *Why* would we want to change the field here? */
	current_field = cb_get_real_field ();
#endif
  }
#line 15467 "parser.c"
    break;

  case 569:
#line 6490 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 15475 "parser.c"
    break;

  case 572:
#line 6502 "parser.y"
  {
	yyval = cb_build_filler ();
	qualifier = NULL;
	keys_list = NULL;
	non_const_word = 0;
  }
#line 15486 "parser.c"
    break;

  case 574:
#line 6513 "parser.y"
  {
	yyval = yyvsp[0];
	qualifier = yyvsp[0];
	keys_list = NULL;
	non_const_word = 0;
  }
#line 15497 "parser.c"
    break;

  case 575:
#line 6523 "parser.y"
  {
	yyval = NULL;
  }
#line 15505 "parser.c"
    break;

  case 576:
#line 6527 "parser.y"
  {
	if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("%s is invalid in a user FUNCTION"), "GLOBAL");
		yyval = NULL;
	} else {
		yyval = cb_null;
	}
  }
#line 15518 "parser.c"
    break;

  case 577:
#line 6538 "parser.y"
                                        { yyval = yyvsp[0]; }
#line 15524 "parser.c"
    break;

  case 578:
#line 6539 "parser.y"
                                                { yyval = cb_build_const_length (yyvsp[0]); }
#line 15530 "parser.c"
    break;

  case 579:
#line 6541 "parser.y"
                                { yyval = cb_build_const_length (yyvsp[0]); }
#line 15536 "parser.c"
    break;

  case 580:
#line 6546 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 15544 "parser.c"
    break;

  case 581:
#line 6550 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 15552 "parser.c"
    break;

  case 582:
#line 6556 "parser.y"
  {
	yyval = cb_int1;
  }
#line 15560 "parser.c"
    break;

  case 583:
#line 6560 "parser.y"
  {
	yyval = cb_int2;
  }
#line 15568 "parser.c"
    break;

  case 584:
#line 6564 "parser.y"
  {
	yyval = cb_int4;
  }
#line 15576 "parser.c"
    break;

  case 585:
#line 6568 "parser.y"
  {
	yyval = cb_int8;
  }
#line 15584 "parser.c"
    break;

  case 586:
#line 6572 "parser.y"
  {
	yyval = cb_int ((int)sizeof(long));
  }
#line 15592 "parser.c"
    break;

  case 587:
#line 6576 "parser.y"
  {
	yyval = cb_int ((int)sizeof(void *));
  }
#line 15600 "parser.c"
    break;

  case 588:
#line 6580 "parser.y"
  {
	if (cb_binary_comp_1) {
		yyval = cb_int2;
	} else {
		yyval = cb_int ((int)sizeof(float));
	}
  }
#line 15612 "parser.c"
    break;

  case 589:
#line 6588 "parser.y"
  {
	yyval = cb_int ((int)sizeof(float));
  }
#line 15620 "parser.c"
    break;

  case 590:
#line 6592 "parser.y"
  {
	yyval = cb_int ((int)sizeof(double));
  }
#line 15628 "parser.c"
    break;

  case 591:
#line 6596 "parser.y"
  {
	yyval = cb_int4;
  }
#line 15636 "parser.c"
    break;

  case 592:
#line 6600 "parser.y"
  {
	yyval = cb_int8;
  }
#line 15644 "parser.c"
    break;

  case 593:
#line 6604 "parser.y"
  {
	yyval = cb_int16;
  }
#line 15652 "parser.c"
    break;

  case 594:
#line 6608 "parser.y"
  {
	yyerrok;
	cb_unput_dot ();
	check_pic_duplicate = 0;
	check_duplicate = 0;
	current_field = cb_get_real_field ();
  }
#line 15664 "parser.c"
    break;

  case 604:
#line 6640 "parser.y"
  {
	cb_tree renames_target = cb_ref (yyvsp[-1]);

	size_t sav = cb_needs_01;
	cb_needs_01 = 0;

	non_const_word = 0;

	if (set_current_field (yyvsp[-5], yyvsp[-4])) {
		/* error in the definition, no further checks possible */
	} else if (renames_target == cb_error_node) {
		/* error in the target, skip further checks */
		current_field->flag_invalid = 1;
	} else {
		cb_tree renames_thru = yyvsp[0];

		current_field->redefines = CB_FIELD (renames_target);
		if (renames_thru) {
			renames_thru = cb_ref (renames_thru);
		}
		if (CB_VALID_TREE (renames_thru)) {
			current_field->rename_thru = CB_FIELD (renames_thru);
		} else {
			/* If there is no THRU clause, RENAMES acts like REDEFINES. */
			current_field->pic = current_field->redefines->pic;
		}

		if (cb_validate_renames_item (current_field, yyvsp[-1], yyvsp[0])) {
			current_field->flag_invalid = 1;
		} else {
			/* ensure the reference was validated as this
			   also calculates the reference' picture and size */
			if (!current_field->redefines->flag_is_verified) {
				cb_validate_field (current_field->redefines);
			}
		}
	}
	cb_needs_01 = sav;
  }
#line 15708 "parser.c"
    break;

  case 605:
#line 6683 "parser.y"
  {
	yyval = NULL;
  }
#line 15716 "parser.c"
    break;

  case 606:
#line 6687 "parser.y"
  {
	yyval = yyvsp[0] == cb_error_node ? NULL : yyvsp[0];
  }
#line 15724 "parser.c"
    break;

  case 607:
#line 6694 "parser.y"
  {
	if (set_current_field (yyvsp[-1], yyvsp[0])) {
		YYERROR;
	}
  }
#line 15734 "parser.c"
    break;

  case 608:
#line 6700 "parser.y"
  {
	cb_validate_88_item (current_field);
  }
#line 15742 "parser.c"
    break;

  case 609:
#line 6707 "parser.y"
  {
	cb_tree x;
	int	level;

	cobc_cs_check = 0;
	level = cb_get_level (yyvsp[-4]);
	/* Free tree associated with level number */
	cobc_parse_free (yyvsp[-4]);
	if (level != 1) {
		cb_error (_("CONSTANT item not at 01 level"));
	} else if (yyvsp[0]) {
		if (cb_verify(cb_constant_01, "01 CONSTANT")) {
			x = cb_build_constant (yyvsp[-3], yyvsp[0]);
			CB_FIELD (x)->flag_item_78 = 1;
			CB_FIELD (x)->flag_constant = 1;
			CB_FIELD (x)->level = 1;
			CB_FIELD (x)->values = yyvsp[0];
			cb_needs_01 = 1;
			if (yyvsp[-1]) {
				CB_FIELD (x)->flag_is_global = 1;
			}
			/* Ignore return value */
			(void)cb_validate_78_item (CB_FIELD (x), 0);
		}
	}
  }
#line 15773 "parser.c"
    break;

  case 610:
#line 6734 "parser.y"
  {
	if (set_current_field (yyvsp[-1], yyvsp[0])) {
		YYERROR;
	}
  }
#line 15783 "parser.c"
    break;

  case 611:
#line 6741 "parser.y"
  {
	/* Reset to last non-78 item */
	current_field = cb_validate_78_item (current_field, 0);
  }
#line 15792 "parser.c"
    break;

  case 612:
#line 6749 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 15800 "parser.c"
    break;

  case 613:
#line 6753 "parser.y"
  {
	yyval = CB_LIST_INIT(cb_build_const_from (yyvsp[0]));
  }
#line 15808 "parser.c"
    break;

  case 614:
#line 6760 "parser.y"
  {
	if (CB_VALID_TREE (current_field)) {
		current_field->values = yyvsp[0];
	}
  }
#line 15818 "parser.c"
    break;

  case 615:
#line 6766 "parser.y"
  {
	current_field->values = CB_LIST_INIT (cb_build_const_start (current_field, yyvsp[0]));
  }
#line 15826 "parser.c"
    break;

  case 616:
#line 6770 "parser.y"
  {
	current_field->values = CB_LIST_INIT (cb_build_const_next (current_field));
  }
#line 15834 "parser.c"
    break;

  case 617:
#line 6776 "parser.y"
                                        { yyval = CB_LIST_INIT (yyvsp[0]); }
#line 15840 "parser.c"
    break;

  case 618:
#line 6777 "parser.y"
                                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 15846 "parser.c"
    break;

  case 619:
#line 6781 "parser.y"
                                { yyval = yyvsp[0]; }
#line 15852 "parser.c"
    break;

  case 620:
#line 6782 "parser.y"
                                { yyval = cb_build_alphanumeric_literal ("(", 1); }
#line 15858 "parser.c"
    break;

  case 621:
#line 6783 "parser.y"
                                { yyval = cb_build_alphanumeric_literal (")", 1); }
#line 15864 "parser.c"
    break;

  case 622:
#line 6784 "parser.y"
                                { yyval = cb_build_alphanumeric_literal ("+", 1); }
#line 15870 "parser.c"
    break;

  case 623:
#line 6785 "parser.y"
                                { yyval = cb_build_alphanumeric_literal ("-", 1); }
#line 15876 "parser.c"
    break;

  case 624:
#line 6786 "parser.y"
                                { yyval = cb_build_alphanumeric_literal ("*", 1); }
#line 15882 "parser.c"
    break;

  case 625:
#line 6787 "parser.y"
                                { yyval = cb_build_alphanumeric_literal ("/", 1); }
#line 15888 "parser.c"
    break;

  case 626:
#line 6788 "parser.y"
                                { yyval = cb_build_alphanumeric_literal ("&", 1); }
#line 15894 "parser.c"
    break;

  case 627:
#line 6789 "parser.y"
                                { yyval = cb_build_alphanumeric_literal ("|", 1); }
#line 15900 "parser.c"
    break;

  case 628:
#line 6790 "parser.y"
                                { yyval = cb_build_alphanumeric_literal ("^", 1); }
#line 15906 "parser.c"
    break;

  case 631:
#line 6800 "parser.y"
  {
	save_tree = cb_int0;
  }
#line 15914 "parser.c"
    break;

  case 654:
#line 6835 "parser.y"
  {
	check_repeated ("REDEFINES", SYN_CLAUSE_1, &check_pic_duplicate);
	if (save_tree != NULL) {
		cb_verify_x (yyvsp[0], cb_free_redefines_position,
			     _("REDEFINES clause not following entry-name"));
	}

	current_field->redefines = cb_resolve_redefines (current_field, yyvsp[0]);
	if (current_field->redefines == NULL) {
		current_field->flag_is_verified = 1;
		current_field->flag_invalid = 1;
		YYERROR;
	}
  }
#line 15933 "parser.c"
    break;

  case 655:
#line 6856 "parser.y"
  {
	if (!check_repeated ("LIKE", SYN_CLAUSE_30, &check_pic_duplicate)) {
		if (current_field->external_definition) {
			emit_conflicting_clause_message ("TYPE TO", "SAME AS");
		}
		setup_external_definition (yyvsp[-1], 0);
		current_field->like_modifier = yyvsp[0];
		CB_PENDING_X (yyvsp[-1], "LIKE clause");
	}
  }
#line 15948 "parser.c"
    break;

  case 656:
#line 6869 "parser.y"
                { yyval = cb_int0; }
#line 15954 "parser.c"
    break;

  case 658:
#line 6874 "parser.y"
  {
	yyval = yyvsp[-1];
  }
#line 15962 "parser.c"
    break;

  case 659:
#line 6883 "parser.y"
  {
	if (!check_repeated ("SAME AS", SYN_CLAUSE_30, &check_pic_duplicate)) {
		if (current_field->external_definition) {
			emit_conflicting_clause_message ("TYPE TO", "SAME AS");
		}
		cb_verify (cb_same_as_clause, _("SAME AS clause"));
		setup_external_definition (yyvsp[0], 0);
	}


  }
#line 15978 "parser.c"
    break;

  case 660:
#line 6901 "parser.y"
  {
	if (current_field->flag_is_typedef) {
		emit_duplicate_clause_message ("TYPEDEF");
		YYERROR;
	}
	/* note: no explicit verification as all dialects with this reserved word use it */
	current_field->flag_is_typedef = 1;
	within_typedef_definition = 1;

	if (current_field->level != 1 && current_field->level != 77) {
		cb_error (_("%s only allowed at 01/77 level"), "TYPEDEF");
	}
	if (!qualifier) {
		cb_error (_("%s requires a data name"), "TYPEDEF");
	}
	if (current_storage == CB_STORAGE_SCREEN
	 || current_storage == CB_STORAGE_REPORT) {
		cb_error (_("%s not allowed in %s"), "TYPEDEF",
			enum_explain_storage(current_storage));
	}
  }
#line 16004 "parser.c"
    break;

  case 662:
#line 6926 "parser.y"
  {
	CB_PENDING ("TYPEDEF STRONG");
  }
#line 16012 "parser.c"
    break;

  case 663:
#line 6936 "parser.y"
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
#line 16040 "parser.c"
    break;

  case 664:
#line 6963 "parser.y"
  {
	current_field->ename = cb_to_cname (current_field->name);
  }
#line 16048 "parser.c"
    break;

  case 665:
#line 6967 "parser.y"
  {
	current_field->ename = cb_to_cname ((const char *)CB_LITERAL (yyvsp[0])->data);
  }
#line 16056 "parser.c"
    break;

  case 668:
#line 6980 "parser.y"
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
#line 16079 "parser.c"
    break;

  case 669:
#line 7004 "parser.y"
  {
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "SPECIAL-NAMES");
	} else {
		cb_verify (cb_special_names_clause, "SPECIAL-NAMES clause");
	}
  }
#line 16091 "parser.c"
    break;

  case 671:
#line 7016 "parser.y"
  {
	if (current_program->cursor_pos) {
		emit_duplicate_clause_message ("CURSOR");
	} else {
		current_program->cursor_pos = cb_build_reference (current_field->name);
	}
  }
#line 16103 "parser.c"
    break;

  case 672:
#line 7024 "parser.y"
  {
	if (current_program->crt_status) {
		emit_duplicate_clause_message ("CRT STATUS");
	} else {
		current_program->crt_status = cb_build_reference (current_field->name);
	}
  }
#line 16115 "parser.c"
    break;

  case 673:
#line 7041 "parser.y"
  {
#if 0 /* not yet implemented */
	if (current_program->screen_control) {
		emit_duplicate_clause_message ("SCREEN CONTROL");
	} else {
		CB_PENDING ("SCREEN CONTROL");
	}
#else
	CB_PENDING ("SCREEN CONTROL");
#endif
  }
#line 16131 "parser.c"
    break;

  case 674:
#line 7053 "parser.y"
  {
#if 0 /* not yet implemented */
	if (current_program->event_status) {
		emit_duplicate_clause_message ("EVENT STATUS");
	} else {
		CB_PENDING ("EVENT STATUS");
	}
#else
	CB_PENDING ("EVENT STATUS");
#endif
  }
#line 16147 "parser.c"
    break;

  case 675:
#line 7070 "parser.y"
  {
	check_repeated ("VOLATILE", SYN_CLAUSE_24, &check_pic_duplicate);
	/* note: there is no reason to check current_storage as we only parse
	         volatile_clause and its parent tokens where applicable,
	         same is true for level 66,78,88 */
	/* missing part: always generate and initialize storage */
	CB_UNFINISHED ("VOLATILE");
	current_field->flag_volatile = 1;
	/* TODO: set VOLATILE flag for all parent fields */
  }
#line 16162 "parser.c"
    break;

  case 676:
#line 7088 "parser.y"
  {
	check_repeated ("PICTURE", SYN_CLAUSE_4, &check_pic_duplicate);
	current_field->pic = CB_PICTURE (yyvsp[-1]);

	if (CB_VALID_TREE (yyvsp[0])) {
		if (  (current_field->pic->category != CB_CATEGORY_NUMERIC
		    && current_field->pic->category != CB_CATEGORY_NUMERIC_EDITED)
		 || strpbrk (current_field->pic->orig, " CRDB-*") /* the standard seems to forbid also ',' */) {
			cb_error_x (yyvsp[-1], _("a locale-format PICTURE string must only consist of '9', '.', '+', 'Z' and the currency-sign"));
		} else {
			/* TODO: check that not we're not within a CONSTANT RECORD */
			CB_PENDING_X (yyvsp[-1], "locale-format PICTURE");
		}
	}
  }
#line 16182 "parser.c"
    break;

  case 677:
#line 7107 "parser.y"
  { yyval = NULL; }
#line 16188 "parser.c"
    break;

  case 678:
#line 7109 "parser.y"
  {
	/* $2 -> optional locale-name to be used */
	yyval = yyvsp[0];
  }
#line 16197 "parser.c"
    break;

  case 680:
#line 7118 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 16205 "parser.c"
    break;

  case 681:
#line 7126 "parser.y"
  {
	if (CB_LOCALE_NAME_P (cb_ref (yyvsp[0]))) {
		yyval = yyvsp[0];
	} else {
		cb_error_x (yyvsp[0], _("'%s' is not a locale-name"),	cb_name (yyvsp[0]));
		yyval = cb_error_node;
	}
  }
#line 16218 "parser.c"
    break;

  case 682:
#line 7141 "parser.y"
  {
	cb_verify (cb_type_to_clause, _("TYPE TO clause"));
	setup_external_definition_type (yyvsp[0]);
  }
#line 16227 "parser.c"
    break;

  case 685:
#line 7154 "parser.y"
  {
	{
		cb_tree x = cb_try_ref (yyvsp[0]);
		if (!CB_INVALID_TREE (x) && CB_FIELD_P (x) && CB_FIELD (x)->flag_is_typedef) {
			if (!check_repeated ("USAGE", SYN_CLAUSE_5, &check_pic_duplicate)) {
				if (current_field->external_definition) {
					emit_conflicting_clause_message ("USAGE", "SAME AS / TYPE TO");
				} else {
					cb_verify (cb_usage_type_name, _("USAGE type-name"));
					/* replace usage by type definition */
					check_pic_duplicate &= ~SYN_CLAUSE_5;
					check_repeated ("USAGE/TYPE", SYN_CLAUSE_31, &check_pic_duplicate);
					setup_external_definition (yyvsp[0], 1);
					break;	/* everything done here */
				}
			}
			YYERROR;
		}
	}
	if (is_reserved_word (CB_NAME (yyvsp[0]))) {
		cb_error_x (yyvsp[0], _("'%s' is not a valid USAGE"), CB_NAME (yyvsp[0]));
	} else if (is_default_reserved_word (CB_NAME (yyvsp[0]))) {
		cb_error_x (yyvsp[0], _("'%s' is not defined, but is a reserved word in another dialect"),
				CB_NAME (yyvsp[0]));
	} else {
		cb_error_x (yyvsp[0], _("unknown USAGE: %s"), CB_NAME (yyvsp[0]));
	}
	check_and_set_usage (CB_USAGE_ERROR);
	YYERROR;
  }
#line 16262 "parser.c"
    break;

  case 686:
#line 7185 "parser.y"
  {
	check_and_set_usage (CB_USAGE_ERROR);
  }
#line 16270 "parser.c"
    break;

  case 687:
#line 7192 "parser.y"
  {
	check_and_set_usage (CB_USAGE_BINARY);
  }
#line 16278 "parser.c"
    break;

  case 688:
#line 7196 "parser.y"
  {
	check_and_set_usage (CB_USAGE_BIT);
	CB_PENDING ("USAGE BIT");
  }
#line 16287 "parser.c"
    break;

  case 689:
#line 7201 "parser.y"
  {
	check_and_set_usage (CB_USAGE_BINARY);
  }
#line 16295 "parser.c"
    break;

  case 690:
#line 7205 "parser.y"
  {
	/* see FR #310 */
	CB_PENDING ("USAGE COMP-0");
  }
#line 16304 "parser.c"
    break;

  case 691:
#line 7210 "parser.y"
  {
	current_field->flag_comp_1 = 1;
	if (cb_binary_comp_1) {
		check_and_set_usage (CB_USAGE_SIGNED_SHORT);
	} else {
		check_and_set_usage (CB_USAGE_FLOAT);
	}
  }
#line 16317 "parser.c"
    break;

  case 692:
#line 7219 "parser.y"
  {
	check_and_set_usage (CB_USAGE_DOUBLE);
  }
#line 16325 "parser.c"
    break;

  case 693:
#line 7223 "parser.y"
  {
	check_and_set_usage (CB_USAGE_PACKED);
  }
#line 16333 "parser.c"
    break;

  case 694:
#line 7227 "parser.y"
  {
	check_and_set_usage (CB_USAGE_BINARY);
  }
#line 16341 "parser.c"
    break;

  case 695:
#line 7231 "parser.y"
  {
	check_and_set_usage (CB_USAGE_COMP_5);
  }
#line 16349 "parser.c"
    break;

  case 696:
#line 7235 "parser.y"
  {
	check_and_set_usage (CB_USAGE_COMP_6);
  }
#line 16357 "parser.c"
    break;

  case 697:
#line 7239 "parser.y"
  {
	check_and_set_usage (CB_USAGE_COMP_X);
  }
#line 16365 "parser.c"
    break;

  case 698:
#line 7243 "parser.y"
  {
	check_and_set_usage (CB_USAGE_COMP_N);
  }
#line 16373 "parser.c"
    break;

  case 699:
#line 7247 "parser.y"
  {
	check_and_set_usage (CB_USAGE_FLOAT);
  }
#line 16381 "parser.c"
    break;

  case 700:
#line 7251 "parser.y"
  {
	check_and_set_usage (CB_USAGE_DISPLAY);
  }
#line 16389 "parser.c"
    break;

  case 701:
#line 7255 "parser.y"
  {
	check_and_set_usage (CB_USAGE_INDEX);
  }
#line 16397 "parser.c"
    break;

  case 702:
#line 7259 "parser.y"
  {
	check_and_set_usage (CB_USAGE_PACKED);
  }
#line 16405 "parser.c"
    break;

  case 703:
#line 7263 "parser.y"
  {
	check_and_set_usage (CB_USAGE_POINTER);
	current_field->flag_is_pointer = 1;
  }
#line 16414 "parser.c"
    break;

  case 704:
#line 7268 "parser.y"
  {
	check_and_set_usage (CB_USAGE_PROGRAM_POINTER);
	current_field->flag_is_pointer = 1;
  }
#line 16423 "parser.c"
    break;

  case 705:
#line 7273 "parser.y"
  {
	check_and_set_usage (CB_USAGE_HNDL);
  }
#line 16431 "parser.c"
    break;

  case 706:
#line 7277 "parser.y"
  {
	check_and_set_usage (CB_USAGE_HNDL_WINDOW);
  }
#line 16439 "parser.c"
    break;

  case 707:
#line 7281 "parser.y"
  {
	check_and_set_usage (CB_USAGE_HNDL_SUBWINDOW);
  }
#line 16447 "parser.c"
    break;

  case 708:
#line 7285 "parser.y"
  {
	check_and_set_usage (CB_USAGE_HNDL_FONT);
	CB_PENDING ("HANDLE OF FONT");
  }
#line 16456 "parser.c"
    break;

  case 709:
#line 7290 "parser.y"
  {
	check_and_set_usage (CB_USAGE_HNDL_THREAD);
  }
#line 16464 "parser.c"
    break;

  case 710:
#line 7294 "parser.y"
  {
	check_and_set_usage (CB_USAGE_HNDL_MENU);
	CB_PENDING ("HANDLE OF MENU");
  }
#line 16473 "parser.c"
    break;

  case 711:
#line 7299 "parser.y"
  {
	check_and_set_usage (CB_USAGE_HNDL_VARIANT);
  }
#line 16481 "parser.c"
    break;

  case 712:
#line 7303 "parser.y"
  {
	check_and_set_usage (CB_USAGE_HNDL_LM);
	CB_PENDING ("HANDLE OF LAYOUT-MANAGER");
  }
#line 16490 "parser.c"
    break;

  case 713:
#line 7308 "parser.y"
  {
	check_and_set_usage (CB_USAGE_HNDL);
	CB_PENDING ("HANDLE OF control-type");
  }
#line 16499 "parser.c"
    break;

  case 714:
#line 7313 "parser.y"
  {
	check_and_set_usage (CB_USAGE_HNDL);
	cb_error_x (yyvsp[0], _("unknown HANDLE type: %s"), CB_NAME (yyvsp[0]));
  }
#line 16508 "parser.c"
    break;

  case 715:
#line 7318 "parser.y"
  {
	check_and_set_usage (CB_USAGE_SIGNED_SHORT);
  }
#line 16516 "parser.c"
    break;

  case 716:
#line 7322 "parser.y"
  {
	check_and_set_usage (CB_USAGE_SIGNED_INT);
  }
#line 16524 "parser.c"
    break;

  case 717:
#line 7326 "parser.y"
  {
#ifdef COB_32_BIT_LONG
	check_and_set_usage (CB_USAGE_SIGNED_INT);
#else
	check_and_set_usage (CB_USAGE_SIGNED_LONG);
#endif
  }
#line 16536 "parser.c"
    break;

  case 718:
#line 7334 "parser.y"
  {
	check_and_set_usage (CB_USAGE_UNSIGNED_SHORT);
  }
#line 16544 "parser.c"
    break;

  case 719:
#line 7338 "parser.y"
  {
	check_and_set_usage (CB_USAGE_UNSIGNED_INT);
  }
#line 16552 "parser.c"
    break;

  case 720:
#line 7342 "parser.y"
  {
#ifdef COB_32_BIT_LONG
	check_and_set_usage (CB_USAGE_UNSIGNED_INT);
#else
	check_and_set_usage (CB_USAGE_UNSIGNED_LONG);
#endif
  }
#line 16564 "parser.c"
    break;

  case 721:
#line 7350 "parser.y"
  {
	check_and_set_usage (CB_USAGE_SIGNED_CHAR);
  }
#line 16572 "parser.c"
    break;

  case 722:
#line 7354 "parser.y"
  {
	check_and_set_usage (CB_USAGE_UNSIGNED_CHAR);
  }
#line 16580 "parser.c"
    break;

  case 723:
#line 7358 "parser.y"
  {
	check_and_set_usage (CB_USAGE_SIGNED_SHORT);
  }
#line 16588 "parser.c"
    break;

  case 724:
#line 7362 "parser.y"
  {
	check_and_set_usage (CB_USAGE_UNSIGNED_SHORT);
  }
#line 16596 "parser.c"
    break;

  case 725:
#line 7366 "parser.y"
  {
	check_and_set_usage (CB_USAGE_SIGNED_INT);
  }
#line 16604 "parser.c"
    break;

  case 726:
#line 7370 "parser.y"
  {
	check_and_set_usage (CB_USAGE_UNSIGNED_INT);
  }
#line 16612 "parser.c"
    break;

  case 727:
#line 7374 "parser.y"
  {
	check_and_set_usage (CB_USAGE_SIGNED_LONG);
  }
#line 16620 "parser.c"
    break;

  case 728:
#line 7378 "parser.y"
  {
	check_and_set_usage (CB_USAGE_UNSIGNED_LONG);
  }
#line 16628 "parser.c"
    break;

  case 729:
#line 7382 "parser.y"
  {
#ifdef COB_32_BIT_LONG
	check_and_set_usage (CB_USAGE_SIGNED_INT);
#else
	check_and_set_usage (CB_USAGE_SIGNED_LONG);
#endif
  }
#line 16640 "parser.c"
    break;

  case 730:
#line 7390 "parser.y"
  {
#ifdef COB_32_BIT_LONG
	check_and_set_usage (CB_USAGE_UNSIGNED_INT);
#else
	check_and_set_usage (CB_USAGE_UNSIGNED_LONG);
#endif
  }
#line 16652 "parser.c"
    break;

  case 731:
#line 7398 "parser.y"
  {
	check_and_set_usage (CB_USAGE_FP_BIN32);
  }
#line 16660 "parser.c"
    break;

  case 732:
#line 7402 "parser.y"
  {
	check_and_set_usage (CB_USAGE_FP_BIN64);
  }
#line 16668 "parser.c"
    break;

  case 733:
#line 7406 "parser.y"
  {
	check_and_set_usage (CB_USAGE_FP_BIN128);
  }
#line 16676 "parser.c"
    break;

  case 734:
#line 7410 "parser.y"
  {
	check_and_set_usage (CB_USAGE_FP_DEC64);
  }
#line 16684 "parser.c"
    break;

  case 735:
#line 7414 "parser.y"
  {
	check_and_set_usage (CB_USAGE_FP_DEC128);
  }
#line 16692 "parser.c"
    break;

  case 736:
#line 7418 "parser.y"
  {
	check_repeated ("USAGE", SYN_CLAUSE_5, &check_pic_duplicate);
	CB_UNFINISHED ("USAGE NATIONAL");
  }
#line 16701 "parser.c"
    break;

  case 748:
#line 7448 "parser.y"
  {
	check_repeated ("SIGN", SYN_CLAUSE_6, &check_pic_duplicate);
	current_field->flag_sign_clause = 1;
	current_field->flag_sign_separate = (yyvsp[0] ? 1 : 0);
	current_field->flag_sign_leading  = 1;
  }
#line 16712 "parser.c"
    break;

  case 749:
#line 7455 "parser.y"
  {
	check_repeated ("SIGN", SYN_CLAUSE_6, &check_pic_duplicate);
	current_field->flag_sign_clause = 1;
	current_field->flag_sign_separate = (yyvsp[0] ? 1 : 0);
	current_field->flag_sign_leading  = 0;
  }
#line 16723 "parser.c"
    break;

  case 750:
#line 7469 "parser.y"
  {
	/* most of the field attributes are set when parsing the phrases */;
	setup_occurs ();
	setup_occurs_min_max (yyvsp[-4], yyvsp[-3]);
  }
#line 16733 "parser.c"
    break;

  case 752:
#line 7478 "parser.y"
  {
	current_field->step_count = cb_get_int (yyvsp[0]);
  }
#line 16741 "parser.c"
    break;

  case 753:
#line 7488 "parser.y"
  {
	/* most of the field attributes are set when parsing the phrases */;
	setup_occurs ();
	setup_occurs_min_max (yyvsp[-4], yyvsp[-3]);
  }
#line 16751 "parser.c"
    break;

  case 754:
#line 7495 "parser.y"
  {
	current_field->flag_unbounded = 1;
	if (current_field->parent) {
		current_field->parent->flag_unbounded = 1;
	}
	current_field->depending = yyvsp[-1];
	/* most of the field attributes are set when parsing the phrases */;
	setup_occurs ();
	setup_occurs_min_max (yyvsp[-6], cb_int0);
  }
#line 16766 "parser.c"
    break;

  case 755:
#line 7507 "parser.y"
  {
	setup_occurs ();
	current_field->occurs_min = yyvsp[-3] ? cb_get_int (yyvsp[-3]) : 0;
	if (yyvsp[-2]) {
		current_field->occurs_max = cb_get_int (yyvsp[-2]);
		if (current_field->occurs_max <= current_field->occurs_min) {
			cb_error (_("OCCURS TO must be greater than OCCURS FROM"));
		}
	} else {
		current_field->occurs_max = 0;
	}
	CB_PENDING ("OCCURS DYNAMIC");
  }
#line 16784 "parser.c"
    break;

  case 756:
#line 7523 "parser.y"
                                { yyval = NULL; }
#line 16790 "parser.c"
    break;

  case 757:
#line 7524 "parser.y"
                                { yyval = yyvsp[0]; }
#line 16796 "parser.c"
    break;

  case 758:
#line 7528 "parser.y"
                                { yyval = NULL; }
#line 16802 "parser.c"
    break;

  case 759:
#line 7529 "parser.y"
                                { yyval = yyvsp[0]; }
#line 16808 "parser.c"
    break;

  case 760:
#line 7533 "parser.y"
                                { yyval = NULL; }
#line 16814 "parser.c"
    break;

  case 761:
#line 7534 "parser.y"
                                { yyval = yyvsp[-1]; }
#line 16820 "parser.c"
    break;

  case 763:
#line 7539 "parser.y"
  {
	current_field->depending = yyvsp[0];
  }
#line 16828 "parser.c"
    break;

  case 765:
#line 7545 "parser.y"
  {
	yyval = cb_build_index (yyvsp[0], cb_zero, 0, current_field);
	CB_FIELD_PTR (yyval)->index_type = CB_STATIC_INT_INDEX;
  }
#line 16837 "parser.c"
    break;

  case 767:
#line 7553 "parser.y"
  {
	/* current_field->initialized = 1; */
  }
#line 16845 "parser.c"
    break;

  case 770:
#line 7562 "parser.y"
  {
	if (!cb_relaxed_syntax_checks) {
		cb_error (_("INDEXED should follow ASCENDING/DESCENDING"));
	} else {
		cb_warning (cb_warn_additional, _("INDEXED should follow ASCENDING/DESCENDING"));
	}
  }
#line 16857 "parser.c"
    break;

  case 774:
#line 7576 "parser.y"
  {
	cb_tree		l;
	struct cb_key	*keys;
	int		i;
	int		nkeys;

	l = yyvsp[0];
	nkeys = cb_list_length (yyvsp[0]);
	keys = cobc_parse_malloc (sizeof (struct cb_key) * nkeys);

	for (i = 0; i < nkeys; i++) {
		keys[i].dir = CB_PURPOSE_INT (l);
		keys[i].key = CB_VALUE (l);
		l = CB_CHAIN (l);
	}
	current_field->keys = keys;
	current_field->nkeys = nkeys;
  }
#line 16880 "parser.c"
    break;

  case 777:
#line 7603 "parser.y"
  {
	cb_tree ref = NULL;
	cb_tree rchain = NULL;
	cb_tree l;

	/* create reference chaing all the way up
	   as later fields may have same name */
	if (!within_typedef_definition) {
		rchain = cb_build_full_field_reference (current_field->parent);
	}

	for (l = yyvsp[0]; l; l = CB_CHAIN (l)) {
		CB_PURPOSE (l) = yyvsp[-3];
		ref = CB_VALUE (l);
		if (CB_VALID_TREE(ref)) {
			CB_REFERENCE (ref)->chain = rchain;
		}
	}
	keys_list = cb_list_append (keys_list, yyvsp[0]);
	yyval = keys_list;
  }
#line 16906 "parser.c"
    break;

  case 778:
#line 7627 "parser.y"
                                { yyval = cb_int (COB_ASCENDING); }
#line 16912 "parser.c"
    break;

  case 779:
#line 7628 "parser.y"
                                { yyval = cb_int (COB_DESCENDING); }
#line 16918 "parser.c"
    break;

  case 782:
#line 7637 "parser.y"
  {
	current_field->index_list = yyvsp[0];
  }
#line 16926 "parser.c"
    break;

  case 783:
#line 7643 "parser.y"
                                { yyval = CB_LIST_INIT (yyvsp[0]); }
#line 16932 "parser.c"
    break;

  case 784:
#line 7645 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 16938 "parser.c"
    break;

  case 785:
#line 7650 "parser.y"
  {
	yyval = cb_build_index (yyvsp[0], cb_int1, 1U, current_field);
	CB_FIELD_PTR (yyval)->index_type = CB_STATIC_INT_INDEX;
  }
#line 16947 "parser.c"
    break;

  case 786:
#line 7661 "parser.y"
  {
	check_repeated ("JUSTIFIED", SYN_CLAUSE_8, &check_pic_duplicate);
	current_field->flag_justified = 1;
  }
#line 16956 "parser.c"
    break;

  case 787:
#line 7672 "parser.y"
  {
	check_repeated ("SYNCHRONIZED", SYN_CLAUSE_9, &check_pic_duplicate);
	if (cb_verify (cb_synchronized_clause, _("SYNCHRONIZED clause"))) {
		current_field->flag_synchronized = 1;
	}
  }
#line 16967 "parser.c"
    break;

  case 790:
#line 7684 "parser.y"
  {
	CB_PENDING ("SYNCHRONIZED RIGHT");
  }
#line 16975 "parser.c"
    break;

  case 791:
#line 7694 "parser.y"
  {
	check_repeated ("BLANK", SYN_CLAUSE_10, &check_pic_duplicate);
	current_field->flag_blank_zero = 1;
  }
#line 16984 "parser.c"
    break;

  case 792:
#line 7705 "parser.y"
  {
	check_repeated ("BASED", SYN_CLAUSE_11, &check_pic_duplicate);
	if (current_storage == CB_STORAGE_FILE) {
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
#line 17009 "parser.c"
    break;

  case 793:
#line 7731 "parser.y"
  {
	check_repeated ("VALUE", SYN_CLAUSE_12, &check_pic_duplicate);
	current_field->values = yyvsp[0];
  }
#line 17018 "parser.c"
    break;

  case 795:
#line 7739 "parser.y"
                                { yyval = CB_LIST_INIT (yyvsp[0]); }
#line 17024 "parser.c"
    break;

  case 796:
#line 7740 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 17030 "parser.c"
    break;

  case 797:
#line 7744 "parser.y"
                                                { yyval = CB_BUILD_PAIR (yyvsp[-2], yyvsp[0]); }
#line 17036 "parser.c"
    break;

  case 800:
#line 7751 "parser.y"
  {
	if (current_field->level != 88) {
		cb_error (_("FALSE clause only allowed for 88 level"));
	}
	current_field->false_88 = CB_LIST_INIT (yyvsp[0]);
  }
#line 17047 "parser.c"
    break;

  case 801:
#line 7763 "parser.y"
  {
	check_repeated ("ANY", SYN_CLAUSE_14, &check_pic_duplicate);
	if (current_field->flag_item_based) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "ANY LENGTH");
	} else {
		current_field->flag_any_length = 1;
	}
  }
#line 17060 "parser.c"
    break;

  case 802:
#line 7772 "parser.y"
  {
	check_repeated ("ANY", SYN_CLAUSE_14, &check_pic_duplicate);
	if (current_field->flag_item_based) {
		cb_error (_("%s and %s are mutually exclusive"), "BASED", "ANY NUMERIC");
	} else {
		current_field->flag_any_length = 1;
		current_field->flag_any_numeric = 1;
	}
  }
#line 17074 "parser.c"
    break;

  case 803:
#line 7787 "parser.y"
  {
	check_repeated ("EXTERNAL-FORM", SYN_CLAUSE_2, &check_pic_duplicate);
	CB_PENDING ("EXTERNAL-FORM");
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
#line 17094 "parser.c"
    break;

  case 804:
#line 7810 "parser.y"
  {
	check_repeated ("IDENTIFIED BY", SYN_CLAUSE_3, &check_pic_duplicate);
	if (!current_field->flag_is_external_form) {
		CB_PENDING ("EXTERNAL-FORM (IDENTIFIED BY)");
		if (current_storage != CB_STORAGE_WORKING) {
			cb_error (_("%s not allowed here"), "IDENTIFIED BY");
		} else if (!qualifier) {
			cb_error (_("%s requires a data name"), "IDENTIFIED BY");
		} else if (current_field->redefines) {
			cb_error (_("%s and %s combination not allowed"), "IDENTIFIED BY", "REDEFINES");
		}
	}
	current_field->external_form_identifier = yyvsp[0];
  }
#line 17113 "parser.c"
    break;

  case 806:
#line 7830 "parser.y"
  {
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_LOCAL_STORAGE_SECTION;
	current_storage = CB_STORAGE_LOCAL;
	if (current_program->nested_level) {
		cb_error (_("%s not allowed in nested programs"), "LOCAL-STORAGE");
	} else if (cb_local_implies_recursive) {
		current_program->flag_recursive = 1;
	}
  }
#line 17128 "parser.c"
    break;

  case 807:
#line 7841 "parser.y"
  {
	if (yyvsp[0]) {
		current_program->local_storage = CB_FIELD (yyvsp[0]);
	}
  }
#line 17138 "parser.c"
    break;

  case 809:
#line 7853 "parser.y"
  {
	check_headers_present (COBC_HD_DATA_DIVISION, 0, 0, 0);
	header_check |= COBC_HD_LINKAGE_SECTION;
	current_storage = CB_STORAGE_LINKAGE;
  }
#line 17148 "parser.c"
    break;

  case 810:
#line 7859 "parser.y"
  {
	if (yyvsp[0]) {
		current_program->linkage_storage = CB_FIELD (yyvsp[0]);
	}
  }
#line 17158 "parser.c"
    break;

  case 812:
#line 7870 "parser.y"
  {
	header_check |= COBC_HD_REPORT_SECTION;
	current_storage = CB_STORAGE_REPORT;
	description_field = NULL;
	current_program->flag_report = 1;
	cb_clear_real_field ();
  }
#line 17170 "parser.c"
    break;

  case 816:
#line 7888 "parser.y"
  {
	if (CB_INVALID_TREE (yyvsp[0])) {
		YYERROR;
	} else {
		current_field = NULL;
		control_field = NULL;
		description_field = NULL;
		current_report = CB_REPORT_PTR (yyvsp[0]);
	}
	check_duplicate = 0;
  }
#line 17186 "parser.c"
    break;

  case 817:
#line 7901 "parser.y"
  {
	yyval = get_finalized_description_tree ();

	current_program->report_storage = description_field;
	current_program->flag_report = 1;
	if (current_report->records == NULL) {
		current_report->records = description_field;
	}
	finalize_report (current_report, description_field);
  }
#line 17201 "parser.c"
    break;

  case 820:
#line 7916 "parser.y"
  {
	yyerrok;
  }
#line 17209 "parser.c"
    break;

  case 821:
#line 7923 "parser.y"
  {
	check_repeated ("GLOBAL", SYN_CLAUSE_1, &check_duplicate);
	current_report->global = 1;
	cb_error (_("GLOBAL is not allowed with RD"));
  }
#line 17219 "parser.c"
    break;

  case 822:
#line 7929 "parser.y"
  {
	check_repeated ("CODE", SYN_CLAUSE_2, &check_duplicate);
	current_report->code_clause = yyvsp[0];
  }
#line 17228 "parser.c"
    break;

  case 825:
#line 7941 "parser.y"
  {
	check_repeated ("CONTROL", SYN_CLAUSE_3, &check_duplicate);
  }
#line 17236 "parser.c"
    break;

  case 829:
#line 7954 "parser.y"
  {
	current_report->control_final = 1;
  }
#line 17244 "parser.c"
    break;

  case 832:
#line 7966 "parser.y"
  {
	/* Add field to current control list */
	CB_ADD_TO_CHAIN (yyvsp[0], current_report->controls);
  }
#line 17253 "parser.c"
    break;

  case 833:
#line 7977 "parser.y"
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
#line 17292 "parser.c"
    break;

  case 834:
#line 8015 "parser.y"
  {
	if (CB_LITERAL_P (yyvsp[-1])) {
		current_report->lines = cb_get_int (yyvsp[-1]);
		if (current_report->lines > 999) {
			cb_error ("PAGE LIMIT lines > 999");
		}
	} else {
		current_report->t_lines = yyvsp[-1];
	}
  }
#line 17307 "parser.c"
    break;

  case 836:
#line 8027 "parser.y"
  {
	if (CB_LITERAL_P (yyvsp[-2])) {
		current_report->lines = cb_get_int (yyvsp[-2]);
		if (current_report->lines > 999) {
			cb_error ("PAGE LIMIT lines > 999");
		}
	} else {
		current_report->t_lines = yyvsp[-2];
	}
  }
#line 17322 "parser.c"
    break;

  case 837:
#line 8041 "parser.y"
  {
	/* may be repeated later by page detail */
	check_repeated ("LINE LIMIT", SYN_CLAUSE_5, &check_duplicate);
	if (CB_LITERAL_P (yyvsp[-1])) {
		current_report->columns = cb_get_int (yyvsp[-1]);
	} else {
		current_report->t_columns = yyvsp[-1];
	}
  }
#line 17336 "parser.c"
    break;

  case 847:
#line 8069 "parser.y"
  {
	check_repeated ("LINE LIMIT", SYN_CLAUSE_5, &check_duplicate);
	if (CB_LITERAL_P (yyvsp[0])) {
		current_report->columns = cb_get_int (yyvsp[0]);
	} else {
		current_report->t_columns = yyvsp[0];
	}
  }
#line 17349 "parser.c"
    break;

  case 848:
#line 8081 "parser.y"
  {
	check_repeated ("HEADING", SYN_CLAUSE_6, &check_duplicate);
	error_if_no_page_lines_limit ("HEADING");

	if (CB_LITERAL_P (yyvsp[0])) {
		current_report->heading = cb_get_int (yyvsp[0]);
	} else {
		current_report->t_heading = yyvsp[0];
	}
  }
#line 17364 "parser.c"
    break;

  case 849:
#line 8095 "parser.y"
  {
	check_repeated ("FIRST DETAIL", SYN_CLAUSE_7, &check_duplicate);
	error_if_no_page_lines_limit ("FIRST DETAIL");

	if (CB_LITERAL_P (yyvsp[0])) {
		current_report->first_detail = cb_get_int (yyvsp[0]);
	} else {
		current_report->t_first_detail = yyvsp[0];
	}
  }
#line 17379 "parser.c"
    break;

  case 850:
#line 8109 "parser.y"
  {
	check_repeated ("LAST CONTROL HEADING", SYN_CLAUSE_8, &check_duplicate);
	error_if_no_page_lines_limit ("LAST CONTROL HEADING");

	if (CB_LITERAL_P (yyvsp[0])) {
		current_report->last_control = cb_get_int (yyvsp[0]);
	} else {
		current_report->t_last_control = yyvsp[0];
	}
  }
#line 17394 "parser.c"
    break;

  case 851:
#line 8123 "parser.y"
  {
	check_repeated ("LAST DETAIL", SYN_CLAUSE_9, &check_duplicate);
	error_if_no_page_lines_limit ("LAST DETAIL");

	if (CB_LITERAL_P (yyvsp[0])) {
		current_report->last_detail = cb_get_int (yyvsp[0]);
	} else {
		current_report->t_last_detail = yyvsp[0];
	}
  }
#line 17409 "parser.c"
    break;

  case 852:
#line 8137 "parser.y"
  {
	check_repeated ("FOOTING", SYN_CLAUSE_10, &check_duplicate);
	error_if_no_page_lines_limit ("FOOTING");

	if (CB_LITERAL_P (yyvsp[0])) {
		current_report->footing = cb_get_int (yyvsp[0]);
	} else {
		current_report->t_footing = yyvsp[0];
	}
  }
#line 17424 "parser.c"
    break;

  case 855:
#line 8155 "parser.y"
  {
	if (set_current_field(yyvsp[-1], yyvsp[0])) {
		YYERROR;
	}
	if (!description_field) {
		description_field = current_field;
	}
  }
#line 17437 "parser.c"
    break;

  case 857:
#line 8165 "parser.y"
  {
	/* Free tree associated with level number */
	cobc_parse_free (yyvsp[-2]);
	cb_unput_dot ();
	yyerrok;
	check_pic_duplicate = 0;
	check_duplicate = 0;
	current_field = cb_get_real_field ();
  }
#line 17451 "parser.c"
    break;

  case 877:
#line 8202 "parser.y"
  {
	check_repeated ("TYPE IS", SYN_CLAUSE_16, &check_pic_duplicate);
  }
#line 17459 "parser.c"
    break;

  case 878:
#line 8209 "parser.y"
  {
	current_field->report_flag |= COB_REPORT_HEADING;
  }
#line 17467 "parser.c"
    break;

  case 879:
#line 8213 "parser.y"
  {
	current_field->report_flag |= COB_REPORT_PAGE_HEADING;
  }
#line 17475 "parser.c"
    break;

  case 882:
#line 8219 "parser.y"
  {
	if (current_report != NULL) {
		current_report->has_detail = 1;
	}
	current_field->report_flag |= COB_REPORT_DETAIL;
  }
#line 17486 "parser.c"
    break;

  case 883:
#line 8226 "parser.y"
  {
	current_field->report_flag |= COB_REPORT_PAGE_FOOTING;
  }
#line 17494 "parser.c"
    break;

  case 884:
#line 8230 "parser.y"
  {
	current_field->report_flag |= COB_REPORT_FOOTING;
  }
#line 17502 "parser.c"
    break;

  case 885:
#line 8237 "parser.y"
  {
	current_field->report_flag |= COB_REPORT_CONTROL_HEADING;
  }
#line 17510 "parser.c"
    break;

  case 886:
#line 8241 "parser.y"
  {
	current_field->report_flag |= COB_REPORT_CONTROL_HEADING;
	current_field->report_control = yyvsp[-1];
	if (yyvsp[0]) {
		current_field->report_flag |= COB_REPORT_PAGE;
	}
  }
#line 17522 "parser.c"
    break;

  case 887:
#line 8249 "parser.y"
  {
	current_field->report_flag |= COB_REPORT_CONTROL_HEADING_FINAL;
  }
#line 17530 "parser.c"
    break;

  case 888:
#line 8258 "parser.y"
                {yyval = NULL;}
#line 17536 "parser.c"
    break;

  case 889:
#line 8259 "parser.y"
                        {yyval = cb_int0;}
#line 17542 "parser.c"
    break;

  case 890:
#line 8264 "parser.y"
  {
	current_field->report_flag |= COB_REPORT_CONTROL_FOOTING;
  }
#line 17550 "parser.c"
    break;

  case 891:
#line 8268 "parser.y"
  {
	current_field->report_flag |= COB_REPORT_CONTROL_FOOTING;
	current_field->report_control = yyvsp[-1];
  }
#line 17559 "parser.c"
    break;

  case 892:
#line 8273 "parser.y"
  {
	current_field->report_flag |= COB_REPORT_CONTROL_FOOTING_FINAL;
  }
#line 17567 "parser.c"
    break;

  case 893:
#line 8277 "parser.y"
  {
	current_field->report_flag |= COB_REPORT_CONTROL_FOOTING;
	current_field->report_flag |= COB_REPORT_ALL;
  }
#line 17576 "parser.c"
    break;

  case 894:
#line 8285 "parser.y"
  {
	check_repeated ("NEXT GROUP", SYN_CLAUSE_17, &check_pic_duplicate);
  }
#line 17584 "parser.c"
    break;

  case 895:
#line 8292 "parser.y"
  {
	if (CB_LITERAL_P(yyvsp[0]) && CB_LITERAL (yyvsp[0])->sign > 0) {
		current_field->report_flag |= COB_REPORT_NEXT_GROUP_PLUS;
	} else {
		current_field->report_flag |= COB_REPORT_NEXT_GROUP_LINE;
	}
	current_field->next_group_line = cb_get_int (yyvsp[0]);
  }
#line 17597 "parser.c"
    break;

  case 896:
#line 8301 "parser.y"
  {
	current_field->report_flag |= COB_REPORT_NEXT_GROUP_PLUS;
	current_field->next_group_line = cb_get_int(yyvsp[0]);
  }
#line 17606 "parser.c"
    break;

  case 897:
#line 8306 "parser.y"
  {
	current_field->report_flag |= COB_REPORT_NEXT_GROUP_PAGE;
  }
#line 17614 "parser.c"
    break;

  case 901:
#line 8319 "parser.y"
  {
	check_repeated ("SUM", SYN_CLAUSE_19, &check_pic_duplicate);
	current_field->report_sum_list = yyvsp[-1];
	build_sum_counter (current_report, current_field);
  }
#line 17624 "parser.c"
    break;

  case 904:
#line 8329 "parser.y"
  {
	current_field->report_sum_upon = yyvsp[0];
  }
#line 17632 "parser.c"
    break;

  case 905:
#line 8336 "parser.y"
  {
	current_field->report_reset = yyvsp[0];
  }
#line 17640 "parser.c"
    break;

  case 906:
#line 8340 "parser.y"
  {
	current_field->report_flag |= COB_REPORT_RESET_FINAL;
  }
#line 17648 "parser.c"
    break;

  case 907:
#line 8347 "parser.y"
  {
	check_repeated ("PRESENT", SYN_CLAUSE_20, &check_pic_duplicate);
	current_field->report_when = yyvsp[0];
  }
#line 17657 "parser.c"
    break;

  case 908:
#line 8352 "parser.y"
  {
	check_repeated ("PRESENT", SYN_CLAUSE_20, &check_pic_duplicate);
	current_field->report_flag |= COB_REPORT_PRESENT;
	current_field->report_flag &= ~COB_REPORT_BEFORE;
  }
#line 17667 "parser.c"
    break;

  case 909:
#line 8358 "parser.y"
  {
	check_repeated ("PRESENT", SYN_CLAUSE_20, &check_pic_duplicate);
	current_field->report_flag |= COB_REPORT_PRESENT;
	current_field->report_flag &= ~COB_REPORT_BEFORE;
	current_field->report_flag |= COB_REPORT_PAGE;
  }
#line 17678 "parser.c"
    break;

  case 910:
#line 8365 "parser.y"
  {
	check_repeated ("PRESENT", SYN_CLAUSE_20, &check_pic_duplicate);
	current_field->report_flag |= COB_REPORT_PRESENT;
	current_field->report_flag |= COB_REPORT_BEFORE;
  }
#line 17688 "parser.c"
    break;

  case 911:
#line 8371 "parser.y"
  {
	check_repeated ("PRESENT", SYN_CLAUSE_20, &check_pic_duplicate);
	current_field->report_flag |= COB_REPORT_PRESENT;
	current_field->report_flag |= COB_REPORT_BEFORE;
	current_field->report_flag |= COB_REPORT_PAGE;
  }
#line 17699 "parser.c"
    break;

  case 912:
#line 8381 "parser.y"
  {
	current_field->report_flag |= COB_REPORT_PRESENT;
  }
#line 17707 "parser.c"
    break;

  case 913:
#line 8385 "parser.y"
  {
	current_field->report_flag |= COB_REPORT_PRESENT;
	current_field->report_flag |= COB_REPORT_NEGATE;
  }
#line 17716 "parser.c"
    break;

  case 916:
#line 8398 "parser.y"
  {
	current_field->report_flag |= COB_REPORT_PAGE;
  }
#line 17724 "parser.c"
    break;

  case 917:
#line 8402 "parser.y"
  {
	current_field->report_control = yyvsp[0];
  }
#line 17732 "parser.c"
    break;

  case 919:
#line 8410 "parser.y"
  {
	CB_PENDING ("RW VARYING clause");
  }
#line 17740 "parser.c"
    break;

  case 920:
#line 8417 "parser.y"
  {
	check_repeated ("LINE", SYN_CLAUSE_21, &check_pic_duplicate);
	current_field->report_flag |= COB_REPORT_LINE;
  }
#line 17749 "parser.c"
    break;

  case 925:
#line 8435 "parser.y"
  {
	current_field->report_flag |= COB_REPORT_LINE_NEXT_PAGE;
  }
#line 17757 "parser.c"
    break;

  case 926:
#line 8439 "parser.y"
  {
	current_field->report_line = cb_get_int (yyvsp[0]);
	if (yyvsp[-1]) {
		current_field->report_flag |= COB_REPORT_LINE_PLUS;
		if (current_field->report_line == 0) {
			CB_PENDING ("LINE PLUS 0");
		}
	}
  }
#line 17771 "parser.c"
    break;

  case 927:
#line 8453 "parser.y"
  {
	check_repeated ("COLUMN", SYN_CLAUSE_18, &check_pic_duplicate);
	if ((current_field->report_flag & (COB_REPORT_COLUMN_LEFT|COB_REPORT_COLUMN_RIGHT|COB_REPORT_COLUMN_CENTER))
	 && (current_field->report_flag & COB_REPORT_COLUMN_PLUS)) {
		if (cb_relaxed_syntax_checks) {
			cb_warning (COBC_WARN_FILLER, _("PLUS is not recommended with LEFT, RIGHT or CENTER"));
		} else {
			cb_error (_("PLUS is not allowed with LEFT, RIGHT or CENTER"));
		}
	}
  }
#line 17787 "parser.c"
    break;

  case 931:
#line 8477 "parser.y"
  {
	current_field->report_flag |= COB_REPORT_COLUMN_LEFT;
  }
#line 17795 "parser.c"
    break;

  case 932:
#line 8481 "parser.y"
  {
	current_field->report_flag |= COB_REPORT_COLUMN_RIGHT;
  }
#line 17803 "parser.c"
    break;

  case 933:
#line 8485 "parser.y"
  {
	current_field->report_flag |= COB_REPORT_COLUMN_CENTER;
  }
#line 17811 "parser.c"
    break;

  case 934:
#line 8492 "parser.y"
  {
	int colnum = cb_get_int (yyvsp[0]);
	if (colnum != 0) {
		if (current_field->parent
		 && current_field->parent->children == current_field) {
			cb_warning (COBC_WARN_FILLER, _("PLUS is ignored on first field of line"));
			if (current_field->step_count == 0) {
				current_field->step_count = colnum;
			}
		} else {
			current_field->report_flag |= COB_REPORT_COLUMN_PLUS;
		}
	} else {
		colnum = 0;
	}
	if (current_field->report_column == 0) {
		current_field->report_column = colnum;
	}
	current_field->report_num_col++;
  }
#line 17836 "parser.c"
    break;

  case 938:
#line 8522 "parser.y"
  {
	int colnum;
	colnum = cb_get_int (yyvsp[0]);
	if (colnum < 0) {
		/* already handled by integer check */
	} else if (colnum == 0) {
		cb_error (_("invalid COLUMN integer; must be > 0"));
	} else if (colnum <= current_field->report_column) {
		cb_warning (COBC_WARN_FILLER, _("COLUMN numbers should increase"));
	}
	current_field->report_column_list =
			cb_list_append (current_field->report_column_list, CB_LIST_INIT (yyvsp[0]));
	if (current_field->report_column == 0) {
		current_field->report_column = colnum;
	}
	current_field->report_num_col++;
  }
#line 17858 "parser.c"
    break;

  case 939:
#line 8543 "parser.y"
  {
	check_repeated ("SOURCE", SYN_CLAUSE_22, &check_pic_duplicate);
	current_field->report_source = yyvsp[-1];
  }
#line 17867 "parser.c"
    break;

  case 940:
#line 8551 "parser.y"
  {
	check_repeated ("GROUP", SYN_CLAUSE_23, &check_pic_duplicate);
	current_field->report_flag |= COB_REPORT_GROUP_INDICATE;
  }
#line 17876 "parser.c"
    break;

  case 942:
#line 8561 "parser.y"
  {
	cobc_cs_check = CB_CS_SCREEN;
	current_storage = CB_STORAGE_SCREEN;
	current_field = NULL;
	description_field = NULL;
	cb_clear_real_field ();
  }
#line 17888 "parser.c"
    break;

  case 943:
#line 8569 "parser.y"
  {
	if (description_field) {
		get_finalized_description_tree ();
		current_program->screen_storage = description_field;
		current_program->flag_screen = 1;
	}
	cobc_cs_check = 0;
  }
#line 17901 "parser.c"
    break;

  case 949:
#line 8592 "parser.y"
  {
	if (set_current_field (yyvsp[-1], yyvsp[0])) {
		YYERROR;
	}
	if (current_field->parent) {
		current_field->screen_foreg = current_field->parent->screen_foreg;
		current_field->screen_backg = current_field->parent->screen_backg;
		current_field->screen_prompt = current_field->parent->screen_prompt;
	}
  }
#line 17916 "parser.c"
    break;

  case 950:
#line 8603 "parser.y"
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

	if (!description_field) {
		description_field = current_field;
	}
	if (current_field->flag_occurs
	 && !has_relative_pos (current_field)) {
		cb_error (_("relative LINE/COLUMN clause required with OCCURS"));
	}
  }
#line 17958 "parser.c"
    break;

  case 951:
#line 8642 "parser.y"
  {
	if (set_current_field (yyvsp[-1], yyvsp[0])) {
		YYERROR;
	}

	if (current_field->parent) {
		current_field->screen_foreg = current_field->parent->screen_foreg;
		current_field->screen_backg = current_field->parent->screen_backg;
		current_field->screen_prompt = current_field->parent->screen_prompt;
	}
  }
#line 17974 "parser.c"
    break;

  case 952:
#line 8654 "parser.y"
  {
	CB_PENDING ("GRAPHICAL CONTROL");
  }
#line 17982 "parser.c"
    break;

  case 953:
#line 8659 "parser.y"
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

	if (!description_field) {
		description_field = current_field;
	}
	if (current_field->flag_occurs
	 && !has_relative_pos (current_field)) {
		cb_error (_("relative LINE/COLUMN clause required with OCCURS"));
	}
	cobc_cs_check = CB_CS_SCREEN;
  }
#line 18025 "parser.c"
    break;

  case 954:
#line 8699 "parser.y"
  {
	/*
	  Tree associated with level number has already been freed; we don't
	  need to do anything here.
	*/
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
#line 18047 "parser.c"
    break;

  case 957:
#line 8725 "parser.y"
  {
	set_screen_attr_with_conflict ("BLANK LINE", COB_SCREEN_BLANK_LINE,
				       "BLANK SCREEN", COB_SCREEN_BLANK_SCREEN);
  }
#line 18056 "parser.c"
    break;

  case 958:
#line 8730 "parser.y"
  {
	set_screen_attr_with_conflict ("BLANK SCREEN", COB_SCREEN_BLANK_SCREEN,
				       "BLANK LINE", COB_SCREEN_BLANK_LINE);
  }
#line 18065 "parser.c"
    break;

  case 959:
#line 8735 "parser.y"
  {
	set_screen_attr ("BELL", COB_SCREEN_BELL);
  }
#line 18073 "parser.c"
    break;

  case 960:
#line 8739 "parser.y"
  {
	set_screen_attr ("BLINK", COB_SCREEN_BLINK);
  }
#line 18081 "parser.c"
    break;

  case 961:
#line 8743 "parser.y"
  {
	set_screen_attr_with_conflict ("ERASE EOL", COB_SCREEN_ERASE_EOL,
				       "ERASE EOS", COB_SCREEN_ERASE_EOS);
  }
#line 18090 "parser.c"
    break;

  case 962:
#line 8748 "parser.y"
  {
	set_screen_attr_with_conflict ("ERASE EOS", COB_SCREEN_ERASE_EOS,
				       "ERASE EOL", COB_SCREEN_ERASE_EOL);
  }
#line 18099 "parser.c"
    break;

  case 963:
#line 8753 "parser.y"
  {
	set_screen_attr_with_conflict ("HIGHLIGHT", COB_SCREEN_HIGHLIGHT,
				       "LOWLIGHT", COB_SCREEN_LOWLIGHT);
  }
#line 18108 "parser.c"
    break;

  case 964:
#line 8758 "parser.y"
  {
	set_screen_attr_with_conflict ("LOWLIGHT", COB_SCREEN_LOWLIGHT,
				       "HIGHLIGHT", COB_SCREEN_HIGHLIGHT);
  }
#line 18117 "parser.c"
    break;

  case 965:
#line 8763 "parser.y"
  {
	CB_PENDING ("STANDARD intensity");
#if 0 /* in general we could simply remove high/low, but for syntax checks
	we still need a flag */
	set_screen_attr_with_conflict ("LOWLIGHT", COB_SCREEN_LOWLIGHT,
				       "HIGHLIGHT", COB_SCREEN_HIGHLIGHT);
#endif
  }
#line 18130 "parser.c"
    break;

  case 966:
#line 8772 "parser.y"
  {
	CB_PENDING ("BACKGROUND intensity");
  }
#line 18138 "parser.c"
    break;

  case 967:
#line 8776 "parser.y"
  {
	CB_PENDING ("BACKGROUND intensity");
  }
#line 18146 "parser.c"
    break;

  case 968:
#line 8780 "parser.y"
  {
	CB_PENDING ("BACKGROUND intensity");
  }
#line 18154 "parser.c"
    break;

  case 969:
#line 8784 "parser.y"
  {
	set_screen_attr ("REVERSE-VIDEO", COB_SCREEN_REVERSE);
  }
#line 18162 "parser.c"
    break;

  case 970:
#line 8788 "parser.y"
  {
	/* set_screen_attr ("SIZE", COB_SCREEN_SIZE); */
	CB_PENDING ("SIZE clause");
	current_field->size = cb_get_int (yyvsp[0]);
  }
#line 18172 "parser.c"
    break;

  case 971:
#line 8794 "parser.y"
  {
	CB_PENDING (_("screen positions from data-item"));
  }
#line 18180 "parser.c"
    break;

  case 972:
#line 8798 "parser.y"
  {
	CB_PENDING (_("screen positions from data-item"));
	CB_PENDING ("SIZE clause");
  }
#line 18189 "parser.c"
    break;

  case 973:
#line 8803 "parser.y"
  {
	/* set_screen_attr ("SIZE", COB_SCREEN_SIZE); */
	CB_PENDING ("SIZE clause");
	current_field->size = cb_get_int (yyvsp[0]);
  }
#line 18199 "parser.c"
    break;

  case 974:
#line 8809 "parser.y"
  {
	set_screen_attr ("UNDERLINE", COB_SCREEN_UNDERLINE);
  }
#line 18207 "parser.c"
    break;

  case 975:
#line 8813 "parser.y"
  {
	set_screen_attr ("OVERLINE", COB_SCREEN_OVERLINE);
	CB_PENDING ("OVERLINE");
  }
#line 18216 "parser.c"
    break;

  case 976:
#line 8818 "parser.y"
  {
	set_screen_attr ("GRID", COB_SCREEN_GRID);
	CB_PENDING ("GRID");
  }
#line 18225 "parser.c"
    break;

  case 977:
#line 8823 "parser.y"
  {
	set_screen_attr ("LEFTLINE", COB_SCREEN_LEFTLINE);
	CB_PENDING ("LEFTLINE");
  }
#line 18234 "parser.c"
    break;

  case 978:
#line 8828 "parser.y"
  {
	set_screen_attr_with_conflict ("AUTO", COB_SCREEN_AUTO,
				       "TAB", COB_SCREEN_TAB);
  }
#line 18243 "parser.c"
    break;

  case 979:
#line 8833 "parser.y"
  {
	set_screen_attr_with_conflict ("TAB", COB_SCREEN_TAB,
				       "AUTO", COB_SCREEN_AUTO);
  }
#line 18252 "parser.c"
    break;

  case 980:
#line 8838 "parser.y"
  {
	set_screen_attr_with_conflict ("SECURE", COB_SCREEN_SECURE,
				       "NO-ECHO", COB_SCREEN_NO_ECHO);
  }
#line 18261 "parser.c"
    break;

  case 981:
#line 8843 "parser.y"
  {
	if (cb_no_echo_means_secure) {
		set_screen_attr ("SECURE", COB_SCREEN_SECURE);
	} else {
		set_screen_attr_with_conflict ("NO-ECHO", COB_SCREEN_NO_ECHO,
					       "SECURE", COB_SCREEN_SECURE);
	}
  }
#line 18274 "parser.c"
    break;

  case 982:
#line 8852 "parser.y"
  {
	set_screen_attr ("REQUIRED", COB_SCREEN_REQUIRED);
  }
#line 18282 "parser.c"
    break;

  case 983:
#line 8856 "parser.y"
  {
	set_screen_attr ("FULL", COB_SCREEN_FULL);
  }
#line 18290 "parser.c"
    break;

  case 984:
#line 8860 "parser.y"
  {
	/* FIXME: ACUCOBOL and (undocumented) MF have CHARACTER as optional here */
	set_screen_attr ("PROMPT", COB_SCREEN_PROMPT);
	current_field->screen_prompt = yyvsp[0];
  }
#line 18300 "parser.c"
    break;

  case 985:
#line 8866 "parser.y"
  {
	set_screen_attr ("PROMPT", COB_SCREEN_PROMPT);
  }
#line 18308 "parser.c"
    break;

  case 986:
#line 8870 "parser.y"
  {
	set_screen_attr ("INITIAL", COB_SCREEN_INITIAL);
  }
#line 18316 "parser.c"
    break;

  case 987:
#line 8874 "parser.y"
  {
	check_repeated ("LINE", SYN_CLAUSE_16, &check_pic_duplicate);
  }
#line 18324 "parser.c"
    break;

  case 988:
#line 8878 "parser.y"
  {
	CB_PENDING ("LINES clause");	/* note: should only occur with controls */
  }
#line 18332 "parser.c"
    break;

  case 989:
#line 8882 "parser.y"
  {
	/*check_repeated ("CLINE", SYN_CLAUSE_5000, &check_pic_duplicate);*/
  }
#line 18340 "parser.c"
    break;

  case 990:
#line 8886 "parser.y"
  {
	check_repeated ("COLUMN", SYN_CLAUSE_17, &check_pic_duplicate);
  }
#line 18348 "parser.c"
    break;

  case 991:
#line 8890 "parser.y"
  {
	/*check_repeated ("CCOL", SYN_CLAUSE_5001, &check_pic_duplicate);*/
  }
#line 18356 "parser.c"
    break;

  case 992:
#line 8894 "parser.y"
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
#line 18371 "parser.c"
    break;

  case 993:
#line 8905 "parser.y"
  {
	check_repeated ("FOREGROUND-COLOR", SYN_CLAUSE_18, &check_pic_duplicate);
	current_field->screen_foreg = yyvsp[0];
  }
#line 18380 "parser.c"
    break;

  case 994:
#line 8910 "parser.y"
  {
	check_repeated ("BACKGROUND-COLOR", SYN_CLAUSE_19, &check_pic_duplicate);
	current_field->screen_backg = yyvsp[0];
  }
#line 18389 "parser.c"
    break;

  case 1003:
#line 8926 "parser.y"
  {
	yyval = check_not_88_level (yyvsp[0]);

	check_repeated ("USING", SYN_CLAUSE_20, &check_pic_duplicate);
	current_field->screen_from = yyval;
	current_field->screen_to = yyval;
	current_field->screen_flag |= COB_SCREEN_INPUT;
  }
#line 18402 "parser.c"
    break;

  case 1004:
#line 8935 "parser.y"
  {
	check_repeated ("FROM", SYN_CLAUSE_21, &check_pic_duplicate);
	current_field->screen_from = yyvsp[0];
  }
#line 18411 "parser.c"
    break;

  case 1005:
#line 8940 "parser.y"
  {
	yyval = check_not_88_level (yyvsp[0]);

	check_repeated ("TO", SYN_CLAUSE_22, &check_pic_duplicate);
	current_field->screen_to = yyval;
	current_field->screen_flag |= COB_SCREEN_INPUT;
  }
#line 18423 "parser.c"
    break;

  case 1007:
#line 8952 "parser.y"
  {
	cobc_cs_check |= CB_CS_GRAPHICAL_CONTROL;
  }
#line 18431 "parser.c"
    break;

  case 1307:
#line 9541 "parser.y"
              { yyval = NULL; }
#line 18437 "parser.c"
    break;

  case 1308:
#line 9542 "parser.y"
              { yyval = yyvsp[0]; }
#line 18443 "parser.c"
    break;

  case 1309:
#line 9546 "parser.y"
              { yyval = cb_int0; }
#line 18449 "parser.c"
    break;

  case 1312:
#line 9554 "parser.y"
               { yyval = cb_int1; }
#line 18455 "parser.c"
    break;

  case 1316:
#line 9566 "parser.y"
                { yyval = yyvsp[-1]; }
#line 18461 "parser.c"
    break;

  case 1317:
#line 9567 "parser.y"
                { yyval = cb_int1; }
#line 18467 "parser.c"
    break;

  case 1318:
#line 9571 "parser.y"
                { yyval = NULL; }
#line 18473 "parser.c"
    break;

  case 1319:
#line 9572 "parser.y"
                        { yyval = cb_int0; }
#line 18479 "parser.c"
    break;

  case 1320:
#line 9577 "parser.y"
  {
	if (yyvsp[0]) {
		current_field->screen_line = yyvsp[0];
	}
  }
#line 18489 "parser.c"
    break;

  case 1322:
#line 9587 "parser.y"
  {
	current_field->screen_flag |= COB_SCREEN_LINE_PLUS;
  }
#line 18497 "parser.c"
    break;

  case 1323:
#line 9591 "parser.y"
  {
	current_field->screen_flag |= COB_SCREEN_LINE_MINUS;
  }
#line 18505 "parser.c"
    break;

  case 1324:
#line 9598 "parser.y"
  {
	if (yyvsp[0]) {
		current_field->screen_column = yyvsp[0];
	}
  }
#line 18515 "parser.c"
    break;

  case 1325:
#line 9607 "parser.y"
  {
	/* Nothing */
  }
#line 18523 "parser.c"
    break;

  case 1326:
#line 9611 "parser.y"
  {
	current_field->screen_flag |= COB_SCREEN_COLUMN_PLUS;
  }
#line 18531 "parser.c"
    break;

  case 1327:
#line 9615 "parser.y"
  {
	current_field->screen_flag |= COB_SCREEN_COLUMN_MINUS;
  }
#line 18539 "parser.c"
    break;

  case 1328:
#line 9622 "parser.y"
  {
	CB_PENDING (_("OCCURS screen items"));
	check_repeated ("OCCURS", SYN_CLAUSE_23, &check_pic_duplicate);
	current_field->occurs_max = cb_get_int (yyvsp[-1]);
	current_field->occurs_min = current_field->occurs_max;
	current_field->indexes++;
	current_field->flag_occurs = 1;
  }
#line 18552 "parser.c"
    break;

  case 1329:
#line 9634 "parser.y"
  {
	CB_PENDING (_("GLOBAL screen items"));
  }
#line 18560 "parser.c"
    break;

  case 1330:
#line 9643 "parser.y"
  {
	current_section = NULL;
	current_paragraph = NULL;
	check_pic_duplicate = 0;
	check_duplicate = 0;
	if (!current_program->entry_convention) {
		current_program->entry_convention = cb_int (CB_CONV_COBOL);
	}
  }
#line 18574 "parser.c"
    break;

  case 1332:
#line 9657 "parser.y"
  {
	current_section = NULL;
	current_paragraph = NULL;
	check_pic_duplicate = 0;
	check_duplicate = 0;
	cobc_in_procedure = 1U;
	cb_set_system_names ();
	backup_current_pos ();
  }
#line 18588 "parser.c"
    break;

  case 1333:
#line 9667 "parser.y"
  {
	cb_tree call_conv = yyvsp[-4];
	if (yyvsp[-3]) {
		call_conv = yyvsp[-3];
		if (yyvsp[-4]) {
			/* note: $4 is likely to be a reference to SPECIAL-NAMES */
			cb_error_x (yyvsp[-3], _("%s and %s are mutually exclusive"),
				"CALL-CONVENTION", "WITH LINKAGE");
		}
	}
	if (call_conv) {
		if (current_program->entry_convention) {
			cb_warning (COBC_WARN_FILLER,
				_("overriding convention specified in ENTRY-CONVENTION"));
		}
		current_program->entry_convention = call_conv;
	} else if (!current_program->entry_convention) {
		current_program->entry_convention = cb_int (CB_CONV_COBOL);
	}
	header_check |= COBC_HD_PROCEDURE_DIVISION;
  }
#line 18614 "parser.c"
    break;

  case 1334:
#line 9689 "parser.y"
  {
	if (current_program->flag_main
	 && !current_program->flag_chained && yyvsp[-4]) {
		cb_error (_("executable program requested but PROCEDURE/ENTRY has USING clause"));
	}
	/* Main entry point */
	emit_entry (current_program->program_id, 0, yyvsp[-4], NULL);
	current_program->num_proc_params = cb_list_length (yyvsp[-4]);
	if (current_program->source_name) {
		emit_entry (current_program->source_name, 1, yyvsp[-4], NULL);
	}
  }
#line 18631 "parser.c"
    break;

  case 1335:
#line 9702 "parser.y"
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
#line 18650 "parser.c"
    break;

  case 1336:
#line 9717 "parser.y"
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
#line 18684 "parser.c"
    break;

  case 1338:
#line 9751 "parser.y"
  {
	yyval = NULL;
  }
#line 18692 "parser.c"
    break;

  case 1339:
#line 9755 "parser.y"
  {
	call_mode = CB_CALL_BY_REFERENCE;
	size_mode = CB_SIZE_4;
  }
#line 18701 "parser.c"
    break;

  case 1340:
#line 9760 "parser.y"
  {
	if (cb_list_length (yyvsp[0]) > MAX_CALL_FIELD_PARAMS) {
		cb_error (_("number of arguments exceeds maximum %d"),
			  MAX_CALL_FIELD_PARAMS);
	}
	yyval = yyvsp[0];
  }
#line 18713 "parser.c"
    break;

  case 1341:
#line 9768 "parser.y"
  {
	call_mode = CB_CALL_BY_REFERENCE;
	if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("CHAINING invalid in user FUNCTION"));
	} else {
		current_program->flag_chained = 1;
	}
  }
#line 18726 "parser.c"
    break;

  case 1342:
#line 9777 "parser.y"
  {
	if (cb_list_length (yyvsp[0]) > MAX_CALL_FIELD_PARAMS) {
		cb_error (_("number of arguments exceeds maximum %d"),
			  MAX_CALL_FIELD_PARAMS);
	}
	yyval = yyvsp[0];
  }
#line 18738 "parser.c"
    break;

  case 1343:
#line 9787 "parser.y"
                                { yyval = yyvsp[0]; }
#line 18744 "parser.c"
    break;

  case 1344:
#line 9789 "parser.y"
                                { yyval = cb_list_append (yyvsp[-1], yyvsp[0]); }
#line 18750 "parser.c"
    break;

  case 1345:
#line 9794 "parser.y"
  {
	cb_tree		x;
	struct cb_field	*f;

	x = cb_build_identifier (yyvsp[-1], 0);
	if (yyvsp[-2] == cb_int1 && CB_VALID_TREE (x) && cb_ref (x) != cb_error_node) {
		f = CB_FIELD (cb_ref (x));
		f->flag_is_pdiv_opt = 1;
	}

	if (call_mode == CB_CALL_BY_VALUE
	 && CB_REFERENCE_P (yyvsp[-1])
	 && CB_FIELD (cb_ref (yyvsp[-1]))->flag_any_length) {
		cb_error_x (yyvsp[-1], _("ANY LENGTH items may only be BY REFERENCE formal parameters"));
	}

	yyval = CB_BUILD_PAIR (cb_int (call_mode), x);
	CB_SIZES (yyval) = size_mode;
  }
#line 18774 "parser.c"
    break;

  case 1347:
#line 9818 "parser.y"
  {
	call_mode = CB_CALL_BY_REFERENCE;
  }
#line 18782 "parser.c"
    break;

  case 1348:
#line 9822 "parser.y"
  {
	if (current_program->flag_chained) {
		cb_error (_("%s not allowed in CHAINED programs"), "BY VALUE");
	} else {
		CB_UNFINISHED (_("parameters passed BY VALUE"));
		call_mode = CB_CALL_BY_VALUE;
	}
  }
#line 18795 "parser.c"
    break;

  case 1350:
#line 9835 "parser.y"
  {
	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else {
		size_mode = CB_SIZE_AUTO;
	}
  }
#line 18807 "parser.c"
    break;

  case 1351:
#line 9843 "parser.y"
  {
	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else {
		size_mode = CB_SIZE_4;
	}
  }
#line 18819 "parser.c"
    break;

  case 1352:
#line 9851 "parser.y"
  {
	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else {
		size_mode = CB_SIZE_AUTO | CB_SIZE_UNSIGNED;
	}
  }
#line 18831 "parser.c"
    break;

  case 1353:
#line 9859 "parser.y"
  {
	if (size_mode) {
		size_mode |= CB_SIZE_UNSIGNED;
	}
  }
#line 18841 "parser.c"
    break;

  case 1355:
#line 9869 "parser.y"
  {
	unsigned char *s = CB_LITERAL (yyvsp[0])->data;
	size_mode = 0;

	if (call_mode != CB_CALL_BY_VALUE) {
		cb_error (_("SIZE only allowed for BY VALUE items"));
	} else if (CB_LITERAL (yyvsp[0])->size != 1) {
		cb_error_x (yyvsp[0], _("invalid value for SIZE"));
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
			cb_error_x (yyvsp[0], _("invalid value for SIZE"));
			break;
		}
	}
  }
#line 18875 "parser.c"
    break;

  case 1357:
#line 9907 "parser.y"
  {
	CB_PENDING_X (yyvsp[-1], _("MEMORY SIZE phrase in CALL statement"));
  }
#line 18883 "parser.c"
    break;

  case 1358:
#line 9914 "parser.y"
  {
	yyval = cb_int0;
  }
#line 18891 "parser.c"
    break;

  case 1359:
#line 9918 "parser.y"
  {
	if (call_mode != CB_CALL_BY_REFERENCE) {
		cb_error (_("OPTIONAL only allowed for BY REFERENCE items"));
		yyval = cb_int0;
	} else {
		yyval = cb_int1;
	}
  }
#line 18904 "parser.c"
    break;

  case 1360:
#line 9930 "parser.y"
  {
	if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("RETURNING clause is required for a FUNCTION"));
	}
  }
#line 18914 "parser.c"
    break;

  case 1361:
#line 9936 "parser.y"
  {
	if (current_program->flag_main) {
		cb_error (_("RETURNING clause cannot be OMITTED for main program"));
	}
	if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("RETURNING clause cannot be OMITTED for a FUNCTION"));
	}
	current_program->flag_void = 1;
  }
#line 18928 "parser.c"
    break;

  case 1362:
#line 9946 "parser.y"
  {
	struct cb_field	*f;

	if (cb_ref (yyvsp[0]) != cb_error_node) {
		f = CB_FIELD_PTR (yyvsp[0]);
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
#if 0	/* doesn't work for programs, will be fixed with allocating in the source-unit */
			current_program->returning = yyvsp[0];
#else
			if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
				current_program->returning = yyvsp[0];
			} else {
				CB_PENDING ("program RETURNING");
			}
#endif
		}
	}
  }
#line 18965 "parser.c"
    break;

  case 1364:
#line 9982 "parser.y"
  {
	in_declaratives = 1;
	emit_statement (cb_build_comment ("DECLARATIVES"));
  }
#line 18974 "parser.c"
    break;

  case 1365:
#line 9988 "parser.y"
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
#line 19004 "parser.c"
    break;

  case 1370:
#line 10026 "parser.y"
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
#line 19026 "parser.c"
    break;

  case 1372:
#line 10045 "parser.y"
  {
	/* check_unreached = 0; */
	cb_end_statement();
  }
#line 19035 "parser.c"
    break;

  case 1373:
#line 10056 "parser.y"
  {
	non_const_word = 0;
	check_unreached = 0;
	if (cb_build_section_name (yyvsp[-1], 0) == cb_error_node) {
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
	current_section = CB_LABEL (cb_build_label (yyvsp[-1], NULL));
	current_section->flag_section = 1;
	/* Careful here, one negation */
	current_section->flag_real_label = !in_debugging;
	current_section->flag_declaratives = !!in_declaratives;
	current_section->flag_skip_label = !!skip_statements;
	current_paragraph = NULL;
  }
#line 19078 "parser.c"
    break;

  case 1374:
#line 10096 "parser.y"
  {
	emit_statement (CB_TREE (current_section));
  }
#line 19086 "parser.c"
    break;

  case 1377:
#line 10107 "parser.y"
  {
	cb_tree label;

	non_const_word = 0;
	check_unreached = 0;
	if (cb_build_section_name (yyvsp[-1], 1) == cb_error_node) {
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
	current_paragraph = CB_LABEL (cb_build_label (yyvsp[-1], current_section));
	current_paragraph->flag_declaratives = !!in_declaratives;
	current_paragraph->flag_skip_label = !!skip_statements;
	current_paragraph->flag_real_label = !in_debugging;
	current_paragraph->segment = current_section->segment;
	emit_statement (CB_TREE (current_paragraph));
  }
#line 19132 "parser.c"
    break;

  case 1378:
#line 10152 "parser.y"
  {
	non_const_word = 0;
	check_unreached = 0;
	if (cb_build_section_name (yyvsp[0], 0) != cb_error_node) {
		if (is_reserved_word (CB_NAME (yyvsp[0]))) {
			cb_error_x (yyvsp[0], _("'%s' is not a statement"), CB_NAME (yyvsp[0]));
		} else if (is_default_reserved_word (CB_NAME (yyvsp[0]))) {
			cb_error_x (yyvsp[0], _("unknown statement '%s'; it may exist in another dialect"),
				    CB_NAME (yyvsp[0]));
		} else {
			cb_error_x (yyvsp[0], _("unknown statement '%s'"), CB_NAME (yyvsp[0]));
		}
	}
	YYERROR;
  }
#line 19152 "parser.c"
    break;

  case 1379:
#line 10171 "parser.y"
  {
	yyval = NULL;
  }
#line 19160 "parser.c"
    break;

  case 1380:
#line 10175 "parser.y"
  {
	yyval = NULL;
	if (cb_verify (cb_section_segments, _("section segments"))) {
		int segnum = cb_get_int (yyvsp[0]);
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
#line 19186 "parser.c"
    break;

  case 1381:
#line 10203 "parser.y"
  {
	/* push exec_list on the stack ($1), then unset */
	yyval = current_program->exec_list;
	current_program->exec_list = NULL;
	check_unreached = 0;
  }
#line 19197 "parser.c"
    break;

  case 1382:
#line 10209 "parser.y"
  {
	/* push statement on the stack ($2), then unset */
	yyval = CB_TREE (current_statement);
	current_statement = NULL;
  }
#line 19207 "parser.c"
    break;

  case 1383:
#line 10215 "parser.y"
  {
	/* reorder exec_list which was filled in "statements" and push to stack ($$),
	   then backup exec_list and statement from the stack ($1, $2) */
	yyval = cb_list_reverse (current_program->exec_list);
	current_program->exec_list = yyvsp[-2];
	current_statement = CB_STATEMENT (yyvsp[-1]);
  }
#line 19219 "parser.c"
    break;

  case 1384:
#line 10225 "parser.y"
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

	cobc_apply_turn_directives ();
  }
#line 19259 "parser.c"
    break;

  case 1385:
#line 10261 "parser.y"
  {
	cobc_cs_check = 0;
	cobc_apply_turn_directives ();
  }
#line 19268 "parser.c"
    break;

  case 1386:
#line 10266 "parser.y"
  {
	cobc_cs_check = 0;
	cobc_apply_turn_directives ();
  }
#line 19277 "parser.c"
    break;

  case 1451:
#line 10341 "parser.y"
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
#line 19295 "parser.c"
    break;

  case 1452:
#line 10355 "parser.y"
  {
	yyerrok;
	cobc_cs_check = 0;
  }
#line 19304 "parser.c"
    break;

  case 1453:
#line 10366 "parser.y"
  {
	begin_statement ("ACCEPT", TERM_ACCEPT);
	cobc_cs_check = CB_CS_ACCEPT;
  }
#line 19313 "parser.c"
    break;

  case 1455:
#line 10376 "parser.y"
  {
	check_duplicate = 0;
	check_line_col_duplicate = 0;
	line_column = NULL;
  }
#line 19323 "parser.c"
    break;

  case 1456:
#line 10382 "parser.y"
  {
	/* Check for invalid use of screen clauses */
	if (current_statement->attr_ptr
	 || (!is_screen_field (yyvsp[-3]) && line_column)) {
		cb_verify_x (yyvsp[-3], cb_accept_display_extensions,
			     _("non-standard ACCEPT"));
	}

	if (cb_accept_update && !has_dispattr (COB_SCREEN_NO_UPDATE)) {
		set_dispattr (COB_SCREEN_UPDATE);
	}
	if (cb_accept_auto && !has_dispattr (COB_SCREEN_TAB)) {
		set_dispattr (COB_SCREEN_AUTO);
	}
	if (yyvsp[-3] == cb_null && current_statement->attr_ptr) {
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
	cb_emit_accept (yyvsp[-3], line_column, current_statement->attr_ptr);
  }
#line 19355 "parser.c"
    break;

  case 1457:
#line 10410 "parser.y"
  {
	check_duplicate = 0;
	check_line_col_duplicate = 0;
	line_column = NULL;
  }
#line 19365 "parser.c"
    break;

  case 1458:
#line 10416 "parser.y"
  {
	cobc_cs_check = 0;
	CB_PENDING ("ACCEPT FROM SCREEN");
  }
#line 19374 "parser.c"
    break;

  case 1459:
#line 10421 "parser.y"
  {
	cb_emit_accept_line_or_col (yyvsp[-2], 0);
  }
#line 19382 "parser.c"
    break;

  case 1460:
#line 10425 "parser.y"
  {
	cb_emit_accept_line_or_col (yyvsp[-2], 1);
  }
#line 19390 "parser.c"
    break;

  case 1461:
#line 10429 "parser.y"
  {
	/* information about terminal and its capabilities
	cb_emit_accept_terminal_info ($1); */
	CB_PENDING ("ACCEPT FROM TERMINAL INFO");
  }
#line 19400 "parser.c"
    break;

  case 1462:
#line 10435 "parser.y"
  {
	/* information about OS and runtime features
	cb_emit_accept_system_info ($1); */
	CB_PENDING ("ACCEPT FROM SYSTEM INFO");
  }
#line 19410 "parser.c"
    break;

  case 1463:
#line 10441 "parser.y"
  {
	cobc_cs_check = 0;
	cb_emit_accept_date_yyyymmdd (yyvsp[-3]);
  }
#line 19419 "parser.c"
    break;

  case 1464:
#line 10446 "parser.y"
  {
	cobc_cs_check = 0;
	cb_emit_accept_date (yyvsp[-2]);
  }
#line 19428 "parser.c"
    break;

  case 1465:
#line 10451 "parser.y"
  {
	cobc_cs_check = 0;
	cb_emit_accept_day_yyyyddd (yyvsp[-3]);
  }
#line 19437 "parser.c"
    break;

  case 1466:
#line 10456 "parser.y"
  {
	cobc_cs_check = 0;
	cb_emit_accept_day (yyvsp[-2]);
  }
#line 19446 "parser.c"
    break;

  case 1467:
#line 10461 "parser.y"
  {
	cb_emit_accept_day_of_week (yyvsp[-2]);
  }
#line 19454 "parser.c"
    break;

  case 1468:
#line 10467 "parser.y"
  {
	cb_emit_accept_escape_key (yyvsp[-3]);
  }
#line 19462 "parser.c"
    break;

  case 1469:
#line 10473 "parser.y"
  {
	cb_emit_accept_exception_status (yyvsp[-3]);
  }
#line 19470 "parser.c"
    break;

  case 1470:
#line 10477 "parser.y"
  {
	/* check is data from keyboard available? "1", else "0"
	cb_emit_accept_input_status ($1); */
	CB_PENDING ("ACCEPT FROM INPUT STATUS");
  }
#line 19480 "parser.c"
    break;

  case 1471:
#line 10483 "parser.y"
  {
	cb_emit_accept_time (yyvsp[-2]);
  }
#line 19488 "parser.c"
    break;

  case 1472:
#line 10487 "parser.y"
  {
	cobc_cs_check = 0;
	cb_emit_accept_user_name (yyvsp[-3]);
  }
#line 19497 "parser.c"
    break;

  case 1473:
#line 10492 "parser.y"
  {
	cb_emit_accept_command_line (yyvsp[-2]);
  }
#line 19505 "parser.c"
    break;

  case 1474:
#line 10496 "parser.y"
  {
	cb_emit_accept_environment (yyvsp[-3]);
  }
#line 19513 "parser.c"
    break;

  case 1475:
#line 10500 "parser.y"
  {
	cb_emit_get_environment (yyvsp[-1], yyvsp[-4]);
  }
#line 19521 "parser.c"
    break;

  case 1476:
#line 10504 "parser.y"
  {
	cb_emit_accept_arg_number (yyvsp[-2]);
  }
#line 19529 "parser.c"
    break;

  case 1477:
#line 10508 "parser.y"
  {
	cb_emit_accept_arg_value (yyvsp[-3]);
  }
#line 19537 "parser.c"
    break;

  case 1478:
#line 10512 "parser.y"
  {
	cb_emit_accept_mnemonic (yyvsp[-2], yyvsp[0]);
  }
#line 19545 "parser.c"
    break;

  case 1479:
#line 10516 "parser.y"
  {
	cb_emit_accept_name (yyvsp[-2], yyvsp[0]);
  }
#line 19553 "parser.c"
    break;

  case 1480:
#line 10520 "parser.y"
  {
	cb_verify_x (yyvsp[-1], cb_accept_display_extensions,
		     _("non-standard ACCEPT"));

	if (cb_accept_update && !has_dispattr (COB_SCREEN_NO_UPDATE)) {
		set_dispattr (COB_SCREEN_UPDATE);
	}
	if (cb_accept_auto && !has_dispattr (COB_SCREEN_TAB)) {
		set_dispattr (COB_SCREEN_AUTO);
	}
	cobc_cs_check = 0;
	cb_emit_accept (yyvsp[-1], line_column, current_statement->attr_ptr);
  }
#line 19571 "parser.c"
    break;

  case 1481:
#line 10534 "parser.y"
  {
	CB_PENDING ("ACCEPT MESSAGE COUNT");
  }
#line 19579 "parser.c"
    break;

  case 1483:
#line 10542 "parser.y"
  {
	yyval = cb_null;
  }
#line 19587 "parser.c"
    break;

  case 1484:
#line 10548 "parser.y"
  {
	check_duplicate = 0;
	check_line_col_duplicate = 0;
	line_column = NULL;
  }
#line 19597 "parser.c"
    break;

  case 1485:
#line 10554 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 19605 "parser.c"
    break;

  case 1488:
#line 10565 "parser.y"
  {
	line_column = CB_BUILD_PAIR (yyvsp[-3], yyvsp[-1]);
  }
#line 19613 "parser.c"
    break;

  case 1489:
#line 10569 "parser.y"
  {
	line_column = CB_BUILD_PAIR (yyvsp[-2], cb_int0);
  }
#line 19621 "parser.c"
    break;

  case 1490:
#line 10573 "parser.y"
  {
	line_column = CB_BUILD_PAIR (cb_int0, yyvsp[-1]);
  }
#line 19629 "parser.c"
    break;

  case 1491:
#line 10580 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 19637 "parser.c"
    break;

  case 1492:
#line 10584 "parser.y"
  {
	yyval = cb_build_binary_op (yyvsp[-2], '+', yyvsp[0]);
  }
#line 19645 "parser.c"
    break;

  case 1493:
#line 10588 "parser.y"
  {
	yyval = cb_build_binary_op (yyvsp[-2], '-', yyvsp[0]);
  }
#line 19653 "parser.c"
    break;

  case 1501:
#line 10612 "parser.y"
  {
	  check_repeated ("FROM CRT", SYN_CLAUSE_2, &check_duplicate);
  }
#line 19661 "parser.c"
    break;

  case 1502:
#line 10616 "parser.y"
  {
	  check_repeated ("MODE IS BLOCK", SYN_CLAUSE_3, &check_duplicate);
  }
#line 19669 "parser.c"
    break;

  case 1504:
#line 10621 "parser.y"
  {
	check_repeated (_("TIME-OUT or BEFORE TIME clauses"), SYN_CLAUSE_4,
			&check_duplicate);
	set_attribs (NULL, NULL, NULL, yyvsp[0], NULL, NULL, 0);
  }
#line 19679 "parser.c"
    break;

  case 1511:
#line 10646 "parser.y"
  {
	set_attr_with_conflict ("LINE", SYN_CLAUSE_1,
				_("AT screen-location"), SYN_CLAUSE_3, 1,
				&check_line_col_duplicate);

	if ((CB_LITERAL_P (yyvsp[0]) && cb_get_int (yyvsp[0]) == 0) || yyvsp[0] == cb_zero) {
		cb_verify (cb_accept_display_extensions, "LINE 0");
	}

	if (!line_column) {
		line_column = CB_BUILD_PAIR (yyvsp[0], cb_int0);
	} else {
		CB_PAIR_X (line_column) = yyvsp[0];
	}
  }
#line 19699 "parser.c"
    break;

  case 1512:
#line 10662 "parser.y"
  {
	set_attr_with_conflict ("COLUMN", SYN_CLAUSE_2,
				_("AT screen-location"), SYN_CLAUSE_3, 1,
				&check_line_col_duplicate);

	if ((CB_LITERAL_P (yyvsp[0]) && cb_get_int (yyvsp[0]) == 0) || yyvsp[0] == cb_zero) {
		cb_verify (cb_accept_display_extensions, "COLUMN 0");
	}

	if (!line_column) {
		line_column = CB_BUILD_PAIR (cb_int0, yyvsp[0]);
	} else {
		CB_PAIR_Y (line_column) = yyvsp[0];
	}
  }
#line 19719 "parser.c"
    break;

  case 1513:
#line 10678 "parser.y"
  {
	set_attr_with_conflict (_("AT screen-location"), SYN_CLAUSE_3,
				_("LINE or COLUMN"), SYN_CLAUSE_1 | SYN_CLAUSE_2,
				1, &check_line_col_duplicate);

	cb_verify (cb_accept_display_extensions, "AT clause");

	line_column = yyvsp[0];
  }
#line 19733 "parser.c"
    break;

  case 1514:
#line 10691 "parser.y"
  {
	/* FIXME: arithmetic expression should be possible, too, only numeric literals! */
	yyval = yyvsp[0];
  }
#line 19742 "parser.c"
    break;

  case 1515:
#line 10699 "parser.y"
  {
	/* FIXME: arithmetic expression should be possible, too, only numeric literals! */
	yyval = yyvsp[0];
  }
#line 19751 "parser.c"
    break;

  case 1516:
#line 10707 "parser.y"
  {
	cobc_cs_check = 0;
  }
#line 19759 "parser.c"
    break;

  case 1517:
#line 10714 "parser.y"
  {
	check_repeated ("AUTO", SYN_CLAUSE_5, &check_duplicate);
	set_dispattr_with_conflict ("AUTO", COB_SCREEN_AUTO,
				    "TAB", COB_SCREEN_TAB);
  }
#line 19769 "parser.c"
    break;

  case 1518:
#line 10720 "parser.y"
  {
	check_repeated ("TAB", SYN_CLAUSE_6, &check_duplicate);
	set_dispattr_with_conflict ("TAB", COB_SCREEN_TAB,
				    "AUTO", COB_SCREEN_AUTO);
  }
#line 19779 "parser.c"
    break;

  case 1519:
#line 10726 "parser.y"
  {
	check_repeated ("BELL", SYN_CLAUSE_7, &check_duplicate);
	set_dispattr (COB_SCREEN_BELL);
  }
#line 19788 "parser.c"
    break;

  case 1520:
#line 10731 "parser.y"
  {
	check_repeated ("BELL", SYN_CLAUSE_7, &check_duplicate);
	/* FIXME: do we need a COB_NO_SCREEN_BELL here?
	set_dispattr (COB_SCREEN_BELL); */
  }
#line 19798 "parser.c"
    break;

  case 1521:
#line 10737 "parser.y"
  {
	check_repeated ("BLINK", SYN_CLAUSE_8, &check_duplicate);
	set_dispattr (COB_SCREEN_BLINK);
  }
#line 19807 "parser.c"
    break;

  case 1522:
#line 10742 "parser.y"
  {
	check_repeated ("CONVERSION", SYN_CLAUSE_9, &check_duplicate);
	CB_PENDING ("ACCEPT CONVERSION");
  }
#line 19816 "parser.c"
    break;

  case 1523:
#line 10747 "parser.y"
  {
	/* FIXME: arithmetic expression should be possible, too! */
	if (current_program->cursor_pos) {
		emit_duplicate_clause_message ("CURSOR");
	} else {
		/* TODO: actually reasonable and easy extension: an 
		         *offset within the field* [auto-correct to 1/max]
				 (when variable also stored back on return)
		*/
		CB_PENDING ("ACCEPT ... WITH CURSOR");
	}
  }
#line 19833 "parser.c"
    break;

  case 1524:
#line 10760 "parser.y"
  {
	check_repeated ("FULL", SYN_CLAUSE_10, &check_duplicate);
	set_dispattr (COB_SCREEN_FULL);
  }
#line 19842 "parser.c"
    break;

  case 1525:
#line 10765 "parser.y"
  {
	check_repeated ("LEFTLINE", SYN_CLAUSE_12, &check_duplicate);
	set_dispattr (COB_SCREEN_LEFTLINE);
  }
#line 19851 "parser.c"
    break;

  case 1526:
#line 10770 "parser.y"
  {
	check_repeated ("LOWER", SYN_CLAUSE_13, &check_duplicate);
	set_dispattr_with_conflict ("LOWER", COB_SCREEN_LOWER,
				    "UPPER", COB_SCREEN_UPPER);
  }
#line 19861 "parser.c"
    break;

  case 1527:
#line 10776 "parser.y"
  {
	check_repeated ("HIGHLIGHT", SYN_CLAUSE_11, &check_duplicate);
	set_dispattr_with_conflict ("HIGHLIGHT", COB_SCREEN_HIGHLIGHT,
				    "LOWLIGHT", COB_SCREEN_LOWLIGHT);
  }
#line 19871 "parser.c"
    break;

  case 1528:
#line 10782 "parser.y"
  {
	check_repeated ("LOWLIGHT", SYN_CLAUSE_14, &check_duplicate);
	set_dispattr_with_conflict ("LOWLIGHT", COB_SCREEN_LOWLIGHT,
				    "HIGHLIGHT", COB_SCREEN_HIGHLIGHT);
  }
#line 19881 "parser.c"
    break;

  case 1529:
#line 10789 "parser.y"
  {
	CB_PENDING ("SAME phrase");
	/* may not be specified along with the UNDERLINED, BLINK, REVERSED,
	HIGH, LOW, STANDARD, COLOR, FOREGROUND-COLOR, or BACKGROUND-COLOR phrases */
  }
#line 19891 "parser.c"
    break;

  case 1530:
#line 10795 "parser.y"
  {
	CB_PENDING ("STANDARD intensity");
  }
#line 19899 "parser.c"
    break;

  case 1531:
#line 10799 "parser.y"
  {
	CB_PENDING ("BACKGROUND intensity");
  }
#line 19907 "parser.c"
    break;

  case 1532:
#line 10803 "parser.y"
  {
	CB_PENDING ("BACKGROUND intensity");
  }
#line 19915 "parser.c"
    break;

  case 1533:
#line 10807 "parser.y"
  {
	CB_PENDING ("BACKGROUND intensity");
  }
#line 19923 "parser.c"
    break;

  case 1534:
#line 10811 "parser.y"
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
#line 19938 "parser.c"
    break;

  case 1535:
#line 10822 "parser.y"
  {
	check_repeated ("OVERLINE", SYN_CLAUSE_16, &check_duplicate);
	set_dispattr (COB_SCREEN_OVERLINE);
  }
#line 19947 "parser.c"
    break;

  case 1536:
#line 10827 "parser.y"
  {
	/* Note: CHARACTER optional in ACUCOBOL, required by others */
	check_repeated ("PROMPT", SYN_CLAUSE_17, &check_duplicate);
	set_attribs (NULL, NULL, NULL, NULL, yyvsp[0], NULL, COB_SCREEN_PROMPT);
  }
#line 19957 "parser.c"
    break;

  case 1537:
#line 10833 "parser.y"
  {
	check_repeated ("PROMPT", SYN_CLAUSE_17, &check_duplicate);
	set_dispattr (COB_SCREEN_PROMPT);
  }
#line 19966 "parser.c"
    break;

  case 1538:
#line 10838 "parser.y"
  {
	check_repeated ("REQUIRED", SYN_CLAUSE_18, &check_duplicate);
	set_dispattr (COB_SCREEN_REQUIRED);
  }
#line 19975 "parser.c"
    break;

  case 1539:
#line 10843 "parser.y"
  {
	check_repeated ("REVERSE-VIDEO", SYN_CLAUSE_19, &check_duplicate);
	set_dispattr (COB_SCREEN_REVERSE);
  }
#line 19984 "parser.c"
    break;

  case 1540:
#line 10848 "parser.y"
  {
	check_repeated ("SECURE", SYN_CLAUSE_20, &check_duplicate);
	set_dispattr_with_conflict ("SECURE", COB_SCREEN_SECURE,
				    "NO-ECHO", COB_SCREEN_NO_ECHO);
  }
#line 19994 "parser.c"
    break;

  case 1541:
#line 10854 "parser.y"
  {
	/* FIXME: arithmetic expression should be possible, too! */
	check_repeated ("SIZE", SYN_CLAUSE_21, &check_duplicate);
	set_attribs (NULL, NULL, NULL, NULL, NULL, yyvsp[0], 0);
  }
#line 20004 "parser.c"
    break;

  case 1542:
#line 10860 "parser.y"
  {
	check_repeated ("UNDERLINE", SYN_CLAUSE_22, &check_duplicate);
	set_dispattr (COB_SCREEN_UNDERLINE);
  }
#line 20013 "parser.c"
    break;

  case 1543:
#line 10865 "parser.y"
  {
	check_repeated ("NO UPDATE", SYN_CLAUSE_23, &check_duplicate);
	set_dispattr_with_conflict ("NO UPDATE", COB_SCREEN_NO_UPDATE,
				    "UPDATE", COB_SCREEN_UPDATE);
  }
#line 20023 "parser.c"
    break;

  case 1544:
#line 10871 "parser.y"
  {
	check_repeated ("UPDATE", SYN_CLAUSE_24, &check_duplicate);
	set_dispattr_with_conflict ("UPDATE", COB_SCREEN_UPDATE,
				    "NO UPDATE", COB_SCREEN_NO_UPDATE);
  }
#line 20033 "parser.c"
    break;

  case 1545:
#line 10877 "parser.y"
  {
	check_repeated ("UPPER", SYN_CLAUSE_25, &check_duplicate);
	set_dispattr_with_conflict ("UPPER", COB_SCREEN_UPPER,
				    "LOWER", COB_SCREEN_LOWER);
  }
#line 20043 "parser.c"
    break;

  case 1546:
#line 10883 "parser.y"
  {
	/* FIXME: arithmetic expression should be possible, too! */
	check_repeated ("FOREGROUND-COLOR", SYN_CLAUSE_26, &check_duplicate);
	check_repeated ("BACKGROUND-COLOR", SYN_CLAUSE_27, &check_duplicate);
	CB_PENDING ("COLOR");
  }
#line 20054 "parser.c"
    break;

  case 1547:
#line 10890 "parser.y"
  {
	check_repeated ("FOREGROUND-COLOR", SYN_CLAUSE_26, &check_duplicate);
	set_attribs (yyvsp[0], NULL, NULL, NULL, NULL, NULL, 0);
  }
#line 20063 "parser.c"
    break;

  case 1548:
#line 10895 "parser.y"
  {
	check_repeated ("BACKGROUND-COLOR", SYN_CLAUSE_27, &check_duplicate);
	set_attribs (NULL, yyvsp[0], NULL, NULL, NULL, NULL, 0);
  }
#line 20072 "parser.c"
    break;

  case 1549:
#line 10900 "parser.y"
  {
	check_repeated ("SCROLL UP", SYN_CLAUSE_28, &check_duplicate);
	set_attribs_with_conflict (NULL, NULL, yyvsp[0], NULL, NULL, NULL,
				   "SCROLL UP", COB_SCREEN_SCROLL_UP,
				   "SCROLL DOWN", COB_SCREEN_SCROLL_DOWN);
  }
#line 20083 "parser.c"
    break;

  case 1550:
#line 10907 "parser.y"
  {
	check_repeated ("SCROLL DOWN", SYN_CLAUSE_19, &check_duplicate);
	set_attribs_with_conflict (NULL, NULL, yyvsp[0], NULL, NULL, NULL,
				   "SCROLL DOWN", COB_SCREEN_SCROLL_DOWN,
				   "SCROLL UP", COB_SCREEN_SCROLL_UP);
  }
#line 20094 "parser.c"
    break;

  case 1551:
#line 10914 "parser.y"
  {
	check_repeated (_("TIME-OUT or BEFORE TIME clauses"), SYN_CLAUSE_4,
			&check_duplicate);
	set_attribs (NULL, NULL, NULL, yyvsp[0], NULL, NULL, 0);
  }
#line 20104 "parser.c"
    break;

  case 1555:
#line 10927 "parser.y"
  {
	check_repeated ("CONTROL KEY", SYN_CLAUSE_29, &check_duplicate);
	CB_PENDING ("CONTROL KEY");
#if 0 /* should generate the following *after* the ACCEPT is finished */
	cb_emit_accept_escape_key (yyvsp[0]);
#endif
  }
#line 20116 "parser.c"
    break;

  case 1564:
#line 10955 "parser.y"
  {
	TERMINATOR_WARNING (yyvsp[(-2) - (0)], ACCEPT);
  }
#line 20124 "parser.c"
    break;

  case 1565:
#line 10959 "parser.y"
  {
	TERMINATOR_CLEAR (yyvsp[(-2) - (1)], ACCEPT);
# if 0 /* activate only for debugging purposes for attribs
	FIXME: Replace by DEBUG_LOG function */
	if (current_statement->attr_ptr) {
		print_bits (current_statement->attr_ptr->dispattrs);
	} else {
		fputs("No Attribs", stderr);
	}
#endif
  }
#line 20140 "parser.c"
    break;

  case 1566:
#line 10977 "parser.y"
  {
	begin_statement ("ADD", TERM_ADD);
  }
#line 20148 "parser.c"
    break;

  case 1568:
#line 10986 "parser.y"
  {
	cb_emit_arithmetic (yyvsp[-1], '+', cb_build_binary_list (yyvsp[-3], '+'));
  }
#line 20156 "parser.c"
    break;

  case 1569:
#line 10990 "parser.y"
  {
	if (yyvsp[-3]) {
		cb_list_add (yyvsp[-4], yyvsp[-3]);
	}
	cb_emit_arithmetic (yyvsp[-1], 0, cb_build_binary_list (yyvsp[-4], '+'));
  }
#line 20167 "parser.c"
    break;

  case 1570:
#line 10997 "parser.y"
  {
	cb_emit_corresponding (cb_build_add, yyvsp[-2], yyvsp[-4], yyvsp[-1]);
  }
#line 20175 "parser.c"
    break;

  case 1571:
#line 11001 "parser.y"
  {
	CB_PENDING ("ADD TABLE");
	cb_emit_tab_arithmetic (cb_build_add, yyvsp[-4], yyvsp[-6], yyvsp[-3], yyvsp[-2], yyvsp[-1]);
  }
#line 20184 "parser.c"
    break;

  case 1572:
#line 11008 "parser.y"
              { yyval = NULL; }
#line 20190 "parser.c"
    break;

  case 1573:
#line 11009 "parser.y"
              { yyval = yyvsp[0]; }
#line 20196 "parser.c"
    break;

  case 1574:
#line 11014 "parser.y"
  {
	TERMINATOR_WARNING (yyvsp[(-2) - (0)], ADD);
  }
#line 20204 "parser.c"
    break;

  case 1575:
#line 11018 "parser.y"
  {
	TERMINATOR_CLEAR (yyvsp[(-2) - (1)], ADD);
  }
#line 20212 "parser.c"
    break;

  case 1576:
#line 11028 "parser.y"
  {
	begin_statement ("ALLOCATE", 0);
	cobc_cs_check = CB_CS_ALLOCATE;
	current_statement->flag_no_based = 1;
  }
#line 20222 "parser.c"
    break;

  case 1578:
#line 11038 "parser.y"
  {
	cb_emit_allocate (yyvsp[-3], yyvsp[0], NULL, yyvsp[-2]);
  }
#line 20230 "parser.c"
    break;

  case 1579:
#line 11042 "parser.y"
  {
	if (yyvsp[0] == NULL) {
		cb_error_x (CB_TREE (current_statement),
			    _("ALLOCATE CHARACTERS requires RETURNING clause"));
	} else {
		cb_emit_allocate (NULL, yyvsp[0], yyvsp[-4], yyvsp[-2]);
	}
  }
#line 20243 "parser.c"
    break;

  case 1581:
#line 11055 "parser.y"
  {
	int adressing = cb_get_int (yyvsp[0]);

	if (adressing == 24
	 || adressing == 31) {
		cb_warning (COBC_WARN_FILLER, _("ignoring %s phrase"), "LOC");
	} else {
		cb_error (_("addressing mode should be either 24 or 31 bit"));
	}
  }
#line 20258 "parser.c"
    break;

  case 1582:
#line 11067 "parser.y"
                                { yyval = NULL; }
#line 20264 "parser.c"
    break;

  case 1583:
#line 11068 "parser.y"
                                { yyval = yyvsp[0]; }
#line 20270 "parser.c"
    break;

  case 1584:
#line 11076 "parser.y"
  {
	begin_statement ("ALTER", 0);
	cb_verify (cb_alter_statement, "ALTER");
  }
#line 20279 "parser.c"
    break;

  case 1588:
#line 11090 "parser.y"
  {
	cb_emit_alter (yyvsp[-3], yyvsp[0]);
  }
#line 20287 "parser.c"
    break;

  case 1591:
#line 11102 "parser.y"
  {
	begin_statement ("CALL", TERM_CALL);
	cobc_cs_check = CB_CS_CALL;
	call_nothing = 0;
	cobc_allow_program_name = 1;
	backup_current_pos ();
  }
#line 20299 "parser.c"
    break;

  case 1592:
#line 11111 "parser.y"
  {
	cobc_cs_check = 0;
  }
#line 20307 "parser.c"
    break;

  case 1593:
#line 11118 "parser.y"
  {
	cobc_allow_program_name = 0;
  }
#line 20315 "parser.c"
    break;

  case 1594:
#line 11126 "parser.y"
  {
	int call_conv = 0;
	int call_conv_local = 0;

	if (current_program->prog_type == COB_MODULE_TYPE_PROGRAM
	 && !current_program->flag_recursive
	 && is_recursive_call (yyvsp[-6])) {
		cb_warning_x (COBC_WARN_FILLER, yyvsp[-6],
			_("recursive program call - assuming RECURSIVE attribute"));
		current_program->flag_recursive = 1;
	}
	call_conv = current_call_convention;
	if (yyvsp[-3]) {
		if (current_call_convention & CB_CONV_STATIC_LINK) {
			call_conv = CB_INTEGER (yyvsp[-3])->val | CB_CONV_STATIC_LINK;
		} else {
			call_conv = CB_INTEGER (yyvsp[-3])->val;
		}
		if (yyvsp[-8]) {
			/* note: $1 is likely to be a reference to SPECIAL-NAMES */
			cb_error_x (yyvsp[-3], _("%s and %s are mutually exclusive"),
				"CALL-CONVENTION", "WITH LINKAGE");
		}
	}
	if ((CB_PAIR_X (yyvsp[0]) != NULL)
	 && (call_conv & CB_CONV_STATIC_LINK)) {
		cb_warning_x (COBC_WARN_FILLER, yyvsp[-6],
		    _("STATIC CALL convention ignored because of ON EXCEPTION"));
		call_conv &= ~CB_CONV_STATIC_LINK;
	}
	if (yyvsp[-8]) {
		if (CB_INTEGER_P (yyvsp[-8])) {
			call_conv_local = CB_INTEGER (yyvsp[-8])->val;
			if ((CB_PAIR_X (yyvsp[0]) != NULL)
			 && (call_conv_local & CB_CONV_STATIC_LINK)) {
				cb_warning_x (COBC_WARN_FILLER, yyvsp[-8],
					_("ON EXCEPTION ignored because of STATIC CALL"));
				CB_PAIR_X (yyvsp[0]) = NULL;
			}
			call_conv |= call_conv_local;
			if (CB_INTEGER (yyvsp[-8])->val & CB_CONV_COBOL) {
				call_conv &= ~CB_CONV_STDCALL;
			} else {
				call_conv &= ~CB_CONV_COBOL;
			}
		} else {
			call_conv = cb_get_int(yyvsp[-8]);
		}
	}
	/* For CALL ... RETURNING NOTHING, set the call convention bit */
	if (call_nothing) {
		call_conv |= CB_CONV_NO_RET_UPD;
	}
	cb_emit_call (yyvsp[-6], yyvsp[-2], yyvsp[-1], CB_PAIR_X (yyvsp[0]), CB_PAIR_Y (yyvsp[0]),
		      cb_int (call_conv), yyvsp[-7], yyvsp[-4], backup_source_line);
  }
#line 20376 "parser.c"
    break;

  case 1595:
#line 11186 "parser.y"
  {
	yyval = NULL;
  }
#line 20384 "parser.c"
    break;

  case 1596:
#line 11190 "parser.y"
  {
	/* FIXME: hack - fake cs for context-sensitive WITH ... LINKAGE */
	cobc_cs_check |= CB_CS_OPTIONS;
	backup_current_pos ();
  }
#line 20394 "parser.c"
    break;

  case 1597:
#line 11196 "parser.y"
  {
	yyval = yyvsp[-1];
	restore_backup_pos (yyval);
	cobc_cs_check ^= CB_CS_OPTIONS;
	cb_verify_x (yyval, cb_call_convention_linkage, "WITH ... LINKAGE");
  }
#line 20405 "parser.c"
    break;

  case 1598:
#line 11206 "parser.y"
  {
	yyval = cb_int (CB_CONV_STDCALL);
  }
#line 20413 "parser.c"
    break;

  case 1599:
#line 11210 "parser.y"
  {
	yyval = cb_int (CB_CONV_C);
  }
#line 20421 "parser.c"
    break;

  case 1600:
#line 11214 "parser.y"
  {
	yyval = cb_int (CB_CONV_PASCAL);
  }
#line 20429 "parser.c"
    break;

  case 1601:
#line 11221 "parser.y"
  {
	yyval = NULL;
  }
#line 20437 "parser.c"
    break;

  case 1602:
#line 11225 "parser.y"
  {
	cb_verify (cb_call_convention_mnemonic, "CALL-/ENTRY-CONVENTION");
	yyval = yyvsp[0];
  }
#line 20446 "parser.c"
    break;

  case 1603:
#line 11233 "parser.y"
  {
	if (current_call_convention & CB_CONV_COBOL) {
		yyval = cb_int (CB_CONV_STATIC_LINK | CB_CONV_COBOL);
	} else {
		yyval = cb_int (CB_CONV_STATIC_LINK);
	}
  }
#line 20458 "parser.c"
    break;

  case 1604:
#line 11241 "parser.y"
  {
	yyval = cb_int (CB_CONV_STDCALL);
  }
#line 20466 "parser.c"
    break;

  case 1605:
#line 11245 "parser.y"
  {
	yyval = cb_int (CB_CONV_C);
  }
#line 20474 "parser.c"
    break;

  case 1606:
#line 11249 "parser.y"
  {
	yyval = cb_int (CB_CONV_C);
  }
#line 20482 "parser.c"
    break;

  case 1607:
#line 11253 "parser.y"
  {
	yyval = cb_int (CB_CONV_PASCAL);
  }
#line 20490 "parser.c"
    break;

  case 1608:
#line 11257 "parser.y"
  {
	cb_tree		x;

	x = cb_ref (yyvsp[0]);
	if (CB_VALID_TREE (x)) {
		if (CB_SYSTEM_NAME(x)->token != CB_FEATURE_CONVENTION) {
			cb_error_x (yyvsp[0], _("invalid mnemonic name"));
			yyval = NULL;
		} else {
			yyval = CB_SYSTEM_NAME(x)->value;
		}
	} else {
		yyval = NULL;
	}
  }
#line 20510 "parser.c"
    break;

  case 1609:
#line 11276 "parser.y"
  {
	if (CB_LITERAL_P (yyvsp[0])) {
		cb_trim_program_id (yyvsp[0]);
	}
  }
#line 20520 "parser.c"
    break;

  case 1610:
#line 11282 "parser.y"
  {
	cb_verify (cb_program_prototypes, _("CALL/CANCEL with program-prototype-name"));
	/* hack to push the prototype name */
	if (yyvsp[0] && CB_REFERENCE_P (yyvsp[0])) {
		if (yyvsp[-1]) {
			cb_warning_x (COBC_WARN_FILLER, yyvsp[-1], _("id/literal ignored, using prototype name"));
		}
		yyval = yyvsp[0];
	} else if (yyvsp[-1] && CB_LITERAL_P (yyvsp[-1])) {
		yyval = yyvsp[-1];
	} else {
		cb_error (_("NESTED phrase is only valid with literal"));
		yyval = cb_error_node;
	}
  }
#line 20540 "parser.c"
    break;

  case 1611:
#line 11301 "parser.y"
  {
	yyval = NULL;
  }
#line 20548 "parser.c"
    break;

  case 1612:
#line 11306 "parser.y"
  {
	if (CB_LITERAL_P (yyvsp[-1])) {
		cb_trim_program_id (yyvsp[-1]);
	}
	yyval = yyvsp[-1];
  }
#line 20559 "parser.c"
    break;

  case 1613:
#line 11316 "parser.y"
  {
	CB_PENDING ("NESTED phrase for CALL statement");
  }
#line 20567 "parser.c"
    break;

  case 1615:
#line 11324 "parser.y"
  {
	yyval = NULL;
  }
#line 20575 "parser.c"
    break;

  case 1616:
#line 11328 "parser.y"
  {
	call_mode = CB_CALL_BY_REFERENCE;
	size_mode = CB_SIZE_4;
  }
#line 20584 "parser.c"
    break;

  case 1617:
#line 11333 "parser.y"
  {
	if (cb_list_length (yyvsp[0]) > MAX_CALL_FIELD_PARAMS) {
		cb_error_x (CB_TREE (current_statement),
			    _("number of arguments exceeds maximum %d"),
			    MAX_CALL_FIELD_PARAMS);
	}
	yyval = yyvsp[0];
  }
#line 20597 "parser.c"
    break;

  case 1618:
#line 11344 "parser.y"
                                { yyval = yyvsp[0]; }
#line 20603 "parser.c"
    break;

  case 1619:
#line 11346 "parser.y"
                                { yyval = cb_list_append (yyvsp[-1], yyvsp[0]); }
#line 20609 "parser.c"
    break;

  case 1620:
#line 11351 "parser.y"
  {
	if (call_mode != CB_CALL_BY_REFERENCE) {
		cb_error_x (CB_TREE (current_statement),
			    _("OMITTED only allowed when arguments are passed BY REFERENCE"));
	}
	yyval = CB_BUILD_PAIR (cb_int (call_mode), cb_null);
  }
#line 20621 "parser.c"
    break;

  case 1621:
#line 11359 "parser.y"
  {
	int	save_mode;	/* internal single parameter only mode */

	save_mode = call_mode;
	if (call_mode != CB_CALL_BY_REFERENCE) {
		if (CB_FILE_P (yyvsp[0]) || (CB_REFERENCE_P (yyvsp[0]) &&
		    CB_FILE_P (CB_REFERENCE (yyvsp[0])->value))) {
			cb_error_x (CB_TREE (current_statement),
				    _("invalid file name reference"));
		} else if (call_mode == CB_CALL_BY_VALUE) {
			/* FIXME: compiler configuration needed, IBM allows one-byte
			          alphanumeric items [--> a `char`], too, while
			          COBOL 2002/2014 allow only numeric literals
			   --> revise after rw-merge */
			if (cb_category_is_alpha (yyvsp[0])) {
				cb_warning_x (COBC_WARN_FILLER, yyvsp[0],
					      _("BY CONTENT assumed for alphanumeric item '%s'"),
						  cb_name (yyvsp[0]));
				call_mode = CB_CALL_BY_CONTENT;
			} else if (cb_category_is_national (yyvsp[0])) {
				cb_warning_x (COBC_WARN_FILLER, yyvsp[0],
					      _("BY CONTENT assumed for national item '%s'"),
						  cb_name (yyvsp[0]));
				call_mode = CB_CALL_BY_CONTENT;
			}
		}
	}
	yyval = CB_BUILD_PAIR (cb_int (call_mode), yyvsp[0]);
	CB_SIZES (yyval) = size_mode;
	call_mode = save_mode;
  }
#line 20657 "parser.c"
    break;

  case 1623:
#line 11395 "parser.y"
  {
	call_mode = CB_CALL_BY_REFERENCE;
  }
#line 20665 "parser.c"
    break;

  case 1624:
#line 11399 "parser.y"
  {
	if (current_program->flag_chained) {
		cb_error_x (CB_TREE (current_statement),
			    _("%s not allowed in CHAINED programs"), "BY CONTENT");
	} else {
		call_mode = CB_CALL_BY_CONTENT;
	}
  }
#line 20678 "parser.c"
    break;

  case 1625:
#line 11408 "parser.y"
  {
	if (current_program->flag_chained) {
		cb_error_x (CB_TREE (current_statement),
			    _("%s not allowed in CHAINED programs"), "BY VALUE");
	} else {
		call_mode = CB_CALL_BY_VALUE;
	}
  }
#line 20691 "parser.c"
    break;

  case 1626:
#line 11420 "parser.y"
  {
	yyval = NULL;
  }
#line 20699 "parser.c"
    break;

  case 1627:
#line 11424 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 20707 "parser.c"
    break;

  case 1628:
#line 11428 "parser.y"
  {
	yyval = cb_null;
  }
#line 20715 "parser.c"
    break;

  case 1629:
#line 11432 "parser.y"
  {
	call_nothing = CB_CONV_NO_RET_UPD;
	yyval = cb_null;
  }
#line 20724 "parser.c"
    break;

  case 1630:
#line 11437 "parser.y"
  {
	struct cb_field	*f;

	if (cb_ref (yyvsp[0]) != cb_error_node) {
		f = CB_FIELD_PTR (yyvsp[0]);
		if (f->level != 1 && f->level != 77) {
			cb_error (_("RETURNING item must have level 01 or 77"));
			yyval = NULL;
		} else if (f->storage != CB_STORAGE_LINKAGE &&
			   !f->flag_item_based) {
			cb_error (_("RETURNING item must be a LINKAGE SECTION item or have BASED clause"));
			yyval = NULL;
		} else {
			yyval = cb_build_address (yyvsp[0]);
		}
	} else {
		yyval = NULL;
	}
  }
#line 20748 "parser.c"
    break;

  case 1635:
#line 11470 "parser.y"
  {
	yyval = CB_BUILD_PAIR (NULL, NULL);
  }
#line 20756 "parser.c"
    break;

  case 1636:
#line 11474 "parser.y"
  {
	yyval = CB_BUILD_PAIR (yyvsp[-1], yyvsp[0]);
  }
#line 20764 "parser.c"
    break;

  case 1637:
#line 11478 "parser.y"
  {
	if (yyvsp[0]) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT EXCEPTION before EXCEPTION"));
	}
	yyval = CB_BUILD_PAIR (yyvsp[0], yyvsp[-1]);
  }
#line 20776 "parser.c"
    break;

  case 1638:
#line 11489 "parser.y"
  {
	yyval = NULL;
  }
#line 20784 "parser.c"
    break;

  case 1639:
#line 11493 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 20792 "parser.c"
    break;

  case 1640:
#line 11500 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 20800 "parser.c"
    break;

  case 1641:
#line 11504 "parser.y"
  {
	cb_verify (cb_call_overflow, "ON OVERFLOW");
	yyval = yyvsp[0];
  }
#line 20809 "parser.c"
    break;

  case 1642:
#line 11512 "parser.y"
  {
	yyval = NULL;
  }
#line 20817 "parser.c"
    break;

  case 1643:
#line 11516 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 20825 "parser.c"
    break;

  case 1644:
#line 11523 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 20833 "parser.c"
    break;

  case 1645:
#line 11530 "parser.y"
  {
	TERMINATOR_WARNING (yyvsp[(-2) - (0)], CALL);
  }
#line 20841 "parser.c"
    break;

  case 1646:
#line 11534 "parser.y"
  {
	TERMINATOR_CLEAR (yyvsp[(-2) - (1)], CALL);
  }
#line 20849 "parser.c"
    break;

  case 1647:
#line 11544 "parser.y"
  {
	begin_statement ("CANCEL", 0);
	cobc_allow_program_name = 1;
  }
#line 20858 "parser.c"
    break;

  case 1648:
#line 11549 "parser.y"
  {
	cobc_allow_program_name = 0;
  }
#line 20866 "parser.c"
    break;

  case 1649:
#line 11556 "parser.y"
  {
	cb_emit_cancel (yyvsp[0]);
  }
#line 20874 "parser.c"
    break;

  case 1650:
#line 11560 "parser.y"
  {
	cb_emit_cancel (yyvsp[0]);
  }
#line 20882 "parser.c"
    break;

  case 1652:
#line 11568 "parser.y"
  {
	cb_verify (cb_program_prototypes, _("CALL/CANCEL with program-prototype-name"));
  }
#line 20890 "parser.c"
    break;

  case 1653:
#line 11577 "parser.y"
  {
	begin_statement ("CLOSE", 0);
  }
#line 20898 "parser.c"
    break;

  case 1657:
#line 11590 "parser.y"
  {
#if 0 /* CHECKME: likely not needed */
	begin_implicit_statement ();
#endif
	cb_emit_close (yyvsp[-1], yyvsp[0]);
  }
#line 20909 "parser.c"
    break;

  case 1658:
#line 11597 "parser.y"
  {
	begin_implicit_statement ();
	cb_emit_close (yyvsp[-1], yyvsp[0]);
  }
#line 20918 "parser.c"
    break;

  case 1659:
#line 11604 "parser.y"
                                { yyval = cb_int (COB_CLOSE_NORMAL); }
#line 20924 "parser.c"
    break;

  case 1660:
#line 11605 "parser.y"
                                { yyval = cb_int (COB_CLOSE_UNIT); }
#line 20930 "parser.c"
    break;

  case 1661:
#line 11606 "parser.y"
                                { yyval = cb_int (COB_CLOSE_UNIT_REMOVAL); }
#line 20936 "parser.c"
    break;

  case 1662:
#line 11607 "parser.y"
                                { yyval = cb_int (COB_CLOSE_NO_REWIND); }
#line 20942 "parser.c"
    break;

  case 1663:
#line 11608 "parser.y"
                                { yyval = cb_int (COB_CLOSE_LOCK); }
#line 20948 "parser.c"
    break;

  case 1664:
#line 11613 "parser.y"
  {
	CB_PENDING ("GRAPHICAL WINDOW");
	current_statement->name = "CLOSE WINDOW";
  }
#line 20957 "parser.c"
    break;

  case 1665:
#line 11618 "parser.y"
  {
	cb_emit_close_window (yyvsp[-1], yyvsp[0]);
  }
#line 20965 "parser.c"
    break;

  case 1666:
#line 11624 "parser.y"
                                { yyval = NULL; }
#line 20971 "parser.c"
    break;

  case 1667:
#line 11625 "parser.y"
                                { yyval = cb_int0; }
#line 20977 "parser.c"
    break;

  case 1668:
#line 11633 "parser.y"
  {
	begin_statement ("COMPUTE", TERM_COMPUTE);
  }
#line 20985 "parser.c"
    break;

  case 1670:
#line 11642 "parser.y"
  {
	cb_emit_arithmetic (yyvsp[-3], 0, yyvsp[-1]);
  }
#line 20993 "parser.c"
    break;

  case 1671:
#line 11649 "parser.y"
  {
	TERMINATOR_WARNING (yyvsp[(-2) - (0)], COMPUTE);
  }
#line 21001 "parser.c"
    break;

  case 1672:
#line 11653 "parser.y"
  {
	TERMINATOR_CLEAR (yyvsp[(-2) - (1)], COMPUTE);
  }
#line 21009 "parser.c"
    break;

  case 1673:
#line 11663 "parser.y"
  {
	begin_statement ("COMMIT", 0);
	cb_emit_commit ();
  }
#line 21018 "parser.c"
    break;

  case 1674:
#line 11674 "parser.y"
  {
	backup_current_pos ();
  }
#line 21026 "parser.c"
    break;

  case 1675:
#line 11678 "parser.y"
  {
	if (!yyvsp[0]) {
		/* Do not check unreached for CONTINUE without after phrase */
		unsigned int	save_unreached = check_unreached;
		check_unreached = 0;
		begin_statement_from_backup_pos ("CONTINUE", 0);
		cb_emit_continue (NULL);
		check_unreached = save_unreached;
	} else {
		begin_statement_from_backup_pos ("CONTINUE AFTER", 0);
		cb_emit_continue (yyvsp[0]);
	}
  }
#line 21044 "parser.c"
    break;

  case 1676:
#line 11694 "parser.y"
                { yyval = NULL;}
#line 21050 "parser.c"
    break;

  case 1677:
#line 11695 "parser.y"
        {
	/* FIXME: hack - fake cs for context-sensitive SECONDS */
	cobc_cs_check = CB_CS_RETRY;
  }
#line 21059 "parser.c"
    break;

  case 1678:
#line 11700 "parser.y"
  {
	yyval = yyvsp[-1];
  }
#line 21067 "parser.c"
    break;

  case 1679:
#line 11710 "parser.y"
  {
	begin_statement ("DESTROY", 0);
	CB_PENDING ("GRAPHICAL CONTROL");
  }
#line 21076 "parser.c"
    break;

  case 1681:
#line 11719 "parser.y"
  {
	cb_emit_destroy (NULL);
  }
#line 21084 "parser.c"
    break;

  case 1682:
#line 11726 "parser.y"
  {
	cb_emit_destroy (yyvsp[0]);
  }
#line 21092 "parser.c"
    break;

  case 1683:
#line 11736 "parser.y"
  {
	begin_statement ("DELETE", TERM_DELETE);
  }
#line 21100 "parser.c"
    break;

  case 1685:
#line 11745 "parser.y"
  {
	cb_emit_delete (yyvsp[-3]);
  }
#line 21108 "parser.c"
    break;

  case 1687:
#line 11753 "parser.y"
  {
#if 0 /* CHECKME: likely not needed */
	begin_implicit_statement ();
#endif
	cb_emit_delete_file (yyvsp[0]);
  }
#line 21119 "parser.c"
    break;

  case 1688:
#line 11760 "parser.y"
  {
	begin_implicit_statement ();
	cb_emit_delete_file (yyvsp[0]);
  }
#line 21128 "parser.c"
    break;

  case 1689:
#line 11768 "parser.y"
  {
	TERMINATOR_WARNING (yyvsp[(-2) - (0)], DELETE);
  }
#line 21136 "parser.c"
    break;

  case 1690:
#line 11772 "parser.y"
  {
	TERMINATOR_CLEAR (yyvsp[(-2) - (1)], DELETE);
  }
#line 21144 "parser.c"
    break;

  case 1691:
#line 11782 "parser.y"
  {
	begin_statement ("DISABLE", 0);
  }
#line 21152 "parser.c"
    break;

  case 1695:
#line 11796 "parser.y"
  {
	/* Add cb_verify for <= COBOL-85 */
  }
#line 21160 "parser.c"
    break;

  case 1701:
#line 11814 "parser.y"
  {
	begin_statement ("DISPLAY", TERM_DISPLAY);
	cobc_cs_check = CB_CS_DISPLAY;
	display_type = UNKNOWN_DISPLAY;
	is_first_display_item = 1;
  }
#line 21171 "parser.c"
    break;

  case 1703:
#line 11826 "parser.y"
  {
	cb_emit_env_name (yyvsp[-2]);
  }
#line 21179 "parser.c"
    break;

  case 1704:
#line 11830 "parser.y"
  {
	cb_emit_env_value (yyvsp[-2]);
  }
#line 21187 "parser.c"
    break;

  case 1705:
#line 11834 "parser.y"
  {
	cb_emit_arg_number (yyvsp[-2]);
  }
#line 21195 "parser.c"
    break;

  case 1706:
#line 11838 "parser.y"
  {
	cb_emit_command_line (yyvsp[-2]);
  }
#line 21203 "parser.c"
    break;

  case 1714:
#line 11852 "parser.y"
  {
	if (yyvsp[0] != NULL) {
		error_if_different_display_type (yyvsp[0], NULL, NULL, NULL);
		cb_emit_display (yyvsp[0], NULL, cb_int1, NULL, NULL, 0,
				 display_type);
	}
  }
#line 21215 "parser.c"
    break;

  case 1715:
#line 11860 "parser.y"
  {
	set_display_type (yyvsp[0], NULL, NULL, NULL);
	cb_emit_display (yyvsp[0], NULL, cb_int1, NULL, NULL, 1,
			 display_type);
  }
#line 21225 "parser.c"
    break;

  case 1718:
#line 11874 "parser.y"
  {
	check_duplicate = 0;
	check_line_col_duplicate = 0;
	advancing_value = cb_int1;
	upon_value = NULL;
	line_column = NULL;
  }
#line 21237 "parser.c"
    break;

  case 1719:
#line 11882 "parser.y"
  {
	if (yyvsp[-2] == cb_null) {
		/* Emit DISPLAY OMITTED. */
		CB_UNFINISHED_X (CB_TREE(current_statement), "DISPLAY OMITTED");
		error_if_no_advancing_in_screen_display (advancing_value);
	}

	/* Emit device or screen DISPLAY. */

	/*
	  Check that disp_list does not contain an invalid mix of fields.
	*/
	if (display_type == UNKNOWN_DISPLAY) {
		set_display_type (yyvsp[-2], upon_value, line_column,
				  current_statement->attr_ptr);
	} else {
		error_if_different_display_type (yyvsp[-2], upon_value,
						 line_column,
						 current_statement->attr_ptr);
	}

	if (display_type == SCREEN_DISPLAY
	 || display_type == FIELD_ON_SCREEN_DISPLAY) {
		error_if_no_advancing_in_screen_display (advancing_value);
	}

	cb_emit_display (yyvsp[-2], upon_value, advancing_value, line_column,
			 current_statement->attr_ptr,
			 is_first_display_item, display_type);

	is_first_display_item = 0;
  }
#line 21274 "parser.c"
    break;

  case 1720:
#line 11918 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 21282 "parser.c"
    break;

  case 1721:
#line 11922 "parser.y"
  {
	yyval = cb_null;
  }
#line 21290 "parser.c"
    break;

  case 1728:
#line 11944 "parser.y"
  {
	check_repeated ("UPON", SYN_CLAUSE_1, &check_duplicate);
  }
#line 21298 "parser.c"
    break;

  case 1729:
#line 11948 "parser.y"
  {
	check_repeated ("NO ADVANCING", SYN_CLAUSE_2, &check_duplicate);
	advancing_value = cb_int0;
  }
#line 21307 "parser.c"
    break;

  case 1730:
#line 11953 "parser.y"
  {
	check_repeated ("MODE IS BLOCK", SYN_CLAUSE_3, &check_duplicate);
  }
#line 21315 "parser.c"
    break;

  case 1733:
#line 11962 "parser.y"
  {
	  upon_value = NULL;
  }
#line 21323 "parser.c"
    break;

  case 1735:
#line 11970 "parser.y"
  {
	upon_value = cb_build_display_mnemonic (yyvsp[0]);
  }
#line 21331 "parser.c"
    break;

  case 1736:
#line 11974 "parser.y"
  {
	upon_value = cb_build_display_name (yyvsp[0]);
  }
#line 21339 "parser.c"
    break;

  case 1737:
#line 11978 "parser.y"
  {
	upon_value = cb_int2;
  }
#line 21347 "parser.c"
    break;

  case 1738:
#line 11982 "parser.y"
  {
	upon_value = cb_null;
  }
#line 21355 "parser.c"
    break;

  case 1741:
#line 11994 "parser.y"
  {
	check_duplicate = SYN_CLAUSE_10;
	check_line_col_duplicate = 0;
	line_column = NULL;
	set_dispattr_with_conflict ("ERASE EOS", COB_SCREEN_ERASE_EOS,
				    "ERASE EOL", COB_SCREEN_ERASE_EOL);
  }
#line 21367 "parser.c"
    break;

  case 1742:
#line 12002 "parser.y"
  {
	cb_emit_display (CB_LIST_INIT (cb_space), cb_null, cb_int1, line_column, NULL, 1, FIELD_ON_SCREEN_DISPLAY);
  }
#line 21375 "parser.c"
    break;

  case 1743:
#line 12011 "parser.y"
  {
	cb_emit_display (yyvsp[-1], cb_null, cb_int1, line_column, NULL, 1, FIELD_ON_SCREEN_DISPLAY);
  }
#line 21383 "parser.c"
    break;

  case 1744:
#line 12017 "parser.y"
  {
	check_duplicate = 0;
	check_line_col_duplicate = 0;
	line_column = NULL;
  }
#line 21393 "parser.c"
    break;

  case 1745:
#line 12023 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 21401 "parser.c"
    break;

  case 1746:
#line 12030 "parser.y"
  {
	yyval = CB_LIST_INIT (yyvsp[0]);
  }
#line 21409 "parser.c"
    break;

  case 1747:
#line 12034 "parser.y"
  {
	yyval = cb_list_add (yyvsp[-1], yyvsp[0]);
  }
#line 21417 "parser.c"
    break;

  case 1750:
#line 12044 "parser.y"
  {
	set_dispattr_with_conflict ("ERASE EOS", COB_SCREEN_ERASE_EOS,
				    "ERASE EOL", COB_SCREEN_ERASE_EOL);
	yyval = cb_space;
  }
#line 21427 "parser.c"
    break;

  case 1751:
#line 12054 "parser.y"
  {
	CB_UNFINISHED_X (CB_TREE(current_statement), "DISPLAY MESSAGE");
	upon_value = NULL;
  }
#line 21436 "parser.c"
    break;

  case 1752:
#line 12059 "parser.y"
  {
	/* for now: minimal support for display and prompt only */
	if (upon_value) {
		cb_emit_display (CB_LIST_INIT (upon_value), NULL, NULL, NULL,
				 NULL, 1, FIELD_ON_SCREEN_DISPLAY);
	}
	cb_emit_display (yyvsp[-2], NULL, NULL, NULL,
			 NULL, 1, FIELD_ON_SCREEN_DISPLAY);
	cb_emit_accept (cb_null, NULL, NULL);
  }
#line 21451 "parser.c"
    break;

  case 1757:
#line 12083 "parser.y"
  {
	upon_value = yyvsp[0];
  }
#line 21459 "parser.c"
    break;

  case 1762:
#line 12094 "parser.y"
  {
	CB_PENDING ("GRAPHICAL WINDOW");
	current_statement->name = "DISPLAY WINDOW";
  }
#line 21468 "parser.c"
    break;

  case 1763:
#line 12099 "parser.y"
  {
	check_duplicate = 0;
	check_line_col_duplicate = 0;
	line_column = NULL;
	upon_value = NULL; /* Hack: stores the POP-UP AREA */
  }
#line 21479 "parser.c"
    break;

  case 1764:
#line 12106 "parser.y"
  {
	cb_emit_display_window (NULL, upon_value, yyvsp[-2], line_column,
			 current_statement->attr_ptr);
  }
#line 21488 "parser.c"
    break;

  case 1767:
#line 12119 "parser.y"
  {
	CB_PENDING ("GRAPHICAL WINDOW");
	current_statement->name = "DISPLAY FLOATING WINDOW";
  }
#line 21497 "parser.c"
    break;

  case 1768:
#line 12124 "parser.y"
  {
	check_duplicate = 0;
	check_line_col_duplicate = 0;
	line_column = NULL;
	upon_value = NULL; /* Hack: stores the POP-UP AREA */
  }
#line 21508 "parser.c"
    break;

  case 1769:
#line 12131 "parser.y"
  {
	if (yyvsp[-5]) {
		/* TODO: set "CELL WIDTH" and "CELL HEIGHT" to "LABEL FONT" */
		/* if not set already */
	}
	cb_emit_display_window (cb_int0, upon_value, yyvsp[-2], line_column,
			 current_statement->attr_ptr);
  }
#line 21521 "parser.c"
    break;

  case 1770:
#line 12143 "parser.y"
  {
	CB_PENDING ("GRAPHICAL WINDOW");
	current_statement->name = "DISPLAY INITIAL WINDOW";
	check_duplicate = 0;
	check_line_col_duplicate = 0;
	line_column = NULL;
	upon_value = NULL; /* Hack: stores the POP-UP AREA */
	/* TODO: initialize attributes for SHADOW, BOTTOM */
  }
#line 21535 "parser.c"
    break;

  case 1771:
#line 12153 "parser.y"
  {
	if (yyvsp[-3]) {
		/* TODO: set "CELL WIDTH" and "CELL HEIGHT" to "LABEL FONT" */
		/* if not set already */
	}
	cb_emit_display_window (yyvsp[-4], upon_value, NULL, line_column,
			 current_statement->attr_ptr);
  }
#line 21548 "parser.c"
    break;

  case 1772:
#line 12164 "parser.y"
                {yyval = cb_int1;}
#line 21554 "parser.c"
    break;

  case 1773:
#line 12165 "parser.y"
                {yyval = cb_int2;}
#line 21560 "parser.c"
    break;

  case 1774:
#line 12166 "parser.y"
                {yyval = cb_int3;}
#line 21566 "parser.c"
    break;

  case 1775:
#line 12170 "parser.y"
                {yyval = NULL;}
#line 21572 "parser.c"
    break;

  case 1776:
#line 12171 "parser.y"
                {yyval = cb_int1;}
#line 21578 "parser.c"
    break;

  case 1777:
#line 12176 "parser.y"
  {
	yyval = NULL;
  }
#line 21586 "parser.c"
    break;

  case 1778:
#line 12180 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 21594 "parser.c"
    break;

  case 1779:
#line 12187 "parser.y"
  {
	struct cb_field	*f;

	if (cb_ref (yyvsp[0]) != cb_error_node) {
		f = CB_FIELD_PTR (yyvsp[0]);
		if (f->usage != CB_USAGE_HNDL_WINDOW
		 && f->usage != CB_USAGE_HNDL_SUBWINDOW) {
			cb_error_x (yyvsp[0], _("HANDLE must be a %s HANDLE"), "WINDOW");
		}
	}
	yyval = yyvsp[0];
  }
#line 21611 "parser.c"
    break;

  case 1780:
#line 12200 "parser.y"
  {
	struct cb_field	*f;

	if (cb_ref (yyvsp[0]) != cb_error_node) {
		f = CB_FIELD_PTR (yyvsp[0]);
		if (f->usage != CB_USAGE_HNDL) {
			cb_error_x (yyvsp[0], _("HANDLE must be a generic HANDLE"));
		}
	}
	yyval = yyvsp[0];
  }
#line 21627 "parser.c"
    break;

  case 1781:
#line 12212 "parser.y"
  {
	yyval = cb_null;
  }
#line 21635 "parser.c"
    break;

  case 1785:
#line 12227 "parser.y"
  {
	/* TODO: store */
  }
#line 21643 "parser.c"
    break;

  case 1792:
#line 12239 "parser.y"
                        { /* TODO: set attribute */ }
#line 21649 "parser.c"
    break;

  case 1793:
#line 12242 "parser.y"
                        { /* TODO: set attribute */ }
#line 21655 "parser.c"
    break;

  case 1794:
#line 12246 "parser.y"
                { yyval = cb_int0; }
#line 21661 "parser.c"
    break;

  case 1795:
#line 12247 "parser.y"
                        { yyval = cb_int0; }
#line 21667 "parser.c"
    break;

  case 1796:
#line 12248 "parser.y"
                        { yyval = cb_int1; }
#line 21673 "parser.c"
    break;

  case 1797:
#line 12252 "parser.y"
                        { yyval = cb_int0; }
#line 21679 "parser.c"
    break;

  case 1798:
#line 12253 "parser.y"
                        { yyval = cb_int1; }
#line 21685 "parser.c"
    break;

  case 1799:
#line 12254 "parser.y"
                { yyval = cb_int1; }
#line 21691 "parser.c"
    break;

  case 1800:
#line 12255 "parser.y"
                        { yyval = cb_int2; }
#line 21697 "parser.c"
    break;

  case 1805:
#line 12271 "parser.y"
  {
	if (upon_value) {
		emit_duplicate_clause_message("POP-UP AREA");
	}
	upon_value = yyvsp[0];
  }
#line 21708 "parser.c"
    break;

  case 1806:
#line 12281 "parser.y"
  {
	if (!strcmp (current_statement->name, "DISPLAY WINDOW")) {
		cb_error_x (yyvsp[0], _("HANDLE clause invalid for %s"),
			current_statement->name);
		upon_value = cb_error_node;
	} else{
		if (upon_value) {
			emit_duplicate_clause_message("POP-UP AREA / HANDLE IN");
		}
		upon_value = yyvsp[0];
	}
  }
#line 21725 "parser.c"
    break;

  case 1807:
#line 12297 "parser.y"
  {
	check_repeated ("BELL", SYN_CLAUSE_4, &check_duplicate);
	set_dispattr (COB_SCREEN_BELL);
  }
#line 21734 "parser.c"
    break;

  case 1808:
#line 12302 "parser.y"
  {
	check_repeated ("BLANK LINE", SYN_CLAUSE_5, &check_duplicate);
	set_dispattr_with_conflict ("BLANK LINE", COB_SCREEN_BLANK_LINE,
				    "BLANK SCREEN", COB_SCREEN_BLANK_SCREEN);
  }
#line 21744 "parser.c"
    break;

  case 1809:
#line 12308 "parser.y"
  {
	check_repeated ("BLANK SCREEN", SYN_CLAUSE_6, &check_duplicate);
	set_dispattr_with_conflict ("BLANK SCREEN", COB_SCREEN_BLANK_SCREEN,
				    "BLANK LINE", COB_SCREEN_BLANK_LINE);
  }
#line 21754 "parser.c"
    break;

  case 1810:
#line 12314 "parser.y"
  {
	check_repeated ("BLINK", SYN_CLAUSE_7, &check_duplicate);
	set_dispattr (COB_SCREEN_BLINK);
  }
#line 21763 "parser.c"
    break;

  case 1811:
#line 12319 "parser.y"
  {
	check_repeated ("CONVERSION", SYN_CLAUSE_8, &check_duplicate);
	cb_warning (COBC_WARN_FILLER, _("ignoring %s phrase"), "CONVERSION");
  }
#line 21772 "parser.c"
    break;

  case 1812:
#line 12324 "parser.y"
  {
	check_repeated ("ERASE EOL", SYN_CLAUSE_9, &check_duplicate);
	set_dispattr_with_conflict ("ERASE EOL", COB_SCREEN_ERASE_EOL,
				    "ERASE EOS", COB_SCREEN_ERASE_EOS);
  }
#line 21782 "parser.c"
    break;

  case 1813:
#line 12330 "parser.y"
  {
	check_repeated ("ERASE EOS", SYN_CLAUSE_10, &check_duplicate);
	set_dispattr_with_conflict ("ERASE EOS", COB_SCREEN_ERASE_EOS,
				    "ERASE EOL", COB_SCREEN_ERASE_EOL);
  }
#line 21792 "parser.c"
    break;

  case 1814:
#line 12336 "parser.y"
  {
	check_repeated ("HIGHLIGHT", SYN_CLAUSE_11, &check_duplicate);
	set_dispattr_with_conflict ("HIGHLIGHT", COB_SCREEN_HIGHLIGHT,
				    "LOWLIGHT", COB_SCREEN_LOWLIGHT);
  }
#line 21802 "parser.c"
    break;

  case 1815:
#line 12342 "parser.y"
  {
	check_repeated ("LOWLIGHT", SYN_CLAUSE_12, &check_duplicate);
	set_dispattr_with_conflict ("LOWLIGHT", COB_SCREEN_LOWLIGHT,
				    "HIGHLIGHT", COB_SCREEN_HIGHLIGHT);
  }
#line 21812 "parser.c"
    break;

  case 1816:
#line 12349 "parser.y"
  {
	CB_PENDING ("SAME phrase");
	/* may not be specified along with the UNDERLINED, BLINK, REVERSED,
	HIGH, LOW, STANDARD, COLOR, FOREGROUND-COLOR, or BACKGROUND-COLOR phrases */
  }
#line 21822 "parser.c"
    break;

  case 1817:
#line 12355 "parser.y"
  {
	CB_PENDING ("STANDARD intensity");
  }
#line 21830 "parser.c"
    break;

  case 1818:
#line 12359 "parser.y"
  {
	CB_PENDING ("BACKGROUND intensity");
  }
#line 21838 "parser.c"
    break;

  case 1819:
#line 12363 "parser.y"
  {
	CB_PENDING ("BACKGROUND intensity");
  }
#line 21846 "parser.c"
    break;

  case 1820:
#line 12367 "parser.y"
  {
	CB_PENDING ("BACKGROUND intensity");
  }
#line 21854 "parser.c"
    break;

  case 1821:
#line 12371 "parser.y"
  {
	check_repeated ("OVERLINE", SYN_CLAUSE_13, &check_duplicate);
	set_dispattr (COB_SCREEN_OVERLINE);
  }
#line 21863 "parser.c"
    break;

  case 1822:
#line 12376 "parser.y"
  {
	check_repeated ("REVERSE-VIDEO", SYN_CLAUSE_14, &check_duplicate);
	set_dispattr (COB_SCREEN_REVERSE);
  }
#line 21872 "parser.c"
    break;

  case 1823:
#line 12381 "parser.y"
  {
	check_repeated ("SIZE", SYN_CLAUSE_15, &check_duplicate);
	set_attribs (NULL, NULL, NULL, NULL, NULL, yyvsp[0], 0);
  }
#line 21881 "parser.c"
    break;

  case 1824:
#line 12386 "parser.y"
  {
	check_repeated ("UNDERLINE", SYN_CLAUSE_16, &check_duplicate);
	set_dispattr (COB_SCREEN_UNDERLINE);
  }
#line 21890 "parser.c"
    break;

  case 1825:
#line 12391 "parser.y"
  {
	check_repeated ("FOREGROUND-COLOR", SYN_CLAUSE_17, &check_duplicate);
	check_repeated ("BACKGROUND-COLOR", SYN_CLAUSE_18, &check_duplicate);
	CB_PENDING ("COLOR");
  }
#line 21900 "parser.c"
    break;

  case 1826:
#line 12397 "parser.y"
  {
	check_repeated ("FOREGROUND-COLOR", SYN_CLAUSE_17, &check_duplicate);
	set_attribs (yyvsp[0], NULL, NULL, NULL, NULL, NULL, 0);
  }
#line 21909 "parser.c"
    break;

  case 1827:
#line 12402 "parser.y"
  {
	check_repeated ("BACKGROUND-COLOR", SYN_CLAUSE_18, &check_duplicate);
	set_attribs (NULL, yyvsp[0], NULL, NULL, NULL, NULL, 0);
  }
#line 21918 "parser.c"
    break;

  case 1828:
#line 12407 "parser.y"
  {
	check_repeated ("SCROLL UP", SYN_CLAUSE_19, &check_duplicate);
	set_attribs_with_conflict (NULL, NULL, yyvsp[0], NULL, NULL, NULL,
				   "SCROLL UP", COB_SCREEN_SCROLL_UP,
				   "SCROLL DOWN", COB_SCREEN_SCROLL_DOWN);
  }
#line 21929 "parser.c"
    break;

  case 1829:
#line 12414 "parser.y"
  {
	check_repeated ("SCROLL DOWN", SYN_CLAUSE_20, &check_duplicate);
	set_attribs_with_conflict (NULL, NULL, yyvsp[0], NULL, NULL, NULL,
				   "SCROLL DOWN", COB_SCREEN_SCROLL_DOWN,
				   "SCROLL UP", COB_SCREEN_SCROLL_UP);
  }
#line 21940 "parser.c"
    break;

  case 1830:
#line 12424 "parser.y"
  {
	TERMINATOR_WARNING (yyvsp[(-2) - (0)], DISPLAY);
  }
#line 21948 "parser.c"
    break;

  case 1831:
#line 12428 "parser.y"
  {
	TERMINATOR_CLEAR (yyvsp[(-2) - (1)], DISPLAY);
  }
#line 21956 "parser.c"
    break;

  case 1832:
#line 12438 "parser.y"
  {
	begin_statement ("DIVIDE", TERM_DIVIDE);
  }
#line 21964 "parser.c"
    break;

  case 1834:
#line 12447 "parser.y"
  {
	cb_emit_arithmetic (yyvsp[-1], '/', yyvsp[-3]);
  }
#line 21972 "parser.c"
    break;

  case 1835:
#line 12451 "parser.y"
  {
	cb_emit_arithmetic (yyvsp[-1], 0, cb_build_binary_op (yyvsp[-3], '/', yyvsp[-5]));
  }
#line 21980 "parser.c"
    break;

  case 1836:
#line 12455 "parser.y"
  {
	cb_emit_arithmetic (yyvsp[-1], 0, cb_build_binary_op (yyvsp[-5], '/', yyvsp[-3]));
  }
#line 21988 "parser.c"
    break;

  case 1837:
#line 12459 "parser.y"
  {
	cb_emit_divide (yyvsp[-5], yyvsp[-7], yyvsp[-3], yyvsp[-1]);
  }
#line 21996 "parser.c"
    break;

  case 1838:
#line 12463 "parser.y"
  {
	cb_emit_divide (yyvsp[-7], yyvsp[-5], yyvsp[-3], yyvsp[-1]);
  }
#line 22004 "parser.c"
    break;

  case 1839:
#line 12470 "parser.y"
  {
	TERMINATOR_WARNING (yyvsp[(-2) - (0)], DIVIDE);
  }
#line 22012 "parser.c"
    break;

  case 1840:
#line 12474 "parser.y"
  {
	TERMINATOR_CLEAR (yyvsp[(-2) - (1)], DIVIDE);
  }
#line 22020 "parser.c"
    break;

  case 1841:
#line 12484 "parser.y"
  {
	begin_statement ("ENABLE", 0);
  }
#line 22028 "parser.c"
    break;

  case 1843:
#line 12495 "parser.y"
  {
	check_unreached = 0;
	begin_statement ("ENTRY", 0);
	backup_current_pos ();
  }
#line 22038 "parser.c"
    break;

  case 1845:
#line 12502 "parser.y"
  {
	check_unreached = 0;
	begin_statement ("ENTRY FOR GO TO", 0);
	backup_current_pos ();
  }
#line 22048 "parser.c"
    break;

  case 1847:
#line 12512 "parser.y"
  {
	if (current_program->nested_level) {
		cb_error (_("%s is invalid in nested program"), "ENTRY");
	} else if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("%s is invalid in a user FUNCTION"), "ENTRY");
	} else if (cb_verify (cb_entry_statement, "ENTRY")) {
		cb_tree call_conv = yyvsp[-3];
		if (yyvsp[-1]) {
			call_conv = yyvsp[-1];
			if (yyvsp[-3]) {
				/* note: $1 is likely to be a reference to SPECIAL-NAMES */
				cb_error_x (yyvsp[-1], _("%s and %s are mutually exclusive"),
					"CALL-CONVENTION", "WITH LINKAGE");
			}
		}
		if (!cobc_check_valid_name ((char *)(CB_LITERAL (yyvsp[-2])->data), ENTRY_NAME)) {
			emit_entry ((char *)(CB_LITERAL (yyvsp[-2])->data), 1, yyvsp[0], call_conv);
		}
	}
  }
#line 22073 "parser.c"
    break;

  case 1848:
#line 12536 "parser.y"
  {
	if (cb_verify (cb_goto_entry, "ENTRY FOR GO TO")) {
		emit_entry_goto ((char *)(CB_LITERAL (yyvsp[0])->data));
	}
  }
#line 22083 "parser.c"
    break;

  case 1849:
#line 12548 "parser.y"
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
#line 22109 "parser.c"
    break;

  case 1851:
#line 12575 "parser.y"
  {
	if (!skip_statements) {
		cb_emit_evaluate (yyvsp[-1], yyvsp[0]);
	}
	eval_level--;
  }
#line 22120 "parser.c"
    break;

  case 1852:
#line 12584 "parser.y"
                                { yyval = CB_LIST_INIT (yyvsp[0]); }
#line 22126 "parser.c"
    break;

  case 1853:
#line 12586 "parser.y"
                                { yyval = cb_list_add (yyvsp[-2], yyvsp[0]); }
#line 22132 "parser.c"
    break;

  case 1854:
#line 12591 "parser.y"
  {
	yyval = yyvsp[0];
	eval_check[eval_level][eval_inc++] = yyvsp[0];
	if (eval_inc >= EVAL_DEPTH) {
		cb_error (_("maximum evaluate depth exceeded (%d)"),
			  EVAL_DEPTH);
		eval_inc = 0;
		YYERROR;
	}
  }
#line 22147 "parser.c"
    break;

  case 1855:
#line 12602 "parser.y"
  {
	yyval = cb_true;
	eval_check[eval_level][eval_inc++] = NULL;
	if (eval_inc >= EVAL_DEPTH) {
		cb_error (_("maximum evaluate depth exceeded (%d)"),
			  EVAL_DEPTH);
		eval_inc = 0;
		YYERROR;
	}
  }
#line 22162 "parser.c"
    break;

  case 1856:
#line 12613 "parser.y"
  {
	yyval = cb_false;
	eval_check[eval_level][eval_inc++] = cb_false;
	if (eval_inc >= EVAL_DEPTH) {
		cb_error (_("maximum evaluate depth exceeded (%d)"),
			  EVAL_DEPTH);
		eval_inc = 0;
		YYERROR;
	}
  }
#line 22177 "parser.c"
    break;

  case 1857:
#line 12627 "parser.y"
  {
	if (yyvsp[0]) {
		yyval = cb_list_add (yyvsp[-1], yyvsp[0]);
	} else {
		yyval = yyvsp[-1];
	}
  }
#line 22189 "parser.c"
    break;

  case 1858:
#line 12636 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 22197 "parser.c"
    break;

  case 1859:
#line 12642 "parser.y"
                                { yyval = CB_LIST_INIT (yyvsp[0]); }
#line 22203 "parser.c"
    break;

  case 1860:
#line 12644 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 22209 "parser.c"
    break;

  case 1861:
#line 12650 "parser.y"
  {
	yyval = CB_BUILD_CHAIN (yyvsp[0], yyvsp[-1]);
	eval_inc2 = 0;
  }
#line 22218 "parser.c"
    break;

  case 1862:
#line 12655 "parser.y"
  {
	eval_inc2 = 0;
	cb_verify (cb_missing_statement,
		_("WHEN without imperative statement"));
	/* Note: we don't clear the EVALUATE terminator here
	         as we'd have to skip this later
	         [side effect: possible warning about missing terminator] */
	yyval = CB_BUILD_CHAIN (CB_LIST_INIT (cb_build_continue ()), yyvsp[-1]);
  }
#line 22232 "parser.c"
    break;

  case 1863:
#line 12665 "parser.y"
  {
	eval_inc2 = 0;
	cb_verify (cb_missing_statement,
		_("WHEN without imperative statement"));
	/* Put the dot token back into the stack for reparse */
	cb_unput_dot ();
	yyval = CB_BUILD_CHAIN (CB_LIST_INIT (cb_build_continue ()), yyvsp[-1]);
  }
#line 22245 "parser.c"
    break;

  case 1864:
#line 12678 "parser.y"
  {
	yyval = CB_BUILD_CHAIN (yyvsp[0], NULL);
	eval_inc2 = 0;
  }
#line 22254 "parser.c"
    break;

  case 1865:
#line 12683 "parser.y"
  {
	eval_inc2 = 0;
	cb_verify (cb_missing_statement,
		_("WHEN OTHER without imperative statement"));
	/* Note: we don't clear the EVALUATE terminator here
	         as we'd have to skip this later
	         [side effect: possible warning about missing terminator] */
	yyval = NULL;
  }
#line 22268 "parser.c"
    break;

  case 1866:
#line 12693 "parser.y"
  {
	eval_inc2 = 0;
	cb_verify (cb_missing_statement,
		_("WHEN OTHER without imperative statement"));
	/* Put the dot token back into the stack for reparse */
	cb_unput_dot ();
	yyval = NULL;
  }
#line 22281 "parser.c"
    break;

  case 1867:
#line 12705 "parser.y"
  {
	backup_current_pos ();
  }
#line 22289 "parser.c"
    break;

  case 1868:
#line 12709 "parser.y"
  {
	yyval = CB_LIST_INIT (yyvsp[0]);
	restore_backup_pos (yyval);
	eval_inc2 = 0;
  }
#line 22299 "parser.c"
    break;

  case 1869:
#line 12716 "parser.y"
  {
	backup_current_pos ();
  }
#line 22307 "parser.c"
    break;

  case 1870:
#line 12720 "parser.y"
  {
	yyval = cb_list_add (yyvsp[-3], yyvsp[0]);
	restore_backup_pos (yyval);
	eval_inc2 = 0;
  }
#line 22317 "parser.c"
    break;

  case 1871:
#line 12728 "parser.y"
                                { yyval = CB_LIST_INIT (yyvsp[0]); }
#line 22323 "parser.c"
    break;

  case 1872:
#line 12730 "parser.y"
                                { yyval = cb_list_add (yyvsp[-2], yyvsp[0]); }
#line 22329 "parser.c"
    break;

  case 1873:
#line 12735 "parser.y"
  {
	cb_tree	not0;
	cb_tree	e1;
	cb_tree	e2;
	cb_tree	x;
	cb_tree	parm1;

	not0 = cb_int0;
	e2 = yyvsp[0];
	x = NULL;
	parm1 = yyvsp[-1];
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
	yyval = CB_BUILD_PAIR (not0, CB_BUILD_PAIR (e1, e2));

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
#line 22408 "parser.c"
    break;

  case 1874:
#line 12809 "parser.y"
                                { yyval = cb_any; eval_inc2++; }
#line 22414 "parser.c"
    break;

  case 1875:
#line 12810 "parser.y"
                                { yyval = cb_true; eval_inc2++; }
#line 22420 "parser.c"
    break;

  case 1876:
#line 12811 "parser.y"
                                { yyval = cb_false; eval_inc2++; }
#line 22426 "parser.c"
    break;

  case 1877:
#line 12812 "parser.y"
                                { yyval = cb_error_node; eval_inc2++; }
#line 22432 "parser.c"
    break;

  case 1878:
#line 12816 "parser.y"
                                { yyval = NULL; }
#line 22438 "parser.c"
    break;

  case 1879:
#line 12817 "parser.y"
                                { yyval = yyvsp[0]; }
#line 22444 "parser.c"
    break;

  case 1880:
#line 12822 "parser.y"
  {
	TERMINATOR_WARNING (yyvsp[(-2) - (0)], EVALUATE);
  }
#line 22452 "parser.c"
    break;

  case 1881:
#line 12826 "parser.y"
  {
	TERMINATOR_CLEAR (yyvsp[(-2) - (1)], EVALUATE);
  }
#line 22460 "parser.c"
    break;

  case 1882:
#line 12835 "parser.y"
  {
	begin_statement ("EXHIBIT", 0);
	line_column = NULL;
	cobc_cs_check = CB_CS_EXHIBIT;
  }
#line 22470 "parser.c"
    break;

  case 1883:
#line 12841 "parser.y"
  {
	cobc_cs_check = 0;
  }
#line 22478 "parser.c"
    break;

  case 1884:
#line 12848 "parser.y"
  {
	if (yyvsp[0] || !yyvsp[-1]) {
		exhibit_named = 1;
		advancing_value = cb_int1;
	} else {
		exhibit_named = 0;
	}
	if (yyvsp[-1]) {
		exhibit_changed = 1;
		/* TODO: feature for a later version (needs temporary fields,
		   one per target, but not duplicated between multiple EXHIBIT) */
		CB_PENDING ("EXHIBIT CHANGED");
		/* note: literals are _always_ displayed, unchanged are replaced
		         by spaces in full length (including the possible NAMED part) */
	} else {
		exhibit_changed = 0;
	}
  }
#line 22501 "parser.c"
    break;

  case 1885:
#line 12867 "parser.y"
  {
	/* note: position-specifier, ERASE and UPON are MS-COBOL extensions,
	         but we won't add an extra dialect option for this - if wanted
			 we can add one for the position-specifier and use that for
			 those clauses, too */
	if (upon_value != NULL) {
		/* TODO: come back to this MS-COBOL feature later */
		CB_PENDING ("EXHIBIT UPON");
	}
	if (yyvsp[-2] != NULL) {
		attach_attrib_to_cur_stmt ();
		current_statement->attr_ptr->dispattrs = COB_SCREEN_ERASE_EOS;
	}
	/* note: while MF does not do this, OSVS had empty line suppression for
	         CHANGED - do the same ... later */
	cb_emit_display (yyvsp[-1], NULL, cb_int1, line_column,
			 current_statement->attr_ptr,
			 0, DEVICE_DISPLAY);
  }
#line 22525 "parser.c"
    break;

  case 1886:
#line 12888 "parser.y"
                { yyval = NULL; }
#line 22531 "parser.c"
    break;

  case 1887:
#line 12888 "parser.y"
                                                { yyval = cb_int0; }
#line 22537 "parser.c"
    break;

  case 1888:
#line 12889 "parser.y"
                { yyval = NULL; }
#line 22543 "parser.c"
    break;

  case 1889:
#line 12889 "parser.y"
                                                { yyval = cb_int0; }
#line 22549 "parser.c"
    break;

  case 1890:
#line 12893 "parser.y"
  {
	if (exhibit_named && !CB_LITERAL_P (yyvsp[0])) {
		yyval = CB_LIST_INIT (cb_exhbit_literal (yyvsp[0]));
		yyval = cb_list_add (yyval, yyvsp[0]);
	} else {
		yyval = CB_LIST_INIT (yyvsp[0]);
	}
  }
#line 22562 "parser.c"
    break;

  case 1891:
#line 12902 "parser.y"
  {
	yyval = cb_list_add (yyvsp[-1], cb_space);
	if (exhibit_named && !CB_LITERAL_P (yyvsp[0])) {
		yyval = cb_list_add (yyval, cb_exhbit_literal (yyvsp[0]));
	}
	yyval = cb_list_add (yyvsp[-1], yyvsp[0]);
  }
#line 22574 "parser.c"
    break;

  case 1894:
#line 12921 "parser.y"
  {
	begin_statement ("EXIT", 0);
	cobc_cs_check = CB_CS_EXIT;
  }
#line 22583 "parser.c"
    break;

  case 1895:
#line 12926 "parser.y"
  {
	cobc_cs_check = 0;
  }
#line 22591 "parser.c"
    break;

  case 1896:
#line 12933 "parser.y"
  {
  /* TODO: add warning/error if there's another statement in the paragraph */
  }
#line 22599 "parser.c"
    break;

  case 1897:
#line 12937 "parser.y"
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
	if (yyvsp[0]) {
		if (!current_program->cb_return_code) {
			cb_error_x (yyvsp[0], _("RETURNING/GIVING not allowed for non-returning runtime elements"));
		} else {
			cb_emit_move (yyvsp[0], CB_LIST_INIT (current_program->cb_return_code));
		}
	}
	current_statement->name = (const char *)"EXIT PROGRAM";
	cb_emit_exit (0);
  }
#line 22628 "parser.c"
    break;

  case 1898:
#line 12962 "parser.y"
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
#line 22646 "parser.c"
    break;

  case 1899:
#line 12976 "parser.y"
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
#line 22673 "parser.c"
    break;

  case 1900:
#line 12999 "parser.y"
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
#line 22700 "parser.c"
    break;

  case 1901:
#line 13022 "parser.y"
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
#line 22725 "parser.c"
    break;

  case 1902:
#line 13043 "parser.y"
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
#line 22750 "parser.c"
    break;

  case 1903:
#line 13066 "parser.y"
                                { yyval = NULL; }
#line 22756 "parser.c"
    break;

  case 1904:
#line 13069 "parser.y"
                        { yyval = yyvsp[0]; }
#line 22762 "parser.c"
    break;

  case 1905:
#line 13077 "parser.y"
  {
	begin_statement ("FREE", 0);
	current_statement->flag_no_based = 1;
  }
#line 22771 "parser.c"
    break;

  case 1907:
#line 13086 "parser.y"
  {
	cb_emit_free (yyvsp[0]);
  }
#line 22779 "parser.c"
    break;

  case 1908:
#line 13096 "parser.y"
  {
	begin_statement ("GENERATE", 0);
  }
#line 22787 "parser.c"
    break;

  case 1910:
#line 13105 "parser.y"
  {
#if 0 /* CHECKME: likely not needed */
	begin_implicit_statement ();
#endif
	if (yyvsp[0] != cb_error_node) {
		cb_emit_generate (yyvsp[0]);
	}
  }
#line 22800 "parser.c"
    break;

  case 1911:
#line 13119 "parser.y"
  {
	if (!current_paragraph->flag_statement) {
		current_paragraph->flag_first_is_goto = 1;
	}
	begin_statement ("GO TO", 0);
	save_debug = start_debug;
	start_debug = 0;
  }
#line 22813 "parser.c"
    break;

  case 1913:
#line 13132 "parser.y"
  {
	cb_emit_goto (yyvsp[-1], yyvsp[0]);
	start_debug = save_debug;
  }
#line 22822 "parser.c"
    break;

  case 1914:
#line 13137 "parser.y"
  {
	if (cb_verify (cb_goto_entry, "ENTRY FOR GO TO")) {
		cb_emit_goto_entry (yyvsp[-1], yyvsp[0]);
	}
	start_debug = save_debug;
  }
#line 22833 "parser.c"
    break;

  case 1915:
#line 13147 "parser.y"
  {
	check_unreached = 1;
	yyval = NULL;
  }
#line 22842 "parser.c"
    break;

  case 1916:
#line 13152 "parser.y"
  {
	check_unreached = 0;
	yyval = yyvsp[0];
  }
#line 22851 "parser.c"
    break;

  case 1917:
#line 13163 "parser.y"
  {
	begin_statement ("GOBACK", 0);
	check_unreached = 1;
	if (yyvsp[0]) {
		if (!current_program->cb_return_code) {
			cb_error_x (yyvsp[0], _("RETURNING/GIVING not allowed for non-returning runtime elements"));
		} else {
			cb_emit_move (yyvsp[0], CB_LIST_INIT (current_program->cb_return_code));
		}
	}
	cb_emit_exit (1U);
  }
#line 22868 "parser.c"
    break;

  case 1918:
#line 13182 "parser.y"
  {
	begin_statement ("IF", TERM_IF);
  }
#line 22876 "parser.c"
    break;

  case 1920:
#line 13191 "parser.y"
  {
	cb_emit_if (yyvsp[(-1) - (5)], yyvsp[-3], yyvsp[0]);
  }
#line 22884 "parser.c"
    break;

  case 1921:
#line 13195 "parser.y"
  {
	cb_emit_if (yyvsp[(-1) - (3)], NULL, yyvsp[0]);
	cb_verify (cb_missing_statement,
		_("IF without imperative statement"));
  }
#line 22894 "parser.c"
    break;

  case 1922:
#line 13201 "parser.y"
  {
	cb_emit_if (yyvsp[(-1) - (2)], yyvsp[0], NULL);
  }
#line 22902 "parser.c"
    break;

  case 1923:
#line 13207 "parser.y"
  {
	cb_save_cond ();
  }
#line 22910 "parser.c"
    break;

  case 1924:
#line 13211 "parser.y"
  {
	cb_save_cond ();
  }
#line 22918 "parser.c"
    break;

  case 1925:
#line 13217 "parser.y"
  {
	cb_true_side ();
  }
#line 22926 "parser.c"
    break;

  case 1926:
#line 13223 "parser.y"
  {
	cb_false_side ();
  }
#line 22934 "parser.c"
    break;

  case 1927:
#line 13230 "parser.y"
  {
	TERMINATOR_WARNING (yyvsp[(-4) - (0)], IF);
	cb_terminate_cond ();
  }
#line 22943 "parser.c"
    break;

  case 1928:
#line 13235 "parser.y"
  {
	TERMINATOR_CLEAR (yyvsp[(-4) - (1)], IF);
	cb_terminate_cond ();
  }
#line 22952 "parser.c"
    break;

  case 1929:
#line 13246 "parser.y"
  {
	begin_statement ("INITIALIZE", 0);
  }
#line 22960 "parser.c"
    break;

  case 1931:
#line 13255 "parser.y"
  {
	cb_emit_initialize (yyvsp[-4], yyvsp[-3], yyvsp[-2], yyvsp[-1], yyvsp[0]);
  }
#line 22968 "parser.c"
    break;

  case 1932:
#line 13261 "parser.y"
                                { yyval = NULL; }
#line 22974 "parser.c"
    break;

  case 1933:
#line 13262 "parser.y"
                                { yyval = cb_true; }
#line 22980 "parser.c"
    break;

  case 1934:
#line 13266 "parser.y"
                                { yyval = NULL; }
#line 22986 "parser.c"
    break;

  case 1935:
#line 13267 "parser.y"
                                { yyval = cb_true; }
#line 22992 "parser.c"
    break;

  case 1936:
#line 13268 "parser.y"
                                { yyval = yyvsp[-2]; }
#line 22998 "parser.c"
    break;

  case 1937:
#line 13273 "parser.y"
  {
	yyval = NULL;
  }
#line 23006 "parser.c"
    break;

  case 1938:
#line 13277 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 23014 "parser.c"
    break;

  case 1939:
#line 13284 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 23022 "parser.c"
    break;

  case 1940:
#line 13289 "parser.y"
  {
	yyval = cb_list_append (yyvsp[-1], yyvsp[0]);
  }
#line 23030 "parser.c"
    break;

  case 1941:
#line 13296 "parser.y"
  {
	yyval = CB_BUILD_PAIR (yyvsp[-3], yyvsp[0]);
  }
#line 23038 "parser.c"
    break;

  case 1942:
#line 13302 "parser.y"
                        { yyval = cb_int (CB_CATEGORY_ALPHABETIC); }
#line 23044 "parser.c"
    break;

  case 1943:
#line 13303 "parser.y"
                        { yyval = cb_int (CB_CATEGORY_ALPHANUMERIC); }
#line 23050 "parser.c"
    break;

  case 1944:
#line 13304 "parser.y"
                        { yyval = cb_int (CB_CATEGORY_NUMERIC); }
#line 23056 "parser.c"
    break;

  case 1945:
#line 13305 "parser.y"
                        { yyval = cb_int (CB_CATEGORY_ALPHANUMERIC_EDITED); }
#line 23062 "parser.c"
    break;

  case 1946:
#line 13306 "parser.y"
                        { yyval = cb_int (CB_CATEGORY_NUMERIC_EDITED); }
#line 23068 "parser.c"
    break;

  case 1947:
#line 13307 "parser.y"
                        { yyval = cb_int (CB_CATEGORY_NATIONAL); }
#line 23074 "parser.c"
    break;

  case 1948:
#line 13308 "parser.y"
                        { yyval = cb_int (CB_CATEGORY_NATIONAL_EDITED); }
#line 23080 "parser.c"
    break;

  case 1949:
#line 13320 "parser.y"
  {
	yyval = NULL;
  }
#line 23088 "parser.c"
    break;

  case 1950:
#line 13324 "parser.y"
  {
	yyval = cb_true;
  }
#line 23096 "parser.c"
    break;

  case 1951:
#line 13333 "parser.y"
  {
	begin_statement ("INITIATE", 0);
  }
#line 23104 "parser.c"
    break;

  case 1953:
#line 13341 "parser.y"
  {
#if 0 /* CHECKME: likely not needed */
	begin_implicit_statement ();
#endif
	if (yyvsp[0] != cb_error_node) {
		cb_emit_initiate (yyvsp[0]);
	}
  }
#line 23117 "parser.c"
    break;

  case 1954:
#line 13350 "parser.y"
  {
	begin_implicit_statement ();
	if (yyvsp[0] != cb_error_node) {
		cb_emit_initiate (yyvsp[0]);
	}
  }
#line 23128 "parser.c"
    break;

  case 1955:
#line 13362 "parser.y"
  {
	begin_statement ("INQUIRE", 0);
	cobc_cs_check = CB_CS_INQUIRE_MODIFY;
  }
#line 23137 "parser.c"
    break;

  case 1956:
#line 13367 "parser.y"
  {
	cobc_cs_check = 0;
  }
#line 23145 "parser.c"
    break;

  case 1959:
#line 13381 "parser.y"
  {
	begin_statement ("INSPECT", 0);
	inspect_keyword = 0;
  }
#line 23154 "parser.c"
    break;

  case 1969:
#line 13409 "parser.y"
  {
	previous_tallying_phrase = NO_PHRASE;
	cb_init_tallying ();
  }
#line 23163 "parser.c"
    break;

  case 1970:
#line 13414 "parser.y"
  {
	if (!(previous_tallying_phrase == CHARACTERS_PHRASE
	      || previous_tallying_phrase == VALUE_REGION_PHRASE)) {
		cb_error (_("TALLYING clause is incomplete"));
	} else {
		cb_emit_inspect (yyvsp[-3], yyvsp[0], TALLYING_CLAUSE);
	}

	yyval = yyvsp[-3];
  }
#line 23178 "parser.c"
    break;

  case 1971:
#line 13430 "parser.y"
  {
	cb_emit_inspect (yyvsp[-2], yyvsp[0], REPLACING_CLAUSE);
	inspect_keyword = 0;
  }
#line 23187 "parser.c"
    break;

  case 1972:
#line 13440 "parser.y"
  {
	cb_tree		x = cb_build_converting (yyvsp[-3], yyvsp[-1], yyvsp[0]);
	cb_emit_inspect (yyvsp[-5], x, CONVERTING_CLAUSE);
  }
#line 23196 "parser.c"
    break;

  case 1973:
#line 13448 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 23204 "parser.c"
    break;

  case 1974:
#line 13452 "parser.y"
  {
	yyval = cb_list_append (yyvsp[-1], yyvsp[0]);
  }
#line 23212 "parser.c"
    break;

  case 1975:
#line 13459 "parser.y"
  {
	check_preceding_tallying_phrases (FOR_PHRASE);
	yyval = cb_build_tallying_data (yyvsp[-1]);
  }
#line 23221 "parser.c"
    break;

  case 1976:
#line 13464 "parser.y"
  {
	check_preceding_tallying_phrases (CHARACTERS_PHRASE);
	yyval = cb_build_tallying_characters (yyvsp[0]);
  }
#line 23230 "parser.c"
    break;

  case 1977:
#line 13469 "parser.y"
  {
	check_preceding_tallying_phrases (ALL_LEADING_TRAILING_PHRASES);
	yyval = cb_build_tallying_all ();
  }
#line 23239 "parser.c"
    break;

  case 1978:
#line 13474 "parser.y"
  {
	check_preceding_tallying_phrases (ALL_LEADING_TRAILING_PHRASES);
	yyval = cb_build_tallying_leading ();
  }
#line 23248 "parser.c"
    break;

  case 1979:
#line 13479 "parser.y"
  {
	check_preceding_tallying_phrases (ALL_LEADING_TRAILING_PHRASES);
	yyval = cb_build_tallying_trailing ();
  }
#line 23257 "parser.c"
    break;

  case 1980:
#line 13484 "parser.y"
  {
	check_preceding_tallying_phrases (VALUE_REGION_PHRASE);
	yyval = cb_build_tallying_value (yyvsp[-1], yyvsp[0]);
  }
#line 23266 "parser.c"
    break;

  case 1981:
#line 13491 "parser.y"
                                { yyval = yyvsp[0]; }
#line 23272 "parser.c"
    break;

  case 1982:
#line 13492 "parser.y"
                                { yyval = cb_list_append (yyvsp[-1], yyvsp[0]); }
#line 23278 "parser.c"
    break;

  case 1983:
#line 13497 "parser.y"
  {
	yyval = cb_build_replacing_characters (yyvsp[-1], yyvsp[0]);
	inspect_keyword = 0;
  }
#line 23287 "parser.c"
    break;

  case 1984:
#line 13502 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 23295 "parser.c"
    break;

  case 1986:
#line 13509 "parser.y"
                                { inspect_keyword = 1; }
#line 23301 "parser.c"
    break;

  case 1987:
#line 13510 "parser.y"
                                { inspect_keyword = 2; }
#line 23307 "parser.c"
    break;

  case 1988:
#line 13511 "parser.y"
                                { inspect_keyword = 3; }
#line 23313 "parser.c"
    break;

  case 1989:
#line 13512 "parser.y"
                                { inspect_keyword = 4; }
#line 23319 "parser.c"
    break;

  case 1990:
#line 13517 "parser.y"
  {
	switch (inspect_keyword) {
		case 1:
			yyval = cb_build_replacing_all (yyvsp[-3], yyvsp[-1], yyvsp[0]);
			break;
		case 2:
			yyval = cb_build_replacing_leading (yyvsp[-3], yyvsp[-1], yyvsp[0]);
			break;
		case 3:
			yyval = cb_build_replacing_first (yyvsp[-3], yyvsp[-1], yyvsp[0]);
			break;
		case 4:
			yyval = cb_build_replacing_trailing (yyvsp[-3], yyvsp[-1], yyvsp[0]);
			break;
		default:
			cb_error_x (CB_TREE (current_statement),
				    _("INSPECT missing ALL/FIRST/LEADING/TRAILING"));
			yyval = cb_build_replacing_all (yyvsp[-3], yyvsp[-1], yyvsp[0]);
			break;
	}
  }
#line 23345 "parser.c"
    break;

  case 1991:
#line 13544 "parser.y"
  {
	yyval = cb_build_inspect_region_start ();
  }
#line 23353 "parser.c"
    break;

  case 1992:
#line 13548 "parser.y"
  {
	yyval = cb_list_add (cb_build_inspect_region_start (), yyvsp[0]);
  }
#line 23361 "parser.c"
    break;

  case 1993:
#line 13552 "parser.y"
  {
	yyval = cb_list_add (cb_build_inspect_region_start (), yyvsp[0]);
  }
#line 23369 "parser.c"
    break;

  case 1994:
#line 13556 "parser.y"
  {
	yyval = cb_list_add (cb_list_add (cb_build_inspect_region_start (), yyvsp[-1]), yyvsp[0]);
  }
#line 23377 "parser.c"
    break;

  case 1995:
#line 13560 "parser.y"
  {
	yyval = cb_list_add (cb_list_add (cb_build_inspect_region_start (), yyvsp[-1]), yyvsp[0]);
  }
#line 23385 "parser.c"
    break;

  case 1996:
#line 13567 "parser.y"
  {
	yyval = CB_BUILD_FUNCALL_1 ("cob_inspect_before", yyvsp[0]);
  }
#line 23393 "parser.c"
    break;

  case 1997:
#line 13574 "parser.y"
  {
	yyval = CB_BUILD_FUNCALL_1 ("cob_inspect_after", yyvsp[0]);
  }
#line 23401 "parser.c"
    break;

  case 1998:
#line 13583 "parser.y"
  {
	begin_statement ("JSON GENERATE", TERM_JSON);
	cobc_in_json_generate_body = 1;
	cobc_cs_check = CB_CS_JSON_GENERATE;
  }
#line 23411 "parser.c"
    break;

  case 2000:
#line 13595 "parser.y"
  {
	ml_suppress_list = NULL;
  }
#line 23419 "parser.c"
    break;

  case 2001:
#line 13600 "parser.y"
  {
	cobc_in_json_generate_body = 0;
	cobc_cs_check = 0;
  }
#line 23428 "parser.c"
    break;

  case 2002:
#line 13605 "parser.y"
  {
	cb_emit_json_generate (yyvsp[-8], yyvsp[-6], yyvsp[-5], yyvsp[-3], ml_suppress_list);
  }
#line 23436 "parser.c"
    break;

  case 2003:
#line 13612 "parser.y"
  {
	yyval = NULL;
  }
#line 23444 "parser.c"
    break;

  case 2004:
#line 13616 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 23452 "parser.c"
    break;

  case 2007:
#line 13628 "parser.y"
  {
	error_if_following_every_clause ();
	add_identifier_to_ml_suppress_conds (yyvsp[0]);
  }
#line 23461 "parser.c"
    break;

  case 2008:
#line 13636 "parser.y"
  {
	TERMINATOR_WARNING (yyvsp[(-2) - (0)], JSON);
  }
#line 23469 "parser.c"
    break;

  case 2009:
#line 13640 "parser.y"
  {
	TERMINATOR_CLEAR (yyvsp[(-2) - (1)], JSON);
  }
#line 23477 "parser.c"
    break;

  case 2010:
#line 13649 "parser.y"
  {
	begin_statement ("JSON PARSE", TERM_JSON);
	CB_PENDING (_("JSON PARSE"));
  }
#line 23486 "parser.c"
    break;

  case 2015:
#line 13674 "parser.y"
  {
	begin_statement ("MERGE", 0);
	current_statement->flag_merge = 1;
  }
#line 23495 "parser.c"
    break;

  case 2017:
#line 13686 "parser.y"
  {
	begin_statement ("MODIFY", TERM_MODIFY);
	cobc_cs_check = CB_CS_INQUIRE_MODIFY;
  }
#line 23504 "parser.c"
    break;

  case 2018:
#line 13692 "parser.y"
  {
	cobc_cs_check = 0;
  }
#line 23512 "parser.c"
    break;

  case 2021:
#line 13704 "parser.y"
  {
	TERMINATOR_WARNING (yyvsp[(-2) - (0)], MODIFY);
  }
#line 23520 "parser.c"
    break;

  case 2022:
#line 13708 "parser.y"
  {
	TERMINATOR_CLEAR (yyvsp[(-2) - (1)], MODIFY);
  }
#line 23528 "parser.c"
    break;

  case 2023:
#line 13718 "parser.y"
  {
	begin_statement ("MOVE", 0);
  }
#line 23536 "parser.c"
    break;

  case 2025:
#line 13726 "parser.y"
  {
	cb_emit_move (yyvsp[-2], yyvsp[0]);
  }
#line 23544 "parser.c"
    break;

  case 2026:
#line 13730 "parser.y"
  {
	cb_emit_move_corresponding (yyvsp[-2], yyvsp[0]);
  }
#line 23552 "parser.c"
    break;

  case 2027:
#line 13740 "parser.y"
  {
	begin_statement ("MULTIPLY", TERM_MULTIPLY);
  }
#line 23560 "parser.c"
    break;

  case 2029:
#line 13749 "parser.y"
  {
	cb_emit_arithmetic (yyvsp[-1], '*', yyvsp[-3]);
  }
#line 23568 "parser.c"
    break;

  case 2030:
#line 13753 "parser.y"
  {
	cb_emit_arithmetic (yyvsp[-1], 0, cb_build_binary_op (yyvsp[-5], '*', yyvsp[-3]));
  }
#line 23576 "parser.c"
    break;

  case 2031:
#line 13760 "parser.y"
  {
	TERMINATOR_WARNING (yyvsp[(-2) - (0)], MULTIPLY);
  }
#line 23584 "parser.c"
    break;

  case 2032:
#line 13764 "parser.y"
  {
	TERMINATOR_CLEAR (yyvsp[(-2) - (1)], MULTIPLY);
  }
#line 23592 "parser.c"
    break;

  case 2033:
#line 13774 "parser.y"
  {
	begin_statement ("OPEN", 0);
	cobc_cs_check = CB_CS_OPEN;
  }
#line 23601 "parser.c"
    break;

  case 2037:
#line 13788 "parser.y"
  {
	cb_tree l;
	cb_tree x;

	if ((yyvsp[-5] && yyvsp[-3]) || (yyvsp[-5] && yyvsp[0]) || (yyvsp[-3] && yyvsp[0])) {
		cb_error_x (CB_TREE (current_statement),
			    _("%s and %s are mutually exclusive"), "SHARING", _("LOCK clauses"));
	}
	if (yyvsp[0]) {
		x = yyvsp[0];
	} else if (yyvsp[-3]) {
		x = yyvsp[-3];
	} else {
		x = yyvsp[-5];
	}

	for (l = yyvsp[-1]; l; l = CB_CHAIN (l)) {
		if (CB_VALID_TREE (CB_VALUE (l))) {
			begin_implicit_statement ();
			cb_emit_open (CB_VALUE (l), yyvsp[-4], x);
		}
	}
  }
#line 23629 "parser.c"
    break;

  case 2038:
#line 13815 "parser.y"
                                { yyval = NULL; }
#line 23635 "parser.c"
    break;

  case 2039:
#line 13816 "parser.y"
                                { yyval = cb_int (COB_LOCK_OPEN_EXCLUSIVE); }
#line 23641 "parser.c"
    break;

  case 2040:
#line 13820 "parser.y"
                                { yyval = cb_int (COB_OPEN_INPUT); }
#line 23647 "parser.c"
    break;

  case 2041:
#line 13821 "parser.y"
                                { yyval = cb_int (COB_OPEN_OUTPUT); }
#line 23653 "parser.c"
    break;

  case 2042:
#line 13822 "parser.y"
                                { yyval = cb_int (COB_OPEN_I_O); }
#line 23659 "parser.c"
    break;

  case 2043:
#line 13823 "parser.y"
                                { yyval = cb_int (COB_OPEN_EXTEND); }
#line 23665 "parser.c"
    break;

  case 2044:
#line 13827 "parser.y"
                                { yyval = NULL; }
#line 23671 "parser.c"
    break;

  case 2045:
#line 13828 "parser.y"
                                { yyval = yyvsp[0]; }
#line 23677 "parser.c"
    break;

  case 2046:
#line 13832 "parser.y"
                                { yyval = NULL; }
#line 23683 "parser.c"
    break;

  case 2047:
#line 13833 "parser.y"
                        { yyval = yyvsp[0]; }
#line 23689 "parser.c"
    break;

  case 2048:
#line 13834 "parser.y"
                         { yyval = NULL; }
#line 23695 "parser.c"
    break;

  case 2049:
#line 13838 "parser.y"
                                        { yyval = yyvsp[-1]; }
#line 23701 "parser.c"
    break;

  case 2050:
#line 13840 "parser.y"
  {
	  (void)cb_verify (CB_OBSOLETE, "OPEN LEAVE/REREAD/DISP");
	  yyval = NULL;
  }
#line 23710 "parser.c"
    break;

  case 2051:
#line 13847 "parser.y"
                                { yyval = yyvsp[0]; }
#line 23716 "parser.c"
    break;

  case 2052:
#line 13848 "parser.y"
                                { yyval = yyvsp[0]; }
#line 23722 "parser.c"
    break;

  case 2053:
#line 13852 "parser.y"
                        { yyval = cb_int (COB_LOCK_OPEN_EXCLUSIVE); }
#line 23728 "parser.c"
    break;

  case 2054:
#line 13854 "parser.y"
  {
	yyval = cb_int (COB_LOCK_OPEN_EXCLUSIVE);
	/* TODO: check for indexed; pass extra flag to fileio */
	CB_PENDING ("WITH MASS-UPDATE");
  }
#line 23738 "parser.c"
    break;

  case 2055:
#line 13860 "parser.y"
  {
	yyval = cb_int (COB_LOCK_OPEN_EXCLUSIVE);
	/* TODO: check for indexed; pass extra flag to fileio */
	CB_PENDING ("WITH BULK-ADDITION");
  }
#line 23748 "parser.c"
    break;

  case 2056:
#line 13868 "parser.y"
                                { yyval = cb_int (COB_LOCK_OPEN_EXCLUSIVE); }
#line 23754 "parser.c"
    break;

  case 2057:
#line 13869 "parser.y"
                        { yyval = NULL; }
#line 23760 "parser.c"
    break;

  case 2058:
#line 13870 "parser.y"
                        { yyval = NULL; }
#line 23766 "parser.c"
    break;

  case 2062:
#line 13882 "parser.y"
  {
	/* FIXME: only allow for sequential files */
	/* FIXME: only allow with INPUT or OUTPUT */
	CB_PENDING ("OPEN WITH NO REWIND");
	yyval = NULL;
  }
#line 23777 "parser.c"
    break;

  case 2063:
#line 13889 "parser.y"
  {
	/* FIXME: only allow for sequential / line-sequential files */
	/* FIXME: only allow with INPUT */
	/* FIXME: add actual compiler configuration */
	if (cb_warn_opt_val[cb_warn_obsolete] == COBC_WARN_AS_ERROR) {
		(void)cb_verify (CB_OBSOLETE, "OPEN REVERSED");
	} else {
		/* FIXME: set file attribute */
		CB_PENDING ("OPEN REVERSED");
	};
	yyval = NULL;
  }
#line 23794 "parser.c"
    break;

  case 2067:
#line 13912 "parser.y"
  {
	begin_statement ("PERFORM", TERM_PERFORM);
	/* Turn off field debug - PERFORM is special */
	save_debug = start_debug;
	start_debug = 0;
	cobc_cs_check = CB_CS_PERFORM;
  }
#line 23806 "parser.c"
    break;

  case 2069:
#line 13927 "parser.y"
  {
	cb_emit_perform (yyvsp[0], yyvsp[-2], yyvsp[-3], yyvsp[-1]);
	start_debug = save_debug;
	cobc_cs_check = 0;
  }
#line 23816 "parser.c"
    break;

  case 2070:
#line 13935 "parser.y"
  {
	CB_ADD_TO_CHAIN (yyvsp[-1], perform_stack);
	/* Restore field debug before inline statements */
	start_debug = save_debug;
	cobc_cs_check = 0;
  }
#line 23827 "parser.c"
    break;

  case 2071:
#line 13942 "parser.y"
  {
	perform_stack = CB_CHAIN (perform_stack);
	cb_emit_perform (yyvsp[-4], yyvsp[-1], yyvsp[-5], yyvsp[-3]);
  }
#line 23836 "parser.c"
    break;

  case 2072:
#line 13949 "parser.y"
  {
	cb_verify (cb_missing_statement,
		_("inline PERFORM without imperative statement"));
  }
#line 23845 "parser.c"
    break;

  case 2073:
#line 13954 "parser.y"
  {
	cb_emit_perform (yyvsp[-3], NULL, yyvsp[-4], yyvsp[-2]);
	start_debug = save_debug;
	cobc_cs_check = 0;
  }
#line 23855 "parser.c"
    break;

  case 2074:
#line 13963 "parser.y"
  {
	if (cb_relaxed_syntax_checks) {
		TERMINATOR_WARNING (yyvsp[(-6) - (0)], PERFORM);
	} else {
		TERMINATOR_ERROR (yyvsp[(-6) - (0)], PERFORM);
	}
  }
#line 23867 "parser.c"
    break;

  case 2075:
#line 13971 "parser.y"
  {
	TERMINATOR_CLEAR (yyvsp[(-6) - (1)], PERFORM);
  }
#line 23875 "parser.c"
    break;

  case 2076:
#line 13978 "parser.y"
  {
	TERMINATOR_CLEAR (yyvsp[(-3) - (1)], PERFORM);
  }
#line 23883 "parser.c"
    break;

  case 2077:
#line 13982 "parser.y"
  {
	if (cb_relaxed_syntax_checks) {
		TERMINATOR_WARNING (yyvsp[(-3) - (1)], PERFORM);
	} else {
		TERMINATOR_ERROR (yyvsp[(-3) - (1)], PERFORM);
	}
	/* Put the dot token back into the stack for reparse */
	cb_unput_dot ();
  }
#line 23897 "parser.c"
    break;

  case 2078:
#line 13995 "parser.y"
  {
	/* Return from $1 */
	CB_REFERENCE (yyvsp[0])->length = cb_true;
	CB_REFERENCE (yyvsp[0])->flag_decl_ok = 1;
	yyval = CB_BUILD_PAIR (yyvsp[0], yyvsp[0]);
  }
#line 23908 "parser.c"
    break;

  case 2079:
#line 14002 "parser.y"
  {
	/* Return from $3 */
	CB_REFERENCE (yyvsp[0])->length = cb_true;
	CB_REFERENCE (yyvsp[-2])->flag_decl_ok = 1;
	CB_REFERENCE (yyvsp[0])->flag_decl_ok = 1;
	yyval = CB_BUILD_PAIR (yyvsp[-2], yyvsp[0]);
  }
#line 23920 "parser.c"
    break;

  case 2080:
#line 14013 "parser.y"
  {
	yyval = cb_build_perform_once (NULL);
  }
#line 23928 "parser.c"
    break;

  case 2081:
#line 14017 "parser.y"
  {
	yyval = cb_build_perform_times (yyvsp[-1]);
	current_program->loop_counter++;
  }
#line 23937 "parser.c"
    break;

  case 2082:
#line 14022 "parser.y"
  {
	yyval = cb_build_perform_forever (NULL);
  }
#line 23945 "parser.c"
    break;

  case 2083:
#line 14026 "parser.y"
  {
	cb_tree varying;

	if (!yyvsp[0]) {
		yyval = cb_build_perform_forever (NULL);
	} else {
		if (yyvsp[-2] == CB_AFTER)
			cb_build_perform_after_until();
		varying = CB_LIST_INIT (cb_build_perform_varying (NULL, NULL, NULL, yyvsp[0]));
		yyval = cb_build_perform_until (yyvsp[-2], varying);
	}
  }
#line 23962 "parser.c"
    break;

  case 2084:
#line 14039 "parser.y"
  {
	yyval = cb_build_perform_until (yyvsp[-2], yyvsp[0]);
  }
#line 23970 "parser.c"
    break;

  case 2085:
#line 14045 "parser.y"
                                { yyval = CB_BEFORE; }
#line 23976 "parser.c"
    break;

  case 2086:
#line 14046 "parser.y"
                                { yyval = yyvsp[0]; }
#line 23982 "parser.c"
    break;

  case 2087:
#line 14050 "parser.y"
                                { yyval = NULL; }
#line 23988 "parser.c"
    break;

  case 2088:
#line 14051 "parser.y"
                                { yyval = yyvsp[0]; }
#line 23994 "parser.c"
    break;

  case 2089:
#line 14054 "parser.y"
                                { yyval = CB_LIST_INIT (yyvsp[0]); }
#line 24000 "parser.c"
    break;

  case 2090:
#line 14056 "parser.y"
                                { yyval = cb_list_add (yyvsp[-2], yyvsp[0]); }
#line 24006 "parser.c"
    break;

  case 2091:
#line 14061 "parser.y"
  {
	cb_tree		x;
	int		data_type_ok = 1;

	if (yyvsp[-5] != cb_error_node
	 && yyvsp[-3] != cb_error_node
	 && yyvsp[-2] != cb_error_node) {

		if (cb_tree_category (yyvsp[-5]) != CB_CATEGORY_NUMERIC) {
			x = cb_ref (yyvsp[-5]);
			cb_error_x (CB_TREE (current_statement),
				_("PERFORM VARYING '%s' (line %d of %s) is not a numeric field"),
				cb_name (x),x->source_line, x->source_file);
			yyval = cb_int1;
			data_type_ok = 0;
		}
		if (cb_tree_category (yyvsp[-3]) != CB_CATEGORY_NUMERIC) {
			x = cb_ref (yyvsp[-3]);
			cb_error_x (CB_TREE (current_statement),
				_("PERFORM VARYING '%s' (line %d of %s) is not a numeric field"),
				cb_name (x),x->source_line, x->source_file);
			yyval = cb_int1;
			data_type_ok = 0;
		}
		if (cb_tree_category (yyvsp[-2]) != CB_CATEGORY_NUMERIC) {
			x = cb_ref (yyvsp[-2]);
			cb_error_x (CB_TREE (current_statement),
				_("PERFORM VARYING '%s' (line %d of %s) is not a numeric field"),
				cb_name (x),x->source_line, x->source_file);
			yyval = cb_int1;
			data_type_ok = 0;
		}

		if (data_type_ok) {
			yyval = cb_build_perform_varying (yyvsp[-5], yyvsp[-3], yyvsp[-2], yyvsp[0]);
		}
	}
  }
#line 24049 "parser.c"
    break;

  case 2092:
#line 14103 "parser.y"
  {
	cb_verify (cb_perform_varying_without_by, _("PERFORM VARYING without BY phrase"));
	yyval = cb_build_numeric_literal (0, "1", 0);
  }
#line 24058 "parser.c"
    break;

  case 2093:
#line 14108 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 24066 "parser.c"
    break;

  case 2094:
#line 14117 "parser.y"
  {
	begin_statement ("PURGE", 0);
  }
#line 24074 "parser.c"
    break;

  case 2095:
#line 14121 "parser.y"
  {
  }
#line 24081 "parser.c"
    break;

  case 2096:
#line 14129 "parser.y"
  {
	begin_statement ("RAISE", 0);
  }
#line 24089 "parser.c"
    break;

  case 2098:
#line 14137 "parser.y"
  {
	CB_PENDING ("RAISE statement");
	/* TODO: check for level 3 error here */
  }
#line 24098 "parser.c"
    break;

  case 2099:
#line 14142 "parser.y"
  {
	/* easy cheating here as we don't have any OO in */
	cb_error(_("'%s' is not an object-reference"), cb_name (yyvsp[0]));
  }
#line 24107 "parser.c"
    break;

  case 2100:
#line 14152 "parser.y"
  {
	/* TODO:
	cb_tree exception = get_exception (CB_NAME($1));
	if (!exception) {
		cb_error (_("'%s' is not an exception-name"), CB_NAME ($1));
	}
	*/
  }
#line 24120 "parser.c"
    break;

  case 2101:
#line 14166 "parser.y"
  {
	begin_statement ("READ", TERM_READ);
	cobc_cs_check = CB_CS_READ;
  }
#line 24129 "parser.c"
    break;

  case 2103:
#line 14176 "parser.y"
  {
	cobc_cs_check = 0;

	if (CB_VALID_TREE (yyvsp[-6])) {
		struct cb_file	*cf;

		cf = CB_FILE(cb_ref (yyvsp[-6]));
		if (yyvsp[-2] && (cf->lock_mode & COB_LOCK_AUTOMATIC)) {
			cb_error_x (CB_TREE (current_statement),
				    _("LOCK clause invalid with file LOCK AUTOMATIC"));
		} else if (yyvsp[-1] &&
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
			cb_emit_read (yyvsp[-6], yyvsp[-5], yyvsp[-3], yyvsp[-1], yyvsp[-2]);
		}
	}
  }
#line 24159 "parser.c"
    break;

  case 2104:
#line 14204 "parser.y"
                                { yyval = NULL; }
#line 24165 "parser.c"
    break;

  case 2105:
#line 14205 "parser.y"
                                { yyval = yyvsp[0]; }
#line 24171 "parser.c"
    break;

  case 2106:
#line 14210 "parser.y"
  {
	yyval = NULL;
  }
#line 24179 "parser.c"
    break;

  case 2107:
#line 14214 "parser.y"
  {
	yyval = cb_int3;
  }
#line 24187 "parser.c"
    break;

  case 2108:
#line 14218 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 24195 "parser.c"
    break;

  case 2109:
#line 14222 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 24203 "parser.c"
    break;

  case 2112:
#line 14234 "parser.y"
  {
	CB_PENDING ("ADVANCING ON LOCK");
  }
#line 24211 "parser.c"
    break;

  case 2116:
#line 14247 "parser.y"
  {
	CB_PENDING ("RETRY");
	cobc_cs_check = 0;
  }
#line 24220 "parser.c"
    break;

  case 2122:
#line 14267 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 24228 "parser.c"
    break;

  case 2123:
#line 14271 "parser.y"
  {
   yyval = cb_int5;
  }
#line 24236 "parser.c"
    break;

  case 2124:
#line 14275 "parser.y"
  {
	/* TO-DO: Merge with RETRY phrase */
	yyval = cb_int4;
  }
#line 24245 "parser.c"
    break;

  case 2125:
#line 14282 "parser.y"
                                { yyval = NULL; }
#line 24251 "parser.c"
    break;

  case 2126:
#line 14283 "parser.y"
                                { yyval = yyvsp[0]; }
#line 24257 "parser.c"
    break;

  case 2129:
#line 14293 "parser.y"
  {
	TERMINATOR_WARNING (yyvsp[(-2) - (0)], READ);
  }
#line 24265 "parser.c"
    break;

  case 2130:
#line 14297 "parser.y"
  {
	TERMINATOR_CLEAR (yyvsp[(-2) - (1)], READ);
  }
#line 24273 "parser.c"
    break;

  case 2131:
#line 14307 "parser.y"
  {
	begin_statement ("READY TRACE", 0);
	cb_emit_ready_trace ();
  }
#line 24282 "parser.c"
    break;

  case 2132:
#line 14317 "parser.y"
  {
	begin_statement ("RECEIVE", TERM_RECEIVE);
  }
#line 24290 "parser.c"
    break;

  case 2146:
#line 14360 "parser.y"
  {
	TERMINATOR_WARNING (yyvsp[(-2) - (0)], RECEIVE);
  }
#line 24298 "parser.c"
    break;

  case 2147:
#line 14364 "parser.y"
  {
	TERMINATOR_CLEAR (yyvsp[(-2) - (1)], RECEIVE);
  }
#line 24306 "parser.c"
    break;

  case 2148:
#line 14373 "parser.y"
  {
	begin_statement ("RELEASE", 0);
  }
#line 24314 "parser.c"
    break;

  case 2150:
#line 14381 "parser.y"
  {
	cb_emit_release (yyvsp[-1], yyvsp[0]);
  }
#line 24322 "parser.c"
    break;

  case 2151:
#line 14391 "parser.y"
  {
	begin_statement ("RESET TRACE", 0);
	cb_emit_reset_trace ();
  }
#line 24331 "parser.c"
    break;

  case 2152:
#line 14401 "parser.y"
  {
	begin_statement ("RETURN", TERM_RETURN);
  }
#line 24339 "parser.c"
    break;

  case 2154:
#line 14410 "parser.y"
  {
	cb_emit_return (yyvsp[-3], yyvsp[-1]);
  }
#line 24347 "parser.c"
    break;

  case 2155:
#line 14417 "parser.y"
  {
	TERMINATOR_WARNING (yyvsp[(-2) - (0)], RETURN);
  }
#line 24355 "parser.c"
    break;

  case 2156:
#line 14421 "parser.y"
  {
	TERMINATOR_CLEAR (yyvsp[(-2) - (1)], RETURN);
  }
#line 24363 "parser.c"
    break;

  case 2157:
#line 14431 "parser.y"
  {
	begin_statement ("REWRITE", TERM_REWRITE);
	/* Special in debugging mode */
	save_debug = start_debug;
	start_debug = 0;
  }
#line 24374 "parser.c"
    break;

  case 2159:
#line 14443 "parser.y"
  {
	cb_emit_rewrite (yyvsp[-4], yyvsp[-3], yyvsp[-1]);
	start_debug = save_debug;
  }
#line 24383 "parser.c"
    break;

  case 2160:
#line 14451 "parser.y"
  {
	yyval = NULL;
  }
#line 24391 "parser.c"
    break;

  case 2162:
#line 14459 "parser.y"
  {
	yyval = cb_int1;
  }
#line 24399 "parser.c"
    break;

  case 2163:
#line 14463 "parser.y"
  {
	yyval = cb_int2;
  }
#line 24407 "parser.c"
    break;

  case 2164:
#line 14470 "parser.y"
  {
	TERMINATOR_WARNING (yyvsp[(-2) - (0)], REWRITE);
  }
#line 24415 "parser.c"
    break;

  case 2165:
#line 14474 "parser.y"
  {
	TERMINATOR_CLEAR (yyvsp[(-2) - (1)], REWRITE);
  }
#line 24423 "parser.c"
    break;

  case 2166:
#line 14484 "parser.y"
  {
	begin_statement ("ROLLBACK", 0);
	cb_emit_rollback ();
  }
#line 24432 "parser.c"
    break;

  case 2167:
#line 14495 "parser.y"
  {
	begin_statement ("SEARCH", TERM_SEARCH);
  }
#line 24440 "parser.c"
    break;

  case 2169:
#line 14504 "parser.y"
  {
	cb_emit_search (yyvsp[-3], yyvsp[-2], yyvsp[-1], yyvsp[0]);
  }
#line 24448 "parser.c"
    break;

  case 2170:
#line 14509 "parser.y"
  {
	current_statement->name = (const char *)"SEARCH ALL";
	cb_emit_search_all (yyvsp[-4], yyvsp[-3], yyvsp[-1], yyvsp[0]);
  }
#line 24457 "parser.c"
    break;

  case 2171:
#line 14516 "parser.y"
                                { yyval = NULL; }
#line 24463 "parser.c"
    break;

  case 2172:
#line 14517 "parser.y"
                                { yyval = yyvsp[0]; }
#line 24469 "parser.c"
    break;

  case 2173:
#line 14522 "parser.y"
  {
	yyval = NULL;
  }
#line 24477 "parser.c"
    break;

  case 2174:
#line 14527 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 24485 "parser.c"
    break;

  case 2175:
#line 14534 "parser.y"
  {
	yyval = CB_LIST_INIT (yyvsp[0]);
  }
#line 24493 "parser.c"
    break;

  case 2176:
#line 14538 "parser.y"
  {
	yyval = cb_list_add (yyvsp[0], yyvsp[-1]);
  }
#line 24501 "parser.c"
    break;

  case 2177:
#line 14546 "parser.y"
  {
	yyval = cb_build_if_check_break (yyvsp[-1], yyvsp[0]);
  }
#line 24509 "parser.c"
    break;

  case 2178:
#line 14553 "parser.y"
  {
	TERMINATOR_WARNING (yyvsp[(-2) - (0)], SEARCH);
  }
#line 24517 "parser.c"
    break;

  case 2179:
#line 14557 "parser.y"
  {
	TERMINATOR_CLEAR (yyvsp[(-2) - (1)], SEARCH);
  }
#line 24525 "parser.c"
    break;

  case 2180:
#line 14567 "parser.y"
  {
	begin_statement ("SEND", 0);
  }
#line 24533 "parser.c"
    break;

  case 2182:
#line 14575 "parser.y"
  {
  }
#line 24540 "parser.c"
    break;

  case 2183:
#line 14578 "parser.y"
  {
  }
#line 24547 "parser.c"
    break;

  case 2186:
#line 14589 "parser.y"
  {
  }
#line 24554 "parser.c"
    break;

  case 2193:
#line 14609 "parser.y"
  {
	begin_statement ("SET", 0);
	set_attr_val_on = 0;
	set_attr_val_off = 0;
	cobc_cs_check = CB_CS_SET;
  }
#line 24565 "parser.c"
    break;

  case 2194:
#line 14616 "parser.y"
  {
	cobc_cs_check = 0;
  }
#line 24573 "parser.c"
    break;

  case 2203:
#line 14633 "parser.y"
                                { yyval = cb_int1; }
#line 24579 "parser.c"
    break;

  case 2204:
#line 14634 "parser.y"
                                { yyval = cb_int0; }
#line 24585 "parser.c"
    break;

  case 2205:
#line 14638 "parser.y"
                                { yyval = cb_int0; }
#line 24591 "parser.c"
    break;

  case 2206:
#line 14639 "parser.y"
                                { yyval = cb_int1; }
#line 24597 "parser.c"
    break;

  case 2207:
#line 14646 "parser.y"
  {
	cb_emit_setenv (yyvsp[-2], yyvsp[0]);
  }
#line 24605 "parser.c"
    break;

  case 2208:
#line 14655 "parser.y"
  {
	cb_emit_set_attribute (yyvsp[-2], set_attr_val_on, set_attr_val_off);
  }
#line 24613 "parser.c"
    break;

  case 2211:
#line 14667 "parser.y"
  {
	bit_set_attr (yyvsp[0], COB_SCREEN_BELL);
  }
#line 24621 "parser.c"
    break;

  case 2212:
#line 14671 "parser.y"
  {
	bit_set_attr (yyvsp[0], COB_SCREEN_BLINK);
  }
#line 24629 "parser.c"
    break;

  case 2213:
#line 14675 "parser.y"
  {
	bit_set_attr (yyvsp[0], COB_SCREEN_HIGHLIGHT);
	check_not_highlight_and_lowlight (set_attr_val_on | set_attr_val_off,
					  COB_SCREEN_HIGHLIGHT);
  }
#line 24639 "parser.c"
    break;

  case 2214:
#line 14681 "parser.y"
  {
	bit_set_attr (yyvsp[0], COB_SCREEN_LOWLIGHT);
	check_not_highlight_and_lowlight (set_attr_val_on | set_attr_val_off,
					  COB_SCREEN_LOWLIGHT);
  }
#line 24649 "parser.c"
    break;

  case 2215:
#line 14687 "parser.y"
  {
	bit_set_attr (yyvsp[0], COB_SCREEN_REVERSE);
  }
#line 24657 "parser.c"
    break;

  case 2216:
#line 14691 "parser.y"
  {
	bit_set_attr (yyvsp[0], COB_SCREEN_UNDERLINE);
  }
#line 24665 "parser.c"
    break;

  case 2217:
#line 14695 "parser.y"
  {
	bit_set_attr (yyvsp[0], COB_SCREEN_LEFTLINE);
  }
#line 24673 "parser.c"
    break;

  case 2218:
#line 14699 "parser.y"
  {
	bit_set_attr (yyvsp[0], COB_SCREEN_OVERLINE);
  }
#line 24681 "parser.c"
    break;

  case 2219:
#line 14708 "parser.y"
  {
	cb_emit_set_to (yyvsp[-3], cb_build_ppointer (yyvsp[0]));
  }
#line 24689 "parser.c"
    break;

  case 2220:
#line 14712 "parser.y"
  {
	cb_emit_set_to (yyvsp[-2], yyvsp[0]);
  }
#line 24697 "parser.c"
    break;

  case 2221:
#line 14716 "parser.y"
  {
	cb_emit_move (cb_build_length (yyvsp[0]), yyvsp[-4]);
  }
#line 24705 "parser.c"
    break;

  case 2222:
#line 14725 "parser.y"
  {
	cb_emit_set_up_down (yyvsp[-3], yyvsp[-2], yyvsp[0]);
  }
#line 24713 "parser.c"
    break;

  case 2225:
#line 14739 "parser.y"
  {
	cb_emit_set_on_off (yyvsp[-2], yyvsp[0]);
  }
#line 24721 "parser.c"
    break;

  case 2228:
#line 14753 "parser.y"
  {
	cb_emit_set_true (yyvsp[-2]);
  }
#line 24729 "parser.c"
    break;

  case 2229:
#line 14757 "parser.y"
  {
	cb_emit_set_false (yyvsp[-2]);
  }
#line 24737 "parser.c"
    break;

  case 2230:
#line 14766 "parser.y"
  {
	  cb_emit_set_last_exception_to_off ();
  }
#line 24745 "parser.c"
    break;

  case 2231:
#line 14775 "parser.y"
  {
	cb_emit_set_thread_priority (yyvsp[-3], yyvsp[0]);
	CB_PENDING ("THREAD");
  }
#line 24754 "parser.c"
    break;

  case 2232:
#line 14786 "parser.y"
  {
	begin_statement ("SORT", 0);
  }
#line 24762 "parser.c"
    break;

  case 2234:
#line 14794 "parser.y"
  {
	cb_tree		x = cb_ref (yyvsp[-3]);

	yyval = NULL;
	if (CB_VALID_TREE (x)) {
		if (yyvsp[-2] == NULL || CB_VALUE(yyvsp[-2]) == NULL) {
			if (CB_FILE_P (x)) {
				cb_error (_("file sort requires KEY phrase"));
				yyvsp[-2] = cb_error_node;
			} else {
				struct cb_field	*f = CB_FIELD_PTR (x);
/* TODO: add compiler configuration cb_sort_without_keys
				if (f->nkeys
				 && cb_verify (cb_sort_without_keys, _("table SORT without keys"))) {
*/
				if (yyvsp[-2] != NULL || f->nkeys) {
					cb_tree lparm;
					if (yyvsp[-2] == NULL) {
						/* create reference to first key */
						x = cb_ref (f->keys[0].key);
					}
					/* use the OCCURS field / its defined KEY as single sort key */
					lparm = cb_list_add (NULL, x);
					/* search order is either specified, otherwise derived from definition */
					if (yyvsp[-2] != NULL) {
						CB_PURPOSE (lparm) = CB_PURPOSE (yyvsp[-2]);
					} else {
						CB_PURPOSE (lparm) = cb_int (f->keys[0].dir);
					}
					yyvsp[-2] = cb_list_append (NULL, lparm);
				} else {
					cb_error (_("table SORT requires KEY phrase"));
					yyvsp[-2] = cb_error_node;
				}
			}
		}
		if (CB_VALID_TREE (yyvsp[-2])) {
			cb_emit_sort_init (yyvsp[-3], yyvsp[-2], alphanumeric_collation, national_collation);
			yyval = yyvsp[-3];
		}
	}
  }
#line 24809 "parser.c"
    break;

  case 2235:
#line 14837 "parser.y"
  {
	if (yyvsp[-2] && CB_VALID_TREE (yyvsp[-6])) {
		cb_emit_sort_finish (yyvsp[-6]);
	}
  }
#line 24819 "parser.c"
    break;

  case 2236:
#line 14845 "parser.y"
                                { yyval = NULL; }
#line 24825 "parser.c"
    break;

  case 2237:
#line 14848 "parser.y"
  {
	cb_tree lparm = yyvsp[0];
	cb_tree l;

	if (lparm == NULL) {
		lparm = CB_LIST_INIT (NULL);
	}
	for (l = lparm; l; l = CB_CHAIN (l)) {
		CB_PURPOSE (l) = yyvsp[-2];
	}
	yyval = cb_list_append (yyvsp[-4], lparm);
  }
#line 24842 "parser.c"
    break;

  case 2238:
#line 14863 "parser.y"
                                { yyval = NULL; }
#line 24848 "parser.c"
    break;

  case 2239:
#line 14864 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 24854 "parser.c"
    break;

  case 2241:
#line 14869 "parser.y"
  {
	/* The GnuCOBOL sort is a stable sort. ie. dups are per default in order */
	/* Therefore nothing to do here */
  }
#line 24863 "parser.c"
    break;

  case 2242:
#line 14877 "parser.y"
  {
	alphanumeric_collation = national_collation = NULL;
  }
#line 24871 "parser.c"
    break;

  case 2244:
#line 14885 "parser.y"
  {
	if (yyvsp[0] && CB_FILE_P (cb_ref (yyvsp[0]))) {
		cb_error (_("file sort requires USING or INPUT PROCEDURE"));
	}
  }
#line 24881 "parser.c"
    break;

  case 2245:
#line 14891 "parser.y"
  {
	if (yyvsp[-2]) {
		if (!CB_FILE_P (cb_ref (yyvsp[-2]))) {
			cb_error (_("USING invalid with table SORT"));
		} else {
			cb_emit_sort_using (yyvsp[-2], yyvsp[0]);
		}
	}
  }
#line 24895 "parser.c"
    break;

  case 2246:
#line 14901 "parser.y"
  {
	if (yyvsp[-4]) {
		if (!CB_FILE_P (cb_ref (yyvsp[-4]))) {
			cb_error (_("INPUT PROCEDURE invalid with table SORT"));
		} else if (current_statement->flag_merge) {
			cb_error (_("INPUT PROCEDURE invalid with MERGE"));
		} else {
			cb_emit_sort_input (yyvsp[0]);
		}
	}
	cobc_cs_check = 0;
  }
#line 24912 "parser.c"
    break;

  case 2247:
#line 14917 "parser.y"
  {
	if (yyvsp[(-1) - (0)] && CB_FILE_P (cb_ref (yyvsp[(-1) - (0)]))) {
		cb_error (_("file sort requires GIVING or OUTPUT PROCEDURE"));
	}
  }
#line 24922 "parser.c"
    break;

  case 2248:
#line 14923 "parser.y"
  {
	if (yyvsp[(-1) - (2)]) {
		if (!CB_FILE_P (cb_ref (yyvsp[(-1) - (2)]))) {
			cb_error (_("GIVING invalid with table SORT"));
		} else {
			cb_emit_sort_giving (yyvsp[(-1) - (2)], yyvsp[0]);
		}
	}
  }
#line 24936 "parser.c"
    break;

  case 2249:
#line 14933 "parser.y"
  {
	if (yyvsp[(-1) - (4)]) {
		if (!CB_FILE_P (cb_ref (yyvsp[(-1) - (4)]))) {
			cb_error (_("OUTPUT PROCEDURE invalid with table SORT"));
		} else {
			cb_emit_sort_output (yyvsp[0]);
		}
	}
	cobc_cs_check = 0;
  }
#line 24951 "parser.c"
    break;

  case 2250:
#line 14950 "parser.y"
  {
	begin_statement ("START", TERM_START);
	start_tree = cb_int (COB_EQ);
  }
#line 24960 "parser.c"
    break;

  case 2252:
#line 14960 "parser.y"
  {
	if (yyvsp[-1] && !yyvsp[-2]) {
		cb_error_x (CB_TREE (current_statement),
			    _("SIZE/LENGTH invalid here"));
	} else {
		cb_emit_start (yyvsp[-3], start_tree, yyvsp[-2], yyvsp[-1]);
	}
  }
#line 24973 "parser.c"
    break;

  case 2253:
#line 14972 "parser.y"
  {
	yyval = NULL;
  }
#line 24981 "parser.c"
    break;

  case 2254:
#line 14976 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 24989 "parser.c"
    break;

  case 2255:
#line 14983 "parser.y"
  {
	yyval = NULL;
  }
#line 24997 "parser.c"
    break;

  case 2256:
#line 14987 "parser.y"
  {
	start_tree = yyvsp[-1];
	yyval = yyvsp[0];
  }
#line 25006 "parser.c"
    break;

  case 2257:
#line 14992 "parser.y"
  {
	start_tree = cb_int (COB_FI);
	yyval = NULL;
  }
#line 25015 "parser.c"
    break;

  case 2258:
#line 14997 "parser.y"
  {
	start_tree = cb_int (COB_LA);
	yyval = NULL;
  }
#line 25024 "parser.c"
    break;

  case 2259:
#line 15004 "parser.y"
                        { yyval = cb_int (COB_EQ); }
#line 25030 "parser.c"
    break;

  case 2260:
#line 15005 "parser.y"
                        { yyval = cb_int (yyvsp[-1] ? COB_LE : COB_GT); }
#line 25036 "parser.c"
    break;

  case 2261:
#line 15006 "parser.y"
                        { yyval = cb_int (yyvsp[-1] ? COB_GE : COB_LT); }
#line 25042 "parser.c"
    break;

  case 2262:
#line 15007 "parser.y"
                        { yyval = cb_int (yyvsp[-1] ? COB_LT : COB_GE); }
#line 25048 "parser.c"
    break;

  case 2263:
#line 15008 "parser.y"
                        { yyval = cb_int (yyvsp[-1] ? COB_GT : COB_LE); }
#line 25054 "parser.c"
    break;

  case 2264:
#line 15009 "parser.y"
                        { yyval = cb_int (COB_NE); }
#line 25060 "parser.c"
    break;

  case 2265:
#line 15014 "parser.y"
  {
	cb_error_x (CB_TREE (current_statement),
		    _("NOT EQUAL condition not allowed on START statement"));
  }
#line 25069 "parser.c"
    break;

  case 2268:
#line 15027 "parser.y"
  {
	TERMINATOR_WARNING (yyvsp[(-2) - (0)], START);
  }
#line 25077 "parser.c"
    break;

  case 2269:
#line 15031 "parser.y"
  {
	TERMINATOR_CLEAR (yyvsp[(-2) - (1)], START);
  }
#line 25085 "parser.c"
    break;

  case 2270:
#line 15041 "parser.y"
  {
	begin_statement ("STOP RUN", 0);
	cobc_cs_check = CB_CS_STOP;
  }
#line 25094 "parser.c"
    break;

  case 2271:
#line 15046 "parser.y"
  {
	cb_emit_stop_run (yyvsp[0]);
	check_unreached = 1;
	cobc_cs_check = 0;
  }
#line 25104 "parser.c"
    break;

  case 2272:
#line 15052 "parser.y"
  {
	begin_statement ("STOP", 0);
	cb_emit_display (CB_LIST_INIT (yyvsp[0]), cb_int0, cb_int1, NULL,
			 NULL, 1, DEVICE_DISPLAY);
	cb_emit_accept (cb_null, NULL, NULL);
	cobc_cs_check = 0;
  }
#line 25116 "parser.c"
    break;

  case 2273:
#line 15060 "parser.y"
  {
	begin_statement ("STOP THREAD", 0);
	cb_emit_stop_thread (yyvsp[0]);
	cobc_cs_check = 0;
	cb_warning_x (COBC_WARN_FILLER, yyvsp[0], _("%s is replaced by %s"), "STOP THREAD", "STOP RUN");
  }
#line 25127 "parser.c"
    break;

  case 2274:
#line 15070 "parser.y"
  {
	if (current_program->cb_return_code) {
		yyval = current_program->cb_return_code;
	} else {
		yyval = cb_int0;
	}
  }
#line 25139 "parser.c"
    break;

  case 2275:
#line 15078 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 25147 "parser.c"
    break;

  case 2276:
#line 15082 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 25155 "parser.c"
    break;

  case 2277:
#line 15086 "parser.y"
  {
	if (yyvsp[0]) {
		yyval = yyvsp[0];
	} else {
		yyval = cb_int1;
	}
  }
#line 25167 "parser.c"
    break;

  case 2278:
#line 15094 "parser.y"
  {
	if (yyvsp[0]) {
		yyval = yyvsp[0];
	} else {
		yyval = cb_int0;
	}
  }
#line 25179 "parser.c"
    break;

  case 2279:
#line 15105 "parser.y"
  {
	yyval = NULL;
  }
#line 25187 "parser.c"
    break;

  case 2280:
#line 15109 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 25195 "parser.c"
    break;

  case 2281:
#line 15116 "parser.y"
  {
	cb_verify (cb_stop_literal_statement, _("STOP literal"));
  }
#line 25203 "parser.c"
    break;

  case 2282:
#line 15120 "parser.y"
  {
	cb_verify (cb_stop_identifier_statement, _("STOP identifier"));
  }
#line 25211 "parser.c"
    break;

  case 2283:
#line 15126 "parser.y"
                                { yyval = yyvsp[0]; }
#line 25217 "parser.c"
    break;

  case 2284:
#line 15127 "parser.y"
                                { yyval = cb_space; }
#line 25223 "parser.c"
    break;

  case 2285:
#line 15128 "parser.y"
                                { yyval = cb_zero; }
#line 25229 "parser.c"
    break;

  case 2286:
#line 15129 "parser.y"
                                { yyval = cb_quote; }
#line 25235 "parser.c"
    break;

  case 2287:
#line 15136 "parser.y"
  {
	begin_statement ("STRING", TERM_STRING);
  }
#line 25243 "parser.c"
    break;

  case 2289:
#line 15145 "parser.y"
  {
	cb_emit_string (yyvsp[-4], yyvsp[-2], yyvsp[-1]);
  }
#line 25251 "parser.c"
    break;

  case 2290:
#line 15151 "parser.y"
  {
	save_tree = NULL;
  }
#line 25259 "parser.c"
    break;

  case 2291:
#line 15155 "parser.y"
  {
	yyval = save_tree;
  }
#line 25267 "parser.c"
    break;

  case 2294:
#line 15167 "parser.y"
  {
	if (!save_tree) {
		save_tree = CB_LIST_INIT (yyvsp[-1]);
	} else {
		save_tree = cb_list_add (save_tree, yyvsp[-1]);
	}
	if (yyvsp[0]) {
		save_tree = cb_list_add (save_tree, yyvsp[0]);
	}
  }
#line 25282 "parser.c"
    break;

  case 2295:
#line 15180 "parser.y"
                        { yyval = NULL; }
#line 25288 "parser.c"
    break;

  case 2296:
#line 15182 "parser.y"
                        { yyval = yyvsp[0]; }
#line 25294 "parser.c"
    break;

  case 2297:
#line 15186 "parser.y"
                { yyval = CB_BUILD_PAIR (cb_int0, NULL); }
#line 25300 "parser.c"
    break;

  case 2298:
#line 15187 "parser.y"
                { yyval = CB_BUILD_PAIR (yyvsp[0], NULL); }
#line 25306 "parser.c"
    break;

  case 2299:
#line 15191 "parser.y"
                                { yyval = NULL; }
#line 25312 "parser.c"
    break;

  case 2300:
#line 15192 "parser.y"
                                { yyval = yyvsp[0]; }
#line 25318 "parser.c"
    break;

  case 2301:
#line 15197 "parser.y"
  {
	TERMINATOR_WARNING (yyvsp[(-2) - (0)], STRING);
  }
#line 25326 "parser.c"
    break;

  case 2302:
#line 15201 "parser.y"
  {
	TERMINATOR_CLEAR (yyvsp[(-2) - (1)], STRING);
  }
#line 25334 "parser.c"
    break;

  case 2303:
#line 15211 "parser.y"
  {
	begin_statement ("SUBTRACT", TERM_SUBTRACT);
  }
#line 25342 "parser.c"
    break;

  case 2305:
#line 15220 "parser.y"
  {
	cb_emit_arithmetic (yyvsp[-1], '-', cb_build_binary_list (yyvsp[-3], '+'));
  }
#line 25350 "parser.c"
    break;

  case 2306:
#line 15224 "parser.y"
  {
	cb_emit_arithmetic (yyvsp[-1], 0, cb_build_binary_list (CB_BUILD_CHAIN (yyvsp[-3], yyvsp[-5]), '-'));
  }
#line 25358 "parser.c"
    break;

  case 2307:
#line 15228 "parser.y"
  {
	cb_emit_corresponding (cb_build_sub, yyvsp[-2], yyvsp[-4], yyvsp[-1]);
  }
#line 25366 "parser.c"
    break;

  case 2308:
#line 15232 "parser.y"
  {
	CB_PENDING ("SUBTRACT TABLE");
	cb_emit_tab_arithmetic (cb_build_sub, yyvsp[-4], yyvsp[-6], yyvsp[-3], yyvsp[-2], yyvsp[-1]);
  }
#line 25375 "parser.c"
    break;

  case 2309:
#line 15240 "parser.y"
  {
	TERMINATOR_WARNING (yyvsp[(-2) - (0)], SUBTRACT);
  }
#line 25383 "parser.c"
    break;

  case 2310:
#line 15244 "parser.y"
  {
	TERMINATOR_CLEAR (yyvsp[(-2) - (1)], SUBTRACT);
  }
#line 25391 "parser.c"
    break;

  case 2311:
#line 15254 "parser.y"
  {
	begin_statement ("SUPPRESS", 0);
	if (!in_declaratives) {
		cb_error_x (CB_TREE (current_statement),
			    _("SUPPRESS statement must be within DECLARATIVES"));
	}
	cb_emit_suppress (control_field);
  }
#line 25404 "parser.c"
    break;

  case 2314:
#line 15272 "parser.y"
  {
	begin_statement ("TERMINATE", 0);
  }
#line 25412 "parser.c"
    break;

  case 2316:
#line 15280 "parser.y"
  {
#if 0 /* CHECKME: likely not needed */
	begin_implicit_statement ();
#endif
	if (yyvsp[0] != cb_error_node) {
	    cb_emit_terminate (yyvsp[0]);
	}
  }
#line 25425 "parser.c"
    break;

  case 2317:
#line 15289 "parser.y"
  {
	begin_implicit_statement ();
	if (yyvsp[0] != cb_error_node) {
		cb_emit_terminate (yyvsp[0]);
	}
  }
#line 25436 "parser.c"
    break;

  case 2318:
#line 15301 "parser.y"
  {
	begin_statement ("TRANSFORM", 0);
  }
#line 25444 "parser.c"
    break;

  case 2320:
#line 15309 "parser.y"
  {
	cb_tree		x;

	x = cb_build_converting (yyvsp[-2], yyvsp[0], cb_build_inspect_region_start ());
	cb_emit_inspect (yyvsp[-4], x, TRANSFORM_STATEMENT);
  }
#line 25455 "parser.c"
    break;

  case 2321:
#line 15322 "parser.y"
  {
	begin_statement ("UNLOCK", 0);
  }
#line 25463 "parser.c"
    break;

  case 2323:
#line 15330 "parser.y"
  {
	if (CB_VALID_TREE (yyvsp[-1])) {
		if (CB_FILE (cb_ref (yyvsp[-1]))->organization == COB_ORG_SORT) {
			cb_error_x (CB_TREE (current_statement),
				    _("UNLOCK invalid for SORT files"));
		} else {
			cb_emit_unlock (yyvsp[-1]);
		}
	}
  }
#line 25478 "parser.c"
    break;

  case 2324:
#line 15346 "parser.y"
  {
	begin_statement ("UNSTRING", TERM_UNSTRING);
  }
#line 25486 "parser.c"
    break;

  case 2326:
#line 15357 "parser.y"
  {
	cb_emit_unstring (yyvsp[-5], yyvsp[-4], yyvsp[-3], yyvsp[-2], yyvsp[-1]);
  }
#line 25494 "parser.c"
    break;

  case 2327:
#line 15363 "parser.y"
                                { yyval = NULL; }
#line 25500 "parser.c"
    break;

  case 2328:
#line 15365 "parser.y"
                                { yyval = yyvsp[0]; }
#line 25506 "parser.c"
    break;

  case 2329:
#line 15369 "parser.y"
                                { yyval = CB_LIST_INIT (yyvsp[0]); }
#line 25512 "parser.c"
    break;

  case 2330:
#line 15371 "parser.y"
                                { yyval = cb_list_add (yyvsp[-2], yyvsp[0]); }
#line 25518 "parser.c"
    break;

  case 2331:
#line 15376 "parser.y"
  {
	yyval = cb_build_unstring_delimited (yyvsp[-1], yyvsp[0]);
  }
#line 25526 "parser.c"
    break;

  case 2332:
#line 15382 "parser.y"
                                { yyval = CB_LIST_INIT (yyvsp[0]); }
#line 25532 "parser.c"
    break;

  case 2333:
#line 15384 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 25538 "parser.c"
    break;

  case 2334:
#line 15389 "parser.y"
  {
	yyval = cb_build_unstring_into (yyvsp[-2], yyvsp[-1], yyvsp[0]);
  }
#line 25546 "parser.c"
    break;

  case 2335:
#line 15395 "parser.y"
                                { yyval = NULL; }
#line 25552 "parser.c"
    break;

  case 2336:
#line 15396 "parser.y"
                                { yyval = yyvsp[0]; }
#line 25558 "parser.c"
    break;

  case 2337:
#line 15400 "parser.y"
                                { yyval = NULL; }
#line 25564 "parser.c"
    break;

  case 2338:
#line 15401 "parser.y"
                                { yyval = yyvsp[0]; }
#line 25570 "parser.c"
    break;

  case 2339:
#line 15406 "parser.y"
  {
	TERMINATOR_WARNING (yyvsp[(-2) - (0)], UNSTRING);
  }
#line 25578 "parser.c"
    break;

  case 2340:
#line 15410 "parser.y"
  {
	TERMINATOR_CLEAR (yyvsp[(-2) - (1)], UNSTRING);
  }
#line 25586 "parser.c"
    break;

  case 2341:
#line 15419 "parser.y"
  {
	begin_statement ("VALIDATE", 0);
  }
#line 25594 "parser.c"
    break;

  case 2342:
#line 15423 "parser.y"
  {
#if 0	/* FIXME: at least add syntax checks here */
	cb_emit_validate (yyvsp[0]);
#else
	CB_PENDING ("VALIDATE");
#endif
  }
#line 25606 "parser.c"
    break;

  case 2343:
#line 15434 "parser.y"
  {
	check_validate_item (yyvsp[0]);
	yyval = CB_LIST_INIT (yyvsp[0]);
  }
#line 25615 "parser.c"
    break;

  case 2344:
#line 15439 "parser.y"
  {
	check_validate_item (yyvsp[0]);
	yyval = cb_list_add (yyvsp[-1], yyvsp[0]);
  }
#line 25624 "parser.c"
    break;

  case 2345:
#line 15450 "parser.y"
  {
	skip_statements = 0;
	in_debugging = 0;
  }
#line 25633 "parser.c"
    break;

  case 2352:
#line 15468 "parser.y"
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
		/* TO-DO: Use cobc_ec_turn? */
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
#line 25660 "parser.c"
    break;

  case 2353:
#line 15494 "parser.y"
  {
	use_global_ind = 0;
  }
#line 25668 "parser.c"
    break;

  case 2354:
#line 15498 "parser.y"
  {
	if (current_program->prog_type == COB_MODULE_TYPE_FUNCTION) {
		cb_error (_("%s is invalid in a user FUNCTION"), "GLOBAL");
	} else {
		use_global_ind = 1;
		current_program->flag_global_use = 1;
	}
  }
#line 25681 "parser.c"
    break;

  case 2355:
#line 15510 "parser.y"
  {
	cb_tree		l;

	for (l = yyvsp[0]; l; l = CB_CHAIN (l)) {
		if (CB_VALID_TREE (CB_VALUE (l))) {
			setup_use_file (CB_FILE (cb_ref (CB_VALUE (l))));
		}
	}
  }
#line 25695 "parser.c"
    break;

  case 2356:
#line 15520 "parser.y"
  {
	current_program->global_handler[COB_OPEN_INPUT].handler_label = current_section;
	current_program->global_handler[COB_OPEN_INPUT].handler_prog = current_program;
  }
#line 25704 "parser.c"
    break;

  case 2357:
#line 15525 "parser.y"
  {
	current_program->global_handler[COB_OPEN_OUTPUT].handler_label = current_section;
	current_program->global_handler[COB_OPEN_OUTPUT].handler_prog = current_program;
  }
#line 25713 "parser.c"
    break;

  case 2358:
#line 15530 "parser.y"
  {
	current_program->global_handler[COB_OPEN_I_O].handler_label = current_section;
	current_program->global_handler[COB_OPEN_I_O].handler_prog = current_program;
  }
#line 25722 "parser.c"
    break;

  case 2359:
#line 15535 "parser.y"
  {
	current_program->global_handler[COB_OPEN_EXTEND].handler_label = current_section;
	current_program->global_handler[COB_OPEN_EXTEND].handler_prog = current_program;
  }
#line 25731 "parser.c"
    break;

  case 2360:
#line 15543 "parser.y"
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
#line 25772 "parser.c"
    break;

  case 2363:
#line 15588 "parser.y"
  {
	if (current_program->flag_debugging) {

		cb_tree		z = CB_LIST_INIT (yyvsp[0]);
		current_program->debug_list =
			cb_list_append (current_program->debug_list, z);
		/* Check backward refs to file/data names */
		if (CB_WORD_COUNT (yyvsp[0]) > 0) {
			cb_tree		l = CB_VALUE (CB_WORD_ITEMS (yyvsp[0]));
			switch (CB_TREE_TAG (l)) {
			case CB_TAG_CD:
				if (CB_CD (l)->flag_field_debug) {
					cb_error_x (yyvsp[0], _("duplicate DEBUGGING target: '%s'"),
					    cb_name (l));
				} else {
					CB_CD (l)->debug_section = current_section;
					CB_CD (l)->flag_field_debug = 1;
				}
				break;
			case CB_TAG_FILE:
				if (CB_FILE (l)->flag_fl_debug) {
					cb_error_x (yyvsp[0], _("duplicate DEBUGGING target: '%s'"),
					    cb_name (l));
				} else {
					CB_FILE (l)->debug_section = current_section;
					CB_FILE (l)->flag_fl_debug = 1;
				}
				break;
			case CB_TAG_FIELD:
				{
					struct cb_field* fld;
					cb_tree		x = cb_ref (yyvsp[0]);
					if (!x || !CB_FIELD_P (x)) {
						break;
					}
					fld = CB_FIELD (x);
					if (fld->flag_item_78) {
						cb_error_x (yyvsp[0], _("constant item cannot be used here"));
					} else if (fld->flag_field_debug) {
						cb_error_x (yyvsp[0], _("duplicate DEBUGGING target: '%s'"),
							cb_name (x));
					} else {
						needs_field_debug = 1;
						fld->debug_section = current_section;
						fld->flag_field_debug = 1;
						CB_PURPOSE (z) = x;
					}
				}
				break;
			default:
				/* Label refs will be checked later (forward/backward ref) */
				break;
			}
		}
		CB_REFERENCE (yyvsp[0])->debug_section = current_section;
		CB_REFERENCE (yyvsp[0])->flag_debug_code = 1;
		CB_REFERENCE (yyvsp[0])->flag_all_debug = 0;
	}
  }
#line 25836 "parser.c"
    break;

  case 2364:
#line 15648 "parser.y"
  {
	if (current_program->flag_debugging) {
		if (current_program->all_procedure) {
			cb_error (_("duplicate USE DEBUGGING ON ALL PROCEDURES"));
		} else {
			current_program->all_procedure = current_section;
		}
	}
  }
#line 25850 "parser.c"
    break;

  case 2365:
#line 15658 "parser.y"
  {
	if (current_program->flag_debugging && yyvsp[0] != cb_error_node) {
		cb_tree x = cb_ref (yyvsp[0]);
		struct cb_field *fld = CB_FIELD (x);
		if (fld->flag_field_debug) {
			cb_error_x (yyvsp[0], _("duplicate DEBUGGING target: '%s'"),
				cb_name (x));
		} else {
			struct cb_reference *r = CB_REFERENCE (yyvsp[0]);
			needs_field_debug = 1;
			fld->debug_section = current_section;
			fld->flag_field_debug = 1;
			fld->flag_all_debug = 1;
			r->debug_section = current_section;
			r->flag_debug_code = 1;
			r->flag_all_debug = 1;
			CB_CHAIN_PAIR (current_program->debug_list, x, yyvsp[0]);
		}
	}
  }
#line 25875 "parser.c"
    break;

  case 2370:
#line 15688 "parser.y"
  {
	if (current_program->nested_level) {
		cb_error (_("%s is invalid in nested program"), "USE AT");
	}
  }
#line 25885 "parser.c"
    break;

  case 2371:
#line 15697 "parser.y"
  {
	emit_statement (cb_build_comment ("USE AT PROGRAM START"));
	backup_current_pos ();
	CB_PENDING ("USE AT PROGRAM START");
	/* emit_entry ("_AT_START", 0, NULL, NULL); */
  }
#line 25896 "parser.c"
    break;

  case 2372:
#line 15704 "parser.y"
  {
	emit_statement (cb_build_comment ("USE AT PROGRAM END"));
	backup_current_pos ();
	CB_PENDING ("USE AT PROGRAM END");
	/* emit_entry ("_AT_END", 0, NULL, NULL); */
  }
#line 25907 "parser.c"
    break;

  case 2373:
#line 15715 "parser.y"
  {
	current_section->flag_real_label = 1;
	current_section->flag_declaratives = 1;
	current_section->flag_begin = 1;
	current_section->flag_return = 1;
	current_section->flag_declarative_exit = 1;
	current_section->flag_real_label = 1;
	current_section->flag_skip_label = 0;

	if (yyvsp[0] != cb_error_node) {
		char	wrk[COB_MINI_BUFF];
		struct cb_field		*f = CB_FIELD_PTR(yyvsp[0]);
		control_field = f;
		f->report_decl_id = current_section->id;
		if (f->report != NULL) {
			f->report->has_declarative = 1;
		}
		snprintf (wrk, COB_MINI_MAX, "USE BEFORE REPORTING %s is %s%d",
			f->name, CB_PREFIX_LABEL, current_section->id);
		emit_statement (cb_build_comment (cobc_parse_strdup(wrk)));
	}
  }
#line 25934 "parser.c"
    break;

  case 2376:
#line 15746 "parser.y"
  {
	current_section->flag_real_label = 1;
	emit_statement (cb_build_comment ("USE AFTER EXCEPTION CONDITION"));
	CB_PENDING ("USE AFTER EXCEPTION CONDITION");
  }
#line 25944 "parser.c"
    break;

  case 2377:
#line 15752 "parser.y"
  {
	cb_tree		l;

	for (l = yyvsp[0]; l; l = CB_CHAIN (l)) {
		if (CB_VALID_TREE (CB_VALUE (l))) {
			setup_use_file (CB_FILE (cb_ref (CB_VALUE (l))));
		}
	}
	current_section->flag_real_label = 1;
	emit_statement(cb_build_comment("USE AFTER EXCEPTION CONDITION"));
	CB_PENDING("USE AFTER EXCEPTION CONDITION");
  }
#line 25961 "parser.c"
    break;

  case 2380:
#line 15775 "parser.y"
  {
	begin_statement ("WRITE", TERM_WRITE);
	/* Special in debugging mode */
	save_debug = start_debug;
	start_debug = 0;
  }
#line 25972 "parser.c"
    break;

  case 2382:
#line 15787 "parser.y"
  {
	if (CB_VALID_TREE (yyvsp[-5])) {
		cb_emit_write (yyvsp[-5], yyvsp[-4], yyvsp[-3], yyvsp[-1]);
	}
	start_debug = save_debug;
  }
#line 25983 "parser.c"
    break;

  case 2383:
#line 15796 "parser.y"
                                { yyval = NULL; }
#line 25989 "parser.c"
    break;

  case 2384:
#line 15797 "parser.y"
                                { yyval = yyvsp[0]; }
#line 25995 "parser.c"
    break;

  case 2385:
#line 15802 "parser.y"
  {
	yyval = cb_int0;
  }
#line 26003 "parser.c"
    break;

  case 2386:
#line 15806 "parser.y"
  {
	yyval = cb_build_write_advancing_lines (yyvsp[-3], yyvsp[-1]);
  }
#line 26011 "parser.c"
    break;

  case 2387:
#line 15810 "parser.y"
  {
	yyval = cb_build_write_advancing_mnemonic (yyvsp[-2], yyvsp[0]);
  }
#line 26019 "parser.c"
    break;

  case 2388:
#line 15814 "parser.y"
  {
	yyval = cb_build_write_advancing_page (yyvsp[-2]);
  }
#line 26027 "parser.c"
    break;

  case 2389:
#line 15820 "parser.y"
                                { yyval = CB_BEFORE; }
#line 26033 "parser.c"
    break;

  case 2390:
#line 15821 "parser.y"
                                { yyval = CB_AFTER; }
#line 26039 "parser.c"
    break;

  case 2394:
#line 15832 "parser.y"
  {
	TERMINATOR_WARNING (yyvsp[(-2) - (0)], WRITE);
  }
#line 26047 "parser.c"
    break;

  case 2395:
#line 15836 "parser.y"
  {
	TERMINATOR_CLEAR (yyvsp[(-2) - (1)], WRITE);
  }
#line 26055 "parser.c"
    break;

  case 2396:
#line 15845 "parser.y"
  {
	begin_statement ("XML GENERATE", TERM_XML);
	cobc_in_xml_generate_body = 1;
	cobc_cs_check = CB_CS_XML_GENERATE;
  }
#line 26065 "parser.c"
    break;

  case 2398:
#line 15857 "parser.y"
  {
	xml_encoding = NULL;
	with_xml_dec = 0;
	with_attrs = 0;
	ml_suppress_list = NULL;
  }
#line 26076 "parser.c"
    break;

  case 2399:
#line 15868 "parser.y"
  {
	cobc_in_xml_generate_body = 0;
	cobc_cs_check = 0;
  }
#line 26085 "parser.c"
    break;

  case 2400:
#line 15873 "parser.y"
  {
	cb_emit_xml_generate (yyvsp[-11], yyvsp[-9], yyvsp[-8], xml_encoding, with_xml_dec,
			      with_attrs, yyvsp[-5], yyvsp[-4], yyvsp[-3], ml_suppress_list);
  }
#line 26094 "parser.c"
    break;

  case 2406:
#line 15895 "parser.y"
  {
	xml_encoding = yyvsp[0];
	if (with_xml_dec) {
		cb_error (_("ENCODING clause must come before XML-DECLARATION"));
	} else if (with_attrs) {
		cb_error (_("ENCODING clause must come before ATTRIBUTES"));
	}
	cb_verify (cb_xml_generate_extra_phrases,
		   _("XML GENERATE ENCODING clause"));
	CB_PENDING ("XML GENERATE ENCODING");
  }
#line 26110 "parser.c"
    break;

  case 2407:
#line 15907 "parser.y"
  {
	with_xml_dec = 1;
	if (with_attrs) {
		cb_error (_("XML-DECLARATION clause must come before ATTRIBUTES"));
	}
	cb_verify (cb_xml_generate_extra_phrases,
		   _("XML GENERATE XML-DECLARATION clause"));
  }
#line 26123 "parser.c"
    break;

  case 2408:
#line 15916 "parser.y"
  {
	with_attrs = 1;
	cb_verify (cb_xml_generate_extra_phrases,
		   _("XML GENERATE WITH ATTRIBUTES clause"));
  }
#line 26133 "parser.c"
    break;

  case 2409:
#line 15925 "parser.y"
  {
	 yyval = NULL;
  }
#line 26141 "parser.c"
    break;

  case 2410:
#line 15929 "parser.y"
  {
	yyval = CB_BUILD_PAIR (yyvsp[-1], yyvsp[0]);
	cb_verify (cb_xml_generate_extra_phrases,
		   _("XML GENERATE NAMESPACE clause"));
  }
#line 26151 "parser.c"
    break;

  case 2411:
#line 15938 "parser.y"
  {
	yyval = cb_null;
  }
#line 26159 "parser.c"
    break;

  case 2412:
#line 15942 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 26167 "parser.c"
    break;

  case 2413:
#line 15949 "parser.y"
  {
	yyval = NULL;
  }
#line 26175 "parser.c"
    break;

  case 2414:
#line 15953 "parser.y"
  {
	yyval = yyvsp[0];
	cb_verify (cb_xml_generate_extra_phrases,
		   _("XML GENERATE NAME OF clause"));
  }
#line 26185 "parser.c"
    break;

  case 2415:
#line 15962 "parser.y"
  {
	yyval = CB_LIST_INIT (yyvsp[0]);
  }
#line 26193 "parser.c"
    break;

  case 2416:
#line 15966 "parser.y"
  {
	yyval = cb_list_add (yyvsp[-1], yyvsp[0]);
  }
#line 26201 "parser.c"
    break;

  case 2417:
#line 15973 "parser.y"
  {
	yyval = CB_BUILD_PAIR (yyvsp[-2], yyvsp[0]);
  }
#line 26209 "parser.c"
    break;

  case 2418:
#line 15980 "parser.y"
  {
	yyval = NULL;
  }
#line 26217 "parser.c"
    break;

  case 2419:
#line 15984 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 26225 "parser.c"
    break;

  case 2420:
#line 15991 "parser.y"
  {
	yyval = CB_LIST_INIT (yyvsp[0]);
  }
#line 26233 "parser.c"
    break;

  case 2421:
#line 15995 "parser.y"
  {
	yyval = cb_list_add (yyvsp[-1], yyvsp[0]);
  }
#line 26241 "parser.c"
    break;

  case 2422:
#line 16002 "parser.y"
  {
	yyval = CB_BUILD_PAIR (yyvsp[-2], yyvsp[0]);
  }
#line 26249 "parser.c"
    break;

  case 2423:
#line 16006 "parser.y"
  {
	yyval = CB_BUILD_PAIR (yyvsp[-2], cb_null);
  }
#line 26257 "parser.c"
    break;

  case 2424:
#line 16013 "parser.y"
  {
       yyval = NULL;
  }
#line 26265 "parser.c"
    break;

  case 2425:
#line 16017 "parser.y"
  {
       yyval = yyvsp[0];
       	cb_verify (cb_xml_generate_extra_phrases,
		   _("XML GENERATE TYPE OF clause"));
  }
#line 26275 "parser.c"
    break;

  case 2426:
#line 16026 "parser.y"
  {
	yyval = CB_LIST_INIT (yyvsp[0]);
  }
#line 26283 "parser.c"
    break;

  case 2427:
#line 16030 "parser.y"
  {
	yyval = cb_list_add (yyvsp[-1], yyvsp[0]);
  }
#line 26291 "parser.c"
    break;

  case 2428:
#line 16037 "parser.y"
  {
	yyval = CB_BUILD_PAIR (yyvsp[-2], yyvsp[0]);
  }
#line 26299 "parser.c"
    break;

  case 2429:
#line 16044 "parser.y"
  {
	yyval = cb_int ((int) CB_ML_ANY_TYPE);
  }
#line 26307 "parser.c"
    break;

  case 2431:
#line 16051 "parser.y"
                { yyval = cb_int ((int) CB_ML_ATTRIBUTE); }
#line 26313 "parser.c"
    break;

  case 2432:
#line 16052 "parser.y"
                { yyval = cb_int ((int) CB_ML_ELEMENT); }
#line 26319 "parser.c"
    break;

  case 2433:
#line 16053 "parser.y"
                { yyval = cb_int ((int) CB_ML_CONTENT); }
#line 26325 "parser.c"
    break;

  case 2435:
#line 16059 "parser.y"
  {
	cb_verify (cb_xml_generate_extra_phrases,
		   _("XML GENERATE SUPPRESS clause"));
  }
#line 26334 "parser.c"
    break;

  case 2438:
#line 16072 "parser.y"
  {
	error_if_following_every_clause ();
	add_identifier_to_ml_suppress_conds (yyvsp[0]);
  }
#line 26343 "parser.c"
    break;

  case 2439:
#line 16077 "parser.y"
  {
	error_if_following_every_clause ();
	add_type_to_ml_suppress_conds (ml_suppress_category, (enum cb_ml_type) CB_INTEGER (yyvsp[0])->val);
  }
#line 26352 "parser.c"
    break;

  case 2440:
#line 16082 "parser.y"
  {
	add_when_to_ml_suppress_conds (yyvsp[0]);
  }
#line 26360 "parser.c"
    break;

  case 2441:
#line 16089 "parser.y"
  {
	ml_suppress_category = CB_ML_SUPPRESS_CAT_NUMERIC;
	yyval = yyvsp[0];
  }
#line 26369 "parser.c"
    break;

  case 2442:
#line 16094 "parser.y"
  {
	ml_suppress_category = CB_ML_SUPPRESS_CAT_NONNUMERIC;
	yyval = yyvsp[0];
  }
#line 26378 "parser.c"
    break;

  case 2443:
#line 16099 "parser.y"
  {
	ml_suppress_category = CB_ML_SUPPRESS_CAT_ANY;
	yyval = yyvsp[0];
  }
#line 26387 "parser.c"
    break;

  case 2444:
#line 16107 "parser.y"
  {
	yyval = CB_LIST_INIT (yyvsp[0]);
  }
#line 26395 "parser.c"
    break;

  case 2445:
#line 16111 "parser.y"
  {
       yyval = cb_list_add (yyvsp[-2], yyvsp[0]);
  }
#line 26403 "parser.c"
    break;

  case 2446:
#line 16118 "parser.y"
  {
	TERMINATOR_WARNING (yyvsp[(-2) - (0)], XML);
  }
#line 26411 "parser.c"
    break;

  case 2447:
#line 16122 "parser.y"
  {
	TERMINATOR_CLEAR (yyvsp[(-2) - (1)], XML);
  }
#line 26419 "parser.c"
    break;

  case 2448:
#line 16132 "parser.y"
  {
	begin_statement ("XML PARSE", TERM_XML);
	/* TO-DO: Add xml-parse and xml-parse-extra-phrases config options. */
	CB_PENDING ("XML PARSE");
	cobc_cs_check = CB_CS_XML_PARSE;
  }
#line 26430 "parser.c"
    break;

  case 2450:
#line 16148 "parser.y"
  {
	cobc_cs_check = 0;
  }
#line 26438 "parser.c"
    break;

  case 2459:
#line 16172 "parser.y"
  {
	if (CB_FILE_P (cb_ref (yyvsp[0]))) {
		yyval = yyvsp[0];
	} else {
		cb_error_x (yyvsp[0], _("'%s' is not a file name"), CB_NAME (yyvsp[0]));
		yyval = cb_error_node;
	}
  }
#line 26451 "parser.c"
    break;

  case 2462:
#line 16190 "parser.y"
  {
	if (yyvsp[0]) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT EXCEPTION before EXCEPTION"));
	}
  }
#line 26462 "parser.c"
    break;

  case 2463:
#line 16200 "parser.y"
  {
	yyval = NULL;
  }
#line 26470 "parser.c"
    break;

  case 2464:
#line 16204 "parser.y"
  {
	yyval = cb_int1;
  }
#line 26478 "parser.c"
    break;

  case 2465:
#line 16211 "parser.y"
  {
	current_statement->handler_type = ACCEPT_HANDLER;
	current_statement->ex_handler = yyvsp[0];
  }
#line 26487 "parser.c"
    break;

  case 2470:
#line 16229 "parser.y"
  {
	current_statement->handler_type = ACCEPT_HANDLER;
	current_statement->not_ex_handler = yyvsp[0];
  }
#line 26496 "parser.c"
    break;

  case 2475:
#line 16245 "parser.y"
  {
	if (yyvsp[0]) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT EXCEPTION before EXCEPTION"));
	}
  }
#line 26507 "parser.c"
    break;

  case 2476:
#line 16255 "parser.y"
  {
	yyval = NULL;
  }
#line 26515 "parser.c"
    break;

  case 2477:
#line 16259 "parser.y"
  {
	yyval = cb_int1;
  }
#line 26523 "parser.c"
    break;

  case 2478:
#line 16266 "parser.y"
  {
	current_statement->handler_type = DISPLAY_HANDLER;
	current_statement->ex_handler = yyvsp[0];
  }
#line 26532 "parser.c"
    break;

  case 2481:
#line 16279 "parser.y"
  {
	current_statement->handler_type = DISPLAY_HANDLER;
	current_statement->not_ex_handler = yyvsp[0];
  }
#line 26541 "parser.c"
    break;

  case 2484:
#line 16289 "parser.y"
  {
	if (yyvsp[0]) {
		cb_verify (cb_not_exception_before_exception,
			   _("NOT EXCEPTION before EXCEPTION"));
	}
  }
#line 26552 "parser.c"
    break;

  case 2485:
#line 16299 "parser.y"
  {
	yyval = NULL;
  }
#line 26560 "parser.c"
    break;

  case 2486:
#line 16303 "parser.y"
  {
	yyval = cb_int1;
  }
#line 26568 "parser.c"
    break;

  case 2487:
#line 16310 "parser.y"
  {
	current_statement->handler_type = XML_HANDLER;
	current_statement->ex_handler = yyvsp[0];
  }
#line 26577 "parser.c"
    break;

  case 2490:
#line 16323 "parser.y"
  {
	current_statement->handler_type = XML_HANDLER;
	current_statement->not_ex_handler = yyvsp[0];
  }
#line 26586 "parser.c"
    break;

  case 2493:
#line 16333 "parser.y"
  {
	if (yyvsp[0]) {
		cb_verify (cb_not_exception_before_exception,
			   _("NOT EXCEPTION before EXCEPTION"));
	}
  }
#line 26597 "parser.c"
    break;

  case 2494:
#line 16343 "parser.y"
  {
	yyval = NULL;
  }
#line 26605 "parser.c"
    break;

  case 2495:
#line 16347 "parser.y"
  {
	yyval = cb_int1;
  }
#line 26613 "parser.c"
    break;

  case 2496:
#line 16354 "parser.y"
  {
	current_statement->handler_type = JSON_HANDLER;
	current_statement->ex_handler = yyvsp[0];
  }
#line 26622 "parser.c"
    break;

  case 2499:
#line 16367 "parser.y"
  {
	current_statement->handler_type = JSON_HANDLER;
	current_statement->not_ex_handler = yyvsp[0];
  }
#line 26631 "parser.c"
    break;

  case 2502:
#line 16379 "parser.y"
  {
	if (yyvsp[0]) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT SIZE ERROR before SIZE ERROR"));
	}
  }
#line 26642 "parser.c"
    break;

  case 2503:
#line 16389 "parser.y"
  {
	yyval = NULL;
  }
#line 26650 "parser.c"
    break;

  case 2504:
#line 16393 "parser.y"
  {
	yyval = cb_int1;
  }
#line 26658 "parser.c"
    break;

  case 2505:
#line 16400 "parser.y"
  {
	current_statement->handler_type = SIZE_ERROR_HANDLER;
	current_statement->ex_handler = yyvsp[0];
  }
#line 26667 "parser.c"
    break;

  case 2508:
#line 16413 "parser.y"
  {
	current_statement->handler_type = SIZE_ERROR_HANDLER;
	current_statement->not_ex_handler = yyvsp[0];
  }
#line 26676 "parser.c"
    break;

  case 2511:
#line 16425 "parser.y"
  {
	if (yyvsp[0]) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT OVERFLOW before OVERFLOW"));
	}
  }
#line 26687 "parser.c"
    break;

  case 2512:
#line 16435 "parser.y"
  {
	yyval = NULL;
  }
#line 26695 "parser.c"
    break;

  case 2513:
#line 16439 "parser.y"
  {
	yyval = cb_int1;
  }
#line 26703 "parser.c"
    break;

  case 2514:
#line 16446 "parser.y"
  {
	current_statement->handler_type = OVERFLOW_HANDLER;
	current_statement->ex_handler = yyvsp[0];
  }
#line 26712 "parser.c"
    break;

  case 2517:
#line 16459 "parser.y"
  {
	current_statement->handler_type = OVERFLOW_HANDLER;
	current_statement->not_ex_handler = yyvsp[0];
  }
#line 26721 "parser.c"
    break;

  case 2519:
#line 16471 "parser.y"
  {
	cb_verify (cb_not_exception_before_exception, "NOT AT END before AT END");
  }
#line 26729 "parser.c"
    break;

  case 2521:
#line 16480 "parser.y"
  {
	if (yyvsp[0]) {
		cb_verify (cb_not_exception_before_exception, "NOT AT END before AT END");
	}
  }
#line 26739 "parser.c"
    break;

  case 2522:
#line 16489 "parser.y"
  {
	yyval = NULL;
  }
#line 26747 "parser.c"
    break;

  case 2523:
#line 16493 "parser.y"
  {
	yyval = cb_int1;
  }
#line 26755 "parser.c"
    break;

  case 2524:
#line 16500 "parser.y"
  {
	current_statement->handler_type = AT_END_HANDLER;
	current_statement->ex_handler = yyvsp[0];
  }
#line 26764 "parser.c"
    break;

  case 2527:
#line 16513 "parser.y"
  {
	current_statement->handler_type = AT_END_HANDLER;
	current_statement->not_ex_handler = yyvsp[0];
  }
#line 26773 "parser.c"
    break;

  case 2529:
#line 16524 "parser.y"
  {
	if (yyvsp[0]) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT AT END-OF-PAGE before AT END-OF-PAGE"));
	}
  }
#line 26784 "parser.c"
    break;

  case 2530:
#line 16534 "parser.y"
  {
	yyval = NULL;
  }
#line 26792 "parser.c"
    break;

  case 2531:
#line 16538 "parser.y"
  {
	yyval = cb_int1;
  }
#line 26800 "parser.c"
    break;

  case 2532:
#line 16545 "parser.y"
  {
	current_statement->handler_type = EOP_HANDLER;
	current_statement->ex_handler = yyvsp[0];
  }
#line 26809 "parser.c"
    break;

  case 2535:
#line 16558 "parser.y"
  {
	current_statement->handler_type = EOP_HANDLER;
	current_statement->not_ex_handler = yyvsp[0];
  }
#line 26818 "parser.c"
    break;

  case 2539:
#line 16574 "parser.y"
  {
	if (yyvsp[0]) {
		cb_verify (cb_not_exception_before_exception,
			_("NOT INVALID KEY before INVALID KEY"));
	}
  }
#line 26829 "parser.c"
    break;

  case 2540:
#line 16584 "parser.y"
  {
	yyval = NULL;
  }
#line 26837 "parser.c"
    break;

  case 2541:
#line 16588 "parser.y"
  {
	yyval = cb_int1;
  }
#line 26845 "parser.c"
    break;

  case 2542:
#line 16595 "parser.y"
  {
	current_statement->handler_type = INVALID_KEY_HANDLER;
	current_statement->ex_handler = yyvsp[0];
  }
#line 26854 "parser.c"
    break;

  case 2545:
#line 16608 "parser.y"
  {
	current_statement->handler_type = INVALID_KEY_HANDLER;
	current_statement->not_ex_handler = yyvsp[0];
  }
#line 26863 "parser.c"
    break;

  case 2546:
#line 16618 "parser.y"
  {
	yyval = NULL;
  }
#line 26871 "parser.c"
    break;

  case 2547:
#line 16622 "parser.y"
  {
	yyval = cb_int1;
	CB_PENDING ("THREAD");
  }
#line 26880 "parser.c"
    break;

  case 2548:
#line 16630 "parser.y"
  {
	yyval = NULL;
  }
#line 26888 "parser.c"
    break;

  case 2549:
#line 16634 "parser.y"
  {
	yyval = yyvsp[0];
	CB_PENDING ("THREAD");
  }
#line 26897 "parser.c"
    break;

  case 2550:
#line 16642 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 26905 "parser.c"
    break;

  case 2551:
#line 16646 "parser.y"
  {
	yyval = NULL;
  }
#line 26913 "parser.c"
    break;

  case 2552:
#line 16655 "parser.y"
  {
	yyval = cb_one;
  }
#line 26921 "parser.c"
    break;

  case 2553:
#line 16659 "parser.y"
  {
	yyval = yyvsp[-1];
  }
#line 26929 "parser.c"
    break;

  case 2554:
#line 16665 "parser.y"
                                { yyval = NULL; }
#line 26935 "parser.c"
    break;

  case 2555:
#line 16666 "parser.y"
                                { yyval = yyvsp[0]; }
#line 26941 "parser.c"
    break;

  case 2556:
#line 16673 "parser.y"
  {
	yyval = cb_build_cond (yyvsp[0]);
	cb_end_cond (yyval);
  }
#line 26950 "parser.c"
    break;

  case 2557:
#line 16678 "parser.y"
  {
	yyval = cb_error_node;
	cb_end_cond (yyval);
  }
#line 26959 "parser.c"
    break;

  case 2558:
#line 16686 "parser.y"
  {
	yyval = cb_build_expr (yyvsp[0]);
  }
#line 26967 "parser.c"
    break;

  case 2559:
#line 16692 "parser.y"
  {
	current_expr = NULL;
	cb_exp_line = cb_source_line;
  }
#line 26976 "parser.c"
    break;

  case 2560:
#line 16697 "parser.y"
  {
	yyval = cb_list_reverse (current_expr);
  }
#line 26984 "parser.c"
    break;

  case 2563:
#line 16708 "parser.y"
                                { push_expr ('x', yyvsp[0]); }
#line 26990 "parser.c"
    break;

  case 2566:
#line 16713 "parser.y"
                                        { push_expr ('x', cb_zero); }
#line 26996 "parser.c"
    break;

  case 2567:
#line 16715 "parser.y"
                                { push_expr ('(', NULL); }
#line 27002 "parser.c"
    break;

  case 2568:
#line 16716 "parser.y"
                                { push_expr (')', NULL); }
#line 27008 "parser.c"
    break;

  case 2569:
#line 16718 "parser.y"
                                { push_expr ('+', NULL); }
#line 27014 "parser.c"
    break;

  case 2570:
#line 16719 "parser.y"
                                { push_expr ('-', NULL); }
#line 27020 "parser.c"
    break;

  case 2571:
#line 16720 "parser.y"
                                { push_expr ('*', NULL); }
#line 27026 "parser.c"
    break;

  case 2572:
#line 16721 "parser.y"
                                { push_expr ('/', NULL); }
#line 27032 "parser.c"
    break;

  case 2573:
#line 16722 "parser.y"
                                { push_expr ('^', NULL); }
#line 27038 "parser.c"
    break;

  case 2575:
#line 16725 "parser.y"
                                { push_expr ('&', NULL); }
#line 27044 "parser.c"
    break;

  case 2576:
#line 16726 "parser.y"
                                { push_expr ('|', NULL); }
#line 27050 "parser.c"
    break;

  case 2579:
#line 16735 "parser.y"
                                { push_expr ('!', NULL); }
#line 27056 "parser.c"
    break;

  case 2580:
#line 16738 "parser.y"
                                { push_expr ('C', yyvsp[0]); }
#line 27062 "parser.c"
    break;

  case 2581:
#line 16740 "parser.y"
                                { push_expr ('=', NULL); }
#line 27068 "parser.c"
    break;

  case 2582:
#line 16741 "parser.y"
                                { push_expr ('>', NULL); }
#line 27074 "parser.c"
    break;

  case 2583:
#line 16742 "parser.y"
                                { push_expr ('<', NULL); }
#line 27080 "parser.c"
    break;

  case 2584:
#line 16743 "parser.y"
                                { push_expr (']', NULL); }
#line 27086 "parser.c"
    break;

  case 2585:
#line 16744 "parser.y"
                                { push_expr ('[', NULL); }
#line 27092 "parser.c"
    break;

  case 2586:
#line 16745 "parser.y"
                                { push_expr ('~', NULL); }
#line 27098 "parser.c"
    break;

  case 2587:
#line 16747 "parser.y"
                                { push_expr ('O', NULL); }
#line 27104 "parser.c"
    break;

  case 2588:
#line 16748 "parser.y"
                                { push_expr ('9', NULL); }
#line 27110 "parser.c"
    break;

  case 2589:
#line 16749 "parser.y"
                                { push_expr ('A', NULL); }
#line 27116 "parser.c"
    break;

  case 2590:
#line 16750 "parser.y"
                                { push_expr ('L', NULL); }
#line 27122 "parser.c"
    break;

  case 2591:
#line 16751 "parser.y"
                                { push_expr ('U', NULL); }
#line 27128 "parser.c"
    break;

  case 2592:
#line 16754 "parser.y"
                                { push_expr ('P', NULL); }
#line 27134 "parser.c"
    break;

  case 2593:
#line 16755 "parser.y"
                                { push_expr ('N', NULL); }
#line 27140 "parser.c"
    break;

  case 2602:
#line 16785 "parser.y"
  {
	yyval = CB_LIST_INIT (yyvsp[0]);
  }
#line 27148 "parser.c"
    break;

  case 2603:
#line 16789 "parser.y"
  {
	yyval = cb_list_add (yyvsp[-2], yyvsp[0]);
  }
#line 27156 "parser.c"
    break;

  case 2607:
#line 16801 "parser.y"
                                { yyval = cb_build_binary_op (yyvsp[-2], '+', yyvsp[0]); }
#line 27162 "parser.c"
    break;

  case 2608:
#line 16802 "parser.y"
                                { yyval = cb_build_binary_op (yyvsp[-2], '-', yyvsp[0]); }
#line 27168 "parser.c"
    break;

  case 2609:
#line 16803 "parser.y"
                                { yyval = yyvsp[0]; }
#line 27174 "parser.c"
    break;

  case 2610:
#line 16807 "parser.y"
                                { yyval = cb_build_binary_op (yyvsp[-2], '*', yyvsp[0]); }
#line 27180 "parser.c"
    break;

  case 2611:
#line 16808 "parser.y"
                                { yyval = cb_build_binary_op (yyvsp[-2], '/', yyvsp[0]); }
#line 27186 "parser.c"
    break;

  case 2612:
#line 16809 "parser.y"
                                { yyval = yyvsp[0]; }
#line 27192 "parser.c"
    break;

  case 2613:
#line 16814 "parser.y"
  {
	yyval = cb_build_binary_op (yyvsp[-2], '^', yyvsp[0]);
  }
#line 27200 "parser.c"
    break;

  case 2614:
#line 16817 "parser.y"
                                { yyval = yyvsp[0]; }
#line 27206 "parser.c"
    break;

  case 2615:
#line 16821 "parser.y"
                                { yyval = yyvsp[0]; }
#line 27212 "parser.c"
    break;

  case 2616:
#line 16822 "parser.y"
                                { yyval = cb_build_binary_op (cb_zero, '-', yyvsp[0]); }
#line 27218 "parser.c"
    break;

  case 2617:
#line 16823 "parser.y"
                                { yyval = yyvsp[0]; }
#line 27224 "parser.c"
    break;

  case 2618:
#line 16826 "parser.y"
                                        { yyval = yyvsp[-1]; }
#line 27230 "parser.c"
    break;

  case 2619:
#line 16827 "parser.y"
                                        { yyval = yyvsp[0]; }
#line 27236 "parser.c"
    break;

  case 2620:
#line 16838 "parser.y"
  {
	if (current_linage > 1) {
		cb_error (_("LINAGE-COUNTER must be qualified here"));
		yyval = cb_error_node;
	} else if (current_linage == 0) {
		cb_error (_("invalid LINAGE-COUNTER usage"));
		yyval = cb_error_node;
	} else {
		yyval = linage_file->linage_ctr;
	}
  }
#line 27252 "parser.c"
    break;

  case 2621:
#line 16850 "parser.y"
  {
	if (CB_FILE_P (cb_ref (yyvsp[0]))) {
		yyval = CB_FILE (cb_ref (yyvsp[0]))->linage_ctr;
	} else {
		cb_error_x (yyvsp[0], _("'%s' is not a file name"), CB_NAME (yyvsp[0]));
		yyval = cb_error_node;
	}
  }
#line 27265 "parser.c"
    break;

  case 2622:
#line 16859 "parser.y"
  {
	if (report_count > 1) {
		if (current_report != NULL) {
			yyval = current_report->line_counter;
		} else {
			cb_error (_("LINE-COUNTER must be qualified here"));
			yyval = cb_error_node;
		}
	} else if (report_count == 0) {
		cb_error (_("invalid LINE-COUNTER usage"));
		yyval = cb_error_node;
	} else {
		yyval = report_instance->line_counter;
	}
  }
#line 27285 "parser.c"
    break;

  case 2623:
#line 16875 "parser.y"
  {
	if (CB_REF_OR_REPORT_P (yyvsp[0])) {
		yyval = CB_REPORT_PTR (yyvsp[0])->line_counter;
	} else {
		cb_error_x (yyvsp[0], _("'%s' is not a report name"), CB_NAME (yyvsp[0]));
		yyval = cb_error_node;
	}
  }
#line 27298 "parser.c"
    break;

  case 2624:
#line 16884 "parser.y"
  {
	if (report_count > 1) {
		if (current_report != NULL) {
			yyval = current_report->page_counter;
		} else {
			cb_error (_("PAGE-COUNTER must be qualified here"));
			yyval = cb_error_node;
		}
	} else if (report_count == 0) {
		cb_error (_("invalid PAGE-COUNTER usage"));
		yyval = cb_error_node;
	} else {
		yyval = report_instance->page_counter;
	}
  }
#line 27318 "parser.c"
    break;

  case 2625:
#line 16900 "parser.y"
  {
	if (CB_REF_OR_REPORT_P (yyvsp[0])) {
		yyval = CB_REPORT_PTR (yyvsp[0])->page_counter;
	} else {
		cb_error_x (yyvsp[0], _("'%s' is not a report name"), CB_NAME (yyvsp[0]));
		yyval = cb_error_node;
	}
  }
#line 27331 "parser.c"
    break;

  case 2626:
#line 16914 "parser.y"
                                { yyval = yyvsp[0]; }
#line 27337 "parser.c"
    break;

  case 2627:
#line 16916 "parser.y"
                                { yyval = cb_list_append (yyvsp[-1], yyvsp[0]); }
#line 27343 "parser.c"
    break;

  case 2628:
#line 16921 "parser.y"
  {
	yyval = CB_BUILD_PAIR (yyvsp[0], yyvsp[-1]);
  }
#line 27351 "parser.c"
    break;

  case 2629:
#line 16929 "parser.y"
                                { cb_build_identifier (yyvsp[0], 0); }
#line 27357 "parser.c"
    break;

  case 2630:
#line 16936 "parser.y"
  {
	if (!CB_FILE_P (cb_ref (yyvsp[0]))) {
		yyval = yyvsp[0];
	} else {
		cb_error_x (yyvsp[0], _("%s requires a record name as subject"),
			current_statement->name);
		yyval = cb_error_node;
	}
  }
#line 27371 "parser.c"
    break;

  case 2631:
#line 16946 "parser.y"
  {
	if (CB_FILE_P (cb_ref (yyvsp[0]))) {
		yyval = yyvsp[0];
	} else {
		cb_error_x (yyvsp[0], _("'%s' is not a file name"), CB_NAME (yyvsp[0]));
		yyval = cb_error_node;
	}
  }
#line 27384 "parser.c"
    break;

  case 2632:
#line 16960 "parser.y"
  {
	cb_tree x;

	x = cb_ref (yyvsp[0]);
	if (!CB_FIELD_P (x)) {
		yyval = cb_error_node;
	} else if (!CB_FIELD (x)->index_list) {
		cb_error_x (yyvsp[0], _("'%s' not indexed"), cb_name (yyvsp[0]));
		cb_note_x (COB_WARNOPT_NONE, x, _("'%s' defined here"), cb_name (x));
		yyval = cb_error_node;
	} else {
		yyval = yyvsp[0];
	}
  }
#line 27403 "parser.c"
    break;

  case 2633:
#line 16980 "parser.y"
  {
	yyval = CB_LIST_INIT (yyvsp[0]);
  }
#line 27411 "parser.c"
    break;

  case 2634:
#line 16984 "parser.y"
  {
	cb_tree		l;

	if (CB_VALID_TREE (yyvsp[0])) {
		for (l = yyvsp[-1]; l; l = CB_CHAIN (l)) {
			if (CB_VALID_TREE (CB_VALUE (l)) &&
			    !strcasecmp (CB_NAME (yyvsp[0]), CB_NAME (CB_VALUE (l)))) {
				cb_error_x (yyvsp[0], _("multiple reference to '%s' "),
					    CB_NAME (yyvsp[0]));
				break;
			}
		}
		if (!l) {
			yyval = cb_list_add (yyvsp[-1], yyvsp[0]);
		}
	}
  }
#line 27433 "parser.c"
    break;

  case 2635:
#line 17005 "parser.y"
  {
	yyval = CB_LIST_INIT (yyvsp[0]);
  }
#line 27441 "parser.c"
    break;

  case 2636:
#line 17009 "parser.y"
  {
	cb_tree		l;

	if (CB_VALID_TREE (yyvsp[0])) {
		for (l = yyvsp[-2]; l; l = CB_CHAIN (l)) {
			if (CB_VALID_TREE (CB_VALUE (l)) &&
			    !strcasecmp (CB_NAME (yyvsp[0]), CB_NAME (CB_VALUE (l)))) {
				cb_error_x (yyvsp[0], _("multiple reference to '%s' "),
					    CB_NAME (yyvsp[-1]));
				break;
			}
		}
		if (!l) {
			yyval = cb_list_add (yyvsp[-2], yyvsp[0]);
		}
	}
  }
#line 27463 "parser.c"
    break;

  case 2637:
#line 17030 "parser.y"
  {
	if (CB_FILE_P (cb_ref (yyvsp[0]))) {
		yyval = yyvsp[0];
	} else {
		cb_error_x (yyvsp[0], _("'%s' is not a file name"), CB_NAME (yyvsp[0]));
		yyval = cb_error_node;
	}
  }
#line 27476 "parser.c"
    break;

  case 2638:
#line 17042 "parser.y"
  {
	if (CB_CD_P (cb_ref (yyvsp[0]))) {
		yyval = yyvsp[0];
	} else {
		cb_error_x (yyvsp[0], _("'%s' is not a CD name"), CB_NAME (yyvsp[0]));
		yyval = cb_error_node;
	}
  }
#line 27489 "parser.c"
    break;

  case 2639:
#line 17056 "parser.y"
  {
	if (CB_REF_OR_REPORT_P (yyvsp[0])) {
		yyval = yyvsp[0];
	} else {
		cb_error (_("'%s' is not a valid report name"), CB_NAME (yyvsp[0]));
		yyval = cb_error_node;
	}
  }
#line 27502 "parser.c"
    break;

  case 2640:
#line 17069 "parser.y"
                                { yyval = CB_LIST_INIT (yyvsp[0]); }
#line 27508 "parser.c"
    break;

  case 2641:
#line 17071 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 27514 "parser.c"
    break;

  case 2642:
#line 17075 "parser.y"
                                { yyval = yyvsp[0]; }
#line 27520 "parser.c"
    break;

  case 2643:
#line 17081 "parser.y"
                        { yyval = CB_LIST_INIT (yyvsp[0]); }
#line 27526 "parser.c"
    break;

  case 2644:
#line 17083 "parser.y"
                        { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 27532 "parser.c"
    break;

  case 2645:
#line 17088 "parser.y"
  {
	yyval = cb_build_reference ((char *)(CB_LITERAL (yyvsp[0])->data));
  }
#line 27540 "parser.c"
    break;

  case 2646:
#line 17097 "parser.y"
                                { yyval = NULL; }
#line 27546 "parser.c"
    break;

  case 2647:
#line 17099 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 27552 "parser.c"
    break;

  case 2648:
#line 17104 "parser.y"
  {
	struct cb_reference *r = CB_REFERENCE (yyvsp[0]);

	r->offset = CB_TREE (current_section);
	r->flag_in_decl = !!in_declaratives;
	r->flag_ignored = cb_set_ignore_error (-1);

	yyval = yyvsp[0];
	CB_ADD_TO_CHAIN (yyvsp[0], current_program->label_list);
  }
#line 27567 "parser.c"
    break;

  case 2651:
#line 17120 "parser.y"
  {
	CB_REFERENCE (yyvsp[-2])->chain = yyvsp[0];
  }
#line 27575 "parser.c"
    break;

  case 2652:
#line 17127 "parser.y"
  {
	yyval = cb_build_reference ((char *)(CB_LITERAL (yyvsp[0])->data));
	yyval->source_file = yyvsp[0]->source_file;
	yyval->source_line = yyvsp[0]->source_line;
  }
#line 27585 "parser.c"
    break;

  case 2653:
#line 17137 "parser.y"
                                { yyval = CB_LIST_INIT (yyvsp[0]); }
#line 27591 "parser.c"
    break;

  case 2654:
#line 17138 "parser.y"
                                { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 27597 "parser.c"
    break;

  case 2655:
#line 17143 "parser.y"
  {
	yyval = yyvsp[0];
	CB_ADD_TO_CHAIN (yyval, current_program->reference_list);
  }
#line 27606 "parser.c"
    break;

  case 2656:
#line 17150 "parser.y"
                {yyval = NULL;}
#line 27612 "parser.c"
    break;

  case 2657:
#line 17151 "parser.y"
                        {yyval = yyvsp[0];}
#line 27618 "parser.c"
    break;

  case 2658:
#line 17155 "parser.y"
                                        { yyval = CB_LIST_INIT (yyvsp[0]); }
#line 27624 "parser.c"
    break;

  case 2659:
#line 17156 "parser.y"
                                        { yyval = cb_list_add (yyvsp[-1], yyvsp[0]); }
#line 27630 "parser.c"
    break;

  case 2660:
#line 17161 "parser.y"
  {
	if (!within_typedef_definition) {
		CB_ADD_TO_CHAIN (yyvsp[0], current_program->reference_list);
	}
  }
#line 27640 "parser.c"
    break;

  case 2661:
#line 17173 "parser.y"
  {
	yyval = CB_LIST_INIT (yyvsp[0]);
  }
#line 27648 "parser.c"
    break;

  case 2662:
#line 17177 "parser.y"
  {
	yyval = cb_list_add (yyvsp[-1], yyvsp[0]);
  }
#line 27656 "parser.c"
    break;

  case 2663:
#line 17184 "parser.y"
  {
	yyval = yyvsp[0];
	CB_REFERENCE(yyval)->flag_optional = 1;
	CB_ADD_TO_CHAIN (yyval, current_program->reference_list);
  }
#line 27666 "parser.c"
    break;

  case 2666:
#line 17200 "parser.y"
  {
	if (CB_WORD_COUNT (yyvsp[0]) > 0) {
		redefinition_error (yyvsp[0]);
		yyval = cb_error_node;
	} else {
		yyval = yyvsp[0];
	}
  }
#line 27679 "parser.c"
    break;

  case 2667:
#line 17209 "parser.y"
  {
	yyclearin;
	yyerrok;
	yyval = cb_error_node;
  }
#line 27689 "parser.c"
    break;

  case 2668:
#line 17220 "parser.y"
  {
	if (CB_REFERENCE (yyvsp[0])->flag_duped || CB_WORD_COUNT (yyvsp[0]) > 0) {
		redefinition_error (yyvsp[0]);
		yyval = NULL;
	} else {
		CB_WORD_COUNT (yyvsp[0])++;
		yyval = yyvsp[0];
	}
  }
#line 27703 "parser.c"
    break;

  case 2669:
#line 17237 "parser.y"
  {
	yyval = CB_LIST_INIT (yyvsp[0]);
  }
#line 27711 "parser.c"
    break;

  case 2670:
#line 17241 "parser.y"
  {
	yyval = cb_list_add (yyvsp[-1], yyvsp[0]);
  }
#line 27719 "parser.c"
    break;

  case 2673:
#line 17250 "parser.y"
  {
	yyval = cb_build_address (yyvsp[0]);
  }
#line 27727 "parser.c"
    break;

  case 2674:
#line 17256 "parser.y"
                { yyval = NULL; }
#line 27733 "parser.c"
    break;

  case 2675:
#line 17257 "parser.y"
                { yyval = yyvsp[0]; }
#line 27739 "parser.c"
    break;

  case 2676:
#line 17262 "parser.y"
  {
	yyval = CB_LIST_INIT (yyvsp[0]);
  }
#line 27747 "parser.c"
    break;

  case 2677:
#line 17266 "parser.y"
  {
	yyval = cb_list_add (yyvsp[-1], yyvsp[0]);
  }
#line 27755 "parser.c"
    break;

  case 2685:
#line 17286 "parser.y"
  {
	yyval = cb_build_length (yyvsp[0]);
  }
#line 27763 "parser.c"
    break;

  case 2686:
#line 17290 "parser.y"
  {
	yyval = cb_build_length (yyvsp[0]);
  }
#line 27771 "parser.c"
    break;

  case 2687:
#line 17294 "parser.y"
  {
	yyval = cb_build_length (yyvsp[0]);
  }
#line 27779 "parser.c"
    break;

  case 2688:
#line 17298 "parser.y"
  {
	yyval = cb_build_ppointer (yyvsp[0]);
  }
#line 27787 "parser.c"
    break;

  case 2689:
#line 17302 "parser.y"
  {
	yyval = cb_build_address (check_not_88_level (yyvsp[0]));
  }
#line 27795 "parser.c"
    break;

  case 2690:
#line 17306 "parser.y"
  {
	CB_PENDING ("EXTFH address");
  }
#line 27803 "parser.c"
    break;

  case 2691:
#line 17310 "parser.y"
  {
	CB_PENDING ("EXTFH address");
  }
#line 27811 "parser.c"
    break;

  case 2692:
#line 17314 "parser.y"
  {
	cb_tree		x;
	cb_tree		switch_id;

	x = cb_ref (yyvsp[0]);
	if (CB_VALID_TREE (x)) {
		if (CB_SYSTEM_NAME (x)->category != CB_SWITCH_NAME) {
			cb_error_x (yyvsp[0], _("invalid mnemonic identifier"));
			yyval = cb_error_node;
		} else {
			switch_id = cb_int (CB_SYSTEM_NAME (x)->token);
			yyval = CB_BUILD_FUNCALL_1 ("cob_switch_value", switch_id);
		}
	} else {
		yyval = cb_error_node;
	}
  }
#line 27833 "parser.c"
    break;

  case 2693:
#line 17335 "parser.y"
  {
	/* FIXME: check with "lookup_register ("LENGTH OF") != NULL"
	          if we actually want to do this,
	          otherwise raise an error "not defined in this dialect"
	*/
  }
#line 27844 "parser.c"
    break;

  case 2694:
#line 17345 "parser.y"
  {
	yyval = CB_LIST_INIT (yyvsp[0]);
  }
#line 27852 "parser.c"
    break;

  case 2695:
#line 17349 "parser.y"
  {
	yyval = cb_list_add (yyvsp[-1], yyvsp[0]);
  }
#line 27860 "parser.c"
    break;

  case 2703:
#line 17366 "parser.y"
  {
	yyval = cb_build_length (yyvsp[0]);
  }
#line 27868 "parser.c"
    break;

  case 2704:
#line 17370 "parser.y"
  {
	yyval = cb_build_length (yyvsp[0]);
  }
#line 27876 "parser.c"
    break;

  case 2705:
#line 17374 "parser.y"
  {
	yyval = cb_build_length (yyvsp[0]);
  }
#line 27884 "parser.c"
    break;

  case 2709:
#line 17384 "parser.y"
  {
	yyval = cb_build_length (yyvsp[0]);
  }
#line 27892 "parser.c"
    break;

  case 2710:
#line 17388 "parser.y"
  {
	yyval = cb_build_length (yyvsp[0]);
  }
#line 27900 "parser.c"
    break;

  case 2711:
#line 17392 "parser.y"
  {
	yyval = cb_build_length (yyvsp[0]);
  }
#line 27908 "parser.c"
    break;

  case 2712:
#line 17399 "parser.y"
  {
	if (CB_TREE_CATEGORY (yyvsp[0]) != CB_CATEGORY_NUMERIC) {
		cb_error_x (yyvsp[0], _("a numeric literal is expected here"));
		yyval = cb_error_node;
	} else {
		yyval = yyvsp[0];
	}
  }
#line 27921 "parser.c"
    break;

  case 2713:
#line 17411 "parser.y"
  {
	if (CB_TREE_CATEGORY (yyvsp[0]) == CB_CATEGORY_NUMERIC) {
		cb_error_x (yyvsp[0], _("a non-numeric literal is expected here"));
		yyval = cb_error_node;
	} else {
		yyval = yyvsp[0];
	}
  }
#line 27934 "parser.c"
    break;

  case 2714:
#line 17423 "parser.y"
  {
	if (cb_tree_category (yyvsp[0]) != CB_CATEGORY_NUMERIC
	 || cb_get_int (yyvsp[0]) == 0) {
		cb_error (_("non-zero value expected"));
		yyval = cb_int1;
	} else {
		yyval = yyvsp[0];
	}
  }
#line 27948 "parser.c"
    break;

  case 2719:
#line 17447 "parser.y"
  {
	error_if_not_usage_display_or_nonnumeric_lit (yyvsp[0]);
  }
#line 27956 "parser.c"
    break;

  case 2720:
#line 17454 "parser.y"
  {
	error_if_not_usage_display_or_nonnumeric_lit (yyvsp[0]);
  }
#line 27964 "parser.c"
    break;

  case 2722:
#line 17462 "parser.y"
  {
	  error_if_not_usage_display_or_nonnumeric_lit (yyvsp[0]);
  }
#line 27972 "parser.c"
    break;

  case 2724:
#line 17470 "parser.y"
  {
	  error_if_not_usage_display_or_nonnumeric_lit (yyvsp[0]);
  }
#line 27980 "parser.c"
    break;

  case 2730:
#line 17488 "parser.y"
  {
	yyval = check_not_88_level (yyvsp[0]);
  }
#line 27988 "parser.c"
    break;

  case 2732:
#line 17496 "parser.y"
  {
	yyval = check_not_88_level (yyvsp[0]);
  }
#line 27996 "parser.c"
    break;

  case 2735:
#line 17505 "parser.y"
  {
	yyval = check_not_88_level (yyvsp[0]);
  }
#line 28004 "parser.c"
    break;

  case 2738:
#line 17514 "parser.y"
  {
	yyval = check_not_88_level (yyvsp[0]);
  }
#line 28012 "parser.c"
    break;

  case 2740:
#line 17519 "parser.y"
  {
	yyval = cb_zero;
  }
#line 28020 "parser.c"
    break;

  case 2741:
#line 17528 "parser.y"
  {
	yyval = check_not_88_level (yyvsp[0]);
  }
#line 28028 "parser.c"
    break;

  case 2745:
#line 17544 "parser.y"
  {
	yyval = check_not_88_level (yyvsp[0]);
  }
#line 28036 "parser.c"
    break;

  case 2747:
#line 17552 "parser.y"
  {
	yyval = check_not_88_level (yyvsp[0]);
  }
#line 28044 "parser.c"
    break;

  case 2750:
#line 17562 "parser.y"
                                { yyval = cb_build_identifier (yyvsp[0], 0); }
#line 28050 "parser.c"
    break;

  case 2751:
#line 17566 "parser.y"
                                { yyval = cb_build_identifier (yyvsp[0], 1); }
#line 28056 "parser.c"
    break;

  case 2752:
#line 17570 "parser.y"
                                { yyval = yyvsp[0]; }
#line 28062 "parser.c"
    break;

  case 2753:
#line 17571 "parser.y"
                                { yyval = yyvsp[-1]; }
#line 28068 "parser.c"
    break;

  case 2754:
#line 17576 "parser.y"
  {
	error_if_not_usage_display_or_nonnumeric_lit (yyvsp[0]);
  }
#line 28076 "parser.c"
    break;

  case 2755:
#line 17583 "parser.y"
  {
	if (yyvsp[0] != cb_error_node
	    && cb_tree_category (yyvsp[0]) != CB_CATEGORY_NUMERIC) {
		cb_error_x (yyvsp[0], _("'%s' is not numeric"), cb_name (yyvsp[0]));
	}
  }
#line 28087 "parser.c"
    break;

  case 2756:
#line 17593 "parser.y"
  {
	cb_tree x = NULL;
	if (CB_REFERENCE_P (yyvsp[0])) {
		x = cb_ref (yyvsp[0]);
	}
	if (x && (CB_FIELD_P (x) || CB_FILE_P (x))) {
		yyval = cb_build_identifier (yyvsp[0], 0);
	} else {
		if (x != cb_error_node) {
			cb_error_x (yyvsp[0], _("'%s' is not a field or file"), cb_name (yyvsp[0]));
		}
		yyval = cb_error_node;
	}
  }
#line 28106 "parser.c"
    break;

  case 2757:
#line 17612 "parser.y"
  {
	cb_tree x = NULL;
	if (CB_REFERENCE_P (yyvsp[0])) {
		x = cb_ref (yyvsp[0]);
	}

	if (x && CB_FIELD_P (x)) {
		yyval = yyvsp[0];
	} else {
		if (x != cb_error_node) {
			cb_error_x (yyvsp[0], _("'%s' is not a field"), cb_name (yyvsp[0]));
		}
		yyval = cb_error_node;
	}
  }
#line 28126 "parser.c"
    break;

  case 2758:
#line 17633 "parser.y"
  {
	cb_tree x = NULL;
	if (CB_REFERENCE_P (yyvsp[0])) {
		x = cb_ref (yyvsp[0]);
	}

	if (x && CB_FIELD_P (x) && CB_FIELD (x)->flag_is_typedef) {
		yyval = yyvsp[0];
	} else {
		if (x != cb_error_node) {
			cb_error_x (yyvsp[0], _("'%s' is not a type-name"), cb_name (yyvsp[0]));
		}
		yyval = cb_error_node;
	}
  }
#line 28146 "parser.c"
    break;

  case 2759:
#line 17652 "parser.y"
  {
	cb_tree x = NULL;
	if (CB_REFERENCE_P (yyvsp[0])) {
		x = cb_ref (yyvsp[0]);
	}
	if (x && CB_FIELD_P (x)) {
		yyval = cb_build_identifier (yyvsp[0], 0);
	} else {
		if (x != cb_error_node) {
			cb_error_x (yyvsp[0], _("'%s' is not a field"), cb_name (yyvsp[0]));
		}
		yyval = cb_error_node;
	}
  }
#line 28165 "parser.c"
    break;

  case 2760:
#line 17670 "parser.y"
  {
	yyval = yyvsp[-2];
	if (start_debug) {
		cb_check_field_debug (yyvsp[-2]);
	}
  }
#line 28176 "parser.c"
    break;

  case 2761:
#line 17677 "parser.y"
  {
	yyval = yyvsp[-1];
	if (start_debug) {
		cb_check_field_debug (yyvsp[-1]);
	}
  }
#line 28187 "parser.c"
    break;

  case 2762:
#line 17684 "parser.y"
  {
	yyval = yyvsp[-1];
	if (start_debug) {
		cb_check_field_debug (yyvsp[-1]);
	}
  }
#line 28198 "parser.c"
    break;

  case 2763:
#line 17691 "parser.y"
  {
	yyval = yyvsp[0];
	if (start_debug) {
		cb_check_field_debug (yyvsp[0]);
	}
  }
#line 28209 "parser.c"
    break;

  case 2764:
#line 17701 "parser.y"
  {
	yyval = CB_LIST_INIT (yyvsp[0]);
  }
#line 28217 "parser.c"
    break;

  case 2765:
#line 17705 "parser.y"
  {
	yyval = cb_list_add (yyvsp[-1], yyvsp[0]);
  }
#line 28225 "parser.c"
    break;

  case 2766:
#line 17712 "parser.y"
  {
	yyval = cb_build_identifier (yyvsp[0], 0);
  }
#line 28233 "parser.c"
    break;

  case 2767:
#line 17716 "parser.y"
  {
	yyval = cb_build_identifier (yyvsp[0], 0);
  }
#line 28241 "parser.c"
    break;

  case 2768:
#line 17723 "parser.y"
  {
	yyval = yyvsp[-2];
	if (CB_REFERENCE_P (yyvsp[-2])) {
		CB_REFERENCE (yyvsp[-2])->flag_target = 1;
	}
	if (start_debug) {
		cb_check_field_debug (yyvsp[-2]);
	}
  }
#line 28255 "parser.c"
    break;

  case 2769:
#line 17733 "parser.y"
  {
	yyval = yyvsp[-1];
	if (CB_REFERENCE_P (yyvsp[-1])) {
		CB_REFERENCE (yyvsp[-1])->flag_target = 1;
	}
	if (start_debug) {
		cb_check_field_debug (yyvsp[-1]);
	}
  }
#line 28269 "parser.c"
    break;

  case 2770:
#line 17743 "parser.y"
  {
	yyval = yyvsp[-1];
	if (CB_REFERENCE_P (yyvsp[-1])) {
		CB_REFERENCE (yyvsp[-1])->flag_target = 1;
	}
	if (start_debug) {
		cb_check_field_debug (yyvsp[-1]);
	}
  }
#line 28283 "parser.c"
    break;

  case 2771:
#line 17753 "parser.y"
  {
	yyval = yyvsp[0];
	if (CB_REFERENCE_P (yyvsp[0])) {
		CB_REFERENCE (yyvsp[0])->flag_target = 1;
	}
	if (start_debug) {
		cb_check_field_debug (yyvsp[0]);
	}
  }
#line 28297 "parser.c"
    break;

  case 2772:
#line 17766 "parser.y"
  {
	cb_tree x = NULL;
	yyval = yyvsp[0];
	if (start_debug) {
		cb_check_field_debug (yyvsp[0]);
	}
	if (CB_REFERENCE_P (yyvsp[0])) {
		x = cb_ref (yyvsp[0]);
	}
	if (x && CB_FIELD_P (x)) {
		yyval = cb_build_identifier (yyvsp[0], 0);
		error_if_not_usage_display_or_nonnumeric_lit (yyvsp[0]);
	} else if (x && CB_ALPHABET_NAME_P (x)) {
		/* TODO: add check for subscript/ ref-mod here [not allowed] */
		yyval = cb_build_identifier (yyvsp[0], 0);
	} else {
		if (x != cb_error_node) {
			cb_error_x (yyvsp[0], _("'%s' is not a field or alphabet"), cb_name (yyvsp[0]));
		}
		yyval = cb_error_node;
	}
  }
#line 28324 "parser.c"
    break;

  case 2773:
#line 17792 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 28332 "parser.c"
    break;

  case 2774:
#line 17796 "parser.y"
  {
	yyval = yyvsp[-2];
	CB_REFERENCE (yyvsp[-2])->chain = yyvsp[0];
  }
#line 28341 "parser.c"
    break;

  case 2775:
#line 17803 "parser.y"
  {
	start_tree = NULL;	/* actually not needed - initialized for clarity only */
  }
#line 28349 "parser.c"
    break;

  case 2776:
#line 17807 "parser.y"
  {
	if (yyvsp[0] == cb_error_node) {
		cb_error_x (start_tree, _("a subscripted data-item cannot be used here"));
	}
	yyval = start_tree;
  }
#line 28360 "parser.c"
    break;

  case 2777:
#line 17817 "parser.y"
  {
	start_tree = yyvsp[0];
	yyval = yyvsp[0];
  }
#line 28369 "parser.c"
    break;

  case 2778:
#line 17822 "parser.y"
  {
	start_tree = yyvsp[-2];
	yyval = cb_error_node;
  }
#line 28378 "parser.c"
    break;

  case 2779:
#line 17830 "parser.y"
  {
	yyval = yyvsp[-3];
	CB_REFERENCE (yyvsp[-3])->subs = cb_list_reverse (yyvsp[-1]);
  }
#line 28387 "parser.c"
    break;

  case 2780:
#line 17838 "parser.y"
  {
	CB_REFERENCE (yyvsp[-4])->offset = yyvsp[-2];
  }
#line 28395 "parser.c"
    break;

  case 2781:
#line 17842 "parser.y"
  {
	CB_REFERENCE (yyvsp[-5])->offset = yyvsp[-3];
	CB_REFERENCE (yyvsp[-5])->length = yyvsp[-1];
  }
#line 28404 "parser.c"
    break;

  case 2782:
#line 17852 "parser.y"
  {
	if (cb_tree_category (yyvsp[0]) != CB_CATEGORY_NUMERIC
	 || !CB_LITERAL_P(yyvsp[0])
	 || CB_LITERAL (yyvsp[0])->sign
	 || CB_LITERAL (yyvsp[0])->scale) {
		cb_error (_("unsigned integer value expected"));
		yyval = cb_build_numeric_literal (-1, "1", 0);
	} else {
		yyval = yyvsp[0];
	}
  }
#line 28420 "parser.c"
    break;

  case 2783:
#line 17867 "parser.y"
  {
	if (cb_tree_category (yyvsp[0]) != CB_CATEGORY_NUMERIC) {
		cb_error (_("integer value expected"));
		yyval = cb_int1;
	} else if (CB_LITERAL_P (yyvsp[0])
		&& (CB_LITERAL (yyvsp[0])->sign || CB_LITERAL (yyvsp[0])->scale)) {
		cb_error (_("integer value expected"));
		yyval = cb_int1;
	} else {
		int	n = cb_get_int (yyvsp[0]);
		if (n < 1 || n > 256) {
			cb_error (_("invalid symbolic integer"));
			yyval = cb_int1;
		} else {
			yyval = yyvsp[0];
		}
	}
  }
#line 28443 "parser.c"
    break;

  case 2784:
#line 17889 "parser.y"
  {
	if (cb_tree_category (yyvsp[0]) != CB_CATEGORY_NUMERIC
	 || !CB_LITERAL_P(yyvsp[0])
	 || CB_LITERAL (yyvsp[0])->sign
	 || CB_LITERAL (yyvsp[0])->scale) {
		cb_error (_("unsigned positive integer value expected"));
		yyval = cb_int1;
	} else {
		if (cb_get_int (yyvsp[0]) < 1) {
			cb_error (_("unsigned positive integer value expected"));
			yyval = cb_int1;
		} else {
			yyval = yyvsp[0];
		}
	}
  }
#line 28464 "parser.c"
    break;

  case 2785:
#line 17909 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 28472 "parser.c"
    break;

  case 2786:
#line 17913 "parser.y"
  {
	yyval = cb_int0;
  }
#line 28480 "parser.c"
    break;

  case 2787:
#line 17920 "parser.y"
  {
	if (cb_tree_category (yyvsp[0]) == CB_CATEGORY_NUMERIC) {
		if (CB_LITERAL (yyvsp[0])->sign || CB_LITERAL (yyvsp[0])->scale) {
			cb_error (_("integer value expected"));
		} else {
			int	n = cb_get_int (yyvsp[0]);
			if (n < 1 || n > 256) {
				cb_error (_("invalid CLASS value"));
			}
		}
	}
	yyval = yyvsp[0];
  }
#line 28498 "parser.c"
    break;

  case 2788:
#line 17933 "parser.y"
                                { yyval = cb_space; }
#line 28504 "parser.c"
    break;

  case 2789:
#line 17934 "parser.y"
                                { yyval = cb_zero; }
#line 28510 "parser.c"
    break;

  case 2790:
#line 17935 "parser.y"
                                { yyval = cb_quote; }
#line 28516 "parser.c"
    break;

  case 2791:
#line 17936 "parser.y"
                                { yyval = cb_high; }
#line 28522 "parser.c"
    break;

  case 2792:
#line 17937 "parser.y"
                                { yyval = cb_low; }
#line 28528 "parser.c"
    break;

  case 2793:
#line 17938 "parser.y"
                                { yyval = cb_null; }
#line 28534 "parser.c"
    break;

  case 2794:
#line 17943 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 28542 "parser.c"
    break;

  case 2795:
#line 17947 "parser.y"
  {
	struct cb_literal	*l;

	if (CB_LITERAL_P (yyvsp[0])) {
		/* We must not alter the original definition */
		l = cobc_parse_malloc (sizeof(struct cb_literal));
		*l = *(CB_LITERAL(yyvsp[0]));
		l->all = 1;
		yyval = CB_TREE (l);
	} else {
		yyval = yyvsp[0];
	}
  }
#line 28560 "parser.c"
    break;

  case 2796:
#line 17964 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 28568 "parser.c"
    break;

  case 2797:
#line 17968 "parser.y"
  {
	yyval = cb_concat_literals (yyvsp[-2], yyvsp[0]);
  }
#line 28576 "parser.c"
    break;

  case 2798:
#line 17974 "parser.y"
                                { yyval = yyvsp[0]; }
#line 28582 "parser.c"
    break;

  case 2799:
#line 17975 "parser.y"
                                { yyval = cb_space; }
#line 28588 "parser.c"
    break;

  case 2800:
#line 17976 "parser.y"
                                { yyval = cb_zero; }
#line 28594 "parser.c"
    break;

  case 2801:
#line 17977 "parser.y"
                                { yyval = cb_quote; }
#line 28600 "parser.c"
    break;

  case 2802:
#line 17978 "parser.y"
                                { yyval = cb_high; }
#line 28606 "parser.c"
    break;

  case 2803:
#line 17979 "parser.y"
                                { yyval = cb_low; }
#line 28612 "parser.c"
    break;

  case 2804:
#line 17980 "parser.y"
                                { yyval = cb_null; }
#line 28618 "parser.c"
    break;

  case 2805:
#line 17984 "parser.y"
                                { yyval = cb_space; }
#line 28624 "parser.c"
    break;

  case 2806:
#line 17985 "parser.y"
                                { yyval = cb_zero; }
#line 28630 "parser.c"
    break;

  case 2807:
#line 17986 "parser.y"
                                { yyval = cb_high; }
#line 28636 "parser.c"
    break;

  case 2808:
#line 17987 "parser.y"
                                { yyval = cb_low; }
#line 28642 "parser.c"
    break;

  case 2809:
#line 17994 "parser.y"
  {
	yyval = cb_build_intrinsic (yyvsp[-1], NULL, yyvsp[0], 0);
  }
#line 28650 "parser.c"
    break;

  case 2810:
#line 17998 "parser.y"
  {
	yyval = cb_build_intrinsic (yyvsp[-4], CB_LIST_INIT (yyvsp[-2]), yyvsp[0], 0);
  }
#line 28658 "parser.c"
    break;

  case 2811:
#line 18002 "parser.y"
  {
	yyval = cb_build_intrinsic (yyvsp[-4], yyvsp[-2], yyvsp[0], 0);
  }
#line 28666 "parser.c"
    break;

  case 2812:
#line 18006 "parser.y"
  {
	yyval = cb_build_intrinsic (yyvsp[-4], yyvsp[-2], yyvsp[0], 0);
  }
#line 28674 "parser.c"
    break;

  case 2813:
#line 18010 "parser.y"
  {
	yyval = cb_build_intrinsic (yyvsp[-3], yyvsp[-1], NULL, 0);
  }
#line 28682 "parser.c"
    break;

  case 2814:
#line 18014 "parser.y"
  {
	CB_PENDING (_("PHYSICAL argument for LENGTH functions"));
	yyval = cb_build_intrinsic (yyvsp[-4], yyvsp[-2], NULL, 0);
  }
#line 28691 "parser.c"
    break;

  case 2815:
#line 18019 "parser.y"
  {
	yyval = cb_build_intrinsic (yyvsp[-3], yyvsp[-1], NULL, 0);
  }
#line 28699 "parser.c"
    break;

  case 2816:
#line 18023 "parser.y"
  {
	yyval = cb_build_intrinsic (yyvsp[-4], yyvsp[-2], yyvsp[0], 0);
  }
#line 28707 "parser.c"
    break;

  case 2817:
#line 18027 "parser.y"
  {
	yyval = cb_build_intrinsic (yyvsp[-4], yyvsp[-2], yyvsp[0], 0);
  }
#line 28715 "parser.c"
    break;

  case 2818:
#line 18031 "parser.y"
  {
	yyval = cb_build_intrinsic (yyvsp[-4], yyvsp[-2], yyvsp[0], 0);
  }
#line 28723 "parser.c"
    break;

  case 2819:
#line 18035 "parser.y"
  {
	  yyval = cb_build_intrinsic (yyvsp[-4], yyvsp[-2], yyvsp[0], 0);
  }
#line 28731 "parser.c"
    break;

  case 2820:
#line 18039 "parser.y"
  {
	  yyval = cb_build_intrinsic (yyvsp[-4], yyvsp[-2], yyvsp[0], 0);
  }
#line 28739 "parser.c"
    break;

  case 2821:
#line 18043 "parser.y"
  {
	yyval = cb_build_intrinsic (yyvsp[-1], yyvsp[0], NULL, 0);
  }
#line 28747 "parser.c"
    break;

  case 2822:
#line 18047 "parser.y"
  {
	yyval = cb_build_intrinsic (yyvsp[-1], yyvsp[0], NULL, 1);
  }
#line 28755 "parser.c"
    break;

  case 2834:
#line 18074 "parser.y"
  {
	yyval = NULL;
  }
#line 28763 "parser.c"
    break;

  case 2835:
#line 18078 "parser.y"
  {
	yyval = CB_BUILD_PAIR (yyvsp[-2], NULL);
  }
#line 28771 "parser.c"
    break;

  case 2836:
#line 18082 "parser.y"
  {
	yyval = CB_BUILD_PAIR (yyvsp[-3], yyvsp[-1]);
  }
#line 28779 "parser.c"
    break;

  case 2837:
#line 18089 "parser.y"
  {
	yyval = NULL;
  }
#line 28787 "parser.c"
    break;

  case 2838:
#line 18093 "parser.y"
  {
	yyval = yyvsp[-1];
  }
#line 28795 "parser.c"
    break;

  case 2839:
#line 18097 "parser.y"
  {
	yyval = NULL;
  }
#line 28803 "parser.c"
    break;

  case 2840:
#line 18104 "parser.y"
  {
	cb_tree	x;

	x = CB_LIST_INIT (yyvsp[0]);
	yyval = cb_list_add (x, cb_int0);
  }
#line 28814 "parser.c"
    break;

  case 2841:
#line 18111 "parser.y"
  {
	cb_tree	x;

	x = CB_LIST_INIT (yyvsp[-2]);
	yyval = cb_list_add (x, cb_int1);
  }
#line 28825 "parser.c"
    break;

  case 2842:
#line 18118 "parser.y"
  {
	cb_tree	x;

	x = CB_LIST_INIT (yyvsp[-2]);
	yyval = cb_list_add (x, cb_int2);
  }
#line 28836 "parser.c"
    break;

  case 2843:
#line 18127 "parser.y"
  {
	suppress_data_exceptions = 1;
  }
#line 28844 "parser.c"
    break;

  case 2844:
#line 18131 "parser.y"
  {
	suppress_data_exceptions = 0;
	if (CB_NUMERIC_LITERAL_P(yyvsp[0])) {
		cb_error_x (yyvsp[0], _("a non-numeric literal is expected here"));
		yyval = CB_LIST_INIT (cb_error_node);
	} else {
		yyval = CB_LIST_INIT (yyvsp[0]);
	}
  }
#line 28858 "parser.c"
    break;

  case 2845:
#line 18144 "parser.y"
  {
	cb_tree	x;

	x = CB_LIST_INIT (yyvsp[0]);
	yyval = cb_list_add (x, cb_null);
  }
#line 28869 "parser.c"
    break;

  case 2846:
#line 18151 "parser.y"
  {
	cb_tree	x;

	x = CB_LIST_INIT (yyvsp[-2]);
	yyval = cb_list_add (x, yyvsp[0]);
  }
#line 28880 "parser.c"
    break;

  case 2847:
#line 18161 "parser.y"
  {
	cb_tree	x;

	x = CB_LIST_INIT (yyvsp[0]);
	yyval = cb_list_add (x, cb_null);
  }
#line 28891 "parser.c"
    break;

  case 2848:
#line 18168 "parser.y"
  {
	cb_tree	x;

	x = CB_LIST_INIT (yyvsp[-2]);
	yyval = cb_list_add (x, cb_ref (yyvsp[0]));
  }
#line 28902 "parser.c"
    break;

  case 2849:
#line 18178 "parser.y"
  {
	yyval = cb_list_add (yyvsp[0], cb_int0);
  }
#line 28910 "parser.c"
    break;

  case 2850:
#line 18182 "parser.y"
  {
	const int	num_args = cb_list_length (yyvsp[-2]);

	if (num_args == 4) {
		cb_error_x (yyvsp[-2], _("cannot specify offset and SYSTEM-OFFSET at the same time"));
	}

	yyval = cb_list_add (yyvsp[-2], cb_int1);
  }
#line 28924 "parser.c"
    break;

  case 2851:
#line 18195 "parser.y"
  {
	yyval = cb_list_add (yyvsp[0], cb_int0);
  }
#line 28932 "parser.c"
    break;

  case 2852:
#line 18199 "parser.y"
  {
	const int	num_args = cb_list_length (yyvsp[-2]);

	if (num_args == 3) {
		cb_error_x (yyvsp[-2], _("cannot specify offset and SYSTEM-OFFSET at the same time"));
	}

	yyval = cb_list_add (yyvsp[-2], cb_int1);
  }
#line 28946 "parser.c"
    break;

  case 2853:
#line 18213 "parser.y"
  {
	non_const_word = 1;
  }
#line 28954 "parser.c"
    break;

  case 2854:
#line 18221 "parser.y"
                                { yyval = cb_int0; }
#line 28960 "parser.c"
    break;

  case 2855:
#line 18222 "parser.y"
                                { yyval = cb_int1; }
#line 28966 "parser.c"
    break;

  case 2856:
#line 18226 "parser.y"
                                { yyval = NULL; }
#line 28972 "parser.c"
    break;

  case 2857:
#line 18227 "parser.y"
                        { yyval = cb_int0; }
#line 28978 "parser.c"
    break;

  case 2858:
#line 18228 "parser.y"
                        { yyval = cb_int1; }
#line 28984 "parser.c"
    break;

  case 2859:
#line 18232 "parser.y"
                                { yyval = NULL; }
#line 28990 "parser.c"
    break;

  case 2860:
#line 18233 "parser.y"
                                { yyval = cb_int1; }
#line 28996 "parser.c"
    break;

  case 2861:
#line 18238 "parser.y"
  {
	yyval = NULL;
  }
#line 29004 "parser.c"
    break;

  case 2862:
#line 18242 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 29012 "parser.c"
    break;

  case 2863:
#line 18249 "parser.y"
  {
	yyval = NULL;
  }
#line 29020 "parser.c"
    break;

  case 2864:
#line 18253 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 29028 "parser.c"
    break;

  case 2865:
#line 18260 "parser.y"
                                { yyval = cb_int0; }
#line 29034 "parser.c"
    break;

  case 2866:
#line 18261 "parser.y"
                                { yyval = cb_int1; }
#line 29040 "parser.c"
    break;

  case 2867:
#line 18262 "parser.y"
                                { yyval = cb_int2; }
#line 29046 "parser.c"
    break;

  case 2868:
#line 18266 "parser.y"
                                { yyval = NULL; }
#line 29052 "parser.c"
    break;

  case 2869:
#line 18267 "parser.y"
                                { yyval = cb_true; }
#line 29058 "parser.c"
    break;

  case 2870:
#line 18271 "parser.y"
                                { yyval = cb_int (cb_flag_optional_file); }
#line 29064 "parser.c"
    break;

  case 2871:
#line 18272 "parser.y"
                                { yyval = cb_int1; }
#line 29070 "parser.c"
    break;

  case 2872:
#line 18273 "parser.y"
                                { yyval = cb_int0; }
#line 29076 "parser.c"
    break;

  case 2873:
#line 18278 "parser.y"
  {
	yyval = cb_int0;
  }
#line 29084 "parser.c"
    break;

  case 2874:
#line 18282 "parser.y"
  {
	if (yyvsp[0]) {
		yyval = yyvsp[0];
	} else {
		yyval = default_rounded_mode;
	}
	cobc_cs_check = 0;
  }
#line 29097 "parser.c"
    break;

  case 2875:
#line 18294 "parser.y"
  {
	yyval = NULL;
	cobc_cs_check = 0;
  }
#line 29106 "parser.c"
    break;

  case 2876:
#line 18299 "parser.y"
  {
	yyval = yyvsp[0];
	cobc_cs_check = 0;
  }
#line 29115 "parser.c"
    break;

  case 2877:
#line 18307 "parser.y"
  {
	yyval = cb_int (COB_STORE_ROUND | COB_STORE_AWAY_FROM_ZERO);
  }
#line 29123 "parser.c"
    break;

  case 2878:
#line 18311 "parser.y"
  {
	yyval = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_AWAY_FROM_ZERO);
  }
#line 29131 "parser.c"
    break;

  case 2879:
#line 18315 "parser.y"
  {
	yyval = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_EVEN);
  }
#line 29139 "parser.c"
    break;

  case 2880:
#line 18319 "parser.y"
  {
	yyval = cb_int (COB_STORE_ROUND | COB_STORE_NEAR_TOWARD_ZERO);
  }
#line 29147 "parser.c"
    break;

  case 2881:
#line 18323 "parser.y"
  {
	yyval = cb_int (COB_STORE_ROUND | COB_STORE_PROHIBITED);
  }
#line 29155 "parser.c"
    break;

  case 2882:
#line 18327 "parser.y"
  {
	yyval = cb_int (COB_STORE_ROUND | COB_STORE_TOWARD_GREATER);
  }
#line 29163 "parser.c"
    break;

  case 2883:
#line 18331 "parser.y"
  {
	yyval = cb_int (COB_STORE_ROUND | COB_STORE_TOWARD_LESSER);
  }
#line 29171 "parser.c"
    break;

  case 2884:
#line 18335 "parser.y"
  {
	yyval = cb_int (COB_STORE_ROUND | COB_STORE_TRUNCATION);
  }
#line 29179 "parser.c"
    break;

  case 2885:
#line 18341 "parser.y"
                                { yyval = NULL; }
#line 29185 "parser.c"
    break;

  case 2886:
#line 18342 "parser.y"
                                { yyval = cb_int1; }
#line 29191 "parser.c"
    break;

  case 2887:
#line 18346 "parser.y"
                                { yyval = NULL; }
#line 29197 "parser.c"
    break;

  case 2888:
#line 18348 "parser.y"
  {
	cb_tree	x;

	x = CB_LIST_INIT (yyvsp[-3]);
	yyval = cb_list_add (x, yyvsp[-1]);
  }
#line 29208 "parser.c"
    break;

  case 2889:
#line 18357 "parser.y"
                                { yyval = NULL; }
#line 29214 "parser.c"
    break;

  case 2890:
#line 18359 "parser.y"
  {
	yyval = yyvsp[0];
  }
#line 29222 "parser.c"
    break;

  case 2891:
#line 18368 "parser.y"
  {
	cobc_repeat_last_token = 1;
  }
#line 29230 "parser.c"
    break;

  case 2892:
#line 18372 "parser.y"
  {
	cobc_repeat_last_token = 1;
  }
#line 29238 "parser.c"
    break;

  case 2893:
#line 18376 "parser.y"
  {
	cobc_repeat_last_token = 0;
  }
#line 29246 "parser.c"
    break;

  case 2894:
#line 18380 "parser.y"
  {
	cobc_repeat_last_token = 0;
  }
#line 29254 "parser.c"
    break;


#line 29258 "parser.c"

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
  {
    const int yylhs = yyr1[yyn] - YYNTOKENS;
    const int yyi = yypgoto[yylhs] + *yyssp;
    yystate = (0 <= yyi && yyi <= YYLAST && yycheck[yyi] == *yyssp
               ? yytable[yyi]
               : yydefgoto[yylhs]);
  }

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
            yymsg = YY_CAST (char *, YYSTACK_ALLOC (YY_CAST (YYSIZE_T, yymsg_alloc)));
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
  /* Pacify compilers when the user code never invokes YYERROR and the
     label yyerrorlab therefore never appears in user code.  */
  if (0)
    YYERROR;

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


/*-----------------------------------------------------.
| yyreturn -- parsing is finished, return the result.  |
`-----------------------------------------------------*/
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
                  yystos[+*yyssp], yyvsp);
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
#line 18578 "parser.y"

