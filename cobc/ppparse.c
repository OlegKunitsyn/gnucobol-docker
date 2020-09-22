/* A Bison parser, made by GNU Bison 3.3.2.  */

/* Bison implementation for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2015, 2018-2019 Free Software Foundation,
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
#define YYBISON_VERSION "3.3.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1


/* Substitute the variable and function names.  */
#define yyparse         ppparse
#define yylex           pplex
#define yyerror         pperror
#define yydebug         ppdebug
#define yynerrs         ppnerrs

#define yylval          pplval
#define yychar          ppchar

/* First part of user prologue.  */
#line 34 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:337  */

#include <config.h>

#include <stdio.h>
#include <stdarg.h>
#include <stdlib.h>
#include <string.h>
#include <ctype.h>

#define	COB_IN_PPPARSE	1
#include "cobc.h"
#include "tree.h"

#ifndef	_STDLIB_H
#define	_STDLIB_H 1
#endif

#define pperror(x)	cb_error_always ("%s", x)

#define COND_EQ		0
#define COND_LT		1U
#define COND_GT		2U
#define COND_LE		3U
#define COND_GE		4U
#define COND_NE		5U

/* Global variables */

int				current_call_convention;

/* Local variables */

static struct cb_define_struct	*ppp_setvar_list = NULL;
static unsigned int		current_cmd = 0;

/* Local functions */

static char *
fix_filename (char *name)
{
	/* remove quotation from alphanumeric literals */
	if (name[0] == '\'' || name[0] == '\"') {
		name++;
		name[strlen (name) - 1] = 0;
	}
	return name;
}

static char *
fold_lower (char *name)
{
	unsigned char	*p;

	for (p = (unsigned char *)name; *p; p++) {
		if (isupper (*p)) {
			*p = (cob_u8_t)tolower (*p);
		}
	}
	return name;
}

static char *
fold_upper (char *name)
{
	unsigned char	*p;

	for (p = (unsigned char *)name; *p; p++) {
		if (islower (*p)) {
			*p = (cob_u8_t)toupper (*p);
		}
	}
	return name;
}

static struct cb_replace_list *
ppp_replace_list_add (struct cb_replace_list *list,
		     const struct cb_text_list *old_text,
		     const struct cb_text_list *new_text,
		     const unsigned int lead_or_trail)
{
	struct cb_replace_list *p;

	p = cobc_plex_malloc (sizeof (struct cb_replace_list));
	p->line_num = cb_source_line;
	p->old_text = old_text;
	p->new_text = new_text;
	p->lead_trail = lead_or_trail;
	if (!list) {
		p->last = p;
		return p;
	}
	list->last->next = p;
	list->last = p;
	return list;
}

static unsigned int
ppp_set_value (struct cb_define_struct *p, const char *value)
{
	const char	*s;
	size_t		size;
	unsigned int	dotseen;
	int		sign;
	int		int_part;
	int		dec_part;

	if (!value) {
		p->deftype = PLEX_DEF_NONE;
		p->value = NULL;
		p->sign = 0;
		p->int_part = 0;
		p->dec_part = 0;
		return 0;
	}

	if (*value == '"' || *value == '\'') {
		sign = *value;
		p->value = cobc_plex_strdup (value + 1);
		size = strlen (p->value) - 1;
		if (sign != p->value[size]) {
			p->value = NULL;
			p->deftype = PLEX_DEF_NONE;
			return 1;
		}
		p->value[size] = 0;
		p->deftype = PLEX_DEF_LIT;
		p->sign = 0;
		p->int_part = 0;
		p->dec_part = 0;
		return 0;
	}

	p->value = cobc_plex_strdup (value);
	p->deftype = PLEX_DEF_NUM;
	p->sign = 0;
	p->int_part = 0;
	p->dec_part = 0;

	sign = 0;
	if (*value == '+') {
		value++;
	} else if (*value == '-') {
		value++;
		sign = 1;
	}
	int_part = 0;
	dec_part = 0;
	size = 0;
	dotseen = 0;
	s = value;
	for ( ; *s; ++s, ++size) {
		if (*s == '.') {
			if (dotseen) {
				p->deftype = PLEX_DEF_NONE;
				return 1;
			}
			dotseen = 1;
			continue;
		}
		if (*s > '9' || *s < '0') {
			p->deftype = PLEX_DEF_NONE;
			return 1;
		}
		if (!dotseen) {
			int_part = (int_part * 10) + (*s - '0');
		} else {
			dec_part = (dec_part * 10) + (*s - '0');
		}
	}

	if (!int_part && !dec_part) {
		sign = 0;
	}
	p->sign = sign;
	p->int_part = int_part;
	p->dec_part = dec_part;
	return 0;
}

static unsigned int
ppp_compare_vals (const struct cb_define_struct *p1,
		 const struct cb_define_struct *p2,
		 const unsigned int cond)
{
	int	result;

	if (!p1 || !p2) {
		return 0;
	}
	if (p1->deftype != PLEX_DEF_LIT && p1->deftype != PLEX_DEF_NUM) {
		return 0;
	}
	if (p2->deftype != PLEX_DEF_LIT && p2->deftype != PLEX_DEF_NUM) {
		return 0;
	}
	if (p1->deftype != p2->deftype) {
		cb_warning (COBC_WARN_FILLER, _("directive comparison on different types"));
		return 0;
	}
	if (p1->deftype == PLEX_DEF_LIT) {
		result = strcmp (p1->value, p2->value);
	} else {
		if (p1->sign && !p2->sign) {
			result = -1;
		} else if (!p1->sign && p2->sign) {
			result = 1;
		} else if (p1->int_part < p2->int_part) {
			if (p1->sign) {
				result = 1;
			} else {
				result = -1;
			}
		} else if (p1->int_part > p2->int_part) {
			if (p1->sign) {
				result = -1;
			} else {
				result = 1;
			}
		} else if (p1->dec_part < p2->dec_part) {
			if (p1->sign) {
				result = 1;
			} else {
				result = -1;
			}
		} else if (p1->dec_part > p2->dec_part) {
			if (p1->sign) {
				result = -1;
			} else {
				result = 1;
			}
		} else {
			result = 0;
		}
	}
	switch (cond) {
	case COND_EQ:
		return (result == 0);
	case COND_LT:
		return (result < 0);
	case COND_GT:
		return (result > 0);
	case COND_LE:
		return (result <= 0);
	case COND_GE:
		return (result >= 0);
	case COND_NE:
		return (result != 0);
	default:
		break;
	}
	return 0;
}

static struct cb_define_struct *
ppp_define_add (struct cb_define_struct *list, const char *name,
		const char *text, const unsigned int override)
{
	struct cb_define_struct	*p;
	struct cb_define_struct	*l;

	/* Check duplicate */
	for (l = list; l; l = l->next) {
		if (!strcasecmp (name, l->name)) {
			if (!override && l->deftype != PLEX_DEF_DEL) {
				cb_error (_("duplicate DEFINE directive '%s'"), name);
				return NULL;
			}
			if (l->value) {
				l->value = NULL;
			}
			if (ppp_set_value (l, text)) {
				cb_error (_("invalid constant in DEFINE directive"));
				return NULL;
			}
			return list;
		}
	}

	p = cobc_plex_malloc (sizeof (struct cb_define_struct));
	p->name = cobc_plex_strdup (name);
	if (ppp_set_value (p, text)) {
		cb_error (_("invalid constant in DEFINE directive"));
		return NULL;
	}

	if (!list) {
		p->last = p;
		return p;
	}
	list->last->next = p;
	list->last = p;
	return list;
}

static void
ppp_define_del (const char *name)
{
	struct cb_define_struct	*l;

	for (l = ppp_setvar_list; l; l = l->next) {
		if (!strcmp (name, l->name)) {
			l->deftype = PLEX_DEF_DEL;
			if (l->value) {
				l->value = NULL;
			}
			l->sign = 0;
			l->int_part = 0;
			l->dec_part = 0;
			break;
		}
	}
}

void
ppp_clear_lists (void)
{
	ppp_setvar_list = NULL;
}

struct cb_define_struct *
ppp_search_lists (const char *name)
{
	struct cb_define_struct	*p;

	for (p = ppp_setvar_list; p; p = p->next) {
		if (p->name == NULL) {
			continue;
		}
		if (!strcasecmp (name, p->name)) {
			if (p->deftype != PLEX_DEF_DEL) {
				return p;
			}
			break;
		}
	}
	return NULL;
}

static struct cb_text_list *
ppp_list_add (struct cb_text_list *list, const char *text)
{
	struct cb_text_list	*p;

	p = cobc_plex_malloc (sizeof (struct cb_text_list));
	p->text = cobc_plex_strdup (text);
	if (!list) {
		p->last = p;
		return p;
	}
	list->last->next = p;
	list->last = p;
	return list;
}

static struct cb_text_list *
ppp_list_append (struct cb_text_list *list_1, struct cb_text_list *list_2)
{
	struct cb_text_list	*list_1_end;

	if (!list_1) {
		return list_2;
	}

	for (list_1_end = list_1;
	     list_1_end->next;
	     list_1_end = list_1_end->next);
	list_1_end->next = list_2;
	list_2->last = list_1_end;

	return list_1;
}

static unsigned int
ppp_search_comp_vars (const char *name)
{
#undef	CB_PARSE_DEF
#define	CB_PARSE_DEF(x,z)	if (!strcasecmp (name, x)) return (z);
#include "ppparse.def"
#undef	CB_PARSE_DEF
	cb_warning (COBC_WARN_FILLER, _("compiler flag '%s' unknown"), name);
	return 0;
}

static unsigned int
ppp_check_needs_quote (const char *envval)
{
	const char	*s;
	size_t		size;
	unsigned int	dot_seen;
	unsigned int	sign_seen;

	/* Non-quoted value - Check if possible numeric */
	dot_seen = 0;
	sign_seen = 0;
	size = 0;
	s = envval;
	if (*s == '+' || *s == '-') {
		sign_seen = 1;
		size++;
		s++;
	}
	for (; *s; ++s) {
		if (*s == '.') {
			if (dot_seen) {
				break;
			}
			dot_seen = 1;
			size++;
			continue;
		}
		if (*s > '9' || *s < '0') {
			break;
		}
		size++;
	}

	if (*s || size <= ((size_t)dot_seen + sign_seen)) {
		return 1;
	}
	return 0;
}

static void
ppp_error_invalid_option (const char *directive, const char *option)
{
	cb_error (_("invalid %s directive option '%s'"), directive, option);
}

static void
append_to_turn_list (struct cb_text_list *ec_names, int enable, int with_location)
{
	struct cb_turn_list	*l;
	struct cb_turn_list	*turn_list_end;

	/* Add turn directive data to end of cb_turn_list */
	l = cobc_plex_malloc (sizeof (struct cb_turn_list));
	l->ec_names = ec_names;
	l->enable = enable;
	l->with_location = with_location;
	l->next = NULL;
	/* The line number is set properly in the scanner */
	l->line = -1;
	
	if (cb_turn_list) {
		for (turn_list_end = cb_turn_list;
		     turn_list_end->next;
		     turn_list_end = turn_list_end->next);
		turn_list_end->next = l;
	} else {
		cb_turn_list = l;
	}

	/*
	  Output #TURN so we can assign a line number to this data later in the
	  scanner.
	*/
	fprintf (ppout, "#TURN\n");
}

/* Global functions */

void
ppparse_clear_vars (const struct cb_define_struct *p)
{
	const struct cb_define_struct	*q;

	ppp_setvar_list = NULL;
	/* Set standard DEFINE's */
	if (cb_perform_osvs) {
		ppp_setvar_list = ppp_define_add (ppp_setvar_list,
						  "PERFORM-TYPE",
						  "'OSVS'", 0);
	} else {
		ppp_setvar_list = ppp_define_add (ppp_setvar_list,
						  "PERFORM-TYPE",
						  "'MF'", 0);
	}
	if (cb_ebcdic_sign) {
		ppp_setvar_list = ppp_define_add (ppp_setvar_list,
						  "SIGN",
						  "'EBCDIC'", 0);
	} else {
		ppp_setvar_list = ppp_define_add (ppp_setvar_list,
						  "SIGN",
						  "'ASCII'", 0);
	}
#ifdef	WORDS_BIGENDIAN
	ppp_setvar_list = ppp_define_add (ppp_setvar_list,
					  "ENDIAN",
					  "'BIG'", 0);
#else
	ppp_setvar_list = ppp_define_add (ppp_setvar_list,
					  "ENDIAN",
					  "'LITTLE'", 0);
#endif
#if	' ' == 0x20
	ppp_setvar_list = ppp_define_add (ppp_setvar_list,
					  "CHARSET",
					  "'ASCII'", 0);
#elif	' ' == 0x40
	ppp_setvar_list = ppp_define_add (ppp_setvar_list,
					  "CHARSET",
					  "'EBCDIC'", 0);
#else
	ppp_setvar_list = ppp_define_add (ppp_setvar_list,
					  "CHARSET",
					  "'UNKNOWN'", 0);
#endif
	/* Set DEFINE's from '-D' option(s) */
	for (q = p; q; q = q->next) {
		ppp_setvar_list = ppp_define_add (ppp_setvar_list,
						  q->name,
						  q->value, 0);
	}
	/* reset CALL CONVENTION */
	current_call_convention = CB_CONV_COBOL;
}


#line 598 "ppparse.c" /* yacc.c:337  */
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

/* In a future release of Bison, this section will be replaced
   by #include "y.tab.h".  */
#ifndef YY_PP_PPPARSE_H_INCLUDED
# define YY_PP_PPPARSE_H_INCLUDED
/* Debug traces.  */
#ifndef YYDEBUG
# define YYDEBUG 0
#endif
#if YYDEBUG
extern int ppdebug;
#endif

/* Token type.  */
#ifndef YYTOKENTYPE
# define YYTOKENTYPE
  enum yytokentype
  {
    TOKEN_EOF = 0,
    ALSO = 258,
    BY = 259,
    COPY = 260,
    EQEQ = 261,
    IN = 262,
    LAST = 263,
    LEADING = 264,
    OF = 265,
    OFF = 266,
    PRINTING = 267,
    REPLACE = 268,
    REPLACING = 269,
    SUPPRESS = 270,
    TRAILING = 271,
    DOT = 272,
    GARBAGE = 273,
    LISTING_DIRECTIVE = 274,
    LISTING_STATEMENT = 275,
    TITLE_STATEMENT = 276,
    CONTROL_STATEMENT = 277,
    SOURCE = 278,
    NOSOURCE = 279,
    LIST = 280,
    NOLIST = 281,
    MAP = 282,
    NOMAP = 283,
    LEAP_SECOND_DIRECTIVE = 284,
    SOURCE_DIRECTIVE = 285,
    FORMAT = 286,
    IS = 287,
    FIXED = 288,
    FREE = 289,
    VARIABLE = 290,
    CALL_DIRECTIVE = 291,
    COBOL = 292,
    TOK_EXTERN = 293,
    STDCALL = 294,
    STATIC = 295,
    DEFINE_DIRECTIVE = 296,
    AS = 297,
    PARAMETER = 298,
    OVERRIDE = 299,
    SET_DIRECTIVE = 300,
    ADDRSV = 301,
    ADDSYN = 302,
    ASSIGN = 303,
    BOUND = 304,
    CALLFH = 305,
    COMP1 = 306,
    CONSTANT = 307,
    FOLDCOPYNAME = 308,
    MAKESYN = 309,
    NOBOUND = 310,
    NOFOLDCOPYNAME = 311,
    NOSSRANGE = 312,
    REMOVE = 313,
    SOURCEFORMAT = 314,
    SSRANGE = 315,
    IF_DIRECTIVE = 316,
    ELSE_DIRECTIVE = 317,
    ENDIF_DIRECTIVE = 318,
    ELIF_DIRECTIVE = 319,
    GE = 320,
    LE = 321,
    LT = 322,
    GT = 323,
    EQ = 324,
    NE = 325,
    NOT = 326,
    THAN = 327,
    TO = 328,
    OR = 329,
    EQUAL = 330,
    GREATER = 331,
    LESS = 332,
    SET = 333,
    DEFINED = 334,
    TURN_DIRECTIVE = 335,
    ON = 336,
    CHECKING = 337,
    WITH = 338,
    LOCATION = 339,
    TERMINATOR = 340,
    TOKEN = 341,
    TEXT_NAME = 342,
    VARIABLE_NAME = 343,
    LITERAL = 344
  };
#endif
/* Tokens.  */
#define TOKEN_EOF 0
#define ALSO 258
#define BY 259
#define COPY 260
#define EQEQ 261
#define IN 262
#define LAST 263
#define LEADING 264
#define OF 265
#define OFF 266
#define PRINTING 267
#define REPLACE 268
#define REPLACING 269
#define SUPPRESS 270
#define TRAILING 271
#define DOT 272
#define GARBAGE 273
#define LISTING_DIRECTIVE 274
#define LISTING_STATEMENT 275
#define TITLE_STATEMENT 276
#define CONTROL_STATEMENT 277
#define SOURCE 278
#define NOSOURCE 279
#define LIST 280
#define NOLIST 281
#define MAP 282
#define NOMAP 283
#define LEAP_SECOND_DIRECTIVE 284
#define SOURCE_DIRECTIVE 285
#define FORMAT 286
#define IS 287
#define FIXED 288
#define FREE 289
#define VARIABLE 290
#define CALL_DIRECTIVE 291
#define COBOL 292
#define TOK_EXTERN 293
#define STDCALL 294
#define STATIC 295
#define DEFINE_DIRECTIVE 296
#define AS 297
#define PARAMETER 298
#define OVERRIDE 299
#define SET_DIRECTIVE 300
#define ADDRSV 301
#define ADDSYN 302
#define ASSIGN 303
#define BOUND 304
#define CALLFH 305
#define COMP1 306
#define CONSTANT 307
#define FOLDCOPYNAME 308
#define MAKESYN 309
#define NOBOUND 310
#define NOFOLDCOPYNAME 311
#define NOSSRANGE 312
#define REMOVE 313
#define SOURCEFORMAT 314
#define SSRANGE 315
#define IF_DIRECTIVE 316
#define ELSE_DIRECTIVE 317
#define ENDIF_DIRECTIVE 318
#define ELIF_DIRECTIVE 319
#define GE 320
#define LE 321
#define LT 322
#define GT 323
#define EQ 324
#define NE 325
#define NOT 326
#define THAN 327
#define TO 328
#define OR 329
#define EQUAL 330
#define GREATER 331
#define LESS 332
#define SET 333
#define DEFINED 334
#define TURN_DIRECTIVE 335
#define ON 336
#define CHECKING 337
#define WITH 338
#define LOCATION 339
#define TERMINATOR 340
#define TOKEN 341
#define TEXT_NAME 342
#define VARIABLE_NAME 343
#define LITERAL 344

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 554 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:352  */

	char			*s;
	struct cb_text_list	*l;
	struct cb_replace_list	*r;
	struct cb_define_struct	*ds;
	unsigned int		ui;
	int			si;

#line 830 "ppparse.c" /* yacc.c:352  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE pplval;

int ppparse (void);

#endif /* !YY_PP_PPPARSE_H_INCLUDED  */



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
typedef unsigned short yytype_uint16;
#endif

#ifdef YYTYPE_INT16
typedef YYTYPE_INT16 yytype_int16;
#else
typedef short yytype_int16;
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
#  define YYSIZE_T unsigned
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

/* Suppress unused-variable warnings by "using" E.  */
#if ! defined lint || defined __GNUC__
# define YYUSE(E) ((void) (E))
#else
# define YYUSE(E) /* empty */
#endif

#if defined __GNUC__ && ! defined __ICC && 407 <= __GNUC__ * 100 + __GNUC_MINOR__
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
#define YYFINAL  2
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   221

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  92
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  60
/* YYNRULES -- Number of rules.  */
#define YYNRULES  170
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  248

#define YYUNDEFTOK  2
#define YYMAXUTOK   344

/* YYTRANSLATE(TOKEN-NUM) -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, with out-of-bounds checking.  */
#define YYTRANSLATE(YYX)                                                \
  ((unsigned) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      90,    91,     2,     2,     2,     2,     2,     2,     2,     2,
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
      85,    86,    87,    88,    89
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   698,   698,   699,   703,   704,   705,   706,   707,   714,
     715,   716,   717,   718,   719,   721,   720,   726,   725,   730,
     734,   739,   738,   751,   752,   756,   767,   768,   776,   784,
     802,   807,   817,   821,   839,   857,   861,   866,   870,   878,
     886,   894,   920,   956,   960,   967,   968,   975,   984,   987,
     994,  1003,  1008,  1012,  1017,  1025,  1026,  1030,  1039,  1073,
    1077,  1093,  1100,  1103,  1104,  1108,  1109,  1113,  1114,  1118,
    1119,  1120,  1121,  1122,  1123,  1126,  1127,  1130,  1132,  1136,
    1140,  1147,  1151,  1158,  1162,  1166,  1173,  1174,  1178,  1179,
    1183,  1184,  1188,  1193,  1198,  1203,  1210,  1217,  1224,  1234,
    1249,  1256,  1257,  1261,  1274,  1288,  1292,  1296,  1300,  1304,
    1308,  1312,  1316,  1320,  1324,  1328,  1335,  1343,  1352,  1365,
    1368,  1375,  1376,  1379,  1380,  1385,  1388,  1395,  1399,  1406,
    1410,  1414,  1418,  1425,  1429,  1436,  1440,  1444,  1451,  1458,
    1462,  1469,  1473,  1480,  1484,  1491,  1498,  1513,  1517,  1525,
    1529,  1539,  1542,  1550,  1553,  1561,  1564,  1572,  1575,  1581,
    1581,  1582,  1582,  1583,  1583,  1584,  1584,  1585,  1585,  1586,
    1586
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 1
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "\"end of file\"", "error", "$undefined", "ALSO", "BY", "COPY",
  "\"==\"", "IN", "LAST", "LEADING", "OF", "OFF", "PRINTING", "REPLACE",
  "REPLACING", "SUPPRESS", "TRAILING", "\".\"", "\"word\"",
  "LISTING_DIRECTIVE", "LISTING_STATEMENT", "TITLE_STATEMENT",
  "CONTROL_STATEMENT", "SOURCE", "NOSOURCE", "LIST", "NOLIST", "MAP",
  "NOMAP", "LEAP_SECOND_DIRECTIVE", "SOURCE_DIRECTIVE", "FORMAT", "IS",
  "FIXED", "FREE", "VARIABLE", "CALL_DIRECTIVE", "COBOL", "\"EXTERN\"",
  "STDCALL", "STATIC", "DEFINE_DIRECTIVE", "AS", "PARAMETER", "OVERRIDE",
  "SET_DIRECTIVE", "ADDRSV", "ADDSYN", "ASSIGN", "BOUND", "CALLFH",
  "COMP1", "CONSTANT", "FOLDCOPYNAME", "MAKESYN", "NOBOUND",
  "NOFOLDCOPYNAME", "NOSSRANGE", "REMOVE", "SOURCEFORMAT", "SSRANGE",
  "IF_DIRECTIVE", "ELSE_DIRECTIVE", "ENDIF_DIRECTIVE", "ELIF_DIRECTIVE",
  "\">=\"", "\"<=\"", "\"<\"", "\">\"", "\"=\"", "\"<>\"", "NOT", "THAN",
  "TO", "OR", "EQUAL", "GREATER", "LESS", "SET", "DEFINED",
  "TURN_DIRECTIVE", "ON", "CHECKING", "WITH", "LOCATION",
  "\"end of line\"", "\"Identifier or Literal\"", "\"Text-Name\"",
  "\"Variable\"", "\"Literal\"", "'('", "')'", "$accept", "statement_list",
  "statement", "directive", "$@1", "$@2", "$@3", "set_directive",
  "set_choice", "alnum_list", "alnum_equality_list", "alnum_equality",
  "set_options", "source_directive", "format_type", "_literal",
  "define_directive", "listing_directive", "listing_statement",
  "control_options", "control_option", "_dot", "leap_second_directive",
  "turn_directive", "ec_list", "on_or_off", "on_with_loc", "with_loc",
  "call_directive", "call_choice", "if_directive", "variable_or_literal",
  "object_id", "condition_clause", "copy_statement", "copy_source",
  "copy_in", "in_or_of", "copy_suppress", "copy_replacing",
  "replace_statement", "replacing_list", "text_src", "text_dst",
  "text_partial_src", "text_partial_dst", "token_list", "identifier",
  "subscripts", "lead_trail", "_override", "_not", "_also", "_last", "_as",
  "_format", "_is", "_printing", "_than", "_to", YY_NULLPTR
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
      40,    41
};
# endif

#define YYPACT_NINF -140

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-140)))

#define YYTABLE_NINF -160

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -140,     3,  -140,     1,    35,    19,  -140,   -42,   141,    20,
      18,  -140,   -18,    72,  -140,  -140,  -140,  -140,   -36,  -140,
     -31,  -140,    40,    52,  -140,  -140,    46,  -140,  -140,    -2,
      48,  -140,  -140,  -140,    55,  -140,  -140,  -140,  -140,  -140,
    -140,    85,  -140,  -140,  -140,  -140,  -140,  -140,    31,    42,
      29,   -25,  -140,  -140,  -140,    14,    53,    14,    59,  -140,
      60,    61,    69,    47,    14,  -140,  -140,  -140,    53,    47,
      70,   -14,    72,  -140,   -48,   -48,  -140,  -140,   -46,  -140,
    -140,  -140,  -140,  -140,   126,     1,    84,  -140,  -140,  -140,
      -2,   154,    -5,   165,  -140,  -140,    87,  -140,    88,  -140,
      43,  -140,  -140,  -140,  -140,    42,  -140,    47,  -140,     8,
     105,    14,  -140,  -140,    89,  -140,  -140,  -140,  -140,    90,
      91,  -140,    89,    92,  -140,  -140,  -140,    93,  -140,   -23,
     -17,  -140,  -140,  -140,    15,  -140,   163,   162,  -140,  -140,
       0,   173,   165,     4,    97,    98,    99,   100,   183,  -140,
    -140,  -140,  -140,  -140,  -140,  -140,  -140,   101,  -140,   144,
     144,   102,  -140,  -140,  -140,  -140,  -140,  -140,   118,   118,
    -140,    11,   108,  -140,  -140,  -140,  -140,  -140,  -140,    -2,
    -140,  -140,  -140,     4,   189,     5,  -140,    -5,  -140,  -140,
    -140,   -41,   188,   190,   144,  -140,  -140,  -140,  -140,  -140,
      68,    86,  -140,  -140,    -2,  -140,   190,  -140,     6,  -140,
    -140,  -140,     7,  -140,  -140,  -140,  -140,  -140,  -140,  -140,
    -140,   122,   125,   125,  -140,  -140,    16,    16,  -140,  -140,
    -140,   192,  -140,  -140,  -140,   127,   128,  -140,  -140,  -140,
    -140,  -140,   124,   129,   122,   122,  -140,  -140
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,     0,     1,     0,   155,    62,    65,     0,     0,    77,
     161,    21,     0,     0,    15,    19,    20,    17,     0,     3,
       0,     7,     0,     0,   117,   118,   119,   156,   158,     0,
       0,    64,    63,    13,    75,    69,    70,    71,    72,    73,
      74,    75,    67,    79,    78,    14,   162,     9,   163,     0,
       0,   159,   102,    10,    61,     0,     0,     0,     0,    30,
      32,     0,     0,   159,     0,    36,    37,    38,     0,   159,
      55,    48,    11,    23,     0,     0,    81,    12,     0,     6,
       4,     5,   121,   122,   123,     0,     0,   149,   150,   143,
     127,     0,   134,     0,   128,    76,     0,    68,     0,   164,
       0,    92,    93,    94,    95,    22,    90,   159,   160,     0,
       0,    39,    45,    43,    27,    28,    29,    31,    33,     0,
       0,    35,    40,     0,    56,    42,    26,     0,    24,   163,
     163,    16,   100,    18,     0,    82,   165,   125,   120,   141,
       0,     0,     0,     0,     0,     0,     0,     0,     0,    66,
       8,    54,    51,    52,    53,    50,    91,     0,    59,   151,
     151,     0,    46,    44,    25,    34,    41,    49,   153,   153,
      85,    84,     0,    89,    80,    83,    87,   166,   124,     0,
     116,   133,   142,     0,     0,     0,   129,   137,   144,   145,
     147,     0,     0,     0,   151,   152,    58,    57,    47,   154,
       0,     0,    86,    88,   126,   131,     0,   135,     0,   148,
     146,   138,     0,   130,    60,   110,   112,   113,   111,   114,
     115,   169,   167,   167,    97,    96,     0,     0,   132,   136,
     139,     0,   170,   109,   168,   106,   108,   104,   103,    98,
      99,   140,     0,     0,   169,   169,   105,   107
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -140,  -140,  -140,  -140,  -140,  -140,  -140,  -140,   131,   132,
    -140,   -37,  -140,  -140,  -140,  -140,  -140,  -140,  -140,  -140,
     164,   166,  -140,  -140,  -140,  -140,  -140,    37,  -140,   104,
     135,   194,   -16,    12,  -140,   130,  -140,  -140,  -140,  -140,
    -140,    33,   -90,    34,    74,    13,    36,  -125,  -140,   -89,
    -139,    45,  -140,  -140,   -34,  -140,    10,  -140,    -3,  -138
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,    19,    20,    74,    75,    49,    72,    73,   114,
     111,   112,   126,    47,   155,   125,    53,    33,    21,    41,
      42,    96,    45,    77,    78,   174,   175,   176,   105,   106,
     131,   132,   239,   226,    22,    26,    84,    85,   137,   180,
      23,    90,    91,   186,   148,   213,   140,    92,   191,    93,
     196,   200,    29,    30,   109,    48,   100,   178,   235,   233
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     141,   142,   144,     2,    86,   145,   181,    87,     3,    99,
     185,   207,   229,   230,    88,    99,     4,   108,   187,   158,
     115,   197,     5,     6,     7,     8,   170,   121,   108,   120,
      31,    43,     9,    10,    50,   123,   134,   127,    27,    11,
     129,   130,   135,    28,    12,   209,  -157,    34,    13,    46,
     210,   159,    76,    82,    79,   214,    83,    80,   187,    94,
    -101,   151,  -101,    99,    14,    15,    16,    17,  -102,    81,
      51,    52,    95,   157,   162,  -159,   152,   153,   154,   101,
     102,   103,   104,    18,    89,   146,   182,    24,    25,   108,
      89,   139,   182,   231,   172,   173,   171,   160,   172,   173,
      32,    44,    95,   110,   237,   238,   246,   247,    35,    36,
      37,    38,    39,    40,   141,   142,    55,   107,    56,    57,
      58,    59,    60,    61,    62,    63,    64,    65,    66,    67,
      68,    69,    70,   215,   216,   217,   218,   219,   220,   168,
     169,   136,   113,   221,   222,   223,   224,   225,   116,   117,
     118,   215,   216,   217,   218,   219,   220,   119,   143,   124,
      71,   221,   222,   223,    35,    36,    37,    38,    39,    40,
     139,   147,   149,   150,   161,   177,   179,   183,   163,   164,
     165,   166,   167,   188,   189,   190,   192,   193,   195,   199,
     194,   198,   203,   206,   211,   232,   212,   234,   241,   244,
     122,   242,   243,   128,   245,    97,    54,    98,   202,   156,
     133,   240,   204,   227,   201,   138,   184,   205,     0,   228,
     236,   208
};

static const yytype_int16 yycheck[] =
{
      90,    90,     7,     0,     6,    10,     6,     9,     5,    32,
       6,     6,     6,     6,    16,    32,    13,    42,   143,    11,
      57,   160,    19,    20,    21,    22,    11,    64,    42,    63,
      11,    11,    29,    30,    52,    69,    82,    71,     3,    36,
      88,    89,    88,     8,    41,    86,    11,    89,    45,    31,
      91,    43,    88,     7,    85,   194,    10,    17,   183,    11,
      85,    18,    85,    32,    61,    62,    63,    64,    85,    17,
      88,    89,    17,   107,   111,    89,    33,    34,    35,    37,
      38,    39,    40,    80,    86,    90,    86,    86,    87,    42,
      86,    86,    86,    86,    83,    84,    81,    89,    83,    84,
      81,    81,    17,    89,    88,    89,   244,   245,    23,    24,
      25,    26,    27,    28,   204,   204,    44,    88,    46,    47,
      48,    49,    50,    51,    52,    53,    54,    55,    56,    57,
      58,    59,    60,    65,    66,    67,    68,    69,    70,   129,
     130,    15,    89,    75,    76,    77,    78,    79,    89,    89,
      89,    65,    66,    67,    68,    69,    70,    88,     4,    89,
      88,    75,    76,    77,    23,    24,    25,    26,    27,    28,
      86,     6,    85,    85,    69,    12,    14,     4,    89,    89,
      89,    89,    89,    86,    86,    86,    86,     4,    44,    71,
      89,    89,    84,     4,     6,    73,     6,    72,     6,    75,
      68,    74,    74,    72,    75,    41,    12,    41,   171,   105,
      75,   227,   179,   201,   169,    85,   142,   183,    -1,   206,
     223,   185
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    93,     0,     5,    13,    19,    20,    21,    22,    29,
      30,    36,    41,    45,    61,    62,    63,    64,    80,    94,
      95,   110,   126,   132,    86,    87,   127,     3,     8,   144,
     145,    11,    81,   109,    89,    23,    24,    25,    26,    27,
      28,   111,   112,    11,    81,   114,    31,   105,   147,    98,
      52,    88,    89,   108,   123,    44,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    56,    57,    58,    59,
      60,    88,    99,   100,    96,    97,    88,   115,   116,    85,
      17,    17,     7,    10,   128,   129,     6,     9,    16,    86,
     133,   134,   139,   141,    11,    17,   113,   112,   113,    32,
     148,    37,    38,    39,    40,   120,   121,    88,    42,   146,
      89,   102,   103,    89,   101,   103,    89,    89,    89,    88,
     146,   103,   101,   146,    89,   107,   104,   146,   100,    88,
      89,   122,   123,   122,    82,    88,    15,   130,   127,    86,
     138,   134,   141,     4,     7,    10,    90,     6,   136,    85,
      85,    18,    33,    34,    35,   106,   121,   146,    11,    43,
      89,    69,   103,    89,    89,    89,    89,    89,   148,   148,
      11,    81,    83,    84,   117,   118,   119,    12,   149,    14,
     131,     6,    86,     4,   136,     6,   135,   139,    86,    86,
      86,   140,    86,     4,    89,    44,   142,   142,    89,    71,
     143,   143,   119,    84,   133,   135,     4,     6,   138,    86,
      91,     6,     6,   137,   142,    65,    66,    67,    68,    69,
      70,    75,    76,    77,    78,    79,   125,   125,   137,     6,
       6,    86,    73,   151,    72,   150,   150,    88,    89,   124,
     124,     6,    74,    74,    75,    75,   151,   151
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    92,    93,    93,    94,    94,    94,    94,    94,    95,
      95,    95,    95,    95,    95,    96,    95,    97,    95,    95,
      95,    98,    95,    99,    99,   100,   100,   100,   100,   100,
     100,   100,   100,   100,   100,   100,   100,   100,   100,   100,
     100,   100,   100,   101,   101,   102,   102,   103,   104,   104,
     105,   106,   106,   106,   106,   107,   107,   108,   108,   108,
     108,   108,   109,   109,   109,   110,   110,   111,   111,   112,
     112,   112,   112,   112,   112,   113,   113,   114,   114,   114,
     115,   116,   116,   117,   117,   117,   118,   118,   119,   119,
     120,   120,   121,   121,   121,   121,   122,   122,   122,   122,
     122,   123,   123,   124,   124,   125,   125,   125,   125,   125,
     125,   125,   125,   125,   125,   125,   126,   127,   127,   128,
     128,   129,   129,   130,   130,   131,   131,   132,   132,   133,
     133,   133,   133,   134,   134,   135,   135,   135,   136,   137,
     137,   138,   138,   139,   139,   139,   139,   140,   140,   141,
     141,   142,   142,   143,   143,   144,   144,   145,   145,   146,
     146,   147,   147,   148,   148,   149,   149,   150,   150,   151,
     151
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     2,     2,     2,     1,     4,     2,
       2,     2,     2,     2,     2,     0,     3,     0,     3,     1,
       1,     0,     3,     1,     2,     3,     2,     2,     2,     2,
       1,     2,     1,     2,     3,     2,     1,     1,     1,     2,
       2,     3,     2,     1,     2,     1,     2,     3,     0,     2,
       3,     1,     1,     1,     1,     0,     1,     4,     4,     3,
       5,     1,     0,     1,     1,     1,     4,     1,     2,     1,
       1,     1,     1,     1,     1,     0,     1,     0,     1,     1,
       3,     1,     2,     1,     1,     1,     2,     1,     2,     1,
       1,     2,     1,     1,     1,     1,     4,     4,     5,     5,
       1,     1,     1,     1,     1,     5,     2,     5,     2,     2,
       1,     1,     1,     1,     1,     1,     5,     1,     1,     0,
       2,     1,     1,     0,     2,     0,     2,     3,     3,     3,
       4,     4,     5,     3,     1,     2,     3,     1,     3,     2,
       3,     1,     2,     1,     3,     3,     4,     1,     2,     1,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1
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
  YYUSE (yytype);
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
  unsigned long yylno = yyrline[yyrule];
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

  if (! yyres)
    return yystrlen (yystr);

  return (YYSIZE_T) (yystpcpy (yyres, yystr) - yyres);
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
    YYSIZE_T yysize1 = yysize + yystrlen (yyformat);
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
| yynewstate -- push a new state, which is found in yystate.  |
`------------------------------------------------------------*/
yynewstate:
  /* In all cases, when you get here, the value and location stacks
     have just been pushed.  So pushing a state here evens the stacks.  */
  yyssp++;


/*--------------------------------------------------------------------.
| yynewstate -- set current state (the top of the stack) to yystate.  |
`--------------------------------------------------------------------*/
yysetstate:
  *yyssp = (yytype_int16) yystate;

  if (yyss + yystacksize - 1 <= yyssp)
#if !defined yyoverflow && !defined YYSTACK_RELOCATE
    goto yyexhaustedlab;
#else
    {
      /* Get the current used size of the three stacks, in elements.  */
      YYSIZE_T yysize = (YYSIZE_T) (yyssp - yyss + 1);

# if defined yyoverflow
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
# else /* defined YYSTACK_RELOCATE */
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
# undef YYSTACK_RELOCATE
        if (yyss1 != yyssa)
          YYSTACK_FREE (yyss1);
      }
# endif

      yyssp = yyss + yysize - 1;
      yyvsp = yyvs + yysize - 1;

      YYDPRINTF ((stderr, "Stack size increased to %lu\n",
                  (unsigned long) yystacksize));

      if (yyss + yystacksize - 1 <= yyssp)
        YYABORT;
    }
#endif /* !defined yyoverflow && !defined YYSTACK_RELOCATE */

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
        case 8:
#line 708 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	CB_PENDING (_("*CONTROL statement"));
  }
#line 2135 "ppparse.c" /* yacc.c:1652  */
    break;

  case 15:
#line 721 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	current_cmd = PLEX_ACT_IF;
  }
#line 2143 "ppparse.c" /* yacc.c:1652  */
    break;

  case 17:
#line 726 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	current_cmd = PLEX_ACT_ELIF;
  }
#line 2151 "ppparse.c" /* yacc.c:1652  */
    break;

  case 19:
#line 731 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	plex_action_directive (PLEX_ACT_ELSE, 0);
  }
#line 2159 "ppparse.c" /* yacc.c:1652  */
    break;

  case 20:
#line 735 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	plex_action_directive (PLEX_ACT_END, 0);
  }
#line 2167 "ppparse.c" /* yacc.c:1652  */
    break;

  case 21:
#line 739 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	current_call_convention = 0;
  }
#line 2175 "ppparse.c" /* yacc.c:1652  */
    break;

  case 22:
#line 743 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	if (current_call_convention == CB_CONV_STATIC_LINK) {
		current_call_convention |= CB_CONV_COBOL;
	};
  }
#line 2185 "ppparse.c" /* yacc.c:1652  */
    break;

  case 25:
#line 757 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	/* note: the old version was _as LITERAL but MF doesn't support this */
	struct cb_define_struct	*p;

	p = ppp_define_add (ppp_setvar_list, (yyvsp[-1].s), (yyvsp[0].s), 1);
	if (p) {
		ppp_setvar_list = p;
		fprintf (ppout, "#DEFLIT %s %s\n", (yyvsp[-1].s), (yyvsp[0].s));
	}
  }
#line 2200 "ppparse.c" /* yacc.c:1652  */
    break;

  case 27:
#line 769 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	struct cb_text_list	*l;

	for (l = (yyvsp[0].l); l; l = l->next) {
		fprintf (ppout, "#ADDRSV %s\n", l->text);
	}
  }
#line 2212 "ppparse.c" /* yacc.c:1652  */
    break;

  case 28:
#line 777 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	struct cb_text_list	*l;
	
	for (l = (yyvsp[0].l); l; l = l->next->next) {
		fprintf (ppout, "#ADDSYN %s %s\n", l->text, l->next->text);
	}
  }
#line 2224 "ppparse.c" /* yacc.c:1652  */
    break;

  case 29:
#line 785 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	char	*p = (yyvsp[0].s);
	size_t	size;

	/* Remove surrounding quotes/brackets */
	++p;
	size = strlen (p) - 1;
	p[size] = '\0';

	if (!strcasecmp (p, "EXTERNAL")) {
		fprintf (ppout, "#ASSIGN %d\n", (int)CB_ASSIGN_EXT_FILE_NAME_REQUIRED);
	} else if (!strcasecmp (p, "DYNAMIC")) {
		fprintf (ppout, "#ASSIGN %d\n", (int)CB_ASSIGN_VARIABLE_DEFAULT);
	} else {
		ppp_error_invalid_option ("ASSIGN", p);
	}	
  }
#line 2246 "ppparse.c" /* yacc.c:1652  */
    break;

  case 30:
#line 803 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	/* Enable EC-BOUND-SUBSCRIPT checking */
	append_to_turn_list (ppp_list_add (NULL, "EC-BOUND-SUBSCRIPT"), 1, 0);
  }
#line 2255 "ppparse.c" /* yacc.c:1652  */
    break;

  case 31:
#line 808 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	char	*p = (yyvsp[0].s);
	/* Remove surrounding quotes/brackets */
	size_t	size;
	++p;
	size = strlen (p) - 1;
	p[size] = '\0';
	fprintf (ppout, "#CALLFH \"%s\"\n", p);
  }
#line 2269 "ppparse.c" /* yacc.c:1652  */
    break;

  case 32:
#line 818 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	fprintf (ppout, "#CALLFH \"EXTFH\"\n");
  }
#line 2277 "ppparse.c" /* yacc.c:1652  */
    break;

  case 33:
#line 822 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	char	*p = (yyvsp[0].s);
	size_t	size;

	/* Remove surrounding quotes/brackets */
	++p;
	size = strlen (p) - 1;
	p[size] = '\0';

	if (!strcasecmp (p, "BINARY")) {
		cb_binary_comp_1 = 1;
	} else if (!strcasecmp (p, "FLOAT")) {
		cb_binary_comp_1 = 0;
	} else {
		ppp_error_invalid_option ("COMP1", p);
	}
  }
#line 2299 "ppparse.c" /* yacc.c:1652  */
    break;

  case 34:
#line 840 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	char	*p = (yyvsp[0].s);
	size_t	size;

	/* Remove surrounding quotes/brackets */
	++p;
	size = strlen (p) - 1;
	p[size] = '\0';

	if (!strcasecmp (p, "UPPER")) {
		cb_fold_copy = COB_FOLD_UPPER;
	} else if (!strcasecmp (p, "LOWER")) {
		cb_fold_copy = COB_FOLD_LOWER;
	} else {
		ppp_error_invalid_option ("FOLD-COPY-NAME", p);
	}
  }
#line 2321 "ppparse.c" /* yacc.c:1652  */
    break;

  case 35:
#line 858 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	fprintf (ppout, "#MAKESYN %s %s\n", (yyvsp[0].l)->text, (yyvsp[0].l)->next->text);
  }
#line 2329 "ppparse.c" /* yacc.c:1652  */
    break;

  case 36:
#line 862 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	/* Disable EC-BOUND-SUBSCRIPT checking */
	append_to_turn_list (ppp_list_add (NULL, "EC-BOUND-SUBSCRIPT"), 0, 0);
  }
#line 2338 "ppparse.c" /* yacc.c:1652  */
    break;

  case 37:
#line 867 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	cb_fold_copy = 0;
  }
#line 2346 "ppparse.c" /* yacc.c:1652  */
    break;

  case 38:
#line 871 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	/* Disable EC-BOUND-SUBSCRIPT and -REF-MOD checking */
	struct cb_text_list	*txt = ppp_list_add (NULL, "EC-BOUND-SUBSCRIPT");
	txt = ppp_list_add (txt, "EC-BOUND-REF-MOD");
	
	append_to_turn_list (txt, 0, 0);
  }
#line 2358 "ppparse.c" /* yacc.c:1652  */
    break;

  case 39:
#line 879 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
      struct cb_text_list	*l;

      for (l = (yyvsp[0].l); l; l = l->next->next) {
	      fprintf (ppout, "#OVERRIDE %s %s\n", l->text, l->next->text);
      }
  }
#line 2370 "ppparse.c" /* yacc.c:1652  */
    break;

  case 40:
#line 887 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	struct cb_text_list	*l;

	for (l = (yyvsp[0].l); l; l = l->next) {
		fprintf (ppout, "#REMOVE %s\n", l->text);
	}
  }
#line 2382 "ppparse.c" /* yacc.c:1652  */
    break;

  case 41:
#line 895 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	char	*p = (yyvsp[0].s);
	size_t	size;

	/* Remove surrounding quotes/brackets */
	++p;
	size = strlen (p) - 1;
	p[size] = '\0';

	if (!strcasecmp (p, "FIXED")) {
		cb_source_format = CB_FORMAT_FIXED;
		cb_text_column = cb_config_text_column;
	} else if (!strcasecmp (p, "FREE")) {
		cb_source_format = CB_FORMAT_FREE;
	} else if (!strcasecmp (p, "VARIABLE")) {
		cb_source_format = CB_FORMAT_FIXED;
		/* This value matches most MF Visual COBOL 4.0 version. */
		cb_text_column = 250;
	} else {
		ppp_error_invalid_option ("SOURCEFORMAT", p);
	}
	if (cb_src_list_file) {
		cb_current_file->source_format = cb_source_format;
	}
  }
#line 2412 "ppparse.c" /* yacc.c:1652  */
    break;

  case 42:
#line 921 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	char	*p = (yyvsp[0].s);
	size_t	size;
	struct cb_text_list	*txt;

	
	/* Remove surrounding quotes/brackets */
	if (p) {
		++p;
		size = strlen (p) - 1;
		p[size] = '\0';
	}

	/* Enable EC-BOUND-SUBSCRIPT and -REF-MOD checking */
	if (p && !strcasecmp (p, "1")) {
		/* At runtime only */
		CB_PENDING ("SSRANGE(1)");
	} else if (!p || !strcasecmp (p, "2")) {
		/*  At compile- and runtime */
		txt = ppp_list_add (NULL, "EC-BOUND-SUBSCRIPT");
		txt = ppp_list_add (txt, "EC-BOUND-REF-MOD");
		append_to_turn_list (txt, 1, 0);
	} else if (p && !strcasecmp (p, "3")) {
		/*
		  At compile- and runtime, and allowing zero-length ref mod at
		  runtime
		*/
		CB_PENDING ("SSRANGE(3)");
	} else {
		ppp_error_invalid_option ("SSRANGE", p);
	}
  }
#line 2449 "ppparse.c" /* yacc.c:1652  */
    break;

  case 43:
#line 957 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.l) = ppp_list_add (NULL, (yyvsp[0].s));
  }
#line 2457 "ppparse.c" /* yacc.c:1652  */
    break;

  case 44:
#line 961 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.l) = ppp_list_add ((yyvsp[-1].l), (yyvsp[0].s));
  }
#line 2465 "ppparse.c" /* yacc.c:1652  */
    break;

  case 46:
#line 969 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	  (yyval.l) = ppp_list_append ((yyvsp[-1].l), (yyvsp[0].l));
  }
#line 2473 "ppparse.c" /* yacc.c:1652  */
    break;

  case 47:
#line 976 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.l) = ppp_list_add (NULL, (yyvsp[-2].s));
	(yyval.l) = ppp_list_add ((yyval.l), (yyvsp[0].s));
  }
#line 2482 "ppparse.c" /* yacc.c:1652  */
    break;

  case 48:
#line 984 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	fprintf (ppout, "#OPTION %s\n", (yyvsp[0].s));
  }
#line 2490 "ppparse.c" /* yacc.c:1652  */
    break;

  case 49:
#line 988 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	fprintf (ppout, "#OPTION %s %s\n", (yyvsp[-2].s), (yyvsp[0].s));
  }
#line 2498 "ppparse.c" /* yacc.c:1652  */
    break;

  case 50:
#line 995 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	  if (cb_src_list_file) {
		  cb_current_file->source_format = cb_source_format;
	  }
  }
#line 2508 "ppparse.c" /* yacc.c:1652  */
    break;

  case 51:
#line 1004 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	cb_source_format = CB_FORMAT_FIXED;
	cb_text_column = cb_config_text_column;
  }
#line 2517 "ppparse.c" /* yacc.c:1652  */
    break;

  case 52:
#line 1009 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	cb_source_format = CB_FORMAT_FREE;
  }
#line 2525 "ppparse.c" /* yacc.c:1652  */
    break;

  case 53:
#line 1013 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	cb_source_format = CB_FORMAT_FIXED;
	cb_text_column = 500;
  }
#line 2534 "ppparse.c" /* yacc.c:1652  */
    break;

  case 54:
#line 1018 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	cb_error (_("invalid %s directive"), "SOURCE");
	YYERROR;
  }
#line 2543 "ppparse.c" /* yacc.c:1652  */
    break;

  case 55:
#line 1025 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    { (yyval.s) = NULL; }
#line 2549 "ppparse.c" /* yacc.c:1652  */
    break;

  case 57:
#line 1031 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	struct cb_define_struct	*p;

	p = ppp_define_add (ppp_setvar_list, (yyvsp[-3].s), (yyvsp[-1].s), (yyvsp[0].ui));
	if (p) {
		ppp_setvar_list = p;
	}
  }
#line 2562 "ppparse.c" /* yacc.c:1652  */
    break;

  case 58:
#line 1040 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	char			*s;
	char			*q;
	struct cb_define_struct	*p;
	size_t			size;

	s = getenv ((yyvsp[-3].s));
	q = NULL;
	if (s && *s && *s != ' ') {
		if (*s == '"' || *s == '\'') {
			size = strlen (s) - 1U;
			/* Ignore if improperly quoted */
			if (s[0] == s[size]) {
				q = s;
			}
		} else {
			if (ppp_check_needs_quote (s)) {
				/* Alphanumeric literal */
				q = cobc_plex_malloc (strlen (s) + 4U);
				sprintf (q, "'%s'", s);
			} else {
				/* Numeric literal */
				q = s;
			}
		}
	}
	if (q) {
		p = ppp_define_add (ppp_setvar_list, (yyvsp[-3].s), q, (yyvsp[0].ui));
		if (p) {
			ppp_setvar_list = p;
		}
	}
  }
#line 2600 "ppparse.c" /* yacc.c:1652  */
    break;

  case 59:
#line 1074 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	ppp_define_del ((yyvsp[-2].s));
  }
#line 2608 "ppparse.c" /* yacc.c:1652  */
    break;

  case 60:
#line 1078 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
  /* OpenCOBOL/GnuCOBOL 2.0 extension: MF $SET CONSTANT in 2002+ style as
     >> DEFINE CONSTANT var [AS] literal  archaic extension:
     use plain  >> DEFINE var [AS] literal  for conditional compilation and
     use        01 CONSTANT with/without FROM clause  for constant definitions */
	struct cb_define_struct	*p;

	if (cb_verify (cb_define_constant_directive, ">> DEFINE CONSTANT var")) {
		p = ppp_define_add (ppp_setvar_list, (yyvsp[-3].s), (yyvsp[-1].s), (yyvsp[0].ui));
		if (p) {
			ppp_setvar_list = p;
			fprintf (ppout, "#DEFLIT %s %s%s\n", (yyvsp[-3].s), (yyvsp[-1].s), (yyvsp[0].ui) ? " OVERRIDE" : "");
		}
	}
  }
#line 2628 "ppparse.c" /* yacc.c:1652  */
    break;

  case 61:
#line 1094 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	cb_error (_("invalid %s directive"), "DEFINE/SET");
  }
#line 2636 "ppparse.c" /* yacc.c:1652  */
    break;

  case 78:
#line 1133 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	CB_PENDING (_("LEAP-SECOND ON directive"));
  }
#line 2644 "ppparse.c" /* yacc.c:1652  */
    break;

  case 80:
#line 1141 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	append_to_turn_list ((yyvsp[-2].l), !!(yyvsp[0].ui), (yyvsp[0].ui) == 2U);
  }
#line 2652 "ppparse.c" /* yacc.c:1652  */
    break;

  case 81:
#line 1148 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.l) = ppp_list_add (NULL, (yyvsp[0].s));
  }
#line 2660 "ppparse.c" /* yacc.c:1652  */
    break;

  case 82:
#line 1152 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.l) = ppp_list_add ((yyvsp[-1].l), (yyvsp[0].s));
  }
#line 2668 "ppparse.c" /* yacc.c:1652  */
    break;

  case 83:
#line 1159 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.ui) = 2U;
  }
#line 2676 "ppparse.c" /* yacc.c:1652  */
    break;

  case 84:
#line 1163 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.ui) = 1U;
  }
#line 2684 "ppparse.c" /* yacc.c:1652  */
    break;

  case 85:
#line 1167 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.ui) = 0;
  }
#line 2692 "ppparse.c" /* yacc.c:1652  */
    break;

  case 92:
#line 1189 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	current_call_convention |= CB_CONV_COBOL;
	current_call_convention &= ~CB_CONV_STDCALL;
  }
#line 2701 "ppparse.c" /* yacc.c:1652  */
    break;

  case 93:
#line 1194 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	current_call_convention &= ~CB_CONV_STDCALL;
	current_call_convention &= ~CB_CONV_COBOL;
  }
#line 2710 "ppparse.c" /* yacc.c:1652  */
    break;

  case 94:
#line 1199 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	current_call_convention |= CB_CONV_STDCALL;
	current_call_convention &= ~CB_CONV_COBOL;
  }
#line 2719 "ppparse.c" /* yacc.c:1652  */
    break;

  case 95:
#line 1204 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	current_call_convention |= CB_CONV_STATIC_LINK;
  }
#line 2727 "ppparse.c" /* yacc.c:1652  */
    break;

  case 96:
#line 1211 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	unsigned int		found;

	found = (ppp_search_lists ((yyvsp[-3].s)) != NULL);
	plex_action_directive (current_cmd, found ^ (yyvsp[-1].ui));
  }
#line 2738 "ppparse.c" /* yacc.c:1652  */
    break;

  case 97:
#line 1218 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	unsigned int		found;

	found = ppp_search_comp_vars ((yyvsp[-3].s));
	plex_action_directive (current_cmd, found ^ (yyvsp[-1].ui));
  }
#line 2749 "ppparse.c" /* yacc.c:1652  */
    break;

  case 98:
#line 1225 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	struct cb_define_struct	*p;
	unsigned int		found;

	found = 0;
	p = ppp_search_lists ((yyvsp[-4].s));
	found = ppp_compare_vals (p, (yyvsp[0].ds), (yyvsp[-1].ui));
	plex_action_directive (current_cmd, found ^ (yyvsp[-2].ui));
  }
#line 2763 "ppparse.c" /* yacc.c:1652  */
    break;

  case 99:
#line 1235 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	struct cb_define_struct	*p;
	unsigned int		found;

	found = 0;
	p = cobc_plex_malloc (sizeof (struct cb_define_struct));
	p->next = NULL;
	if (ppp_set_value (p, (yyvsp[-4].s))) {
		cb_error (_("invalid constant"));
	} else {
		found = ppp_compare_vals (p, (yyvsp[0].ds), (yyvsp[-1].ui));
	}
	plex_action_directive (current_cmd, found ^ (yyvsp[-2].ui));
  }
#line 2782 "ppparse.c" /* yacc.c:1652  */
    break;

  case 100:
#line 1250 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	cb_error (_("invalid %s directive"), "IF/ELIF");
  }
#line 2790 "ppparse.c" /* yacc.c:1652  */
    break;

  case 103:
#line 1262 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	struct cb_define_struct	*p;

	p = cobc_plex_malloc (sizeof (struct cb_define_struct));
	p->next = NULL;
	if (ppp_set_value (p, (yyvsp[0].s))) {
		cb_error (_("invalid constant"));
		(yyval.ds) = NULL;
	} else {
		(yyval.ds) = p;
	}
  }
#line 2807 "ppparse.c" /* yacc.c:1652  */
    break;

  case 104:
#line 1275 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	struct cb_define_struct	*p;

	p = ppp_search_lists ((yyvsp[0].s));
	if (p != NULL && p->deftype != PLEX_DEF_NONE) {
		(yyval.ds) = p;
	} else {
		(yyval.ds) = NULL;
	}
  }
#line 2822 "ppparse.c" /* yacc.c:1652  */
    break;

  case 105:
#line 1289 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.ui) = COND_GE;
  }
#line 2830 "ppparse.c" /* yacc.c:1652  */
    break;

  case 106:
#line 1293 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.ui) = COND_GT;
  }
#line 2838 "ppparse.c" /* yacc.c:1652  */
    break;

  case 107:
#line 1297 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.ui) = COND_LE;
  }
#line 2846 "ppparse.c" /* yacc.c:1652  */
    break;

  case 108:
#line 1301 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.ui) = COND_LT;
  }
#line 2854 "ppparse.c" /* yacc.c:1652  */
    break;

  case 109:
#line 1305 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.ui) = COND_EQ;
  }
#line 2862 "ppparse.c" /* yacc.c:1652  */
    break;

  case 110:
#line 1309 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.ui) = COND_GE;
  }
#line 2870 "ppparse.c" /* yacc.c:1652  */
    break;

  case 111:
#line 1313 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.ui) = COND_GT;
  }
#line 2878 "ppparse.c" /* yacc.c:1652  */
    break;

  case 112:
#line 1317 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.ui) = COND_LE;
  }
#line 2886 "ppparse.c" /* yacc.c:1652  */
    break;

  case 113:
#line 1321 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.ui) = COND_LT;
  }
#line 2894 "ppparse.c" /* yacc.c:1652  */
    break;

  case 114:
#line 1325 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.ui) = COND_EQ;
  }
#line 2902 "ppparse.c" /* yacc.c:1652  */
    break;

  case 115:
#line 1329 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.ui) = COND_NE;
  }
#line 2910 "ppparse.c" /* yacc.c:1652  */
    break;

  case 116:
#line 1336 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	fputc ('\n', ppout);
	ppcopy ((yyvsp[-3].s), (yyvsp[-2].s), (yyvsp[0].r));
  }
#line 2919 "ppparse.c" /* yacc.c:1652  */
    break;

  case 117:
#line 1344 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.s) = fix_filename ((yyvsp[0].s));
	if (cb_fold_copy == COB_FOLD_LOWER) {
		(yyval.s) = fold_lower ((yyval.s));
	} else if (cb_fold_copy == COB_FOLD_UPPER) {
		(yyval.s) = fold_upper ((yyval.s));
	}
  }
#line 2932 "ppparse.c" /* yacc.c:1652  */
    break;

  case 118:
#line 1353 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.s) = (yyvsp[0].s);
	if (cb_fold_copy == COB_FOLD_LOWER) {
		(yyval.s) = fold_lower ((yyval.s));
	} else {
		(yyval.s) = fold_upper ((yyval.s));
	}
  }
#line 2945 "ppparse.c" /* yacc.c:1652  */
    break;

  case 119:
#line 1365 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.s) = NULL;
  }
#line 2953 "ppparse.c" /* yacc.c:1652  */
    break;

  case 120:
#line 1369 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.s) = (yyvsp[0].s);
  }
#line 2961 "ppparse.c" /* yacc.c:1652  */
    break;

  case 125:
#line 1385 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.r) = NULL;
  }
#line 2969 "ppparse.c" /* yacc.c:1652  */
    break;

  case 126:
#line 1389 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.r) = (yyvsp[0].r);
  }
#line 2977 "ppparse.c" /* yacc.c:1652  */
    break;

  case 127:
#line 1396 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	pp_set_replace_list ((yyvsp[0].r), (yyvsp[-1].ui));
  }
#line 2985 "ppparse.c" /* yacc.c:1652  */
    break;

  case 128:
#line 1400 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	pp_set_replace_list (NULL, (yyvsp[-1].ui));
  }
#line 2993 "ppparse.c" /* yacc.c:1652  */
    break;

  case 129:
#line 1407 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.r) = ppp_replace_list_add (NULL, (yyvsp[-2].l), (yyvsp[0].l), 0);
  }
#line 3001 "ppparse.c" /* yacc.c:1652  */
    break;

  case 130:
#line 1411 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.r) = ppp_replace_list_add (NULL, (yyvsp[-2].l), (yyvsp[0].l), (yyvsp[-3].ui));
  }
#line 3009 "ppparse.c" /* yacc.c:1652  */
    break;

  case 131:
#line 1415 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.r) = ppp_replace_list_add ((yyvsp[-3].r), (yyvsp[-2].l), (yyvsp[0].l), 0);
  }
#line 3017 "ppparse.c" /* yacc.c:1652  */
    break;

  case 132:
#line 1419 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.r) = ppp_replace_list_add ((yyvsp[-4].r), (yyvsp[-2].l), (yyvsp[0].l), (yyvsp[-3].ui));
  }
#line 3025 "ppparse.c" /* yacc.c:1652  */
    break;

  case 133:
#line 1426 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.l) = (yyvsp[-1].l);
  }
#line 3033 "ppparse.c" /* yacc.c:1652  */
    break;

  case 134:
#line 1430 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.l) = (yyvsp[0].l);
  }
#line 3041 "ppparse.c" /* yacc.c:1652  */
    break;

  case 135:
#line 1437 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.l) = NULL;
  }
#line 3049 "ppparse.c" /* yacc.c:1652  */
    break;

  case 136:
#line 1441 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.l) = (yyvsp[-1].l);
  }
#line 3057 "ppparse.c" /* yacc.c:1652  */
    break;

  case 137:
#line 1445 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.l) = (yyvsp[0].l);
  }
#line 3065 "ppparse.c" /* yacc.c:1652  */
    break;

  case 138:
#line 1452 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.l) = ppp_list_add (NULL, (yyvsp[-1].s));
  }
#line 3073 "ppparse.c" /* yacc.c:1652  */
    break;

  case 139:
#line 1459 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.l) = NULL;
  }
#line 3081 "ppparse.c" /* yacc.c:1652  */
    break;

  case 140:
#line 1463 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.l) = ppp_list_add (NULL, (yyvsp[-1].s));
  }
#line 3089 "ppparse.c" /* yacc.c:1652  */
    break;

  case 141:
#line 1470 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.l) = ppp_list_add (NULL, (yyvsp[0].s));
  }
#line 3097 "ppparse.c" /* yacc.c:1652  */
    break;

  case 142:
#line 1474 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.l) = ppp_list_add ((yyvsp[-1].l), (yyvsp[0].s));
  }
#line 3105 "ppparse.c" /* yacc.c:1652  */
    break;

  case 143:
#line 1481 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.l) = ppp_list_add (NULL, (yyvsp[0].s));
  }
#line 3113 "ppparse.c" /* yacc.c:1652  */
    break;

  case 144:
#line 1485 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.l) = ppp_list_add ((yyvsp[-2].l), " ");
	(yyval.l) = ppp_list_add ((yyval.l), "IN");
	(yyval.l) = ppp_list_add ((yyval.l), " ");
	(yyval.l) = ppp_list_add ((yyval.l), (yyvsp[0].s));
  }
#line 3124 "ppparse.c" /* yacc.c:1652  */
    break;

  case 145:
#line 1492 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.l) = ppp_list_add ((yyvsp[-2].l), " ");
	(yyval.l) = ppp_list_add ((yyval.l), "OF");
	(yyval.l) = ppp_list_add ((yyval.l), " ");
	(yyval.l) = ppp_list_add ((yyval.l), (yyvsp[0].s));
  }
#line 3135 "ppparse.c" /* yacc.c:1652  */
    break;

  case 146:
#line 1499 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	struct cb_text_list *l;

	(yyval.l) = ppp_list_add ((yyvsp[-3].l), " ");
	(yyval.l) = ppp_list_add ((yyval.l), "(");
	(yyvsp[-1].l) = ppp_list_add ((yyvsp[-1].l), ")");
	for (l = (yyval.l); l->next; l = l->next) {
		;
	}
	l->next = (yyvsp[-1].l);
  }
#line 3151 "ppparse.c" /* yacc.c:1652  */
    break;

  case 147:
#line 1514 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.l) = ppp_list_add (NULL, (yyvsp[0].s));
  }
#line 3159 "ppparse.c" /* yacc.c:1652  */
    break;

  case 148:
#line 1518 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.l) = ppp_list_add ((yyvsp[-1].l), " ");
	(yyval.l) = ppp_list_add ((yyval.l), (yyvsp[0].s));
  }
#line 3168 "ppparse.c" /* yacc.c:1652  */
    break;

  case 149:
#line 1526 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.ui) = CB_REPLACE_LEADING;
  }
#line 3176 "ppparse.c" /* yacc.c:1652  */
    break;

  case 150:
#line 1530 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.ui) = CB_REPLACE_TRAILING;
  }
#line 3184 "ppparse.c" /* yacc.c:1652  */
    break;

  case 151:
#line 1539 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.ui) = 0;
  }
#line 3192 "ppparse.c" /* yacc.c:1652  */
    break;

  case 152:
#line 1543 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.ui) = 1U;
  }
#line 3200 "ppparse.c" /* yacc.c:1652  */
    break;

  case 153:
#line 1550 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.ui) = 0;
  }
#line 3208 "ppparse.c" /* yacc.c:1652  */
    break;

  case 154:
#line 1554 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.ui) = 1U;
  }
#line 3216 "ppparse.c" /* yacc.c:1652  */
    break;

  case 155:
#line 1561 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.ui) = 0;
  }
#line 3224 "ppparse.c" /* yacc.c:1652  */
    break;

  case 156:
#line 1565 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.ui) = 1U;
  }
#line 3232 "ppparse.c" /* yacc.c:1652  */
    break;

  case 157:
#line 1572 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.ui) = 0;
  }
#line 3240 "ppparse.c" /* yacc.c:1652  */
    break;

  case 158:
#line 1576 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1652  */
    {
	(yyval.ui) = 1U;
  }
#line 3248 "ppparse.c" /* yacc.c:1652  */
    break;


#line 3252 "ppparse.c" /* yacc.c:1652  */
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
#line 1588 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1918  */

