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


/* Substitute the variable and function names.  */
#define yyparse         ppparse
#define yylex           pplex
#define yyerror         pperror
#define yydebug         ppdebug
#define yynerrs         ppnerrs

#define yylval          pplval
#define yychar          ppchar

/* Copy the first part of user declarations.  */
#line 34 "ppparse.y" /* yacc.c:339  */

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


#line 563 "ppparse.c" /* yacc.c:339  */

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
    CALLFH = 303,
    COMP1 = 304,
    CONSTANT = 305,
    FOLDCOPYNAME = 306,
    MAKESYN = 307,
    NOFOLDCOPYNAME = 308,
    REMOVE = 309,
    SOURCEFORMAT = 310,
    IF_DIRECTIVE = 311,
    ELSE_DIRECTIVE = 312,
    ENDIF_DIRECTIVE = 313,
    ELIF_DIRECTIVE = 314,
    GE = 315,
    LE = 316,
    LT = 317,
    GT = 318,
    EQ = 319,
    NE = 320,
    NOT = 321,
    THAN = 322,
    TO = 323,
    OR = 324,
    EQUAL = 325,
    GREATER = 326,
    LESS = 327,
    SET = 328,
    DEFINED = 329,
    TURN_DIRECTIVE = 330,
    ON = 331,
    CHECKING = 332,
    WITH = 333,
    LOCATION = 334,
    TERMINATOR = 335,
    TOKEN = 336,
    TEXT_NAME = 337,
    VARIABLE_NAME = 338,
    LITERAL = 339
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
#define CALLFH 303
#define COMP1 304
#define CONSTANT 305
#define FOLDCOPYNAME 306
#define MAKESYN 307
#define NOFOLDCOPYNAME 308
#define REMOVE 309
#define SOURCEFORMAT 310
#define IF_DIRECTIVE 311
#define ELSE_DIRECTIVE 312
#define ENDIF_DIRECTIVE 313
#define ELIF_DIRECTIVE 314
#define GE 315
#define LE 316
#define LT 317
#define GT 318
#define EQ 319
#define NE 320
#define NOT 321
#define THAN 322
#define TO 323
#define OR 324
#define EQUAL 325
#define GREATER 326
#define LESS 327
#define SET 328
#define DEFINED 329
#define TURN_DIRECTIVE 330
#define ON 331
#define CHECKING 332
#define WITH 333
#define LOCATION 334
#define TERMINATOR 335
#define TOKEN 336
#define TEXT_NAME 337
#define VARIABLE_NAME 338
#define LITERAL 339

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 523 "ppparse.y" /* yacc.c:355  */

	char			*s;
	struct cb_text_list	*l;
	struct cb_replace_list	*r;
	struct cb_define_struct	*ds;
	unsigned int		ui;
	int			si;

#line 782 "ppparse.c" /* yacc.c:355  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE pplval;

int ppparse (void);

#endif /* !YY_PP_PPPARSE_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 799 "ppparse.c" /* yacc.c:358  */

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
#define YYFINAL  2
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   216

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  87
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  58
/* YYNRULES -- Number of rules.  */
#define YYNRULES  162
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  239

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   339

#define YYTRANSLATE(YYX)                                                \
  ((unsigned int) (YYX) <= YYMAXUTOK ? yytranslate[YYX] : YYUNDEFTOK)

/* YYTRANSLATE[TOKEN-NUM] -- Symbol number corresponding to TOKEN-NUM
   as returned by yylex, without out-of-bounds checking.  */
static const yytype_uint8 yytranslate[] =
{
       0,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,     2,     2,
      85,    86,     2,     2,     2,     2,     2,     2,     2,     2,
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
      75,    76,    77,    78,    79,    80,    81,    82,    83,    84
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint16 yyrline[] =
{
       0,   659,   659,   660,   664,   665,   666,   667,   668,   675,
     676,   677,   678,   679,   680,   682,   681,   687,   686,   691,
     695,   700,   699,   712,   713,   717,   728,   729,   737,   745,
     755,   759,   777,   795,   799,   803,   811,   819,   848,   852,
     859,   860,   867,   876,   879,   886,   895,   900,   904,   909,
     917,   926,   960,   964,   980,   987,   990,   991,   995,   996,
    1000,  1001,  1005,  1006,  1007,  1008,  1009,  1010,  1013,  1014,
    1017,  1019,  1023,  1027,  1034,  1035,  1038,  1040,  1041,  1042,
    1046,  1047,  1051,  1052,  1056,  1061,  1066,  1071,  1078,  1085,
    1092,  1102,  1117,  1124,  1125,  1129,  1142,  1156,  1160,  1164,
    1168,  1172,  1176,  1180,  1184,  1188,  1192,  1196,  1203,  1211,
    1220,  1233,  1236,  1243,  1244,  1247,  1248,  1253,  1256,  1263,
    1267,  1274,  1278,  1282,  1286,  1293,  1297,  1304,  1308,  1312,
    1319,  1326,  1330,  1337,  1341,  1348,  1352,  1359,  1366,  1381,
    1385,  1393,  1397,  1407,  1410,  1418,  1421,  1429,  1432,  1440,
    1443,  1449,  1449,  1450,  1450,  1451,  1451,  1452,  1452,  1453,
    1453,  1454,  1454
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
  "SET_DIRECTIVE", "ADDRSV", "ADDSYN", "CALLFH", "COMP1", "CONSTANT",
  "FOLDCOPYNAME", "MAKESYN", "NOFOLDCOPYNAME", "REMOVE", "SOURCEFORMAT",
  "IF_DIRECTIVE", "ELSE_DIRECTIVE", "ENDIF_DIRECTIVE", "ELIF_DIRECTIVE",
  "\">=\"", "\"<=\"", "\"<\"", "\">\"", "\"=\"", "\"<>\"", "NOT", "THAN",
  "TO", "OR", "EQUAL", "GREATER", "LESS", "SET", "DEFINED",
  "TURN_DIRECTIVE", "ON", "CHECKING", "WITH", "LOCATION",
  "\"end of line\"", "\"Identifier or Literal\"", "\"Text-Name\"",
  "\"Variable\"", "\"Literal\"", "'('", "')'", "$accept", "statement_list",
  "statement", "directive", "$@1", "$@2", "$@3", "set_directive",
  "set_choice", "alnum_list", "alnum_equality_list", "alnum_equality",
  "set_options", "source_directive", "format_type", "define_directive",
  "listing_directive", "listing_statement", "control_options",
  "control_option", "_dot", "leap_second_directive", "turn_directive",
  "ec_list", "on_or_off", "with_loc", "call_directive", "call_choice",
  "if_directive", "variable_or_literal", "object_id", "condition_clause",
  "copy_statement", "copy_source", "copy_in", "in_or_of", "copy_suppress",
  "copy_replacing", "replace_statement", "replacing_list", "text_src",
  "text_dst", "text_partial_src", "text_partial_dst", "token_list",
  "identifier", "subscripts", "lead_trail", "_override", "_not", "_also",
  "_last", "_as", "_format", "_is", "_printing", "_than", "_to", YY_NULLPTR
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
     335,   336,   337,   338,   339,    40,    41
};
# endif

#define YYPACT_NINF -116

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-116)))

#define YYTABLE_NINF -152

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
    -116,     3,  -116,   -28,    35,    19,  -116,   -56,   123,    20,
      14,  -116,   -16,    53,  -116,  -116,  -116,  -116,   -33,  -116,
     -11,  -116,    38,    59,  -116,  -116,    42,  -116,  -116,    -2,
      73,  -116,  -116,  -116,    92,  -116,  -116,  -116,  -116,  -116,
    -116,   117,  -116,  -116,  -116,  -116,  -116,  -116,    66,    81,
      50,   -24,  -116,  -116,  -116,    51,    72,    51,    74,    75,
      77,   115,    51,  -116,    72,   115,   -21,    53,  -116,   -12,
     -12,  -116,  -116,   -48,  -116,  -116,  -116,  -116,  -116,   146,
     -28,    82,  -116,  -116,  -116,    -2,   158,    -5,   159,  -116,
    -116,    84,  -116,    86,  -116,    40,  -116,  -116,  -116,  -116,
      81,  -116,   115,  -116,     8,   103,    51,  -116,  -116,    85,
    -116,  -116,  -116,    87,    88,  -116,    85,    89,  -116,    90,
    -116,   -23,   -15,  -116,  -116,  -116,    15,  -116,   156,   161,
    -116,  -116,     0,   166,   159,     4,    95,    96,    97,    98,
     176,  -116,  -116,  -116,  -116,  -116,  -116,  -116,  -116,    99,
    -116,   137,   137,   100,  -116,  -116,  -116,  -116,  -116,  -116,
     116,   116,  -116,    11,   106,  -116,  -116,  -116,  -116,  -116,
      -2,  -116,  -116,  -116,     4,   182,     5,  -116,    -5,  -116,
    -116,  -116,   -39,   181,   183,   137,  -116,  -116,  -116,  -116,
    -116,    52,    67,  -116,  -116,    -2,  -116,   183,  -116,     6,
    -116,  -116,  -116,     7,  -116,  -116,  -116,  -116,  -116,  -116,
    -116,  -116,   120,   124,   124,  -116,  -116,    -1,    -1,  -116,
    -116,  -116,   184,  -116,  -116,  -116,   125,   126,  -116,  -116,
    -116,  -116,  -116,   122,   127,   120,   120,  -116,  -116
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       2,     0,     1,     0,   147,    55,    58,     0,     0,    70,
     153,    21,     0,     0,    15,    19,    20,    17,     0,     3,
       0,     7,     0,     0,   109,   110,   111,   148,   150,     0,
       0,    57,    56,    13,    68,    62,    63,    64,    65,    66,
      67,    68,    60,    72,    71,    14,   154,     9,   155,     0,
       0,   151,    94,    10,    54,     0,     0,     0,    30,     0,
       0,   151,     0,    34,     0,   151,    43,    11,    23,     0,
       0,    74,    12,     0,     6,     4,     5,   113,   114,   115,
       0,     0,   141,   142,   135,   119,     0,   126,     0,   120,
      69,     0,    61,     0,   156,     0,    84,    85,    86,    87,
      22,    82,   151,   152,     0,     0,    35,    40,    38,    27,
      28,    29,    31,     0,     0,    33,    36,     0,    26,     0,
      24,   155,   155,    16,    92,    18,    76,    75,   157,   117,
     112,   133,     0,     0,     0,     0,     0,     0,     0,     0,
       0,    59,     8,    49,    46,    47,    48,    45,    83,     0,
      52,   143,   143,     0,    41,    39,    25,    32,    37,    44,
     145,   145,    77,     0,     0,    81,    73,    79,   158,   116,
       0,   108,   125,   134,     0,     0,     0,   121,   129,   136,
     137,   139,     0,     0,     0,   143,   144,    51,    50,    42,
     146,     0,     0,    78,    80,   118,   123,     0,   127,     0,
     140,   138,   130,     0,   122,    53,   102,   104,   105,   103,
     106,   107,   161,   159,   159,    89,    88,     0,     0,   124,
     128,   131,     0,   162,   101,   160,    98,   100,    96,    95,
      90,    91,   132,     0,     0,   161,   161,    97,    99
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
    -116,  -116,  -116,  -116,  -116,  -116,  -116,  -116,   129,   134,
    -116,   -42,  -116,  -116,  -116,  -116,  -116,  -116,  -116,   152,
     160,  -116,  -116,  -116,  -116,    36,  -116,   102,   130,   191,
     -14,    13,  -116,   128,  -116,  -116,  -116,  -116,  -116,    37,
     -85,    32,    76,    12,    39,  -108,  -116,   -84,  -115,    55,
    -116,  -116,   -25,  -116,    31,  -116,    -3,   -81
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     1,    19,    20,    69,    70,    49,    67,    68,   109,
     106,   107,   118,    47,   147,    53,    33,    21,    41,    42,
      91,    45,    72,    73,   166,   167,   100,   101,   123,   124,
     230,   217,    22,    26,    79,    80,   129,   171,    23,    85,
      86,   177,   140,   204,   132,    87,   182,    88,   187,   191,
      29,    30,   104,    48,    95,   169,   226,   224
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_int16 yytable[] =
{
     133,   134,   136,     2,    81,   137,   172,    82,     3,    94,
     176,   198,   220,   221,    83,   110,     4,    94,   103,   150,
     115,   103,     5,     6,     7,     8,   162,   178,    34,   126,
      31,    43,     9,    10,    50,   127,   114,   188,    27,    11,
     117,   119,   200,    28,    12,    46,  -149,   201,    13,    77,
      71,   151,    78,    24,    25,    75,   -93,   -93,   143,    14,
      15,    16,    17,  -151,   154,   -94,   178,    51,    52,    74,
     205,   121,   122,   144,   145,   146,    76,   149,    18,    84,
     138,   173,   228,   229,    89,    84,   131,   173,   222,   164,
     165,   163,   152,   164,   165,    32,    44,    55,    94,    56,
      57,    58,    59,    60,    61,    62,    63,    64,    65,    90,
     133,   134,   206,   207,   208,   209,   210,   211,    96,    97,
      98,    99,   212,   213,   214,   215,   216,   206,   207,   208,
     209,   210,   211,   102,    90,   105,    66,   212,   213,   214,
      35,    36,    37,    38,    39,    40,    35,    36,    37,    38,
      39,    40,   160,   161,   237,   238,   108,   103,   111,   112,
     113,   128,   135,   131,   141,   139,   142,   153,   168,   155,
     174,   156,   157,   158,   159,   170,   179,   180,   181,   183,
     184,   186,   190,   185,   189,   194,   197,   202,   223,   203,
     232,   225,   235,    92,   233,   234,   120,   236,   116,   193,
     125,    93,   148,    54,   231,   218,   196,   195,   130,   219,
     175,   227,     0,     0,     0,   199,   192
};

static const yytype_int16 yycheck[] =
{
      85,    85,     7,     0,     6,    10,     6,     9,     5,    32,
       6,     6,     6,     6,    16,    57,    13,    32,    42,    11,
      62,    42,    19,    20,    21,    22,    11,   135,    84,    77,
      11,    11,    29,    30,    50,    83,    61,   152,     3,    36,
      65,    66,    81,     8,    41,    31,    11,    86,    45,     7,
      83,    43,    10,    81,    82,    17,    80,    80,    18,    56,
      57,    58,    59,    84,   106,    80,   174,    83,    84,    80,
     185,    83,    84,    33,    34,    35,    17,   102,    75,    81,
      85,    81,    83,    84,    11,    81,    81,    81,    81,    78,
      79,    76,    84,    78,    79,    76,    76,    44,    32,    46,
      47,    48,    49,    50,    51,    52,    53,    54,    55,    17,
     195,   195,    60,    61,    62,    63,    64,    65,    37,    38,
      39,    40,    70,    71,    72,    73,    74,    60,    61,    62,
      63,    64,    65,    83,    17,    84,    83,    70,    71,    72,
      23,    24,    25,    26,    27,    28,    23,    24,    25,    26,
      27,    28,   121,   122,   235,   236,    84,    42,    84,    84,
      83,    15,     4,    81,    80,     6,    80,    64,    12,    84,
       4,    84,    84,    84,    84,    14,    81,    81,    81,    81,
       4,    44,    66,    84,    84,    79,     4,     6,    68,     6,
       6,    67,    70,    41,    69,    69,    67,    70,    64,   163,
      70,    41,   100,    12,   218,   192,   174,   170,    80,   197,
     134,   214,    -1,    -1,    -1,   176,   161
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    88,     0,     5,    13,    19,    20,    21,    22,    29,
      30,    36,    41,    45,    56,    57,    58,    59,    75,    89,
      90,   104,   119,   125,    81,    82,   120,     3,     8,   137,
     138,    11,    76,   103,    84,    23,    24,    25,    26,    27,
      28,   105,   106,    11,    76,   108,    31,   100,   140,    93,
      50,    83,    84,   102,   116,    44,    46,    47,    48,    49,
      50,    51,    52,    53,    54,    55,    83,    94,    95,    91,
      92,    83,   109,   110,    80,    17,    17,     7,    10,   121,
     122,     6,     9,    16,    81,   126,   127,   132,   134,    11,
      17,   107,   106,   107,    32,   141,    37,    38,    39,    40,
     113,   114,    83,    42,   139,    84,    97,    98,    84,    96,
      98,    84,    84,    83,   139,    98,    96,   139,    99,   139,
      95,    83,    84,   115,   116,   115,    77,    83,    15,   123,
     120,    81,   131,   127,   134,     4,     7,    10,    85,     6,
     129,    80,    80,    18,    33,    34,    35,   101,   114,   139,
      11,    43,    84,    64,    98,    84,    84,    84,    84,    84,
     141,   141,    11,    76,    78,    79,   111,   112,    12,   142,
      14,   124,     6,    81,     4,   129,     6,   128,   132,    81,
      81,    81,   133,    81,     4,    84,    44,   135,   135,    84,
      66,   136,   136,   112,    79,   126,   128,     4,     6,   131,
      81,    86,     6,     6,   130,   135,    60,    61,    62,    63,
      64,    65,    70,    71,    72,    73,    74,   118,   118,   130,
       6,     6,    81,    68,   144,    67,   143,   143,    83,    84,
     117,   117,     6,    69,    69,    70,    70,   144,   144
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    87,    88,    88,    89,    89,    89,    89,    89,    90,
      90,    90,    90,    90,    90,    91,    90,    92,    90,    90,
      90,    93,    90,    94,    94,    95,    95,    95,    95,    95,
      95,    95,    95,    95,    95,    95,    95,    95,    96,    96,
      97,    97,    98,    99,    99,   100,   101,   101,   101,   101,
     102,   102,   102,   102,   102,   103,   103,   103,   104,   104,
     105,   105,   106,   106,   106,   106,   106,   106,   107,   107,
     108,   108,   108,   109,   110,   110,   111,   111,   111,   111,
     112,   112,   113,   113,   114,   114,   114,   114,   115,   115,
     115,   115,   115,   116,   116,   117,   117,   118,   118,   118,
     118,   118,   118,   118,   118,   118,   118,   118,   119,   120,
     120,   121,   121,   122,   122,   123,   123,   124,   124,   125,
     125,   126,   126,   126,   126,   127,   127,   128,   128,   128,
     129,   130,   130,   131,   131,   132,   132,   132,   132,   133,
     133,   134,   134,   135,   135,   136,   136,   137,   137,   138,
     138,   139,   139,   140,   140,   141,   141,   142,   142,   143,
     143,   144,   144
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     0,     2,     2,     2,     2,     1,     4,     2,
       2,     2,     2,     2,     2,     0,     3,     0,     3,     1,
       1,     0,     3,     1,     2,     3,     2,     2,     2,     2,
       1,     2,     3,     2,     1,     2,     2,     3,     1,     2,
       1,     2,     3,     0,     2,     3,     1,     1,     1,     1,
       4,     4,     3,     5,     1,     0,     1,     1,     1,     4,
       1,     2,     1,     1,     1,     1,     1,     1,     0,     1,
       0,     1,     1,     3,     1,     2,     0,     1,     2,     1,
       2,     1,     1,     2,     1,     1,     1,     1,     4,     4,
       5,     5,     1,     1,     1,     1,     1,     5,     2,     5,
       2,     2,     1,     1,     1,     1,     1,     1,     5,     1,
       1,     0,     2,     1,     1,     0,     2,     0,     2,     3,
       3,     3,     4,     4,     5,     3,     1,     2,     3,     1,
       3,     2,     3,     1,     2,     1,     3,     3,     4,     1,
       2,     1,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     1,     0,     1,     0,     1,     0,     1,     0,
       1,     0,     1
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
        case 8:
#line 669 "ppparse.y" /* yacc.c:1646  */
    {
	CB_PENDING (_("*CONTROL statement"));
  }
#line 2077 "ppparse.c" /* yacc.c:1646  */
    break;

  case 15:
#line 682 "ppparse.y" /* yacc.c:1646  */
    {
	current_cmd = PLEX_ACT_IF;
  }
#line 2085 "ppparse.c" /* yacc.c:1646  */
    break;

  case 17:
#line 687 "ppparse.y" /* yacc.c:1646  */
    {
	current_cmd = PLEX_ACT_ELIF;
  }
#line 2093 "ppparse.c" /* yacc.c:1646  */
    break;

  case 19:
#line 692 "ppparse.y" /* yacc.c:1646  */
    {
	plex_action_directive (PLEX_ACT_ELSE, 0);
  }
#line 2101 "ppparse.c" /* yacc.c:1646  */
    break;

  case 20:
#line 696 "ppparse.y" /* yacc.c:1646  */
    {
	plex_action_directive (PLEX_ACT_END, 0);
  }
#line 2109 "ppparse.c" /* yacc.c:1646  */
    break;

  case 21:
#line 700 "ppparse.y" /* yacc.c:1646  */
    {
	current_call_convention = 0;
  }
#line 2117 "ppparse.c" /* yacc.c:1646  */
    break;

  case 22:
#line 704 "ppparse.y" /* yacc.c:1646  */
    {
	if (current_call_convention == CB_CONV_STATIC_LINK) {
		current_call_convention |= CB_CONV_COBOL;
	};
  }
#line 2127 "ppparse.c" /* yacc.c:1646  */
    break;

  case 25:
#line 718 "ppparse.y" /* yacc.c:1646  */
    {
	/* note: the old version was _as LITERAL but MF doesn't support this */
	struct cb_define_struct	*p;

	p = ppp_define_add (ppp_setvar_list, (yyvsp[-1].s), (yyvsp[0].s), 1);
	if (p) {
		ppp_setvar_list = p;
		fprintf (ppout, "#DEFLIT %s %s\n", (yyvsp[-1].s), (yyvsp[0].s));
	}
  }
#line 2142 "ppparse.c" /* yacc.c:1646  */
    break;

  case 27:
#line 730 "ppparse.y" /* yacc.c:1646  */
    {
	struct cb_text_list	*l;

	for (l = (yyvsp[0].l); l; l = l->next) {
		fprintf (ppout, "#ADDRSV %s\n", l->text);
	}
  }
#line 2154 "ppparse.c" /* yacc.c:1646  */
    break;

  case 28:
#line 738 "ppparse.y" /* yacc.c:1646  */
    {
      struct cb_text_list	*l;

      for (l = (yyvsp[0].l); l; l = l->next->next) {
	      fprintf (ppout, "#ADDSYN %s %s\n", l->text, l->next->text);
      }
  }
#line 2166 "ppparse.c" /* yacc.c:1646  */
    break;

  case 29:
#line 746 "ppparse.y" /* yacc.c:1646  */
    {
	char	*p = (yyvsp[0].s);
	/* Remove surrounding quotes/brackets */
	size_t	size;
	++p;
	size = strlen (p) - 1;
	p[size] = '\0';
	fprintf (ppout, "#CALLFH \"%s\"\n", p);
  }
#line 2180 "ppparse.c" /* yacc.c:1646  */
    break;

  case 30:
#line 756 "ppparse.y" /* yacc.c:1646  */
    {
	fprintf (ppout, "#CALLFH \"EXTFH\"\n");
  }
#line 2188 "ppparse.c" /* yacc.c:1646  */
    break;

  case 31:
#line 760 "ppparse.y" /* yacc.c:1646  */
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
#line 2210 "ppparse.c" /* yacc.c:1646  */
    break;

  case 32:
#line 778 "ppparse.y" /* yacc.c:1646  */
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
#line 2232 "ppparse.c" /* yacc.c:1646  */
    break;

  case 33:
#line 796 "ppparse.y" /* yacc.c:1646  */
    {
	fprintf (ppout, "#MAKESYN %s %s\n", (yyvsp[0].l)->text, (yyvsp[0].l)->next->text);
  }
#line 2240 "ppparse.c" /* yacc.c:1646  */
    break;

  case 34:
#line 800 "ppparse.y" /* yacc.c:1646  */
    {
	cb_fold_copy = 0;
  }
#line 2248 "ppparse.c" /* yacc.c:1646  */
    break;

  case 35:
#line 804 "ppparse.y" /* yacc.c:1646  */
    {
      struct cb_text_list	*l;

      for (l = (yyvsp[0].l); l; l = l->next->next) {
	      fprintf (ppout, "#OVERRIDE %s %s\n", l->text, l->next->text);
      }
  }
#line 2260 "ppparse.c" /* yacc.c:1646  */
    break;

  case 36:
#line 812 "ppparse.y" /* yacc.c:1646  */
    {
	struct cb_text_list	*l;

	for (l = (yyvsp[0].l); l; l = l->next) {
		fprintf (ppout, "#REMOVE %s\n", l->text);
	}
  }
#line 2272 "ppparse.c" /* yacc.c:1646  */
    break;

  case 37:
#line 820 "ppparse.y" /* yacc.c:1646  */
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
#line 2302 "ppparse.c" /* yacc.c:1646  */
    break;

  case 38:
#line 849 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.l) = ppp_list_add (NULL, (yyvsp[0].s));
  }
#line 2310 "ppparse.c" /* yacc.c:1646  */
    break;

  case 39:
#line 853 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.l) = ppp_list_add ((yyvsp[-1].l), (yyvsp[0].s));
  }
#line 2318 "ppparse.c" /* yacc.c:1646  */
    break;

  case 41:
#line 861 "ppparse.y" /* yacc.c:1646  */
    {
	  (yyval.l) = ppp_list_append ((yyvsp[-1].l), (yyvsp[0].l));
  }
#line 2326 "ppparse.c" /* yacc.c:1646  */
    break;

  case 42:
#line 868 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.l) = ppp_list_add (NULL, (yyvsp[-2].s));
	(yyval.l) = ppp_list_add ((yyval.l), (yyvsp[0].s));
  }
#line 2335 "ppparse.c" /* yacc.c:1646  */
    break;

  case 43:
#line 876 "ppparse.y" /* yacc.c:1646  */
    {
	fprintf (ppout, "#OPTION %s\n", (yyvsp[0].s));
  }
#line 2343 "ppparse.c" /* yacc.c:1646  */
    break;

  case 44:
#line 880 "ppparse.y" /* yacc.c:1646  */
    {
	fprintf (ppout, "#OPTION %s %s\n", (yyvsp[-2].s), (yyvsp[0].s));
  }
#line 2351 "ppparse.c" /* yacc.c:1646  */
    break;

  case 45:
#line 887 "ppparse.y" /* yacc.c:1646  */
    {
	  if (cb_src_list_file) {
		  cb_current_file->source_format = cb_source_format;
	  }
  }
#line 2361 "ppparse.c" /* yacc.c:1646  */
    break;

  case 46:
#line 896 "ppparse.y" /* yacc.c:1646  */
    {
	cb_source_format = CB_FORMAT_FIXED;
	cb_text_column = cb_config_text_column;
  }
#line 2370 "ppparse.c" /* yacc.c:1646  */
    break;

  case 47:
#line 901 "ppparse.y" /* yacc.c:1646  */
    {
	cb_source_format = CB_FORMAT_FREE;
  }
#line 2378 "ppparse.c" /* yacc.c:1646  */
    break;

  case 48:
#line 905 "ppparse.y" /* yacc.c:1646  */
    {
	cb_source_format = CB_FORMAT_FIXED;
	cb_text_column = 500;
  }
#line 2387 "ppparse.c" /* yacc.c:1646  */
    break;

  case 49:
#line 910 "ppparse.y" /* yacc.c:1646  */
    {
	cb_error (_("invalid %s directive"), "SOURCE");
	YYERROR;
  }
#line 2396 "ppparse.c" /* yacc.c:1646  */
    break;

  case 50:
#line 918 "ppparse.y" /* yacc.c:1646  */
    {
	struct cb_define_struct	*p;

	p = ppp_define_add (ppp_setvar_list, (yyvsp[-3].s), (yyvsp[-1].s), (yyvsp[0].ui));
	if (p) {
		ppp_setvar_list = p;
	}
  }
#line 2409 "ppparse.c" /* yacc.c:1646  */
    break;

  case 51:
#line 927 "ppparse.y" /* yacc.c:1646  */
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
#line 2447 "ppparse.c" /* yacc.c:1646  */
    break;

  case 52:
#line 961 "ppparse.y" /* yacc.c:1646  */
    {
	ppp_define_del ((yyvsp[-2].s));
  }
#line 2455 "ppparse.c" /* yacc.c:1646  */
    break;

  case 53:
#line 965 "ppparse.y" /* yacc.c:1646  */
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
#line 2475 "ppparse.c" /* yacc.c:1646  */
    break;

  case 54:
#line 981 "ppparse.y" /* yacc.c:1646  */
    {
	cb_error (_("invalid %s directive"), "DEFINE/SET");
  }
#line 2483 "ppparse.c" /* yacc.c:1646  */
    break;

  case 71:
#line 1020 "ppparse.y" /* yacc.c:1646  */
    {
	CB_PENDING (_("LEAP-SECOND ON directive"));
  }
#line 2491 "ppparse.c" /* yacc.c:1646  */
    break;

  case 73:
#line 1028 "ppparse.y" /* yacc.c:1646  */
    {
	CB_PENDING (_("TURN directive"));
  }
#line 2499 "ppparse.c" /* yacc.c:1646  */
    break;

  case 84:
#line 1057 "ppparse.y" /* yacc.c:1646  */
    {
	current_call_convention |= CB_CONV_COBOL;
	current_call_convention &= ~CB_CONV_STDCALL;
  }
#line 2508 "ppparse.c" /* yacc.c:1646  */
    break;

  case 85:
#line 1062 "ppparse.y" /* yacc.c:1646  */
    {
	current_call_convention &= ~CB_CONV_STDCALL;
	current_call_convention &= ~CB_CONV_COBOL;
  }
#line 2517 "ppparse.c" /* yacc.c:1646  */
    break;

  case 86:
#line 1067 "ppparse.y" /* yacc.c:1646  */
    {
	current_call_convention |= CB_CONV_STDCALL;
	current_call_convention &= ~CB_CONV_COBOL;
  }
#line 2526 "ppparse.c" /* yacc.c:1646  */
    break;

  case 87:
#line 1072 "ppparse.y" /* yacc.c:1646  */
    {
	current_call_convention |= CB_CONV_STATIC_LINK;
  }
#line 2534 "ppparse.c" /* yacc.c:1646  */
    break;

  case 88:
#line 1079 "ppparse.y" /* yacc.c:1646  */
    {
	unsigned int		found;

	found = (ppp_search_lists ((yyvsp[-3].s)) != NULL);
	plex_action_directive (current_cmd, found ^ (yyvsp[-1].ui));
  }
#line 2545 "ppparse.c" /* yacc.c:1646  */
    break;

  case 89:
#line 1086 "ppparse.y" /* yacc.c:1646  */
    {
	unsigned int		found;

	found = ppp_search_comp_vars ((yyvsp[-3].s));
	plex_action_directive (current_cmd, found ^ (yyvsp[-1].ui));
  }
#line 2556 "ppparse.c" /* yacc.c:1646  */
    break;

  case 90:
#line 1093 "ppparse.y" /* yacc.c:1646  */
    {
	struct cb_define_struct	*p;
	unsigned int		found;

	found = 0;
	p = ppp_search_lists ((yyvsp[-4].s));
	found = ppp_compare_vals (p, (yyvsp[0].ds), (yyvsp[-1].ui));
	plex_action_directive (current_cmd, found ^ (yyvsp[-2].ui));
  }
#line 2570 "ppparse.c" /* yacc.c:1646  */
    break;

  case 91:
#line 1103 "ppparse.y" /* yacc.c:1646  */
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
#line 2589 "ppparse.c" /* yacc.c:1646  */
    break;

  case 92:
#line 1118 "ppparse.y" /* yacc.c:1646  */
    {
	cb_error (_("invalid %s directive"), "IF/ELIF");
  }
#line 2597 "ppparse.c" /* yacc.c:1646  */
    break;

  case 95:
#line 1130 "ppparse.y" /* yacc.c:1646  */
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
#line 2614 "ppparse.c" /* yacc.c:1646  */
    break;

  case 96:
#line 1143 "ppparse.y" /* yacc.c:1646  */
    {
	struct cb_define_struct	*p;

	p = ppp_search_lists ((yyvsp[0].s));
	if (p != NULL && p->deftype != PLEX_DEF_NONE) {
		(yyval.ds) = p;
	} else {
		(yyval.ds) = NULL;
	}
  }
#line 2629 "ppparse.c" /* yacc.c:1646  */
    break;

  case 97:
#line 1157 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.ui) = COND_GE;
  }
#line 2637 "ppparse.c" /* yacc.c:1646  */
    break;

  case 98:
#line 1161 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.ui) = COND_GT;
  }
#line 2645 "ppparse.c" /* yacc.c:1646  */
    break;

  case 99:
#line 1165 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.ui) = COND_LE;
  }
#line 2653 "ppparse.c" /* yacc.c:1646  */
    break;

  case 100:
#line 1169 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.ui) = COND_LT;
  }
#line 2661 "ppparse.c" /* yacc.c:1646  */
    break;

  case 101:
#line 1173 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.ui) = COND_EQ;
  }
#line 2669 "ppparse.c" /* yacc.c:1646  */
    break;

  case 102:
#line 1177 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.ui) = COND_GE;
  }
#line 2677 "ppparse.c" /* yacc.c:1646  */
    break;

  case 103:
#line 1181 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.ui) = COND_GT;
  }
#line 2685 "ppparse.c" /* yacc.c:1646  */
    break;

  case 104:
#line 1185 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.ui) = COND_LE;
  }
#line 2693 "ppparse.c" /* yacc.c:1646  */
    break;

  case 105:
#line 1189 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.ui) = COND_LT;
  }
#line 2701 "ppparse.c" /* yacc.c:1646  */
    break;

  case 106:
#line 1193 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.ui) = COND_EQ;
  }
#line 2709 "ppparse.c" /* yacc.c:1646  */
    break;

  case 107:
#line 1197 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.ui) = COND_NE;
  }
#line 2717 "ppparse.c" /* yacc.c:1646  */
    break;

  case 108:
#line 1204 "ppparse.y" /* yacc.c:1646  */
    {
	fputc ('\n', ppout);
	ppcopy ((yyvsp[-3].s), (yyvsp[-2].s), (yyvsp[0].r));
  }
#line 2726 "ppparse.c" /* yacc.c:1646  */
    break;

  case 109:
#line 1212 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.s) = fix_filename ((yyvsp[0].s));
	if (cb_fold_copy == COB_FOLD_LOWER) {
		(yyval.s) = fold_lower ((yyval.s));
	} else if (cb_fold_copy == COB_FOLD_UPPER) {
		(yyval.s) = fold_upper ((yyval.s));
	}
  }
#line 2739 "ppparse.c" /* yacc.c:1646  */
    break;

  case 110:
#line 1221 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.s) = (yyvsp[0].s);
	if (cb_fold_copy == COB_FOLD_LOWER) {
		(yyval.s) = fold_lower ((yyval.s));
	} else {
		(yyval.s) = fold_upper ((yyval.s));
	}
  }
#line 2752 "ppparse.c" /* yacc.c:1646  */
    break;

  case 111:
#line 1233 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.s) = NULL;
  }
#line 2760 "ppparse.c" /* yacc.c:1646  */
    break;

  case 112:
#line 1237 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.s) = (yyvsp[0].s);
  }
#line 2768 "ppparse.c" /* yacc.c:1646  */
    break;

  case 117:
#line 1253 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.r) = NULL;
  }
#line 2776 "ppparse.c" /* yacc.c:1646  */
    break;

  case 118:
#line 1257 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.r) = (yyvsp[0].r);
  }
#line 2784 "ppparse.c" /* yacc.c:1646  */
    break;

  case 119:
#line 1264 "ppparse.y" /* yacc.c:1646  */
    {
	pp_set_replace_list ((yyvsp[0].r), (yyvsp[-1].ui));
  }
#line 2792 "ppparse.c" /* yacc.c:1646  */
    break;

  case 120:
#line 1268 "ppparse.y" /* yacc.c:1646  */
    {
	pp_set_replace_list (NULL, (yyvsp[-1].ui));
  }
#line 2800 "ppparse.c" /* yacc.c:1646  */
    break;

  case 121:
#line 1275 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.r) = ppp_replace_list_add (NULL, (yyvsp[-2].l), (yyvsp[0].l), 0);
  }
#line 2808 "ppparse.c" /* yacc.c:1646  */
    break;

  case 122:
#line 1279 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.r) = ppp_replace_list_add (NULL, (yyvsp[-2].l), (yyvsp[0].l), (yyvsp[-3].ui));
  }
#line 2816 "ppparse.c" /* yacc.c:1646  */
    break;

  case 123:
#line 1283 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.r) = ppp_replace_list_add ((yyvsp[-3].r), (yyvsp[-2].l), (yyvsp[0].l), 0);
  }
#line 2824 "ppparse.c" /* yacc.c:1646  */
    break;

  case 124:
#line 1287 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.r) = ppp_replace_list_add ((yyvsp[-4].r), (yyvsp[-2].l), (yyvsp[0].l), (yyvsp[-3].ui));
  }
#line 2832 "ppparse.c" /* yacc.c:1646  */
    break;

  case 125:
#line 1294 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.l) = (yyvsp[-1].l);
  }
#line 2840 "ppparse.c" /* yacc.c:1646  */
    break;

  case 126:
#line 1298 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.l) = (yyvsp[0].l);
  }
#line 2848 "ppparse.c" /* yacc.c:1646  */
    break;

  case 127:
#line 1305 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.l) = NULL;
  }
#line 2856 "ppparse.c" /* yacc.c:1646  */
    break;

  case 128:
#line 1309 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.l) = (yyvsp[-1].l);
  }
#line 2864 "ppparse.c" /* yacc.c:1646  */
    break;

  case 129:
#line 1313 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.l) = (yyvsp[0].l);
  }
#line 2872 "ppparse.c" /* yacc.c:1646  */
    break;

  case 130:
#line 1320 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.l) = ppp_list_add (NULL, (yyvsp[-1].s));
  }
#line 2880 "ppparse.c" /* yacc.c:1646  */
    break;

  case 131:
#line 1327 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.l) = NULL;
  }
#line 2888 "ppparse.c" /* yacc.c:1646  */
    break;

  case 132:
#line 1331 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.l) = ppp_list_add (NULL, (yyvsp[-1].s));
  }
#line 2896 "ppparse.c" /* yacc.c:1646  */
    break;

  case 133:
#line 1338 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.l) = ppp_list_add (NULL, (yyvsp[0].s));
  }
#line 2904 "ppparse.c" /* yacc.c:1646  */
    break;

  case 134:
#line 1342 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.l) = ppp_list_add ((yyvsp[-1].l), (yyvsp[0].s));
  }
#line 2912 "ppparse.c" /* yacc.c:1646  */
    break;

  case 135:
#line 1349 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.l) = ppp_list_add (NULL, (yyvsp[0].s));
  }
#line 2920 "ppparse.c" /* yacc.c:1646  */
    break;

  case 136:
#line 1353 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.l) = ppp_list_add ((yyvsp[-2].l), " ");
	(yyval.l) = ppp_list_add ((yyval.l), "IN");
	(yyval.l) = ppp_list_add ((yyval.l), " ");
	(yyval.l) = ppp_list_add ((yyval.l), (yyvsp[0].s));
  }
#line 2931 "ppparse.c" /* yacc.c:1646  */
    break;

  case 137:
#line 1360 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.l) = ppp_list_add ((yyvsp[-2].l), " ");
	(yyval.l) = ppp_list_add ((yyval.l), "OF");
	(yyval.l) = ppp_list_add ((yyval.l), " ");
	(yyval.l) = ppp_list_add ((yyval.l), (yyvsp[0].s));
  }
#line 2942 "ppparse.c" /* yacc.c:1646  */
    break;

  case 138:
#line 1367 "ppparse.y" /* yacc.c:1646  */
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
#line 2958 "ppparse.c" /* yacc.c:1646  */
    break;

  case 139:
#line 1382 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.l) = ppp_list_add (NULL, (yyvsp[0].s));
  }
#line 2966 "ppparse.c" /* yacc.c:1646  */
    break;

  case 140:
#line 1386 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.l) = ppp_list_add ((yyvsp[-1].l), " ");
	(yyval.l) = ppp_list_add ((yyval.l), (yyvsp[0].s));
  }
#line 2975 "ppparse.c" /* yacc.c:1646  */
    break;

  case 141:
#line 1394 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.ui) = CB_REPLACE_LEADING;
  }
#line 2983 "ppparse.c" /* yacc.c:1646  */
    break;

  case 142:
#line 1398 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.ui) = CB_REPLACE_TRAILING;
  }
#line 2991 "ppparse.c" /* yacc.c:1646  */
    break;

  case 143:
#line 1407 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.ui) = 0;
  }
#line 2999 "ppparse.c" /* yacc.c:1646  */
    break;

  case 144:
#line 1411 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.ui) = 1U;
  }
#line 3007 "ppparse.c" /* yacc.c:1646  */
    break;

  case 145:
#line 1418 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.ui) = 0;
  }
#line 3015 "ppparse.c" /* yacc.c:1646  */
    break;

  case 146:
#line 1422 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.ui) = 1U;
  }
#line 3023 "ppparse.c" /* yacc.c:1646  */
    break;

  case 147:
#line 1429 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.ui) = 0;
  }
#line 3031 "ppparse.c" /* yacc.c:1646  */
    break;

  case 148:
#line 1433 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.ui) = 1U;
  }
#line 3039 "ppparse.c" /* yacc.c:1646  */
    break;

  case 149:
#line 1440 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.ui) = 0;
  }
#line 3047 "ppparse.c" /* yacc.c:1646  */
    break;

  case 150:
#line 1444 "ppparse.y" /* yacc.c:1646  */
    {
	(yyval.ui) = 1U;
  }
#line 3055 "ppparse.c" /* yacc.c:1646  */
    break;


#line 3059 "ppparse.c" /* yacc.c:1646  */
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
#line 1456 "ppparse.y" /* yacc.c:1906  */

