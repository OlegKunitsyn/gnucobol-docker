/* A Bison parser, made by GNU Bison 3.0.4.  */

/* Bison interface for Yacc-like parsers in C

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
    COMP1 = 303,
    CONSTANT = 304,
    FOLDCOPYNAME = 305,
    MAKESYN = 306,
    NOFOLDCOPYNAME = 307,
    REMOVE = 308,
    SOURCEFORMAT = 309,
    IF_DIRECTIVE = 310,
    ELSE_DIRECTIVE = 311,
    ENDIF_DIRECTIVE = 312,
    ELIF_DIRECTIVE = 313,
    GE = 314,
    LE = 315,
    LT = 316,
    GT = 317,
    EQ = 318,
    NE = 319,
    NOT = 320,
    THAN = 321,
    TO = 322,
    OR = 323,
    EQUAL = 324,
    GREATER = 325,
    LESS = 326,
    SET = 327,
    DEFINED = 328,
    TURN_DIRECTIVE = 329,
    ON = 330,
    CHECKING = 331,
    WITH = 332,
    LOCATION = 333,
    TERMINATOR = 334,
    TOKEN = 335,
    VARIABLE_NAME = 336,
    LITERAL = 337
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
#define COMP1 303
#define CONSTANT 304
#define FOLDCOPYNAME 305
#define MAKESYN 306
#define NOFOLDCOPYNAME 307
#define REMOVE 308
#define SOURCEFORMAT 309
#define IF_DIRECTIVE 310
#define ELSE_DIRECTIVE 311
#define ENDIF_DIRECTIVE 312
#define ELIF_DIRECTIVE 313
#define GE 314
#define LE 315
#define LT 316
#define GT 317
#define EQ 318
#define NE 319
#define NOT 320
#define THAN 321
#define TO 322
#define OR 323
#define EQUAL 324
#define GREATER 325
#define LESS 326
#define SET 327
#define DEFINED 328
#define TURN_DIRECTIVE 329
#define ON 330
#define CHECKING 331
#define WITH 332
#define LOCATION 333
#define TERMINATOR 334
#define TOKEN 335
#define VARIABLE_NAME 336
#define LITERAL 337

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED

union YYSTYPE
{
#line 523 "/media/sf_dev_desktop/gnucobol-3.x/cobc/ppparse.y" /* yacc.c:1909  */

	char			*s;
	struct cb_text_list	*l;
	struct cb_replace_list	*r;
	struct cb_define_struct	*ds;
	unsigned int		ui;
	int			si;

#line 229 "ppparse.h" /* yacc.c:1909  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE pplval;

int ppparse (void);

#endif /* !YY_PP_PPPARSE_H_INCLUDED  */
