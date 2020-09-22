/* A Bison parser, made by GNU Bison 3.3.2.  */

/* Bison interface for Yacc-like parsers in C

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

/* Undocumented macros, especially those whose name start with YY_,
   are private implementation details.  Do not rely on them.  */

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
#line 554 "/mnt/d/Programme/Entwicklung/GnuCOBOL/code/tags/gnucobol-3.1-rc1/cobc/ppparse.y" /* yacc.c:1921  */

	char			*s;
	struct cb_text_list	*l;
	struct cb_replace_list	*r;
	struct cb_define_struct	*ds;
	unsigned int		ui;
	int			si;

#line 247 "ppparse.h" /* yacc.c:1921  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE pplval;

int ppparse (void);

#endif /* !YY_PP_PPPARSE_H_INCLUDED  */
