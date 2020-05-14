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
#line 523 "ppparse.y" /* yacc.c:1909  */

	char			*s;
	struct cb_text_list	*l;
	struct cb_replace_list	*r;
	struct cb_define_struct	*ds;
	unsigned int		ui;
	int			si;

#line 233 "ppparse.h" /* yacc.c:1909  */
};

typedef union YYSTYPE YYSTYPE;
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE pplval;

int ppparse (void);

#endif /* !YY_PP_PPPARSE_H_INCLUDED  */
