/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison interface for Yacc-like parsers in C

   Copyright (C) 1984, 1989-1990, 2000-2013 Free Software Foundation, Inc.

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

#ifndef YY_YY_GENERATE_PARSER_TAB_H_INCLUDED
# define YY_YY_GENERATE_PARSER_TAB_H_INCLUDED
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
    DELIM = 258,
    EQ = 259,
    ALLOC = 260,
    BOOL = 261,
    CHAR = 262,
    DOUBLE = 263,
    FLOAT = 264,
    INT = 265,
    LINT = 266,
    STR = 267,
    TYPE = 268,
    CONTAINS = 269,
    EXTENDS = 270,
    END = 271,
    IMPLICIT = 272,
    INCLUDE = 273,
    KIND = 274,
    MODULE = 275,
    ONLY = 276,
    PARAM = 277,
    SUBROUT = 278,
    USE = 279,
    NAME = 280,
    NUM = 281
  };
#endif

/* Value type.  */
#if ! defined YYSTYPE && ! defined YYSTYPE_IS_DECLARED
typedef union YYSTYPE YYSTYPE;
union YYSTYPE
{
#line 31 "generate_parser.y" /* yacc.c:1909  */

    char sval[STR_LEN];
    char stck[3][STR_LEN];

#line 86 "generate_parser.tab.h" /* yacc.c:1909  */
};
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (FILE* f[], const char* filename);

#endif /* !YY_YY_GENERATE_PARSER_TAB_H_INCLUDED  */
