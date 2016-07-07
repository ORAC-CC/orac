/* A Bison parser, made by GNU Bison 3.0.2.  */

/* Bison implementation for Yacc-like parsers in C

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
#define YYBISON_VERSION "3.0.2"

/* Skeleton name.  */
#define YYSKELETON_NAME "yacc.c"

/* Pure parsers.  */
#define YYPURE 0

/* Push parsers.  */
#define YYPUSH 0

/* Pull parsers.  */
#define YYPULL 1




/* Copy the first part of user declarations.  */
#line 23 "generate_parser.y" /* yacc.c:339  */

#include <ctype.h>

#include "generate_parser.h"

int yylex();
void yyerror(FILE* f[], const char* filename, char *s);

// Locals
char current_type[STR_LEN];

#line 78 "generate_parser.tab.c" /* yacc.c:339  */

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
# define YYERROR_VERBOSE 0
#endif

/* In a future release of Bison, this section will be replaced
   by #include "generate_parser.tab.h".  */
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
#line 36 "generate_parser.y" /* yacc.c:355  */

    char sval[STR_LEN];
    char stck[3][STR_LEN];

#line 150 "generate_parser.tab.c" /* yacc.c:355  */
};
# define YYSTYPE_IS_TRIVIAL 1
# define YYSTYPE_IS_DECLARED 1
#endif


extern YYSTYPE yylval;

int yyparse (FILE* f[], const char* filename);

#endif /* !YY_YY_GENERATE_PARSER_TAB_H_INCLUDED  */

/* Copy the second part of user declarations.  */

#line 165 "generate_parser.tab.c" /* yacc.c:358  */

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
#define YYFINAL  5
/* YYLAST -- Last index in YYTABLE.  */
#define YYLAST   208

/* YYNTOKENS -- Number of terminals.  */
#define YYNTOKENS  31
/* YYNNTS -- Number of nonterminals.  */
#define YYNNTS  26
/* YYNRULES -- Number of rules.  */
#define YYNRULES  89
/* YYNSTATES -- Number of states.  */
#define YYNSTATES  157

/* YYTRANSLATE[YYX] -- Symbol number corresponding to YYX as returned
   by yylex, with out-of-bounds checking.  */
#define YYUNDEFTOK  2
#define YYMAXUTOK   281

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
      29,    30,     2,     2,    27,     2,     2,     2,     2,     2,
       2,     2,     2,     2,     2,     2,     2,     2,    28,     2,
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
      25,    26
};

#if YYDEBUG
  /* YYRLINE[YYN] -- Source line where rule number YYN was defined.  */
static const yytype_uint8 yyrline[] =
{
       0,    54,    54,    56,    58,    59,    60,    61,    64,    65,
      66,    68,    69,    72,    73,    76,    77,    79,    80,    81,
      82,    83,    85,    87,    90,    92,    98,    99,   101,   104,
     105,   108,   116,   127,   138,   146,   154,   160,   161,   162,
     165,   167,   169,   172,   175,   178,   181,   184,   187,   192,
     193,   194,   197,   199,   201,   202,   204,   204,   204,   204,
     204,   204,   204,   206,   207,   209,   209,   209,   209,   209,
     209,   209,   209,   210,   210,   210,   210,   210,   210,   211,
     211,   211,   211,   211,   211,   211,   212,   212,   212,   212
};
#endif

#if YYDEBUG || YYERROR_VERBOSE || 0
/* YYTNAME[SYMBOL-NUM] -- String name of the symbol SYMBOL-NUM.
   First, the terminals, then, starting at YYNTOKENS, nonterminals.  */
static const char *const yytname[] =
{
  "$end", "error", "$undefined", "DELIM", "EQ", "ALLOC", "BOOL", "CHAR",
  "DOUBLE", "FLOAT", "INT", "LINT", "STR", "TYPE", "CONTAINS", "EXTENDS",
  "END", "IMPLICIT", "INCLUDE", "KIND", "MODULE", "ONLY", "PARAM",
  "SUBROUT", "USE", "NAME", "NUM", "','", "':'", "'('", "')'", "$accept",
  "module_def", "module_name", "definitions", "uses", "use_line", "names",
  "contents", "contents_lines", "include_line", "subroutine", "type_def",
  "type_open", "extension", "type_close", "variables", "variable",
  "parameter", "declaration", "datatype", "dims", "val", "trash", "text",
  "code", "any_token", YY_NULLPTR
};
#endif

# ifdef YYPRINT
/* YYTOKNUM[NUM] -- (External) token number corresponding to the
   (internal) symbol number NUM (which must be that of a token).  */
static const yytype_uint16 yytoknum[] =
{
       0,   256,   257,   258,   259,   260,   261,   262,   263,   264,
     265,   266,   267,   268,   269,   270,   271,   272,   273,   274,
     275,   276,   277,   278,   279,   280,   281,    44,    58,    40,
      41
};
# endif

#define YYPACT_NINF -84

#define yypact_value_is_default(Yystate) \
  (!!((Yystate) == (-84)))

#define YYTABLE_NINF -1

#define yytable_value_is_error(Yytable_value) \
  0

  /* YYPACT[STATE-NUM] -- Index in YYTABLE of the portion describing
     STATE-NUM.  */
static const yytype_int16 yypact[] =
{
      72,    77,   128,   115,   -84,   -84,   113,     6,   -84,   116,
     114,   -84,   119,   -84,   -84,   -84,   -84,   -84,   -84,   117,
      90,   -84,   106,   -84,    35,   112,   118,   127,   120,   108,
     131,   -84,   -84,   121,   122,    78,   -84,    66,   123,   130,
      64,   124,   125,   -84,   132,   133,   108,   -84,   -84,    72,
       9,   150,   134,   142,   -84,   -84,   135,   151,   -84,   158,
     159,   136,   -84,   137,   140,   -84,   138,   -84,   -84,   -84,
     -84,   -84,   102,   143,   139,   145,   121,   168,   147,    99,
     -84,   148,   144,   124,     9,   -84,   -84,   172,   -84,   -84,
     152,    32,   146,   -84,   175,   103,   149,   155,   153,   110,
      51,   -84,   -84,   115,   -84,   -84,   156,    51,   -84,   -84,
     -84,   -84,   -84,   -84,   -84,   -84,    51,   -84,    15,   107,
      51,   -84,    38,   157,   -84,   -84,   -84,   -84,   -84,   -84,
     -84,   -84,   -84,   -84,   -84,   -84,   -84,   -84,   -84,   -84,
     -84,   -84,   -84,   -84,   -84,   -84,   -84,   -84,   -84,   -84,
      -1,   -84,   160,   161,   -84,   -84,   -84
};

  /* YYDEFACT[STATE-NUM] -- Default reduction number in state STATE-NUM.
     Performed when YYTABLE does not specify something else to do.  Zero
     means the default is an error.  */
static const yytype_uint8 yydefact[] =
{
       0,     0,     0,    10,     3,     1,     0,     0,     9,    12,
       0,     8,     0,    43,    44,    48,    47,    45,    46,    27,
      16,     6,     0,     7,     0,    42,     0,     0,     0,    21,
       0,     4,     5,    51,     0,     0,    30,     0,     0,     0,
       0,     0,     0,    25,     0,     0,    15,    19,    20,     0,
       0,     0,     0,     0,    24,    29,     0,     0,    39,     0,
       0,     0,    14,    11,     0,    22,     0,    17,    18,     2,
      52,    53,     0,     0,     0,     0,    51,     0,     0,     0,
      40,     0,     0,     0,     0,    50,    31,     0,    28,    34,
       0,     0,     0,    13,     0,     0,     0,     0,     0,     0,
       0,    41,    26,    10,    49,    35,     0,    52,    36,    58,
      56,    57,    61,    62,    59,    60,    38,    55,     0,     0,
      37,    54,     0,     0,    32,    69,    72,    65,    66,    67,
      70,    74,    76,    78,    84,    85,    68,    73,    71,    75,
      77,    79,    82,    83,    80,    81,    88,    89,    86,    87,
       0,    64,     0,     0,    63,    33,    23
};

  /* YYPGOTO[NTERM-NUM].  */
static const yytype_int16 yypgoto[] =
{
     -84,   -84,   154,   -84,    59,    -7,    98,   -84,   -84,   141,
     162,   163,   -84,   -84,   -84,   -84,   164,   169,    60,   -39,
     126,   -68,    81,   -83,   -84,    41
};

  /* YYDEFGOTO[NTERM-NUM].  */
static const yytype_int16 yydefgoto[] =
{
      -1,     2,     3,    20,     7,     8,    63,    30,    46,    47,
      48,    21,    22,    28,    54,    35,    36,    23,    24,    25,
      51,    72,   116,   117,   150,   151
};

  /* YYTABLE[YYPACT[STATE-NUM]] -- What to do in state STATE-NUM.  If
     positive, shift that token.  If negative, reduce the rule whose
     number is the opposite.  If YYTABLE_NINF, syntax error.  */
static const yytype_uint8 yytable[] =
{
      11,    61,   125,   126,   127,   128,   129,   130,   131,   132,
     133,   134,   135,   136,   137,   138,    96,   139,   140,   141,
     142,   143,   153,    10,   144,   145,   146,   147,   148,   149,
       6,   108,   122,   121,    70,    71,    99,   121,    38,     6,
      92,   125,   126,   127,   128,   129,   130,   131,   132,   133,
     134,   135,   136,   137,   138,   109,   139,   140,   141,   142,
     143,   100,    39,   144,   145,   146,   147,   148,   149,    56,
      13,    14,    15,    16,    17,    18,   110,   111,   112,   113,
     114,   115,    37,    60,    13,    14,    15,    16,    17,    18,
      33,    34,     1,    57,    53,    37,    13,    14,    15,    16,
      17,    18,     4,    19,    29,    13,    14,    15,    16,    17,
      18,    11,    13,    14,    15,    16,    17,    18,    33,    34,
      13,    14,    15,    16,    17,    18,    44,    19,     5,    84,
      81,    45,    85,   103,   123,   107,    71,   124,     9,     6,
      26,    40,    42,    12,    27,    43,    41,    49,    58,    62,
      50,    52,    59,    73,    64,    75,    77,    65,    66,    74,
      76,    78,   118,    79,    81,    82,    80,    83,    86,    87,
      88,    90,    91,    93,    94,    97,   101,    98,   102,   104,
     105,    95,   106,    31,   119,   152,   156,    67,   120,    32,
     155,   154,     0,     0,     0,     0,     0,     0,     0,    55,
       0,     0,    89,    69,     0,     0,     0,     0,    68
};

static const yytype_int16 yycheck[] =
{
       7,    40,     3,     4,     5,     6,     7,     8,     9,    10,
      11,    12,    13,    14,    15,    16,    84,    18,    19,    20,
      21,    22,    23,    17,    25,    26,    27,    28,    29,    30,
      24,    99,    17,   116,    25,    26,     4,   120,     3,    24,
      79,     3,     4,     5,     6,     7,     8,     9,    10,    11,
      12,    13,    14,    15,    16,     4,    18,    19,    20,    21,
      22,    29,    27,    25,    26,    27,    28,    29,    30,     3,
       6,     7,     8,     9,    10,    11,    25,    26,    27,    28,
      29,    30,    22,    19,     6,     7,     8,     9,    10,    11,
      12,    13,    20,    27,    16,    35,     6,     7,     8,     9,
      10,    11,    25,    13,    14,     6,     7,     8,     9,    10,
      11,   118,     6,     7,     8,     9,    10,    11,    12,    13,
       6,     7,     8,     9,    10,    11,    18,    13,     0,    27,
      27,    23,    30,    30,    27,    25,    26,    30,    25,    24,
      21,    29,    15,    27,    27,    25,    28,    16,    25,    25,
      29,    29,    22,     3,    29,    13,     5,    25,    25,    25,
      25,     3,   103,     4,    27,    25,    30,    29,    25,    30,
      25,     3,    25,    25,    30,     3,    30,    25,     3,    30,
      25,    83,    29,    20,    28,    28,    25,    46,   107,    20,
      30,   150,    -1,    -1,    -1,    -1,    -1,    -1,    -1,    35,
      -1,    -1,    76,    49,    -1,    -1,    -1,    -1,    46
};

  /* YYSTOS[STATE-NUM] -- The (internal number of the) accessing
     symbol of state STATE-NUM.  */
static const yytype_uint8 yystos[] =
{
       0,    20,    32,    33,    25,     0,    24,    35,    36,    25,
      17,    36,    27,     6,     7,     8,     9,    10,    11,    13,
      34,    42,    43,    48,    49,    50,    21,    27,    44,    14,
      38,    42,    48,    12,    13,    46,    47,    49,     3,    27,
      29,    28,    15,    25,    18,    23,    39,    40,    41,    16,
      29,    51,    29,    16,    45,    47,     3,    27,    25,    22,
      19,    50,    25,    37,    29,    25,    25,    40,    41,    33,
      25,    26,    52,     3,    25,    13,    25,     5,     3,     4,
      30,    27,    25,    29,    27,    30,    25,    30,    25,    51,
       3,    25,    50,    25,    30,    37,    52,     3,    25,     4,
      29,    30,     3,    30,    30,    25,    29,    25,    52,     4,
      25,    26,    27,    28,    29,    30,    53,    54,    35,    28,
      53,    54,    17,    27,    30,     3,     4,     5,     6,     7,
       8,     9,    10,    11,    12,    13,    14,    15,    16,    18,
      19,    20,    21,    22,    25,    26,    27,    28,    29,    30,
      55,    56,    28,    23,    56,    30,    25
};

  /* YYR1[YYN] -- Symbol number of symbol that rule YYN derives.  */
static const yytype_uint8 yyr1[] =
{
       0,    31,    32,    33,    34,    34,    34,    34,    35,    35,
      35,    36,    36,    37,    37,    38,    38,    39,    39,    39,
      39,    39,    40,    41,    42,    43,    44,    44,    45,    46,
      46,    47,    47,    47,    47,    47,    48,    48,    48,    48,
      49,    49,    49,    50,    50,    50,    50,    50,    50,    51,
      51,    51,    52,    52,    53,    53,    54,    54,    54,    54,
      54,    54,    54,    55,    55,    56,    56,    56,    56,    56,
      56,    56,    56,    56,    56,    56,    56,    56,    56,    56,
      56,    56,    56,    56,    56,    56,    56,    56,    56,    56
};

  /* YYR2[YYN] -- Number of symbols on the right hand side of rule YYN.  */
static const yytype_uint8 yyr2[] =
{
       0,     2,     7,     2,     2,     2,     1,     1,     2,     1,
       0,     6,     2,     3,     1,     2,     0,     2,     2,     1,
       1,     0,     2,    10,     3,     3,     6,     0,     3,     2,
       1,     4,     8,    10,     4,     6,     7,     8,     7,     3,
       4,     6,     1,     1,     1,     1,     1,     1,     1,     5,
       3,     0,     1,     1,     2,     1,     1,     1,     1,     1,
       1,     1,     1,     2,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1,
       1,     1,     1,     1,     1,     1,     1,     1,     1,     1
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
      yyerror (f, filename, YY_("syntax error: cannot back up")); \
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
                  Type, Value, f, filename); \
      YYFPRINTF (stderr, "\n");                                           \
    }                                                                     \
} while (0)


/*----------------------------------------.
| Print this symbol's value on YYOUTPUT.  |
`----------------------------------------*/

static void
yy_symbol_value_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, FILE* f[], const char* filename)
{
  FILE *yyo = yyoutput;
  YYUSE (yyo);
  YYUSE (f);
  YYUSE (filename);
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
yy_symbol_print (FILE *yyoutput, int yytype, YYSTYPE const * const yyvaluep, FILE* f[], const char* filename)
{
  YYFPRINTF (yyoutput, "%s %s (",
             yytype < YYNTOKENS ? "token" : "nterm", yytname[yytype]);

  yy_symbol_value_print (yyoutput, yytype, yyvaluep, f, filename);
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
yy_reduce_print (yytype_int16 *yyssp, YYSTYPE *yyvsp, int yyrule, FILE* f[], const char* filename)
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
                                              , f, filename);
      YYFPRINTF (stderr, "\n");
    }
}

# define YY_REDUCE_PRINT(Rule)          \
do {                                    \
  if (yydebug)                          \
    yy_reduce_print (yyssp, yyvsp, Rule, f, filename); \
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
yydestruct (const char *yymsg, int yytype, YYSTYPE *yyvaluep, FILE* f[], const char* filename)
{
  YYUSE (yyvaluep);
  YYUSE (f);
  YYUSE (filename);
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
yyparse (FILE* f[], const char* filename)
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
        case 25:
#line 92 "generate_parser.y" /* yacc.c:1646  */
    {
                    // Declare current structure is this type
                    strupp(current_type, (yyvsp[0].sval));
                    print_struct_in_def(f, current_type, (yyvsp[-1].sval));
                }
#line 1364 "generate_parser.tab.c" /* yacc.c:1646  */
    break;

  case 26:
#line 98 "generate_parser.y" /* yacc.c:1646  */
    { strcpy((yyval.sval), (yyvsp[-2].sval)); }
#line 1370 "generate_parser.tab.c" /* yacc.c:1646  */
    break;

  case 27:
#line 99 "generate_parser.y" /* yacc.c:1646  */
    { (yyval.sval)[0] = '\0'; }
#line 1376 "generate_parser.tab.c" /* yacc.c:1646  */
    break;

  case 28:
#line 101 "generate_parser.y" /* yacc.c:1646  */
    { fprintf(f[C_DEF], "} %s;\n\n", (yyvsp[0].sval)); }
#line 1382 "generate_parser.tab.c" /* yacc.c:1646  */
    break;

  case 31:
#line 108 "generate_parser.y" /* yacc.c:1646  */
    {
                    // A string
                    print_var_in_c_def(f, (yyvsp[0].sval), "char");
                    print_str_in_x_rul(f, current_type, (yyvsp[0].sval));
                    print_var_in_c_wrapper(f, current_type, (yyvsp[0].sval), "char");
                    print_var_in_f(f, current_type, (yyvsp[0].sval),
                                   "character(c_char)","*");
                }
#line 1395 "generate_parser.tab.c" /* yacc.c:1646  */
    break;

  case 32:
#line 116 "generate_parser.y" /* yacc.c:1646  */
    {
                    // A 1D allocatable array
                    print_var_in_c_def(f, (yyvsp[-3].sval), (yyvsp[-7].stck)[TYPE_C]);
                    print_dim_in_c_def(f, (yyvsp[-3].sval), 1);

                    print_var_in_x_rul(f, current_type, (yyvsp[-3].sval), (yyvsp[-7].stck)[TYPE_C],
                                       (yyvsp[-7].stck)[TYPE_UPP], " ", 1);
                    print_alloc_in_c_wrapper(f, current_type, (yyvsp[-3].sval),
                                             (yyvsp[-7].stck)[TYPE_C], 1);
                    print_alloc_in_f(f, current_type, (yyvsp[-3].sval), 1);
                }
#line 1411 "generate_parser.tab.c" /* yacc.c:1646  */
    break;

  case 33:
#line 127 "generate_parser.y" /* yacc.c:1646  */
    {
                    // A 2D allocatable array
                    print_var_in_c_def(f, (yyvsp[-5].sval), (yyvsp[-9].stck)[TYPE_C]);
                    print_dim_in_c_def(f, (yyvsp[-5].sval), 2);

                    print_var_in_x_rul(f, current_type, (yyvsp[-5].sval), (yyvsp[-9].stck)[TYPE_C],
                                       (yyvsp[-9].stck)[TYPE_UPP], " ", 2);
                    print_alloc_in_c_wrapper(f, current_type, (yyvsp[-5].sval),
                                             (yyvsp[-9].stck)[TYPE_C], 2);
                    print_alloc_in_f(f, current_type, (yyvsp[-5].sval), 2);
               }
#line 1427 "generate_parser.tab.c" /* yacc.c:1646  */
    break;

  case 34:
#line 138 "generate_parser.y" /* yacc.c:1646  */
    {
                    // A static array or scalar
                    print_var_in_c_def(f, (yyvsp[-1].sval), (yyvsp[-3].stck)[TYPE_C]);
                    print_var_in_x_rul(f, current_type, (yyvsp[-1].sval), (yyvsp[-3].stck)[TYPE_C],
                                       (yyvsp[-3].stck)[TYPE_UPP], (yyvsp[0].sval), 0);
                    print_var_in_c_wrapper(f, current_type, (yyvsp[-1].sval), (yyvsp[-3].stck)[TYPE_C]);
                    print_var_in_f(f, current_type, (yyvsp[-1].sval), (yyvsp[-3].stck)[TYPE_F], (yyvsp[0].sval));
                }
#line 1440 "generate_parser.tab.c" /* yacc.c:1646  */
    break;

  case 35:
#line 146 "generate_parser.y" /* yacc.c:1646  */
    {
                    // Sort out tree of names within structure
                    print_struct_in_c_def(f, (yyvsp[-3].sval), current_type, (yyvsp[0].sval));
                    print_struct_in_x_rul(f, (yyvsp[-3].sval), current_type, (yyvsp[0].sval));
                    print_struct_in_f(f, (yyvsp[-3].sval), current_type, (yyvsp[0].sval));
                }
#line 1451 "generate_parser.tab.c" /* yacc.c:1646  */
    break;

  case 36:
#line 154 "generate_parser.y" /* yacc.c:1646  */
    {
                    print_const_in_c_def(f, (yyvsp[-2].sval), (yyvsp[0].sval));
                    print_const_in_x_rul(f, (yyvsp[-2].sval));
                    print_const_in_f(f, (yyvsp[-2].sval), (yyvsp[0].sval));
                }
#line 1461 "generate_parser.tab.c" /* yacc.c:1646  */
    break;

  case 40:
#line 165 "generate_parser.y" /* yacc.c:1646  */
    {
                                         memcpy((yyval.stck), (yyvsp[-1].stck), STR_LEN*3); }
#line 1468 "generate_parser.tab.c" /* yacc.c:1646  */
    break;

  case 41:
#line 167 "generate_parser.y" /* yacc.c:1646  */
    {
                                         memcpy((yyval.stck), (yyvsp[-1].stck), STR_LEN*3); }
#line 1475 "generate_parser.tab.c" /* yacc.c:1646  */
    break;

  case 42:
#line 169 "generate_parser.y" /* yacc.c:1646  */
    { memcpy((yyval.stck), (yyvsp[0].stck), STR_LEN*3); }
#line 1481 "generate_parser.tab.c" /* yacc.c:1646  */
    break;

  case 43:
#line 172 "generate_parser.y" /* yacc.c:1646  */
    { strcpy((yyval.stck)[TYPE_UPP], "BOOL");
                                         strcpy((yyval.stck)[TYPE_C], "bool");
                                         strcpy((yyval.stck)[TYPE_F], "logical"); }
#line 1489 "generate_parser.tab.c" /* yacc.c:1646  */
    break;

  case 44:
#line 175 "generate_parser.y" /* yacc.c:1646  */
    { strcpy((yyval.stck)[TYPE_UPP], "CHAR");
                                         strcpy((yyval.stck)[TYPE_C], "char");
                                         strcpy((yyval.stck)[TYPE_F], "integer(byte)"); }
#line 1497 "generate_parser.tab.c" /* yacc.c:1646  */
    break;

  case 45:
#line 178 "generate_parser.y" /* yacc.c:1646  */
    { strcpy((yyval.stck)[TYPE_UPP], "INT");
                                         strcpy((yyval.stck)[TYPE_C], "int");
                                         strcpy((yyval.stck)[TYPE_F], "integer"); }
#line 1505 "generate_parser.tab.c" /* yacc.c:1646  */
    break;

  case 46:
#line 181 "generate_parser.y" /* yacc.c:1646  */
    { strcpy((yyval.stck)[TYPE_UPP], "INT");
                                         strcpy((yyval.stck)[TYPE_C], "long int");
                                         strcpy((yyval.stck)[TYPE_F], "integer(lint)"); }
#line 1513 "generate_parser.tab.c" /* yacc.c:1646  */
    break;

  case 47:
#line 184 "generate_parser.y" /* yacc.c:1646  */
    { strcpy((yyval.stck)[TYPE_UPP], "FLOAT");
                                         strcpy((yyval.stck)[TYPE_C], "float");
                                         strcpy((yyval.stck)[TYPE_F], "real"); }
#line 1521 "generate_parser.tab.c" /* yacc.c:1646  */
    break;

  case 48:
#line 187 "generate_parser.y" /* yacc.c:1646  */
    { strcpy((yyval.stck)[TYPE_UPP], "FLOAT");
                                         strcpy((yyval.stck)[TYPE_C], "double");
                                         strcpy((yyval.stck)[TYPE_F], "real(dreal)"); }
#line 1529 "generate_parser.tab.c" /* yacc.c:1646  */
    break;

  case 49:
#line 192 "generate_parser.y" /* yacc.c:1646  */
    { sprintf((yyval.sval), "%s, %s", (yyvsp[-3].sval), (yyvsp[-1].sval)); }
#line 1535 "generate_parser.tab.c" /* yacc.c:1646  */
    break;

  case 50:
#line 193 "generate_parser.y" /* yacc.c:1646  */
    { sprintf((yyval.sval), "%s", (yyvsp[-1].sval)); }
#line 1541 "generate_parser.tab.c" /* yacc.c:1646  */
    break;

  case 51:
#line 194 "generate_parser.y" /* yacc.c:1646  */
    { strcpy((yyval.sval), ""); }
#line 1547 "generate_parser.tab.c" /* yacc.c:1646  */
    break;

  case 52:
#line 197 "generate_parser.y" /* yacc.c:1646  */
    { strupp((yyval.sval), (yyvsp[0].sval));
                                         strcat((yyval.sval), "_TOKEN"); }
#line 1554 "generate_parser.tab.c" /* yacc.c:1646  */
    break;

  case 53:
#line 199 "generate_parser.y" /* yacc.c:1646  */
    { strcpy((yyval.sval), (yyvsp[0].sval)); }
#line 1560 "generate_parser.tab.c" /* yacc.c:1646  */
    break;


#line 1564 "generate_parser.tab.c" /* yacc.c:1646  */
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
      yyerror (f, filename, YY_("syntax error"));
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
        yyerror (f, filename, yymsgp);
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
                      yytoken, &yylval, f, filename);
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
                  yystos[yystate], yyvsp, f, filename);
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
  yyerror (f, filename, YY_("memory exhausted"));
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
                  yytoken, &yylval, f, filename);
    }
  /* Do not reclaim the symbols of the rule whose action triggered
     this YYABORT or YYACCEPT.  */
  YYPOPSTACK (yylen);
  YY_STACK_PRINT (yyss, yyssp);
  while (yyssp != yyss)
    {
      yydestruct ("Cleanup: popping",
                  yystos[*yyssp], yyvsp, f, filename);
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
#line 215 "generate_parser.y" /* yacc.c:1906  */


// Make string upper case
char* strupp(char* out, char* in) {
    int i;
    int n = strlen(in);
    for (i=0; i<=n; i++) { out[i] = toupper(in[i]); }
    return(out);
}

// Declare a structure within another structure in C header file
void print_struct_in_c_def(FILE* f[], char* struct_name, char* parent_struct,
                           char* var_name) {
    char upp_struct_name[STR_LEN];
    strupp(upp_struct_name, struct_name);

    // Note that any element of this structure will be an element of the parent
    fprintf(f[C_DEF], "#define %s_VARIABLE %s_VARIABLE.%s\n",
            upp_struct_name, parent_struct, var_name);
    // Declaration of structure variable
    fprintf(f[C_DEF], "    %s %s;\n", struct_name, var_name);
}

// Declare variable within structure in C header file
void print_var_in_c_def(FILE* f[], char* name, char* type) {
    fprintf(f[C_DEF], "    %s* %s;\n", type, name);
}

// Declare dimension size variable for allocatable array in C header file
void print_dim_in_c_def(FILE* f[], char* name, int dim) {
    int i;
    for (i=0; i<dim; i++) {
        fprintf(f[C_DEF], "    int %s_dim%d;\n", name, i);
    }
}

// Define a token to represent some constant defined for Fortran in C header file
void print_const_in_c_def(FILE* f[], char* name, char* value) {
    char upp_name[STR_LEN];
    strupp(upp_name, name);
    fprintf(f[C_DEF], "#define %s_TOKEN %s\n", upp_name, value);
}

// Write flex rule to parse some constant defined for Fortran
void print_const_in_x_rul(FILE* f[], char* name) {
    char upp_name[STR_LEN];
    strupp(upp_name, name);

    fprintf(f[X_RUL], "(?i:\"%s\") {\n", name);
    fprintf(f[X_RUL], "      yylval->val = %s_TOKEN;\n", upp_name);
    fprintf(f[X_RUL], "      return(token::NUM); }\n");
}

// Write flex rule to parse a string variable
void print_str_in_x_rul(FILE* f[], char* parent_struct, char* name) {
    fprintf(f[X_RUL], "<%s_SC>(?i:\"%s\") {\n", parent_struct, name);
    fprintf(f[X_RUL], "      yylval->str_v = %s_VARIABLE.%s;\n",
            parent_struct, name);
    fprintf(f[X_RUL], "      BEGIN(INITIAL); return(token::STR_VAR); }\n");
    fprintf(f[F_CP2], "   call strip_c_nulls(%s_VARIABLE%%%s)\n",
            parent_struct, name);
}

// Write flex rule to parse a structure
void print_struct_in_x_rul(FILE* f[], char* struct_name, char* parent_struct,
                             char* var_name) {
    char upp_struct_name[STR_LEN];
    strupp(upp_struct_name, struct_name);

    fprintf(f[X_RUL], "<%s_SC>(?i:\"%s\"){DELIM} {\n", parent_struct, var_name);
    fprintf(f[X_RUL], "      BEGIN(%s_SC); }\n", upp_struct_name);
}

// Write flex rule to parse a variable
void print_var_in_x_rul(FILE* f[], char* parent_struct, char* name, char* type_c,
                        char* type_u, char* len, int alloc) {
    int i;
    char tmp_len[STR_LEN], tmp[STR_LEN];
    strcpy(tmp_len, len);

    // Remove space from "long int"
    char tmp_type[STR_LEN];
    strcpy(tmp_type, type_c);
    char* p = strstr(tmp_type, " ");
    if (p != NULL) *p = '_';

    // Define rule
    fprintf(f[X_RUL], "<%s_SC>(?i:\"%s\") {\n", parent_struct, name);

    // If an allocated array, call Fortran to allocate
    if (alloc > 0) {
        tmp_len[0] = '\0';

        fprintf(f[X_RUL], "      fort_alloc_%s_%dd(&%s_VARIABLE.%s, ",
                tmp_type, alloc, parent_struct, name);
        for (i=0; i<alloc; i++) {
            fprintf(f[X_RUL], "&%s_VARIABLE.%s_dim%d, ", parent_struct, name, i);

            sprintf(tmp, "%s_VARIABLE.%s_dim%d", parent_struct, name, i);
            strcat(tmp_len, tmp);
            if (i+1 < alloc) strcat(tmp_len, ", ");
        }
        fprintf(f[X_RUL], "%s_%s_DIMS);\n", parent_struct, name);
    }

    // Point a Target class to this variable and return token
    fprintf(f[X_RUL],
            "      yylval->%s_v = new Target<%s> (%s_VARIABLE.%s",
            tmp_type, type_c, parent_struct, name);
    if (strlen(tmp_len) > 0) { fprintf(f[X_RUL], ", %s", tmp_len); }
    fputs(");\n", f[X_RUL]);
    fprintf(f[X_RUL], "      BEGIN(INITIAL); return(token::%s_VAR); }\n",
            type_u);
}

// Declare beginning of a structure in C and Flex
void print_struct_in_def(FILE* f[], char* type, char* exten_name) {
    char upp_exten_name[STR_LEN];

    fputs("typedef struct {\n", f[C_DEF]);
    fprintf(f[X_DEF], "%%x %s_SC\n", type);
    if (strlen(exten_name) > 0) {
        strupp(upp_exten_name, exten_name);
        fprintf(f[C_DEF], "#define %s_VARIABLE %s_VARIABLE.%s_\n    %s %s_;\n",
                upp_exten_name, type, exten_name, exten_name, exten_name);
        fprintf(f[F_DEF], "#define %s_VARIABLE %s_VARIABLE%%%s\n",
                upp_exten_name, type, exten_name);
    }
}

// Write code to copy static variable through C wrapper
void print_var_in_c_wrapper(FILE* f[], char* parent_struct, char* name,
                            char* type) {
    fprintf(f[C_ARG], "   %s* %s__%s,\n", type, parent_struct, name);
    fprintf(f[C_CP1], "   %s_VARIABLE.%s = %s__%s;\n",
            parent_struct, name, parent_struct, name);
}

// Write code to copy allocatable array through C wrapper
void print_alloc_in_c_wrapper(FILE* f[], char* parent_struct, char* name,
                              char* type, int alloc) {
    int i;

    // We explicitly pass a C pointer from Fortran to identify unallocated arrays
    fprintf(f[C_ARG], "   %s** %s__%s,\n", type, parent_struct, name);
    fprintf(f[C_CP1], "   %s_VARIABLE.%s = *%s__%s;\n",
            parent_struct, name, parent_struct, name);

    // Deal with array dimensions
    for (i=0; i<alloc; i++) {
        // Declare as argument of C wrapper
        fprintf(f[C_ARG], "   int* %s__%s_dim%d,\n", parent_struct, name, i);

        // Copy the pointer into the C structure
        fprintf(f[C_CP1], "   %s_VARIABLE.%s_dim%d = *%s__%s_dim%d;\n",
                parent_struct, name, i, parent_struct, name, i);

        // If the array is allocated, copy the pointer out of the wrapper
        fprintf(f[C_CP2], "   if (%s_VARIABLE.%s != NULL) {\n",
                parent_struct, name);
        fprintf(f[C_CP2], "      *%s__%s = %s_VARIABLE.%s;\n",
                parent_struct, name, parent_struct, name);
        fprintf(f[C_CP2], "      *%s__%s_dim%d = %s_VARIABLE.%s_dim%d;\n   }\n",
                parent_struct, name, i, parent_struct, name, i);
    }
}

// Print structure parentage in Fortran header
void print_struct_in_f(FILE* f[], char* struct_name, char* parent_struct,
                       char* var_name) {
    char upp_struct_name[STR_LEN];
    strupp(upp_struct_name, struct_name);

    fprintf(f[F_DEF], "#define %s_VARIABLE %s_VARIABLE%%%s\n",
            upp_struct_name, parent_struct, var_name);
}

// Print token definition in Fortran header
void print_const_in_f(FILE* f[], char* name, char* value) {
    char upp_name[STR_LEN];
    strupp(upp_name, name);

    fprintf(f[F_DEF], "#define %s_TOKEN %s\n", upp_name, value);
}

// Print a static variable into Fortran interface
void print_var_in_f(FILE* f[], char* parent_struct, char* name, char* type,
                    char* dims) {
    // Argument of CF interface
    fprintf(f[F_ARG], "         %s__%s, &\n", parent_struct, name);

    // Argument of C wrapper from Fortran
    fprintf(f[F_AR2], "         %s_VARIABLE%%%s, &\n", parent_struct, name);

    // Definitions in CF interface
    fprintf(f[F_DEF], "         %s :: %s__%s", type, parent_struct, name);
    if (strlen(dims) > 0) { fprintf(f[F_DEF], "(%s)", dims); }
    fputs("\n", f[F_DEF]);
}

// Print allocatable variable into Fortran interface
void print_alloc_in_f(FILE* f[], char* parent_struct, char* name, int alloc) {
    int i;

    // Declare variable
    fprintf(f[F_ARG], "         %s__%s, &\n", parent_struct, name);
    fprintf(f[F_AR1], "         type(c_ptr) :: %s__%s\n", parent_struct, name);
    fprintf(f[F_AR2], "         %s__%s, &\n", parent_struct, name);

    /* When entering Fortran wrapper, copy array details to C wrapper when array
       already associated */
    fprintf(f[F_CP1], "   if (associated(%s_VARIABLE%%%s)) then\n",
            parent_struct, name);
    fprintf(f[F_CP1], "      %s__%s = c_loc(%s_VARIABLE%%%s(1",
            parent_struct, name, parent_struct, name);
    for (i=1; i<alloc; i++) fputs(",1", f[F_CP1]);
    fputs("))\n", f[F_CP1]);

    // When leaving Fortran wrapper, if array is allocated, override strct
    fputs("   if (", f[F_CP2]);

    // Add dimension variables
    for (i=0; i<alloc; i++) {
        fprintf(f[F_ARG], "         %s__%s_dim%d, &\n", parent_struct, name, i);
        fprintf(f[F_AR1], "         integer :: %s__%s_dim%d\n",
                parent_struct, name, i);
        fprintf(f[F_AR2], "         %s__%s_dim%d, &\n", parent_struct, name, i);

        fprintf(f[F_CP1], "      %s__%s_dim%d = size(%s_VARIABLE%%%s,%d)\n",
                parent_struct, name, i, parent_struct, name, i+1);

        if (i > 0) fputs(" .or. ", f[F_CP2]);
        fprintf(f[F_CP2], "size(%s_VARIABLE%%%s,%d) /= %s__%s_dim%d",
                parent_struct, name, i+1, parent_struct, name, i);
    }

    // When not associated, copy NULL
    fputs("   else\n", f[F_CP1]);
    fprintf(f[F_CP1], "      %s__%s = C_NULL_PTR\n", parent_struct, name);

    fputs(") ", f[F_CP2]);
    fprintf(f[F_CP2], "call c_f_pointer(%s__%s, %s_VARIABLE%%%s, [",
            parent_struct, name, parent_struct, name);

    // Second lot of dimension variables
    for (i=0; i<alloc; i++) {
        fprintf(f[F_CP1], "      %s__%s_dim%d = 0\n", parent_struct, name, i);
        if (i > 0) fputs(",", f[F_CP2]);
        fprintf(f[F_CP2], "%s__%s_dim%d", parent_struct, name, i);
    }

    // Close statements
    fputs("   end if\n", f[F_CP1]);
    fputs("])\n", f[F_CP2]);
}
