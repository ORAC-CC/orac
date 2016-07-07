 /*
 NAME:
    struct_parser.lex
 PURPOSE:
    The skeleton of a flex parser to read the contents of a text file into a
    Fortran structure. The start conditions and rules to identify variables are
    imported from the output of generate_parser to produce the final parser for
    a specific Fortran structure.
 HISTORY:
    07 Apr 2016, ACP: Initial version
    19 Apr 2016, ACP: Converted into C++ as structures were ungainly.
    09 Jun 2016, ACP: Final version
 */
%{
#include "struct_parser.hh"
#include "struct_parser.tab.hh"
#include XSTR(DIM_DEFINITIONS)

#define YY_USER_ACTION if (yytext[0] == '\n') { yylloc->lines(); } \
                       else { yylloc->columns (yyleng); }

typedef yy::CLASS_NAME::token token;

extern "C" {
    void WRAPPER_NAME_C(
#include XCAT3(INC_PATH, c_arg, inc)
        int* stat, const char* filename);

    void fort_alloc_bool_1d(bool** , int* , int* );
    void fort_alloc_bool_2d(bool** , int* , int* , int* , int* );
    void fort_alloc_int_1d(int** , int* , int* );
    void fort_alloc_int_2d(int** , int* , int* , int* , int* );
    void fort_alloc_float_1d(float** , int* , int* );
    void fort_alloc_float_2d(float** , int* , int* , int* , int* );
}
%}

%option warn nodefault
%option noyywrap
%option yylineno
%option nounput

     /* ---------- Token definitions ---------- */
FLEX_DEFINITIONS

DELIM   [%\.]
QUOTE   [\"\']
VAL     [\+\-]?[0-9]+

%%

%{
     // Step location counter for next token
     yylloc->step ();
%}

     /* ---------- Parsing rules ---------- */
FLEX_RULES

 /* Rule for parent structure */
<INITIAL>(?i:"STRUCT_NAME"){DELIM} {
    BEGIN(STRUCT_CONDITION); }

 /* Manually coded rules */

 /* Things to ignore */
<*>[ \t]                    ;
<*>#[^\n]*                  ;

 /* Syntactic punctuation */
\n                          return(token::ENDL);   // One command per line
=                           return(token::EQ);     // Delimit variable from value
[\[\(]                      return(token::OPENB);  // Begin array slice
[\]\)]                      return(token::CLOSEB); // End array slice
,                           return(token::COMMA);  // 1D array delimiter
;                           return(token::SEMI);   // 2D array delimiter
:                           return(token::COLON);  // Dimension delimiter

 /* Data */
[\+\-0-9\.e]+             { yylval->val = atof(yytext);
                            return(token::NUM); }  // Float or ints
\".+\"                    { yylval->str = new std::string(yytext, yyleng-1);
                            yylval->str->erase(0, 1);
                            return(token::WORD); } // Quoted string
[^ \t%\.#\n=\[\(\]\),;:]+ { yylval->str = new std::string(yytext, yyleng);
                            return(token::WORD); } // String

%%

void WRAPPER_NAME_C(
#include XCAT3(INC_PATH, c_arg, inc)
    int* stat, const char* filename) {
    // Open file and parser
    yyin = fopen(filename, "r");
    PARENT_STRUCT_TYPE strct;
    yy::CLASS_NAME parser(strct);

#include XCAT3(INC_PATH, c_cpy, inc)

    // Parse file
    *stat = parser.parse();

#include XCAT3(INC_PATH, c_cpy2, inc)

    // Tidying
    fclose(yyin);
    yylex_destroy();
    return;
}

