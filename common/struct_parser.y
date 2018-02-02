 /*
 NAME:
    struct_parser.y
 PURPOSE:
    Bison grammar to read the contents of a driver file and write it's
    contents into a Fortran structure.
 HISTORY:
    07 Apr 2016, ACP: Initial version
    19 Apr 2016, ACP: Converted into C++ as structures were ungainly.
    09 Jun 2016, ACP: Final version
    19 Jan 2018, GET: Improved labelling of different define statement
                      syntax for different versions of Bison
 */
%language "C++"
%defines
%locations

 // Arguments for the parser function
 // Use the following definition if using bison 3.X
 //%define parser_class_name {CLASS_NAME}
 // Use this instead if using bison 2.X
 %define parser_class_name "CLASS_NAME"

%parse-param {PARENT_STRUCT_TYPE &strct}
%lex-param   {PARENT_STRUCT_TYPE &strct}


%{
#include "struct_parser.hh"

#define MY_ERR yy::CLASS_NAME::error

 // Globals
int j_buf = 0;
int i_undeclared = 0;
%}

/* ----------- Token Definitions ----------- */
%union {
    std::string* str;
    float        val;

    Matrix* vec;
    int span[DIM_MAX];
    int spans[DIM_MAX][2];

    Target<bool>*     bool_v;
    Target<char>*     char_v;
    Target<int>*      int_v;
    Target<long int>* long_int_v;
    Target<float>*    float_v;
    Target<double>*   double_v;
    char*             str_v;
}

%token  <str>           WORD
%token  <val>           NUM
%token  <bool_v>        BOOL_VAR
%token  <char_v>        CHAR_VAR
%token  <int_v>         INT_VAR
%token  <float_v>       FLOAT_VAR
%token  <str_v>         STR_VAR
%type   <vec>           nums bools
%type   <val>           bool
%type   <spans>         dims
%type   <span>          dim

%nonassoc               EQ COLON
%right                  OPENB
%left                   COMMA SEMI CLOSEB ENDL

%destructor { free ($$); } <bool_v> <char_v> <int_v> <float_v> <vec> <str>

%{
extern YY_DECL;
%}

%%
 /* ----------- Grammar ----------- */

 // Complete driver file
file:           file ENDL line
        |       line
        ;
 // One variable declaration
line:           BOOL_VAR dims EQ bools {
                                    try {
                                       $1->set_slice($2); $1->read_buf($4);
                                       delete($1); delete($4);
                                    } catch(std::string msg) {
                                       MY_ERR(@1, msg); YYABORT; }
                }
        |       CHAR_VAR dims EQ nums {
                                    try {
                                       $1->set_slice($2); $1->read_buf($4);
                                       delete($1); delete($4);
                                    } catch(std::string msg) {
                                       MY_ERR(@1, msg); YYABORT; }
                }
        |       INT_VAR dims EQ nums {
                                    try {
                                       $1->set_slice($2); $1->read_buf($4);
                                       delete($1); delete($4);
                                    } catch(std::string msg) {
                                       MY_ERR(@1, msg); YYABORT; }
                }
        |       FLOAT_VAR dims EQ nums {
                                    try {
                                       $1->set_slice($2); $1->read_buf($4);
                                       delete($1); delete($4);
                                    } catch(std::string msg) {
                                       MY_ERR(@1, msg); YYABORT; }
                }
        |       STR_VAR EQ WORD   { strcpy($1, $3->c_str());
                                    delete($3);
                }
        |       /* Empty */
        ;
 // Array slice (only 1 or 2D)
dims:           OPENB dim COMMA dim CLOSEB {
                                    $$[0][0] = $2[0]; $$[0][1] = $2[1];
                                    $$[1][0] = $4[0]; $$[1][1] = $4[1];
                }
        |       OPENB dim CLOSEB  { $$[0][0] = $2[0]; $$[0][1] = $2[1];
                                    $$[1][0] = 0;     $$[1][1] = INT_MAX;
                }
        |       /* Empty */       { $$[0][0] = 0;     $$[0][1] = INT_MAX;
                                    $$[1][0] = 0;     $$[1][1] = INT_MAX;
                }
        ;
 // One dimension of an array slice (using Fortran array indexing)
dim:            NUM COLON NUM     { $$[0] = (int)$1 - 1; $$[1] = (int)$3 - 1; }
        |       COLON NUM         { $$[0] = 0;           $$[1] = (int)$2 - 1; }
        |       NUM COLON         { $$[0] = (int)$1 - 1; $$[1] = INT_MAX; }
        |       NUM               { $$[0] = (int)$1 - 1; $$[1] = $$[0]; }
        |       COLON             { $$[0] = 0;           $$[1] = INT_MAX; }
        |       /* Empty */       { $$[0] = 0;           $$[1] = INT_MAX; }
        ;
 // One or more numbers are copied into the buffer array
nums:           nums SEMI NUM     { $$ = $1; $$->push_back(empty_vector);
                                    ++j_buf; $$->at(j_buf).push_back($3);
                }
        |       nums COMMA NUM    { $$ = $1; $$->at(j_buf).push_back($3);
                }
        |       NUM               { $$ = new Matrix (1, empty_vector);
                                    j_buf=0; $$->at(j_buf).push_back($1);
                }
        ;
 /* One or more booleans */
bools:          bools SEMI bool   { $$ = $1; $$->push_back(empty_vector);
                                    ++j_buf; $$->at(j_buf).push_back((bool)$3);
                }
        |       bools COMMA bool  { $$ = $1; $$->at(j_buf).push_back((bool)$3);
                }
        |       bool              { $$ = new Matrix (1, empty_vector);
                                    j_buf=0; $$->at(j_buf).push_back((bool)$1);
                }
        ;
bool:           NUM               { switch ((int)$1) {
                                    case 0:   $$ = 0.; break;
                                    case 1:   $$ = 1.; break;
                                    default:  MY_ERR(@1, "Invalid boolean");
                                              YYABORT; }
                }
        |       WORD              { switch ($1->at(0)) {
                                    case 'F':
                                    case 'f':
                                    case 'N':
                                    case 'n': $$ = 0.; break;
                                    case 'T':
                                    case 't':
                                    case 'Y':
                                    case 'y': $$ = 1.; break;
                                    default:  MY_ERR(@1, "Invalid boolean");
                                              YYABORT; }
                                    delete($1);
                }
        ;

%%

void MY_ERR(yy::location const &loc, const std::string &s) {
    std::cerr << "parse_driver) ERROR at L" << loc.begin.line << ") " << s
              << std::endl;
}
