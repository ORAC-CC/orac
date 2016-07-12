 /*
 NAME:
    generate_parser.y
 PURPOSE:
    A bison parser that produces another bison parser. Given a Fortran module
    that defines various structure defintions, this outputs:
    (a) a header file defining the C equivalent to the Fortran module;
    (b) two files that define Flex tokens and rules to associate the variable's
        name with it's position in the memory.
    These are wrapped into struct_parser.lex to produce a .lex file that can
    write into those structures.
 HISTORY:
    06 Apr 2016, ACP: Initial version (with apologies to experienced coders)
    21 Apr 2016, ACP: Produces both C and lex outputs.
    09 Jun 2016, ACP: Final version
 */
%language "C"
%defines

 // Arguments for the parser function
%parse-param {FILE* f[]}
%parse-param {const char* filename}

%{
#include <ctype.h>

#include "generate_parser.h"

int yylex();
void yyerror(FILE* f[], const char* filename, char *s);

// Locals
char current_type[STR_LEN];
%}

/* Definitions */
%union {
    char sval[STR_LEN];
    char stck[3][STR_LEN];
}

%token DELIM EQ
%token ALLOC
%token BOOL CHAR DOUBLE FLOAT INT LINT STR TYPE
%token CONTAINS EXTENDS END IMPLICIT INCLUDE KIND MODULE ONLY PARAM SUBROUT USE

%token  <sval>          NAME NUM
%type   <sval>          dims extension val
%type   <stck>          datatype declaration

%%
 /* Rules */

 // Definition of a module
module_def:     module_name uses IMPLICIT definitions contents END module_name
        ;
module_name:    MODULE NAME
        ;
definitions:    definitions type_def
        |       definitions parameter
        |       type_def
        |       parameter
        ;
 // External module "use" calls
uses:           uses use_line
        |       use_line
        |       /* Empty */
        ;
use_line:       USE NAME ',' ONLY ':' names
        |       USE NAME
        ;
 // A list of (module) names
names:          names ',' NAME
        |       NAME
        ;
 // C include statements
contents:       CONTAINS contents_lines
        |       /* Empty */
        ;
contents_lines: contents_lines include_line
        |       contents_lines subroutine
        |       include_line
        |       subroutine
        |       /* Empty */
        ;
include_line:   INCLUDE NAME
        ;
subroutine:     SUBROUT NAME '(' names ')' uses IMPLICIT code SUBROUT NAME
        ;
 // Defintion of a Fortran structure (i.e. defined type)
type_def:       type_open variables type_close
        ;
type_open:      TYPE extension NAME {
                    // Declare current structure is this type
                    strupp(current_type, $3);
                    print_struct_in_def(f, current_type, $2);
                }
        ;
extension:      ',' EXTENDS '(' NAME ')' DELIM { strcpy($$, $4); }
        |       /* Empty */                    { $$[0] = '\0'; }
        ;
type_close:     END TYPE NAME          { fprintf(f[C_DEF], "} %s;\n\n", $3); }
        ;
 // Declaration of variables in Fortran
variables:      variables variable
        |       variable
        ;
 // Translate Fortran declariation into flex rule and C declaration
variable:       STR dims DELIM NAME {
                    // A string
                    print_var_in_c_def(f, $4, "char");
                    print_str_in_x_rul(f, current_type, $4);
                    print_var_in_c_wrapper(f, current_type, $4, "char");
                    print_var_in_f(f, current_type, $4,
                                   "character(c_char)","*");
                }
        |       declaration ',' ALLOC DELIM NAME '(' ':' ')' {
                    // A 1D allocatable array
                    print_var_in_c_def(f, $5, $1[TYPE_C]);
                    print_dim_in_c_def(f, $5, 1);

                    print_var_in_x_rul(f, current_type, $5, $1[TYPE_C],
                                       $1[TYPE_UPP], " ", 1);
                    print_alloc_in_c_wrapper(f, current_type, $5,
                                             $1[TYPE_C], 1);
                    print_alloc_in_f(f, current_type, $5, 1);
                }
        |       declaration ',' ALLOC DELIM NAME '(' ':' ',' ':' ')' {
                    // A 2D allocatable array
                    print_var_in_c_def(f, $5, $1[TYPE_C]);
                    print_dim_in_c_def(f, $5, 2);

                    print_var_in_x_rul(f, current_type, $5, $1[TYPE_C],
                                       $1[TYPE_UPP], " ", 2);
                    print_alloc_in_c_wrapper(f, current_type, $5,
                                             $1[TYPE_C], 2);
                    print_alloc_in_f(f, current_type, $5, 2);
               }
        |       declaration DELIM NAME dims {
                    // A static array or scalar
                    print_var_in_c_def(f, $3, $1[TYPE_C]);
                    print_var_in_x_rul(f, current_type, $3, $1[TYPE_C],
                                       $1[TYPE_UPP], $4, 0);
                    print_var_in_c_wrapper(f, current_type, $3, $1[TYPE_C]);
                    print_var_in_f(f, current_type, $3, $1[TYPE_F], $4);
                }
        |       TYPE '(' NAME ')' DELIM NAME {
                    // Sort out tree of names within structure
                    print_struct_in_c_def(f, $3, current_type, $6);
                    print_struct_in_x_rul(f, $3, current_type, $6);
                    print_struct_in_f(f, $3, current_type, $6);
                }
        ;
 // Declaration of parameters in Fortran
parameter:      declaration ',' PARAM DELIM NAME EQ val {
                    print_const_in_c_def(f, $5, $7);
                    print_const_in_x_rul(f, $5);
                    print_const_in_f(f, $5, $7);
                }
        // Remaining rules are special cases to be coded manually
        |       declaration ',' PARAM DELIM NAME EQ NAME trash
        |       declaration ',' PARAM DELIM NAME '(' trash
        |       declaration DELIM NAME
        ;
 // A variable's type declaration, such as "integer(lint)"
declaration:    datatype '(' datatype ')' {
                                         memcpy($$, $3, STR_LEN*3); }
        |       datatype '(' KIND EQ datatype ')' {
                                         memcpy($$, $5, STR_LEN*3); }
        |       datatype               { memcpy($$, $1, STR_LEN*3); }
        ;
 // A Fortran data type
datatype:       BOOL                   { strcpy($$[TYPE_UPP], "BOOL");
                                         strcpy($$[TYPE_C], "bool");
                                         strcpy($$[TYPE_F], "logical"); }
        |       CHAR                   { strcpy($$[TYPE_UPP], "CHAR");
                                         strcpy($$[TYPE_C], "char");
                                         strcpy($$[TYPE_F], "integer(byte)"); }
        |       INT                    { strcpy($$[TYPE_UPP], "INT");
                                         strcpy($$[TYPE_C], "int");
                                         strcpy($$[TYPE_F], "integer"); }
        |       LINT                   { strcpy($$[TYPE_UPP], "INT");
                                         strcpy($$[TYPE_C], "long int");
                                         strcpy($$[TYPE_F], "integer(lint)"); }
        |       FLOAT                  { strcpy($$[TYPE_UPP], "FLOAT");
                                         strcpy($$[TYPE_C], "float");
                                         strcpy($$[TYPE_F], "real"); }
        |       DOUBLE                 { strcpy($$[TYPE_UPP], "FLOAT");
                                         strcpy($$[TYPE_C], "double");
                                         strcpy($$[TYPE_F], "real(dreal)"); }
        ;
 // Dimensions of an array (flattening 2D arrays so we can use pointers later)
dims:           '(' val ',' val ')'    { sprintf($$, "%s, %s", $2, $4); }
        |       '(' val ')'            { sprintf($$, "%s", $2); }
        |       /* Empty */            { strcpy($$, ""); }
        ;
 // Useful tokens
val:            NAME                   { strupp($$, $1);
                                         strcat($$, "_TOKEN"); }
        |       NUM                    { strcpy($$, $1); }
        ;
trash:          trash text
        |       text
        ;
text:           NAME | NUM | EQ | '(' | ')' | ',' | ':'
        ;
code:           code any_token
        |       any_token
        ;
any_token:      ALLOC | BOOL | CHAR | CONTAINS | DELIM | DOUBLE | END | EQ
        |       EXTENDS | FLOAT | INCLUDE | INT | KIND | LINT
        |       MODULE | NAME | NUM | ONLY | PARAM | STR | TYPE
        |       '(' | ')' | ',' | ':'
        ;

%%

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
    fprintf(f[F_CP2], "   call c_to_fortran_str(%s_VARIABLE%%%s)\n",
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
    fprintf(f[F_CP2], "   if (.not. associated(%s_VARIABLE%%%s) .or. ",
            parent_struct, name);

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
