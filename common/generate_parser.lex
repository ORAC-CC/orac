 /*
 NAME:
    generate_parser.lex
 PURPOSE:
    Flex token definitions for generate_parser.
 HISTORY:
    09 Jun 2016, ACP: Initial version
 */
%{
#include "generate_parser.h"
#include "generate_parser.tab.h"

FILE* open_part_file(const char* path, const char* suff);
%}

%option warn nodefault
%option outfile="generate_parser.c"
%option yylineno
%option noyywrap
%option noinput
%option nounput

%x NAMING

%%
     /* ---------- Rules ---------- */
<*>![^\n]*         { ; /* Ignore comments */ }
<*>&.*\n           { ; /* Continue current state to next line */ }
::                 { BEGIN(NAMING); return(DELIM); }
<*>\n              { BEGIN(INITIAL); }

byte               { return(CHAR); }
character          { return(STR); }
contains           { return(CONTAINS); }
dreal              { return(DOUBLE); }
end                { return(END); }
extends            { return(EXTENDS); }
"implicit none"    { return(IMPLICIT); }
integer            { return(INT); }
kind               { return(KIND); }
lint               { return(LINT); }
logical            { return(BOOL); }
module             { return(MODULE); }
only               { return(ONLY); }
parameter          { return(PARAM); }
pointer            { return(ALLOC); }
real               { return(FLOAT); }
sint               { return(INT); }
sreal              { return(FLOAT); }
subroutine         { return(SUBROUT); }
type               { return(TYPE); }
use                { return(USE); }
\#include          { return(INCLUDE); }

<*>=               { return(EQ); }
<*>:               { return(':'); }
<*>,               { return(','); }
<*>\(              { return('('); }
<*>\)              { return(')'); }

<NAMING>(?i:".false.") {
                     strcpy(yylval.sval, "false"); return(NUM); }
<NAMING>(?i:".true.") {
                     strcpy(yylval.sval, "true"); return(NUM); }

<*>[\+\-0-9\.Ee]+  { strcpy(yylval.sval, yytext); return(NUM); }
<*>\".*\"          { memcpy(yylval.sval, yytext+1, yyleng-2);
                     yylval.sval[yyleng-2]='\0';
                     return(NAME); }
<*>[[:alnum:]_]+   { strcpy(yylval.sval, yytext); return(NAME); }

<*>.               { ; /* Ignore undeclared characters */ }

%%

int main(int argc, char const* argv[]) {
    FILE* f[N_INC_FILES];
    int stat, i;

    // Open files for I/O
    if (argc < 2) {
        printf("generate_parser) Minimum of two few arguments are required\n");
        return(-1);
    }

    // Output filenames derived from routine name

    // C defintion of structure
    f[C_DEF] = open_part_file(argv[1], "h");
    // Flex rules to parse that structure
    f[X_RUL] = open_part_file(argv[1], "l_rul.inc");
    // Start conditions for those Flex rules
    f[X_DEF] = open_part_file(argv[1], "l_def.inc");
    // Arguments of the parser wrapper C fct
    f[C_ARG] = open_part_file(argv[1], "c_arg.inc");
    // Contents of wrapper fct, copying Fortran pointers to C
    f[C_CP1] = open_part_file(argv[1], "c_cpy.inc");
    // C code to copy newly allocated pointers to Fortran
    f[C_CP2] = open_part_file(argv[1], "c_cpy2.inc");
    // Arguments of Fortran wrapper fct
    f[F_ARG] = open_part_file(argv[1], "f_arg.inc");
    // Variable declarations for Fortran to C interface
    f[F_DEF] = open_part_file(argv[1], "f_def.inc");
    // Variable declarations for Fortran allocatable arrays
    f[F_AR1] = open_part_file(argv[1], "f_arr.inc");
    // Variable declarations for Fortran wrapper fct
    f[F_AR2] = open_part_file(argv[1], "f_arg2.inc");
    // Fortran code to transfer allocatable arrays
    f[F_CP1] = open_part_file(argv[1], "f_cpy.inc");
    // Fortran code to copy newly allocated pointers
    f[F_CP2] = open_part_file(argv[1], "f_cpy2.inc");
    // Fortran code to print variables in Fortran
    f[F_PRI] = open_part_file(argv[1], "f_pri.inc");

    // Write permanent headers for files
    fputs("#ifndef WRAPPER_DEF_H\n#define WRAPPER_DEF_H\n", f[C_DEF]);

    // To avoid gcc warning maybe-uninitialized
    stat = 0;

    // Run parser on each input file in turn
    for (i=2; i<argc; i++) {
        yyin = fopen(argv[i], "r");
        if (yyin == NULL) {
            printf("generate_parser) Could not open input file %s\n", argv[i]);
            return(-2);
        }
        fprintf(f[C_DEF], "\n\n// ---- Definitions from %s\n\n", argv[i]);
        yylineno = 1; // Reset line numbering
        stat = yyparse(f, argv[i]);
        fclose(yyin);
        if (stat != 0) break;
    }

    if (stat == 0) fputs("\n#endif\n", f[C_DEF]);

    // Clean up
    for (i=0; i<N_INC_FILES; i++) { fclose(f[i]); }
    return(stat);
}

void yyerror(FILE* f[], const char* filename, char *s) {
    printf("generate_parser) ERROR at L%d, %s) %s\n", yylineno, filename, s);
}

FILE* open_part_file(const char* path, const char* suff) {
    char filename[STR_LEN];
    strcpy(filename, path);
    strcat(filename, ".");
    strcat(filename, suff);
    FILE* file = fopen(filename, "w");
    if (file == NULL) {
        printf("generate_parser) Could not open output file %s\n", filename);
        exit(-2);
    }
    return(file);
}
