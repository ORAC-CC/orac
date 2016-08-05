 /*
 NAME:
    generate_parser.h
 PURPOSE:
    Declarations for generate_parser.
 HISTORY:
    09 Jun 2016, ACP: Initial version
    13 Jul 2016, GRM: Add TYPE_FC, N_TYPES, N_STCK.
 */
#ifndef GENERATE_PARSER_H
#define GENERATE_PARSER_H

#include <stdio.h>
#include <string.h>

// Length of strings
#define STR_LEN 1024

// Subscripts of first dimension of stck arrays
#define TYPE_UPP 0
#define TYPE_C   1
#define TYPE_F   2
#define TYPE_FC  3
#define N_TYPES  4

// Subscripts of f FILE pointer array
#define C_DEF 0
#define X_RUL 1
#define X_DEF 2
#define C_ARG 3
#define C_CP1 4
#define C_CP2 5
#define F_ARG 6
#define F_DEF 7
#define F_AR1 8
#define F_AR2 9
#define F_CP1 10
#define F_CP2 11
#define F_PRI 12
#define X_SED 13
#define N_INC_FILES 14

#define N_STCK N_TYPES

// Function declarations
char* strupp(char* , char* );
void print_struct_in_c_def(FILE** , char* , char* , char* );
void print_var_in_c_def(FILE** , char* , char* );
void print_dim_in_c_def(FILE** , char* , int );
void print_const_in_c_def(FILE** , char* , char* );
void print_const_in_x_rul(FILE** , char* );
void print_str_in_x_rul(FILE** , char* , char* );
void print_struct_in_x_rul(FILE** , char* , char* , char* );
void print_var_in_x_rul(FILE** , char* , char* , char* , char* , char* , int );
void print_struct_in_def(FILE** , char* , char* );
void print_var_in_c_wrapper(FILE** , char* , char* , char* );
void print_alloc_in_c_wrapper(FILE** , char* , char* , char* , int );
void print_struct_in_f(FILE** , char* , char* , char* );
void print_const_in_f(FILE** , char* , char* );
void print_var_in_f(FILE** , char* , char* , char* , char* );
void print_alloc_in_f(FILE** , char* , char* , int );
void print_print_str_in_f(FILE* f[], char* parent_struct, char* name);
void print_print_var_in_f(FILE* f[], char* parent_struct, char* name, char* type_c,
                          char* len, int alloc);

#endif
