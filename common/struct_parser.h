 /*
 NAME:
    struct_parser.h
 PURPOSE:
    Declarations needed by both struct_parser.y and ".lex
 HISTORY:
    09 Jun 2016, ACP: Initial version
 */
#ifndef STRUCT_PARSER_H1
#define STRUCT_PARSER_H1

/* Concatenation macros */
#ifdef STR
#undef STR
#endif
#define STR(a) #a

#ifdef XSTR
#undef XSTR
#endif
#define XSTR(a) STR(a)

#ifdef CAT2
#undef CAT2
#endif
#define CAT2(a, b) STR(a.b)

#ifdef XCAT2
#undef XCAT2
#endif
#define XCAT2(x, y) CAT2(x, y)

#ifdef CAT3
#undef CAT3
#endif
#define CAT3(a, b, c) STR(a.b.c)

#ifdef XCAT3
#undef XCAT3
#endif
#define XCAT3(x, y, z) CAT3(x, y, z)

 /* A token somewhat too awkward to read from the Fortran source */
#define MAXSTATEVAR_TOKEN ITS_TOKEN + MAXNUMSOLAR_TOKEN*MAXRHO_XX_TOKEN + \
                          MAXNUMVIEWS_TOKEN
#endif
