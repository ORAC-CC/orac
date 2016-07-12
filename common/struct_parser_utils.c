/*
NAME:
   struct_parser_utils.c
PURPOSE:
   Utilities used by struct_parser based parsers.
HISTORY:
   11 Jul 2016, GRM: Initial version
*/

#include <stdio.h>
#include "struct_parser.h"


/* Concatenation macros */
#ifdef CAT
#undef CAT
#endif
#define CAT(a, b) a##b

#ifdef XCAT
#undef XCAT
#endif
#define XCAT(x, y) CAT(x, y)


#ifdef XXX
#undef XXX
#endif
#define XXX(a, b) ((a) <= (b) ? (0) : ((a) - (b)))


#define TYPE_C char *
#define TYPE_NAME string
#define TYPE_FORMAT "\'%s\'"
#include "print_data_template.h"
#undef TYPE_C
#undef TYPE_NAME
#undef TYPE_FORMAT

#define TYPE_C int
#define TYPE_NAME bool
#define TYPE_FORMAT "%d"
#include "print_data_template.h"
#undef TYPE_C
#undef TYPE_NAME
#undef TYPE_FORMAT

#define TYPE_C char
#define TYPE_NAME char
#define TYPE_FORMAT "%d"
#include "print_data_template.h"
#undef TYPE_C
#undef TYPE_NAME
#undef TYPE_FORMAT

#define TYPE_C int
#define TYPE_NAME int
#define TYPE_FORMAT "%d"
#include "print_data_template.h"
#undef TYPE_C
#undef TYPE_NAME
#undef TYPE_FORMAT

#define TYPE_C float
#define TYPE_NAME float
#define TYPE_FORMAT "%e"
#include "print_data_template.h"
#undef TYPE_C
#undef TYPE_NAME
#undef TYPE_FORMAT


static const char* key_format = "%-56s = ";


static int dims_valid(unsigned int n_dims, const unsigned int *dims) {

    int i;

    for (i = 0; i < n_dims; ++i) {
        if (dims[i] == 0)
            return 0;
    }

    return 1;
}


unsigned int parser_print_string(char *buf, unsigned int size,
                                 const char* name, const char* value) {

    unsigned count = 0;
    const unsigned int one = 1;

    count += snprintf(buf + count, XXX(size, count), key_format, name);
    count += snprint_array_string(buf + count, XXX(size, count), &value, 1, &one);
    count += snprintf(buf + count, XXX(size, count), "\n");

    return count;
}


unsigned int parser_print_bool_scalar(char *buf, unsigned int size,
                                      const char* name, const int value) {

    unsigned count = 0;
    const unsigned int one = 1;

    count += snprintf(buf + count, XXX(size, count), key_format, name);
    count += snprint_array_bool(buf + count, XXX(size, count), &value, 1, &one);
    count += snprintf(buf + count, XXX(size, count), "\n");

    return count;
}


unsigned int parser_print_bool_array(char *buf, unsigned int size,
                                     const char* name, const int* value,
                                     unsigned int n_dims, const unsigned int *dims) {

    unsigned count = 0;

    if (! dims_valid(n_dims, dims))
        return 0;

    count += snprintf(buf + count, XXX(size, count), key_format, name);
    count += snprint_array_bool(buf + count, XXX(size, count), value, n_dims, dims);
    count += snprintf(buf + count, XXX(size, count), "\n");

    return count;
}


unsigned int parser_print_char_scalar(char *buf, unsigned int size,
                                      const char* name, const char value) {

    unsigned count = 0;
    const unsigned int one = 1;

    count += snprintf(buf + count, XXX(size, count), key_format, name);
    count += snprint_array_char(buf + count, XXX(size, count), &value, 1, &one);
    count += snprintf(buf + count, XXX(size, count), "\n");

    return count;
}


unsigned int parser_print_char_array(char *buf, unsigned int size,
                                     const char* name, const char* value,
                                     unsigned int n_dims, const unsigned int *dims) {

    unsigned count = 0;

    if (! dims_valid(n_dims, dims))
        return 0;

    count += snprintf(buf + count, XXX(size, count), key_format, name);
    count += snprint_array_char(buf + count, XXX(size, count), value, n_dims, dims);
    count += snprintf(buf + count, XXX(size, count), "\n");

    return count;
}


unsigned int parser_print_int_scalar(char *buf, unsigned int size,
                                     const char* name, const int value) {

    unsigned count = 0;
    const unsigned int one = 1;

    count += snprintf(buf + count, XXX(size, count), key_format, name);
    count += snprint_array_int(buf + count, XXX(size, count), &value, 1, &one);
    count += snprintf(buf + count, XXX(size, count), "\n");

    return count;
}


unsigned int parser_print_int_array(char *buf, unsigned int size,
                                    const char* name, const int* value,
                                    unsigned int n_dims, const unsigned int *dims) {

    unsigned count = 0;

    if (! dims_valid(n_dims, dims))
        return 0;

    count += snprintf(buf + count, XXX(size, count), key_format, name);
    count += snprint_array_int(buf + count, XXX(size, count), value, n_dims, dims);
    count += snprintf(buf + count, XXX(size, count), "\n");

    return count;
}


unsigned int parser_print_float_scalar(char *buf, unsigned int size,
                                       const char* name, const float value) {

    unsigned count = 0;
    const unsigned int one = 1;

    count += snprintf(buf + count, XXX(size, count), key_format, name);
    count += snprint_array_float(buf + count, XXX(size, count), &value, 1, &one);
    count += snprintf(buf + count, XXX(size, count), "\n");

    return count;
}


unsigned int parser_print_float_array(char *buf, unsigned int size,
                                      const char* name, const float* value,
                                      unsigned int n_dims, const unsigned int *dims) {

    unsigned count = 0;

    if (! dims_valid(n_dims, dims))
        return 0;

    count += snprintf(buf + count, XXX(size, count), key_format, name);
    count += snprint_array_float(buf + count, XXX(size, count), value, n_dims, dims);
    count += snprintf(buf + count, XXX(size, count), "\n");

    return count;
}
