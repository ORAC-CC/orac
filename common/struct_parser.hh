/*
NAME:
   parse_driver.hh
PURPOSE:
   C++ header for flex/bison parser to read text files into a Fortran structure.
HISTORY:
   20 Apr 2016, ACP: Initial version
   09 Jun 2016, ACP: Final version
*/

#include <stdio.h>
#include <string.h>
#include <cstdlib>
#include <vector>
#include "struct_parser.h"

#ifndef STRUCT_PARSER_H2
#define STRUCT_PARSER_H2

#define INT_MAX 2147483647
#define DIM_MAX 2

#include XCAT2(INC_PATH, h)

// Pass Ctrl structure into parser
#define YY_DECL int yylex(yy::CLASS_NAME::semantic_type *yylval, \
                          yy::CLASS_NAME::location_type *yylloc, \
                          PARENT_STRUCT_TYPE &strct)

// Read buffer types
typedef std::vector<std::vector<float> > Matrix;
const std::vector<float> empty_vector;


/*--------- Template class for pointers to Ctrl structure elements --------*/
template<typename T> class Target {
private:
    T*  ptr;               // Pointer to Fortran data
    int len[DIM_MAX];      // Size of those dimensions
    int read_srt[DIM_MAX]; // Index to start reading from buffer
    int read_end[DIM_MAX]; // Index to end reading from buffer
public:
    Target(T *variable, const unsigned int l0 = 1, const unsigned int l1 = 1);
    void set_slice(int slice[DIM_MAX][2]); // Override default span of read
    void read_buf(Matrix* buffer);         // Read a buffer into pointer
};

// Constructor, with each dimension defaulting to unity
template<typename T> Target<T>::Target(T* variable,
                                     const unsigned int l0,
                                     const unsigned int l1) {
    ptr    = variable;
    len[0] = l0;
    read_srt[0] = 0;
    read_end[0] = l0-1;
    len[1] = l1;
    read_srt[1] = 0;
    read_end[1] = l1-1;
}

// Set limits of array reading
template<typename T> void Target<T>::set_slice(int slice[DIM_MAX][2]) {
    int i;

    for (i=0; i<DIM_MAX; i++) {
        if (slice[i][0] < 0 || slice[i][0] >= len[i]) {
            throw "Invalid start of array slice";
        } else {
            read_srt[i] = slice[i][0];
        }

        if (slice[i][1] == INT_MAX) {
            read_end[i] = len[i]-1;
        } else if (slice[i][1] < slice[i][0] || slice[i][1] >= len[i]) {
            throw "Invalid end of array slice";
        } else {
            read_end[i] = slice[i][1];
        }
    }
}

// Copy data from Matrix buffer into target array
template<typename T> void Target<T>::read_buf(Matrix* buffer) {
    int i, j, i_, j_;

    // As the driver file is in Fortran format, [1] is the slow dimension
    if (buffer->size() != read_end[1]-read_srt[1]+1) {
        throw "Incorrect number of rows of data provided";
    }

    for (j=read_srt[1]; j<=read_end[1]; j++) {
        j_ = j-read_srt[1];

        if (buffer->at(j_).size() != read_end[0]-read_srt[0]+1) {
            throw "Incorrect number of columns of data provided";
        }

        for (i=read_srt[0]; i<=read_end[0]; i++) {
            i_ = i-read_srt[0];
            ptr[i + j*len[0]] = (T)buffer->at(j_)[i_];
        }
    }
}

#endif
