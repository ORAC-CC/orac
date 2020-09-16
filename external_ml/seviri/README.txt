To compile this library:
--------------------------------------------------------
1. modify make.config to match your system.
   1.1 Select GNU or ECMWF Cray compiler
   1.2 Set path to your Python and Numpy includes
   1.3 Modify CFLAGS and FFLAGS if needed
2. run 'make'.
3. static library 'libsevann.a' will be created.
4. Fortran module file 'SEVIRI_NEURAL_NET_M.mod' for use 
   inside ORAC will be created.
--------------------------------------------------------

To include this library into ORAC:
--------------------------------------------------------
1. Compile this library as described above.
2. Compile the pre_processor with 
   "-DINCLUDE_SEVIRI_NEURALNET" macro.
3. Run ORAC with preproc driver option 
   "USE_SEVIRI_ANN=True" (default is to False) and 
   "USE_GSICS=True" (default is to True).
-------------------------------------------------------
