include make.config

all: libsevann.a

# compilation rules for C
py2c.o: py2c.c
	$(CC) -o $@ -c $(INC) $(CFLAGS) $<

# compilation rules for Fortran
seviri_neural_net.o: seviri_neural_net.F90
	$(F90) -o $@ -c $(INC) $(FFLAGS) $<

# generate static library from object files
libsevann.a: py2c.o seviri_neural_net.o
	ar -rcs libsevann.a py2c.o seviri_neural_net.o

clean:
	rm -f *.a *.o *.mod 

