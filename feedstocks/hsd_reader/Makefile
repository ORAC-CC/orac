#******************************************************************************%
# *
# *    Copyright (C) 2016-2018 Simon Proud <simon.proud@physics.ox.ac.uk>
# *
# *    This source code is licensed under the GNU General Public License (GPL),
# *    Version 3.  See the file COPYING for more details.
# *
# ******************************************************************************

.SUFFIXES: .c .f90

include make.inc

OBJECTS = himawari.o \
	himawari_headerinfo.o \
	himawari_readheader.o \
	himawari_utils.o \
	himawari_nav.o \
	himawari_readwrite.o

SOLOBS = solpos.o


all: solar.a \
	AHI \
	libhimawari_util.a

solar.a: $(SOLOBS)
	ar -rs solar_util.a $(SOLOBS)

libhimawari_util.a: $(OBJECTS) $(SOLOBS)
	ar -rs libhimawari_util.a $(OBJECTS) $(SOLOBS)

AHI: himawari.o himawari_headerinfo.o himawari_readheader.o himawari_utils.o himawari_nav.o himawari_readwrite.o AHI_Example.o libhimawari_util.a solar_util.a
	$(F90) -o AHI himawari.o $(F90FLAGS) himawari_headerinfo.o himawari_readheader.o himawari_utils.o himawari_nav.o himawari_readwrite.o  AHI_Example.o libhimawari_util.a solar_util.a  $(INCDIRS) $(LIBDIRS) $(LINKS)

clean:
	rm -f *.a *.o *.mod AHI

.c.o:
	$(CC) $(CCFLAGS) $(INCDIRS) -c -o $*.o $<

%.o : %.mod

.f90.o:
	$(F90) -o $*.o -c $(F90FLAGS) $(INCDIRS) $<

