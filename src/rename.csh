#!/bin/csh
#
# C shell script to change the file extension on ECP source code when 
# moving from DEC Unix (.f90 extension) to Linux (.F90) or vice versa.
#
# Andy Smith, 14th June 2001
#

if ($#argv != 1) then
   echo "Usage:"
   echo "   - DEC Unix to Linux:    rename.csh d2l"
   echo "   - Linux to DEC Unix:    rename.csh l2d"
   exit (1)
endif

if ($argv[1] == "d2l") then
   foreach file (*.f90)
      mv $file $file:r.F90
   end

else if ($argv[1] == "l2d") then
   foreach file (*.F90)
      mv $file $file:r.f90
   end

else
   echo "Invalid argument: Usage:"
   echo "   - DEC Unix to Linux:    rename.csh d2l"
   echo "   - Linux to DEC Unix:    rename.csh l2d"
   exit (2)
   
endif

exit(0)
