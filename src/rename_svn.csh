#!/bin/csh
#
# C shell script to change the file extension on ECP source code when 
# moving from DEC Unix (.f90 extension) to Linux (.F90) or vice versa.
#
# Andy Smith, 14th June 2001
# Matthias Jerg, 8th December 2011: changed simple mv command to the svn move and created new script bases on rename.csh
#

if ($#argv != 1) then
   echo "Usage:"
   echo "   - DEC Unix to Linux:    rename_svn.csh d2l"
   echo "   - Linux to DEC Unix:    rename_svn.csh l2d"
   exit (1)
endif

if ($argv[1] == "d2l") then
   foreach file (*.f90)
      svn move $file $file:r.F90
   end

else if ($argv[1] == "l2d") then
   foreach file (*.F90)
      svn move  $file $file:r.f90
   end

else
   echo "Invalid argument: Usage:"
   echo "   - DEC Unix to Linux:    rename_svn.csh d2l"
   echo "   - Linux to DEC Unix:    rename_svn.csh l2d"
   exit (2)
   
endif

exit(0)
