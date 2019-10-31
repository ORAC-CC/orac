! Prints the full version number of the linked RTTOV library
program rttov_version

   use rttov_const, only: version, release, minor_version

   implicit none

   write(*,'(i2,".",i1,".",i1)') version, release, minor_version

end program rttov_version
