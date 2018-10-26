!-------------------------------------------------------------------------------
! Name: read_abi.F90
!
! Purpose:
! Module for GOES-R series read routines.
!
! History:
! 2018/02/10, SP: First version.
! 2018/10/26, SP: Rename to prevent clashes with GOES-Imager reader
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module read_abi_m
   use iso_c_binding

   implicit none
   interface

      integer(c_int) function get_sza_saa(year,month,day,hour,minute,lat,lon,sza,saa) bind(C, name = 'get_sza_saa')

         use iso_c_binding

         implicit none

         integer(c_int), intent(in), value       :: year
         integer(c_int), intent(in), value       :: month
         integer(c_int), intent(in), value       :: day
         integer(c_int), intent(in), value       :: hour
         integer(c_int), intent(in), value       :: minute
         real(c_float),  intent(in), value       :: lat
         real(c_float),  intent(in), value       :: lon
         real(c_float),  intent(out)             :: sza
         real(c_float),  intent(out)             :: saa

      end function get_sza_saa
   end interface

contains

#include "read_abi_main.F90"
#include "read_abi_funcs.F90"

end module read_abi_m
