! Name:
!   sigtrap.F90
!
! Description:
!  ECMWF signal trapping part
!
!   License/Copyright
!   Copyright 2011, RAL Space, Science and Technology Facilities Council and University
!   of Oxford. DWD
!
!   This file and the associated documentation and source code files are part of ORAC.
!
!   ORAC is free software: you can redistribute it and/or modify
!   it under the terms of the GNU General Public License as published by
!   the Free Software Foundation, either version 3 of the License, or
!   (at your option) any later version.
!
!   ORAC is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
!   GNU General Public License for more details.
!
!   You should have received a copy of the GNU General Public License
!   along with ORAC. If not, see http://www.gnu.org/licenses/
!
!
! Arguments:
!    Name       Type    In/Out/Both    Description
!    N/A
!
! Algorithm:
!
! Local variables:
!    Name       Type    Description
!
! History:
!     13th Jul 2012, Matthias Jerg : Original version
!
! Bugs:
!   None known
!
! $Id$
!
!---------------------------------------------------------------------

integer*4 core_dump_flag,ireturn,signals(1),signal_trap
real a

core_dump_flag=0
signals(1)=0
ireturn=signal_trap(core_dump_flag,signals)
if(ireturn .lt. 0) then
   write(*,*) 'ERROR'
elseif(ireturn .eq. 0) then
   write(*,*) 'fpe trapping is not set'
else
   write(*,*) 'fpe trapping mode=',ireturn
end if
