! Routines obtained from:
!    www.ccpo.odu.edu/~klinck/SOGOBEC/cruise/timedate/julianday.html
! Old Dominion University Center for Coastal Physical Oceonography
! Southern Ocean GLOBE!
! written by J. Klinck
! July, 2000.
! send questions to klinck@ccpo.odu.edu
!
! modified to be used in ORAC by Matt Christensen RAL-STFC - 4/11/15
!
! convert from gregorian to julian dates

subroutine greg2jul(year,month,day,jday)

   real jday,jday1,jday2!,fracday,rday

   integer year,month,day!,yearday
!  integer numargs,iargc,ireturn,getarg

!  character time*8,date*11
!  character string*80

!  numargs = iargc()
!
!  if (numargs .ne. 3) then
!     write(*,*) '  incorrect usage. '
!     write(*,*) '    greg2jul day month year'
!     stop
!  end if

!  get first command line argument
!  ireturn = getarg(1,string)
!
!  read(string,*) rday
!
!  ireturn = getarg(2,string)
!
!  read(string,*) month
!
!  ireturn = getarg(3,string)
!
!  read(string,*) year
!
!  year = 2008
!  month = 2
!  rday = 10
!  day=ifix(rday)
!  fracday = rday -ifix(rday)

   call modjulianday(day, year, month, 1., jday1)
   call modjulianday(0  , year, 1    , 1., jday2)

!  case study
!  call modjulianday(20, 2008, 12, 1, jday1)
!  call modjulianday(0, 2008, 1, 1, jday2)
   jday = jday1-jday2
!  print*,jday
!  write(*,*) day,month,year,jday1,jday2,jday1-jday2
!
end


! calculate the julian day from day, month, year and fraction of a day
subroutine modjulianday(day, year, month, fracday, jday)
   implicit none
!
   real jday, fracday
!
   integer julday,day,month,year,julianday
   integer offset

!  original from HOPS model
   data offset /2440000/
!
   julianday = julday(month, day, year) - offset
!
   jday = julianday + fracday
!
   return
end

function julday(mm,id,iyyy)

   parameter (igreg=15+31*(10+12*1582))

   if (iyyy.eq.0) stop 'there is no year zero.'
   if (iyyy.lt.0) iyyy=iyyy+1
   if (mm.gt.2) then
     jy=iyyy
     jm=mm+1
   else
     jy=iyyy-1
     jm=mm+13
   end if
   julday=int(365.25*jy)+int(30.6001*jm)+id+1720995
   if (id+31*(mm+12*iyyy).ge.igreg) then
     ja=int(0.01*jy)
     julday=julday+2-ja+int(0.25*ja)
   end if

   return
end
