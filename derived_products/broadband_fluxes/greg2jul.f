      subroutine greg2jul(year,month,day,jday)
c    routines obtained from www.ccpo.odu.edu/~klinck/SOGOBEC/cruise/timedate/julianday.thml
c    Old Dominion University Center for Coastal Physical Oceonography
c    Southern Ocean GLOBEC
c    written by J. Klinck
c    July, 2000.
c    send questions to klinck@ccpo.odu.edu
c
c    modified to be used in ORAC by Matt Christensen RAL-STFC - 4/11/15
c
c    convert from gregorian to julian dates
c
      real jday,jday1,jday2,fracday, rday
c
      integer year, month, day, yearday
      integer numargs,iargc,ireturn,getarg
c
      character time*8, date*11
      character string*80
c
c      numargs = iargc()
c
c      if(numargs .ne. 3) then
c         write(*,*) '  incorrect usage. '
c         write(*,*) '    greg2jul day month year'
c         stop
c      endif
c                                get first command line argument
c      ireturn = getarg(1,string)
c
c      read(string,*) rday
c
c      ireturn = getarg(2,string)
c
c      read(string,*) month
c
c      ireturn = getarg(3,string)
c
c      read(string,*) year
c
c      year = 2008
c      month = 2
c      rday = 10
c      day=ifix(rday)
c      fracday = rday -ifix(rday)

      call modjulianday(day, year, month, 1      , jday1)
      call modjulianday(0  , year, 1    , 1      , jday2)

c     case study
c      call modjulianday(20, 2008, 12, 1, jday1)
c      call modjulianday(0, 2008, 1, 1, jday2)
      jday = jday1-jday2
c      print*,jday
c      write(*,*) day,month,year,jday1,jday2,jday1-jday2
c
      end

      subroutine modjulianday(day, year, month, fracday, jday)
c
c      calculate the julian day from day, month, year and fraction of a
day
c 
      implicit none
c     
      real jday, fracday
c
      integer julday,day,month,year,julianday
      integer offset
c                    original from HOPS model
      data offset /2440000/
c
      julianday = julday(month, day, year) - offset
c
      jday = julianday + fracday
c
      return
      end

      function julday(mm,id,iyyy)
      parameter (igreg=15+31*(10+12*1582))
      if (iyyy.eq.0) pause 'there is no year zero.'
      if (iyyy.lt.0) iyyy=iyyy+1
      if (mm.gt.2) then
        jy=iyyy
        jm=mm+1
      else
        jy=iyyy-1
        jm=mm+13
      endif
      julday=int(365.25*jy)+int(30.6001*jm)+id+1720995
      if (id+31*(mm+12*iyyy).ge.igreg) then
        ja=int(0.01*jy)
        julday=julday+2-ja+int(0.25*ja)
      endif
      return
      end

