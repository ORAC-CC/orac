!-------------------------------------------------------------------------------
! Name: collocate_aerosol2cloud.F90
!
! Purpose:
! Used to match A(A)TSR CCI aerosol retrievals (at 10 km resolution) to the 1-km
! (instrument) pixel retrievals in the primary and secondary CC4CL products.
!
! Inputs:
! anum: number of aerosol cci retrievals in sinusoidal grid
! aLon(anum): longitude of aerosol cci retrieval in sinusoidal grid
! aLat(anum): latitude of aerosol cci retrieval in sinusoidal grid
! xdim: number of across-track scan pixels at instrument resolution
! ydim: number of along-track scan-line pixels at instrument resolution
! cLon(xdim,ydim): longitude of instrument resolution pixel for cloud product
! cLat(xdim,ydim): latitude of instrument resolution pixel for cloud product
!
! Output:
! aID(xdim,ydim): index location of aerosol cci that coincides with across/along
! track pixel
!
! Subroutines:
! haversine.F90 --> computes great circle distance between pixels
!
! History:
! 2015/15/14, MC: Initial implementation
! 2015/21/14, MC: Committed to repository
! 2015/27/14, MC: Fixed bug in longitude range for swaths across the
!    international dateline.
! 2016/04/05, GM: Real should not be used for indexing.
!
! Bugs:
! Potential problem with method 1: have not accounted for longitude reversal
! around the international dateline - need to add this.
! Problem in method 2 which is currently being commented out. The code compiles
! and runs okay but produces incorrect collocations. Needs further investigation
! before this part is to be used.
!-------------------------------------------------------------------------------

subroutine collocate_aerosol2cloud(anum,aLon,aLat,xdim,ydim,cLon,cLat,aID)

   implicit none

   ! Input arguments
   integer, intent(in) :: anum,xdim,ydim
   real, intent(in), dimension(anum) :: aLon
   real, intent(in), dimension(anum) :: aLat
   real, intent(in), dimension(xdim,ydim) :: cLon
   real, intent(in), dimension(xdim,ydim) :: cLat

   ! Output arguments
   integer, intent(out), dimension(xdim,ydim) :: aID

   ! Local variables
   integer :: i,j,k,iY,iX,tct
   integer, dimension(2000) :: tID
   real :: lonmin,lonmax,latmin,latmax
   real :: d
   real :: start,finish
   real, dimension(xdim,ydim) :: aDist
   integer, dimension(50) :: aID2
   real, dimension(50) :: aDist2
   integer :: act2
   integer :: aID3(1)
   real :: aDist3(1)

   integer, dimension(360,180,500) :: rID ! valid up to 75 degrees
   integer, dimension(360,180) :: rct
   integer :: trct

   integer, dimension(360,180,200) :: srID
   integer, dimension(360,180) :: srct
   integer :: strct

   integer, dimension(360,180,2000) :: hilatID ! valid up to 85 degrees
   integer, dimension(360,180) :: hilatct
   integer :: hilattrct

   integer, dimension(360) :: lonID
   integer, dimension(180) :: latID
   integer :: tXID,tYID

   integer :: flag,Wrap

   ! initialize output values
   aDist(:,:) = -9999.


   !----------------------------------------------------------------------------
   ! Main program
   !----------------------------------------------------------------------------

   call cpu_time(start)
   print*, 'COLLOCATING AEROSOL (10KM) TO CLOUD (1KM)'
   ! Method 1 (~12 mins) (construct index based on each satellite row)
   do iY = 11, ydim-11 ! loop over each scanline
!  do iY = 13499, 13501 ! loop over each scanline
      ! longitude minimum & maximum over across scan pixels
      lonmin = minval(cLon(:,iY-10:iY+10))
      lonmax = maxval(cLon(:,iY-10:iY+10))
      Wrap = 0
      ! international dateline fix
      if (abs(lonmin) .gt. 175. .or. abs(lonmax) .gt. 175.) Wrap=1


      ! latitude min&max over across scan pixels
      latmin = minval(cLat(:,iY-10:iY+10))
      latmax = maxval(cLat(:,iY-10:iY+10))

      ! Loop over all cci aerosol pixels to find the range of points that lie
      ! within the geographic region of the scanline - equivalent where
      ! statement in IDL
      tID(:) = -1 ! set temporary index to -1
      tct = 1
      do k = 1, anum
         if (Wrap .eq. 0) then
            if (aLat(k) .ge. latmin-0.15 .and. aLat(k) .le. latmax+0.15 .and. &
                aLon(k) .ge. lonmin-0.15 .and. aLon(k) .le. lonmax+0.15) then
               tID(tct) = k
               tct = tct+1
            end if
         end if

         ! international dateline
         if (Wrap .eq. 1) then
            if ((aLat(k) .ge. latmin-0.15 .and. aLat(k) .le. latmax+0.15) .and. &
                (aLon(k) .lt. -175. .or. aLon(k) .gt. 175.) ) then
               tID(tct) = k
               tct = tct+1
            end if
         end if
      end do

      if (tct .gt. 1) then
         ! loop over each scan pixel
         do iX = 1, xdim
            act2 = 1
!           print*,iY,iX,': 3',tct

            do k = 1, tct-1 !loop over each aerosol within known range (from above)
!           if (iY .eq. 1491) &
!              print*,iY,iX,': 4',k,cLat(iX,iY),cLon(iX,iY) !,aLat(tID(k)),aLon(tID(k))

            ! compute distance between points
            call haversine(cLat(iX,iY),cLon(iX,iY),aLat(tID(k)),aLon(tID(k)),d)

!           if (iY .eq. 1491) print*,iY,iX,': 5',k,d,act2

            ! select pairs within 15 km of each other
            if (d .le. 15.) then
               aID2(act2) = tID(k)
               aDist2(act2) = d
               act2 = act2+1
            end if ! within 15 km

!           if (iY .eq. 1491) print*,iY,iX,': 6',k,d,act2

            ! select nearest neighbor
            if (act2 .gt. 1) then
               aID3 = aID2(minloc(aDist2(1:act2-1)))
               aDist3 = aDist2(minloc(aDist2(1:act2-1)))
               aID(iX,iY) = aID3(1)
               aDist(iX,iY) = aDist3(1)
            end if

!           if (iY .eq. 1491) print*,iY,iX,': 7',k,d,act2

            end do !loop over aerosol in geographic range
         end do !x-loop over across track pixels
      end if !range
   end do !y-loop over along-track pixels


   !----------------------------------------------------------------------------
   ! Method 2 (~1 min to run)
   ! SKIPPING
   ! This approach is much faster but it is currently not working....
   !----------------------------------------------------------------------------
   if (4 .gt. 5) then
      ! Grid aerosol lat/lon to 1x1 degree grid
      ! 3 composites for treatment of boundaries
      print*, 'GRIDDING DATA FOR COLLOCATION ~15 seconds'
      do i = 1, 360
         do j = 1, 180
            lonmin = i-181
            lonmax = i-180
            latmin = j-91
            latmax = j-90
            lonID(i) = i-181
            latID(j) = j-91
            Wrap = 0
            if (lonmin .eq. -180. .or. lonmax .eq. 180.) then
               Wrap = 1
               lonmin = 178.5
               lonmax = -178.5
            end if

            strct = 1
            trct = 1
            hilattrct = 1
            do k = 1, anum
               ! 3 Gridded composites
               ! Smaller definitely inside box
               if (aLat(k) .ge. latmin .and. aLat(k) .lt. latmax .and. &
                   aLon(k) .ge. lonmin .and. aLon(k) .lt. lonmax) then
                  srID(i,j,strct) = k
                  strct = strct+1
               end if

               ! Could be outside box
               if (aLat(k) .ge. latmin-0.135 .and. aLat(k) .lt. latmax+0.135 .and. &
                   aLon(k) .ge. lonmin-0.5 .and. aLon(k) .lt. lonmax+0.5) then
                  rID(i,j,trct) = k
                  trct = trct+1
               end if

               ! Probably outside box high/latitudes
               if (aLat(k) .ge. latmin-1.35 .and. aLat(k) .lt. latmax+1.35 .and. &
                   aLon(k) .ge. lonmin-1.5 .and. aLon(k) .lt. lonmax+1.5) then
                  hilatID(i,j,hilattrct) = k
                  hilattrct = hilattrct+1
               end if
            end do
            rct(i,j) = trct
            srct(i,j) = strct
            hilatct(i,j) = hilattrct
         end do
      end do

      ! Maximum pixels in a grid-box
      print*, maxval(rct)
      print*, maxval(srct)
      print*, maxval(hilatct)

      print*, 'collocating ~1min'

      do iX = 1, xdim ! loop over satellite-X
         print*, iX
         do iY = 1, ydim ! loop over satellite-Y
            tct = 1
            !get index for current region
            tXID = int(cLon(iX,iY))+181
            tYID = int(cLat(iX,iY))+91
            tct  = rct(tXID,tYID)
            if (tct .gt. 1) then ! aerosol data needs to exist;
                                 ! it won't in twilight/dark regions
               flag = 0

               ! Low-latitude pixel
               if (abs(cLat(iX,iY)) .lt. 75.) then

                  ! Data point is well inside the window
                  ! Use composite 1 (faster collocation over smaller number of points)
                  if (abs(cLon(iX,iY)-lonID(int(cLon(iX,iY))+181)) .gt. 0.15 .and. &
                      abs(cLat(iX,iY)-latID(int(cLat(iX,iY))+181)) .gt. 0.15 ) then
                     flag = 2
                     tct = srct(tXID,tYID)
                     do k = 1, tct
                        call haversine(cLat(iX,iY),cLon(iX,iY), &
                             aLat(srID(tXID,tYID,k)),aLon(srID(tXID,tYID,k)),d)
                        ! Choose the very first aerosol pixel that is within 10
                        ! km of cloud pixel
                        if (d .le. 10.) then
                           aID(iX,iY) = srID(tXID,tYID,k)
                           aDist(iX,iY) = d
                           exit ! break out of loop once you find a pair within 10 km
                        end if ! within 10 km
                     end do
                  end if

                  ! Data point is near the edge of the window
                  ! Use composite 2 (slower collocation over larger number of
                  ! points that can exist outside of grid-box)
                  if (abs(cLon(iX,iY)-lonID(int(cLon(iX,iY))+181)) .lt. 0.15 .or. &
                      abs(cLat(iX,iY)-latID(int(cLat(iX,iY))+181)) .lt. 0.15 ) then
                     flag = 1
                     tct  = rct(tXID,tYID)
                     do k = 1, tct
                        call haversine(cLat(iX,iY),cLon(iX,iY), &
                             aLat(rID(tXID,tYID,k)),aLon(rID(tXID,tYID,k)),d)
!                       print*,k,cLat(iX,iY),cLon(iX,iY), &
!                              aLat(rID(tXID,tYID,k)),aLon(rID(tXID,tYID,k)),d
                        ! Choose the very first aerosol pixel that is within 10
                        ! km of cloud pixel
                        if (d .le. 10.) then
                           aID(iX,iY) = rID(tXID,tYID,k)
                           aDist(iX,iY) = d
                           exit !break out of loop once you find a pair within 10 km
                        end if !within 10 km
                     end do
                  end if
               end if ! low-latitude

               ! High-latitude
               if (abs(cLat(iX,iY)) .ge. 75. .and. abs(cLat(iX,iY)) .lt. 85.) then
                  flag = 3
                  tct = hilatct(tXID,tYID)
                  do k = 1, tct
                     call haversine(cLat(iX,iY),cLon(iX,iY), &
                          aLat(hilatID(tXID,tYID,k)),aLon(hilatID(tXID,tYID,k)),d)
                     ! Choose the very first aerosol pixel that is within 10 km
                     ! of cloud pixel
                     if (d .le. 10.) then
                        aID(iX,iY) = hilatID(tXID,tYID,k)
                        aDist(iX,iY) = d
                        exit ! break out of loop once you find a pair within 10 km
                     end if ! within 10 km
                  end do
               end if

               ! Over-the-pole
               if (abs(cLat(iX,iY)) .ge. 85.) then
                  flag = 4
                  ! needs to be coded
               end if

               if (flag .eq. 0) then
                  print*, cLon(iX,iY),cLat(iX,iY)
                  print*, int(cLon(iX,iY))+181,int(cLat(iX,iY))+91
                  print*, lonID(int(cLon(iX,iY))+181),latID(int(cLat(iX,iY))+91)
                  print*, rct(tXID,tYID),srct(tXID,tYID)
                  print*, ''
                  print*, 'flag = 0; something went wrong!'
                  stop
               end if
            end if ! data in region
         end do
      end do
   end if ! skip approach

   call cpu_time(finish)

   print*, finish-start,' seconds elapsed'

   return
end subroutine collocate_aerosol2cloud


! Great circle distance
subroutine haversine(deglat1,deglon1,deglat2,deglon2,dist)

   implicit none

   real, intent(in) :: deglat1,deglon1,deglat2,deglon2
   real :: a,c,dist,dlat,dlon,lat1,lat2
   real, parameter :: radius = 6372.8
   real, parameter :: Pi = 3.1415927

   dlat = (deglat2-deglat1)*Pi/180.
   dlon = (deglon2-deglon1)*Pi/180.
   lat1 = (deglat1)*Pi/180.
   lat2 = (deglat2)*Pi/180.
   a = (sin(dlat/2))**2 + cos(lat1)*cos(lat2)*(sin(dlon/2))**2
   c = 2*asin(sqrt(a))
   dist = radius*c

!  print '(A,F9.4,A)', 'distance: ',dist,' km'

end subroutine haversine
