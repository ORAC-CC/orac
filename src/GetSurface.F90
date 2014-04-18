! Name:
!    Get_Surface
!
! Purpose:
!    Passes surface reflectances for the solar channels and current super-pixel
!    array according to the method defined by Ctrl%Rs%Flag to Get_Rs.
!    Get_Rs calculates the mean surface reflectance of the super pixel.
!    Also calculates the covariances for the solar channels.
!
! Arguments:
!    Name      Type          In/Out  Description
!    Ctrl      struct        In      Control structure
!    SAD_Chan  struct        In      SAD channel structure
!    SPixel    alloc struct  Both    Super-pixel structure
!    MSI_Data  struct        In      Data structure. Contains the multi-spectral
!                                    image measurements, location values,
!                                    geometry etc for the current image segment,
!                                    from which the current SPixel values will
!                                    be extracted.
!    status    integer       Out     Error status
!
! Algorithm:
!    Depending on the method specified for setting surface reflectance
!    (Ctrl%Rs%Flag):
!    - Ctrl
!        Ctrl: get B values from Ctrl struct
!              set Sb using Ctrl values
!        Calculate an overall B value for the super-pixel in each channel,
!           using the land/sea flags to pick out whether each pixel in the
!           super-pixel is land or sea and combining the results for the two
!           surface types.
!        Similarly, calculate on-diagonal covariance terms Sb
!        Calculate off-diagonal Sb terms using the correlation between channels
!           Ctrl%Surface%Cb
!        Set SPixel values for total number of land and sea pixels
!        Call Get_Rs to average reflectance values over the super-pixel
!
!    - MDAD
!        not supported
!
!    - AUX
!        Use albedo value read from Aux file. 
!
! Local variables:
!    Name        Type         Description
!    b           real array   Reflectance values (number of channels, land/sea)
!    Sb          real array   Reflectance fractional errors (number of channels,
!                             land/sea)
!    SPixel_b    real array   Super pixel array of b (using super pixel surface
!                             flags)
!    SPixel_Sb   real array   Super pixel array of Sb (using super pixel
!                             surface flags)
!    i, j, k, jj int          Loop counters
!    qc1, 1c2    int          Used in quality control of albedo data (aux method)
!    message     char         Log file message
!    solar_factor real        Solar zenith angle correction applied to albedo
!
! History:
!   1st December, 2000, Kevin M. Smith : original version
!   4th December, 2000, KMS : included Get_Surface_Rs subroutine
!  19th December, 2000, KMS : Replaced Data_LSFlags array with Data structure
!  16th January,  2001, KMS : Re-drafted
!  24th January,  2001, KMS : Moved allocations to ECP main on integration
!   8th February, 2001, KMS : Corrected error in calculation of b
!   16th Mar 2001, Andy Smith:
!      Removed checking for invalid "GetSurface" method.
!      Using named constants for selection method.
!    3rd Apr 2001, Andy Smith:
!      Replaced loop over channels for setting b in the Ctrl case with a whole
!      array assignment. Similar for Sb in both Ctrl and SAD cases.
!   25th Jun 2001, Andy Smith:
!      Completed header comments.
!   7th Aug 2001, Andy Smith:
!      Updates for image segmentation. Selection of values from the MSI Data
!      structure arrays now need to use a y value that refers to the image
!      segment currently held in memory rather than the whole image area.
!      X co-ords are unchanged since the segment is the full image width.
!      Renamed structure Data to MSI_Data since Data is a reserved word (hasn't
!      caused any problems so far but it might).
!      Added argument intent.
!  22nd Oct 2001, Andy Smith:
!      Updated comments.
!   ***************************** ECV work starts here ********************
!    22nd Feb 2011, Andy Smith:
!       Re-introducing changes made in late 2001/2002.
!   (29th May 2002, Caroline Poulsen
!       Changed the routine to be able to read auxillary albedo information)
!   (10th Mar 2011, Andy Smith: plus later albedo changes made by Oxford.
!       Replaced Ctrl%Ind values in array size declarations by SPixel equivalents
!       Updated loops for b, Sb setting to use SPixel ind as well - not done in 
!       ORAC code)
!   30th Mar 2011, Andy Smith:
!      Removal of super-pixel averaging. Process single pixels at X0, Y0, 
!      removed other SPixel indices Xc, Yc, Xn, Yn etc. 
!      SPixel_B and SPixel_Sb re-dimensioned to remove SPixel size. 
!    5th Apr 2011, Andy Smith:
!      Removed selection methods SAD and SDAD. SelmMDAD renamed SelmMeas.
!      SAD_Chan struct no longer needed here: argument list updated. 
!      Bug fix from super-pixel removal: calc of spixel_b was still using 
!      index j (pixel position with super-pixel) when indexing the MSI_Data 
!      ALB array. 
!  26th Apr 2011, Andy Smith:
!     Extension to handle multiple instrument views. Commented out setting of 
!     unused variable solar_factor. 
!  19th May 2011, Andy Smith:
!     Multiple views (2). Commented out setting of qc1, qc2 as these are not 
!     used and the assigment refers specifically to channels 5 and 6 of the 
!     albedo data - may change in future and may be specific to one instrument. 
!   8th Jun 2011, Andy Smith:
!     Removed calls to Write_Log to improve performance (add ifdef DEBUG).
!
!   8th Dec 2011, Caroline poulsen:
!     changed y_id to chi.
!   13th Dec 2011, Caroline poulsen:
!     added byte 2 unsigned integer routine to make g95 compatible.
!   27th Jan 2012, Chris Arnold
!     bugfix: added call to byte to uint routine for AUX method (ln 349)
!   1st May 2012 Caroline Poulsen added in the solar factor remove
!    factor 1 for multiplication
!   09/09/2013 CP changed indexing on albedo data set  have to be really
!      careful here as aatsr preprocessing reads out albedo for 55 channel.
!   CP removed albedo scaling factor as netcdf value doe not need scaling
!   01/10/2013 GT Bug-fix: Changed Y_ID to ChI when indexing albedo data
!      (from MSI_Data%ALB). Y_ID gives the Channel ID numbers, rather than
!      their index numbers within the data arrays.
!
!2013-11-19 MJ changes channel indices to ysolar_msi as this gives the indices of thr solar channels
!as stored in the MSI array.
!   01/10/2013 GM: Remove incorrect use of b1_to_ui1() for implicit conversion.
!
! Bugs:
!   AS, Mar 2011. The Aux method albedo code includes specific handling
!   for channel 5, which presumably assumes that we are processing ATSR data 
!   and makes this code harder to generalise to other instruments. 
!
! $Id$
!
!------------------------------------------------------------------------------------
subroutine Get_Surface(Ctrl, SPixel, MSI_Data, status)

    use ECP_Constants
    use CTRL_def
    use SPixel_def
    use Data_def

    implicit none

!   Define arguments

    type(CTRL_t), intent(in)      :: Ctrl
    type(SPixel_t), intent(inout) :: SPixel
    type(Data_t), intent(in)      :: MSI_Data
    integer, intent(out)          :: status

!   Define local variables

    real             :: b( SPixel%Ind%NSolar, 2)
    real             :: Sb(SPixel%Ind%NSolar, 2)
    real             :: SPixel_b(SPixel%Ind%NSolar)
    real             :: SPixel_Sb(SPixel%Ind%NSolar, SPixel%Ind%NSolar)
    integer          :: i,j, jj,intflag
!   integer          :: qc1,qc2   ! N.B. qc vars are set but not used - code commented out
    real             :: solar_factor ! Is set but not used - commented out
#ifdef DEBUG
    character(180)   :: message
#endif
!   Initialise

    status = 0
    SPixel%Surface%Land  = 0
    SPixel%Surface%Sea   = 0
    SPixel%Surface%NLand = 0
    SPixel%Surface%NSea  = 0
    SPixel_Sb  = 0.0
    SPixel_b  = 0.0
    

!   Get the surface reflectances etc for the super pixel array according to 
!   the specified method

    !   CTRL method
    if (Ctrl%Rs%Flag == SelmCtrl) then
    
       !      Get the surface flag super pixel array (only if the Ctrl or SAD method
       !      is set). Note:  The QC mask is applied later in all calculations
       !      involving the surface flags
       !      Note: setting by these methods could be done higher up in ECP, as the
       !      assignment only needs to be done once for each run rather than once per
!      super-pixel.

       SPixel%Surface%Flags = MSI_Data%LSFlags(SPixel%Loc%X0, SPixel%Loc%YSeg0)
	  
!      Loop over channels to assign b, Sb. Cb is taken straight from Ctrl%Rs%Cb
!      in both methods.
!      Note: This cannot be done in the main loop because the off-diagonal
!         calculation of the covariance requires element i+1 to NSolar (which
!          would not have been calculated).

!         Ctrl method uses Ctrl values for b, Sb
!          do i = 1, Ctrl%Ind%NSolar       	  
!              b(i,1:2) = Ctrl%Rs%b(i, 1:2)
!	     Sb(i,1:2) = Ctrl%Rs%Sb
!          end do

         Sb = Ctrl%Rs%Sb
         b  = Ctrl%Rs%b

       do i = 1, SPixel%Ind%NSolar
       
!         Calculate b (sea then land, sea flags set at 0 in SPixel%Surface%Flags,
!                                    land flags set at 1 in SPixel%Surface%Flags)
          intflag = SPixel%Surface%Flags

          SPixel_b(i) =  float(-(intflag-1)) * &
	                       float(SPixel%Mask) * b(i,1)

          SPixel_b(i) =        SPixel_b(i)        			          + &
	                    (float(intflag) * &
		             float(SPixel%Mask) * b(i,2))


!         Calculate Sb 

!         On diagonals (sea then land)

          SPixel_Sb(i,i) =  float(-(intflag-1)) * &
	                          float(SPixel%Mask)                  * &
			          b(i,1) * b(i,1) * Sb(i,1) * Sb(i,1)
					
	  SPixel_Sb(i,i) =        SPixel_Sb(i,i)          + &
                               (float(intflag)  * &
				float(SPixel%Mask)                * &
                                b(i,2) * b(i,2) * Sb(i,2) * Sb(i,2))	

!         Off diagonals (loop over half of the matrix, and swap indices for other half)
				      
          do j = i+1, SPixel%ind%NSolar					   

!            Sea

	     SPixel_Sb(i,j) =  float(-(intflag-1)) * &
	                             float(SPixel%Mask)                  * &
	                             b(i,1) * b(j,1) * Sb(i,1) * Sb(j,1) * Ctrl%Rs%Cb
					   
	     SPixel_Sb(j,i) =  SPixel_Sb(i,j)	
		
!            Land		
					       
	     SPixel_Sb(i,j) =        SPixel_Sb(i,j)            + &
	                          (float(intflag)    * &
			           float(SPixel%Mask)                  * &
				   b(i,2) * b(j,2) * Sb(i,2) * Sb(j,2) * Ctrl%Rs%Cb)
					   
	     SPixel_Sb(j,i) =  SPixel_Sb(i,j)
					    
          end do				             
       end do

       SPixel%Surface%NLand = SPixel%Surface%Flags
       SPixel%Surface%NSea  = SPixel%NMask - SPixel%Surface%NLand
       
       if (SPixel%Surface%NLand > 0) SPixel%Surface%Land = 1
       if (SPixel%Surface%NSea  > 0) SPixel%Surface%Sea  = 1
       
!      Call Get_Rs to calculate super pixel averages
    
       call Get_Rs(Ctrl, SPixel, SPixel_b, SPixel_Sb, status)  
    
!   Meas method (not supported in ECP delivery)    
    
    else if (Ctrl%Rs%Flag == SelmMeas) then
    
!   AUX method: use Albedo data to set Rs
    
    else if (Ctrl%Rs%Flag == SelmAux) then

       SPixel%Surface%Flags = MSI_Data%LSFlags(SPixel%Loc%X0, &
          SPixel%Loc%YSeg0)

!      rearrange the msi_data%alb array to be same dimensions as Spixel_b

      Sb = Ctrl%Rs%Sb
      do i = 1,SPixel%Ind%NSolar

         !MJ ORG if (Ctrl%Ind%Chi(i) == 5) then
         !MJ new:
         if(Ctrl%Ind%Y_Id(Ctrl%Ind%Chi(i)) .eq. 5 .and. Ctrl%Inst%Name .eq. 'AATSR') then 
            if (SPixel%Surface%Flags == 0) then ! sea
               SPixel_b(i)  = Ctrl%Rs%b(i,1)
            else
               SPixel_b(i) = Ctrl%Rs%b(i,2)
            endif
         else
            !  AS, Apr 2011: solar_factor is set but not used. Commented out to avoid 
            !  re-shaping for multiple views. 

            solar_factor = 1. / cos(SPixel%Geom%solzen(1) * (Pi / 180.0))

!           AS, Mar 2011, presumably the 0.0001 scales down albedo from 
!           values stored as int (percentage value * 1000) to a fraction.
            SPixel_b(i) = MSI_Data%ALB(SPixel%Loc%X0, SPixel%Loc%YSeg0, &
                 Ctrl%Ind%ysolar_msi(i)) /solar_factor
            !mj orgSPixel_b(i) = MSI_Data%ALB(SPixel%Loc%X0, SPixel%Loc%YSeg0, &
            !Ctrl%Ind%ChI(i)) /solar_factor

!             qc1=0
!            qc2=0
!            call mvbits(MSI_Data%ALB(SPixel%Loc%X0, &
!                  SPixel%Loc%YSeg0,5),0,2,qc1,0)
!            call mvbits(MSI_Data%ALB(SPixel%Loc%X0, &
!                  SPixel%Loc%YSeg0,6),0,4,qc2,0)

!                  Check for sea: replace by Ctrl value
!                  Otherwise if land, check for missing values and replace
!                  by Ctrl value
!                  N.B. assumes either all channel albedos are present,
!                  or none are.
!
!                  now apply quality control to the albedo values
!
            if (SPixel%Surface%Flags == 0) then ! sea
         
               if (SPixel_b(i) .ge. 1.0) then
                  !MJ ORG if (SPixel_b(i) == 1) then
                  status = SPixelSurfglint
#ifdef DEBUG
                  write(unit=message, fmt=*) &
                  'Get_Surface: Sunglint region predicted over ocean by Cox and Monks' &
                  // ' in pixel at:', SPixel%Loc%X0, SPixel%Loc%Y0
                  call Write_log(Ctrl, trim(message), status)
#endif
               else
                  ! No command here will result in
                  ! cox and munk values over the sea
                  !Spixel_b(i)  = Ctrl%Rs%b(i,1)
               endif
         
            else if (SPixel_b(i) == 0.0) then  ! missing land
               SPixel_b(i) = Ctrl%Rs%b(i,2)
            else if (Spixel_b(i) > 1) then ! odd value land
               SPixel_b(i) = Ctrl%Rs%b(i,2)
               ! comment out this section to install loose qc
               !!!else if(( qc1 == 1) .and. (qc2 >= 9 )) then
               ! use the default value MODIS not reliable
               !!!!Spixel_b(i) = Ctrl%Rs%b(i,2)
            !else if(( qc1 > 1)) then
               ! use the default value MODIS not reliable
            !  Spixel_b(i) = Ctrl%Rs%b(i,2)
            end if
         
            !Apply super-pixel mask to exclude "bad" pixels
         
         endif ! end checking the number of solar channels

      enddo !End of loop over solar channels

!         Calculate Sb!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      if (status == 0) then
         SPixel%Surface%NLand = SPixel%Surface%Flags
         SPixel%Surface%NSea  = SPixel%NMask - SPixel%Surface%NLand
   
         if (SPixel%Surface%NLand > 0) SPixel%Surface%Land = 1
         if (SPixel%Surface%NSea  > 0) SPixel%Surface%Sea  = 1
   
         do i = 1, SPixel%Ind%NSolar
!         On diagonals (sea then land)

            intflag = SPixel%Surface%Flags

            if (SPixel%Surface%Sea == 1) then
               SPixel_Sb(i,i) =  float(-(SPixel%Surface%Flags-1)) * &
                                    float(SPixel%Mask)                    * &
                                    SPixel_b(i) * SPixel_b(i) *Sb(i,1) * Sb(i,1)
         
            else
               SPixel_Sb(i,i) =        SPixel_Sb(i,i)          + &
                                    (float(intflag)  * &
                                    float(SPixel%Mask)                 * &
                                    SPixel_b(i) * SPixel_b(i)  * Sb(i,2) * Sb(i,2))
            endif
         
!           Calculate  SPixel_b

            SPixel_b(i) = SPixel_b(i) * float(SPixel%Mask)

!           Off diagonals (loop over half of the matrix, and swap indices for other half)

            do jj = i+1, SPixel%ind%NSolar
               if (SPixel%Surface%Sea == 1) then
!           SeaCtrl%Rs%Cb
                  SPixel_Sb(i,jj) = float(-(intflag-1)) * &
                                       float(SPixel%Mask)                    * &
                                       Spixel_b(i) * Spixel_b(jj)    * &
                                       Sb(i,1) * Sb(jj,1) * Ctrl%Rs%Cb

               else
!            Land
                  SPixel_Sb(i,jj) =        SPixel_Sb(i,jj)       + &
                                       (float(intflag) * &
                                       float(SPixel%Mask)                * &
                                       Spixel_b(i) * Spixel_b(jj)* &
                                       Sb(i,2) * Sb(jj,2) * Ctrl%Rs%Cb)
               endif
               SPixel_Sb(jj,i) =  SPixel_Sb(i,jj)
            end do ! end do jj = (channel)
          end do ! end do i = (channel)

!write(*,*) 'GetS SPixel_b', SPixel_b 
!write(*,*) 'GetS SPixel_Sb', SPixel_Sb

         SPixel%Surface%NLand = SPixel%Surface%Flags
         SPixel%Surface%NSea  = SPixel%NMask - SPixel%Surface%NLand
   
         if (SPixel%Surface%NLand > 0) SPixel%Surface%Land = 1
         if (SPixel%Surface%NSea  > 0) SPixel%Surface%Sea  = 1
   
   !      Call Get_Rs to calculate super pixel averages
   
         call Get_Rs(Ctrl, SPixel, SPixel_b, SPixel_Sb, status)
      end if ! end if status ok after SPixel_b setting 
   
    end if ! end if selection method Aux


#ifdef DEBUG    
    if (SPixel%Surface%Land + SPixel%Surface%Sea > 1) then
    
!      Write warning to log file that the surface pixel contains mixed surface 
!      types

       write(unit=message, fmt=*) &
          'Get_Surface: WARNING pixel contains mixed surface types'
       call Write_log(Ctrl, trim(message), status)
    
    end if
#endif

end subroutine Get_Surface
