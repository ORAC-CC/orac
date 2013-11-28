
! Name:
!    Get_Measurements
!
! Purpose:
!    Gets the 'average' measurements for the current super-pixel, plus the
!    error covariance.
!
! Arguments:
!    Name     Type        In/Out   Description
!    Ctrl     struct      In       Control structure
!    SAD_Chan struct      In       Array of structs containing channel
!                                     characteristics.
!    SPixel   struct      Both     Super-pixel structure
!    MSI_Data struct      In       Data structure. Contains the multi-spectral
!                                  image measurements, location values,
!                                  geometry etc for the current image segment,
!                                  from which the current SPixel values will 
!                                  be extracted.
!    status   integer     Out      Error status
!    
! Algorithm:
!    Determine which averaging method is required
!       'All'
!           Average measurements at the four corners of the super-pixel
!       'Cloudy'
!           Average all cloudy pixels as defined by SPixel%CloudFlags
!       'Central'
!           Take measurements from the 'central' pixel as defined by SPixel%Loc%Xc and Yc
!    Average measurements output to SPixel%Ym
!
!    Allocate super-pixel error covariance Sy to correct number of channels
!    if (Homogeneity noise requested)
!       for each channel, add the NeHomog noise from SAD_Chan to the 
!       appropriate diagonal value in Sy
!        - in daytime the noise value is taken from the Solar sub-structure for
!          "pure" solar channels, the thermal sub-struct for thermal channels
!          and the sum of the two for mixed channels
!        - otherwise, only thermal noise is used 
!    if (Co-registration noise requested)
!       for each channel, add the NeCoreg noise from SAD_Chan to the 
!       appropriate diagonal value in Sy
!       - same day/twi/night conditions apply as for NeHomog
!
! Local variables:
!    Name          Type   Description
!    i,j           int    Loop counters
!    StartChan     int    Loop start value, first thermal channel
!    Rad          real    Radiance calculated from noise equivalent brightness 
!                         temperature used in setting Sy for mixed 
!                         thermal/solar channels.
!    dR_dT        real    Gradient of Rad w.r.t temp. 
!
! History:
!   29th November, 2000, Kevin M. Smith : original version
!    4th December, 2000, KMS : corrections to 'average all' method
!   19th December, 2000, KMS : Replaced Data_MSI array with Data stucture
!    8th  January, 2001, KMS : Included quality control mask
!   11th  January, 2001, KMS : Added check for allowed solar zenith angles
!   18th  January, 2001, KMS : Changed Sol_zen to Solzen in SPixel structure
!   16th Mar 2001, Andy Smith:
!      Using named constants for averaging method and SPixel index values 
!      to select thermal channels.
!      Removed warning set if solar zenith angle > Max in Ctrl file (only 
!      indicates non-daytime data).
!      Removed check on Ctrl%Resoln%Ameth: now done once only in ReadDriver.
!    3rd Apr 2001, Andy Smith:
!      Replaced loop over channels for AMethCentral case with a whole-array
!      assignment. Should be more efficient.
!   17th May 2001, Andy Smith:
!      Added setting of the SPixel%Sy measurement error covariance values.
!      New argument SAD_Chan required for noise information.
!   11th Jun 2001, Andy Smith:
!      Change to setting of Sy for mixed channels.
!   25th Jun 2001, Andy Smith:
!      Header comments updated following recent changes to functionality.
!   7th Aug 2001, Andy Smith:
!      Updates for image segmentation. Selection of values from the MSI Data
!      structure arrays now need to use a y value that refers to the image 
!      segment currently held in memory rather than the whole image area.
!      X co-ords are unchanged since the segment is the full image width.
!      Renamed structure Data to MSI_Data since Data is a reserved word (hasn't
!      caused any problems so far but it might).  
!  14th Aug 2001, Andy Smith:
!      Change to Sy setting. When homog and coreg noise are added for purely
!      solar channels, the values in the SAD_Chan arrays (which are percentages)
!      must be multiplied by the measurement values to convert them to the 
!      reflectance errors. Thermal channel values are already stored as BT
!      error.
!  21st Sept 2001, Andy Smith:
!      Memory leak fix. Now deallocates Spixel%Ym and Sy before each allocation.
!      It is assumed that an initial allocation was made (in ECP main) otherwise
!      the first deallocate will fail.
!   *************************** ECV work starts here *********************
!   23rd Feb 2011, Andy Smith:
!      Cloud flags converted to real to match current ORAC data. 
!   30th Mar 2011, Andy Smith:
!      Removal of super-pixel averaging. Process single pixels at X0, Y0, 
!      removed other SPixel indices Xc, Yc, Xn, Yn etc. 
!  21st Apr 2011, Andy Smith:
!      Extension to handle multiple instrument views. New Spixel array View_Idx,
!      holds the view index values for active channels in this pixel. 
! 15th June 2012 C. Poulsen added illum array
!
! Bugs:
!   None known.
!
!------------------------------------------------------------------------------------
subroutine Get_Measurements(Ctrl, SAD_Chan, SPixel, MSI_Data, status)
   
    use CTRL_def
    use SAD_Chan_def
    use SPixel_def
    use Data_def
    use ECP_Constants

    implicit none

!   Define arguments

    type(CTRL_t), intent(in)      :: Ctrl
    type(SAD_Chan_t), intent(in)  :: SAD_Chan(Ctrl%Ind%Ny)
    type(SPixel_t), intent(inout) :: SPixel
    type(Data_t), intent(in)      :: MSI_Data
    integer, intent(out)          :: status

!   Define local variables

    integer        :: i, j
!    integer        :: N_ThermChan
    integer        :: StartChan
    character(180) :: message
    real           :: Rad, dR_dT

!   Set status to zero
    status = 0
    
!   Check whether solar channels can be used and allocate size of SPixel%Ym
    
!    if (SPixel%Geom%Solzen > Ctrl%MaxSolZen) then
!       N_ThermChan = Ctrl%Ind%Ny - Ctrl%Ind%NSolar

    if (SPixel%Ind%NSolar == 0) then
       StartChan = SPixel%Ind%ThermalFirst
    else
       StartChan = SPixel%Ind%SolarFirst
    end if

    deallocate(SPixel%Ym)
    allocate(SPixel%Ym(SPixel%Ind%Ny))
    deallocate(SPixel%ViewIdx)
    allocate(SPixel%ViewIdx(SPixel%Ind%Ny))

!   Assign SPixel values. 
!   Use Ctrl indices for the Data array since this is populated for all
!   requested channels (not just those that are valid for the current SPixel).

   SPixel%Ym = MSI_Data%MSI(SPixel%Loc%X0, SPixel%Loc%YSeg0, &
        & StartChan:Ctrl%Ind%Ny)
   SPixel%ViewIdx = Ctrl%Ind%ViewIdx(StartChan: Ctrl%Ind%Ny)
   SPixel%Ind%Nviews = Ctrl%Ind%NViews


!   Allocate the measurement error covariance array to the appropriate size
!   for the SPixel and set the values. Initial value is the same as Ctrl.

    deallocate(SPixel%Sy)
    allocate(SPixel%Sy(SPixel%Ind%Ny, SPixel%Ind%Ny))
    SPixel%Sy = Ctrl%Sy(StartChan:Ctrl%Ind%Ny, StartChan:Ctrl%Ind%Ny)

!   Add in the Homog and Coreg noise IF the appropriate Ctrl flags are set
!   (For mixed solar/thermal channels, the value in daytime is a sum of  
!   the thermal and solar contributions).

    if (Ctrl%Eqmpn%Homog /= 0) then
       do i = 1, SPixel%Ind%Ny
          j = i+StartChan-1
          
          if  (SPixel%Ym(i) .le. 0.0) then
             SPixel%Sy(i,i)=1e6
          end if

          !daylight
          if (SPixel%Illum(1) == IDay) then
             if (SAD_Chan(j)%Solar%Flag /= 0) then
                if (SAD_Chan(j)%Thermal%Flag /= 0) then ! Mixed channel?
                   !both solar and thermal => mixed
!                  Convert NedR to brightness temperature and add to Sy
!                  Also add in the thermal contribution to Sy

                   call T2R(1, SAD_Chan(j), SPixel%Ym(i), Rad, dR_dT, status)
                   SPixel%Sy(i,i) = SPixel%Sy(i,i) + &
                        & SAD_Chan(j)%Thermal%NeHomog(Ctrl%CloudType) + & 
                        & SAD_Chan(j)%Solar%NeHomog(Ctrl%CloudType) * &
                        & (SAD_Chan(j)%Solar%f0 / dR_dT) ** 2

                else ! Pure solar channel, just add the solar Ne contribution
                   SPixel%Sy(i,i) = SPixel%Sy(i,i) + &
                        & SAD_Chan(j)%Solar%NeHomog(Ctrl%CloudType) * &
                        & SPixel%Ym(i) * SPixel%Ym(i)               
                end if
             end if
     
          else ! Night/twilight, only thermal channel info required
             if (SAD_Chan(j)%Thermal%Flag /= 0) then
                SPixel%Sy(i,i) = SPixel%Sy(i,i) + &
                     & SAD_Chan(j)%Thermal%NeHomog(Ctrl%CloudType)
             end if
          end if
       end do
    end if ! End of Homog noise section
    
    if (Ctrl%Eqmpn%Coreg /= 0) then
       do i = 1, SPixel%Ind%Ny
          j = i+StartChan-1
          if (SPixel%Illum(1) == IDay) then
             if (SAD_Chan(j)%Solar%Flag /= 0) then
                if (SAD_Chan(j)%Thermal%Flag /= 0) then ! Mixed channel?

!                  Convert NedR to brightness temperature and add to Sy
!                  Also add in the thermal contribution to Sy

                   call T2R(1, SAD_Chan(j), SPixel%Ym(i), Rad, dR_dT, status)
                   SPixel%Sy(i,i) = SPixel%Sy(i,i) + &
                        & SAD_Chan(j)%Thermal%NeCoreg(Ctrl%CloudType) + & 
                        & SAD_Chan(j)%Solar%NeCoreg(Ctrl%CloudType) * &
                        & (SAD_Chan(j)%Solar%f0 / dR_dT) ** 2

                else ! Pure solar channel, just add the solar Ne contribution
                   SPixel%Sy(i,i) = SPixel%Sy(i,i) + &
                        & SAD_Chan(j)%Solar%NeCoreg(Ctrl%CloudType) * &
                        & SPixel%Ym(i) * SPixel%Ym(i)
                end if
             end if

          else ! Night/twilight, only thermal channel info required
             if (SAD_Chan(j)%Thermal%Flag /= 0) then
                SPixel%Sy(i,i) = SPixel%Sy(i,i) + &
                     & SAD_Chan(j)%Thermal%NeCoreg(Ctrl%CloudType)
             end if
          end if
       end do
    end if ! End of Coreg noise section

  end subroutine Get_Measurements
