!-------------------------------------------------------------------------------
! Name:
!    Read_Illum_nc
!
! Purpose:
!    Controls the reading of illumination data
!
! Arguments:
!    Name     Type          In/Out/Both Description
!    Ctrl     struct        Both        Control structure
!    NSegs    int           In          Number of image segments read in by
!                                       previous calls to this routine.
!    SegSize  int           In          Size of image segment in rows of pixels.
!    MSI_files_open Logical In          Indicates whether the MSI data file is
!                                       open (if not, open it).
!    lun      int           Both        File unit number set by this routine
!                                       when file is opened, passed back and
!                                       forth for subsequent reads.
!    MSI_Data struct        Both        Data structure: the Geometry data part
!                                       of this struct is populated by this
!                                       routine, and is overwritten on
!                                       successive calls.
!
! Algorithm:
!
! Local variables:
!    Name Type Description
!
! History:
!    18/06/2012, C. Poulsen: Original version
!    08/07/2012, C. Poulsen: Initialised ios value
!    01/10/2012, C. Poulsen: Introduced new class i.e a day measurement with no
!       effective radius channel i.e 1.6 or 3.7 the pixel will not be processed
!       in this case. So will make the code run faster
!    xx/xx/2013, MJ: Implements code for MODIS and AVHRR processing
!    24/07/2013, AP: Added MODIS-TERRA and AQUA as valid instruments
!    18/11/2013, MJ: Fixes a number of problems with this subroutine: refch2 for
!       modis is corrected from 19 to 20. ysolar_msi and ythermal_mis is now
!       used in indexing the MSI array, as this gives the indices of the
!       channels as they are stored in the MSI array.
!    31/01/2014, MJ: Adds code for setting of AVHRR refch
!    01/04/2014, MJ: Rewrites routine partly to robustly set illumination
!    03/04/2014, CP: Initialize illumination with fill value
!    03/07/2014, CP: Added in options for when only a single IR channel is
!       present replace 0.o with missing value.
!    01/08/2014, GM: Changed illumination selection logic to catch a couple of
!       missing cases and cleanup.
!    17/09/2014, GM: Fix bugs related to checking for out-of-range measurement
!       values.
!    2014/12/19, AP: YSolar and YThermal now contain the index of solar/thermal
!       channels with respect to the channels actually processed, rather than the
!       MSI file.
!    2015/01/12, AP: Simplify logic for identifying missing channels.
!    2015/01/15, GM: Bug fix in illumination logic under twilight conditions and
!       removed old logic.
!    2015/01/20, GM: Fixed my previous commit.
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Read_Illum_nc(Ctrl, NSegs, SegSize, MSI_Data, verbose)

   use CTRL_def
   use ECP_Constants
   use Int_Routines_def, only : find_in_array

   implicit none

   ! Argument declarations

   type(CTRL_t), intent(in)    :: Ctrl
   integer,      intent(in)    :: NSegs    ! Number of segments read so far
   integer,      intent(in)    :: SegSize  ! Size of image segment in rows of
                                           ! pixels.
   type(Data_t), intent(inout) :: MSI_Data
   logical,      intent(in)    :: verbose

   ! Local variables

   logical :: flag
   integer :: view,i,j,ic
   integer :: refch1,refch2
   integer :: n_vis_bad_ref,n_vis_bad_tau,n_ir_bad
   integer :: i_missing_vis_ref,i_missing_vis_tau,i_missing_ir

   allocate(MSI_Data%illum(Ctrl%Ind%Xmax, SegSize, Ctrl%Ind%NViews))
   MSI_Data%illum=byte_fill_value

   ! Set channel number in instrument notation which can be used for effective
   ! radius retrieval (could be made dynamic but set static for each instrument
   ! here).  Also, set minimum radiance.
   if (Ctrl%Inst%Name(1:5) .eq. 'MODIS') then
      refch1=6
      refch2=20
   else if (Ctrl%Inst%Name(1:5) .eq. 'AVHRR') then
      refch1=3
      refch2=4
   else if (Ctrl%Inst%Name(1:5) .eq. 'AATSR') then
      refch1=4
      refch2=5
   end if

   ! Loop over all channels and set values to zero where missing. Make sure that
   ! enough channels are present.
   ! First ensure that the illumination is consistent in all views
   ! loop over observations in y direction
   do i = 1,Ctrl%Ind%Xmax
      do j = 1,Ctrl%Resoln%SegSize
         do view = 1,Ctrl%Ind%NViews
            n_vis_bad_ref=0
            n_vis_bad_tau=0
            n_ir_bad=0
            i_missing_vis_ref=-1
            i_missing_vis_tau=-1
            i_missing_ir=-1

            do ic=1,Ctrl%Ind%Ny
               ! Select appropriate limits for the value based on if this channel
               ! is listed as being thermal. (ACP: Some flag of channel
               ! properties would be more efficient.)
               flag = .false.
               if (btest(Ctrl%Ind%Ch_Is(ic), ThermalBit)) then
                  if (MSI_Data%MSI(i,j,ic) < BTMin .or. &
                      MSI_Data%MSI(i,j,ic) > BTMax) then
                     ! Missing mixed channels need to be noted twice
                     if (btest(Ctrl%Ind%Ch_Is(ic), SolarBit)) flag = .true.

                     ! Note missing thermal channel
                     n_ir_bad=n_ir_bad+1
                     i_missing_ir=find_in_array(Ctrl%Ind%YThermal, ic)
                     MSI_Data%MSI(i,j,ic) = MissingXn
                     ! ACP: The above simple counts channels in the order
                     ! presented, not in increasing wavelength (as do equivalents
                     ! below).
                  end if
               else
                  if (MSI_Data%MSI(i,j,ic) < RefMin .or. &
                      MSI_Data%MSI(i,j,ic) > RefMax) then
                     flag = .true. ! Assume non-thermal implies solar

                     MSI_Data%MSI(i,j,ic) = MissingXn
                  end if
               end if

               if (flag) then
                  ! Note which type of solar channel was missing
                  if (Ctrl%Ind%Y_ID(ic) == refch1 .or. &
                      Ctrl%Ind%Y_ID(ic) == refch2) then
                     ! Effective radius channel
                     n_vis_bad_ref=n_vis_bad_ref+1
                     i_missing_vis_ref=find_in_array(Ctrl%Ind%YSolar, ic)
                     MSI_Data%MSI(i,j,ic) = MissingXn
                  else
                     ! Tau channel
                     n_vis_bad_tau=n_vis_bad_tau+1
                     i_missing_vis_tau=find_in_array(Ctrl%Ind%YSolar, ic)
                     MSI_Data%MSI(i,j,ic) = MissingXn
                  end if
               end if
            end do

!           print *, i, j, n_vis_bad_ref, n_vis_bad_tau, n_ir_bad, &
!                    i_missing_vis_ref, i_missing_vis_tau, i_missing_ir, &
!                    MSI_Data%Geometry%Sol(i, j, 1)

            ! Now determine illumination conditions based on solar illumination
            ! and amount of available channels.

            ! Daytime
            if (MSI_Data%Geometry%Sol(i, j, 1) .lt. Ctrl%MaxSolZen) then
               ! Daytime, all sw channels and all lw channels
               if ((n_vis_bad_ref .eq. 0 .and. n_vis_bad_tau .eq. 0) .and. &
                    n_ir_bad .eq. 0) then
                  MSI_Data%Illum(i,j,view) = IDay

               ! Daytime, a single sw tau channel missing
               else if ((n_vis_bad_ref .eq. 0 .and. n_vis_bad_tau .eq. 1) .and. &
                         n_ir_bad .eq. 0) then
                  if (i_missing_vis_tau .eq. 1) then
                     MSI_Data%Illum(i,j,view) = IDayMissingSingleVisFirst
                  else if (i_missing_vis_tau .eq. 2) then
                     MSI_Data%Illum(i,j,view) = IDayMissingSingleVisSecond
                  end if

               ! Daytime, a single lw any channel missing
               else if ((n_vis_bad_ref .eq. 0 .and. n_vis_bad_tau .eq. 0) .and. &
                         n_ir_bad .eq. 1) then
                  if (i_missing_ir .eq. 1)  then
                     MSI_Data%Illum(i,j,view) = IDayMissingSingleIRFirst
                  else if (i_missing_ir .eq. 2) then
                     MSI_Data%Illum(i,j,view) = IDayMissingSingleIRSecond
                  else if (i_missing_ir .eq. 3) then
                     MSI_Data%Illum(i,j,view) = IDayMissingSingleIRThird
                  end if

               ! Daytime, at least one sw ref channel and/or more than one sw tau
               ! channel missing
               else if ((n_vis_bad_ref .gt. 0 .or.  n_vis_bad_tau .gt. 0) .and. &
                         n_ir_bad .eq. 0) then
                  MSI_Data%Illum(i,j,view) = INight

               ! Daytime, at least one sw ref channel and/or more than one sw tau
               ! channel missing and a single lw any channel missing
               else if ((n_vis_bad_ref .gt. 0 .or.  n_vis_bad_tau .gt. 0) .and. &
                         n_ir_bad .eq. 1) then
                  if (i_missing_ir .eq. 1) then
                     MSI_Data%Illum(i,j,view) = INightMissingSingleIRFirst
                  else if (i_missing_ir .eq. 2) then
                     MSI_Data%Illum(i,j,view) = INightMissingSingleIRSecond
                  else if (i_missing_ir .eq. 3) then
                     MSI_Data%Illum(i,j,view) = INightMissingSingleIRThird
                  end if
               end if

            ! Twilight
            else if (MSI_Data%Geometry%Sol(i, j, 1) .ge. Ctrl%MaxSolZen .and. &
                     MSI_Data%Geometry%Sol(i, j, 1) .le. Ctrl%Sunset) then
               if (n_ir_bad .eq. 0) then
                  MSI_Data%Illum(i,j,view) = ITwi
               endif

            ! Night time
            else if (MSI_Data%Geometry%Sol(i, j, 1) .gt. Ctrl%Sunset) then
               ! Night time, all lw channels
               if (n_ir_bad .eq. 0) then
                  MSI_Data%Illum(i,j,view) = INight

               ! Night time, a single lw channel missing
               else if (n_ir_bad .eq. 1) then

                  if (i_missing_ir .eq. 1) then
                     MSI_Data%Illum(i,j,view) = INightMissingSingleIRFirst
                  else if (i_missing_ir .eq. 2) then
                     MSI_Data%Illum(i,j,view) = INightMissingSingleIRSecond
                  else if (i_missing_ir .eq. 3) then
                     MSI_Data%Illum(i,j,view) = INightMissingSingleIRThird
                  end if
               end if
            end if
         end do
      end do
   end do

end subroutine Read_Illum_nc
