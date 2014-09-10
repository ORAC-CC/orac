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
!    status   integer       Out         Error status
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
!
! Bugs:
!    None known.
!
! $Id$
!
!-------------------------------------------------------------------------------

subroutine Read_Illum_nc(Ctrl, NSegs, SegSize, MSI_Data, verbose)

   use CTRL_def
   use Data_def
   use ECP_Constants

   implicit none

   ! Argument declarations

   type(CTRL_t), intent(inout) :: Ctrl
   integer,      intent(in)    :: NSegs    ! Number of segments read so far
   integer,      intent(in)    :: SegSize  ! Size of image segment in rows of
                                           ! pixels.
   type(Data_t), intent(inout) :: MSI_Data
   logical,      intent(in)    :: verbose

   ! Local variables

   integer        :: view,i,j,ic
   integer        :: refch1,refch2
   integer        :: n_vis_bad_ref,n_vis_bad_tau,n_ir_bad
   integer        :: i_missing_vis_ref,i_missing_vis_tau,i_missing_ir

   allocate(MSI_Data%illum(Ctrl%Ind%Xmax, SegSize, Ctrl%Ind%NViews))
   MSI_Data%illum=byte_fill_value

   ! Set channel number in instrument notation which can be used for effective
   ! radius retrieval (could be made dynamic but set static for each instrument
   ! here).  Also, set minimum radiance.
   if ((trim(Ctrl%inst%name) .eq. trim('MODIS-AQUA')) .or. &
       (trim(Ctrl%Inst%Name) .eq. trim('MODIS-TERRA'))) then
      refch1=6
      refch2=20
   else if (trim(Ctrl%inst%name(1:5)) .eq. 'AVHRR') then
      refch1=3
      refch2=4
   else if (trim(Ctrl%inst%name)      .eq. 'AATSR') then
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
            i_missing_vis_ref=-1
            i_missing_vis_tau=-1

            ! Check pure solar channels
            do ic=1,Ctrl%Ind%Nsolar
               ! Check the ref channels
               if ((Ctrl%Ind%Y_id(Ctrl%Ind%ysolar(ic)) .eq. refch1) .or. &
                   (Ctrl%Ind%Y_id(Ctrl%Ind%ysolar(ic)) .eq. refch2)) then
                  if (MSI_Data%MSI(i,j,Ctrl%Ind%ysolar_msi(ic)) .lt. RefMin .and. &
                      MSI_Data%MSI(i,j,Ctrl%Ind%ysolar_msi(ic)) .gt. RefMax) then
                     n_vis_bad_ref=n_vis_bad_ref+1
                     i_missing_vis_ref=ic
                     MSI_Data%MSI(i,j,Ctrl%Ind%ysolar_msi(ic)) = MissingXn
                  end if
               end if

               ! Check the tau channels
               if ((Ctrl%Ind%Y_id(Ctrl%Ind%ysolar(ic)) .ne. refch1) .and. &
                   (Ctrl%Ind%Y_id(Ctrl%Ind%ysolar(ic)) .ne. refch2)) then
                  if (MSI_Data%MSI(i,j,Ctrl%Ind%ysolar_msi(ic)) .lt. RefMin .and. &
                      MSI_Data%MSI(i,j,Ctrl%Ind%ysolar_msi(ic)) .gt. RefMax) then
                     n_vis_bad_tau=n_vis_bad_tau+1
                     i_missing_vis_tau=ic
                     MSI_Data%MSI(i,j,Ctrl%Ind%ysolar_msi(ic)) = MissingXn
                  end if
               end if
            end do

            n_ir_bad=0
            i_missing_ir=-1

            ! Check pure ir channels
            do ic=1,Ctrl%Ind%Nthermal
               if (MSI_Data%MSI(i,j,Ctrl%Ind%ythermal_msi(ic)) .lt. BTMin .and. &
                   MSI_Data%MSI(i,j,Ctrl%Ind%ythermal_msi(ic)) .gt. BTMax) then
                  n_ir_bad=n_ir_bad+1
                  i_missing_ir=ic
                  MSI_Data%MSI(i,j,Ctrl%Ind%ythermal_msi(ic))=MissingXn
               end if
            end do

!           print *, n_vis_bad_ref, n_vis_bad_tau, n_ir_bad, &
!                    i_missing_vis_ref, i_missing_vis_tau, i_missing_ir, &
!                    MSI_Data%Geometry%Sol(i, j, 1)

            ! Now determine illumination conditions based on solar illumination
            ! and amount of available channels.
if (.false.) then
            !-------------------------------------------------------------------
            ! Original logic
            !-------------------------------------------------------------------

            ! All channels available
            if (n_vis_bad_ref .eq. 0 .and. n_vis_bad_tau .eq. 0) then
               ! Daytime
               if (MSI_Data%Geometry%Sol(i, j, 1) .lt. Ctrl%MaxSolZen) then
                  MSI_Data%Illum(i,j,view) = IDay

               ! Twilight
               else if (MSI_Data%Geometry%Sol(i, j, 1) .ge. Ctrl%MaxSolZen .and. &
                        MSI_Data%Geometry%Sol(i, j, 1) .le. Ctrl%Sunset) then
                  MSI_Data%Illum(i,j,view) = ITwi

               ! Night
               else if (MSI_Data%Geometry%Sol(i, j, 1) .gt. Ctrl%Sunset) then
                  MSI_Data%Illum(i,j,view) = INight
               end if

            ! Some solar channels missing
            else if (n_vis_bad_ref .gt. 0 .or. n_vis_bad_tau .gt. 0) then
               ! Twilight
               if (MSI_Data%Geometry%Sol(i, j, 1) .ge. Ctrl%MaxSolZen .and. &
                   MSI_Data%Geometry%Sol(i, j, 1) .le. Ctrl%Sunset) then
                  MSI_Data%Illum(i,j,view) = ITwi

               ! Night
               else if (MSI_Data%Geometry%Sol(i, j, 1) .gt. Ctrl%Sunset) then
                  MSI_Data%Illum(i,j,view) = INight
               end if
            end if

else if (.false.) then
            !-------------------------------------------------------------------
            ! Caroline's new logic
            !-------------------------------------------------------------------

            if (n_vis_bad_ref .eq. 0) then
               ! Daytime, all sw channels and all lw channels
               if (MSI_Data%Geometry%Sol(i, j, 1) .lt. Ctrl%MaxSolZen .and. &
                   n_vis_bad_tau .eq. 0 .and. n_ir_bad .eq. 0) then
                  MSI_Data%Illum(i,j,view) = IDay

               ! Daytime, a single sw channel missing
               else if (MSI_Data%Geometry%Sol(i, j, 1) .lt. Ctrl%MaxSolZen .and. &
                        n_vis_bad_tau .eq. 1 .and. n_ir_bad .eq. 0)  then
                  if (i_missing_vis_tau .eq. 1)  then
                     MSI_Data%Illum(i,j,view) = IDayMissingSingleVisFirst
                  else if (i_missing_vis_tau .eq. 2)  then
                     MSI_Data%Illum(i,j,view) = IDayMissingSingleVisSecond
                  end if

               ! Daytime, a single ir channel missing good
               else if (MSI_Data%Geometry%Sol(i, j, 1) .lt. Ctrl%MaxSolZen .and. &
                        n_vis_bad_tau .eq. 0 .and. n_ir_bad .eq. 1)  then
                  if (i_missing_ir .eq. 1)  then
                     MSI_Data%Illum(i,j,view) = IDayMissingSingleIRFirst
                  else if (i_missing_ir .eq. 2)  then
                     MSI_Data%Illum(i,j,view) = IDayMissingSingleIRSecond
                  else if (i_missing_ir .eq. 3)  then
                     MSI_Data%Illum(i,j,view) = IDayMissingSingleIRThird
                  end if

               ! Sun is close to sunset
               else if (MSI_Data%Geometry%Sol(i, j, 1) .ge. Ctrl%MaxSolZen .and. &
                        MSI_Data%Geometry%Sol(i, j, 1) .le. Ctrl%Sunset) then
                  MSI_Data%Illum(i,j,view) = ITwi

               ! Sun is below horizon
               else if (MSI_Data%Geometry%Sol(i, j, 1) .gt. Ctrl%Sunset) then
                  MSI_Data%Illum(i,j,view) = INight
               end if

            ! Some solar channels gone only do night retrieval
            else if (n_vis_bad_ref .gt. 0 .or. n_vis_bad_tau .gt. 0) then
               if (MSI_Data%Geometry%Sol(i, j, 1) .ge. Ctrl%MaxSolZen .and. &
                   MSI_Data%Geometry%Sol(i, j, 1) .le. Ctrl%Sunset) then
                  MSI_Data%Illum(i,j,view) = ITwi

               ! Sun below horizon
               else if (MSI_Data%Geometry%Sol(i, j, 1) .gt. Ctrl%Sunset .and. &
                        n_ir_bad .eq. 0) then
                  MSI_Data%Illum(i,j,view) = INight

               else if (MSI_Data%Geometry%Sol(i, j, 1) .gt. Ctrl%Sunset .and. &
                        n_ir_bad .gt. 0) then
                  if (i_missing_ir .eq. 1)  then
                     MSI_Data%Illum(i,j,view) = INightMissingSingleIRFirst
                  else if (i_missing_ir .eq. 2)  then
                     MSI_Data%Illum(i,j,view) = INightMissingSingleIRSecond
                  else if (i_missing_ir .eq. 3)  then
                     MSI_Data%Illum(i,j,view) = INightMissingSingleIRThird
                  end if
               end if
            end if

else
            !-------------------------------------------------------------------
            ! Greg's new logic (based on Caroline's but catches a couple more cases)
            !-------------------------------------------------------------------

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
               MSI_Data%Illum(i,j,view) = ITwi

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
end if
         end do
      end do
   end do


end subroutine Read_Illum_nc
