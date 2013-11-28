! Name:
!    FM
!
! Purpose:
!    Main forward model routine.
!    Calls subroutines Interpol_Thermal and Interpol_Solar to interpolate
!    the RTM quantities in SPixel%RTM onto the cloud pressure level.
!    The thermal and shortwave forward model calculations take place in
!    subroutines FM_Thermal and FM_Solar. The latter is only called during daytime
!    conditions. At twilight and nightime only FM_Thermal is run. At twilight only
!    the pure thermal channels are used. At night all thermal channels are used.
!    Daytime retrievals can use all available channels. In this last case pure thermal
!    and pure solar channels are separated from the mixed channels during the
!    calculation of the measurement vector Y. For the mixed channels, Y is caculated
!    by summing contributions from reflectances (converted to radiances using solar
!    constants f0) and radiances. R2T is used to convert the resulting total radiances
!    to brightness temperatures. The gradients dY_dX are treated in a similar way,
!    using dT_dR from the previous call to R2T. The results from the FMs are then
!    combined into one measurement vector and array of gradients.
!
! Arguments:
!    Name        Type           In/Out   Description
!    Ctrl        struct         In       Control structure
!    SPixel      alloc struct   In       Super pixel structure
!    SAD_Chan    struct arr     In       Array of SAD_Chan structures
!    SAD_LUT     struct arr     In       Array of SAD_LUT structures
!    RTM_Pc      struct         Both     Array to hold RTM data interpolated to
!                                        current Pc value. Passed in because
!                                        it contains allocated arrays. Populated
!                                        and used locally
!    X           real arr       In      State vector
!    Y           real arr       Out      Calculated measurement vector
!    dY_dX       real arr       Out      Gradient in Y wrt state parameters and Rs
!    status      int            Out      Error status
!    
! Algorithm:
!    Update state vector X.
!    Interpolate LW RTM data onto cloud pressure level. (all LW channels, 
!       including those with thermal and solar components)
!    Set zeroth point grid info. for SAD_LUT CRP interpolation (call Set_GZero).
!    Run thermal forward model (all channels with thermal component).
!    If daytime:
!       Interpolate SW RTM data onto cloud pressure level ("purely" solar 
!          channels only, no mixed channels). 
!       Run the shortwave forward model (all channels with solar component).
!       Combine the thermal and shortwave calculated radiances and reflectance 
!       vectors and write to measurement vector Y (similarly for gradients 
!       dY_dX). (Mixed channels are summed in BT via a call to R2T).
!    If twilight:
!       Only write radiances from the purely thermal channels to Y (similarly 
!       for dY_dX).
!    If nightime:
!       Write all thermal channels (including mixed channels) to Y and dY_dX.
!
! Local variables:
!    Name       Type       Description
!    BT         real arr   Brightness temperatures
!    d_BT       real arr   Gradients in brightness temperatures
!    Rad        real arr   Radiances
!    d_Rad      real arr   Gradients in radiances
!    GZero      struct     Zeroth point grid info for SAD_LUT CRP array
!                          interpolation
!    Ref        real arr   Reflectances
!    d_Ref      real arr   Gradients in reflectances
!    Y_R        real arr   Measurement in radiance (mixed channels only)
!    dT_dR      real arr   Gradient in BT wrt radiance (mixed channels only)
!    CRP      float array  Interpolated cloud radiative properties
!                          Calculated by FM_Solar/Thermal, and passed
!                          between them since CRP for Td for the mixed channels
!                          is set by FM_Thermal and required by FM_Solar. 
!    d_CRP    float array  Grads. of interpolated cloud rad. properties
!                          (see comment for CRP).
!    ThF, ThL  real        First, last thermal channel indices for RTM_Pc%LW 
!                          arrays. These are required since the arrays are 
!                          allocated once only to Ctrl%Ind%NThermal, rather 
!                          than allocated per pixel to the number of useful LW
!                          channels (depending on day, night, twilight). Hence
!                          we may or may not want the first element of these
!                          arrays (channel Ctrl%Ind%ThermalFirst, may not equal
!                          SPixel%Ind%ThermalFirst).
!  
! History:
!  2nd February, 2001, Kevin M. Smith : original version (draft)
!  2nd Mar 2001, Andy Smith
!    Updates to draft.
!  7th Mar 2001, Andy Smith:
!    New arrays in RTM_Pc for overall dTac_dPc and dTbc_dPc.
!    Adding breakpoint outputs.
!  8th Mar 2001, Andy Smith:
!    dB_dTs no longer required as an argument to FM_Thermal.
! 15th Mar 2001, Andy Smith:
!    Added ThF and ThL indices for RTM_Pc%LW arrays. Required because these
!    arrays are allocated to match the no. of thermal channels requested, but 
!    in twilight not all of the requested thermal channels may be used.
!    Moved InterpolSolar call into the "daytime" section. Not required in
!    twilight or nighttime conditions.
!    Breakpoint SAD_Chan%Desc indexing changed: failed for night-time 
!    conditions (needs checking)
!    Using SPixel%Ind%ThermalLast as the end of the channel range for 
!    RTM_Pc%Tac and Tbc, instead of Ctrl%Ind%Ny.
!    Changed declaration of SAD_Chan to size Ctrl%Ind%Ny, not SPixel%Ind%Ny.
!    (Allows use of SPixel%Ind%ThermalFirst etc to select out channel ranges).
!    Changed declaration of CRP, dCRP to size Ctrl%Ind%Ny, not SPixel%Ind%Ny.
! 11th Apr 2001, Andy Smith:
!    f0 argument no longer required. Now part of SPixel.
!  4th May 2011, Andy Smith:
!    Extension to multiple instrument views. Values depending on 
!    viewing geometry are now arrays (no of views). 
!    Added SetGZero argument for no of channels, needed for array sizing. 
!    In test for daytime/night conditions, add array index to Solzen. Assume 
!    that the same illumination applies to all instrumwent views in a given 
!    pixel, so we only need to test one element of the array.
! 19th May 2011, Andy Smith:
!    Multiple instrument views, part 2. 
!    In twilight and night conditions the last element of dY_dX for each channel
!    can be NaN or some large value. Try initialising. It is not clear why this 
!    has arisen now, but uninitialised values can be dangerous anyway. 
! 5th Sep 2011, Chris Arnold:
!    Now calls either linear *or* cubic-spline based RTM interpolation schemes  !    depending on RTMIntflag
! 13th December C Poulsen deallocated gzero array at end of routine
! 24th Jan 2012 C Poulsen changed array input into fmthermal and
!                 fmsolar to avoid no contiguous arrays'
! 15th June 2012 C Poulsen changed the way day was defined to use
!                illum value 
! 20120814 MJ removes bug in Gzero allocation that made ORAC crash.
! 20120817 MJ fixed bug unitialized variables which cause NaN crash.

! 20121024 CP notice alot of this routine was modified without commenting here!
! 20121024 CP removed hardwiring of variable indices as this caused a segmentation fault as there wer infact 3 solar channels

! 20/09/2012 CP assigned Y to explicit sizeY(1:SPixel%Ind%Ny) = BT(1:SPixel%Ind%Ny)
! 2013 MJ makes some changes to merge code versions
! 20131125 MJ dynmically sets upper limit for CTP to highest pressure in profile to avoid extrapolation problems.



! Bugs:
!   None known.
!
!------------------------------------------------------------------------------------
subroutine FM(Ctrl, SPixel, SAD_Chan, SAD_LUT, RTM_Pc, X, Y, dY_dX, status)

  use ECP_Constants
  use Ctrl_def
  use SPixel_def
  use SAD_Chan_def
  use SAD_LUT_def
  use RTM_Pc_def
  use GZero_def
  
  implicit none
  
  !  Declare arguments
  
  type(Ctrl_t)     :: Ctrl
  type(SPixel_t)   :: SPixel
  type(SAD_Chan_t) :: SAD_Chan(Ctrl%Ind%Ny)
  type(SAD_LUT_t)  :: SAD_LUT  
  type(RTM_Pc_t)   :: RTM_Pc
  real             :: X(MaxStateVar)
  real             :: Y(SPixel%Ind%Ny)
  real             :: dY_dX(SPixel%Ind%Ny,(MaxStateVar+1))
  integer          :: status
  
  !  Declare local variables
  
  type(GZero_t)  :: GZero
  real           :: BT(SPixel%Ind%NThermal)
  real           :: d_BT(SPixel%Ind%NThermal, MaxStateVar)
  real           :: Rad(SPixel%Ind%NThermal)
  real           :: d_Rad(SPixel%Ind%NThermal, MaxStateVar)
  real           :: Ref(SPixel%Ind%NSolar)
  real           :: d_Ref(SPixel%Ind%NSolar, MaxStateVar+1)
  real           :: temp_solar_d_Ref(SPixel%Ind%NSolar, MaxStateVar+1)
  real           :: Y_R(SPixel%Ind%NMixed)
  real           :: dT_dR(SPixel%Ind%NMixed)
  real           :: CRP(Ctrl%Ind%Ny, MaxCRProps)
!MJ TEST  real           :: CRP(SPixel%Ind%Ny, MaxCRProps)
  real           :: temp_thermal_CRP(Ctrl%Ind%NThermal, MaxCRProps)
!MJ TEST  real           :: temp_thermal_CRP(SPixel%Ind%NThermal, MaxCRProps)
  real           :: temp_solar_CRP(Ctrl%Ind%NSolar, MaxCRProps)
!MJ TEST  real           :: temp_solar_CRP(SPixel%Ind%NSolar, MaxCRProps)
  real           :: d_CRP(Ctrl%Ind%Ny, MaxCRProps, 2)
!MJ TEST           :: d_CRP(SPixel%Ind%Ny, MaxCRProps, 2)
  real           :: temp_thermal_d_CRP(Ctrl%Ind%NThermal, MaxCRProps, 2)
!MJ TEST  real           :: temp_thermal_d_CRP(SPixel%Ind%NThermal, MaxCRProps, 2)
  real           :: temp_solar_d_CRP(Ctrl%Ind%NSolar, MaxCRProps, 2)
!MJ TEST  real           :: temp_solar_d_CRP(SPixel%Ind%NSolar, MaxCRProps, 2)
  integer        :: ThF, ThL     ! First, last thermal channel indices
  ! for RTM_Pc%LW arrays
  integer        :: i,j
  integer        :: bkp_lun, ios ! Unit number and IO status value for 
  ! breakpoint output file
  
  !   Write(*,*) 'START FM'
  Y = 0.0
  dy_dX = 0.0
  
  !Dynamicall set upper limit of cloud top pressure to lowest profile pressure of current pixel.
  Ctrl%Invpar%Xulim(3)=SPixel%RTM%LW%p(SPixel%RTM%LW%Np)

  !  Call routine to interpolate RTM data to the cloud pressure level.
  !  Interpol_Thermal returns transmittances in the LW part of RTM_Pc.
  
  if (Ctrl%RTMIntflag .eq. 0) then
     
     !write(*,*) 'test size'
     !write(*,*) SPixel%Ind%ThermalFirst,SPixel%Ind%ThermalLast
     !write(*,*) size(SAD_Chan)
     
     call Interpol_Thermal(Ctrl, SPixel, X(iPc), &
          & SAD_Chan(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast), &
          & RTM_Pc, status)   

  else if (Ctrl%RTMIntflag .eq. 1) then
 
     call Interpol_Thermal_spline(Ctrl, SPixel, X(iPc), &
          & SAD_Chan(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast), &
          & RTM_Pc, status)
  else
     status = RTMIntflagErr
     call Write_Log(Ctrl, 'FM.f90: RTM Interp flag error:', status)
     write(*,*) 'FM.f90: RTM Interp flag error:', status
  endif

  !  Call Set_GZero (results used in both FM_Thermal and FM_Solar).

  allocate(GZero%iSaZ0(Spixel%Ind%Ny))	
  GZero%iSaZ0=0
  allocate(GZero%iSoZ0(Spixel%Ind%Ny))
  GZero%iSoZ0=0
  allocate(GZero%iRA0(Spixel%Ind%Ny))
  GZero%iRA0=0
  allocate(GZero%iSaZ1(Spixel%Ind%Ny))
  GZero%iSaZ1=0
  allocate(GZero%iSoZ1(Spixel%Ind%Ny))
  GZero%iSoZ1=0
  allocate(GZero%iRA1(Spixel%Ind%Ny))
  GZero%iRA1=0
  allocate(GZero%dSaZ(Spixel%Ind%Ny))
  GZero%dSaZ=0
  allocate(GZero%dSoZ(Spixel%Ind%Ny))
  GZero%dSoZ=0
  allocate(GZero%dRA(Spixel%Ind%Ny))
  GZero%dRA=0
  allocate(GZero%Sa1(Spixel%Ind%Ny))
  GZero%Sa1=0
  allocate(GZero%So1(Spixel%Ind%Ny))
  GZero%So1=0
  allocate(GZero%Ra1(Spixel%Ind%Ny))
  GZero%Ra1=0
  
  CRP=0.00 !MJ
  d_CRP=0.00 !MJ

  if (status == 0) then

     !MJORG      call Set_GZero(X(iTau), X(iRe), SPixel%Geom, Spixel%Ind%Ny, &
     !MJORG call Set_GZero(X(iTau), X(iRe), SPixel, Spixel%Ind%Ny, &
     !MJ ORG SPixel%ViewIdx, SAD_LUT, GZero, status)

     !locate intervalls of the LUTS in which the current tau and ref values fall
     call Set_GZero(X(iTau),X(iRe),SPixel,SAD_LUT, GZero, status)
  
  endif

!     Combine long and short wave transmittance values (depending on whether it 
!     is daytime, twilight or nightime).
!     Note: For channels that are both thermal and solar the transmittances are
!        taken from the interpolated thermal RTM values.  In the delivery 
!        implementation LOWTRAN SW calculations are very approximate.  Also, in 
!        any implementation, transmittances from the LW model are guaranteed to 
!        be consistent with the radiance terms.

!     Assign long wave transmittances to the combined Tac and Tbc vectors.
!     Use ThF and ThL to access the first and last required thermal channels
!     from RTM_Pc%LW arrays, since these are always allocated to size 
!     Ctrl%Ind%NThermal, but not all thermal channels are used in all Spixels
!     (hence SPixel%Ind%ThermalFirst may not equal Ctrl%Ind%ThermalFirst).
!     The problem does not occur with solar channels as we always use either
!     all requested solar channels or none at all.

   !write(*,*) 'SPixel%Ind%ThermalFirst',SPixel%Ind%ThermalFirst
  

  if (status == 0) then
     !these next two lines exclude the mixed channel during twilight conditions
     !from the treatment of the thermal channels in the LW array(r.h.s). 
     !On l.h.s this is done via getillum
     ThF = 1 + SPixel%Ind%ThermalFirst - Ctrl%Ind%ThermalFirst
     ThL = Ctrl%Ind%NThermal
     
     RTM_Pc%Tac(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast) = &
          & RTM_Pc%LW%Tac(ThF:ThL)
     RTM_Pc%Tbc(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast) = &
          & RTM_Pc%LW%Tbc(ThF:ThL)
     
     RTM_Pc%dTac_dPc(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast) = &
          & RTM_Pc%LW%dTac_dPc(ThF:ThL)
     RTM_Pc%dTbc_dPc(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast) = &
          & RTM_Pc%LW%dTbc_dPc(ThF:ThL)
     
      !     Call thermal forward model (required for day, twilight and night)
      !print*,'indi',SPixel%Ind%ThermalFirst,SPixel%Ind%ThermalLast
      !MST

!MJ what is this for?      
!!$      if(scan(trim(Ctrl%Inst%Name),'AVHRR') .gt. 0) then
!!$         if(SPixel%Ind%ThermalFirst .eq. 3) then
!!$            temp_thermal_CRP=CRP(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast,:)
!!$            temp_thermal_d_CRP=d_CRP(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast,:,:)
!!$         endif
!!$         if(SPixel%Ind%ThermalFirst .eq. 4) then
!!$            temp_thermal_CRP=CRP(SPixel%Ind%ThermalFirst-1:SPixel%Ind%ThermalLast,:)
!!$            temp_thermal_d_CRP=d_CRP(SPixel%Ind%ThermalFirst-1:SPixel%Ind%ThermalLast,:,:)
!!$         endif
!!$      else
!!$         temp_thermal_CRP=CRP(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast,:)
!!$         temp_thermal_d_CRP=d_CRP(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast,:,:)
!!$      endif

      !MJ use original version
      !MJ!if(SPixel%Ind%ThermalFirst .eq. 3) then
     write(*,*) 'bounds',&
          & SPixel%Ind%nThermal,SPixel%Ind%ThermalFirst,SPixel%Ind%ThermalLast
     temp_thermal_CRP=CRP(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast,:)
     temp_thermal_d_CRP=d_CRP(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast,:,:)
         !MJ!endif

!MJ
!!$      if(SPixel%Ind%ThermalFirst .eq. 4) then
!!$         temp_thermal_CRP=CRP(SPixel%Ind%ThermalFirst-1:SPixel%Ind%ThermalLast,:)
!!$         temp_thermal_d_CRP=d_CRP(SPixel%Ind%ThermalFirst-1:SPixel%Ind%ThermalLast,:,:)
!!$      endif

     call FM_Thermal(Ctrl, SAD_LUT, SPixel, &
          & SAD_Chan(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast),     &
          & RTM_Pc, X, GZero,                             &
          & temp_thermal_CRP,        &
          & temp_thermal_d_CRP,    &
!MJ TEST          & CRP,        &
!MJ TEST          & d_CRP,    &
          & BT, d_BT, Rad, d_Rad, status)


!MST
!!$      if(scan(trim(Ctrl%Inst%Name),'AVHRR') .gt. 0) then
!!$         if(SPixel%Ind%ThermalFirst .eq. 3) then
!!$            CRP(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast,:)=temp_thermal_CRP
!!$            d_CRP(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast,:,:)= temp_thermal_d_CRP
!!$         endif
!!$         if(SPixel%Ind%ThermalFirst .eq. 4) then
!!$            CRP(SPixel%Ind%ThermalFirst-1:SPixel%Ind%ThermalLast,:)=temp_thermal_CRP
!!$            d_CRP(SPixel%Ind%ThermalFirst-1:SPixel%Ind%ThermalLast,:,:)= temp_thermal_d_CRP
!!$         endif
!!$      else
!!$         temp_thermal_CRP=CRP(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast,:)
!!$         temp_thermal_d_CRP=d_CRP(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast,:,:)
!!$      endif

     !if(SPixel%Ind%ThermalFirst .eq. 3) then
     CRP(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast,:)=temp_thermal_CRP
     d_CRP(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast,:,:)= temp_thermal_d_CRP
      !endif
         !if(SPixel%Ind%ThermalFirst .eq. 4) then
     !CRP(SPixel%Ind%ThermalFirst-1:SPixel%Ind%ThermalLast,:)=temp_thermal_CRP
     !d_CRP(SPixel%Ind%ThermalFirst-1:SPixel%Ind%ThermalLast,:,:)= temp_thermal_d_CRP
      !endif
      
!     Daytime
     !MJ ORGif ((SPixel%Illum(1) .eq. 1) .and. status == 0) then
     if ((SPixel%Illum(1) .eq. IDay) .and. status == 0) then

        !        Call routine to interpolate RTM data to the cloud pressure level.
        !        Interpol_Solar populates the SW part of RTM_Pc.
        if (Ctrl%RTMIntflag .eq. 0) then
           
           call Interpol_Solar(Ctrl, SPixel, X(iPc), RTM_Pc, status)

        else if (Ctrl%RTMIntflag .eq. 1) then

           call Interpol_Solar_spline(Ctrl, SPixel, X(iPc), RTM_Pc, status)

        else
           status = RTMIntflagErr
           call Write_Log(Ctrl, 'FM.f90: RTM Interp flag error:', status)
        endif


!        Assign short wave transmittances to the combined (long and short wave)
!        Tac and Tbc vectors.
         !This means (thermalfirst-1) only the purely solar channels are stored here!?! yes, this must have been wrong.
        !write(*,*) 'test1234',SPixel%Ind%SolarFirst,SPixel%Ind%ThermalFirst-1,size(RTM_Pc%SW%Tac)
!MJ ORG
!!$         RTM_Pc%Tac(SPixel%Ind%SolarFirst:SPixel%Ind%ThermalFirst-1) = &
!!$              & RTM_Pc%SW%Tac(:)
!!$         
!!$
!!$         RTM_Pc%Tbc(SPixel%Ind%SolarFirst:SPixel%Ind%ThermalFirst-1) = &
!!$              & RTM_Pc%SW%Tbc(:) 
!!$
!!$
!!$         RTM_Pc%dTac_dPc(SPixel%Ind%SolarFirst:SPixel%Ind%ThermalFirst-1) = &
!!$              & RTM_Pc%SW%dTac_dPc(:)
!!$
!!$
!!$         RTM_Pc%dTbc_dPc(SPixel%Ind%SolarFirst:SPixel%Ind%ThermalFirst-1) = &
!!$              & RTM_Pc%SW%dTbc_dPc(:)  

!MJ NEW
        RTM_Pc%Tac(SPixel%Ind%SolarFirst:SPixel%Ind%SolarLast) = &
             & RTM_Pc%SW%Tac(:)
                
        RTM_Pc%Tbc(SPixel%Ind%SolarFirst:SPixel%Ind%SolarLast) = &
             & RTM_Pc%SW%Tbc(:) 
        
        
        RTM_Pc%dTac_dPc(SPixel%Ind%SolarFirst:SPixel%Ind%SolarLast) = &
             & RTM_Pc%SW%dTac_dPc(:)
        
        
        RTM_Pc%dTbc_dPc(SPixel%Ind%SolarFirst:SPixel%Ind%SolarLast) = &
             & RTM_Pc%SW%dTbc_dPc(:)  
        

         !        Call short wave forward model 
         !        Note that solar channels only are passed (including mixed channels).  
        d_Ref=0.00 !MJ
        Ref=0.00 !MJ
        temp_solar_d_Ref=0.00
        !call Set_CRP_Solar(Ctrl, SPixel%Ind, GZero, SAD_LUT, CRP, d_CRP, status)
        temp_solar_CRP=CRP(SPixel%Ind%SolarFirst:SPixel%Ind%SolarLast, :)
        temp_solar_d_CRP=d_CRP(SPixel%Ind%SolarFirst:SPixel%Ind%SolarLast, :,:)
        temp_solar_d_Ref=d_Ref(SPixel%Ind%SolarFirst:SPixel%Ind%SolarLast, :)

        call FM_Solar(Ctrl, SAD_LUT, SPixel, RTM_Pc, X, GZero,       &
!!$             & CRP,     &
!!$             & d_CRP,  &
!!$             & Ref(SPixel%Ind%SolarFirst:SPixel%Ind%SolarLast),        &
!!$             & d_Ref, status)
             & temp_solar_CRP,     &
             & temp_solar_d_CRP,  &
             & Ref(SPixel%Ind%SolarFirst:SPixel%Ind%SolarLast),        &
             & temp_solar_d_Ref, status)

        CRP(SPixel%Ind%SolarFirst:SPixel%Ind%SolarLast, :)=temp_solar_CRP
        d_CRP(SPixel%Ind%SolarFirst:SPixel%Ind%SolarLast, :,:)=temp_solar_d_CRP
        d_Ref(SPixel%Ind%SolarFirst:SPixel%Ind%SolarLast, :)=temp_solar_d_Ref
        
!        Combine the results from the LW and SW forward models

        if (status == 0) then 
           !           Purely solar channels
           Y(1:(SPixel%Ind%ThermalFirst-1)) = Ref(1:(SPixel%Ind%ThermalFirst-1))

           dY_dX(1:(SPixel%Ind%ThermalFirst-1),:) = d_Ref(1:(SPixel%Ind%ThermalFirst-1),:)


           !Purely thermal channels. Y array is of size Ny channels, 
           !whereas BT ands d_BT hold only thermal channels.    
           !1+SPixel%Ind%NMixed offsets the starting index by the number of mixed channels (s.b.)
   
           Y(SPixel%Ind%SolarLast+1:SPixel%Ind%Ny) =    &
                & BT(1+SPixel%Ind%NMixed:SPixel%Ind%NThermal)	

           dY_dX(SPixel%Ind%SolarLast+1:SPixel%Ind%Ny,1:MaxStateVar) = &
                & d_BT(1+SPixel%Ind%NMixed:SPixel%Ind%NThermal,:)

           !Although there is no value w.r.t Rs for the thermal channels,
           !the dY_dX contains space for it, so set it to 0.
           dY_dX(SPixel%Ind%SolarLast+1:SPixel%Ind%Ny, IRs) = 0

           !Mixed channels - when there are mixed channels present loop over them.
           !The mixed channel measurements are in brightness temperature.
           !Convert reflectances to radiances using the solar constant f0 and 
           !calculate total radiances. The total radiances are converted to 
           !brightness temperatures using R2T.

           if (SPixel%Ind%SolarLast >= SPixel%Ind%ThermalFirst) then
              
              !Sum the radiances and the reflectances (converted to radiances 
              !using f0) to give total radiance for the current scene, Y_R.
              
              Y_R(:) =  Rad(1:SPixel%Ind%NMixed) + &
                   & (SPixel%f0(SPixel%Ind%ThermalFirst:SPixel%Ind%SolarLast) * &
                   & Ref(SPixel%Ind%ThermalFirst:SPixel%Ind%SolarLast) )
              
!              Call R2T to convert the scene radiance Y_R to brightness 
!              temperature. Write the result into the appropriate part of the 
!              measurement vector Y. The gradient dT_dR calculated at the scene
!              radiance is used later.

              call R2T( SPixel%Ind%NMixed,&
                   & SAD_Chan(SPixel%Ind%ThermalFirst:SPixel%Ind%SolarLast),  &
                   & Y_R(:), Y(SPixel%Ind%ThermalFirst:SPixel%Ind%SolarLast), &
                   & dT_dR(:), status )

              if (status == 0) then		      
                 !                 The gradients in Y w.r.t. state vector X.  Use dT_dR calculated
!                 in the previous call to R2T to convert dR_dX to dT_dX (i.e.
!                 dY_dX). Write result into appropriate part of dY_dX array.
   
                 do i=1,MaxStateVar

                    dY_dX(SPixel%Ind%ThermalFirst:SPixel%Ind%SolarLast,i) = &      
                         & ( d_Rad(1:SPixel%Ind%NMixed,i) +                               &
                         & ( SPixel%f0(SPixel%Ind%ThermalFirst:SPixel%Ind%SolarLast) * &
                         & d_Ref(SPixel%Ind%ThermalFirst:SPixel%Ind%SolarLast,i) ) ) * &
                         & dT_dR(:)

                 end do
                  !                 Deal with the Rs terms
                 
                 dY_dX(SPixel%Ind%ThermalFirst:SPixel%Ind%SolarLast,IRs) =    &     
                      & SPixel%f0(SPixel%Ind%ThermalFirst:SPixel%Ind%SolarLast) * &
                      d_Ref(SPixel%Ind%ThermalFirst:SPixel%Ind%SolarLast,IRs) * &
                      & dT_dR(:)

		  
              end if ! status 0 from R2T           
           end if    ! (SolarLast >= ThermalFirst)          
        end if	      ! status == 0 from FM_Solar   
!      end if	      ! day time  
       
        !     Twilight and nightime       
     !MJ ORG else if (status == 0) then 
        !twilight
     elseif ((SPixel%Illum(1) .eq.  ITwi .or. SPixel%Illum(1) .eq. INight ) .and. status == 0) then

!     Solar channels are set to zeros as these are not used (replace 
!     previous Spixel values) 
!write(*,*)'sy',size(y)
!write(*,*)'cc',SPixel%Ind%ThermalFirst-1
!write(*,*)'sy dy_dx',size(dY_dX)
!      Y(1:(SPixel%Ind%ThermalFirst-1))   = 0.0   
!      dY_dX(1:(SPixel%Ind%ThermalFirst-1),:) = 0.0   
            
!     All available thermal channels (not mixed channels at twilight)

!      Y(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast)       = BT	       
!      dY_dX(SPixel%Ind%ThermalFirst:SPixel%Ind%ThermalLast,1:MaxStateVar) &
!         = d_BT(:,:)

!     The above is not necessary since Y and dY_dX are passed with size 
!     SPixel%Ind%Ny.
!     Hence, for nighttime or twilight there are only thermal channels passed.  

        Y(1:SPixel%Ind%Ny) = BT(1:SPixel%Ind%Ny)
        dY_dX(:, 1:MaxStateVar) = d_BT


     end if
  end if
   
   !if (associated(GZero%iSaZ0)) deallocate(GZero%iSaZ0)
   !if (associated(GZero%iSoZ0)) deallocate(GZero%iSoZ0)
   !if (associated(GZero%iRA0))  deallocate(GZero%iRA0)
   !if (associated(GZero%iSaZ1)) deallocate(GZero%iSaZ1)
   !if (associated(GZero%iSoZ1)) deallocate(GZero%iSoZ1)
   !if (associated(GZero%iRA1))  deallocate(GZero%iRA1)
   !if (associated(GZero%dSaZ))  deallocate(GZero%dSaZ)
   !if (associated(GZero%dSoZ))  deallocate(GZero%dSoZ)
   !if (associated(GZero%dRA))   deallocate(GZero%dRA)
   !if (associated(GZero%Sa1))   deallocate(GZero%Sa1)
   !if (associated(GZero%So1))   deallocate(GZero%So1)
   !if (associated(GZero%Ra1))   deallocate(GZero%Ra1)

   deallocate(GZero%iSaZ0)
   deallocate(GZero%iSoZ0)
   deallocate(GZero%iRA0)
   deallocate(GZero%iSaZ1)
   deallocate(GZero%iSoZ1)
   deallocate(GZero%iRA1)
   deallocate(GZero%dSaZ)
   deallocate(GZero%dSoZ)
   deallocate(GZero%dRA)
   deallocate(GZero%Sa1)
   deallocate(GZero%So1)
   deallocate(GZero%Ra1)



!  Open breakpoint file if required, and write our reflectances and gradients. 

#ifdef BKP
   if (Ctrl%Bkpl >= BkpL_FM) then
      call Find_Lun(bkp_lun)
      open(unit=bkp_lun,      & 
           file=Ctrl%FID%Bkp, &
	   status='old',      &
	   position='append', &
	   iostat=ios)
      if (ios /= 0) then
         status = BkpFileOpenErr
	 call Write_Log(Ctrl, 'FM: Error opening breakpoint file', status)
      else
         write(bkp_lun,'(/,a)')'FM:'
      end if
      
      write(bkp_lun,'(2(a,f9.2))') ' SolZen(1): ',SPixel%Geom%Solzen(1), &
         '  Max Sol Zen: ', Ctrl%MaxSolzen

      do i=1, SPixel%Ind%NThermal
         write(bkp_lun,'(3a,f9.4,a,5f9.4)')'Channel: ', &
	    SAD_Chan(i+(SPixel%Ind%ThermalFirst-1))%Desc,  &
	    ' Rad ',Rad(i), ' dRad ',(d_Rad(i,j), j=1,MaxStateVar)
      end do
      do i=1, SPixel%Ind%NThermal
         write(bkp_lun,'(3a,f9.4,a,5f9.4)')'Channel: ', &
	    SAD_Chan(i+(SPixel%Ind%ThermalFirst-1))%Desc, &
	    ' BT  ',BT(i), ' dBT  ',(d_BT(i,j), j=1,MaxStateVar)
      end do
      write(bkp_lun,'(/)')

      if (SPixel%Geom%Solzen(1) < Ctrl%MaxSolzen) then
         do i=1, SPixel%Ind%NSolar
            write(bkp_lun,'(3a,f9.4,a,6f9.4)')'Channel: ', &
	       SAD_Chan(i)%Desc, ' Ref ',Ref(i), ' dRef ', &
	       (d_Ref(i,j),j=1,MaxStateVar+1)
	 end do
         write(bkp_lun,'(/)')
	 		
         do i=1, SPixel%Ind%Ny
            write(bkp_lun,'(3a,f9.4,a,6f9.4)') 'Channel: ', &
	       SAD_Chan(i)%Desc, ' Y: ',Y(i),' dY_dX: ',&
	       (dY_dX(i,j),j=1,MaxStateVar+1)
	 end do 
      else

       do i=1, SPixel%Ind%NThermal
	    write(bkp_lun,'(3a,f9.4,a,6f9.4)') 'Channel: ', &
	       SAD_Chan(i+(SPixel%Ind%ThermalFirst-1))%Desc, ' Y: ',Y(i),&
	       ' dY_dX: ',(dY_dX(i,j),j=1,MaxStateVar+1)
	 end do
      end if
      write(bkp_lun,'(/)')
      
      write(bkp_lun, '(a,/)') 'FM: end'
      close(unit=bkp_lun)
   end if   
#endif

!   Write(*,*) 'END FM'

end subroutine FM
