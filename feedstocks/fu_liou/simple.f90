USE FULIOUMULTI
USE GENERATE_FULIOU_LEVELS ,only : gflq, generate_level_scheme
USE EXTRAS       ,only : getatmosphere, aer_scale_hgt
USE CALIPSO_OUTPUT, only : pack_sky,print_pack_sky,skyp,SKYP_TYPE

USE ICEDIRSFC,only: tau_uc !! Debug Diagnostic
implicit none
 real  aot_by_band,aotf,wlf
 common /aotbyband/ aot_by_band(18) 
 common /tau_spline_aot/ aotf(18),wlf(18)
      

TYPE (SKYP_TYPE) ut,tu

real psfc
integer kk,i,is
real psel(6)

 call set_default_options_fu ! Sets some of the more obsure inputs to reasonable values.
fi%lscm	   = .false. 
fi%lscm(1:2)	   = .true. 
fi%lscm	   = .true. 
!InPut Profile Assignment
 call getatmosphere('../../testatms/jmls.lay ', &
! call getatmosphere('./testatms/jmls.lay ',&
 FI%VI%nlev,&
 FI%VI%pp,&
 FI%VI%pt,&
 FI%VI%ph,&
 FI%VI%po,&
 FI%pts)
 FI%VI%nlev = FI%VI%nlev+1  ! LAYER(getatm) to LEVEL

 FI%VI%hsfc = 0.00 !! SURFACE GEOPOTENTIAL OF FI%VI profile
! FI%VI%hsfc = 1600 !! SURFACE GEOPOTENTIAL OF FI%VI profile

 !gflq%hsfc = 1500. !Meters Surface elev. of ACTUAL FOV... to nearest 120m Multiple
  gflq%hsfc = 2.0! 
  

 gflq%mode = 'CALIP'
 gflq%mode = 'CERE3'

 gflq%nld =4
 gflq%internal_levels(1:4) = (/70.,200.,500.,850./)

fi%HYBRID_SW_SOLVER =.true. !200130802 SYNI Ed4 $S Clear , 2S HOMO CLD , GWTSA INHOM Cld
!fi%HYBRID_SW_SOLVER =.false. !checks isksolve,fourssl

fi%isksolve= 1  ! Solver Method (0=fu 1=gwtsa) 

fi%ss	   = 1365 ! Solar Constant wm-2
fi%u0      =  1.0 ! Cosine Solar Zenith Angle
fi%ur      =  0.8 ! Cosine View Zenith Angle (for IR Radiance)



!-------Cnd 2
fi%wp_hgt_flag = 0  ! Constant lwc with height
fi%wp_hgt_flag = 1  ! Water Cloud  Top thicker than Base
fi%wp_hgt_flag = 2  ! Ice Cloud  Bottom thicker than top


fi%fc(1)%dpi%ldpi = .false.
fi%fc(1)%cldfrac   = 1.00000    ! Cloud Fraction (0-1) 
fi%fc(1)%novl      =   2 
fi%fc(1)%novl      =   1 

FI%VD%cldpres(1:2, 1,1) = (/200,400/)
FI%VD%cldpres(1:2, 1,2) = (/704,725/)
!FI%VD%cldpres(1:2, 1,1) = (/400,800/)

fi%fc(1)%rphase(1)    =  2.0    ! Cloud Phase 1=Water 2=Ice
fi%fc(1)%de(1) = 60.
fi%fc(1)%re(1) = 15.

!fi%fc(1)%asp(1) = exp(iasp*0.1) !! Fu 20006 Ice AspectRatio !!!!! NEW FOR 20010130


fi%fc(1)%tau_vis(1)       = 10.00	    ! Cloud Visible Optical Depth ( Minnis)
fi%fc(1)%sc(1)%mn_lin_tau =  fi%fc(1)%tau_vis(1) *1.15


!-----
fi%fc(1)%rphase(2)    =  1.0    ! Cloud Phase 1=Water 2=Ice
fi%fc(1)%de(2) = 70.
fi%fc(1)%re(2) = 10.

!fi%fc(1)%asp(1) = exp(iasp*0.1) !! Fu 20006 Ice AspectRatio !!!!! NEW FOR 20010130

fi%fc(1)%tau_vis(2)       = 30	    ! Cloud Visible Optical Depth ( Minnis)
fi%fc(1)%sc(2)%mn_lin_tau =  fi%fc(1)%tau_vis(2) 

fi%fc(1)%tau_vis(2)       = 1E-20	    ! Cloud Visible Optical Depth ( Minnis)
fi%fc(1)%sc(2)%mn_lin_tau = 1E-20 !fi%fc(1)%tau_vis(2) 


!Surface Properties --------------------------------------------------

!Allow different albedos for Aerosol Vs. NO Aerosol cases , And for each Clear/Cloud Conditions
fi%sfcalb(1:18,1,0)  = 0.0 ! Clear sky -Spectral Surface Albedo SW
fi%sfcalb(1:18,2,0)  = 0.0 ! Pristine sky -Spectral Surface Albedo SW
fi%sfcalb(1:18,1,1:)  = 0.0  ! CLOUDY w/AOT  sky -Spectral Surface Albedo SW
fi%sfcalb(1:18,2,1:)  = 0.0  ! CLOUDY w/o AOT sky -Spectral Surface Albedo SW

fi%ee(1:12)  = 0.99 ! Spectral Surface Emissivity LW

!Aerosols ------------------------------------------------------------
fi%nac		     = 2	   ! 2 aerosol types 
fi%itps(1)	     = 2	   ! Continental see types (1-18)
fi%itps(2)	     = 1	   
!fi%itps(2)	     = 11	   ! Soot	  see types (1-18)

fi%n_atau	      = 1	   ! 1 wavelength input for aerosols
fi%a_wli(1)	      = 0.641	   ! AOT wavelength(microns) of a_taus
fi%a_taus(1,1)	      =  0.80	   ! AOT for constituent 1

fi%a_taus(1,2)	      =  0.20	   ! AOT for constituent 2

!----------------------------------------------------------------------

 call generate_level_scheme !! Define model Fixed layer structure pre-cloud by fixed DZ intervals...
! call print_vla_in 

 call prepare_model_profile_fu !! CALL After all FI%VD and FI%VI structures are defined.
 call vla_interface_fu     ! uses FI%VO !! Assign Model ATM Profile and CLD Levels
! call print_vla_out

!Aerosol Profile (after fi%pp is created )-----------------------------

 call aer_scale_hgt(fi%nv,fi%pp,3.0,fi%aprofs(1:fi%nv,1) )
 call aer_scale_hgt(fi%nv,fi%pp,3.0,fi%aprofs(1:fi%nv,2) )
! RADIATVE TRANSFER --------------------------------------------------

 call print_in_fu		   ! PRINTS INPUTS  AS ASCII 
 
 call rad_multi_fu  ! CALL THE CODE !!!

! call print_out_fu		   ! PRINTS Lots of OUTPUTS  AS ASCII

 call pack_sky
 
 call print_pack_sky

stop ' Simple.f90 normal end'

end
