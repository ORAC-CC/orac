!-------------------------------------------------------------------------------
! Name: netcdf_structures.F90
!
! Purpose:
! Define variables types which hold the preprocessing output information and
! file, dimension and variable IDs.
!
! Description and Algorithm details:
!
! Arguments:
! Name Type In/Out/Both Description
!
! Local variables:
! Name Type Description
!
! History:
! 2012/05/17: MJ produces initial version of code
! 2012/05/24: MJ adds some commenting
! 2012/08/02: MJ adds some more ids for writing of RTTOV output to netcdf file
! 2012/08/08: CP added albd
! 2012/11/08: CP added in level ids for netcdf files
! 2013/03/07: CP added in some diagnostics albedo and q
! 2013        MJ adds skintid_pw, lnspid_pw
! 2013        CP removes skintid_pw,lnspid_pw declared twice
! 2013/10/23: AP Tidying
! 2014/05/23: GM Some more cleaning and removal of unused elements.
!
! $Id$
!
! Bugs:
! None known
!
!-------------------------------------------------------------------------------

module netcdf_structures

   use preproc_constants

   implicit none

   type netcdf_info_s

      ! file ids
      integer :: ncid_alb,ncid_cf,ncid_config,ncid_geo,ncid_loc,ncid_lsf, &
                 ncid_lwrtm,ncid_msi,ncid_prtm,ncid_scan,ncid_swrtm

      ! fundamental dimensions ids for the different dimensions in the different
      ! files
      integer :: datedim_lw,datedim_pw,datedim_sw
      integer :: lwchanneldim,swchanneldim
      integer :: viewdim_lw,viewdim_sw
      integer :: skintid_pw,lnspid_pw

      integer :: xdim_alb,ydim_alb
      integer :: xdim_cf,ydim_cf
      integer :: xdim_geo,ydim_geo
      integer :: xdim_loc,ydim_loc
      integer :: xdim_lsf,ydim_lsf
      integer :: xdim_lw,ydim_lw
      integer :: xdim_msi,ydim_msi
      integer :: xdim_pw,ydim_pw
      integer :: xdim_sw,ydim_sw

      integer :: xydim_lw,xydim_pw,xydim_sw

      integer :: layerdim_sw,leveldim_sw
      integer :: layerdim_pw,leveldim_pw
      integer :: layerdim_lw,leveldim_lw

      integer :: xdim_scan,ydim_scan

      integer :: vdim_geo,vdim_msi
      integer :: cdim_alb,cdim_emis
      integer :: cdim_config,cdim_config_alb,cdim_config_emis,cdim_msi
      integer :: xdim_config,ydim_config


      ! variable ids

      ! alb file
      integer :: albid,emisid,channelnalbid,channelnemisid

      ! cf file
      integer :: cfid

      ! config file:
      integer :: channelninid_config,channelnabsid_config,channelwlabsid_config, &
                 channelswflag_config,channellwflag_config,channelprocflag_config,&
                 channelnalbid_config,channelnemisid_config

      ! geo file
      integer :: solzenid,satzenid,solazid,satazid,relazid,senazid

      ! loc file
      integer :: latid,lonid

      ! lsf file
      integer :: lsfid

      ! lwrtm file:
      integer :: counterid_lw
      integer :: date_id_lw,instr_id_lwm,channels_id_lw,channels_id_instr_lw, &
                 wvn_id_lw
      integer :: lonid_lw,latid_lw,pprofile_lay_id_lw,pprofile_lev_id_lw
      integer :: tac_id_lw,tbc_id_lw,emiss_id_lw,rbc_up_id_lw,rac_up_id_lw, &
                 rac_down_id_lw
      integer :: solzaid_lw,satzaid_lw,relaziid_lw

      ! msi file:
      integer :: msid,timeid,channelninid,channelnabsid,channelwlabsid, &
                 channelswflag,channellwflag,channelprocflag

      ! prtm file:
      integer :: date_id_pw
      integer :: iid_pw,jid_pw,counterid_pw
      integer :: lonid_pw,latid_pw,pprofile_lay_id_pw,tprofile_lay_id_pw, &
                 hprofile_lay_id_pw,qprofile_lay_id_pw,satzenid_pw,solzenid_pw, &
                 lsfid_pw
      integer :: pprofile_lev_id_pw,tprofile_lev_id_pw,hprofile_lev_id_pw, &
                 qprofile_lev_id_pw

      ! scan file
      integer :: uscanid,vscanid

      ! swrtm file:
      integer :: counterid_sw
      integer :: date_id_sw,instr_id_swm,channels_id_sw,channels_id_instr_sw, &
                 wvn_id_sw
      integer :: lonid_sw,latid_sw,pprofile_lay_id_sw,pprofile_lev_id_sw
      integer :: tac_id_sw,tbc_id_sw
      integer :: solzaid_sw,satzaid_sw,relaziid_sw


      ! derived dimensions ids
      integer :: xycdim_lw(2)
      integer :: xyzdim_lw(2),xyzdim_pw(2),xyzdim_sw(2)
      integer :: xyzcdim_lw(3),xyzcdim_sw(3)
      integer :: xyzcvdim_lw(4),xyzcvdim_sw(4)
      integer :: xyvdim_lw(2),xyvdim_sw(2)

      ! for 2D variables (horitontal dimension)
      integer(kind=lint) :: start_1d(1),counter_1d(1),stride_1d(1)
      ! for 3D variables (horizontal+vertical)
      integer(kind=lint) :: start_2d(2),counter_2d(2),stride_2d(2)
      ! for 4D variables (space+wavelength)
      integer(kind=lint) :: start_3d(3),counter_3d(3),stride_3d(3)
      ! for 5D variables (space+wavelength+viewing direction)
      integer(kind=lint) :: start_4d(4),counter_4d(4),stride_4d(4)

  end type netcdf_info_s

end module netcdf_structures
