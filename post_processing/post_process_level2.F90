  ! Name:
  ! post_process_level2 
  !
  ! Purpose:
  !    read in level2 ORAC output and apply postprocessing to select cloudmask
  !    and phase.
  !
  ! Description:
  !    An input data file is required to be present
  ! Arguments:
  !    Name       Type    In/Out/Both    Description
  !    filename
  !
  ! Algorithm:
  !    N/A
  !
  ! select min cost for each pixel after applying ranges
  !
  !     ci=cost(*,ip)
  !     ri=cre(*,ip)
  !     oi=lopd(*,ip)
  !     wh=where(ri lt rmin or ri gt rmax or oi lt omin or oi gt omax,nw)
  !     if nw gt 0 then ci(wh)=1d9
  !
  ! deselect ice cloud if cost similar to liquid and TC is high 
  !
  !     if nl eq 1 and ni eq 1 then begin
  !           if ci(il) lt ci(ii)*1.5 or ci(il) lt ny*1.5 and tc(ip) gt 273
  !             then ci(ii)=1e9
  !     endif
  !
  ! deselect ash if cost similar to liquid or ice or very low altitude
  !
  !     if nv eq 1 and ni eq 1 then if ci(ii) lt ci(iv)*1.1 then ci(iv)=1e9
  !     if nv eq 1 and nl eq 1 then if ci(il) lt ci(iv)*1.1 then ci(iv)=1e9
  !     iphase(ip)=(where(ci eq min(ci)))(0)
  !
  ! cloud mask selected if retrieval has converged and if optical depth >cot_thres
  !
  !
  ! Local variables:
  !    Name       Type    Description
  !
  ! History:
  !   9/2/2012 Caroline Poulsen :original version
  !    needs modifying to work with AATSR/AVHRR
  !    does not output covariance information into secondary output file.
  !    have not included cloud mask at present.
  ! 2012/03/18 Caroline Poulsen modified to add cloud flag
  ! 2012/07/06 MJ extensively overhauls and restructures the code
  ! 2012/07/13 MJ extends range of valid ref for netcdf write
  ! 2012/07/23 MJ adds choice of strictness of cloud checks.
  ! 2012/07/23 MJ adds code to play with cloud screening during day and night
  !20120827 MJ better implements time variable in output.
  !November 2012 to Jan. 2013 SS, MJ AND MS make major changes to cloud masking approach, 
  ! MJ implements untested OMP parallelization, changes wrt CF compliance.
  ! 2013/01/17 Matthias Jerg: Adds code to accommodate uncertainties of ctt and cth
  ! 2014/01/22 MJ fixes FP overflow with COT uncertainty.
  !2014/01/23 MJ changes of how COT uncertainty is treated.
  !2014/02/07 MJ corrects logical error (water got penalty when ice prop.
  ! were out of range and vice versa),cleans up code 
  !2014/02/10 MJ reorders and rewrites the penalty assignment in phase selection
  !2014/03/14 MJ removes code to scale cot from log_10(tau) to tau as this
  ! step was already introduced in ORAC.
  !2014/07/08 CP: added more illumination options
  !2014/07/08 CP: added in extra subroutine that uses Istomina like tests to
  ! detect clear snow and ice
  !2014/09/29 changed wat read_inter)sec is called now need a instrument name
  !2014/10/24, OS: added subroutine option for wrapper mode, and further
  ! wrapper-specific details; SteSta added new fill value option for the
  !  postprocessing cloud mask
  !2014/11/20, OS: renamed module neural_net_constants to neural_net_constants_postproc;
  !  some editing; replaced hard-coded NN values with variables     
  !2015/02/05, OS: changed nint to lint; further replacements of variables
  !                defined in vartypes_pp with those defined in common_constants;
  !                added phase_post; Pavolonis phase is now used as basis for
  !2015/02/06 CP added in case of ML IR only retrieval
  !2015/02/06 CP tidied up phase selection removed obsolete stuff
  !2015/02/06 CP added case of IR only multi layer selection
  !                phase-dependent variables
  !2015/02/07 CP massive tidy up including of common constants
  !2015/03/19 OS COT maximum value set to 100, CWP scaled accordingly + minor editing
  !2015/04/22 OS only apply Istomina retyping when cloud mask says cloud 
  !2015/07/06 OS added flags "one_phase_only" and "cloudy_only" as 
  !                file input; reclassify phase if CTT does not match
  !                phase temperature range (i.e. no ice if CTT>273.15K);
  !                removed some debugging statements and further clean up
  !2015/07/10 OS fixed bug related to using flag one_phase_only
  !
  ! Bugs:
  !    None known.
  !  will not work where nthermal=3
  ! works only with one viewing angle
  ! $Id$
  !
  !TODO: rewrite reading of data done
  !TODO: seperate processing of primary and secondary file, make secondary
  ! optional. Is in progress
  !TODO: put EVERYTHING of the actual processing in loops with along track
  ! outside and across track inside done
  !TODO: seperate cloudmasking and phase determination partly done
  !TODO: deallocate EVERYTHING that is not absolutely necessary. done
  !TODO :QIALITY CONTROL ETC.?
  !TODO: work on secondary file contents step by step to keep RAM req. low
  !---------------------------------------------------------------------
  !---------------------------------------------------
  !---------------------------------------------------

#ifndef WRAPPER
  program post_process_level2
#else
    subroutine post_process_level2(mytask,ntasks,lower_bound,upper_bound,path_and_file)
#endif
      !---------------------------------------------------
      !---------------------------------------------------

      use netcdf

      use scanline_structure

      use vartypes_pp
      use common_constants

      use structures_pp

      use neural_net_constants_postproc
      use global_attributes
      use source_attributes

      implicit none

      type(global_attributes_s) :: global_atts
      type(source_attributes_s) :: source_atts

      integer          :: status = 0 ! Status value returned from subroutines
      integer :: ncid,ierr,wo,ny=5,nx=5, iinput,nsolar=3,nthermal=2
      integer(kind=lint) :: ix,iy,ixstart,ixstop,iystart,iystop,i,j,nviews=1,ia
      logical :: lcovar=.true.
      INTEGER :: ncid_primary,ncid_secondary,ncid_input,iviews,dims_var(2),is,js
      integer :: ivar,idim,ndim,nvar,nattr,dummyint,jdim,iphase
      real :: dummyreal,resin

      character(len=path_length) :: ecmwf_path,path_and_file,&
           & fnamewat_prim,fnameice_prim,fnamewat_sec,fnameice_sec, &
	   fnamemli_sec,fnamemli_prim
      character(len=20) :: input_num,input_num1,input_num2
      character(len=500) :: input_dummy,s_input_dummy  
      integer (kind=lint), allocatable :: dimids(:), varids(:), attrids(:), dimlength(:)


      character(len=var_length), allocatable :: dname(:)

      character (len=var_length), allocatable, dimension(:) ::  available_names(:)

      character(len=path_length) :: name,fname,L2_primary_outputpath_and_file, &
           L2_secondary_outputpath_and_file,time_str

      character(len=var_length) ::  chan_id(5),inst

      integer :: proc_flag(5)

      character(len=var_length) :: uuid_tag_primary,uuid_tag_secondary

      integer (kind=lint)  :: xdim,ydim,levlistdim,levtypedim,timedim,k

      real :: minre(2),maxre(2),minod(2),maxod(2),maxcost=1000000.,costfactor=1.5, &
           cot_thres,cot_thres1,cot_thres2
      logical :: one_phase_only, cloudy_only

      integer(kind=byte), parameter :: IPhaseClU = 0     ! clear/unknoiwn
      integer(kind=byte), parameter :: IPhaseWat = 1     ! Water
      integer(kind=byte), parameter :: IPhaseIce = 2     ! Ice
      integer(kind=byte), parameter :: IPhasemli = 3     ! MLI

      integer(kind=byte), allocatable, dimension(:,:) :: phaseflag

      integer(kind=lint) :: nvars=1,nvars_errors=1, n_val_plus_error=2, &
           n_oe_features=3


      real (kind=sreal) ::tempdiff,newcot

      type(l2_input_struct_2d_primary) :: l2_input_2dwat_primary, &
           l2_input_2dice_primary, l2_input_2dmli_primary
      type(l2_input_struct_2d_secondary) ::  l2_input_2d_secondary
      type(l2_input_struct_2d_refl_bt) :: l2_input_2dice_refl_bt, &
           l2_input_2dwat_refl_bt,l2_input_2dmli_refl_bt
      type(spixel_scanline_primary_output) :: spixel_scan_out
      type(spixel_scanline_output_rt) ::  spixel_scan_out_rt

      real(kind=sreal) :: precision,huge_value,log10huge_value,log10precision

      integer(kind=byte) :: lsec_flag,lstrict,snow_ice_flag,mli_flag

      integer :: nargs

      character(len=attribute_length_long) :: reference,history,summary,keywords, &
           comment,license,csource,std_name_voc
      character(len=attribute_length) :: cprodtype, l1closure, platform, csensor
      CHARACTER(len=attribute_length) :: cncver,ccon,cinst, &
           instname, contact, website, prodtime, ctitle, cproc, cprocver, &
           l2cproc, l3cproc,l2cprocver, l3cprocver, prod_name, &
           year, month, day,grid_type, project,cfile_version

      integer(kind=byte),allocatable, dimension(:) :: flag_vals
      integer :: nflags

      integer :: nthreads,nompthreads,OMP_GET_max_THREADS

      !nn variables
      integer(kind=lint) :: noob,noob_total !number of pixels out of bounds
      integer(kind=lint) :: nneurons !number of employed neurons
      integer(kind=lint) :: ninput !number of criterias (input dimensions of nn)
      integer(kind=lint) :: noutput !number of output dimensions (1 as we only have cm)
      real(kind=sreal),allocatable, dimension(:,:) :: inv,minmax_train,scales
      real(kind=sreal),allocatable, dimension(:) :: input,outv
      real(kind=sreal) :: output
      real(kind=sreal) :: oscales(3)
      real(kind=sreal) :: temperature,cutoff,bias_i,bias_h
      integer :: solar_chan_id(2)
      integer(kind=lint) :: nchan_solar

      integer :: ibit

      ! maximum COT and corresponding scaling value
      real(kind=sreal) :: cot_max, cot_scale 

      !this is for the wrapper
      integer :: mytask,ntasks,lower_bound,upper_bound

      !how many threads are available?
      !nompthreads=OMP_get_max_threads()
      !!if( nompthreads .ge. 4)  nompthreads=4
      !!nompthreads=max(1,nompthreads)
      !!call OMP_set_num_threads(nompthreads)
      !write(*,*) 'Along-track loop of postprocessing running on: ', nompthreads, 'threads'

      wo=0 ! be more verbose on output if = 1 (to be replaced with verbose keyword)
      mli_flag=0

      noob=0_lint
      noob_total=0_lint

      precision=tiny(1.0_sreal)
      huge_value=huge(1.0_sreal)
      log10huge_value=log10(huge_value)
      ! include a log10precision value for COT + Uncertainty otherwise no
      !  optical thickness value below 1 will be written
      log10precision=log10(precision)

#ifndef WRAPPER
      nargs = COMMAND_ARGUMENT_COUNT()
#else
      nargs=-1
#endif

      !if no argument was given then read standard file
      if(nargs .eq. 0 ) then

         call get_environment_variable("ORAC_TEXTIN",path_and_file)

      elseif(nargs .eq. 1) then

         call get_command_argument(1,path_and_file)

      elseif(nargs .eq. -1) then

         write(*,*) 'inside postproc ',trim(adjustl(path_and_file))

      endif

      write(*,*)'path_and_file =',path_and_file
      !read from an external file
      open(11,file=trim(adjustl(path_and_file)), &
           & status='old', form='formatted')
      read(11,*) fnamewat_prim
      write(*,*) 'fnamewat_prim =',fnamewat_prim
      read(11,*) fnameice_prim
      write(*,*) 'fnameice_prim =',fnameice_prim
      if ( mli_flag .eq. 1) then
      	 read(11,*) fnamemli_prim
      	 write(*,*) 'fnamemli_prim =',fnamemli_prim
      endif

      read(11,*) fnamewat_sec
      write(*,*) 'sec wat =',fnamewat_sec
      read(11,*) fnameice_sec
      write(*,*) 'secice =',fnameice_sec
      if ( mli_flag .eq. 1) then
      	 read(11,*) fnamemli_sec
      	 write(*,*) 'secice =',fnamemli_sec
      endif

      read(11,*) L2_primary_outputpath_and_file
      write(*,*) 'prin =',L2_primary_outputpath_and_file
      read(11,*) L2_secondary_outputpath_and_file
      write(*,*) 'sec =',L2_secondary_outputpath_and_file
      read(11,*) one_phase_only
      write(*,*) 'one_phase_only =',one_phase_only
      read(11,*) cloudy_only
      write(*,*) 'cloudy_only =',cloudy_only
      read(11,*) cot_thres
      write(*,*) 'cot_thres =',cot_thres
      read(11,*) cot_thres1
      write(*,*) 'cot_thres1 =',cot_thres1
      read(11,*) cot_thres2
      write(*,*) 'cot_thres2 =',cot_thres2
      read(11,*) proc_flag 
      write(*,*)'proc =', proc_flag 
      read(11,*) inst
      write(*,*) 'insts =',inst
      read(11,*) lsec_flag
      write(*,*) 'lsec =',lsec_flag 
      read(11,*) lstrict
!!!xxx too late to read in here
      !      read(11,*) mli_flag
      !      write(*,*) 'mli_flag =',mli_flag
      !      close(11)
      write(*,*) 'cot_thres and kind =',cot_thres,cot_thres1,cot_thres2, &
           kind(cot_thres),kind(cot_thres1),kind(cot_thres2)

      ! read ice and water intermediate files and put into a structure

      call read_input_dimensions(fnameice_prim,xdim,ydim,wo)
      write(*,*) 'read input dimensions stop'

      ! phase = ICE
      write(*,*) 'PICE'
      iphase=2
      call set_l2_input_struct_2d_primary_ice(iphase,l2_input_2dice_primary,xdim,ydim)
      write(*,*) 'read ice start  =', fnameice_prim
      call read_inter_file_ice(iphase,fnameice_prim,l2_input_2dice_primary, &
           xdim,ydim,global_atts,source_atts,wo,ierr)
      write(*,*) 'read ice stop' 

      ! phase = WAT
      write(*,*) 'PWAT'
      iphase=1
      call set_l2_input_struct_2d_primary_wat(iphase,l2_input_2dwat_primary,xdim,ydim)
      write(*,*) 'read wat start  =',fnamewat_prim
      call read_inter_file_water(iphase,fnamewat_prim,l2_input_2dwat_primary, &
           xdim,ydim,global_atts,source_atts,wo,ierr)
      write(*,*) 'read wat stop'

      call set_l2_input_struct_2d_secondary(l2_input_2d_secondary,xdim,ydim)
      write(*,*) 'read ice secondary start  =',fnameice_sec
      call  read_inter_sec_file(inst,fnameice_sec,l2_input_2d_secondary,xdim,ydim,wo,ierr)
      !write(*,*)'albedo',l2_input_2d_secondary%albedo_in_channel_no_2

      mli_flag=0
      if (mli_flag .gt. 0 )then	
         ! phase = MLI
         write(*,*) 'PMLI'
         iphase=3
         call set_l2_input_struct_2d_primary_mli(iphase,l2_input_2dmli_primary,xdim,ydim)
         write(*,*) 'read mli start  =',fnamemli_prim
         call read_inter_file_mli(iphase,fnamemli_prim,l2_input_2dmli_primary, &
              xdim,ydim,global_atts,source_atts,wo,ierr)
         write(*,*) 'read mli stop'



      endif


      !set the loop bounds
      ixstart=1
      ixstop=xdim

      iystart=1
      iystop=ydim

      allocate(phaseflag(ixstart:ixstop,iystart:iystop))

      !set default: ice wins
      phaseflag=2

      write(*,*) 'Processing limits:'
      write(*,*) ixstart,ixstop
      write(*,*) iystart,iystop
!!!!!!!!!!!!!!!!!!!!!!!!!!!!
      ! loop over pixels        !!
!!!!!!!!!!!!!!!!!!!!!!!!!!!!



      do j=iystart,iystop
         do i=ixstart,ixstop

            !default: ICE wins, meaning only ice structure is overwritten 
            !and used in the end for the output
            l2_input_2dice_primary%phase(i,j)=iphaseice
            l2_input_2dice_primary%phase_post(i,j) = iphaseice  

            !Information of Pavolonis cloud type
            !C,N,F,W,S,M,I,Ci,O
            !Liquid cloud phase comprises the following categories:
            !0? Clear
            !2? fog,
            !3? warm liquid water clouds, and
            !4? supercooled-mixed-phased clouds.
            !? Ice cloud phase comprises the following categories:
            !6? opaque ice clouds/deep convection,
            !7? nonopaque high ice clouds (e.g. cirrus),
            !8? cloud overlap 4 (e.g. multiple cloud layers), and

            ! here apply Pavolonis phase information to select retrieval phase variables 
            ! select water type overwrite ice
            if( l2_input_2dice_primary%cldtype(i,j) .gt. 1 .and. &
                 l2_input_2dice_primary%cldtype(i,j) .lt. 5 ) then

               phaseflag(i,j) = 1_byte 
               if ( ( .not. one_phase_only ) .and. & ! only reclassify if both phases were processed
                    ( ( (l2_input_2dwat_primary%ctt(i,j) .lt. 233.16) .and. &
                    ( l2_input_2dwat_primary%ctt(i,j) .ne. sreal_fill_value) ) .and. &
                    ( (l2_input_2dice_primary%ctt(i,j) .lt. 273.16 ) .and. &
                    (l2_input_2dice_primary%ctt(i,j) .ne. sreal_fill_value) ) ) ) phaseflag(i,j) = 2_byte

            else

               phaseflag(i,j) = 2_byte
               if ( ( .not. one_phase_only ) .and. & ! only reclassify if both phases were processed
                    ( ( l2_input_2dice_primary%ctt(i,j) .ge. 273.16 ) .and. ( l2_input_2dwat_primary%ctt(i,j) .ge. 233.16 ) ) ) phaseflag(i,j) = 1_byte

            endif

            if (phaseflag(i,j) .eq. 1_byte) then

               l2_input_2dice_primary%cot(i,j)=l2_input_2dwat_primary%cot(i,j)
               l2_input_2dice_primary%ref(i,j)=l2_input_2dwat_primary%ref(i,j)
               l2_input_2dice_primary%ctp(i,j)=l2_input_2dwat_primary%ctp(i,j)
               l2_input_2dice_primary%stemp(i,j)=l2_input_2dwat_primary%stemp(i,j)
               l2_input_2dice_primary%cth(i,j)=l2_input_2dwat_primary%cth(i,j)
               l2_input_2dice_primary%cth_uncertainty(i,j)&
                    &=l2_input_2dwat_primary%cth_uncertainty(i,j)
               l2_input_2dice_primary%ctt(i,j)=l2_input_2dwat_primary%ctt(i,j)
               l2_input_2dice_primary%ctt_uncertainty(i,j)&
                    &=l2_input_2dwat_primary%ctt_uncertainty(i,j)
               l2_input_2dice_primary%cwp(i,j)=l2_input_2dwat_primary%cwp(i,j)
               l2_input_2dice_primary%cloud_albedo(i,j,:)=l2_input_2dwat_primary%cloud_albedo(i,j,:)
               l2_input_2dice_primary%cot_uncertainty(i,j)&
                    &=l2_input_2dwat_primary%cot_uncertainty(i,j)
               l2_input_2dice_primary%ref_uncertainty(i,j)&
                    &=l2_input_2dwat_primary%ref_uncertainty(i,j)
               l2_input_2dice_primary%ctp_uncertainty(i,j)&
                    &=l2_input_2dwat_primary%ctp_uncertainty(i,j)
               l2_input_2dice_primary%cc_total_uncertainty(i,j)&
                    &=l2_input_2dwat_primary%cc_total_uncertainty(i,j)
               l2_input_2dice_primary%stemp_uncertainty(i,j)&
                    &=l2_input_2dwat_primary%stemp_uncertainty(i,j)
               l2_input_2dice_primary%cwp_uncertainty(i,j) &
                    &=l2_input_2dwat_primary%cwp_uncertainty(i,j)
               l2_input_2dice_primary%convergence(i,j)=l2_input_2dwat_primary&
                    &%convergence(i,j)
               l2_input_2dice_primary%niter(i,j)=l2_input_2dwat_primary%niter(i,j)
               l2_input_2dice_primary%phase(i,j)=iphasewat
               l2_input_2dice_primary%costja(i,j)=l2_input_2dwat_primary%costja(i,j)
               l2_input_2dice_primary%costjm(i,j)=l2_input_2dwat_primary%costjm(i,j)
               l2_input_2dice_primary%qcflag(i,j)=l2_input_2dwat_primary%qcflag(i,j)

            endif

            l2_input_2dice_primary%cc_total(i,j)= l2_input_2dice_primary%cldmask(i,j)
            !            l2_input_2dice_primary%cc_total_uncertainty(i,j)= l2_input_2dice_primary%cldmask_uncertainty(i,j)

            !if tau too high, set to max value not to fill value
            if(l2_input_2dice_primary%cot(i,j) .ge. dither) then
               if(l2_input_2dice_primary%cot_uncertainty(i,j) .le. log10precision) then
                  newcot=-999.0
               elseif(l2_input_2dice_primary%cot_uncertainty(i,j)&
                    &+log(l2_input_2dice_primary%cot(i,j)) .gt. &
                    & log10huge_value) then
                  newcot=320.0
               else
                  newcot=10.0**(l2_input_2dice_primary%cot_uncertainty(i,j))&
                       &*l2_input_2dice_primary%cot(i,j)
               endif
            else
               newcot=-999.0
            endif
            l2_input_2dice_primary%cot_uncertainty(i,j)=newcot

            !Don't set fill values and leave final masking of products to user
            !obsolete: if cloud free, set primary retrieval parameters to fill value
            if(l2_input_2dice_primary%cc_total(i,j) .eq. 0.0) then

               !set phase to clear/unknown
               l2_input_2dice_primary%phase(i,j)=IPhaseClU
               l2_input_2dice_primary%phase_post(i,j)=IPhaseClU

            endif

         enddo
      enddo


      !deallocate the water structure
      iphase=1

      call unset_l2_input_struct_2d_primary_wat(iphase,l2_input_2dwat_primary)
      call unset_l2_input_struct_2d_secondary(l2_input_2d_secondary)

      !now write things out
      !open the netcdf output file
      call nc_create_global_l2_pp(trim(adjustl(L2_primary_outputpath_and_file)),&
           & ncid_primary,  ixstop-ixstart+1, iystop-iystart+1, dims_var, &
	   wo,1,global_atts,source_atts,status)

      !allocate the structure which hold the output in its final form
      call alloc_spixel_scan_out_pp( ixstart,ixstop,iystart,iystop,nviews,spixel_scan_out)

      !define scales,ranges etc.
#include "def_vars_primary_pp.inc"

      !put results in final output arrays with final datatypes
      do j=iystart,iystop
         do i=ixstart,ixstop
#include "prepare_primary_pp.inc"
         enddo
      enddo

      !deallocate primary file ice input
      iphase=2
      call unset_l2_input_struct_2d_primary_ice(iphase,l2_input_2dice_primary)

      !now write everything in one big chunk of data to disk
#include "write_primary_pp.inc"

      !deallocate output structure
      call dealloc_spixel_scan_out_pp(spixel_scan_out)

      !close output file
      call nc_close_pp(ncid_primary,trim(adjustl(L2_primary_outputpath_and_file)),wo)

      deallocate(phaseflag)

#ifdef WRAPPER
    end subroutine post_process_level2
#else
  end program post_process_level2
#endif




