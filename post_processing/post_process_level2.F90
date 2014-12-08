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
      integer(kind=nint) :: ix,iy,ixstart,ixstop,iystart,iystop,i,j,nviews=1,ia
      logical :: lcovar=.true.
      INTEGER :: ncid_primary,ncid_secondary,ncid_input,iviews,dims_var(2),is,js
      integer :: ivar,idim,ndim,nvar,nattr,dummyint,jdim,iphase
      real :: dummyreal,resin

      character(len=cpathlength) :: ecmwf_path,path_and_file,&
           & fnamewat_prim,fnameice_prim,fnamewat_sec,fnameice_sec
      character(len=20) :: input_num,input_num1,input_num2
      character(len=500) :: input_dummy,s_input_dummy  
      integer (kind=nint), allocatable :: dimids(:), varids(:), attrids(:), dimlength(:)


      character(len=varlength), allocatable :: dname(:)

      character (len=varlength), allocatable, dimension(:) ::  available_names(:)

      character(len=cpathlength) :: name,fname,L2_primary_outputpath_and_file, &
           L2_secondary_outputpath_and_file

      character(len=varlength) ::  chan_id(5),inst

      integer :: proc_flag(5)

      character(len=uuid_length) :: uuid_tag_primary,uuid_tag_secondary

      integer (kind=nint)  :: xdim,ydim,levlistdim,levtypedim,timedim,k

      real :: minre(2),maxre(2),minod(2),maxod(2),maxcost=1000000.,costfactor=1.5, &
           cot_thres,cot_thres1,cot_thres2

      integer, parameter :: IPhaseClU = 0     ! clear/unknoiwn
      integer, parameter :: IPhaseWat = 1     ! Water
      integer, parameter :: IPhaseIce = 2     ! Ice

      integer(kind=byte), allocatable, dimension(:,:) :: phaseflag

      integer :: penaltyflag_w, penaltyflag_i

      integer(kind=nint) :: nvars=1,nvars_errors=1, n_val_plus_error=2,n_oe_features=3

      real (kind=sreal) ::  costice,  costwat ,newcot, costice_store, costwat_store, &
           temp_thres,temp_thres_h,temp_thres_m,temp_thres_l,temp_thres1, &
           ctp_bound, ctp_bound_up,ctp_thres, ctp_thres1,ctt_bound, &
           ctt_bound_summer,ctt_bound_winter,ctt_thres,ctp_udivctp, tempdiff

      type(l2_input_struct_2d_primary) :: l2_input_2dwat_primary, l2_input_2dice_primary
      type(l2_input_struct_2d_secondary) ::  l2_input_2d_secondary
      type(l2_input_struct_2d_refl_bt) :: l2_input_2dice_refl_bt,l2_input_2dwat_refl_bt
      type(spixel_scanline_primary_output) :: spixel_scan_out
      type(spixel_scanline_output_rt) ::  spixel_scan_out_rt

      real(kind=sreal) :: precision,huge_value,log10huge_value,log10precision

      integer(kind=byte) :: lsec_flag,lstrict,snow_ice_flag

      integer :: nargs

      character(len=description_length) :: reference,history,summary,keywords, &
           comment,license,csource,std_name_voc
      character(len=paramlength) :: cprodtype, l1closure, platform, csensor
      CHARACTER(len= attribute_length) :: cncver,ccon,cinst, &
           instname, contact, website, prodtime, ctitle, cproc, cprocver, &
           l2cproc, l3cproc,l2cprocver, l3cprocver, prod_name, &
           year, month, day,grid_type, project,cfile_version

      integer(kind=byte),allocatable, dimension(:) :: flag_vals
      integer :: nflags

      integer :: nthreads,nompthreads,OMP_GET_max_THREADS

      !nn variables
      integer(kind=lint) :: noob,noob_total !number of pixels out of bounds
      integer(kind=nint) :: nneurons !number of employed neurons
      integer(kind=nint) :: ninput !number of criterias (input dimensions of nn)
      integer(kind=nint) :: noutput !number of output dimensions (1 as we only have cm)
      real(kind=sreal),allocatable, dimension(:,:) :: inv,minmax_train,scales
      real(kind=sreal),allocatable, dimension(:) :: input,outv
      real(kind=sreal) :: output
      real(kind=sreal) :: oscales(3)
      real(kind=sreal) :: temperature,cutoff,bias_i,bias_h
      integer :: solar_chan_id(2)
     integer(kind=lint) :: nchan_solar

      integer :: ibit

      !this is for the wrapper
      integer :: mytask,ntasks,lower_bound,upper_bound

      !how many threads are available?
      !nompthreads=OMP_get_max_threads()
      !!if( nompthreads .ge. 4)  nompthreads=4
      !!nompthreads=max(1,nompthreads)
      !!call OMP_set_num_threads(nompthreads)
      !write(*,*) 'Along-track loop of postprocessing running on: ', nompthreads, 'threads'

      wo=1 ! be more verbose on output if = 1 (to be replaced with verbose keyword)

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


      !read from an external file
      open(11,file=trim(adjustl(path_and_file)), &
           & status='old', form='formatted')
      read(11,*) fnamewat_prim
      write(*,*) 'fnamewat_prim',fnamewat_prim
      read(11,*) fnameice_prim
      write(*,*) 'fnameice_prim',fnameice_prim
      read(11,*) fnamewat_sec
      write(*,*) 'sec wat',fnamewat_sec
      read(11,*) fnameice_sec
      write(*,*) 'secice',fnameice_sec
      read(11,*) L2_primary_outputpath_and_file
      write(*,*) 'prin',L2_primary_outputpath_and_file
      read(11,*) L2_secondary_outputpath_and_file
      write(*,*) 'sec',L2_secondary_outputpath_and_file
      read(11,*) minre
      write(*,*) 'min re',minre
      read(11,*) maxre
      write(*,*) 'maxre',maxre
      read(11,*) minod
      write(*,*) 'minod',minod
      read(11,*) maxod
      write(*,*) 'max od',maxod
      read(11,*) maxcost
      write(*,*) 'cost',maxcost
      read(11,*) costfactor
      write(*,*) 'costfactor',costfactor
      read(11,*) cot_thres
      write(*,*) 'cot_thres',cot_thres
      read(11,*) cot_thres1
      write(*,*) 'cot_thres1',cot_thres1
      read(11,*) cot_thres2
      write(*,*) 'cot_thres2',cot_thres2
      read(11,*) proc_flag 
      write(*,*)'proc', proc_flag 
      read(11,*) inst
      write(*,*) 'insts',inst
      write(*,*) 'fnamewat ',fnamewat_prim
      read(11,*) lsec_flag
      write(*,*) 'lsec',lsec_flag 
      read(11,*) lstrict
      write(*,*) 'cot_thres and kind',cot_thres,cot_thres1,cot_thres2, &
           kind(cot_thres),kind(cot_thres1),kind(cot_thres2)
      read(11,*) temp_thres_h
      read(11,*) temp_thres_m
      read(11,*) temp_thres_l
      read(11,*) temp_thres1
      write(*,*) 'temp_thres_h,m,l',temp_thres_h,temp_thres_m,temp_thres_l, &
           temp_thres1,kind(temp_thres_h), kind(temp_thres_m), &
           kind(temp_thres_l),kind(temp_thres1)
      read(11,*) ctt_bound
      read(11,*) ctt_bound_winter
      read(11,*) ctt_bound_summer
      write(*,*)  'ctt_bound',ctt_bound,ctt_bound_winter,ctt_bound_summer,kind(ctt_bound), &
           kind(ctt_bound_winter),kind(ctt_bound_summer)
      read(11,*) ctt_thres
      write(*,*)  'ctt_thres',ctt_thres,kind(ctt_thres)
      read(11,*) ctp_thres
      read(11,*) ctp_thres1
      write(*,*)  'ctp_thres',ctp_thres,ctp_thres1,kind(ctp_thres),kind(ctp_thres1)
      read(11,*) ctp_bound
      write(*,*)  'ctp_bound',ctp_bound,kind(ctp_bound)
      read(11,*) ctp_bound_up
      write(*,*)  'ctp_bound_up',ctp_bound_up,kind(ctp_bound_up)
      read(11,*) ctp_udivctp
      write(*,*)  'ctp_udivctp',ctp_udivctp,kind(ctp_udivctp)

      !here all those important attributes are read in to be written to the netcdf file
!      read(11,*) uuid_tag_primary
!      uuid_tag_primary=trim(adjustl(uuid_tag_primary))
!      read(11,*)  uuid_tag_secondary
!      uuid_tag_secondary=trim(adjustl(uuid_tag_secondary))
!      read(11,*) platform
!      read(11,*) prodtime
!      read(11,*) prod_name
!      read(11,*) cprodtype
!      read(11,*) cncver
!      read(11,*) ccon
!      read(11,*) cinst
!      read(11,*) l2cproc
!      read(11,*) l2cprocver
!      read(11,*) contact
!      read(11,*) website
!      read(11,*) reference
!      read(11,*) history
!      read(11,*) summary
!      read(11,*) keywords
!      read(11,*) comment
!      read(11,*) project
!      read(11,*) license
!      read(11,*) cfile_version
!      read(11,*) csource
!      read(11,*) 
!      read(11,*) std_name_voc

      close(11)


      ! read ice and water intermediate files and put into a structure
!      write(*,*) 'post cinst ', cinst
!      write(*,*) 'post plat ', platform
!      write(*,*) 'post hist ', history
!      write(*,*) 'post prodt ', cprodtype
!      write(*,*) 'post contact ', contact
!      write(*,*) 'read input dimensions start'
      call read_input_dimensions(fnameice_prim,xdim,ydim,wo)
      write(*,*) 'read input dimensions stop'

      ! phase = ICE
      write(*,*) 'PICE'
      iphase=2
      call set_l2_input_struct_2d_primary_ice(iphase,l2_input_2dice_primary,xdim,ydim)
      write(*,*) 'read ice start ', fnameice_prim
      call read_inter_file_ice(iphase,fnameice_prim,l2_input_2dice_primary, &
           xdim,ydim,global_atts,source_atts,wo,ierr)
      write(*,*) 'read ice stop' 

      ! phase = WAT
      write(*,*) 'PWAT'
      iphase=1
      call set_l2_input_struct_2d_primary_wat(iphase,l2_input_2dwat_primary,xdim,ydim)
      write(*,*) 'read wat start ',fnamewat_prim
      call read_inter_file_water(iphase,fnamewat_prim,l2_input_2dwat_primary, &
           xdim,ydim,global_atts,source_atts,wo,ierr)
      write(*,*) 'read wat stop'
      call set_l2_input_struct_2d_secondary(l2_input_2d_secondary,xdim,ydim)
      write(*,*) 'read wat secondary start ',fnameice_sec
      call  read_inter_sec_file(inst,fnameice_sec,l2_input_2d_secondary,xdim,ydim,wo,ierr)

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


      !loop over pixels
      !OMP PARALLEL DO &
      !OMP PRIVATE(dummyreal,costwat,costice,temp_thres,tempdiff,i,j,nneurons
      !,ninput,noutput,minmax_train,inv,outv,input,scales,oscales,temperature
      !,bias_i,bias_h,output) &
      !OMP FIRTSTPRIVATE(costfactor,newcot) &
      !OMP REDUCTION(+:noob) &
      !OMP SCHEDULE(GUIDED,100)
      do j=iystart,iystop
         do i=ixstart,ixstop

            costwat=0.00_sreal
            costice=0.00_sreal

            costwat_store=0.00_sreal
            costice_store=0.00_sreal

            !default: ICE wins, meaning only ice structure is overwritten 
            !and used in the end for the output
            l2_input_2dice_primary%phase(i,j)=iphaseice

            !Determine the total cost(s) for WAT and ICE
            if(l2_input_2dwat_primary%costja(i,j) .gt. 0.0_sreal) then

               costwat=l2_input_2dwat_primary%costja(i,j)

            endif

            if(l2_input_2dwat_primary%costjm(i,j) .gt. 0.0_sreal) then

               costwat=costwat+l2_input_2dwat_primary%costjm(i,j)

            endif
            costwat_store=costwat


            if(l2_input_2dice_primary%costja(i,j) .gt. 0.0_sreal) then

               costice=l2_input_2dice_primary%costja(i,j)


            endif

            if(l2_input_2dice_primary%costjm(i,j) .gt. 0.0_sreal) then

               costice=costice+l2_input_2dice_primary%costjm(i,j)

            endif
            costice_store=costice

            penaltyflag_w=0
            penaltyflag_i=0
 

            !As a reminder:
            ! - bits 1 to MaxStateVar: set to 1 if
            !   parameter estimated retrieval error 
            !   too large
            ! - bit MaxStateVar+1: retrieval did not
            !   converge 
            ! - bit MaxStateVar+2: retrieval solution
            !   cost too great

            !can only strictly evaluate cost if method has converged:
            if ( ibits ( l2_input_2dwat_primary%qcflag(i,j), maxstatevar+1, 1 ) &
                 .ne. 0_sint ) then  !convergence NOT reached

               costwat=maxcost
               penaltyflag_w=1

               !has converged
            else

               !but cost out of bounds
               !leave that commented out as bounds of cost is tricky and not
               ! too well understood

               if ( ibits ( l2_input_2dwat_primary%qcflag(i,j), maxstatevar+2, 1 ) &
                    .ne. 0_sint ) then  !cost NOT within bounds

                  ! costwat=maxcost
                  ! penaltyflag_w=1

               endif

            endif

            if ( ibits ( l2_input_2dice_primary%qcflag(i,j), maxstatevar+1 , 1 ) &
                 .ne. 0_sint ) then  !convergence NOT reached

               costice=maxcost
               penaltyflag_i=1

               !has converged
            else

               !but cost out of bounds
               !leave that commented out as bounds of cost is tricky and not
               ! too well understood
               if ( ibits ( l2_input_2dice_primary%qcflag(i,j), maxstatevar+2, 1 ) &
                    .ne. 0_sint ) then  !cost NOT within bounds

                  ! costice=maxcost
                  ! penaltyflag_i=1

               endif

            endif

            !Check physical soundness of result and re-set phase if necessary     

            !check if during day
            if(l2_input_2dice_primary%illum(i,j) .eq. 1_byte .or.&
                 & l2_input_2dice_primary%illum(i,j) .eq. 4_byte .or. &
                 & l2_input_2dice_primary%illum(i,j) .eq. 5_byte .or.&
                 & l2_input_2dice_primary%illum(i,j) .eq. 6_byte .or. &
                 & l2_input_2dice_primary%illum(i,j) .eq. 7_byte .or.&
                 & l2_input_2dice_primary%illum(i,j) .eq. 8_byte .or. &
                 & l2_input_2dice_primary%illum(i,j) .eq. 9_byte  ) then

               !if droplets are out of reasonable water range give water a high
               ! cost as penalty
               if(l2_input_2dwat_primary%ref(i,j) .lt. minre(1) .or. &
                    & l2_input_2dwat_primary%ref(i,j) .gt. maxre(1) .or.&
                    & l2_input_2dwat_primary%cot(i,j) .lt. minod(1) .or. &
                    & l2_input_2dwat_primary%cot(i,j)  .gt. maxod(1)) then
                  penaltyflag_w=1
                  costwat=maxcost
               endif

               !if crystals are out of reasonable ice range give ice a high cost as penalty
               if(l2_input_2dice_primary%ref(i,j) .lt. minre(2) .or. &
                    & l2_input_2dice_primary%ref(i,j) .gt. maxre(2) .or.&
                    & l2_input_2dice_primary%cot(i,j) .lt. minod(2) .or. &
                    & l2_input_2dice_primary%cot(i,j)  .gt. maxod(2)) then 
                  penaltyflag_i=1
                  costice=maxcost
               endif

               ! if missing IR channel then definitely ice
               if(l2_input_2dice_primary%illum(i,j) .eq. 9_byte) then
                  penaltyflag_w=1
                  costwat=maxcost
               endif

            endif ! end of illumination if

            !MJ ORG:
            ! if (costwat .lt. costice*costfactor .or. costwat .lt. ny*costfactor .and. &
            ! & l2_input_2dwat_primary%ctt(i,j) .gt. 273.) then
            ! costice=maxcost
            ! endif

            !make sure no ice above 0 deg. celcius
            if(l2_input_2dice_primary%ctt(i,j) .gt. 273.) then 
               penaltyflag_i=1
               costice=maxcost
               !as there can be supercooled water no need to test the complementary
               !and above 0 deg celcius this has to be water now
               penaltyflag_w=0
               costwat=costwat_store
            endif

            !set phase to unknown/clear when both phases got penalties
            if(penaltyflag_w .eq. 1 .and. penaltyflag_i .eq. 1 ) then
               l2_input_2dice_primary%phase(i,j)=IPhaseClU
               !look where uncertainties are larger (sum of other bits) and decide then?
               !write(*,*) 'sumbits',bit_size(l2_input_2dice_primary%qcflag(i,j))
               !write(*,*) 'LIQ',(btest(l2_input_2dwat_primary%qcflag(i,j)
               !!,ibit),ibit=1,bit_size(l2_input_2dwat_primary%qcflag(i,j)))
               !write(*,*) 'ICE',(btest(l2_input_2dice_primary%qcflag(i,j)
               !!,ibit),ibit=1,bit_size(l2_input_2dice_primary%qcflag(i,j)))
            endif

            !Now select the phase with the smallest cost
            !overwrite ICE structure entry if WAT has smaller cost

            !water has the upper hand
            if(costwat .le.  costice) then

               phaseflag(i,j) = 1_byte

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

            !Set dynamic stemp-ctt threshold for high/middle/low clouds (not
            ! used at the moment)
            !MJ write(*,*) 'ctp_high_middle and kinds',&
            !MJ & ctp_high,ctp_middle,kind(ctp_high),kind(ctp_middle)
            !high clouds
            if( l2_input_2dice_primary%ctp(i,j) .le. ctp_high ) &
                 temp_thres=temp_thres_h
            !middle
            if( l2_input_2dice_primary%ctp(i,j) .gt. ctp_high .and.&
                 l2_input_2dice_primary%ctp(i,j) .le. ctp_middle ) &
                 temp_thres=temp_thres_m
            !low 
            if( l2_input_2dice_primary%ctp(i,j) .gt. ctp_middle ) &
                 temp_thres=temp_thres_l

            !set everything to "cloud free" as default
            l2_input_2dice_primary%cc_total(i,j)=0.0

            !now assign cloud flag by cloud mask

            !compute difference between surface temperature and ctt
            if(l2_input_2dice_primary%stemp(i,j) .ge. filter_micro .and. &
                 l2_input_2dice_primary%ctt(i,j) .ge. filter_micro) then
               tempdiff=(l2_input_2dice_primary%stemp(i,j)-l2_input_2dice_primary%ctt(i,j))
            else
               tempdiff=real_fill_value
            endif

            ! DAY cloud mask
            if(l2_input_2dice_primary%illum(i,j) .eq. 1_byte .or.&
                 l2_input_2dice_primary%illum(i,j) .eq. 4_byte .or. &
                 l2_input_2dice_primary%illum(i,j) .eq. 5_byte .or.&
                 l2_input_2dice_primary%illum(i,j) .eq. 6_byte .or. &
                 l2_input_2dice_primary%illum(i,j) .eq. 7_byte .or.&
                 l2_input_2dice_primary%illum(i,j) .eq. 8_byte .or. &
                 l2_input_2dice_primary%illum(i,j) .eq. 9_byte ) then

               !write(*,*) 'Processing NN_CM for DAY' 
               nneurons=nneurons_ex1 !set number of neurons
               ninput=ninput_ex1 !set number of "criteria" aka input for the neural network
               noutput=1 !this may be obsolete
               allocate(minmax_train(ninput,2))
               minmax_train=minmax_train_ex1 !ranges variables within training was performed
               allocate(inv(ninput+1,nneurons))
               inv=inv_ex1 !"weights" for input
               allocate(outv(nneurons+1))
               outv=outv_ex1 !"weights" for output
               allocate(input(ninput+1)) !input aka "criteria"
               input(1)=l2_input_2dice_primary%ctp(i,j)
               input(2)=l2_input_2dice_primary%ctt(i,j)
               input(3)=l2_input_2dice_primary%cot(i,j)
               input(4)=l2_input_2dice_primary%stemp(i,j)
               input(5)=tempdiff
               dummyreal=real(l2_input_2dice_primary%lsflag(i,j),kind=sreal)
               input(6)=dummyreal
               allocate(scales(ninput,2))
               scales=scales_ex1 !parameters to scale input?
               oscales=oscales_ex1 !parameters to scale output?
               temperature=temperature_ex1 
               cutoff=cutoff_ex1
               bias_i=bias_i_ex1 
               bias_h=bias_h_ex1 
            else
               ! NIGHT cloud mask
               !write(*,*) 'Processing NN_CM for NIGHT/TWILIGHT'
               nneurons=nneurons_ex2 !set number of neurons
               ninput=ninput_ex2 !set number of "criteria" aka input for the neural network
               noutput=1 !this may be obsolete
               allocate(minmax_train(ninput,2))
               minmax_train=minmax_train_ex2 !ranges variables within training was performed
               allocate(inv(ninput+1,nneurons))
               inv=inv_ex2 !"weights" for input
               allocate(outv(nneurons+1))
               outv=outv_ex2 !"weights" for output
               allocate(input(ninput+1)) !input aka "criteria"
               input(1)=l2_input_2dice_primary%ctp(i,j)
               input(2)=l2_input_2dice_primary%ctt(i,j)
               input(3)=l2_input_2dice_primary%stemp(i,j)
               input(4)=tempdiff
               input(5)=real(l2_input_2dice_primary%lsflag(i,j),kind=sreal)
               allocate(scales(ninput,2))
               scales=scales_ex2 !parameters to scale input?
               oscales=oscales_ex2 !parameters to scale output?
               temperature=temperature_ex2 !"temperature"
               cutoff=cutoff_ex2
               bias_i=bias_i_ex2 !"bias"
               bias_h=bias_h_ex2 !"bias"
            endif

            !the subroutine which carries out neural network computation
            call neural_net(nneurons,ninput,noutput,minmax_train,inv,outv,input, &
                 scales,oscales,cutoff,bias_i,bias_h,temperature,output,noob)
            !store output (0,1) in structure for future evaluation and output to netcdf
            l2_input_2dice_primary%cccot(i,j)=output

            ! deallocate variables
            deallocate(minmax_train)
            deallocate(inv)
            deallocate(outv)
            deallocate(input)
            deallocate(scales)

            !now use nn output together with threshold to map to cloudmask
            !sea
            if(l2_input_2dice_primary%lsflag(i,j) .eq. 0_byte) then
               if(l2_input_2dice_primary%cccot(i,j) .gt. cot_thres1) then
                  l2_input_2dice_primary%cc_total(i,j)=1.0
               else
                  l2_input_2dice_primary%cc_total(i,j)=0.0
               endif
               !land
            elseif(l2_input_2dice_primary%lsflag(i,j) .eq. 1_byte) then
               if(l2_input_2dice_primary%cccot(i,j) .gt. cot_thres2) then
                  l2_input_2dice_primary%cc_total(i,j)=1.0
               else
                  l2_input_2dice_primary%cc_total(i,j)=0.0
               endif
            endif

            ! What are we doing if at least 1 input parameter is not within trained
            ! range, e.g. is a fillvalue ?
            ! For now 5 cases are defined to deal with it, choose best one later
            ! noob equals 1 if one or more input parameter is not within trained range      
            if (noob .eq. 1_lint) then
               ! Case 1) trust the ann and ...
               !just do nothing
               ! Case 2) set it to clear
               !l2_input_2dice_primary%cccot(i,j)= real_fill_value
               !l2_input_2dice_primary%cc_total(i,j)=0.0
               ! Case 3) set it to cloudy
               !l2_input_2dice_primary%cccot(i,j)= 1.0
               !l2_input_2dice_primary%cc_total(i,j)=1.0
               ! Case 4) set it to fillvalue
               !l2_input_2dice_primary%cccot(i,j)=real_fill_value
               !l2_input_2dice_primary%cc_total(i,j)=nint_fill_value
               ! Case 5) trust ann, set cldflag to fillvalue only if all channels are       
               !  below 0. (=fillvalue)
               if (l2_input_2dice_primary%ctp(i,j) .lt. 0 &
                    .and. l2_input_2dice_primary%stemp(i,j) .lt. 0 ) &
                    l2_input_2dice_primary%cc_total(i,j) = nint_fill_value
            endif

            !
            !now apply clear snow ice identification
            !

            call snow_ice_mask(l2_input_2dice_primary,l2_input_2d_secondary&
                 &,snow_ice_flag,inst,i,j)
!write(*,*)'snow_ice_flag',snow_ice_flag,l2_input_2dice_primary%cc_total(i,j)
            if (snow_ice_flag .eq. 1) then
               ! this pixel is actually clear not cloud
               l2_input_2dice_primary%cc_total(i,j)=0.0
! write(*,*)'after snow_ice_flag',snow_ice_flag,l2_input_2dice_primary%cc_total(i,j)
           endif

            !sstapelb changed to log10precision 
            !write(*,*) 'log tau',l2_input_2dice_primary%cot(i,j)
            !MJ rearranges below lines:
            !if tau too high, set to max value not to fill value
            if(l2_input_2dice_primary%cot(i,j) .ge. dither) then
               if(l2_input_2dice_primary%cot_uncertainty(i,j) .le. log10precision) then
                  newcot=-999.0
               elseif(l2_input_2dice_primary%cot_uncertainty(i,j)&
                    &+log(l2_input_2dice_primary%cot(i,j)) .gt. &
                    & log10huge_value) then
                  newcot=320.0
               else
                  !newcot=10.0**(l2_input_2dice_primary%cot_uncertainty(i,j))
                  !*log(10.0)*l2_input_2dice_primary%cot(i,j)
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
               !           l2_input_2dice_primary%cot(i,j)=real_fill_value
               !           l2_input_2dice_primary%ref(i,j)=real_fill_value
               !           l2_input_2dice_primary%ctp(i,j)=real_fill_value
               !           l2_input_2dice_primary%cth(i,j)=real_fill_value
               !           l2_input_2dice_primary%ctt(i,j)=real_fill_value
               !           l2_input_2dice_primary%cwp(i,j)=real_fill_value
               !           l2_input_2dice_primary%cot_uncertainty(i,j)=real_fill_value
               !           l2_input_2dice_primary%ref_uncertainty(i,j)=real_fill_value
               !           l2_input_2dice_primary%ctp_uncertainty(i,j)=real_fill_value
               !           l2_input_2dice_primary%cwp_uncertainty(i,j)=real_fill_value
               !set phase to clear/unknown
               l2_input_2dice_primary%phase(i,j)=IPhaseClU
            endif

         enddo
      enddo
      !OMP END PARALLEL DO
      noob_total=noob_total+noob

      !deallocate the water structure
      iphase=1
 
      call unset_l2_input_struct_2d_primary_wat(iphase,l2_input_2dwat_primary)
      call unset_l2_input_struct_2d_secondary(l2_input_2d_secondary)
 
      !now write things out
      !open the netcdf output file
      call nc_create_global_l2_pp(trim(adjustl(L2_primary_outputpath_and_file)),&
           & ncid_primary,  ixstop-ixstart+1, iystop-iystart+1, dims_var, &
	   wo,1,global_atts,source_atts,status)
      write(*,*) 'nn'
      !allocate the structure which hold the output in its final form
      call alloc_spixel_scan_out_pp( ixstart,ixstop,iystart,iystop,nviews,spixel_scan_out)

      !define scales,ranges etc.
#include "def_vars_primary_pp.inc"
      write(*,*) 'oo'
      !put results in final output arrays with final datatypes
      do j=iystart,iystop
         do i=ixstart,ixstop
#include "prepare_primary_pp.inc"
         enddo
      enddo

      !deallocate primary file ice input
      iphase=2
      call unset_l2_input_struct_2d_primary_ice(iphase,l2_input_2dice_primary)
      write(*,*) 'pp'
      !now write everything in one big chunk of data to disk
#include "write_primary_pp.inc"
      write(*,*) 'qq'
      !deallocate output structure
      call dealloc_spixel_scan_out_pp(spixel_scan_out)
      write(*,*) 'rr'
      !close output file
      call nc_close_pp(ncid_primary,trim(adjustl(L2_primary_outputpath_and_file)),wo)
      write(*,*) 'ss'
      !Also work on the secondary file.
      !These things are only placeholders at the moment!
      if(lsec_flag .eq. 1) then
         !     
         !     !these can be read in from the file itself later on
         !     l2_input_2dice_refl_bt%nchannels_sw=3
         !     l2_input_2dice_refl_bt%nchannels_lw=2
         !     l2_input_2dice_refl_bt%nchannels=l2_input_2dice_refl_bt%nchannels_lw+l2_input_2dice_refl_bt%nchannels_sw
         !
         !     l2_input_2dwat_refl_bt%nchannels_sw=l2_input_2dice_refl_bt%nchannels_sw
         !     l2_input_2dwat_refl_bt%nchannels_lw=l2_input_2dice_refl_bt%nchannels_lw
         !     l2_input_2dwat_refl_bt%nchannels=l2_input_2dice_refl_bt%nchannels
         !
         !     !process RT variables
         !     iphase=2
         !     call read_refl_and_bt(iphase,fnameice_sec,chan_id,l2_input_2dice_refl_bt,xdim,ydim,wo,ierr)
         !     iphase=1
         !     call read_refl_and_bt(iphase,fnamewat_sec,chan_id,l2_input_2dwat_refl_bt,xdim,ydim,wo,ierr)
         !
         !
         !     !loop over pixels
         !     do j=iystart,iystop
         !        do i=ixstart,ixstop
         !        
         !           if(phaseflag(i,j) .eq. 1_byte) then
         !  
         !              l2_input_2dice_refl_bt%albedo(i,j,:)=l2_input_2dwat_refl_bt%albedo(i,j,:)
         !              l2_input_2dice_refl_bt%reflectance_residual(i,j,:)=l2_input_2dwat_refl_bt%reflectance_residual(i,j,:)
         !              l2_input_2dice_refl_bt%brightness_temperature_residual(i,j,:)=&
         !                   & l2_input_2dwat_refl_bt%brightness_temperature_residual(i,j,:)
         !              l2_input_2dice_refl_bt%reflectance(i,j,:)=l2_input_2dwat_refl_bt%reflectance(i,j,:)
         !              l2_input_2dice_refl_bt%brightness_temperature(i,j,:)=l2_input_2dwat_refl_bt%brightness_temperature(i,j,:)
         !
         !           endif
         !
         !        enddo
         !     enddo
         !
         !     !deallocate water RT structure
         !     call unset_l2_refl_and_bt(l2_input_2dwat_refl_bt)
         !
         !     !now write things out
         !     !open the netcdf output file
         !     call nc_create_global_l2_pp(trim(adjustl(L2_secondary_outputpath_and_file)),&
         !          & ncid_secondary,  ixstop-ixstart+1, iystop-iystart+1, dims_var, wo,1,status)
         !
         !     !allocate the structure which hold the output in its final form
         !     call alloc_spixel_scan_out_rt_pp( ixstart,ixstop,iystart,iystop,l2_input_2dice_refl_bt,spixel_scan_out_rt)
         !
         !     !define scales,ranges etc.
         !#include "def_vars_rt_pp.inc"
         !     
         !     !put results in final output arrays with final datatypes
         !     do j=iystart,iystop
         !        do i=ixstart,ixstop
         !#include "prepare_rt_pp.inc"
         !        enddo
         !     enddo
         !
         !     !deallocate ice RT structure
         !     call unset_l2_refl_and_bt(l2_input_2dice_refl_bt)
         !     
         !     !now write everything in one big chunk of data to disk
         !     !include "write_rt_pp.inc.f90"
         !     
         !     !deallocate output structure
         !     call dealloc_spixel_scan_out_rt_pp(spixel_scan_out_rt)
         !
         !     !close output file
         !     call nc_close_pp(ncid_secondary,trim(adjustl(L2_secondary_outputpath_and_file)),wo)
         !     
      endif

      !deallocate phaseflag
      deallocate(phaseflag)

#ifdef WRAPPER
    end subroutine post_process_level2
#else
  end program post_process_level2
#endif




