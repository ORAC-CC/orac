!-------------------------------------------------------------------------------
! Name:
! post_process_level2
!
! Purpose:
! Read in level2 ORAC output and apply postprocessing to select cloudmask
! and phase.
!
! Description and Algorithm details:
!
! select min cost for each pixel after applying ranges
!
!    ci=cost(*,ip)
!    ri=cre(*,ip)
!    oi=lopd(*,ip)
!    wh=where(ri lt rmin or ri gt rmax or oi lt omin or oi gt omax,nw)
!    if nw gt 0 then ci(wh)=1d9
!
! deselect ice cloud if cost similar to liquid and TC is high
!
!    if nl eq 1 and ni eq 1 then
!       if ci(il) lt ci(ii)*1.5 or ci(il) lt ny*1.5 and tc(ip) gt 273 then
!          ci(ii)=1e9
!    end if
!
! deselect ash if cost similar to liquid or ice or very low altitude
!
!    if nv eq 1 and ni eq 1 then if ci(ii) lt ci(iv)*1.1 then ci(iv)=1e9
!    if nv eq 1 and nl eq 1 then if ci(il) lt ci(iv)*1.1 then ci(iv)=1e9
!    iphase(ip)=(where(ci eq min(ci)))(0)
!
!    cloud mask selected if retrieval has converged and if optical depth >
!       cot_thres
!
! Arguments:
! Name Type In/Out/Both Description
!
! History:
! 09/02/2012, CP: Original version. Needs modifying to work with AATSR/AVHRR
!    does not output covariance information into secondary output file.  Have
!    not included cloud mask at present.
! 2012/03/18, CP: modified to add cloud flag
! 2012/07/06, MJ: extensively overhauls and restructures the code
! 2012/07/13, MJ: extends range of valid ref for netcdf write
! 2012/07/23, MJ: adds choice of strictness of cloud checks.
! 2012/07/23, MJ: adds code to play with cloud screening during day and night
! 2012/08/27, MJ: better implements time variable in output.
! 2012/11 to 2013/01, SS, MJ and MS: make major changes to cloud masking
!    approach,
! 2012/11 to 2013/01, SS, MJ: implements untested OMP parallelization, changes
!    wrt CF compliance.
! 2013/01/17, MJ: Adds code to accommodate uncertainties of ctt and cth
! 2014/01/22, MJ: fixes FP overflow with COT uncertainty.
! 2014/01/23, MJ: changes of how COT uncertainty is treated.
! 2014/02/07, MJ: corrects logical error (water got penalty when ice prop.
! were out of range and vice versa),cleans up code
! 2014/02/10, MJ: reorders and rewrites the penalty assignment in phase
!    selection
! 2014/03/14, MJ: removes code to scale cot from log_10(tau) to tau as this step
!    was already introduced in ORAC.
! 2014/07/08, CP: added more illumination options
! 2014/07/08, CP: added in extra subroutine that uses Istomina like tests to
!    detect clear snow and ice
! 2014/09/29, changed wat read_inter)sec is called now need a instrument name
! 2014/10/24, OS: added subroutine option for wrapper mode, and further
!    wrapper-specific details; SteSta added new fill value option for the
!    postprocessing cloud mask
! 2014/11/20, OS: renamed module neural_net_constants to
!    neural_net_constants_postproc; some editing; replaced hard-coded NN values
!    with variables
! 2015/02/05, OS: changed nint to lint; further replacements of variables
!    defined in vartypes_pp with those defined in common_constants; added
!    phase_post; Pavolonis phase is now used as basis for
! 2015/02/06, CP: added in case of ML IR only retrieval
! 2015/02/06, CP: tidied up phase selection removed obsolete stuff
! 2015/02/06, CP: added case of IR only multi layer selection phase-dependent
!    variables
! 2015/02/07, CP: massive tidy up including of common constants
! 2015/03/19, OS: COT maximum value set to 100, CWP scaled accordingly + minor
!    editing
! 2015/04/22, OS: only apply Istomina retyping when cloud mask says cloud
! 2015/07/06, OS: added flags "one_phase_only" and "cloudy_only" as file input;
!    reclassify phase if CTT does not match phase temperature range (i.e. no ice
!    if CTT>273.15K); removed some debugging statements and further clean up
! 2015/07/10, OS: fixed bug related to using flag one_phase_only
! 2015/07/16, GM: Major cleanup.
!
! $Id$
!
! Bugs:
!    Works only with one viewing angle.
!-------------------------------------------------------------------------------

#ifndef WRAPPER
program post_process_level2
#else
subroutine post_process_level2(mytask,ntasks,lower_bound,upper_bound,path_and_file)
#endif

   use common_constants
   use global_attributes
   use netcdf
   use neural_net_constants_postproc
   use orac_ncdf
   use scanline_structure
   use source_attributes
   use structures_pp
   use vartypes_pp

   implicit none

   logical, parameter :: verbose = .true.

   integer,            parameter :: MaxNumMeas = 36

   integer(kind=byte), parameter :: IPhaseClU  = 0 ! clear/unknoiwn
   integer(kind=byte), parameter :: IPhaseWat  = 1 ! Water
   integer(kind=byte), parameter :: IPhaseIce  = 2 ! Ice
   integer(kind=byte), parameter :: IPhasemli  = 3 ! MLI

   integer :: i, j

   integer :: nargs
#ifdef WRAPPER
   integer :: mytask,ntasks,lower_bound,upper_bound
#endif
   integer(kind=byte)        :: mli_flag = 0

   logical                   :: one_phase_only, cloudy_only
   real                      :: cot_thres, cot_thres1, cot_thres2
   integer                   :: proc_flag(5)
   character(len=var_length) :: inst

   character(len=path_length) :: path_and_file, &
                                 fnamewat_prim, fnameice_prim, &
                                 fnamewat_sec,fnameice_sec, &
                                 fnamemli_sec,fnamemli_prim

   character(len=path_length) :: L2_primary_outputpath_and_file, &
                                 L2_secondary_outputpath_and_file

   type(global_attributes_s)  :: global_atts
   type(source_attributes_s)  :: source_atts

   integer :: ncid_primary, ncid_secondary, dims_var(2), varid
   character(len=32)         :: input_num
   character(len=var_length) :: varname

   type(l2_input_struct_2d_primary)       :: l2_input_2dwat_primary, &
                                             l2_input_2dice_primary, &
                                             l2_input_2dmli_primary
   type(l2_input_struct_2d_secondary)     :: l2_input_2dwat_secondary, &
                                             l2_input_2dice_secondary
   type(spixel_scanline_primary_output)   :: spixel_scan_out
   type(spixel_scanline_secondary_output) :: spixel_scan_out_sec

   type(counts_and_indexes)               :: indexing

   integer (kind=lint) :: xdim,ydim
   integer(kind=lint)  :: ixstart,ixstop,iystart,iystop

   integer(kind=byte)  :: lsec_flag,lstrict

   integer(kind=byte), allocatable, dimension(:,:) :: phaseflag

   real(kind=sreal)    :: newcot
   real(kind=sreal)    :: precision,huge_value,log10huge_value,log10precision


   precision=tiny(1.0_sreal)
   huge_value=huge(1.0_sreal)
   log10huge_value=log10(huge_value)
   ! include a log10precision value for COT + Uncertainty otherwise no optical
   ! thickness value below 1 will be written
   log10precision=log10(precision)

#ifndef WRAPPER
      nargs = COMMAND_ARGUMENT_COUNT()
#else
      nargs=-1
#endif

   ! if no argument was given then read standard file
   if (nargs .eq. 0 ) then
      call get_environment_variable("ORAC_TEXTIN",path_and_file)
   else if (nargs .eq. 1) then
      call get_command_argument(1,path_and_file)
   else if (nargs .eq. -1) then
      write(*,*) 'inside postproc ',trim(adjustl(path_and_file))
   end if

   write(*,*)'path_and_file = ', trim(path_and_file)

   ! read from an external file
   open(11,file=trim(adjustl(path_and_file)), status='old', form='formatted')

   read(11,*) fnamewat_prim
   write(*,*) 'primary water input = ', trim(fnamewat_prim)
   read(11,*) fnameice_prim
   write(*,*) 'primary ice input = ', trim(fnameice_prim)
   if ( mli_flag .eq. 1) then
      read(11,*) fnamemli_prim
      write(*,*) 'primary mli input = ', trim(fnamemli_prim)
   end if

   read(11,*) fnamewat_sec
   write(*,*) 'secondary water input = ', trim(fnamewat_sec)
   read(11,*) fnameice_sec
   write(*,*) 'secondary ice input = ', trim(fnameice_sec)
   if ( mli_flag .eq. 1) then
      read(11,*) fnamemli_sec
      write(*,*) 'secondary mli input = ', trim(fnamemli_sec)
   end if

   read(11,*) L2_primary_outputpath_and_file
   write(*,*) 'primary = output', trim(L2_primary_outputpath_and_file)
   read(11,*) L2_secondary_outputpath_and_file
   write(*,*) 'secondary output = ', trim(L2_secondary_outputpath_and_file)
   read(11,*) one_phase_only
   write(*,*) 'one_phase_only = ', one_phase_only
   read(11,*) cloudy_only
   write(*,*) 'cloudy_only = ', cloudy_only
   read(11,*) cot_thres
   write(*,*) 'cot_thres = ', cot_thres
   read(11,*) cot_thres1
   write(*,*) 'cot_thres1 = ', cot_thres1
   read(11,*) cot_thres2
   write(*,*) 'cot_thres2 = ', cot_thres2
   read(11,*) proc_flag
   write(*,*)'proc = ', proc_flag
   read(11,*) inst
   write(*,*) 'insts = ', inst
   read(11,*) lsec_flag
   write(*,*) 'lsec = ', lsec_flag
   read(11,*) lstrict


   ! Here we have a general hack to get some channel indexing information from
   ! the secondary measurment variable names.

   write(*,*) 'Obtaining channel indexing'

   indexing%Nx = 4 ! This count is hardwired

   indexing%NViews = 1

   write(*,*) 'Opening ive secondary input file: ', trim(fnameice_sec)
   call nc_open(ncid_secondary,fnameice_sec)

   indexing%Ny = 0
   indexing%NSolar = 0
   indexing%NThermal = 0

   allocate(indexing%Y_Id(MaxNumMeas))
   allocate(indexing%Ch_Is(MaxNumMeas))
   indexing%Ch_Is = 0

   do i = 1, MaxNumMeas
      write(input_num, "(i4)") i
      varname = 'reflectance_in_channel_no_'//trim(adjustl(input_num))
      if (nf90_inq_varid(ncid_secondary,varname,varid) .eq. NF90_NOERR) then
              indexing%Ny = indexing%Ny + 1
              indexing%NSolar = indexing%NSolar + 1
              indexing%Y_Id(indexing%Ny) = i
              indexing%Ch_Is(indexing%Ny) = &
                 ibset(indexing%Ch_Is(indexing%Ny), SolarBit)
      else
         write(input_num, "(i4)") i
         varname = 'brightness_temperature_in_channel_no_'//trim(adjustl(input_num))
         if (nf90_inq_varid(ncid_secondary,varname,varid) .eq. NF90_NOERR) then
              indexing%Ny = indexing%Ny + 1
              indexing%NThermal = indexing%NThermal + 1
              indexing%Y_Id(indexing%Ny) = i
              indexing%Ch_Is(indexing%Ny) = &
                 ibset(indexing%Ch_Is(indexing%Ny), ThermalBit)
         end if
      end if
   end do
   write(*,*) 'Closing secondary input file.'
   if (nf90_close(ncid_secondary) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_close()'
      stop error_stop_code
   end if


   ! read ice and water intermediate files and put into a structure

   write(*,*) 'Reading input dimensions'
   call read_input_dimensions(fnameice_prim,xdim,ydim,verbose)

   write(*,*) '***** ICE *****'
   write(*,*) 'read ice primary'
   call set_l2_input_struct_2d_primary_all(l2_input_2dice_primary,xdim,ydim, &
      indexing)
   call read_inter_file_all(fnameice_prim,l2_input_2dice_primary,xdim,ydim, &
      indexing,global_atts,source_atts,verbose)
   if (L2_secondary_outputpath_and_file .ne. '') then
      write(*,*) 'read ice secondary'
      call set_l2_input_struct_2d_secondary_all(l2_input_2dice_secondary,xdim,ydim, &
         indexing)
      call read_inter_sec_file_all(fnameice_sec,l2_input_2dice_secondary,xdim,ydim, &
         indexing,verbose)
   endif

   write(*,*) '***** WAT *****'
   write(*,*) 'read wat primary'
   call set_l2_input_struct_2d_primary_class(l2_input_2dwat_primary,xdim,ydim, &
      indexing)
   call read_inter_file_class(fnamewat_prim,l2_input_2dwat_primary,xdim,ydim, &
      indexing,global_atts,source_atts,verbose)
   if (L2_secondary_outputpath_and_file .ne. '') then
      write(*,*) 'read wat secondary'
      call set_l2_input_struct_2d_secondary_class(l2_input_2dwat_secondary,xdim,ydim, &
         indexing)
      call read_inter_sec_file_class(fnamewat_sec,l2_input_2dwat_secondary,xdim,ydim, &
         indexing,verbose)
   endif

   mli_flag=0
   if (mli_flag .gt. 0) then
      write(*,*) '***** MLI *****'
      write(*,*) 'read mli primary'
      call set_l2_input_struct_2d_primary_class(l2_input_2dmli_primary,xdim,ydim)
      call read_inter_file_class(fnamemli_prim,l2_input_2dmli_primary,xdim,ydim, &
         global_atts,source_atts,verbose)
   end if

   ! set the loop bounds
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
         if (l2_input_2dice_primary%cldtype(i,j) .gt. 1 .and. &
             l2_input_2dice_primary%cldtype(i,j) .lt. 5 ) then

            phaseflag(i,j) = 1_byte
            if ((.not. one_phase_only) .and. & ! only reclassify if both phases were processed
               (((l2_input_2dwat_primary%ctt(i,j) .lt. 233.16) .and. &
                 (l2_input_2dwat_primary%ctt(i,j) .ne. sreal_fill_value)) .and. &
                ((l2_input_2dice_primary%ctt(i,j) .lt. 273.16 ) .and. &
                 (l2_input_2dice_primary%ctt(i,j) .ne. sreal_fill_value)))) &
               phaseflag(i,j) = 2_byte
            else
               phaseflag(i,j) = 2_byte
               if ((.not. one_phase_only ) .and. & ! only reclassify if both phases were processed
                   ((l2_input_2dice_primary%ctt(i,j) .ge. 273.16) .and. &
                    (l2_input_2dwat_primary%ctt(i,j) .ge. 233.16))) &
               phaseflag(i,j) = 1_byte
            end if

            if (phaseflag(i,j) .eq. 1_byte) then

               ! primary file
               l2_input_2dice_primary%cot(i,j) = &
                     l2_input_2dwat_primary%cot(i,j)
               l2_input_2dice_primary%ref(i,j) = &
                     l2_input_2dwat_primary%ref(i,j)
               l2_input_2dice_primary%ctp(i,j) = &
                     l2_input_2dwat_primary%ctp(i,j)
               l2_input_2dice_primary%stemp(i,j) = &
                     l2_input_2dwat_primary%stemp(i,j)
               l2_input_2dice_primary%cth(i,j) = &
                     l2_input_2dwat_primary%cth(i,j)
               l2_input_2dice_primary%cth_uncertainty(i,j) = &
                     l2_input_2dwat_primary%cth_uncertainty(i,j)
               l2_input_2dice_primary%ctt(i,j) = &
                     l2_input_2dwat_primary%ctt(i,j)
               l2_input_2dice_primary%ctt_uncertainty(i,j) = &
                     l2_input_2dwat_primary%ctt_uncertainty(i,j)
               l2_input_2dice_primary%cwp(i,j) = &
                     l2_input_2dwat_primary%cwp(i,j)
               l2_input_2dice_primary%cloud_albedo(i,j,:) = &
                     l2_input_2dwat_primary%cloud_albedo(i,j,:)

               l2_input_2dice_primary%cot_uncertainty(i,j) = &
                     l2_input_2dwat_primary%cot_uncertainty(i,j)
               l2_input_2dice_primary%ref_uncertainty(i,j) = &
                     l2_input_2dwat_primary%ref_uncertainty(i,j)
               l2_input_2dice_primary%ctp_uncertainty(i,j) = &
                     l2_input_2dwat_primary%ctp_uncertainty(i,j)
               l2_input_2dice_primary%cc_total_uncertainty(i,j) = &
                     l2_input_2dwat_primary%cc_total_uncertainty(i,j)
               l2_input_2dice_primary%stemp_uncertainty(i,j) = &
                     l2_input_2dwat_primary%stemp_uncertainty(i,j)
               l2_input_2dice_primary%cwp_uncertainty(i,j) = &
                     l2_input_2dwat_primary%cwp_uncertainty(i,j)

               l2_input_2dice_primary%convergence(i,j) = &
                     l2_input_2dwat_primary%convergence(i,j)

               l2_input_2dice_primary%niter(i,j) = &
                     l2_input_2dwat_primary%niter(i,j)

               l2_input_2dice_primary%phase(i,j) = &
                     iphasewat

               l2_input_2dice_primary%costja(i,j) = &
                     l2_input_2dwat_primary%costja(i,j)
               l2_input_2dice_primary%costjm(i,j) = &
                     l2_input_2dwat_primary%costjm(i,j)

               l2_input_2dice_primary%qcflag(i,j) = &
                     l2_input_2dwat_primary%qcflag(i,j)

               ! secondary file
               if (L2_secondary_outputpath_and_file .ne. '') then
                  l2_input_2dice_secondary%ctp_ap(i,j) = &
                        l2_input_2dwat_secondary%ctp_ap(i,j)
                  l2_input_2dice_secondary%ctp_fg(i,j) = &
                        l2_input_2dwat_secondary%ctp_fg(i,j)
                  l2_input_2dice_secondary%ref_ap(i,j) = &
                        l2_input_2dwat_secondary%ref_ap(i,j)
                  l2_input_2dice_secondary%ref_fg(i,j) = &
                        l2_input_2dwat_secondary%ref_fg(i,j)
                  l2_input_2dice_secondary%cot_ap(i,j) = &
                        l2_input_2dwat_secondary%cot_ap(i,j)
                  l2_input_2dice_secondary%cot_fg(i,j) = &
                        l2_input_2dwat_secondary%cot_fg(i,j)
                  l2_input_2dice_secondary%stemp_ap(i,j) = &
                        l2_input_2dwat_secondary%stemp_ap(i,j)
                  l2_input_2dice_secondary%stemp_fg(i,j) = &
                        l2_input_2dwat_secondary%stemp_fg(i,j)

                  l2_input_2dice_secondary%y0(i,j,:) = &
                        l2_input_2dwat_secondary%y0(i,j,:)

                  l2_input_2dice_secondary%residuals(i,j,:) = &
                        l2_input_2dwat_secondary%residuals(i,j,:)
               end if
            end if

            l2_input_2dice_primary%cc_total(i,j) = &
               l2_input_2dice_primary%cldmask(i,j)
!           l2_input_2dice_primary%cc_total_uncertainty(i,j) = &
!              l2_input_2dice_primary%cldmask_uncertainty(i,j)

            ! if tau too high, set to max value not to fill value
            if (l2_input_2dice_primary%cot(i,j) .ge. dither) then
               if (l2_input_2dice_primary%cot_uncertainty(i,j) .le. &
                   log10precision) then
                  newcot=-999.0
               else if(l2_input_2dice_primary%cot_uncertainty(i,j) &
                  +log(l2_input_2dice_primary%cot(i,j)) .gt. &
                  log10huge_value) then
                  newcot=320.0
               else
                  newcot=10.0**(l2_input_2dice_primary%cot_uncertainty(i,j)) &
                     *l2_input_2dice_primary%cot(i,j)
               end if
            else
               newcot=-999.0
            end if
            l2_input_2dice_primary%cot_uncertainty(i,j)=newcot

            ! Don't set fill values and leave final masking of products to user
            ! obsolete: if cloud free, set primary retrieval parameters to fill value
            if(l2_input_2dice_primary%cc_total(i,j) .eq. 0.0) then
               ! set phase to clear/unknown
               l2_input_2dice_primary%phase(i,j)=IPhaseClU
               l2_input_2dice_primary%phase_post(i,j)=IPhaseClU
            end if
         end do
      end do

      ! deallocate the water structure

      call unset_l2_input_struct_2d_primary_class(l2_input_2dwat_primary)
      if (L2_secondary_outputpath_and_file .ne. '') then
         call unset_l2_input_struct_2d_secondary_class(l2_input_2dwat_secondary)
      endif

      ! now write things out

      ! open the netcdf output file
      call nc_create(trim(adjustl(L2_primary_outputpath_and_file)), ncid_primary, &
         ixstop-ixstart+1, iystop-iystart+1, dims_var, 1, global_atts, source_atts)
      if (L2_secondary_outputpath_and_file .ne. '') then
         call nc_create(trim(adjustl(L2_secondary_outputpath_and_file)), ncid_secondary, &
            ixstop-ixstart+1, iystop-iystart+1, dims_var, 2, global_atts, source_atts)
      end if

      ! allocate the structures which hold the output in its final form
      call alloc_spixel_scan_out_pp(ixstart, ixstop, iystart, iystop, indexing, &
         spixel_scan_out)
      if (L2_secondary_outputpath_and_file .ne. '') then
         call alloc_spixel_scan_out_sec_pp(ixstart, ixstop, iystart, iystop, indexing, &
            spixel_scan_out_sec)
      end if

      ! define variables
         call def_vars_primary_pp(ncid_primary, indexing, dims_var,  &
            spixel_scan_out, global_atts, verbose)
      if (L2_secondary_outputpath_and_file .ne. '') then
         call def_vars_secondary_pp(ncid_secondary, indexing, dims_var, &
            spixel_scan_out_sec, global_atts, verbose)
      end if

      ! put results in final output arrays with final datatypes
      do j=iystart,iystop
         do i=ixstart,ixstop
            call prepare_primary_pp(i, j, indexing, l2_input_2dice_primary, &
               spixel_scan_out)
            if (L2_secondary_outputpath_and_file .ne. '') then
               call prepare_secondary_pp(i, j, indexing, l2_input_2dice_secondary, &
                  spixel_scan_out_sec)
            end if
         end do
      end do

      ! deallocate primary file ice input
      call unset_l2_input_struct_2d_primary_all(l2_input_2dice_primary)
      if (L2_secondary_outputpath_and_file .ne. '') then
         call unset_l2_input_struct_2d_secondary_all(l2_input_2dice_secondary)
      endif

      ! now write everything in one big chunk of data to disk
      call write_primary_pp(ncid_primary, ixstart, ixstop, iystart, iystop, &
         indexing, spixel_scan_out, global_atts)
      if (L2_secondary_outputpath_and_file .ne. '') then
         call write_secondary_pp(ncid_secondary, ixstart, ixstop, iystart, iystop, &
            indexing, spixel_scan_out_sec, global_atts)
      end if

      ! deallocate output structure
      call dealloc_spixel_scan_out_pp(spixel_scan_out)
      if (L2_secondary_outputpath_and_file .ne. '') then
         call dealloc_spixel_scan_out_sec_pp(spixel_scan_out_sec)
      end if


      ! close output file
      if (nf90_close(ncid_primary) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: nf90_close()'
         stop error_stop_code
      end if

      if (L2_secondary_outputpath_and_file .ne. '') then
         if (nf90_close(ncid_secondary) .ne. NF90_NOERR) then
            write(*,*) 'ERROR: nf90_close()'
            stop error_stop_code
         end if
      end if


      deallocate(phaseflag)

      deallocate(indexing%Y_Id)
      deallocate(indexing%Ch_Is)

#ifdef WRAPPER
   end subroutine post_process_level2
#else
   end program post_process_level2
#endif
