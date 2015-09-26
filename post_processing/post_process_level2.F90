!-------------------------------------------------------------------------------
! Name:
! post_process_level2
!
! Purpose:
! Read in level2 ORAC output and apply post processing to select cloudmask
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
!    post processing cloud mask
! 2014/11/20, OS: renamed module neural_net_constants to
!    neural_net_constants_postproc; some editing; replaced hard-coded NN values
!    with variables
! 2015/02/05, OS: changed nint to lint; further replacements of variables
!    defined in postproc_constants with those defined in common_constants; added
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
! 2015/09/06, GM: Adapt to use the output routines in common/.
! 2015/09/07, GM: Propagation of COT uncertainty from log10(COT) space was being
!    being done incorrectly.  Anyway, it has been moved to the main processor
!    where it is now being done correctly.
! 2015/09/07, GM: Move the overwrite of cc_total_uncertainty by
!    cldmask_uncertainty from the main processor to here.  This overwrite is
!    Cloud CCI specific and will therefore have to be handled once aerosol and
!    ash support is added to the post processor.
! 2015/09/14, GM: Add support for optional driver file arguments.
! 2015/09/14, GM: Add use_bayesian_selection as an optional driver file argument.
!    Default is use_bayesian_selection=false.  With this option any number of
!    input primary/secondary file pairs is supported.  To support this lines
!    after the required lines that do not parse as a label=value are considered
!    a primary file name followed by a secondary file name on the next line.
! 2015/09/14, GM: Add boolean option use_netcdf_compression.
! 2015/09/26, GM: Add optional input parameters cost_thresh and norm_prob_thresh
!    (normalized probability) for Bayesian selection.  After the `best', based
!    on cost, class is determined any pixel not meeting these thresholds are set
!    to fill value.  Each threshold can be turned off individually by setting
!    them to zero.
! 2015/09/26, GM: Removed eight unused mandatory driver file lines.
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
   use orac_input
   use orac_ncdf
   use orac_output
   use parsing
   use postproc_constants
   use postproc_utils
   use prepare_output
   use source_attributes

   implicit none

   logical,            parameter :: verbose = .true.

   integer,            parameter :: MaxInFiles = 32

   integer,            parameter :: MaxNumMeas = 36

   integer(kind=byte), parameter :: IWat       = 1
   integer(kind=byte), parameter :: IIce       = 2

   integer(kind=byte), parameter :: IPhaseClU  = 0 ! clear/unknoiwn
   integer(kind=byte), parameter :: IPhaseWat  = 1 ! Water
   integer(kind=byte), parameter :: IPhaseIce  = 2 ! Ice
   integer(kind=byte), parameter :: IPhasemli  = 3 ! MLI

   integer                     :: i, j, k

   integer                     :: nargs
#ifdef WRAPPER
   integer                     :: mytask, ntasks, lower_bound, upper_bound
#endif
   character(len=path_length)  :: label, value

   logical                     :: do_secondary = .false.

   logical                     :: one_phase_only

   real                        :: cost_thresh = 0.
   real                        :: norm_prob_thresh = .75
   logical                     :: use_bayesian_selection = .false.
   logical                     :: use_netcdf_compression = .true.

   integer                     :: n_in_files

   character(len=path_length)  :: path_and_file

   character(len=path_length)  :: in_files_primary(MaxInFiles), &
                                  in_files_secondary(MaxInFiles)

   character(len=path_length)  :: out_file_primary, out_file_secondary

   integer                     :: ncid_primary, ncid_secondary, dims_var(2), &
                                  varid
   character(len=32)           :: input_num
   character(len=var_length)   :: varname

   type(global_attributes_s)   :: global_atts
   type(source_attributes_s)   :: source_atts

   type(input_data_primary)    :: input_primary(MaxInFiles)
   type(input_data_secondary)  :: input_secondary(MaxInFiles)

   type(output_data_primary)   :: output_primary
   type(output_data_secondary) :: output_secondary

   type(counts_and_indexes)    :: indexing

   integer(kind=lint)          :: xdim,ydim
   integer(kind=lint)          :: ixstart,ixstop,iystart,iystop

   integer(kind=byte)          :: phase_flag

   integer                     :: i_min_costjm
   real                        :: a_min_costjm
   real                        :: sum_prob

   integer                     :: deflate_level2
   logical                     :: shuffle_flag2
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

   read(11,*) in_files_primary(IWat)
   write(*,*) 'primary water input = ', trim(in_files_primary(IWat))
   read(11,*) in_files_primary(IIce)
   write(*,*) 'primary ice input = ', trim(in_files_primary(IIce))

   read(11,*) in_files_secondary(IWat)
   write(*,*) 'secondary water input = ', trim(in_files_secondary(IWat))
   read(11,*) in_files_secondary(IIce)
   write(*,*) 'secondary ice input = ', trim(in_files_secondary(IIce))

   read(11,*) out_file_primary
   write(*,*) 'primary output = ', trim(out_file_primary)
   read(11,*) out_file_secondary
   write(*,*) 'secondary output = ', trim(out_file_secondary)
   read(11,*) one_phase_only
   write(*,*) 'one_phase_only = ', one_phase_only

   n_in_files = 2

   if (out_file_secondary .ne. '') do_secondary = .true.

   do while (parse_driver(11, value, label) == 0)
     call clean_driver_label(label)
     select case (label)
     case('COST_THRESH')
        if (parse_string(value, cost_thresh) /= 0) &
           call handle_parse_error(label)
     case('NORM_PROB_THRESH')
        if (parse_string(value, norm_prob_thresh) /= 0) &
           call handle_parse_error(label)
     case('USE_BAYESIAN_SELECTION')
        if (parse_string(value, use_bayesian_selection) /= 0) &
           call handle_parse_error(label)
     case('USE_NETCDF_COMPRESSION')
        if (parse_string(value, use_netcdf_compression) /= 0) &
           call handle_parse_error(label)
     case('')
        n_in_files = n_in_files + 1
        in_files_primary(n_in_files) = value
        if (parse_driver(11, value, label) /= 0) then
           write(*,*) 'ERROR: Problem parsing secondary file name for ' // &
                      'input number: ', n_in_files
           stop error_stop_code
        end if
        in_files_secondary(n_in_files) = value
     case default
        write(*,*) 'ERROR: Unknown option: ', trim(label)
        stop error_stop_code
     end select
   end do


   ! Here we have a general hack to get some channel indexing information from
   ! the secondary measurment variable names.

   write(*,*) 'Obtaining channel indexing'

   indexing%Nx = 4 ! This count is hardwired for now

   indexing%NViews = 1

   write(*,*) 'Opening first secondary input file: ', trim(in_files_secondary(IIce))
   call nc_open(ncid_secondary,in_files_secondary(IIce))

   indexing%Ny = 0
   indexing%NSolar = 0
   indexing%NThermal = 0

   allocate(indexing%YSolar(MaxNumMeas))
   allocate(indexing%Y_Id(MaxNumMeas))
   allocate(indexing%Ch_Is(MaxNumMeas))
   indexing%Ch_Is = 0

   do i = 1, MaxNumMeas
      write(input_num, "(i4)") i
      varname = 'reflectance_in_channel_no_'//trim(adjustl(input_num))
      if (nf90_inq_varid(ncid_secondary,varname,varid) .eq. NF90_NOERR) then
              indexing%Ny = indexing%Ny + 1
              indexing%NSolar = indexing%NSolar + 1
              indexing%YSolar(indexing%NSolar) = indexing%Ny
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
   write(*,*) 'Closing first secondary input file.'
   if (nf90_close(ncid_secondary) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_close()'
      stop error_stop_code
   end if


   ! read intermediate input files and into a structures

   write(*,*) 'Reading input dimensions'
   call read_input_dimensions(in_files_primary(1),xdim,ydim,verbose)

   write(*,*) '********************************'
   write(*,*) 'read: ', trim(in_files_primary(1))
   call alloc_input_data_primary_all(input_primary(1),xdim,ydim,indexing)
   call read_input_primary_all(in_files_primary(1),input_primary(1),xdim, &
      ydim,indexing,global_atts,source_atts,verbose)
   if (do_secondary) then
      write(*,*) 'read: ', trim(in_files_secondary(1))
      call alloc_input_data_secondary_all(input_secondary(1),xdim,ydim, &
         indexing)
      call read_input_secondary_all(in_files_secondary(1),input_secondary(1), &
         xdim,ydim,indexing,verbose)
   end if

   do i = 2, n_in_files
      write(*,*) '********************************'
      write(*,*) 'read: ', trim(in_files_primary(i))
      call alloc_input_data_primary_class(input_primary(i),xdim,ydim,indexing)
      call read_input_primary_class(in_files_primary(i),input_primary(i),xdim, &
         ydim,indexing,global_atts,verbose)
      if (do_secondary) then
         write(*,*) 'read: ', trim(in_files_secondary(i))
         call alloc_input_data_secondary_class(input_secondary(i),xdim,ydim, &
            indexing)
         call read_input_secondary_class(in_files_secondary(i),input_secondary(i), &
            xdim,ydim,indexing,verbose)
      end if
   end do


   ! set the loop bounds
   ixstart=1
   ixstop=xdim
   iystart=1
   iystop=ydim

   write(*,*) 'Processing limits:'
   write(*,*) ixstart, ixstop
   write(*,*) iystart, iystop

   do j=iystart,iystop
      do i=ixstart,ixstop

         ! Cloud CCI selection
         if (.not. use_bayesian_selection) then
            ! default: ICE wins, meaning only ice structure is overwritten and used
            ! in the end for the output
            input_primary(IWat)%phase(i,j)=IPhaseWat

            ! Information of Pavolonis cloud type
            ! C,N,F,W,S,M,I,Ci,O

            ! Liquid cloud phase comprises the following categories:
            ! 0? Clear
            ! 2? fog
            ! 3? warm liquid water clouds, and
            ! 4? supercooled-mixed-phased clouds

            ! ? Ice cloud phase comprises the following categories:
            ! 6? opaque ice clouds/deep convection
            ! 7? nonopaque high ice clouds (e.g. cirrus)
            ! 8? cloud overlap 4 (e.g. multiple cloud layers)

            ! here apply Pavolonis phase information to select retrieval phase variables
            ! select water type overwrite ice
            phase_flag = 2_byte
            if (input_primary(IWat)%cldtype(i,j) .gt. 1 .and. &
                input_primary(IWat)%cldtype(i,j) .lt. 5) then

               phase_flag = 1_byte
               if (.not. one_phase_only .and. & ! only reclassify if both phases were processed
                   ((input_primary(IIce)%ctt(i,j) .ne. sreal_fill_value .and. &
                     input_primary(IIce)%ctt(i,j) .lt. 233.16) .and. &
                    (input_primary(IWat)%ctt(i,j) .ne. sreal_fill_value .and. &
                     input_primary(IWat)%ctt(i,j) .lt. 273.16))) &
                  phase_flag = 2_byte
            else
               phase_flag = 2_byte
               if (.not. one_phase_only .and. & ! only reclassify if both phases were processed
                   ((input_primary(IIce)%ctt(i,j) .ne. sreal_fill_value .and. &
                     input_primary(IIce)%ctt(i,j) .ge. 233.16) .and. &
                    (input_primary(IWat)%ctt(i,j) .ne. sreal_fill_value .and. &
                     input_primary(IWat)%ctt(i,j) .ge. 273.16))) &
               phase_flag = 1_byte
            end if

            if (phase_flag .eq. 2_byte) then
               call copy_class_specific_inputs(i, j, &
                  input_primary(IWat), input_primary(IIce), &
                  input_secondary(IWat), input_secondary(IIce), do_secondary)

               input_primary(IWat)%phase(i,j) = IPhaseIce
            end if

            ! Overwrite cc_total with cldmask for Cloud CCI
            input_primary(IWat)%cc_total(i,j) = &
               input_primary(IWat)%cldmask(i,j)
            input_primary(IWat)%cc_total_uncertainty(i,j) = &
               input_primary(IWat)%cldmask_uncertainty(i,j)

            if (input_primary(IWat)%cc_total(i,j) .eq. 0.0) then
               ! set phase to clear/unknown
               input_primary(IWat)%phase(i,j)=IPhaseClU
            end if

         ! Bayesian based selection
         else
            sum_prob = 0.
            a_min_costjm = huge(a_min_costjm)
            do k = 1, n_in_files
               if (input_primary(k)%costjm(i,j) .lt. a_min_costjm) then
                  a_min_costjm = input_primary(k)%costjm(i,j)
                  i_min_costjm = k
               end if
               sum_prob = sum_prob + exp(-input_primary(k)%costjm(i,j) / 2.)
            end do

            if (input_primary(i_min_costjm)%costjm(i,j) .lt. cost_thresh .or. &
                input_primary(i_min_costjm)%costjm(i,j) / sum_prob .lt. norm_prob_thresh) then
               call init_class_specific_inputs(i, j, input_primary(1), input_secondary(1), do_secondary)
            else
               if (i_min_costjm .ne. 1) then
                  call copy_class_specific_inputs(i, j, &
                     input_primary(1), input_primary(i_min_costjm), &
                     input_secondary(1), input_secondary(i_min_costjm), do_secondary)
               end if

               input_primary(1)%phase(i,j) = i_min_costjm
            endif
         end if
      end do
   end do


   ! deallocate all input structures except for the first one
   do i = 2, n_in_files
      call dealloc_input_data_primary_class(input_primary(i))
      if (do_secondary) then
         call dealloc_input_data_secondary_class(input_secondary(i))
      end if
   end do


   ! allocate the structures which hold the output in its final form
   call alloc_output_data_primary(ixstart, ixstop, iystart, iystop, indexing%NViews, indexing%Ny, output_primary, .true., .false., .false.)
   if (do_secondary) then
      call alloc_output_data_secondary(ixstart, ixstop, iystart, iystop, indexing%Ny, indexing%Nx, output_secondary, .false.)
   end if

   ! open the netcdf output file
   call nc_create(trim(adjustl(out_file_primary)), ncid_primary, &
      ixstop-ixstart+1, iystop-iystart+1, dims_var, 1, global_atts, source_atts)
   if (do_secondary) then
      call nc_create(trim(adjustl(out_file_secondary)), ncid_secondary, &
         ixstop-ixstart+1, iystop-iystart+1, dims_var, 2, global_atts, source_atts)
   end if

   ! define netcdf variables
   if (use_netcdf_compression) then
      deflate_level2 = deflate_level
      shuffle_flag2  = shuffle_flag
   else
      deflate_level2 = 0
      shuffle_flag2  = .false.
   end if
   call def_output_primary(ncid_primary, dims_var, output_primary, global_atts%sensor, indexing%NViews, indexing%Ny, indexing%NSolar, indexing%YSolar, indexing%Y_Id, indexing%Ch_Is, 100, input_primary(1)%qc_flag_meanings, deflate_level2, shuffle_flag2, verbose, .true., .false., .false., .true., .false.)
   if (do_secondary) then
      call def_output_secondary(ncid_secondary, dims_var, output_secondary, indexing%Ny, indexing%NSolar, indexing%YSolar, indexing%Y_Id, indexing%Ch_Is, ThermalBit, deflate_level2, shuffle_flag2, 0, 0, verbose, .false.)
   end if

   ! put results in final output arrays with final datatypes
   do j=iystart,iystop
      do i=ixstart,ixstop
         call prepare_output_primary(i, j, indexing, input_primary(1), &
            output_primary)
         if (do_secondary) then
            call prepare_output_secondary(i, j, indexing, input_secondary(1), &
               output_secondary, .false.)
         end if
      end do
   end do

   ! deallocate the first input structure
   call dealloc_input_data_primary_all(input_primary(1))
   if (do_secondary) then
      call dealloc_input_data_secondary_all(input_secondary(1))
   end if

   ! write output to netcdf variables
   call write_output_primary(ncid_primary, ixstart, ixstop, iystart, iystop, output_primary, indexing%NViews, indexing%NSolar, indexing%Y_Id, .true., .false., .false., .true., .false.)
   if (do_secondary) then
      call write_output_secondary(ncid_secondary, ixstart, ixstop, iystart, iystop, output_secondary, indexing%NViews, indexing%Ny, indexing%NSolar, indexing%Nx, indexing%Y_Id, .false.)
   end if

   ! close output file
   if (nf90_close(ncid_primary) .ne. NF90_NOERR) then
      write(*,*) 'ERROR: nf90_close()'
      stop error_stop_code
   end if

   if (do_secondary) then
      if (nf90_close(ncid_secondary) .ne. NF90_NOERR) then
         write(*,*) 'ERROR: nf90_close()'
         stop error_stop_code
      end if
   end if


   ! deallocate output structure
   call dealloc_output_data_primary(output_primary, .true., .false., .false.)
   if (do_secondary) then
      call dealloc_output_data_secondary(output_secondary, .false.)
   end if


   deallocate(indexing%YSolar)
   deallocate(indexing%Y_Id)
   deallocate(indexing%Ch_Is)

#ifdef WRAPPER
end subroutine post_process_level2
#else
end program post_process_level2
#endif
