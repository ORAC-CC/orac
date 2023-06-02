!*******************************************************************************
!
! Copyright (C) 2000-2018, RAL Space, Science and Technology Facilities Council
! Copyright (C) 2000-2018, University of Oxford
! Copyright (C) 2011-2018, Deutscher Wetterdienst
!
! This file is part of the Optimal Retrieval of Aerosol and Cloud (ORAC).
!
! ORAC is free software: you can redistribute it and/or modify it under the
! terms of the GNU General Public License as published by the Free Software
! Foundation, either version 3 of the License, or (at your option) any later
! version.
!
! ORAC is distributed in the hope that it will be useful, but WITHOUT ANY
! WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR
! A PARTICULAR PURPOSE. See the GNU General Public License for more details.
!
! You should have received a copy of the GNU General Public License along with
! ORAC. If not, see <http://www.gnu.org/licenses/>.
!
!*******************************************************************************


!-------------------------------------------------------------------------------
! Name:
! orac_postproc
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
! 2012/03/18, CP: Modified to add cloud flag
! 2012/07/06, MJ: Extensively overhauls and restructures the code
! 2012/07/13, MJ: Extends range of valid ref for netcdf write
! 2012/07/23, MJ: Adds choice of strictness of cloud checks.
! 2012/07/23, MJ: Adds code to play with cloud screening during day and night
! 2012/08/27, MJ: Better implements time variable in output.
! 2012/11 to 2013/01, SS, MJ and MS: make major changes to cloud masking
!    approach,
! 2012/11 to 2013/01, SS, MJ: Implements untested OMP parallelization, changes
!    wrt CF compliance.
! 2013/01/17, MJ: Adds code to accommodate uncertainties of ctt and cth
! 2014/01/22, MJ: Fixes FP overflow with COT uncertainty.
! 2014/01/23, MJ: Changes of how COT uncertainty is treated.
! 2014/02/07, MJ: Corrects logical error (water got penalty when ice prop.
! were out of range and vice versa),cleans up code
! 2014/02/10, MJ: Reorders and rewrites the penalty assignment in phase
!    selection
! 2014/03/14, MJ: Removes code to scale cot from log_10(tau) to tau as this step
!    was already introduced in ORAC.
! 2014/07/08, CP: Added more illumination options
! 2014/07/08, CP: Added in extra subroutine that uses Istomina like tests to
!    detect clear snow and ice
! 2014/09/29, changed wat read_inter)sec is called now need a instrument name
! 2014/10/24, OS: Added subroutine option for wrapper mode, and further
!    wrapper-specific details; SteSta added new fill value option for the
!    post processing cloud mask
! 2014/11/20, OS: Renamed module neural_net_constants to
!    neural_net_constants_postproc; some editing; replaced hard-coded NN values
!    with variables
! 2015/02/05, OS: Changed nint to lint; further replacements of variables
!    defined in postproc_constants with those defined in common_constants; added
!    phase_post; Pavolonis phase is now used as basis for
! 2015/02/06, CP: Added in case of ML IR only retrieval
! 2015/02/06, CP: Tidied up phase selection removed obsolete stuff
! 2015/02/06, CP: Added case of IR only multi layer selection phase-dependent
!    variables
! 2015/02/07, CP: Massive tidy up including of common constants
! 2015/03/19, OS: COT maximum value set to 100, CWP scaled accordingly + minor
!    editing
! 2015/04/22, OS: Only apply Istomina retyping when cloud mask says cloud
! 2015/07/06, OS: Added flags "one_phase_only" and "cloudy_only" as file input;
!    reclassify phase if CTT does not match phase temperature range (i.e. no ice
!    if CTT>273.15K); removed some debugging statements and further clean up
! 2015/07/10, OS: Fixed bug related to using flag one_phase_only
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
! 2015/09/26, GM: Remove eight unused mandatory driver file lines.
! 2015/11/17, OS: Some bug fixing in correctly switching phase and cloud types.
!    Previously, switching was wrong and, additionally, phase was ice for all
!    cloud free pixels. This should all be resolved with this commit.
! 2016/02/02, GM: Add option output_optical_props_at_night.
! 2016/03/11, AP: Read both aerosol and cloud inputs.
! 2016/04/28, AP: Add multiple views.
! 2016/06/06, SP: Updates for bayesian selection without huge memory usage.
! 2016/07/09, SP: Further memory usage options: Can now use 'chunking' to reduce
!    the amount of memory used. Works in same way as preproc option.
! 2017/01/09, CP: Added ML cloud variables and changed phase to handle ML so it
!    can be recognised in L3.
! 2017/01/09, CP: Bug fix to ml code which was upsetting aerosol retrieval
! 2017/06/22, OS: Introduced ann phase, which is now the default for phase
!    selection; to choose Pavolonis for phase selection, set use_ann_phase to
!    false; NOTE: there is no overlap type when using ann phase, in which case
!    no multilayer phase flag will be set!
! 2017/09/05, CP: Added logical flag at the top of the driver file to turn on
!    ML post processing.
! 2017/10/05, GM: Get the value of use_ann_phase from the input files.
! 2018/06/08, SP: Add ability to parallax correct the data.
!
! Bugs:
! - Use of input_primary%cldtype is hardwired to view element 1.
! - no phase switch applied on temperature of multi layer cloud
!-------------------------------------------------------------------------------

#ifndef WRAPPER
program orac_postproc
#else
subroutine orac_postproc(mytask, ntasks, lower_bound, upper_bound, &
                          path_and_file)
#endif

    use chunk_utils_m
    use constants_cloud_typing_pavolonis_m
    use global_attributes_m
    use orac_input_m
    use orac_ncdf_m
    use orac_output_m
    use parsing_m
    use postproc_constants_m
    use postproc_utils_m
    use prepare_output_pp_m
    use source_attributes_m
    use correct_parallax_m

    implicit none

    integer, parameter           :: MaxInFiles = 32

    integer, parameter           :: IWat = 1
    integer, parameter           :: IIce = 2
    integer, parameter           :: IMul = 3

    integer                      :: i, j, k

    integer                      :: nargs
#ifdef WRAPPER
    integer                      :: mytask, ntasks, lower_bound, upper_bound
#endif
    character(len=path_length)   :: label, value

    logical                      :: switch_phases
    logical                      :: do_secondary = .false.

    real                         :: cost_thresh = 0.
    real                         :: norm_prob_thresh = .75
    logical                      :: output_optical_props_at_night = .false.
    logical                      :: use_ann_phase
    logical                      :: use_bayesian_selection = .false.
    logical                      :: use_new_bayesian_selection = .false.
    logical                      :: use_chunks = .false.
    logical                      :: use_netcdf_compression = .true.
    logical                      :: corr_plx = .false.
    logical                      :: verbose = .true.

    logical                      :: use_ml

    integer                      :: n_in_files

    character(len=path_length)   :: path_and_file

    character(len=path_length)   :: in_files_primary(MaxInFiles), &
         in_files_secondary(MaxInFiles)

    character(len=path_length)   :: out_file_primary, out_file_secondary

    integer                      :: ncid_primary, ncid_secondary, dims_var(3)

    type(global_attributes_t)    :: global_atts
    type(source_attributes_t)    :: source_atts

    type(input_data_primary_t)   :: input_primary(0:MaxInFiles)
    type(input_data_secondary_t) :: input_secondary(0:MaxInFiles)

    type(output_data_primary_t)  :: output_primary
    type(output_data_secondary_t):: output_secondary

    type(input_indices_t)        :: indexing, loop_ind(MaxInFiles)

    integer                      :: phase_flag
    integer                      :: index_space

    ! New variables for chunked post-processing
    integer                      :: chunksize
    integer                      :: i_chunk
    integer                      :: n_chunks
    integer, allocatable         :: chunk_starts(:)
    integer, allocatable         :: chunk_ends(:)
    integer                      :: segment_starts(2)
    integer                      :: segment_ends(2)
    integer                      :: n_segments

    ! Temperature limits for phase reclassification
    real, parameter              :: switch_wat_limit = 233.16
    real, parameter              :: switch_ice_limit = 273.16

    integer                      :: i_min_costjm
    real                         :: a_max_prob, a_min_cost, a_prob, a_cost
    real                         :: sum_prob

    integer                      :: deflate_level2
    logical                      :: shuffle_flag2
#ifndef WRAPPER
    nargs = COMMAND_ARGUMENT_COUNT()
#else
    nargs=-1
#endif

    ! If no argument was given then read standard file
    if (nargs == 0 ) then
       call get_environment_variable("ORAC_TEXTIN", path_and_file)
    else if (nargs == 1) then
       call get_command_argument(1, path_and_file)
    else if (nargs == -1) then
       index_space = index(path_and_file, " ")
       path_and_file = path_and_file(1:(index_space-1))
       if (verbose) write(*,*) 'inside postproc ', trim(adjustl(path_and_file))
    end if

    ! Read from driver file

    open(11,file=trim(adjustl(path_and_file)), status='old', form='formatted')

    read(11,*) use_ml

    read(11,'(A)') in_files_primary(IWat)
    read(11,'(A)') in_files_primary(IIce)

    if (use_ml) then
       read(11,'(A)') in_files_primary(IMul)
    end if

    read(11,'(A)') in_files_secondary(IWat)
    read(11,'(A)') in_files_secondary(IIce)

    if (use_ml) then
       read(11,'(A)') in_files_secondary(IMul)
    end if

    read(11,'(A)') out_file_primary
    read(11,'(A)') out_file_secondary
    read(11,*) switch_phases

    ! if single layer cloud then
    n_in_files = 2
    ! if multi layer cloud then
    if (use_ml) then
       n_in_files = 3
    end if

    if (out_file_secondary /= '') do_secondary = .true.

    ! Optional arguments and additional files
    do while (parse_driver(11, value, label) == 0)
       call clean_driver_label(label)
       select case (label)
       case('COST_THRESH')
          if (parse_string(value, cost_thresh) /= 0) &
               call handle_parse_error(label)
       case('NORM_PROB_THRESH')
          if (parse_string(value, norm_prob_thresh) /= 0) &
               call handle_parse_error(label)
       case('OUTPUT_OPTICAL_PROPS_AT_NIGHT')
          if (parse_string(value, output_optical_props_at_night) /= 0) &
               call handle_parse_error(label)
       case('USE_BAYESIAN_SELECTION')
          if (parse_string(value, use_bayesian_selection) /= 0) &
               call handle_parse_error(label)
       case('USE_NEW_BAYESIAN_SELECTION')
          if (parse_string(value, use_new_bayesian_selection) /= 0) &
               call handle_parse_error(label)
       case('USE_CHUNKING')
          if (parse_string(value, use_chunks) /= 0) &
               call handle_parse_error(label)
       case('USE_NETCDF_COMPRESSION')
          if (parse_string(value, use_netcdf_compression) /= 0) &
               call handle_parse_error(label)
       case('CORRECT_PARALLAX')
          if (parse_string(value, corr_plx) /= 0) &
               call handle_parse_error(label)
       case('VERBOSE')
          if (parse_string(value, verbose) /= 0) &
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
    close(11)

    if (use_chunks .and. corr_plx) then
       write(*,*)'ERROR: Cannot parallax correct if chunking is enabled. STOP.'
       stop
    end if

    ! If using new bayesian selection then ensure both bayesian flags are true
    if (use_new_bayesian_selection) then
       use_bayesian_selection = .true.
    end if

    if (verbose) then
       write(*,*) ' n_in_files = ',  n_in_files
       write(*,*) 'path_and_file = ', trim(path_and_file)
       write(*,*) 'primary water input = ', trim(in_files_primary(IWat))
       write(*,*) 'primary ice input = ', trim(in_files_primary(IIce))
       if (use_ml) then
          write(*,*) 'primary ml input = ', trim(in_files_primary(IMul))
       end if
       write(*,*) 'secondary water input = ', trim(in_files_secondary(IWat))
       write(*,*) 'secondary ice input = ', trim(in_files_secondary(IIce))
       if (use_ml) then
          write(*,*) 'secondary ml input = ', trim(in_files_secondary(IMul))
       end if
       write(*,*) 'primary output = ', trim(out_file_primary)
       write(*,*) 'secondary output = ', trim(out_file_secondary)
       write(*,*) 'switch_phases = ', switch_phases
       write(*,*) 'use_chunks = ', use_chunks
       write(*,*) 'use_ml = ', use_ml
    end if

    ! Find which non-Bayesian selection to use
    if (.not. use_bayesian_selection) then
       call get_use_ann_phase(in_files_primary, n_in_files, use_ann_phase, &
                              use_ml, verbose)

       ! ANN phase does not have an ML flag
!      if (use_ml .and. use_ann_phase) then
!         write(*,*) 'ERROR: ANN phase selection does not have ML support'
!         stop error_stop_code
!      end if
    end if

    ! Determine which channels/views exist across all input files
    do i = 1, n_in_files
       call read_input_dimensions(in_files_primary(i), loop_ind(i), verbose)
       call determine_channel_indexing(in_files_primary(i), loop_ind(i), verbose)
    end do
    call nullify_indexing(indexing)
    call cross_reference_indexing(n_in_files, loop_ind, indexing)

    n_chunks = 1

    ! Settings not inherited from input files
    indexing%flags%do_indexing        = .false.
    indexing%flags%do_phase_pavolonis = .true.
    indexing%flags%do_phase           = .true.

    ! If ever desired, processing limits should be set here from a keyword above
    indexing%X0 = 1
    indexing%X1 = indexing%Xdim
    indexing%Y0 = 1
    indexing%Y1 = indexing%Ydim
    loop_ind(:)%X0 = 1
    loop_ind(:)%X1 = indexing%Xdim
    loop_ind(:)%Y0 = 1
    loop_ind(:)%Y1 = indexing%Ydim

    if (.not. use_chunks) then
       n_chunks = 1

       allocate(chunk_starts(n_chunks))
       allocate(chunk_ends(n_chunks))

       chunk_starts(1) = 1
       chunk_ends(1)   = indexing%Ydim
    else
       n_segments = 1
       segment_starts(1) = 1
       segment_ends(1)   = indexing%Ydim

!      chunksize = 4096
       chunksize = 1024

       n_chunks = calc_n_chunks(n_segments, segment_starts, segment_ends, &
            chunksize)

       allocate(chunk_starts(n_chunks))
       allocate(chunk_ends(n_chunks))

       call chunkify(n_segments, segment_starts, segment_ends, chunksize, &
            n_chunks, chunk_starts, chunk_ends)
    end if

    if (verbose) then
       write(*,*) 'The number of chunks to be processed: ', n_chunks
       write(*,*) 'The chunks to be processed are (i_chunk, chunk_start, ' // &
            'chunk_end, chunk_size):'
       do i_chunk = 1, n_chunks
          write(*,*) i_chunk, chunk_starts(i_chunk), chunk_ends(i_chunk), &
               chunk_ends(i_chunk)-chunk_starts(i_chunk)+1
       end do
    end if

    ! Allocate the structures which hold the output in its final form
    call alloc_output_data_primary(indexing%common_indices_t, 100, &
         output_primary)
    if (do_secondary) then
       call alloc_output_data_secondary(indexing%common_indices_t, &
            output_secondary)
    end if

    do i_chunk=1, n_chunks ! Chunking
       if (use_chunks .and. verbose) &
            write(*,*) 'Processing chunk: ', i_chunk, 'between', &
                chunk_starts(i_chunk), 'and', chunk_ends(i_chunk)

       loop_ind(:)%Y0 = chunk_starts(i_chunk)
       loop_ind(:)%Y1 = chunk_ends(i_chunk)
       indexing%Y0 = chunk_starts(i_chunk)
       indexing%Y1 = chunk_ends(i_chunk)
       ! Allocate the array used in bayesian selection of type
       ! This is needed for minimisation of memory overhead, only costs are
       ! loaded in the first instance. Later other data is loaded but only for
       ! the best class
       !     allocate(indexing%best_infile(indexing%X0:indexing%X1, &
       !                                   indexing%Y0:indexing%Y1))
       !     do i = 1, n_in_files
       !        nullify(indexing%best_infile)
       !     end do

       ! Read once-only inputs (does not include all retrieved variables on
       ! standard ones)
       call alloc_input_data_primary_all(indexing, input_primary(0))
       call read_input_primary_once(n_in_files, in_files_primary, &
            input_primary(0), indexing, loop_ind, global_atts, source_atts, &
            chunk_starts(i_chunk), use_ml, verbose)

       if (do_secondary) then
          call alloc_input_data_secondary_all(indexing, input_secondary(0))
          call read_input_secondary_once(n_in_files, in_files_secondary, &
               input_secondary(0), indexing, loop_ind, chunk_starts(i_chunk), &
               verbose)
       end if

       ! Read fields that vary from file to file
       if (.not. use_new_bayesian_selection) then
          do i = 1, n_in_files
             if (verbose) write(*,*) '********************************'
             if (verbose) write(*,*) 'read: ', trim(in_files_primary(i))
             call alloc_input_data_primary_class(loop_ind(i), input_primary(i))
             call read_input_primary_class(in_files_primary(i), &
                  input_primary(i), loop_ind(i), .False., &
                  chunk_starts(i_chunk), verbose)
             if (do_secondary) then
                if (verbose) write(*,*) 'read: ', trim(in_files_secondary(i))
                call alloc_input_data_secondary_class(loop_ind(i), &
                     input_secondary(i))
                call read_input_secondary_class(in_files_secondary(i), &
                     input_secondary(i), loop_ind(i), chunk_starts(i_chunk), &
                     verbose)
             end if
          end do
       else ! Use traditional selection method
          ! Load only the cost values from input files
          do i = 1, n_in_files
             call alloc_input_data_only_cost(loop_ind(i), input_primary(i), &
                  input_secondary(i))
             call read_input_primary_class(in_files_primary(i), &
                  input_primary(i), loop_ind(i), .True., chunk_starts(i_chunk), &
                  verbose)
          end do

          ! Allocate a primary array to store temporary input data
          call alloc_input_data_primary_all(indexing, input_primary(1))

          if (do_secondary) then
             call alloc_input_data_secondary_all(indexing, input_secondary(1))
          end if

          ! Find the input file with the lowest cost for each pixel
          do j=indexing%Y0, indexing%Y1
             do i=indexing%X0, indexing%X1
                sum_prob = 0.
                a_min_cost   = huge(a_min_cost)
                i_min_costjm = 0
                do k = 1, n_in_files
                   if (input_primary(k)%costjm(i,j) /= sreal_fill_value) then
                      a_cost = input_primary(k)%costjm(i,j)
                      a_prob = exp(-input_primary(k)%costjm(i,j) / 2.)
                      sum_prob = sum_prob + a_prob

                      if (a_cost < a_min_cost) then
                         a_min_cost   = a_cost
                         a_max_prob   = a_prob
                         i_min_costjm = k
                      end if
                   end if
                end do
                if (a_min_cost >= cost_thresh .and. i_min_costjm /= 0 .and. &
                     a_max_prob / sum_prob >= norm_prob_thresh) then
                   input_primary(0)%phase(i,j) = i_min_costjm
                end if
             end do
          end do

          do k = 1, n_in_files
             if (verbose) write(*,*) '********************************'
             if (verbose) write(*,*) 'read: ', trim(in_files_primary(k))

             call read_input_primary_class(in_files_primary(k), &
                  input_primary(1), loop_ind(k), .False., &
                  chunk_starts(i_chunk), verbose)

             if (do_secondary) then
                if (verbose) write(*,*) 'read: ', trim(in_files_secondary(k))
                call read_input_secondary_class(in_files_secondary(k), &
                     input_secondary(1), loop_ind(k), chunk_starts(i_chunk), &
                     verbose)
             end if

             do j=indexing%Y0, indexing%Y1
                do i=indexing%X0, indexing%X1
                   if (input_primary(0)%phase(i,j) .eq. k) then
                      call copy_class_specific_inputs(i, j, loop_ind(k), &
                           input_primary(0), input_primary(1), &
                           input_secondary(0), input_secondary(1), &
                           do_secondary)
                      input_primary(0)%phase(i,j) = k
                   end if
                end do
             end do
          end do
       end if

       if (verbose) then
          write(*,*) 'Processing limits:'
          write(*,*) indexing%X0, indexing%X1
          write(*,*) indexing%Y0, indexing%Y1
       end if

       do j=indexing%Y0, indexing%Y1

          do i=indexing%X0, indexing%X1

             ! Cloud CCI selection
             if (.not. use_bayesian_selection) then
                ! Default is ICE wins, meaning only ice structure is copied to
                ! output.

                ! Information of Pavolonis cloud type
                ! C,N,F,W,S,M,I,Ci,O

                ! Liquid cloud phase comprises the following categories:
                ! 0? Clear
                ! 2? fog
                ! 3? warm liquid water clouds
                ! 4? supercooled-mixed-phased clouds

                ! Ice cloud phase comprises the following categories:
                ! 6? opaque ice clouds/deep convection
                ! 7? nonopaque high ice clouds (e.g. cirrus)
                ! 8? cloud overlap 4 (e.g. multiple cloud layers)

                ! Apply Pavolonis phase information to select retrieval phase
                ! variables select water type overwrite ice
                if (use_ann_phase) then
                   if (use_ml .and. &
                       input_primary(0)%cldtype(i,j,1) == OVERLAP_TYPE) then
                      phase_flag = 3_byte
                   else
                      select case (input_primary(0)%ann_phase(i,j,1))
                      case(LIQUID)
                         phase_flag = 1_byte
                      case(ICE)
                         phase_flag = 2_byte
                      case default
                         phase_flag = 0_byte
                      end select
                   end if
                else
                   select case (input_primary(0)%cldtype(i,j,1))
                   case(FOG_TYPE, &
                        WATER_TYPE, &
                        SUPERCOOLED_TYPE)
                      phase_flag = 1_byte
                   case(OPAQUE_ICE_TYPE, &
                        CIRRUS_TYPE, &
                        PROB_OPAQUE_ICE_TYPE, &
                        PROB_CLEAR_TYPE)
                      phase_flag = 2_byte
                   case(OVERLAP_TYPE)
                      if (use_ml .and. &
                           ! only set if ML cost is less than SL cost
                           (input_primary(IMul)%costjm(i,j) <= &
                            input_primary(IIce)%costjm(i,j))) then
                         phase_flag = 3_byte
                      else
                         phase_flag = 2_byte
                      end if
                   case default
                      phase_flag = 0_byte
                   end select
                end if

                ! Only consider reclassifying if both phases were processed
                if (switch_phases) then
                   ! water to ice
                   if ((phase_flag == 1_byte) .and. &
                       ((input_primary(IWat)%ctt(i,j) /= sreal_fill_value .and. &
                         input_primary(IWat)%ctt(i,j) < switch_wat_limit) .and. &
                        (input_primary(IIce)%ctt(i,j) /= sreal_fill_value .and. &
                         input_primary(IIce)%ctt(i,j) < switch_ice_limit))) then
                      phase_flag = 2_byte
                      input_primary(0)%cldtype(i,j,1) = SWITCHED_TO_ICE_TYPE
                   ! ice to water
                   elseif ((phase_flag == 2_byte) .and. &
                        ((input_primary(IWat)%ctt(i,j) /= sreal_fill_value .and. &
                        input_primary(IWat)%ctt(i,j) >= switch_wat_limit) .and. &
                        (input_primary(IIce)%ctt(i,j) /= sreal_fill_value .and. &
                        input_primary(IIce)%ctt(i,j) >= switch_ice_limit))) then
                      phase_flag = 1_byte
                      input_primary(0)%cldtype(i,j,1) = SWITCHED_TO_WATER_TYPE
                   end if
                end if

                ! Once phase is selected fill in the values 1st for wat then for
                ! ice.

                if (phase_flag == 1_byte) then
                   call copy_class_specific_inputs(i, j, loop_ind(IWat), &
                        input_primary(0), input_primary(IWat), &
                        input_secondary(0), input_secondary(IWat), do_secondary)
                   input_primary(0)%phase(i,j) = IPhaseWat
                else if (phase_flag == 2_byte) then
                   call copy_class_specific_inputs(i, j, loop_ind(IIce), &
                        input_primary(0), input_primary(IIce), &
                        input_secondary(0), input_secondary(IIce), do_secondary)
                   input_primary(0)%phase(i,j) = IPhaseIce
                else if (phase_flag == 3_byte) then
                   ! Multilayer cloud
                   call copy_class_specific_inputs(i, j, loop_ind(IMul), &
                        input_primary(0), input_primary(IMul),&
                        input_secondary(0), input_secondary(IMul), do_secondary)
                   input_primary(0)%phase(i,j) = IPhaseMul
                end if

                ! Overwrite cc_total with cldmask for Cloud CCI
                input_primary(0)%cc_total(i,j) = input_primary(0)%cldmask(i,j,1)
                input_primary(0)%cc_total_uncertainty(i,j) = &
                     input_primary(0)%cldmask_uncertainty(i,j,1)

                if (input_primary(0)%cc_total(i,j) == 0.0) then
                   ! Set phase to clear/unknown
                   input_primary(0)%phase(i,j) = IPhaseClU
                end if

                ! Bayesian based selection
             else if (.not. use_new_bayesian_selection) then
                sum_prob = 0.
                a_min_cost = huge(a_min_cost)
                i_min_costjm = 0.
                do k = 1, n_in_files
                   if (input_primary(k)%costjm(i,j) /= sreal_fill_value) then
                      a_cost = input_primary(k)%costjm(i,j)
                      a_prob = exp(-input_primary(k)%costjm(i,j) / 2.)
                      sum_prob = sum_prob + a_prob
                      if (a_cost < a_min_cost) then
                         a_min_cost   = a_cost
                         a_max_prob   = a_prob
                         i_min_costjm = k
                      end if
                   end if
                end do

                if (a_min_cost >= cost_thresh .and. i_min_costjm /= 0 .and. &
                     a_max_prob / sum_prob >= norm_prob_thresh) then
                   call copy_class_specific_inputs(i, j, &
                        loop_ind(i_min_costjm), input_primary(0), &
                        input_primary(i_min_costjm), input_secondary(0), &
                        input_secondary(i_min_costjm), do_secondary)
                   input_primary(0)%phase(i,j) = i_min_costjm
                end if
             end if
          end do
       end do


       ! Deallocate all input structures except for the first one
       if (.not. use_new_bayesian_selection) then
          do i = 1, n_in_files
             call dealloc_input_data_primary_class(input_primary(i))
             if (do_secondary)  &
                  call dealloc_input_data_secondary_class(input_secondary(i))
          end do
       else
          call dealloc_input_data_primary_all(input_primary(1))
          if (do_secondary)  &
               call dealloc_input_data_secondary_all(input_secondary(1))

          do i = 2, n_in_files
             call dealloc_input_data_primary_class(input_primary(i))
             if (do_secondary)  &
                  call dealloc_input_data_secondary_class(input_secondary(i))
          end do
       end if


       ! If parallax correction is enabled then correct parallax
       if (corr_plx) call correct_parallax(input_primary(0), indexing, global_atts, verbose)

       ! Put results in final output arrays with final datatypes
       do j=indexing%Y0, indexing%Y1
          do i=indexing%X0, indexing%X1
             call prepare_output_primary_pp(i, j, indexing%common_indices_t, &
                  input_primary(0), output_primary, &
                  output_optical_props_at_night)
             if (do_secondary) call prepare_output_secondary_pp(i, j,  &
                  indexing%common_indices_t, input_secondary(0), &
                  output_secondary)
          end do
       end do


       if (i_chunk .lt. n_chunks) then
          ! Deallocate the first input structure
          call dealloc_input_data_primary_all(input_primary(0))
          if (do_secondary) then
             call dealloc_input_data_secondary_all(input_secondary(0))
          end if
       end if

    end do !Chunking

    indexing%Y0=1

    ! Open the netcdf output file
    call ncdf_create(trim(adjustl(out_file_primary)), ncid_primary, &
         indexing%Xdim, indexing%Ydim, indexing%NViews, dims_var, 1, &
         global_atts, source_atts)
    if (do_secondary) then
       call ncdf_create(trim(adjustl(out_file_secondary)), ncid_secondary, &
            indexing%Xdim, indexing%Ydim, indexing%NViews, dims_var, 2, &
            global_atts, source_atts)
    end if

    ! Copy variable masks
    output_primary%qc_flag_masks    = input_primary(0)%qc_flag_masks
    output_primary%qc_flag_meanings = input_primary(0)%qc_flag_meanings
    output_primary%ch_flag_masks    = input_primary(0)%ch_flag_masks
    output_primary%ch_flag_meanings = input_primary(0)%ch_flag_meanings
    output_primary%vr_flag_masks    = input_primary(0)%vr_flag_masks
    output_primary%vr_flag_meanings = input_primary(0)%vr_flag_meanings

    ! Define netcdf variables
    if (use_netcdf_compression) then
       deflate_level2 = deflate_level
       shuffle_flag2  = shuffle_flag
    else
       deflate_level2 = 0
       shuffle_flag2  = .false.
    end if
    call def_output_primary(ncid_primary, dims_var, output_primary, &
         indexing%common_indices_t, deflate_level2, shuffle_flag2, &
         .false., phases=loop_ind(1:n_in_files)%LUTClass)
    if (do_secondary) then
       call def_output_secondary(ncid_secondary, dims_var, output_secondary, &
            indexing%common_indices_t, deflate_level2, shuffle_flag2, .false.)
    end if

    do i = 1, n_in_files
       call dealloc_input_indices(loop_ind(i))
    end do

    ! Deallocate the first input structure
    call dealloc_input_data_primary_all(input_primary(0))
    if (do_secondary) then
       call dealloc_input_data_secondary_all(input_secondary(0))
    end if

    ! Write output to netcdf variables
    call write_output_primary(ncid_primary, indexing%common_indices_t, &
         output_primary)
    if (do_secondary) then
       call write_output_secondary(ncid_secondary, indexing%common_indices_t, &
            output_secondary)
    end if

    ! Close output file
    call ncdf_close(ncid_primary, 'orac_postproc()')
    if (do_secondary) then
       call ncdf_close(ncid_secondary, 'orac_postproc()')
    end if

    ! Deallocate output structure
    call dealloc_output_data_primary(output_primary)
    if (do_secondary) then
       call dealloc_output_data_secondary(output_secondary)
    end if

    call dealloc_common_indices(indexing%common_indices_t)

    deallocate(chunk_starts)
    deallocate(chunk_ends)

#ifdef WRAPPER
end subroutine orac_postproc
#else
end program orac_postproc
#endif
