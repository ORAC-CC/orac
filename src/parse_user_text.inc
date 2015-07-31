integer function PARSE_STRING_NAME_0D(in, out) result(status)
   use constants_cloud_typing_pavolonis
   use ECP_Constants

   implicit none

   character(len=*), intent(in)  :: in
   PARSE_STRING_TYPE(kind=PARSE_STRING_KIND), intent(out) :: out
   integer                       :: ios
   integer                       :: op, cl, l
   character(len=FilenameLen)    :: label, idx

   status = 0

   ! Attempt to read as an integer
   read(in, *, iostat=ios) out
   if (ios /= 0) then
      ! If failed, attempt to interpret human-readable text

      ! Check for array indexing via a round bracket.
      op = index(in, '(')
      if (op > 1) then
         ! We are dealing with an array parameter.
         cl = index(in, ')')
         if (op+1 <= cl) then
            ! Closing bracket before opening
            status  = PARSE_ERR_CONV
         else
            idx = adjustl(in(op+1:cl-1)) ! String between brackets
            label = in(1:op-1)
            call clean_driver_label(label)

            ! Compare string to all ECP_Constants array parameters listed
            select case (label)
!            case('IRS')
!               status = PARSE_ARRAY_INDEX_NAME(idx, IRs, out)
!            case('ISP')
!               status = PARSE_ARRAY_INDEX_NAME(idx, ISP, out)
!            case('ISS')
!               status = PARSE_ARRAY_INDEX_NAME(idx, ISS, out)
            case default
               status  = PARSE_ERR_CONV
            end select
         end if
      else
         ! Compare string to all ECP_Constants parameters listed
         l = len_trim(in)
         label = in(1:l)
         call clean_driver_label(label)

         select case (label)
         ! State vector indices
         case('ITAU')
            out = ITau
         case('IRE')
            out = IRe
         case('IPC')
            out = IPc
         case('IFR')
            out = IFr
         case('ITS')
            out = ITs
         ! Parameter selection methods
         case('SELMCTRL')
            out = SelmCtrl
         case('SELMMEAS')
            out = SelmMeas
         case('SELMAUX')
            out = SelmAux
         ! RTM interpolation methods
         case('RTMINTMETHLINEAR')
            out = RTMIntMethLinear
         case('RTMINTMETHSPLINE')
            out = RTMIntMethSpline
         case('RTMINTMETHNONE')
            out = RTMIntMethNone
         ! LUT interpolaton method selection
         case('LUTINTMETHLINEAR')
            out = LUTIntMethLinear
         case('LUTINTMETHBICUBIC')
            out = LUTIntMethBicubic
         ! Breakpoint file output level selection
         case('BKPL_FM')
            out = Bkpl_FM
         case('BKPL_FM_SOLAR')
            out = Bkpl_FM_Solar
         case('BKPL_FM_THERMAL')
            out = Bkpl_FM_Thermal
         case('BKPL_GET_SPIXEL')
            out = Bkpl_Get_SPixel
         case('BKPL_INTERPOL_SOLAR')
            out = Bkpl_Interpol_Solar
         case('BKPL_INTERPOL_THERMAL')
            out = Bkpl_Interpol_Thermal
         case('BKPL_INVERTMARQUARDT_1')
            out = Bkpl_InvertMarquardt_1
         case('BKPL_INVERTMARQUARDT_2')
            out = Bkpl_InvertMarquardt_2
         case('BKPL_INVERTMARQUARDT_3')
            out = Bkpl_InvertMarquardt_3
         case('BKPL_INVERTMARQUARDT_4')
            out = Bkpl_InvertMarquardt_4
         case('BKPL_READ_LUT_1')
            out = Bkpl_Read_LUT_1
         case('BKPL_READ_LUT_2')
            out = Bkpl_Read_LUT_2
         ! Pavolonis cloud typing outputs
         case('CLEAR_TYPE','CLEAR')
            out = CLEAR_TYPE
         case('PROB_CLEAR_TYPE','PROB_CLEAR')
            out = PROB_CLEAR_TYPE
         case('FOG_TYPE','FOG')
            out = FOG_TYPE
         case('WATER_TYPE','WATER')
            out = WATER_TYPE
         case('SUPERCOOLED_TYPE','SUPERCOOLED')
            out = SUPERCOOLED_TYPE
         case('MIXED_TYPE','MIXED')
            out = MIXED_TYPE
         case('OPAQUE_ICE_TYPE','OPAQUE_ICE')
            out = OPAQUE_ICE_TYPE
         case('CIRRUS_TYPE','CIRRUS')
            out = CIRRUS_TYPE
         case('OVERLAP_TYPE','OVERLAP')
            out = OVERLAP_TYPE
         case('PROB_OPAQUE_ICE_TYPE','PROB_OPAQUE_ICE')
            out = PROB_OPAQUE_ICE_TYPE
         case default
            status = PARSE_ERR_CONV
         end select
      end if
   end if
end function PARSE_STRING_NAME_0D


integer function PARSE_ARRAY_INDEX_NAME(idx, arr, out) result(status)
   implicit none

   character(len=*), intent(in)  :: idx
   integer,          intent(in)  :: arr(:)
   PARSE_STRING_TYPE(kind=PARSE_STRING_KIND), intent(out) :: out
   integer                       :: i

   status = parse_string(idx, i)
   if (status == 0) then
      ! Check index within bounds
      if (i > 0 .and. i <= size(arr)) then
         out = arr(i)
      else
         status = PARSE_ERR_BAD_ARR
      end if
   end if
end function PARSE_ARRAY_INDEX_NAME
