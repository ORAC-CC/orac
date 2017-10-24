recursive integer function PARSE_STRING_NAME_0D(in, out, ch_ids) result(status)
   use constants_cloud_typing_pavolonis_m
   use Int_Routines_m, only : find_in_array
   use ORAC_Constants_m

   implicit none

   character(len=*),  intent(in)  :: in
   PARSE_STRING_TYPE(kind=PARSE_STRING_KIND), intent(out) :: out
   integer, optional, intent(in)  :: ch_ids(:)
   integer                        :: ios
   integer                        :: op, cl, l, ch
   character(len=FilenameLen)     :: label, idx

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
         if (op+1 >= cl .or. .not. present(ch_ids)) then
            ! Closing bracket before opening (or not a wavelength array)
            status  = PARSE_ERR_CONV
         else
            idx = adjustl(in(op+1:cl-1)) ! String between brackets
            label = in(1:op-1)
            call clean_driver_label(label)

            ! Compare string to all ORAC_Constants array parameters listed
            select case (label)
            case('IRS')
               status = PARSE_ARRAY_INDEX_NAME_2D(idx, IRs, out, ch_ids)
            case('ISP')
               status = PARSE_ARRAY_INDEX_NAME_1D(idx, ISP, out, ch_ids)
            case('ISS')
               status = PARSE_ARRAY_INDEX_NAME_1D(idx, ISS, out, ch_ids)
            case default
               status  = PARSE_ERR_CONV
            end select
         end if
      else
         ! Compare string to all ORAC_Constants parameters listed
         l = len_trim(in)
         label = in(1:l)
         call clean_driver_label(label)

         ! Check for channel number indexing
         if (present(ch_ids)) then
            if (label(1:2) == 'CH') then
               read(label(3:), *, iostat=ios) ch
               if (ios /= 0) then
                  status = PARSE_ERR_CONV
               else
                  ! Return the index of the channel that has the requested ID
                  out = find_in_array(ch_ids, ch)
                  if (out == 0) status = PARSE_ERR_CONV
               end if

               return
            end if
         end if

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
         case('ITAU2')
            out = ITau
         case('IRE2')
            out = IRe
         case('IPC2')
            out = IPc
         case('IFR2')
            out = IFr
         case('ITS')
            out = ITs
         ! BRDF parameter indices
         case('IRHO_0V')
            out = IRho_0V
         case('IRHO_0D')
            out = IRho_0D
         case('IRHO_DV')
            out = IRho_DV
         case('IRHO_DD')
            out = IRho_DD
         ! Parameter selection methods
         case('SELMCTRL')
            out = SelmCtrl
         case('SELMMEAS')
            out = SelmMeas
         case('SELMAUX')
            out = SelmAux
         case('SELMPREV')
            out = SelmPrev
         ! Retrieval approaches
         case('CLSCLDWAT')
            out = ClsCldWat
         case('CLSCLDICE')
            out = ClsCldIce
         case('CLSAEROX')
            out = ClsAerOx
         case('CLSAERSW')
            out = ClsAerSw
         case('CLSASHEYJ')
            out = ClsAshEyj
         case('APPCLD1L')
            out = AppCld1L
         case('APPCLD2L')
            out = AppCld2L
         case('AEROX')
            out = AppAerOx
         case('AERSW')
            out = AppAerSw
         case('APPAEROX')
            out = AppAerOx
         case('APPAERSW')
            out = AppAerSw
         ! XIndex indices
         case('ISWAN_S')
            out = ISwan_S
         case('ISWAN_P')
            out = ISwan_P
         ! B indices
         case('ISEA')
            out = ISea
         case('ILAND')
            out = ILand
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
         ! Illumination conditions
         case('IDAY')
            out = IDay
         case('ITWI')
            out = ITwi
         case('INIGHT')
            out = INight
         ! Pavolonis cloud typing outputs
         case('CLEAR_TYPE','CLEAR')
            out = CLEAR_TYPE
         case('SWITCHED_TO_WATER_TYPE','SWITCHED_TO_WATER')
            out = SWITCHED_TO_WATER_TYPE
         case('FOG_TYPE','FOG')
            out = FOG_TYPE
         case('WATER_TYPE','WATER')
            out = WATER_TYPE
         case('SUPERCOOLED_TYPE','SUPERCOOLED')
            out = SUPERCOOLED_TYPE
         case('SWITCHED_TO_ICE_TYPE','SWITCHED_TO_ICE')
            out = SWITCHED_TO_ICE_TYPE
         case('OPAQUE_ICE_TYPE','OPAQUE_ICE')
            out = OPAQUE_ICE_TYPE
         case('CIRRUS_TYPE','CIRRUS')
            out = CIRRUS_TYPE
         case('OVERLAP_TYPE','OVERLAP')
            out = OVERLAP_TYPE
         case('PROB_OPAQUE_ICE_TYPE','PROB_OPAQUE_ICE')
            out = PROB_OPAQUE_ICE_TYPE
         case('PROB_CLEAR_TYPE','PROB_CLEAR')
            out = PROB_CLEAR_TYPE
         case default
            status = PARSE_ERR_CONV
         end select
      end if
   end if
end function PARSE_STRING_NAME_0D


recursive integer function PARSE_ARRAY_INDEX_NAME_1D(idx, arr, out, ch_ids) result(status)
   implicit none

   character(len=*), intent(in)  :: idx
   integer,          intent(in)  :: arr(:)
   PARSE_STRING_TYPE(kind=PARSE_STRING_KIND), intent(out) :: out
   integer,          intent(in)  :: ch_ids(:)
   PARSE_STRING_TYPE(kind=PARSE_STRING_KIND) :: i

   status = PARSE_STRING_NAME_0D(idx, i, ch_ids=ch_ids)
   if (status == 0) then
      ! Check index within bounds
      if (i > 0 .and. i <= size(arr)) then
         out = arr(i)
      else
         status = PARSE_ERR_BAD_ARR
      end if
   end if
end function PARSE_ARRAY_INDEX_NAME_1D


recursive integer function PARSE_ARRAY_INDEX_NAME_2D(idx, arr, out, ch_ids) result(status)
   implicit none

   character(len=*), intent(in)  :: idx
   integer,          intent(in)  :: arr(:,:)
   PARSE_STRING_TYPE(kind=PARSE_STRING_KIND), intent(out) :: out
   integer,          intent(in)  :: ch_ids(:)
   PARSE_STRING_TYPE(kind=PARSE_STRING_KIND) :: i(2)

   status = PARSE_STRING_NAME_1D(idx, i, ch_ids=ch_ids)
   if (status == 0) then
      ! Check index within bounds
      if (i(1) > 0 .and. i(2) > 0 .and. &
           i(1) <= size(arr,1) .and. i(2) <= size(arr,2)) then
         out = arr(i(1),i(2))
      else
         status = PARSE_ERR_BAD_ARR
      end if
   end if
end function PARSE_ARRAY_INDEX_NAME_2D
