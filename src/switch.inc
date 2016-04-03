function SWITCH_NAME(Approach, Default, &
     Cld, CldWat, CldIce, &
     Aer, AerOx, AerSw, &
     AshEyj) result(out)
   use ECP_constants_m, ApCldWat => CldWat, ApCldIce => CldIce, &
                        ApAerSw  => AerSw,  ApAerOx  => AerOx, ApAshEyj => AshEyj

   implicit none

   integer,               intent(in) :: Approach
   SWITCH_TYPE,           intent(in) :: Default
   SWITCH_TYPE, optional, intent(in) :: Cld
   SWITCH_TYPE, optional, intent(in) :: CldWat
   SWITCH_TYPE, optional, intent(in) :: CldIce
   SWITCH_TYPE, optional, intent(in) :: Aer
   SWITCH_TYPE, optional, intent(in) :: AerOx
   SWITCH_TYPE, optional, intent(in) :: AerSw
   SWITCH_TYPE, optional, intent(in) :: AshEyj
   SWITCH_TYPE                       :: out

   select case (Approach)
   case(ApCldWat)
      if (present(CldWat)) then
         out = CldWat
      else if (present(Cld)) then
         out = Cld
      else
         out = Default
      end if
   case(ApCldIce)
      if (present(CldIce)) then
         out = CldIce
      else if (present(Cld)) then
         out = Cld
      else
         out = Default
      end if
   case(ApAerOx)
      if (present(AerOx)) then
         out = AerOx
      else if (present(Aer)) then
         out = Aer
      else
         out = Default
      end if
   case(ApAerSw)
      if (present(AerSw)) then
         out = AerSw
      else if (present(Aer)) then
         out = Aer
      else
         out = Default
      end if
   case(ApAshEyj)
      if (present(AshEyj)) then
         out = AshEyj
      else
         out = Default
      end if
   case default
      write(*,*) 'ERROR: switch(): Unsupported retrieval approach, ', &
           Approach
      stop error_stop_code
   end select

end function SWITCH_NAME
