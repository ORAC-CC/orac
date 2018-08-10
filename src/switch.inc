function SWITCH_NAME_APP(Approach, Default, &
                         Cld, Cld1L, Cld2L, &
                         Aer, AerOx, AerSw, &
                         AerO1) result(out)
   use ORAC_constants_m

   implicit none

   integer,               intent(in) :: Approach
   SWITCH_TYPE,           intent(in) :: Default
   SWITCH_TYPE, optional, intent(in) :: Cld
   SWITCH_TYPE, optional, intent(in) :: Cld1L
   SWITCH_TYPE, optional, intent(in) :: Cld2L
   SWITCH_TYPE, optional, intent(in) :: Aer
   SWITCH_TYPE, optional, intent(in) :: AerOx
   SWITCH_TYPE, optional, intent(in) :: AerSw
   SWITCH_TYPE, optional, intent(in) :: AerO1
   SWITCH_TYPE                       :: out

   select case (Approach)
   case(AppCld1L)
      if (present(Cld1L)) then
         out = Cld1L
      else if (present(Cld)) then
         out = Cld
      else
         out = Default
      end if
   case(AppCld2L)
      if (present(Cld2L)) then
         out = Cld2L
      else if (present(Cld)) then
         out = Cld
      else
         out = Default
      end if
   case(AppAerOx)
      if (present(AerOx)) then
         out = AerOx
      else if (present(Aer)) then
         out = Aer
      else
         out = Default
      end if
   case(AppAerSw)
      if (present(AerSw)) then
         out = AerSw
      else if (present(Aer)) then
         out = Aer
      else
         out = Default
      end if
   case(AppAerO1)
      if (present(AerO1)) then
         out = AerO1
     else if (present(Aer)) then
        out = Aer
     else
        out = Default
     end if
  case default
      write(*,*) 'ERROR: switch_app(): Unsupported retrieval approach, ', &
           Approach
      stop error_stop_code
   end select

end function SWITCH_NAME_APP


function SWITCH_NAME_CLS(Class, Default, &
                         Cld, CldWat, CldIce, &
                         Aer, AerOx, AerSw, AerBR, &
                         AshEyj) result(out)
   use ORAC_constants_m

   implicit none

   integer,               intent(in) :: Class
   SWITCH_TYPE,           intent(in) :: Default
   SWITCH_TYPE, optional, intent(in) :: Cld
   SWITCH_TYPE, optional, intent(in) :: CldWat
   SWITCH_TYPE, optional, intent(in) :: CldIce
   SWITCH_TYPE, optional, intent(in) :: Aer
   SWITCH_TYPE, optional, intent(in) :: AerOx
   SWITCH_TYPE, optional, intent(in) :: AerSw
   SWITCH_TYPE, optional, intent(in) :: AerBR
   SWITCH_TYPE, optional, intent(in) :: AshEyj
   SWITCH_TYPE                       :: out

   select case (Class)
   case(ClsCldWat)
      if (present(CldWat)) then
         out = CldWat
      else if (present(Cld)) then
         out = Cld
      else
         out = Default
      end if
   case(ClsCldIce)
      if (present(CldIce)) then
         out = CldIce
      else if (present(Cld)) then
         out = Cld
      else
         out = Default
      end if
   case(ClsAerOx)
      if (present(AerOx)) then
         out = AerOx
      else if (present(Aer)) then
         out = Aer
      else
         out = Default
      end if
   case(ClsAerSw)
      if (present(AerSw)) then
         out = AerSw
      else if (present(Aer)) then
         out = Aer
      else
         out = Default
      end if
   case(ClsAerBR)
      if (present(AerBR)) then
         out = AerBR
      else if (present(Aer)) then
         out = Aer
      else
         out = Default
      end if
   case(ClsAshEyj)
      if (present(AshEyj)) then
         out = AshEyj
      else
         out = Default
      end if
   case default
      write(*,*) 'ERROR: switch_cls(): Unsupported retrieval class, ', &
           Class
      stop error_stop_code
   end select

end function SWITCH_NAME_CLS
