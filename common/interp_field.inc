!-------------------------------------------------------------------------------
! Name: interp_field
!
! Purpose:
! Given the output of bilinear_coefs, this actually performs the appropriate
! interpolation on the data.
!
! Description and Algorithm details:
! Determine which of the four corners around the desired point are missing
! data and act accordingly (see ACP's lab book, p.629 for more details):
! - If none are missing, perform a bilinear interpolation using the four
!   corners.
! - If one is missing, calculate the coefficients of a triangular interpolation
!   (see http://en.wikipedia.org/wiki/Barycentric_coordinate_system). If all
!   of them are within [0,1], the triangular interpolation is performed. If not,
!   the point is outside of that triangle and the nearest neighbour is returned
!   instead. A sketch of the situation is shown below, where 1,2,3 are valid
!   data points at the corners and X is the missing value.
!
!      Nearest point               /
!      3 here          3          X
!                      |\        /
!                      | \        <- Line of u = t
!                      |  \    /
!                      |   \
!      _ _ _ _ _ _ _ _ |    \/        Nearest point
!            /\        |     \        2 here
!      Line of u=0.5   |      \
!                      |       \
!      Nearest point   |Triangle\ <- Line of u + t = 1
!      1 here          |here     \
!                      1__________2
!                           |
!
!                           | <- Line of t = 0.5
!
!                           |
! - If two are missing, return the nearest neighbour.
! - If three are missing, return the only valid value.
! - If all four values are missing, return a missing value.
!
! Arguments:
! Name     Type    In/Out/Both Description
! ------------------------------------------------------------------------------
! in       real    In   The data to be interpolated.
! out      real    Both The result of the interpolation.
! interpol struct  In   The output of bilinear_coefs.
!
! History:
! 2014/08/05, AP: Initial version.
!
! $Id$
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

   select case (count(miss))
   case(0)
      ! bilinear interpolation
      datout = (1.-interp%t)*(1.-interp%u)* bot_left + &
                   interp%t *(1.-interp%u)* bot_rght + &
               (1.-interp%t)*    interp%u * top_left + &
                   interp%t *    interp%u * top_rght
   !----------------------------------------------------------------------------
   case(1)
      ! triangular interpolation
      if (miss(1)) then
         coef = [1.-interp%u, 1.-interp%t, interp%u+interp%t-1.]

         ! if outside valid triangle, do nearest neighbour instead
         if (all(coef <= 1. .and. coef >= 0.)) then
            datout = coef(1) * bot_rght + coef(2) * top_left + coef(3) * top_rght
         else
            if (interp%u > 0.5 .and. interp%t > 0.5) then
               datout = top_rght
            else if (interp%t > interp%u) then
               datout = bot_rght
            else
               datout = top_left
            end if
         end if
      else if (miss(2)) then
         coef = [1.-interp%u, interp%u-interp%t, interp%t]

         ! if outside valid triangle, do nearest neighbour instead
         if (all(coef <= 1. .and. coef >= 0.)) then
            datout = coef(1) * bot_left + coef(2) * top_left + coef(3) * top_rght
         else
            if (interp%u > 0.5 .and. interp%t < 0.5) then
               datout = top_left
            else if (interp%t+interp%u > 1.) then
               datout = top_rght
            else
               datout = bot_left
            end if
         end if
      else if (miss(3)) then
         coef = [1.-interp%t, interp%t-interp%u, interp%u]

         ! if outside valid triangle, do nearest neighbour instead
         if (all(coef <= 1. .and. coef >= 0.)) then
            datout = coef(1) * bot_left + coef(2) * bot_rght + coef(3) * top_rght
         else
            if (interp%u < 0.5 .and. interp%t > 0.5) then
               datout = bot_rght
            else if (interp%t+interp%u > 1.) then
               datout = top_rght
            else
               datout = bot_left
            end if
         end if
      else ! miss(4)
         coef = [1.-interp%u-interp%t, interp%t, interp%u]

         ! if outside valid triangle, do nearest neighbour instead
         if (all(coef <= 1. .and. coef >= 0.)) then
            datout = coef(1) * bot_left + coef(2) * bot_rght + coef(3) * top_left
         else
            if (interp%u < 0.5 .and. interp%t < 0.5) then
               datout = bot_left
            else if (interp%t > interp%u) then
               datout = bot_rght
            else
               datout = top_left
            end if
         end if
      end if
    !----------------------------------------------------------------------------
   case(2)
      ! output nearest neighbour
      if (miss(1)) then
         if (miss(2)) then                   ! O O
            if (interp%t < 0.5) then         ! X X
               datout = top_left
            else
               datout = top_rght
            end if
         else if (miss(3)) then              ! X O
            if (interp%u < 0.5) then         ! X O
               datout = bot_rght
            else
               datout = top_rght
            end if
         else ! miss(4)                      ! O X
            if (interp%t > interp%u) then    ! X O
               datout = bot_rght
            else
               datout = top_left
            end if
         end if
      else if (miss(2)) then
         if (miss(3)) then                   ! X O
            if (interp%u+interp%t < 1.) then ! O X
               datout = bot_left
            else
               datout = top_rght
            end if
         else ! miss(4)                      ! O X
            if (interp%u < 0.5) then         ! O X
               datout = bot_left
            else
               datout = top_left
            end if
         end if
      else ! miss(3) .and. miss(4)           ! X X
         if (interp%t < 0.5) then            ! O O
            datout = bot_left
         else
            datout = bot_rght
         end if
      end if
   !----------------------------------------------------------------------------
   case(3)
      ! output only available value
      if (.not. miss(1)) then
         datout = bot_left
      else if (.not. miss(2)) then
         datout = bot_rght
      else if (.not. miss(3)) then
         datout = top_left
      else ! .not. miss(4)
         datout = top_rght
      end if
   !----------------------------------------------------------------------------
   case(4)
      ! output fill value
      datout = sreal_fill_value
   end select
