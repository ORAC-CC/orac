

! CVS:  $Id: bandsolve.F,v 1.1 2001/04/30 08:43:42 norm Exp $
! CVS:  $Name:  $      

      module bandsolve
      
      implicit none
      
      public::bandec, banbks
      
      contains
      
      subroutine bandec(a, n, m1, m2, np, mp, al, mpl, indx, d)
      
         use kinds
      
         integer(kind=int_kind), intent(in) :: n, m1, m2, np, mp, mpl
      
         real(kind=dbl_kind), intent(inout), dimension(1:,1:) :: a
      
         integer(kind=int_kind), intent(out), dimension(1:) :: indx
         real(kind=dbl_kind), intent(out), dimension(1:,1:) :: al
         real(kind=dbl_kind), intent(out) :: d
      
         !Local variables
         real(kind=dbl_kind), parameter:: tiny=1.0e-20_dbl_kind
         integer(kind=int_kind) ::  i, j, k, ell, mm
         real(kind=dbl_kind) :: dum
      
         mm = m1 + m2 + 1
      
      !  if(mm > mp .or. m1 > mpl .or. n > np) then
      !      print *, "Bad arguments in bandec"
      !      !pause
      !   endif
      
      
         ell = m1
      
         do i = 1, m1
            do j = m1+2-i, mm
               a(i,j-ell) = a(i,j)
            enddo
            ell = ell - 1
            do j = mm-ell, mm
               a(i,j) = 0.0
            enddo
         enddo
      
         d = 1.0
         ell = m1
         do k = 1,n
            dum = a(k,1)
            i = k
            if (ell < n) ell = ell+1
            do j = k+1, ell
               if (abs(a(j,1)) > abs(dum)) then
                  dum = a(j,1)
                  i = j
               endif
            enddo
            indx(k) = i
            if (dum == 0.0) a(k,1) = tiny       !This was in num. rec.
            if (i /= k) then
              d = -d
               do j = 1, mm
                  dum = a(k,j)
                  a(k,j) = a(i,j)
                  a(i,j) = dum
               enddo
            endif
            do i = k+1, ell
              dum = a(i,1)/a(k,1)
               al(k,i-k) = dum
               do j = 2, mm
                  a(i, j-1) = a(i,j) - dum*a(k,j)
               enddo
               a(i,mm) = 0.0
            enddo
         enddo
         return
      end subroutine bandec
      
      
      subroutine banbks(a, n, m1, m2, np, mp, al, mpl, indx, b)
      
         use kinds
      
         integer(kind=int_kind), intent(in):: n, m1, m2, np, mp, mpl
         integer(kind=int_kind), intent(in), dimension(:):: indx
         real(kind=dbl_kind), intent(in), dimension(:,:):: a, al
      
         real(kind=dbl_kind), intent(inout), dimension(:):: b
      
         !Local variables
      
         integer(kind=int_kind):: i, k, ell, mm
         real(kind=dbl_kind):: dum
      
         mm = m1 + m2 + 1
      
         if (mm > mp .or. m1 > mpl .or. n > np) then
            print *, "Bad arguments in banbks"
         endif
      
         ell = m1
      
         do k = 1, n
            i = indx(k)
            if (i /= k) then
               dum = b(k)
               b(k) = b(i)
               b(i) = dum
            endif
            if (ell < n) ell = ell+1
            do i = k+1, ell
               b(i) = b(i) - al(k,i-k)*b(k)
            enddo
         enddo
      
         ell = 1
         do i = n, 1, -1
            dum = b(i)
            do k = 2, ell
               dum = dum - a(i,k)*b(k+i-1)
            enddo
            b(i) = dum/a(i,1)
            if (ell < mm) ell = ell+1
         enddo
         return
      end subroutine banbks
      
      end module bandsolve
