!-------------------------------------------------------------------------------
! Name: chunk_utils.F90
!
! Purpose:
!
! History:
! 2016/07/11, SP: Initial, replaces preprocessor specific chunking routines
!
!
! Bugs:
! None known.
!-------------------------------------------------------------------------------

module chunk_utils_m

   implicit none

   private

   public :: calc_n_chunks, &
             chunkify

contains

   function calc_n_chunks(n_segments, segment_starts, segment_ends, &
                          chunk_size) result (n_chunks)

      implicit none

      integer, intent(in) :: n_segments
      integer, intent(in) :: segment_starts(n_segments)
      integer, intent(in) :: segment_ends(n_segments)
      integer, intent(in) :: chunk_size
      integer             :: n_chunks

      integer :: i

      n_chunks = 0

      do i = 1, n_segments
         n_chunks = n_chunks + (segment_ends(i) - segment_starts(i)) / chunk_size + 1
      end do

   end function calc_n_chunks


   subroutine chunkify(n_segments, segment_starts, segment_ends, &
                       chunk_size, n_chunks, chunk_starts, chunk_ends)

      implicit none

      integer, intent(in)  :: n_segments
      integer, intent(in)  :: segment_starts(n_segments)
      integer, intent(in)  :: segment_ends(n_segments)
      integer, intent(in)  :: chunk_size
      integer, intent(out) :: n_chunks
      integer, intent(out) :: chunk_starts(*)
      integer, intent(out) :: chunk_ends(*)

      integer :: i

      n_chunks = 1

      do i = 1, n_segments
         chunk_starts(n_chunks) = segment_starts(i)

         do while (chunk_starts(n_chunks) + chunk_size .lt. segment_ends(i))
            chunk_ends(n_chunks) = chunk_starts(n_chunks) + chunk_size - 1
            n_chunks = n_chunks + 1
            chunk_starts(n_chunks) = chunk_starts(n_chunks - 1) + chunk_size
         end do

         chunk_ends(n_chunks) = segment_ends(i)

         n_chunks = n_chunks + 1
      end do

      n_chunks = n_chunks - 1

   end subroutine chunkify
end module chunk_utils_m
