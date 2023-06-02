subroutine NCDF_READ_NAME_0D(ncid, name, val)
   implicit none

   integer,                                 intent(in)    :: ncid
   character(len=*),                        intent(in)    :: name
   NCDF_READ_TYPE(kind=NCDF_READ_KIND),     intent(inout) :: val

   integer                                  :: ierr, vid
   logical                                  :: fv_flag, vr_flag
#ifdef DEBUG
   character(len=unitlength)                :: unit
#endif
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)      :: fv, sf, of, vmin, vmax
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)      :: fill=NCDF_READ_FILL_VALUE

#include "ncdf_open_field.inc"
#ifdef DEBUG
   print*, 'Reading variable: ', trim(name)
#endif
   ! read data
   ierr = nf90_get_var(ncid, vid, val)
   if (ierr.ne.NF90_NOERR) then
      print*, 'ERROR: ncdf_read_file(): Could not read variable ',trim(name)
      print*, trim(nf90_strerror(ierr))
      stop error_stop_code
   end if

   ! replace file's fill value and out of range values with our own and apply
   ! scale factor/offset
   if (fv_flag) then
      if (val.eq.fv) val = fill
   end if
   if (vr_flag) then
      if (val.ne.fill .and. val.lt.vmin) val = fill
      if (val.ne.fill .and. val.gt.vmax) val = fill
   end if
   if (sf .ne. 1.0 .or. of .ne. 0.0) then
     val = sf*val + of
   end if

   ! additional information for print out
#ifdef DEBUG
   if (nf90_get_att(ncid, vid, 'units', unit).eq.NF90_NOERR) then
      call c_to_f_str(unit)
      print*, 'Field units: ', trim(unit)
   end if
   if (vr_flag) then
      print*, 'Field valid min: ', sf*vmin + of
      print*, 'Field valid max: ', sf*vmax + of
   end if
#endif

end subroutine NCDF_READ_NAME_0D


subroutine NCDF_READ_NAME_1D(ncid, name, val, dim, ind, start)
   implicit none

   integer,                                     intent(in)    :: ncid
   character(len=*),                            intent(in)    :: name
   NCDF_READ_TYPE(kind=NCDF_READ_KIND), target, intent(inout) :: val(:)
   integer, optional,                           intent(in)    :: dim, ind(:)
   integer, optional,                           intent(in)    :: start(:)

   integer                                      :: ierr, vid, i
   integer, dimension(1)                        :: start_pos, counter, stride
   logical                                      :: fv_flag, vr_flag
#ifdef DEBUG
   character(len=unitlength)                    :: unit
#endif
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)          :: fv, sf, of, vmin, vmax
   NCDF_READ_TYPE(kind=NCDF_READ_KIND), pointer :: arr(:)
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)          :: fill=NCDF_READ_FILL_VALUE

   start_pos = 1
   counter = size(val,1)
   stride = 1

   if (present(start)) then
      start_pos = start
   end if

#include "ncdf_open_field.inc"

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i = 1, size(ind)
         arr => val(i:i)

         start_pos(dim) = ind(i)
#include "ncdf_read_field.inc"
      end do
   else
      ! read everything
      arr => val
#include "ncdf_read_field.inc"
   end if

end subroutine NCDF_READ_NAME_1D


subroutine NCDF_READ_NAME_2D(ncid, name, val, dim, ind, start)
   implicit none

   integer,           intent(in)                              :: ncid
   character(len=*),  intent(in)                              :: name
   NCDF_READ_TYPE(kind=NCDF_READ_KIND), target, intent(inout) :: val(:,:)
   integer, optional, intent(in)                              :: dim, ind(:)
   integer, optional, intent(in)                              :: start(:)

   integer                                      :: ierr, vid, i
   integer, dimension(2)                        :: start_pos, counter, stride
   logical                                      :: fv_flag, vr_flag
#ifdef DEBUG
   character(len=unitlength)                    :: unit
#endif
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)          :: fv, sf, of, vmin, vmax
   NCDF_READ_TYPE(kind=NCDF_READ_KIND), pointer :: arr(:,:)
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)          :: fill=NCDF_READ_FILL_VALUE

   start_pos = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   stride = 1

   if (present(start)) then
      start_pos = start
   end if

#include "ncdf_open_field.inc"

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i = 1, size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:)
         case(2)
            arr => val(:,i:i)
         end select

         start_pos(dim) = ind(i)
#include "ncdf_read_field.inc"
      end do
   else
      ! read everything
      arr => val
#include "ncdf_read_field.inc"
   end if

end subroutine NCDF_READ_NAME_2D


subroutine NCDF_READ_NAME_3D(ncid, name, val, dim, ind, start)
   implicit none

   integer,           intent(in)                              :: ncid
   character(len=*),  intent(in)                              :: name
   NCDF_READ_TYPE(kind=NCDF_READ_KIND), target, intent(inout) :: val(:,:,:)
   integer, optional, intent(in)                              :: dim, ind(:)
   integer, optional, intent(in)                              :: start(:)

   integer                                      :: ierr, vid, i
   integer, dimension(3)                        :: start_pos, counter, stride
   logical                                      :: fv_flag, vr_flag
#ifdef DEBUG
   character(len=unitlength)                    :: unit
#endif
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)          :: fv, sf, of, vmin, vmax
   NCDF_READ_TYPE(kind=NCDF_READ_KIND), pointer :: arr(:,:,:)
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)          :: fill=NCDF_READ_FILL_VALUE

   start_pos = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   stride = 1

   if (present(start)) then
      start_pos = start
   end if

#include "ncdf_open_field.inc"

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i = 1, size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:,:)
         case(2)
            arr => val(:,i:i,:)
         case(3)
            arr => val(:,:,i:i)
         end select

         start_pos(dim) = ind(i)
#include "ncdf_read_field.inc"
      end do
   else
      ! read everything
      arr => val
#include "ncdf_read_field.inc"
   end if

end subroutine NCDF_READ_NAME_3D


subroutine NCDF_READ_NAME_4D(ncid, name, val, dim, ind, start)
   implicit none

   integer,                                     intent(in)    :: ncid
   character(len=*),                            intent(in)    :: name
   NCDF_READ_TYPE(kind=NCDF_READ_KIND), target, intent(inout) :: val(:,:,:,:)
   integer, optional,                           intent(in)    :: dim, ind(:)
   integer, optional, intent(in)                              :: start(:)

   integer                                      :: ierr, vid, i
   integer, dimension(4)                        :: start_pos, counter, stride
   logical                                      :: fv_flag, vr_flag
#ifdef DEBUG
   character(len=unitlength)                    :: unit
#endif
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)          :: fv, sf, of, vmin, vmax
   NCDF_READ_TYPE(kind=NCDF_READ_KIND), pointer :: arr(:,:,:,:)
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)          :: fill=NCDF_READ_FILL_VALUE

   start_pos = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   counter(4) = size(val,4)
   stride = 1

   if (present(start)) then
      start_pos = start
   end if

#include "ncdf_open_field.inc"

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i = 1, size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:,:,:)
         case(2)
            arr => val(:,i:i,:,:)
         case(3)
            arr => val(:,:,i:i,:)
         case(4)
            arr => val(:,:,:,i:i)
         end select

         start_pos(dim) = ind(i)
#include "ncdf_read_field.inc"
      end do
   else
      ! read everything
      arr => val
#include "ncdf_read_field.inc"
   end if

end subroutine NCDF_READ_NAME_4D


subroutine NCDF_READ_NAME_5D(ncid, name, val, dim, ind, start)
   implicit none

   integer,                                     intent(in)    :: ncid
   character(len=*),                            intent(in)    :: name
   NCDF_READ_TYPE(kind=NCDF_READ_KIND), target, intent(inout) :: val(:,:,:,:,:)
   integer, optional,                           intent(in)    :: dim, ind(:)
   integer, optional, intent(in)                              :: start(:)

   integer                                      :: ierr, vid, i
   integer, dimension(5)                        :: start_pos, counter, stride
   logical                                      :: fv_flag, vr_flag
#ifdef DEBUG
   character(len=unitlength)                    :: unit
#endif
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)          :: fv, sf, of, vmin, vmax
   NCDF_READ_TYPE(kind=NCDF_READ_KIND), pointer :: arr(:,:,:,:,:)
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)          :: fill=NCDF_READ_FILL_VALUE

   start_pos = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   counter(4) = size(val,4)
   counter(5) = size(val,5)
   stride = 1

   if (present(start)) then
      start_pos = start
   end if

#include "ncdf_open_field.inc"

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i = 1, size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:,:,:,:)
         case(2)
            arr => val(:,i:i,:,:,:)
         case(3)
            arr => val(:,:,i:i,:,:)
         case(4)
            arr => val(:,:,:,i:i,:)
         case(5)
            arr => val(:,:,:,:,i:i)
         end select

         start_pos(dim) = ind(i)
#include "ncdf_read_field.inc"
      end do
   else
      ! read everything
      arr => val
#include "ncdf_read_field.inc"
   end if

end subroutine NCDF_READ_NAME_5D


subroutine NCDF_READ_NAME_6D(ncid, name, val, dim, ind, start)
   implicit none

   integer,                                     intent(in)    :: ncid
   character(len=*),                            intent(in)    :: name
   NCDF_READ_TYPE(kind=NCDF_READ_KIND), target, intent(inout) :: val(:,:,:,:,:,:)
   integer, optional,                           intent(in)    :: dim, ind(:)
   integer, optional, intent(in)                              :: start(:)

   integer                                      :: ierr, vid, i
   integer, dimension(6)                        :: start_pos, counter, stride
   logical                                      :: fv_flag, vr_flag
#ifdef DEBUG
   character(len=unitlength)                    :: unit
#endif
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)          :: fv, sf, of, vmin, vmax
   NCDF_READ_TYPE(kind=NCDF_READ_KIND), pointer :: arr(:,:,:,:,:,:)
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)          :: fill=NCDF_READ_FILL_VALUE

   start_pos = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   counter(4) = size(val,4)
   counter(5) = size(val,5)
   counter(6) = size(val,6)
   stride = 1

   if (present(start)) then
      start_pos = start
   end if

#include "ncdf_open_field.inc"

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i = 1, size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:,:,:,:,:)
         case(2)
            arr => val(:,i:i,:,:,:,:)
         case(3)
            arr => val(:,:,i:i,:,:,:)
         case(4)
            arr => val(:,:,:,i:i,:,:)
         case(5)
            arr => val(:,:,:,:,i:i,:)
         case(6)
            arr => val(:,:,:,:,:,i:i)
         end select

         start_pos(dim) = ind(i)
#include "ncdf_read_field.inc"
      end do
   else
      ! read everything
      arr => val
#include "ncdf_read_field.inc"
   end if

end subroutine NCDF_READ_NAME_6D


#ifdef NCDF_RD_P_TYPE

subroutine NCDF_READ_PACKED_NAME_1D(ncid, name, val, dim, ind, start)
   implicit none

   integer,                                     intent(in)    :: ncid
   character(len=*),                            intent(in)    :: name
   NCDF_READ_TYPE(kind=NCDF_READ_KIND), target, intent(inout) :: val(:)
   integer, optional,                           intent(in)    :: dim, ind(:)
   integer, optional, intent(in)                              :: start(:)

   integer                                      :: ierr, vid, i
   integer, dimension(1)                        :: start_pos, counter, stride
   logical                                      :: fv_flag, vr_flag
#ifdef DEBUG
   character(len=unitlength)                    :: unit
#endif
   NCDF_RD_P_TYPE(kind=NCDF_RD_P_KIND)          :: fv, vmin, vmax
   NCDF_RD_P_TYPE(kind=NCDF_RD_P_KIND)          :: arr_packed(1)
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)          :: sf, of
   NCDF_READ_TYPE(kind=NCDF_READ_KIND), pointer :: arr(:)
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)          :: fill=NCDF_READ_FILL_VALUE

   start_pos = 1
   counter = size(val,1)
   stride = 1

   if (present(start)) then
      start_pos = start
   end if

#include "ncdf_open_field.inc"

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i = 1, size(ind)
         arr => val(i:i)

         start_pos(dim) = ind(i)
#include "ncdf_read_packed_field.inc"
      end do
   else
      ! read everything
      arr => val
#include "ncdf_read_packed_field.inc"
   end if

end subroutine NCDF_READ_PACKED_NAME_1D


subroutine NCDF_READ_PACKED_NAME_2D(ncid, name, val, dim, ind, start)
   implicit none

   integer,                                     intent(in)    :: ncid
   character(len=*),                            intent(in)    :: name
   NCDF_READ_TYPE(kind=NCDF_READ_KIND), target, intent(inout) :: val(:,:)
   integer, optional,                           intent(in)    :: dim, ind(:)
   integer, optional,                           intent(in)    :: start(:)

   integer                                          :: ierr, vid, i
   integer, dimension(2)                            :: start_pos, counter, stride
   logical                                          :: fv_flag, vr_flag
#ifdef DEBUG
   character(len=unitlength)                        :: unit
#endif
   NCDF_RD_P_TYPE(kind=NCDF_RD_P_KIND)              :: fv, vmin, vmax
   NCDF_RD_P_TYPE(kind=NCDF_RD_P_KIND), allocatable :: arr_packed(:,:)
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)              :: sf, of
   NCDF_READ_TYPE(kind=NCDF_READ_KIND), pointer     :: arr(:,:)
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)              :: fill=NCDF_READ_FILL_VALUE

   start_pos = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   stride = 1

   if (present(start)) then
      start_pos = start
   end if

#include "ncdf_open_field.inc"

   if (present(dim)) then
      ! only read some channels from the array

      select case (dim)
      case(1)
         allocate(arr_packed(1, counter(2)))
      case(2)
         allocate(arr_packed(counter(1), 1))
      end select

      counter(dim) = 1
      do i = 1, size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:)
         case(2)
            arr => val(:,i:i)
         end select

         start_pos(dim) = ind(i)
#include "ncdf_read_packed_field.inc"
      end do
   else
      ! read everything
      allocate(arr_packed(counter(1), counter(2)))
      arr => val
#include "ncdf_read_packed_field.inc"
   end if

   deallocate(arr_packed)

end subroutine NCDF_READ_PACKED_NAME_2D


subroutine NCDF_READ_PACKED_NAME_3D(ncid, name, val, dim, ind, start)
   implicit none

   integer,           intent(in)                              :: ncid
   character(len=*),  intent(in)                              :: name
   NCDF_READ_TYPE(kind=NCDF_READ_KIND), target, intent(inout) :: val(:,:,:)
   integer, optional, intent(in)                              :: dim, ind(:)
   integer, optional, intent(in)                              :: start(:)

   integer                                          :: ierr, vid, i
   integer, dimension(3)                            :: start_pos, counter, stride
   logical                                          :: fv_flag, vr_flag
#ifdef DEBUG
   character(len=unitlength)                        :: unit
#endif
   NCDF_RD_P_TYPE(kind=NCDF_RD_P_KIND)              :: fv, vmin, vmax
   NCDF_RD_P_TYPE(kind=NCDF_RD_P_KIND), allocatable :: arr_packed(:,:,:)
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)              :: sf, of
   NCDF_READ_TYPE(kind=NCDF_READ_KIND), pointer     :: arr(:,:,:)
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)              :: fill=NCDF_READ_FILL_VALUE

   start_pos = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   stride = 1

   if (present(start)) then
      start_pos = start
   end if

#include "ncdf_open_field.inc"

   if (present(dim)) then
      ! only read some channels from the array

      select case (dim)
      case(1)
         allocate(arr_packed(1, counter(2), counter(3)))
      case(2)
         allocate(arr_packed(counter(1), 1, counter(3)))
      case(3)
         allocate(arr_packed(counter(1), counter(2), 1))
      end select

      counter(dim) = 1
      do i = 1, size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:,:)
         case(2)
            arr => val(:,i:i,:)
         case(3)
            arr => val(:,:,i:i)
         end select

         start_pos(dim) = ind(i)
#include "ncdf_read_field.inc"
      end do
   else
      ! read everything
      allocate(arr_packed(counter(1), counter(2), counter(3)))
      arr => val
#include "ncdf_read_field.inc"
   end if

   deallocate(arr_packed)

end subroutine NCDF_READ_PACKED_NAME_3D


subroutine NCDF_READ_PACKED_NAME_4D(ncid, name, val, dim, ind, start)
   implicit none

   integer,                                     intent(in)    :: ncid
   character(len=*),                            intent(in)    :: name
   NCDF_READ_TYPE(kind=NCDF_READ_KIND), target, intent(inout) :: val(:,:,:,:)
   integer, optional,                           intent(in)    :: dim, ind(:)
   integer, optional,                           intent(in)    :: start(:)

   integer                                          :: ierr, vid, i
   integer, dimension(4)                            :: start_pos, counter, stride
   logical                                          :: fv_flag, vr_flag
#ifdef DEBUG
   character(len=unitlength)                        :: unit
#endif
   NCDF_RD_P_TYPE(kind=NCDF_RD_P_KIND)              :: fv, vmin, vmax
   NCDF_RD_P_TYPE(kind=NCDF_RD_P_KIND), allocatable :: arr_packed(:,:,:,:)
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)              :: sf, of
   NCDF_READ_TYPE(kind=NCDF_READ_KIND), pointer     :: arr(:,:,:,:)
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)              :: fill=NCDF_READ_FILL_VALUE

   start_pos = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   counter(4) = size(val,4)
   stride = 1

   if (present(start)) then
      start_pos = start
   end if

#include "ncdf_open_field.inc"

   if (present(dim)) then
      ! only read some channels from the array
      select case (dim)
      case(1)
         allocate(arr_packed(1, counter(2), counter(3), counter(4)))
      case(2)
         allocate(arr_packed(counter(1), 1, counter(3), counter(4)))
      case(3)
         allocate(arr_packed(counter(1), counter(2), 1, counter(4)))
      case(4)
         allocate(arr_packed(counter(1), counter(2), counter(3), 1))
      end select

      counter(dim) = 1
      do i = 1, size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:,:,:)
         case(2)
            arr => val(:,i:i,:,:)
         case(3)
            arr => val(:,:,i:i,:)
         case(4)
            arr => val(:,:,:,i:i)
         end select

         start_pos(dim) = ind(i)
#include "ncdf_read_field.inc"
      end do
   else
      ! read everything
      allocate(arr_packed(counter(1), counter(2), counter(3), counter(4)))
      arr => val
#include "ncdf_read_field.inc"
   end if

   deallocate(arr_packed)

end subroutine NCDF_READ_PACKED_NAME_4D


subroutine NCDF_READ_PACKED_NAME_5D(ncid, name, val, dim, ind, start)
   implicit none

   integer,                                     intent(in)    :: ncid
   character(len=*),                            intent(in)    :: name
   NCDF_READ_TYPE(kind=NCDF_READ_KIND), target, intent(inout) :: val(:,:,:,:,:)
   integer, optional,                           intent(in)    :: dim, ind(:)
   integer, optional,                           intent(in)    :: start(:)

   integer                                          :: ierr, vid, i
   integer, dimension(5)                            :: start_pos, counter, stride
   logical                                          :: fv_flag, vr_flag
#ifdef DEBUG
   character(len=unitlength)                        :: unit
#endif
   NCDF_RD_P_TYPE(kind=NCDF_RD_P_KIND)              :: fv, vmin, vmax
   NCDF_RD_P_TYPE(kind=NCDF_RD_P_KIND), allocatable :: arr_packed(:,:,:,:,:)
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)              :: sf, of
   NCDF_READ_TYPE(kind=NCDF_READ_KIND), pointer     :: arr(:,:,:,:,:)
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)              :: fill=NCDF_READ_FILL_VALUE

   start_pos = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   counter(4) = size(val,4)
   counter(5) = size(val,5)
   stride = 1

   if (present(start)) then
      start_pos = start
   end if

#include "ncdf_open_field.inc"

   if (present(dim)) then
      ! only read some channels from the array
      select case (dim)
      case(1)
         allocate(arr_packed(1, counter(2), counter(3), counter(4), counter(5)))
      case(2)
         allocate(arr_packed(counter(1), 1, counter(3), counter(4), counter(5)))
      case(3)
         allocate(arr_packed(counter(1), counter(2), 1, counter(4), counter(5)))
      case(4)
         allocate(arr_packed(counter(1), counter(2), counter(3), 1, counter(5)))
      case(5)
         allocate(arr_packed(counter(1), counter(2), counter(3), counter(4), 1))
      end select

      counter(dim) = 1
      do i = 1, size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:,:,:,:)
         case(2)
            arr => val(:,i:i,:,:,:)
         case(3)
            arr => val(:,:,i:i,:,:)
         case(4)
            arr => val(:,:,:,i:i,:)
         case(5)
            arr => val(:,:,:,:,i:i)
         end select

         start_pos(dim) = ind(i)
#include "ncdf_read_field.inc"
      end do
   else
      ! read everything
      allocate(arr_packed(counter(1), counter(2), counter(3), counter(4), counter(5)))
      arr => val
#include "ncdf_read_field.inc"
   end if

   deallocate(arr_packed)

end subroutine NCDF_READ_PACKED_NAME_5D


subroutine NCDF_READ_PACKED_NAME_6D(ncid, name, val, dim, ind, start)
   implicit none

   integer,                                     intent(in)    :: ncid
   character(len=*),                            intent(in)    :: name
   NCDF_READ_TYPE(kind=NCDF_READ_KIND), target, intent(inout) :: val(:,:,:,:,:,:)
   integer, optional,                           intent(in)    :: dim, ind(:)
   integer, optional,                           intent(in)    :: start(:)

   integer                                          :: ierr, vid, i
   integer, dimension(6)                            :: start_pos, counter, stride
   logical                                          :: fv_flag, vr_flag
#ifdef DEBUG
   character(len=unitlength)                        :: unit
#endif
   NCDF_RD_P_TYPE(kind=NCDF_RD_P_KIND)              :: fv, vmin, vmax
   NCDF_RD_P_TYPE(kind=NCDF_RD_P_KIND), allocatable :: arr_packed(:,:,:,:,:,:)
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)              :: sf, of
   NCDF_READ_TYPE(kind=NCDF_READ_KIND), pointer     :: arr(:,:,:,:,:,:)
   NCDF_READ_TYPE(kind=NCDF_READ_KIND)              :: fill=NCDF_READ_FILL_VALUE

   start_pos = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   counter(4) = size(val,4)
   counter(5) = size(val,5)
   counter(6) = size(val,6)
   stride = 1

   if (present(start)) then
      start_pos = start
   end if

#include "ncdf_open_field.inc"

   if (present(dim)) then
      ! only read some channels from the array
      select case (dim)
      case(1)
         allocate(arr_packed(1, counter(2), counter(3), counter(4), counter(5), counter(6)))
      case(2)
         allocate(arr_packed(counter(1), 1, counter(3), counter(4), counter(5), counter(6)))
      case(3)
         allocate(arr_packed(counter(1), counter(2), 1, counter(4), counter(5), counter(6)))
      case(4)
         allocate(arr_packed(counter(1), counter(2), counter(3), 1, counter(5), counter(6)))
      case(5)
         allocate(arr_packed(counter(1), counter(2), counter(3), counter(4), 1, counter(6)))
      case(6)
         allocate(arr_packed(counter(1), counter(2), counter(3), counter(4), counter(5), 1))
      end select

      counter(dim) = 1
      do i = 1, size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:,:,:,:,:)
         case(2)
            arr => val(:,i:i,:,:,:,:)
         case(3)
            arr => val(:,:,i:i,:,:,:)
         case(4)
            arr => val(:,:,:,i:i,:,:)
         case(5)
            arr => val(:,:,:,:,i:i,:)
         case(6)
            arr => val(:,:,:,:,:,i:i)
         end select

         start_pos(dim) = ind(i)
#include "ncdf_read_field.inc"
      end do
   else
      ! read everything
      allocate(arr_packed(counter(1), counter(2), counter(3), counter(4), counter(5), counter(6)))
      arr => val
#include "ncdf_read_field.inc"
   end if

   deallocate(arr_packed)

end subroutine NCDF_READ_PACKED_NAME_6D

#endif
