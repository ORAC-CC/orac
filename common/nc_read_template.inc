subroutine NC_READ_NAME_1D(ncid, name, val, verbose, dim, ind, startp)
   implicit none

   integer,                                 intent(in)    :: ncid
   character(len=*),                        intent(in)    :: name
   NC_READ_TYPE(kind=NC_READ_KIND), target, intent(inout) :: val(:)
   logical,                                 intent(in)    :: verbose
   integer, optional,                       intent(in)    :: dim, ind(:)
   integer, optional,                       intent(in)    :: startp(:)

   integer                                  :: ierr, vid, i
   integer, dimension(1)                    :: start, counter, stride
   logical                                  :: fv_flag, vr_flag
   character(len=unitlength)                :: unit
   NC_READ_TYPE(kind=NC_READ_KIND)          :: fv, sf, of, vmin, vmax
   NC_READ_TYPE(kind=NC_READ_KIND), pointer :: arr(:)
   NC_READ_TYPE(kind=NC_READ_KIND)          :: fill=NC_READ_FILL_VALUE

   start = 1
   counter = size(val,1)
   stride = 1

   if (present(startp)) then
   	start=startp
   endif

   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i=1,size(ind)
         arr => val(i:i)

         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      arr => val
      include 'nc_read_field.inc'
   end if

end subroutine NC_READ_NAME_1D


subroutine NC_READ_NAME_2D(ncid, name, val, verbose, dim, ind, startp)
   implicit none

   integer,           intent(in)                          :: ncid
   character(len=*),  intent(in)                          :: name
   NC_READ_TYPE(kind=NC_READ_KIND), target, intent(inout) :: val(:,:)
   logical,           intent(in)                          :: verbose
   integer, optional, intent(in)                          :: dim, ind(:)
   integer, optional, intent(in)                          :: startp(:)

   integer                                  :: ierr, vid, i
   integer, dimension(2)                    :: start, counter, stride
   logical                                  :: fv_flag, vr_flag
   character(len=unitlength)                :: unit
   NC_READ_TYPE(kind=NC_READ_KIND)          :: fv, sf, of, vmin, vmax
   NC_READ_TYPE(kind=NC_READ_KIND), pointer :: arr(:,:)
   NC_READ_TYPE(kind=NC_READ_KIND)          :: fill=NC_READ_FILL_VALUE

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   stride = 1

   if (present(startp)) then
   	start=startp
   endif

   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i=1,size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:)
         case(2)
            arr => val(:,i:i)
         end select

         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      arr => val
      include 'nc_read_field.inc'
   end if

end subroutine NC_READ_NAME_2D


subroutine NC_READ_NAME_3D(ncid, name, val, verbose, dim, ind, startp)
   implicit none

   integer,           intent(in)                          :: ncid
   character(len=*),  intent(in)                          :: name
   NC_READ_TYPE(kind=NC_READ_KIND), target, intent(inout) :: val(:,:,:)
   logical,           intent(in)                          :: verbose
   integer, optional, intent(in)                          :: dim, ind(:)
   integer, optional, intent(in)                          :: startp(:)

   integer                                  :: ierr, vid, i
   integer, dimension(3)                    :: start, counter, stride
   logical                                  :: fv_flag, vr_flag
   character(len=unitlength)                :: unit
   NC_READ_TYPE(kind=NC_READ_KIND)          :: fv, sf, of, vmin, vmax
   NC_READ_TYPE(kind=NC_READ_KIND), pointer :: arr(:,:,:)
   NC_READ_TYPE(kind=NC_READ_KIND)          :: fill=NC_READ_FILL_VALUE

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   stride = 1

   if (present(startp)) then
   	start=startp
   endif

   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i=1,size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:,:)
         case(2)
            arr => val(:,i:i,:)
         case(3)
            arr => val(:,:,i:i)
         end select

         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      arr => val
      include 'nc_read_field.inc'
   end if

end subroutine NC_READ_NAME_3D


subroutine NC_READ_NAME_4D(ncid, name, val, verbose, dim, ind, startp)
   implicit none

   integer,                                 intent(in)    :: ncid
   character(len=*),                        intent(in)    :: name
   NC_READ_TYPE(kind=NC_READ_KIND), target, intent(inout) :: val(:,:,:,:)
   logical,                                 intent(in)    :: verbose
   integer, optional,                       intent(in)    :: dim, ind(:)
   integer, optional, intent(in)                          :: startp(:)

   integer                                  :: ierr, vid, i
   integer, dimension(4)                    :: start, counter, stride
   logical                                  :: fv_flag, vr_flag
   character(len=unitlength)                :: unit
   NC_READ_TYPE(kind=NC_READ_KIND)          :: fv, sf, of, vmin, vmax
   NC_READ_TYPE(kind=NC_READ_KIND), pointer :: arr(:,:,:,:)
   NC_READ_TYPE(kind=NC_READ_KIND)          :: fill=NC_READ_FILL_VALUE

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   counter(4) = size(val,4)
   stride = 1

   if (present(startp)) then
   	start=startp
   endif

   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i=1,size(ind)
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

         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      arr => val
      include 'nc_read_field.inc'
   end if

end subroutine NC_READ_NAME_4D


#ifdef NC_RD_P_TYPE

subroutine NC_READ_PACKED_NAME_1D(ncid, name, val, verbose, dim, ind, startp)
   implicit none

   integer,                                 intent(in)    :: ncid
   character(len=*),                        intent(in)    :: name
   NC_READ_TYPE(kind=NC_READ_KIND), target, intent(inout) :: val(:)
   logical,                                 intent(in)    :: verbose
   integer, optional,                       intent(in)    :: dim, ind(:)
   integer, optional, intent(in)                          :: startp(:)

   integer                                  :: ierr, vid, i
   integer, dimension(1)                    :: start, counter, stride
   logical                                  :: fv_flag, vr_flag
   character(len=unitlength)                :: unit
   NC_RD_P_TYPE(kind=NC_RD_P_KIND)          :: fv, vmin, vmax
   NC_RD_P_TYPE(kind=NC_RD_P_KIND)          :: arr_packed(1)
   NC_READ_TYPE(kind=NC_READ_KIND)          :: sf, of
   NC_READ_TYPE(kind=NC_READ_KIND), pointer :: arr(:)
   NC_READ_TYPE(kind=NC_READ_KIND)          :: fill=NC_READ_FILL_VALUE

   start = 1
   counter = size(val,1)
   stride = 1

   if (present(startp)) then
   	start=startp
   endif

   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array
      counter(dim) = 1
      do i=1,size(ind)
         arr => val(i:i)

         start(dim) = ind(i)
         include 'nc_read_packed_field.inc'
      end do
   else
      ! read everything
      arr => val
      include 'nc_read_packed_field.inc'
   end if

end subroutine NC_READ_PACKED_NAME_1D


subroutine NC_READ_PACKED_NAME_2D(ncid, name, val, verbose, dim, ind, startp)
   implicit none

   integer,                                 intent(in)    :: ncid
   character(len=*),                        intent(in)    :: name
   NC_READ_TYPE(kind=NC_READ_KIND), target, intent(inout) :: val(:,:)
   logical,                                 intent(in)    :: verbose
   integer, optional,                       intent(in)    :: dim, ind(:)
   integer, optional,                       intent(in)    :: startp(:)

   integer                                      :: ierr, vid, i
   integer, dimension(2)                        :: start, counter, stride
   logical                                      :: fv_flag, vr_flag
   character(len=unitlength)                    :: unit
   NC_RD_P_TYPE(kind=NC_RD_P_KIND)              :: fv, vmin, vmax
   NC_RD_P_TYPE(kind=NC_RD_P_KIND), allocatable :: arr_packed(:,:)
   NC_READ_TYPE(kind=NC_READ_KIND)              :: sf, of
   NC_READ_TYPE(kind=NC_READ_KIND), pointer     :: arr(:,:)
   NC_READ_TYPE(kind=NC_READ_KIND)              :: fill=NC_READ_FILL_VALUE

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   stride = 1

   if (present(startp)) then
   	start=startp
   endif

   include 'nc_open_field.inc'

   if (present(dim)) then
      ! only read some channels from the array

      select case (dim)
      case(1)
         allocate(arr_packed(1, counter(2)))
      case(2)
         allocate(arr_packed(counter(1), 1))
      end select

      counter(dim) = 1
      do i=1,size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:)
         case(2)
            arr => val(:,i:i)
         end select

         start(dim) = ind(i)
         include 'nc_read_packed_field.inc'
      end do
   else
      ! read everything
      allocate(arr_packed(counter(1), counter(2)))
      arr => val
      include 'nc_read_packed_field.inc'
   end if

   deallocate(arr_packed)

end subroutine NC_READ_PACKED_NAME_2D


subroutine NC_READ_PACKED_NAME_3D(ncid, name, val, verbose, dim, ind, startp)
   implicit none

   integer,           intent(in)                          :: ncid
   character(len=*),  intent(in)                          :: name
   NC_READ_TYPE(kind=NC_READ_KIND), target, intent(inout) :: val(:,:,:)
   logical,           intent(in)                          :: verbose
   integer, optional, intent(in)                          :: dim, ind(:)
   integer, optional, intent(in)                          :: startp(:)

   integer                                      :: ierr, vid, i
   integer, dimension(3)                        :: start, counter, stride
   logical                                      :: fv_flag, vr_flag
   character(len=unitlength)                    :: unit
   NC_RD_P_TYPE(kind=NC_RD_P_KIND)              :: fv, vmin, vmax
   NC_RD_P_TYPE(kind=NC_RD_P_KIND), allocatable :: arr_packed(:,:,:)
   NC_READ_TYPE(kind=NC_READ_KIND)              :: sf, of
   NC_READ_TYPE(kind=NC_READ_KIND), pointer     :: arr(:,:,:)
   NC_READ_TYPE(kind=NC_READ_KIND)              :: fill=NC_READ_FILL_VALUE

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   stride = 1

   if (present(startp)) then
   	start=startp
   endif

   include 'nc_open_field.inc'

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
      do i=1,size(ind)
         select case (dim)
         case(1)
            arr => val(i:i,:,:)
         case(2)
            arr => val(:,i:i,:)
         case(3)
            arr => val(:,:,i:i)
         end select

         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      allocate(arr_packed(counter(1), counter(2), counter(3)))
      arr => val
      include 'nc_read_field.inc'
   end if

   deallocate(arr_packed)

end subroutine NC_READ_PACKED_NAME_3D


subroutine NC_READ_PACKED_NAME_4D(ncid, name, val, verbose, dim, ind, startp)
   implicit none

   integer,                                 intent(in)    :: ncid
   character(len=*),                        intent(in)    :: name
   NC_READ_TYPE(kind=NC_READ_KIND), target, intent(inout) :: val(:,:,:,:)
   logical,                                 intent(in)    :: verbose
   integer, optional,                       intent(in)    :: dim, ind(:)
   integer, optional,                       intent(in)    :: startp(:)

   integer                                      :: ierr, vid, i
   integer, dimension(4)                        :: start, counter, stride
   logical                                      :: fv_flag, vr_flag
   character(len=unitlength)                    :: unit
   NC_RD_P_TYPE(kind=NC_RD_P_KIND)              :: fv, vmin, vmax
   NC_RD_P_TYPE(kind=NC_RD_P_KIND), allocatable :: arr_packed(:,:,:,:)
   NC_READ_TYPE(kind=NC_READ_KIND)              :: sf, of
   NC_READ_TYPE(kind=NC_READ_KIND), pointer     :: arr(:,:,:,:)
   NC_READ_TYPE(kind=NC_READ_KIND)              :: fill=NC_READ_FILL_VALUE

   start = 1
   counter(1) = size(val,1)
   counter(2) = size(val,2)
   counter(3) = size(val,3)
   counter(4) = size(val,4)
   stride = 1

   if (present(startp)) then
   	start=startp
   endif

   include 'nc_open_field.inc'

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
      do i=1,size(ind)
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

         start(dim) = ind(i)
         include 'nc_read_field.inc'
      end do
   else
      ! read everything
      allocate(arr_packed(counter(1), counter(2), counter(3), counter(4)))
      arr => val
      include 'nc_read_field.inc'
   end if

   deallocate(arr_packed)

end subroutine NC_READ_PACKED_NAME_4D

#endif
