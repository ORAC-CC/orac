! Name:
!   Read_INTArray
!
! Purpose:
!   Reads three dimensional Integer array given file unit number,
!   array dimensions, and the required channel indices.
!   Data files should be sequential unformatted, stored so that all channel
!   measurements are available at each image pixel.
!
! Arguments:
!   Name       Type          In/Out/Both   Description
!   lun        int           In            File unit number
!   X          int           In            Array dimension x (x-pixels)
!   Y          int           In            Array dimension y (y-pixels)
!   Zin        int           In            Input array dimension z (channels):
!                                          the file stores data for all 
!                                          channels.
!   Zout       int           In            Output array dimension z (channels),
!                                          depends on user channel selection.
!   ch_index   int array     In            Indices of channels to use
!   out        float array   Out           Output array
!   row        int           Out           Image row counter, used for error
!                                          checking if end of file detected.
!   ios        int           Out           I/O read status                        
!    
! Algorithm:
!   For each image row (1 to Y)
!      read a row of data (all channels at each X) into array
!      copy the row into array2, swapping channel values from 2nd dimension of
!        array into the 1st dimension of array2
!
!   if (no read errors)
!      step through possible channel values
!         if (channel number in ch_index array)
!            copy channel values from array2 to out array
!
! Local variables:
!   Name       Type          Description
!   i          int           Counter: used to count through "channel" values at 
!                            each pixel location within "array".
!   k          int           Counter: used for indexing channel values in the
!                            out array (different from the values in array since
!                            array holds all chans, out holds user selection).
!   array      float array   2-D floating point array: holds 1 "row" of image 
!                            data after reading from the file, i.e. all channel 
!                            values at each x location for a given y.
!                            Dimensions are (channels, x).
!
! History:
!   2nd November, 2000, Kevin M. Smith : original version
!   27th July 2001, Andy Smith:
!      Checked out to experiment with different ways of reading in data for
!      image segmentation.
!   10th Aug 2001, Andy Smith:
!      Basic image segmentation working. Currently works only if the segment 
!      size fits a whole number of times into the image, which excludes some
!      super-pixel sizes. Updating to load the out array as each row is read
!      in. This will enable the routine to handle end of file during segment
!      read, which is necessary if all SPixel sizes are to be allowable.
!   23rd Aug 2001, Andy Smith:
!      Bug fix: two Zs required: Zin and Zout. The temporary array "array" used
!      to read from the file must be sized to hold all instrument channels in 
!      the input file. The output array is sized according to the user's 
!      channels selection.
!   1st August 2002, Caroline Poulsen:
!      Adapted ReadFParray to ReadINT array to cope with reading
!      integer albedo values
! Bugs:
!   None known.
!
!------------------------------------------------------------------------------------
subroutine Read_INTArray(lun, X, Y, Zin, Zout, ch_index, out, row, ios)

   implicit none

!  Argument declarations

   integer, intent(in)   :: lun, X, Y, Zin, Zout
   integer, intent(in)   :: ch_index(Zout)
   integer, intent(out)     :: out(X, Y, Zout)
   integer, intent(out)  :: row
   integer, intent(out)  :: ios

!  Local variables

   integer :: i, k
   integer    :: array(Zin, X)  ! Holds 1 row of data from the file

!  Read 3-D array in 2-D sections: each section in the new form (27th July 2001)
!  is a row at a fixed y ("latitude"), in which x varies and all channel values
!  for a given (x,y) are stored together. Read in a row at a time (up to the 
!  required segment size), copying to the out array only the channels 
!  requested, swapping the order of the dimensions so that channels become "z".
!  ios is used to detect end of file as well as read errors (the ios value is
!  dealt with by the calling routine). Negative ios indicates end of record or
!  end of file during read. If the EOF occurs in the correct place the last
!  required row will already have been read in and loaded into out.

   do row=1,Y
      read(unit=lun, iostat=ios) array ! Read a row: all channels for eacb x
      if (ios /= 0) exit
      
      k = 1
      do i=1, Zout
         out(:, row, k) = array(ch_index(i),:) 
	 k = k + 1      


      end do
   end do

end subroutine Read_INTArray
