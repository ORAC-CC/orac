! Name:
!   Read_ByteArray
!
! Purpose:
!   Reads generic three dimensional byte array given file unit number,
!   array dimensions, and the required channel indices.
!   Data files should be sequential unformatted.
!
! Arguments:
!   Name       Type         In/Out   Description
!   lun        int          In       File unit number
!   X          int          In       Array dimension x (x-pixels)
!   Y          int          In       Array dimension y (y-pixels)
!   Z          int          In       Array dimension z (channels)
!   ch_index   int array    In       Indices of channels to use
!   out        byte array   Out      Output array
!   row        int          Out      Number of last row of pixels read from the
!                                    latest segment. Used for error checking if
!                                    end of file detected.
!   ios        int          Out      I/O read status                        
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
!   array      int(1) array  2-D byte array: holds 1 "row" of image 
!                            data after reading from the file, i.e. all channel 
!                            values at each x location for a given y.
!                            Dimensions are (channels, x).
!
! History:
!   3rd November, 2000, Kevin M. Smith : original version
!   2nd August 2001, Andy Smith:
!      Updated to read array of data row by row rather than in a single, 
!      whole array read. This will allow reading and processing of images 
!      in segments. 
!      Replaced arrays declared as "byte" with "integer(kind=1)". 
!      Probably more portable.
!   10th Aug 2001, Andy Smith:
!      Basic image segmentation working. Currently works only if the segment 
!      size fits a whole number of times into the image, which excludes some
!      super-pixel sizes. Updating to load the out array as each row is read
!      in. This will enable the routine to handle end of file during segment
!      read, which is necessary if all SPixel sizes are to be allowable.
!      Added row argument.  
!
! Bugs:
!   None known.
!
!------------------------------------------------------------------------------------
subroutine Read_ByteArray(lun, X, Y, Z, ch_index, out, row, ios)

   implicit none

!  Argument declarations

   integer, intent(in)          :: lun, X, Y, Z
   integer, intent(in)          :: ch_index(Z)
   integer(kind=1), intent(out) :: out(X, Y, Z)
   integer, intent(out)         :: row   
   integer, intent(out)         :: ios   

!  Local variables

   integer            :: i, k
   integer(kind=1)    :: array(ch_index(Z), X)

!  Read 3-D array in 2-D sections: each section in the new form (2nd Aug 2001)
!  is a row at a fixed y (latitude?), in which x varies and all channel values
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
      do i=1,Z
         out(:, row, k) = array(ch_index(i),:) 
	 k = k + 1      
      end do
   end do

end subroutine Read_ByteArray
