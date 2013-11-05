; $Id: //depot/idl/IDL_71/idldir/lib/read_gif.pro#1 $
;
; Copyright (c) 1992-2009, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.

;----------------------------------------------------------------------
;
;  GifReadByte
;       Read a single byte out of the given file
;
FUNCTION GifReadByte, unit
        COMPILE_OPT hidden

        ch      = 0b
        READU, unit, ch
        RETURN, ch
END

;----------------------------------------------------------------------
;+
; NAME:
;       READ_GIF
;
; PURPOSE:
;       Read the contents of a GIF format image file and return the image
;       and color table vectors (if present) in the form of IDL variables.
;
; CATEGORY:
;       Input/Output.
;
; CALLING SEQUENCE:
;       READ_GIF, File, Image [, R, G, B]
;
; INPUTS:
;       File:   Scalar string giving the name of the rasterfile to read
;
; OUTPUTS:
;       Image:  The 2D byte array to contain the image.
;
; OPTIONAL OUTPUT PARAMETERS:
;    R, G, B:  The variables to contain the red, green, and blue color
;        vectors if the rasterfile containes colormaps. When reading the
;        second and subsequent images, R, G, and B are not returned
;        unless there is a local color table for that image.
;
; Keyword Inputs:
;    BACKGROUND_COLOR = Set this keyword to a named variable in which to
;        return the index of the background color within the global color
;        table.
;
;    CLOSE = if set, closes any open file if the MULTIPLE images
;        per file mode was used.  If this keyword is present,
;        nothing is read, and all other parameters are ignored.
;
;    DELAY_TIME = set this keyword to a named variable in which to return
;        the delay in hundredths (1/100) of a second that the decoder should
;        wait after displaying the current image. If the delay time is not
;        defined in the file then a value of 0 will be returned.
;        For multiple images there may be a different value for each image
;        within the file.
;
;    DISPOSAL_METHOD = set this keyword to a named variable in which to return
;        the disposal method that the decoder should use after displaying
;        the current image. Possible values are:
;        0: No disposal specified. The decoder is not required
;           to take any action.
;        1: Do not dispose. The graphic is to be left in place.
;        2: Restore to background color. The area used by the
;           graphic must be restored to the background color.
;        3: Restore to previous. The decoder is required to
;           restore the area overwritten by the graphic with
;           what was there prior to rendering the graphic.
;        4-7: Not currently defined by the Gif89a spec.
;        For multiple images there may be a different value for each image
;        within the file.
;
;    MULTIPLE = if set, read files containing multiple images per
;        file. Each call to READ_GIF returns the next image,
;        with the file remaining open between calls. The File
;        parameter is ignored after the first call. Reading
;        past the last image returns a scalar value of -1 in IMAGE, and
;        closes the file.
;
;    REPEAT_COUNT = set this keyword to a named variable in which to return
;        the repeat count for the animation within the file. A repeat count
;        of zero indicates an infinite repeat. If the repeat count is
;        not defined within the file (the Netscape application extension is
;        not present) then a value of -1 is returned.
;
;    TRANSPARENT = Set this keyword to a byte value giving the index
;        within the color table to be designated as the transparent color.
;        If there is no transparency then a value of -1 is returned.
;        For multiple images there may be a different value for each image
;        within the file.
;
;    USER_INPUT = Set this keyword to a named variable in which to return
;        the user input flag for the current image. A value of 0 indicates
;        that no user input should be required, while a value of 1 indicates
;        that the application should wait before continuing processing.
;        The nature of the user input is determined by the application
;        (Carriage Return, Mouse Button Click, etc.).
;        When both DELAY_TIME and USER_INPUT are present, the decoder
;        should continue processing when user input is received or
;        when the delay time expires, whichever occurs first.
;        For multiple images there may be a different value for each image
;        within the file.
;
; SIDE EFFECTS:
;       None.
;
; COMMON BLOCKS:
;       READ_GIF_COMMON.
;
; RESTRICTIONS:
;       The Graphics Interchange Format(c) is the Copyright property
;       of CompuServ Incorporated.  GIF(sm) is a Service Mark property of
;       CompuServ Incorporated.
;
; EXAMPLE:
;       To open and read the GIF image file named "foo.gif" in the current
;       directory, store the image in the variable IMAGE1, and store the color
;       vectors in the variables R, G, and B, enter:
;
;               READ_GIF, "foo.gif", IMAGE1, R, G, B
;
;       To load the new color table and display the image, enter:
;
;               TVLCT, R, G, B
;               TV, IMAGE1
;
; MODIFICATION HISTORY:
;       Written June 1992, JWG
;       Added GIF89a and interlaced format, Jan, 1995, DMS.
;       Added MULTIPLE and CLOSE, Aug, 1996.
; 	August, 2000  KDB
;	 - Fixed issues with multiple image files that contain
;	   images of differing sizes.
;	 - Cleaned up the formatting and added comments.
;	 - Removed junk reads used to skip data and made
;	   use of point_lun (save some memory cycles).
;   May 2006, CT: Added BACKGROUND_COLOR, DELAY_TIME, DISPOSAL_METHOD,
;       REPEAT_COUNT, TRANSPARENT, USER_INPUT keywords.
;       Added support for local colormaps. Improved error handling.
;-
;
pro read_gif, file, image, r, g, b, $
    BACKGROUND_COLOR=backgroundColor, $
    CLOSE=close, $
    DELAY_TIME=delayTime, $
    DISPOSAL_METHOD=disposalMethod, $
    MULTIPLE=mult, $
    REPEAT_COUNT=repeatCount, $
    TRANSPARENT=transparent, $
    USER_INPUT=userInput

    compile_opt idl2

    ; Define GIF header (and screen descriptor. Used for Multiple)
    COMMON READ_GIF_COMMON, unit, scrWidth, scrHeight, $
        globalBackground, errCount, prevImage, prevTransparent

   on_error, 2          ;Return to caller on errors

   if (N_Elements(unit) eq 0) then unit = -1
   if (N_Elements(errCount) eq 0) then errCount = 0
   image  = -1          ;No image read yet
   transparent = -1
   delayTime = 0L
   repeatCount = -1L
   disposalMethod = 0L
   userInput = 0L
   backgroundColor = 0

closeFile:
    if keyword_set(close) then begin
        if (unit gt 0) then FREE_LUN, unit
        unit = -1
        prevImage = 0
        return
    endif

    CATCH, errorIndex
    if (errorIndex NE 0) then begin
        CATCH,/CANCEL   ; no infinite loops...
        if (unit gt 0) then FREE_LUN, unit
        unit = -1
        if (errCount lt 5) then MESSAGE, !ERROR_STATE.MSG, /INFO
        MESSAGE, /RESET
        prevImage = 0
        return
     endif

   ;; Main GIF Header declaration.
   header = { magic	: bytarr(6),            $
              width_lo  : 0b,                   $
              width_hi  : 0b,                   $
              height_lo : 0b, 			$
              height_hi : 0b,                   $
              screen_info : 0b, 		$
	      background : 0b, 			$
	      reserved  : 0b }

   ;; local image header declaration
   ihdr  = {   left_lo         : 0B,           $
               left_hi         : 0B,           $
               top_lo          : 0B,           $
               top_hi          : 0B,           $
               iwidth_lo       : 0B,           $
               iwidth_hi       : 0B,           $
               iheight_lo      : 0B,           $
               iheight_hi      : 0B,           $
               image_info      : 0b }        ; its content

   if (keyword_set(mult) && unit gt 0) then goto, next_image

   if(unit gt 0)then $
      free_lun, unit

   OPENR, unit, file, /GET_LUN, /BLOCK
   READU, unit, header          ;Read gif header

    errCount = 0
    prevImage = 0

   ;; Check Magic in header: GIF87a or GIF89a.
   gif  = STRING(header.magic[0:2])
   vers = STRING(header.magic[3:5])

   if( gif NE 'GIF')then $
       MESSAGE, 'File ' + file + ' is not a GIF file.'

   if (vers ne '87a' && vers ne '89a') then $
        MESSAGE, /INFO, 'Unknown GIF Version: '+vers+'. Attempting to read...'

   ;; Get the virtual screen width and height

   scrWidth   = header.width_hi * 256L + header.width_lo
   scrHeight  = header.height_hi * 256L + header.height_lo

    ; header.screen_info has the following format:
    ; Bit 7: global color table flag
    ; Bits 6-4: color resolution (# of colors available in original image)
    ; Bit 3: sort flag, whether color table is sorted in decreasing importance
    ; Bits 2-0: size of global color table

   ; bits per pixel contained in the 3 least-significant bits.
   hasGlobalColorTable = (header.screen_info AND '80'X) NE 0
   globalColorTableBits  = (header.screen_info and 7) + 1
   color_map_size  = 2L ^ globalColorTableBits

   globalBackground = hasGlobalColorTable ? header.background : 0b

   ;; Read in the colormap (optional)
   ; Highest bit contains the global color table flag.
   if (hasGlobalColorTable) then begin
      map     = BYTARR(3,color_map_size, /NOZERO)
      READU, unit, map
      map     = transpose(map)
      r       = map[*,0]
      g       = map[*,1]
      b       = map[*,2]
   endif

   ;; Read the image description

next_image:

    backgroundColor = globalBackground

   while( 1 )do begin                ;; Read till we get a terminator
      cmd = GifReadByte(unit)        ;; Loop thru commands in file.

      case (cmd) of
      '3B'x:    begin                  ;; GIF trailer (0x3b)
           close = 1
           GOTO, closeFile
           END
      '2C'x:    begin                  ;; Image description (0x2c)
           readu,unit,ihdr

           ; Read local colormaps and override the global colormap.
           if ((ihdr.image_info and '80'X) ne 0) then begin
              lcolor_map_size = 2L^((ihdr.image_info and 7) + 1)
              map     = BYTARR(3,lcolor_map_size, /NOZERO)
              READU, unit, map
              map     = transpose(map)
              r       = map[*,0]
              g       = map[*,1]
              b       = map[*,2]
           endif

           ;; Size of this image?
           iWidth   = ihdr.iwidth_hi  * 256L + ihdr.iwidth_lo
           iHeight  = ihdr.iheight_hi * 256L + ihdr.iheight_lo

           ;; Allocate an array to hold the image
           image   = BYTARR(iWidth, iHeight, /NOZERO)

           ;; Now call special GIF-LZW routine hidden within IDL
           ;; to do the ugly serial bit stream decoding

           DECODE_GIF,unit,image           ; magic

           ;; This should be the 0 byte that ends the series:
           junk = GifReadByte(unit)

           if (junk ne 0 && errCount lt 5)then begin
              message,/info,'No trailing 0.'
              errCount++
           endif

           ;; Reorder rows in an interlaced image

           if((ihdr.image_info AND '40'X) NE 0 )then begin
              l = lindgen(iHeight)        ;Row indices...

              ;;  Gif interlace ordering

              p = [l[where(l mod 8 eq 0)], l[where(l mod 8 eq 4)], $
                   l[where(l mod 4 eq 2)], l[where(l and 1)]]

              tmpImage = bytarr(iWidth, iHeight, /NOZERO)
              l = iHeight-1
              for i=0, l do $
                  tmpImage[0, l-p[i]] = image[*,l-i]
              image = temporary(tmpImage)
            endif

            ; Ok, is this image the same size as the screen size (main image)?
            sameSize = 0
            if( iHeight ne scrHeight || iWidth ne scrWidth)then begin
                x0 = ihdr.left_hi * 256L + ihdr.left_lo
                y0 = ihdr.top_hi * 256L + ihdr.top_lo
                ; Sometimes the global image dimensions are wrong,
                ; so expand them if necessary.
                if (x0+iWidth gt scrWidth) then scrWidth = x0+iWidth
                if (y0+iHeight gt scrHeight) then scrHeight = y0+iHeight
                y0 = scrHeight - (y0 + iHeight) ; reverse the y coordinate

                ; If previous image exists, embed the image within it.
                sameSize = Array_Equal(Size(prevImage, /DIMENSIONS), $
                    [scrWidth, scrHeight])
                if (sameSize) then begin
                    newImage = prevImage
                    ; The transparent pixel index might have changed from
                    ; the previous image. If so, update it to the new value.
                    ; Note: This isn't exactly correct: values in the old
                    ; image may now have the same value as the new transparent
                    ; index, which will lead to incorrect display.
                    ; Not much we can do about this, as we are trying to
                    ; simply return an image, not display it.
                    if (prevTransparent ge 0 && transparent ge 0 && $
                        prevTransparent ne transparent) then begin
                        trans = Where(newImage eq prevTransparent, ntrans)
                        if (ntrans gt 0) then begin
                            newImage[trans] = transparent
                        endif
                    endif
                endif else begin
                    ; Construct a new image, using either the transparent
                    ; color or the background color.
                    value = transparent ge 0 ? transparent : globalBackground
                    newImage = Replicate(value, scrWidth, scrHeight)
                endelse
                ; Only overlay non-transparent pixels.
                if (transparent ge 0) then begin
                    trans = Where(image eq transparent, ntrans)
                    if (ntrans gt 0) then begin
                        imageBak = newImage[x0:x0+iWidth-1, y0:y0+iHeight-1]
                        image[trans] = imageBak[trans]
                        imageBak = 0
                    endif
                endif

                newImage[x0, y0] = image
                image = Temporary(newImage)
            endif

            if (keyword_set(mult)) then begin
                ; Cache the current image for use with the next subimage.
                ; The disposal methods are:
                ;   0 - No disposal specified.
                ;   1 - Do not dispose. The graphic is to be left in place.
                ;   2 - Restore to background color.
                ;   3 - Restore to previous.
                switch (disposalMethod) of
                0: ; fall thru
                1: begin  ; cache the entire image
                    prevImage = image
                    prevTransparent = transparent
                    break
                   end
                2: begin  ; restore the subimage pixels to background color
                    if (sameSize) then begin
                        prevImage[x0:x0+iWidth-1,y0:y0+iHeight-1] = globalBackground
                    endif
                    break
                   end
                else: ; do nothing
                endswitch
                return        ;Leave file open
            endif

            close = 1  ; otherwise close the file
            GOTO, closeFile
         end  ; Image description (0x2c)
     '21'x:    BEGIN              ;Gif Extension block (0x21)
            label = GifReadByte(unit)       ; extension block label
            switch (label) of
            'F9'x: begin  ; Graphic Control Extension (for transparency)
                gce = { $
                    blockSize: 0b, $  ; always 4
                    flags: 0b, $
                    delayTimeLo: 0b, $
                    delayTimeHi: 0b, $
                    transparent: 0b, $
                    terminator: 0b $
                }
                READU, unit, gce
                delayTime = gce.delayTimeHi*256L + gce.delayTimeLo
                ; Transparent flag is the 1st (lowest) bit
                if ((gce.flags and 1) ne 0) then transparent = gce.transparent
                ; User input is the 2nd bit
                userInput = Long((gce.flags and 2) ne 0)
                ; Disposal method is in bits 3-5
                disposalMethod = Long(IShft(gce.flags and '1C'x, -2))
                break
              end
            'FF'x: begin  ; Application extension (e.g. Netscape)
                blk_size = GifReadByte(unit)
                appExt = {name: Bytarr(8), $  ; e.g. "NETSCAPE"
                    code: Bytarr(3), $     ; e.g. "2.0"
                    length: 0b $            ; always 3
                    }
                READU, unit, appExt
                name = String(appExt.name)
                if (name eq 'NETSCAPE' && appExt.length eq 3b) then begin
                    data = {flag: 0b, $
                        repeatCountLo: 0b, $
                        repeatCountHi: 0b, $
                        terminator: 0b}
                    READU, unit, data
                    repeatCount = data.repeatCountHi*256L + data.repeatCountLo
                endif else begin
                    ; Unknown data, just skip over
                    blk_size = appExt.length + 1 ; include terminator
                    point_lun, (-unit), iPnt
                    point_lun, unit, iPnt + blk_size
                endelse
                break
                end
            'FE'x: ; fall thru, Comment extension
            else: begin ; ignore unknown extensions
                repeat begin
                    blk_size = GifReadByte(unit)
                    point_lun, (-unit), iPnt
                    point_lun, unit, iPnt + blk_size
                endrep until (blk_size eq 0)
                end
            endswitch
         end  ;Gif Extension block (0x21)

     ELSE: begin
        if (errCount lt 5) then begin
            message,/INFO, 'Unknown GIF keyword "' + $
                string(cmd, format='("0x",Z2.2)') + '" in ' + file
            errCount++
        endif
        GOTO, closeFile
        end
     ENDCASE
   endwhile

END
