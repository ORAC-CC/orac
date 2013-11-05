; $Id: //depot/idl/IDL_71/idldir/lib/congrid.pro#1 $
;
; Copyright (c) 1988-2009, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.
;
;+
; NAME:
;   CONGRID
;
; PURPOSE:
;       Shrink or expand the size of an array by an arbitrary amount.
;       This IDL procedure simulates the action of the VAX/VMS
;       CONGRID/CONGRIDI function.
;
;   This function is similar to "REBIN" in that it can resize a
;       one, two, or three dimensional array.   "REBIN", however,
;       requires that the new array size must be an integer multiple
;       of the original size.   CONGRID will resize an array to any
;       arbitrary size (REBIN is somewhat faster, however).
;       REBIN averages multiple points when shrinking an array,
;       while CONGRID just resamples the array.
;
; CATEGORY:
;       Array Manipulation.
;
; CALLING SEQUENCE:
;   array = CONGRID(array, x, y, z)
;
; INPUTS:
;       array:  A 1, 2, or 3 dimensional array to resize.
;               Data Type : Any type except string or structure.
;
;       x:      The new X dimension of the resized array.
;               Data Type : Int or Long (greater than or equal to 2).
;
; OPTIONAL INPUTS:
;       y:      The new Y dimension of the resized array.   If the original
;               array has only 1 dimension then y is ignored.   If the
;               original array has 2 or 3 dimensions then y MUST be present.
;
;       z:      The new Z dimension of the resized array.   If the original
;               array has only 1 or 2 dimensions then z is ignored.   If the
;               original array has 3 dimensions then z MUST be present.
;
; KEYWORD PARAMETERS:
;
;   CENTER: If this keyword is set, shift the interpolation so that points
;       in the input and output arrays are assumed to lie at the midpoint
;       of their coordinates rather than at their lower-left corner.
;
;   INTERP: If set, causes linear interpolation to be used.
;       Otherwise, the nearest-neighbor method is used.
;
;   CUBIC:  If specified and non-zero, "Cubic convolution"
;       interpolation is used.  This is a more
;       accurate, but more time-consuming, form of interpolation.
;       CUBIC has no effect when used with 3 dimensional arrays.
;       If this parameter is negative and non-zero, it specifies the
;       value of the cubic interpolation parameter as described
;       in the INTERPOLATE function.  Valid ranges are -1 <= Cubic < 0.
;       Positive non-zero values of CUBIC (e.g. specifying /CUBIC)
;       produce the default value of the interpolation parameter
;       which is -1.0.
;
;       MINUS_ONE:
;               If set, will prevent CONGRID from extrapolating one row or
;               column beyond the bounds of the input array.   For example,
;               If the input array has the dimensions (i, j) and the
;               output array has the dimensions (x, y), then by
;               default the array is resampled by a factor of (i/x)
;               in the X direction and (j/y) in the Y direction.
;               If MINUS_ONE is present (AND IS NON-ZERO) then the array
;               will be resampled by the factors (i-1)/(x-1) and (j-1)/(y-1).
;
; OUTPUTS:
;   The returned array has the same number of dimensions as the original
;       array and is of the same data type.   The returned array will have
;       the dimensions (x), (x, y), or (x, y, z) depending on how many
;       dimensions the input array had.
;
; PROCEDURE:
;       IF the input array has three dimensions, or if INTERP is set,
;       then the IDL interpolate function is used to interpolate the
;       data values.
;       If the input array has two dimensions, and INTERP is NOT set,
;       then the IDL POLY_2D function is used for nearest neighbor sampling.
;       If the input array has one dimension, and INTERP is NOT set,
;       then nearest neighbor sampling is used.
;
; EXAMPLE:
;       ; vol is a 3-D array with the dimensions (80, 100, 57)
;       ; Resize vol to be a (90, 90, 80) array
;       vol = CONGRID(vol, 90, 90, 80)
;
; MODIFICATION HISTORY:
;       DMS, Sept. 1988.
;       DMS, Added the MINUS_ONE keyword, Sept. 1992.
;   Daniel Carr. Re-wrote to handle one and three dimensional arrays
;                    using INTERPOLATE function.
;   DMS, RSI, Nov, 1993.  Added CUBIC keyword.
;       SJL, Nov, 1997.  Formatting, conform to IDL style guide.
;       CT, RSI, April 2001. Added /CENTER keyword. Correct POLY_2D interp.
;-

function CONGRID, arr, x, y, z, $
    CENTER=center, $
    CUBIC = cubicIn, $
    INTERP=interp, $
    MINUS_ONE=minus_one

    COMPILE_OPT idl2
    ON_ERROR, 2     ;Return to caller if error

    ndim = SIZE(arr, /N_DIMENSIONS)
    dims = SIZE(arr, /DIMENSIONS)

    if ((ndim lt 1) or (ndim gt 3)) then $
      Message, 'Array must have 1, 2, or 3 dimensions.'

    ;;  Supply defaults = no interpolate, and no minus_one.
    int = KEYWORD_SET(interp)
    m1 = KEYWORD_SET(minus_one)
    cubic = (N_ELEMENTS(cubicIn) gt 0) ? cubicIn : 0
    if (cubic ne 0) then int = 1    ;Cubic implies interpolate
    offset = KEYWORD_SET(center) ? 0.5 : 0.0

    ; Construct new interpolate coordinates.
    ; Skip this for 2D nearest-neighbor since we use POLY_2D instead.
    if ((ndim ne 2) or ((ndim eq 2) and int)) then begin
        ; Note that we need to use "offset" twice: Once to shift the new
        ; coordinates to the midpoint, and again to shift the location of
        ; the original coordinates to their midpoint.
        switch ndim of  ; Fall through for ndim>1.
            3: srz = float(dims[2]-m1)/(z-m1)*(findgen(z) + offset) - offset
            2: sry = float(dims[1]-m1)/(y-m1)*(findgen(y) + offset) - offset
            1: srx = float(dims[0]-m1)/(x-m1)*(findgen(x) + offset) - offset
        endswitch
    endif

    case ndim of
        1: begin                ; *** ONE DIMENSIONAL ARRAY
            arr_r = (int) ? INTERPOLATE(arr, srx, CUBIC = cubic) : $
                arr[ROUND(srx)]
           end
        2: begin                ; *** TWO DIMENSIONAL ARRAY
            if (int) then begin  ; bilinear or cubic
                arr_r = INTERPOLATE(arr, srx, sry, /GRID, CUBIC=cubic)
            endif else begin  ; nearest neighbor
                ; Note: For expansion, divide by (x-1) so that CONGRID
                ; will agree with REBIN.
                expand = (x gt dims[0])
                xm1 = (m1 or expand) ? x-1 : x
                arr_r = POLY_2D(arr, $
                    [[0,0],[(dims[0]-m1)/float(xm1),0]], $ ;Use poly_2d
                    [[0,(dims[1]-m1)/float(y-m1)],[0,0]],int,x,y)
            endelse
           end
        3: begin                ; *** THREE DIMENSIONAL ARRAY
            ; Only supports linear interpolation.
            arr_r = INTERPOLATE(arr, srx, sry, srz, /GRID)
           end
    endcase

    return, arr_r
end
