; $Id: //depot/idl/IDL_71/idldir/lib/reverse.pro#1 $
;
; Copyright (c) 1991-2009, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.

;+
; NAME:
;   REVERSE
;
; PURPOSE:
;   Reverse the order of rows or columns in an array or vector.
;
; CATEGORY:
;   Array manipulation.
;
; CALLING SEQUENCE:
;   Result = REVERSE(Array [, Subscript_Index])
;
; INPUTS:
;   Array:    The array or vector containing the original data.
;
; OPTIONAL INPUT PARAMETERS:
; Subscript_Index:  If this parameter is omitted or 1, the first subscript is
;     reversed (i.e., rows are reversed).  Set this parameter to
;     2 to reverse columns.
;
; KEYWORD PARAMETERS:
;   OVERWRITE = Set this keyword to do the transformation "in-place".
;             The result overwrites the previous contents of the variable.
;
; OUTPUTS:
;   REVERSE returns the input array, but reversed about
;   one of its dimensions.
;
; COMMON BLOCKS:
;   None.
;
; SIDE EFFECTS:
;   None.
;
; RESTRICTIONS:
;   None.
;
; PROCEDURE:
;   Uses the REFORM function.
;
; MODIFICATION HISTORY:
;   Old.
;   Apr, 1991, DMS,   Added 3D reversing.
;       Sept, 1992 Mark L. Rivers, added simple return for scaler argument
;   Sept, 1994. Added default for 3D case.
;   May 2000, CT, Rewrote to handle any dimensions, added OVERWRITE keyword.
;   Nov 2004, CT: Drop trailing dimensions of length 1 for all arrays.
;   June 2005, CT: Don't drop middle dimensions of length 1.
;-

function reverse, a, subscriptIn, $
    OVERWRITE=overwrite

    on_error,2                             ;Return to caller if an error occurs

    ndims = SIZE(a, /N_DIMENSIONS)
    dimensions = SIZE(a, /DIMENSIONS)

    b = KEYWORD_SET(overwrite) ? TEMPORARY(a) : a

    if ndims eq 0 then return, b

    subscript = (N_ELEMENTS(subscriptIn) gt 0) ? subscriptIn : 1
    IF (subscript GT ndims) THEN MESSAGE, $
       "Subscript_index must be less than or equal to number of dimensions."


    ; No need to reverse a dimension of length 1.
    if (dimensions[subscript-1] le 1) then return, b

    ; Drop trailing dimensions of length 1. We cannot directly use
    ; REFORM since it will drop middle dims of length 1.
    ndims = MAX(WHERE(dimensions gt 1)) + 1

    ; All dims are length 1. We're done.
    if (ndims lt 1) then return, b

    dimensions = dimensions[0:ndims-1]
    b = REFORM(b, dimensions, /OVERWRITE)

; handle 1 or 2 dimensions using ROTATE for efficiency
    if (ndims eq 1) then return, ROTATE(b,5)
    if (ndims eq 2) then begin
       case (subscript) of
       1: return, ROTATE(b, 5)
       2: return, ROTATE(b, 7)
       endcase
    endif


; for 3 or more dimensions, collapse down to 3 dimensions & loop over index
; compress the smaller (inner) & larger (outer) dimensions
; so we only have to deal with a 3-dimensional array

    nDo = dimensions[subscript-1]   ; dimension size for Subscript_index

; array size for dimensions smaller than Subscript_index
    nLess = 1L
    FOR i=0,subscript-2 DO nLess = nLess*dimensions[i]
; array size for dimensions greater than Subscript_index
    nMore = 1L
    FOR i=subscript,ndims-1 DO nMore = nMore*dimensions[i]
    b = REFORM(b, nLess, nDo, nMore, /OVERWRITE)


    ; manually loop over the middle dimension (could do this using an
    ; index array, but it might use too much memory)
    FOR i=0ull,(nDo-1)/2 DO BEGIN
       temp = b[*,nDo-1-i,*]
       b[0,nDo-i-1,0] = b[*,i,*]
       b[0,i,0] = temp
    ENDFOR

    return, REFORM(b,dimensions,/OVERWRITE)  ; restore original dimensions

end
