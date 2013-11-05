; $Id: //depot/idl/IDL_71/idldir/lib/poly.pro#1 $
;
; Copyright (c) 1983-2009, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.

;+
; NAME:
;	POLY
;
; PURPOSE:
;	Evaluate a polynomial function of a variable.
;
; CATEGORY:
;	C1 - Operations on polynomials.
;
; CALLING SEQUENCE:
;	Result = POLY(X,C)
;
; INPUTS:
;	X:	The variable.  This value can be a scalar, vector or array.
;
;	C:	The vector of polynomial coefficients.  The degree of
;		of the polynomial is N_ELEMENTS(C) - 1.
;
; OUTPUTS:
;	POLY returns a result equal to:
;		 C[0] + c[1] * X + c[2]*x^2 + ...
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	Straightforward.
;
; MODIFICATION HISTORY:
;	DMS, Written, January, 1983.
;   CT, RSI, Nov 2004: Special case for zero-order polynomial.
;       Be sure to still return an array.
;-

FUNCTION POLY,X,C

    compile_opt idl2

    on_error,2		;Return to caller if an error occurs

    N = N_ELEMENTS(C)-1	;Find degree of polynomial

    ; Special case for N=0. Be sure to return a result of the
    ; same type and array dimensions as X.
    if (n eq 0) then $
        return, x*0 + c[0]

    Y = c[n]
    for i=n-1,0,-1 do y = TEMPORARY(y) * x + c[i]

    return,y

end

