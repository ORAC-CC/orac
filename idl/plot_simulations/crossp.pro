; $Id: //depot/idl/IDL_71/idldir/lib/crossp.pro#1 $
;
; Copyright (c) 1983-2009, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.
;
Function Crossp,v1,v2
;
;+
; NAME:
;	CROSSP
;
; PURPOSE:
;	Evaluate the vector or cross-product of vectors v1 and v2.
;
; CATEGORY:
;	Vector mathematics.
;
; CALLING SEQUENCE:
;	Result = CROSSP(v1, v2)
;
; INPUTS:
;	v1, v2:  Three-element vectors.
;
; OUTPUTS:
;	Returns a 3-element, floating-point vector.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	Vectors must have 3 elements.
;
; PROCEDURE:
;	v1 X v2 = | i  j  k  | = (b1c2 - b2c1)i + (c1a2-c2a1)j + (a1b2-a2b1)k
;		  | a1 b1 c1 |
;		  | a2 b2 c2 |
;
; MODIFICATION HISTORY:
;	Written, DMS, Aug, 1983;
;-
	on_error,2                      ;Return to caller if an error occurs
	return,[v1[1]*v2[2]-v2[1]*v1[2], V1[2]*v2[0]-V2[2]*v1[0], $
		v1[0]*v2[1]-v2[0]*v1[1] ]
end

