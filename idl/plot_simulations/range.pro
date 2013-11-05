;===========================================================================
;+
;	function RANGE
;
;	Description
; Return the range spanned by an array ([min,max])
;
;	Parameters
; X array to examine
; 
;	Keywords 
; TOTAL return total range spanned
; NAN ignore non-numbers (-inf, NaN etc)
;
;	Date
;	B. Latter : 21st August 2001
;-
;===========================================================================
function range,x,total=total,nan=nan
mn=min(x,max=mx,nan=nan)
range=[mn,mx]
if keyword_set(total) then range=mx-mn
return,range
end
