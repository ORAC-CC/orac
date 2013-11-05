;===============================================================================
;+
; MEDIAN1
;
; Return median value of a set of numbers
;
; PARAMETERS
;	
;
; KEYWORDS
;	PC	Return given percentile of set of numbers
; 		(i.e median =50% of values above)
;		(90% = 90% of values below)
;		(can be an array to return model than 1 percentile)
;
; R.S. 03/10/05
; $Id: median1.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function median1,x,pc=pc
	so=sort(x)
	if n_elements(pc) eq 0 then pc=50.
	nx=n_elements(x)
	np=nx*float(pc)/100
	return,x(so(np))
end
