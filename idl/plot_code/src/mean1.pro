;===============================================================================
;+
; MEAN1
;
; Returns the mean of an array
;
; PARAMETERS
;	D	data
;
; KEYWORDS
;	
;
; R.S. 13/09/97
; $Id: mean1.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function mean1,d
	return,total(d)/n_elements(d)
end
