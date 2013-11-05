;===============================================================================
;+
; KYWD_DEF
;
; Returns 0 if variable not defined or the value of the variable if it is
;
; PARAMETERS
;	X	Variable
;
; KEYWORDS
;	
;
; R.S. 19/06/03
; $Id: kywd_def.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function kywd_def,x
	if n_elements(x) eq 0 then return,0 $
	else return,x
end
