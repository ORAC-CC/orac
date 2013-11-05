;===============================================================================
;+
; DEF_CHS
;
; Set a default charsize which will not depend greatly on number of
; rows defined in !p.muli
;
; PARAMETERS
;	
;
; KEYWORDS
;	
;
; R.S. 24/01/09
; $Id: def_chs.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function def_chs
	nr=!p.multi(2)
	if nr gt 2 then return,2.0 else return,1.0
end
