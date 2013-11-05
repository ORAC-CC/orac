;===============================================================================
;+
; IS_TAG
;
; returns 1 if given string is a tag of the given structure
;
; PARAMETERS
;	X	Structure
;	S	String
;
; KEYWORDS
;	WH	Return the matching tag index
;
; R.S. 09/02/00
; $Id: is_tag.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function is_tag,x,s,wh=wh
	if n_elements(x) eq 0 then return,0

; Previously - but this takes a long time if x is a large structure !!!
;	if n_tags(x(0)) eq 0 then return,0
;	wh=(where(tag_names(x(0)) eq strupcase(s)))(0)
;	if wh ne -1 then return,1 else return,0

        if n_elements(x) eq 1 then begin 
	   if n_tags(x) eq 0 then return,0
	   wh=(where(tag_names(x) eq strupcase(s)))(0)
	   if wh ne -1 then return,1 else return,0
        endif else return,is_tag(x[0],s,wh=wh)
end
