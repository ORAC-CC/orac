;===============================================================================
;+
; STRCAT
;
; Concatenate string array to 1 string
;
; PARAMETERS
;	S	String array
;
; KEYWORDS
;	DEL	Delimeter (default = space)
;
; R.S. 30/01/04
; $Id: strcat.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function strcat,s,del=del
	if n_elements(del) eq 0 then del=' '
	s1=s(0)
	for i=1,n_elements(s)-1 do s1=s1+del+s(i)
	return,s1
end
