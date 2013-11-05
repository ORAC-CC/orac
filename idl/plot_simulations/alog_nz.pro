;===============================================================================
;+
; ALOG_NZ
;
; Take log of an array, checking for invalid values and setting
; the result of these to a no-data value
;
; PARAMETERS
;	A
;
; KEYWORDS
;	ND	Set no-data value	
;
; R.S. 17/05/07
; $Id: alog_nz.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function alog_nz,a,no_data=no_data
	if n_elements(no_data) eq 0 then no_data=-999
	if n_elements(a) eq 1 then c=a*0+no_data $
	else c=make_array(size=size(a),value=no_data)
	wh=where(a gt 0,nw)
	if nw gt 0 then c(wh)=alog(a(wh))
	return,c
end
