;===============================================================================
;+
; INTERPOL_RS
;
; Call IDL INTERPOL with extrapolation at constant value
;
; PARAMETERS
;	Y1	Value to be interpolated
;	X1	Original X axis values associated with each value of Y1
;	X2	New X axis
;
; KEYWORDS
;	ZERO	Set extrapolations to no-data value (zero by default)
;	EXTRA	Allow extrapolation
;	ND	Set no-data value
;
; R.S. 26/01/04
; $Id: interpol_rs.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function interpol_rs,y1,x1,x2,zero=zero,extra=extra,nd=nd
	if keyword_set(extra) then return,interpol(y1,x1,x2)
	max1=max(x1,min=min1)
	whr=where(x2 ge min1 and x2 le max1)
	if keyword_set(zero) then begin
		if n_elements(nd) eq 0 then nd=0.
		y1mi=nd
		y1ma=nd
	endif else begin
		whmi=(where(x1 eq min1))(0)
		whma=(where(x1 eq max1))(0)
		y1mi=y1(whmi)
		y1ma=y1(whma)
	endelse
	sz=size(x2)
	if sz(0) eq 0 then y2=y1mi $
	else y2=make_array(size=sz,val=y1mi)
	if whr(0) ne -1 then begin
		y2(whr)=interpol(y1,x1,x2(whr))
	endif
	whg=where(x2 gt max1)
	if whg(0) ne -1 then y2(whg)=y1ma
	return,y2
end
