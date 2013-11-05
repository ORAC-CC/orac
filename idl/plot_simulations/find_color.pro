;===============================================================================
;+
; FIND_COLOR
;
; This procedure returns the color index nearest to the specified RGB color.
; E.g. find_color(0,0,0) returns the color index nearest to black.
;
; PARAMETERS
;	RED
;	GREEN	(omit for greyscale)
;	BLUE	(omit for greyscale)
;    optionally...
;	CTRED
;	CTGREEN	Specify clour table values rather than obtain them from TVLCT
;	CTBLUE
;	
; KEYWORDS
;	 THR Specify a closeness threshold. color must be within THR or 
;		-1 returned.
;	
;
; R.S. 29/1/96
; $Id: find_color.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function find_color,r,g,b,red,green,blue,thr=thr
	if n_elements(thr) eq 0 then thr=1e20
	if n_elements(r) eq 0 then r=0
	if n_elements(r) eq 3 then begin
		g=r(1)
		b=r(2)
		r=r(0)
	endif
	if n_elements(g) eq 0 then g=r
	if n_elements(b) eq 0 then b=r
	if n_elements(red) eq 0 then tvlct,red,green,blue,/get
	dist1=red-r
	dist2=green-g
	dist3=blue-b
	dist=dist1^2.+dist2^2.+dist3^2.
	min_d=min(dist)
	if min_d gt thr then return,-1
	wh=where(dist eq min_d)
	return,wh(0)
end
