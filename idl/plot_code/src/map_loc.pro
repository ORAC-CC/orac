;===============================================================================
;+
; MAP_LOC
;
; This draws a map centred on the specified lat/lon
;
; PARAMETERS
;	LAT0	Latitude
;	LON	Longitude
;
; KEYWORDS
;	_EXTRA	Sent to KMAP_SET
;	DL	Map lat/lon range about point
;	ZOOM	Set zoom fator to adjust area mapped.
;	NOP	Do not plot symbol on map
;	SYMSIZE	As plot keyword
;	COLOR	As plot keyword
;	THICK	As plot keyword
;	PSYM	As plot keyword
;	
;
; R.S. 27/07/98
; $Id: map_loc.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
pro map_loc,lat0,lon,color=color,thick=thick,psym=psym,dl=dl,zoom=zoom,_EXTRA=extra,$
	symsize=symsize,nop=nop
	if not keyword_set(zoom) then zoom=1.
	if not keyword_set(dl) then dl=15
	if n_elements(color) eq 0 then color=2
	if n_elements(thick) eq 0 then thick=!p.thick
	if n_elements(symsize) eq 0 then symsize=!p.symsize
	if n_elements(psym) eq 0 then psym=2
	if n_elements(lat0) eq 2 then begin
		lat=lat0(0) & lon=lat0(1)
	endif else begin
		lat=lat0
	endelse
	dlr=dl*!pi/180./zoom
	if dlr gt !pi/2.001 then dlr=!pi/2.001
	ll1=gtcircle(lat,lon,lat,lon-0.01,[dlr,-dlr])
	if lat gt 0. then begin
		ll0=gtcircle(lat,lon,lat-0.01,lon,[-dlr,dlr])
		limit=[ll1(0,0),ll1(1,0),$
			ll0(0,0),ll0(1,0),$
			ll1(0,1),ll1(1,1),$
			ll0(0,1),ll0(1,1)]
	endif else begin
		ll0=gtcircle(lat,lon,lat+0.01,lon,[dlr,-dlr])
		limit=[ll1(0,0),ll1(1,0),$
			ll0(0,0),ll0(1,0),$
			ll1(0,1),ll1(1,1),$
			ll0(0,1),ll0(1,1)]
	endelse
	kmap_set,ori=[lat,lon,0.],lim=limit,_EXTRA=extra,/ortho,/adv
	if not keyword_set(nop) then plots,lon,lat,psym=psym,color=color,thick=thick,symsize=symsize
end
