;===============================================================================
;+
; LLSUB.PRO
;
; This function returns the angle in radians subtended at the centre of the 
; Earth by a pair (or pairs) of latitude,longitude points.
;
; PARAMETERS
;	LAT1	The latitude of the first point (can be array for more points)
;	LON1	The longitude of the first point (can be array for more points)
;	LAT2	The latitude of the second point (can be array for more points)
;	LON2	The longitude of the second point (can be array for more points)
;
; KEYWORDS
;	DIST    Set this to return the surface distance between the points.
;	DEGREES Set this to get return the angle in degrees, rather than rads.
;	REARTH	Specify earth radius for DIST
;
;
; R.Siddans 6/1/95
; $Id: llsub.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================

function llsub,lat1,lon1,lat2,lon2,dist=dist,degrees=deg,rearth=rearth
;
; work out angle subtended at centre of earth
;
	dg2rad=1.745329e-2    	; factor to convert degress to radians
	cos_a1=cos(lat1*dg2rad)
	sin_a1=sin(lat1*dg2rad)
	cos_a2=cos(lat2*dg2rad)
	sin_a2=sin(lat2*dg2rad)
	cos_b1=cos(lon1*dg2rad)
	sin_b1=sin(lon1*dg2rad)
	cos_b2=cos(lon2*dg2rad)
	sin_b2=sin(lon2*dg2rad)

	cos_angle=cos_a1*cos_b1*cos_a2*cos_b2+$
		cos_a1*sin_b1*cos_a2*sin_b2+$
		sin_a1*sin_a2
;
; trap rounding error producing cos_angle just > 1
;
	wh=where(cos_angle gt 1)
	if wh(0) ne -1 then begin
		if max(cos_angle(wh)) gt 1.001 then message,'cos error'
		cos_angle(wh)=1.
	endif
	answer=acos(cos_angle)
;
; convert to distance in km if required...
;
	if keyword_set(dist) then begin
			if n_elements(rearth) eq 0 then rearth=6375.14 
			answer=answer*rearth
	endif else if keyword_set(deg) then answer=answer/dg2rad

	return,answer
end
