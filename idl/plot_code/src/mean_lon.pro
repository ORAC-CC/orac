;===============================================================================
;+
; MEAN_LON
;
; This function returns the mean of a number of longitudes (or angles in
; degrees), taking care of +/-180 degree cases (or 0/360 degrees).
;
; PARAMETERS
;	LONS	longitudes to be averaged
;
; KEYWORDS
;	SDEV	set to variable to hold standard deviation of longitudes
;		on return
;
; R.S. 22/8/96
; $Id: mean_lon.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function mean_lon,tlons,sdev=sdev
	if n_elements(tlons) eq 1 then return,tlons
	lons=tlons mod 360.
	wh=where(lons gt 180.)
;
; convert 0-360 longs to -180-180
;
	if wh(0) ne -1 then begin
		lons(wh)=lons(wh)-360
		f360=1
	endif else f360=0
;
; find mean/variance of lons
;
	std=stderr(lons,mean=mean1,sdev=sdev1)
;
; convert to 0-360 and do it again
;
	wh=where(lons lt 0.)
	if wh(0) ne -1 then begin
		lons(wh)=lons(wh)+360.
	endif
	std=stderr(lons,mean=mean2,sdev=sdev2)
;
; select mean which has smallest sdev
;
	if sdev2 lt sdev1 then begin
		mean=mean2
		sdev=sdev2
	endif else begin
		mean=mean1
		sdev=sdev1
	endelse
;
; convert to lie within original lon range (+/-180 or 0-360)
;
	if f360 and mean lt 0 then mean=mean+360.
	if not f360 and mean gt 180. then mean=mean-360.
	return,mean
end
