;===============================================================================
;+
; DEF_SINUSOIDAL
;
; Define paramters of a sinusoidal grid
;
; PARAMETERS
;	
;
; KEYWORDS
;	RES	Set resolution / km
;	NEQ	Set number of boxes around equator (over-rides RES)
;		NB: NEQ should be EVEN for grid which is symmetric North-South
;		NB: If NEQ is exactly divisible by 4 then equator lies
;		    on a boundary between grid-tiles
;		    If NEQ mod 4 = 2, then equator bisects tiles
;		Set NEQ=4008 for a ~10km grid with tiles bounded
;		by the equator. Set to 400 for ~100km grid.
;		    
;
; R.S. 10/05/05
; $Id: def_sinusoidal.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function def_sinusoidal,res=res,neq=neq
	r_e=6378.137d0 	; equatorial earth radius
	if n_elements(res) eq 0 then begin
		if n_elements(neq) eq 0 then neq=20d0
		res=2d0*!dpi*r_e/neq
	endif else begin
		neq=2d0*!dpi*r_e/res
	endelse
	rg=r_e/res
	u0=-(!dpi*rg+0.5)		; grid coords of 0 lat, lon0
	v0=-(!dpi/2*rg+0.5)	; 
	lon0=0d0	; centre longitude
	nrow=(long(neq)+0.5)/2
	lat=(dindgen(nrow)+0.5)/rg/!dpi*180d0-90d0
	nbin=long(cos(lat*!dpi/180)*nrow*2)
	if neq mod 2 eq 0 then begin
		wh=where((nbin mod 2) eq 1,nw)
	endif else begin
		wh=where((nbin mod 2) eq 0,nw)
	endelse
	if nw gt 0 then nbin(wh)=nbin(wh)+1
	basebin=[0l,long((total(nbin,/cum,/doub))(0:nrow-2))]
	st={rg:rg,lon0:lon0,u0:u0,v0:v0,res:res,neq:neq,nbin:nbin,basebin:basebin,lat:lat}
	return,st
end
