;===============================================================================
;+
; STANDARD_GRID_LL
;
; Return lat/lon of every point on a 
; grid defined by DEFINE_STANDARD_GRID.
;
; PARAMETERS
;	SG	Return from DEFINE_STANDARD_GRID
; Can optionally define the following (otherwise all points in grid evaluated)
;	US	U coords actually required
;	VS	V coords actually required
;
; KEYWORDS
;	_EXTRA	Set to DEFINE_STANDARD_GRID and SETUP_LLBIN
;
; R.S. 29/04/09
; $Id: standard_grid_ll.pro 1117 2011-08-08 09:08:27Z rsiddans $
;-
;===============================================================================
function standard_grid_ll,sg,us,vs
	if n_elements(sg) eq 0 then sg=define_standard_grid(_EXTRA=extra)
	if n_elements(vs) eq 0 then begin
	    if sg.sat ne 1 then begin
		u=long(expand_lag(sg.ug,np=nu,/mid))
		v=long(expand_lag(sg.vg,np=nv,/mid))
		us=u(*,replicate(0,nv))
		vs=transpose(v(*,replicate(0,nu)))
	    endif
	endif else begin
		nu=n_elements(us(*,0))
		nv=n_elements(vs(0,*))
	endelse
;
; convert lat,lon to standard grid coords)
;
	if sg.sd eq 1 then begin
		gp=geo_pix2ll(vs,us)
		lat=gp.lat
		lon=gp.lon
	endif else if n_elements(sg.ll) eq 2 then begin
		lat=us*sg.ll(0)-90
		lon=vs*sg.ll(1)-180
	endif else if sg.neq gt 0 then begin
		sinusoidal2ll,us,vs,lat,lon,neq=sg.neq
	endif else if sg.sat eq 1 then begin
;
; lat,lon expected to be present in this case
;
		lat=sg.lat
		lon=sg.lon
		if n_elements(us) gt 0 and n_elements(us) ne n_elements(lat) then message,'selecting u,v does not work with sat grid at moment!'
		us=-999l
		vs=-999l
		nu=0l
		nv=0l
	endif else message,'Did not recognize grid!'
	return,{lat:lat,lon:lon,u:us,v:vs,nu:nu,nv:nv,id:sg.id}
end
