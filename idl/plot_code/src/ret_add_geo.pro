;===============================================================================
;+
; RET_ADD_GEO
;
; Add geolocation information from header to a cldmodel ret structure
; (Enables geolocation to be kept associated with sub-selected/concatenated 
; ret results)
;
; PARAMETERS
;	
;
; KEYWORDS
;	
;
; R.S. 18/05/11
; $Id$
;-
;===============================================================================
function ret_add_geo,x,h
	sz=size(x)
	nx=n_elements(x)
	if nx ne n_elements(h.u) then message,'X,H have incompatible dimensions!'
	if h.sg.sat eq 1 then begin
		ll={lat:h.sg.lat,lon:h.sg.lon}
	endif else ll=standard_grid_ll(h.sg,h.u,h.v)
;	lat=ll.lat
;	lon=ll.lon
	u=h.u
	v=h.v
	for i=0l,nx-1 do begin
;		x1=create_struct(x(i),{lat:lat(i),lon:lon(i),u:u(i),v:v(i)})
x1=create_struct(x(i),{u:u(i),v:v(i)})
		if n_tags(xs) eq 0 then xs=replicate(zero_struct(x1),nx)
		xs(i)=x1
	endfor
	return,xs
end
