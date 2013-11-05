;===============================================================================
;+
; DEFINE_STANDARD_GRID
;
; Define a grid to span a given lat/lon range, for later
; use by setup_standard_bin
;
; Structure returned gives the u,v ranges the re-projected grid
; required to span the given lat/lon range
; 
; PARAMETERS
;	None
;
; KEYWORDS
;	_EXTRA Any keyword accepted by MAP_LIMITS can be used to define lat/lon
;		range of grid, otherwise full globe assumed.
;	NEQ	Number of points round equator of sinusoidal grid
;	SD	Set to grid as SEVIRI observations
;
; R.S. 28/10/05
;-
;===============================================================================
function define_standard_grid,neq=neq,_EXTRA=extra,sd=sd
	lim=map_limits(/n_europe)
	latr=[lim(0),lim(2)]
	lonr=[lim(1),lim(3)]
	if keyword_set(sd) then begin
;
; Geostationary imager sampling
; NB does not apply user defined lat/lon range at the moment !!!
;
;stop
;print,range(latr,/tot)
;print,range(lonr,/tot)
		if range(latr,/tot) gt 140 and range(lonr,/tot) gt 140 then begin
			latr=[-65d0,65d0]
			lonr=[-65d0,65d0]
			umin=0d0
			umax=3712d0
			vmin=0d0
			vmax=3712d0
			id='sevdisk'
		endif else begin
			rc=geo_ll2pix(latr([0,0,1,1]),lonr([0,1,0,1]))
			umin=min(rc.column-1)
			umax=max(rc.column)
			vmin=min(rc.line-1)
			vmax=max(rc.line)
			id='sevdisk-u'+trim_zero(umin)+'-'+trim_zero(umax)+$
				'-v'+trim_zero(vmin)+'-'+trim_zero(vmax)
		endelse
	endif else begin

;print,'latr',latr
;stop
;
; sinusoidal grid
;
		if n_elements(neq) eq 0 then neq=4008l	; globaerosol=10km resolutions
;
; generate array of latitudes sampling every grid box
;
		dlat=180d0/neq/2	; latitude grid spacing
		nlat=(latr(1)-latr(0))/dlat+1
		lats=dindgen(nlat)*dlat+latr(0)
;
; convert coors of E,W edges of required box, to work out range of u,v
; needed to span given geographical limits
;
		ll2sinusoidal,lats,lonr(replicate(0,nlat)),u1,v1,neq=neq
		ll2sinusoidal,lats,lonr(replicate(1,nlat)),u2,v2,neq=neq
		umax=max([u1,u2],min=umin)
		umin=double(long(umin)+0.5)
		umax=double(long(umax+0.5)+0.5)
		vmax=max([v1,v2],min=vmin)
		vmin=double(long(vmin)+0.5)
		vmax=double(long(vmax+0.5)+0.5)
;
; define a string identifyer for the grid
;
		id='neq'+trim_zero(neq)
		if umax-umin ne neq   then id=id+'_ur'+trim_zero(long(umin+0.5))+'-'+trim_zero(long(umax-0.5))
		if vmax-vmin ne neq/2 then id=id+'_vr'+trim_zero(long(vmin+0.5))+'-'+trim_zero(long(vmax-0.5))
	endelse
	return,{latr:latr,lonr:lonr,$
		ug:[umin,umax,1d0],$
		vg:[vmin,vmax,1d0],$
		id:id,$
		neq:kywd_def(neq),$
		sd:kywd_def(sd)}
end
