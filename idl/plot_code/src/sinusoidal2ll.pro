;===============================================================================
;+
; SINUSOIDAL2LL
;
; Convert sinusoidal representation to lat / lon
; Returns longitude -999 if grid point is not on the globe.
;
; U,V are indices in the longitude and latitude (respectively)
; direction to the sinusoidal grid. U=1,V=1 is the South West corner
; of the grid (although this point not defined on earth surface).
; Integer values of U,V refer to the centre of grid tiles.
; (Adding/subtracting 0.5 gives grid edges/vertices)
;
; PARAMETERS
;	U	
;	V
;  On return
;	LAT	latitude / degrees
;	LON	longitude / degrees
;
; KEYWORDS
;	PARAMS	- grid parameters (from def_sinusoidal)
; R.S. 10/05/05
; $Id: sinusoidal2ll.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
pro sinusoidal2ll,u,v,lat,lon,params=params,_EXTRA=extra
  if n_elements(params) eq 0 then params=def_sinusoidal(_EXTRA=extra)
  
  phi = (v+params.v0)/params.Rg;
  lam = (u+params.u0)/(params.Rg*cos(phi));
  
  lat = phi*180d0/!dpi
  lon = lam*180d0/!dpi + params.lon0;

  wh=where(lon lt -180 or lon gt 180,nw)
  if nw gt 0 then lon(wh)=-999d0

  ;lon = atan(sin(lon*!dpi/180),cos(lon*!dpi/180))*180d0/!dpi
end
