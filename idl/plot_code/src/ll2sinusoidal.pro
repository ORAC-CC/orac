;===============================================================================
;+
; LL2SINUSOIDAL
;
; Convert lat/lon to sinusoidal representation
;
; U,V are floating point "indices" in the longitude and latitude (respectively)
; direction to the sinusoidal grid. U=1,V=1 is the South West corner
; of the grid (although this point not defined on earth surface).
; Integer values of U,V refer to the centre of grid tiles.
; (Adding/subtracting 0.5 gives grid edges/vertices)
;
; PARAMETERS
;	LAT	latitude / degrees
;	LON	longitude / degrees
;  on return
;	U	Coords in sinusoidal projection.
;	V
;	NB U,V returned as floating point numbers (always > 0 for valid lat.lon), which
;       are in units of PARAMS.RES (i.e. U=1 corresponds to PARAMS.RES km at the equator)
;       If U,V are rounded to the nearest integer then together they identify the sinusoidal
;       grid tile inside which LAT,LON fall.
;
; KEYWORDS
;	PARAMS	- grid parameters (from def_sinusoidal)
; R.S. 10/05/05
; $Id: ll2sinusoidal.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
pro ll2sinusoidal,lat,lon,u,v,params=params,_EXTRA=extra
  if n_tags(params) eq 0 then params=def_sinusoidal(_EXTRA=extra)
  dlon = lon - params.lon0
  dlon = atan(sin(dlon*!dpi/180),cos(dlon*!dpi/180))*180d0/!dpi
  
  phi = lat*!dpi/180
  lam = dlon*!dpi/180
  
  u =  params.Rg * lam * cos (phi) - params.u0
  v =  params.Rg * phi             - params.v0
end
