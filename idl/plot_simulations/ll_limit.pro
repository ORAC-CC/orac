;==========================================================================
;+
;	function LL_LIMIT
;
;	Description
; Set the limit array for use with map_set based on passed lat/lon
; 
;	Use
; 
;	Parameters
;  I  LAT array of latitudes
;  I  LON array of longitudes
; (I) LATP lat padding to give extra space around LAT points
; (I) LONP lon padding to give extra space around LON points
; 
;	Keywords
;  I  CENTRE Set to calculate and return centre lat/lon values
;  O  CLAT Centre of regions latitude
;  O  CLON Centre of regions longitude
;
;	Date
;	B. Latter : 12 December 2002
; $Id: ll_limit.pro 222 2009-06-08 15:56:30Z blatter $
;-
;==========================================================================
function ll_limit,lat,lon,latp,lonp,centre=centre,clat=clat,clon=clon
if n_elements(latp) eq 0 then latp=0
if n_elements(lonp) eq 0 then lonp=0
if size(lat,/type) eq 8 then begin
	latmx=max(lat.lat,min=latmn)
	lonmx=max(lat.lon,min=lonmn)
endif else begin
	latmx=max(lat,min=latmn)
	lonmx=max(lon,min=lonmn)
endelse
lat0=latmn-latp
lon0=lonmn-lonp
lat1=latmx+latp
lon1=lonmx+lonp
if keyword_set(centre) then begin
	clat=(lat1+lat0)/2.
	clon=(lon1+lon0)/2.
endif
return,[lat0,lon0,lat1,lon1]
end
