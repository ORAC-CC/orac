;===============================================================================
;+
; GET_LAND_FLAG_RS
;
; Return flag 1=sea,2=land,0=not-defined (wrong lat/lon)
; 
;
; PARAMETERS
;	LAT	Latitutes
;	LON	Longitudes
;	RES	(optional) define resolution
;
; KEYWORDS
;	LAND_OR_COAST	Set option for 
;	NOTEST	Do not perform test to check landflagging working
;
; R.S. 24/01/06
; $Id: get_land_flag_rs.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function get_land_flag_rs,lat,lon,res,land_or_coast=land_or_coast,notest=notest
	if not keyword_set(notest) then begin
		l1=get_land_flag_rs([0,52],[0,0],/notest)
		if l1(0) ne 1 or l1(1) ne 2 then message,'Land flag error'
	endif
	if n_elements(res) eq 0 then res=2
	lat1=float(lat)
	lon1=float(lon)
	sz=size(lat1)
	wh=where(abs(lat1) gt 90 or abs(lon1) gt 180,nw,comp=whok,ncomp=nok)
	if nw gt 0 then begin
		flag=intarr(n_elements(lat))
		if nok gt 0 then flag(whok)=get_land_flag_rs(lat(whok),lon(whok),res,land_or_coast=land_or_coast,notest=notest)
		return,flag
	endif
	along_track_dist=0l
	res1=long(res)
	if n_elements(land_or_coast) eq 0 then land_or_coast=2
	land_or_coast1=long(land_or_coast)
	surf_flag=fix(lat1*0)
	if sz(0) eq 2 then begin
		max_x=long64(sz(1))
		max_y=long64(sz(2))
	endif else if sz(0) eq 1 then begin
		max_x=long64(sz(1))
		max_y=long64(1)
	endif else if sz(0) eq 0 then begin
		max_x=long64(1)
		max_y=long64(1)
	endif else message,'Lat,lon can only be 2d array at most'
	if strpos(!version.arch,'64') lt 0 then begin
		max_x=long(max_x)
		max_y=long(max_y)
	endif
	lib=strtrim(getenv('LANDMASK'),2)
	if strlen(lib) eq 0 then lib='libget_land_mask.so'
	lml=strtrim(getenv('LANDMASK_LUT'),2)
	if strlen(lml) eq 0 then setenv,'LANDMASK_LUT=/.'
	res = call_external(lib,'get_land_flag_long',lat1,lon1,along_track_dist,res1,$
		land_or_coast1,surf_flag,max_x,max_y)
	return,surf_flag
end
