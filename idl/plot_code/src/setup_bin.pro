;===============================================================================
;+
; SETUP_LLBIN
;
; Associated points with each interval in regular grid
;
; Returns a structure containing the REVERSE_INDICES and
; other useful stuff from the
; HISTOGRAM function, as well as n points in each grid box.
;
; Result can be put through apply_llgrid_stat
; 
;
; PARAMETERS
;	LAT	coord of data points
;	LAG	3 element array specifying [min_lat,max_lat,delta_lat]
;		or 1 element specifying delta_lat, to span range of LAT
;
; KEYWORDS
;	RS	Set to return structure (with N set to 0) if no data in grid
;		(otherwise returns 0 value)
;
; R.S. 13/07/06
; $Id: setup_bin.pro 965 2011-06-10 09:46:02Z rsiddans $
;-
;===============================================================================
function setup_bin,lat,lag1,quiet=quiet,rs=rs
;
; create integer indices for each grid point
;
	if n_elements(lag1) eq 1 then begin
		lag=[min(lat),max(lat),lag1]
		nla=long((lag(1)-lag(0))/lag(2)+lag(2)/1d5)
		if lag(0)+lag(2)*nla lt max(lag) then lag(1)=lag(1)+lag1
	endif else lag=lag1
	nla=long((lag(1)-lag(0))/lag(2)+lag(2)/1d5)
	if nla lt 1 then message,'Bad LAT Grid'
	lag=[lag(0),nla*lag(2)+lag(0),lag(2)]
	wh=where(lat ge lag(0) and lat lt lag(1),nw)
	if nw eq 0 then begin
		if not keyword_set(quiet) then message,'No points on grid',/info
		if keyword_set(rs) then return,{n:lonarr(nla)}
		return,0d0
	endif
;
; generate index for each grid point
;
	ii=long((lat(wh)-lag(0))/lag(2))
	n=histogram(ii,binsize=1,min=0l,max=nla-1,reverse_ind=ir,$
		omin=omin,omax=omax,loc=loc)
	n=reform(n,nla)
;
; generate vectors describing bins
;
	glat=dindgen(nla+1)*lag(2)+lag(0)
	mlat=(glat(1:*)+glat)/2
;
; separate index to indices from indices themselves
; then apply the where array to get indices to original data
; (not only the sub-set within the lat range)
;	
	ii=ir(0:nla-1)-nla
	ir=wh(ir(nla:*))

	return,{lat:glat,$	; bounding lat of each bin
		lon:[-999,999],$
		mlat:mlat,$	; mean lat in each bin
		mlon:-999,$
		n:n,$			; N points in each bin  
		ii:ii,$			; ir(ii(i):ii(i+1)) gives indices to 
		ir:ir,$			;  original grid points in bin i.
		omin:omin,$		; as histogram
		omax:omax}
end
