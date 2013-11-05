;===============================================================================
;+
; SETUP_LLBIN
;
; Associate lat/lon points with each tile in a regular lat/lon grid
; Can also deal with binning in 3D (see ZV,ZG keywords below)
;
; Returns a structure containing the REVERSE_INDICES and
; other useful stuff from the
; HISTOGRAM function, as well as n points in each grid box.
;
; Returns structure as follows:
;   LAT             Latitudes of bin boundaries (n_bins + 1)
;   LON             Longitudes of bin boundaries
;   MLAT            Mean latitude of each bin (n_bins)
;   MLON            Mean longitude of each bin
;   N               Number of points in each bin
;   II              As output by IDL HISTOGRAM
;   IR              As output by IDL HISTOGRAM
;   OMIN            As output by IDL HISTOGRAM
;   OMAX            As output by IDL HISTOGRAM
;   NN              Set to 1 if NN set below
;;
; 
;
; PARAMETERS
;	LAT	Latitude of data points
;	LON	Longitude of data poinrs
;	LAG	3 element array specifying [min_lat,max_lat,delta_lat]
;		or 1 element specifying delta_lat, to span range of LAT
;	LOG	3 element array specifying [min_lon,max_lon,delta_lon]
;		or 1 element specifying delta_lon, to span range of LON
;
; KEYWORDS
;	NN	Set to return similar structure, but which will
;		perform nearest neighbour interpolation
;	DLMAX	Maximum longitude range to consider points for NN interpolation
;		(used with NN to speed up)
;	RS	Set to return structure even if no points on grid
;		instead of 0
;	EXT	Set to extrapolate first and last bins (in both dimensions)
;		so that values < first bin boundary or > last bin boundary
;		are counted in the corresponding extreme bin.
;	ZV	Set to Z values of points to activate 3-D binning
;	ZG	Set to 3-element vector (like LAG,LOG) defining the bins in Z
;
; R.S. 13/07/06
; $Id: setup_llbin.pro 1056 2011-08-08 08:51:48Z rsiddans $
;-
;===============================================================================
function setup_llbin,lat,lon,lag1,log1,nn=nn,dlmax=dlmax,rs=rs,ext=ext,zv=zv,zg=zg1
;
; great integer indices for each grid point
;
	if n_elements(lag1) eq 1 then lag=[min(lat),max(lat),lag1] else lag=lag1
	if n_elements(log1) eq 0 then log1=lag1
	if n_elements(log1) eq 1 then log=[min(lon),max(lon),log1] else log=log1
	if n_elements(zv) gt 0 then begin
		zg=zg1
		do_zg=1
	endif else do_zg=0
	nlo=long((log(1)-log(0))/log(2)+log(2)/1d5)
	nla=long((lag(1)-lag(0))/lag(2)+lag(2)/1d5)
	if do_zg then nz=long((zg(1)-zg(0))/zg(2)+zg(2)/1d5) else nz=1l
	if nlo lt 1 then message,'Bad LON Grid'
	if nla lt 1 then message,'Bad LAT Grid'
	if nz  lt 1 then message,'Bad Z Grid'
	lag=[lag(0),nla*lag(2)+lag(0),lag(2)]
	log=[log(0),nlo*log(2)+log(0),log(2)]
	if do_zg then zg=[zg(0),nz*zg(2)+zg(0),zg(2)]
;
; bin extrapolation
;
	if keyword_set(ext) then begin
		lat1=lat
		lon1=lon
		wh=where(lat le lag(0),nw)
		if nw gt 0 then lat1(wh)=lag(0)+float(lag(2))/2
		wh=where(lat ge lag(1),nw)
		if nw gt 0 then lat1(wh)=lag(1)-float(lag(2))/2
		wh=where(lon le log(0),nw)
		if nw gt 0 then lon1(wh)=log(0)+float(log(2))/2
		wh=where(lon ge log(1),nw)
		if nw gt 0 then lon1(wh)=log(1)-float(log(2))/2
		if do_zg then begin
			zv1=zv
			wh=where(zv le zg(0),nw)
			if nw gt 0 then zv1(wh)=zg(0)+zg(2)/2
			wh=where(zv ge zg(1),nw)
			if nw gt 0 then zv1(wh)=zg(1)-zg(2)/2
		endif
		return,setup_llbin(lat1,lon1,lag,log,nn=nn,dlmax=dlmax,rs=rs,zv=zv1,zg=zg)
	endif
;
; generate vectors describing bins
;
	glat=dindgen(nla+1)*lag(2)+lag(0)
	mlat=(glat(1:*)+glat)/2
	glon=dindgen(nlo+1)*log(2)+log(0)
	mlon=(glon(1:*)+glon)/2
	if do_zg then begin
		gz=dindgen(nz+1)*zg(2)+zg(0)
		mz=(gz(1:*)+gz)/2
		wh=where(lat ge lag(0) and lat lt lag(1) and lon ge log(0) and lon le log(1) $
			and zv ge zg(0) and zv le zg(1),nw)
	endif else begin
		wh=where(lat ge lag(0) and lat lt lag(1) and lon ge log(0) and lon le log(1),nw)
	endelse
	if nw eq 0 then begin
		if keyword_set(rs) then begin
		    if do_zg then begin
			return,{lat:glat,lon:glon,$	; bounding lat/lons of each bin
				mlat:mlat,mlon:mlon,$	; mean lat/lon in each bin
				z:gz,mz:mz,$
				n:lonarr(nlo,nla,nz)}
		    endif else begin
			return,{lat:glat,lon:glon,$	; bounding lat/lons of each bin
				mlat:mlat,mlon:mlon,$	; mean lat/lon in each bin
				n:lonarr(nlo,nla)}
		    endelse
		endif else begin
			message,'No points on grid',/info
			return,0d0
		endelse
	endif
	if keyword_set(nn) then begin
		if do_zg then message,'3D NN gridding not implemented !'
		if n_elements(dlmax) eq 0 then dlmax=360.
;
; nearest neighbour version
;
		ilat=long((lat(wh)-lag(0))/lag(2))
		ilon=long((lon(wh)-log(0))/log(2))
		dim=long(dlmax/log(2))+1
		n=replicate(1l,nlo,nla)
		ir=lonarr(nlo,nla)
		ii=lindgen(nlo,nla)
		for ilo=0,nlo-1 do begin
			dlo=abs(ilon-ilo)
			whlo=where(dlo lt dim,nwlo)
			if nwlo gt 0 then begin
				dlo=dlo(whlo)
				for ila=0,nla-1 do begin
					dla=ilat(whlo)-ila
					dist=dlo*dlo+dla*dla
					whm=where(dist eq min(dist))
					ir(ilo,ila)=wh(whlo(whm(0)))
				endfor
			endif
			omax=0l
			omin=0l
runbar,ilo,nlo,rbp
		endfor
		ir=reform(ir,nlo,nla)
	endif else begin
;
; Averaging version (based on histogram)
; generate index for each grid point
;
		ii=long((lat(wh)-lag(0))/lag(2))*nlo+$
			long((lon(wh)-log(0))/log(2))
		if do_zg then ii=ii+long((zv(wh)-zg(0))/zg(2))*nlo*nla
		n=histogram(ii,binsize=1,min=0l,max=nlo*nla*nz-1,reverse_ind=ir,$
			omin=omin,omax=omax,loc=loc)
		if do_zg then begin
			n=reform(n,nlo,nla,nz)
		endif else begin
			n=reform(n,nlo,nla)
		endelse
;
; separate index to indices from indices themselves
; then apply the where array to get indices to original data
; (not only the sub-set within the lat/lon range)
;	
		ii=ir(0:nlo*nla*nz-1)-nlo*nla*nz
		ir=wh(ir(nlo*nla*nz:*))
	endelse

	s={lat:glat,lon:glon,$	; bounding lat/lons of each bin
		mlat:mlat,mlon:mlon,$	; mean lat/lon in each bin
		n:n,$			; N points in each bin  
		ii:ii,$			; ir(ii(i):ii(i+1)) gives indices to 
		ir:ir,$			;  original grid points in bin i.
		omin:omin,$		; as histogram
		omax:omax,$
		nn:kywd_def(nn)}
	if do_zg then s=create_struct(s,{z:gz,mz:mz})
	return,s
end
