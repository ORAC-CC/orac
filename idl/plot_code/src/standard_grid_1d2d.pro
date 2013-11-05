;===============================================================================
;+
; STANDARD_GRID_1D2D
;
; Given 1d set of points defined on standard grid, return 2d organised array
;
; PARAMETERS
;	D	Data
;	U	U-coords of each D on standard grid (columns)
;	V	V-coords of each D on standard grid (lines)
;
; KEYWORDS
;	UR	Explicitly set range of U spanned by returned data
;	VR	Explicitly set range of V spanned by returned data
;	NODATA	Set no-data value
;	RLL	Set to standard grid definition structure to
; 		return data reprojected onto regular lat/lon
;		grid (for sinusoidal std grid only!)
;	LL	Set /return result of standard_grid_ll to improve
;		speed when RLL set
;	LONR	Return longitude range of image when RLL set
;	LATR	Return latitude  range of image when RLL set
;	RU	Set to reverse columns (useful for SEVIRI grid, which
;		otherwise is the mirror of standard map orientation)
;	ORIGIN	Set to return origin of regular lat/lon grid - 
;		normally this will be [0,0] but if image wraps over
;		+/-180 then it will be set to [0,180], this should
;		then be used with map_set if you want to oveplot coastlines on
;		an image (only relevant when RLL set)
;	NOWRAP	always have map origin [0,0] even if data wraps over data-line
;
; R.S. 30/04/09
; $Id: standard_grid_1d2d.pro 798 2011-04-04 09:40:42Z rsiddans $
;-
;===============================================================================
function standard_grid_sin2ll,di,ur,vr,sg,nodata,ll=ll,latr=latr,lonr=lonr,origin=origin,nowrap=nowrap
	if sg.sd eq 1 then return,d
        if n_elements(ll) eq 0 then begin
                sg1=sg
                sg1.ug=[ur,1]
                sg1.vg=[vr,1]
                ll=standard_grid_ll(sg1)
        endif
	if ll.nu ne n_elements(di(*,0)) or ll.nv ne n_elements(di(0,*)) then message,'Grid miss-match'
        di2=dblarr(sg.neq,ll.nv)+nodata
        ilon=dindgen(sg.neq)
        iumin=sg.neq
        iumax=-sg.neq
        for iv=0,ll.nv-1 do begin
                il2=sg.neq/2+(ilon-sg.neq/2)*cos(ll.lat(0,iv)*!dtor)
                wh=where(il2 ge ur(0) and il2 le ur(1),nw)
                if nw gt 0 then begin
                        dis=di(il2(wh)-ur(0),iv)
                        di2(ilon(wh),iv)=dis
                        whnv=where(dis ne nodata,nv)
                        if nv gt 0 then begin
                                ilmax=max(ilon(wh(whnv)),min=ilmin)
                                if ilmax gt iumax then iumax=ilmax
                                if ilmin lt iumin then iumin=ilmin
                        endif
                endif
        endfor
	di2=di2(iumin:iumax,*)
	lonr=(double([iumin,iumax])+0.5)/sg.neq*360-180
	latr=[ll.lat(0,0),ll.lat(0,ll.nv-1)]
;
; detect image wrapping round dateline (if so will occupy all u)
; if that happens try to make image/lon range smaller by shifting longitudes by 180 degrees
;
	sz=size(di2)
	if sz(1) eq sg.neq and not keyword_set(nowrap) then begin
		di2=shift(di2,sg.neq/2)
		wnz=where(total(di2 ne nodata,2) ne 0,nw)
		imax=max(wnz,min=imin)
		if nw gt 0 then begin
			di2=di2(imin:imax,*)
			lonr=360d0/sg.neq*(double([imin,imax])+0.5d0)
			origin=[0,180]
		endif
	endif else origin=[0,0]
	return,di2
end
function standard_grid_1d2d,d,u,v,nodata=nodata,ur=ur,vr=vr,rll=rll,ll=ll,lonr=lonr,latr=latr,ru=ru,origin=origin,nowrap=nowrap
	sz=size(u)
	if sz(0) eq 0 then message,'U not defined!'
	if sz(0) eq 2 then return,d			; assume data already gridded
	sz=size(d)
	if sz(0) gt 1 then begin
;
; cope with multi-dim input - call for each set of data at a time
;
		d2=d
		d2=reform2d(reform(d2),sz)
		nd2=n_elements(d2(0,*))
		for id=0l,nd2-1 do begin
			im=standard_grid_1d2d(d2(*,id),u,v,nodata=nodata,ll=ll,rll=rll,origin=origin,lonr=lonr,latr=latr,ru=ru,ur=ur,vr=vr)
			if n_elements(ims) eq 0 then ims=im(*,*,replicate(0,nd2))
			ims(0,0,id)=im
		endfor
		si=size(im)
		return,reform(ims,[si(1:2),sz(2:sz(0))])
	endif
	if n_elements(nodata) eq 0 then nodata=-999d0
	if n_elements(ur) eq 0 then ur=range(u)
	if n_elements(vr) eq 0 then vr=range(v)
	url=long(ur)
	vrl=long(vr)
	nu=(url(1)-url(0))
	nv=(vrl(1)-vrl(0))
	d2=make_array(nu,nv,value=(d(0))*0+nodata)	; array of same type as D initialised to no-data value
	iu=long(u-url(0))
	iv=long(v-vrl(0))
	wh=where(iu ge 0 and iu lt nu and iv ge 0 and iv lt nv,nw)
	if nw gt 0 then d2(iu,iv)=d
	if keyword_set(rll) then begin
		if rll.sd eq 1 then begin
			if n_elements(ru) eq 0 then ru=1
		endif else if rll.sat eq 1 then begin
			if n_elements(ru) eq 0 then ru=0
		endif else if n_elements(rll.ll) eq 1 then begin
			if rll.sd eq 0 then d2=standard_grid_sin2ll(d2,ur,vr,rll,nodata,ll=ll,lonr=lonr,latr=latr,origin=origin,nowrap=nowrap)
		endif
	endif
	if keyword_set(ru) then begin
		irev=reverse(lindgen(nu))
		d2=d2(irev,*)
	endif
	return,d2
end
