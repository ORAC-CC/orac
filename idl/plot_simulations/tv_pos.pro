;===============================================================================
;+
; TV_POS
;
; This does the same as TV, except that the position keyword defines the
; normalised limits of the image.
;
; PARAMETERS
;	IM	As TV
;
; KEYWORDS
;	_EXTRA sent to TV
;	POSITION defines normalised extent of image. If not set, then region is
;		taken from current clip region.
;	SCL	Set to use TVSCL
;	ISEL	set sampling interval to apply before plotting to PS device
;		(reduce size of PS !) - 2 element vector of x,y sampling intervali
;	FULL	Set to plot full size in whole window
;	FP	Set to plot full size in current p.multi plot window
;
; R.S. 29/04/97
; $Id: tv_pos.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
pro tv_pos,im,x,y,_EXTRA=extra,position=position,scl=scl,isel=isel,full=full,fp=fp,true=true
	if not keyword_set(scl) then proc='tv' else proc='tvscl'
	if keyword_set(full) then plot,[0,1],position=[0,0,1,1],/nodata,xst=5,yst=5
	if keyword_set(fp) then plot,[0,1],position=ypos([0,0,1,1],[0,0,1,1]),/nodata,xst=5,yst=5
	if not keyword_set(position) then begin
		cr0=convert_coord(!p.clip(0),!p.clip(1),/dev,/to_normal)
        	cr1=convert_coord(!p.clip(2),!p.clip(3),/dev,/to_normal)
		position=[cr0(0:1),cr1(0:1)]
	endif
	if !d.name ne 'PS' then begin
		co0=convert_coord(position(0),position(1),/norm,/to_dev)
		co1=convert_coord(position(2),position(3),/norm,/to_dev)
		xs=co1(0)-co0(0)
		ys=co1(1)-co0(1)
		im2=congrid(im(*,*,0),xs,ys)
		sz=size(im)
		if sz(0) eq 3 then begin
			im2=im2(*,*,replicate(0,sz(3)))
			for i=1,sz(3)-1 do begin
				im2(0,0,i)=congrid(im(*,*,i),xs,ys)
			endfor
		endif
		call_procedure,proc,im2,co0(0),co0(1),_EXTRA=extra,/dev,true=true
	endif else begin
		if keyword_set(isel) then begin
			nx=n_elements(im(*,0))
			ny=n_elements(im(0,*))
			isx=lindgen(nx/isel(0))*isel(0)
			isy=lindgen(ny/isel(1))*isel(1)
			im1=im(isx,*) & im1=im1(*,isy)
		endif else im1=im
		xs=position(2)-position(0)
		ys=position(3)-position(1)
		if keyword_set(true) then begin
;
; avoid sending true colours through the colour table map -
; temporarily set colour table to b-w linear
;
			tvlct,r0,g0,b0,/get
			loadct,0
		endif
		call_procedure,proc,im1,position(0),position(1),_EXTRA=extra,$
			xsize=xs,ysize=ys,/normal,true=true
		if keyword_set(true) then tvlct,r0,g0,b0
	endelse
end
