;===============================================================================
;+
; SETUP_LINEAR
;
; This sets up parameters to allow rapid linear interpolation of a number of
; of functions y_n(x0) to a new grid, x1, assuming that the x0 and x1 are
; constant and only the values of y are changing.
;
; Interpolation can then be performed for all y_n:
;	y_n(x1) = w1.y_n(i1) + w2.y_n(i2)
;
; or for an array...
;
; 	di = f_matvec(d(i1,*),w1) + f_matvec(d(i2,*),w2) 
;
; or 	di = f_matcol(d(*,i1),w1) + f_matcol(d(*,i2),w2)
; depending on order of d
;
; PARAMETERS
;	X0	Original grid
;	X1	New grid
;	I1	First index to Y
;	I2	Second index to Y
;	W1	Weight associated with I1
;	W2	Weight associated with I2
;	WC	See spline below
;	WD	See spline below
;
;
; KEYWORDS
;	ZERO	Set to zero extrapolations
;	EXT	Set to perform extrapolations
;	NN 	Set up to do nearest neighbour (still apply ZERO keyword)
;	OP	If n_elements(x1) eq 1, then return scaler not vector 
;		results from I1,I2,W1,W2
;	DWDX	Return derivative of W2 wrt x (1- deriv W1 wrt X)
;	VL	Set to use IDL's value_locate function instead of get_nns
;		(may be faster). ASSUMES X0 IS MONOTONICALLY ASCENDING
;	SPLINE	Set to also compute weights to be applied to 2nd derivatives to
;		optain a cubic spline interpolation (see num recip in c s3.3). 
;		In this case, arguments WC and WD will be defined,
;		giving coefs C and D in eqn 3.3.3. 
;		Keywords DCDX and DDDX are also returned, containing
;		the derivatives of WC and WD wrt X. Note
;		that  = w1*y(i1)+w2*y(i2)+wc*d2ydx2(i1)+wd*d2ydx2(i2)
;		where yi is spline interpolated value, and d2ydx2 are
;		the 2nd derivatives of y, obtained using SPL_INIT.
;
; R.S. 23/03/98
; $Id: setup_linear.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
pro setup_linear,x0,x1,i1,i2,w1,w2,wc,wd,zero=zero,ext=ext,nn=nn,op=op,dwdx=dwdx,vl=vl,spline=spline,$
	dcdx=dcdx,dddx=dddx
	n1=n_elements(x1)
	if keyword_set(spline) then if n_elements(ext) eq 0 then ext=1
	if n_elements(x0) eq 0 then message,'X0 undefined'
	if n_elements(x0) eq 1 then begin
;
; deal with case of only 1 element in input array 
;
		if keyword_set(zero) then begin
			message,'ZERO extrapolation with only 1 data value not implemented !'
		endif else begin
			i1=lonarr(n1)
			i2=lonarr(n1)
			w2=dblarr(n1)
			w1=w2+1
			dwdx=dblarr(n1)
			if n_elements(x1) eq 1 and keyword_set(op) then begin
				i1=i1(0)
				i2=i2(0)
				w1=w1(0)
				w2=w2(0)
				dwdx=dwdx(0)
			endif
			return
		endelse
	endif
	if keyword_set(vl) then begin
		n0=n_elements(x0)
		ilow=value_locate(x0,x1)
		ihigh=ilow+1
		wh=where(ilow lt 0,nw)
		if nw gt 0 then ilow(wh)=0
		wh=where(ihigh ge n0,nw)
		if nw gt 0 then ihigh(wh)=n0-1
	endif else begin
		nns=get_nns(x1,x0,ilow,ihigh,/glh)
	endelse
	sz=size(x1)
	if sz(sz(0)+1) eq 5 then w2=dblarr(n1) else w2=fltarr(n1)
	dwdx=w2
	if keyword_set(ext) then begin
		n0=n_elements(x0)
		whoor=where(ihigh eq ilow,noor)
		if noor gt 0 then begin
			wh0=where(ihigh(whoor) eq 0)
			if wh0(0) ne -1 then ihigh(whoor(wh0))=1
			wh0=where(ihigh(whoor) eq n0-1)
			if wh0(0) ne -1 then ilow(whoor(wh0))=n0-2
		endif
	endif
	i1=ilow
	i2=ihigh
	wh=where(ihigh ne ilow,nne)
	if nne ne 0 then begin
		dwdx(wh)=1d0/(x0(ihigh(wh))-x0(ilow(wh)))
		w2(wh)=(x1(wh)-x0(ilow(wh)))/(x0(ihigh(wh))-x0(ilow(wh)))
	endif
	if keyword_set(nn) then begin
		w2=long(w2+0.5)
		if sz(sz(0)+1) eq 5 then w2=double(w2) else w2=float(w2)
	endif
	w1=1.-w2
	if keyword_set(zero) then begin
		xmax=max(x0,min=xmin)
		wh=where(x1 gt xmax or x1 lt xmin)
		if wh(0) ne -1 then begin
			w1(wh)=0.
			w2(wh)=0.
			dwdx(wh)=0.
		endif
	endif
	if n_elements(x1) eq 1 and keyword_set(op) then begin
		i1=i1(0)
		i2=i2(0)
		w1=w1(0)
		w2=w2(0)
		dwdx=dwdx(0)
	endif
	if keyword_set(spline) then begin
;
; compute coefs of spline interpolation and derivatives wrt x
;
if min(abs(dwdx)) eq 0 then stop
		dx2=dwdx*dwdx*6
		w12=w1*w1
		w22=w2*w2
		wc=(w12*w1-w1)/dx2
		dcdx=-dwdx*(3*w12-1)/dx2
		wd=(w22*w2-w2)/dx2
		dddx=dwdx*(3*w22-1)/dx2
if noor gt 0 then begin
;
; do not apply spline out of range - do linear extrapolation
;
	wc(whoor)=0
	wd(whoor)=0
	dcdx(whoor)=0
	dddx(whoor)=0
endif
	endif
end
