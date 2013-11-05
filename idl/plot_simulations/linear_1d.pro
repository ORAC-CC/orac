;===============================================================================
;+
; LINEAR_1D.PRO
;
; function returns linear interpolation(s) to a 1-d array.
;
; PARAMETERS
;	V	Input vector
;	X	X values of input. Program is faster if this is in 
;		ascending order
;	U	X values of output.
;
; KEYWORDS
;
;	EXTRAPOLATE	Set to allow extrapolation, otherwise nearest
;			extreme value is supplied.
;	PD	Set for positive definite result. If interpolation/
;		extrapolation gives a negative value, 0 is returned.
;       LOG	Set to do logarithmic interpolation/extrapolation
;	ZERO	Set out of range points to zero.
;	XFLAG	Set this to the name of an integer which flags whether or
;		not any points were out of range. (0 for no points out of range,
;		1 otherwise).
;	TEQ	Set to run through array and remove points with identical X values
;
;
; R.Siddans 31/3/95
; $Id: linear_1d.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function linear_1d,v1,x1,u,pd=pd,extrapolate=extrap,log=log,zero=zero,xflag=xflag,$
	teq=teq
	n_out=n_elements(u)
	n_in=n_elements(v1)
	xflag=0
	result=u
	if n_in eq 1 then return,replicate(v1(0),n_out)
	if keyword_set(log) then v=alog(v1) else v=v1
	x=x1
	asc=where((x(1:n_in-1)-x(0:n_in-2)) lt 0.0)
	if asc(0) ne -1 then begin
;
; sort array
;
		sx=sort(x)
		v=v(sx)
		x=x(sx)
	endif
	if keyword_set(teq) then begin
		nx=n_elements(x)
		dx=[999.,x(1:nx-1)-x(0:nx-2)]
		wh=where(dx eq 0.)
		if wh(0) ne -1 then begin
			;message,/cont,'WARNING: Points with identical X values removed. First value at given point retained.'
			wh=where(dx ne 0.)
			x=x(wh)
			v=v(wh)
		endif
		n_in=n_elements(v)
	endif
	for i=0L,n_out-1 do begin
		ui=u(i)
		wh_gt=where(x gt ui)
		wh_gt=wh_gt(0)
		if wh_gt eq -1 then begin
;
; u > all x values 
;
			i2=n_in-1
			xflag=1
			if keyword_set(extrap) then i1=i2-1 $
			else i1=i2
		endif else if wh_gt eq 0 then begin
;
; u < all x values
;
			i1=0
			xflag=1
			if keyword_set(extrap) then i2=1 $
			else i2=i1
		endif else begin
;
; u within range of x values
;
			i2=wh_gt
			i1=i2-1
		endelse
		if i1 eq i2 then begin
			if keyword_set(zero) then begin
				if ui eq x(i1) then result(i)=v(i1) $
				else result(i)=0.
			endif else result(i)=v(i1)
		endif else begin
			result(i)=v(i1)+(ui-x(i1))*(v(i2)-v(i1))/$
				(x(i2)-x(i1))
		endelse
		if keyword_set(pd) then begin
			if result(i) lt 0.0 then res(i)=0.0
	       	endif
	endfor
	if keyword_set(log) then result=exp(result)
	return,result
end
