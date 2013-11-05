;===============================================================================;+
;+
; STDERR.PRO
;
; This routine returns the standard error of a set of data. Error can be
; determined after polynomial fit if required.
;
; PARAMETERS
;	Y	Data values
;	X	x-values for data (only required if fitting performed)
;
; KEYWORDS
;	FIT	Set to order of polynomial fit to be applied before
;		getting stderr.
;	MEAN 	Mean of data (after any poly fit).
;	SDEV	Standard devisiton of data (after any poly fit).
;	RMS	Return adjusted rms deviation of data (after any poly fit).
;	PLOT	Plot the fit
;
;
; R.S. 20/11/95
; $Id: stderr.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function stderr,y,x,fit=fit,mean=mean,sdev=sdev,rms=rms,plot=plot
	sz=size(y)
	if sz(0) eq 1 then nvec=1 else $
	if sz(0) eq 2 then nvec=sz(2) else message,'Y must have 1 or 2 dimensions.'
	if sz(sz(0)+1) eq 5 then dbl=1 else dbl=0
	nx=sz(1)
	if nx lt 2 then message,'Data must have at least 2 points.'
	if n_elements(x) eq 0 then x=findgen(nx)

	if dbl then begin
		sdev=dblarr(nvec)
		mean=dblarr(nvec)
		fnx=double(nx)
	endif else begin
		sdev=fltarr(nvec)
		mean=dblarr(nvec)
		fnx=float(nx)
	endelse
;
; get mean/std.deviation
;
	for i=0,nvec-1 do begin
		vec=y(*,i)
		if n_elements(fit) gt 0 then begin
			coefs=poly_fit(x,vec,fit)
			if keyword_set(plot) then begin
				if i eq 0 then set_a4,n=nvec
				plot,x,vec,psy=1,/yno
				oplot,x,poly(x,coefs)
			endif
			vec=vec-poly(x,coefs)
		endif
		tot=total(vec)
		tot2=total(vec*vec)
		mean[i]=tot/fnx
		sdev(i)=sqrt(tot2/fnx - mean[i]*mean[i])
	endfor
;
; convert to standard error and rms deviation
;
	std=sdev/sqrt(fnx-1.)
	rms=std*sqrt(fnx)
;
; convert from 1-element arrays
;
	if n_elements(std) eq 1 then begin
		std=std(0)
		sdev=sdev(0)
		mean=mean[0]
		rms=rms(0)
	endif
	return,std
end
