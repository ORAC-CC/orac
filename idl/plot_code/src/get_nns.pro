;===============================================================================
;+
; GET_NNS
;
; This routine finds the closest values in an array to all the elements in
; another array.
; A long array will be returned, containing the indices of X2 which give
; the nearest value to each element of X1.
;
; PARAMETERS
;	X10	Arrays to be seached
;	X20	Array of values for which nearest values in X10 required
;	ILOW	Index to lower element in X10 if GLH set
;	IHIGH	Index to higher element if GLH set
;
; KEYWORDS
;	GLH	Get indices of highest X2 below X1 and lowest X2 above
;		(i.e. values of X2 which contain X1). 
;	REG	Exploit regularity of grid 
;
; R.S. 20/8/96
; $Id: get_nns.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function get_nns,x10,x20,ilow,ihigh,glh=glh,reg=reg
	if keyword_set(reg) then begin
		dx=x20(1)-x20(0)
		ixf=(x10-x20(0))/dx
		inn=long(ixf+0.5)
		nx=n_elements(x20)
		wh=where(inn gt nx-1) & if wh(0) ne -1 then inn(wh)=nx-1
		wh=where(inn lt 0) & if wh(0) ne -1 then inn(wh)=0
		if keyword_set(glh) then begin
			if dx gt 0 then begin
				ilow=long(ixf)
				ihigh=long(ixf+1)
			endif else begin
				ihigh=long(ixf)
				ilow=long(ixf+1)
			endelse
			wh=where(ilow gt nx-1) & if wh(0) ne -1 then ilow(wh)=nx-1
			wh=where(ihigh gt nx-1) & if wh(0) ne -1 then ihigh(wh)=nx-1
			wh=where(ilow lt 0) & if wh(0) ne -1 then ilow(wh)=0
			wh=where(ihigh lt 0) & if wh(0) ne -1 then ihigh(wh)=0
		endif
		return,inn
	endif
;
; sort arrays into order for better search efficiency
;
	so1=sort(x10)
	so2=sort(x20)
	x1=x10(so1)
	x2=x20(so2)
	nx1=n_elements(x1)
	nx2=n_elements(x2)
	ind=lonarr(nx1)
	if keyword_set(glh) then begin
		ilow=lonarr(nx1)
		ihigh=lonarr(nx1)
	endif
;
; do search
;
	ii=0L
	i=0L
	while i lt nx1 do begin
		x=x1(i)
		if ii lt nx2-1 then begin
			d1=abs(x2(ii)-x)
			d2=abs(x2(ii+1)-x)
			while d2 le d1 and ii lt nx2-2 do begin
				ii=ii+1
				d1=d2
				d2=abs(x2(ii+1)-x)
			endwhile
			if d2 lt d1 then ii=ii+1
		endif
		ind(i)=ii
		if keyword_set(glh) then begin
			if x2(ii) gt x then begin
				ihigh(i)=ii
				if ii gt 0 then ilow(i)=ii-1 else ilow(i)=ii
			endif else begin
				ilow(i)=ii
				if ii lt nx2-1 then ihigh(i)=ii+1 else ihigh(i)=ii
			endelse
		endif
		i=i+1
	endwhile
;
; return unsorted version of indices
;
	ss1=sort(so1)
	if keyword_set(glh) then begin
		ihigh=so2(ihigh(ss1))
		ilow=so2(ilow(ss1))
	endif
	return,so2(ind(ss1))
end
