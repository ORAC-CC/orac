;===============================================================================
;+
; SIG_FIGS
;
; Return string version of number to given significant figures
;
; PARAMETERS
;	X	Values(s)
;	N	Significant figures
;
; KEYWORDS
;	LATEX	Write exponential format for latex
;	TRIM	Set to remove 0s at end of number after apply N sig figs
;	TZP	Set to remove '0.' at beginning of a number (only have '.')
;
; R.S. 21/01/03
; $Id: sig_figs.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function sig_figs,x,n,latex=latex,trim=trim,tzp=tzp
	nx=n_elements(x)
	sz=size(x)
	if n_elements(sz) eq 3 then s='' $
	else begin
		sz(sz(0)+1)=7
		s=make_array(size=sz)
	endelse
	ap=abs(x)
	wh=where(ap ne 0,nw)
	ep=ap
	if nw gt 0 then ep(wh)=double(long(alog10(ap(wh))))
	wh=where(ap gt 1)
	if wh(0) ne -1 then ep(wh)=ep(wh)+1
	fac=(10d0^(n-ep))
	v1=divide_nz(double(long(ap*fac+0.5)),fac)
	wh=where(x lt 0)
	if wh(0) ne -1 then v1(wh)=-v1(wh)
	maf=5	; max power to print in E format not double
	mif=-2	; min power to print in E format not double
	for i=0l,nx-1 do begin
	    if x(i) eq 0 then begin
		s(i)=0
	    endif else begin
		if ep(i) gt maf then begin
			form='(E'+trim_zero(n+6)+'.'+trim_zero(n-1)+')'
		endif else if ep(i) lt mif then begin
			form='(E'+trim_zero(n+6)+'.'+trim_zero(n-1)+')'
		endif else if ep(i) gt 0 then begin
			if ep(i) lt n then begin
				form='(F'+trim_zero(n+2)+$
					'.'+trim_zero(n-ep(i))+')'
			endif else begin
				form='(I'+trim_zero(ep(i)+2)+')'
			endelse
		endif else begin
			form='(F'+trim_zero(n-ep(i)+3)+$
				'.'+trim_zero(n-ep(i))+')'
		endelse
		s(i)=string(v1(i),form=form)
		if keyword_set(trim) then s(i)=strupcase(trim_zero(float(s(i))))
		if keyword_set(latex) then begin
			sp=strpos(s(i),'E')
			if sp ge 0 then begin
				s1=strmid(s(i),0,sp)
				s2=strmid(s(i),sp+1)
				s(i)=s1+'$\times10^{'+$
					trim_zero(float(s2))+'}$'
			endif
		endif
		if keyword_set(tzp) then begin
			s(i)=strtrim(s(i),2)
			if strpos(s(i),'0.') eq 0 then s(i)=strmid(s(i),1)
		endif
	    endelse
	endfor
	return,strtrim(s,2)
	
end
