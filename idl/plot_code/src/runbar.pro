;===============================================================================
;+
; RUNBAR
;
; Print a bar indicating progress through a loop
;
; PARAMETERS
;	I	Loop counter
;	N	N elements in I
;	P	Previous position / time structure.
;		If passed in/out will stop re-drawing bar if I not
;		sufficiently changed to modify the progress bar
;
; KEYWORDS
;	NC	Number of chars in bar
;
; R.S. 26/01/04
; $Id: runbar.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
pro runbar,i,n,p,nc=nc
	if n_elements(nc) eq 0 then nc=40
	if n_tags(p) eq 0 or i eq 0 then p={t0:systime(1),ic:-1}
	frac=float(i)/float(n-1)
	ic=fix(frac*nc)
	if ic eq p.ic and i lt n-1 then begin
		return
	endif
	txt=replicate(46b,nc)
	if ic gt 0 then txt(0:ic-1)='35'
;
; calculate time remaining
;
	if p.ic gt 2 then begin
		tpi=float(systime(1)-p.t0)/i
		trem=trim_zero(long(tpi*i))+'s '+trim_zero(long(tpi*(n-i)))+'s '
	endif else begin
		tpi=0.
		trem=''
	endelse
	p={t0:p.t0,ic:ic}
;
; print,bar
;
	str='|'+string(txt)+'| '+trim_zero(fix(frac*100))+'% '
	if i eq n-1 then str=str+' Total time: '+trim_zero(tpi*i)+'s' $
	else if frac gt 0.05 then str=str+trem
	str=str+string(13b)
	print,str,format='(A,$)'
	if i ge n-1 then print,''
end
