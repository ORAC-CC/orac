;===========================================================================
;+
;	function FNAME
;
;	Description
; Strip suffix from filename. Searches passed scalar/array removes for
; prefix and suffix. (Default returns from last '/' to last '.' or passed
; pre/suffix.) e.g for reducing path+filename to filename only.
;
;	Parameters
; X string/array to check
;
;	Keywords
; PRE prefix to remove (default is '/')
; SUF suffix to check for (default is from '.')
; A prefix removed
; B suffix removed
;
;	Date
;	B. Latter : 30th April 2001
;-
;===========================================================================
function fname,x,pre=pre0,suf=suf0,a=a,b=b
if n_elements(pre0) eq 0 then pre='/' else pre=pre0
if n_elements(suf0) eq 0 then suf='.' else suf=suf0
xl=strlen(x)
pl=strlen(pre)
sl=strlen(suf)
y=x & a=mk_var(size(x)) & b=a
for i=0,n_elements(x)-1 do begin
	p=strpos(x(i),pre,/reverse_search)
	if p lt 0 then a(i)='' else begin ; separate prefix
		a(i)=strmid(x(i),0,p+pl)
		y(i)=strmid(x(i),p+pl,xl(i))
	endelse
	s=strpos(y(i),suf,/reverse_search)
	if s lt 0 then b(i)='' else begin ; separate prefix
		b(i)=strmid(y(i),s,xl(i))
		y(i)=strmid(y(i),0,s)
	endelse
endfor ; i
return,y
end
