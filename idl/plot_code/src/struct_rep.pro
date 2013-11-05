;===============================================================================
;+
; STRUCT_REP
;
; Replace given tag in anonymous structure with given value
; (even if different size to original). 
;
; Warning: this routine not good on memory usage.
;
; PARAMETERS
;	X	Structure. If this is an array then all elements of the
;		array will be replace with the same V
;	TAG	Tag to be replaced
;	V	Value to set tag to.
;
; KEYWORDS
;	T2	Set to new name for the tag which is replaced
;
; R.S. 25/04/01
; $Id: struct_rep.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function struct_rep,x,tag,val,t2=t2
	nx=n_elements(x)
	if nx gt 1 then begin
		for i=0l,nx-1 do begin
			x1=struct_rep(x(i),tag,val,t2=t2)
			if i eq 0l then x2=replicate(zero_struct(x1),nx)
			x2(i)=x1
		endfor
		return,x2
	endif
	sz=size(tag)
	tn=tag_names(x)
	if sz(sz(0)+1) eq 7 then begin
		wh=where(tn eq strupcase(tag))
	endif else wh=tag
	if wh(0) eq -1 then message,'No such tag: '+trim_zero(tag)
	for i=0,n_elements(tn)-1 do begin
		tn1=tn(i)
		if i ne wh(0) then begin
			v1=x.(i)
		endif else begin
			v1=val
			if keyword_set(t2) then tn1=t2
		endelse
		if i eq 0 then x1=create_struct(tn1,v1) $
		else x1=create_struct(x1,tn1,v1)
	endfor
	return,x1
end
