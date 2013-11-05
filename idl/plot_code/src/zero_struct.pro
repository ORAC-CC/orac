;===============================================================================
;+
; ZERO_STRUCT
;
; Set all tags in a structure to zero (or other null value)
; Set strings to empty strings & pointers to null pointers
; NB DOES NOT FREE MEMORY pointed too by pointers
;
; PARAMETERS
;	S	Original structure
;
; KEYWORDS
;	NULL	Specify null value for numeric fields
;
; R.S. 08/08/06
; $Id: zero_struct.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function zero_struct,s,null=null
	if n_elements(null) eq 0 then null=0
	s1=s
	for i=0,n_tags(s1)-1 do begin
		sz=size(s1.(i))
		ty=sz(sz(0)+1)
		if ty eq 7 then v='' $
		else if ty eq 8 then v=zero_struct((s1.(i))(0)) $
		else if ty eq 10 then v=ptr_new() $
		else v=null
		if ty lt 7 then s1.(i)=s1.(i)*0+null $
		else begin
			s2=s1.(i)
			for ii=0l,n_elements(s2)-1 do s2(ii)=v
			s1.(i)=s2
		endelse
	endfor
	return,s1
end
