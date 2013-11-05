;============================================================================
;+
; function STR_PAD
;
; Description
; Optionally add extra character to make fixed length.
;	Extra characters may be added before or after integer
;
; Arguments
;  I  S0  String to pad
;  I  LEN Length to expand value too 
; 
; Keywords
;  I  XTRA Extra character to be used to fill to LEN (default '0')
;  I  AFTER Add characters after I (default is before)
;
;	Date
; B. Latter : 10th July 2001
;-
;============================================================================
function str_pad,s0,len,xtra=xtra,after=after

s=s0
	if (size(xtra))(1) ne 0 then pad=xtra else pad='0' ; define padding string
	padding=''
	for l=0,len-1 do padding=padding+pad
	for iy=0,n_elements(s(0,*))-1 do begin ; loop over 2nd dimension
		for ix=0,n_elements(s(*,0))-1 do begin ; loop over 1st dimension
			sl=strlen(s(ix,iy)) ; length of existing element
			tmp=strmid(padding,0,len-sl) ; get required padding (multi-char pad)
			if keyword_set(after) then s(ix,iy)=s(ix,iy)+tmp else s(ix,iy)=tmp+s(ix,iy)
		endfor ; end 1st dimension
	endfor ; end 2nd dimension

return,s
end
