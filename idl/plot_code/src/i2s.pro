;============================================================================
;+
;	function I2S
;
;	Description
; Convert integer to string removing extra blank spaces
; Optionally add extra character to make fixed length.
; Extra characters may be added before or after integer
;
;	Arguments
;  I  I Integer (scalar/vector/matrix) to be coverted to a string
;  I  LEN Length to expand value too 
; 
;	Keywords
;  I  XTRA extra character to be used to fill to LEN (default '0')
;  I  AFTER add characters after I (default is before)
;  I  MAXVAL LEN values are max values, set length based on these
;
;	Date
; B.Latter : 12th December 2000
; B.Latter : 07 August 2006 : Add MAXVAL and accept multiple lengths
;-
;============================================================================
function i2s,i,len,xtra=xtra,after=after,maxval=maxval
s=strtrim(string(long64(i)),2) ; remove extra spaces

;if keyword_set(len) then s=str_pad(s,len,xtra=xtra,after=after)
if keyword_set(len) then begin
	nlen=n_elements(len)
	if nlen eq 1 then s=str_pad(s,len,xtra=xtra,after=after) else begin
		if nlen ne n_elements(s) then $
			message,'Miss-match in required length info. Stopping...'
; If max values passed, find length of each and set padding accordingly
		if keyword_set(maxval) then sl=strlen(strtrim(string(len),2)) $
		else sl=len ; lengths passed
		for i=0,nlen-1 do s[i]=str_pad(s[i],sl[i],xtra=xtra,after=after)
	endelse
endif
return,s
end
