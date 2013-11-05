;===============================================================================
;+
; DIVIDE_NZ
;
; Divide one array by another checking for 0 values in second array first
;
; PARAMETERS
;	A	numerator
;	B	denominator
;
; KEYWORDS
;	NO_DATA	Set a value which will appear in result where division invalid
;	ZERO	Set to value in data treated as 0 (so this can divide arrays
;		ignoring no-data values)
;		
;
; R.S. 17/05/07
; $Id: divide_nz.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function divide_nz,a,b,no_data=no_data,zero=zero
	if n_elements(no_data) eq 0 then no_data=0
	if n_elements(zero) eq 0 then zero=0
	c=a*0+b*0+no_data
	if n_elements(b) eq 1 then begin
		if b eq 0 then return,c else return,a/b
	endif
	wh=where(b ne zero and finite(b) eq 1,nw)
	if nw gt 0 then c(wh)=a(wh)/b(wh)
	return,c
end
