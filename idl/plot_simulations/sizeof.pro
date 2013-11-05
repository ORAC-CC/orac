;===============================================================================
;+
; SIZEOF
;
; Return number of bytes to store each element of given variable
;
; PARAMETERS
;	D	Data
;
; KEYWORDS
;	
;
; R.S. 16/06/11
; $Id$
;-
;===============================================================================
function sizeof,d
	sz=size(d)
	it=sz(sz(0)+1)
	n32=4
	sz=[-999l,1,2,4,4,8,8,-999,-999,16,n32,-999,2,4,8,8]
	return,sz(it)
end
