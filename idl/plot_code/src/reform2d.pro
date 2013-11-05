;===============================================================================
;+
; REFORM2D
;
; Reform multi-dim array into 2D array, keeping first dimension
; and joining all the rest together
;
; PARAMETERS
;I	D	multi-dim array
;O	SZ	The original array "size"
;
; KEYWORDS
;	BACK	Set to a "size" array to reform the 2-d array back to its
;		original dimensions
;	KL	keep last dimension and join all previos
;	NOTEMP	Do not use temporary variable (so input not destroyed)
;
; R.S. 02/12/03
; $Id: reform2d.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function reform2d,d,sz,back=bsz,kl=kl,notemp=notemp
	if keyword_set(bsz) then begin
		if bsz(0) le 1 then return,d $
		else return,reform(temporary(d),bsz(1:bsz(0)))
	endif
	sz=size(d)
	if sz(0) le 2 then return,d
	if keyword_set(kl) then begin
		d2=sz(sz(0)+2)/sz(sz(0))
		if keyword_set(notemp) then return,reform(d,d2,sz(sz(0)))
		return,reform(temporary(d),d2,sz(sz(0)))
	endif else begin
		d2=sz(sz(0)+2)/sz(1)
		if keyword_set(notemp) then return,reform(d,sz(1),d2)
		return,reform(temporary(d),sz(1),d2)
	endelse
end
