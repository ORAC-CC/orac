;===============================================================================
;+
; C0.PRO
;
; This function returns the forground color index to use by default on 
; X window or postscript displays
;
; R.S. 13/9/95
; $Id: c0.pro 414 2010-09-14 13:56:52Z rsiddans $
;-
;===============================================================================
function c0
	dev=strupcase(!d.name)
	if dev eq 'X' or dev eq 'Z' then return,!d.n_colors-1 else $
	if dev eq 'PS' then return,0 else $
	begin
		print,'C0: Unknown device: ',dev
		retall
	endelse
end
