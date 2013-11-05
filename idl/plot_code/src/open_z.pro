;===============================================================================
;+
; OPEN_Z
;
; Open Z buffer in normal way for constructing pngs
;
; PARAMETERS
;	PD	On return contains previous device name
;
; KEYWORDS
;	XSIZE	X-size (pixels)
;	YSIZE	Y-size
;	Z_BUFFERING
;
; R.S. 22/05/07
; $Id: open_z.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
pro open_z,xsize=xs,ysize=ys,z_buffering=z_buffering,pd
	pd=!d.name
	set_plot,'z'
        if n_elements(xs) eq 0 then xs=4008
        if n_elements(ys) eq 0 then ys=2004
        if n_elements(z_buffering) eq 0 then z_buffering=0
	device,z_buffering=z_buffering
        device,set_resolution=[xs,ys]
        device,set_colors=256
end
