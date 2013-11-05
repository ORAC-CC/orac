;===============================================================================
;+
; SVGIF
;
; This saves the current plot window as a GIF
;
; PARAMETERS
;	FILE	Name of file to write
;
; KEYWORDS
;	XSIZE	Set to subsample image to output GIF of specified
;		XSIZE
;	PPM	Save as PPM
;	PPM	Save as PNG
;
; R.S. 14/04/97
; $Id: svgif.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
pro svgif,file2,xsize=xsize,ppm=ppm,png=png,true=true
	if n_elements(file2) eq 0 then file='idl.gif' $
	else file=new_name(file2,'gif')
	if n_elements(png) eq 0 then png=1
	pngfi=new_name(file,'png')
	if keyword_set(true) then begin
		im=tvrd(true=true)
		write_png,pngfi,im,r,g,b
		message,/cont,'Written file: '+pngfi
		return
	endif
	im=tvrd()
	sz=size(im)
	if sz(0) ne 2 then message,'Cannot interpret image dimensions'
	xs=sz(1)
	ys=sz(2)
	if keyword_set(xsize) then begin
		xs2=xsize
		frac=float(xs2)/float(xs)
		ys2=fix(frac*float(xs2))
		im=congrid(im,xs2,ys2)
	endif
	tvlct,r,g,b,/get
	write_png,pngfi,im,r,g,b
	message,/cont,'Written file: '+pngfi
	if keyword_set(png) then return
	if keyword_set(ppm) then begin
		cmd='pngtopnm '+pngfi+' | ppmtogif > '+file
	endif else begin
		file=new_name(file,'ppm')
		cmd='pngtopnm '+pngfi+' > '+file
	endelse
	spawn,cmd
	message,/cont,'Written file: '+file
end
