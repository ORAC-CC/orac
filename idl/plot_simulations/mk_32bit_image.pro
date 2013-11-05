;===============================================================================
;+
; MK_32BIT_IMAGE
;
; Return 32 bit colour index for use with e.g. PL_BOXMAP from RGB colour values
;
; PARAMETERS
;	R	Red values 
;	G	Green values
;	B	Blue values
;
; KEYWORDS
;	CMAX	Set set maximum value of R,G,B used in scaling values
;		to run from 0-255 in each 8 bit
;	GAMMA	Apply gamma correction 
;
; R.S. 31/03/10
; $Id: mk_32bit_image.pro 730 2011-03-22 14:59:04Z rsiddans $
;-
;===============================================================================
function mk_32bit_image,r,g,b,gamma=gamma,cmax=cmax,cmin=cmin
	if n_elements(cmax) eq 0 then cmax=max([r,g,b])
	if n_elements(cmin) eq 0 then cmin=0.
	r1=interpol_rs([0.,1],[cmin,cmax],r)
	g1=interpol_rs([0.,1],[cmin,cmax],g)
	b1=interpol_rs([0.,1],[cmin,cmax],b)
;print,range(r1)
;print,range(g1)
;print,range(b1)
	if keyword_set(gamma) then begin
		r1=r1^gamma
		g1=g1^gamma
		b1=b1^gamma
	endif
	b=long(r1*255)+long(g1*255)*256l+long(b1*255)*256l*256l
	return,b
end
