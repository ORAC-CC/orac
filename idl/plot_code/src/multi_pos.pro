;===============================================================================
;+
; MULTI_POS
;
; This function takes a 4-element POSITION (see plot keyword) and returns
; the vector scaled into the correct plot window for the current !p.multi
; values. i.e. it allows consistent use of the POSITION keyword while in 
; multiple plot mode.
;
; PARAMETERS
;	P0	The 4-elements normalise position
;
; KEYWORDS
;       CBAR    Set this to get position suitable for defualt position of color bar in CBAR routine.
;       BT      Set this for position OK for a big title, subtitle and figure string.
;
; R.S. 19/07/1995
; $Id: multi_pos.pro 414 2010-09-14 13:56:52Z rsiddans $
;-
;===============================================================================
function multi_pos,p0,cbar=cbar,bt=bt
	chs=!p.charsize
	if chs le 0 then chs=1.
	if keyword_set(cbar) or n_elements(p0) eq 0 then p0=[0.2,0.12,0.95,0.9]
	if keyword_set(bt) then $
		p0= [0.1085*chs,0.1*chs,1.-0.0322*chs,1.-0.03*chs]
; pns= [0.1085*chs,0.0605*chs,1.-0.0322*chs,1.-0.03*chs]
	n=!p.multi(0)
	nc=max([!p.multi(1),1])
	nr=max([!p.multi(2),1])
	np=nr*nc
	if n le 0 then n=np
	cp=np-n
	if !p.multi(4) eq 0 then begin
;
; column major
;
		cc=cp mod nc
		cr=cp/nc
	endif else begin
;
; row major
;
		cc=cp/nr
		cr=cp mod nr
	endelse
;
; get normalised coords of bottom left hand corner of current plot window
;
	fnc=float(nc)
	fnr=float(nr)
	x0=float(cc)/fnc
	y0=float(nr-cr-1)/fnr
	p1=[p0(0)/fnc+x0,$
		p0(1)/fnr+y0,$
		p0(2)/fnc+x0,$
		p0(3)/fnr+y0]
	return,p1
end
