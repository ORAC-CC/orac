;==========================================================================
;+
; YPOS
;
; This function returns a vector giveing the normalised position of a 
; plot which will fit evenly within the current !p.multi setting, while
; still allowing the specified top, bottom, left and right,
; normalised margins in the whole plot window.
;
; X position 
;
; PARAMETERS
;	WPOS	Specifies normalised position for whole plot
;		window (i.e. all plots fit within this position).
;		See plot keyword position for format.
;	PPOS	Specifies normalised position of plot
;		(are enclosed by axes only) within the
;		individual plot window
;
; KEYWORDS
;	MASK	An array which defines the plot positions e.g.
;		mask=	00
;			00
;			12
;		returns positions to get a big first plot with the
;		next two plots underneath.
;	MARGIN	Treat ppos as specifying margins for all plots, 
;		using normalised coords in whole page.
;
; If these are omitted then default values are assumed.
;
; R.S. 6/11/95
; $Id: ypos.pro 414 2010-09-14 13:56:52Z rsiddans $
;-
;==========================================================================
function ypos,wpos,ppos,mask=mask,margin=margin
	chs=!p.charsize
	if chs le 0 then chs=1.

	if n_elements(wpos) eq 0 then wpos=[0.,0.03,1.,0.96]
	n=!p.multi(0)
	nc=max([!p.multi(1),1])
	nr=max([!p.multi(2),1])
	np=nr*nc
	if n le 0 then n=np
	cp=np-n
	if not keyword_set(mask) then begin
;
; default row major...
;
		if !p.multi(4) eq 0 then mask=findgen(nc,nr) $
;
; default column major...
;
		else mask=reform(transpose(findgen(nc,nr)),nc,nr)
	endif

	sz=size(mask)
	mnc=sz(1)
	if sz(0) gt 1 then mnr=sz(2) else mnr=1
	cm=where(mask eq cp)
	if cm(0) eq -1 then message,'Current plot not defined in mask.'
	cmc=cm mod mnc
	cmr=cm / mnc
	fnc=float(mnc)			; total number of rows
	fnr=float(mnr)			; number of columns
	cnc=float(max(cmc)-min(cmc)+1)  ; number of columns used by this plot
	cnr=float(max(cmr)-min(cmr)+1)	; number of rows used by this plot
	r0=float(max(cmr))		; bottom left row of current plot
	c0=float(min(cmc))		; bottom left col of current plot
;
; get normalised coords of bottom left hand corner of current plot window
;
	dx=(wpos(2)-wpos(0))
	dy=(wpos(3)-wpos(1))
	x0=(wpos(0)+wpos(2))/2.-dx/2.
	y0=(wpos(1)+wpos(3))/2.-dy/2.
;
; get normalised coords of individual plot
;
	dx0=dx/fnc	; normalised x-size of 1 plot
	dy0=dy/fnr	; normalised y-size of 1 plot
	dx1=dx0*cnc	; normalised x-size of current plot
	dy1=dy0*cnr	; normalised y-size of current plot
	x1=x0+dx0*c0
	y1=y0+dy0*(fnr-r0-1.)
	x2=x1+dx1
	y2=y1+dy1
;
; adjust for normalised window or margins within individual plot window
;
	schs=chs/max([fnc,fnr])
	nx_chs=schs*float(!d.x_ch_size)/float(!d.x_size)
	ny_chs=schs*float(!d.y_ch_size)/float(!d.y_size)
	
	if keyword_set(margin) then begin
		if n_elements(ppos) eq 0 then $
			ppos=[10.*nx_chs,5.*ny_chs,3.*nx_chs,5.*ny_chs]
		y2=y2-ppos(3)
		y1=y1+ppos(1)
		x1=x1+ppos(0)
		x2=x2-ppos(2)
	endif else begin
		if n_elements(ppos) eq 0 then $
			ppos=[8.*nx_chs,5.*ny_chs,1.-3.*nx_chs,1.-5.*ny_chs]
		y2=y2-(1.-ppos(3))*dy1
		y1=y1+ppos(1)*dy1
		x1=x1+ppos(0)*dx1
		x2=x2-(1.-ppos(2))*dx1
	endelse
	return,[x1,y1,x2,y2]
end
