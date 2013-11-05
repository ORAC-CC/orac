;===============================================================================
;+
; CC_BOX1
;
; Plots 'contour' as boxes from QUICK_CC,/CCB
;
; PARAMETERS
;	D	Data
;	X	X-axis 
;	Y	Y-axis 
;	LEV	contour levels
;	COL	colours
;
; KEYWORDS
;	MAX_VALUE	No-data max-value  (don't plot if greater than this)
;	MIN_VALUE	No-data min-value  (don't plot if less than this)
;	PV	print value (or string see SPV) in centre of box 
;	CHARSIZE char-size for value
;	FORM	Format string for text on box
;	SPV	Set of strings to print in box
;		Array must be NX,NY,NL
;		which NL in number of lines of text in each box
;	NLG	Do not add <,> signs where colour scale exceeded
;		1 = fill regions where data above levels, not below
;		2 = fill regions where data below levels, not above
;		3 = fill regions where data above and below levels
;		4 = only fill where data between levels
;	PLG	After applying NLG, if a <,> sign is to be printed,
;		instead print the value of the data.
;	SFPV	Set number of significant figures for default PV strings
;
; R.S. 05/09/01
;-
;===============================================================================
pro cc_box1,d,x,y,lev,col,max_value=max_v,pv=pv,charsize=chs,form=form,min_value=min_v,nlg=nlg,spv=spv,plg=plg,sfpv=sfpv,flg=flg
	if n_elements(sfpv) eq 0 then sfpv=2
	ny=n_elements(d(0,*))-1
	nx=n_elements(d(*,0))-1
	isx=[0,0,1,1,0]
	isy=[0,1,1,0,0]
	if n_elements(max_v) eq 0 then max_v=max(abs(d))*2
	if n_elements(min_v) eq 0 then min_v=min(-abs(d))*2
	if n_elements(chs) eq 0 then chs=!p.charsize/2.
;
; get colour table for deciding text colour
;
	tvlct,r,g,b,/get
	r=float(r) & g=float(g) & b=float(b)
	ins=sqrt(r*r+g*g+b*b)
	fgc=c0()
	bgc=!p.background
	nlg=kywd_def(nlg)
	for ix=0l,nx-1,2 do for iy=0l,ny-1,2 do begin
		pv1=keyword_set(pv)
		if d(ix,iy) le max_v and d(ix,iy) ge min_v then begin
			wh=(where(lev ge d(ix,iy)))(0)
			if wh eq -1 then wh=n_elements(lev)
			if wh eq 0 then wh=1
			col1=col(wh(0)-1)
;
; decide annotation colour
;
			if ins(col1) lt 240 then xycol=bgc $
			else xycol=fgc
;
; draw < > signs on boxes outside the level range
;
			if nlg eq 0 then begin
				if d(ix,iy) lt min(lev) then begin
					if keyword_set(plg) then pv1=1 else begin
						plots,x(ix+[1,0,1]),$
							(y(iy+[0,0,1])+y(iy+[0,1,1]))/2,$
							noclip=0
					endelse
					xycol=fgc
				endif else if d(ix,iy) gt max(lev) then begin
					if keyword_set(plg) then pv1=1 else begin
						plots,x(ix+[0,1,0]),$
							(y(iy+[0,0,1])+y(iy+[0,1,1]))/2,$
							noclip=0
					endelse
					xycol=fgc
				endif else polyfill,x(isx+ix),y(isy+iy),color=col1,noclip=0
			endif else begin
				if d(ix,iy) lt min(lev) then begin
				    if nlg/2 mod 2 eq 1 then $
					polyfill,x(isx+ix),y(isy+iy),color=col1,noclip=0 else xycol=fgc
				endif else if d(ix,iy) gt max(lev) then begin
				    if nlg mod 2 eq 1 then $
					polyfill,x(isx+ix),y(isy+iy),color=col1,noclip=0 else xycol=fgc
				endif else polyfill,x(isx+ix),y(isy+iy),color=col1,noclip=0
			endelse
		endif
		if keyword_set(pv1) then begin
			if d(ix,iy) lt min_v then xycol=fgc
			if keyword_set(spv) then xyos=reform(spv(ix/2,iy/2,*)) $
			else xyos=sig_figs(d(ix,iy),sfpv,/tzp,/trim)
			nlin=n_elements(xyos)
			yp=interpol_rs(y([iy,iy+1]),[0.,1.],$
				(findgen(nlin)+1)/(nlin+1))
			xp=mean1(x([ix,ix+1]))
;yp=.37
			for ilin=0l,nlin-1 do xyouts,xp-.2,yp(ilin)-.25,$
				strtrim(xyos(nlin-1-ilin),2),$
				alig=0.3,chars=chs,noclip=0,col=xycol


		endif
	endfor
end
