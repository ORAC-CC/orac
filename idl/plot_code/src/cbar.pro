;===============================================================================
;+
; CBAR.PRO
;
; This routine plots a color bar. Use MULTI_POS and POSITION plot keyword 
; to get plot and CBAR in correct place.
;
; PARAMETERS
;	LEVS1	Data values
;	COLS	Corresponding colors
;
; KEYWORDS
;	POSITION Set to 4-element position of color bar: [x0,y0,x1,y1]
;		 where [x0,y0] is bottom left, [x1,y1] is top right. This
;		 should be in normalised coords of current plot window
;		 (even if multiple plots are being produced)
;	STEP	Set this to plot a stepped color bar (only specified colours
;		are plotted), otherwise smooth color change is produced.
;	TITLE	Set to title of data
;	NOMULT  Set so that POSITION is specified in absolute plot window.
;	CHARSIZE As plot keyword
;	NOADV	Set to not push back !p.multi before plotting
;	NOLINES	Do not overplot lines on colours
;	LOG	Set for log colour bar
;	TICKNAME Define for use with STEP
;	TICKS    Define for use with STEP
;	TICKV    Define for use with STEP
;
; R.S. 19/7/95
; $Id: cbar.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
pro cbar,levs1,cols,position=position0,title=title,charsize=chs,$
	nomult=nomult,step=step,noadv=noadv,nolines=nolines,log=log,$
	tickname=tickname,tickv=tickv,ticks=ticks

	if keyword_set(step) then begin
		levs=indgen(n_elements(levs1))
		if n_elements(tickname) eq 0 then tickname=trim_zero(levs1)
		if n_elements(ticks) eq 0 then ticks=n_elements(tickname)-1
		if n_elementS(tickv) eq 0 then tickv=levs
	endif else levs=levs1
	if not keyword_set(chs) then chs=!p.charsize
	if n_elements(cols) eq 0 then begin
		lmax=max(levs,min=lmin)
		nl=n_elements(levs)
		cols=float(!d.table_size-4)*(levs-lmin)/(lmax-lmin)+2
	endif
	nc=n_elements(cols)
	if n_elements(levs) ne nc then begin
		print,'CBAR: Number of colours not equal to number of levels.'
	endif
	if not keyword_set(noadv) then !p.multi(0)=!p.multi(0)+1
	if not keyword_set(position0) then $
		position0=[0.07,0.12,0.12,0.9]
	if not keyword_set(nomult) then $
		position=multi_pos(position0) $
	else position=position0
	if keyword_set(log) then levs=10.^levs
	x=reform([levs,levs],n_elements(levs),2)
	if position(2)-position(0) lt position(3)-position(1) then begin
		x=transpose(x)
		xst=5
		yst=1
		xv=indgen(2)
		yv=levs
		if keyword_set(title) then ytitle=title else ytitle=''
		xtitle=''
		xbox=[0,0,1,1,0]
		ybox=[levs(0),levs(nc-1),levs(nc-1),levs(0),levs(0)]
		if keyword_set(step) then begin
			yticks=ticks
			ytickv=tickv
			ytickname=tickname
		endif
		left=1
		xty=0
		yty=keyword_set(log)
	endif else begin
		yst=5
		xst=1
		yv=indgen(2)
		xv=levs
		if keyword_set(title) then xtitle=title else xtitle=''
		ytitle=''
		ybox=[0,0,1,1,0]
		xbox=[levs(0),levs(nc-1),levs(nc-1),levs(0),levs(0)]
		if keyword_set(step) then begin
			xticks=ticks
			xtickv=tickv
			xtickname=tickname
		endif
		left=0
		xty=keyword_set(log)
		yty=0
	endelse
	ptl=!p.ticklen
	!p.ticklen=0
	if n_elements(xticks) gt 0 then begin
	    contour,x,xv,yv,/fill,position=position,levels=levs,c_colors=cols,$
		xst=xst,yst=yst,/follow,xticklen=0,yticklen=0,$
		xtitle=xtitle,ytitle=ytitle,charsize=chs,$
		xtickv=xtickv,xticks=xticks,xtickname=xtickname,$
		yticks=1,ytickv=[0,1],yty=yty,xty=xty
	endif else if n_elements(yticks) gt 0 then begin
	    contour,x,xv,yv,/fill,position=position,levels=levs,c_colors=cols,$
		xst=xst,yst=yst,/follow,xticklen=0,yticklen=0,$
		xtitle=xtitle,ytitle=ytitle,charsize=chs,$
		ytickv=ytickv,yticks=yticks,ytickname=ytickname ,$
		xticks=1,xtickv=[0,1],yty=yty,xty=xty
	endif else begin
		if left eq 1 then begin
		    contour,x,xv,yv,/fill,position=position,levels=levs,c_colors=cols,$
			xst=xst,yst=yst,/follow,xticklen=0,yticklen=0,$
			xtitle=xtitle,ytitle=ytitle,charsize=chs,$
			xticks=1,xtickv=[0,1],yty=yty,xty=xty
		endif else begin
		    contour,x,xv,yv,/fill,position=position,levels=levs,c_colors=cols,$
			xst=xst,yst=yst,/follow,xticklen=0,yticklen=0,$
			xtitle=xtitle,ytitle=ytitle,charsize=chs,$
			yticks=1,ytickv=[0,1],yty=yty,xty=xty
		endelse
	endelse
	if not keyword_set(nolines) then contour,x,xv,yv,levels=levs,c_labels=replicate(0,nc),/over
	!p.ticklen=ptl
	plots,xbox,ybox
end
