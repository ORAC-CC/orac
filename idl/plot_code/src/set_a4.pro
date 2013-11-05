;===============================================================================
;+
; SET_A4.PRO
;
; This procedure sets up A4 plotting on either X windows or PS.
; The x-window shows the plot region used by the PS output (i.e.
; it does not include the plot margins)
;
; PARAMETERS
;	NONE
;
; KEYWORDS
;	COLS	Set to number of columns of plots
;	ROWS	Set to number of rows of plots
;	NUMBER  Set to number of plots on page (COLS and ROWS are
;		calculated).
;	LANDSCAPE Set to get landscape plot
;	PORTRAIT  Set to get portrait plot (the default)
;	XSIZE	Set to scale the normal size of the X window.
;	YMARGIN Set y-margin in CM (as seen in portrait mode)
;		can be 2 element vector ([top,bottom]) or 
;		1 value specifying both margins
;	XMARGIN Set x-margin in CM (as seen in portrait mode)
;		can be 2 element vector ([left,right]) or 
;		1 value specifying both margins
;	BW	Don't set color table with RS's SET_COLOR
;	EPSF	Don't set landscape for PS (allow incorporation
;		into latex docs)
;	_EXTRA	Sent to SET_COLOR
;	HIDE	If X-display then send window to background as soon as created
;
; R.Siddans 9/2/95
; $Id$
;-
;===============================================================================
pro set_a4,cols=cols,rows=rows,landscape=landscape,portrait=portrait,$
	xsize=xsize,number=num,ymargin=ym,xmargin=xm,bw=nocol,epsf=epsf,$
	title=title,wn=wn,_EXTRA=extra,hide=hide
;
; call dummy run if no window set
;
	if !d.name eq 'X' then begin
		if n_elements(wn) gt 0 then win1=0 > wn else win1=0 > !d.window
		if !d.window eq -1 then window,win1
	endif
;
; set up defaults
;
	if keyword_set(epsf) then begin
		xm=0.
		ym=0.
	endif
	if not keyword_set(rows) then rows=1
	if not keyword_set(cols) then cols=1
	if not keyword_set(landscape) then portrait=1 else portrait=0
	if not keyword_set(xsize) then begin
		disp=get_word(0,strupcase(getenv('DISPLAY')),del='.')
		if disp eq 'QUILTY' then xsize=0.6 else xsize=1.0
	endif
	s4scale=strtrim(getenv('SETA4_SCALE'),2)
	if strlen(s4scale) gt 0 then xsize=float(s4scale)
	if not keyword_set(tsize) then tsize=1.0
	if not keyword_set(ym) then ym=3.
	if not keyword_set(xm) then xm=2.
	if keyword_set(num) then set_rowscols,num,rows,cols
	if n_elements(ym) eq 1 then ym=[ym,ym]
	if n_elements(xm) eq 1 then xm=[xm,xm]
	
	xsize=float(xsize)
;
; set up X
;
	pd=!d.name
	if pd eq 'X' or pd eq 'Z' then begin
		set_plot,'ps'
		set_a4,xm=xm,ym=ym,land=landscape,portrait=portrait,bw=nocol
		xcs=float(!d.x_ch_size)/!d.x_size
		ycs=float(!d.y_ch_size)/!d.y_size
		set_plot,pd
		xs=float(!d.x_ch_size)/xcs
		ys=float(!d.y_ch_size)/ycs
		if !d.name eq 'Z' then begin
			device,set_resolution=[long(xs)*xsize,long(ys)*xsize]
		endif else begin
;			win=!d.window
;			if win eq -1 then win=0
;			window,win,ysize=long(ys)*xsize,xsize=long(xs)*xsize
			if keyword_set(title) then $
				title1='IDL '+strtrim(string(win1),2)+': '+title
			xysize=long(ys)*xsize
			xxsize=long(xs)*xsize
			device,get_screen_size=ssz
			scale=min([1.,float(ssz(0))/xxsize,float(ssz(1))/xysize])
			xysize=xysize*scale
			xxsize=xxsize*scale
			window,win1,ysize=xysize,xsize=xxsize,title=title1
			if keyword_set(hide) then wshow,win1,0
		endelse
	endif
;
; set up postscript...
;
	if !d.name eq 'PS' then begin
		xs0=21.1
		ys0=1.41*xs0
		xs=xs0-xm(0)-xm(1)
		ys=ys0-ym(0)-ym(1)
		
		if portrait then begin
			device,/portrait,font_size=10
			device,yoffset=ym(0),xoffset=xm(0)
			device,ysize=ys,xsize=xs
		endif else begin
			if not keyword_set(epsf) then $
				device,/landscape,font_size=10 $
			else device,/portrait,font_size=10
			device,yoffset=ys0-ym(1),xoffset=xm(0)
			device,xsize=ys,ysize=xs
		endelse
	endif 
;
; set up multiple plots
;
	!p.multi=[0,cols,rows,0,0]
;
; set color table
;
	if not keyword_set(nocol) then set_color,/ps,_EXTRA=extra
;
; set AL environment variable
;
	set_al
end

