;===============================================================================
;+
; QUICK_CIM
;
; Plots a 2-d array as an image with colour bar in similar way to QUICK_CC/CC_BOX.
; Note this can accept x/y range and title - if these are provided then they
; apply to the box drawn under the plotted image - they can be useful to indicate axes
; but are only sensible if the image x/y coordinate is linear.
;
; PARAMETERS
;	IM	2-d array to be plotted
;
; KEYWORDS
;	LEVELS	Specify levels used to span available colour table
;	CPOS	Specify position of map
;	BPOS	Specify postion of colour bar
;	DTITLE	Colour bar axis title
;	TITLE	Image title
;	LCB	Left colour bar
;	BCB	Bottom colour bar
;	BRD	As QUICK_CC
;	CRD	As QUICK_CC
;	MIN_VALUE Set minimum data value
;	COL	Return the colours
;	XTITLE	X axis title
;	YTITLE	Y axis title
;	XRANGE	X range 
;	YRANGE	Y range 
;	CBLEN	Length of colour bar relative to plot (defaul=1)
;	B32	Set if the data is actually a true colour image encoded to 32 bits
;		(as in mk_32bit_image)
;	NOBORDER	Don't draw border around image
;	KML	Run QUICK_CIM_KML - set this to the name of the
;		required output file
;
; R.S. 19/02/01
; $Id: quick_cim.pro 823 2011-04-08 15:34:30Z rsiddans $
;-
;===============================================================================
pro quick_cim,im,levels=lev,bpos=bpos,cpos=cpos,dtitle=dtitle,_EXTRA=extra,chars=chars,$
	position=position,bcb=bcb,lcb=lcb,fac=fac,num=num,hires=hires,brd=brd,crd=crd,$
	min_value=min_value,cbs=cbs,title=title,nobar=nobar,colors=col,xrange=xrange,yrange=yrange,$
	xtitle=xtitle,ytitle=ytitle,$
	cbticks=cbticks,cbtickv=cbtickv,cbtickname=cbtickname,cblen=cblen,b32=b32,$
	xstyle=xstyle,ystle=ystyle,noborder=noborder,kml=kml

	if keyword_set(kml) then begin
		quick_cim_kml,kml,im,levels=lev,dtitle=dtitle,_EXTRA=extra,$
			min_v=min_value,title=title,colors=col,$
			b32=b32
		return
	endif

	sz=size(im)
	if sz(0) ne 2 then message,'IM must be 2d array'
	if n_elements(lev) eq 0 then begin
		lev=clevels(im,min_v=min_value)
	endif
	nl=n_elements(lev)
	if n_elements(col) eq 0 then col=findgen(nl)/(nl-1)*(!d.table_size-4)+2
;
; generate actual image to be sent to plot device - 
; colour indices derived based on defined levels / colours to
; match colour bar
;
	if not keyword_set(b32) then begin
		im2=bytarr(sz(1),sz(2))+col(0)
		for i=0,nl-1 do begin
			wh=where(im gt lev(i),nw)
			if nw gt 0 then im2(wh)=col(i)
		endfor
		if n_elements(min_value) gt 0 then begin
			wh=where(im lt min_value,nw)
			if nw gt 0 then im2(wh)=!p.background
		endif
		tru=0
	endif else begin
;
; decode 32 bit image into r,g,b
;
		lim=long(im)
		imb=lim/256l/256l
		lim=lim-imb*(256l*256l)
		img=lim/256l
		imr=lim-img*256l
		im2=[[[imr]],[[img]],[[imb]]]
		tru=3
	endelse
;
; make plot
;
	p1=[0.,0.02,1,0.98]
	if n_elements(position) eq 0 then position=ypos()
;
; determine colour bar/plot positions
;
	pp=position
	co0=convert_coord(pp(0:1),/normal,/to_device)
	co1=convert_coord(pp(2:3),/normal,/to_device)
	if keyword_set(bcb) then begin
		dx=0.
		dy=1.
	endif else if keyword_set(lcb) then begin
		dx=1.
		dy=0.
	endif else begin
		dx=co1(0)-co0(0)
		dy=co1(1)-co0(1)
	endelse
	if keyword_set(nobar) then begin
		brd=0.
		crd=0.
	endif
	get_cc_position,pp,brd=brd,crd=crd,dx gt dy,cblen=cblen,bpos,cpos
	if n_elements(cpos) eq 0 then begin
		p2c=[0.1,0.02,0.2,0.97]
		cpos=ypos(p1,p2c)
	endif
	if !p.multi(0) eq 0 and not keyword_set(nobar) then begin
		plot,[0,1],/nodata,xst=5,yst=5 
		!p.multi(0)=!p.multi(0)+1
		noe=1 
	endif else noe=0
	if not keyword_set(nobar) and not keyword_set(b32) then cbar,lev,col,position=bpos,/nomul,title=dtitle,chars=chars,step=cbs,$
		ticks=cbticks,tickv=cbtickv,tickname=cbtickname
	if n_elements(xstyle) eq 0 then if keyword_set(xtitle) or n_elements(xrange) gt 1 then xstyle=1 else xstyle=5
	if n_elements(ystyle) eq 0 then if keyword_set(ytitle) or n_elements(yrange) gt 1 then ystyle=1 else ystyle=5
	plot,[0,sz(1)],[0,sz(2)],xst=xstyle,yst=ystyle,/nodata,position=cpos,title=title,chars=chars,xrange=xrange,yrange=yrange,noe=noe,$
		xtitle=xtitle,ytitle=ytitle
	tv_pos,im2,tru=tru
	if not keyword_set(noborder) then plots,!x.crange([0,0,1,1,0]),!y.crange([0,1,1,0,0])
end
