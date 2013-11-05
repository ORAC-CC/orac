;===============================================================================
;+
; QUICK_CC
;
; This program produces a quick colour contour plot.
;
; PARAMETERS
;	D1	Array to be contoured
;	X	X coordinates
;	Y	Y coordinates
;
; KEYWORDS
;	LCB	Force colour bar to left of plot
;	BCB	Force colour bar below plot
;	XTITLE	X axis title
;	YTITLE	Y axis title
;	DTITLE	Title of data being plotted (for color bar)
;	LEVELS	Contour levels
;	POSITION Position of window containing contour and color bar
;	RANGE	Approx range of levels
;	XSTYLE	As plot keyword
;	YSTYLE	As plot keyword
;	XTYPE	As plot keyword
;	YTYPE	As plot keyword
;	XRANGE	As plot keyword
;	YRANGE	As plot keyword
;	NOE	Don't erase previous contour
;	NOBAR	Don't draw color bar
;	CHARSIZE	As plot keyword
;	MAX_VALUE As contour keyword
;	CELL_FILL 	As contour keyword
;	OVER	Set to draw line contour over colour contour.
;	LAND	Set A4 to landscape. (land=n for n plots)
;	PORTRAIT Set A4 to portrait. (portrait=n for n plots)
;	TITLE	See TITLE proceedure
;	SUBTITLE See TITLE proceedure
;	FIGURE 	See TITLE proceedure
;	CPOS	Returns position of contour plot
;	BPOS	Returns position of color bar
;	NOCGAP	Set to not run cgap.pro (by default set)
;	EXTRA	Name of routine to set up plot coords. Routine must
;		accept POSITION and NOE keywords. Any unrecognised keywords sent
;		to this routine. KMAP_SET can be used here to get contour
;		over a map.
;	NOTITLE Don't do a main title at all.
;	CBSTEP	Do step type colour bar
;	COLORS	Return colour levels.
;	AFTER	Set to run EXTRA only after plotting contour
;	CLOSED  See contour keyword
;	LINE	Just do line contour
;	C_CHARSIZE size of over-plot contour labels
;	CRD	Adjust left side of contour position to avoid overplotting axis annotation
;	BRD	Adjust right side of bar position to avoid overplotting axis annotation
;	CEXT	Structure of extra keywords for contour.
;	RCT	Reverse colour table
;	NCONT	Do not draw contour (stop after _EXTRA)
;	CBEXTRA	Define operation after color bar drawn
;		{CMD:'name of procedure to execute',$
;		  KW:_EXTRA structure of keywords to be passed}
;       MIN_V	Contour keyword
;	XTICKS	As plot keyword
;       XTICKLEN	As plot keyword
;       XTICKNAME	As plot keyword
;       XTICKFORMAT	As plot keyword
;       XTICKV	As plot keyword
;       YTICKS	As plot keyword
;       YTICKNAME	As plot keyword
;       YTICKFORMAT	As plot keyword
;       YTICKV	As plot keyword
;	XGAP	See CGAP.pro
;	YGAP	See CGAP.pro
;	XGO	See CGO.pro
;	YGO	See CGAP.pro
;	NSIG	See CGAP.pro
;	CCB	Use CC_BOX instead of contour
;	EXCCB	_EXTRA sent to CC_BOX
;	DLOG	Take LOG10 of data before plotting
;	IMRES	Image resolution for current device
;	_EXTRA	Sent to EXTRA command
;	IMAGE	Form contour as an image first then display
;		(can save time displaying in final PDF/PS file if
;		large data set)
;	OBX	Only plot box around plot (useful with IMAGE if KMAP_SET
;		used for plotting as otherwise axes are not handled correctly)
;
; R.S. 16/11/95
;-
;===============================================================================
pro quick_cc,d1,x,y,xtitle=xti,ytitle=yti,dtitle=dti,position=position,$
	levels=levels,xstyle=xst,ystyle=yst,xtype=xty,ytype=yty,over=over,$
	land=land,portrait=portrait,title=title,subtitle=subtitle,$
	figure=figure,range=range,charsize=charsize,$
	c_charsize=c_charsize,cpos=cpos,bpos=bpos,$
	xrange=xrange,yrange=yrange,noe=noe,nobar=nobar,nocgap=nocgap,$
	max_value=max_value,cell_fill=cell_fill,extra=extra,_EXTRA=keyext,$
	notitle=notitle,cbstep=cbstep,colors=colors,after=after,closed=closed,line=jline,$
	lcb=lcb,bcb=bcb,min_v=min_v,xgap=xgap,ygap=ygap,$
	crd=crd,brd=brd,cext=cext,$
	xticks=xticks,xticklen=xticklen,xtickname=xtickname,xtickv=xtickv,$
	yticks=yticks,ytickname=ytickname,ytickv=ytickv,rct=rct,ncont=ncont,$
	xgo=xgo,ygo=ygo,nsig=nsig,ccb=ccb,exccb=exccb,dlog=dlog,$
	xtickformat=xtickformat,ytickformat=ytickformat,$
	cbextra=cbextra,image=image,imres=imres,obx=obx


	if n_elements(xst) eq 0 then xst=1
	if n_elements(yst) eq 0 then yst=1
	if keyword_set(image) then begin
;
; get the image by plotting to the z-buffer
;
		pm=!p.multi
		pd=!d.name
		set_plot,'z'
		!p.multi=0
		if n_elements(imres) eq 0 then imres=[n_elements(d1(0,*)),n_elements(d1(*,0))]*4
		device,set_resolution=imres
		device,z_buffering=0
		if keyword_set(after) and keyword_set(extra) then extra1=0 else extra1=kywd_def(extra)
		quick_cc,d1,x,y,position=[0,0,1,1],$
        		levels=levels,xstyle=(xst mod 2)+4,ystyle=(yst mod 2)+4,$
			xtype=xty,ytype=yty,over=over,$
        		land=land,portrait=portrait,$
        		figure=figure,range=range,charsize=charsize,$
			/notitle,/nobar,$
        		c_charsize=c_charsize,$
        		xrange=xrange,yrange=yrange,$
			nocgap=nocgap,$
        		max_value=max_value,cell_fill=cell_fill,$
			extra=extra1,_EXTRA=keyext,$
        		cbstep=cbstep,colors=colors,after=after,$
			closed=closed,line=jline,$
        		lcb=lcb,bcb=bcb,min_v=min_v,xgap=xgap,ygap=ygap,$
        		crd=crd,brd=brd,cext=cext,$
        		xticks=xticks,xticklen=xticklen,xtickname=xtickname,xtickv=xtickv,$
        		yticks=yticks,ytickname=ytickname,ytickv=ytickv,rct=rct,ncont=ncont,$
        		xgo=xgo,ygo=ygo,nsig=nsig,ccb=ccb,exccb=exccb,dlog=dlog,$
        		xtickformat=xtickformat,ytickformat=ytickformat,$
        		cbextra=cbextra,/ccimage
		im_cxr=!x.crange
		im_cyr=!y.crange
		if keyword_set(yty) then im_cyr=10d0^im_cyr
		if keyword_set(xty) then im_cxr=10d0^im_cxr
		im=tvrd()
		tvlct,r,g,b,/get
		device,/close
		set_plot,pd
		if strupcase(pd) eq 'PS' then begin
			w255=where(im eq 255,n255)
			w0=where(im eq 0,n0)
			if n255 gt 0 then im(w255)=0
			if n0 gt 0 then im(w0)=255
		endif
		!p.multi=pm
	endif
	if n_elements(nocgap) eq 0 then nocgap=1
	if keyword_set(jline) then nobar=1
	if not keyword_set(title) then title=' '
	if not keyword_set(subtitle) then subtitle=' '
	if not keyword_set(figure) then figure=' '
	if not keyword_set(charsize) then begin
		charsize=1.
		if !p.multi(2) ne 0 then charsize=charsize/!p.multi(2)
	endif
	if keyword_set(extra) and not keyword_set(after) then noe=1

	if keyword_set(land) then set_a4,/land,num=land
	if keyword_set(portrait) then set_a4,num=portrait
	if keyword_set(land) or keyword_set(portrait) then set_color,/rs,/ps
	if not keyword_set(yrange) then yrange=[-1,-1]
	if not keyword_set(xrange) then xrange=[-1,-1]

	if not keyword_set(position) then position=ypos()
	d=d1
	if keyword_set(dlog) then d=alog10(d)
;
; sort out where to put colour bar (side/bottom) depending on relative
; x/y dimensions
;
	if not keyword_set(nobar) then begin
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
		if n_elements(brd) eq 0 then brd=0.06
		if n_elements(crd) eq 0 then crd=0.17
		if dx gt dy then begin
			if n_elements(brd) eq 2 then begin
				bpos=[pp(0)+(pp(2)-pp(0))*brd(0),pp(1),pp(0)+(pp(2)-pp(0))*brd(1),pp(3)]
			endif else begin
				bpos=[pp(0),pp(1),pp(0)+(pp(2)-pp(0))*brd,pp(3)]
			endelse
			cpos=[pp(0)+(pp(2)-pp(0))*crd,pp(1),pp(2),pp(3)]
		endif else begin
			if n_elements(brd) eq 2 then begin
				bpos=[pp(0),pp(1)+(pp(3)-pp(1))*brd(0),pp(2),pp(1)+(pp(3)-pp(1))*brd(1)]
			endif else begin
				bpos=[pp(0),pp(1),pp(2),pp(1)+(pp(3)-pp(1))*brd]
			endelse
			cpos=[pp(0),pp(1)+(pp(3)-pp(1))*crd,pp(2),pp(3)]
		endelse
	endif else cpos=position
;
; sort out defaults
;
	if keyword_set(max_value) and not keyword_set(range) then begin
		whr=where(d lt max_value)
		if whr(0) eq -1 then message,'No values below MAX_VALUE !'
		range=[min(d),max(d(whr))]
	endif
	if not keyword_set(max_value) then begin
		max_value=max(d)+abs(max(d))/10.
	endif
	if not keyword_set(levels) then levels=clevels(d,range=range,min_v=min_v)
;print,d
;print,clevels

	sz=size(d)
	if sz(0) ne 2 then begin
		wh=where(sz(1:sz(0)) ne 1)
		if n_elements(wh) eq 2 then begin
			d=reform(d,sz(wh(0)+1),sz(wh(1)+1))
			sz=size(d)
		endif else message,'DATA must have 2 dimensions.'
	endif
	if not keyword_set(x) then x=findgen(sz(1))
	if not keyword_set(y) then y=findgen(sz(2))
	nl=n_elements(levels)
;	colors=long(findgen(nl)*float(!d.n_colors-4)/float(nl-1))+2
; to work on 24 bit display, use !d.table_size not n_colors
	if n_elements(colors) ne nl then colors=long(findgen(nl)*float(!d.table_size-4)/float(nl-1))+2
	if not keyword_set(xty) then xty=0
	if not keyword_set(yty) then yty=0
	if not keyword_set(xti) then xti=' '
	if not keyword_set(yti) then yti=' '
;
; stop data at top/bottom level before contour...
;
	maxl=max(levels,min=minl)
	;wh=where(d gt maxl)
        ;if wh(0) ne -1 then d(wh)=maxl
	maxd=max(d)
if not keyword_set(ccb) then begin
	if maxd gt maxl then begin
		colors2=[colors,colors(nl-1)]
		levels2=[levels,maxd]
	endif else begin
		colors2=colors
		levels2=levels
	endelse
	if n_elements(min_v) ne 0 then begin
		if min_v lt levels2(0) then begin
			levels2=[(min_v+min(levels))/2.,levels]
			colors2=[!p.background,colors]
		endif
		;levels2=[(min_v+min(levels))/2.,levels]
	endif else begin
		wh=where(d lt minl)
		if wh(0) ne -1 then d(wh)=minl
	endelse
endif else begin
	colors2=colors
	levels2=levels
endelse
;
; reverse color table if required
;
	if keyword_set(rct) then colors2=reverse(colors2)
;
; contour data...
;
	if keyword_set(cell_fill) then begin
		fill=0
		follow=0
	endif else begin
		fill=1
		follow=1
	endelse
;
; do colour bar...
;
	if not keyword_set(nobar) then begin
		if keyword_set(rct) then colors3=reverse(colors) else colors3=colors
		plot,[0,1],/nodata,xst=5,yst=5
		cbar,levels,colors3,position=bpos,title=dti,/nomult,$
              		charsize=charsize,step=cbstep
		!p.multi(0)=!p.multi(0)+1
		if keyword_set(cbextra) then $
			call_procedure,cbextra.cmd,_EXTRA=cbextra.kw
	endif
	if keyword_set(image) then begin
		pm=!p.multi
		plot,im_cxr,im_cyr,/nodata,xstyle=(xst mod 2)+4,ystyle=(yst mod 2)+4,$
			xtype=xty,ytype=yty,$
			position=cpos,$
			charsize=charsize,yrange=yrange,xrange=xrange
		tv_pos,im
		!p.multi=pm
		if keyword_set(obx) then begin
			plots,!x.crange([0,0,1,1,0]),!y.crange([0,1,1,0,0])
			plot,[0,1],/nodata,xst=5,yst=5,title=title,charsize=charsize
		endif else begin
		    plot,im_cxr,im_cyr,/nodata,xstyle=xst,ystyle=yst,xtype=xty,ytype=yty,$
                        xtitle=xti,ytitle=yti,$
			position=cpos,title=title,$
			charsize=charsize,yrange=yrange,xrange=xrange,$
			xticks=xticks,xticklen=xticklen,xtickname=xtickname,xtickv=xtickv,$
			yticks=yticks,ytickname=ytickname,ytickv=ytickv,$
			xtickformat=xtickformat,ytickformat=ytickformat
		endelse
		if keyword_set(extra) and not keyword_set(after) then begin
			call_procedure,extra,position=cpos,$
                		_EXTRA=keyext,/noe
		endif
		if not keyword_set(noe) and not keyword_set(notitle) then title,' ',subtitle,figure
		return
	endif
	if keyword_set(extra) and not keyword_set(after) then call_procedure,extra,position=cpos,$
		_EXTRA=keyext,title=title,subtitle=subtitle,charsize=charsize
	if keyword_set(ncont) then return
	if keyword_set(jline) then begin
		contour,d,x,y,xstyle=xst,ystyle=yst,xtype=xty,ytype=yty,$
			xtitle=xti,ytitle=yti,$
			levels=levels2,position=cpos,follow=follow,title=title,$
			charsize=charsize,yrange=yrange,xrange=xrange,over=noe,$
			max_value=max_value,noe=noe,closed=closed,$
			c_labels=replicate(1,n_elements(levels2)),$
        		xticks=xticks,xticklen=xticklen,xtickname=xtickname,xtickv=xtickv,$
        		yticks=yticks,ytickname=ytickname,ytickv=ytickv,$
			xtickformat=xtickformat,ytickformat=ytickformat
	endif else begin
		contour,d,x,y,xstyle=xst,ystyle=yst,xtype=xty,ytype=yty,$
			xtitle=xti,ytitle=yti,fill=fill,c_colors=colors2,$
			levels=levels2,position=cpos,follow=follow,title=title,$
			charsize=charsize,yrange=yrange,xrange=xrange,over=noe,$
			max_value=max_value,cell_fill=cell_fill,noe=noe,closed=closed,$;,min_v=min_v,$
			_EXTRA=cext,$
        		xticks=xticks,xticklen=xticklen,xtickname=xtickname,xtickv=xtickv,$
        		yticks=yticks,ytickname=ytickname,ytickv=ytickv,nodata=ccb,$
			xtickformat=xtickformat,ytickformat=ytickformat
		if keyword_set(ccb) then begin
			cc_box1,d,x,y,levels2,colors2,max_value=max_value,$
				_EXTRA=exccb,min_value=min_v
		endif
		if not keyword_set(c_charsize) then c_charsize=charsize
		if keyword_set(over) then contour,d,x,y,xstyle=xst,ystyle=yst,$
			xtype=xty,ytype=yty,xtitle=xti,ytitle=yti,$
        	        levels=levels,/over,position=cpos,/follow,$
        	        charsize=charsize,yrange=yrange,xrange=xrange,$
			max_value=max_value,c_label=replicate(1,n_elements(levels)),closed=closed,$
			c_charsize=c_charsize,min_v=min_v,$
        		xticks=xticks,xticklen=xticklen,xtickname=xtickname,xtickv=xtickv,$
        		yticks=yticks,ytickname=ytickname,ytickv=ytickv,$
			xtickformat=xtickformat,ytickformat=ytickformat
		if keyword_set(extra) and not keyword_set(after) then call_procedure,extra,position=cpos,$
                	_EXTRA=keyext,/noe $
		else contour,d,x,y,xstyle=xst,ystyle=yst,xtype=xty,ytype=yty,$
                	xtitle=' ',ytitle=' ',$
                	position=cpos,title=' ',$
                	charsize=charsize,yrange=yrange,xrange=xrange,$
                	/noe,/nodata,closed=closed,min_v=min_v,_EXTRA=cext,$
        		xticks=xticks,xticklen=xticklen,xtickname=xtickname,xtickv=xtickv,$
        		yticks=yticks,ytickname=ytickname,ytickv=ytickv,$
			xtickformat=xtickformat,ytickformat=ytickformat
	endelse

;
; fill in large data gaps
;
	if not keyword_set(nocgap) then begin
		cgap,x,y,xstyle=xst,ystyle=yst,xtype=xty,ytype=yty,$
			position=cpos,xgap=xgap,ygap=ygap,xgo=xgo,ygo=ygo,$
			nsig=nsig
	endif
	if keyword_set(extra) and keyword_set(after) then begin
		call_procedure,extra,position=cpos,$
                _EXTRA=keyext,/noe
	endif
;
; do title
;
	if not keyword_set(noe) and not keyword_set(notitle) then title,' ',subtitle,figure
end
