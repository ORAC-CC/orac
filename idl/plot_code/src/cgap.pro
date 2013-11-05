;===============================================================================
;+
; CGAP
;
; This routine blanks out areas of contour plots for which there are large
; gaps in the original data array. The relevent contour plot must be in the
; current plot window.
;
; This command will also remove the current axis so keyword parameters may
; need to be supplied to correctly re-draw the axes
;
; PARAMETERS
;	X	X values for data 
;	Y 	Y values 
;
; KEYWORDS
;	XGAP	Set to threshold gap in X axis data to blank out.
;		Automatic threshold is set by histogramming X and Y data.
;	YGAP	Set to threshold gap in Y axis data to blank out.
;	NSIG	Set to number of sigma outside of mean data
;		separation to blank out
;	XGO	Only do gaps in X
;	YGO	Only do gaps in Y
;       XSTYLE	As plot keyword
;       XTYPE	As plot keyword
;       YSTYLE	As plot keyword
;       YTYPE	As plot keyword
;       XTICKS	As plot keyword
;       YTICKS	As plot keyword
;       POSITION	As plot keyword
;
; R.S. 29/7/96
; $Id: cgap.pro 1212 2011-09-28 08:40:29Z rsiddans $
;-
;===============================================================================
pro cgap,x,y,xgap=xgap,ygap=ygap,nsig=nsig,$
	xstyle=xstyle,xtype=xtype,$
	ystyle=ystyle,ytype=ytype,$
	xticks=xticks,$
	yticks=yticks,$
	position=position,xgo=xgo,ygo=ygo

	if not keyword_set(nsig) then nsig=3.
;
; sort out defaults for axis keywords
;
	if not keyword_set(xstyle) then xstyle=!x.style
	if not keyword_set(xtype) then xtype=!x.type
	if not keyword_set(xticks) then xticks=!x.ticks
	if not keyword_set(ystyle) then ystyle=!y.style
	if not keyword_set(ytype) then ytype=!y.type
	if not keyword_set(yticks) then yticks=!y.ticks
	if not keyword_set(position) then position=!p.position
;
; determine gaps...
;
	nx=n_elements(x)
	ny=n_elements(y)
	dx=abs(x(1:nx-1)-x(0:nx-2))
	dy=abs(y(1:ny-1)-y(0:ny-2))
	if not keyword_set(xgap) then begin
;
; do 3 sigma test on dx to determine anomalously large gaps
;
		test=1
		dt=dx
		while test ne 0 and n_elements(dt) gt 2 do begin
			std=stderr(dt,mean=mean,sdev=sdev)
			wh=where(abs(dt-mean(0)) le sdev(0)*nsig)
			if n_elements(wh) ne n_elements(dt) and wh(0) ne -1 then begin
				dt=dt(wh)
			endif else test=0
		endwhile
		xgap=max(dt)
	endif
	if not keyword_set(ygap) then begin
		test=1
		dt=dy
		while test ne 0 and n_elements(dt) gt 2 do begin
			std=stderr(dt,mean=mean,sdev=sdev)
			wh=where(abs(dt-mean(0)) le sdev(0)*nsig)
			if n_elements(wh) ne n_elements(dt) and wh(0) ne -1 then begin
				dt=dt(wh)
			endif else test=0
		endwhile
		ygap=max(dt)
	endif
;
; now find gaps in X and blank them out...
;
if not keyword_set(ygo) then begin
	wh=where(dx gt xgap)
	if wh(0) ne -1 then begin
		y0=!y.crange(0)
		y1=!y.crange(1)
		for i=0,n_elements(wh)-1 do begin
			x0=x(wh(i))
			x1=x(wh(i)+1)
			polyfill,[x0,x0,x1,x1,x0],[y0,y1,y1,y0,y0],$
				color=!p.background,noclip=0
		endfor
	endif
endif
;
; now find gaps in Y and blank them out...
;
if not keyword_set(xgo) then begin
        wh=where(dy gt ygap)    
        if wh(0) ne -1 then begin
                x0=!x.crange(0)
                x1=!x.crange(1)
                for i=0,n_elements(wh)-1 do begin
                        y0=y(wh(i))
                        y1=y(wh(i)+1)
                        polyfill,[x0,x0,x1,x1,x0],[y0,y1,y1,y0,y0],$
                                color=!p.background,noclip=0
                endfor
        endif
endif
;
; redraw axes
;
	if xstyle mod 2 eq 0 then xst=xstyle+1 else xst=xstyle
	if ystyle mod 2 eq 0 then yst=ystyle+1 else yst=ystyle

	notick=replicate(' ',30)
	plot,[0,1],xrange=!x.crange,yrange=!y.crange,$
		xstyle=xst,ystyle=yst,xtitle=' ',ytitle=' ',$
		ytype=ytype,xtype=xtype,/nodata,/noe,$
		position=position,xtickname=notick,ytickname=notick
end
