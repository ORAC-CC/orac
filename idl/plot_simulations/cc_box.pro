;===============================================================================
;+
; CC_BOX
;
; Use quick_cc to plot array as grid of coloured boxes...
;
; Works exactly like quick_cc if X,Y have same number of elements as D(*,0)
; and D(0,*) respectively, but if they have 1 more element, then these used
; as box edges and blocked array will be plotted - this can be done in X,Y
; or both dimensions
;
; PARAMETERS
;	D1	Data
;	X2	X-axis
;	Y2	Y-axis
;
; KEYWORDS
;	CPOS	Return position of main plot
;	PV	Print data-value in each box
;	PCHARSIZE Size of characters used with PV
;	FORM	Format used with PV
;	OUT	Output the plotted data to a text file
;	XLAB	Label to appear on x axis at centre of each box
;	YLAB	Label to appear on y axis at centre of each box
;	LABSIZE	basic charsize for labels
;	XLBSIZE	Size for x-axis labels
;	YLBSIZE	Size for y-axis labels
;	XLBLINE	Line-style used with x-axis label
;	YLBLINE	Line-style used with y-axis label
;	SPV	Value printed when PV set (default = data value)
;       NLG     Do not add <,> signs where colour scale exceeded
;               1 = fill regions where data above levels, not below
;               2 = fill regions where data below levels, not above
;               3 = fill regions where data above and below levels
;               4 = only fill where data between levels
;       PLG     After applying NLG, if a <,> sign is to be printed,
;               instead print the value of the data.
;       SFPV    Set number of significant figures for default PV strings
;	XTITLE	As usual
;	YTITLE	As usual
;	XTIPOS	Normalised (relative to plot frame) position of xtitle away from x-axis
;	YTIPOS	Normalised (relative to plot frame) position of ytitle away from y-axis
;		NB BRD,CRD used as normal with QUICK_CC to move colour bar relative to plot
;	ACHARSIZE Set size of axis text relative to CHARSIZE
;	_EXTRA	Sent to QUICK_CC
;
; R.S. 05/09/01
; $Id: cc_box.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
pro cc_box_labs,_extra=extra,$
	xlabname=xlabname,xlabpos=xlabpos,xlabsize=xlabsize,$
	ylabname=ylabname,ylabpos=ylabpos,ylabsize=ylabsize,$
	xlabppos=xlabppos,ylabppos=ylabppos,$
	xlabline=xlabline,ylabline=ylabline
	for i=0l,n_elements(xlabpos)-1 do begin
		xyouts,xlabpos(i),!y.crange(0),' '+xlabname(i),orient=270, alig=0.,/data,$
			charsize=xlabsize
		if xlabline(i) ge 0 then plots,xlabppos([i,i]),!y.crange,line=xlabline(i)
	endfor
	for i=0l,n_elements(ylabpos)-1 do begin
		xyouts,!x.crange(0),ylabpos(i),ylabname(i)+' ',orient=0, alig=1.,/data,$
			charsize=ylabsize
		if ylabline(i) ge 0 then plots,!x.crange,ylabppos([i,i]),line=ylabline(i)
	endfor
end
pro cc_box,d1,x2,y2,_EXTRA=extra,cpos=cpos,$
	pv=pv,plg=plg,pcharsize=pvchs,form=form,out=out,$
	xlab=xlab,ylab=ylab,$
	labsize=labsize,xlbsize=xlabsize,ylbsize=ylabsize,$
	xlbline=xlbline,ylbline=ylbline,spv=spv,nlg=nlg,sfpv=sfpv,flg=flg,$
	xtitle=xtitle,ytitle=ytitle,xtipos=xtipos,ytipos=ytipos,charsize=chs,$
	acharsize=acharsize

	if n_elements(y2) eq 0 then y2=findgen(n_elements(d1(0,*))+1)
	if n_elements(x2) eq 0 then x2=findgen(n_elements(d1(*,0))+1)
	if n_elements(pv) eq 0 then pv=0
	if n_elements(sfpv) eq 0 then sfpv=2
	if n_elements(form) eq 0 then form='(E7.1)'
	if n_elements(pvchs) eq 0 then pvchs=!p.charsize/2.
	if n_elements(chs) eq 0 then chs=def_chs()
	if n_elements(acharsize) eq 0 then begin
		if !p.multi(2) gt 2 or !p.multi(1) gt 2 then acharsize=0.6 $
		else acharsize=1.
	endif
	d=d1
	nxd=n_elements(d(*,0))
	nyd=n_elements(d(0,*))
	if n_elements(x2) eq nxd then x1=interpol(x2,findgen(nxd),findgen(nxd+1)-0.5) else x1=float(x2)
	if n_elements(y2) eq nyd then y1=interpol(y2,findgen(nyd),findgen(nyd+1)-0.5) else y1=float(y2)
	nx=n_elements(x1)
	ny=n_elements(y1)
	if keyword_set(spv) then pv=1
	if keyword_set(pv) and n_elements(spv) eq 0 then spv=sig_figs(d,3) 
	if ny ne nyd and ny ne nyd+1 then message,'Y-dim error'
	if nx ne nxd and nx ne nxd+1 then message,'X-dim error'
	if nx eq nxd then x=x1 else begin
		xd=(x1(1)-x1(0))/1000d0
		x=[x1(0:nx-2),x1(1:nx-2)-xd,x1(nx-1)]
		d=[d,d]
		so=sort(x)
		d=d(so,*)
		x=x(so)
	endelse
	if ny eq nyd then y=y1 else begin
		yd=(y1(1)-y1(0))/100
		y=[y1(0:ny-2),y1(1:ny-2)-yd,y1(ny-1)]
		d=[[d],[d]]
		so=sort(y)
		d=d(*,so)
		y=y(so)
	endelse
	ccb=1
	exccb={pv:pv,$
		plg:kywd_def(plg),$
		sfpv:kywd_def(sfpv),$
		charsize:pvchs,form:form,spv:kywd_def(spv),nlg:kywd_def(nlg),flg:kywd_def(flg)}
	if n_tags(extra) eq 0 then extra={dummy:0l}
	if n_elements(labsize) eq 0 then labsize=!p.charsize
	if keyword_set(xlab) then begin
		if n_elements(xlab) ne nxd then message,'Wrong number of XLAB'
		isel=lindgen(nxd)*2
		xm=(x(isel)+x(isel+1))/2
		xt=x([isel,nxd*2-1])
		if n_elements(xlabsize) eq 0 then xlabsize=labsize
		if n_elements(xlbline) eq 0 then xlabline=0 else xlabline=xlbline
		if n_elements(xlabline) eq 1 then xlabline=replicate(xlabline,nxd)
		extra=create_struct(extra,$
			'xticks',1,$
			'xtickv',range(x),$
			'xtickname',[' ',' '],$
			'xlabname',xlab,$
			'xlabpos',xm,$
			'xlabppos',xt,$
			'xlabsize',xlabsize,$
			'xlabline',xlabline)
	endif
	if keyword_set(ylab) then begin
		if n_elements(ylab) ne nyd then message,'Wrong number of YLAB'
		isel=lindgen(nyd)*2
		ym=(y(isel)+y(isel+1))/2
		yt=y([isel,nyd*2-1])
		if n_elements(ylabsize) eq 0 then ylabsize=labsize
		if n_elements(ylbline) eq 0 then ylabline=0 else ylabline=ylbline
		if n_elements(ylabline) eq 1 then ylabline=replicate(ylabline,nyd)
		extra=create_struct(extra,$
			'yticks',1,$
			'ytickv',range(y),$
			'ytickname',[' ',' '],$
			'ylabname',ylab,$
			'ylabpos',ym,$
			'ylabppos',yt,$
			'ylabsize',ylabsize,$
			'ylabline',ylabline)
	endif
	if keyword_set(xlab) or keyword_set(ylab) then begin
		extra=create_struct(extra,'extra','cc_box_labs','after',1)
	endif
	quick_cc,d,x,y,_EXTRA=extra,/noc,cpos=cpos,ccb=ccb,/pv,exccb=exccb,chars=chs
;
; put axis title in controlled position
;
	if n_elements(xtipos) eq 0 then xtipos=-0.11
	if n_elements(ytipos) eq 0 then ytipos=-0.11
	if keyword_set(xtitle) then xyouts,interpol(!x.crange,[0.,1],0.5),interpol(!y.crange,[0.,1],xtipos),xtitle,chars=chs*acharsize,alig=0.5,/data
	if keyword_set(ytitle) then xyouts,interpol(!x.crange,[0.,1],ytipos),interpol(!y.crange,[0.,1],0.5),ytitle,chars=chs*acharsize,alig=0.5,/data,orient=270
	if keyword_set(out) then begin
		sz=size(out)
		if sz(sz(0)+1) eq 7 then ofi=out else ofi='cc_box_out.txt'
		get_lun,lun
		openw,lun,ofi
		printf,lun,';X'
		printf,lun,size(x1)
		printf,lun,x1
		printf,lun,';Y'
		printf,lun,size(y1)
		printf,lun,y1
		printf,lun,';D'
		printf,lun,size(d1)
		printf,lun,d1
		close_file,lun
	endif
end
