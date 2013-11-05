;===============================================================================
;+
; TITLE.PRO
;
; This routine puts a title on a page of plots.
;
; PARAMETERS
;	TITLE	The title to plot
;	SUBTITLE The subtitle.
;	FIGURE	String containing figure number
;
; KEYWORDS
;	TSIZE	Sets the relative text size to use.
;	MTSIZE  Sets the relative text size to use for main title only
;	STSIZE  Sets the relative text size to use for main title only
;	FTSIZE  Sets the relative text size to use for figure title only
;	DTSIZE	Set size for date
;	NONAME	Set to not plot my name in BL corner
;	YSUB	Specify Y position of subtitle
;	LOGO	Plot CLRC logo
;
; Subtitle may be omitted.
;
; R.Siddans 21/3/95
; $Id: title.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
pro title,ti,st,ft,tsize=tsize,align=align,noname=noname,ysub=ysub,mtsize=mtsize,$
	stsize=stsize,ftsize=ftsize,logo=logo,dtsize=dtsize

	if n_elements(dtsize) eq 0 then dtsize=0.5
	rel_tht=float(!d.y_ch_size)/float(!d.y_size)
	if not keyword_set(tsize) then tsize=1.0
	if not keyword_set(mtsize) then mtsize=1.5
	if not keyword_set(stsize) then stsize=1.0
	if not keyword_set(ftsize) then ftsize=1.3
	if not keyword_set(align) then align=0.5
	if not keyword_set(ysub) then begin
		if n_elements(ft) eq 0 then ysub=0. else $
		ysub=rel_tht*tsize*ftsize+rel_tht*tsize*stsize*1.5
	endif

	if n_elements(ti) gt 0 then begin
		title_height=rel_tht*tsize*mtsize
		xyouts,0.5,1.0-title_height,ti,align=align,/normal,$
			size=tsize*mtsize
	endif
	if n_elements(st) gt 0 then begin
		xyouts,0.5,ysub,st,align=align,/normal,size=tsize*stsize
	endif
	if n_elements(ft) gt 0 then begin
		xyouts,0.5,0.,ft,align=align,/normal,size=tsize*ftsize
	endif
;
; print date and my name...
;
	if not keyword_set(noname) then begin
		sd=systime()
		day=get_word(0,sd,/mult)
		month=get_word(1,sd,/mult)
		date=fix(get_word(2,sd,/mult))
		sdate=string(date,format='(I0)')
		time=get_word(3,sd,/mult)
		year=fix(get_word(4,sd,/mult))
		year=year mod 100
		syear=string(year,format='(I0)')
		hours=get_word(0,time,delim=':',/mult)
		mins=get_word(1,time,delim=':',/mult)
		month=strupcase(month)
		mstrs=['JAN','FEB','MAR','APR','MAY','JUN','JUL',$
			'AUG','SEP','OCT','NOV','DEC']
		month=where(mstrs eq month)+1
		month=month(0)
		smonth=string(month,format='(I0)')
		if month lt 10 then smonth='0'+smonth
		if year lt 10 then syear='0'+syear
		if date lt 10 then sdate='0'+sdate
		
;		xyouts,0.,0.,charsize=dtsize,'R.S. '+hours+':'+mins+' '+$
		xyouts,0.,0.,charsize=dtsize,rsg_user_name(/init)+' '+hours+':'+mins+' '+$
			sdate+'/'+smonth+'/'+syear+'.',/normal
	endif
	if keyword_set(logo) then logo
end
