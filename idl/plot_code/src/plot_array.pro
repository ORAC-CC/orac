;===============================================================================
;+
; PLOT_ARRAY
;
; This plots all rows of the given array
;
; PARAMETERS
;	D0	data array d(*,i) will be plotted for all
;	X01	Optional X values of the columns of D
;	I0	Labels for each row.
;
; KEYWORDS
;	_EXTRA	Sent to plot
;	NPP	Number of lines per plot.
;	IRANGE	Set to range of rows to plot
;	MEAN	Set to plot mean of data on each plot
;	STDERR	Set to plot mean of data, with standard error on each plot
;	SDEV	Set to plot mean of data, with standard deviation
;	NOLEG	Set to not draw legend
;	MCOL	Set foreground plot color
;	BW	Set for B and white plot
;	LTITLE 	Legend title
;	LSIZE 	Legend size
;	TRANS	Set to transpose x/y axes
;	WAIT	Pause before each line plotted
;	PSYM	Set plot symbol to be used.
;	SAMP	Set to sampling interval to select lines to plot
;	NOE	overplot
;	LINEA	Add iteger to linestyles
;	OFFX	Set to add given offset to X-values of each line
;	FCOLS	Force color indices
;	LEX	Extra structure for LEGEND
;	ONE	Set to force all lines on one plot
;	LSTR	Return structure of legend arguments
;	XGAP	Plot withing showing lines where gap in axis values
;		bigger than this.
;	XY	Sent to legend
;	LFILL	Fill legend box
;	TRI	Sent to LEGEND (as TR)
;       CC	Sent to LEGEND
;       BL      Sent to LEGEND
;       TL      Sent to LEGEND
;       BR      Sent to LEGEND
;       TC      Sent to LEGEND
;       BC      Sent to LEGEND
;       CR      Sent to LEGEND
;	THICK	As plot keyword
;	COLOR	As plot keyword
;	XRANGE	As plot keyword
;	YRANGE	As plot keyword
;	MTH	Set default line thickness
;	FLINES	Set linestyle for each line individually (supply array)
;	NODATA	Do not plot data (axes etc only)
;	SPLIT	Sent to LEGEND
;	WFAC	Sent to LEGEND
;	CKF	Set to only plot rows which have some finite values
;	WNF	Return selected rows if CKF is set.
;
; R.S. 20/06/97
; $Id: plot_array.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
pro plot_array,d0,x01,i0,npp=npp,xrange=xrange,yrange=yrange,_EXTRA=extra,$
	tri=tr,cc=cc,bl=bl,tl=tl,br=br,tc=tc,bc=bc,irange=irange,mean=mean,noleg=noleg,$
	mcol=mcol,stderr=pstd,bw=bw,thick=thick,ltitle=ltitle,trans=trans,$
	lsize=lsize,sdev=psdev,color=cols,mth=mth,wait=wait,psym=psym,samp=samp,$
	noe=noe,linea=linea,offx=offx,fcols=fcols1,lex=lex,flines=flines1,one=one,cr=cr,$
	lstr=lstr,xgap=xgap,xy=xy,nodata=nodata,split=split,lfill=lfill,wfac=wfac,ckf=ckf,wnf=wnf,$
	mdif=mdif,length=length,box=box


	if n_elements(box) eq 0 then box=1
	if keyword_set(fcols1) then fcols=fcols1
	if keyword_set(flines1) then flines=flines1
	if n_elements(offx) eq 0 then offx=0.
	if keyword_set(std) or keyword_set(sdev) then mean=1
	if not keyword_set(tr) and not keyword_set(tl) and $
		not keyword_set(cr) and not keyword_set(bl) and not keyword_set(br) then tl=1
	if not keyword_set(npp) then if keyword_set(one) then npp=n_elements(d0(0,*))*2 else npp=10
	if n_elements(x01) eq 0 then x0=findgen(n_elements(d0(*,0))) $
	else x0=x01
	if keyword_set(xgap) then begin
		dx=x0(1:*)-x0
		wh=where(dx gt xgap)
		if wh(0) ne -1 then begin
			j0=0
			wh=[wh,n_elements(x0)-1]
			for i=0,n_elements(wh)-1 do begin
				j1=wh(i)
				if not keyword_set(xrange) then xrange=range(x0,/nan)
				if not keyword_set(yrange) then yrange=range(d0,/nan)
				plot_array,d0(j0:j1,*),x0(j0:j1),i0,npp=npp,xrange=xrange,yrange=yrange,_EXTRA=extra,$
        tri=tr,cc=cc,bl=bl,tl=tl,br=br,tc=tc,bc=bc,mean=mean,noleg=noleg,$
        mcol=mcol,stderr=pstd,bw=bw,thick=thick,ltitle=ltitle,trans=trans,$
        lsize=lsize,sdev=psdev,mth=mth,wait=wait,psym=psym,samp=samp,$
        noe=noe,linea=linea,offx=offx,fcols=fcols,lex=lex,flines=flines,one=one,cr=cr,$
        lstr=lstr,xy=xy,wfac=wfac,mdif=mdif,length=length,box=box
				j0=wh(i)+1
				noe=1
			endfor
			return
		endif
	endif
	if n_elements(i0) eq 0 then i0=findgen(n_elements(d0(0,*)))
	if n_elements(psym) eq 0 then psym=0
	if not keyword_set(xrange) then xrange=range(x0,/nan)
	if xrange(1) eq xrange(0) then xrange=range(x0,/nan)
	if not keyword_set(irange) then irange=[min(i0),max(i0)]
	wh=where(x0 ge min(xrange) and x0 le max(xrange))
	if wh(0) eq -1 then begin
		message,'No points in XRANGE !',/info
		return
	endif
	x=x0
	if n_elements(x0(0,*)) eq 1 then x0=x0(*,replicate(0,n_elements(d0(0,*))))
	dr=d0(wh,*)
	x=x0
	d=d0
	wh=where(i0 ge irange(0) and i0 le irange(1))
	if wh(0) eq -1 then message,'No points in IRANGE !'
	i1=i0(wh)
	d=d(*,wh)
	if keyword_set(ckf) then begin
		tf=long(total(finite(d),1,/doub)+0.001)
		wnf=where(tf gt 0,nnf)
		if nnf gt 0 then begin
			d=d(*,wnf)
			i1=i1(wnf)
			if keyword_set(fcols) then fcols=fcols(wnf)
			if keyword_set(flines) then flines=flines(wnf)
		endif
	endif 
	nr=n_elements(d(0,*))
	mls=0 & if n_elements(mth) eq 0 then if !d.name eq 'PS' then mth=3 else mth=2 ; mean line styles
	if not keyword_set(mcol) then mcol=c0()
	if n_elements(samp) eq 0 then samp=1
	for i=0l,nr-1,npp do begin
		j=i+npp-1
		if j gt nr-1 then j=nr-1
		d2=d(*,i:j)
		d2r=dr(*,i:j)
		i2=i1(indgen((j-i+1)/samp)*samp+i)
		if not keyword_set(yrange) then begin
			yra=range(d2r,/nan)
		endif else yra=yrange
		if keyword_set(bw) then begin
			def_colors,(j-i+1)/samp,cols,lines,thicks
		endif else begin
			def_colors,(j-i+1)/samp,cols,lines,thicks,/color
			if keyword_set(flines) then lines=flines(0:n_elements(cols)-1)
			if keyword_set(fcols) then cols=fcols(0:n_elements(cols)-1)
			if keyword_set(thick) then thicks(*)=thick
		endelse
		if n_elements(psym) eq 1 then psyms=replicate(psym,n_elements(cols)) else psyms=psym
		if keyword_set(linea) then lines=lines+linea
		if not keyword_set(trans) then begin
			if not keyword_set(noe) then $
				plot,x(*,0),d2(*,0),xrange=xrange,yrange=yra,_EXTRA=extra,$
					/nodata
			if keyword_set(nodata) then goto,doleg
			oplot,x(*,0),d2(*,0),_EXTRA=extra,$
				color=cols(0),line=lines(0),thick=thicks(0),psym=psyms(0)
			for k=1,j-i,samp do begin
				if keyword_set(wait) then any_key
				oplot,x(*,k)+k*offx,d2(*,k),col=cols(k/samp),$
					line=lines(k/samp),thick=thicks(k/samp),psym=psyms(k/samp)
			endfor
		endif else begin
			if not keyword_set(noe) then $
				plot,d2(*,0),x(*,0),xrange=yra,yrange=xrange,_EXTRA=extra,$
					/nodata
			if keyword_set(nodata) then goto,doleg
			oplot,d2(*,0),x(*,0),_EXTRA=extra,$
				color=cols(0),line=lines(0),thick=thicks(0),psym=psyms(0)
			for k=1,j-i,samp do begin
				if keyword_set(wait) then any_key
				oplot,d2(*,k),x(*,k)+k*offx,col=cols(k/samp),$
					line=lines(k/samp),thick=thicks(k/samp),psym=psyms(k/samp)
			endfor
		endelse
doleg:
		txt=trim_zero(i2)
		if i ne j and keyword_set(mean) then begin
			cols=[cols,mcol]
			thicks=[thicks,mth]
			lines=[lines,mls]
			txt=[txt,'Mean']
			se=stderr(transpose(d2),mean=mean,sdev=sdev)
			if keyword_set(pstd) then begin
				error_bar,x,mean,0,se,col=mcol,$
					lines=mls,thick=mth
			endif
			if keyword_set(psdev) then begin
				oplot,x,mean+sdev,color=mcol,lines=2,$
					thick=mth
				oplot,x,mean-sdev,color=mcol,lines=2,$
					thick=mth
			endif
			oplot,x,mean,color=mcol,lines=mls,$
				thick=mth
		endif
		lstr={col:cols,lin:lines,thic:thicks,txt:txt,psym:psyms}
		if n_elements(lfill) eq 0 then lfill=1
		if not keyword_set(noleg) then $
			legend,color=cols,lines=lines,psym=psyms,thick=thicks,box=box,fill=lfill,$
			tr=tr,cc=cc,bl=bl,tl=tl,br=br,txt,$
			tc=tc,bc=bc,cr=cr,$
			title=ltitle,size=lsize,_EXTRA=lex,xy=xy,split=split,wfac=wfac,mdif=mdif,length=length
		if !p.multi(0) eq 0 and j ne nr-1 then any_key
	endfor
end
