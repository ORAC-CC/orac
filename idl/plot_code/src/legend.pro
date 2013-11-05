;===============================================================================
;+
; LEGEND.PRO
;
; This procedure plots a legend.
;
; Note that pmnormal is called so that coordinates are nomalised in the plot
; region even if p.multi is set.
;
; LEGEND will use !AL settings if present
;
; PARAMETERS
;	X00       Normalised X coord for top left of legend
;	Y00	Normalised Y coord for top left of legend
;	STRS2	The legend strings
;
; KEYWORDS
;       LINEST  A vector of associated line styles
;       COLOR   A vector of associated colors
;       THICK   A vector of associated line thicknesses
;       PSYM    A vector of associated symbols
;       SIZE    Sets the text size
;       TSIZE   Sets the text size for the title
;       TITLE   A title for the legend
;       BOX     Set this to draw a box round the legend
;       LENGTH  The line length to be used.
;       WINDOW  Set to get legend normalised in plot window, rather than plot region (within plot).
;       FILL    Set this to fill the plotted box.
;       TEXT_ONLY       Set this for no plot symbols, just lines of text.
;       BL      "    " bottom left "  "
;       TL      legend in top left of plot
;       BR      "    " bottom right "  "
;       TR      "    " top right "  "
;       WFAC    Multiply legend box width by given factor
;       CL      "    " centre left "  "
;       CR      "    " centre right "  "
;       TC      "    " top centre "  "
;       BC      "    " bottom centre "  "
;       CC      "    " centre "  "
;       ANNOTATE        Set to array of strings to be used instead of
;       ALIGN   Define alignment of text
;       AP      Position automatically based on minimum pixel count from TL to CC above
;       HFACTOR Scale line separation (default 1.1)
;       SPLIT   Split words in strings over specified length
;       XY      Set xy position of legend (instead of argument)
;       FCOL    Color for filling of legend box
;       MDIF    Something to do with box border size
; 
; R.Siddans 8/2/95
; $Id: legend.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
pro get_legbox,mdif,x01,x02,y01,y02,boxw,boxh,x0,y0,x1,y1,$
	bl=bl,tl=tl,br=br,tr=tr,$
	cl=cl,cr=cr,tc=tc,bc=bc,cc=cc,$
	window=window,ap=ap,split=split,xy=xy

	if keyword_set(ap) then begin
		x00=x0
		y00=y0
		x10=x1
		y10=y1
		for i=0,8 do begin
if i eq 0 then get_legbox,mdif,x01,x02,y01,y02,boxw,boxh,x0,y0,x1,y1,/tl $
else if i eq 1 then get_legbox,mdif,x01,x02,y01,y02,boxw,boxh,x0,y0,x1,y1,/tr $
else if i eq 2 then get_legbox,mdif,x01,x02,y01,y02,boxw,boxh,x0,y0,x1,y1,/bl $
else if i eq 3 then get_legbox,mdif,x01,x02,y01,y02,boxw,boxh,x0,y0,x1,y1,/br $
else if i eq 4 then get_legbox,mdif,x01,x02,y01,y02,boxw,boxh,x0,y0,x1,y1,/cl $
else if i eq 5 then get_legbox,mdif,x01,x02,y01,y02,boxw,boxh,x0,y0,x1,y1,/cr $
else if i eq 6 then get_legbox,mdif,x01,x02,y01,y02,boxw,boxh,x0,y0,x1,y1,/tc $
else if i eq 7 then get_legbox,mdif,x01,x02,y01,y02,boxw,boxh,x0,y0,x1,y1,/tr $
else if i eq 8 then get_legbox,mdif,x01,x02,y01,y02,boxw,boxh,x0,y0,x1,y1,/cc
			res=convert_coord([x0,x0,x0+boxw,x0+boxw,x0],$
				[y0,y0-boxh,y0-boxh,y0,y0],/normal,/to_device)
			im=tvrd(res(0,0),res(1,2),res(0,2)-res(0,0),res(1,0)-res(1,2))
			tp=total(im ne !p.background)
			if n_elements(min_tp) eq 0 then min_tp=!d.x_size*!d.y_size
			if tp lt min_tp then begin
				x0_min=x0
				y0_min=y0
				x1_min=x1
				y1_min=y1
				min_tp=tp
			endif
			x0=x00
			y0=y00
			x1=x10
			y1=y10
		endfor
		x0=x0_min
		y0=y0_min
		x1=x1_min
		y1=y1_min
		return
	endif

	xmdif=mdif
	ymdif=mdif*float(!d.x_size)/!d.y_size
	if keyword_set(tl) then begin
		x01=xmdif
		y01=1.-ymdif
	endif
	if keyword_set(tr) then begin
		x01=1.-xmdif
		y01=1.-ymdif
	endif
	if keyword_set(bl) then begin
		x01=xmdif
		y01=ymdif
	endif
	if keyword_set(br) then begin
		x01=1.-xmdif
		y01=ymdif
	endif
	if keyword_set(cl) then begin
		x01=xmdif
		y01=0.5
	endif
	if keyword_set(cr) then begin
		x01=1.-xmdif
		y01=0.5
	endif
	if keyword_set(tc) then begin
		x01=0.5
		y01=1.-ymdif
	endif
	if keyword_set(bc) then begin
		x01=0.5
		y01=ymdif
	endif
	if keyword_set(cc) then begin
		x01=0.5
		y01=0.5
	endif
	if keyword_set(xy) then begin
		x01=xy(0)
		y01=xy(1)
	endif
	pmnormal,x01,y01,x02,y02,window=window
	if keyword_set(tr) or keyword_set(br) or keyword_set(cr) $
		then x02=x02-boxw
	if keyword_set(br) or keyword_set(bl) or keyword_set(bc) $
		then y02=y02+boxh
	if keyword_set(cl) or keyword_set(cr) or keyword_set(cc) $
		then y02=y02+boxh/2.
	if keyword_set(tc) or keyword_set(bc) or keyword_set(cc) $
		then x02=x02-boxw/2.
	dx=x02-x0
	dy=y02-y0
	x0=x0+dx
	y0=y0+dy
	x1=x1+dx
	y1=y1+dy
end

pro legend,x00,y00,strs2,linest=linest,color=color,thick=thick,psym=psym,$
	size=size,tsize=tsize,title=title,box=box,length=length,window=window,$
	fill=fill,text_only=txt,bl=bl,tl=tl,br=br,tr=tr,wfac=wfac,$
	cl=cl,cr=cr,tc=tc,bc=bc,cc=cc,annotate=ann,align=dalign,ap=ap,hfactor=hfactor,$
	split=split,xy=xy,fcol=fcol,mdif=mdif

	if n_elements(dalign) eq 0 then dalign=0.

	if n_elements(mdif) eq 0 then mdif=0.05
	if n_elements(strs2) eq 0 then begin
		strs=x00
		x01=0
		x02=0
	endif else begin
		x01=x00
		y01=y00
		strs=strs2
	endelse
	nlegs=n_elements(strs)
	tht=!p.charsize
	if tht le 0 then tht=1.
	if not keyword_set(size) then size=tht/max([float(!p.multi(1)),1.])
	if not keyword_set(tsize) then tsize=1.2*size
	if not keyword_set(length) then length=0.05*size
	if not keyword_set(linest) then linest=replicate(0,nlegs)
	if not keyword_set(thick) then thick=replicate(0,nlegs)
	if not keyword_set(psym) then psym=replicate(0,nlegs)
	if not keyword_set(color) then color=replicate(c0(),nlegs)
	size=float(size)
	tsize=float(tsize)
	if keyword_set(tl) or keyword_set(bl) $
	   or keyword_set(tr) or keyword_set(br) $
	   or keyword_set(cl) or keyword_set(cr) $
	   or keyword_set(tc) or keyword_set(bc) $
	   or keyword_set(cc) or keyword_set(ap) then begin
		x01=0.
		y01=0.
	endif else if n_elements(strs2) eq 0 then begin
		ap=1
		x01=0.
		y01=0.
	endif
	if keyword_set(split) then begin
		wh=where(strlen(strs) gt split)
		if wh(0) ne -1 then begin
			nlegs3=nlegs+n_elements(wh)
			strs3=strarr(nlegs3)
			color3=lonarr(nlegs3)
			linest3=lonarr(nlegs3)
			thick3=lonarr(nlegs3)
			psym3=lonarr(nlegs3)
			i3=0
			iwh=0
			for i=0,nlegs-1 do begin
				strs3(i3)=strs(i)
				color3(i3)=color(i)
				linest3(i3)=linest(i)
				thick3(i3)=thick(i)
				psym3(i3)=psym(i)
				if iwh lt n_elements(wh) then if i eq wh(iwh) then begin
					iwh=iwh+1
					psp=split
					while psp gt 0 and strmid(strs(i),psp,1) ne ' ' do psp=psp-1
					if psp le 0 or psp ge split-1 then psp=split-1
					strs3(i3)=strtrim(strmid(strs(i),0,psp+1),2)
					i3=i3+1
					strs3(i3)=strtrim(strmid(strs(i),psp+1,strlen(strs(i))),2)
					color3(i3)=!p.background
				endif
				i3=i3+1
			endfor
			legend,x01,y01,strs3,linest=linest3,color=color3,thick=thick3,psym=psym3,$
				size=size,tsize=tsize,title=title,box=box,length=length,window=window,$
				fill=fill,text_only=txt,bl=bl,tl=tl,br=br,tr=tr,wfac=wfac,$
				cl=cl,cr=cr,tc=tc,bc=bc,cc=cc,annotate=ann,align=dalign,ap=ap,hfactor=hfactor,$
				split=split,xy=xy,fcol=fcol,mdif=mdif
			return
		endif
	endif
;
; convert x,y to nomalised in plot region
;
	pmnormal,x01,y01,x0,y0,window=window
;
; set up margins and gap between line and string
;
	lm=0.02*size
	rm=0.01*size
	wgap=0.02*size
	hgap=0.005*size
	tm=0.007*size
	bm=0.01*size
	if keyword_set(txt) then begin
		lm=rm
		wgap=0.
		length=0.
	endif
;
; get normalised text size
;
	text_ht=float(!d.y_ch_size)/float(!d.y_size)*size
	text_wd=float(!d.x_ch_size)/float(!d.x_size)*size
	if n_elements(hfactor) eq 0 then hfactor=1.1
;
; work out legend dimensions
;
	max_len=0
	for i=0,nlegs-1 do begin
		strs(i)=strtrim(strs(i),2)
		max_len=max([max_len,strlen(strrcom(strs(i)))])
	endfor
	min_psym=min(psym)
	if min_psym gt 0 then length=text_wd
 	boxw=lm+length+wgap+text_wd*max_len+rm
	boxh=tm+hfactor*text_ht*nlegs+bm
	x1=x0+lm
	y1=y0-tm-text_ht/2.0
	if keyword_set(title) then begin
		title=strtrim(title,2)
		boxh=boxh+n_elements(title)*hfactor*text_ht*tsize/size+hgap
		boxw1=boxw
		boxw2=lm+strlen(title)*text_wd*tsize/size+rm
		boxw=max([boxw2,boxw])
		y1=y1-n_elements(title)*hfactor*text_ht*tsize/size-hgap
		x1=x0+lm+(boxw-boxw1)/2.0
	endif
	dx=0.
	dy=0.
	if keyword_set(wfac) then boxw=boxw*wfac
	get_legbox,mdif,x01,x02,y01,y02,boxw,boxh,x0,y0,x1,y1,$
		bl=bl,tl=tl,br=br,tr=tr,$
		cl=cl,cr=cr,tc=tc,bc=bc,cc=cc,$
		window=window,ap=ap,xy=xy
;
; erase plot beneath legend box
;
	if keyword_set(fill) then begin
		if n_elements(fcol) eq 0 then fcol=!p.background
		polyfill,[x0,x0,x0+boxw,x0+boxw,x0],$
			[y0,y0-boxh,y0-boxh,y0,y0],/normal,$
			color=fcol
	endif
;
; draw box
;
	if keyword_set(box) then begin
		plots,[x0,x0,x0+boxw,x0+boxw,x0],$
			[y0,y0-boxh,y0-boxh,y0,y0],/normal
	endif
	if keyword_set(title) then begin
		for iti=0,n_elements(title)-1 do begin
			title_x=x0+boxw/2.0
			title_y=y0-text_ht*tsize/size*(iti+1)
			xyouts,title_x,title_y,title(iti),$
				align=0.5,/normal,size=tsize
		endfor
	endif

	for i=0,nlegs-1 do begin
	  	cy=y1-float(i)*hfactor*text_ht
		psi=psym(i)
		lpsi=long(psi+0.001)
		if abs(psi) gt 8 then begin
			if abs(psi-lpsi) gt 0.2 then fill=0 else fill=1
			set_user_sym,abs(lpsi)-9,fill=fill
			if lpsi lt 0 then psi=-8 else psi=8
		endif
		if psi gt 0 then $
			plots,x1+length/2.,cy,$
				linest=linest(i),color=color(i),$
				thick=thick(i),$
				psym=psi,/normal $
		else if not keyword_set(txt) then begin
			if keyword_set(ann) then begin
				xyouts,x1+length/2.,cy,ann(i),alig=0.5,/norm,$
					size=size
			endif else begin
				plots,[x1,x1+length],[cy,cy],$
					linest=linest(i),color=color(i),$
					thick=thick(i),$
					psym=psi,/normal
			endelse
		endif
		if dalign eq 0 then xp=x1+length+wgap $
		else if dalign eq 1 then xp=x0+boxw $
		else xp=x0+boxw/2.0
		xyouts,xp,cy-text_ht/4.,$
			strs(i),size=size,/normal,align=dalign
	endfor
end
