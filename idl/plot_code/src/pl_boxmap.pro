;===============================================================================
;+
; PL_BOXMAP
;
; Plots colour-coded boxes over map
; If NOMAP set then just plots colour-coded boxes on arbitray grid (i.e. not a map!)
;
; PARAMETERS
;	BLO	NB+1,NP Longitudes defining vertices of a closed polygon
;		which will be filled with colour coded value.
;		NB=4 for square, 3 for triangle etc
;		If 1d array supplied then data just plotted with PSYM=8,
;		which is set using my set_user_sym routine
;	BLA	as above latitude
;	D	NP data values
;
; KEYWORDS
;       LEVELS  Specify levels used to span available colour table
;       BPOS    Specify postion of colour bar
;       CPOS    Specify position of map
;       DTITLE  Colour bar axis title
;       _EXTRA  Is sent to KMAP_SET so keywords like LONR,LATR,EUR control map range etc
;       CHARS   As plot keyword
;       ATH     Set to threshold on max(lat.range,lon.range) of map above which not to draww ann
;       ANSIZE  Charsize for annotation
;       ANN     Set to set of string annotations for each D
;       POSITION        As plot keyword
;       BCB     As plot keyword
;       LCB     As plot keyword
;       FAC     Sent to SET_USER_SYM
;       NUM     Sent to SET_USER_SYM
;       HIRES   As map_set
;       NOCLIP  Usual meaning
;       TRUE    Set if D contains 32 bit colour indices for true-colour plotting
;       NOBAR   Do not draw colour bar
;       CBS     Sent to cbar
;       MIN_VALUE       Usual meaning
;       IMAGE   Plot data as image to x-buffer then use TV to display. (Generates more compact PS/PDF and allows full 24 bits with /TRUE)
;       BRD     As quick_cc
;       CRD     As quick_cc
;       NOCON   Do not overplot continents,map grid
;       NOE     Overplot boxes on existing map, LEVELS Must be defined in this case
;       CRANGE  Colour table range to use (ignored with /true)
;       MLINETHICK      As map_set
;	OPLIN	Overplot border of box as line
;	NOMAP	Do simply x-y coordinates, not a map
;	MCOL	Set COLOR keyword to kmap_set
;	RCOL	Return colours used
;
; R.S. 19/02/01
; $Id: pl_boxmap.pro 1714 2012-08-20 17:41:11Z rsiddans $
;-
;===============================================================================
pro pl_boxmap,blo,bla,d,levels=lev,bpos=bpos,cpos=cpos,dtitle=title,_EXTRA=extra,charsize=chars,$
	ath=ath,ansize=ansize,ann=ann,position=position,bcb=bcb,lcb=lcb,fac=fac,num=num,hires=hires,$
	noclip=noclip,true=true,nobar=nobar,cbs=cbs,min_value=min_value,image=image,brd=brd,crd=crd,nocon=nocon,$
	noe=noe,crange=crange,mlinethick=mlinethick,imres=imres,oplin=oplin,nomap=nomap,mcol=mcol,rcol=cols

	if keyword_set(image) then begin
;
; get the image by plotting to the z-buffer
;
		pm=!p.multi
		pd=!d.name
		set_plot,'z'
		!p.multi=0
		if n_elements(imres) eq 0 then imres=[1024,700]
		if n_elements(position) eq 0 then position=ypos()
		imr=long([float(imres(0))*(position(2)-position(0))+1,float(imres(1))*(position(3)-position(1))+1])
		device,set_resolution=imr
		device,z_buffering=0
		if keyword_set(true) then begin
			pbk=!p.background
			if pd ne 'PS' then !p.background=255
			tvlct,r0,g0,b0,/get
			loadct,0
			b=d/256/256
			g=d/256 mod 256
			r=d mod 256
			pl_boxmap,blo,bla,r,levels=findgen(256),dtitle=title,_EXTRA=extra,chars=chars,$
				ath=ath,ansize=ansize,ann=ann,position=[0.,0,1,1],bcb=bcb,lcb=lcb,fac=fac,num=num,hires=hires,$
				noclip=noclip,/nobar,cbs=cbs,min_value=min_value,brd=0,crd=0,/nocon,nomap=nomap,mcol=mcol
			imr=tvrd()
			pl_boxmap,blo,bla,g,levels=findgen(256),dtitle=title,_EXTRA=extra,chars=chars,$
				ath=ath,ansize=ansize,ann=ann,position=[0.,0,1,1],bcb=bcb,lcb=lcb,fac=fac,num=num,hires=hires,$
				noclip=noclip,/nobar,cbs=cbs,min_value=min_value,brd=0,crd=0,/nocon,nomap=nomap,mcol=mcol
			img=tvrd()
			pl_boxmap,blo,bla,b,levels=findgen(256),dtitle=title,_EXTRA=extra,chars=chars,$
				ath=ath,ansize=ansize,ann=ann,position=[0.,0,1,1],bcb=bcb,lcb=lcb,fac=fac,num=num,hires=hires,$
				noclip=noclip,/nobar,cbs=cbs,min_value=min_value,brd=0,crd=0,/nocon,nomap=nomap,mcol=mcol
			imb=tvrd()
			im=[[[imr]],[[img]],[[imb]]]
			tvlct,r0,g0,b0
			!p.background=pbk
		endif else begin
			pl_boxmap,blo,bla,d,levels=lev,dtitle=title,_EXTRA=extra,chars=chars,$
				ath=ath,ansize=ansize,ann=ann,position=[0.,0,1,1],bcb=bcb,lcb=lcb,fac=fac,num=num,hires=hires,$
				noclip=noclip,true=true,/nobar,cbs=cbs,min_value=min_value,brd=0,crd=0,/nocon
			im=tvrd()
		endelse
		im_cxr=!x.crange
		im_cyr=!y.crange
		if keyword_set(yty) then im_cyr=10d0^im_cyr
		if keyword_set(xty) then im_cxr=10d0^im_cxr
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
	if n_elements(noclip) eq 0 then noclip=0
	if n_elements(ath) eq 0 then ath=999.
	if keyword_set(true) then begin
		cols=d
		nobar=1
	endif else begin
		if n_elements(lev) eq 0 then begin
			if keyword_set(noe) then message,'LEVELS must be defined if NOE set!'
			lev=clevels(d)
		endif
		nl=n_elements(lev)
		if n_elements(crange) eq 2 then $
			col=rescale(findgen(nl),crange) else $
		col=findgen(nl)/(nl-1)*(!d.table_size-4)+2
;
; generate actual colours for each datavalue -
; colour indices derived based on defined levels / colours to
; match colour bar
;
		cols=bytarr(n_elements(d))+2b
		for i=0,nl-1 do begin
	       		wh=where(d gt lev(i),nw)
			if nw gt 0 then cols(wh)=col(i)
		endfor
		if n_elements(min_value) gt 0 then begin
			wh=where(d lt min_value,nw)
			if nw gt 0 then cols(wh)=!p.background
		endif
	endelse
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
	if n_elements(brd) eq 0 then brd=0.06
	if n_elements(crd) eq 0 then crd=0.17
	if dx gt dy then begin
		bpos=[pp(0),pp(1),pp(0)+(pp(2)-pp(0))*brd,pp(3)]
		cpos=[pp(0)+(pp(2)-pp(0))*crd,pp(1),pp(2),pp(3)]
	endif else begin
		bpos=[pp(0),pp(1),pp(2),pp(1)+(pp(3)-pp(1))*brd]
		cpos=[pp(0),pp(1)+(pp(3)-pp(1))*crd,pp(2),pp(3)]
	endelse
	if n_elements(cpos) eq 0 then begin
		p2c=[0.1,0.02,0.2,0.97]
		cpos=ypos(p1,p2c)
	endif
	if not keyword_set(noe) then begin
		if not keyword_set(nobar) then begin
			plot,[0,1],xst=5,yst=5,/nodata
			cbar,lev,col,position=bpos,/nomul,title=title,chars=chars,step=cbs
			!p.multi(0)=!p.multi(0)+1
		endif
		if keyword_set(nomap) then begin
			plot,range(blo),range(bla),/nodata,position=cpos,_EXTRA=extra,chars=chars
		endif else begin
			kmap_set,position=cpos,_EXTRA=extra,chars=chars,/adv,/nocon,/nogrid,col=mcol
		endelse
	endif
	np=n_elements(d)
	mlim=max([!x.crange(1)-!x.crange(0),!y.crange(1)-!y.crange(0)])
	if keyword_set(ann) and mlim lt ath then pan=1 else pan=0
	sz=size(blo)
	if keyword_set(image) then begin
		if keyword_set(true) then tv_pos,im,tru=3 else tv_pos,im
	endif else begin
		if keyword_set(true) then begin
			if !d.name eq 'PS' or !d.name eq 'Z' then begin
;
; avoid sending true colours through the colour table map -
; temporarily set colour table to b-w linear
;
				b=cols/256/256
				g=cols/256 mod 256
				r=cols mod 256
				tvlct,r0,g0,b0,/get
				cols=color_quan(r,g,b,r1,g1,b1)
				tvlct,r1,g1,b1
			endif else begin
				device,/decomp
			endelse
		endif
		if sz(0) eq 1 then begin
			set_user_sym,num,fac=fac
			plots,blo,bla,col=cols,psy=8,noclip=noclip
		endif else begin
			for i=0l,np-1 do begin
				if cols(i) ne !p.background then begin
					polyfill,blo(*,i),bla(*,i),col=cols(i),noclip=noclip
					if keyword_set(oplin) then plots,blo(*,i),bla(*,i),col=c0()
					if keyword_set(pan) then begin
						xyouts,max(blo(*,i)),mean(bla(*,i)),chars=ansize,ann(i),/data,noclip=noclip
					endif
				endif
			endfor
		endelse
		if keyword_set(true) then begin
			if !d.name eq 'PS' or !d.name eq 'Z' then begin
				tvlct,r0,g0,b0
			endif else begin
				device,decomp=0
			endelse
		endif
	endelse
	if not keyword_set(nocon) and not keyword_set(nomap) then begin
		map_continents,hires=hires,mlinethick=mlinethick
		map_grid
	endif
end
