;===========================================================================
;+
; LOGO
;
; This puts a GIF logo on a plot. On X device nearest colours in the
; current colour table to those in the GIF are used. On PS real 
; colours will appear.
;
; PARAMETERS
;	X 	X position of logo
;	Y	Y position of logo
;
; KEYWORDS
;	XSIZE   Width of logo
;	YSIZE   Height of logo
;	FILE	Name of image file to be plotted
;	LCT	Set to load color table in GIF file. By default
;		(for x windows only) current colour table is 
;		used. For PS GIF's colour table is loaded anyway.
;	BW	Use B/W logo.
;	
; 
; R.S. 28/10/96
; $Id: logo.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===========================================================================
pro logo,x,y,file=file,xsize=xsize,ysize=ysize,lct=lct,bw=bw
	if not keyword_set(file) then begin
		if keyword_set(bw) then file='$RS_HOME/idl/clx2_small_bw.gif' $
		else file='$RS_HOME/idl/clx2_small.gif'
	endif
	if n_elements(xsize) eq 0 then xsize=0.07
	read_gif,file,logo,r,g,b
	sz=size(logo)
	xd=float(sz(1))
        yd=float(sz(2))
	xs=!d.x_size
        ys=!d.y_size
	ncol=n_elements(r)
	if n_elements(ysize) eq 0 then ysize=float(xsize)*$
			yd/xd*float(xs)/float(ys)
        if n_elements(x) eq 0 then x=0.
        if n_elements(y) eq 0 then y=1.-ysize
	if !d.name ne 'PS' then begin
;
; re-bin image to fit space...
;
		nxp=max([fix(xsize*xs),1])
		nyp=max([fix(ysize*ys),1])
		logo2=byte(congrid(logo,nxp,nyp))
;
; get nearest colours out of current table
;
		if not keyword_set(lct) then begin
			sz=size(logo2)
			logo3=make_array(size=sz)
			cmap=bytarr(n_elements(r))
			for i=0,ncol-1 do begin
				cmap(i)=find_color(r(i),g(i),b(i))
			endfor
			logo=cmap(logo2)
		endif else begin
			ncol=!d.n_colors
			ncmap=n_elements(r)
			logo2=byte(float(logo2)*float(ncol)/float(ncmap))
			bmap=fix(findgen(ncol)*float(ncmap)/float(ncol-1))
			ri=r(bmap)
			gi=g(bmap)
			bi=b(bmap)
			tvlct,ri,gi,bi
			logo=logo2
		endelse
	endif
	if !d.name eq 'PS' then begin
		tvlct,/get,r0,g0,b0
		tvlct,r,g,b
	endif
	tv,logo,x,y,xsize=xsize,ysize=ysize,/normal
	if !d.name eq 'PS' then tvlct,r0,g0,b0
end
