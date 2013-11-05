;===============================================================================
;+
; LOADCT_RS
;
; Loads useful colour table for contour plots
;
; PARAMETERS
;	
;
; KEYWORDS
;	TEST	Load in original colour tables for manipulation/later editing
;		of the csel array
;	OUT	Write new colour table definition file
;	STEP	Set for stepped colour table
;	PS	Set for white background/black foreground rather than vice/versa
;	GAMMA	Apply gamma correction
;
; R.S. 27/04/00
; CP. 18/06/13 remove path from location of ct_rs.str file
;
; $Id: loadct_rs.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
pro loadct_rs,test=test,out=out,step=step,ps=ps,gamma=gamma,ifi=ifi,tw=tw,rct=c,rev=rev
	if n_elements(ifi) eq 0 then ifi='ct_rs.str'
	if keyword_set(test) or keyword_set(out) then begin
		loadct,33
		tvlct,/get,r1,g1,b1
		loadct,23
		tvlct,/get,r2,g2,b2
;
; define array selecting colors - negative nos from table 23, positive from 33
;
		csel=[-5,-28,8,52,74,92,-134,-141,-169,-193,145,161,-215,175,187,205,232,250]
		r=bytarr(n_elements(csel))
		g=bytarr(n_elements(csel))
		b=bytarr(n_elements(csel))
		wh=where(csel ge 0)
		if wh(0) ne -1 then begin
			r(wh)=r1(csel(wh))
			g(wh)=g1(csel(wh))
			b(wh)=b1(csel(wh))
		endif
		wh=where(csel lt 0)
		if wh(0) ne -1 then begin
			r(wh)=r2(-csel(wh))
			g(wh)=g2(-csel(wh))
			b(wh)=b2(-csel(wh))
		endif
stop
		if keyword_set(out) then wr_struct,ifi,{r:r,g:g,b:b}
	endif
	if n_tags(c) eq 0 then c=rd_struct(ifi)
;	if !d.n_colors lt 4 then message,'Must have at least 4 colours available !'
;	nc=!d.n_colors-3
; !d.n_colors gives problems on 24 bit graphics cards (B.Latter 24th Jan 2002)
	if !d.table_size lt 4 then message,'Must have at least 4 colours available !'
	nc=!d.table_size-3
	ni=n_elements(c.r)
	i1=findgen(ni)/(ni-1)
	i0=findgen(nc)/(nc)
	if keyword_set(step) then begin
		i1=(findgen(ni)+0.5)/(ni)
		in=get_nns(i0,i1)
		r=c.r(in)
		g=c.g(in)
		b=c.b(in)
	endif else begin
		i1=findgen(ni)/(ni-1)
		setup_linear,i1,i0,j1,j2,w1,w2
		r=byte(float(c.r(j1))*w1+float(c.r(j2))*w2)
		g=byte(float(c.g(j1))*w1+float(c.g(j2))*w2)
		b=byte(float(c.b(j1))*w1+float(c.b(j2))*w2)
	endelse
;
; set foreground / background colour
;
	udname=strupcase(!d.name)
	if udname eq 'X' or udname eq 'Z' then begin
		if keyword_set(ps) then bg=255 else bg=0
		fgd=255-bg
		fg=fgd
	endif else begin
		bg=0
		fg=0
		fgd=255
	endelse
	if keyword_set(gamma) then begin
		n = n_elements(r)
		s1 = findgen(n)
		s2 = float(n)*((findgen(n)/n)^gamma)
		r=byte(linear_1d(float(r),s1,s2))
		g=byte(linear_1d(float(g),s1,s2))
		b=byte(linear_1d(float(b),s1,s2))
	endif
	if keyword_set(rev) then begin
		r=reverse(r)
		g=reverse(g)
		b=reverse(b)
	endif
	r=[bg,fg,r,fgd]
	g=[bg,fg,g,fgd]
	b=[bg,fg,b,fgd]
if keyword_set(tw) then begin
	r(!d.table_size-2)=255
	g(!d.table_size-2)=255
	b(!d.table_size-2)=255
endif
	tvlct,r,g,b
	if keyword_set(test) then begin
		set_a4,/land,r=2,xm=6,/bw
;		nc=!d.n_colors
		nc=!d.table_size
		im=findgen(nc)
		im=im(*,replicate(0,2))
		chs=0.8
		plot,im,xticks=32,/xst,chars=chs
		tv_pos,im
		quick_cc,im
	endif
end
