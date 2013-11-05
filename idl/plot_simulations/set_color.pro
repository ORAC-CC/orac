;===============================================================================
;+
; SET_COLOR.PRO
;
; This procedure sets up a version of the idl 16 color table, but with
; the color indices running from 2 to 17 for each color. 0 is graphics
; device background color (black for X, white for PS). 
; 1 and !d.n_colors is graphics foreground. Remaining colors are as
; specified in the structure returned by this routine. All others
; colors are left the same as the color map in use before this routine
; was called.
;
; PARAMETERS
;	COLS	Returns a structure containing the color index of the
;		given color
;
; KEYWORDS
;	PS	Set this to get white backgrounf/black foreground on X display
;		to simulate PS plot.
;	BR	Load up my blue to red table
;	PR	Load up my purple to red table
;	BW	Load up my black and white
;	GREY	Set to do grey scale. shades are as different from
;		each other as possible. For use with what should be colour
;		plots on B+W printers
;	RWB	runs blue-white-red
;	GAMMA	Apply gamma correction to tables
;	CT	Set to IDL colour table number
;	REVERSE	Set to reverse normal colour table (other than fg/bg colours)
;	GW	Set for grey-white colour scale (useful for overplotting line
;		contour on grey scale contour).
;	RS	Set color using LOADCT_RS
;	JR	Set to JRs line-drawing colors
;       CLOUD   A.Stevens Cloud mask colours for ATSR/AATSR images
;	BGRP	Blue-green-red-pink-white
;	TW	Set top colour = white in conjunction with RS colour table
;	STRIPE	Purple-red with stripes
;	LD	Standard colour-table except each colour alternates with
;		a darker version of the same, i.e. red,dark-red,green,dark-green etc
;
; R.S. 14/9/95
; $Id: set_color.pro 736 2011-03-22 15:00:58Z rsiddans $
;-
;===============================================================================
pro set_color,cols,ps=ps,br=br,pr=pr,bw=bw,atsr=atsr,grey=grey,rwb=rwb,$
	gamma=gamma,ct=ct,reverse=rev,gw=gw,rs=rs,jr=jr,cloud=cloud,bgrp=bgrp,tw=tw,rct=rct,stripe=stripe,omi=omi,ld=ld

;	if !d.n_colors eq 16777216l then begin
;;
;; 24 bit mode - just set fg black and bg white
;;
;		!p.background=16777215l
;		!p.color=0l
;		return
;	endif
	if keyword_set(rs) then begin
		loadct_rs,ps=ps,gamma=gamma,tw=tw,rct=rct,rev=rev
		return
	endif
	if keyword_set(omi) then begin
		loadct_omi,ps=ps,gamma=gamma,rev=rev
		return
	endif
	if keyword_set(bgrp) then begin
		loadct_rs,ps=ps,gamma=gamma,ifi='$RS_HOME/idl/msg9p7/ps/colour_table.str'
		return
	endif
	if keyword_set(atsr) then loadct,0
	tvlct,r,g,b,/get
	dnc=!d.table_size
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
	if keyword_set(ct) then br=1
	if keyword_set(br) or keyword_set(pr) then begin
		if not keyword_set(ct) then if keyword_set(pr) then $
			ct=23 else ct=33
		loadct,ct
		if keyword_set(gamma) then gamma_ct,gamma
		tvlct,red,green,blue,/get
		if keyword_set(rev) then begin
			red=reverse(red)
			green=reverse(green)
			blue=reverse(blue)
		endif
		red(0)=bg
		green(0)=bg
		blue(0)=bg
		red(dnc-1)=fgd
		green(dnc-1)=fgd
		blue(dnc-1)=fgd
		tvlct,red,green,blue
	endif else if keyword_set(bw) then begin
		loadct,0
		tvlct,red,green,blue,/get
		if keyword_set(rev) then begin
			red=reverse(red)
			green=reverse(green)
			blue=reverse(blue)
		endif
		red(0)=bg
		green(0)=bg
		blue(0)=bg
		red(dnc-1)=fgd
		green(dnc-1)=fgd
		blue(dnc-1)=fgd
		tvlct,red,green,blue
	endif else if keyword_set(stripe) then begin
		loadct,23
		tvlct,red,green,blue,/get
		if keyword_set(rev) then begin
			red=reverse(red)
			green=reverse(green)
			blue=reverse(blue)
		endif
		red(0)=bg
		green(0)=bg
		blue(0)=bg
		red(dnc-1)=fgd
		green(dnc-1)=fgd
		blue(dnc-1)=fgd
		tvlct,red,green,blue
	endif else if keyword_set(gw) then begin
		loadct,0
		tvlct,red,green,blue,/get
		mar=max(red,min=mir)
		ms=mir+0.35*(mar-mir)
		nr=n_elements(red)
		red=ms+findgen(nr)/(nr-1.)*(mar-ms)
		green=red
		blue=red
		if keyword_set(rev) then begin
			red=reverse(red)
			green=reverse(green)
			blue=reverse(blue)
		endif
		red(0)=bg
		green(0)=bg
		blue(0)=bg
		red(dnc-1)=fgd
		green(dnc-1)=fgd
		blue(dnc-1)=fgd
		tvlct,red,green,blue
	endif else if keyword_set(grey) then begin
		fcl=0.7
		tvlct,grey,green,blue,/get
		if keyword_set(rev) then begin
			red=reverse(red)
			green=reverse(green)
			blue=reverse(blue)
		endif
		grey(dnc-1)=fgd
		fill=[bg,fg,175,75,150,100,200,125]
		grey(0)=fill(0:min([n_elements(fill),dnc-1])-1)
		tvlct,grey,grey,grey
	endif else if keyword_set(rwb) then begin
		tvlct,red,green,blue,/get
		if keyword_set(rev) then begin
			red=reverse(red)
			green=reverse(green)
			blue=reverse(blue)
		endif
		red(0)=bg
		green(0)=bg
		blue(0)=bg
		red(dnc-1)=fgd
		green(dnc-1)=fgd
		blue(dnc-1)=fgd
		color_convert,red,green,blue,h,l,s,/rgb_hls
		nc=dnc-2
		if nc gt 0 then begin
			sat=replicate(1.,nc)
			nc2=nc/2
			inc=findgen(nc2)/(nc2-1)
			hue=replicate(240.,nc2)
			light=inc*0.7+0.3
			if nc mod 2 eq 1 then begin
				hue=[hue,hue(nc2-1)]
				light=[light,light(nc2-1)]
			endif
			hue=[hue,replicate(0.,nc2)]
			light=[light,reverse(inc)*0.6+0.3]
			h(1)=hue
			l(1)=light
			s(1)=sat
		endif
		tvlct,h,l,s,/hls
	endif else if keyword_set(cloud) then begin
 		gbr_zflag_cols,cols  ; load cloud flagging colors (1->17)
	endif else begin
		if keyword_set(atsr) then begin
			rgb=[$
			   bg, bg,  bg,$
			   fg, fg,  fg,$
			 255,   0,   0,$        ; red
			 255, 138,   0]        ; orange
		endif else if keyword_set(jr) then begin
			x=rd_struct('$RS_HOME/idl/jr_ct.str')
			nj=n_elements(x.r)
			rgb=[bg, bg,  bg]
			rgb=[rgb,reform([transpose(x.r(1:nj-1)),transpose(x.g(1:nj-1)),transpose(x.b(1:nj-1))],(nj-1)*3)]
		endif else begin
			rgb=[$
			   bg, bg,  bg,$
			   fg, fg,  fg,$
			 220,   0,   0,$        ; 2 dark red
			   0, 170,   0,$        ; 3 dark green
			   0,   0, 255,$        ; 4 blue
			 255,   0, 255,$        ; 5 majenta
			   0, 255, 255,$	; 6 cyan
			 250, 220,   0,$        ; 7 yellow
			 255, 138,   0,$        ; 8 orange
			 177,  55,  40,$	; 9 brown
			   0, 250,   0,$        ; 10
			   0, 100,   0,$        ; 11 green
			 150, 150, 150,$	; 12 grey
			 166,   0, 179,$        ; 13 CLRC purple
			 220, 132, 132,$	; pastel red
			 177, 128, 122,$	; pastel brown
			 173, 107, 179,$	; pastel purple
			 255, 208, 153,$	; pastel orange
			 200, 200, 200,$	; 14 light grey
			 220, 220, 220,$	; 15 lighter grey
			 180, 180, 180,$	; 16 darker grey
			 150, 150, 150,$		; 17 darker grey
			   0, 150, 255,$       ; 18 light blue
			   0,   0, 170]       ; 19 dark blue
		endelse
		ncols=n_elements(rgb)/3
	 	rgb=transpose(reform(rgb,3,ncols))
		if keyword_set(ld) and ncols lt n_elements(r)/2 then begin
			rgb=[rgb,rgb(2:*,*)/2]
			isel=[0,1,lindgen(ncols-2)*2+2,lindgen(ncols-2)*2+3]
			rgb=rgb(sort(isel),*)
			ncols=n_elements(rgb)/2
		endif
		if ncols gt dnc then begin
			ncols=dnc
			rgb=rgb(0:ncols-1,*)
		endif
		r(0)=rgb(*,0)
		g(0)=rgb(*,1)
		b(0)=rgb(*,2)
		r(dnc-1)=fgd
		g(dnc-1)=fgd
		b(dnc-1)=fgd
		tvlct,r,g,b
		cols={$
			bg:		0,$
			fg:		1,$
			dark_red:	2,$
			dark_green:	3,$
			blue:		4,$
			majenta:	5,$
			cyan:		6,$
			yellow:		7,$
			red:		8,$
			green:		9,$
			orange:		10,$
			brown:		11,$
			grey:		12,$
			light_grey:	13}
	endelse
end
