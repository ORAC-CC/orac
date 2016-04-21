PRO nk_ref_grid, r1, r2, r3,r4,chan, title, ad=ad, overplot=overplot, col = col, label = label, psym=psym

	;Creates a Nakajima and King vertically homogeneous or adiabatically stratified grid with tau/re and N/H coordinates respectively.
	;r1    : array of reference channel reflectances (0.86 microns)
	;r2    : "        " absoption channel reflectances
	;chan  : MODIS absorption channel, 1 = 1.6 microns, 2 = 2.1 microns, 3 = 3.7 microns
	;title : Put a title on that fancy plot!
	
	;KEYWORDS:
	; AD       : make an N and H grid using an adiabatic table
	; Overplot : If you've used IDL, then I don't need to explain this...
	; Col      : Specify a color (0-255) for the data points.  The default is 250
	; Label    : Turn on labeling of tau/re  or n/h isolines.
	; psym     : Specify the plot symbol.  The default is 3

	RESTORE, '~/idl/cloud_model/ad_mie2.sav' 
	RESTORE, '~/idl/cloud_model/vh_mie2.sav'  
	
	IF KEYWORD_SET(ad) THEN x = ad_mie ELSE x = vh_mie
	IF KEYWORD_SET(psym) EQ 0 THEN psym = 3
	
	dims = SIZE(reform(r1)) 
	
	x1     = reform(r1)/100.0 
	xtitle = 'R0.86 [1]'

	mu  =  '!9' + STRING("155B) + '!X'
	tau =  '!9' + STRING("164B) + '!X'
	
	CASE chan OF 
		1: BEGIN
			x2     = x.r160
			ytitle = 'R1.6 [1]'
;print,'x2',x2
x2=reform(r2)/100.0 

		END
		2: BEGIN
			x2     = x.r210
			ytitle = 'R2.1 [1]'
		END
		3: BEGIN
			x2     = x.r370
			ytitle = 'R3.7 [1]'
x2=reform(r2)/100.0 
		END
	ENDCASE
	
;	set_a4,/rs,ro=2	
	IF KEYWORD_SET(overplot) THEN BEGIN
;
		;LOADCT, 39
	
		IF KEYWORD_SET(col) THEN pcol = col ELSE pcol = 250
		OPLOT, r1,r2,psym = psym, col = pcol 		
	
	ENDIF ELSE BEGIN
	
		x2_max = MAX(x2)
;	set_a4,/rs,ro=2
	;	LOADCT,0
maxrange=1
	if ytitle eq 'R3.7 [1]' then maxrange=0.4

		PLOT, [0,1], [0,x2_max], /NODATA, xtitle = xtitle, ytitle = ytitle, title=title,xrange=[0,1],yrange=[0,maxrange];, /isotropic
		FOR i=0,dims[1]-1 DO OPLOT, x1[i,*],x2[i,*], col = 50
		FOR i=0,dims[2]-1 DO OPLOT, x1[*,i],x2[*,i], col = 50
;	set_a4,/rs,ro=2
		;LOADCT, 39
	
;		IF KEYWORD_SET(col) THEN pcol = col ELSE pcol = 250
;		OPLOT, reform(r3)/100.0,reform(r4)/100.0,psym = psym, col = pcol 
	
	ENDELSE
	
	IF KEYWORD_SET(label) THEN BEGIN
		
		IF KEYWORD_SET(ad) THEN BEGIN
	
			FOR i=4,47,8 DO XYOUTS, x1[0,i], x2[0,i]- 0.025, 'H = '+strcompress(FIX(x.h[0,i])), CHARSIZE = 0.7, ORIENTATION = 270, /DATA
			FOR i=0,39,10 DO XYOUTS, x1[i,0]- 0.05, x2[i,0], 'N = '+strcompress(FIX(x.n[i,0])), CHARSIZE = 0.7, /DATA
	
		ENDIF ELSE BEGIN
	

nopd=n_elements(r3)
			FOR i=1,N_ELEMENTS(x1[0,*])-1 DO XYOUTS, x1[nopd-1,i]+0.01, x2[nopd-1,i], 'r = '+strcompress(FIX(r4[i]))+' '+mu+'m', CHARSIZE = 0.7, /DATA
			FOR i=0,N_ELEMENTS(x1[*,0])-1 DO XYOUTS, x1[i,0], x2[i,0]+0.01, 'COD'+' = '+Strcompress(FIX(10^r3[i])), CHARSIZE = 1.0, ORIENTATION = 90, /DATA
				
		ENDELSE
	
	ENDIF
	
END
