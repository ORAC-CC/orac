;===============================================================================
;+
; SET_USER_SYM
;
; Set USERSYM to my useful symbols (these then used by setting PSYM=8)...
;
; PARAMETERS
;	NUM	Index of symbol
;		0 triangle pointing up
;		1 square
;		2 circle
;		3 diamond
;		4 triangle pointing down
;
; KEYWORDS
;	FILL	Set to fill or not
;	COLOR	As plot keyword
;	THICK	As plot keyword
;	FAC	Set factor by which to scale default symbol size
;
; R.S. 01/11/02
; $Id: set_user_sym.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
pro set_user_sym,num,fill=fill,color=color,thick=thick,fac=fac
	if n_elements(color) gt 0 then begin
		if color eq 0 then color1=c0() else color1=color
	endif
	if n_elements(num) eq 0 then num=1
	if n_elements(fac) eq 0 then fac=1.
	if n_elements(fill) eq 0 then fill=1
	if num eq 0 then begin
		x=(findgen(4)/3*360+60)*!pi/180
		usersym,sin(x)*1.4*fac,-cos(x)*1.4*fac,fill=fill,color=color1,thick=thick
	endif else if num eq 1 then begin
		usersym,[-1,1.,1,-1,-1]*fac,[-1.,-1,1,1,-1]*fac,fill=fill,color=color1,thick=thick
	endif else if num eq 2 then begin
		x=findgen(41)/40*2*!pi
		usersym,sin(x)*fac,cos(x)*fac,fill=fill,color=color1,thick=thick
	endif else if num eq 3 then begin
		usersym,[-1,0,1,0,-1]*fac,[0,1,0,-1,0]*fac,fill=fill,color=color1,thick=thick
	endif else if num eq 4 then begin
		x=(findgen(4)/3*360+60)*!pi/180
		usersym,sin(x)*1.4*fac,cos(x)*1.4*fac,fill=fill,color=color1,thick=thick
	endif else if num eq 5 then begin
		x=(findgen(4)/3*360+60)*!pi/180
		usersym,cos(x)*1.4*fac,sin(x)*1.4*fac,fill=fill,color=color1,thick=thick
	endif else if num eq 6 or num ge 10 then begin
		x=(findgen(4)/3*360+60)*!pi/180
		usersym,-cos(x)*1.4*fac,sin(x)*1.4*fac,fill=fill,color=color1,thick=thick
	endif else if num ge 7 and num le 9 then begin
		npts=num-3
		x=(findgen(npts*2+1)/(npts*2)*360+90)*!pi/180
		amp=float((indgen(npts*2+1)+1) mod 2)*0.7+0.7
		usersym,cos(x)*amp*fac,sin(x)*amp*fac,fill=fill,color=color1,thick=thick
	endif else message,'Undefined sym'
end
