;==============================================================================
;+
; HSV_CT
;
; Apply a saturation and value conversion to the current CT
;
; PARAMETERS
;	R,G,B  Returns CT values
;
; KEYWORDS
;	S,V	New saturation and value (scales original)
;	H	New hue values - rotation in angle / degrees
;	NLCT	Do not loadct
;	
;
; R.S. 16/04/02
; $Id: hsv_ct.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;==============================================================================
pro hsv_ct,r2,g2,b2,s=s0,v=v0,dm=dm,nlct=nlct,h=h0
	if keyword_set(dm) then begin
;
; test fade-out of standard CT
;
		set_a4,/la
		plot,[0,1],xst=5,yst=5,position=[0,0,1,1]
		tv_pos,findgen(4,4)+1
		for j=0,3 do begin
			wait,1
			for i=10,0,-1 do begin
				set_color,/ps & hsv_ct,s=float(i)/10
				tv_pos,findgen(4,4)+1
				wait,0.1
			endfor
		endfor
		set_color,/ps
		return
	endif
	tvlct,r,g,b,/get
	nr=n_elements(r)
	r2=r & g2=g & b2=b
	for i=2,nr-2 do begin
		color_convert,r(i),g(i),b(i),htmp,stmp,vtmp,/rgb_hsv
		if n_elements(s0) ne 0 then s=stmp*s0 else s=stmp
		if n_elements(v0) ne 0 then v=vtmp*v0 else v=vtmp
		if n_elements(h0) ne 0 then begin
			h=htmp+h0
			h=atan(sin(h*!dtor),cos(h*!dtor))/!dtor
			wh=where(h lt 0,nw)
			if nw gt 0 then h(wh)=h(wh)+360
		endif else h=htmp
		color_convert,h,s,v,rtmp,gtmp,btmp,/hsv_rgb
		r2(i)=rtmp
		g2(i)=gtmp
		b2(i)=btmp
	endfor
	if not keyword_set(nlct) then tvlct,r2,g2,b2
end
