;===============================================================================
;+
; SET_AL2
;
; This routine sets up the environment variables for my automatic
; line style/colour/thickness stuff.
; 
; Structure set up is:
;	!al.linestyle	linestyles to be used
;	!al.color	colours to be used
;	!al.thick	thicknesses to be used.
;	!al.psym	psyms to be used.
;	!al.iline	index of last line drawn to line,color,thick arrays
;
; PARAMETERS
;	NONE
;
; KEYWORDS
;	BW	Set for black and white defaults. Otherwise get colour
;		setup.
;	LINES	Set line styles
;	COLOR	Set colors
;	THICK	Set thicks
;	PSYM	Set psyms
;	CYCLE	Determines which lines parameter is cycled quickest.
;		1=cycle lines, then color,then thick)
;		2=cycle color, then line, then thick)
;		3=cycle lines, then thick,then color)
;
; NOTE: linestyle,color and thick are cycled all combinations are obtained,
; but psym is directly set to the specified values
;
; R.S. 26/8/96
;-
;===============================================================================
pro set_al2,bw=bw,color=color,thick=thick,lines=lines,cycle=cycle,psym=psym
;
; set up defaults
;
	if not keyword_set(psym) then psym=0
	if not keyword_set(lines) then lines=[0,2,1,3,4,5]
	if not keyword_set(color) then begin
		if keyword_set(bw) then begin
			color=[1,2,3]
			cycle=3
		endif else begin
			color=indgen(12)+1
			cycle=2
		endelse
	endif else begin
		if not keyword_set(cycle) then begin
			if n_elements(color) ge n_elements(lines) then $
				cycle=2 $
			else cycle=1
		endif
	endelse
	if not keyword_set(thick) then begin
		if not keyword_set(bw) then thick=[2,3,4,5] else thick=[1,2,3,4]
	endif
;
; make PS lines thicker
;
	if !d.name eq 'PS' then thick=fix(float(thick)*1.5)
;
; make arrays in order determined by cycle
;
	nli=n_elements(lines)
	nco=n_elements(color)
	nth=n_elements(thick)
	np=nli*nco*nth
	li=intarr(np)
	co=intarr(np)
	th=intarr(np)
	ip=0
	if cycle eq 1 then begin
		for ith=0,nth-1 do for ico=0,nco-1 do for ili=0,nli-1 do begin
			li(ip)=lines(ili)
			co(ip)=color(ico)
			th(ip)=thick(ith)
			ip=ip+1
		endfor
	endif else if cycle eq 2 then begin
		for ith=0,nth-1 do for ili=0,nli-1 do for ico=0,nco-1 do begin
			li(ip)=lines(ili)
			co(ip)=color(ico)
			th(ip)=thick(ith)
			ip=ip+1
		endfor
	endif else if cycle eq 3 then begin
		for ico=0,nco-1 do for ith=0,nth-1 do for ili=0,nli-1 do begin
			li(ip)=lines(ili)
			co(ip)=color(ico)
			th(ip)=thick(ith)
			ip=ip+1
		endfor
	endif else message,'Unknown CYCLE !'
;
; load environment variable
;
	nel=min([n_elements(li),n_elements(!al.linestyle)])
	!al.linestyle=li(0:nel-1)
	!al.color=co(0:nel-1)
	!al.thick=th(0:nel-1)
	nel=min([n_elements(psym),n_elements(!al.linestyle)])
	!al.psym=psym
	!al.iline=0
end
