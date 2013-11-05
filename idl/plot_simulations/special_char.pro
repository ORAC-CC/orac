;===============================================================================
;+
; SPECIAL_CHAR
;
; return string which will give variety of special characters independent
; of whether current device is ps or not
;
; PARAMETERS
;	required character specified as keyword below
;
; KEYWORDS
;	TICK	tick symbol
;	CROSS	cross symbol
;	APRROX	Approx =
;	MU	greek mu
;	PM	Plus/minus sign
;	DELTA	little delta
;	CDELTA	big delta
;
; R.S. 02/07/04
; $Id: special_char.pro 737 2011-03-22 15:01:12Z rsiddans $
;-
;===============================================================================
function special_char,tick=tick,cross=cross,approx=approx,mu=mu,pm=pm,delta=delta,cdelta=cdelta
	s=''
	if !d.name eq 'PS' then begin
		if keyword_set(tick) then s='!103!13'
		if keyword_set(cross) then s='!107!13'
		if keyword_set(approx) then s='~'
		if keyword_set(mu) then s='!9m!3!X'
		if keyword_set(delta) then s='!9d!3!X'
		if keyword_set(cdelta) then s='!9D!3!X'
		if keyword_set(pm) then s='!9'+string(177b)+'!X'
	endif else begin
		if keyword_set(tick) then s='!9C!3'
		if keyword_set(cross) then s='!9X!3'
		if keyword_set(approx) then s='!9N!3'
		if keyword_set(mu) then s='!7l!3!X'
		if keyword_set(delta) then s='!7d!3!X'
		if keyword_set(cdelta) then s='!7D!3!X'
		if keyword_set(pm) then s=string(177b)
	endelse
	return,s
end
