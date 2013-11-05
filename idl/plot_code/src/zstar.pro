;+
; this converts pressure in HPa (mbars) into Z* height in km.
; You can supply a float or an array of floats to be converted.
;
; PARAMETERS
;	P	Pressure in hPa or altitude in km
;	DPDZ	derivative of result with respect to input
;		calculated if KN set
;
; KEYWORDS
;	TOP	Set to return approx pressure, given height
;	KN	Set to calculate derivate of P wrt altitude or 
;		vice versa depending on TOP
; R.S. 1/1/96
; $Id: zstar.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
function zstar,pressure,dodi,top=top,kn=kn

	if keyword_set(top) then begin
		p=10.^(3.-pressure/16.)
		if keyword_set(kn) then dodi=-1./(16*alog10(exp(1.)))*p
		return,p
	endif else begin
		z=16.*(3.-alog10(pressure))
		if keyword_set(kn) then dodi=-16.*alog10(exp(1.))/pressure
		return,z
	endelse
end

