;===============================================================================
;+
; MK_3P7_REFLECTANCE
;
; Estimate solar reflectance contribution to 3.7 micron channel
;
; PARAMETERS
;	Y	Set of measurements (NCHAN,NOBSERVATIONS)
;	S	Corresponding instrument definition structure
;
; KEYWORDS
;	
;
; R.S. 22/02/11
; $Id$
;-
;===============================================================================
function mk_3p7_reflectance,y,s
	ic=get_nns([3.7,11],s.mwl)
	ic37=ic(0)
	ic11=ic(1)
	b11=reform(y(ic11,*))
	b37=reform(y(ic37,*))

	i37_total=bt_rttov(b37,s.rttov(ic37))

	i37_thermal=bt_rttov(b11,s.rttov(ic37))
	i37_solar=i37_total-i37_thermal
	r37=i37_solar*!pi/(s.i0/100.)
	return,r37
end
