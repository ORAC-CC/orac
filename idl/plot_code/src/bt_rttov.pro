;===============================================================================
;+
; BT_RTTOV
;
; Calculate Planck function radiance in mW/cm-1/m2/sr 
; for instrument channel using RTTOV method
;
; Not function assume it is provided with an array of temperatures and an array of channels,
; and that all temperatures should be evaluated for all channels - so 2d array
; returned if >1 channel and >1 temp.
; Set keyword O2O if you are passing and array of temps and array of channels
; and want one-to-one correspondence between these i.e. t_i evaluated only for channel_i
; (Then number of temps and number of channels should be the same)
;
;
; PARAMETERS
;	T	Temperature (array) in K
;	S	Sub-structure RTTOV from rd_imager_chans(/rttov) 
;	KN	(optional) will contain deriv of radiance wrt T if keyword KN set
;
; KEYWORDS
;	INV	Set to return brightness temperature given radiance
;	KN	Set to calculate derivative of radiance wrt temperature
;	O2O	Set to evaluate 1 temp for 1 channel not all temps for all channels
;
; R.S. 08/10/10
; $Id: bt_rttov.pro 685 2011-03-18 14:18:30Z rsiddans $
;-
;===============================================================================
function bt_rttov,t,s,kn,kn=dkn,inv=inv,o2o=o2o
	nt=n_elements(t)
;
; cope with more than one channel by multiple calls
; more than one channel can be defined by array of s structures or arrays within a single s
;
	if not keyword_set(o2o) then begin
		ns=n_elements(s)
		if ns gt 1 then begin
			rad=dblarr(nt,ns)
			if keyword_set(dkn) then kn=dblarr(nt,ns)
			for i=0l,ns-1 do begin
				rad(0,i)=bt_rttov(t,s(i),inv=inv,kn1,kn=dkn)
				if keyword_set(dkn) then kn(0,i)=kn1
			endfor
			return,rad
		endif
		ns=n_elements(s.planck1)
		if ns gt 1 then begin
			rad=dblarr(nt,ns)
			if keyword_set(dkn) then kn=dblarr(nt,ns)
			offset=s.ff.offset
			slope=s.ff.slope
			planck1=s.planck1
			planck2=s.planck2
			for i=0l,ns-1 do begin
				s1={ff:{offset:offset(i),slope:slope(i)},planck1:planck1(i),planck2:planck2(i)}
				rad(0,i)=bt_rttov(t,s1,inv=inv,kn1,kn=dkn)
				if keyword_set(dkn) then kn(0,i)=kn1
			endfor
			return,rad
		endif
	endif
;
; do the single channel calculation
;
	planck1=s.planck1
	planck2=s.planck2
	offset=s.ff.offset
	slope=s.ff.slope
	w=where(slope le 0,nw)	; trap 0 slope for vis channels
	if nw gt 0 then slope(w)=1d0
	if keyword_set(inv) then begin
		p1=planck1/t+1d0
		lp1=alog_nz(p1)
		t_effective=planck2/lp1
		rad=(t_effective-offset)/slope
		if keyword_set(dkn) then kn=planck1*planck2/(p1*t*t*lp1*lp1)/slope
	endif else begin
		t_effective=offset + slope * t
		hkctf=planck2/t_effective
		ep=exp(hkctf)
		ep1=ep-1d0
     		rad = planck1 / ep1
		if keyword_set(dkn) then kn = slope*rad*ep/ep1*hkctf/t_effective
	endelse
	return,rad
end
