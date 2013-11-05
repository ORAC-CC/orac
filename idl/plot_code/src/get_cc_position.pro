;===============================================================================
;+
; GET_CC_POSITION
;
; Set position of colour bar and contour plot for quick_cc,quick_cim,cc_box etc 
;
; PARAMETERS
;	PP	position for bar + main plot
;	DX_GT_DY 0 or 1 for bottom or left hand bar
;	BPOS	Returned bar position
;	CPOS	Returned main plot position
;
; KEYWORDS
;	CBLEN	Normalised length of colour bar
;	BRD	normalised edge of colour bar
;	CRD	normalised edge of main plot
;
; R.S. 23/03/11
; $Id$
;-
;===============================================================================
pro get_cc_position,pp,dx_gt_dy,cblen=cblen,bpos,cpos,brd=brd,crd=crd
        if n_elements(brd) eq 0 then brd=0.06
        if n_elements(crd) eq 0 then crd=0.17
	if n_elements(cblen) eq 0 then cblen=1.
	cboff=(1.-cblen)/2
	if dx_gt_dy then begin
		if n_elements(brd) eq 2 then begin
			bpos=[pp(0)+(pp(2)-pp(0))*brd(0),pp(1),pp(0)+(pp(2)-pp(0))*brd(1),pp(3)]
		endif else begin
			bpos=[pp(0),pp(1),pp(0)+(pp(2)-pp(0))*brd,pp(3)]
		endelse
		cpos=[pp(0)+(pp(2)-pp(0))*crd,pp(1),pp(2),pp(3)]
		bpos([1,3])=interpol(bpos([1,3]),[0.,1.],[0.+cboff,1.-cboff])
	endif else begin
		if n_elements(brd) eq 2 then begin
			bpos=[pp(0),pp(1)+(pp(3)-pp(1))*brd(0),pp(2),pp(1)+(pp(3)-pp(1))*brd(1)]
		endif else begin
			bpos=[pp(0),pp(1),pp(2),pp(1)+(pp(3)-pp(1))*brd]
		endelse
		cpos=[pp(0),pp(1)+(pp(3)-pp(1))*crd,pp(2),pp(3)]
		bpos([0,2])=interpol(bpos([0,2]),[0.,1.],[0.+cboff,1.-cboff])
	endelse
end
