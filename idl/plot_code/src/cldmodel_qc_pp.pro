;===============================================================================
;+
; CLDMODEL_QC
;
; Apply some quality control criteris to retrieval results
; NB will lose any arrangement of retrievals in multi-dimensional array
; (e.g. npixels,ntypes)
;
; PARAMETERS
;	X	Array of retrieval results from e.g. RD_RET_CLDMODEL
;
; KEYWORDS
;	IOK	Return indices of results passing QC
;	NOK	Return number passing QC
;	MCOT	Min COT
;	MCOST 	Max cost
;
; R.S. 01/05/09
; Cp added convergence criteria
; $Id: cldmodel_qc.pro 1102 2011-08-08 09:03:10Z rsiddans $
;-
;===============================================================================
function cldmodel_qc_pp,x,iok=iok,nok=nok,mmask=mmask,mcost=mcost
	ny=n_elements(x(0).y)
	if n_elements(mcost) eq 0 then mcost=1e9
	if n_elements(mmask) eq 0 then cflag=0
	iok=where(x.mask/100.0 eq 1 and x.xn(0) gt 0.35 and x.cost lt mcost*ny and x.conv lt 1,nok)
	if nok eq 0 then begin
		message,'No good retrievals !',/info
		return,0
	endif
	return,x(iok)
end
