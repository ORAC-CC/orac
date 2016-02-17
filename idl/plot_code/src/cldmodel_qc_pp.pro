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
;       snowmask apply snowmask in addition to normal qc
;
; R.S. 01/05/09
; Cp 2014/01/28 added convergence criteria
; $Id: cldmodel_qc.pro 1102 2011-08-08 09:03:10Z rsiddans $
;-
;===============================================================================
function cldmodel_qc_pp,x,iok=iok,nok=nok,mmask=mmask,mcost=mcost,istomina=istomina,v1=v1,inst=inst,aer=aer,whbad=whbad

	ny=n_elements(x(0).y)
	if n_elements(mcost) eq 0 then mcost=1e9
	if n_elements(mmask) eq 0 then cflag=0
;	iok=where(x.mask/100.0 eq 1 and x.xn(0) gt 0.35 and x.cost lt mcost*ny and x.conv lt 1 and (x.tc lt 300 and  x.xn(0) gt 1.0) and x.ni lt 25  ,nok)

if ~keyword_set(v1) then begin
;	iok=where(x.mask/100.0 eq 1 or x.xn(0) gt 5  ,nok)
	iok=where(x.nn_pre_mask eq 1  ,nok)
;print,nok
iok=where(x.mask/100.0 eq 1 ,nok)
;print,nok
;print,range(x.mask)
;stop
     endif else begin
;version 1
	iok=where(x.mask/100.0 eq 1 ,nok)
        if inst eq 'avhrr' then iok=where(x.nn_pre_mask eq 1.0  ,nok)

     endelse


if keyword_set(aer) then begin

	iok=where(x.cth lt 4.  and x.cloud_type eq 0 ,complement=whbad,nok)
endif


	if nok eq 0 then begin
		message,'No good retrievals !',/info
		return,0
	endif
	return,x(iok)
end
