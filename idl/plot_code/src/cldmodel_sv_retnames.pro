;===============================================================================
;+
; CLDMODEL_SV_RETNAMES
;
; Return names of retrieved parameters, matching the state-vector length
;
; PARAMETERS
;	SV	State vector definition structure
;
; KEYWORDS
;	
;
; R.S. 01/04/11
; $Id: cldmodel_sv_retnames.pro 779 2011-04-04 08:29:27Z rsiddans $
;-
;===============================================================================
function cldmodel_sv_retnames,sv
        short_names=strarr(sv.n_total)
        long_names=strarr(sv.n_total)
        units=strarr(sv.n_total)
        for iret=0,sv.nret-1 do begin
                it=sv.wret(iret)
                xi=sv.x.(it)
                print,'xi',xi
		if xi.nx gt 1 then adds='-'+trim_zero(indgen(xi.nx)) else adds=''
print,'xi',sv.x.(it)
print, 'short',short_names
print,'io',xi.i0

                short_names(xi.i0)=xi.short_name+adds
                long_names(xi.i0)=xi.long_name+adds
                units(xi.i0)=replicate(xi.unit,xi.nx)
        endfor
        return,{short_names:short_names,$
        	long_names:long_names,$
        	units:units}
end
