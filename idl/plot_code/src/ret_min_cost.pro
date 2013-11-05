;===============================================================================
;+
; RET_MIN_COST
;
; Return retrieval results for case with minimum cost after first applying
; ranges specified by CLDMODEL_RANGES to eliminate unrealistic results.
;
; PARAMETERS
;	X	Result structure from ret_cldmodel
;	TYPES	Names of each type - if this set ranges will be applied.
;		retrievals outside ranges specified by CLDMODEL_RANGES
;		will have cost set to high value, so other more
;		realistic results are preferred.
;
; KEYWORDS
;	IPHASE	Set to variable which will contain index of selected phase
;
; R.S. 05/02/09
; $Id: ret_min_cost.pro 952 2011-06-10 09:42:08Z rsiddans $
;-
;===============================================================================
function ret_min_cost,x,types,iphase=iphase,h=h
	sz=size(x)
	if sz(0) eq 1 then return,x	; ignore 1d data 
	if sz(0) ne 2 then begin
		message,'Expected 2d data !',/info
		return,0
	endif
	np=n_elements(x(0,*))
	nt=n_elements(x(*,0))
	cost=x.cost
	cre=x.xn(1)
	lopd=x.xn(0)
	if is_tag(x,'zc') then cth=x.zc(0) else cth=x.xn(2)
	iphase=intarr(np)
	ny=n_elements(x(0).y)
;        lat=h.sg.lat
;        lon=h.sg.lon

;
; identify ice and liquid cloud types
;
	il=where(types eq 'LIQUID',nl)
	ii=where(types eq 'ICE',ni)	
	iv=where(strupcase(strmid(types,0,4)) eq 'VOLC',nv)	
	if nl gt 0 then tcl=reform(x(il,*).tc)
        if ni gt 0 then tci=reform(x(ii,*).tc)
;
; get ranges for each type
;
	if n_elements(types) gt 0 then begin
		rr=cldmodel_ranges(types)
		rmin=reform(rr.re(0))
		rmax=reform(rr.re(1))
		omin=reform(rr.lopd(0))
		omax=reform(rr.lopd(1))
	endif else begin
		rmin=replicate(-1d9,nt)
		rmax=replicate(1d9,nt)
		omin=replicate(-1d9,nt)
		omax=replicate(1d9,nt)
	endelse

;
; select min cost for each pixel after applying ranges
;
	for ip=0l,np-1 do begin
		ci=cost(*,ip)
		ri=cre(*,ip)
		oi=lopd(*,ip)
                zi=cth(*,ip)
		wh=where(ri lt rmin or ri gt rmax or oi lt omin or oi gt omax,nw)

		if nw gt 0 then ci(wh)=1d9

;
; deselect ice cloud if cost similar to liquid and TC is high 
;
		if nl eq 1 and ni eq 1 then begin
			if ci(il) lt ci(ii)*1.5 or ci(il) lt ny*1.5 and tcl(ip)/100.0 gt 273. then ci(ii)=1e9
                     endif


;
; deselect ice cloud if too close to ground and reff toohigh, i.e
; likely to be snow or ice 
;


;if abs(lat(ip)) gt 60 then begin
;		if nl eq 1 and ni eq 1 then begin
;
;		if  ((tci(ip)/100.0 gt 267.)  and (zi(ii)/100.0 lt 4.0) and (ri(ii) gt 100.)) then ci(ii)=1e9
;
;endif
;
;if  ((tci(ip)/100.0 gt 267.)  and (zi(ii)/100.0 lt 4.0) and (ri(ii) gt 100.)) then ci(ii)=1e9
;
;
;;	        if  tci(ip)/100.0 gt 267.  and zi(ii)/100.0 lt 1.9 and ri(ii) gt 140. then print, tci(ip)/100.0,zi(ii)/100.0 ,ri(ii)
;
;		endif
;
; deselect ash if cost similar to liquid or ice or very low altitude
;
		if nv eq 1 and ni eq 1 then if ci(ii) lt ci(iv)*1.1 then ci(iv)=1e9
		if nv eq 1 and nl eq 1 then if ci(il) lt ci(iv)*1.1 then ci(iv)=1e9
		iphase(ip)=(where(ci eq min(ci)))(0)
	endfor
	xc=x(lindgen(np)*nt+iphase)
	return,xc
end
