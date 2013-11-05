;+
; DEF_CLDMODEL_SV
;
; Make structure defining the state vector for use by RET_CLDMODEL3
;
; PARAMETERS
;	S	Instrument definition structure (only needed if some options
;		set e.g. SRFC
; KEYWORDS
;	L2	Set up two-layer retrieval
;	TWORE	Set up retrieval of vertical gradient in effective adius
;	LCOT	Retrieve log10 cloud optical thickness
;	CRE	Retrieve cloud effective radius / microns
;	PC	Retrieve cloud pressure / hPa
;	ZSTAR	Retrieve PC in zstar / km
;	TS	Retrieve surface temperature
;	FRAC	Retrieve cloud fraction
;	SRF1	Retrieve 1 surface reflectance scaling factor applied to all channels
;		If set to 1 then this is done by retrieval as normal.
;		If set to 2 then scale factor is treated as a forward model error
;		which is applied to SY (SY becomes state depedent)
;	CSRF	Retrieve 1 surface reflectance scaling factor for each solar channel.
;		If set to 2 then do surface like swansea, so this means 1 value per channel
;		(s) and 1 value per view (p)
;	SO2	Retrieve column SO2 scaling factor
;	H2O	Retrieve column H2O scaling factor
;	TYPES	Set to array of type names (LUT structure tag names) and routine will
;		return structure which contains array of state-vector values for each
;		of these types, set to appropriate values by SET_SV_APRIORI_TYPES
;	LIN	Set to retrieve COT linear (not in log)
;	SA	Define prior covariance matrix, over-riding other settings of 
; 		prior error - code only checks this has right size, user must
;		make sure it corresponds correctly to the state-vector
;-
function dcs_double,x
	x1=double(x)
	if n_elements(x1) eq 1 then x1=x1(0)	; ensure scalar if 1 elements
	return,x1
end
function def_cldmodel_sv1,ud,kw,ap,ae,short_name,long_name,unit,log=log
	if n_elements(log) eq 0 then log=0
	llog=long(log)
	if not keyword_set(kw) then begin
		return,{nx:0l,ap:ap,ae:ud,fg:ud,short_name:short_name,long_name:long_name,unit:unit,log:llog}
	endif else begin
		return,{nx:n_elements(ap),$	; no.elements in this product
			i0:0l,i1:0l,$		; will be start/stop indices in whole state vec
			ap:dcs_double(ap),$			; default a priori state
			ae:dcs_double(ae),$			; default a priori error
			fg:dcs_double(ap),$			; default first guess
			short_name:short_name,long_name:long_name,unit:unit,log:llog}
	endelse
end
function def_cldmodel_sv,s,l2=l2,twore=twore,$
	lcot=lcot,cre=cre,pc=pc,zstar=ret_zstar,ts=ts,$
	frac=frac,srf=srf,csrf=csrf,so2=so2,h2o=h2o,types=types,lin=cot_lin,sa=sa

;
; set default state vec
;
	if n_elements(lcot) eq 0 then lcot=1
	if n_elements(cre) eq 0 then cre=1
	if n_elements(ts) eq 0 then ts=1
	if n_elements(pc) eq 0 then pc=1
	if n_elements(ret_zstar) eq 0 then ret_zstar=1
	if n_elements(cot_lin) eq 0 then cot_lin=0
	if keyword_set(csrf) then srf=1
	if n_elements(srf) eq 0 then srf=1
;
; check conflicts
;
	if keyword_set(frac) and keyword_set(l2) then message,'FRAC,L2 conflict!'
;
; setup defn structure for each retrieved state vec
;	
	ud=-999d0	; Value used to indicate ret_cldmodel3 must fill it in (scene dependent)
;
; first stuff that depends on whether L2 set or not
;
	lcot_s='LCOT' & lcot_l='Log10 ( Optical Depth )' & lcot_u='-'
	cre_s='Re' & cre_l='Effective radius' & cre_u='microns'
	pc_s='PC' & pc_l='Cloud pressure' & pc_u='hPa'
	if keyword_set(l2) then begin
 	 x={lcot   :def_cldmodel_sv1(ud,lcot ,[-1.,-1.],[10.,10],lcot_s,lcot_l,lcot_u,/log),$
	    cre  :def_cldmodel_sv1(ud,cre  ,[10.,40.],[10.,40],cre_s,cre_l,cre_u),$
	    pc   :def_cldmodel_sv1(ud,pc  ,[800.,300.],[300.,300.],pc_s,pc_l,pc_u)}
if l2 ne 1 and l2 ne 6 then message,'bad l2'
	endif else if keyword_set(twore) then begin
 	 x={lcot  :def_cldmodel_sv1(ud,lcot ,-1.,1e3,lcot_s,lcot_l,lcot_u,/log),$
	    cre  :def_cldmodel_sv1(ud,cre  ,[10.,60.],[1e3,1e3],cre_s,cre_l,cre_u),$
	    pc   :def_cldmodel_sv1(ud,pc  ,500.,1e3,pc_s,pc_l,pc_u)}
if l2 ne 1 then message,'bad l2'
	endif else begin
 	 x={lcot   :def_cldmodel_sv1(ud,lcot ,-0.2,1e3,lcot_s,lcot_l,lcot_u,/log),$
	    cre  :def_cldmodel_sv1(ud,cre  ,10.,1e3,cre_s,cre_l,cre_u),$
	    pc   :def_cldmodel_sv1(ud,pc  ,500.,1e3,pc_s,pc_l,pc_u)}
	endelse
;
; convert to linear in optical depth
;
	if keyword_set(cot_lin) then begin
		al10e=alog10(exp(1.))
		x.lcot.ap=10.^x.lcot.ap
		x.lcot.ae=x.lcot.ap*x.lcot.ae/al10e
		x.lcot.fg=10.^x.lcot.fg
		x.lcot.short_name='COT'
		x.lcot.long_name='Optical Depth'
		x.lcot.log=0
	endif
;
; covert to zstar
;
	if keyword_set(ret_zstar) then begin
		x.pc.unit='Z*km'
		x.pc.ap=zstar(x.pc.ap)
		x.pc.fg=zstar(x.pc.fg)
		x.pc.ae=10.
	endif
;
; now add others
;
	if keyword_set(csrf) then begin
;
; work out how many channels for which surface reflectance factor retrieved
;
		wc=where(s.solar_flag ne 0 and s.iview eq 0,nc)
		if nc eq 0 then message,'No solar channels idenfied for view 0'
;
; work out which multi-view channels these correspond to and record in icv
;
		nv=max(s.iview)+1
		icv=lonarr(nv,nc)-1
		ivc=lonarr(n_elements(s))-1
		for ic=0,nc-1 do begin
			wcv=where(s.name eq s(ic).name,ncv)
			if ncv ne nv then message,'All views not present for a channel for which surface reflectance factor retrieved !'
			icv(0,ic)=wcv
			ivc(wcv)=ic
		endfor
		if csrf eq 2 then begin
			nxs=nc+nv
			aps=0.1
			aes=1.
			sname='Surface reflectance s('+trim_zero(nc)+'), p('+trim_zero(nv)+')'
		endif else begin
			nxs=nc
			aps=1.
			aes=0.1
			sname='Surface reflectance scaling factor'
		endelse
	endif else begin
		nxs=1
		aps=1.
		aes=0.1
		sname='Surface reflectance scaling factor'
	endelse
	srfs=def_cldmodel_sv1(ud,srf,  replicate(aps,nxs),replicate(aes,nxs),'SRFC',sname,'-')
	if keyword_set(csrf) then srfs=create_struct(srfs,{nv:nv,nc:nc,icv:icv,ivc:ivc})
;
; ts special case with different ae over land and sea
;
	xts=create_struct(def_cldmodel_sv1(ud,ts   ,ud,    1.,'Ts','Surface temperature','K'),{ae_land:6.})
	x=create_struct(x,{$
	    ts   :xts,$
	    frac :def_cldmodel_sv1(ud,frac ,  1.,  1.,'f','Area fraction','-'),$
	    so2  :def_cldmodel_sv1(ud,so2  ,  0.,  3,'LSO2','Log10( SO2 / DU )','-',log=1),$
	    ;so2  :def_cldmodel_sv1(ud,so2  ,  1.,  1e3,'SO2','Column SO2','DU',log=0),$
	    h2o  :def_cldmodel_sv1(ud,h2o  ,  1., 0.5,'H2O','H2O scale factor','-'),$
	    srf  :srfs})
;
; define indices indicating which element in the state vector corresponds to each product
;
	np=n_tags(x)
	i0=0l
	nx=lonarr(np)
	for ip=0,np-1 do begin
		xi=x.(ip)
		if xi.nx gt 0 then begin
			xi.i0=i0
			xi.i1=i0+xi.nx-1
			x.(ip)=xi
			i0=i0+xi.nx
			nx(ip)=xi.nx
		endif
	endfor
	iret=(nx gt 0)
	wret=where(iret,nret)
	if nret eq 0 then message,'Nothing set to be retrieved !'
	id=strcat(trim_zero(long(iret)),del='')
	if keyword_set(l2) then id=id+'-l2-'+trim_zero(l2)
	if keyword_set(twore) then id=id+'-twore'
	if kywd_def(srf) eq 2 then id=id+'-srfSy'
	sv={l2:kywd_def(l2),twore:kywd_def(twore),$
		srf:kywd_def(srf),$
		csrf:kywd_def(csrf),zstar:kywd_def(ret_zstar),$
		iret:iret,$	; foreach product (tag in x) this is 0 if retrieed, 1 not
		wret:wret,$	; where iret eq 1
		nret:nret,$	; number of retrieved products (where iret eq 1)
		nx:nx,$		; number of elements in ech product
		n_total:long(total(nx)),$ ; total number of elements in state vector
		id:id,$		; a string identifying the state-vector definition
		undefined:ud,$	; 'undefined' value used in x
		x:x}
	if keyword_set(types) then sv=set_sv_apriori_types(sv,types)
	if keyword_set(sa) then begin
		if n_total ne 1 then begin
			sz=size(sa)
			if sz(0) ne 2 and n_total ne 1 then message,'Exected 2d array for SA'
			if sz(1) ne sz(2) then message,'SA expected to be square!'
		endif
		if n_elements(sa) ne n_total*n_total then message,'Bad size for SA'
		sv=create_struct(sv,{sa:sa})
	endif
	return,sv
end



