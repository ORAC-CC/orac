;===============================================================================
;+
; RD_RET_CLDMODEL
;
; Read result file from RET_CLDMODEL
;
; PARAMETERS
;	FI	File name
;
; KEYWORDS
;	H	Return file header
;	ND	Do not read data, only header
;	ISEL	Set indices of results to be read - should be
;		subset of pixels processed so need to read header,
;		ID which pixels required based on line,column and then
;		send index to these pixels in isel
;		(in this case the header returned in H will be modified
;		as if the file contained only the selected pixels)
;	RECOVER	Read as many records as possible from the file
;		(So recover points up to point of retrieval crash)
;	FH	Return header exactly as read from file (without
;		additional things which are set here)
;
; R.S. 19/12/08
; $Id: rd_ret_cldmodel.pro 1480 2012-04-04 13:28:39Z rsiddans $
;-
;===============================================================================
function rd_ret_cldmodel,fi,h=h,nd=nd,isel=isel,notr=notr,recover=recover,fh=fh
	if n_elements(recover) eq 0 then recover=1
	if keyword_set(recover) then notr=0
if not keyword_set(notr) then begin
catch,Error_status
IF Error_status NE 0 THEN BEGIN
PRINT, 'Error index: ', Error_status
PRINT, 'Error message:', !ERR_STRING ;
CATCH, /CANCEL
close,/all
heap_gc
if keyword_set(recover) and n_tags(res) gt 0 then begin
message,/info,'Warning: File not complete, recovering valid records'
	wok=where(res(0,*).tc gt 0,nok)
	if nok gt 0 then begin
		res=res(*,wok)
		h=struct_rep(h,'u',h.u(wok))
		h=struct_rep(h,'v',h.v(wok))
		if h.sg.sat eq 1 then begin
			sg=h.sg
			sg=struct_rep(sg,'lon',sg.lon(wok))
			sg=struct_rep(sg,'lat',sg.lat(wok))
			sg.latr=range(sg.lat)
			sg.lonr=range(sg.lon)
			h=struct_rep(h,'sg',sg)
		endif
		return,res
	endif else return,0
endif else begin
	return,0
endelse
ENDIF
endif
	if n_elements(fi) eq 0 then fi='$CLDMODEL_PATH/out/MSG2-SEVI-MSG15-0100-NA-20080613115741.str'
	openr,lun,fi,/get,/compress
	ver=0.
	sl=0l
	readu,lun,ver
	if ver gt 3.19 then message,'Warning: Version not implemented for read: '+trim_zero(ver),/info
;
; on basis of version define and read header
;
	readu,lun,sl
	if ver ge 3.00 then begin
		h=rd_struct1(lun,asc=0)
		fh=h
;
; add some details to the basic header
;
		nst=n_elements(h.types)
		if h.sv.l2 gt 0 then nst=nst-1
		h=create_struct(h,{version:ver,nx:h.sv.n_total,ny:n_elements(h.ye),npix:n_elements(h.u),nst:nst})
	endif else if ver ge 2.00 then begin
		h=rd_struct1(lun,asc=0)
		fh=h
;
; add some details to the basic header
;
		nst=n_elements(h.types)
		if h.l2 gt 0 then nst=nst-1
		h=create_struct(h,{version:ver,nx:n_elements(h.ae_sea),ny:n_elements(h.ye),npix:n_elements(h.u),nst:nst})
	endif else begin
		if ver ge 1.11 then begin
			h={file:string(replicate(32b,sl)),npix:0l,nx:0l,ny:0l,nst:0l,nbl:0l,nbc:0l,l2:0l,zstar:1l,diag:0l}
		endif else if ver ge 1.1 then begin
			h={file:string(replicate(32b,sl)),npix:0l,nx:0l,ny:0l,nst:0l,nbl:0l,nbc:0l,l2:0l,diag:0l}
		endif else begin
			h={file:string(replicate(32b,sl)),npix:0l,nx:0l,ny:0l,nst:0l,diag:0l}
		endelse
		readu,lun,h
		fh=h
		if ver ge 1.1 then begin
			hx={line:lonarr(h.npix),column:lonarr(h.npix),ye:fltarr(h.ny),$
				ae_sea:fltarr(h.nx),ae_land:fltarr(h.nx),$
				cor_px:intarr(h.nx)}
		endif else if ver ge 1.02 then begin
			hx={line:intarr(h.npix),column:intarr(h.npix),ye:fltarr(h.ny),ae_sea:fltarr(h.nx),ae_land:fltarr(h.nx)}
		endif else begin
			hx={line:intarr(h.npix),column:intarr(h.npix),ye:fltarr(h.ny),ae:fltarr(h.nx)}
		endelse
		readu,lun,hx
		h=create_struct(h,hx,{version:ver,ntypes:h.nst})
		if ver lt 1.11 then h=create_struct(h,{nbl:0l,nbc:0l})
	endelse
	if ver lt 3.13 then h=create_struct(h,{time:replicate(0d0,n_elements(h.u))})
	if ver lt 3 then begin
;
; add sv structure for backwards compatibility
;
		h=create_struct(h,{sv:def_cldmodel_sv(l2=h.l2,srf=0,lcot=1,cre=1,pc=1,ts=1,twore=h.twore,zstar=h.zstar)})
;
; add sat tag to sg for backwards compatibility
;
		if is_tag(h.sg,'sat') eq 0 then h=struct_rep(h,'sg',create_struct(h.sg,{sat:0l}))
	endif
	if keyword_set(nd) then return,h
;
; now read results
;
	if ver ge 3.0 then begin
		if h.sv.l2 eq 0 then cd=0. else cd=fltarr(2)
		res1={y:fltarr(h.ny),y0:fltarr(h.ny),yn:fltarr(h.ny),xn:fltarr(h.nx),cost:0.,conv:0,tc:0.,itype:0,column_density:cd}
		if ver ge 3.11 then res1=create_struct(res1,{zc:0.})
		if ver ge 3.13 then begin
			nsol=total(long(h.s.solar_flag gt 0),/pres)
			nir=total(long(h.s.thermal_flag gt 0),/pres)
			if nir eq 0 then nir=1
			res1=create_struct(res1,{ni:0,white_sky_albedo:fltarr(nsol),clearsky_bt:fltarr(nir)})
		endif
		if ver ge 3.19 then res1=create_struct(res1,{lza:0.,sza:0.,raz:0.})
		if h.dia gt 0 then begin
			if ver ge 3.1 then res1=create_struct(res1,{x0:fltarr(h.nx)})
			if h.dia eq 1 then begin
				res1=create_struct(res1,{kn:fltarr(h.nx,h.ny),sx:fltarr(h.nx,h.nx),column_density_error:cd,ae:fltarr(h.nx),syd:fltarr(h.ny)})
			endif else if h.dia eq 2 then begin
				res1=create_struct(res1,{xe:fltarr(h.nx),column_density_error:cd})
			endif else message,'unknown DIA'
		endif
	endif else if ver ge 2.0 then begin
		nblock=(h.nbl*2+1)*(h.nbc*2+1)
		if h.l2 eq 0 then begin
			res1={y:fltarr(h.ny),y0:fltarr(h.ny),yn:fltarr(h.ny),xn:fltarr(h.nx),cost:0.,conv:0,tc:0.,itype:0}
		endif else begin
			res1={y:fltarr(h.ny,nblock),y0:fltarr(h.ny,nblock),yn:fltarr(h.ny,nblock),xn:fltarr(h.nx,nblock),cost:0.,conv:0,tc:fltarr(nblock)}
		endelse
		if ver ge 2.04 then begin
		    if h.dia eq 1 then begin
			if h.l2 eq 0 then begin
				res1=create_struct(res1,{kn:fltarr(h.nx,h.ny),sx:fltarr(h.nx,h.nx),sa:fltarr(h.nx,h.nx),syd:fltarr(h.ny)})
			endif else begin
				res1=create_struct(res1,{kn:fltarr(h.nx*nblock,h.ny*nblock),sx:fltarr(h.nx*nblock,h.nx*nblock),sa:fltarr(h.nx*nblock,h.nx*nblock),syd:fltarr(h.ny*nblock)})
			endelse
		    endif else if h.dia eq 2 then begin
			if h.l2 eq 0 then begin
				res1=create_struct(res1,{xe:fltarr(h.nx)})
			endif else begin
				res1=create_struct(res1,{xe:fltarr(h.nx*nblock)})
			endelse
		    endif
		endif
	endif else if ver ge 1.1 then begin
		nblock=(h.nbl*2+1)*(h.nbc*2+1)
		res1={y:fltarr(h.ny,nblock),y0:fltarr(h.ny,nblock),yn:fltarr(h.ny,nblock),xn:fltarr(h.nx,nblock),cost:0.,conv:0,tc:fltarr(nblock)}
	endif else if ver ge 1.01 then begin
		res1={y:fltarr(h.ny),y0:fltarr(h.ny),yn:fltarr(h.ny),xn:fltarr(h.nx),cost:0.,conv:0,tc:0.}
	endif else begin
		res1={y:fltarr(h.ny),y0:fltarr(h.ny),yn:fltarr(h.ny),xn:fltarr(h.nx),cost:0.,conv:0}
	endelse
	nsel=n_elements(isel)
	if nsel eq 0 then begin
;
; if readiing all do it asap...
;
		res=replicate(res1,h.nst,h.npix)
		readu,lun,res
	endif else begin
;
; otherwise loop over selected pixels needed
;
		res=replicate(res1,h.nst,nsel)
		res2=replicate(res1,h.nst)
		ip=-1l
		so=sort(isel)	; sort selected pixels into ascending order
		iss=isel(so)
		for i=0l,nsel-1 do begin
			for ii=ip+1,iss(i) do readu,lun,res2
			res(0,i)=res2
			ip=isel(i)
		endfor
		bk=sort(so)
		res=res(*,bk)
;
; modify head
;
		h.npix=nsel
		if h.version lt 2 then begin
			h=strcolumnct_rep(h,'column',h.column(isel))
			h=struct_rep(h,'line',h.line(isel))
		endif else begin
			h=struct_rep(h,'u',h.u(isel))
			h=struct_rep(h,'v',h.v(isel))
		endelse
	endelse
	close_file,lun
	return,res
end
