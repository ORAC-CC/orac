;===============================================================================
;+
; RD_STRUCT1
;
; Writes a structre to a file such that it can be quickly read in again
;
; PARAMETERS
;	FILE	Name of file to be read or LUN
;
; KEYWORDS
;	ASC	Set to write an ascii file
;	COMPRESS Set if file is GZIP'd compressed (on IDL output)
;	CHK	Check file existance before openning
;	FF	Check to wait for file to exist
;       MKC	-
;       CEL	-
;       IC	-
;	
;
; R.S. 29/07/99
; $Id: rd_struct1.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function rd_struct1,file,asc=asc,compress=compress,chk=chk,ff=fofi,mkc=mkc,cel=cel,ic=ic
	sz=size(file)
	if n_elements(ic) eq 0 then ic=0
	if sz(sz(0)+1) eq 7 then begin
		if keyword_set(chk) then begin
			ff=rs_findfile(file,count=nfi)
			if nfi eq 0 then begin
				message,/cont,'File not found'
				return,-999
			endif
		endif
		get_lun,lun
retry:
		if keyword_set(fofi) then begin
			ff=rs_findfile(file,count=nfi)
			if nfi eq 0 then begin
				wait,2
				goto,retry
			endif
			on_ioerror,retry
		endif
		openr,lun,file,compress=compress
	endif else lun=file
	if eof(lun) then return,0
	while not eof(lun) do begin
		tn=rs_read(lun,asc=asc,mkc=mkc,cel=cel,ic=ic)
		if tn eq 'END_OF_STRUCTURE' then begin
			if n_elements(sname) ne 0 then begin
				if keyword_set(mkc) then begin
					if sname eq 'ANONYMOUS' then begin
						sname='anon'+trim_zero(ic)
						s=create_struct(s,name=sname)
						ic=ic+1
					endif
					if n_elements(cel) eq 0 then begin
						cel1=[c_tdstruct(s),sname]
						cel=cel1
					endif else begin
;
; add to list of structures if not already used this structure before
;
						whpn=where(cel(2,*) eq sname)
						if whpn(0) eq -1 then begin
							cel1=[c_tdstruct(s),sname]
							cel=[[cel],[cel1]]
						endif
					endelse
				endif
			endif
			goto,skip
		endif
		if tn eq 'START_OF_STRUCTURE' then begin
			sname=rs_read(lun,asc=asc,mkc=mkc,cel=cel,ic=ic)
		endif else if tn eq 'START_OF_STRUCTARR' then begin
			nel=rs_read(lun,asc=asc,mkc=mkc,cel=cel,ic=ic)
			sname=rs_read(lun,asc=asc,mkc=mkc,cel=cel,ic=ic)
			s=create_struct(name=sname,rd_struct1(lun,asc=asc,mkc=mkc,cel=cel,ic=ic))
			s=replicate(s,nel)
			for i=1,nel-1 do $
				s(i)=create_struct(name=sname,rd_struct1(lun,asc=asc,ic=ic))
		endif else if tn eq 'START_OF_PTARR' then begin
			nel=rs_read(lun,asc=asc,mkc=mkc,cel=cel,ic=ic)
			s=ptrarr(nel)
			for i=0,nel-1 do begin
				si=rd_struct1(lun,asc=asc,mkc=mkc,cel=cel,ic=ic)
				if n_tags(si) ne 0 then s(i)=ptr_new(si)
			endfor
		endif else if tn eq 'NULL_PTR' then begin
			s=0
			return,s
		endif else begin
			v=rs_read(lun,asc=asc,mkc=mkc,cel=cel,ic=ic)
			if n_elements(s) eq 0 then s=create_struct(tn,v) $
			else s=create_struct(s,tn,v)
		endelse
	endwhile
skip:
	if sz(sz(0)+1) eq 7 then begin
		close,lun
		free_lun,lun
		if keyword_set(mkc) then begin
;
; replace final anon structure name with something sensible...
;
			if sname eq 'anon'+trim_zero(ic-1) then begin
				cel=strreplace(cel,'struct '+sname,'struct mainstr')
				cel=strreplace(cel,'rd_'+sname,'rd_mainstr')
			endif
			bna='rd_struct'
			ofh=bna+'.h'
			ofc=bna+'.c'
			hc=['int *rd_struct_dims(FILE *in);\\char *rd_struct_name(FILE *in);\\char **rd_struct_strs(FILE *in,int *dims);\\',$
				reform(cel(0,*))]
			rc=['#include<stdio.h>\\#include"'+ofh+'"\\ \\',$
				reform(cel(1,*))]
			wr_struct_cel,ofh,hc
			wr_struct_cel,ofc,rc
		endif
	endif
	return,s
end
