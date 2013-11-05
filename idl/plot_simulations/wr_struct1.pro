;===============================================================================
;+
; WR_STRUCT
;
; Writes a structre to a file such that it can be quickly read in again
;
; PARAMETERS
;	FILE	Name of file to write or LUN of already open file
;	X	Name of structure
;
; KEYWORDS
;	ASC	Set to write an ascii file
;	TERM	Set to write to terminal
;	COMPRESS	Set to write GZIP'd compressed file
;	D2F	Convert any double precision entries to float
;
; R.S. 29/07/99
;-
;===============================================================================
pro wr_struct1,file,x,asc=asc,term=term,compress=compress,d2f=d2f
	szf=size(file)
	nel=n_elements(x)
	if szf(szf(0)+1) eq 7 then begin
		get_lun,lun
		openw,lun,file,compress=compress
	endif else lun=file
	if nel gt 1 then begin
		sz=size(x)
		if sz(sz(0)+1) eq 8 then begin
			rs_write,lun,'START_OF_STRUCTARR',asc=asc,term=term
			rs_write,lun,nel,asc=asc,term=term
			rs_write,lun,tag_names(x(0),/structure_name),asc=asc,term=term
			for i=0,nel-1 do wr_struct1,lun,x(i),asc=asc,term=term,d2f=d2f
			goto,close_file
		endif else begin
			rs_write,lun,'START_OF_PTARR',asc=asc,term=term
			rs_write,lun,nel,asc=asc,term=term
			for i=0,nel-1 do if ptr_valid(x(i)) then wr_struct1,lun,*x(i),d2f=d2f else rs_write,lun,'NULL_PTR'
			goto,close_file
		endelse
	endif
	tn=tag_names(x)
	sn=tag_names(x,/str)
	if strlen(sn) eq 0 then sn='ANONYMOUS'
	rs_write,lun,'START_OF_STRUCTURE',asc=asc,term=term
	rs_write,lun,sn,asc=asc,term=term
	for i=0,n_tags(x)-1 do begin
		rs_write,lun,tn(i),asc=asc,term=term,d2f=d2f
		sz=size(x.(i))
		if sz(sz(0)+1) eq 8 then begin
			if keyword_set(asc) then begin
				printf,lun,sz
			endif else if keyword_set(term) then begin
				print,sz
			endif else begin
				writeu,lun,sz
			endelse
			xd=x.(i)
			for j=0,n_elements(x.(i))-1 do wr_struct1,lun,xd(j),asc=asc,term=term,d2f=d2f
		endif else begin
			rs_write,lun,x.(i),term=term,asc=asc,d2f=d2f
		endelse
	endfor
	rs_write,lun,'END_OF_STRUCTURE',asc=asc,term=term
close_file:
	if szf(szf(0)+1) eq 7 then begin
		close_file,lun
	endif
end
