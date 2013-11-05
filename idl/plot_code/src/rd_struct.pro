;===============================================================================
;+
; RD_STRUCT
;
; Writes a structre to a file such that it can be quickly read in again
;
; PARAMETERS
;	FILE1	Name of file to be read or LUN
;
; KEYWORDS
;	ASC	Set to write an ascii file
;	COMPRESS Set if file is GZIP'd compressed (on IDL output)
;	CHK	Check file existance before openning
;	FF	Check to wait for file to exist
;	OLD	Read old format file...
;
; R.S. 29/07/99
; $Id: rd_struct.pro 1553 2012-05-22 13:06:32Z rsiddans $
;-
;===============================================================================
function rd_struct,file1,asc=asc,compress=compress,chk=chk,ff=fofi,old=old,_EXTRA=extra
on_ioerror,skip
	if not keyword_set(old) and not keyword_set(asc) and not keyword_set(chk) then $
		return,rd_struct_fast(file1,_EXTRA=extra)
;
; check file is idl save file, otherwise call old rd_struct code...
;
	file=expand_env(file1)
	if keyword_set(chk) then begin
		ff=rs_findfile(file,count=nfi)
		if nfi eq 0 then begin
			message,/cont,'File not found'
			return,-999
		endif
	endif
	if not keyword_set(old) then begin
		dum=0L
		get_lun,lun
		openr,lun,file
		readu,lun,dum
		close,lun & free_lun,lun
		if dum ne 67129939L then old=1
	endif
	if keyword_set(old) or keyword_set(asc) then begin
		return,rd_struct1(file,asc=asc,compress=compress,chk=chk,ff=fofi)
	endif
	restore,file=file,_EXTRA=extra
	if n_elements(x) eq 0 then message,'File does not contain expected data !'
	return,x
skip:
	return,0
end
