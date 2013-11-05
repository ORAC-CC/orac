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
;	ASC		Set to write an ascii file
;	TERM		Set to write to terminal
;	COMPRESS	Set to write compressed file
;	QUIET		Supress message
;	MKD		Make directory if it doesn't already exist
;	GW		Set file group write
;	
;
; R.S. 29/07/99
; $Id: wr_struct.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
pro wr_struct,file0,x,asc=asc,term=term,compress=compress,quiet=quiet,mkd=mkd,gw=gw
	if n_elements(x) eq 0 then begin
		message,'WARNING: X undefined',/info
		return
	endif
	if n_elements(compress) eq 0 then compress=1
	file=expand_env(file0)
	if keyword_set(mkd) then file_mkdir,file_name(file,/dir)
;	if keyword_set(asc) or keyword_set(term) then begin
;		wr_struct1,file,x,asc=asc,term=term,compress=compress
;		return
;	endif
	save,x,file=file,compress=compress
	if keyword_set(gw) then file_chmod,file,/g_write
	if not keyword_set(quiet) then print,'Written file: '+file
end
