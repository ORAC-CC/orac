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
;	
;
; R.S. 29/07/99
;-
;===============================================================================
pro wr_struct,file0,x,asc=asc,term=term,compress=compress,quiet=quiet
	if n_elements(x) eq 0 then begin
		message,'WARNING: X undefined',/info
		return
	endif
	file=expand_env(file0)
	if keyword_set(asc) or keyword_set(term) then begin
		wr_struct1,file,x,asc=asc,term=term,compress=compress
		return
	endif
	save,x,file=file,compress=compress
	if not keyword_set(quiet) then message,/info,'Written file: '+file
end
