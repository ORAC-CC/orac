;===============================================================================
;+
; RD_STRUCT_FAST
;
; Read "save" format structure file without any checking
;
; PARAMETERS
;	FILE	Name of file
;
; KEYWORDS
;	CHECK	Check if file exists
;
; R.S. 29/07/99
; $Id: rd_struct_fast.pro 1554 2012-05-22 13:06:50Z rsiddans $
;-
;===============================================================================
function rd_struct_fast,file,check=check,_EXTRA=extra
	on_ioerror,badfile
	restore,file=file,_EXTRA=extra
	return,x
badfile:
	if keyword_set(check) then message,'File does not exist: '+file
	return,0
end
