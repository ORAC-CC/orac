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
;
; R.S. 29/07/99
;-
;===============================================================================
function rd_struct_fast,file
	on_ioerror,badfile
	restore,file=file
	return,x
badfile:
	return,0
end
