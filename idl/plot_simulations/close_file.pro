;===============================================================================
;+
; CLOSE_FILE
;
; Close a LUN, free it and report status
;
; PARAMETERS
;	LUN	Unit number
;
; KEYWORDS
;	NOFREE	Do not free the LUN, just close/report
;	QUIET	Do not print messages
;
; R.S. 02/07/01
;-
;===============================================================================
pro close_file,lun,nofree=nofree,quiet=quiet
	for i=0,n_elements(lun)-1 do begin
		f=fstat(lun(i))
		if f.write eq 1 then msg='Written: '+f.name $
		else msg='Read: '+f.name
		if f.open eq 1 then begin
			close,lun(i)
			if not keyword_set(quiet) then message,/info,msg
		endif
		if not keyword_set(nofree) then free_lun,lun(i)
	endfor
end
