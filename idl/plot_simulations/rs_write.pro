;===============================================================================
;+
; RS_WRITE
;
; Output statement with file/terminal/ascii options specified as
; keywords
;
; PARAMETERS
;	LUN	Unit number
;	V	Variable
;
; KEYWORDS
;	ASC	Write in ASCII format
;	TERM	Write to terminal
;	SZO	Only write size of data
;	D2F	Convert double to float
;
; R.S. 29/07/99
;-
;===============================================================================
pro rs_write,lun,v,asc=asc,term=term,szo=szo,d2f=d2f
	sz=size(v)
	if keyword_set(d2f) then begin
		if sz(sz(0)+1) eq 5 then v=float(v)
		sz=size(v)
	endif
	if keyword_set(term) then begin
		print,sz
		if not keyword_set(szo) then begin
			if sz(sz(0)+1) eq 7 then for i=0,n_elements(v)-1 do print,v([i]) $
			else print,v 
		endif
	endif else if keyword_set(asc) then begin
		printf,lun,sz
		if not keyword_set(szo) then begin
			if sz(sz(0)+1) eq 7 then for i=0,n_elements(v)-1 do printf,lun,v([i]) $
			else printf,lun,v 
		endif
	endif else begin
;
; write string length before a string
;
		sz=size(v)
		writeu,lun,size(v)
		if sz(sz(0)+1) eq 7 then writeu,lun,strlen(v)
		if not keyword_set(szo) then writeu,lun,v
	endelse
end
