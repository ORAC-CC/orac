;===============================================================================
;+
; STRPOSMUL
;
; Return array of all positions of given character in string
;
; PARAMETERS
;	STR	String
;	SS	Search string
;
; KEYWORDS
;	
;
; R.S. 24/08/01
;-
;===============================================================================
function strposmul,str,ss,count=np
	np=0
	sp=strpos(str,ss)
	sl=strlen(str)
	while sp ne -1 and sp lt sl do begin
		np=np+1
		if n_elements(sps) eq 0 then sps=sp else sps=[sps,sp]
		sp=strpos(str,ss,sp+1)
	endwhile
	if n_elements(sps) eq 0 then sps=-1
	return,sps
end
