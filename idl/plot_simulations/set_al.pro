;===============================================================================
;+
; SET_AL
;
; This routine basically calls set_al2, but initializes the environment
; variable !AL if necessary first.
;
; KEYWORDS
;	MAX	Set to maximum number of lines per plot
;
; 26/8/96
;-
;===============================================================================
pro set_al,_EXTRA=extra,max=max
	if not keyword_set(max) then max=100
	defsysv,'!al',exists=exists
	ind=intarr(max)
	if not exists then defsysv,'!AL',$
		{al,linestyle:ind,color:ind,thick:ind,psym:ind,iline:0}
	set_al2,_EXTRA=extra
end
