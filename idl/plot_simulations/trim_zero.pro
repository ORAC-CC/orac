;===============================================================================
;+
; TRIM_ZERO
; 
; This function removes all zeroes after a '.'in given string(s), together with
; all leading and trailing spaces.
;
; PARAMETERS
;	STR0	string(s)
;
; KEYWORDS
;	PP	Also replace decimal point by 'p'
;	UPP	Also replace decimal point by 'P'
;
; R.S.  1/1/2000
; $Id: trim_zero.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function trim_zero,str0,pp=pp,upp=upp
	str=strtrim(str0,2)
	sz=size(str1)
	for i=0,n_elements(str)-1 do begin
		stmp=str(i)
		if strpos(stmp,'.') ge 0 then begin
;               
; curtail temporary string at position of 'e+' or 'e-' in order to
; trim zeroes in exponentials.
;
			xp=max([strpos(stmp,'e+'),strpos(stmp,'e-')])
			if xp gt 0 then begin
				stail=strmid(stmp,xp,strlen(stmp))
				stmp=strmid(stmp,0,xp-1)
			endif else stail=''
			sl=strlen(stmp)-1
			ep=strmid(stmp,sl,1)
			while ep eq '0' and sl gt 1 do begin
				stmp=strmid(stmp,0,sl)
				sl=sl-1
				ep=strmid(stmp,sl,1)
			endwhile
			if ep eq '.' then stmp=strmid(stmp,0,sl)
			str(i)=stmp+stail
		endif
	endfor
	if keyword_set(pp) then str=strreplace(str,'.','p')
	if keyword_set(upp) then str=strreplace(str,'.','P')
	return,str
end
