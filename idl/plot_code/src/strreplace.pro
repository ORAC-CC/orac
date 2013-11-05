;===============================================================================
;+
; STRREPLACE
;
; This function replaces occurences of 1 string with another.
;
; PARAMETERS
;	LINES	String or array of strings to work on.
;	OLD	Char or string to replace (may be an array of multiple things to be replace
;		by NEW)
;	NEW	Char or string to replace OLD with. 
;		If undefined NEW is set to an empty string (so OLD is removed)
; KEYWORDS
;	NREP	Returns number of replace operations performed
;	MREP	Set maximum number of replacements/line (other occurences left unreplaced)
;		At least one replacement always done
;	IREP	Obsolete
;
; R.Siddans 22/4/96
; $Id: strreplace.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function strreplace,lines,old,new,nrep=nrep,mrep=mrep,irep=irep
	if n_elements(new) eq 0 then new=''
	no=n_elements(old)
	if no gt 1 then begin
		if n_elements(new) eq no then news=new else news=replicate(new,no)
		res=lines
		nrep=lonarr(no)
		for i=0,no-1 do begin
			res=strreplace(res,old(i),news(i),nrep=nrep1,mrep=mrep)
			nrep(i)=nrep1
		endfor
		return,res
	endif
	nrep=0
	res=lines
	olen=strlen(old)
	nlen=strlen(new)
	for i=0,n_elements(lines)-1 do begin
		tmp=lines(i)
		if olen gt 0 then begin
			len=strlen(tmp)
			pos=strpos(tmp,old)
			while pos ge 0 do begin
				nrep=nrep+1
				tmp=strmid(tmp,0,pos)+new+strmid(tmp,pos+olen,len)
				pos=strpos(tmp,old,pos+nlen)
				if keyword_set(mrep) then if nrep ge mrep then pos=-1
			endwhile
		endif
		res(i)=tmp
	endfor
	return,res
end
