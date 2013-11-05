;===================================================================================
;+
; RS_FINDFILE
;
; This works exactly like findfile except that '~' is replaced by $HOME before
; being passed to the IDL library routine.
;
; PARAMETERS
; 	FILE 	String to search for. Vector searches for matches of
;		any of the strings provided.
;
; KEYWORDS
;	TCBO1	Set if 'there can be only one' match.
;	AL1	Set to exit unless 'At Least 1' file found
;	DIR	Set to find directory names 
;	RSH	Run 'ls' command on other machine
; $Id: rs_findfile.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===================================================================================
function rs_findfile,count=count,file,_EXTRA=extra,tcbo1=tcbo1,al1=al1,dir=dir,rsh=rsh
	if keyword_set(rsh) or keyword_set(dir) then begin
		tfi='tmp_rs_findfile.txt'
		if keyword_set(dir) then begin
			cmd='ls -1d '+file
		endif else begin
			cmd='ls -1 '+file
		endelse
		if keyword_set(rsh) then cmd='rsh '+rsh+" '"+cmd+"'"
		cmd=cmd+' > '+tfi
		spawn,cmd
		rsread,tfi,res,/str
		spawn,'rm -f ' +tfi
		if strlen(res(0)) eq 0 then count=0 else count=n_elements(res)
		return,res
	endif
	if n_elements(file) eq 0 then message,'FILE not defined !'
	if strlen(strtrim(file(0),2)) eq 0 then message,'FILE not defined !'
	for i=0,n_elements(file)-1 do begin
		file2=strreplace(file(i),'~','$HOME')
		res1=findfile(file2,_EXTRA=extra,count=count1)
		if i eq 0 then begin
			res=res1
			count=count1
		endif else if count1 gt 0 then begin
			if count eq 0 then begin
				res=res1
				count=count1
			endif else begin
				res=[res,res1]
				count=count+count1
			endelse
		endif
	endfor
	if (keyword_set(al1) or keyword_set(tcbo1)) and count eq 0 then $
		message,'No such file: '+file
	if keyword_set(tcbo1) then begin
		if count gt 1 then begin
			for i=0,count-1 do message,/cont,res(i)
			message,'Ambiguous file: '+file
		endif
		res=res(0)
	endif
	return,res
end
