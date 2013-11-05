;===============================================================================
;+
; EXPAND_ENV
;
; This expands all the environment variables in the given string
;
; PARAMETERS
;	STRING2	String to be expanded
;
; KEYWORDS
;	
;
; R.S. 07/08/97
; $Id: expand_env.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function expand_env,string2
	string=string2
	pd=strpos(string,'$')
	while pd ge 0 do begin
		var=''
		if strmid(string,pd+1,1) eq '{' then begin
			p2=strpos(string,'}',pd)
			if p2 lt 0 then begin
				message,/cont,'Unmatched "{" in string.'
				goto,skip
			endif
			var=strmid(string,pd+2,p2-pd-2)
		endif else begin
			i=1
			sl=strlen(string)-pd
			ch=strmid(string,pd+i,1)
			while i lt sl and ch ne ' ' and ch ne '}' and $
				ch ne '/' and ch ne '.' and ch ne '$' do begin
				var=var+ch
				i=i+1
				ch=strmid(string,pd+i,1)
			endwhile
			p2=pd+i-1
		endelse
		evar=getenv(var)
		string=strmid(string,0,pd)+evar+strmid(string,p2+1,strlen(string))
		pd=pd-1
skip:
		pd=strpos(string,'$',pd+1)
	endwhile
	return,string
end
