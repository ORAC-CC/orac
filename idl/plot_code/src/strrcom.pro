;====================================================================================
;+
; STRRCOM
;
; This function returns the given string(s) with all formatting ("!") commands
; removed.
;
; PARAMETERS
;	S	String or string array
;
; R.S. 1/9/95
;-
;====================================================================================
function strrcom,s
	sz=size(s)
	if n_elements(s) eq 1 then begin
		s1=strarr(1)
		s1(0)=s
	endif else s1=s
	commands='!'+[string(indgen(18)+3,format='(I0)'),$
		'A','B','C','D','E','I','L','N',$
		'R','S','U','S','G','W','X','!']
	nc=n_elements(commands)
	ns=n_elements(s1)

	for i=0,ns-1 do begin
		st=s1(i)
		st2=''
		lst=strlen(st)
		j=0
		while j lt lst do begin
			char=strmid(st,j,1)
			if char eq '!' then begin
				match=-1
				for k=0,nc-1 do begin
					cs=commands(k)
					lcs=strlen(cs)
					if cs eq strupcase(strmid(st,j,lcs)) then $
						match=k
				endfor
				if match ge 0 then begin
					j=j+strlen(commands(match))-1
					if commands(match) eq '!!' then st2=st2+'!'
				endif else st2=st2+'!'
			endif else st2=st2+char
      			j=j+1
		endwhile
		s1(i)=st2
	endfor
	if sz(0) eq 0 then s1=s1(0)
	return,s1
end
