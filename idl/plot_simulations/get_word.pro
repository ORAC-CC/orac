;+
;this function returns the Nth word from a given string which is delimited by
; a particular character. Multiple consequecutive delimiters can optionally be
; ignored or not. More than 1 delimiter can be specified.
function get_word,N,strs,delimiter=delims,multiple=mult,old=old
;-

    
    if n lt 0 then old=1
    if n_elements(delims) gt 1 then old=1
    if keyword_set(old) then begin
	return,get_word_old(N,strs,delimiter=delims,multiple=mult)
    endif
	
    rstrs=''
    if n_elements(delims) eq 0 then begin
	delims=' '
	rtab=1
    endif else rtab=0
    for istr=0,n_elements(strs)-1 do begin
	strng=strs(istr)
;
; replace tab with space...
;
	if rtab then begin
		b=byte(strng)
		wh=where(b eq 9)
		if wh(0) ne -1 then begin
			b(wh)=32
			strng=string(b)
		endif
	endif

	ss=(strsplit(strng,delims,/extract))
	nw=n_elements(ss)
	if n gt nw-1 then ss=ss(n mod nw) else ss=ss(n)
	rstrs=[rstrs,ss]
    endfor
    rstrs=rstrs(1:n_elements(rstrs)-1)
    if n_elements(rstrs) eq 1 then rstrs=rstrs(0)
    return,rstrs
end

