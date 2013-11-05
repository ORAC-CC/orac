;+
;this function returns the Nth word from a given string which is delimited by
; a particular character. Multiple consequecutive delimiters can optionally be
; ignored or not. More than 1 delimiter can be specified.
function get_word_old,N,strs,delimiter=delims,multiple=mult
;-

    rstrs=''
	
    for istr=0,n_elements(strs)-1 do begin
	string=strs(istr)

	if not keyword_set(delims) then delims=[' ','	'] ; i.e. space,tab
	delim=delims(0)
	for i=1,n_elements(delims)-1 do string=strreplace(string,delims(i),delim)
	if keyword_set(mult) then begin
		mult=1
	endif else begin
		mult=0
	endelse

	word_no=0
	pos=0
	length=strlen(string)

; skip beginning delimeters...
	if mult eq 1 then $
		while strmid(string,pos,1) eq delim do pos=pos+1   
; find Nth word
	prev=pos-1
	if N ne 0 then begin
		while word_no lt N and pos lt length do begin
			pos=strpos(string,delim,pos+1)
			if mult eq 1 then begin
				if pos ne prev+1 then word_no=word_no+1
				prev=pos
			endif else begin
				word_no=word_no+1
			endelse
		endwhile
		pos=pos+1
	endif

; skip more delimiters        
	if mult eq 1 then $
		while strmid(string,pos,1) eq delim do pos=pos+1   

; find end of word
	pos2=strpos(string,delim,pos)
	if pos2 lt 1 then pos2=length

; return Nth word
	rstrs=[rstrs,strmid(string,pos,pos2-pos)]
    endfor
    rstrs=rstrs(1:n_elements(rstrs)-1)
    if n_elements(rstrs) eq 1 then rstrs=rstrs(0)
    return,rstrs
end

