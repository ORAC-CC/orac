;===============================================================================
;+
; COUNT_WORDS.PRO
;
; this function counts the number of wordsin given string which is delimited by
; a particular character. Multiple consequecutive delimiters can optionally be
; ignored or not.
;
; PARAMETERS
;	STRING1	string to be counted
;
; KEYWORDS
;	STST	Return start position of all words
;	DELIMITER	Set char which delimits words
;       MULTIPLE	Set to treat multiple delimiters as 1
;
; R.S. 1/1/97
;-
;===============================================================================

function count_words,string1,delimiter=delim,multiple=mult,stst=stst

	string=string1
	
	if not keyword_set(delim) then delim=' '
	if keyword_set(mult) then begin
		mult=1
	endif else begin
		mult=0
	endelse

	if delim eq ' ' then string=strcompress(string)
; above line takes care of tabs (compressed to single space).

	word_no=0
	pos=0
	length=strlen(string)

; skip beginning delimeters...
	if mult eq 1 then $
		while strmid(string,pos,1) eq delim do pos=pos+1  $
	else pos=strpos(string,delim)
; find number of words
	while pos ge 0 do begin
		if mult eq 1 then $
			while strmid(string,pos,1) eq delim do pos=pos+1
		if pos lt length then begin
			if n_elements(poss) eq 0 then poss=pos $
			else poss=[poss,pos]
			word_no=word_no+1
		endif
		pos=strpos(string,delim,pos+1)
	endwhile

; return number of words
	if n_elements(poss) eq 0 then poss=pos
	stst=poss
	return,word_no
end

