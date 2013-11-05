;===============================================================================
;+
; STR_SUB_FILE
;
; This takes as input a string and returns another string after substituting 
; the contents of a file and / or environment variables.
;
; "<",">" in the file are taken to delimit file names which will be replaced 
; by the first line of the referenced file.
;
; Also allowed is specification of form
; <FILE|N>
; the N after the '|' specifies the line within FILE to be used (0 based index)
; RS_GET_LINE used to read lines, so comment lines can be ignore
;
; PARAMETERS
;	STR	String (array if necessary)
;
; KEYWORDS
;	DEL	Specify to limit array of file name delimiters as alternatives
;		to ['<','>']
;	EXTRA	Sent to RS_GET_LINE
;	
;
; R.S. 24/08/01
; $Id: str_sub_file.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function str_sub_file,str,del=del1,_EXTRA=extra
	str2=str
	if n_elements(del1) eq 2 then del=del1 else del=['<','>']
	line=''
	for i=0,n_elements(str)-1 do begin
		s1=expand_env(str(i))
		bra=strposmul(s1,del(0),count=nb)
		lde=strposmul(s1,'|',count=nk1)
		ket=strposmul(s1,del(1),count=nk)
		if nb ne nk then message,'Unbalanced file delimiters'
		if nb gt 0 then begin
			get_lun,lun
			s2=strmid(s1,0,bra(0))
			for j=0,nb-1 do begin
				wh=(where(lde gt bra(j) and lde lt ket(j)))(0)
				if wh ne -1 then begin
					fi=strmid(s1,bra(j)+1,lde(wh)-bra(j)-1)
					fili=strmid(s1,lde(wh)+1,ket(j)-lde(wh)-1)
					nl=long(fili)
				endif else begin
					fi=strmid(s1,bra(j)+1,ket(j)-bra(j)-1)
					nl=0
				endelse
				openr,lun,fi
				for il=0,nl do rs_get_line,lun,line,_EXTRA=extra
				close,lun 
				if j ne nb-1 then fp=bra(j+1) else fp=strlen(s1)
				s2=s2+strtrim(line,2)+strmid(s1,ket(j)+1,fp-ket(j)-1)
			endfor
			free_lun,lun
			str2(i)=s2
		endif else str2(i)=s1
	endfor
	return,str2
end
