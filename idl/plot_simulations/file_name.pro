;===================================================================================
;+
; FILE_NAME
;
; This breaks up a file name string.
;
; PARAMETERS
;	FILE1	File name
;
; KEYWORDS
;	DIR	Return directory part of file name.
;	DELIM	Return file (without directory or extension) split up as array
;		accoding to specified delimiter (default is '_')
;	EXT	Return extension
;	BASE	Return file (without directory) without extension
;
; R.S. 16/10/96
; $Id: file_name.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===================================================================================
function file_name,file1,dir=dir,delim=delim,ext=ext,base=base

   nfi=n_elements(file1)
   for ifi = 0,nfi-1 do begin
	file=file1(ifi)
        cp=-2
        while cp ne -1 do begin
                lp=cp
                cp=strpos(file,'/',lp+1)
        endwhile
	if lp lt 0 then begin
		dname=''
		fname=file
	endif else begin
		dname=strmid(file,0,lp+1)
		fname=strmid(file,lp+1,strlen(file))
	endelse
	if keyword_set(dir) then begin
		if strlen(dname) eq 0 then ans1='./' $
		else ans1=dname
	endif else begin
        	cp=-2
        	while cp ne -1 do begin
        	        lp=cp
        	        cp=strpos(fname,'.',lp+1)
        	endwhile
		if lp lt 0 then begin
			ename=''
			bname=fname
		endif else begin
			bname=strmid(fname,0,lp)
               	 	ename=strmid(fname,lp+1,strlen(fname))
		endelse
		if keyword_set(ext) then ans1=ename $
		else if keyword_set(base) then ans1=bname $
		else if keyword_set(delim) then begin
			sz=size(delim)
			if sz(sz(0)+1) ne 7 then del='_' else del=delim(0)
			cw=count_words(bname,del=del,/mult)
			snames=strarr(cw)
			for i=0,cw-1 do snames(i)=get_word(i,bname,del=del,/mult)
			ans1=snames
		endif else ans1=fname
	endelse
	if ifi eq 0 then ans=ans1 else ans=[ans,ans1]
   endfor
   return,ans
end
