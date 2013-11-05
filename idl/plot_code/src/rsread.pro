;===============================================================================
;+
; RSREAD.PRO
;
; this procedure reads the contents of a file into a floating point array
; 
; KEYWORDS
;	IGNORE=
;	specifies a vector of strings. Lines beginning with any of these
;	will be ignored.
;
;	DELIM=
;	Specify the delimiter which separates the columns
;
;	MAX=
;	specify maximum number of lines in file.
;
;	LINES=
;	specify number of lines of data in file.
;	Its much quicker to do this if no ignore strings are specified,
;	otherwise it makes no difference.
;
;	DOUBLE
;	Set this to read into a double precision array
;
;	STR	Set to read lines in file into string array
;	HL	Specify number of header lines to ignore
;       CSEL    Specify columns of ascii text to be selected in each line
;               e.g. '1 JAN 1 2' cannot be read as array because of text.
;               specify csel=[[0,1],[5,-1]] would skip the columns containing 'JAN'
;               and allow rest of data to be read. 2-D array for
;               multiple sets of columns to skip. Negative number indicates all
;               of remainder of line to be selected.
;               NOT COMPATIBLE WITH NLINES
;
;
; R.Siddans 23/11/94
; $Id: rsread.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================

pro rsread,file,data,ignore=ignore,delim=delim,max=max,lines=nlines,$
	double=dbl,comm=comm,str=str,hl=hl,csel=csel

	if not keyword_set(ignore) then begin
		ignore=-1
		quick=1
	endif else quick=0
	if not keyword_set(max) then max=long(10000)

; open file
	get_lun,lun
	openr,lun,file
; skip header
	line=''
	if n_elements(hl) ne 0 then begin
		comm=strarr(hl)
		for i=0,hl-1 do begin
			readf,lun,line
			comm(i)=line
		endfor
		line=''
	endif
; find first non blank line
	while strlen(line) lt 1 and not eof(lun) do begin
		stat=fstat(lun)
		start=stat.cur_ptr
		readf,lun,line
	endwhile
	if strlen(line) eq 0 then begin
		if keyword_set(str) then data=line $
		else data=-999.
		close,lun & free_lun,lun
		message,/cont,'Warning: empty file detected !'
		return
	endif
; position to first non-blank line.
	point_lun,lun,start
;
; read file as strings if required...
;
	if keyword_set(str) then begin
		data=strarr(max)
		nl=0L
		while not eof(lun) do begin
			rs_get_line,lun,line,ignore=ignore,comm=comm,csel=csel
			if strlen(strtrim(line,2)) gt 0 then begin
				data(nl)=line
				nl=nl+1
			endif
		endwhile
		if nl gt 0 then data=data(0:nl-1) else $
			data=''
		close,lun & free_lun,lun
		return
	endif
	
; read first non blank line and count number of columns
	rs_get_line,lun,line,ignore=ignore,comm=comm,csel=csel
	ncols=count_words(line,/multi)
;	print,'Number of columns of data: ',ncols

; create array and read in data
	if keyword_set(nlines) and quick then begin
		if keyword_set(dbl) then $
			data=dblarr(ncols,nlines) $
		else $
			data=fltarr(ncols,nlines)
		point_lun,lun,start
		readf,lun,data
	endif else begin
		if keyword_set(dbl) then begin
			data=dblarr(ncols,max)
			temp=dblarr(ncols)
		endif else begin
			data=fltarr(ncols,max)
			temp=fltarr(ncols)
		endelse
		nlines=0L
		on_ioerror,terminate
		while not eof(lun) do begin
			reads,line,temp
			data(*,nlines)=temp
			comm1='XXX'
			rs_get_line,lun,line,ignore=ignore,comm=comm1,csel=csel
			if comm1(0) ne 'XXX' then begin
				if n_elements(comm) eq 0 then comm=comm1 $
				else comm=[comm,comm1]
			endif
			nlines=nlines+1    
		endwhile
		if strlen(strtrim(line,2)) ne 0 then begin
			reads,line,temp
			data(*,nlines)=temp
                        nlines=nlines+1
		endif
terminate:
		data=data(*,0:nlines-1)
	endelse
;	print,'Number of lines of data  :',nlines
; close file
	close,lun
	free_lun,lun
end

