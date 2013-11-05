;+
; This procedure gets a line of data from an ASCII formatted file.
; lines with commts can optionally be ignored by specifying a vector of
; strings. Lines begining with these strings will then be ignored.
;
; KEYWORDS
;	FREP	If first character of any line is '<', then
;		remainder of line is assumed to be a file name. The first
;		line of this file is returned instead of the line beginning
;		'<'
;       CSEL    Specify columns of ascii text to be selected in each line
;               e.g. '1 JAN 1 2' cannot be read as array because of text.
;               specify csel=[[0,1],[5,-1]] would skip the columns containing 'JAN'
;               and allow rest of data to be read. 2-D array for
;               multiple sets of columns to skip. Negative number indicates all
;		of remainder of line to be selected.
;               NOT COMPATIBLE WITH NLINES
;
;	SSF	run STR_SUB_FILE on the line
;	EXT 	Specify string list of functions to be applied to the line.
;	KEXT 	_EXTRA structure for each element of EXT
;-
pro rs_get_line,lun,line,ignore=ignore,ccom=ccom,noblank=nob,trim=trim,$
	comm=comm,expand_env=eenv,frep=frep,csel=csel,ssf=ssf,ext=ext,kext=kext

	line=' '
	comm1='XXX'
	if not keyword_set(ccom) then begin
		if not keyword_set(ignore) then begin
			readf,lun,line
			if keyword_set(csel) then line=str_csel(line,csel)
			return
		endif
;
; ignores line beginning with given chars
;
		ign=1
		while ign eq 1 and not eof(lun) do begin
			readf,lun,line
			if keyword_set(trim) then line=strtrim(line,2)
			num=n_elements(ignore)-1
			ign=0
			for i=0,num do begin
				if strpos(line,ignore(i)) eq 0 then begin
					ign=1
					comm1=[comm1,strmid(line,strlen(ignore(i)),$
						strlen(line))]
				endif
			endfor
			if ign eq 0 and keyword_set(nob) then $
				if strlen(strtrim(line,2)) eq 0 then ign=1
			if ign eq 0 and keyword_set(csel) then line=str_csel(line,csel)
		endwhile
	endif else begin
;
; ignores chars within /* to */ (i.e. C comments)
;
		ign=1
		text=2
		while ign eq 1 and not eof(lun) do begin
			readf,lun,line
			if text eq 2 and strpos(line,'/*') eq -1 then begin
				line2=line
				ok=1
				sln=1
			endif else begin
				line2=''
				ok=0
				sln=1
				for i=0,strlen(line)-1 do begin
					if text eq 1 then text=2 else begin
						c2=strmid(line,i,2)
						if c2 eq '/*' then text=0
						if c2 eq '*/' then text=1
						if text eq 2 then begin
							line2=line2+strmid(line,i,1)
							ok=1
						endif
					endelse
				endfor
			endelse
			if keyword_set(nob) then $
				if strlen(strtrim(line,2)) eq 0 then sln=0
			if ok eq 1 and sln eq 1 then ign=0
		endwhile
	endelse
	if keyword_set(trim) then line=strtrim(line,2)
	if n_elements(comm1) gt 1 then comm=comm1(1:n_elements(comm1)-1)
	if keyword_set(eenv) then line=expand_env(line)
	if keyword_set(frep) then begin
		if strmid(line,0,1) eq '<' then begin
			rfile=rs_findfile(strmid(line,1,strlen(line)),count=nfi)
			if nfi ne 1 then begin
				message,/cont,'Unknown or ambiguous file: '+line
				return
			endif
			get_lun,rfl
			openr,rfl,rfile(0)
			rs_get_line,rfl,line,ignore=ignore,expand_env=eenv,csel=csel
			close,rfl
			free_lun,rfl
		endif
	endif
	if n_elements(ext) ne 0 then begin
		for i=0,n_elements(ext)-1 do line=call_function(iext(i),line,_extra=kext(i))
	endif
	if keyword_set(ssf) then line=str_sub_file(line)
end
