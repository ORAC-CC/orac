;===========================================================================
;+
;	pro WR_SAV
;
;	Description
; Allow structures to be saved using SAVE/RESTORE functions, but by calling
; as special routines, variable can be reloaded under different names to
; the ones they had originally. Will always store variable as X.
; Path directories are created as necessary.
; Use 'rd_sav.pro' to reload variable.
; New wr_structure code using SAVE/RESTORE
; (Obsolete keyword OLD force to use old style (.str))
;
;	Parameters
;  I  FILENAME filename to save as (.sav added) (.str removed if not /old)
;	 I  X structure/variable to be saved
;
;	Keywords 
;  I  COMPRESS Compress file (internal IDL gzip) (default=1 for .sav files)
;  _EXTRA passed to FILE_CHMOD to change file permissions
;        (also applies to created directory - but only last level!!)
;
;	Date
;	B. Latter : 14th June 2001
; B. Latter : 7th Nov 2005 + Set group access (/g_write etc)
;-
;===========================================================================
pro wr_sav,filename,x,compress=compress,_extra=extra

sav_file_suffix='.sav' ; standard suffix to use (must match in rd_sav.pro)

file=fname(filename,a=path,b=suf)
if suf eq '.str' then begin
	wr_struct,filename,x,compress=compress,_extra=extra  ; use R.Siddans code
	return
endif

; Ensure output filename finishes with '.sav'
if suf ne sav_file_suffix then file=file+suf ; re-add the identified suffix
ofile=path+file+sav_file_suffix

; Change directory permission to match that of file
; Primarily to enable group write (g_write) permission
nt=n_elements(extra)
if nt gt 0 then begin
	tags=tag_names(extra)
	wh=where(strmid(tags,1,1) eq '_',nwh)
	if nwh gt 0 then chmod=1
endif

; Check if path exists, if not create
if path ne '' then begin
	if !version.release gt 5.4 then begin
		path=expand_env(path)
		if not (file_test(path,/dir)) then begin
			file_mkdir,path
			if keyword_set(chmod) then file_chmod,path,_extra=extra[wh]
		endif
	endif else begin ; for older versions (slower)
		spawn,'mkdir -p '+path
		if keyword_set(chmod) then file_chmod,path,_extra=extra[wh]
	endelse
endif else begin
; Check have write permission - not done yet...
endelse

; Save the data structure
if n_elements(compress) eq 0 then comp=1 else comp=compress
save,x,filename=ofile,compress=comp,_extra=extra

; Modify access levels if specified
if keyword_set(chmod) then file_chmod,ofile,_extra=extra
end
