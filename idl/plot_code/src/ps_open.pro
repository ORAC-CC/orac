;===============================================================================
;+
; PS_OPEN
;
; This sets up PS plotting with palatino font and in color
;
; PARAMETERS
;	FILE1	Name of PS file (default ios IDL.PS)
;	PAGE	Name of variable containing page number. PS file will
;		be FILE_PAGE.ps. PAGE will be incremented by 1 on retrun.
;
; KEYWORDS
;	BW	Set for black and white printer. Filename will have
;		bw added to it.
;	EPSF	Simply modifies output filename, for code to work,
;		SET_A4 must be called with epsf parameter
;	NN	Don't modify specified filename at all
;	MKDIR	Make directory implied by FILE if it doesn't exist
;		
; R.S. 24/11/95
; $Id: ps_open.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
pro ps_open,file1,page,bw=bw,epsf=epsf,nn=nn,mkdir=mkdir
	if keyword_set(mkdir) then file_mkdir,file_name(file1,/dir)
	if keyword_set(epsf) then ext='.eps' else ext='.ps'
	if !d.name eq 'PS' then device,/close else set_plot,'ps'
device,bits=8
if keyword_set(epsf) then device,/encapsulated
	if not keyword_set(bw) then device,color=1 else device,color=0
	!p.font=0
	device,/palatino
	if n_elements(file1) eq 0 then file='idl.ps' else file=file1
	if file eq new_name(file) then file=file+ext
	if keyword_set(bw) then file=new_name(file)+'_bw'+ext
	if n_elements(page) gt 0 then begin
		file=new_name(file)+'_'+trim_zero(page)+ext
		page=page+1
	endif
	file=expand_env(file)
	if keyword_set(nn) then file=file1
	message,/cont,'Postscript file: '+file
	device,file=file
end
