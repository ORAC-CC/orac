;===============================================================================
;+
; READU_F77
;
; Read record from a fortran formatted binary file, but without needing
; to have opened it with /F77 set
;
; PARAMETERS
;	LUN	unit number
;	D	Array of correct size to hold the current record
;		(will be filled on return if all goes well)
;
; KEYWORDS
;	
;
; R.S. 16/06/11
; $Id$
;-
;===============================================================================
pro readu_f77,lun,d
	sz=size(d)
	len1=0l
	len2=0l
	it=sz(sz(0)+1)
	if it eq 7 then begin
;
; deal with special case of strings
;
		ns=n_elements(d)
		if ns gt 1 then begin
			for is=0l,ns-1 do begin
				d1=''
				readu_f77,lun,d1
				d(is)=d1
			endfor
			return
		endif
		readu,lun,len1
		b=bytarr(len1)
		readu,lun,b
		d=string(b)
	endif else if it gt 5 then begin
		message,'Cannot read data type: '+trim_zero(sz(sz(0)+1))
	endif else begin
		readu,lun,len1

		nd=n_elements(d)*sizeof(d)
;                print,'nd',n_elements(d)
;                print,'sizeof',sizeof(d)
;                print,'len',trim_zero(len1)
		if len1 ne nd then message,'Bad rec len: '+trim_zero(len1)+' vs '+trim_zero(nd)
		readu,lun,d
	endelse
	readu,lun,len2
	if len1 ne len2 then message,'Error reading record!'
end
