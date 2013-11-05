;===============================================================================
;+
; RS_READ
;
; Output statement with file/terminal/ascii options specified as
; keywords. Assumes SIZE of variable to be read is written first.
;
; PARAMETERS
;	LUN	Unit number
;
; KEYWORDS
;	ASC read from ascii file	
;	SZO	Set if only size is to be read (data output by
;		RS_WRITE,/SZO)
;       MKC	-
;       CEL	-
;       IC	-
;
; R.S. 29/07/99
; $Id: rs_read.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function rs_read,lun,asc=asc,szo=szo,mkc=mkc,cel=cel,ic=ic
	sz=rd_size(lun,asc=asc)
	if sz(sz(0)+1) eq 8 then begin
		nel=sz(sz(0)+2)
		for i=0,nel-1 do begin
			s=rd_struct1(lun,asc=asc,mkc=(keyword_set(mkc) and i eq 0),cel=cel,ic=ic)
			if i eq 0 then v=s else v=[v,s]
		endfor
		return,v
	endif
	if keyword_set(szo) then begin
		v=sz
		if not keyword_set(asc) and not keyword_set(term) and $
			sz(sz(0)+1) eq 7 then begin
;
;  append string lengths to size array in this case...
;
			sl=lonarr(sz(sz(0)+2))
			readu,lun,sl
			v=[v,sl]
		endif
		return,v
	endif
	if sz(0) ne 0 then v=make_array(size=sz) $
	else begin
		if sz(1) eq 1 then v=0B $
		else if sz(1) eq 2 then v=0 $
		else if sz(1) eq 3 then v=0L $
		else if sz(1) eq 4 then v=0. $
		else if sz(1) eq 5 then v=0.d $
		else if sz(1) eq 7 then v='' $
		else message,'Uknown data type !'
	endelse
	if keyword_set(asc) then begin
		readf,lun,v
	endif else begin
		if sz(sz(0)+1) eq 7 then begin
;
; read all string lengths...
;
			slsz=sz
			slsz(sz(0)+1)=3
			if slsz(0) eq 0 then sl=0L else $
				sl=make_array(size=slsz)
			readu,lun,sl
			for i=0,max(sl)-1 do begin
				wh=where(sl gt i)
				if wh(0) ne -1 then v(wh)=v(wh)+' '
			endfor
;
; read all strings...
;
		endif
		readu,lun,v
	endelse
	return,v
end
