;===============================================================================
;+
; RD_SIZE
;
; This reads the output of an IDL size command from a binary file
; (i.e. a long array of unknown length). e.g. if a file was
; written using: 
;	writeu,lun,size(x)
; The command:
;	rd_size,lun
; will read in the size of the array x.
;
; PARAMETERS
;	LUN	Unit from which to read
;
; KEYWORDS
;	MBA	Set to definately return the size of an array,
;		even if it's only has one element. This
;		ensures output of this routine can be used
;		with mk_array.
;	ASC	Set to read from an ascii file.
;	
;
; R.S. 25/07/97
; $Id: rd_size.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function rd_size,lun,mba=mba,asc=asc
	nd=0L
	if keyword_set(asc) then begin
		line=''
		readf,lun,line
		reads,line,nd
		cw=count_words(line,/mul)
		if nd+3 gt cw then begin
			sz1=lonarr(cw-1)
			reads,line,nd,sz1
			sz2=lonarr(nd+3-cw)
			readf,lun,sz2
			sz=[sz1,sz2]
		endif else begin
			sz=lonarr(nd+2)
			reads,line,nd,sz
		endelse
	endif else begin
		readu,lun,nd
		sz=lonarr(nd+2)
		readu,lun,sz
	endelse
	if keyword_set(mba) and nd eq 0 then return,[1,1,sz] else $
	return,[nd,sz]
end
