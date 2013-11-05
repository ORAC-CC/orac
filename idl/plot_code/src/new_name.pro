;===============================================================================
;+
; NEW_NAME.PRO
;
; This function returns a given filename with its extension replaced
; by the given extension. If the original name has no extension, then the
; new extension is simly added. This program works by finding the last '.'
; in the file name so it should work under VMS, Unix or DOS.
;
; PARAMETERS
;	FINS	The original filename
;       EXT	The new file extension. If EXT is not defined then the original extension (including '.') is removed.
;
; R.Siddans 15/12/94
; $Id: new_name.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================

function new_name,fins,ext
	
;
; add leading full stop from extension
;
    if n_elements(ext) eq 0 then ext='' else $
    if strpos(ext,".") ne 0 then ext='.'+ext

    rfin=''
    for ifin=0,n_elements(fins)-1 do begin
	fin=fins(ifin)
;
; find last full stop
;
	pos=0
	ln=strlen(fin)
	while pos ne -1 do begin
		lpos=pos
		pos=strpos(fin,".",pos+1)
	endwhile
	tfin=strmid(fin,0,lpos)
	if strlen(tfin) eq 0 then tfin=fin
	rfin=[rfin,tfin+ext]
    endfor
    rfin=rfin(1:n_elements(rfin)-1)
    if n_elements(rfin) eq 1 then rfin=rfin(0)
    return,rfin
end
