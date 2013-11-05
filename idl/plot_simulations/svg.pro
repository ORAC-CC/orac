;===============================================================================
;+
; SVG
;
; Save current window as /ftp/pub/rs_idl.gif 
;
; PARAMETERS
;	
;
; KEYWORDS
;	
;
; R.S. 24/04/99
; $Id: svg.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
pro svg
	ofi='/ftp/pub/rs_idl.gif'
;	ofi='$RS_HOME//rs_idl.gif'
	svgif,ofi
	message,/cont,'Window -> '+ofi
end
