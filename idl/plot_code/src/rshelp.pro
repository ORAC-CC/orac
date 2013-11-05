;===============================================================================
;+
; RSHELP.PRO
;
; This routines calls DOC_LIBRARY to give manual pages for my files.
;
; PARAMETERS
;	NAME	A string giving the name of the routine.
;
; R.Siddans 29/1/95
; $Id: rshelp.pro 414 2010-09-14 13:56:52Z rsiddans $
;-
;===============================================================================
pro rshelp,name
	doc_library,name
end

