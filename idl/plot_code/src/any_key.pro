;===============================================================================
;+
; ANY_KEY.PRO
;
; This procedure waits for a key press id the display device is 'X' and 
; does nothing anyway.
; PARAMETERS
;
; KEYWORDS
;	SAY	Set to name of string to be printed
;	KEY	Set to variable to hold the character of the key pressed.
;
; r.s. 1/1/97
; $Id: any_key.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
pro any_key,say=say,key=key
	if getenv('IDL_SVG') ne '' then svg
	if !d.name ne 'X' then return
;
; swallow type-ahead buffer...
;
	key=1
	while key ne 0 do key=get_kbrd(0)
;
; message and get key
;
	if not keyword_set(say) then say='Press any key...'
	print,say
	key=get_kbrd(1)
	if strupcase(key) eq 'X' then retall
end
