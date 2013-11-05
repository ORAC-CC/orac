;===============================================================================
;+
; SET_ROWSCOLS
;
; Set number of rows/columns to fit A4 reasonably well given number of panels
; required.
;
; PARAMETERS
;	NUM	Number required
;	ROWS	On return
;	COLS	On return
;
; KEYWORDS
;	LANDSCAPE Set if page landscape
;
; R.S. 15/10/01
; $Id: set_rowscols.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
pro set_rowscols,num,cols,rows,landscape=land
	cols=fix(sqrt(float(num)*1.41))
	rows=num/cols
	if rows*cols ne num then rows=rows+1
	if keyword_set(landscape) then begin
		temp=cols
		cols=rows
		rows=temp
	endif
end
