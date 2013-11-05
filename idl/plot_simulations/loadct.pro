; $Id: //depot/idl/IDL_71/idldir/lib/loadct.pro#1 $
;
; Copyright (c) 1982-2009, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.

;+
; NAME:
;	LOADCT
;
; PURPOSE:
;	Load predefined color tables.
;
; CATEGORY:
;	Image display.
;
; CALLING SEQUENCE:
;	LOADCT [, Table]
;
; OPTIONAL INPUTS:
;	Table:	The number of the pre-defined color table to load, from 0
;		to 15.  If this value is omitted, a menu of the available
;		tables is printed and the user is prompted to enter a table
;		number.
;
; KEYWORD PARAMETERS:
;	FILE:	If this keyword is set, the file by the given name is used
;		instead of the file colors1.tbl in the IDL directory.  This
;		allows multiple IDL users to have their own color table file.
;		The specified file must exist.
;	GET_NAMES: If this keyword is present AND DEFINED, the names
;		of the color tables are returned as a string array.
;		No changes are made to the color table.
;	NCOLORS = number of colors to use.  Use color indices from 0
;		to the smaller of !D.TABLE_SIZE-1 and NCOLORS-1.
;		Default = !D.TABLE_SIZE = all available colors.
;	SILENT:	If this keyword is set, the Color Table message is suppressed.
;	BOTTOM = first color index to use. Use color indices from BOTTOM to
;		BOTTOM+NCOLORS-1.  Default = 0.
;   RGB_TABLE: Set this keyword to a named variable in which to return
;       the desired color table as an [NCOLORS, 3] array.
;       If this keyword is set, then the color table is not loaded into
;       the display, but is simply returned to the user. In addition,
;       if RGB_TABLE is set then SILENT is also set to true.
;
; OUTPUTS:
;	No explicit outputs.
;
; COMMON BLOCKS:
;	COLORS:	The IDL color common block.
;
; SIDE EFFECTS:
;	The color tables of the currently-selected device are modified.
;
; RESTRICTIONS:
;	Works from the file: $IDL_DIR/resource/colors/colors1.tbl or the file specified
;	with the FILE keyword.
;
; PROCEDURE:
;	The file "colors1.tbl" or the user-supplied file is read.  If
;       the currently selected device doesn't have 256 colors, the color
;	data is interpolated from 256 colors to the number of colors
;	available.
;
;	The colors loaded into the display are saved in the common
;	block COLORS, as both the current and original color vectors.
;
;	Interpolation:  If the current device has less than 256 colors,
;	the color table data is interpolated to cover the number of
;	colors in the device.
;
; MODIFICATION HISTORY:
;	Old.  For a widgetized version of this routine, see XLOADCT in the IDL
;		widget library.
;	DMS, 7/92, Added new color table format providing for more than
;		16 tables.  Now uses file colors1.tbl.  Old LOADCT procedure
;		is now OLD_LOADCT.
;	ACY, 9/92, Make a pixmap if no windows exist for X windows to
;		determine properly the number of available colors.
;		Add FILE keyword.
;	WSO, 1/95, Updated for new directory structure
;	AB, 10/3/95, The number of entries in the COLORS common block is
;		now always !D.TABLE_SIZE instead of NCOLORS + BOTTOM as
;		before. This better reflects the true state of the device and
;		works with other color manipulations routines.
;   DLD, 09/98, Avoid repeating a color table name in the printed list.
;   CT, Nov 2006: Added RGB_TABLE keyword.
;
;-
PRO loadct, table_number, SILENT = silent, GET_NAMES = names, FILE=file, $
	NCOLORS = nc1, BOTTOM=bottom, RGB_TABLE=rgbTable

compile_opt idl2

common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr


on_ioerror, bad
on_error, 2		;Return to caller if error
get_lun, lun

rgbTable = Arg_Present(rgbTable)
silent = N_Elements(silent) ? silent : rgbTable

if !d.name eq 'X' and !d.window eq -1 then begin  ;Uninitialized?
;	If so, make a dummy window to determine the # of colors available.
	window,/free,/pixmap,xs=4, ys=4
	wdelete, !d.window
	endif

if n_elements(bottom) gt 0 then cbot = bottom > 0 < (!D.TABLE_SIZE-1) $
	else cbot = 0
nc = !d.table_size - cbot
if n_elements(nc1) gt 0 then nc = nc < nc1

if nc eq 0 then message, 'Device has static color tables.  Can''t load.'

if (n_elements(file) GT 0) then filename = file $
else filename = filepath('colors1.tbl', subdir=['resource', 'colors'])

openr,lun, filename, /block

ntables = 0b
readu, lun, ntables

; Read names?
IF (n_params() eq 0 || arg_present(names) || ~silent) then begin
    names = bytarr(32, ntables)
    point_lun, lun, ntables * 768L + 1	;Read table names
    readu, lun, names
    names = strtrim(names, 2)
    IF arg_present(names) THEN goto, close_file  ;Return names?
ENDIF

if n_params() lt 1 then begin	;Summarize table?
	nlines = (ntables + 2) / 3	;# of lines to print
        nend = nlines - ((nlines*3) - ntables)
	for i=0, nend-1 do $		;Print each line
	  print, format="(i2,'- ',a17, 3x, i2,'- ',a17, 3x, i2,'- ',a17)", $
	    i, names[i], i+nlines, names[i+nlines], i+2*nlines < (ntables-1), $
		names[i+2*nlines < (ntables-1)]
        if (nend lt nlines) then begin
          for i=nend, nlines-1 do $
	    print, format="(i2,'- ',a17, 3x, i2,'- ',a17)", $
	      i, names[i], i+nlines, names[i+nlines]
        endif

        table_number = 0
	read, table_number, PROMPT='Enter table number: '
	endif

if (table_number ge ntables) or (table_number lt 0) then begin
    message, 'Table number must be from 0 to ' + strtrim(ntables-1, 2)
endif

;Tables defined?
if (~rgbTable && n_elements(r_orig) lt !d.table_size) then begin
	r_orig = BYTSCL(indgen(!d.table_size))
	g_orig = r_orig
	b_orig = r_orig
endif


if keyword_set(silent) eq 0 then $
	message,'Loading table ' + names[table_number],/INFO
aa=assoc(lun, bytarr(256),1)	;Read 256 long ints
r = aa[table_number*3]
g = aa[table_number*3+1]
b = aa[table_number*3+2]

if nc ne 256 then begin	;Interpolate
	p = (lindgen(nc) * 255) / (nc-1)
	r = r[p]
	g = g[p]
	b = b[p]
	endif

if (rgbTable) then begin
    rgbTable = [[r], [g], [b]]
endif else begin
    r_orig[cbot] = r
    g_orig[cbot] = g
    b_orig[cbot] = b
    r_curr = r_orig
    g_curr = g_orig
    b_curr = b_orig
    tvlct,r, g, b, cbot
endelse
goto, close_file

bad:
  message, /CONTINUE, 'Error reading file: ' + filename + ', ' + !error_state.msg

close_file:
  free_lun,lun

end
