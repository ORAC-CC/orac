;==============================================================================;
;+
; DEF_COLORS
;
; This returns colors, linestyles and thicks for given number of lines
;
; PARAMETERS
;	N 	Number of lines
;    On return...
;	COLS	Colours
;	LINES	Line styles
;	THICKS	Thicknesses
;
; KEYWORDS
;	COLOR	Set for color plots
;	MCOL	Maximum number of colors allowed before cycling into line
;		styles.
;
; R.S. 24/3/97
; $Id: def_colors.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;==============================================================================;
pro def_colors,n,cols,lines,thicks,color=color,mcol=mcol
	if not keyword_set(mcol) then mcol=12
	if !d.name eq 'PS' then pfac=2 else pfac=1
	if keyword_set(color) then begin
                cols=(indgen(n) mod mcol) +1
                lo=[0,2,3,1,4,5]
                while n_elements(lo) lt n/mcol+1 do $
                        lo=[lo,lo]
                lines=replicate(lo(0),mcol)
                i=1
                while n_elements(lines) lt n do begin
                        lines=[lines,replicate(lo(i),mcol)]
                        i=i+1
                endwhile
                lines=lines(0:n-1)
                if !d.name eq 'PS' then th=4 else th=2
		thicks=replicate(th,n)
	endif else begin
		cols=replicate(c0(),n)
		lines=[0,2,3,1,4,5]
		while n_elements(lines) lt n do lines=[lines,lines]
		lines=lines(0:n-1)
		thicks=indgen(n)/6*pfac +1
	endelse
end

