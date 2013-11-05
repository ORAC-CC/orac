;===============================================================================
;+
; CLEVELS.PRO
;
; this function returns a vector, the elements of which correspond
; to reasonably sensible levels for use in the IDL CONTOUR routine.
; the user supplies the array to be contoured.
; 19 elements are used by default.
; if the keyword 'last' is set, the last contour levels set up by this
; program are used.
;
;    E.g.
;	IDL> set_ps
;    ; read in your array
;	IDL> levs=clevels(array)
;	IDL> contour,array,levels=levs
;
;Parameters
;
;    ARRAY2	the array to be contoured.
;
;
;Keywords
;
;    LEVELS Determines the number of levels to be returned (defaults to 19).
;    STEP	Fixes the interval between the contours.
;    STANDARD	Fixes a level which must be contoured. The levels returned will be set
;		so that IDL will label the standard contour level by default 
;		(assuming this contour falls within the range of the data). If you
;		explicitly specify the levels to be labelled then you'll have to
;		make sure the standard level is actually labelled yourself (although
;		it will still be drawn).
;    RANGE	Set approx range of values
;	HELP	Print help on usage
;	MIN_V	Specify minimum value below which values ignored
;	MAX_VALUE	Specify maximum value below which values ignored
;
; R.S. 1/12/94
; $Id: clevels.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function clevels,array2,levels=levels,step=rstep,$
	help=help,standard=standard,max_value=max_val,range=range,min_v=min_v

;
; only test on finite array elements
;
whf=where(finite(array2) eq 1)
if whf(0) eq -1 then begin
	message,/cont,'No finite elements in array !'
	return,findgen(11)/10.
endif
array=array2(whf)
if n_elements(min_v) ne 0 then begin
	whf=where(array ge min_v)
	if whf(0) eq -1 then begin
		message,/cont,'No elements gt min_v'
		return,findgen(11)/10
	endif
	array=array(whf)
endif
if keyword_set(help) then begin
        rshelp,'clevels'
        return,0
endif
if not keyword_set(levels) then levels=30
if keyword_set(max_val) then begin
; set array elements to the current minimum value if they are above the maximum
	min=min(array)
	mvs=where(array gt max_val)
	if mvs(0) ne -1 then array(mvs)=min
endif

;get max and min values of the array...
max=max(array,min=min)

if keyword_set(range) then begin
	max=range[1]
	min=range[0]
endif
if max eq min then begin
	message,'Array is a constant value ('+string(min)+')',/cont
	return,[min-1,min,min+1]
;	if max eq 0. then begin
;		max=1.
;		min=-1.
;	endif else begin
;		max=max*1.02
;		min=min*0.98
;	endelse
;	rstep=(max-min)/10
endif
if not keyword_set(rstep) then begin
; if the step between levels is not set then work a sensible one out...
	step=(max-min)/float(levels)

; round off to nearest order of magnitude below this step. 
	oom=float(long(alog10(step)))
	ok=0
	while not ok do begin
		nst=[1d0,2.,5.,10.]*10d0^oom
		if min ge 0d0 then low=double(long(min/nst))*nst $
		else low=double(long(min/nst)-1)*nst
		high=low+double(levels-1)*nst
		whr=where(high gt max)
		if whr(0) eq -1 then oom=oom+1. $
		else if whr(0) eq 0 then oom=oom-1. $
		else ok=1
	endwhile
	rstep=nst(whr(0))
	low=low(whr(0))
endif else begin

; determine lowest level

	if min ge 0.0 then low=double(long(min/rstep))*rstep $
	else low=double(long(min/rstep)-1)*rstep
endelse

; if a standard level is required then make sure it is included in the levels,
; by adjusting the low value.

if keyword_set(standard) then begin
	low=low+((standard-low) mod rstep)
	if low gt min then low=low-rstep
; now make sure that the standard level is labelled. This is done by making
; sure the index of the standard level in the array is odd (or would be odd if
; it fitted within the array range). IDL will now label this level by default.
; You will have to do something else is you specifically specify which levels
; to label.
	if long((standard-low)/rstep) mod 2 eq 1 then low=low-rstep
endif

; work out levels

levs=findgen(levels)*rstep+low

;
; remove all but one of the levels above and below min/max values
;
wh=where(levs lt min)
if n_elements(wh) gt 1 then whmin=wh(n_elements(wh)-1) else whmin=0
wh=where(levs gt max)
if n_elements(wh) gt 1 then whmax=wh(0) else whmax=n_elements(levs)-1
levs=levs(whmin:whmax)
return,levs
end
