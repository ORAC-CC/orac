;===========================================================================
;+
;	function RESCALE
;
;	Description
; Rescale a vector of number to distributed between two new values
;
;	Parameters
; XIN vector to be rescaled
; RANGE new min and max of scale (default is 0->1)
;
;	Keywords 
;  I  MIN Set min limit on input to consider
;  I  MAX Set max limit on input to consider
;  I  LOW Value to set numbers exceeding min value to on return
;  I  HIGH Value to set numbers exceeding max value to on return
;          (default is to set to min/max)
;
;	Date
;	B. Latter : 15th November 2001
; $Id: rescale.pro 222 2009-06-08 15:56:30Z blatter $
;-
;===========================================================================
function rescale,xin,range,min=min,max=max,low=low,high=high

x=xin ; copy variable to avoid changing original
sh=(size(high))[1] ; for checking keywords set (even if value is 0)
sl=(size(low))[1]

; Force data to lie in required range or min->max
; Set outside data to low/high or min/max
if (size(min))[1] ne 0 then begin
	wh1=where(x lt min,count)
	if wh1[0] ge 0 then if sl eq 0 then x[wh1]=min $
			else x[wh1]=low ; note normalise will reset these on output
endif else min=min(x)

if (size(max))[1] ne 0 then begin
	wh2=where(x gt max,count)
	if wh2[0] ge 0 then if sh eq 0 then x[wh2]=max $
			else x[wh2]=high ; note normalise will reset these on output
endif else max=max(x)

mm=1./(max-min)
x=(x-min)*mm   ; rescale between 0 and 1

if (size(range))[1] ne 0 then $
	x=range[0]+x*(range[1]-range[0]) ; bring up to new scale

; Reset out of range values by passes low/high values
if sl gt 0 then if wh1[0] ge 0 then x[wh1]=low
if sh gt 0 then if wh2[0] ge 0 then x[wh2]=high

return,x
end
