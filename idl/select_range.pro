;+
; NAME:
;   SELECT_RANGE
;
; PURPOSE:
;   Determine an appropriate scale of a colour plot.
;
; CATEGORY:
;   ORAC plotting tools.
;
; CALLING SEQUENCE:
;   range = SELECT_RANGE(values, settings)
;
; INPUTS:
;   values   = The values to be plotted.
;   settings = A structure (as outlined in PLOT_SETTINGS).
;
; OPTIONAL INPUTS:
;   None.
;	
; KEYWORD PARAMETERS:
;   None.
;	
; OUTPUTS:
;   range    = A two-element array giving the min and max of the colourbar.
; 
; OPTIONAL OUTPUTS:
;   None.
;
; RESTRICTIONS:
;   CGPERCENTILES (from the Coyote library).
;
; MODIFICATION HISTORY:
;   15 Jul 2014 - Initial version by ACPovey (povey@atm.ox.ac.uk) 
;-
FUNCTION SELECT_RANGE, tmp, set
   ON_ERROR, 2
   COMPILE_OPT LOGICAL_PREDICATE, STRICTARR, STRICTARRSUBS

   if set.full then ran=[MIN(tmp,max=m,/nan),m] else begin
      if set.abs then begin
         ;; for a difference plot, ensure top and bottom range are equal
         ;; firstly, check there is variation in the field
         if N_ELEMENTS(UNIQ(tmp)) eq 1 then RETURN, [0.,1.]
         
         perc=[.79]
         repeat begin
            perc+=.01
            l=CGPERCENTILES(ABS(tmp),perc=perc)
         endrep until l ne 0
         ran=[-l,l]
      endif else if set.log then $
         ran=EXP(CGPERCENTILES(ALOG(tmp[WHERE(tmp gt 0.)]),perc=[.1,.9])) $
      else if set.bottom then ran=CGPERCENTILES(tmp,perc=[.1,.9]) $
      else ran=[0.,CGPERCENTILES(tmp,perc=[.9])]
   endelse

   ;; for bad fields, select 0:1 or 0:value
   if ran[0] ge ran[1] then ran[ran[0] le 0] = ran[0] eq 0

   RETURN, ran

END
