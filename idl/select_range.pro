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
;   15 Jul 2014 - ACP: Initial version (povey@atm.ox.ac.uk)
;   23 Jul 2014 - ACP: Altered use of LOG plots.
;-
FUNCTION SELECT_RANGE, tmp, set
   ON_ERROR, 2
   COMPILE_OPT LOGICAL_PREDICATE, STRICTARR, STRICTARRSUBS

   if set.log then begin
      ;; log plots need to consider only positive values
      p=WHERE(tmp gt 0., np)
      if np le 0 then RETURN, [1.,10.]
      if N_ELEMENTS(UNIQ(tmp[p])) eq 1 then RETURN, [tmp[p[0]]*0.1,tmp[p[0]]]
      perc=.11
      repeat begin
         perc-=.01
         ran=EXP(CGPERCENTILES(ALOG(tmp[p]),perc=[perc,1.-perc]))
      endrep until ran[0] ne ran[1]
      RETURN, ran
   endif

   if N_ELEMENTS(UNIQ(tmp)) eq 1 then begin
      ;; if there is only one valid value, select a placeholder range
      if set.abs then RETURN, tmp[0] eq 0 ? [-1.,1.] : [-ABS(tmp[0]),ABS(tmp[0])]
      if tmp[0] eq 0 then RETURN, [0.,1.]
      RETURN, tmp[0] lt 0 ? [tmp[0],0.] : [0., tmp[0]]
   endif

   ;; otherwise, consider desired settings
   if set.abs then begin
      if set.full then l=MAX(ABS(tmp),/nan) else begin
         ;; for a difference plot, ensure top and bottom range are equal
         perc=[.79]
         repeat begin
            perc+=.01
            l=CGPERCENTILES(ABS(tmp),perc=perc)
         endrep until l ne 0
      endelse
      ran=[-l,l]
   endif else if set.full then ran=[MIN(tmp,max=m,/nan),m] $
   else if set.bottom then ran=CGPERCENTILES(tmp,perc=[.1,.9]) $
   else ran=[0.,CGPERCENTILES(tmp,perc=[.9])]

   ;; for bad fields, select 0:1 or 0:value
   if ran[0] ge ran[1] then ran[ran[0] le 0] = ran[0] eq 0

   RETURN, ran

END
