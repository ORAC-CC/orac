;+
; NAME:
;   FMT
;
; PURPOSE:
;   Format pretty plot titles, possibly with units.
;
; CATEGORY:
;   ORAC plotting tools
;
; CALLING SEQUENCE:
;   result = FMT(title [, units])
;
; INPUTS:
;   title  = String giving plot title.
;   units  = String giving plot units.
;
; OPTIONAL INPUTS:
;   None.
;	
; KEYWORD PARAMETERS:
;   None.
;	
; OUTPUTS:
;   result = A string formatted such that TITLE is in bold and UNITS are in
;            square brackets.
; 
; OPTIONAL OUTPUTS:
;   None.
;
; RESTRICTIONS:
;   None.
;
; MODIFICATION HISTORY:
;   15 Jul 2014 - Initial version by ACPovey (povey@atm.ox.ac.uk) 
;-
FUNCTION FMT, t, u
   ON_ERROR, 2
   COMPILE_OPT HIDDEN, LOGICAL_PREDICATE, STRICTARR, STRICTARRSUBS
   ;; Format a title and unit for printing as a plot title
   if KEYWORD_SET(u) $
   then RETURN, '!15'+t+'!13 ['+u+']!X'  $
   else RETURN, '!15'+t+'!X'
END
