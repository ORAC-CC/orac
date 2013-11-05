; $Id: //depot/Release/IDL_81/idl/idldir/lib/strsplit.pro#1 $

; Copyright (c) 1999-2011, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.

;+
; NAME:
;       STRSPLIT
;
; PURPOSE:
;   Wrapper on the build in system routine STRTOK that implements exactly
;   the same interface as STRTOK, but with the STRSPLIT name.
;
;       The reason for doing this is so that if a user has their own
;   STRSPLIT in their local user library, their version will superceed
;   this one. RSI does not recommend this practice, but it is
;   allowed for backwards compatability reasons. See the
;       documentation for STRSPLIT in the IDL Reference manual
;   for details on arguments, keywords, and results.
;
;
; MODIFICATION HISTORY:
;   14 October 1999, AB, RSI.
;   AB, 5/4/2001, Switch from using _EXTRA to _STRICT_EXTRA, so that
;       incorrect keywords passed to STRTOK will issue proper
;       error messages instead of being silently ignored.
;   CT, 5/12/2010: Add support for string & pattern arrays, returns LIST.
;-

function strsplit, stringIn, pattern, $
  COUNT=count, LENGTH=length, EXTRACT=extract, $
  _REF_EXTRA=extra

    compile_opt idl2, hidden
    ON_ERROR, 2  ; return to caller
    
    ; Handle array input. Return a LIST of results.
    n = N_ELEMENTS(stringIn)
    if (n gt 1) then begin

      np = N_ELEMENTS(pattern)
      if (np gt 1 && np ne n) then $
        MESSAGE, 'PATTERN must be a scalar or have the same number of elements as STRING.'

      result = LIST()
      count = LONARR(n)
      if (ARG_PRESENT(length)) then length = LIST()

      for i=0,n-1 do begin
        result1 = (N_PARAMS() eq 1) ? $
          STRTOK(stringIn[i], COUNT=c, LENGTH=len, EXTRACT=extract, _STRICT_EXTRA=extra) : $
          STRTOK(stringIn[i], pattern[[i]], COUNT=c, LENGTH=len, EXTRACT=extract, _STRICT_EXTRA=extra)
        result.Add, result1
        count[i] = c
        if (ARG_PRESENT(length)) then length.Add, len
      endfor

      return, result
    endif


    ; Scalar input.
    RETURN, (n_params() eq 1) ? $
      STRTOK(stringIn, COUNT=count, LENGTH=length, EXTRACT=extract, _STRICT_EXTRA=extra) : $
      STRTOK(stringIn, pattern, COUNT=count, LENGTH=length, EXTRACT=extract, _STRICT_EXTRA=extra)

end
