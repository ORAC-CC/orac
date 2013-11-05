; $Id: //depot/idl/IDL_71/idldir/lib/strsplit.pro#1 $

; Copyright (c) 1999-2009, ITT Visual Information Solutions. All
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
;-

function strsplit, stringIn, pattern, _ref_extra=extra

    ON_ERROR, 2  ; return to caller
    RETURN, (n_params() eq 1) ? STRTOK(stringIn, _STRICT_EXTRA=extra) : $
        STRTOK(stringIn, pattern, _STRICT_EXTRA=extra)

end
