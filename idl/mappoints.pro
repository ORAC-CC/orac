;+
; Wrapper for new MAPPOINTS procedure to allow use of old version
;
; 11 Nov 2012 - ACP: POSITION added here so it can work as an output.
; 03 Dec 2013 - ACP: Added ON_ERROR statement.
; 19 Dec 2013 - AJAS: Stopped passing /DEBUG, POSITION and /STOP to old version.
;-
pro MAPPOINTS, pts, lat, lon, old=old, position=pos, debug=debug, $
               stop=stp, _extra=ext
   ON_ERROR, KEYWORD_SET(debug) || KEYWORD_SET(stp) ? 0 : 2

   if KEYWORD_SET(old) then begin
      MAPPOINTS_OLD, pts, lat, lon,_strict_extra=ext
   endif else begin
      MAPPOINTS_NEW, pts, lat, lon, position=pos, debug=debug, $
                     stop=stp,_strict_extra=ext
   endelse
END
