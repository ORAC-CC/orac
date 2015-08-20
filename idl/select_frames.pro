;+
; NAME:
;   SELECT_FRAMES
;
; PURPOSE:
;   Determines the LIMIT and CENTRE keywords for MAPPOINTS given the lat/longs
;   to be plotted and the number of frames over which it is desired to plot them.
;
; CATEGORY:
;   Plotting tools
;
; CALLING SEQUENCE:
;   SELECT_FRAMES, lat, lon, line, nline, frames, centre, limit
;
; INPUTS:
;   lat    = Latitude of points to be plotted.
;   lon    = Longitude of points to be plotted.
;   line   = Number of points along-track (second dimension of lat).
;   nline  = Number of points across-track (first dimension of lat).
;   frames = The number of figures across which to plot the swath.
;
; OPTIONAL INPUTS:
;   None.
;
; KEYWORD PARAMETERS:
;   None.
;
; OUTPUTS:
;   limit  = [minimum latitude, minimum longitude, maximum latitude, maximum
;            longitude]
;   centre = Coordinates at which to centre the map projection.
;
; OPTIONAL OUTPUTS:
;   None.
;
; RESTRICTIONS:
;   None.
;
; MODIFICATION HISTORY:
;   28 Jul 2014 - Initial version by ACPovey (povey@atm.ox.ac.uk)
;   10 Dec 2014 - New logic, abandoning doing something clever. Find the min/max
;                 of the corners of the swath. If any points in the swath are
;                 within 2 deg of the dateline, shift to a 0-360 coord system
;                 first.
;-
FUNCTION ANY, a
   for i=1,N_ELEMENTS(a)-1 do if a[i] || a[i-1] then RETURN, 1b
   RETURN, 0b
END

PRO SELECT_FRAMES, lat, lon, line1, nl1, frames, cent, lim
   ON_ERROR, 2
   COMPILE_OPT LOGICAL_PREDICATE, STRICTARR, STRICTARRSUBS

   ;; determine limits of swath (to minimise plot area)
   length=CEIL(FLOAT(line1) / frames)
   cent=FLTARR(2,frames)
   lim=FLTARR(4,frames)
   for i=0,frames-1 do begin
      start = i*length
      endl = ((i+1)*length-1) < (line1-1)
      middle = (start + endl)/2

      minlat=MIN(lat[*,start : endl],max=maxlat)
      if maxlat gt 89.8 then begin
         cent[*,i]=[90.,lon[nl1/2, middle]]
         lim[*,i]=[minlat,-180.,90.,180.]
      endif else if minlat lt -89.8 then begin
         cent[*,i]=[-90.,lon[nl1/2, middle]]
         lim[*,i]=[-90.,-180.,maxlat,180.]
      endif else begin
         cent[*,i]=[lat[nl1/2, middle], lon[nl1/2, middle]]

         ;; If within 2 deg of dateline, assume it is crossed
         corners = [lon[0,start],lon[nl1-1,start],lon[0,endl],lon[nl1-1,endl]]
         if ANY(ABS(lon[*,start : endl]) gt 178.) then begin
            ;; Centre plot on dateline rather than meridian
            corners[WHERE(corners lt 0.)] += 360.
            minlon = MIN(corners, max=maxlon)
            if minlon gt 180. then minlon -= 360.
            if maxlon gt 180. then maxlon -= 360.
         endif else minlon = MIN(corners, max=maxlon) ; Dateline not crossed

         lim[*,i] = [minlat, minlon, maxlat, maxlon]
      endelse
   endfor

END
