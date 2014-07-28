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
;-
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
         ;; Determine the left and right-most lons of first swath edge
         if lon[0,start] gt lon[1,start] OR $
            lon[1,start] gt lon[2,start] then begin
            minl1=lon[nl1-1,start]
            maxl1=lon[0,start]
         endif else begin
            minl1=lon[0,start]
            maxl1=lon[nl1-1,start]
         endelse
         ;; Determine the left and right-most lons of second swath edge
         if lon[0,endl] gt lon[1,endl] OR $
            lon[1,endl] gt lon[2,endl] then begin
            minl2=lon[nl1-1,endl]
            maxl2=lon[0,endl]
         endif else begin
            minl2=lon[0,endl]
            maxl2=lon[nl1-1,endl]
         endelse
         ;; Check if dateline crosses swath
         minlon = ABS(minl1-minl2) gt 180. XOR minl1 lt minl2 ? minl1 : minl2
         maxlon = ABS(maxl1-maxl2) gt 180. XOR maxl1 gt maxl2 ? maxl1 : maxl2
         lim[*,i]=[minlat,minlon,maxlat,maxlon]      
      endelse
   endfor

END
