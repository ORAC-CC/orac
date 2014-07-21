;+
; NAME:
;   WRAP_MAPPOINTS
;
; PURPOSE:
;   A wrapper for the MAPPOINTS procedure used by PLOT_ORAC to ensure appropriate
;   formatting.
;
; CATEGORY:
;   ORAC plotting tools
;
; CALLING SEQUENCE:
;   WRAP_MAPPOINTS, data, lat, lon, settings, plot_settings, filter, line, nl, 
;                   symsize [, l [, m]]
;
; INPUTS:
;   data     = Information to be plotted.
;   lat      = The latitude of DATA.
;   lon      = The longitude of DATA.
;   settings = A single structure output by PLOT_SETTINGS.
;   plot_settings = A single structure for input to PLOT_POSITION.
;   filter   = A binary filter for DATA.
;   line     = The number of along-track lines to plot per figure.
;   nl       = The number of points across-track.
;   symsize  = The size of the plotting symbol.
;
; OPTIONAL INPUTS:
;   l = When plotting fields with a dimension beyond lat/lon, this denotes the
;       index such that it can be included in the plot header.
;   m = As L, but for fields with two additional dimensions.
;	
; KEYWORD PARAMETERS:
;   None.
;	
; OUTPUTS:
;   None.
; 
; OPTIONAL OUTPUTS:
;   None.
;
; RESTRICTIONS:
;   PLOT_SETTING structure.
;
; MODIFICATION HISTORY:
;   15 Jul 2014 - Initial version by ACPovey (povey@atm.ox.ac.uk) 
;   21 Jul 2014 - ACP: Fixed bug when using LINE.
;-
PRO WRAP_MAPPOINTS, d, lat, lon, set, plot_set, filt, line, nl, syms, l, m, $
                    debug=debug
   ON_ERROR, KEYWORD_SET(debug) ? 0 : 2
   COMPILE_OPT LOGICAL_PREDICATE, STRICTARR, STRICTARRSUBS

   ;; determine range for plot colourbar
   ran = FINITE(set.range[0]) ? set.range : SELECT_RANGE(d[WHERE(filt)], set)

   n=N_ELEMENTS(lat)
   start=0
   while start lt n do begin
      PLOT_POSITION, plot_set, pos, bpos, debug=debug

      ;; print title
      title = set.title eq '' ? FMT(set.name) : set.title
      if ARG_PRESENT(l) then begin
         if ARG_PRESENT(m) $
         then tit=title+' ('+STRING(m,format='(i0)')+','+ $
                  STRING(l,format='(i0)')+')' $
         else tit=title+' ('+STRING(l,format='(i0)')+')'
      endif else tit=title
      XYOUTS,/normal,align=0.5,.5*(pos[0]+pos[2]),color=plot_set.col, $
             pos[3] + (set.mode gt 0 ? 2.:.5)*!d.y_ch_size/!d.y_size, $
             tit

      endl = (start + nl*line - 1) < (n - 1)

      ;; determine limits of swath (to minimise plot area)
      minlat=MIN(lat[start : endl],max=maxlat)
      if maxlat gt 89.8 then begin
         cent=[90.,lon[.5*(start+endl)]]
         lim=[minlat,-180.,90.,180.]
      endif else if minlat lt -89.8 then begin
         cent=[-90.,lon[.5*(start+endl)]]
         lim=[-90.,-180.,maxlat,180.] 
      endif else begin
         cent=[lat[.5*(start+endl)],lon[.5*(start+endl)]]
         ;; Determine the left and right-most lons of first swath edge
         if lon[start] gt lon[start+1] OR $
            lon[start+1] gt lon[start+2] then begin
            minl1=lon[start+nl-1]
            maxl1=lon[start]
         endif else begin
            minl1=lon[start]
            maxl1=lon[start+nl-1]
         endelse
         ;; Determine the left and right-most lons of second swath edge
         if lon[endl-nl+1] gt lon[endl-nl+2] OR $
            lon[endl-nl+2] gt lon[endl-nl+3] then begin
            minl2=lon[endl]
            maxl2=lon[endl-nl+1]
         endif else begin
            minl2=lon[endl-nl+1]
            maxl2=lon[endl]
         endelse
         ;; Check if dateline crosses swath
         minlon = ABS(minl1-minl2) gt 180. XOR minl1 lt minl2 ? minl1 : minl2
         maxlon = ABS(maxl1-maxl2) gt 180. XOR maxl1 gt maxl2 ? maxl1 : maxl2
         lim=[minlat,minlon,maxlat,maxlon]      
      endelse

      ;; difference plots shouldn't have flag labels
      if ~set.abs then begin
         plabels=WHERE(set.blabels ne '',nbl)
         labels = nbl gt 0 ? set.blabels[plabels] : ''
      endif

      ;; plot field
      MAPPOINTS, d[start : endl], lat[start : endl], lon[start : endl], $
                 bposition=bpos, btickformat=set.btf, centre=cent, $
                 colourbar_labels=labels, diffcolourbar=set.abs, $
                 dpcm=150, filter=filt, limit=lim, log=set.log, $
                 nlevels=set.nlevels, /noerase, plot_colour=plot_set.col, $
                 position=pos, range=ran, /silent, syms=syms, debug=debug

      if set.outline then begin
         edge=start + nl*LINDGEN(line < FLOOR((n-start)/nl))
         OPLOT,lon[start : start+nl-1],lat[start : start+nl-1]
         OPLOT,lon[edge],lat[edge]
         OPLOT,lon[endl-nl+1 : endl],lat[endl-nl+1 : endl]
         OPLOT,lon[edge+nl-1],lat[edge+nl-1]
      endif
      start = endl + 1
   endwhile

END
