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
;   15 Jul 2014 - ACP: Initial version (povey@atm.ox.ac.uk).
;   21 Jul 2014 - ACP: Fixed bug when using LINE.
;   28 Jul 2014 - ACP: Altered range behaviour for FALSECOLOUR plots.
;-
PRO WRAP_MAPPOINTS, d, lat, lon, set, plot_set, filt, line, nl, syms, l, m, $
                    debug=debug, short=short, false=false
   ON_ERROR, KEYWORD_SET(debug) ? 0 : 2
   COMPILE_OPT LOGICAL_PREDICATE, STRICTARR, STRICTARRSUBS

   ;; determine range for plot colourbar
   if KEYWORD_SET(false) then begin
      if FINITE(set.range[0]) then fcnorm=set.range 
   endif else $
      ran = FINITE(set.range[0]) ? set.range : SELECT_RANGE(d[WHERE(filt)], set)

   for i=0,plot_set.frames-1 do begin
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

      ;; difference plots shouldn't have flag labels
      if ~set.abs then begin
         plabels=WHERE(set.blabels ne '',nbl)
         labels = nbl gt 0 ? set.blabels[plabels] : ''
      endif

      ;; plot field
      if KEYWORD_SET(short) then $
         IMAGE_PLOT, d, $
                     bar_position=bpos, cbar_labels=labels, $
                     diffcolourbar=set.abs, filter=filt, $
                     log=set.log, nlevels=set.nlevels, /noerase, $
                     color=plot_set.col, position=pos, range=ran $
      else $
         MAPPOINTS, d, lat, lon, $
                    bposition=bpos, btickformat=set.btf, $
                    centre=plot_set.centre[*,i], colourbar_labels=labels, $
                    debug=debug, diffcolourbar=set.abs, dpcm=150, filter=filt, $
                    limit=plot_set.limit[*,i], log=set.log, fcnorm=fcnorm, $
                    nlevels=set.nlevels, /noerase, plot_colour=plot_set.col, $
                    position=pos, range=ran, /silent, syms=syms, false=false
   endfor

END
