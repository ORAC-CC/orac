;+
; NAME:
;   PLOT_POSITION
;
; PURPOSE:
;   Returns appropriate values of the POSITION keyword, in normalised coords,
;   for a plot and its colourbar using an appropriate structure. If a new page
;   is required, that is created.
;
; CATEGORY:
;   Plotting tools
;
; CALLING SEQUENCE:
;   PLOT_POSITION, settings, position, colourbar_position
;
; INPUTS:
;   settings = A structure with the fields:
;      TAG:    The root filename for plotting windows.
;      LABEL:  An identifying string to be plotted in the top corner of each page
;      SHEET:  The page number of the current sheet.
;      FONT_S: Font size.
;      XS:     Horizontal size of plotting window.
;      YS:     Vertical size of the plotting window.
;      CS:     Vertical extent of a character, in normalised coords.
;      NX:     Number of plots horizontally.
;      NY:     Number of plots vertically.
;      X0:     FLTARR(NX) - the coordinate of the left edge.
;      X1:     FLTARR(NX) - the coordinate of the right edge.
;      Y0:     FLTARR(NX) - the coordinate of the bottom edge.
;      Y1:     FLTARR(NX) - the coordinate of the top edge.
;      GRIDI:  Current horizontal plot number.
;      GRIDJ:  Current vertical plot number.
;      COL:    Plot colour (decomposed).
;
; OPTIONAL INPUTS:
;   None.
;	
; KEYWORD PARAMETERS:
;   None.
;	
; OUTPUTS:
;   position = A four-element array giving the normalised coordinates of the
;              corners of the plotting window.
;   colourbar_position = A four-element array giving the normalised coordinates
;              of the courners of the colourbar.
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
PRO PLOT_POSITION, plot_set, pos, bpos, debug=debug
   ON_ERROR, KEYWORD_SET(debug) ? 0 : 2
   COMPILE_OPT HIDDEN, LOGICAL_PREDICATE, STRICTARR, STRICTARRSUBS

   ;; if at end of page, start new sheet
   if ~(plot_set.gridi OR plot_set.gridj) then begin
      DEVICE,/close
      ++plot_set.sheet
      filen=plot_set.tag+'.N'+STRING(plot_set.sheet,format='(i02)')+'.eps'
      DEVICE,/encapsulated,font_s=plot_set.font_s,filen=filen, $
             xsize=plot_set.xs,ysize=plot_set.ys
      ;; print descriptive label
      XYOUTS,/normal, 0.5*plot_set.cs/plot_set.xs, $
             1.0-1.5*plot_set.cs/plot_set.ys, '!16'+plot_set.label+'!X'
   endif

   ;; determine plot position
   pos=[plot_set.x0[plot_set.gridi], plot_set.y0[plot_set.gridj], $
        plot_set.x1[plot_set.gridi], plot_set.y1[plot_set.gridj]]
   bpos=[pos[0]*!d.x_size-2.2*!d.y_ch_size,pos[1]*!d.y_size, $
         pos[0]*!d.x_size-1.7*!d.y_ch_size,pos[3]*!d.y_size]

   ;; iterate plot position
   if plot_set.gridi ge (plot_set.nx-1) then begin
      plot_set.gridi = 0
      plot_set.gridj = plot_set.gridj ge (plot_set.ny-1) ? 0 : plot_set.gridj+1
   endif else ++plot_set.gridi

END
