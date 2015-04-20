;+
; NAME:
;   INIT_PLOT_SET
;
; PURPOSE:
;   Generates a structure summarising the parameters for forming a plot window
;   in PLOT_ORAC.
;
; CATEGORY:
;   ORAC plotting tools
;
; CALLING SEQUENCE:
;   settings = INIT_PLOT_SET(frames, limit, centre, label [, font_s=value]
;                            [, xsize=value] [, ysize=value] [, nx=value]
;                            [, ny=value] [, scale=value])
;
; INPUTS:
;   frames = The number of figures across which to plot the swath.
;   limit  = [minimum latitude, minimum longitude, maximum latitude, maximum
;            longitude]
;   centre = Coordinates at which to centre the map projection.
;   label  = Description to be printed at the top of each page.
;
; OPTIONAL INPUTS:
;   FONT_SIZE  = Desired font size.
;   X|YSIZE    = Horizontal|Vertical extent of the output panes.
;   NX|Y       = Horizontal|Vertical number of plots.
;   SCALE      = A number to multiply both NX|Y. A useful way to zoom in.
;
; KEYWORD PARAMETERS:
;   None.
;
; OUTPUTS:
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
;      Y0:     FLTARR(NY) - the coordinate of the bottom edge.
;      Y1:     FLTARR(NY) - the coordinate of the top edge.
;      GRIDI:  Current horizontal plot number.
;      GRIDJ:  Current vertical plot number.
;      COL:    Plot colour (decomposed).
;
; OPTIONAL OUTPUTS:
;   None.
;
; RESTRICTIONS:
;   None known.
;
; MODIFICATION HISTORY:
;   28 Jul 2014 - ACP: Initial version
;-
FUNCTION INIT_PLOT_SET, frames, lim, cent, label, font_s=font_s, $
                        xsize=xs, ysize=ys, nx=nx, ny=ny, scale=scale, $
                        left=left, bottom=bott, horz=horz, vert=vert
   ON_ERROR, 2
   COMPILE_OPT LOGICAL_PREDICATE, STRICTARR, STRICTARRSUBS

   ;; process inputs
   if ~KEYWORD_SET(xs) then xs=28.3
   if ~KEYWORD_SET(ys) then ys=19.6
   if ~KEYWORD_SET(nx) then nx=4
   if ~KEYWORD_SET(ny) then ny=3
   if ~KEYWORD_SET(font_s) then font_s=8.
   if KEYWORD_SET(scale) then begin
      xs*=scale
      ys*=scale
   endif
   if ~KEYWORD_SET(left) then left=7.0
   if ~KEYWORD_SET(bott) then bott=1.5
   if ~KEYWORD_SET(horz) then horz=9.0
   if ~KEYWORD_SET(vert) then vert=5.0

   ;; form plot settings structure
   plot_set = {tag:'', label:label, sheet:-1, font_s:font_s, xs:xs, ys:ys,  $
               cs:0.,nx:nx, ny:ny, x0:FLTARR(nx), x1:FLTARR(nx), y0:FLTARR(ny), $
               y1:FLTARR(ny), gridi:0, gridj:0, col:0, $
               frames:frames, limit:lim, centre:cent}

   ;; determine character size in PS plot
   test_file='plot_preproc_test.eps'
   DEVICE, /encapsulated, font_s=font_s, xsize=xs, ysize=ys, filen=test_file
   PLOT,[0,1]
   plot_set.cs=ys*!d.y_ch_size/!d.y_size
   DEVICE,/close
   if FILE_TEST(test_file,/regular) then FILE_DELETE,test_file

   ;; determine plot grid in normalised coordinates
   sx=horz*plot_set.cs           ; horizontal padding around plot
   sy=vert*plot_set.cs           ; vertical padding around plot
   tx=(plot_set.xs - sx*plot_set.nx) / plot_set.nx ; width of a plot
   ty=(plot_set.ys - sy*plot_set.ny) / plot_set.ny ; height of a plot
   plot_set.x0=(left*plot_set.cs + $
                (tx+sx)*FINDGEN(plot_set.nx))/plot_set.xs ; left edge
   plot_set.x1=plot_set.x0 + tx/plot_set.xs               ; right edge
   plot_set.y0=(bott*plot_set.cs + (ty+sy)* $
                (plot_set.ny-1.-FINDGEN(plot_set.ny)))/plot_set.ys ; bottom edge
   plot_set.y1=plot_set.y0 + ty/plot_set.ys                        ; top edge

   RETURN, plot_set
END
