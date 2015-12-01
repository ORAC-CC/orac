;+
; NAME:
;   MAPPOINTS_NEW
;
; PURPOSE:
;   Plots an array of values, with corresponding lat/lon values, onto a map
;   (intentionally emulating the syntax of MAPPOINTS, upon which this routine is
;   based). Makes use of  the Z buffer to produce the plot image, such that the
;   IDL MAP routines may be used to do all of the heavy lifting, whilst
;   minimising memory and processing overheads compared to MAPPOINTS.
;
; CATEGORY:
;   Plotting routine
;
; CALLING SEQUENCE:
;   MAPPOINTS_NEW, pts, lat, lon, [limit=array | centre=array],
;                  falsecolour={1 | 2 | 3}, fcnorm=array, nlevels=value,
;                  [levels=array | range=range], /log, filter=array,
;                  mycolours=array, psym=value, symsize=value, /squares, /back,
;                  /diffcolourbar, /diffcolourgrey, /nogrey, /rywdiff,
;                  /d2colourbar, /red, /green, colourtable=value, /mercator,
;                  /stereo, /hammer, /cartesian_projection, /nocolourbar,
;                  /isotropic, /nobox, /hires, /coast, /countries, /usa,
;                  /rivers, /nocontinents, /nogrid, dpcm=value, units=string,
;                  title=string, colourbar_labels=array, charsize=value,
;                  keyticks=value, position=array, plotpoints=array,
;                  plotpsym=value, plotposition=variable, btickformat=string,
;                  bposition=array
;
; INPUTS:
;   pts       = an array of points to be plotted.
;   lat       = an array of latitudes for each point in pts.
;   lon       = an array of longitudes for each point in pts.
;
; OPTIONAL INPUTS:
;   limit     = [min_lat,min_lon,max_lat,max_lon], the range of the plotting
;                  window.
;   centre    = [lat,lon], the coordinates of the centre of the plotting
;               window. Is ignored if LIMIT is set.
;   falsecolour = produce a false-colour RGB image, with its value indicating
;               the dimension over which the image is interleaved
;               (1 => 3*nx*ny, 2 => nx*3*ny, 3 => nx*ny*3).
;   fcnorm    = [min_colour,max_colour], for a FALSECOLOUR image this sets the
;               lower and upper limits by which the input is scalled.
;   nlevels   = the number of colour levels to be used in the plot.
;   range     = the range of the colour levels.
;   levels    = the values of the colour levels. Takes precedence over NLEVELS
;               and RANGE.
;   filter    = an array of binary flags for each point in pts determining if
;               it should be plotted.
;   mycolours = a prescribed colour table, passed to TVLCT.
;   psym      = the plotting symbol to be used. Default is 3, a dot.
;   symsize   = the size of the plotting symbol. If set while PSYM isn't, the
;               plotting symbol is set to a circle.
;   colourtable = index of a colour table to be loaded with LOADCT.
;   dpcm      = when plotting to a postscript, this sets the number of pixels
;               per centimetre . Default is 59.1, equivalent to 150 ppi print
;               resolution.
;   units     = label left of the colourbar.
;   title     = label above the plot.
;   colourbar_labels = to be used when plotting flags, being equally spaced
;               values that correspond to some text description. This specifies
;               the labels for the levels of the colourbar. I also recommend
;               setting LEVELS.
;   keyticks  = the number of ticks on the colourbar - 1 (i.e. the YTICKS
;               keyword).
;   charsize  = character size for all plots. Default is 1.
;   position  = [x_min,y_min,x_max,y_max], position for plotting window in
;               normalised coordinates.
;   plotpoints = [*,2], an array of X-Y coordinates to be plotted over the
;               image. DEPRECIATED as one can now overplot directly onto the
;               image.
;   plotpsym  = plot symbol with which PLOTPOINTS should be plotted.
;   x|ymargin = [left|bottom,right|top], the margins of the plot in character
;               units.
;   btickformat = YTICKFORMAT for the colourbar.
;   label     = on the plotting grid, label every nth parallel.
;   bposition = [x_min,y_min,x_max,y_max], position for colourbar in device
;               coordinates.
;
; KEYWORD PARAMETERS:
;   LOG       = use a logarithmic rather than linear colour scale.
;   SQUARES   = if SYMSIZE has been set, plot hollow squares rather than
;               circles.
;   BACK      = inverts the order of the colour table.
;   DIFFCOLOURBAR, DIFFCOLOURGREY, NOGREY, RYWDIFF, D2COLOURBAR, OMI = selects
;               a colour table from EODG routines.
;   RED, GREEN = if NOGREY is set, produces a green and/or red colour table.
;   MERCATOR, STEREO, HAMMER, CARTESIAN_PROJECTION = selects map projection.
;               Default is CARTESIAN.
;   HIRES, COAST, COUNTRIES, USA, RIVERS = specifies which features should be
;               plotted over the image.
;   NOCONTINENTS = disables overplotted features. Overrules HIRES, COAST,
;               COUNTRIES, USA, RIVERS.
;   NOCOLOURBAR = disables the colourbar.
;   ISOTROPIC = force lat and lon to take equal distances.
;   NOBOX     = writes lat/lon key inside plot, rather than at the edges.
;   NOGRID    = in addition to NOBOX, do not plot lat/lon grid lines.
;   NOERASE   = don't clear plot window before plotting. Needed for multiplots
;               without !P.MULTI.
;   DATELINE  = centre map around the International Date Line rather than the
;               Greenwich Meridian.
;   WHITE_BACK = force the background to be white (and lines to be black).
;
; OUTPUTS:
;   plotposition = [x_min,y_min,x_max,y_max], the position of the plot window.
;
; RESTRICTIONS:
;   Uses the Z buffer, so any information already placed there will be erased.
;   Uses COLOUR_WHTEDIFF, COLOUR_PS, COLOUR_CBW from EODG libraries.
;
; MODIFICATION HISTORY:
;   Written by ACPovey (povey@atm.ox.ac.uk)
;   16 Jul 2013 - ACP: Replaces previous MAPPOINTS. A wrapper has been made
;      to permit calls to the original with the keyword OLD.
;   27 Jul 2013 - AJAS; Corrected bug with AXIS, yaxis=1 for colourbar_labels
;      keyword.
;   26 Sep 2013 - AJAS: Corrected bug with /CARTESIAN keyword.
;   11 Nov 2013 - ACP: Added CENTRE functionality for /CARTESIAN. Doesn't plot
;       NaN.
;   03 Dec 2013 - ACP: Standardised indentation and line length.
;   16 Jan 2014 - ACP: FALSECOLOUR bug fixes.
;   10 Apr 2014 - ACP: Added BPOSITION. Automated LIMIT determination now
;       considers FILTER.
;   13 Jun 2014 - ACP: Points equal to the maximum of RANGE now plot as the
;      last level colour rather than the 'greater than' colour.
;   15 Jul 2014 - ACP: Added COLOUR keyword. Changed behaviour when using
;      COLOURBAR_LABELS.
;   28 Jul 2014 - ACP: Fixed bug in with FALSECOLOUR that allocated the wrong
;      background colour.
;   06 Aug 2014 - G Thomas: Made falsecolour keyword functional
;   13 Oct 2014 - ACP: CARTESIAN now forces the central latitude to be zero so
;      ensure the expected x-y grid is plotted.
;   15 Oct 2014 - ACP: Alter colourbar behaviour to make sense. Add WHITE_BACK
;      keyword.
;   19 Dec 2014 - ACP: Slight tweak to MYCOLOURS.
;   09 Feb 2015 - ACP: Bug fix to points below range.
;   06 Mar 2015 - AJAS: Bug fix for UNITS keyword and TITLE when /WHITE_BACK set
;   29 Jun 2015 - ACP: Changed top/bot colour for greyscale colourbar.
;   14 Jul 2015 - CH: Added CB_TOP_COLOUR and CB_BOT_COLOUR keywords to allow the
;      colours at the top and bottom of the colour bar to be specified by the
;      user. Corrected the centring of the map for regions that cross the
;      dateline.
;   16 Nov 2015 - ACP: Added ROBINSON projection. Best used with CENTRE.
;-
pro MAPPOINTS_NEW, pts, lat, lon, limit=lim, centre=centre, $
                   falsecolour=falsecolour, fcnorm=fcnorm, nlevels=nlevels, $
                   levels=levels, log=log, range=range, filter=filter, $
                   mycolours=mycolours, back=back, psym=psym, symsize=symsize, $
                   squares=squares, diffcolourbar=diffcolourbar, $
                   diffcolourgrey=diffcolourgrey, nogrey=nogrey, $
                   rywdiff=rywdiff, d2colourbar=d2colourbar, red=rplt, $
                   green=gplt, colourtable=colourtable, omi=omi, $
                   mercator=mercator,stereo=stereo,hammer=hammer, $
                   robinson=robinson, $
                   cartesian_projection=cartesian, nocolourbar=nocolourbar, $
                   isotropic=isotropic, dpcm=dpcm, nobox=nobox, hires=hires, $
                   coast=coast, countries=countries, usa=usa, rivers=rivers, $
                   nocontinents=nocontinents, nogrid=nogrid, noerase=noerase, $
                   units=units, colourbar_labels=colourbar_labels, $
                   title=title, charsize=charsize, keyticks=keyticks, $
                   position=pos,xmargin=xmar,ymargin=ymar,btickformat=btickf, $
                   plotpoints=plotpoints, plotpsym=plotpsym, debug=debug, $
                   stop=stp, silent=silent, label=label, bposition=bpos, $
                   central_azimuth=azi, plot_colour=colour, white_back=white, $
                   output_limit=limit, CB_TOP_COLOUR=cb_top_colour, $
                   CB_BOT_COLOUR=cb_bot_colour

   ON_ERROR, KEYWORD_SET(debug) || KEYWORD_SET(stp) ? 0 : 2
   COMPILE_OPT HIDDEN, LOGICAL_PREDICATE, STRICTARR, STRICTARRSUBS

   if ~KEYWORD_SET(falsecolour) then falsecolour=0
   n = falsecolour ? N_ELEMENTS(pts)/3 : N_ELEMENTS(pts)
   if N_ELEMENTS(lat) ne n OR N_ELEMENTS(lon) ne n then begin
      MESSAGE,/cont,'Latitude and longitude arrays must be specified and '+$
              'have the same number of elements as the first argument. '+$
              'Returning without plotting.'
      RETURN           ; this sort of non-fatal error message will be used a lot
   endif
   if KEYWORD_SET(range) AND KEYWORD_SET(levels) then $
      MESSAGE,/cont,'LEVELS and RANGE should not be set simultaneously!'
   if ~KEYWORD_SET(charsize) then charsize=1.
   tit = KEYWORD_SET(title) ? title+'!C' : '!C'
   if falsecolour then begin
      nocolourbar=1
      if ~(falsecolour eq 1 OR falsecolour eq 2 OR falsecolour eq 3) then begin
         MESSAGE,/cont,'FALSECOLOUR must be 1,2,3 if set. Returning '+$
                 'without plotting.'
         RETURN
      endif
   endif
   if ~KEYWORD_SET(nocolourbar) then nocolourbar=0
   if ~KEYWORD_SET(dpcm) then dpcm=59.1 ; = 150 ppi print resolution
   if ~KEYWORD_SET(label) then label=3
   if N_ELEMENTS(xmar) ne 2 then xmar = nocolourbar ? [9,6] : [13,4]
   if N_ELEMENTS(ymar) ne 2 then ymar = [2,4]
   prev_plot=!d.name
   if prev_plot eq 'Z' then begin
      MESSAGE,'Currently in Z buffer. Please pick a physical plotting device.'
      RETURN
   endif
   if falsecolour && (prev_plot eq 'X') then DEVICE,true_color=24
   ;; this was originally done by the EODG colour routines
   if prev_plot eq 'PS' then DEVICE,bits_per_pixel=8
   if KEYWORD_SET(cartesian) then begin
      mercator=0
      stereo=0
      hammer=0
   endif
   advance = !p.multi[1]*!p.multi[2] gt 1

   ;; save current colourtable
   TVLCT, save_ct, /get

   ;; set limits of plot sensibly
   if ~KEYWORD_SET(lim) then if N_ELEMENTS(filter) eq n then $
      lim=[MIN(lat[WHERE(filter)],max=maxlat,/nan), $
           MIN(lon[WHERE(filter)],max=maxlon,/nan), maxlat, maxlon] else $
      lim=[MIN(lat,max=maxlat,/nan), MIN(lon,max=maxlon,/nan), maxlat, maxlon]
   limit = DOUBLE([lim[0] > (-90d0), lim[1], lim[2] < 90d0, lim[3]])

   ;; find points in range, considering LIMIT and FILTER (saves processing time)
   lon1=ATAN(SIN(limit[1]*!DtoR), COS(limit[1]*!DtoR))*!RaDeg
   lon2=ATAN(SIN(limit[3]*!DtoR), COS(limit[3]*!DtoR))*!RaDeg
   if lon1 ge lon2 then begin   ; implies region crosses the dateline
      if N_ELEMENTS(filter) eq n then begin
         ok=WHERE(lat ge limit[0] AND lat le limit[2] AND $
                  ((lon ge lon1 AND lon le 180d0) OR $
                   (lon ge -180d0 AND lon le lon2)) AND filter,nok)
      endif else begin
         ok=WHERE(lat ge limit[0] AND lat le limit[2] AND $
                  ((lon ge lon1 AND lon le 180d0) OR $
                   (lon ge -180d0 AND lon le lon2)) AND FINITE(pts),nok)
      endelse
   endif else begin
      if N_ELEMENTS(filter) eq n then begin
         ok=WHERE(lat ge limit[0] AND lat le limit[2] AND lon ge limit[1] AND $
                  lon le limit[3] AND filter,nok)
      endif else begin
         ok=WHERE(lat ge limit[0] AND lat le limit[2] AND lon ge limit[1] AND $
                  lon le limit[3] AND FINITE(pts),nok)
      endelse
   endelse
   if nok eq 0 then begin
      MESSAGE,/cont,'No points within given lat/lon limits. Returning '+ $
              'without plotting.'
      RETURN
   endif

   ;;--------------------------------------------------------------------------
   ;; select background colour
   if !d.name eq 'PS' || KEYWORD_SET(white) then white_back=1 else white_back=0
   background_save = !p.background
   background = white_back ? (falsecolour ? 16777215 : 255) : 0
   !p.background = background

   ;; set map grid in current plotting area and read its position with
   ;; other properties
   if ~KEYWORD_SET(centre) then begin
      if KEYWORD_SET(cartesian) then BEGIN
         IF (lim[1] ge lim[3]) THEN $ ;implies region crossed dateline
            centre = [0.,0.5*((-180-(180-lim[1]))+lim[3])] $
         ELSE centre=[0.,.5*(lim[1]+lim[3])]
         ENDIF else centre=.5*[lim[0]+lim[2], lim[1]+lim[3]]
   endif
   ;; AJAS Added color keyword so that the white background gets a black title.
   MAP_SET,limit=limit,advance=advance,xmar=xmar,ymar=ymar,/noborder, $
           title=tit,charsize=charsize,isotropic=isotropic,position=pos, $
           noerase=noerase,central=azi,centre[0],centre[1],robinson=robinson, $
           COLOR= white_back ? 0 : (falsecolour ? 16777215 : 255)
   pos=[!x.window[0],!y.window[0],!x.window[1],!y.window[1]]

   ;; check if plot has scalable pixels and determine plot grid size
   if !d.flags AND 1 then begin
      xs=ROUND(dpcm/!d.x_px_cm*(pos[2]-pos[0])*!d.x_size)
      ys=ROUND(dpcm/!d.y_px_cm*(pos[3]-pos[1])*!d.y_size)
   endif else begin
      xs=FLOOR((pos[2]-pos[0])*!d.x_size) ; DEVICE units
      ys=FLOOR((pos[3]-pos[1])*!d.y_size)
   endelse

   ;; save map plot
   psave=!p & xsave=!x & ysave=!y & dsave=!d

   ;; set plotting symbol
   if KEYWORD_SET(symsize) then begin
      if ~KEYWORD_SET(psym) then begin
         if KEYWORD_SET(squares) then $
            USERSYM,[-1,1,1,-1,-1],[1,1,-1,-1,1],/fill $
         else begin
            circle=!pi/8.*FINDGEN(16)
            USERSYM,COS(circle),SIN(circle),/fill
         endelse
         psym=8
      endif
   endif else if ~KEYWORD_SET(psym) then psym=3

   ;; load the Z buffer, set to the size of the plotting area
   SET_PLOT,'Z',/copy
   !p.background = background
   ERASE
   DEVICE,get_pixel_depth=pix_depth
   DEVICE,set_res=[xs,ys],z_buffer=0,set_pixel_depth=falsecolour ? 24 : 8
   ;; set character size on the Z buffer so SYMSIZE works as you would expect
   if prev_plot eq 'PS' $
   then DEVICE,set_char=ROUND([dsave.x_ch_size*dpcm/dsave.x_px_cm, $
                               dsave.y_ch_size*dpcm/dsave.y_px_cm]) $
   else DEVICE,set_char=[dsave.x_ch_size,dsave.y_ch_size]

   ;; make a map that fills the entire buffer
   MAP_SET,limit=limit,/noborder,position=[0.,0.,1.,1.],centre[0],centre[1], $
           mercator=mercator,stereo=stereo,hammer=hammer,central=azi, $
           robinson=robinson

   ;; plot data points
   if falsecolour then begin
      if ~KEYWORD_SET(silent) then PRINT,!error_state.msg_prefix+'MAPPOINTS: '+$
                                         'Producing a false colour image.'

      ;; use keyword to split off colour components
      case falsecolour of
         1: begin
            if ~KEYWORD_SET(fcnorm) then begin
               fc=[[MIN((pts[0,*,*])[ok],max=maxf,/nan),maxf], $
                   [MIN((pts[1,*,*])[ok],max=maxf,/nan),maxf], $
                   [MIN((pts[2,*,*])[ok],max=maxf,/nan),maxf]]
               fcnorm=[MIN(fc[0,*]),MAX(fc[1,*])]
            endif
            red=BYTSCL((pts[0,*,*])[ok],min=fcnorm[0],max=fcnorm[1],/nan)
            green=BYTSCL((pts[1,*,*])[ok],min=fcnorm[0],max=fcnorm[1],/nan)
            blue=BYTSCL((pts[2,*,*])[ok],min=fcnorm[0],max=fcnorm[1],/nan)
         end
         2: begin
            if ~KEYWORD_SET(fcnorm) then begin
               fc=[[MIN((pts[*,0,*])[ok],max=maxf,/nan),maxf], $
                   [MIN((pts[*,1,*])[ok],max=maxf,/nan),maxf], $
                   [MIN((pts[*,2,*])[ok],max=maxf,/nan),maxf]]
               fcnorm=[MIN(fc[0,*]),MAX(fc[1,*])]
            endif
            red=BYTSCL((pts[*,0,*])[ok],min=fcnorm[0],max=fcnorm[1],/nan)
            green=BYTSCL((pts[*,1,*])[ok],min=fcnorm[0],max=fcnorm[1],/nan)
            blue=BYTSCL((pts[*,2,*])[ok],min=fcnorm[0],max=fcnorm[1],/nan)
         end
         3: begin
            if ~KEYWORD_SET(fcnorm) then begin
               fc=[[MIN((pts[*,*,0])[ok],max=maxf,/nan),maxf], $
                   [MIN((pts[*,*,1])[ok],max=maxf,/nan),maxf], $
                   [MIN((pts[*,*,2])[ok],max=maxf,/nan),maxf]]
               fcnorm=[MIN(fc[0,*]),MAX(fc[1,*])]
            endif
            red=BYTSCL((pts[*,*,0])[ok],min=fcnorm[0],max=fcnorm[1],/nan)
            green=BYTSCL((pts[*,*,1])[ok],min=fcnorm[0],max=fcnorm[1],/nan)
            blue=BYTSCL((pts[*,*,2])[ok],min=fcnorm[0],max=fcnorm[1],/nan)
         end
      endcase                  ; no ELSE as already dealt with at top of program

      ;; process colours
      if prev_plot eq 'PS' then TVLCT,BINDGEN(256),BINDGEN(256),BINDGEN(256)
      fcolour=TEMPORARY(red) + 256l*(TEMPORARY(green) +256l*TEMPORARY(blue))

      ;; plot points
      for j=0l,nok-1 do $
         PLOTS,lon[ok[j]],lat[ok[j]],psym=psym,symsize=symsize,color=fcolour[j]
   endif else begin    ; ~falsecolour ------------------------------------------
      ;; deal with LEVELS and assign initial colour index for each valid point
      if ~KEYWORD_SET(range) then if KEYWORD_SET(log) $
      then range=DOUBLE([MIN(pts[ok[WHERE(pts[ok] gt 0.)]],max=maxr,/nan),maxr]) $
      else range=DOUBLE([MIN(pts[ok],max=maxr,/nan),maxr])
      if range[0] eq range[1] then range[range[0] le 0] = range[0] eq 0
      if KEYWORD_SET(levels) then begin
         if KEYWORD_SET(nlevels) then $
            MESSAGE,/cont,'LEVELS and NLEVELS should not be set simultaneously.'
         nlevels=N_ELEMENTS(levels)
         index=INTARR(nok)

         ;; deal with potentially non-uniform levels given by user
         q=WHERE(pts[ok] lt levels[0],nq)
         if nq gt 0 then index[q]=0 ; below first level
         for i=1l,nlevels-1 do begin
            q=WHERE(pts[ok] ge levels[i-1] AND pts[ok] lt levels[i],nq)
            if nq gt 0 then index[q]=i
         endfor
         q=WHERE(pts[ok] ge levels[nlevels-1],nq)
         if nq gt 0 then index[q]=nlevels ; above top level
      endif else begin
         if ~KEYWORD_SET(nlevels) then $
            nlevels = KEYWORD_SET(mycolours) ? N_ELEMENTS(mycolours)/3 : 16
         if KEYWORD_SET(log) then begin
            levels=10d0^(ALOG10(range[0])+ALOG10(range[1]/range[0])/ $
                         (nlevels-1d0)*DINDGEN(nlevels))
            index=1+(nlevels-1d0)/ALOG10(range[1]/range[0])* $
                  ALOG10(pts[ok]/range[0])
         endif else begin
            levels=range[0]+(range[1]-range[0])/(nlevels-1d0)*DINDGEN(nlevels)
            index=1+(nlevels-1d0)/(range[1]-range[0])*(pts[ok]-range[0])
         endelse
      endelse
      if TOTAL(~FINITE(levels)) gt 0 then MESSAGE, 'Bad levels. Check range.'

      ;; deal with colour table
      colours=nlevels
      if KEYWORD_SET(mycolours) then begin
         smc=SIZE(mycolours)
         if smc[0] eq 2 && smc[1] eq nlevels && smc[2] eq 3 then begin
            tempcol=BYTARR(nlevels+2,3)
            tempcol[1:nlevels,*]=mycolours
            tempcol[nlevels+1,*]=255b
            TVLCT, tempcol
         endif else MESSAGE,'MYCOLOURS must be an [nlevels,3] byte array.'
      endif else if N_ELEMENTS(colourtable) eq 1 then begin
         if colourtable eq 0 then begin
            LOADCT,/silent,colourtable,ncolors=colours+2,rgb_table=rgb,/bottom
         endif else begin
            LOADCT,/silent,colourtable,ncolors=colours,rgb_table=rgb_,/bottom
            ;; force first indices to be black/white
            rgb=BYTARR(colours+2,3)
            rgb[0,*] = 0
            rgb[1:colours,*] = rgb_
            rgb[colours+1,*] = 255
         endelse

         if KEYWORD_SET(back) $
         then TVLCT,REVERSE(rgb[*,0]),REVERSE(rgb[*,1]),REVERSE(rgb[*,2]) $
         else TVLCT,rgb
      endif $
      else if KEYWORD_SET(diffcolourbar) then $
         ;; colours+1 as tables use 0=black and ncol=white
         COLOUR_WHTEDIFF,colours+1,back=back $
      else if KEYWORD_SET(diffcolourgrey) then $
         COLOUR_WHTEDIFF,colours+1,/grey,back=back $
      else if KEYWORD_SET(nogrey) then $
         COLOUR_PS,colours+1,/no_grey,back=back,green=gplt, $
         red=rplt,rywdiff=rywdiff $
      else if KEYWORD_SET(rywdiff) then COLOUR_PS,colours+1,/rywdiff $
      else if KEYWORD_SET(d2colourbar) then COLOUR_PS,colours+1,/bwr2diff $
      else if KEYWORD_SET(omi) then COLOUR_PS,colours+1,/omi $
      else COLOUR_CBW,colours,2 ; the second argument is GREYS

      ;; assign colour values for points outside of designated range
      IF (KEYWORD_SET(cb_top_colour) OR KEYWORD_SET(cb_bot_colour)) THEN BEGIN
         TVLCT,r,g,b,/GET

         IF KEYWORD_SET(cb_top_colour) THEN BEGIN
            r[colours+1] = cb_top_colour[0]
            g[colours+1] = cb_top_colour[1]
            b[colours+1] = cb_top_colour[2]
         ENDIF
         IF KEYWORD_SET(cb_bot_colour) THEN BEGIN
            r[colours+2] = cb_bot_colour[0]
            g[colours+2] = cb_bot_colour[1]
            b[colours+2] = cb_bot_colour[2]
         ENDIF

         TVLCT,r,g,b

         IF KEYWORD_SET(cb_top_colour) THEN top_colour=colours+1
         IF KEYWORD_SET(cb_bot_colour) THEN bot_colour=colours+2
      ENDIF ELSE if KEYWORD_SET(mycolours) || KEYWORD_SET(nogrey) || $
         KEYWORD_SET(diffcolourbar) || KEYWORD_SET(diffcolourgrey) || $
         KEYWORD_SET(rywdiff) || KEYWORD_SET(d2colourbar) || $
         KEYWORD_SET(diffcolourgrey) then begin
         bot_colour=1
         top_colour=colours
      endif else if KEYWORD_SET(omi) || N_ELEMENTS(colourtable) eq 1 then begin
         if N_ELEMENTS(colourtable) eq 1 && colourtable eq 0 then begin
            bot_colour=0
            top_colour=colours
         endif else begin
            ;; invent two greys for bottom/top colours (ACP: Rather poor soln)
            TVLCT,rgb,/get
            rgb[colours+1,*]=85
            rgb[colours+2,*]=170
            TVLCT,rgb

            bot_colour=colours+2
            top_colour=colours+1
         endelse
      endif else begin
         bot_colour=colours+1
         top_colour=colours+2
      endelse
      q=WHERE(index gt nlevels,nq) & if nq gt 0 then index[q]=top_colour
      q=WHERE(index lt 1,nq) & if nq gt 0 then index[q]=bot_colour

      ;; plot points
      for j=0l,nok-1 do $
         PLOTS,lon[ok[j]],lat[ok[j]],psym=psym,symsize=symsize,color=index[j]
   endelse
   TVLCT, my_ct, /get
   image=TVRD(true=falsecolour)

   ;; revert Z buffer and plot to its original state
   ERASE
   DEVICE,set_pixel_depth=pix_depth,z_buffer=1
   SET_PLOT,prev_plot,/copy
   !p=psave & !x=xsave & !y=ysave

   ;; draw image
   if falsecolour then begin
      DEVICE,get_decomposed=decomp & DEVICE,/decomposed
   endif
   if !d.flags AND 1 $
   then TV,image,pos[0],pos[1],xsize=(pos[2]-pos[0]), $
           ysize=(pos[3]-pos[1]),/norm,true=falsecolour ? 3:0 $
   else TV,image,pos[0]*!d.x_size,pos[1]*!d.y_size,true=falsecolour ? 3:0
   if falsecolour then if ~decomp then DEVICE,decomposed=0

   ;; add desired features
   if KEYWORD_SET(colour) then TVLCT, save_ct else begin
      LOADCT,0,/silent
      colour = white_back ? 0 : 255
   endelse
   if ~KEYWORD_SET(nogrid) then $
      MAP_GRID,box=~KEYWORD_SET(nobox),label=label,/nogrid, $
               charsize=charsize,color=colour
   if KEYWORD_SET(nogrid) || KEYWORD_SET(nobox) then $
      ;; draw box as I skipped it earlier
      PLOTS,[pos[0],pos[2],pos[2],pos[0],pos[0]], $
            [pos[1],pos[1],pos[3],pos[3],pos[1]],color=colour,/normal
   if ~KEYWORD_SET(nocontinents) then $
      MAP_CONTINENTS,hires=hires,countries=countries,usa=usa, $
                     coast=KEYWORD_SET(coast) || KEYWORD_SET(countries), $
                     rivers=rivers,color=colour
   if KEYWORD_SET(plotpoints) then  $
      PLOTS,plotpoints[*,1],plotpoints[*,0],psym=plotpsym,symsize=2, $
            thick=3,color=colour

   if ~nocolourbar then begin
;     cw=CONVERT_COORD(!d.x_ch_size,0,0,/device,/to_norm)
;     bpos=[pos[0]-5*cw[0],pos[1],pos[0]-3*cx[0],pos[3]]
      if N_ELEMENTS(bpos) ne 4 then $
         bpos=[pos[0]*!d.x_size-5*!d.x_ch_size,pos[1]*!d.y_size, $
               pos[0]*!d.x_size-3*!d.x_ch_size,pos[3]*!d.y_size]

      TVLCT, my_ct
;     if !d.flags AND 1 $
;       then TV,BINDGEN(1,nlevels-1)+1b,bpos[0],bpos[1],xsize=bpos[2]-bpos[0],ysize=bpos[3]-bpos[1] $
;       else TV,CONGRID(BINDGEN(1,nlevels-1)+1b, bpos[2]-bpos[0], bpos[3]-bpos[1]),bpos[0],bpos[1]
      pointx=[bpos[0],.5*(bpos[0]+bpos[2]),bpos[2]]
      pointy1=bpos[1]+[0,-2*!d.x_ch_size,0] & pointy2=bpos[3]-[1,-2*!d.x_ch_size,1]
      POLYFILL,pointx,pointy1,color=bot_colour,/device
      POLYFILL,pointx,pointy2,color=top_colour,/device
      if KEYWORD_SET(colourbar_labels) then begin
         ;; an equally spaced set of flags
         spacing=(levels[nlevels-1]-levels[0])/(nlevels-1d0)
         mid_lev=levels+0.5*spacing
         ext_lev=[levels,levels[nlevels-1]+1]
         yrange=[levels[0],levels[nlevels-1]+spacing]
         CONTOUR,ext_lev##[1,1],[0,1],ext_lev,c_colors=INDGEN(nlevels)+1, $
                 /device,levels=levels,/fill, $
                 /noerase,position=bpos,xstyle=4, $
                 xtickname=[' ',' '],xticks=1,ylog=log,ystyle=5, $
                 yrange=yrange, $
                 ytickname=colourbar_labels,ytitle=units, $
                 yticks=N_ELEMENTS(colourbar_labels)-1,ytickv=mid_lev
         if KEYWORD_SET(colour) then TVLCT, save_ct else LOADCT,0,/silent
         AXIS,charsize=charsize,color=colour,yaxis=0,ylog=log,ystyle=1, $
              yrange=yrange,yticklen=.2, $
              ytickname=colourbar_labels,ytitle=units, $
              yticks=N_ELEMENTS(colourbar_labels)-1,ytickf=btickf,ytickv=mid_lev
         AXIS,charsize=charsize,color=colour,yaxis=1,ylog=log,ystyle=1, $
              yrange=yrange,yticklen=.2, $
              ytickname=REPLICATE(' ',N_ELEMENTS(colourbar_labels)), $
              ytitle=units,yticks=N_ELEMENTS(colourbar_labels)-1,ytickv=mid_lev
      endif else begin
         ;; if less than keyticks levels, label them exactly.
         ;; Otherwise, label axis normally.
         if ~KEYWORD_SET(keyticks) then keyticks=6
         CONTOUR,levels##[1,1],[0,1],levels,c_colors=INDGEN(nlevels)+1, $
                 charsize=charsize,color=colour,/device,/fill,levels=levels, $
                 /noerase,position=bpos,xminor=1,xstyle=4,xtickname=[' ',' '], $
                 xticks=1,ylog=log,ystyle=5,yrange=levels[[0,nlevels-1]], $
                 yticklen=.2,ytitle=units,yticks=(nlevels-1) < keyticks
         if KEYWORD_SET(colour) then TVLCT, save_ct else LOADCT,0,/silent
         AXIS,charsize=charsize,color=colour,yaxis=0,ylog=log,ystyle=1, $
              yrange=levels[[0,nlevels-1]],yticklen=.2,ytitle=units, $
              yticks=(nlevels-1) < keyticks,ytickf=btickf
         ;; AJAS removed YTITLE=units from the following AXIS command.
         AXIS,charsize=charsize,color=colour,yaxis=1,ylog=log,ystyle=1, $
              yrange=levels[[0,nlevels-1]],yticklen=.2, $
              ytickname=REPLICATE(' ',10), $
              yticks=(nlevels-1) < keyticks
      endelse
      PLOTS,pointx,pointy1,/device,color=colour
      PLOTS,pointx,pointy2,/device,color=colour

      ;; reset plotting window
      !p=psave & !x=xsave & !y=ysave
      !p.background = background_save
   endif

   TVLCT, save_ct
   if KEYWORD_SET(stp) then STOP
END
