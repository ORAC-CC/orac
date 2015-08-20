;+
pro mappoints_old, pts_in, lat_in, lon_in, Limit=Limit, Nlevels=NLevels, $
               Levels=Levels, Log=Log, Range=Range, nogrid=nogrid, nobox=nobox,$
               nocontinents=nocontinents, map=map, scale=scale, $
               filter=filter, centre=centre, symsize=symsize, $
               nocolourbar=nocolourbar, title=title, units=units, $
               Hires=Hires, mercator=mercator, stereo=stereo, hammer=hammer,$
               cartesian_projection=cartesian, isotropic=isotropic,$
               colourtable=colourtable, plotpoints=plotpoints, $
               help=help,diffcolourbar=diffcolourbar,d2colourbar=d2colourbar, $
               diffcolourgrey=diffcolourgrey, falsecolour=falsecolour, $
               mycolours=mycolours, $
               fcnorm=fcnorm, nogrey=nogrey, plotpsym=plotpsym, $
               colourbar_labels=colourbar_labels, coast=coast, $
               countries=countries, usa=usa, rivers=rivers, green=greenplot, $
               red=redplot, back=back, rywdiff=rywdiff, $
               charsize=charsize, psym=psym, squares=squares, $
               keyticks=keyticks, plotpos=plotpos


;
; Plots an array of values, with corresponding Lat/Lon values,
; on to a map
;
; REVISION HISTORY
; GET 17 Aug 05 Adapted from mapgrapel2.
; GET 22 Sep 06 Added fill_grid keyword.
; GET 18 Dec 06 Added diffcolourgrey keyword
; GET 19 Dec 06 Added coast keyword
; GET 25 Sep 07 Added code to deal with lat/lon inputs outside
;               the acceptable range
; GET 09 Oct 07 Added fcnorm keyword and altered false colour code to
;               deal with image arrays with -ve values.
; AS2 30 Mar 09 Only set decomposed=0 after fc image if !d.name ='X'
; AS2 18 Aug 09 Added countries, usa, green, red, and back keywords as
;               well as an option for a cartesian projection of lat
;               and longitude and /isotropic.
; AMS 16 Apr 10 Added /rywdiff keyword for red-yellow-white-grey colour
;               bar option added to colour_ps
; GET 30 Jun 10 Added plotpos keyword for returning the coordinates of
;               the main plot window.
;               Also removed the fill_grid keyword, which never worked
;               properly anyway.
; AMS 08 Jul 10 Added rivers keyword to display rivers
; CPA 15 Oct 10 Added PLOTPSYM keyword
; CPA 2  Nov 10 Added d2colourbar keyword
; AS2 15 May 12 Changed colourtable handling.
; AS2 03 Sep 12 Changed handling of LEVELS so that irregular levels
;               can be used. Added warnings about conflicting use of
;               LEVELS, NLEVELS, and RANGE. Added MYCOLOURS keyword.
; ACP 15 Jul 13 Superceded with MAPPOINTS_NEW and made wrapper to allow
;               calls to this version with the keyword /OLD.
;
; KNOWN BUGS
; none known

; REQUIRED PARAMETERS
; pts    an array containing the points to be plotted
; lat    an array containing the latitude for each point in pts
; lon    an array containing the longitude for each point in pts
; NOTE: pts, lat and lon MUST have the same number of elements

; OPTIONAL PARAMETERS
; LIMIT = [min_lat,min_lon,max_lat,max_lon]
;              Defines the latitude/longitude range of the plot
; NLEVELS     = Integer defining the number of colour levels to use
; LEVELS      = Array of values defining the colour levels to use
; /LOG          plots a log image
; RANGE = [min,max]
; /NOCONTINENTS forces continents to NOT be drawn
; /NOGRID       forces grid NOT to be drawn
; /NOCOLOURBAR  forces colourbar to NOT be drawn
; COLOURTABLE = uses the specified predefined IDL colour table (see
;               LOADCT in the IDL help)
; MYCOLOURS   = Passes a custom set of colour values [nlevels,3] to
;               use. This should override COLOURTABLE use.
; PLOTPOINTS =  A Nx2 array specifying points to be overploted (as
;               crosses) in lat (1st row)/lon (2nd row)
; PLOTPSYM   =  Sets plotting symbol for points overplotted by
;		PLOTPOINTS
; /DIFFCOLOURBAR Uses the blue-white-red colour bar. Useful for
;               displaying differences (eg. residuals)
; /d2colourbar Uses the purple-blue-white-yellow-red colour bar. Useful for
;               displaying differences (eg. residuals)
; FALSECOLOUR = {1|2|3}
;               Produces a false colour RGB image. If the value is
;               set to 1 the input array must be a 3*nx*ny array,
;               2 implies nx*3*ny and 3 implies nx*ny*3. The colour
;               channels are ordered red,green,blue.
;               ONLY WORKS FOR SCREEN OUTPUT (since true-colour
;               plotting to postscript is not supported by IDL)
; FCNORM = [min,max] Define the normalisation to apply to the pixels
;               when producing a false colour image. If not set the
;               minimum and maximum values of the data array will be
;               used.
; PSYM =        Allows the user to overide the default plotting symbol
;               (dots if symsize isn't set, circles if it is)
; /SQUARES      Uses filled squares (insted of circles) for ploting
; PLOTPOS = variable
;               If set to a named variable, this keyword will return
;               the position coordinates of the main plot. This can
;               be used to overplot onto a previously produced
;               mappoints map, using the position and noerase keywords
;               to plot etc.
; /COUNTRIES    Passed to map_continents, this draws national borders as well
;               as continental ones.
; /RIVERS       Passed to map_continents, this draws rivers. Note that
;               there's an awful lot of rivers in the world to draw!
; /USA          Passed to map_continents, this draws USA state borders.
;-
  COMPILE_OPT HIDDEN
  IF keyword_set(help) THEN BEGIN
      print,'pro mappoints, pts, lat, lon, Limit=Limit, Nlevels=NLevels, $'
      print,'Levels=Levels, Log=Log, Range=Range, nogrid=nogrid, $'
      print,'nocontinents=nocontinents, map=map, scale=scale, $'
      print,'filter=filter, centre=centre, symsize=symsize, $'
      print,'nocolourbar=nocolourbar, title=title, units=units, $'
      print,'Hires=Hires, mercator=mercator, stereo=stereo, $'
      print,'colourtable=colourtable, plotpoints=plotpoints, help=help, $'
      print,'diffcolourbar=diffcolourbar, d2colourbar=d2colourbar, $'
      print,'plotpsym=plotpsym'
      print,''
      print,'Plots an array of values, with corresponding Lat/Lon values,'
      print,'on to a map'
      print,'REQUIRED PARAMETERS'
      print,'pts    an array containing the points to be plotted'
      print,'lat    an array containing the latitude for each point in pts'
      print,'lon    an array containing the longitude for each point in pts'
      print,'NOTE: pts, lat and lon MUST have the same number of elements'
      print,'NOTE: that the keywords apply to all the plots drawn in this call.'
      print,'If you want maps with different keywords then plot them individually'
      print,'OPTIONAL PARAMETERS'
      print,'LIMIT = [min_lat,min_lon,max_lat,max_lon]'
      print,'              Defines the latitude/longitude range of the plot'
      print,'NLEVELS     = Integer defining the number of colour levels to use'
      print,'LEVELS      = Array of values defining the colour levels to use'
      print,'/LOG          plots a log image'
      print,'RANGE = [min,max]'
      print,'/NOCONTINENTS forces continents to NOT be drawn'
      print,'/NOGRID       forces grid NOT to be drawn'
      print,'/NOCOLOURBAR  forces colourbar to NOT be drawn'
      print,'COLOURTABLE = uses the specified predefined IDL colour table (see'
      print,'              LOADCT in the IDL help)'
      print,'PLOTPOINTS =  A Nx2 array especifying points to be overploted (as'
      print,'              crosses) in lat (1st row)/lon (2nd row)'
      print,'PLOTPSYM   =  Sets plotting symbol for points overplotted by PLOTPOINTS'
      print,'/DIFFCOLOURBAR Uses the blue-white-red colour bar. Useful for'
      print,'              displaying differences (eg. residuals)'
      print,'/d2colourbar Uses the purple-blue-white-yellow-red colour bar. Useful for'
      print,'              displaying differences (eg. residuals)'
      print,'FALSECOLOUR = {1|2|3}'
      print,'              Produces a false colour RGB image. If the value is'
      print,'              set to 1 the input array must be a 3*nx*ny array,'
      print,'              2 implies nx*3*ny and 3 implies nx*ny*3. The colour'
      print,'              channels are ordered red,green,blue.'
      print,'              ONLY WORKS FOR SCREEN OUTPUT (since true-colour'
      print,'              plotting to postscript is not supported by IDL)'
      print,'FCNORM = [min,max] Define the normalisation to apply to the pixels'
      print,'               when producing a false colour image. If not set the'
      print,'               minimum and maximum values of the data array will be'
      print,'               used.'
      print,'PSYM =       Allows the user to overide the default plotting symbol'
      print,"              (dots if symsize isn't set, circles if it is)"
      print,'/SQUARES     Uses filled squares (insted of circles) for ploting'
      print,'/COUNTRIES    Passed to map_continents, this draws national borders as well as continental ones.'
      print,'/RIVERS       Passed to map_continents, this draws rivers. Note that  there are an awful lot of rivers in the world to draw!'
      print,'/USA          Passed to map_continents, this draws USA state borders.'

      return
  ENDIF

; Save the incoming colourtable, to restore at the end.
tvlct, save_ct, /get

; Check input data
Nz = n_elements(pts_in)
if keyword_set(falsecolour) then Nz = Nz/3

if (n_elements(lat_in) ne Nz or n_elements(lon_in) ne Nz) then begin
   print, 'Latitude and Longitude arrays must be specified and'
   print, 'must have the same number of elements as pts.'
   print, 'MAPPOINTS returning without plotting'
   return
endif
If KEYWORD_SET(range) AND KEYWORD_SET(levels) THEN MESSAGE, /CONTINUE, $
   'Keywords LEVELS and RANGE should not be set simultaneously! '+$
   'Results may not be as expected...'

; Check for acceptable lat/lons
ok_loc = where(lat_in ge  -90.0 and lat_in le  90 and $
               lon_in ge -180.0 and lon_in le 180, n_loc)
if n_loc eq 0 then begin
   print, 'Latitude and/or longitude arrays contain no good points'
   print, 'MAPPOINTS returning without plotting'
   return
endif

if keyword_set(falsecolour) then begin
   if falsecolour eq 1 then begin
      pts = reform(pts_in(0,*,*),Nz)
      red = reform(pts_in(0,*,*),Nz)
      green = reform(pts_in(1,*,*),Nz)
      blue = reform(pts_in(2,*,*),Nz)
   endif else if falsecolour eq 2 then begin
      pts = reform(pts_in(*,0,*),Nz)
      red = reform(pts_in(*,0,*),Nz)
      green = reform(pts_in(*,1,*),Nz)
      blue = reform(pts_in(*,2,*),Nz)
   endif else if falsecolour eq 3 then begin
      pts = reform(pts_in(*,*,0),Nz)
      red = reform(pts_in(*,*,0),Nz)
      green = reform(pts_in(*,*,1),Nz)
      blue = reform(pts_in(*,*,2),Nz)
   endif else begin
      print,'MAPPOINTS: Falsecolour must be 1, 2, 3 if set'
      print,'           Returning without plotting'
      return
   endelse
   lat = reform(lat_in,Nz)
   lon = reform(lon_in,Nz)
endif else begin
   pts = reform(pts_in,Nz)
   lat = reform(lat_in,Nz)
   lon = reform(lon_in,Nz)
endelse

; Remove any points which are don't have acceptable lat/lons
pts = pts[ok_loc]
lat = lat[ok_loc]
lon = lon[ok_loc]
if keyword_set(falsecolour) then begin
   red = red[ok_loc]
   green = green[ok_loc]
   blue = blue[ok_loc]
endif
if keyword_set(filter) then filter = filter[ok_loc]

; Number of colour or grey levels
  If Not Keyword_Set(Nlevels) Then Begin
     If Keyword_Set(levels) Then BEGIN
        NLevels = N_Elements(Levels)
     ENDIF ELSE BEGIN
        IF KEYWORD_SET(mycolours) THEN NLevels = N_ELEMENTS(mycolours)/3 $
                                  ELSE NLevels = 16
     ENDELSE
  Endif ELSE IF KEYWORD_SET(LEVELS) THEN MESSAGE,/CONTINUE,$
     'You should not set LEVELS and NLEVELS simultaneously. Results will '+$
     'not be as expected!'

; Set the number of colours in the colour table
  Colours = NLevels
  if Not keyword_set(falsecolour) then begin
      IF KEYWORD_SET( mycolours ) THEN BEGIN
         smc = SIZE( mycolours )
         IF smc[0] EQ 2 && smc[1] EQ NLevels && smc[2] EQ 3 THEN BEGIN
            TVLCT, mycolours
         ENDIF ELSE BEGIN
            HELP, mycolours, NLevels
            MESSAGE,'MYCOLOURS must be an [nlevels,3] byte array'
         ENDELSE
      ENDIF ELSE IF n_elements(colourtable) EQ 1 THEN BEGIN
          loadct,colourtable,ncolors=Colours,/silent, rgb_table=rgb_loadct
          IF KEYWORD_SET(back) THEN BEGIN

             TVLCT, REVERSE( rgb_loadct[*,0] ), $
                    REVERSE( rgb_loadct[*,1] ), $
                    REVERSE( rgb_loadct[*,2] )

          ENDIF ELSE BEGIN
             TVLCT, rgb_loadct
          ENDELSE
;          ; Add a black on the end...
;          tvlct, my_ct, /get
;          tvlct, [ my_ct, [[0],[0],[0]] ]
;          my_black = colours
       ENDIF ELSE BEGIN
          ;my_black = !null
          Greys = 4 ; 4 implies black, dark grey, light grey, white in grey part of colour table
          IF Keyword_Set(diffcolourbar) then colour_whtediff,Colours+1,back=back $
          ELSE IF Keyword_Set(diffcolourgrey) then colour_whtediff,Colours+1,/grey,back=back $
          ELSE IF Keyword_Set(nogrey) then colour_ps,colours+1,/no_grey,$
                                          green=greenplot,red=redplot,back=back,rywdiff=rywdiff $
          ELSE IF Keyword_Set(rywdiff) then colour_ps,colours+1,rywdiff=rywdiff $
          ELSE IF Keyword_Set(d2colourbar) then colour_ps,colours+1,/bwr2diff $
          ELSE colour_cbw,Colours,Greys
      ENDELSE
  endif

  ; Get the main colour table.
  tvlct, my_ct, /get

; Multi plot
  NPlots = !P.multi(1) * !P.multi(2)
  Advance  = NPlots Gt 1 ; 0: 1 plot per page; 1 more than 1 plot per page
                         ; i.e advance = 1 if more than one plot per page
; Unscale the fonts
  If not keyword_set(charsize) then $
    If (NPlots Le 4) then Charsize=1 Else Charsize=1.

  If Not Keyword_set(Title) Then Title = ''
  IF NOT Keyword_set(Units) THEN Units = ''


; Detemine values in Lat Lon range
  Inpic = Replicate(1,Nz)         ; Plot all pixels
  If Keyword_Set(Limit) Then Begin   ; otherwise just those inside the region of interest
    Limit = Double(Limit)            ; Yes double precision is necessary
  EndIf Else Begin
    minlat = min(lat,max=maxlat,/nan)
    minlon = min(lon,max=maxlon,/nan)
    Limit = double([minlat,minlon,maxlat,maxlon])
  Endelse
; Place limits in range [-180,180] as this is how the data is
  Lon1 = atan(sin(Limit(1)*!dtor),cos(Limit(1)*!dtor))*!radeg
  Lon2 = atan(sin(Limit(3)*!dtor),cos(Limit(3)*!dtor))*!radeg
  If  (Lon1 Gt Lon2) then begin ;  implies region crosses the dateline
    Inpic = (Lat Ge Limit(0)) And (Lat Le Limit(2)) And $
            (((Lon Ge Lon1) And (Lon Le 180D0)) Or $
             ((Lon Ge -180D0) And (Lon Le Lon2)))
  EndIf Else $
    Inpic = (Lat Ge Limit(0)) And (Lat Le Limit(2)) And $
            (Lon Ge Limit(1)) And (Lon Le Limit(3))

; Determine good values
  If Keyword_Set(Filter) Then $
    OK = where(Inpic And Filter , Good) $
  Else $
    OK = where(Inpic, Good)

  If (Good Eq 0) Then begin
    print,'No good values to plot'
    Return
  EndIf

; Map edge coordinates
  latmin = min (lat(OK))
  latmax = max (lat(OK))
  Lontemp = lon(OK)

  if (max(Lon(OK))-min(Lon(OK)) Gt 359.0) then begin ; image straddles -180/180 meridian
    Q = where(Lontemp Lt 0.0,Count)
    if (Count gt 0) then Lontemp(Q) = 360.0+lontemp(Q)
  EnDif

  lonmin = min (LonTemp)
  lonmax = max (LonTemp)

  If Not Keyword_Set(Limit) Then Limit = [Latmin,LonMin,Latmax,Lonmax]

  If Keyword_Set(Centre) Then Begin
    latcentre = centre(0)
    loncentre = centre(1)
  EndIf Else Begin
    latcentre = 0.5*(limit(0)+limit(2))
    loncentre = 0.5*(limit(1)+limit(3))
  EndElse

; Range of values plotted
  If Not Keyword_Set(Range) Then Range = [min(pts(ok),/nan), max(pts(ok),/nan)]

; Colour Level Values
  If Not Keyword_Set(Levels) Then Begin
    If keyword_set(Log) Then $
      Levels = 10^(Alog10(Range(0)) + Alog10(Range(1)/Range(0)) * Findgen(NLevels)/(NLevels-1)) $
    Else $
      Levels = Range(0) + (Range(1)-Range(0)) * Findgen(NLevels)/(NLevels-1)
; Colour Level Indices
    If keyword_set(Log) Then $
       Index = 1 + (NLevels - 1) * Alog10(pts(ok)/ Range(0)) / Alog10(Range(1)/Range(0)) $
    Else $
       Index = 1 + (NLevels - 1) * (pts(ok) - Range(0)) / (Range(1)-Range(0))

  Endif ELSE BEGIN
     ; AJAS Fix for non-uniform levels....
     Index = intarr( good )
     FOR il = 0, NLevels-1 DO BEGIN
        CASE il OF
           0l:        qindex = WHERE( pts(ok) LT LEVELS[il], nqi )
           NLevels-1: qindex = WHERE( pts(ok) GE LEVELS[il-1], nqi )
           else:      qindex = WHERE( pts(ok) GE LEVELS[il-1] AND $
                                      pts(ok) LT LEVELS[il], nqi )
        ENDCASE
        IF nqi GT 0 THEN Index[ qindex ] = il
     ENDFOR
;     print, index
;     print, pts(ok)
  ENDELSE

; Set index to grey if we are outside range unless no_grey is set, then
; points below the range will be black (screen) or white (ps) and those
; above will be white (screen) or black (ps)
  if keyword_set(nogrey) then begin
      Q = where(pts(OK) Lt Range(0),Count)
      If (Count Gt 0) Then Index(Q) = Colours
      Q = where(pts(OK) Gt Range(1),Count)
      If (Count Gt 0) Then Index(Q) = 0
  endif else if keyword_set(diffcolourbar) or $
                keyword_set(diffcolourgrey) then begin
      Q = where(pts(OK) Lt Range(0),Count)
      If (Count Gt 0) Then Index(Q) = 1
      Q = where(pts(OK) Gt Range(1),Count)
      If (Count Gt 0) Then Index(Q) = Colours-1
  endif else begin
      Q = where(pts(OK) Lt Range(0),Count)
      If (Count Gt 0) Then Index(Q) = Colours+2
      Q = where(pts(OK) Gt Range(1),Count)
      If (Count Gt 0) Then Index(Q) = Colours+3
  endelse



; Let's do it
; Draw the map
; Set the xmargin appropriately depending on whether we want a colour
; bar or not
if keyword_set(nocolourbar) or keyword_set(falsecolour) then begin
xmarg = [9,6]
ymarg = [2,4]
endif else begin
xmarg = [13,4]
ymarg = [2,4]
endelse

  ;map_set,latcentre,loncentre,Limit=Limit,Advance=Advance,xmargin=[13,4],ymargin=[2,4],Title='!6'+Title+'!C',charsize=charsize,mercator=mercator,stereo=stereo

; Make sure the frame of the plot is the right colour.
tvlct, save_ct
loadct, 0,/silent
if !d.name ne 'PS'  then !p.color = 255

if keyword_set(cartesian) then begin
   map_set, limit=limit,Advance=Advance,xmargin=xmarg,ymargin=ymarg,$
            Title=Title+'!C',charsize=charsize,isotropic=isotropic
endif else begin
   map_set,latcentre,loncentre,Limit=Limit,Advance=Advance,$
           xmargin=xmarg,ymargin=ymarg,Title=Title+'!C',$
           charsize=charsize,mercator=mercator,stereo=stereo,$
           isotropic=isotropic,hammer=hammer
endelse

  tvlct, my_ct
; plot the points
  if keyword_set(falsecolour) then begin
     ; Plot a false colour image of the radiances
     ; No colour bar...
     nocolourbar=1
     if !d.name eq 'X' then $
        device,/decomposed,true_color=24 ; Set the display to show true colour images
     print,'Producing a false colour image'
     if not keyword_set(fcnorm) then $
        fcnorm = [min([red,green,blue],/nan),max([red,green,blue],/nan)]
     red1   = bytscl(red[OK], min=fcnorm[0], max=fcnorm[1], /nan)
     green1 = bytscl(green[OK], min=fcnorm[0], max=fcnorm[1], /nan)
     blue1  = bytscl(blue[OK], min=fcnorm[0], max=fcnorm[1], /nan)
     colour = red1 + 256l*(green1 + 256l*blue1)
     if !d.name eq 'PS' then begin
        colour = Color_Quan(red1, green1, blue1, r, g, b)
        TVLCT, r, g, b
     endif
     If Keyword_Set(Symsize)  Then Begin
         if keyword_set(squares) then begin
             usersym, [-1,1,1,-1,-1], [1,1,-1,-1,1], /fill
         endif else begin
             CIRCLE = FINDGEN(16)*(!PI*2/16.)
             USERSYM, COS(CIRCLE), SIN(CIRCLE), /FILL
         endelse
         if not keyword_set(psym) then psym = 8
         for j = 0l,Good-1 do plots,lon(OK(j)),lat(OK(j)),$
                                    psym=psym,symsize=symsize,$
                                    color= colour(j)
     EndIf Else Begin
        if not keyword_set(psym) then psym = 3
        for j = 0l,Good-1 do plots,lon(OK(j)),lat(OK(j)),psym=psym,$
                                   color= colour(j)
     Endelse
     ; Re-enable the 8 bit colour maps (if we were plotting to screen).
     if !d.name eq 'X' then device,decomposed=0
     ; Load the black-white colour table for doing the axes etc
     loadct,0
  endif else begin
     ; Plot the data using the colour table
     If Keyword_Set(Symsize)  Then Begin
         if keyword_set(squares) then begin
             usersym, [-1,1,1,-1,-1], [1,1,-1,-1,1], /fill
         endif else begin
             CIRCLE = FINDGEN(16)*(!PI*2/16.)
             USERSYM, COS(CIRCLE), SIN(CIRCLE), /FILL
         endelse
         if not keyword_set(psym) then psym = 8
         for j = 0l,Good-1 do plots,lon(OK(j)),lat(OK(j)),psym=psym, $
                                    color=Index(j),symsize=symsize
     EndIf Else Begin
        if not keyword_set(psym) then psym = 3
        for j = 0l,Good-1 do plots,lon(OK(j)),lat(OK(j)),psym=psym,color=Index(j)
     EndElse
  endelse

  ; pick the black colour for appropriate device with colour table 0
  loadct,0,/silent
  cblack = !d.name eq 'PS' ? 0 : 255
; add the grid
  box = keyword_set( nobox ) ? 0 : 1
  if Not keyword_set(nogrid)  then map_grid, box=box, label=3,/nogrid,$
                                            charsize=charsize,color=cblack
; add the continents
  if Not keyword_set(nocontinents) then map_continents,hires=hires,$
                                                       coast=(KEYWORD_SET(coast) OR $
                                                              KEYWORD_SET(countries)),$
                                                       countries=countries, usa = usa,$
                                                       rivers=rivers,color=cblack
; add user specified points

  tvlct, my_ct
  if not keyword_set(plotpsym) then plotpsym = 1
  IF keyword_set(plotpoints) THEN  $
    plots,plotpoints[*,1],plotpoints[*,0],psym=plotpsym,symsize=2,thick=3


; what is the position of the main plot window?
  plotpos = [!X.Window[0],!Y.Window[0],!X.Window[1],!Y.Window[1]]

  Cw = convert_coord(!D.X_Ch_Size, 0, 0, Device=1, To_Norm = 1)
  Cwx = Cw(0)
  Position = [!X.Window(0) - 5 * Cwx,!Y.Window(0),!X.Window(0) - 3 * Cwx,!Y.Window(1)] ;$


; Draw the scale
  iii0 = keyword_set(colourtable) ? 1 : 0
  if Not keyword_set(nocolourbar) then begin
    if keyword_set(colourbar_labels) then begin
       ; Do the countour
       for iii=0, iii0 do begin
      contour,Levels##replicate(1,11),findgen(11),Levels,fill=1-iii, $
              Levels = Levels,c_color=findgen(NLevels)+1,$
      xticks=1,xminor=1,xtickname=[' ',' '],position= position,/noerase,$
      ytitle=Units,ystyle=1,charsize=charsize,ylog=log,$
              yticks= n_elements(colourbar_labels)-1,$
              ytickname = colourbar_labels, $
              color=iii ? cblack : !null
       loadct,0, /silent
       endfor
    endif else begin
       if ~keyword_set(keyticks) then keyticks=6
       for iii=0,iii0 do begin
      contour,Levels##replicate(1,11)*(1-iii),findgen(11),Levels,fill=1-iii, $
              Levels = Levels,c_color=findgen(NLevels)+1,$
      xticks=1,xminor=1,xtickname=[' ',' '],position= position,/noerase,$
      ytitle=Units,ystyle=1,charsize=charsize,ylog=log,yticks=keyticks,$
              color=iii ? cblack : !null
      loadct,0, /silent
      endfor
    endelse
  endif

; Restore the incoming colour table.
tvlct, save_ct
return

End
