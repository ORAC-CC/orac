; $Id: //depot/Release/IDL_81/idl/idldir/lib/map_continents.pro#1 $
;
; Copyright (c) 1993-2011, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.

;+
; NAME:
;       MAP_CONTINENTS
;
; PURPOSE:
;       The MAP_CONTINENTS procedure draws continental boundaries,
; filled continents, political boundaries, coastlines, and/or rivers,
; over an existing map projection established by MAP_SET. Outlines can
; be drawn in low or high-resolution (if the optional high-resolution
; CIA World Map database is installed). If MAP_CONTINENTS is called
; without any keywords, it draws low-resolution, unfilled continent
; outlines.
;
; MAP_SET must be called before MAP_CONTINENTS to establish the
; projection type, the center of the projection, polar rotation
; and geographic limits.
;
; Keywords not recognized are passed along in _EXTRA to PLOTS and/or
; POLYFILL depending on options requested.
;
; CATEGORY:
;              MAPPING
;
; CALLING SEQUENCE:
;              MAP_CONTINENTS
;
; INPUTS:
;              NONE
;
; INPUT KEYWORD PARAMETERS:
; COASTS -- Set this keyword to draw coastlines, islands, and lakes instead of
;           the default continent outlines. Note that if you are using the
;           low-resolution map database (if the HIRES keyword is not set), many
;           islands are drawn even when COASTS is not set. If you are using the
;           high-resolution map database (if the HIRES keyword is set),
;           no islands are drawn unless COASTS is set.
;
; COLOR -- The color index of the lines being drawn.
;
; CONTINENTS:  Set this keyword to plot the continental boundaries.
;       This is the default.
;       Note that if you are using the low-resolution map database
;       (if the HIRES keyword is not set), outlines for continents, islands,
;       and lakes are drawn when the CONTINENTS keyword is set. If
;       you are using the high-resolution map database (if the HIRES keyword
;       is set), only continental outlines are drawn when the CONTINENTS
;       keyword is set. To draw islands and lakes when using the
;       high-resolution map database, use the COASTS keyword.
;
; COUNTRIES -- Set this keyword to draw political boundaries as of 1993.
; _EXTRA -- Other keywords passed along to PLOTS/POLYFILL depending on
;           options selected. Note that command line keywords (like
;           MLINETHICK) will take precedence over options specified with
;           _EXTRA or their natural names (like THICK).
; FILL_CONTINENTS -- Set this keyword to 1 to fill continent boundaries with
;                    a solid color. The color is set by the COLOR keyword.
;                    Set this keyword to 2 to fill continent boundaries with a
;                    line fill. For line filling, the COLOR, MLINESTYLE,
;                    MLINETHICK, ORIENTATION, and SPACING keywords can be used
;                    to control the type of line fill. Any option valid for
;                    polyfill can also be used (i.e. PATTERN).
;
; HIRES -- Set this keyword to use high-resolution map data instead of the
;          default low-resolution data. This option is only available if you
;          have installed the optional high-resolution map datasets. If the
;          high-resolution data is not available, a warning is printed and
;          the low-resolution data is used instead.
;
;          This keyword can be used in conjunction with the COASTS, COUNTRIES,
;          FILL_CONTINENTS, and RIVERS keywords.
;
; LIMIT: Set this keyword to a four-element vector
;        [Lat min, Lon min, Lat max, Lon max ] to only plot continents that
;        pass through the LIMIT rectangle.
;        The points (Lat min, Lon min ) and (Lat max, Lon max ) are
;        the latitudes and longitudes of two points diagonal from each other
;        on the region’s boundary.
;        The default is to use the limits from the current map projection.
;
;     Note - Line segments for continents which extend outside of the
;        LIMIT rectangle will still be plotted.
;
; MAP_STRUCTURE: Set this keyword to a !MAP structure, as returned from
;      MAP_PROJ_INIT. If this keyword is set then the !MAP system variable
;      is ignored, and the polygons and polylines are drawn using
;      UV (Cartesian) coordinates.
;
; MLINESTYLE -- The line style of the boundaries being drawn.
;               The default is solid lines.
; MLINETHICK -- The thickness of the boundary or fill lines.
;               The default thickness is 1.
; ORIENTATION -- Set this keyword to the counterclockwise angle in degrees
;                from horizontal that the line fill should be drawn. The
;                default is 0. This keyword only has effect if the
;                FILL_CONTINENTS keyword is set to 2.
; RIVERS  -- Set this keyword to draw rivers.
; SPACING -- Set this keyword to the spacing, in centimeters, for a line fill.
;            This keyword only has effect if the FILL_CONTINENTS keyword is
;            set to 2. The default is 0.5 centimeters.
;
; T3D: Set this keyword to indicate that the generalized transformation
;      matrix in !P.T is to be used. If not present, the user-supplied
;      coordinates are simply scaled to screen coordinates.
;
; USA -- Set this keyword to draw borders for each state in the United States
;        in addition to continental boundaries.
;
; ZVALUE: Sets the Z coordinate, in normalized coordinates in the
;         range of 0 to 1, at which to output the continents.
;
;      Note - This keyword has effect only if keyword T3D is set and the
;         transformation is stored in !P.T
;
; OUTPUT KEYWORD PARAMETERS:
;              NONE
;
; OUTPUTS:
;       Draws continents, etc. over the current map display.
;
; COMMON BLOCKS:
;       None.
;
; SIDE EFFECTS:
;       None.
;
; RESTRICTIONS:
;       See DESCRIPTION.
;
; DESCRIPTION:
;       See PURPOSE.
;
; EXAMPLES:
;
;         Draw Low-Resolution continents, with high resolution
;         political boundaries.
;
;         MAP_SET
;         MAP_CONTINENTS
;         MAP_CONTINENTS,/hires,/countries
;
;   Example using MAP_STRUCTURE:
;       ; GCTP Polar stereographic projection
;       mapStruct = MAP_PROJ_INIT(106, LIMIT=[0,-180,90,180], $
;           CENTER_LATITUDE=90)
;       ; Create a plot window using the UV Cartesian range.
;       PLOT, mapStruct.uv_box[[0,2]],mapStruct.uv_box[[1,3]], $
;           /NODATA, /ISOTROPIC, XSTYLE=1, YSTYLE=1
;       MAP_CONTINENTS, MAP_STRUCTURE=mapStruct
;       MAP_GRID, MAP_STRUCTURE=mapStruct
;
; DEVELOPMENT NOTES:
;         This version uses !type=3 and uses the NEW (IDL5) map software.
;         requires map_struct_append in map_set.pro
;
; MODIFICATION HISTORY:
;   SVP, 11/96 ;  Added header template.
;   CT, RSI, Nov 2002: Added MAP keyword.
;   CT, RSI, Feb 2004: Renamed MAP keyword to MAP_STRUCTURE, now documented.
;
;-


; -----------------------------------------------------------------------------
FUNCTION map_getindex,indx, error
;
; Used to read in the index file for
; each map data file.  On successful completion, error is set to 0.
;
COMPILE_OPT hidden

openr, lun, indx, /xdr, /get_lun, error = error
if error ne 0 then return, 0		;File not there or unreadable
segments=0L & readu, lun, segments
dx_map=replicate({ fptr:0L, npts:0L,latmax:0.,latmin:0.,$
                   lonmax:0.,lonmin:0. }, segments )
readu, lun, dx_map & free_lun, lun
free_lun,lun
return, dx_map
END


; -----------------------------------------------------------------------------
PRO map_do_segments, fnames, name, hires, bounds, zvalue, extra, $
    POLYFILL=poly, MAP_STRUCTURE=mapStruct
; Output a segment file:
; fnames = [lowresname, hiresname]
; name = description for error message (boundaries, rivers, etc.
; hires = 0 for low res, 1 for hires
; zvalue = Z value for 3D
; bounds = lat/lon bound.
; Polyfill = 0 for lines, 1 for polyfill

; Hires = 1 to do hires, 0 for low

COMPILE_OPT hidden

poly = KEYWORD_SET(poly)

lun = -1
sub = (['low', 'high'])[hires]
fndx = FILEPATH(fnames[hires]+'.ndx', SUBDIR=['resource','maps',sub])
dat =  FILEPATH(fnames[hires]+'.dat', SUBDIR=['resource','maps',sub])
ndx = map_getindex(fndx, error)		;OPEN it
if error eq 0 then openr, lun, dat,/xdr,/stream, /get, error = error
if (error ne 0) and hires then begin 	;Try low res as a fallback
    message, 'High Res Map File: '+name+' not found, trying low res.', /INFO
    fndx = FILEPATH(fnames[0]+'.ndx', SUBDIR=['resource','maps','low'])
    dat =  FILEPATH(fnames[0]+'.dat', SUBDIR=['resource','maps','low'])
    ndx = map_getindex(fndx, error)		;OPEN it
    endif			;Hires
if lun lt 0 then openr, lun, dat,/xdr,/stream, /get, error = error
if error ne 0 then message, 'Map file:'+fnames[hires]+' not found'

; Output a bunch of segments from a standard format file.
lonmin = bounds[0]
lonmax = bounds[2]
latmin = bounds[1]
latmax = bounds[3]

;This shouldn't be necessary, but people do sometimes provide screwey inputs:
while lonmin gt 180 do lonmin = lonmin - 360.
while lonmin lt -180 do lonmin = lonmin + 360.
while lonmax gt 180 do lonmax = lonmax - 360.
while lonmax lt lonmin do lonmax = lonmax + 360.

test_lon = ((lonmax-lonmin) mod 360.) ne 0.0
test_lat = (latmin gt -90. or latmax lt 90.) and (latmin ne latmax)

; Prune segments if bounds are set.
if test_lon then begin          ;Longitude ranges are tricky.
                                ; This relies on the following:
                                ;   -180 le lonmin le 180  and
                                ;   -180 le ndx.lonmin le 180 and
                                ;   ndx.lonmax  > ndx.lonmin  and
                                ;   lonmax > lonmin
    x0 = ndx.lonmax - lonmin    ;nmax > lonmin
    good = where(x0 lt 0.0, count)
    if count ne 0 then x0[good] = x0[good] + 360.
    x1 = lonmax - ndx.lonmin    ;nmin < lonmax
    if count ne 0 then x1[good] = x1[good] - 360.
    good = (x0 le (lonmax-lonmin)) or (x1 gt 0)

    if test_lat then begin      ;test lat & lon
        subs = where((ndx.latmin lt latmax) and (ndx.latmax gt latmin) and $
                     good, count)
        if count ne 0 then ndx = ndx[subs]
    endif else begin            ;lon only
        subs = where(good, count)
        if count ne 0 then ndx = ndx[subs]
    endelse
endif else if test_lat then begin ;Lat only
    subs = where((ndx.latmin lt latmax) and (ndx.latmax gt latmin), count)
    if count ne 0 then ndx = ndx[subs]
endif else count = n_elements(ndx) ;test neither

; ************** Draw the segments ***************************
;
if count gt 0 then begin

    for i=0, count-1 do begin

        point_lun, lun, ndx[i].fptr

        if ndx[i].npts lt 2 then $
            continue

    	xy=fltarr(2,ndx[i].npts, /NOZERO)
    	readu,lun,xy
    	xy = REVERSE(xy)

    	if (N_TAGS(mapStruct) gt 0) then begin

        	; Convert from lon/lat to Cartesian coordinates.
    	    xy = poly ? $
        	    MAP_PROJ_FORWARD(xy, MAP_STRUCTURE=mapStruct, POLYGONS=polyconn) : $
        	    MAP_PROJ_FORWARD(xy, MAP_STRUCTURE=mapStruct, POLYLINES=polyconn)

        	n = N_ELEMENTS(xy)/2
        	if (n eq 0) then $    ; all points were clipped
        	    continue

        	index = 0L
            ; Loop thru each polyline in connectivity list.
        	while (index lt n) do begin
        	    ipoly = polyconn[index + 1 : index + polyconn[index]]
                CALL_PROCEDURE, poly ? 'POLYFILL' : 'PLOTS', $
                    xy[0, ipoly], xy[1, ipoly], zvalue, $
                    NOCLIP=0, _EXTRA=extra
                index = index + polyconn[index] + 1
            endwhile

    	endif else begin

    	    ; MAP_SET has been called. Go thru internal mapping.
            CALL_PROCEDURE, poly ? 'POLYFILL' : 'PLOTS', $
                xy[0,*], xy[1,*], zvalue, NOCLIP=0, _EXTRA=extra

        endelse

    endfor
endif

FREE_LUN, lun

end


; -----------------------------------------------------------------------------
pro map_do_unitedstates, cont, hires, bounds, zvalue, $
    MAP_STRUCTURE=mapStruct, _EXTRA=_extra

    compile_opt idl2, hidden

    lonmin = bounds[0]
    lonmax = bounds[2]
    latmin = bounds[1]
    latmax = bounds[3]

  map_file=FILEPATH('supmap.dat',subdir=['resource','maps'])
  openr, lun, /get, map_file,/xdr,/stream
  npts = 0L

  index = (cont && ~hires) ? 2 : 1

  ; 	  cont us_only  both
  fbyte = [ 0, 71612L, 165096L]
  nsegs = [ 283, 325, 594 ]


  point_lun, lun, fbyte[index]

  for i=1,nsegs[index] do begin	;Draw each segment

	READU, lun, npts,maxlat,minlat,maxlon,minlon
	npts = npts / 2		;# of points
	xy = fltarr(2,npts)
	READU, lun, xy
	xy = REVERSE(xy)

    ; Check for out of plot range.
	if (maxlat lt latmin) || (minlat gt latmax) then $
	    continue
	if ((maxlon lt lonmin) || (minlon gt lonmax)) then $
        if ~(lonmax gt 180. && maxlon + 360. ge lonmin) then $
            continue

	if (N_TAGS(mapStruct) gt 0) then begin

    	; Convert from lon/lat to Cartesian coordinates.
	    xy = MAP_PROJ_FORWARD(xy, MAP_STRUCTURE=mapStruct, POLYLINES=polyconn)

    	n = N_ELEMENTS(xy)/2
    	if (n eq 0) then $    ; all points were clipped
    	    continue

    	index = 0L
        ; Loop thru each polyline in connectivity list.
    	while (index lt n) do begin
    	    ipoly = polyconn[index + 1 : index + polyconn[index]]
            PLOTS, xy[0, ipoly], xy[1, ipoly], zvalue, $
                NOCLIP=0, _EXTRA=_extra
            index = index + polyconn[index] + 1
        endwhile

	endif else begin

	    ; MAP_SET has been called. Go thru internal mapping.
        plots,xy[0,*],xy[1,*],zvalue,NOCLIP=0,_EXTRA=_extra

    endelse

  endfor

  FREE_LUN, lun

end


; -----------------------------------------------------------------------------
PRO Map_Continents, $
 USA = kusa, CONTINENTS = kcont, COUNTRIES=kcountries, $
 HIRES=khires, FILL_CONTINENTS=kfill_continents, COASTS=kcoasts, $
 LIMITS = lim_u, MLINESTYLE = mlinestyle, MLINETHICK = mlinethick, $
 SPACING=spacing, COLOR=color, T3D=T3D, ORIENTATION=orientation, $
 ZVALUE=zvalue, RIVERS=krivers, $
 MAP_STRUCTURE=mapStruct, $
 _EXTRA=extra
;
;
;       if fill_continents =1 you get solid polygons
;       if fill_continents =2 you get lines
;

ON_ERROR, 2

if (!x.type NE 3) and (N_TAGS(mapStruct) eq 0) THEN $
    message,'Map transform not established.'

; 			Map_continents keyword defaults:
cont = keyword_set(kcont)
usa = keyword_set(kusa)
rivers = keyword_set(krivers)
coasts = keyword_set(kcoasts)
countries = keyword_set(kcountries)
hires = keyword_set(khires)

if n_elements(kfill_continents) then fill_continents=kfill_continents $
  else fill_continents = 0

if (usa+countries+coasts+rivers eq 0) and n_elements(kcont) eq 0 then cont = 1
if n_elements(zvalue) eq 0 then zvalue = 0.

; Check for user-defined limit, or !map limits.
l = (n_elements(lim_u) eq 4) ? lim_u : $
    (N_TAGS(mapStruct) gt 0) ? mapStruct.ll_box : !map.ll_box

; Check for bad limits.
if ((l[0] ge l[2]) or (l[1] ge l[3])) then $
    l = [-90., -360., 90., 360.]

latmin = l[0] & lonmin = l[1]
latmax = l[2] & lonmax = l[3]

 ;WHY do we have so many ways of expressing a rectangle??
bounds = [lonmin, latmin, lonmax, latmax]

;
; Process the _EXTRA for graphics keywords. Normally the _EXTRA
; values overwrite the input values, but not for maps.
;
if n_elements(mlinestyle) then $
    map_struct_append, extra, 'LINESTYLE', mlinestyle
if n_elements(mlinethick) then $
   map_struct_append, extra, 'THICK', mlinethick
if n_elements(spacing) and (fill_continents eq 2) then $
   map_struct_append, extra, 'SPACING', spacing
if n_elements(color) then $
   map_struct_append, extra, 'COLOR',color
if n_elements(orientation) and (fill_continents eq 2)then $
   map_struct_append, extra, 'ORIENTATION', orientation
if n_elements(t3d) then map_struct_append, extra,'t3d',t3d
;	Process line-filled continents:
if fill_continents eq 2 then map_struct_append, extra, 'LINE_FILL', 1

; if n_tags(extra) gt 0 then help, /st, extra  ;Debugging

if (rivers) then begin	;Rivers
   map_do_segments, ['rlow', 'rhigh'], 'Rivers', $
    hires, bounds, zvalue, extra, MAP_STRUCTURE=mapStruct
endif				;Rivers

if (countries) then begin	;*** Countries ****
   map_do_segments, ['blow', 'bhigh'], 'Boundaries', $
    hires, bounds, zvalue, extra, MAP_STRUCTURE=mapStruct
endif				;countries

if (coasts) then begin
   map_do_segments, ['clow', 'chigh'], 'Coasts', $
    hires, bounds, zvalue, extra, MAP_STRUCTURE=mapStruct
endif				;Coasts

; If both CONTINENTS and USA are specified, then we don't need
; to do the continents here (they will happen below).
; However, if FILL_CONTINENTS or HIRES are also set, then we
; *do* need to do the continents.
if (fill_continents || (cont && (hires || ~usa))) then begin
   map_do_segments, ['plow', 'phigh'], 'Continents', $
	hires, bounds, zvalue, extra, POLYFILL=fill_continents ne 0, $
	MAP_STRUCTURE=mapStruct
endif				;continents

if (usa) then begin		;States in USA, a different file
  map_do_unitedstates, cont, hires, bounds, zvalue, $
    MAP_STRUCTURE=mapStruct, _EXTRA=extra
endif				;USA

end
