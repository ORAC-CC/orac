; $Id: //depot/Release/IDL_81/idl/idldir/lib/map_set.pro#1 $
;
; Copyright (c) 1993-2011, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.


;-------------------------------------------------------------------------
; MAP_SET
;
; KEYWORDS: (partial list)
;
;  NAME: Set this keyword to a string indicating the projection
;        that you wish to use. A list of available projections can
;        be found using MAP_PROJ_INFO, PROJ_NAMES=names.
;        This keyword will override any of the individual projection
;        keywords.
;
;  PROJECTION: Set this keyword to a projection number indicating the
;        projection that you wish to use. This keyword will override NAME
;        or any of the individual projection keywords.
;
;      Note - This keyword is intended for internal use only, and is subject
;        to change in the future.
;
;  REVERSE: Set this keyword to reverse the X and/or Y axes:
;         0 = no reversal (the default)
;         1 = reverse X
;         2 = reverse Y
;         3 = reverse both.
;
;  MODIFICATION HISTORY:
;   Written: DMS, RSI, A long long time ago.
;
;   Modified: CT, RSI, July 2003: Removed CONIC hack for setting the
;       standard parallels using the p0Lat, p0Lon, Rot arguments.
;       This hadn't been documented since IDL4.0, and was preventing
;       the Rot argument from actually rotating the map.
;       Users should use STANDARD_PARALLELS for conic projections.
;   CT, RSI, June 2004: Move map_horizon, map_point_valid, map_proj_info
;       map_satellite_limit, map_struct_append into separate file since
;       these are used by map_grid and map_continents.
;       Makes creating a save file easier.
;

;-------------------------------------------------------------------------
PRO MAP_STRUCT_MERGE, orig, add, SUPERCEDE = super
; Add the keywords in the struct add to the original struct orig.
; ADD must be either undefined or defined as a structure.
; If a given tag name is present in both structures:
;   If SUPERCEDE is set, then the value of add.tag replaces the
;   original value.  If SUPERCEDE is not set, the original value
;   remains unchanged.
COMPILE_OPT hidden

if n_elements(orig) eq 0 then begin
    if n_elements(add) ne 0 then orig = add
    return
endif

otag = tag_names(orig)
atag = tag_names(add)
for i=0, n_elements(atag)-1 do begin ;Add the new ones
    match = where(otag eq atag[i], count)
    if count eq 0 then $
      orig = create_struct(orig, atag[i], add.(i)) $ ;Disjoint tag name, add it
    else if keyword_set(super) then $ ;Got a match, if super is set, overwrite.
      orig.(match[0]) = add.(i) ;Common, add if set
endfor
end


;-------------------------------------------------------------------------
PRO map_set_ll_box
  COMPILE_OPT hidden

;Try to determine a lon/lat range from the existing parameters, and
;save in !MAP.LL_BOX = [latmin, lonmin, latmax, lonmax].
; If we can't reliably determine the lat/lon limits then the
; coorresponding min and max are both set to zero.

; If either pole is mappable, then all longitudes are mappable
north = map_point_valid(0, 90)
south = map_point_valid(0, -90)
dateline = (north + south eq 0) and map_point_valid(-180., !map.p0lat) and $
  (map_point_valid(0., !map.p0lat) eq 0) ; Dateline visible

if north and south then begin   ;Both poles visible
    sum = 0
    for i=0,270,90 do sum = sum + map_point_valid(!map.p0lon+i, 0)
    if sum eq 4 then begin      ;Entire globe is visible
        lonmin = !map.p0lon - 180 ;Center window about projection center
        while (lonmin lt -180) do lonmin += 360
        !map.LL_BOX = [-90, lonmin, 90, lonmin + 360.]
    endif
    return                      ;The whole globe [might be] visible
endif

; Edges are in order: bottom, right, top, left.

sidex = [0, 2, 2, 0 ,0]         ;X index of UV corner
sidey = [1, 1, 3, 3, 1]         ;Y index
; convert to float to truncate extraneous digits
; (otherwise Mercator will have incorrect LL_BOX for non-zero p0lon)
box = float(!map.uv_box)
uvcen = (box[[0,1]] + box[[2,3]])/2.
del = 8                         ;# of intervals

again: found = bytarr(4)
if north then begin
    lonmax = 180. & lonmin = -180. ;All longitudes are visible
    latmax = 90. & latmin = 90.
endif else if south then begin
    lonmax = 180. & lonmin = -180.
    latmax = -90. & latmin = -90.
endif else begin
    if dateline then begin
        lonmax = 0. & lonmin = 360.
    endif else begin
        lonmax = -180. & lonmin = 180.
    endelse
    latmin = 90. & latmax = -90.
endelse

; March along the 4 uv edges of the plot, keeping the range in
; geo-coordinates.


for iside = 0, 3 do begin
    s0 = box[[sidex[iside], sidey[iside]]] ;Start of edge
    s1 = box[[sidex[iside+1], sidey[iside+1]]] ;End of edge
    LastPointGood = 0
    for i = 0, del-1 do begin
        s2 = s0 + (s1-s0) * (float(i)/del) ;UV coordinate
        n2 = s2 * [!x.s[1], !y.s[1]] + [!x.s[0], !y.s[0]] ;Normalized coord
        p = convert_coord(n2[0], n2[1], /NORMAL, /TO_DATA)
        if finite(p[0]) then begin
            if dateline and (p[0] lt 0) then p[0] = p[0] + 360
            lonmax = lonmax > p[0]
            lonmin = lonmin < p[0]
            latmax = latmax > p[1]
            latmin = latmin < p[1]
            found[iside] = 1    ;Found a legit point
; This is a special KLUDGE to detect where the dateline crosses the left
; or right edge.
            if (dateline + north + south eq 0) and LastPointGood and $
              (iside eq 1 or iside eq 3) then begin
                if abs(lonlast - p[0]) gt 180 then begin ;Dateline cross?
                    dateline = 1 ;Yes...
                    goto, again ;And rescale...
                endif
            endif
            LastPointGood = 1
            lonlast = p[0]
        endif else LastPointGood = 0
    endfor                      ;for i
endfor                          ;for iside


if total(found) eq 4 then begin ;If we didn't values along 4 sides then punt
    !map.ll_box = [latmin, lonmin, latmax, lonmax]
;    print, !map.ll_box, format='(4f10.2)'
endif

end



;-------------------------------------------------------------------------
PRO map_set_limits, limit, uvrange, CYLINDRICAL=is_cyl
; Limit = 4 or 8 point vector containing lat/lon limits. (input).
;    WARNING: limit may be changed.
; uvrange = uv extent of map in form of [umin, vmin, umax, vmax]
; (output).  Also, on output, limit is converted to the 8 point form.
;

COMPILE_OPT hidden

; Set unit scaling for convert_coord:
!x.s = [0,1]
!y.s = [0,1]

if n_elements(limit) eq 4 then begin ;cvt 4 pt to 8 pt limit.

; ***********        limit = [latmin, lonmin, latmax, lonmax]

; Finding the UV extent of a map, given a 4 point limit is a process
; frought with perils.  Previous methods didn't always work, making a
; map that didn't show the entire range of limits. This uses the
; brute force method.....
; ***********        limit = [latmin, lonmin, latmax, lonmax]


    londel = float(limit[3] - limit[1]) ;Check grid within the lat/lon limits
    if londel gt 360. then message, $
      'Longitude limits exceed 360 degrees', /CONTINUE
    if londel lt 0 then londel = londel + 360
    latdel = float(limit[2] - limit[0])
    nlon = (1+ceil(londel / 10.)) > 13 ;At least 13 pnts or every 10 degs
    nlat = (1+ceil(latdel / 10.)) > 9 ;At least 9 pnts or every 10 degs
    ; Because of roundoff errors, you can sometimes get out of range
    ; latitudes, so just clip to -90 and +90.
    lats = -90 > (findgen(nlat) * (latdel / (nlat-1)) + limit[0]) < 90
    lons = findgen(nlon) * (londel / (nlon-1)) + limit[1]
    eps = 1.0e-4                ;Fudge factor for cylindricals

;If cylindrical and the map covers 360 degrees of longitude, don't use
;the whole extent because of the ambiguity of +/- 180.
    if keyword_set(is_cyl) and londel eq 360 then $
      lons = findgen(nlon) * ((londel - eps) / (nlon-1)) + (limit[1]+eps/2.)

    uv = convert_coord(lons # replicate(1., nlat), $ ;m x n array of lons
                       replicate(1.,nlon) # lats, $ ;m x n array of lats
                       /TO_NORM, /DATA)
    u = uv[0,*];
    if total(finite(u)) lt 2 then $ ;Any points good?
      message, 'At least two limit points must be mappable', /INFO
                                ; Extract the range
    uvrange = [min(u,/NAN, MAX=umax), min(uv[1,*],/NAN, MAX=vmax), $
                   umax, vmax]
    if uvrange[0] eq uvrange[2] or uvrange[1] eq uvrange[3] then $
      message, 'Invalid map limits', /INFO

endif else if n_elements(limit) eq 8 then begin ;8 point case is much simpler

; 8 point limit in form of:
;    [latLeft,lonLeft, latTop, lonTop, LatRt, lonRt, LatBot, LonBot]
;
    for i=0,3 do begin
        uv = convert_coord(limit[i*2+1], limit[i*2], /TO_NORM, /DATA)
        if finite(uv[0]) eq 0 then $
          message, 'Unmappable limit point: ' + $
            string(limit[i*2+1], limit[i*2]), /INFO
;   Left = uvrange(0)=uv(0); Top = uvrange(3) = uv(1); Right =
;   uvrange(2) = uv(0); Bot = uvrange(1) = uv(1)
        uvrange[([0,3,2,1])[i]] = uv[([0,1,0,1])[i]]
    endfor
endif else message, 'Map limit must have 4 or 8 points'

if uvrange[0] ge uvrange[2] or uvrange[1] ge uvrange[3] then $
  message, 'Warning, MAP limits are invalid.', /CONTINUE
end



;-------------------------------------------------------------------------
FUNCTION map_rotxyz, p, rx, ry, rz ;Rotate the vector p(3) counterclockwise
; about the X, Y, and Z, by the amounts rx, ry, rz, degrees, in order.
;
COMPILE_OPT hidden

p1 = p
dtor = !dpi/ 180.

if rx ne 0.0 then begin
    sx = sin(rx * dtor) & cx = cos(rx * dtor)
    t = [[1,0,0],[0,cx, sx], [0, -sx, cx]]
    p1 = t # p1
endif
if ry ne 0.0 then begin
    sy = sin(ry * dtor) & cy = cos(ry * dtor)
    t = [[cy, 0, -sy], [0, 1, 0], [sy, 0, cy]]
    p1 = t # p1
endif
if rz ne 0.0 then begin
    sz = sin(rz * dtor) & cz = cos(rz * dtor)
    t = [[cz, sz, 0], [ -sz, cz, 0], [0,0,1]]
    p1 = t # p1
endif
return, p1
end


;-------------------------------------------------------------------------
FUNCTION GREAT_CIRCLE, lon0, lat0, lon1, lat1, RADIANS=radians, PRINT=print
; Return: [sin(c), cos(c), sin(az), cos(az)] from the great circle
; distance between two points.  c is the great circle angular distance,
; and az is the azimuth between two points.

COMPILE_OPT hidden

if KEYWORD_SET(radians) eq 0 then k = !dtor else k = 1.0

coslt1 = cos(k*lat1)
sinlt1 = sin(k*lat1)
coslt0 = cos(k*lat0)
sinlt0 = sin(k*lat0)

cosl0l1 = cos(k*(lon1-lon0))
sinl0l1 = sin(k*(lon1-lon0))

cosc = sinlt0 * sinlt1 + coslt0 * coslt1 * cosl0l1 ;Cos of angle between pnts
sinc = sqrt(1.0 - cosc^2)
if abs(sinc) gt 1.0e-6 then begin
    cosaz = (coslt0 * sinlt1 - sinlt0*coslt1*cosl0l1) / sinc ;Azmuith
    sinaz = sinl0l1*coslt1/sinc
endif else begin        ;Its antipodal
    cosaz = 1.0
    sinaz = 0.0
endelse

if keyword_set(print) then begin
    print, 'Great circle distance: ', acos(cosc) / k
    print, 'Azimuth: ', atan(sinaz, cosaz)/k
;    print,'sinaz, cosaz = ', sinaz, cosaz
endif
return, [sinc, cosc, sinaz, cosaz]
end


;-------------------------------------------------------------------------
PRO map_set_split, ADD=add, EQUATOR = equator
;  Set the splitting hemisphere.  If EQUATOR is set, set the splitting
;  point as if the center of projection was on the equator, i.e. use
;  a latitude of 0.
COMPILE_OPT hidden

pole = !map.pole[4:6]       ;Locn of pole

if keyword_set(equator) then begin ;use lat = 0
    split = [!map.p0lon, 0, -sin(!map.u0), cos(!map.u0), 0.0, 0.0]
endif else begin
    coslat = cos(!map.v0)
    p0 = [ cos(!map.u0) * coslat, sin(!map.u0) * coslat, sin(!map.v0)]
    pln = crossp(p0, pole)
    split=[!map.p0lon, !map.p0lat, pln, 0.0]
endelse

noadd = keyword_set(add) eq 0   ;= 0 if we're adding
MAP_CLIP_SET, RESET=noadd, SPLIT=split, TRANSFORM=noadd
end



;-------------------------------------------------------------------------
PRO map_set_clip, PNAME=pname, IPROJ=iproj, CLIP_RADIUS = r0, LIMIT=limit

COMPILE_OPT hidden

; Set up the default clipping/splitting for the given projection.
;
if n_elements(pname) eq 0 then map_proj_info, iproj, NAME=pname
if n_elements(iproj) eq 0 then begin
    map_proj_info, PROJ_NAMES=pnames
    pname = pnames[(where(pname eq pnames, count))[0]]
    if count eq 0 then message, "No such projection available: " + pname
endif


r = -1.0            ;Great circle clipping radius...
map_proj_info, iproj, CONIC=is_conic

if is_conic then begin          ;Conical projection?

    map_set_split

    ; Hemisphere of the two standard parallels.
    isign1 = (!map.p[3] ge 0.0) ? 1 : -1
    isign2 = (!map.p[4] ge 0.0) ? 1 : -1

    ; If LIMIT was provided then use the southern or northern
    ; latitude depending upon the hemisphere.
    if (N_ELEMENTS(limit) eq 4) then begin
        clipmin = ABS(limit[(isign1 gt 0) ? 0 : 2])
        clipmax = ABS(limit[(isign1 gt 0) ? 2 : 0])
    endif else begin
        ; If both standard parallels are on the same side of Equator,
        ; then clip opposite hemisphere at 10 degs.
        ; Otherwise clip poles in both hemisphere at 75 degs.
        clipmin = (isign1 eq isign2) ? 10 : 75
        clipmax = 75
    endelse

    map_clip_set, CLIP_PLANE=[0,0,isign1, sin(!dtor * clipmin)]
    map_clip_set, CLIP_PLANE=[0,0,-isign1, sin(!dtor * clipmax)]

endif else if pname eq 'Mercator' or pname eq 'MillerCylindrical' then begin
    rm = 0.975                  ;Go out 97.5% of the way to the poles
    map_set_split
    map_clip_set, CLIP_PLANE=[-!map.pole[4:6], rm]
    map_clip_set, CLIP_PLANE=[!map.pole[4:6], rm]

endif $                         ;Other cylindricals
else if pname eq "Stereographic" or pname eq "Orthographic" then r = -1.0e-5 $
else if pname eq "Satellite" then r = - 1.01/!map.p[0] $
else if pname eq "Gnomic" then r = -0.5 $
else if pname eq "LambertAzimuthal" or pname eq "AzimuthalEquidistant" then $
  r=0.8 $
else if pname eq "TransverseMercator" then begin
; Clip plane is perpendicular to the XY projection of the vector to the
; center of projection, and to the XY Plane.
    map_clip_set, CLIP_PLANE=[cos(!map.u0), sin(!map.u0), 0, 0]
endif else if pname eq "GoodesHomolosine" then begin
    r = -1.0
;  What a screwey interrupted projection.  Splits are at 180 degrees
;  from the seams.
    splits = [-180., -40, -100, -20, 80]+180. + !map.p0lon
    for i=0, n_elements(splits)-1 do begin ;Add each split
        theta = !dtor * splits[i]
        MAP_CLIP_SET, SPLIT=[splits[i], 0, -sin(theta), cos(theta), 0., 0.]
    endfor
endif else begin                      ;If we haven't setup a clip, add splitting
    map_proj_info, /CURRENT, CYLINDRICAL=is_cyl
    if is_cyl then map_set_split
endelse

;   If r is set, set clipping to points on a plane whose normal
;   passes thru the center (u0,v0), and at a distance of r from the
;   origin.  r < 0 is the side towards (u0,v0).

if r ne -1.0 then begin
    if n_elements(r0) ne 0 then r = r0 ;User supplied value?
    map_clip_set, CLIP_PLANE=[cos(!map.u0) * !map.coso, $
                              sin(!map.u0) * !map.coso, !map.sino, r]
endif
map_clip_set, /TRANSFORM
end



;-------------------------------------------------------------------------
PRO MAP_SET, p0lat, p0lon, rot, $
;   ********** Projection keywords:
  PROJECTION=proj, $              ;The projection index
  NAME=name, $                    ;The projection name as a string
  STEREOGRAPHIC = stereographic, $
  ORTHOGRAPHIC = orthographic, $
  CONIC = conic, $
  LAMBERT = lambert, $
  GNOMIC = gnomic, $
  AZIMUTHAL = azimuth, $
  SATELLITE = satellite, $      ;Also called general perspective proj
  CYLINDRICAL = cylindrical, $
  MERCATOR = mercator, $
  MILLER_CYLINDRICAL=miller, $
  MOLLWEIDE = mollweide, $
  SINUSOIDAL = sinusoidal, $
  AITOFF = aitoff, $            ;Original Aitoff projection
  HAMMER = hammer, $            ;This is really the Hammer-Aitoff projection
  ALBERS = albers, $            ;Alber's equal-area conic
  TRANSVERSE_MERCATOR = utm, $  ;Transverse mercator for an ellipsoid
  ROBINSON = robinson, $        ;Robinson projection
  GOODESHOMOLOSINE = goodes, $  ;Goode's Homolosine.  Origin should be (0,0).
                                ; Also called Gauss-Krueger in Europe.
; **** Projection specific keywords:
  ELLIPSOID = ellips, $         ;Defines the ellipsoid for projections
                                ;that handle the ellipsoidal case,
                                ;currently Transverse Mercator and
                                ;Lambert's conical.
                                ;3 elements: a = equatorial radius (meters), e^2
                                ;= eccentricity squared, k0 = scale on
                                ;central meridian. e^2 = 2*f-f^2,
                                ;where f = 1-b/a, b = polar radius
                                ;Default is the Clarke 1866 ellipsoid, =
                                ; [6378206.4d0, 0.00676866d0, 0.9996d0]
CENTRAL_AZIMUTH=cent_azim, $    ;Angle of central azimuth (degrees) for:
                                ; Cylindrical, Mercator, Miller,
                                ; Mollw, Robinson, and Sinusoidal
                                ; projections. Default = 0.
                                ; The pole is placed at an azimuth of
                                ; CENTRAL_AZIMUTH degrees CCW of north.
STANDARD_PARALLELS = std_p, $   ;One or two standard parallels for conics,
                                ; One or two element array.
SAT_P = Sat_p, $                ;Satellite parameters: Altitude expressed in
                                ; units of radii, Omega, and rotation.
                                ; Rotation may also specified by the
                                ; rot parameter.
;   ********** MAP_SET specific keywords:
CLIP=clip, $                    ;Default = do map specific clipping,
                                ; CLIP=0 to disable
REVERSE=reverse, $              ;Reverse X and/or Y axes. 0=none, 1 = reverse X,
                                ;  2 = reverse Y, 3 = reverse both.
SCALE=scale, $                  ;Construct isotropic map with a given scale.
                                ; Map Scale is 1:scale, otherwise fit to window
ISOTROPIC = iso, $,             ;If set, make X and Y scales equal
LIMIT = limit, $                ;4 or 8 point lat/lon limit:
                                ; 4 point: [latmin, lonmin, latmax, lonmax]
                                ; 8 point: [latLeft,lonLeft, latTop,
                                ;       lonTop, LatRt, lonRt, LatBot, LonBot]
;   ********** MAP_SET graphics keywords:
NOERASE=noerase, TITLE=title,$
  ADVANCE = advance, COLOR=color, POSITION = position, $
  NOBORDER=noborder, T3D=t3d, ZVALUE=zvalue, $
  CHARSIZE = charsize, XMARGIN=xmargin, YMARGIN=ymargin, $
;   ********** MAP_HORIZON keywords:
HORIZON=horizon, E_HORIZON=ehorizon, $ ; E_HORIZON = structure containing
                                ; extra keywords passed to the
                                ; map_horizon procedure, e.g.
                                ; E_HORIZON={FILL:1}
;   ********** MAP_CONTINENTS keywords:
CONTINENTS = continents, E_CONTINENTS=econt, $ ;E_CONTINENTS = structure
                                ; containing extra keywords to be
                                ; passed to
                                ; map_continents, e.g. E_CONTINENTS={FILL:1}
  USA=usa, HIRES = hires, $
  MLINESTYLE=mlinestyle, MLINETHICK=mlinethick, CON_COLOR=con_color, $

;   ********** MAP_GRID keywords:
  GRID=grid, E_GRID=egrid, $    ;E_GRID = extra keywords structure
  GLINESTYLE=glinestyle, GLINETHICK=glinethick, $
  LABEL=label, LATALIGN=latalign, LATDEL=latdel, LATLAB=latlab, $
  LONALIGN=lonalign, LONDEL=londel, LONLAB=lonlab, $

; Ignored, but here for compatibility:
  WHOLE_MAP=whole_map
; *****************************************************************
;+
;         Limit         =  A four or eight element vector.
;             If a four element vector, [Latmin, Lonmin, Latmax,
;                          Lonmax] specifying the boundaries of the region
;                          to be mapped. (Latmin, Lonmin) and (Latmax,
;                          Lonmax) are the latitudes and longitudes of
;                          two diagonal points on the boundary with
;                          Latmin < Latmax and Lonmin < Lonmax.
;              For maps that cross the international dateline,
;              specify west longitudes as angles between
;              180 and 360.
;             If an eight element vector: [ lat0, lon0,
;               lat1, lon1, lat2, lon2, lat3, lon3] specify
;               four points on the map which give, respectively,
;               the location of a point on the left edge,
;               top edge, right edge, and bottom edge of
;               the map extent.
;-


; If the map range can be simply expressed as [latmin, lonmin, latmax,
; lonmax], then they are contained here.  With many maps, this is
; impossible. These values are used to speed MAP_GRID and
; MAP_CONTINENTS, by not drawing elements that are clearly off the
; map.


common map_set_common, proj_scale, proj_is_cyl, proj_is_conic, $
  proj_names, circle, proj_is_azim, user_defined


on_error, 2                     ;Return to caller if error.
del = 1.0e-6
dtor = !dpi/ 180.
dpi2 = !dpi/2.
!map.ll_box = 0                 ;Assume no limits

if n_elements(proj) then iproj = proj $
else if n_elements(name) then begin ;Name supplied, match it
    map_proj_info, PROJ_NAMES = names ;Available names
    name1 = strcompress(strupcase(name), /REMOVE_ALL) ;Ignore case & blanks
    iproj = (where(name1 eq strupcase(names)))[0] ;Match it
    if iproj lt 1 then message, 'Projection "'+name+'" not available'
endif else if keyword_set(stereographic) then iproj =1 $
  else if keyword_set(orthographic) then iproj =2 $
  else if keyword_set(conic  ) then iproj =3 $
  else if keyword_set(lambert) then iproj =4 $
  else if keyword_set(gnomic ) then iproj =5 $
  else if keyword_set(azimuth) then iproj =6 $
  else if keyword_set(satellite) then iproj =7 $
  else if keyword_set(mercator) then iproj =9 $
  else if keyword_set(mollweide) then iproj = 10 $
  else if keyword_set(sinusoidal) then iproj = 11 $
  else if keyword_set(aitoff) then iproj = 12 $
  else if keyword_set(hammer) then iproj = 13 $
  else if keyword_set(albers) then iproj = 14 $
  else if keyword_set(utm) then iproj = 15 $
  else if keyword_set(miller) then iproj = 16 $
  else if keyword_set(robinson) then iproj = 17 $
  else if keyword_set(goodes) then iproj = 19 $
  else iproj=8                                  ;Assume cylindrical

 ;Special case for lambert's conic on an ellipsoid.
if keyword_set(ellips) and iproj eq 3 then iproj = 18

!map.projection = iproj
map_proj_info, /CURRENT, NAME=pname, CONIC=is_conic ;Get symbolic name

; Initial erase?
if !P.multi[0] eq 0 and keyword_set(advance) then erase
if (not KEYWORD_SET(noerase)) and (not KEYWORD_SET(advance)) THEN erase

 ; ******** Supply defaults  **********

if n_params() lt 3 then rot = 0.0d0
if n_params() lt 2 then p0lon = 0d0
if n_params() lt 1 then p0lat = 0d0

if n_elements(cent_azim) le 0 then cent_azim = 0.0
if n_elements(color) eq 0 then color = !p.color  ;Default color
if n_elements(title) eq 0 then title = " "
if n_elements(t3d) le 0 then t3d = 0
if n_elements(zvalue) eq 0 then zvalue = 0.
if n_elements(charsize) eq 0 then charsize = !p.charsize
if charsize le 0.0 then charsize = 1.0
if n_elements(clip) eq 0 then clip = 1 ;default for CLIP is ON

if pname eq "GoodesHomolosine" and p0lat ne 0 then begin
    message,"Goode's Homolosine: center is always on equator.", /CONTINUE
    p0lat = 0.0
endif

if abs(p0lat) gt 90.0 then $
   message,'Latitude must be in range of +/- 90 degrees'
if abs(p0lon) gt 360.0 then $
   message,'Longitude must be in range of +/- 360 degrees'

u = double(p0lon) mod 360               ;Reduce lon to +/- 180
if (u lt -180) then u += 360 $
else if (u gt 180) then u -= 360

!map.p0lon = u
!map.p0lat = p0lat
!map.u0 = u * dtor
!map.v0 = p0lat * dtor

!x.type = 3                     ;New map code
!y.type = 0

;   Simple projection?  (used for (psuedo) cylindricals)
!map.simple = abs(p0lat) le del and abs(cent_azim) le del

!map.sino = sin(!map.v0)
!map.coso = cos(!map.v0)

!map.rotation = rot
!map.sinr = sin(rot * dtor)
!map.cosr = cos(rot * dtor)

;   Compute position of Pole which is a distance of !PI/2 from
;   (p0lon, p0lat), at an azimuth of rot CCW of north.
;
pole = [0.0, sin(cent_azim * dtor), cos(cent_azim * dtor)]
p2 = map_rotxyz(pole, 0, -p0lat, u) ;Rotate to put origin at (0,1,0)
plat = asin(p2[2])
cosla = sqrt(1.0-p2[2]^2)
if cosla eq 0.0 then begin         ;On pole?
    plon = 0.0 & sinln = 0.0 & cosln = 1.0
  endif else begin
    plon = atan(p2[1], p2[0])
    sinln = p2[1] / cosla & cosln = p2[0] / cosla
  endelse

        ; lon/lat, sin(lat), cos(lat), xyz, Location of pole
!map.pole = [plon, plat, p2[2], cosla, p2]

if n_elements(ellips) ne 3 then begin ;Default == Clarke 1866 ellipsoid
    ellips1 = [6378206.4d0, 0.00676866d0, 0.9996d0]
endif else begin
    ellips1 = ellips
    ; Semimajor must be positive. e^2 must be nonnegative and < 1.
    if (ellips[0] le 0 || ellips[1] lt 0 || ellips[1] ge 1) then $
        MESSAGE, 'Illegal keyword value for ELLIPSOID.'
endelse

!map.a = 1.0                    ;Default ellipsoid = sphere of radius 1.
!map.e2 = 0.

if is_conic then begin
    if n_elements(std_p) eq 0 then begin
        if p0lat ne 0 then s = [p0lat, p0lat] else s = [20., 20.]
    endif else if n_elements(std_p) eq 1 then s = [std_p, std_p] $
    else if n_elements(std_p) eq 2 then s = std_p $
    else message, 'STANDARD_PARALLELS must have 1 or 2 elements'
    s = s * dtor

    if ABS(!map.p0lat) eq 90. then $
      message, 'Center of projection may not be pole for conics'

    if pname eq "LambertConic" then begin ; *****
        if s[0] eq s[1] then n = sin(s[0]) $
        else n = alog(cos(s[0])/cos(s[1])) / $
          alog(tan(!dpi/4+s[1]/2)/tan(!dpi/4+s[0]/2))
        if n eq 0.0 then message, $
          'Equator is illegal standard parallel for conic'
        F = cos(s[0]) * tan(!dpi/4 + s[0]/2)^n/n
        r0 = F/tan(!dpi/4+!map.v0/2.)^n
        !map.p = [n, F, r0, s, 0.0]
    endif else if pname eq "LambertConicEllipsoid" then begin ;*****
        if s[0] * s[1] lt 0 then $
          message, 'Standard parallels must be in same hemisphere'
        e = sqrt(ellips1[1])
;Compute n, F, r0 using formulae from Snyder, Page 107-108.
        m = cos(s) / sqrt(1.0d0- ellips1[1] * sin(s)^2)
        t = [!map.v0, s[0], s[1]] ;Angles for t
        t = tan(!dpi/4 - t/2.0) / $
          ((1.0 - e * sin(t)) / (1.0 + e*sin(t)))^(e/2.)
        if s[0] eq s[1] then n = sin(s[0]) $
        else n = (alog(m[0]) - alog(m[1])) / (alog(t[1]) - alog(t[2]))
        F = m[0] / (n * t[1] ^ n)
        r0 = ellips1[0] * F * t[0] ^ n
        !map.p = [n, F, r0, s]
        !map.a = ellips1[0]     ;Save ellipsoid
        !map.e2 = ellips1[1]
    endif else begin            ;Albers conic
        n = (sin(s[0]) + sin(s[1]))/2.
        if n eq 0 then begin
            message,'Standard parallels should not be 0', /INFO
            n = 0.5
        endif
        c = cos(s[0])^2 + 2 * n * sin(s[0])
        r0 = sqrt(c - 2*n*sin(!map.v0))/n
        !map.p = [n, c, r0, s]
    endelse
endif

if pname eq "Satellite" then begin ;Special params for satellite projection.
    if n_elements(sat_p) eq 0 then !map.p[0] = 2.0 $ ;Altitude in radii
    else !map.p[0] = sat_p[0]
    if n_elements(sat_p) le 1 then omega = 0d0 else omega = dtor * sat_p[1]
;Save em.  sat(1) = TRUE for simple case (Vertical perspective)
    !map.p[1] = omega
    !map.p[2] = sin(omega)  ;Somega = p[1]
    !map.p[3] = cos(omega)  ;comega
    if n_elements(sat_p) ge 3 then begin ; For backwards compatibility, if
; Sat_p contains three elements, interpret the 3rd element as the rotation:

        !map.rotation = sat_p[2] + rot
        !map.sinr = sin(!map.rotation * dtor)
        !map.cosr = cos(!map.rotation * dtor)
    endif                       ;3 elements
endif

map_proj_info, /CURRENT, UV_RANGE=uvrange, CYLINDRICAL = is_cyl

; Set up cylindrical projections
if pname eq "TransverseMercator" then begin ;Special params for UTM
;   ellips is a 3 element array containing the ellipsoid parameters:
;           [a, e^2, k0]
;   a = Equatorial radius, in meters.
;   b = polar radius, b = a * (1-f), f = 1-b/a
;   e^2 = eccentricity^2 = 2*f-f^2, where f = flattening.
;   k0 = scale on central meridian, = 0.9996 for UTM.
    e_2 = ellips1[1]/(1.0-ellips1[1])

;               k0        e_2   b          m0  m
    !map.p = [ellips1[2], e_2, ellips1[1], 0., 0.]
    !map.a = ellips1[0]
    !map.e2 = ellips1[1]
    !x.s = [0,1] & !y.s = [0,1] & !x.type = 3 ;Fake coordinate system.
    q = convert_coord(u, p0lat, /data, /to_norm) ;Get m0
    !map.p[3] = !map.p[4]     ;Set m0 from Center of projection

endif else if is_cyl then begin ;other Cylindrical or pseudo-cylind proj?
; I don't know why we can't solve for this angle (the azimuth of (u0,v0)
; from (xp, yp)) using the law of sines.  This is solving it the hard
; and long way.....  But it works....
    az = atan(!map.coso * sin(!map.u0-plon), $
              cosla * !map.sino - p2[2] * !map.coso  *cos(!map.u0-plon))
    !map.p[0] = dpi2-az
;    print, 'Pole: ', plon/dtor, plat /dtor, ', Az=',az / dtor
endif


uvrange_orig = uvrange          ;Save original UV

if is_cyl then begin            ;Adjust for rotation & wrap in cylind projs
;   Set the x coordinate of the edge in UV space for wrap-around detection.
;   This is only revelant in (pseudo) cylindrical projections.
    u = uvrange[[0,0,2,2]]  ;The 4 corners.
    v = uvrange[[1,3,1,3]]
    t1 = u * !map.cosr + v * !map.sinr ;Now rotate by - angle...
    v =  v * !map.cosr - u * !map.sinr
    uvrange[0] = min(t1, max=t2)
    uvrange[2] = t2
    uvrange[1] = min(v,  max=t2)
    uvrange[3] = t2
endif

if n_elements(limit) gt 0 then begin ;Get UV range.
    if keyword_set(scale) then $
      message, 'Conflicting keywords specified: LIMIT and SCALE', /INFO
    lim = limit                 ;Save old
    if n_elements(lim) eq 4 then begin
; Adjust longitude range:  lonmax > lonmin, and lonmin in the range of
; -180 to 180.  lonmin = 1, lonmax = 3
        lim = -10000 > lim < 10000  ; Sanity check
        if ABS(lim[3]-lim[1]) gt 360. then message, $
            'Longitude limits exceed 360 degrees', /CONTINUE
        while lim[3] le lim[1] do lim[3] += 360
        while lim[3] gt (lim[1]+360) do lim[3] -= 360
        t1 = [0, 360, 0, 360]
        while lim[1] ge 180 do lim -= t1
        while lim[1] lt -180 do lim += t1
        !map.ll_box = lim       ;Save if 4 point limit
    endif
    map_set_limits, lim, uvrange, CYLINDRICAL=is_cyl
endif

;*****************************************************************

if keyword_set(position) then  $
  plot, [0,1], xsty=5, ysty=5, /NODATA,    $
  TITLE=title, /NOERASE, POSITION=position, CHARSIZE=charsize, COLOR=color $
else begin          ;Position not set
                                ; Use margins
    if n_elements(xmargin) eq 1 then xmar = [xmargin, xmargin] $
    else if n_elements(xmargin) eq 2 then xmar = xmargin $
    else xmar = [1,1]
    if n_elements(ymargin) eq 1 then ymar = [ymargin, ymargin] $
    else if n_elements(ymargin) eq 2 then ymar = ymargin $
    else ymar = [1,2]
    plot, [0,1], XSTYLE=5, YSTYLE=5, /NODATA, COLOR=color,    $
      XMARGIN= xmar, YMARGIN= ymar, TITLE=title, /NOERASE, $
      CHARSIZE = charsize
endelse

!x.type = 3         ;For new maps;

!map.uv_box = uvrange
if keyword_set(noborder) eq 0 then begin
; fudge is used to add a bit of spacing between the border and the
; extent of the map region. fudge of 0.01 indicates that the extra
; spacing (internal map margin) should be 1% of the original map extent.
;
; this extra space is now turned off when the noborder keyword is set
;
    fudge = 0.01
    s = (uvrange[2]-uvrange[0]) * fudge
    ss = (uvrange[3] - uvrange[1]) * fudge
    uvrange = uvrange + [-s, -ss, s, ss]
endif

;   Figure the size of the drawing area on the screen in normalized units
x_size = !x.window[1]-!x.window[0]
y_size = !y.window[1]-!y.window[0]
x_cen = total(!x.window)/2.     ;Midpoints in X & Y
y_cen = total(!y.window)/2.

; Compute the X and Y scale factors, !x.s[1], !y.s[1]:
if keyword_set(scale) then begin ;Absolute scale provided?
                                ;Absolute size of entire map, meters / uvrange
    map_proj_info, /CURRENT, SCALE=meters ;Meters per UV unit
    s = meters / scale          ;scaled
    ss = !d.x_size / !d.x_px_cm / 100. ;width of window in meters
    !x.s[1] = s / ss
    !y.s[1] = !x.s[1] * !d.x_size / !d.y_size ;Adjust for aspect ratio
            ;Adjust the UV range to include the plotting window  ****
    t = [x_size / !x.s[1], y_size / !y.s[1]] / 2 ;half the UV range
    c = [uvrange[0]+uvrange[2], uvrange[1]+uvrange[3]]/2 ;UV center
; Take smallest UV box:
    uvrange = [c[0]-t[0] > uvrange[0], c[1]-t[1] > uvrange[1], $
               c[0]+t[0] < uvrange[2], c[1]+t[1] < uvrange[3]] ;New uv range
    !map.uv_box = uvrange

endif else if keyword_set(iso) then begin
 ; Scale in pixels/uvunit, correct for ISOMETRIC aspect ratio. Use smaller
 ; of X and Y scale factors
    sx = x_size * !d.x_size /(uvrange[2]-uvrange[0]) ;X scale
    sy = y_size * !d.y_size/(uvrange[3]-uvrange[1]) ;Y scale

    !x.s[1] = (sx < sy) / !d.x_size ;Set smaller
    !y.s[1] = !x.s[1] * !d.x_size / !d.y_size
    if sx gt sy then $          ;Resize window to fit map area
      !x.window = x_cen + sy/sx/2 * [-x_size, x_size] $
    else !y.window = y_cen + sx/sy/2 * [-y_size, y_size]
endif else begin                ;Scale to fit WINDOW
    !x.s[1] = x_size/(uvrange[2]-uvrange[0])
    !y.s[1] = y_size/(uvrange[3]-uvrange[1])
endelse

i = n_elements(reverse) eq 0 ? 0 : reverse ;Reverse axes?
if i and 1 then !x.s[1] = -!x.s[1] ;Flip left to right?
if (i and 2) ne 0 then !y.s[1] = -!y.s[1] ;Top to bottom?

; Compute offsets to center UV rectangle in the window
!x.s[0] = x_cen - (uvrange[0]+uvrange[2])/2. * !x.s[1]
!y.s[0] = y_cen - (uvrange[1]+uvrange[3])/2. * !y.s[1]

map_clip_set, /RESET        ;Clear clipping pipeline.

if keyword_set(clip) then begin ;Setup clipping
    r0 = 2

; Special case check for polar projection where the latitude range is
; limited.
    if (n_elements(lim) eq 4) then begin
        if (abs(p0lat) eq 90) and $ ;Center at pole?
          (p0lat eq lim[0] or p0lat eq lim[2]) then $ ;a lat limit is at pole
                                ;Fudge a little to so the last
                                ;parallel isn't clipped. (the 1.0e-5)
          r0 = -cos((abs(lim[2]-lim[0])+ 1.0e-5) * !dtor)
    endif

    if r0 eq 2 then map_set_clip, IPROJ=iproj, LIMIT=lim $ ;Set default clipping
    else map_set_clip, IPROJ=iproj, CLIP_RADIUS=r0, LIMIT=lim ;Set our radius

    if (n_elements(lim) gt 0) or keyword_set(scale) then $ ;Clip UV?
      MAP_CLIP_SET, CLIP_UV = uvrange
                                ; Set up plotting clip window
    t1 = (uvrange[[0,2]] * !x.s[1] + !x.s[0]) * !d.x_size
    !p.clip[[0,2]] = [t1[0] < t1[1], t1[0] > t1[1]]
    t1 = (uvrange[[1,3]] * !y.s[1] + !y.s[0]) * !d.y_size
    !p.clip[[1,3]] = [t1[0] < t1[1], t1[0] > t1[1]]
endif

if clip and total(abs(!map.ll_box)) eq 0 then $ ; Try to make a lon/lat range
  map_set_ll_box

if keyword_set(noborder) eq 0 then  $ ;Draw the border.
  plots, !x.window[[0,1,1,0,0]], !y.window[[0,0,1,1,0]], $
     COLOR=color, zvalue, /NORM, /NOCLIP, T3D=t3d

; Collect the common graphics keywords
map_struct_append, egraphics, "COLOR", color
map_struct_append, egraphics, "T3D", t3d
map_struct_append, egraphics, "ZVALUE", zvalue

; Process MAP_HORIZON keywords: **************
if keyword_set(horizon) or keyword_set(ehorizon) then begin
    map_struct_merge, ehorizon, egraphics ;Add common graphics keywords
    MAP_HORIZON, _EXTRA=ehorizon
endif

; Process MAP_CONTINENT keywords:   **************
if n_elements(mlinestyle) then map_struct_append, econt, "LINESTYLE", mlinestyle
if n_elements(mlinethick) then map_struct_append, econt, "THICK", mlinethick
if n_elements(con_color) then map_struct_append, econt, "COLOR", con_color
if n_elements(hires) then map_struct_append, econt, "HIRES", hires
if n_elements(continents) then map_struct_append, econt, "CONTINENTS", continents
if n_elements(usa) then map_struct_append, econt, "USA", usa
if n_elements(econt) gt 0 or keyword_set(continents) then begin
    map_struct_merge, econt, egraphics ;Add common graphics kwrds
    MAP_CONTINENTS, _EXTRA=econt
    endif

; Process MAP_GRID keywords:    **************
if n_elements(label)  then map_struct_append, egrid, "LABEL", label
if n_elements(latlab) then map_struct_append, egrid, "LATLAB", latlab
if n_elements(lonlab) then map_struct_append, egrid, "LONLAB", lonlab
if n_elements(latdel) then map_struct_append, egrid, "LATDEL", latdel
if n_elements(londel) then map_struct_append, egrid, "LONDEL", londel
if n_elements(latalign) then map_struct_append, egrid, "LATALIGN", latalign
if n_elements(lonalign) then map_struct_append, egrid, "LONALIGN", lonalign
do_grid = (keyword_set(grid) + n_elements(egrid) + n_elements(glinestyle) + $
           n_elements(glinethick)) ne 0

map_struct_merge, egrid, egraphics
if n_elements(glinestyle) then $ ;These keywords supersede those in egraphics
  map_struct_append, egrid, "LINESTYLE", glinestyle
if n_elements(glinethick) then $
  map_struct_append, egrid, "THICK", glinethick
if do_grid then MAP_GRID, CHARSIZE=charsize, _EXTRA=egrid

if KEYWORD_SET(ADVANCE) and !P.Multi[0] gt 0 THEN $
     !P.Multi[0] = !P.Multi[0] - 1 $
else !p.multi[0] = !p.multi[1] * !p.multi[2] - 1

end
