; $Id: //depot/Release/IDL_81/idl/idldir/lib/map_proj_info.pro#1 $
;
; Copyright (c) 2004-2011, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.

; Undocumented helper routines needed for
; MAP_SET, MAP_GRID, MAP_CONTINENTS.
;
; CT, RSI, June 2004: Split off from map_set.pro

;------------------------------------------------------------------------
; Return information about maps in general, or projection iproj.
; Iproj = projection number, Input.  If CURRENT is set, then the
;   current map projection is returned in Iproj.
; CURRENT = if set, use current proj number, and return that index in iproj.
; UV_RANGE =  UV limits of the given projection, [umin, vmin, umax,
;   vmax].  Also keyword UVRANGE is accepted here for bkwds compat.
; SCALE = (output) the number of meters to one unit of UV space.  The
;     radius of the earth, from Clarke's 1866 ellipsoid is used (6378206).
; NAME = the name of the projection.
; CYLINDRICAL = 1 if the projection is cylindrical or pseudo-cylindrical.
; CONIC = 1 if the projection is conic.
; PROJ_NAMES = string array containing the names of all the
;   available projections.
; UV_LIMITS = returned rectangle of the current map in UV coordinates,
;   [Umin, Vmin, Umax, Vmax].
; LL_LIMITS = returned rectangle of the current map in latitude/longitude
;   coordinates, [Lonmin, Latmin, Lonmax, Latmax].  Note, the
;   range may not always be available, and the min and max may
;   both be set to 0.
;
; Issues for a new projection:
; UVrange, proj_scale, is_cyl, is_azimuthal, circle, name, horizon, clipping
;
PRO map_proj_info, iproj, CURRENT=current, $
       UV_RANGE=r, UVRANGE=rdummy, SCALE=meters, NAME=name, CYLINDRICAL=cyl, $
       PROJ_NAMES = proj_names_p, CIRCLE=is_circular, CONIC=is_conic, $
       AZIMUTHAL=is_azimuthal, UV_LIMITS = uv_limits, $
       LL_LIMITS = ll_limits

    COMPILE_OPT hidden


    common map_set_common, proj_scale, proj_is_cyl, proj_is_conic, $
      proj_names, circle, proj_is_azim, user_defined

    on_error, 2
    if n_elements(proj_scale) eq 0 then begin ;Init constant arrays?
        p2 = 2 * !pi
        s8 = 2 * sqrt(8)
        ro = 2 * 2.628

    ;      St Or LC La Gn  Az Sa  Cy  Me  Mo  Si  Ai  Ha Al Tm  Mi Ro Le Go
    ;       1  2  3  4  5   6  7   8   9  10  11  12  13 14 15  16 17 18 19
      proj_scale =  $
        [0, 4, 2, 4, 4, 4, p2, 1, p2, p2, s8, p2, p2, s8, 4, 0, p2,ro, 0,p2]
      proj_is_cyl = $
        [0, 0, 0, 0, 0, 0,  0, 0,  1,  1,  1,  1,  1,  1, 0, 1,  1, 1, 0, 1]
      proj_is_azim = $
        [0, 1, 1, 0, 1, 1,  1, 0,  0,  0,  0,  0,  0,  0, 0, 0,  0, 0, 0, 0]
      proj_is_conic = $
        [0, 0, 0, 1, 0, 0,  0, 0,  0,  0,  0,  0,  0,  0, 1, 0,  0, 0, 1, 0]
      circle = $
        [0, 1, 1, 0, 1, 1,  1, 1,  0,  0,  1,  0,  1,  1, 0, 0, 0,  0, 0, 0]
      proj_names =  $
        ["", "Stereographic", "Orthographic", "LambertConic", "LambertAzimuthal", $
         "Gnomic", "AzimuthalEquidistant", "Satellite", "Cylindrical", $
         "Mercator", "Mollweide", "Sinusoidal", "Aitoff", "HammerAitoff", $
         "AlbersEqualAreaConic", "TransverseMercator", $
         "MillerCylindrical", "Robinson", "LambertConicEllipsoid", $
         "GoodesHomolosine"]

      user_defined = bytarr(n_elements(proj_names))
    endif

    max_proj = n_elements(proj_names)-1
    if arg_present(proj_names_p) then proj_names_p = proj_names
    if KEYWORD_SET(current) then iproj = !map.projection ;Get current projection

    if ARG_PRESENT(uv_limits) then uv_limits = !map.uv_box
    if ARG_PRESENT(ll_limits) then ll_limits = !map.ll_box

    if n_elements(iproj) eq 0 then return ;Rest of params are proj specific

    if iproj lt 1 or iproj gt max_proj then $
      message, 'Projection number must be within range of 1 to'+ strtrim(max_proj,2)

    ones = [-1., -1., 1., 1.]
    r_earth = 6378206.4d0 ;Earth equatorial radius, meters, ; Clarke 1866 ellipsoid

    cyl = proj_is_cyl[iproj]
    name = proj_names[iproj]
    meters = proj_scale[iproj] * r_earth ;Scale [= 0 if 1:1]
    is_circular = circle[iproj]
    is_conic = proj_is_conic[iproj]
    is_azimuthal = proj_is_azim[iproj]

    if arg_present(r) or arg_present(rdummy) or arg_present(meters) then begin
        case iproj of
        1: r = 2 * ones             ;Stereo
        2: r = ones                 ;Ortho
        3: r = 2 * ones             ;Lambert Conic for sphere.
        4: r = 2 * ones             ;lambert
        5: r = 2 * ones             ;gnomic
        6: r =  !DPI * ones         ;azimuth
        7: if !map.p[1] ne 0 then begin ;Complicated satellite?
            map_satellite_limit, 60, xr, yr
            if n_elements(xr) eq 0 then r = [0,0,0,0] $
            else r = [min(xr, max=maxxr), min(yr, max=maxyr), maxxr, maxyr]
            meters = (r[2]-r[0]) * r_earth
        endif else begin
            st = !map.p[0] > 1.0   ;For satellite
            r = sqrt((st-1)/(st+1)) * ones ;Simple case satellite
            meters = (r[2]-r[0]) * r_earth
        endelse
        8: r = [-1., -.5, 1., .5] * !DPI ;Cylind equidistant
        9: r = [-!DPI, -2.43, !DPI, 2.43] ;Mercator  (+/- 80 degs of lat) ~ 98%
        10: r = sqrt(2.0) * [-2.,-1.,2.,1.] ;Mollweide
        11: r = [-1., -.5, 1., .5] * !DPI ;sinusoidal
        12: r = [-1., -.5, 1., .5] * !DPI ;Aitoff
        13: r = sqrt(2.0) * [-2., -1., 2., 1.] ; Hammer-aitoff
        14: r = 2 * ones            ;Albers Conic
        15: r = 2*!pi * r_earth * ones ;Transverse Mercator, Units = meters.
        16: r = [-!DPI, -1.832, !DPI, 1.832] ;Miller Cylind  (+/- 80 degs of lat)
        17: r = [-2.628, -1.349, 2.628, 1.349] ;Robinson
        18: r = 2 * !pi * r_earth * ones ;Lambert Conic ellipsoid, Units = meters
        19: r = [-!DPI, -1.37, !DPI, 1.37] ;Goodes Homolosine
    endcase

    rdummy = r                      ;For backwards compatibility
    if meters ne 0 then meters = meters / (r[2]-r[0]) $ ;Get scale in UV units
    else meters = 1.0               ;UV Units are meters.
    endif

end


