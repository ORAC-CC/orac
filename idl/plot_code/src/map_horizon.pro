; $Id: //depot/Release/IDL_81/idl/idldir/lib/map_horizon.pro#1 $
;
; Copyright (c) 2004-2011, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.

; Undocumented helper routines needed for
; MAP_SET, MAP_GRID, MAP_CONTINENTS.
;
; CT, RSI, June 2004: Split off from map_set.pro

;------------------------------------------------------------------------
; Create the outline of a Goodes Homolosine map.  If FILL is a
; non-zero scalar, store the points of the outline in FILL, otherwise
; just draw the outline.  If FILL is set, at the end, the array FILL
; will contain all the points for the outline.
;
PRO map_GoodeSeg, x0, y0, x1, y1, FILL=fill, _EXTRA=e
    COMPILE_OPT hidden

    nsegs = fix(18 > (abs(x1-x0) > abs(y1-y0))/5) ;Interval: 18 segs or every 5 degs
    x = x0 + findgen(nsegs+1) * (float(x1-x0) / nsegs) + !map.p0lon
    y = y0 + findgen(nsegs+1) * (float(y1-y0) / nsegs)

    if keyword_set(fill) eq 0 then plots, x, y, NOCLIP=0, _EXTRA=e $
    else begin
        if n_elements(fill) eq 1 then fill = transpose([[x],[y]]) $
        else fill = [[fill],[transpose([[x],[y]])]]
    endelse
end


;------------------------------------------------------------------------
; Draw/fill the horizon (limb) of a map
;
PRO map_horizon, FILL=fill, NVERTS=n, ZVALUE=zvalue, _EXTRA=Extra
    COMPILE_OPT hidden

    map_proj_info, /CURRENT, NAME=name, UV_RANGE=p, CIRCLE=is_circular, $
      CONIC=is_conic

    r = !map.rotation       ;Our rotation
    if is_conic then return         ;Can't do conics
    if n_elements(n) le 0 then n = 60 ;# of vertices
    a = findgen(n+1) * (2 * !pi / n) ;N angles from 0 to 2 pi
    bFill = keyword_set(fill)

    if name eq "Satellite" and !map.p[1] ne 0 then begin ;Satellite proj, tilted
        map_satellite_limit, 60, xr, yr
        r = 0.0                     ;No more rotation
    endif else if is_circular then begin ;Std circular projection
        xr = (p[2]-p[0])/2. * cos(a)
        yr = (p[3]-p[1])/2. * sin(a)
    endif else if name eq "Sinusoidal" then begin ;Sinusoidal
    ;    flon = (!map.out(3)-!map.out(2))/360.
        flon = 1.0
        xr = flon * !pi * cos(a)
        yr = fltarr(n+1)
        for i=0,n do case fix(a[i]/(!pi/2)) of
            0:  yr[i] = a[i]
            1:  yr[i] = !pi-a[i]
            2:  yr[i] = !pi-a[i]
            3:  yr[i] = a[i]-2*!pi
            4:  yr[i] = 0.0     ;Last pnt
        endcase
    endif else if name eq "Robinson" then begin ;Robinson
        lp = [2.628, 2.625, 2.616, 2.602, 2.582, 2.557, 2.532, 2.478, 2.422, $
              2.356, 2.281, 2.195, 2.099, 1.997, 1.889, 1.769, 1.633, 1.504, 1.399]
        lm = [0., 0.084, 0.167, 0.251, 0.334, 0.418, 0.502, 0.586, 0.669, 0.752, $
              0.833, 0.913, 0.991, 1.066, 1.138, 1.206, 1.267, 1.317, 1.349]
        xr = [lp, -reverse(lp), -lp, reverse(lp)]
        yr = [lm, reverse(lm), -lm, -reverse(lm)]
    endif else if name eq "GoodesHomolosine" then begin
        delta = 0.001               ;Fudge factor in degs for splitting
        N90 = 90-delta
        S90 = delta-90
        map_GoodeSeg, -180+delta, S90, -180+delta, N90, FILL=bFill, _EXTRA=Extra
        map_GoodeSeg, -40-delta, N90, -40-delta, 0, FILL=bFill, _EXTRA=Extra
        map_GoodeSeg, -40+delta, 0, -40+delta, N90, FILL=bFill, _EXTRA=Extra
        map_GoodeSeg, 180-delta, N90, 180-delta, S90, FILL=bFill, _EXTRA=Extra
        map_GoodeSeg, 80+delta, S90, 80+delta, 0, FILL=bFill, _EXTRA=Extra
        map_GoodeSeg, 80-delta, 0, 80-delta, S90, FILL=bFill, _EXTRA=Extra
        map_GoodeSeg, -20+delta, S90, -20+delta, 0, FILL=bFill, _EXTRA=Extra
        map_GoodeSeg, -20-delta, 0, -20-delta, S90, FILL=bFill, _EXTRA=Extra
        map_GoodeSeg, -100+delta, S90, -100+delta, 0, FILL=bFill, _EXTRA=Extra
        map_GoodeSeg, -100-delta, 0, -100-delta, S90, FILL=bFill, _EXTRA=Extra

       if n_elements(bFill) gt 1 then begin
    ; This is a kludge because filling in map coordinates makes wrap around
    ; errors around the north pole.
            bFill = CONVERT_COORD(REVERSE(bFill,2), /DATA, /TO_NORM)
            polyfill, bFill, NOCLIP=0, _Extra=Extra, /NORM
        endif
        return
    endif else if name eq "UserDefined" then begin ;User projection

    endif else begin                ;Rectangular
        xr = [p[0], p[2], p[2], p[0], p[0]]
        yr = [p[1], p[1], p[3], p[3], p[1]]
    endelse

    if r ne 0.0 then begin
        t1 = xr * !map.cosr + yr * !map.sinr ;Now rotate by - angle...
        yr =  yr * !map.cosr - xr * !map.sinr
        xr = t1
    endif

    xtsave = !x.type
    if n_elements(zvalue) eq 0 THEN zvalue = 0

    !x.type=0           ;Plot in UV space
    if keyword_set(fill) then polyfill, xr, yr, _EXTRA=Extra, NOCLIP=0 $
    else plots, xr, yr, zvalue, _EXTRA=Extra, NOCLIP=0
    !x.type=xtsave
end

