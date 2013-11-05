; $Id: //depot/Release/IDL_81/idl/idldir/lib/map_point_valid.pro#1 $
;
; Copyright (c) 2004-2011, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.

; Undocumented helper routines needed for
; MAP_SET, MAP_GRID, MAP_CONTINENTS.
;
; CT, RSI, June 2004: Split off from map_set.pro

;------------------------------------------------------------------------
; Return 1 if the point at geo-coordinates (lon, lat) is mappable,
;    0 if not.
; Mappable means that the point is transformable, and is not clipped
;   by the mapping pipeline.
; Input: lon, lat = input coordinates in degrees.
; Output: u, v = uv coordinates if point is mappable.  The contents of
;   u and v are undefined if the function returns 0, indicating that the
;   point is not mappable.
;
FUNCTION map_point_valid, lon, lat, u, v
    COMPILE_OPT hidden

    u = 0.0 & v = 0.0
    p = convert_coord(lon, lat, /DATA, /TO_NORM) ;Cvt to normalized coords
    if finite(p[0]) eq 0 then return, 0
    u = (p[0] - !x.s[0]) / !x.s[1]  ;To UV coords
    v = (p[1] - !y.s[0]) / !y.s[1]

    clat = cos(lat * !dtor)         ;Cvt lon lat to cartesian 3D coords
    xyz = [ clat * cos(lon * !dtor), clat * sin(lon * !dtor), sin(lat * !dtor)]

    p = !map.pipeline
    s = size(p)
    for i=0, s[2]-1 do begin        ;# of stages
        case p[0,i] of
            0 : return, 1           ;0 = END, means we're successful.
            1 : dummy = 0           ;Ignore splits
            2 : if total(xyz * p[1:3,i]) + p[4,i] lt 0 then $ ;Outside clip plane?
              return, 0
            3 : dummy = 0           ;Ignore transforms
            4 : if u lt p[1,i] or v lt p[2,i] or $ ;Within UV bounding box?
              u gt p[3,i] or v gt p[4,i] then return, 0
        endcase
    endfor

    return, 1                       ;Should never hit here.
end                             ;map_point_valid

