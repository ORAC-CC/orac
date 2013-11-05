; $Id: //depot/idl/IDL_71/idldir/lib/map_satellite_limit.pro#1 $
;
; Copyright (c) 2004-2009, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.

; Undocumented helper routines needed for
; MAP_SET, MAP_GRID, MAP_CONTINENTS.
;
; CT, RSI, June 2004: Split off from map_set.pro

;------------------------------------------------------------------------
; Return n or less points, defining the limit of the limb of a satellite
; projection
;
PRO map_satellite_limit, n, xr, yr

    COMPILE_OPT hidden

    a = findgen(n) * (2 * !pi / (n-1))
    st = !map.p[0] > 1.0
    st = sqrt((st-1.)/(st+1.))  ;Map limit radius for satellite projection
    xr = cos(a) * st        ;Taken from Snyder Pg. 175
    yr = sin(a) * st
    a =  yr * !map.cosr + xr * !map.sinr
    xr = xr * !map.cosr - yr * !map.sinr
    yr = a
    a = yr * !map.p[2]/(!map.p[0]-1.0) + !map.p[3]
    good = where(a gt 0.0, count)
    if count eq 0 then begin        ;If nothing, return nothing
        trash = temporary(xr) & trash = temporary(yr)
        return
    endif
    if count lt n then begin        ;Extract & shift so pnts on map are contiguous
        i0 = 0          ;Get 1st good pnt following a bad pnt
        for i=1,count-1 do if good[i-1]+1 ne good[i] then i0 = i
        good = shift(good,-i0)
    endif
    a = a[good]
    xr = xr[good] * !map.p[3] / a
    yr = yr[good]/a

end



