; $Id: //depot/idl/IDL_71/idldir/lib/interpol.pro#1 $
;
; Copyright (c) 1982-2009, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.

Function ls2fit, xx, y, xm

COMPILE_OPT idl2, hidden

x = xx - xx[0]                  ;Normalize to preserve significance.
ndegree = 2L
n = n_elements(xx)

corrm = fltarr(ndegree+1, ndegree+1) ;Correlation matrix
b = fltarr(ndegree+1)

corrm[0,0] = n                  ;0 - Form the normal equations
b[0] = total(y)
z = x                           ;1
b[1] = total(y*z)
corrm[[0,1],[1,0]] = total(z)
z = z * x                       ;2
b[2] = total(y*z)
corrm[[0,1,2], [2,1,0]] = total(z)
z = z * x                       ;3
corrm[[1,2],[2,1]] = total(z)
corrm[2,2] = total(z*x)         ;4

c = b # invert(corrm)
xm0 = xm - xx[0]
return, c[0] + c[1] * xm0 + c[2] * xm0^2
end



;+
; NAME:
;	INTERPOL
;
; PURPOSE:
;	Linearly interpolate vectors with a regular or irregular grid.
;	Quadratic or a 4 point least-square fit to a quadratic
;	interpolation may be used as an option.
;
; CATEGORY:
;	E1 - Interpolation
;
; CALLING SEQUENCE:
;	Result = INTERPOL(V, N) 	;For regular grids.
;
;	Result = INTERPOL(V, X, XOUT)	;For irregular grids.
;
; INPUTS:
;	V:	The input vector can be any type except string.
;
;	For regular grids:
;	N:	The number of points in the result when both input and
;		output grids are regular.
;
;	Irregular grids:
;	X:	The absicissae values for V.  This vector must have same # of
;		elements as V.  The values MUST be monotonically ascending
;		or descending.
;
;	XOUT:	The absicissae values for the result.  The result will have
;		the same number of elements as XOUT.  XOUT does not need to be
;		monotonic.  If XOUT is outside the range of X, then the
;		closest two endpoints of (X,V) are linearly extrapolated.
;
; Keyword Input Parameters:
;	LSQUADRATIC = if set, interpolate using a least squares
;	  quadratic fit to the equation y = a + bx + cx^2, for each 4
;	  point neighborhood (x[i-1], x[i], x[i+1], x[i+2]) surrounding
;	  the interval, x[i] <= XOUT < x[i+1].
;
; 	QUADRATIC = if set, interpolate by fitting a quadratic
; 	  y = a + bx + cx^2, to the three point neighborhood (x[i-1],
; 	  x[i], x[i+1]) surrounding the interval x[i] <= XOUT < x[i+1].
;
;	SPLINE = if set, interpolate by fitting a cubic spline to the
;	  4 point neighborhood (x[i-1], x[i], x[i+1], x[i+2]) surrounding
;	  the interval, x[i] <= XOUT < x[i+1].
;
;	Note: if LSQUADRATIC or QUADRATIC or SPLINE is not set,  the
;	default linear interpolation is used.
;
; OUTPUTS:
;	INTERPOL returns a floating-point vector of N points determined
;	by interpolating the input vector by the specified method.
;
;	If the input vector is double or complex, the result is double
;	or complex.
;
; COMMON BLOCKS:
;	None.
;
; SIDE EFFECTS:
;	None.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	For linear interpolation,
;	Result(i) = V(x) + (x - FIX(x)) * (V(x+1) - V(x))
;
;	where 	x = i*(m-1)/(N-1) for regular grids.
;		m = # of elements in V, i=0 to N-1.
;
;	For irregular grids, x = XOUT(i).
;		m = number of points of input vector.
;
;	  For QUADRATIC interpolation, the equation y=a+bx+cx^2 is
;	solved explicitly for each three point interval, and is then
;	evaluated at the interpolate.
;	  For LSQUADRATIC interpolation, the coefficients a, b, and c,
;	from the above equation are found, for the four point
;	interval surrounding the interpolate using a least square
;	fit.  Then the equation is evaluated at the interpolate.
;	  For SPLINE interpolation, a cubic spline is fit over the 4
;	point interval surrounding each interpolate, using the routine
;	SPL_INTERP().
;
; MODIFICATION HISTORY:
;	Written, DMS, October, 1982.
;	Modified, Rob at NCAR, February, 1991.  Made larger arrays possible
;		and correct by using long indexes into the array instead of
;		integers.
;	Modified, DMS, August, 1998.  Now use binary intervals which
;		speed things up considerably when XOUT is random.
;       DMS, May, 1999.  Use new VALUE_LOCATE function to find intervals,
;       	which speeds things up by a factor of around 100, when
;       	interpolating from large arrays.  Also added SPLINE,
;       	QUADRATIC, and LSQUADRATIC keywords.
;   CT, ITTVIS, Feb 2009: CR54942: Automatically filter out NaN values.
;     Clean up code.
;-
;
FUNCTION INTERPOL, VV, XX, XOUT, SPLINE=spline, LSQUADRATIC=ls2, QUADRATIC=quad

  COMPILE_OPT idl2
  
  on_error,2                      ;Return to caller if an error occurs
  
  regular = n_params(0) eq 2
  
  ; Make a copy so we don't overwrite the input arguments.
  v = vv
  x = xx
  m = N_elements(v)               ;# of input pnts

  if (regular) then nOut = LONG(x)
  
  ; Filter out NaN values in both the V and X arguments.
  isNAN = FINITE(v, /NAN)
  if (~regular) then isNAN or= FINITE(x, /NAN)
  
  if (~ARRAY_EQUAL(isNAN, 0)) then begin
    good = WHERE(~isNAN, ngood)
    if (ngood gt 0 && ngood lt m) then begin
      v = v[good]
      if (regular) then begin
        ; We supposedly had a regular grid, but some of the values
        ; were NaN (missing). So construct the irregular grid.
        regular = 0b
        x = LINDGEN(m)
        xout = FINDGEN(nOut) * ((m-1.0) / ((nOut-1.0) > 1.0)) ;Grid points
      endif
      x = x[good]
    endif
  endif

  ; get the number of input points again, in case some NaN's got filtered
  m = N_elements(v)
  type = SIZE(v, /TYPE)

  if regular && $                ;Simple regular case?
    ((keyword_set(ls2) || keyword_set(quad) || keyword_set(spline)) eq 0) $
    then begin
    xout = findgen(nOut)*((m-1.0)/((nOut-1.0) > 1.0)) ;Grid points in V
    xoutInt = long(xout)		;Cvt to integer
    case (type) of
    1: diff = v[1:*] - FIX(v)
    12: diff = v[1:*] - LONG(v)
    13: diff = v[1:*] - LONG64(v)
    15: diff = LONG64(v[1:*]) - LONG64(v)
    else: diff = v[1:*] - v
    endcase
    return, V[xoutInt] + (xout-xoutInt)*diff[xoutInt] ;interpolate
  endif

  if regular then begin           ;Regular intervals??
    xout = findgen(nOut) * ((m-1.0) / ((nOut-1.0) > 1.0)) ;Grid points
    s = long(xout)                 ;Subscripts
  endif else begin                ;Irregular
    if n_elements(x) ne m then $
      message, 'V and X arrays must have same # of elements'
    s = VALUE_LOCATE(x, xout) > 0L < (m-2) ;Subscript intervals.
  endelse

  ; Clip interval, which forces extrapolation.
  ; XOUT[i] is between x[s[i]] and x[s[i]+1].
  
  CASE (1) OF
  
  KEYWORD_SET(ls2): BEGIN  ;Least square fit quadratic, 4 points
    s = s > 1L < (m-3)          ;Make in range.
    p = replicate(v[0]*1.0, n_elements(s)) ;Result
    for i=0L, n_elements(s)-1 do begin
        s0 = s[i]-1
        p[i] = ls2fit(regular ? s0+findgen(4) : x[s0:s0+3], v[s0:s0+3], xout[i])
    endfor
    END
    
  KEYWORD_SET(quad): BEGIN ;Quadratic.
    s = s > 1L < (m-2)          ;In range
    x1 = regular ? float(s) : x[s]
    x0 = regular ? x1-1.0 : x[s-1]
    x2 = regular ? x1+1.0 : x[s+1]
    p = v[s-1] * (xout-x1) * (xout-x2) / ((x0-x1) * (x0-x2)) + $
        v[s] *   (xout-x0) * (xout-x2) / ((x1-x0) * (x1-x2)) + $
        v[s+1] * (xout-x0) * (xout-x1) / ((x2-x0) * (x2-x1))
    END
    
  KEYWORD_SET(spline): BEGIN
    s = s > 1L < (m-3)          ;Make in range.
    p = replicate(v[0], n_elements(s)) ;Result
    sold = -1
    for i=0L, n_elements(s)-1 do begin
        s0 = s[i]-1
        if sold ne s0 then begin
            x0 = regular ? s0+findgen(4): x[s0: s0+3]
            v0 = v[s0: s0+3]
            q = spl_init(x0, v0)
            sold = s0
        endif
        p[i] = spl_interp(x0, v0, q, xout[i])
    endfor
    END
    
  ELSE: begin              ;Linear, not regular
    case (type) of
    1: diff = v[s+1] - FIX(v[s])
    12: diff = v[s+1] - LONG(v[s])
    13: diff = v[s+1] - LONG64(v[s])
    15: diff = LONG64(v[s+1]) - LONG64(v[s])
    else: diff = v[s+1] - v[s]
    endcase
    p = (xout-x[s])*diff/(x[s+1] - x[s]) + v[s]
    end
    
  ENDCASE

  RETURN, p
end
