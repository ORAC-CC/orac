;$Id: //depot/Release/IDL_81/idl/idldir/lib/mean.pro#1 $
;
; Copyright (c) 1997-2011, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.
;+
; NAME:
;       MEAN
;
; PURPOSE:
;       This function computes the mean of an N-element vector. 
;
; CATEGORY:
;       Statistics.
;
; CALLING SEQUENCE:
;       Result = MEAN(X)
;
; INPUTS:
;       X:      An N-element vector of type integer, float or double.
;
; KEYWORD PARAMETERS:
;       DIMENSION: Set this keyword to a scalar indicating the dimension
;         across which to calculate the mean. If this keyword is not
;         present or is zero, then the mean is computed across all
;         dimensions of the input array. If this keyword is present,
;         then the mean is only calculated only across a single dimension.
;         In this case the result is an array with one less dimension
;         than the input.
;       DOUBLE: IF set to a non-zero value, computations are done in
;               double precision arithmetic.
;       NAN:    If set, treat NaN data as missing.
;
; EXAMPLE:
;       Define the N-element vector of sample data.
;         x = [65, 63, 67, 64, 68, 62, 70, 66, 68, 67, 69, 71, 66, 65, 70]
;       Compute the standard deviation.
;         result = MEAN(x)
;       The result should be:
;       66.7333
;
; PROCEDURE:
;       MEAN calls the IDL function MOMENT.
;
; REFERENCE:
;       APPLIED STATISTICS (third edition)
;       J. Neter, W. Wasserman, G.A. Whitmore
;       ISBN 0-205-10328-6
;
; MODIFICATION HISTORY:
;       Written by:  GSL, RSI, August 1997
;       CT, Dec 2009: (from J. Bailin) Added DIMENSION keyword.
;       CT, Dec 2010: Fix NAN keyword to filter out both NaN and Infinity
;             when calculating the count of good values.
;-
function MEAN, X, DIMENSION=dimension, DOUBLE = Double, NAN = nan

  compile_opt idl2, hidden
  ON_ERROR, 2

  if KEYWORD_SET(dimension) then begin
    xdims = SIZE(X, /DIMENSION)
    if (N_ELEMENTS(dimension) gt 1 || $
      dimension[0] lt 1 || dimension[0] gt N_ELEMENTS(xdims)) then begin
      MESSAGE, 'Illegal keyword value for DIMENSION.'
    endif
    if (KEYWORD_SET(nan)) then begin
      nX = TOTAL(FINITE(x), dimension[0], /INTEGER)
    endif else begin
      nX = xdims[dimension[0]-1]
    endelse
    return, TOTAL(X, dimension, DOUBLE=double, NAN=nan)/nX
  endif else begin
    if (KEYWORD_SET(nan)) then begin
      nX = TOTAL(FINITE(x), /INTEGER)
    endif else begin
      nX = N_ELEMENTS(x)
    endelse
    return, TOTAL(X, DOUBLE=Double, NAN=nan)/nX
  endelse
END
