; $Id: //depot/idl/IDL_71/idldir/lib/gamma_ct.pro#1 $
;
; Copyright (c) 1990-2009, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.
;

pro gamma_ct, gamma, CURRENT = current, INTENSITY = intensity
;+
; NAME:
;	GAMMA_CT
;
; PURPOSE:
;	Apply gamma correction to the color table.
;
; CATEGORY:
;	Image display.
;
; CALLING SEQUENCE:
;	GAMMA_CT, Gamma
;	GAMMA_CT, Gamma, /CURRENT
;
; INPUTS:
;	Gamma:	The value of gamma correction.  A value of 1.0 indicates a
;		linear ramp, i.e., no gamma correction.  Higher values of 
;		gamma give more contrast.  Values less than 1.0 yield lower 
;		contrast.
;
; KEYWORD PARAMETERS:
;     CURRENT:	If this keyword is set, apply correction from the current 
;		table.  Otherwise, apply from the original color table.  When
;		CURRENT is set, the color table input to GAMMA_CT
;		is taken from the R_CURR, G_CURR, and B_CURR variables.
;		Otherwise, input is from R_ORIG, G_ORIG, and B_ORIG.
;		The resulting tables are always saved in the "current" table.
;
;   INTENSITY:	If this keyword is set, correct the individual intensities of 
;		each color in the color table.  Otherwise, shift the colors 
;		according to the gamma function.
;
; OUTPUTS:
;	No explicit outputs.  The modified color table vectors
;	are saved in the COLORS common block, as the variables
;	r_curr, g_curr, and b_curr variables.
;
; COMMON BLOCKS:
;	COLORS:	The IDL color table common block.
;
; SIDE EFFECTS:
;	A new color table is loaded, and its contents are placed
;	in the "current" variables of the COLORS common block.
;
; RESTRICTIONS:
;	None.
;
; PROCEDURE:
;	Straightforward.  The gamma correction is implemented
;	as x^gamma, where x is the range of color table indices
;	scaled from 0 to 1.
;
; MODIFICATION HISTORY:
;	DMS, Oct, 1990. Added ability shift intensities of colors, rather 
;			than the mapping of the colors.  DMS, April, 1991.
;-

common colors, r_orig, g_orig, b_orig, r_curr, g_curr, b_curr

n = !d.table_size

if n_elements(r_orig) le 0 then begin
	r_orig = indgen(n) & r_curr = r_orig
	g_orig = r_orig & g_curr = g_orig
	b_orig = r_orig & b_curr = b_orig
	endif

if n_elements(gamma) le 0 then gamma = 1.0

if keyword_set(intensity) then begin
  s = byte(256 *((findgen(256)/256.)^gamma))   ;Scale individ intensities
  if keyword_set(current) then begin
	r_curr = s[r_curr]
	g_curr = s[g_curr]
	b_curr = s[b_curr]
  endif else begin
	r_curr = s[r_orig]
	g_curr = s[g_orig]
	b_curr = s[b_orig]
  endelse
endif else begin   ;Scale color mapping, not intensities
  s = long(n*((findgen(n)/n)^gamma))
  if keyword_set(current) then begin
	r_curr = r_curr[s]
	g_curr = g_curr[s]
	b_curr = b_curr[s]
  endif else begin
	r_curr = r_orig[s]
	g_curr = g_orig[s]
	b_curr = b_orig[s]
  endelse
endelse
tvlct,r_curr, g_curr, b_curr
end
