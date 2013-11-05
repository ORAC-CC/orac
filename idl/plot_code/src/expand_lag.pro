;===============================================================================
;+
; EXPAND_LAG
;
; Given a regular grid specified by 3 element array which defines [start,stop,interval],
; return the grid as an explicit array of all grid points
;
; PARAMETERS
;	LAG	Three element array defining grid as above	
;
; KEYWORDS
;	NP	Set to variable to contain number of points in grid
;	MID	Set to return grid if mid points of bins whose boundaries
;		are defined by LAG
;	M1	Do not return last level
;
; R.S. 04/04/08
; $Id: expand_lag.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function expand_lag,lag1,np=np,mid=mid,m1=m1
	if keyword_set(mid) then begin
		lag=[lag1(0)+lag1(2)/2,lag1(1)-lag1(2)/2,lag1(2)]
	endif else lag=lag1
	np=long((lag(1)-lag(0))/lag(2)+1d-9)+1
	sz=size(lag)
	type=sz(sz(0)+1)
	if keyword_set(m1) then np=np-1
	if type eq 2 then return,indgen(np)*lag(2)+lag(0) $
	else if type eq 3 then return,lindgen(np)*lag(2)+lag(0) $
	else if type eq 4 then return,findgen(np)*lag(2)+lag(0) $
	else if type eq 5 then return,dindgen(np)*lag(2)+lag(0) $
	else message,'Bad type for LAG'
end
