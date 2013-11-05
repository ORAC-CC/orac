;===============================================================================
;+
; MAP_LIMITS
;
; Return some standard limit values to produce maps over specific areas.
;
; PARAMETERS
;
; KEYWORDS
;       UK      Set limit for map of UK
;       EUROPE  Set limit for map of Europe
;       N_EUROPE        Set limit for UK + Northen France/Holland
;       SCAND   Set limit for scandinavia
;       NPOLE   Set for northen hemisphere polar stereographic
;       SEA     South east asia
;       INDO    Indian Ocean region
;       SUBTRO  lat+/- 45
;       AUST    Austalia / NZ / indonesia
;       AFRICA  set for map of Africa
;       NAMERICA        North america
;       SPOLE   Set for southern hemisphere polar stereographic
;       USA     USA
;       N30     N.hem north of 30degN, polar ster.
;       E20     Europe + africa N of 20.
;       LATRANGE        specify lat range
;       LONRANGE        specify lon range
;       SEVIRI  Approx SEVIRI disk
;       AMAZON  as it says
;
; R.S. 21/8/96
; $Id: map_limits.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function map_limits,uk=uk,$
	europe=europe,n_europe=n_europe,scand=scand,$
	npole=npole,sea=sea,indo=indo,subtro=subtro,aust=aust,$
	africa=africa,namerica=nam,spole=spole,usa=usa,n30=n30,$
	e20=e20,latrange=latrange,lonrange=lonrange,$
	seviri=seviri,amazon=amazon

	limit=[-90,-180,90,180]
	if keyword_set(uk) then limit=[45,-15,60,10] $
	else if keyword_set(europe) then limit=[30,-20,76,35] $
	else if keyword_set(seurope) then limit=[30,-20,60,35] $
	else if keyword_set(e20) then limit=[20,-20,75,35] $
	else if keyword_set(n_europe) then limit=[45,-11,62,15] $
	else if keyword_set(scand) then limit=[50,-10,75,50] $
	else if keyword_set(npole) then limit=[45,0,90,360] $
	else if keyword_set(n30) then limit=[30,0,90,360] $
	else if keyword_set(spole) then limit=[-90,0,-45,360] $
	else if keyword_set(africa) then limit=[-40,-20,60,60] $
	else if keyword_set(nam) then limit=[10,-180,70,-20] $
	else if keyword_set(sam) then limit=[-57,-120,30,-30] $
	else if keyword_set(usa) then limit=[22,-130,50,-50] $
	else if keyword_set(aust) then limit=[-55,90,30,180] $
	else if keyword_set(indo) then limit=[-55,10,30,160] $
	else if keyword_set(subtro) then limit=[-45,-180,45,180] $
	else if keyword_set(sea) then limit=[-10,60,50,160] $
	else if keyword_set(seviri) then limit=[-65,-65,65,65] $
	else if keyword_set(trop) then limit=[-30,-180,30,180] $
	else if keyword_set(amazon) then limit=[-30,-95,15,-25]
	limit=double(limit)
	if keyword_set(latrange) then begin
		limit(0)=latrange(0)
		limit(2)=latrange(1)
	endif
	if keyword_set(lonrange) then begin
		limit(1)=lonrange(0)
		limit(3)=lonrange(1)
	endif
	return,double(limit)
end
