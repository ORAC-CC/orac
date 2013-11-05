;===============================================================================
;+
; KMAP_SET
;
; Same as MAP_SET, but p0lat,p0lon,rot are specified by keyword origin.
; Also this routine defaults to /grid,/cont keywords
;
; PARAMETERS
;	None
;
; KEYWORDS
;       ORIGIN  3 element vector of [p0lat,p0lon,rot]
;       _EXTRA  Sent to map_limits
;       NOGRID  Don't do grid
;       NOCONT  Don't do continents
;       LIMIT   Directly set map limit
;       SUBTITLE        Set map subtitle
;       NOERASE is set to 1 by default
;       STEREOGRAPHIC   As map_set
;       PTS     use plots to add specified points to map. 2-d array [LONS,LATS]
;       ADVANC  As map_set
;       FILL    Fill coast lines
;       RLATR   Return lat range plotted
;       RLONR   Return lon range plotted
;       CITY    Run OPLOT_CITIES
;       POP     Sent to OPLOT_CITIES
;       CPSYM   Sent to OPLOT_CITIES
;       CNAME   Sent to OPLOT_CITIES
;       CCHARSIZE       Sent to OPLOT_CITIES
;       CCIMAGE Used internally by quick_cc
;       CSYMSI  Sent to OPLOT_CITIES
;       SPOLE   Set for southern hemisphere polar stereographic
;       NPOLE   Set for northen hemisphere polar stereographic
;       N30     N.hem north of 30degN, polar ster.
;       ROT     Set rotation for SP,NP
;	CTHICK	line thickness for map_continents
;
; R.S. 21/8/96
; $Id: kmap_set.pro 429 2010-10-01 11:21:31Z rsiddans $
;-
;===============================================================================
pro kmap_set,origin=origin,_EXTRA=extra,nogrid=nogrid,nocont=nocont,$
	limit=limit,$
	subtitle=subtitle,noerase=noerase,stereographic=ster,$
	pts=pts,$
	advanc=adv,fill=fill,$
	rlatr=rlatr,rlonr=rlonr,city=city,pop=pop,cpsym=cpsym,cname=cname,$
	ccharsize=ccharsize,ccimage=ccimage,csymsi=csymsi,$
	spole=spole,npole=npole,n30=n30,rot=rot,cthick=cthick,color=color

	if keyword_set(ccimage) then begin
;
; by default do not draw continents on quick_cc data plotted using /image
;
		if n_elements(nocont) eq 0 then nocont=1
		if n_elements(nogrid) eq 0 then nogrid=1
	endif

	if n_elements(adv) eq 0 then adv=0
	if n_elements(noerase) eq 0 then noerase=1
	if not keyword_set(limit) then limit=map_limits(_EXTRA=extra,spole=spole,npole=npole,n30=n30)
	rlatr=limit[[0,2]]
	rlonr=limit[[1,3]]
	if not keyword_set(ster) then ster=0 else ster=1
	if n_elements(rot) eq 0 then rot=0.
	if keyword_set(npole) or keyword_set(n30) then begin
		ster=1
		origin=[90.,0.,rot]
	endif
	if keyword_set(spole) then begin
		ster=1
		origin=[-90.,0.,rot]
	endif
	if not keyword_set(origin) then origin=[0.,0,0]
	if not keyword_set(nocont) then cont=1 else cont=0
	if not keyword_set(nogrid) then grid=1 else grid=0
	origin=[origin,0.,0.,0.]
	map_set,origin(0),origin(1),origin(2),_EXTRA=extra,grid=grid,cont=cont,$
		limit=limit,noerase=noerase,ster=ster,adv=adv
	if keyword_set(color) then begin
		tvlct,r0,g0,b0,/get
		set_color,/ps
		hsv_ct,s=0.3
                dc=!p.color
                !p.color=4
                map_grid,fill_horiz=1,/horiz
                !p.color=dc
		if n_elements(fillcol) eq 0 then fillcol=8
		fill=1
	endif
	if keyword_set(cthick) then map_continents,thick=cthick
	if keyword_set(fill) then map_continents,fill=fill,col=fillcol
	if keyword_set(color) then tvlct,r0,g0,b0
	if keyword_set(pts) then begin
		plots,pts(*,0),pts(*,1),psy=1,noclip=0,symsize=1./float(!p.multi(2))
	endif
	if keyword_set(city) then oplot_cities,pop=pop,name=cname,psym=cpsym,chars=ccharsize,symsi=csymsi
end

