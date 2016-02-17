;===============================================================================
;+
; PLOT_RET_CLDMODEL_MODIS
;
; Wrapper to plot_ret_cldmodel to set options useful for plotting modis results
;
; PARAMETERS
;	
;
; KEYWORDS
;	KML	Make KML file for viewing on Google earth
;	RKML	Set resolution of sinusoidal grid used with KML file
;       ERROR : plot uncertainties of retrieved values
;       COMB: add combined solution option
;       fatplot: plot images wider than default plotting, better for
;modis granules
;
; R.S. 28/03/11
; CP 4 4/2013 CP added error keyword
; cp : 20/06/2013 added in nosec keyword
; cp : 24/2/2014 added night variable
; cp : 24/06/2014 added fatplot

; $Id: plot_ret_cldmodel_modis.pro 937 2011-06-10 09:36:13Z rsiddans $
;-
;===============================================================================
pro plot_ret_cldmodel_modis,x,h,_EXTRA=extra,clon=clon,clat=clat,eop_y=eop_y,eop_x=eop_x,kml=kml,rkml=rkml,error=error,nosec=nosec,fips=fips,night=night,fatplot=fatplot,inst=inst,aer=aer,noir=noir
;
; change v to be the index of along-track lines with valid data (avoid confusing
; null data gaps)
;
;
; identify lines with no good data
;
	v1=standard_grid_1d2d(h.v,h.u,h.v)
	nx=n_elements(v1(*,0))
	v0=total(long(v1 lt -10),1,/pres)
	wok=where(v0 ne nx,nok)
	if nok eq 0 then message,'No points ok !'
;
; identify these lines in the 2d array
;
	wv=where(h.v gt -10,nv)
	if nv eq 0 then message,'No points ok !'
	vr=range(h.v(wv))
;
; define a table which can be used to translate
; the original line number into the index in the list
; of lines with good data
;
	vt=lonarr(vr(1)-vr(0)+1)
	vt(wok)=lindgen(nok)
	v2=h.v
	v2(wv)=vt(v2(wv)-vr(0))
;
; use this modified line number in the plot
;
	h1=h
	h1.v=v2

	if h.sv.n_total eq 4 then begin
		mask=[[0,1,2,3,4,5],[6,7,8,9,10,11]]
	endif else if h.sv.n_total eq 5 then begin
		mask=[[0,1,2,3,4,5,13],[6,7,8,9,10,11,12]]
	endif else if h.sv.n_total eq 6 or h.sv.n_total eq 7 then begin
		mask=[[0,1,2,3,4,5],[6,7,8,9,10,11],[12,13,14,15,16,17]]
	endif else message,'Not set up for current number of state vector elements'
	if keyword_set(kml) then begin
	nr=n_elements(mask(0,*))
	mask=mask(*,indgen(nr*2)/2)
	mask(0,1)=-1
;
; define sinusoidal grid for the data
; when plotted it will be gridded to it
;
		if n_elements(kres) eq 0 then kres=4008l*3l
		sg=define_standard_grid(neq=kres)
		ll2sinusoidal,h1.sg.lat,h1.sg.lon,u,v,neq=sg.neq
		h1.u=u
		h1.v=v
		h1=struct_rep(h1,'sg',sg)
	endif
;
; get image coords of calipso track
;
	if keyword_set(eop_x) then begin
		ext='extra_oplot'
		eop_thick=def_th()
		eop_col=c0()
		ur=range(h1.u)
		eop_x1=float(eop_x-ur(0))/(ur(1)-ur(0))
		eop_y1=float(vt(eop_y-vr(0)))
		vr=range(h1.v)
		eop_y1=float(eop_y1-vr(0))/(vr(1)-vr(0))
		wy=where(eop_y1 ne 0,ny)
		if ny gt 0 then begin
			eop_y1=eop_y1(wy)
			eop_x1=eop_x1(wy)
		endif
	endif
if night gt 0 then begin
	plot_ret_cldmodel_cloud,x,h1,/axti,_EXTRA=extra,mas=mask,p1=[0.01,0.01,0.99,0.98],chars=1.5,crd=0.2,cbl=0.5,$
		p2=[0.14,0.01,0.99,0.92],ext=ext,eop_thick=eop_thick,eop_col=eop_col,eop_x=eop_x1,eop_y=eop_y1,kml=kml,error=error,/noresid,nosec=nosec,fips=fips,/night,fatplot=fatplot,v1=v1,inst=inst,aer=aer,noir=noir
     endif else begin

plot_ret_cldmodel_cloud,x,h1,/axti,_EXTRA=extra,mas=mask,p1=[0.01,0.01,0.99,0.98],chars=1.5,crd=0.2,cbl=0.5,$
		p2=[0.14,0.01,0.99,0.92],ext=ext,eop_thick=eop_thick,eop_col=eop_col,eop_x=eop_x1,eop_y=eop_y1,kml=kml,error=error,/noresid,nosec=nosec,fips=fips,fatplot=fatplot,v1=v1,inst=inst,aer=aer,noir=noir

endelse

end
