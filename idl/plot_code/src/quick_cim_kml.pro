;+
; QUICK_CIM_KML
;
; Given regular lat/lon grid data make Google Earth kml file
; including colour bar.
;
; Intended to be called multiple times so that different layers can be added
;
; for multiple images do ....
; 	quick_cim_kml,ofi,/open
;	for i=0,n do quick_cim_kml,ofi,d(*,*,i),/add,/noclose,title=title(i)
;	quick_cim_kml,ofi,/close
;
; or can just do do quick_cim_kml,ofi,d to make a kmz with one image in it
;
; PARAMETERS
;	OFI	Required output kmz filename - temporary files will 
;		be created initially - this file will only exist once
;		file closed (at end of this routine when noclose not set)
;	D	Data	 (NLON,NLAT)
;
; KEYWORDS
;	TITLE	Used as title for layer - should be short/suitable for use in filename
;	_EXTRA	Sent to quick_cim and cbar
;	ODI	Set output directory
;	ADD	Set to add layers to existing directory
;	NOCLOSE	Do not make the kmz as more layers to be added
;	OPEN	Set layer is selected when kmz openned
;	LATR	Specify latitude range if not global
;	LONR	Specify longitude range if not global
;	CLOSE	Just open the KML file for adding to
;	CLOSE	Just close the KML file (add no images) created by previous calls
;	SINSAMP	average data so pixel size on ground approx equal
;		(average more pixels together as fn of latitude)
;		Makes things look better on GE near the pole
;	RFAC	Factor by which to increase resolution of image, so as to
;		avoid smoothing in G-E
;-
function qck_sinsamp,d,latr,min_v=min_v
	d1=d
	nlat=n_elements(d(0,*))
	nlon=n_elements(d(*,0))
	dlat=double(latr(1)-latr(0))/nlat
	lats=expand_lag([latr,dlat],/mid)
	fac=long(1d0/cos(lats*!dtor))
	for ilat=0,nlat-1 do begin
		nb=1d0/fac
		di=d(*,ilat)
		ni=dblarr(nlon)+1
		if keyword_set(min_v) then begin
			wm=where(di lt min_v,nm)
			if nm gt 0 then begin
				di(wm)=0
				ni(wm)=0
			endif
		endif
		fi=fac(ilat)
		if fi gt 1 then begin
			if fi gt nlon/2 then begin
				dc=total(di)
				nc=total(ni)
				if nc eq 0 then dc=min_v-999
				dc=replicate(dc,nlon)
			endif else begin
				kn=replicate(1d0,fi)
				dc=convol(di,kn,/edge_wr)
				nc=convol(ni,kn,/edge_wr)
				isamp=(long(dindgen(nlon)/fi+0.5)*fi) mod nlon
				dc(isamp)=divide_nz(dc(isamp),nc(isamp))
				if keyword_set(min_v) then begin
					wb=where(nc(isamp) eq 0,nb)
					if nb gt 0 then dc(isamp(wb))=min_v-999
				endif
				dc=dc(isamp)
			endelse
			d1(0,ilat)=dc
		endif
	endfor
	return,d1
end
pro quick_cim_kml,fi,d,open=open,odi=odi,add=add,noclose=noclose,title=title,levels=levels,colors=colors,dtitle=dtitle,latr=latr,lonr=lonr,min_v=min_v,close=close,sinsamp=sinsamp,nodel=nodel,rfac=rfac,b32=b32,nodata=nodata,cbstep=cbstep,range=range
	if n_elements(rfac) eq 0 then rfac=3
	if keyword_set(close) then noclose=0
	if keyword_set(open) then noclose=1
	if n_elements(title) eq 0 then title='quick_cim_data'
	if n_elements(latr) eq 0 then latr=[-90,90]
	if n_elements(lonr) eq 0 then lonr=[-180,180]
	if n_elements(odi) eq 0 then odi=rsg_workdir()+'/kml_tmp/'
	kfi=odi+file_name(fi,/base)+'.kml'
	fti_rep=[' ','/',';','.','%','<','>','?','*','~','!','"',"'",'$','[',']']	; all these replace by '_' in output files
	mti=strreplace(file_name(fi),fti_rep,replicate('_',n_elements(fti_rep)),nrep=nrep)
	while max(nrep) gt 0 do mti=strreplace(mti,'__','_',nrep=nrep)
print,odi
	if keyword_set(close) then goto,close_file
	if not keyword_set(add) then begin
		file_delete,odi,/rec,/quiet
		file_mkdir,odi
	endif
	if not keyword_set(open) then begin
		sz=size(d)
		if sz(0) ne 2 then message,'2d data array expected'
;
; Make PNG file containing data
;
		fti=strreplace(title,fti_rep,replicate('_',n_elements(fti_rep)),nrep=nrep)
		while max(nrep) gt 0 do fti=strreplace(fti,'__','_',nrep=nrep)
print,fti
		sl=strlen(fti)
		if strmid(fti,sl-1) eq '_' then fti=strmid(fti,0,sl-1)
		file_mkdir,odi+fti
		pfi=odi+fti+'/'+fti+'.png'
		cfi=odi+fti+'/'+fti+'_cbar.png'
;
; now make basic image in z buffer
;
		d1=d
if keyword_set(b32) then begin
;
; input data is 32 bit image, so just write it
;
                lim=long(d1)
                imb=lim/256l/256l
                lim=lim-imb*(256l*256l)
                img=lim/256l
                imr=lim-img*256l
                im2=byte([[[imr]],[[img]],[[imb]]])
		iw=im2(*,*,0) eq 255 and im2(*,*,1) eq 255 and im2(*,*,2) eq 255
		im2=[[[im2]],[[(1b-iw)*255b]]]
		write_png,pfi,transpose(im2,[2,0,1])
endif else begin
		if keyword_set(sinsamp) then d1=qck_sinsamp(d1,latr,min_v=min_v)
		pd=!d.name
		pm=!p.multi
		tvlct,r,g,b,/get
		open_z,xsize=sz(1)*rfac,ysize=sz(2)*rfac
		!p.multi=[0,0,0,0,0]
		tvlct,r,g,b
		plot,[0,1],/nodata,xst=5,yst=5
		if n_elements(levels) eq 0 and keyword_set(range) then levels=clevels(range)
		quick_cim,d1,pos=[0,0,1,1],/nobar,_EXTRA=extra,levels=levels,colors=colors,min_v=min_v,xstyle=9,ystyle=9,/noborder,nodata=nodata
		im=tvrd()
		device,/close
;		if keyword_set(min_v) then begin
;			wm=where(d1 lt min_v,nm)
;			if nm gt 0 then im(wm)=!p.background
;		endif
        	write_png,pfi,im,r,g,b,transparent=[!p.background]
;
; now colour bar
;
		open_z,xsize=120,ysize=600
		tvlct,r,g,b
        	cbar,levels,colors,chars=2.,/nomu,pos=[0.65,0.01,0.99,0.99],title=dtitle,_EXTRA=extra,step=cbstep
        	imc=tvrd()
        	device,/close
        	write_png,cfi,imc,r,g,b;,transparent=[!p.background]
;
; return to original graphics device
;
		set_plot,pd
		!p.multi=pm
endelse
;
; now make KML file
;
	endif
	openw,lun,kfi,/get,append=add
if not keyword_set(add) then begin
	printf,lun,'<?xml version="1.0" encoding="UTF-8"?>'
	printf,lun,'<kml xmlns="http://earth.google.com/kml/2.1">'
	printf,lun,'<Folder>'
	printf,lun,'   <open>1</open>'
;	printf,lun,'   <LookAt>'
;	printf,lun,'      <latitude>40</latitude>'
;	printf,lun,'      <longitude>10</longitude>'
;	printf,lun,'      <range>4000000.</range>'
;	printf,lun,'   </LookAt>'
	printf,lun,'   <name>'+mti+'</name>'
;	printf,lun,'       <name>Icons</name>
;	printf,lun,'     <ScreenOverlay>
;	printf,lun,'       <name>Logo</name>
;	printf,lun,'       <Icon>
;	printf,lun,'         <href>globaer_logo.gif</href>
;	printf,lun,'       </Icon>
;	printf,lun,'       <overlayXY x="0" y="1" xunits="fraction" yunits="fraction"/>
;	printf,lun,'       <screenXY x="0.02" y="0.98" xunits="fraction" yunits="fraction"/>
;	printf,lun,'       <rotationXY x="0.5" y="0.5" xunits="fraction" yunits="fraction"/>
;	printf,lun,'       <size x="0.15" y="0" xunits="fraction" yunits="fraction"/>
;	printf,lun,'     </ScreenOverlay>
endif
if not keyword_set(open) then begin
	printf,lun,'<Folder>'
	printf,lun,'   <open>0</open>'
	printf,lun,'   <name>'+title+'</name>'
if not keyword_set(b32) then begin
	printf,lun,'   <ScreenOverlay>
	printf,lun,'       <name>Color bar</name>
	printf,lun,'       <Icon>
	printf,lun,'         <href>'+file_name(cfi)+'</href>
	printf,lun,'       </Icon>
	printf,lun,'       <overlayXY x="0" y="0" xunits="fraction" yunits="fraction"/>
	printf,lun,'       <screenXY x="0.02" y="0.02" xunits="fraction" yunits="fraction"/>
	printf,lun,'       <rotationXY x="0.5" y="0.5" xunits="fraction" yunits="fraction"/>
	printf,lun,'       <size x="0" y="0.4" xunits="fraction" yunits="fraction"/>
	printf,lun,'     </ScreenOverlay>
endif
	printf,lun,'   <GroundOverlay>'
	printf,lun,'      <name>Data</name>'
	printf,lun,'      <visibility>1</visibility>'
	;printf,lun,'      <color>dfffffff</color>'
	printf,lun,'      <color>ffffffff</color>'
	printf,lun,'      <drawOrder>0</drawOrder>'
	printf,lun,'      <Icon>'
	printf,lun,'        <href>'+file_name(pfi)+'</href>'
	printf,lun,'      </Icon>'
	printf,lun,'      <LatLonBox id="khLatLonBox716">'
	printf,lun,'        <north>'+trim_zero(latr(1))+'</north>'
	printf,lun,'        <south>'+trim_zero(latr(0))+'</south>'
	printf,lun,'        <east>'+trim_zero(lonr(1))+'</east>'
	printf,lun,'        <west>'+trim_zero(lonr(0))+'</west>'
	printf,lun,'      </LatLonBox>'
	printf,lun,'      <TimeSpan><begin>1900-01-01</begin><end>2999-01-01</end></TimeSpan>'
;	printf,lun,'        <altitude>100</altitude>'
; 	printf,lun,'        <altitudeMode>absolute</altitudeMode>'
	printf,lun,'    </GroundOverlay>'
	printf,lun,'</Folder>'
endif
	close_file,lun
close_file:
	if not keyword_set(noclose) then begin
		openw,lun,kfi,/get,/append
		printf,lun,'</Folder>'
		printf,lun,'</kml>'
		close_file,lun
		ofi1=new_name(fi,'kmz')
		ff=file_search(odi+'*/*.png',count=nfi)
		if nfi eq 0 then message,'No png files created!'
		spawn,'zip -jv '+ofi1+' '+kfi+' '+strcat(ff)
		if not keyword_set(nodel) then file_delete,/rec,odi
		message,/info,'Written file: '+ofi1
	endif
end
