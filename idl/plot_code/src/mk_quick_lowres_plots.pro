;==========================================================================
;+
;	pro MK_QUICK_LOWRES_PLOTS
;
;	Description
;       This routine produces some quick looks generated using info in
;secondary file
; 
;	Use
; 
;	Parameters
;  
; 
;	Keywords
;       filein: secondary file to plot
;       ps
;       fdir:fileout
;       i37 use 3.7 rather than 1.6
;       lowres generate low res files-not working yet

;	History
;       First version produced to visualise CCI cloud products. 1.6um
;       option looks nicer than 3.7um
;
;	Bugs
;
;	C.Poulsen : 15 April 2016
; $Id$
;-
;==========================================================================
pro mk_quick_lowres_plots,filein=filein,ps=ps,fdir=fdir,i37=i37,lowres=lowres

;
;readin the secondary file and rename some of the variables
;

if ~keyword_set(filein) then files='/misc/wantage_static/cpoulsen/cloud_ecv/postproc/2008/06/20/ESACCI-L2-CLOUD-CLD-AATSR_CC4CL_Envisat_200806202211_fv2.0.secondary.nc'
if keyword_set(filein) then files=filein
print,files

filep=strreplace(files,'secondary','primary')

if ~keyword_set(fdir) then fdir=file_dirname(files)
fb=file_basename(filep,'.primary.nc')
psfi=fdir+'/'+fb+'.ps'

if keyword_set(ps) then ps_open,psfi
nxa=11
set_a4,n=nxa+1,/rs,/landscape
mask=[[0,1,2,3,4,5],[6,7,8,9,10,11]]
        
        if n_elements(p1) eq 0 then p1=[0.02,0.02,0.99,0.93]
        if n_elements(p2) eq 0 then p2=[0.0,0.0,0.98,0.96]

        p1a=[0.02,0.02,0.99,0.93]
        p2a=[0.0,0.5,0.98,0.96]


lat=ncdf_get(filep,'lat')
lon=ncdf_get(filep,'lon')

u=ncdf_get(files,'scanline_u')
v=ncdf_get(files,'scanline_v')

REFLECTANCE_IN_CHANNEL_NO_2=ncdf_get(files,strlowcase('REFLECTANCE_IN_CHANNEL_NO_2'))

REFLECTANCE_IN_CHANNEL_NO_3=ncdf_get(files,strlowcase('REFLECTANCE_IN_CHANNEL_NO_3'))

REFLECTANCE_IN_CHANNEL_NO_4=ncdf_get(files,strlowcase('REFLECTANCE_IN_CHANNEL_NO_4'))



BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_5=ncdf_get(files,strlowcase('BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_5'))
BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6=ncdf_get(files,strlowcase('BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6'))
BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7=ncdf_get(files,strlowcase('BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7'))

nlines=n_elements(REFLECTANCE_IN_CHANNEL_NO_2.value(1,*))


nx=512l
ny=1000l
nplots=fix(nlines/ny)
;
;begin loop over orbit
;


for i=0,nplots-1 do begin
   nys=i*ny
   
   
   nye=nys+ny-1
   nmeas=5
   
   meas=fltarr(nmeas,nx,ny)
   
   
   meas(0,*,*)=REFLECTANCE_IN_CHANNEL_NO_2.value(*,nys:nye)*REFLECTANCE_IN_CHANNEL_NO_2.scale_factor+REFLECTANCE_IN_CHANNEL_NO_2.add_offset
   
   meas(1,*,*)=REFLECTANCE_IN_CHANNEL_NO_3.value(*,nys:nye)*REFLECTANCE_IN_CHANNEL_NO_3.scale_factor+REFLECTANCE_IN_CHANNEL_NO_3.add_offset
   
   if keyword_set(i37) then begin
      meas(2,*,*)=BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_5.value(*,nys:nye)*BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_5.scale_factor+BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_5.add_offset
print,'using 3.7'
   endif else begin
      meas(2,*,*)=REFLECTANCE_IN_CHANNEL_NO_4.value(*,nys:nye)*REFLECTANCE_IN_CHANNEL_NO_4.scale_factor+REFLECTANCE_IN_CHANNEL_NO_4.add_offset
   endelse
   
   meas(3,*,*)=BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6.value(*,nys:nye)*BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6.scale_factor+BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6.add_offset
   
   meas(4,*,*)=BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7.value(*,nys:nye)*BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7.scale_factor+BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7.add_offset
   
;
; plot a map and approx position of data
;
   
   map_loc,mean1(lat.value(*,nys:nye)),mean_lon(reform(lon.value(1,nys:nye))),$
           position=ypos(p1a,p2a,mask=mask),chars=chs,/nop,zoom=0.25,/horiz
   
   bg=setup_llbin(lat.value(*,nys:nye),lon.value(*,nys:nye),[-90,90,1],[-180,180,1])
   wg=where(bg.n gt 0,ng)
   if ng gt 0 then begin
      iglat=wg/n_elements(bg.mlon)
      iglon=wg mod n_elements(bg.mlon)
      for ig=0l,ng-1 do polyfill,bg.lon(iglon(ig)+[0,1,1,0,0]),$
                                 bg.lat(iglat(ig)+[0,0,1,1,0]),col=2
      map_continents
   endif
;
;false colour image
;
   
;
;reduce resolution herey
;
;   redu=4
;   newnx=nx/redu
;   newny=ny/redu
   
;   newmeasa=fltarr(nmeas,newnx,newny)
;   newmeasa=meas(*,indgen(newnx)*redu,indgen(newny)*redu)
   
   if n_elements(fac) eq 0 then fac=0.5
   newmeas=reform(meas,nmeas,nx*ny)
   
   xc={y:newmeas}
   
   ha_sav=rd_sav('/misc/cluster_home/cpoulsen/orac_svn/plot_code/src/ha.sav')
   ha_sav.sv.zstar=0 
   sv_temp=ha_sav.sv
   s_temp=ha_sav.s
   
   nstate=5
   nmeas=5
   datein='20080620'
   
   whgood =where(meas(*,*,1) gt 0,ngood)
   
   latr=range(lat.value(*,nys:nye))
   lonr=range(lon.value(*,nys:nye))
   
   
   inst='AATSR'
   sg_temp={LATR:latr,LONR:lonr,UG:fltarr(3),VG:fltarr(3),LAT:lat.value(*,nys:nye),LON:lon.value(*,nys:nye),ID: inst+'-scan',SAT:1,NEQ:0,LL:0,SD:0,solz:0,satz:0,relaz:0 }
   
   ha_temp={FILE:filep,OFILE:files,PROC_DATE:datein,DATA_DATE:datein,S:s_temp,SV:sv_temp,TIME:0,DIA:2,TYPES:'LIQUID',LUTRE_RANGE:dblarr(2,2),LOPD_RANGE:dblarr(2,2),NSTR:0,RSA:0,BAUM:0,SPI:1,YE:fltarr(nmeas),CTEST:-1,IDC:0,MAX_IT:20,SYI:0,EXTRA:0.0,HX:0,VERSION:3.18,NX:nstate,NY:nmeas,npix:ngood,NST:2,sg:sg_temp}  
   
   if n_elements(ur) eq 0 then ur=range(u.value(*,nys:nye))
   if n_elements(vr) eq 0 then vr=range(v.value(*,nys:nye))


;   d16=min(abs(1.6-h.s.mwl))
;   d37=min(abs(3.7-h.s.mwl))
   
;   if d16 gt 0.5 and d37 lt 0.5 then i37=1 
;   if d16 gt 0.5 and d37 lt 0.5 then print,'i37'   

   im_mea=mk_cldmodel_false_color_cloud(xc,ha_temp,i37=i37)
   
   quick_cim_prc,kml=kmlfi,ha_temp.sg,reform(u.value(*,nys:nye),512000),reform(v.value(*,nys:nye),512000),im_mea,ur=ur,vr=vr,num=2,fac=fac,$
                 position=ypos(p1,p2,mask=mask),chars=chs,title='False colour - meas',image=image,lev=lev,_EXTRA=extra,ext=ext,/tru,/axti,/b32,lcb=lcb,bcb=bcb,nodata=256l*256l*256l-1,eop_col=eop_col,eop_thick=def_th,eop_y=eop_y,eop_x=eop_x
any_key
endfor;nplots

if keyword_set(ps) then ps_close,/pdf

end
