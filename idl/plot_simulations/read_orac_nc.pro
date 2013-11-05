;==========================================================================
;+
;	pro READ_ORAC_NC_RENAME
;
;	Description
;       reads netcdf file and write data into a format that is
;       accepted by plot routine.
;       This routine requires that 4 netcdf files are present the primary and secondary; netcdf files and both ice and water phases.
; 
;	Use
; 
;	Parameters
;  filein: directory +filename with/without .nc extension
;  
;	Keywords
;
;
;	History
;
;	Bugs
;       typical usage idl>read_orac_nc(filein=xxx,/plot)
;	Bugs
;
;	C.Poulsen : 19 December 2011
; $Id$
;-
;==========================================================================
function read_orac_nc,filein=filein,plot=plot
if n_elements(filein) eq 0 then filein='/disks/scratch/cpoulsen/200806201240MODISWAT_380-750v320.primary.nc'

;
;this file is used as a template
;
if strpos(filein,'MODIS') gt 0 then inst='modis'
if strpos(filein,'ATSR') gt 0 then inst='atsr'
if strpos(filein,'AVHRR') gt 0 then inst='avhrr'

if inst eq 'atsr' or inst eq 'avhrr' then message,'routine not valid for this instrument'+' '+inst

fb=file_basename(filein,'primary.nc')
fdir=file_dirname(filein)

checkp=file_search(fdir+'/'+fb+'*.nc',count=pdim)
print,checkp

if pdim lt 2 then message,'not all files present'

posice=strpos(fb,'ICE')
poswat=strpos(fb,'WAT')

if poswat gt 0 then fbice=strreplace(fdir+'/'+fb,'WAT','ICE')
if posice gt 0 then fbwat=strreplace(fdir+'/'+fb,'ICE','WAT')

if poswat gt 0 then fbwat=fdir+'/'+fb
if posice gt 0 then fbice=fdir+'/'+fb

xpice=ncdf_read(fbice+'primary.nc')
xsice=ncdf_read(fbice+'secondary.nc')

xpwat=ncdf_read(fbwat+'primary.nc')
xswat=ncdf_read(fbwat+'secondary.nc')

print,fbice+'primary.nc';
;read in above and refill it
;
nph=2
nmeas=5
nstate=5
res=size(xpice.lat)
nx=res(1)
ny=res(2)

;
;loop over both phase retrievals
;

for p=0,nph-1 do begin
   if p eq 0 then xp=xpwat
   if p eq 1 then xp=xpice

   if p eq 0 then xs=xswat
   if p eq 1 then xs=xsice

   
   xn=fltarr(nx,ny,nstate)
   x0=fltarr(nx,ny,nstate)
   unc=fltarr(nx,ny,nstate)
   meas=fltarr(nx,ny,nmeas)
   xn_sx=fltarr(nx,ny,nstate,nstate)
   ymfit=fltarr(nx,ny,nmeas)
  yn=fltarr(nx,ny,nmeas)
  y0=fltarr(nx,ny,nmeas)

   
   xn(*,*,0)=xp.cot*xp.cot_att.scale_factor
   xn(*,*,1)=xp.ref*xp.ref_att.scale_factor
   xn(*,*,2)=xp.ctp*xp.ctp_att.scale_factor
   xn(*,*,3)=xp.stemp*xp.stemp_att.scale_factor
   xn(*,*,4)=xp.cc_total*xp.cc_total_att.scale_factor

   x0(*,*,0)=xs.cot_fg*xs.cot_fg_att.scale_factor
   x0(*,*,1)=xs.ref_fg*xs.ref_fg_att.scale_factor
   x0(*,*,2)=xs.ctp_fg*xs.ctp_fg_att.scale_factor
   x0(*,*,3)=xs.stemp_fg*xs.stemp_fg_att.scale_factor
   
   unc(*,*,0)=xp.cot_uncertainty*xp.cot_uncertainty_att.scale_factor
   unc(*,*,1)=xp.ref_uncertainty*xp.ref_uncertainty_att.scale_factor
   unc(*,*,2)=xp.ctp_uncertainty*xp.ctp_uncertainty_att.scale_factor
   unc(*,*,3)=xp.stemp_uncertainty*xp.stemp_uncertainty_att.scale_factor
   unc(*,*,4)=xp.cc_total_uncertainty*xp.cc_total_uncertainty_att.scale_factor
   
;
;for MODIS only
;
   if inst eq 'modis' then begin
      meas(*,*,0)=xs.REFLECTANCE_IN_CHANNEL_NO_1*xs.REFLECTANCE_IN_CHANNEL_NO_1_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_1_ATT.add_offset
      meas(*,*,1)=xs.REFLECTANCE_IN_CHANNEL_NO_2*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
      meas(*,*,2)=xs.REFLECTANCE_IN_CHANNEL_NO_6*xs.REFLECTANCE_IN_CHANNEL_NO_6_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_6_ATT.add_offset
      meas(*,*,3)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_31*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_31_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_31_ATT.add_offset
      meas(*,*,4)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_32*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_32_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_32_ATT.add_offset
;set_a4,/rs,ro=2
;quick_cc,meas(*,*,0)
;quick_cc,meas(*,*,3)
;quick_cc,meas(*,*,3)
;stop
      y0(*,*,0)=xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_1*xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_1_ATT.scale_factor+xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_1_ATT.add_offset
      y0(*,*,1)=xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_2*xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
      y0(*,*,2)=xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_6*xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_6_ATT.scale_factor+xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_6_ATT.add_offset
      y0(*,*,3)=xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_31*xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_31_ATT.scale_factor+xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_31_ATT.add_offset
      y0(*,*,4)=xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_32*xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_32_ATT.scale_factor+xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_32_ATT.add_offset
;
;nb note add_offset required
;
      ymfit(*,*,0)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_1*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_1_ATT.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_1_ATT.add_offset
      ymfit(*,*,1)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2_ATT.add_offset
      ymfit(*,*,2)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_6*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_6_ATT.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_6_ATT.add_offset
      ymfit(*,*,3)=xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_31*xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_31_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_31_ATT.add_offset
      ymfit(*,*,4)=xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_32*xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_32_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_32_ATT.add_offset
      
   endif

;
 ;write data to a structure remember scalling factors
;
costbadja=where(xp.costja le 0,ncostja)
costbadjm=where(xp.costjm le 0,ncostjm)
print,ncostja,ncostjm

if ncostja gt 0 then xp.costja(costbadja)=0.0
if ncostjm gt 0 then xp.costjm(costbadjm)=0.0



   xin={y:meas,y0:y0,yn:meas*0.0-999.,xn:xn,sx:unc,cost:(xp.costja+xp.costjm)*xp.COSTJM_ATT.SCALE_FACTOR,conv:abs(xp.convergence-1),tc:xp.ctt,itype:xp.niter*0+p,zc:xp.cth,ni:xp.niter,white_sky_albedo:meas(*,*,0)*0.0-999.,clearsky_bt:meas*0.0-999.,xo:meas*0.0-999.,ae:meas(*,*,*)*0.0-999,ym:meas,cwp:meas(*,*,0)*0.0-999.,lat:xp.lat,lon:xp.lon,ymfit:ymfit,phase:meas(*,*,0)*0,x0:x0}
   
;
;define this for setting up ret structure
;
nmeas=5
nstate=5

;
;define ret
;


igood = where(xin.xn(*,*,2) gt 0. and xin.ymfit(*,*,2) gt -30.,ngood,complement=ibad,ncomplement=nbad)
print,'ngood',ngood
if p eq 0 then begin
ret=replicate({y:fltarr(nmeas),y0:fltarr(nmeas),yn:fltarr(nmeas),xn:fltarr(nstate),cost:0.0,conv:0,tc:0.0,itype:0,COLUMN_DENSITY:0.0,zc:0.0,ni:0,white_sky_albedo:0.0,clearsky_bt:1.0,x0:fltarr(nstate),xe:fltarr(nstate), COLUMN_DENSITY_ERROR:0.0,ymfit:fltarr(nmeas),sx:fltarr(nstate),ae:fltarr(nstate),lat:0.0,lon:0.0},nph,ngood)
endif

ngood2 =n_elements(xp.lat)

;
;reform 2 d array
;
xin_y=reform(xin.y(*,*,*),ngood2,nmeas)
xin_y0=reform(xin.y0(*,*,*),ngood2,nmeas)
xin_ymfit=reform(xin.ymfit(*,*,*),ngood2,nmeas)
xin_xn=reform(xin.xn(*,*,*),ngood2,nstate)
xin_x0=reform(xin.x0(*,*,*),ngood2,nstate)
xin_yn=reform(xin.yn(*,*,*),ngood2,nmeas)
xin_sx=reform(xin.sx(*,*,*),ngood2,nmeas)
xin_ae=reform(xin.ae(*,*,*),ngood2,nstate)
;set_a4,/rs,ro=2
;quick_cc,xin.y(*,*,0)
;quick_cc,xin.ymfit(*,*,0)
iretgood = where(xin_xn(*,2) gt 0,nretgood,complement=ibad,ncomplement=nbad)


   ret(p,*).y(0:nmeas-1)= reform(transpose(xin_y(iretgood,*)),nmeas,1,ngood)
   ret(p,*).Y0(0:nmeas-1)=reform(transpose(xin_y0(iretgood,*)),nmeas,1,ngood)
   ret(p,*).YN(0:nmeas-1)=reform(transpose((xin_ymfit(iretgood,*)+xin_y(iretgood,*))),nmeas,1,ngood)

;set_a4,/rs,ro=2
;quick_cc,xin.ymfit(*,*,0),title='ymfit ch1'
;quick_cc,xin.ymfit(*,*,3),title='ymfit ch4'
;print,range(xin.ymfit(*,*,0))
;stop


   ret(p,*).Ymfit(0:nmeas-1)=reform(transpose(xin_ymfit(iretgood,*)),nmeas,1,ngood)
whbad=where(ret(p,*).ymfit(4) lt -30., nbad)

if nbad gt 0 then begin
for ii=1,4 do ret(p,whbad).YN(ii)=-999.
for ii=1,4 do ret(p,whbad).Y(ii)=-999.
for ii=1,4 do ret(p,whbad).Ymfit(ii)=-999.
for ii=1,4 do print,range(ret(0,*).YN(ii))


endif

for i=0,4 do print,'ymfit',i,range(xin_ymfit(iretgood,i))
for i=0,4 do print,'meas',i,range(xin_y(iretgood,i))
for i=0,4 do print,i,'y',range(xin_ymfit(iretgood,i)+xin_y(iretgood,i))


   ret(p,*).XN=reform(transpose(xin_xn(iretgood,*)),nstate,1,ngood)
      ret(p,*).sx=reform(transpose(xin_sx(iretgood,*)),nstate,1,ngood)
   ret(p,*).COST=reform(transpose(xin.cost(igood)),1,ngood) 
   ret(p,*).lat=reform(transpose(xin.lat(igood)),1,ngood) 
   ret(p,*).lon=reform(transpose(xin.lon(igood)),1,ngood) 
   ret(p,*).CONV=reform(transpose(xin.conv(igood)),1,ngood) 
   ret(p,*).TC = reform(transpose(xin.tc(igood)),1,ngood) 
   ret(p,*).ITYPE=p
   ret(p,*).ZC= reform(transpose(xin.zc(igood)),1,ngood)
   ret(p,*).NI=   reform(transpose(xin.ni(igood)),1,ngood) 
   ret(p,*).WHITE_SKY_ALBEDO=reform(transpose(xin.white_sky_albedo(igood)),1,ngood)  ;reform(transpose(xin.albedo(igood,index_chans_ref)),nchans_ref,1,ngood)
   ret(p,*).CLEARSKY_BT= 0.0 ;reform(xin.meas(igood,4)*0.0,1,ngood) 
   ret(p,*).X0 =  reform(transpose(xin_x0(iretgood,*)),nstate,1,ngood)
;   ret(p,*).ae =  reform(transpose(xin.ae(igood,*)*0.0-999.),nstate,1,ngood)
;        ret(p,*).XE=  reform(transpose(xin.uncertainty(igood,*)),nstate,1,ngood)  
   ret(p,*).COLUMN_DENSITY_ERROR=reform(xin.cwp(igood),1,ngood)
   
endfor ;phas

sg_temp={LATR:fltarr(2),LONR:fltarr(2),UG:fltarr(3),VG:fltarr(3),LAT:xin.lat(igood),LON:xin.lon(igood),ID: 'modis-scan',SAT:1,NEQ:0,LL:0,SD:0 }

ha_sav=rd_sav('h_sav.sav')
sv_temp=ha_sav.sv

s_temp=ha_sav.s

datein=strmid(fb,0,12)
print,datein


ha_temp={FILE:fb,OFILE:fb,PROC_DATE:datein,DATA_DATE:datein,S:s_temp,SV:sv_temp,U:xs.SCANLINE_U[igood],V:xs.SCANLINE_V[igood],TIME:fltarr(ngood),SG:sg_temp,DIA:2,TYPES:'LIQUID',LUTRE_RANGE:dblarr(2,2),LOPD_RANGE:dblarr(2,2),NSTR:0,RSA:0,BAUM:0,SPI:1,YE:fltarr(nmeas),CTEST:-1,IDC:0,MAX_IT:20,SYI:0,EXTRA:0.0,HX:0,VERSION:3.18,NX:nstate,NY:nmeas,npix:ngood,NST:2}  


ha_temp.sv.zstar=0

latr=range(xp.lat[igood])
lonr=range(xp.lon[igood])
ha_temp.sg.LATR =latr 
ha_temp.sg.LONR =lonr 
xu=range(xs.SCANLINE_U[igood])
xv=range(xs.SCANLINE_V[igood])
ha_temp.sg.UG  =   [xu(0)+1,xu(1)+1,1] 
ha_temp.sg.VG  =  [xv(0)+1,xv(1)+1,1]  
ha_temp.sg.LAT=xp.lat[igood] ;      
ha_temp.sg.LON=xp.lon[igood] ;      



if n_elements(plot) gt 0 then begin
plot_ret_cldmodel_modis,ret,ha_temp,_EXTRA=extra,clon=clon,clat=clat,eop_y=eop_y,eop_x=eop_x
endif
return,ret


end
