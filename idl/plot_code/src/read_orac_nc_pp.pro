;==========================================================================
;+
;	pro READ_ORAC_NC_pp
;
;	Description:
;
;       reads orac netcdf post processed file and writes data into a common format that is
;       accepted by plot routine.
;       *****Important info******This routine requires that 2 netcdf
;           files are present the primary and secondary as the
;            secondary has the measurement information and residual information
;       file names must have WAT, ICE or PP in them
;
; 
;      This routine returns a structure with retrieval information
;
;
;	Use:
;
;       N.B this routine uses lots of subroutines which are packaged
;       with this routine. The main plot routine is plot_ret_cldmodel.pro
;       Edit this routine to add new plots
;       this routine was written to adapt from idl output. It could be
;       more efficiently written to read directly from netcdf if someone
;       wants to do this!
;
;	Parameters:
;  
;  
;	Keywords:
;
;       filein: directory +filename with/without .nc extensionplot
;       plot: create plots
;       ps: creates pdf file     
;       itype : set to water or ice when just doing one type of file
;       errflag: returns flag=1 if all files are not present
;       wat: just plot at wat retrievals
;       ice: : just plot ice retrievals
;       dirin: directory to search for files to read
;       dwd : keyword to cope with fact dwd file  have slightly
;different file name and are missing some reflectances in secondary
;file eventually this keyword should become redundant.
;       nosec : set this keyword when secondary file not present
;	History:
;
;       original file C. Poulsen created 2011 1st public release April 2013
;	
;       typical usage idl>x=read_orac_nc_pp(/plot,/ps,/wat)
;       typical usage idl>x=read_orac_nc_pp(/plot,/ps,/error) ;PP file
;       typical usage idl>x=read_orac_nc_pp(/plot,/dwd) ;PP file
;
;	Bugs:
;       Currently reads as default the ice secondary file for residual
;       and measurement information until proper level2 file is created.
;       Will probably fall over with a night file
;       code is hardwired for 5 measurements and 5 state vectors
;       works differently for dwd and ral files
;
;       RAL:
;       to package up newer versions of this software do:
;       mk_vm_src_code -mirror read_orac_nc_pp then rsync
;
;
;	C.Poulsen : 19 December 2011
; cp : 20/06/2013 added in nosec keyword
; cp fixed up reading out of uncertainty information
; cp 5/12/2013 changes to accomodate avhrr data
; $Id$
;-
;==========================================================================
function read_orac_nc_pp,filein=filein,plot=plot,ps=ps,errflag=errflag,wat=wat,ice=ice,dirin=dirin,dwd=dwd,nosec=nosec,error=error,version=version

;if n_elements(version) eq 0 then version='_V3.0'
if n_elements(version) eq 0 then version=''
errflag=0
if ~keyword_set(dirin) then dirin='/misc/wantage_static/cpoulsen/cloud_ecv/postproc/2008/05/13/'


if keyword_set(dwd) then begin
dirin='/cmsaf/cmsaf-cld4/mjerg/test_plotting/'
;dirin='/misc/cluster_home/rsg/Data/projects/ecv_clouds/matthias_test_files/'

dirin='/cmsaf/cmsaf-cld4/mjerg/test_plotting/20080620_AVHRR_noaa16_proc_2_process_ID8206794_US1362069592/noaa16_20080620_0955_99999_satproj_00000_12180_avhrr/post/'
filein=dirin+'20080620095500-ESACCI-L2_CLOUD-CLD_PRODUCTS-AVHRRGAC-NOAA16-fv1.0.nc'
endif

if ~keyword_set(filein) then filein=dirin+'*'+version+'*.PP.primary.nc'

print,'filein a',filein

dirin=file_dirname(filein)+'/'

print,'dirin read_orac_nc_pp',dirin

files=file_search(filein,count=adim)
if adim eq 0 then print,'cannot find any files check directory and filename'
if adim eq 0  then stop

if keyword_set(wat) or  keyword_set(ice) then fbphase=file_basename(filein,'.PP.primary.nc')
if keyword_set(wat) then  filein=dirin+fbphase+'*WAT.prim*nc'
if keyword_set(ice) then  filein=dirin+fbphase+'*ICE.prim*nc'
if keyword_set(wat) or  keyword_set(ice) then  files=file_search(filein,count=adim)
print,'filein read_orac_nc_pp',filein



print,'adim',adim

for i=0,adim-1 do begin




   filein=files(i)
   print,files(i)
   if strpos(filein,'MODIS') gt 0 then inst='modis'
   if strpos(filein,'MYD') gt 0 then inst='modis'
   if strpos(filein,'AATSR') gt 0 then inst='aatsr'
   if strpos(filein,'AVHRR') gt 0 then inst='avhrr'
   
   if inst eq 'atsr'  then message,'routine not valid for this instrument must edit to adapt it!'+' '+inst
   
   fb=file_basename(filein,'primary.nc')
fb1=file_basename(filein,'.primary.nc')
   fd=file_dirname(filein)

psname=fd+'/'+fb1
if keyword_set(nosec) then psfi=psname
if ~keyword_set(nosec) then psfi=psname+'_extra'
 psfi=psfi+'.pdf'

chcek=file_search(psfi,count=cdim)
print,'cdim pdf file',cdim
print,'psfi',psfi
cdim=0
if cdim gt 0 then goto, endloop


   if ~keyword_set(wat) and ~keyword_set(ice) then fbs=file_basename(filein,'.PP.primary.nc')
   if ~keyword_set(wat) and ~keyword_set(ice) and keyword_set(dwd) then fbs=file_basename(filein,'PP.primary.nc')
   print,'filein wat/ice',filein

   if keyword_set(wat)  then fbs=file_basename(filein,'WAT.primary.nc')
   if keyword_set(ice)  then fbs=file_basename(filein,'ICE.primary.nc')

   fdir=file_dirname(filein)
   print,'fbs',fbs
   print,'file:',fdir+'/'+fbs+'*.nc'
   checkp=file_search(fdir+'/'+fbs+'*.nc',count=pdim)
   print,fdir+'/'+fbs+'*.nc'
   print,'pdim',pdim

   if pdim le 1 then begin
      if ~keyword_set(nosec) then begin
         print,'not all files (sec and prim) present',fdir+'/'+fbs+'*.nc'
         errflag=1
         goto,skipend
      endif else begin
         if pdim le 0 then begin
            print,'not all files (prim) present',fdir+'/'+fbs+'*.nc'
            errflag=0
            print,'goto skipend'
            goto,skipend
         endif
 
      endelse

   endif
   
print,fdir+'/'+fb+'primary.nc'
if ~keyword_set(nosec) then print,fdir+'/'+fbs+'ICE.secondary.nc'


   xpwat=ncdf_read(fdir+'/'+fb+'primary.nc')
; temporaray fudge until proper secondary file created
print,'a'
   if ~keyword_set(nosec) then xswat=ncdf_read(fdir+'/'+fbs+'ICE.secondary.nc')
print,'b'
   if ~keyword_set(wat) and ~keyword_set(ice)  then begin
;get ice and water
      fb=strreplace(fb,'WAT','ICE')
      xpice=ncdf_read(fdir+'/'+fb+'primary.nc')
      if ~keyword_set(nosec) then xsice=ncdf_read(fdir+'/'+fbs+'ICE.secondary.nc')
   endif 



nx=n_elements(xpwat.cot(*,1))
ny=n_elements(xpwat.cot(1,*))

 if ~keyword_set(nosec) then begin


;
;create scan lines useful for plotting
;   

    for in=0,nx-1 do begin
       for j=0,ny-1 do begin
          xswat.SCANLINE_U[in, j]=in
          xswat.SCANLINE_v[in,j]=j
       endfor
    endfor
 endif else begin
    SCANLINE_V=fltarr(nx,ny)
    SCANLINE_U=fltarr(nx,ny)
    for in=0,nx-1 do begin
       for j=0,ny-1 do begin
          SCANLINE_U(in,j)=in
          SCANLINE_V(in,j)=j
       endfor
    endfor
    
    
xswat={SCANLINE_V:SCANLINE_V,SCANLINE_U:SCANLINE_U}

endelse ;nosec
   

   
   print,fb+'primary.nc'        ;
;   
;read in above and refill it
;
   if keyword_set(wat)  or keyword_set(ice)then nph=1
   if ~keyword_set(wat) and ~keyword_set(ice) then nph=1
   
   nmeas=5
   nstate=5
   nsol=3
   if inst eq 'modis' then nsol=3
   if inst eq 'avhrr' then nsol=3
   res=size(xpwat.lat)
   nx=res(1)
   ny=res(2)
   
;
;loop over both phase retrievals
;
   
   for p=0,nph-1 do begin
      if p eq 0 then xp=xpwat
      if p eq 0 then xs=xswat
      
      if p eq 1 then xp=xpice
      if p eq 1 then xs=xsice
      
      xn=fltarr(nx,ny,nstate)
      x0=fltarr(nx,ny,nstate)
      unc=fltarr(nx,ny,nstate)
      meas=fltarr(nx,ny,nmeas)
      xn_sx=fltarr(nx,ny,nstate,nstate)
      ymfit=fltarr(nx,ny,nmeas)
      yn=fltarr(nx,ny,nmeas)
      y0=fltarr(nx,ny,nmeas)
      alb=fltarr(nx,ny,nsol)
      
      xn(*,*,0)=xp.cot*xp.cot_att.scale_factor
      xn(*,*,1)=xp.ref*xp.ref_att.scale_factor
      xn(*,*,2)=xp.ctp*xp.ctp_att.scale_factor
      xn(*,*,3)=xp.stemp*xp.stemp_att.scale_factor
      xn(*,*,4)=xp.cc_total*xp.cc_total_att.scale_factor

      if ~keyword_set(nosec) then begin
         if n_elements(xs) gt 0 then begin
            x0(*,*,0)=xs.cot_fg*xs.cot_fg_att.scale_factor
            x0(*,*,1)=xs.ref_fg*xs.ref_fg_att.scale_factor
            x0(*,*,2)=xs.ctp_fg*xs.ctp_fg_att.scale_factor
            x0(*,*,3)=xs.stemp_fg*xs.stemp_fg_att.scale_factor
         endif
      endif
;CVal(OK) = 10^CVal(OK)
          ;Err(OK) = Val(OK) * Err(OK) - not using natural logs!
;          CErr(OK) = CVal(OK) * alog(10) * CErr(OK)

;      unc(*,*,0)=xp.cot_uncertainty*xp.cot_uncertainty_att.scale_factor
      unc(*,*,0)=xp.cot*xp.cot_att.scale_factor* alog(10)*(xp.cot_uncertainty*xp.cot_uncertainty_att.scale_factor)

;pd*xqc.xe(i)/alog10(exp(1.))
      unc(*,*,1)=(xp.ref_uncertainty*xp.ref_uncertainty_att.scale_factor)
      unc(*,*,2)=(xp.ctp_uncertainty*xp.ctp_uncertainty_att.scale_factor)
      unc(*,*,3)=(xp.stemp_uncertainty*xp.stemp_uncertainty_att.scale_factor)
      unc(*,*,4)=xp.cc_total_uncertainty*xp.cc_total_uncertainty_att.scale_factor

      if ~keyword_set(nosec) then begin
         if n_elements(xs) gt 0 then begin
            
            if inst eq 'modis' then begin
               meas(*,*,0)=xs.REFLECTANCE_IN_CHANNEL_NO_2*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
               meas(*,*,1)=xs.REFLECTANCE_IN_CHANNEL_NO_2*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
               meas(*,*,2)=xs.REFLECTANCE_IN_CHANNEL_NO_6*xs.REFLECTANCE_IN_CHANNEL_NO_6_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_6_ATT.add_offset
               
               
               y0(*,*,0)=xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_1*xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_1_ATT.scale_factor+xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_1_ATT.add_offset
               y0(*,*,1)=xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_2*xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
               
               y0(*,*,2)=xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_6*xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_6_ATT.scale_factor+xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_6_ATT.add_offset
               
               y0(*,*,3)=xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_31*xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_31_ATT.scale_factor+xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_31_ATT.add_offset
               y0(*,*,4)=xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_32*xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_32_ATT.scale_factor+xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_32_ATT.add_offset
               
               
               
               ymfit(*,*,0)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_1*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_1_ATT.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_1_ATT.add_offset
               ymfit(*,*,1)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2_ATT.add_offset
               ymfit(*,*,2)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_6*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_6_ATT.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_6_ATT.add_offset
         
               ymfit(*,*,3)=xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_31*xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_31_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_31_ATT.add_offset
               ymfit(*,*,4)=xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_32*xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_32_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_32_ATT.add_offse

                     alb(*,*,0)=xs.albedo_IN_CHANNEL_NO_1*xs.REFLECTANCE_IN_CHANNEL_NO_1_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_1_ATT.add_offset
                  
                  alb(*,*,1)=xs.REFLECTANCE_IN_CHANNEL_NO_2*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset

                     alb(*,*,2)=xs.albedo_IN_CHANNEL_NO_6*xs.REFLECTANCE_IN_CHANNEL_NO_6_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_6_ATT.add_offset


            endif ;modis

      if inst eq 'aatsr' then begin

               alb(*,*,0)=xs.albedo_IN_CHANNEL_NO_2*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
               alb(*,*,1)=xs.albedo_IN_CHANNEL_NO_3*xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.add_offset
               alb(*,*,2)=xs.albedo_IN_CHANNEL_NO_4*xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.add_offset
               meas(*,*,0)=xs.REFLECTANCE_IN_CHANNEL_NO_2*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
               meas(*,*,1)=xs.REFLECTANCE_IN_CHANNEL_NO_3*xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.add_offset
               meas(*,*,2)=xs.REFLECTANCE_IN_CHANNEL_NO_4*xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.add_offset
               meas(*,*,3)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.add_offset
               meas(*,*,4)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.add_offset
               
;      y0(*,*,0)=xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_2*xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
;      y0(*,*,1)=xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_3*xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_3_ATT.scale_factor+xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_3_ATT.add_offset
;      y0(*,*,2)=xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_4*xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_4_ATT.scale_factor+xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_4_ATT.add_offset
;      y0(*,*,3)=xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6*xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.scale_factor+xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.add_offset
;      y0(*,*,4)=xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7*xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.scale_factor+xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.add_offset
;
;nb note add_offset required
;
               ymfit(*,*,0)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2_ATT.add_offset
               ymfit(*,*,1)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_3*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_3_ATT.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_3_ATT.add_offset
               ymfit(*,*,2)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_4*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_4_ATT.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_4_ATT.add_offset
               ymfit(*,*,3)=xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_6*xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_6_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_6_ATT.add_offset
               ymfit(*,*,4)=xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_7*xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_7_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_7_ATT.add_offset
               
               yn(*,*,0)=xs.REFLECTANCE_IN_CHANNEL_NO_2*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset+ymfit(*,*,0)
               yn(*,*,1)=xs.REFLECTANCE_IN_CHANNEL_NO_3*xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.add_offset+ymfit(*,*,1)
               yn(*,*,2)=xs.REFLECTANCE_IN_CHANNEL_NO_4*xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.add_offset+ymfit(*,*,2)
               yn(*,*,3)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.add_offset+ymfit(*,*,3)
               yn(*,*,4)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.add_offset+ymfit(*,*,4)
               
               alb(*,*,0)=xs.albedo_IN_CHANNEL_NO_2*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
               alb(*,*,1)=xs.albedo_IN_CHANNEL_NO_3*xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.add_offset
               alb(*,*,2)=xs.albedo_IN_CHANNEL_NO_4*xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.add_offset
               
            endif;aatsr


      if inst eq 'avhrr' then begin

               alb(*,*,0)=xs.albedo_IN_CHANNEL_NO_2*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
               alb(*,*,1)=xs.albedo_IN_CHANNEL_NO_3*xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.add_offset
               alb(*,*,2)=xs.albedo_IN_CHANNEL_NO_4*xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.add_offset
               meas(*,*,0)=xs.REFLECTANCE_IN_CHANNEL_NO_2*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
               meas(*,*,1)=xs.REFLECTANCE_IN_CHANNEL_NO_3*xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.add_offset
               meas(*,*,2)=xs.REFLECTANCE_IN_CHANNEL_NO_4*xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.add_offset
               meas(*,*,3)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.add_offset
               meas(*,*,4)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.add_offset
               
;      y0(*,*,0)=xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_2*xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
;      y0(*,*,1)=xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_3*xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_3_ATT.scale_factor+xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_3_ATT.add_offset
;      y0(*,*,2)=xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_4*xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_4_ATT.scale_factor+xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_4_ATT.add_offset
;      y0(*,*,3)=xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6*xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.scale_factor+xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.add_offset
;      y0(*,*,4)=xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7*xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.scale_factor+xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.add_offset
;
;nb note add_offset required
;
               ymfit(*,*,0)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2_ATT.add_offset
               ymfit(*,*,1)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_3*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_3_ATT.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_3_ATT.add_offset
               ymfit(*,*,2)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_4*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_4_ATT.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_4_ATT.add_offset
               ymfit(*,*,3)=xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_6*xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_6_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_6_ATT.add_offset
               ymfit(*,*,4)=xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_7*xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_7_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_7_ATT.add_offset
               
               yn(*,*,0)=xs.REFLECTANCE_IN_CHANNEL_NO_2*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset+ymfit(*,*,0)
               yn(*,*,1)=xs.REFLECTANCE_IN_CHANNEL_NO_3*xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.add_offset+ymfit(*,*,1)
               yn(*,*,2)=xs.REFLECTANCE_IN_CHANNEL_NO_4*xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.add_offset+ymfit(*,*,2)
               yn(*,*,3)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.add_offset+ymfit(*,*,3)
               yn(*,*,4)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.add_offset+ymfit(*,*,4)
               
               alb(*,*,0)=xs.albedo_IN_CHANNEL_NO_2*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
               alb(*,*,1)=xs.albedo_IN_CHANNEL_NO_3*xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.add_offset
               alb(*,*,2)=xs.albedo_IN_CHANNEL_NO_4*xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.add_offset
               
            endif;avhrr


         endif ;nosec
      endif;inst aatsr

;
;write data to a structure remember scalling factors
;
      costbadja=where(xp.costja le 0,ncostja)
      costbadjm=where(xp.costjm le 0,ncostjm)
      
      
      if ncostja gt 0 then xp.costja(costbadja)=0.0
      if ncostjm gt 0 then xp.costjm(costbadjm)=0.0

; perform some test printing to see that arrays are filled
if ~keyword_set(nosec) then begin
      print,'meas',range(meas(*,*,0))
      print,range(meas(*,*,1))
      print,range(meas(*,*,2))
      print,range(meas(*,*,3))
      print,range(meas(*,*,4))
   endif

print,'costja',range(xp.costja)
print,'costjm',range(xp.costjm)
; fill the structure      

if keyword_set(ice) or keyword_set(wat) then begin
cvar=xp.cc_total*0.-999
endif else begin
cvar= xp.cccot*xp.cccot_att.SCALE_FACTOR
endelse


      xin={y:meas,y0:y0,yn:yn,xn:xn,sx:unc,cost:(xp.costja+xp.costjm)*xp.COSTJM_ATT.SCALE_FACTOR,conv:abs(xp.convergence),tc:xp.ctt*xp.ctt_att.scale_factor,itype:xp.phase,zc:xp.cth*xp.cth_att.scale_factor,ni:xp.niter,white_sky_albedo:meas(*,*,0)*0.0-999.,clearsky_bt:meas*0.0-999.,xo:meas*0.0-999.,ae:meas(*,*,*)*0.0-999,ym:meas,cwp:meas(*,*,0)*0.0-999.,ymfit:ymfit,phase:meas(*,*,0)*0,x0:x0,lat:xp.lat,lon:xp.lon,solz:xp.SOLAR_ZENITH_VIEW_NO1,satz:xp.SATELLITE_ZENITH_VIEW_NO1,relaz:xp.REL_AZIMUTH_VIEW_NO1,cth:xp.cth*xp.cth_att.SCALE_FACTOR,mask:xp.cc_total,alb:alb,lsflag:xp.lsflag, cccot:cvar}
      
      
;
;define this for setting up ret structure
;
;      nmeas=5
;      nstate=5
      
;
;define ret structure which is the input required plot routine
;

; only pass through values that exist
      if ~keyword_set(nosec) then begin
         igood = where(xin.xn(*,*,2) gt 0. and xin.ymfit(*,*,2) gt -30.,ngood,complement=ibad,ncomplement=nbad)
      endif else begin
         igood = where(xin.xn(*,*,2) gt 0,ngood,complement=ibad,ncomplement=nbad)
      endelse      
print,'ngood',ngood
if ngood le 0 then print,'no good points in this file skip file',filein
if keyword_set(ps) then begin
if ngood le 0 then begin
pdfile=strreplace(psfi,'.ps','.pdf')
print,'pdfile',pdfile
spawn, 'touch '+ pdfile
endif
endif


if ngood eq 0 then goto, skipend
for kk=0,4 do print,'state',range(xin.xn(*,*,kk))
print,'cost aa',range(xin.cost(*,*))
      if p eq 0 then begin
         ret=replicate({y:fltarr(nmeas),y0:fltarr(nmeas),yn:fltarr(nmeas),xn:fltarr(nstate),cost:0.0,conv:0,tc:0.0,itype:0,COLUMN_DENSITY:0.0,zc:0.0,ni:0,white_sky_albedo:0.0,clearsky_bt:1.0,x0:fltarr(nstate),xe:fltarr(nstate), COLUMN_DENSITY_ERROR:0.0,ymfit:fltarr(nmeas),sx:fltarr(nstate),ae:fltarr(nstate),alb:fltarr(nsol),mask:0.0,solz:0.0,satz:0.0,relaz:0.0,cth:0.0,lsflag:0.0,cccot:0.0},nph,ngood)
      endif
      
      ngood2 =n_elements(xp.lat)

print,'ngood1 read_orac',ngood2      
;
;reform 2 d array
;
      xin_y=reform(xin.y(*,*,*),ngood2,nmeas)
      xin_alb=reform(xin.alb(*,*,*),ngood2,nsol)
      xin_y0=reform(xin.y0(*,*,*),ngood2,nmeas)
      xin_ymfit=reform(xin.ymfit(*,*,*),ngood2,nmeas)
      xin_xn=reform(xin.xn(*,*,*),ngood2,nstate)
      xin_cth=reform(xin.cth(*,*,*),ngood2)
      xin_x0=reform(xin.x0(*,*,*),ngood2,nstate)
      xin_yn=reform(xin.yn(*,*,*),ngood2,nmeas)
      xin_sx=reform(xin.sx(*,*,*),ngood2,nmeas)
      xin_ae=reform(xin.ae(*,*,*),ngood2,nstate)
      xin_solz=reform(xin.solz(*,*),ngood2)
      xin_lsflag=reform(xin.lsflag(*,*),ngood2)
      xin_satz=reform(xin.satz(*,*),ngood2)
      xin_relaz=reform(xin.relaz(*,*),ngood2)
       xin_cccot=reform(xin.cccot(*,*),ngood2)
      

      iretgood = where(xin_xn(*,2) gt 0,nretgood,complement=ibad,ncomplement=nbad)
      
      
      ret(p,*).y(0:nmeas-1)= reform(transpose(xin_y(iretgood,*)),nmeas,1,ngood)
      ret(p,*).Y0(0:nmeas-1)=reform(transpose(xin_y0(iretgood,*)),nmeas,1,ngood)
      ret(p,*).YN(0:nmeas-1)=reform(transpose((xin_ymfit(iretgood,*)+xin_y(iretgood,*))),nmeas,1,ngood)
      
      ret(p,*).alb(0:nsol-1)= reform(transpose(xin_alb(iretgood,*)),nsol,1,ngood)
      ret(p,*).Ymfit(0:nmeas-1)=reform(transpose(xin_ymfit(iretgood,*)),nmeas,1,ngood)
      whbad=where(ret(p,*).ymfit(4) lt -30., nbad)
      
      if nbad gt 0 then begin
         for ii=1,4 do ret(p,whbad).YN(ii)=-999.
         for ii=1,4 do ret(p,whbad).Y(ii)=-999.
         for ii=1,4 do ret(p,whbad).Ymfit(ii)=-999.
      endif
      
print,'nbad read_orac',nbad     
      
      ret(p,*).XN=reform(transpose(xin_xn(iretgood,*)),nstate,1,ngood)
      ret(p,*).sx=reform(transpose(xin_sx(iretgood,*)),nstate,1,ngood)
      ret(p,*).xe=reform(transpose(xin_sx(iretgood,*)),nstate,1,ngood)
      ret(p,*).COST=reform(transpose(xin.cost(igood)),1,ngood) 
;   ret(p,*).lat=reform(transpose(xin.lat(igood)),1,ngood) 
;   ret(p,*).lon=reform(transpose(xin.lon(igood)),1,ngood) 
      ret(p,*).CONV=reform(transpose(xin.conv(igood)),1,ngood) 
      ret(p,*).relaz=reform(transpose(xin.relaz(igood)),1,ngood) 
      ret(p,*).solz=reform(transpose(xin.solz(igood)),1,ngood) 
      ret(p,*).lsflag=reform(transpose(xin.lsflag(igood)),1,ngood) 
      ret(p,*).satz=reform(transpose(xin.satz(igood)),1,ngood) 
      ret(p,*).cth=reform(transpose(xin.cth(igood)),1,ngood) 
      ret(p,*).cccot=reform(transpose(xin.cccot(igood)),1,ngood) 
      ret(p,*).TC = reform(transpose(xin.tc(igood)),1,ngood) 
      ret(p,*).ITYPE=reform(transpose(xin.itype(igood)),1,ngood) 
      ret(p,*).mask=reform(transpose(xin.mask(igood)),1,ngood) 
      ret(p,*).ZC= reform(transpose(xin.zc(igood)),1,ngood)
      ret(p,*).NI=   reform(transpose(xin.ni(igood)),1,ngood) 
      ret(p,*).WHITE_SKY_ALBEDO=reform(transpose(xin.white_sky_albedo(igood)),1,ngood) ;reform(transpose(xin.albedo(igood,index_chans_ref)),nchans_ref,1,ngood)
      ret(p,*).CLEARSKY_BT= 0.0 ;reform(xin.meas(igood,4)*0.0,1,ngood) 
      ret(p,*).X0 =  reform(transpose(xin_x0(iretgood,*)),nstate,1,ngood)
;   ret(p,*).ae =  reform(transpose(xin.ae(igood,*)*0.0-999.),nstate,1,ngood)
;        ret(p,*).XE=  reform(transpose(xin.uncertainty(igood,*)),nstate,1,ngood)  
      ret(p,*).COLUMN_DENSITY_ERROR=reform(xin.cwp(igood),1,ngood)
;   ret(p,*).lat=reform(xin.lat(igood),1,ngood)
;   ret(p,*).lon=reform(xin.lon(igood),1,ngood)
      
   endfor                       ;phas
   
latr=range(xin.lat(igood))
lonr=range(xin.lon(igood))

print,'here a read_orac'     

   sg_temp={LATR:latr,LONR:lonr,UG:fltarr(3),VG:fltarr(3),LAT:xin.lat(igood),LON:xin.lon(igood),ID: inst+'-scan',SAT:1,NEQ:0,LL:0,SD:0,solz:xin.solz(igood),satz:xin.satz(igood),relaz:xin.relaz(igood) }

;   ha_sav=rd_sav('~/Temp/orac/h_sav.sav')
   ha_sav=create_h_sav()

   sv_temp=ha_sav.sv
   s_temp=ha_sav.s

   if inst eq 'aatsr' then begin
      posdate=strpos(fb,'ENV__')
      datein=strmid(fb,posdate+5,12)
   endif

   if inst eq 'modis' then begin
      datein=strmid(fb,0,12)
   endif
   

print,'here b read_orac'     
   ha_temp={FILE:fdir+'/'+fb,OFILE:fdir+'/'+fb,PROC_DATE:datein,DATA_DATE:datein,S:s_temp,SV:sv_temp,U:xs.SCANLINE_U[igood],V:xs.SCANLINE_V[igood],TIME:fltarr(ngood),SG:sg_temp,DIA:2,TYPES:'LIQUID',LUTRE_RANGE:dblarr(2,2),LOPD_RANGE:dblarr(2,2),NSTR:0,RSA:0,BAUM:0,SPI:1,YE:fltarr(nmeas),CTEST:-1,IDC:0,MAX_IT:20,SYI:0,EXTRA:0.0,HX:0,VERSION:3.18,NX:nstate,NY:nmeas,npix:ngood,NST:2}  
   

   ha_temp.sv.zstar=0
   
   latr=range(xp.lat[igood])
   lonr=range(xp.lon[igood])
   ha_temp.sg.LATR =latr 
   ha_temp.sg.LONR =lonr

   if n_elements(xs) gt 0 then begin 
      xu=range(xs.SCANLINE_U[igood])
      xv=range(xs.SCANLINE_V[igood])
   endif
   ha_temp.sg.UG  =   [xu(0)+1,xu(1)+1,1] 
   ha_temp.sg.VG  =  [xv(0)+1,xv(1)+1,1]  
   ha_temp.sg.LAT=xp.lat[igood] ;      
   ha_temp.sg.LON=xp.lon[igood] ;      
   
;
;now call the plotting routine
;
print,'here c read_orac'     
   if n_elements(plot) gt 0 then begin
      plot_ret_cldmodel_modis,ret,ha_temp,_EXTRA=extra,clon=clon,clat=clat,eop_y=eop_y,eop_x=eop_x,ps=ps,night=night,itype=itype,/noqc,/rotate,nosec=nosec,error=error;error=error
   endif
   
print,'here d read_orac'     
endloop:

dataout={ret:ret,ha:ha_temp}
print,'here e read_orac'     
skipend:   
endfor                          ;loop over adim



return,dataout


end
