;==========================================================================
;+
;	pro READ_ORAC_NC_pp
;
;	Description:
;
;       reads orac netcdf post processed file and writes data into a common format that is
;       accepted by plot routine.
;       *****Important info******This routine ideally requires that 2 netcdf
;           files are present the primary and secondary as the
;            secondary has the measurement information and residual information
;       file names must have WAT, ICE or PP in them
;
;       if no secondary file is present then use /sec keyword.
;
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
;       ext : variable that can specify unconventional file namimg
;       chunksize:  number of along track pixels to plot into a single
;                   image file useful when studying whole orbits. If
;                   this is not set then defaults to the total lenght
;                    of the file
;
;	History:
;
;       original file C. Poulsen created 2011 1st public release April 2013
;	
;       typical usage idl>x=read_orac_nc_pp(/plot,/ps) ;usual use
;       typical usage idl>x=read_orac_nc_pp(/plot,/ps,dirin='/misc/oxford1/rsg/Data/projects/ecv_clouds/dan_ucl/orac_outputs/stereo/')
;       typical usage idl>x=read_orac_nc_pp(/plot,/ps,chsize=1000)
;       typical usage idl>x=read_orac_nc_pp(/plot,/ps,/ice,ext='_day2')
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
; cp 22/01/2014 changes to deal with missing values
; cp 22/01/2014 tidied up and added in ability to chunk imagery if you
; cp 22/01/2014 edited file so can work with an ice file created with a diferent date
; cp 26/03/2014 updates to code to create chunking of plots and to be
;               more robust
;
; $Id$
;-
;==========================================================================
function read_orac_nc_pp,filein=filein,plot=plot,ps=ps,errflag=errflag,wat=wat,ice=ice,dirin=dirin,dwd=dwd,nosec=nosec,error=error,version=version,ext=ext,chsize=chsize


if n_elements(version) eq 0 then version=''
errflag=0

if ~keyword_set(dirin) then dirin='/misc/wantage_static/cpoulsen/cloud_ecv/postproc/2008/06/20/'

if ~keyword_set(ext) then ext=''


;
;this fordwdw file only
;
if keyword_set(dwd) then begin
   dirin='/cmsaf/cmsaf-cld4/mjerg/test_plotting/'
   dirin='/cmsaf/cmsaf-cld4/mjerg/test_plotting/20080620_AVHRR_noaa16_proc_2_process_ID8206794_US1362069592/noaa16_20080620_0955_99999_satproj_00000_12180_avhrr/post/'
   filein=dirin+'20080620095500-ESACCI-L2_CLOUD-CLD_PRODUCTS-AVHRRGAC-NOAA16-fv1.0.nc'
endif

if ~keyword_set(filein)  and ~keyword_set(ice) and ~keyword_set(wat) then filein=dirin+'*_1*201403*PP*prim*.nc'
if ~keyword_set(filein) and keyword_set(wat) then filein=dirin+'*WAT*prim*.nc'
if ~keyword_set(filein) and keyword_set(ice) then filein=dirin+'*ICE*prim*.nc'

print,'filein ',filein

if ~keyword_set(dirin) then begin
   dirin=file_dirname(filein)+'/'
   
endif else begin
;   filein=dirin+filein
endelse
print,'dirin read_orac_nc_pp',dirin
print,'filein',filein

files=file_search(filein,count=adim)


if adim eq 0 then print,'cannot find any files check directory and filename'
if adim eq 0  then stop

if keyword_set(wat) or  keyword_set(ice) then fbphase=file_basename(files,'.primary'+ext+'.nc')
if keyword_set(wat) or  keyword_set(ice) then print,'fb',fbphase
if keyword_set(wat) then  filein=dirin+fbphase+'*.prim*'+ext+'*nc'
if keyword_set(ice) then  filein=dirin+fbphase+'*.prim*'+ext+'*nc'
if keyword_set(wat) or  keyword_set(ice) then  files=file_search(filein,count=adim)
print,'filein read_orac_nc_pp',filein
print,'No. of files to plot:',adim

for i=0,adim-1 do begin
print,'file no.',i,files(i)
   filein=files(i)

   print,'file plotting now:',files(i)
   if strpos(filein,'MODIS') gt 0 then inst='modis'
   if strpos(filein,'MYD') gt 0 then inst='modis'
   if strpos(filein,'AATSR') gt 0 then inst='aatsr'
   if strpos(filein,'AVHRR') gt 0 then inst='avhrr'
   
   if inst eq 'atsr'  then message,'routine not valid for this instrument must edit to adapt it!'+' '+inst
   
   fb=file_basename(filein,'primary'+ext+'.nc')
   fb1=file_basename(filein,'.primary'+ext+'.nc')
   fd=file_dirname(filein)

   psname=fd+'/'+fb1
   if keyword_set(nosec) then psfisa=psname
   if keyword_set(nosec) then psfisa=psname+'_nosec'
   if ~keyword_set(nosec) then psfisa=psname+'_extrav2'

   
;
; some out put file have different extensions the next few lines cope
; with both
;

   pos1=strpos(filein,'.PP')
   pos2=strpos(filein,'_PP')

   
   if ~keyword_set(wat) and ~keyword_set(ice) and pos1 gt 0 then fbs=file_basename(filein,'.PP.primary.nc')
   if ~keyword_set(wat) and ~keyword_set(ice) and pos2 gt 0 then fbs=file_basename(filein,'_PP.primary.nc')
   if ~keyword_set(wat) and ~keyword_set(ice) and keyword_set(dwd) then fbs=file_basename(filein,'PP.primary.nc')
   
   
   if keyword_set(wat)  then fbs=file_basename(filein,'WAT.primary'+ext+'.nc')
   if keyword_set(ice)  then fbs=file_basename(filein,'ICE.primary'+ext+'.nc')

   fdir=file_dirname(filein)
   
   print,'file:',fdir+'/'+fbs+'*.nc'

   checkp=file_search(fdir+'/'+fbs+'*.nc',count=pdim)
   print,'checkp',fdir+'/'+fbs+'*.nc'
   print,'check primary and secondary file present:',pdim

   if pdim le 1 then begin
      if ~keyword_set(nosec) then begin
         print,'not all files (sec and prim) present',fdir+'/'+fbs+'*.nc'
         errflag=1
         print,'goto skipend'
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
   
   ;print,fdir+'/'+fb+'primary.nc'
   if ~keyword_set(nosec) and keyword_set(ice) then print,fdir+'/'+fbs+'ICE.secondary.nc'
   if ~keyword_set(nosec) and keyword_set(wat) then print,fdir+'/'+fbs+'WAT.secondary.nc'
   
   
   xpwat=ncdf_read(fdir+'/'+fb+'primary'+ext+'.nc')
; temporaray fudge until proper secondary file created
   
   if ~keyword_set(nosec)  and keyword_set(ice) then xswat=ncdf_read(fdir+'/'+fbs+'ICE.secondary'+ext+'.nc')
   if ~keyword_set(nosec)  and keyword_set(wat) then xswat=ncdf_read(fdir+'/'+fbs+'WAT.secondary'+ext+'.nc')
   
   if ~keyword_set(wat) and ~keyword_set(ice)  then begin
;get ice and water
      fb=strreplace(fb,'WAT','ICE')
      xpwat=ncdf_read(fdir+'/'+fb+'primary.nc')
      print,fdir+'/'+fbs+'ICE.secondary'+ext+'.nc'
      filess=file_search(fdir+'/'+strmid(fbs,0,50)+'*ICE*sec*',count=vdim)

      if vdim gt 0 then begin
         if ~keyword_set(nosec) then xswat=ncdf_read(filess(0))
;         if ~keyword_set(nosec) then xswat=ncdf_read(fdir+'/'+fbs+'ICE.secondary'+ext+'.nc')
      endif
   endif 

   
   
   nx=n_elements(xpwat.cot(*,1)) ; across track
   ny=n_elements(xpwat.cot(1,*)) ;along track
   
   nchunks=1
   if ~keyword_set(chsize) then chunksize_file=ny ;chunksize=1000
 if keyword_set(chsize) then begin
chunksize_file=chsize
   if ny gt chunksize_file then nchunks= ceil(ny/chunksize_file)
   if ny gt chunksize_file then print,'this file is chunked',nchunks
endif

   for nc=0, nchunks -1 do begin
 

      if nchunks gt 1 then begin
         nys=nc*chunksize_file 
         nye=(nc+1)*chunksize_file-1
         ny=chunksize_file
         psfis=psfisa+'_'+i2s(nc)
      endif else begin
         nys=0
         nye=ny-1
         ny=ny
         psfis=psfisa
      endelse
	if keyword_set(error) then psfis=psfisa+'_error'
         pdfi=psfis+'.pdf'
         psfi=psfis

    chcek=file_search(pdfi,count=cdim)
   print,'looking for this pdf file',pdfi
   print,'postscript file name',psfi
   print,'Does a previous pdf file exist? pdf file',cdim

   if cdim gt 0 then goto, skipend
      
      if ~keyword_set(nosec) then begin
         
; remi
;create scan lines useful for plotting
;   

         print,'nx nys nye',nx,nys,nye
;account for partial chunks         
         ntot=n_elements(xswat.SCANLINE_U[1, *])
         if nye gt ntot-1 then begin
            nye=ntot-1
            ny=(nye-nys)+1
         endif

         for in=0,nx-1 do begin
            for j=nys,nye-1 do begin
               xswat.SCANLINE_U[in, j]=in
               xswat.SCANLINE_v[in,j]=j
            endfor
         endfor
      endif else begin
         SCANLINE_V=fltarr(nx,ny)
         SCANLINE_U=fltarr(nx,ny)
         for in=0,nx-1 do begin
            for j=nys,nye-1 do begin
               SCANLINE_U(in,j)=in
               SCANLINE_V(in,j)=j
            endfor
         endfor
         
         
         xswat={SCANLINE_V:SCANLINE_V,SCANLINE_U:SCANLINE_U}
         
      endelse                   ;nosec
      
      
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
      ny=ny ;res(2)
      
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
   print,'nys nye',nys,nye
      
         xn(*,*,0)=xp.cot(*,nys:nye)*xp.cot_att.scale_factor
         xn(*,*,1)=xp.ref(*,nys:nye)*xp.ref_att.scale_factor
         xn(*,*,2)=xp.ctp(*,nys:nye)*xp.ctp_att.scale_factor
         xn(*,*,3)=xp.stemp(*,nys:nye)*xp.stemp_att.scale_factor
         xn(*,*,4)=xp.cc_total(*,nys:nye)*xp.cc_total_att.scale_factor
         
         if ~keyword_set(nosec) then begin
            if n_elements(xs) gt 0 then begin
               
;      xs.cot_fg[WHERE(xs.cot_fg ne xs.cot_fg_ATT._fillvalue, /NULL)] = xs.cot_fg*xs.cot_fg_att.scale_factor
;      xs.ref_fg[WHERE(xs.ref_fg ne xs.ref_fg_ATT._fillvalue, /NULL)] = xs.ref_fg*xs.ref_fg_att.scale_factor
;      xs.ctp_fg[WHERE(xs.ctp_fg ne xs.ctp_fg_ATT._fillvalue, /NULL)] = xs.ctp_fg*xs.ctp_fg_att.scale_factor
;      xs.stemp_fg[WHERE(xs.stemp_fg ne xs.stemp_fg_ATT._fillvalue, /NULL)] = xs.stemp_fg*xs.stemp_fg_att.scale_factor

               x0(*,*,0)=xs.cot_fg(*,nys:nye)*xs.cot_fg_att.scale_factor
               x0(*,*,1)=xs.ref_fg(*,nys:nye)*xs.ref_fg_att.scale_factor
               x0(*,*,2)=xs.ctp_fg(*,nys:nye)*xs.ctp_fg_att.scale_factor
               x0(*,*,3)=xs.stemp_fg(*,nys:nye)*xs.stemp_fg_att.scale_factor
            endif
         endif
;CVal(OK) = 10^CVal(OK)
                                ;Err(OK) = Val(OK) * Err(OK) - not using natural logs!
;          CErr(OK) = CVal(OK) * alog(10) * CErr(OK)
         
         
;      xp.cot_uncertainty[WHERE(xp.cot_uncertainty ne xp.cot_uncertainty_ATT._fillvalue, /NULL)] = xp.cot_uncertainty*xp.cot_uncertainty_att.scale_factor
;      xp.ref_uncertainty[WHERE(xp.ref_uncertainty ne xp.ref_uncertainty_ATT._fillvalue, /NULL)] = xp.ref_uncertainty*xp.ref_uncertainty_att.scale_factor
;      xp.ctp_uncertainty[WHERE(xp.ctp_uncertainty ne xp.ctp_uncertainty_ATT._fillvalue, /NULL)] = xp.ctp_uncertainty*xp.ctp_uncertainty_att.scale_factor
;      xp.stemp_uncertainty[WHERE(xp.stemp_uncertainty ne xp.stemp_uncertainty_ATT._fillvalue, /NULL)] = xp.stemp_uncertainty*xp.stemp_uncertainty_att.scale_factor
;      xp.cc_total_uncertainty[WHERE(xp.cc_total_uncertainty ne xp.cc_total_uncertainty_ATT._fillvalue, /NULL)] = xp.cc_total_uncertainty*xp.cc_total_uncertainty_att.scale_factor

         unc(*,*,0)=xp.cot_uncertainty(*,nys:nye)*xp.cot_uncertainty_att.scale_factor
         unc(*,*,1)=xp.ref_uncertainty(*,nys:nye)*xp.ref_uncertainty_att.scale_factor
         unc(*,*,2)=xp.ctp_uncertainty(*,nys:nye)*xp.ctp_uncertainty_att.scale_factor
         unc(*,*,3)=xp.stemp_uncertainty(*,nys:nye)*xp.stemp_uncertainty_att.scale_factor
         unc(*,*,4)=xp.cc_total_uncertainty(*,nys:nye)*xp.cc_total_uncertainty_att.scale_factor
         
         if ~keyword_set(nosec) then begin
            if n_elements(xs) gt 0 then begin
               
               if inst eq 'modis' then begin
                  meas(*,*,0)=xs.REFLECTANCE_IN_CHANNEL_NO_2(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
                  meas(*,*,1)=xs.REFLECTANCE_IN_CHANNEL_NO_2(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
                  meas(*,*,2)=xs.REFLECTANCE_IN_CHANNEL_NO_6(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_6_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_6_ATT.add_offset
                  
                  
                  y0(*,*,0)=xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_1(*,nys:nye)*xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_1_ATT.scale_factor+xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_1_ATT.add_offset
                  y0(*,*,1)=xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_2(*,nys:nye)*xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
                  
                  y0(*,*,2)=xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_6(*,nys:nye)*xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_6_ATT.scale_factor+xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_6_ATT.add_offset
                  
                  y0(*,*,3)=xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_31(*,nys:nye)*xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_31_ATT.scale_factor+xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_31_ATT.add_offset
                  y0(*,*,4)=xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_32(*,nys:nye)*xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_32_ATT.scale_factor+xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_32_ATT.add_offset
                  
                  
                  
                  ymfit(*,*,0)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_1(*,nys:nye)*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_1_ATT.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_1_ATT.add_offset
                  ymfit(*,*,1)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2(*,nys:nye)*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2_ATT.add_offset
                  ymfit(*,*,2)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_6(*,nys:nye)*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_6_ATT.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_6_ATT.add_offset
                  
                  ymfit(*,*,3)=xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_31(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_31_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_31_ATT.add_offset
                  ymfit(*,*,4)=xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_32(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_32_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_32_ATT.add_offse
                  
                  alb(*,*,0)=xs.albedo_IN_CHANNEL_NO_1(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_1_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_1_ATT.add_offset
                  
                  alb(*,*,1)=xs.REFLECTANCE_IN_CHANNEL_NO_2(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
                  
                  alb(*,*,2)=xs.albedo_IN_CHANNEL_NO_6(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_6_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_6_ATT.add_offset
                  
                  
               endif            ;modis
               
               if inst eq 'aatsr' then begin
                  
                  alb(*,*,0)=xs.albedo_IN_CHANNEL_NO_2(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
                  alb(*,*,1)=xs.albedo_IN_CHANNEL_NO_3(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.add_offset
                  alb(*,*,2)=xs.albedo_IN_CHANNEL_NO_4(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.add_offset
                  
;      xs.REFLECTANCE_IN_CHANNEL_NO_2[WHERE(xs.REFLECTANCE_IN_CHANNEL_NO_2 ne xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT._fillvalue, /NULL)] = xs.REFLECTANCE_IN_CHANNEL_NO_2*xs.REFLECTANCE_IN_CHANNEL_NO_2_att.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset

;      xs.REFLECTANCE_IN_CHANNEL_NO_3[WHERE(xs.REFLECTANCE_IN_CHANNEL_NO_3 ne xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT._fillvalue, /NULL)] = xs.REFLECTANCE_IN_CHANNEL_NO_3*xs.REFLECTANCE_IN_CHANNEL_NO_3_att.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.add_offset

;      xs.REFLECTANCE_IN_CHANNEL_NO_4[WHERE(xs.REFLECTANCE_IN_CHANNEL_NO_4 ne xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT._fillvalue, /NULL)] = xs.REFLECTANCE_IN_CHANNEL_NO_4*xs.REFLECTANCE_IN_CHANNEL_NO_4_att.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.add_offset

;      xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6[WHERE(xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6 ne xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT._fillvalue, /NULL)] = xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_att.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.add_offset

;      xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7[WHERE(xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7 ne xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT._fillvalue, /NULL)] = xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_att.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.add_offset



                  meas(*,*,0)=xs.REFLECTANCE_IN_CHANNEL_NO_2(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_2_att.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
                  meas(*,*,1)=xs.REFLECTANCE_IN_CHANNEL_NO_3(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_3_att.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.add_offset
                  meas(*,*,2)=xs.REFLECTANCE_IN_CHANNEL_NO_4(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_4_att.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.add_offset
                  meas(*,*,3)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_att.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.add_offset
                  meas(*,*,4)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_att.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.add_offset
                  
;      y0(*,*,0)=xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_2*xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
;      y0(*,*,1)=xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_3*xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_3_ATT.scale_factor+xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_3_ATT.add_offset
;      y0(*,*,2)=xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_4*xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_4_ATT.scale_factor+xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_4_ATT.add_offset
;      y0(*,*,3)=xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6*xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.scale_factor+xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.add_offset
;      y0(*,*,4)=xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7*xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.scale_factor+xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.add_offset
;
;nb note add_offset required
; xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2


                  ymfit(*,*,0)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2(*,nys:nye)*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2_att.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2_ATT.add_offset
                  ymfit(*,*,1)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_3(*,nys:nye)*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_3_att.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_3_ATT.add_offset
                  ymfit(*,*,2)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_4(*,nys:nye)*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_4_att.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_4_ATT.add_offset
                  ymfit(*,*,3)=xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_6(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_6_att.scale_factor+xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_6_ATT.add_offset
                  ymfit(*,*,4)=xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_7(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_7_att.scale_factor+xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_7_ATT.add_offset
                  
                                    
                  yn(*,*,0)=xs.REFLECTANCE_IN_CHANNEL_NO_2(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset+ymfit(*,*,0)
                  yn(*,*,1)=xs.REFLECTANCE_IN_CHANNEL_NO_3(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.add_offset+ymfit(*,*,1)
                  yn(*,*,2)=xs.REFLECTANCE_IN_CHANNEL_NO_4(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.add_offset+ymfit(*,*,2)
                  yn(*,*,3)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.add_offset+ymfit(*,*,3)
                  
                  yn(*,*,4)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.add_offset+ymfit(*,*,4)
                  
                  alb(*,*,0)=xs.albedo_IN_CHANNEL_NO_2(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
                  alb(*,*,1)=xs.albedo_IN_CHANNEL_NO_3(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.add_offset
                  alb(*,*,2)=xs.albedo_IN_CHANNEL_NO_4(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.add_offset
                  
            endif;aatsr


               if inst eq 'avhrr' then begin
                  
                  alb(*,*,0)=xs.albedo_IN_CHANNEL_NO_2(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
                  alb(*,*,1)=xs.albedo_IN_CHANNEL_NO_3(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.add_offset
                  alb(*,*,2)=xs.albedo_IN_CHANNEL_NO_4(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.add_offset
                  meas(*,*,0)=xs.REFLECTANCE_IN_CHANNEL_NO_2(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
                  meas(*,*,1)=xs.REFLECTANCE_IN_CHANNEL_NO_3(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.add_offset
                  meas(*,*,2)=xs.REFLECTANCE_IN_CHANNEL_NO_4(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.add_offset
                  meas(*,*,3)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.add_offset
                  meas(*,*,4)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.add_offset
                  
;      y0(*,*,0)=xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_2*xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
;      y0(*,*,1)=xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_3*xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_3_ATT.scale_factor+xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_3_ATT.add_offset
;      y0(*,*,2)=xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_4*xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_4_ATT.scale_factor+xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_4_ATT.add_offset
;      y0(*,*,3)=xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6*xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.scale_factor+xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.add_offset
;      y0(*,*,4)=xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7*xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.scale_factor+xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.add_offset
;
;nb note add_offset required
;
                  ymfit(*,*,0)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2(*,nys:nye)*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2_ATT.add_offset
                  ymfit(*,*,1)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_3(*,nys:nye)*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_3_ATT.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_3_ATT.add_offset
                  ymfit(*,*,2)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_4(*,nys:nye)*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_4_ATT.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_4_ATT.add_offset
                  ymfit(*,*,3)=xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_6(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_6_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_6_ATT.add_offset
                  ymfit(*,*,4)=xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_7(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_7_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_7_ATT.add_offset
                  
                  yn(*,*,0)=xs.REFLECTANCE_IN_CHANNEL_NO_2(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset+ymfit(*,*,0)
                  yn(*,*,1)=xs.REFLECTANCE_IN_CHANNEL_NO_3(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.add_offset+ymfit(*,*,1)
                  yn(*,*,2)=xs.REFLECTANCE_IN_CHANNEL_NO_4(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.add_offset+ymfit(*,*,2)
                  yn(*,*,3)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.add_offset+ymfit(*,*,3)
                  yn(*,*,4)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.add_offset+ymfit(*,*,4)
                  
                  alb(*,*,0)=xs.albedo_IN_CHANNEL_NO_2(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
                  alb(*,*,1)=xs.albedo_IN_CHANNEL_NO_3(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.add_offset
                  alb(*,*,2)=xs.albedo_IN_CHANNEL_NO_4(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.add_offset
                  
               endif            ;avhrr
               
               
            endif               ;nosec
         endif                  ;inst aatsr
         
;
;write data to a structure remember scalling factors
;
         costbadja=where(xp.costja le 0,ncostja)
         costbadjm=where(xp.costjm le 0,ncostjm)
         
         
         if ncostja gt 0 then xp.costja(costbadja)=0.0
         if ncostjm gt 0 then xp.costjm(costbadjm)=0.0
         night=0
; perform some test printing to see that arrays are filled
         if ~keyword_set(nosec) then begin
            print,'meas range ch 1:',range(meas(*,*,0))
            print,'meas range ch 2:',range(meas(*,*,1))
            print,'meas range ch 3:',range(meas(*,*,2))
            print,'meas range ch 4:',range(meas(*,*,3))
            print,'meas range ch 5:',range(meas(*,*,4))
if (max(meas(*,*,0)) lt .1) then night=1
if night eq 1 then print,'skipping night file'
if night eq 1 then goto,skipend
            print,'ymfit range ch 1:',range(ymfit(*,*,0))
            print,'ymfit range ch 2:',range(ymfit(*,*,1))
            print,'ymfit range ch 3:',range(ymfit(*,*,2))
            print,'ymfit range ch 4:',range(ymfit(*,*,3))
            print,'ymfit range ch 5:',range(ymfit(*,*,4))
         endif
         
         print,'cost fm range',range(xp.costja)
         print,'cost meas range',range(xp.costjm)
; fill the structure      
         
         if keyword_set(ice) or keyword_set(wat) then begin
            cvar=xp.cc_total*0.-999
         endif else begin
            cvar= xp.cccot*xp.cccot_att.SCALE_FACTOR
         endelse
         
         
         xin={y:meas,y0:y0,yn:yn,xn:xn,sx:unc,cost:(xp.costja(*,nys:nye)+xp.costjm(*,nys:nye))*xp.COSTJM_ATT.SCALE_FACTOR,conv:abs(xp.convergence(*,nys:nye)),tc:xp.ctt(*,nys:nye)*xp.ctt_att.scale_factor,itype:xp.phase(*,nys:nye),zc:xp.cth(*,nys:nye)*xp.cth_att.scale_factor,ni:xp.niter(*,nys:nye),white_sky_albedo:meas(*,*,0)*0.0-999.,clearsky_bt:meas*0.0-999.,xo:meas*0.0-999.,ae:meas(*,*,*)*0.0-999,ym:meas,cwp:meas(*,*,0)*0.0-999.,ymfit:ymfit,phase:xp.phase(*,nys:nye),x0:x0,lat:xp.lat(*,nys:nye),lon:xp.lon(*,nys:nye),solz:xp.SOLAR_ZENITH_VIEW_NO1(*,nys:nye),satz:xp.SATELLITE_ZENITH_VIEW_NO1(*,nys:nye),relaz:xp.REL_AZIMUTH_VIEW_NO1(*,nys:nye),cth:xp.cth(*,nys:nye)*xp.cth_att.SCALE_FACTOR,mask:xp.cc_total(*,nys:nye),alb:alb,lsflag:xp.lsflag(*,nys:nye), cccot:cvar(*,nys:nye)}
      
      
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
         print,'number of good retrievals',ngood
         if ngood le 0 then print,'no good points in this file skip file',filein
         if keyword_set(ps) then begin
            if ngood le 0 then begin
               pdfile=strreplace(psfi,'.ps','.pdf')
               print,'pdfile',pdfile
               spawn, 'touch '+ pdfile
            endif
         endif
         
         
         if ngood eq 0 then goto, skipend
         for kk=0,4 do print,'range state '+i2s(kk)+':',range(xin.xn(*,*,kk))
         
         if p eq 0 then begin
            ret=replicate({y:fltarr(nmeas),y0:fltarr(nmeas),yn:fltarr(nmeas),xn:fltarr(nstate),cost:0.0,conv:0,tc:0.0,itype:0,COLUMN_DENSITY:0.0,zc:0.0,ni:0,white_sky_albedo:0.0,clearsky_bt:1.0,x0:fltarr(nstate),xe:fltarr(nstate), COLUMN_DENSITY_ERROR:0.0,ymfit:fltarr(nmeas),sx:fltarr(nstate),ae:fltarr(nstate),alb:fltarr(nsol),mask:0.0,solz:0.0,satz:0.0,relaz:0.0,cth:0.0,lsflag:0.0,cccot:0.0},nph,ngood)
         endif
         
         ngood2 =n_elements(xp.lat(*,nys:nye))
         
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
;      whbad=where(ret(p,*).ymfit(4) lt -30., nbad)
         
;      if nbad gt 0 then begin
;         for ii=1,4 do ret(p,whbad).YN(ii)=-999.
;         for ii=1,4 do ret(p,whbad).Y(ii)=-999.
;         for ii=1,4 do ret(p,whbad).Ymfit(ii)=-999.
;      endif
      
         print,'number of failed retrievals:',nbad     
      
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
         
      endfor                    ;phas
      
      latr=range(xin.lat(igood))
      lonr=range(xin.lon(igood))
      
   
;      sg_temp={LATR:latr,LONR:lonr,UG:fltarr(3),VG:fltarr(3),LAT:xin.lat(*,nys:nye),LON:xin.lon(*,nys:nye),ID: inst+'-scan',SAT:1,NEQ:0,LL:0,SD:0,solz:xin.solz(*,nys:nye),satz:xin.satz(*,nys:nye),relaz:xin.relaz(*,nys:nye) }

      sg_temp={LATR:latr,LONR:lonr,UG:fltarr(3),VG:fltarr(3),LAT:xin.lat,LON:xin.lon,ID: inst+'-scan',SAT:1,NEQ:0,LL:0,SD:0,solz:xin.solz,satz:xin.satz,relaz:xin.relaz }
      
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
      
      
      ha_temp={FILE:fdir+'/'+fb,OFILE:fdir+'/'+fb,PROC_DATE:datein,DATA_DATE:datein,S:s_temp,SV:sv_temp,U:xs.SCANLINE_U[igood],V:xs.SCANLINE_V[igood],TIME:fltarr(ngood),SG:sg_temp,DIA:2,TYPES:'LIQUID',LUTRE_RANGE:dblarr(2,2),LOPD_RANGE:dblarr(2,2),NSTR:0,RSA:0,BAUM:0,SPI:1,YE:fltarr(nmeas),CTEST:-1,IDC:0,MAX_IT:20,SYI:0,EXTRA:0.0,HX:0,VERSION:3.18,NX:nstate,NY:nmeas,npix:ngood,NST:2}  
      
      
      ha_temp.sv.zstar=0
      
      latr=range(xin.lat)
      lonr=range(xin.lon)

;      latr=range(xin.lat[igood])
;      lonr=range(xin.lon[igood])

      ha_temp.sg.LATR =latr 
      ha_temp.sg.LONR =lonr
  

      if n_elements(xs) gt 0 then begin 
         xu=range(xs.SCANLINE_U[igood])
         xv=range(xs.SCANLINE_V[igood])
      endif
      ha_temp.sg.UG  =   [xu(0)+1,xu(1)+1,1] 
      ha_temp.sg.VG  =  [xv(0)+1,xv(1)+1,1]  
;      ha_temp.sg.LAT=xp.lat[igood] ;      
;      ha_temp.sg.LON=xp.lon[igood] ;      

      ha_temp.sg.LAT=xin.lat
      ha_temp.sg.LON=xin.lon

print,range(   ha_temp.sg.LON)    
;
;now call the plotting routine
;

      if n_elements(plot) gt 0 then begin
         plot_ret_cldmodel_modis,ret,ha_temp,_EXTRA=extra,clon=clon,clat=clat,eop_y=eop_y,eop_x=eop_x,ps=ps,night=night,itype=itype,/noqc,/rotate,nosec=nosec,error=error,fips=psfi ;error=error
      endif
      
endloop:
      
      dataout={ret:ret,ha:ha_temp}
      
skipend:
      print,'here skipend adim i',adim,i     
   endfor ;nchunks

   endfor                       ;loop over adim
   
   
   
   return,dataout
   
   
end
