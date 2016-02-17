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
;       file names must have WAT, ICE MLI or PP in them
;
;       if no secondary file is present then use /nosec keyword.
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
;       nosec : set this keyword when secondary file not present
;       ext : variable that can specify unconventional file namimg
;       chunksize:  number of along track pixels to plot into a single
;                   image file useful when studying whole orbits. If
;                   this is not set then defaults to the total lenght
;                    of the file
;       forc: set this keyword to overwrite plots
;       i37 : set this keyword if using 3.7um instead of 1.6um
;       fatplot : make plots wider
;       istomina : plot snow ice istomina diagnostics
;       v1 : older versions of file with different variable naming
;       mli : 
;       revision : 
;       night : plot for night only
;       latrange : 
;       aer : 
;       noir : plot when no IR channels are present 
;
;	History:
;
;       original file C. Poulsen created 2011 1st public release April 2013
;	

;idl>x=read_orac_nc_pp(/plot,filein='/misc/wantage_static/cpoulsen/cloud_ecv/postproc/2008/06/20/',chsize=1000,/forc,version='',/istomina,/ps,/i37,filter='fv13',/wat)

;x=read_orac_nc_pp(/plot,dirin='/misc/wantage_static/cpoulsen/cloud_ecv/postproc/modis/2008/07/10/',chsize=1000,/forc,version='fv17.0',/istomina,/ps,/i37,/wat)

;	Bugs:
;       Currently reads as default the ice secondary file for residual
;       and measurement information until proper level2 file is created.
;       Will probably fall over with a night file
;       code is hardwired for 5 measurements and 5 state vectors

;
;       RAL:
;       to package up newer versions of this software do:
;       mk_vm_src_code -mirror read_orac_nc_pp then rsync
;
;
;	C.Poulsen : 19 December 2011

; $Id$
;-
;==========================================================================
function read_orac_nc_pp,filein=filein,plot=plot,ps=ps,errflag=errflag,wat=wat,ice=ice,dirin=dirin,nosec=nosec,error=error,version=version,ext=ext,chsize=chsize,i37=i37,forc=forc,fatplot=fatplot,filter=filter,istomina=istomina,v1=v1,mli=mli,revision=revision,night=night,latrange=latrange,old=old,aer=aer,noir=noir


fileset=0
if ~keyword_set(filter) then filter=''
if keyword_set(filein) and  keyword_set(dirin) then fileset=1
if n_elements(version) eq 0 then version=''
if n_elements(revision) eq 0 then revision=''
errflag=0

if ~keyword_set(dirin) then dirin='/misc/wantage_static/cpoulsen/cloud_ecv/postproc/2008/06/20/'


if ~keyword_set(ext) then ext=''


if ~keyword_set(filein)  and ~keyword_set(ice) and ~keyword_set(wat) and ~keyword_set(mli) then filein='*'+filter+'*'+version+'.prim*.nc'
if ~keyword_set(filein) and keyword_set(wat) then filein='*'+filter+'*'+version+'*'+'*WAT*prim*.nc'
if ~keyword_set(filein) and keyword_set(ice) then filein='*'+filter+'*'+version+'*'+'*ICE*prim*.nc'
if ~keyword_set(filein) and keyword_set(mli) then filein='*'+filter+'*'+version+'*'+'*MLI*prim*.nc'

print,'filein a ',filein

if ~keyword_set(dirin) then begin
   dirin=file_dirname(filein)+'/'
   
endif else begin
;   filein=dirin+filein
endelse
print,'dirin read_orac_nc_pp',dirin
print,'filein',filein
if strpos(filein,'/') ge 0 then filein=file_basename(filein)

print,'test',dirin+filein
files=file_search( dirin+filein,count=adim)


if adim eq 0 then print,'cannot find any files check directory and filename',dirin+filein
if adim eq 0  then stop

if keyword_set(wat) or  keyword_set(ice) or keyword_set(mli) then fbphase=file_basename(files,'.primary'+ext+'.nc')

if keyword_set(wat) or  keyword_set(ice) or keyword_set(mli) then print,'fb',fbphase

if keyword_set(wat) or  keyword_set(ice) or keyword_set(mli) then  filein=dirin+fbphase+'*.prim*'+ext+'*nc'

if keyword_set(wat) or  keyword_set(ice)  or keyword_set(mli) then  files=file_search(filein,count=adim)
print,'filein read_orac_nc_pp',filein
print,'No. of files to plot:',adim

for i=0,adim-1 do begin
print,'file no.',i,files(i)
   filein=files(i)

   print,'file plotting now:',files(i)
   if strpos(filein,'MODIS') gt 0 then inst='modis'
   if strpos(filein,'MYD') gt 0 then inst='modis'
   if strpos(filein,'AATSR') gt 0 then inst='aatsr'
if strpos(filein,'ATSR2') gt 0 then inst='atsr2'
   if strpos(filein,'AVHRR') gt 0 then inst='avhrr'
   
   if inst eq 'atsr'  then message,'routine not valid for this instrument must edit to adapt it!'+' '+inst
   
   fb=file_basename(filein,'primary'+ext+'.nc')
   fb1=file_basename(filein,'.primary'+ext+'.nc')
   fd=file_dirname(filein)

   psname=fd+'/'+fb1
   if keyword_set(nosec) then psfisa=psname
   if keyword_set(nosec) then psfisa=psname+'_nosec'
  if keyword_set(fatplot) then psfisa=psname+'_fat'
   if ~keyword_set(nosec) then psfisa=psname+'_extrav2'

   
;
; some out put file have different extensions the next few lines cope
; with both
;
print,filein
print,'version',version
;   pos1=strpos(filein,'_'+version+'.')
   pos1=strpos(filein,version)
   pos2=strpos(filein,'_PP')
print,filein,'_'+version+'.'
print,filein,'_PP'
   print,'pos1,pos2',pos1,pos2
   print,'filein',filein
;help,filein
print,'fileset',fileset

   if ~keyword_set(wat) and ~keyword_set(ice)  and ~keyword_set(mli) and pos1 ge 0 then fbs=file_basename(filein)
fbs=file_basename(filein,'.primary.nc')

   if ~keyword_set(wat) and ~keyword_set(ice)  and ~keyword_set(mli) and pos2 gt 0 then fbs=file_basename(filein,'_PP.'+revision+'primary.nc')


;print,'fbs',fbs

   if fileset eq 1 then goto, skipfile
   watid=strpos(filein,'WAT')
   iceid=strpos(filein,'ICE')
   mliid=strpos(filein,'MLI')
print, watid,iceid,mliid

   
   if keyword_set(wat) or watid gt 0 then fbs=file_basename(filein,'WAT.primary'+ext+'.nc')
   if keyword_set(ice) or iceid gt 0 then fbs=file_basename(filein,'ICE.primary'+ext+'.nc')
   if keyword_set(mli) or mliid gt 0 then fbs=file_basename(filein,'MLI.primary'+ext+'.nc')

print,'fbs',fbs

   fdir=file_dirname(filein)
   
   print,'file:',fdir+'/'+fbs+'*'+revision+'*.nc'

;   checkp=file_search(fdir+'/'+fbs+'*'+revision+'*.nc',count=pdim)
  checkp=file_search(fdir+'/'+fbs+'*'+revision,count=pdim)
 

   print,'checkp',fdir+'/'+fbs+'*'+revision+'*.nc'
   print,'check primary and secondary file present:',pdim

file_num=0
if keyword_set(mli) then file_num=0


   if pdim le file_num then begin
      if ~keyword_set(nosec) then begin
         print,'not all files (sec and prim) present',fdir+'/'+fbs+'*.nc'
         errflag=1
         print,'looking for secondary?? file goto skipend'
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
   if ~keyword_set(nosec) and keyword_set(mli) then print,fdir+'/'+fbs+'MLI.secondary.nc'
   
skipfile:
   if fileset eq 0 then begin
   xpwat=ncdf_read(fdir+'/'+fb+'primary'+ext+'.nc')
endif else begin
print,'b filein',filein
 xpwat=ncdf_read(filein)

fdir=dirin
endelse

   
   if ~keyword_set(nosec)  and keyword_set(ice) then xswat=ncdf_read(fdir+'/'+fbs+'ICE.secondary'+ext+'.nc')
   if ~keyword_set(nosec)  and keyword_set(wat) then xswat=ncdf_read(fdir+'/'+fbs+'WAT.secondary'+ext+'.nc')
   if ~keyword_set(nosec)  and keyword_set(mli) then xswat=ncdf_read(fdir+'/'+fbs+'MLI.secondary'+ext+'.nc')
;   if ~keyword_set(nosec)  and ~keyword_set(wat)and ~keyword_set(ice) then xswat=ncdf_read(fdir+'/'+fbs+'.secondary'+ext+'.nc')


   if ~keyword_set(wat) and ~keyword_set(ice) and ~keyword_set(mli)  then begin
;get ice and water

      fb=strreplace(fb,'WAT','ICE')
      print,'xpwat',fdir+'/'+fb+'primary.nc'


      xpwat=ncdf_read(fdir+'/'+fb+'primary.nc')
      possec=strpos(fdir+'/'+fb+'primary.nc','PP')

      print,'sec file test',fdir+'/'+fbs+'.secondary'+ext+'.nc'

ffwat=fdir+'/'+fb+'primary.nc'

; new files do not have ice in the file name
ffice=strreplace(ffwat,'.primary','.secondary')

;      filess=file_search(fdir+'/'+strmid(fbs,0,80)+'*ICE*sec*',count=vdim)
      filess=file_search(ffice,count=vdim)
      print,'vdim',vdim,ffice

;print,fdir+'/'+strmid(fbs,0,80)+'*ICE*sec*'
if vdim eq 0  and  ~keyword_set(nosec) then goto,skipend

      if vdim gt 0 then begin

         if ~keyword_set(nosec) then begin

            xswat=ncdf_read(filess(0))

            endif
;         if ~keyword_set(nosec) then xswat=ncdf_read(fdir+'/'+fbs+'ICE.secondary'+ext+'.nc')
      endif
   endif 

   print,'after secondatry read'
   
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
      print,'number of chunks to process',nc,nchunks  
      
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
	if keyword_set(error) then psfis=psfis+'_error'
         pdfi=psfis+'.pdf'
         psfi=psfis


;
;estimate max and min lines for latrange this is helpful for
;

         if keyword_set(latrange) then begin
            print,latrange(0)
            latrange=[60,90]
            
            whlat=where(xpwat.lat(1,*) gt latrange(0) and xpwat.lat(1,*) lt latrange(1) ,nn)
            
            xr=range(whlat)
            nys=xr(0)
            nye=xr(1)
            
         endif

;
;create scan lines useful for plotting the swath nicely
;   


         SCANLINE_V=fltarr(nx,ny)
         SCANLINE_U=fltarr(nx,ny)


         for in=0,nx-1 do begin
            j=0
            for jin=nys,nye-1 do begin
               SCANLINE_U(in,j)=in
               SCANLINE_V(in,j)=jin
               j=j+1
            endfor
         endfor
       

         if ~keyword_set(nosec) and ~keyword_set(modis) then begin  
            xswat.SCANLINE_V  =SCANLINE_V
            xswat.SCANLINE_U  =SCANLINE_U
         endif
      
      
      ;   
;read in above and refill it
;
      if keyword_set(wat)  or keyword_set(ice)then nph=1
      if ~keyword_set(wat) and ~keyword_set(ice) then nph=1
      
      nmeas=5
      nstate=5
      nsol=4
      if inst eq 'modis' then nsol=4
      if inst eq 'avhrr' then nsol=4
      res=size(xpwat.lat)
      nx=res(1)
      ny=ny ;res(2)
      
;
;loop over both phase retrievals
;
      

      for p=0,nph-1 do begin
         if p eq 0 then xp=xpwat
         if ~keyword_set(nosec) then if p eq 0 then xs=xswat
         
         if p eq 1 then xp=xpice
         if ~keyword_set(nosec) then if p eq 1 then xs=xsice
         
         xn=fltarr(nx,ny,nstate)
         x0=fltarr(nx,ny,nstate)
         unc=fltarr(nx,ny,nstate)
         meas=fltarr(nx,ny,nmeas)
         xn_sx=fltarr(nx,ny,nstate,nstate)
         ymfit=fltarr(nx,ny,nmeas)
         yn=fltarr(nx,ny,nmeas)
         y0=fltarr(nx,ny,nmeas)
         alb=fltarr(nx,ny,nsol)
         cloud_alb=fltarr(nx,ny,nsol-1)

      
         xn(*,*,0)=xp.cot(*,nys:nye)*xp.cot_att.scale_factor
         xn(*,*,1)=xp.cer(*,nys:nye)*xp.cer_att.scale_factor
         xn(*,*,2)=xp.ctp(*,nys:nye)*xp.ctp_att.scale_factor
         xn(*,*,3)=xp.stemp(*,nys:nye)*xp.stemp_att.scale_factor
         xn(*,*,4)=xp.cc_total(*,nys:nye)*xp.cc_total_att.scale_factor
         
         if ~keyword_set(nosec) then begin
            if n_elements(xs) gt 0 then begin
               


               x0(*,*,0)=xs.cot_fg(*,nys:nye)*xs.cot_fg_att.scale_factor
               x0(*,*,1)=xs.cer_fg(*,nys:nye)*xs.cer_fg_att.scale_factor
               x0(*,*,2)=xs.ctp_fg(*,nys:nye)*xs.ctp_fg_att.scale_factor
               x0(*,*,3)=xs.stemp_fg(*,nys:nye)*xs.stemp_fg_att.scale_factor
            endif
         endif

         unc(*,*,0)=xp.cot_uncertainty(*,nys:nye)*xp.cot_uncertainty_att.scale_factor
         unc(*,*,1)=xp.cer_uncertainty(*,nys:nye)*xp.cer_uncertainty_att.scale_factor
         unc(*,*,2)=xp.ctp_uncertainty(*,nys:nye)*xp.ctp_uncertainty_att.scale_factor
         unc(*,*,3)=xp.stemp_uncertainty(*,nys:nye)*xp.stemp_uncertainty_att.scale_factor
         unc(*,*,4)=xp.cc_total_uncertainty(*,nys:nye)*xp.cc_total_uncertainty_att.scale_factor
         
         if ~keyword_set(nosec) then begin
            if n_elements(xs) gt 0 then begin
               
               if inst eq 'modis' then begin
                  meas(*,*,0)=xs.REFLECTANCE_IN_CHANNEL_NO_1(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_1_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_1_ATT.add_offset
                  meas(*,*,1)=xs.REFLECTANCE_IN_CHANNEL_NO_2(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
                  if ~keyword_set(i37) then begin
                     meas(*,*,2)=xs.REFLECTANCE_IN_CHANNEL_NO_6(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_6_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_6_ATT.add_offset
                  endif else begin

                     if ~keyword_set(noir) then     meas(*,*,2)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_20(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_20_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_20_ATT.add_offset
                  endelse              
                  
                  if ~keyword_set(noir) then                    meas(*,*,3)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_31(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_31_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_31_ATT.add_offset
                  if ~keyword_set(noir) then                    meas(*,*,4)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_32(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_32_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_32_ATT.add_offset
                  
                  
                  y0(*,*,0)=xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_1(*,nys:nye)*xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_1_ATT.scale_factor+xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_1_ATT.add_offset
                  y0(*,*,1)=xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_2(*,nys:nye)*xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
                  
                  if ~keyword_set(i37) then begin
                     y0(*,*,2)=xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_6(*,nys:nye)*xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_6_ATT.scale_factor+xs.FIRSTGUESS_REFLECTANCE_IN_CHANNEL_NO_6_ATT.add_offset
                  endif else begin
                     if ~keyword_set(noir) then                       y0(*,*,2)=xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_20(*,nys:nye)*xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_20_ATT.scale_factor+xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_20_ATT.add_offset
                     
                  endelse                  
                  
                  if ~keyword_set(noir) then                    y0(*,*,3)=xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_31(*,nys:nye)*xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_31_ATT.scale_factor+xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_31_ATT.add_offset
                  
                  if ~keyword_set(noir) then                    y0(*,*,4)=xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_32(*,nys:nye)*xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_32_ATT.scale_factor+xs.FIRSTGUESS_BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_32_ATT.add_offset
                  
                  
                  
                  ymfit(*,*,0)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_1(*,nys:nye)*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_1_ATT.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_1_ATT.add_offset
                  ymfit(*,*,1)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2(*,nys:nye)*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2_ATT.add_offset
                  if ~keyword_set(i37) then begin
                     ymfit(*,*,2)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_6(*,nys:nye)*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_6_ATT.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_6_ATT.add_offset
                  endif else begin
                     
                     if ~keyword_set(noir) then                       ymfit(*,*,2)=xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_20(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_20_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_20_ATT.add_offset
                  endelse                  
                  
                  if ~keyword_set(noir) then                    ymfit(*,*,3)=xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_31(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_31_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_31_ATT.add_offset
                  if ~keyword_set(noir) then                    ymfit(*,*,4)=xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_32(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_32_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_32_ATT.add_offset
                  
                  
                  alb(*,*,0)=xs.albedo_IN_CHANNEL_NO_1(*,nys:nye)*xs.albedo_IN_CHANNEL_NO_1_ATT.scale_factor+xs.albedo_IN_CHANNEL_NO_1_ATT.add_offset
                  
                  alb(*,*,1)=xs.albedo_IN_CHANNEL_NO_2(*,nys:nye)*xs.albedo_IN_CHANNEL_NO_2_ATT.scale_factor+xs.albedo_IN_CHANNEL_NO_2_ATT.add_offset
                  
                  alb(*,*,2)=xs.albedo_IN_CHANNEL_NO_6(*,nys:nye)*xs.albedo_IN_CHANNEL_NO_6_ATT.scale_factor+xs.albedo_IN_CHANNEL_NO_6_ATT.add_offset
                  if keyword_set(i37) then begin
                     alb(*,*,3)=xs.albedo_IN_CHANNEL_NO_20(*,nys:nye)*xs.albedo_IN_CHANNEL_NO_20_ATT.scale_factor+xs.albedo_IN_CHANNEL_NO_20_ATT.add_offset
                  endif
                  
                  
                  cloud_alb(*,*,0)=xp.cloud_albedo_IN_CHANNEL_NO_1(*,nys:nye)*xp.cloud_albedo_IN_CHANNEL_NO_1_ATT.scale_factor+xp.cloud_albedo_IN_CHANNEL_NO_1_ATT.add_offset

                  cloud_alb(*,*,1)=xp.cloud_albedo_IN_CHANNEL_NO_2(*,nys:nye)*xp.cloud_albedo_IN_CHANNEL_NO_2_ATT.scale_factor+xp.cloud_albedo_IN_CHANNEL_NO_2_ATT.add_offset
                  
                  if ~keyword_set(i37) then begin
                     
                     alb(*,*,2)=xs.albedo_IN_CHANNEL_NO_6(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_6_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_6_ATT.add_offset
                  endif else begin
                     alb(*,*,2)=xs.REFLECTANCE_IN_CHANNEL_NO_2(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
                     
                     cloud_alb(*,*,2)=xp.cloud_albedo_IN_CHANNEL_NO_2(*,nys:nye)*xp.cloud_albedo_IN_CHANNEL_NO_2_ATT.scale_factor+xp.cloud_albedo_IN_CHANNEL_NO_2_ATT.add_offset
                  endelse
                  
                  
               endif            ;modis
               
               if inst eq 'aatsr' or inst eq 'atsr2' then begin
                  if ~keyword_set(mli) then begin                  
                     alb(*,*,0)=xs.albedo_IN_CHANNEL_NO_2(*,nys:nye)*xs.albedo_IN_CHANNEL_NO_2_ATT.scale_factor+xs.ALBEDO_IN_CHANNEL_NO_2_ATT.add_offset
                     alb(*,*,1)=xs.albedo_IN_CHANNEL_NO_3(*,nys:nye)*xs.albedo_IN_CHANNEL_NO_3_ATT.scale_factor+xs.ALBEDO_IN_CHANNEL_NO_3_ATT.add_offset
                     
                     alb(*,*,2)=xs.albedo_IN_CHANNEL_NO_4(*,nys:nye)*xs.albedo_IN_CHANNEL_NO_4_ATT.scale_factor+xs.ALBEDO_IN_CHANNEL_NO_4_ATT.add_offset
                     if keyword_set(i37) then begin
                        alb(*,*,3)=xs.albedo_IN_CHANNEL_NO_4(*,nys:nye)*xs.albedo_IN_CHANNEL_NO_4_ATT.scale_factor+xs.ALBEDO_IN_CHANNEL_NO_4_ATT.add_offset
                     endif
                     
                     
                  endif
                  if ~keyword_set(v1)  and ~keyword_set(mli)then begin
                     
                     cloud_alb(*,*,0)=xp.cloud_albedo_IN_CHANNEL_NO_2(*,nys:nye)*xp.cloud_albedo_IN_CHANNEL_NO_2_ATT.scale_factor+xp.cloud_albedo_IN_CHANNEL_NO_2_ATT.add_offset
                     cloud_alb(*,*,1)=xp.cloud_albedo_IN_CHANNEL_NO_3(*,nys:nye)*xp.cloud_albedo_IN_CHANNEL_NO_3_ATT.scale_factor+xp.cloud_albedo_IN_CHANNEL_NO_3_ATT.add_offset
                     
                  endif



                  if ~keyword_set(mli) then begin                  
                     meas(*,*,0)=xs.REFLECTANCE_IN_CHANNEL_NO_2(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_2_att.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
                     meas(*,*,1)=xs.REFLECTANCE_IN_CHANNEL_NO_3(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_3_att.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.add_offset
                  endif
                  
                  if ~keyword_set(i37) then begin
                     meas(*,*,2)=xs.REFLECTANCE_IN_CHANNEL_NO_4(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_4_att.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_4_ATT.add_offset
                  endif else begin
if ~keyword_set(noir) then                       meas(*,*,2)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_5(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_5_att.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_5_ATT.add_offset
                  endelse
if ~keyword_set(noir) then                    meas(*,*,3)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_att.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.add_offset
if ~keyword_set(noir) then                    meas(*,*,4)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_att.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.add_offset
                  


                  if ~keyword_set(mli) then begin                  
                     ymfit(*,*,0)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2(*,nys:nye)*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2_att.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2_ATT.add_offset
                     ymfit(*,*,1)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_3(*,nys:nye)*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_3_att.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_3_ATT.add_offset
                  endif
                  
                  if ~keyword_set(i37) then begin
                     ymfit(*,*,2)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_4(*,nys:nye)*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_4_att.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_4_ATT.add_offset
                  endif else begin
                     
if ~keyword_set(noir) then                       ymfit(*,*,2)=xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_5(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_5_att.scale_factor+xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_5_ATT.add_offset
                  endelse
if ~keyword_set(noir) then                    ymfit(*,*,3)=xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_6(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_6_att.scale_factor+xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_6_ATT.add_offset
if ~keyword_set(noir) then                    ymfit(*,*,4)=xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_7(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_7_att.scale_factor+xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_7_ATT.add_offset
                  
                  if ~keyword_set(mli) then begin                                                 
                     yn(*,*,0)=xs.REFLECTANCE_IN_CHANNEL_NO_2(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset+ymfit(*,*,0)
                     yn(*,*,1)=xs.REFLECTANCE_IN_CHANNEL_NO_3(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.add_offset+ymfit(*,*,1)
                  endif
                  
                  if ~keyword_set(i37) then begin
                     yn(*,*,2)=xs.REFLECTANCE_IN_CHANNEL_NO_3(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_3_ATT.add_offset+ymfit(*,*,2)
                  endif else begin
if ~keyword_set(noir) then                       yn(*,*,2)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_5(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_5_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_5_ATT.add_offset+ymfit(*,*,2)
                  endelse
if ~keyword_set(noir) then                    yn(*,*,3)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.add_offset+ymfit(*,*,3)
                  
if ~keyword_set(noir) then                    yn(*,*,4)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.add_offset+ymfit(*,*,4)
                  
                  if ~keyword_set(mli) then begin                               
                     if ~keyword_set(v1) then begin
                        cloud_alb(*,*,0)=xp.cloud_albedo_IN_CHANNEL_NO_2(*,nys:nye)*xp.cloud_albedo_IN_CHANNEL_NO_2_ATT.scale_factor+xp.cloud_albedo_IN_CHANNEL_NO_2_ATT.add_offset
                        cloud_alb(*,*,1)=xp.cloud_albedo_IN_CHANNEL_NO_3(*,nys:nye)*xp.cloud_albedo_IN_CHANNEL_NO_3_ATT.scale_factor+xp.cloud_albedo_IN_CHANNEL_NO_3_ATT.add_offset
                        
                     endif
                     
                     alb(*,*,0)=xs.albedo_IN_CHANNEL_NO_2(*,nys:nye)*xs.ALBEDO_IN_CHANNEL_NO_2_ATT.scale_factor+xs.ALBEDO_IN_CHANNEL_NO_2_ATT.add_offset
                     alb(*,*,1)=xs.albedo_IN_CHANNEL_NO_3(*,nys:nye)*xs.ALBEDO_IN_CHANNEL_NO_3_ATT.scale_factor+xs.ALBEDO_IN_CHANNEL_NO_3_ATT.add_offset
                     
                     alb(*,*,2)=xs.albedo_IN_CHANNEL_NO_4(*,nys:nye)*xs.ALBEDO_IN_CHANNEL_NO_4_ATT.scale_factor+xs.ALBEDO_IN_CHANNEL_NO_4_ATT.add_offset
                     if keyword_set(i37) then begin
                        alb(*,*,3)=xs.albedo_IN_CHANNEL_NO_4(*,nys:nye)*xs.ALBEDO_IN_CHANNEL_NO_4_ATT.scale_factor+xs.ALBEDO_IN_CHANNEL_NO_4_ATT.add_offset
                     endif

                  endif
               endif            ;aatsr
               
               
               if inst eq 'avhrr' then begin
                  
                  if ~keyword_set(v1) then begin
                     cloud_alb(*,*,0)=xp.cloud_albedo_IN_CHANNEL_NO_1(*,nys:nye)*xp.cloud_albedo_IN_CHANNEL_NO_1_ATT.scale_factor+xp.cloud_albedo_IN_CHANNEL_NO_1_ATT.add_offset
                     cloud_alb(*,*,1)=xp.cloud_albedo_IN_CHANNEL_NO_2(*,nys:nye)*xp.cloud_albedo_IN_CHANNEL_NO_2_ATT.scale_factor+xp.cloud_albedo_IN_CHANNEL_NO_2_att.add_offset
                     cloud_alb(*,*,2)=xp.cloud_albedo_IN_CHANNEL_NO_3(*,nys:nye)*xp.cloud_albedo_IN_CHANNEL_NO_3_ATT.scale_factor+xp.cloud_albedo_IN_CHANNEL_NO_3_att.add_offset
                  endif
                  alb(*,*,0)=xs.albedo_IN_CHANNEL_NO_1(*,nys:nye)*xs.ALBEDO_IN_CHANNEL_NO_1_ATT.scale_factor+xs.ALBEDO_IN_CHANNEL_NO_1_ATT.add_offset
                  alb(*,*,1)=xs.albedo_IN_CHANNEL_NO_2(*,nys:nye)*xs.ALBEDO_IN_CHANNEL_NO_2_ATT.scale_factor+xs.ALBEDO_IN_CHANNEL_NO_2_ATT.add_offset
                  alb(*,*,2)=xs.albedo_IN_CHANNEL_NO_3(*,nys:nye)*xs.ALBEDO_IN_CHANNEL_NO_3_ATT.scale_factor+xs.ALBEDO_IN_CHANNEL_NO_3_ATT.add_offset



                  meas(*,*,0)=xs.REFLECTANCE_IN_CHANNEL_NO_1(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_1_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_1_ATT.add_offset
                  meas(*,*,1)=xs.REFLECTANCE_IN_CHANNEL_NO_2(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
if ~keyword_set(noir) then                    meas(*,*,2)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_4(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_4_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_4_ATT.add_offset
if ~keyword_set(noir) then                    meas(*,*,3)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_5(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_5_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_5_ATT.add_offset

if ~keyword_set(noir) then                    meas(*,*,4)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.add_offset
                  
;
;nb note add_offset required
;
                  ymfit(*,*,0)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_1(*,nys:nye)*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_1_ATT.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_1_ATT.add_offset
                  ymfit(*,*,1)=xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2(*,nys:nye)*xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_RESIDUAL_IN_CHANNEL_NO_2_ATT.add_offset
if ~keyword_set(noir) then                    ymfit(*,*,2)=xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_4(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_4_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_4_ATT.add_offset
if ~keyword_set(noir) then                    ymfit(*,*,3)=xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_5(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_5_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_5_ATT.add_offset
if ~keyword_set(noir) then                    ymfit(*,*,4)=xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_6(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_6_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_RESIDUAL_IN_CHANNEL_NO_6_ATT.add_offset
                  
                  yn(*,*,0)=xs.REFLECTANCE_IN_CHANNEL_NO_1(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_1_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_1_ATT.add_offset+ymfit(*,*,0)
                  yn(*,*,1)=xs.REFLECTANCE_IN_CHANNEL_NO_2(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset+ymfit(*,*,1)
if ~keyword_set(noir) then                    yn(*,*,2)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_4(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_4_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_4_ATT.add_offset+ymfit(*,*,2)
if ~keyword_set(noir) then                    yn(*,*,3)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_5(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_5_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_5_ATT.add_offset+ymfit(*,*,3)
if ~keyword_set(noir) then                    yn(*,*,4)=xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6(*,nys:nye)*xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.scale_factor+xs.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.add_offset+ymfit(*,*,4)
                  
                  alb(*,*,0)=xs.albedo_IN_CHANNEL_NO_1(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_1_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_1_ATT.add_offset
                  alb(*,*,1)=xs.albedo_IN_CHANNEL_NO_2(*,nys:nye)*xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.scale_factor+xs.REFLECTANCE_IN_CHANNEL_NO_2_ATT.add_offset
                  alb(*,*,2)=xs.albedo_IN_CHANNEL_NO_3(*,nys:nye)*xs.albedo_IN_CHANNEL_NO_3_ATT.scale_factor+xs.albedo_IN_CHANNEL_NO_3_ATT.add_offset
                  
                  if ~keyword_set(v1) then begin
                     cloud_alb(*,*,0)=xp.cloud_albedo_IN_CHANNEL_NO_1(*,nys:nye)*xp.cloud_albedo_IN_CHANNEL_NO_1_ATT.scale_factor+xp.cloud_albedo_IN_CHANNEL_NO_1_att.add_offset
                     cloud_alb(*,*,1)=xp.cloud_albedo_IN_CHANNEL_NO_2(*,nys:nye)*xp.cloud_albedo_IN_CHANNEL_NO_2_ATT.scale_factor+xp.cloud_albedo_IN_CHANNEL_NO_2_att.add_offset
                     cloud_alb(*,*,2)=xp.cloud_albedo_IN_CHANNEL_NO_3(*,nys:nye)*xp.cloud_albedo_IN_CHANNEL_NO_3_ATT.scale_factor+xp.cloud_albedo_IN_CHANNEL_NO_3_att.add_offset
                  endif
                  
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
            print,' check to see valida values are being read'
            print,'meas range ch 1:',range(meas(*,*,0))
            print,'meas range ch 2:',range(meas(*,*,1))
            print,'meas range ch 3:',range(meas(*,*,2))
            print,'meas range ch 4:',range(meas(*,*,3))
            print,'meas range ch 5:',range(meas(*,*,4))
            
            if (max(meas(*,*,0)) lt .1) then night=1
            if keyword_set(night) then night=0
            if keyword_set(mli) then goto, skipnightcheck
            if night eq 1 then print,'skipping night file'
            if night eq 1 then goto,skipend
skipnightcheck:
            print,'ymfit range ch 1:',range(ymfit(*,*,0))
            print,'ymfit range ch 2:',range(ymfit(*,*,1))
            print,'ymfit range ch 3:',range(ymfit(*,*,2))
            print,'ymfit range ch 4:',range(ymfit(*,*,3))
            print,'ymfit range ch 5:',range(ymfit(*,*,4))
         endif
         
         print,'cost fm range',range(xp.costja)
         print,'cost meas range',range(xp.costjm)
; fill the structure      
         
         if keyword_set(ice) or keyword_set(wat) or  keyword_set(mli)   then begin
            cvar=xp.cc_total*0.-999
         endif else begin
            cvar=xp.cc_total*0.-999
            
         endelse
         
         
; old version of files         
         if ~keyword_set(v1) then begin 
            
            if ~keyword_set(mli) then begin 
               if n_tags(xs) le 103 then begin

                  xin={y:meas,y0:y0,yn:yn,xn:xn,sx:unc,cost:(xp.costja(*,nys:nye)+xp.costjm(*,nys:nye))*xp.COSTJM_ATT.SCALE_FACTOR,conv:abs(xp.convergence(*,nys:nye)),tc:xp.ctt(*,nys:nye)*xp.ctt_att.scale_factor,itype:xp.phase(*,nys:nye),zc_cor:xp.cth_corrected(*,nys:nye)*xp.cth_corrected_att.scale_factor,zc:xp.cth(*,nys:nye)*xp.cth_att.scale_factor,ni:xp.niter(*,nys:nye),white_sky_albedo:meas(*,*,0)*0.0-999.,clearsky_bt:meas*0.0-999.,xo:meas*0.0-999.,ae:meas(*,*,*)*0.0-999,ym:meas,ymfit:ymfit,phase:xp.phase(*,nys:nye),x0:x0,lat:xp.lat(*,nys:nye),lon:xp.lon(*,nys:nye),solz:xp.SOLAR_ZENITH_VIEW_NO1(*,nys:nye),satz:xp.SATELLITE_ZENITH_VIEW_NO1(*,nys:nye),relaz:xp.REL_AZIMUTH_VIEW_NO1(*,nys:nye),cth:xp.cth(*,nys:nye)*xp.cth_att.SCALE_FACTOR,cth_cor:xp.cth_corrected(*,nys:nye)*xp.cth_corrected_att.SCALE_FACTOR,nn_pre:xp.cccot_pre(*,nys:nye)*xp.cccot_pre_att.SCALE_FACTOR,ice_mask:xp.NISEMASK(*,nys:nye),land_use:xp.lusflag(*,nys:nye),mask:xp.cc_total(*,nys:nye),cloud_type:xp.CLDTYPE(*,nys:nye),alb:alb,lsflag:xp.lsflag(*,nys:nye), cccot:cvar(*,nys:nye),illum:abs(xp.illum(*,nys:nye)),cloud_alb:cloud_alb,nn_pre_mask:xp.cc_total(*,nys:nye),phase_pavolonis:xp.cldtype(*,nys:nye),cwp:xp.cwp(*,nys:nye)*xp.cwp_att.scale_factor}

               endif else begin
                  
                  xin={y:meas,y0:y0,yn:yn,xn:xn,sx:unc,cost:(xp.costja(*,nys:nye)+xp.costjm(*,nys:nye))*xp.COSTJM_ATT.SCALE_FACTOR,conv:abs(xp.convergence(*,nys:nye)),tc:xp.ctt(*,nys:nye)*xp.ctt_att.scale_factor,itype:xp.phase(*,nys:nye),zc:xp.cth(*,nys:nye)*xp.cth_att.scale_factor,ni:xp.niter(*,nys:nye),white_sky_albedo:meas(*,*,0)*0.0-999.,clearsky_bt:meas*0.0-999.,xo:meas*0.0-999.,ae:meas(*,*,*)*0.0-999,ym:meas,ymfit:ymfit,phase:xp.phase(*,nys:nye),x0:x0,lat:xp.lat(*,nys:nye),lon:xp.lon(*,nys:nye),solz:xp.SOLAR_ZENITH_VIEW_NO1(*,nys:nye),satz:xp.SATELLITE_ZENITH_VIEW_NO1(*,nys:nye),relaz:xp.REL_AZIMUTH_VIEW_NO1(*,nys:nye),cth_cor:xp.cth_corrected(*,nys:nye)*xp.cth_corrected_att.SCALE_FACTOR,cth:xp.cth(*,nys:nye)*xp.cth_att.SCALE_FACTOR,nn_pre:xp.cccot_pre(*,nys:nye)*xp.cccot_pre_att.SCALE_FACTOR,ice_mask:xp.NISEMASK(*,nys:nye),land_use:xp.lusflag(*,nys:nye),mask:xp.cc_total(*,nys:nye),cloud_type:xp.CLDTYPE(*,nys:nye),alb:alb,lsflag:xp.lsflag(*,nys:nye), cccot:cvar(*,nys:nye),illum:abs(xp.illum(*,nys:nye)),cloud_alb:cloud_alb,nn_pre_mask:xp.CLouDMASK_pre(*,nys:nye),phase_pavolonis:xp.PHASE_PAVOLONIS(*,nys:nye),cwp:xp.cwp(*,nys:nye)*xp.cwp_att.scale_factor}
               endelse ;tags < 103
               
               
               
            endif else begin;mli
               xin={y:meas,y0:y0,yn:yn,xn:xn,sx:unc,cost:(xp.costja(*,nys:nye)+xp.costjm(*,nys:nye))*xp.COSTJM_ATT.SCALE_FACTOR,conv:abs(xp.convergence(*,nys:nye)),tc:xp.ctt(*,nys:nye)*xp.ctt_att.scale_factor,itype:xp.phase(*,nys:nye),zc:xp.cth(*,nys:nye)*xp.cth_att.scale_factor,ni:xp.niter(*,nys:nye),white_sky_albedo:meas(*,*,0)*0.0-999.,clearsky_bt:meas*0.0-999.,xo:meas*0.0-999.,ae:meas(*,*,*)*0.0-999,ym:meas,ymfit:ymfit,phase:xp.phase(*,nys:nye),x0:x0,lat:xp.lat(*,nys:nye),lon:xp.lon(*,nys:nye),solz:xp.SOLAR_ZENITH_VIEW_NO1(*,nys:nye),satz:xp.SATELLITE_ZENITH_VIEW_NO1(*,nys:nye),relaz:xp.REL_AZIMUTH_VIEW_NO1(*,nys:nye),cth:xp.cth(*,nys:nye)*xp.cth_att.SCALE_FACTOR,cth_cor:xp.cth_corrected(*,nys:nye)*xp.cth_corrected_att.SCALE_FACTOR,nn_pre:xp.cccot_pre(*,nys:nye)*xp.cccot_pre_att.SCALE_FACTOR,ice_mask:xp.NISEMASK(*,nys:nye),land_use:xp.lusflag(*,nys:nye),mask:xp.cldmask(*,nys:nye),cloud_type:xp.CLDTYPE(*,nys:nye),alb:alb,lsflag:xp.lsflag(*,nys:nye), cccot:cvar(*,nys:nye),illum:abs(xp.illum(*,nys:nye)),cloud_alb:cloud_alb,nn_pre_mask:xp.CLDMASK(*,nys:nye),phase_pavolonis:xp.CLDTYPE(*,nys:nye),cwp:xp.cwp(*,nys:nye)*xp.cwp_att.scale_factor}
            endelse
             ;end of mli

         endif else begin ;v1
; older version
            
            xin={y:meas,y0:y0,yn:yn,xn:xn,sx:unc,cost:(xp.costja(*,nys:nye)+xp.costjm(*,nys:nye))*xp.COSTJM_ATT.SCALE_FACTOR,conv:abs(xp.convergence(*,nys:nye)),tc:xp.ctt(*,nys:nye)*xp.ctt_att.scale_factor,itype:xp.phase(*,nys:nye),zc:xp.cth(*,nys:nye)*xp.cth_att.scale_factor,ni:xp.niter(*,nys:nye),white_sky_albedo:meas(*,*,0)*0.0-999.,clearsky_bt:meas*0.0-999.,xo:meas*0.0-999.,ae:meas(*,*,*)*0.0-999,ym:meas,ymfit:ymfit,phase:xp.phase(*,nys:nye),x0:x0,lat:xp.lat(*,nys:nye),lon:xp.lon(*,nys:nye),solz:xp.SOLAR_ZENITH_VIEW_NO1(*,nys:nye),satz:xp.SATELLITE_ZENITH_VIEW_NO1(*,nys:nye),relaz:xp.REL_AZIMUTH_VIEW_NO1(*,nys:nye),cth:xp.cth(*,nys:nye)*xp.cth_att.SCALE_FACTOR,cth_cor:xp.cth_corrected(*,nys:nye)*xp.cth_corrected_att.SCALE_FACTOR,nn_pre:xp.CCCOT(*,nys:nye)*xp.CCCOT_att.SCALE_FACTOR,nn_pre_mask:xp.CC_TOTAL(*,nys:nye),ice_mask:xp.lsflag(*,nys:nye),land_use:xp.lsflag(*,nys:nye)*0,mask:xp.cc_total(*,nys:nye),cloud_type:xp.lsflag(*,nys:nye)*0,alb:alb,lsflag:xp.lsflag(*,nys:nye), cccot:cvar(*,nys:nye),illum:abs(xp.illum(*,nys:nye)),cwp:xp.cwp(*,nys:nye)*xp.cwp_att.scale_factor}
            
         endelse ;end of version
      

      print,' range illum:',range(xp.illum(*,nys:nye))
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
;            igood = where(xin.xn(*,*,2) gt 0. and xin.ymfit(*,*,2) gt -30.,ngood,complement=ibad,ncomplement=nbad)
;           igood = where(xin.xn(*,*,2) gt -999.0 and xin.ymfit(*,*,2)
;           gt -30.,ngood,complement=ibad,ncomplement=nbad)

           igood = where(xin.xn(*,*,2) gt -999.0,ngood,complement=ibad,ncomplement=nbad)


;if keyword_set(latrange) then begin
;      igood = where(xin.xn(*,*,2) gt -999.0 and xin.lat gt latrange(0) and xin.lat lt latrange(2),ngood,complement=ibad,ncomplement=nbad)
;endif

         endif else begin
;            igood = where(xin.xn(*,*,2) gt 0,ngood,complement=ibad,ncomplement=nbad)
if ~keyword_set(noir) then   igood = where(xin.xn(*,*,2) gt -999.0,ngood,complement=ibad,ncomplement=nbad)
if keyword_set(noir) then     igood = where(xin.xn(*,*,1) gt -999.0,ngood,complement=ibad,ncomplement=nbad)
;if keyword_set(latrange) then begin
;      igood = where(xin.xn(*,*,2) gt -999.0 and xin.lat gt latrange(0) and xin.lat lt latrange(2),ngood,complement=ibad,ncomplement=nbad)
;endif

         endelse      
         print,'number of good/bad retrievals',ngood,nbad

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
            ret=replicate({y:fltarr(nmeas),y0:fltarr(nmeas),yn:fltarr(nmeas),xn:fltarr(nstate),cost:0.0,conv:0,illum:0,tc:0.0,itype:0,COLUMN_DENSITY:0.0,zc:0.0,ni:0,white_sky_albedo:0.0,clearsky_bt:1.0,x0:fltarr(nstate),xe:fltarr(nstate), COLUMN_DENSITY_ERROR:0.0,ymfit:fltarr(nmeas),sx:fltarr(nstate),ae:fltarr(nstate),alb:fltarr(nsol),cloud_alb:fltarr(nsol-1),mask:0.0,solz:0.0,satz:0.0,relaz:0.0,cth:0.0,cth_cor:0.0,lsflag:0.0,cccot:0.0,nn_pre:0.0,nn_pre_mask:0.0,ice_mask:0.0,land_use:0.0,cloud_type:0.0,phase_pavolonis:0,cwp:0.0,lat:0.0,lon:0.0},nph,ngood)
         endif
         
         ngood2 =n_elements(xp.lat(*,nys:nye))

;
;reform 2 d array
;
         xin_y=reform(xin.y(*,*,*),ngood2,nmeas)
         xin_alb=reform(xin.alb(*,*,*),ngood2,nsol)
if ~keyword_set(v1) then    xin_cloud_alb=reform(xin.cloud_alb(*,*,*),ngood2,nsol-1)
if keyword_set(v1) then  xin_cloud_alb=reform(xin.alb(*,*,0:2),ngood2,nsol-1);no cloud albedo in thses files
         xin_y0=reform(xin.y0(*,*,*),ngood2,nmeas)
         xin_ymfit=reform(xin.ymfit(*,*,*),ngood2,nmeas)
         xin_xn=reform(xin.xn(*,*,*),ngood2,nstate)
         xin_cth=reform(xin.cth(*,*),ngood2)
         xin_lat=reform(xin.lat(*,*),ngood2)
         xin_lon=reform(xin.lon(*,*),ngood2)
         xin_cth_cor=reform(xin.cth_cor(*,*),ngood2)

       
         xin_cwp=reform(xin.cwp(*,*,*),ngood2)
         xin_x0=reform(xin.x0(*,*,*),ngood2,nstate)
         xin_yn=reform(xin.yn(*,*,*),ngood2,nmeas)
         xin_sx=reform(xin.sx(*,*,*),ngood2,nmeas)
         xin_ae=reform(xin.ae(*,*,*),ngood2,nstate)
         xin_solz=reform(xin.solz(*,*),ngood2)
         xin_lsflag=reform(xin.lsflag(*,*),ngood2)
         xin_satz=reform(xin.satz(*,*),ngood2)
         xin_relaz=reform(xin.relaz(*,*),ngood2)
         xin_cccot=reform(xin.cccot(*,*),ngood2)
         
         
         iretgood = where(xin_xn(*,2) gt -999.0,nretgood,complement=ibad,ncomplement=nbad)
         
         
         ret(p,*).y(0:nmeas-1)= reform(transpose(xin_y(iretgood,*)),nmeas,1,nretgood)
         ret(p,*).Y0(0:nmeas-1)=reform(transpose(xin_y0(iretgood,*)),nmeas,1,nretgood)
         ret(p,*).YN(0:nmeas-1)=reform(transpose((xin_ymfit(iretgood,*)+xin_y(iretgood,*))),nmeas,1,nretgood)
  
         ret(p,*).alb(0:nsol-1)= reform(transpose(xin_alb(iretgood,*)),nsol,1,nretgood)
         ret(p,*).cloud_alb(0:nsol-2)= reform(transpose(xin_cloud_alb(iretgood,*)),nsol-1,1,nretgood)
         ret(p,*).Ymfit(0:nmeas-1)=reform(transpose(xin_ymfit(iretgood,*)),nmeas,1,nretgood)

      
         print,'number of failed retrievals:',nbad     
      
         ret(p,*).XN=reform(transpose(xin_xn(iretgood,*)),nstate,1,nretgood)
         ret(p,*).sx=reform(transpose(xin_sx(iretgood,*)),nstate,1,nretgood)
         ret(p,*).xe=reform(transpose(xin_sx(iretgood,*)),nstate,1,nretgood)
         ret(p,*).COST=reform(transpose(xin.cost(igood)),1,nretgood) 
         ret(p,*).CONV=reform(transpose(xin.conv(igood)),1,nretgood) 
         ret(p,*).illum=reform(transpose(xin.illum(igood)),1,nretgood) 
         ret(p,*).relaz=reform(transpose(xin.relaz(igood)),1,nretgood) 
         ret(p,*).solz=reform(transpose(xin.solz(igood)),1,nretgood) 
         ret(p,*).lat=reform(transpose(xin.lat(igood)),1,nretgood) 
         ret(p,*).lon=reform(transpose(xin.lon(igood)),1,nretgood) 
         ret(p,*).lsflag=reform(transpose(xin.lsflag(igood)),1,nretgood) 
         ret(p,*).satz=reform(transpose(xin.satz(igood)),1,nretgood) 
         ret(p,*).cth=reform(transpose(xin.cth(igood)),1,nretgood) 
         ret(p,*).cth_cor=reform(transpose(xin.cth_cor(igood)),1,nretgood) 
         ret(p,*).cwp=reform(transpose(xin.cwp(igood)),1,nretgood) 
         ret(p,*).cccot=reform(transpose(xin.cccot(igood)),1,nretgood) 
         ret(p,*).TC = reform(transpose(xin.tc(igood)),1,nretgood) 
         ret(p,*).ITYPE=reform(transpose(xin.itype(igood)),1,nretgood)
    if ~keyword_set(v1) then      ret(p,*).phase_pavolonis=reform(transpose(xin.phase_pavolonis(igood)),1,nretgood) 
    if keyword_set(v1)  then    ret(p,*).phase_pavolonis=reform(transpose(xin.cccot(igood)),1,nretgood) 
         ret(p,*).mask=reform(transpose(xin.mask(igood)),1,nretgood) 
         ret(p,*).ZC= reform(transpose(xin.zc(igood)),1,nretgood)
         ret(p,*).NI=   reform(transpose(xin.ni(igood)),1,nretgood) 
         ret(p,*).WHITE_SKY_ALBEDO=reform(transpose(xin.white_sky_albedo(igood)),1,nretgood)
         ret(p,*).CLEARSKY_BT= 0.0 ;reform(xin.meas(igood,4)*0.0,1,nretgood) 
         ret(p,*).X0 =  reform(transpose(xin_x0(iretgood,*)),nstate,1,nretgood)
         ret(p,*).COLUMN_DENSITY_ERROR=reform(xin.cwp(igood),1,nretgood)

         ret(p,*).nn_pre=reform(transpose(xin.nn_pre(igood)),1,nretgood) 
         ret(p,*).nn_pre_mask=reform(transpose(xin.nn_pre_mask(igood)),1,nretgood) 
         ret(p,*).ice_mask=reform(transpose(xin.ice_mask(igood)),1,nretgood) 
         ret(p,*).land_use=reform(transpose(xin.land_use(igood)),1,nretgood) 
         ret(p,*).cloud_type=reform(transpose(xin.cloud_type(igood)),1,nretgood) 

         
      endfor                    ;phas
      
      latr=range(xin.lat(igood))
      lonr=range(xin.lon(igood))
      
;Cp changed decem 6th sat to 0 from 1
      sg_temp={LATR:latr,LONR:lonr,UG:fltarr(3),VG:fltarr(3),LAT:xin.lat,LON:xin.lon,ID: inst+'-scan',SAT:1,NEQ:0,LL:0,SD:0,solz:xin.solz,satz:xin.satz,relaz:xin.relaz }
      
 ;cp  ha_sav=rd_sav('~/Temp/orac/h_sav.sav')
;help,xs,/str
;; for aatsr only
;if tag_exist(xs,'BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_5') then i37=1

;bug need to create rttov structure for 3.7 file
;hs=rd_sav('/home/cluster/cpoulsen/Temp/orac/imager_i37.sav')
;if keyword_set(i37) then ha_sav=rd_sav('/home/cluster/cpoulsen/imager/imager_i37.sav')

;      ha_sav=create_h_sav()
;      print,'a'

;wr_sav,'~/Temp/ha.sav',ha_sav
      ha_sav=rd_sav('ha.sav')

      ha_sav.sv.zstar=0 
      sv_temp=ha_sav.sv
      s_temp=ha_sav.s
      
      
      print,'b'
      if inst eq 'aatsr' then begin
         pos3=strpos(fb,'ENV__')
         if pos3 gt 0 then begin
            posdate=strpos(fb,'ENV__')
            datein=strmid(fb,posdate+5,12)
         endif else begin
            posdate=strpos(fb,'ENV_')
            procdate=strmid(fb,posdate+4,12)
            datein=strmid(fb,posdate+19,12)
            
         endelse ;pos3
         
      endif                     ;aatsr
      if inst eq 'atsr2' then begin
         pos3=strpos(fb,'ERS2_')
         if pos3 gt 0 then begin
            posdate=strpos(fb,'ERS2_')
            datein=strmid(fb,posdate+5,12)
         endif 
      endif
      
      
      if inst eq 'modis' then begin
                                ;datein=strmid(fb,0,12)
         datein=strmid(fb,41,12)
         
      endif                     ;modis
      if inst eq 'avhrr' then begin
         datein=strmid(fb,112,12)
      endif                     ;modis
      timea= dblarr( nretgood)
      if keyword_set(i37) then s_temp.mwl=[0.645832 ,    0.856874  ,    3.7   ,   11.0263 ,     12.0424]
      if ~keyword_set(nosec) then begin
         ha_temp={FILE:fdir+'/'+fb,OFILE:fdir+'/'+fb,PROC_DATE:datein,DATA_DATE:datein,S:s_temp,SV:sv_temp,U:xs.SCANLINE_U[igood],V:xs.SCANLINE_V[igood],TIME:timea[igood],SG:sg_temp,DIA:2,TYPES:'LIQUID',LUTRE_RANGE:dblarr(2,2),LOPD_RANGE:dblarr(2,2),NSTR:0,RSA:0,BAUM:0,SPI:1,YE:fltarr(nmeas),CTEST:-1,IDC:0,MAX_IT:20,SYI:0,EXTRA:0.0,HX:0,VERSION:3.18,NX:nstate,NY:nmeas,npix:nretgood,NST:2}  
      endif else begin
         ha_temp={FILE:fdir+'/'+fb,OFILE:fdir+'/'+fb,PROC_DATE:datein,DATA_DATE:datein,S:s_temp,SV:sv_temp,TIME:timea[igood],SG:sg_temp,DIA:2,TYPES:'LIQUID',LUTRE_RANGE:dblarr(2,2),LOPD_RANGE:dblarr(2,2),NSTR:0,RSA:0,BAUM:0,SPI:1,YE:fltarr(nmeas),CTEST:-1,IDC:0,MAX_IT:20,SYI:0,EXTRA:0.0,HX:0,VERSION:3.18,NX:nstate,NY:nmeas,npix:nretgood,NST:2}  
      endelse

      
      latr=range(xin.lat)
      lonr=range(xin.lon)


      ha_temp.sg.LATR =latr 
      ha_temp.sg.LONR =lonr
  

      if n_elements(xs) gt 0 and ~keyword_set(nosec) then begin 
         xu=range(xs.SCANLINE_U[igood])
         xv=range(xs.SCANLINE_V[igood])
         ha_temp.sg.UG  =   [xu(0)+1,xu(1)+1,1] 
         ha_temp.sg.VG  =  [xv(0)+1,xv(1)+1,1]  
      endif else begin
         if ~keyword_set(nosec) then  begin
            xu=range(xs.SCANLINE_U[igood])
            xv=range(xs.SCANLINE_V[igood])
            ha_temp.sg.UG  =   [xu(0)+1,xu(1)+1,1] 
            ha_temp.sg.VG  =  [xv(0)+1,xv(1)+1,1]
         endif else begin

         endelse
      endelse


      ha_temp.sg.LAT=xin.lat
      ha_temp.time=xp.time[igood]
      ha_temp.sg.LON=xin.lon

;
;now call the plotting routine
;


psfi=strcompress(psfi,/remove_all)
      if n_elements(plot) gt 0 then begin
         plot_ret_cldmodel_modis,ret,ha_temp,_EXTRA=extra,clon=clon,clat=clat,eop_y=eop_y,eop_x=eop_x,ps=ps,night=night,itype=itype,/noqc,/rotate,nosec=nosec,error=error,fips=psfi,fatplot=fatplot,istomina=istomina,v1=v1,inst=inst,aer=aer,noir=noir ;error=error
      endif

endloop:
      
      dataout={ret:ret,ha:ha_temp}
      

      print,'here skipend adim i'
   endfor ;nchunks
skipend:
endfor

                          ;loop over adim
   
   
   
   return,dataout
   
   
end
