;==========================================================================
;+
;	pro RUN_READ_ORAC_NC_PP
;
;	Description
; 
; 
;	Use
; 
;	Parameters
; 
; 
;	Keywords
;
;
;       typical usage
;run_read_orac_nc_pp,dirin='/misc/paris_ftp/pub/cloud_ecv/for_stefan/01/'
; run_read_orac_nc_pp,dirin='/misc/paris_ftp/pub/cloud_ecv/for_stefan/06/'
; run_read_orac_nc_pp,dirin='/misc/paris_ftp/pub/cloud_ecv/for_stefan/07/'
; run_read_orac_nc_pp,dirin='/misc/paris_ftp/pub/cloud_ecv/for_stefan/25/'
;
;	History
;
;	Bugs
;
;	C.Poulsen : 14 March 2013
; $Id$
;-
;==========================================================================
pro run_read_orac_nc_pp,dirin=dirin,nosec=nosec
;dirin='/misc/oxford1/rsg/Data/projects/ecv_clouds/test_output/scarf/2008/v4/'
if n_elements(dirin) eq 0 then dirin='/misc/paris_ftp/pub/cloud_ecv/for_stefan/06/'
filein='*_PP.prim*.nc' ;itype=3

print,'serach',dirin+filein
files=file_search(dirin+filein,count=adim)
print,'adim',adim

for i=0,adim-1 do begin
   
   fb=file_basename(files(i),'.primary.nc')
   fpdf=dirin+files(i)+'.pdf'
   check=file_search(fpdf,count=bdim)
   print,'bdim',bdim
   if bdim eq 0 then begin
      print,'files(i)',files(i)
      x=read_orac_nc_pp(filein=files(i),/plot,/ps,nosec=nosec)
   endif
endfor
end
