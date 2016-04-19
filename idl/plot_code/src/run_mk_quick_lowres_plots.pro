;
;This routine genrates cloud CCI quicklooks
;



pro run_mk_quick_lowres_plots,atsr2=atsr2,dirin=dirin

version='fv2.0'
year=['2003','2002','2004','2005','2006','2007','2008','2009','2010','2011','2012']
if keyword_set(atsr2) then year=['1995','1996','1997','1998','1999','2000','2001','2002','2003']
month=i2s(indgen(12)+1,2)
day=i2s(indgen(31)+1,2)
;for testing
year='2008',
month='01'
day='01'
if ~keyword_set(dirin) then dirin='/group_workspaces/cems/cloud_ecv/public/'
inst='AATSR'
if keyword_set(atsr2) then inst='ATSR2'
nyear=n_elements(year)
nmonth=n_elements(month)
for y=0,nyear-1 do begin
   
   for m=0,nmonth-1 do begin
      for d=0,nday-1 do begin
         
         file1= dirin+year(y)+'/lv2/'+month(m)+'/'+day(d)+'/*'+inst+'ATSR2*'+version+'.prim*.nc'
         
         filein=file_search(file1, count=adim)
         for i=0, adim-1 do begin
            mk_quick_lowres_plots,filein=filein(i),/ps
         endfor
      endfor
      
   endfor
endfor

end
