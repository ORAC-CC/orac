;==========================================================================
;+
;	pro BAD_FILE
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
;	History
;
;	Bugs
;
;	C.Poulsen : 22 May 2014
; $Id$
;-
;==========================================================================

;bad_file,/all
;bad_file,/wat
;bad_file,/ice
;bad_file,/cems
;bad_file,/sec
;

pro bad_file,wat=wat,ice=ice,all=all,notr=notr,sec=sec,cems=cems
if not keyword_set(notr) then begin
     catch,Error_status
     IF Error_status NE 0 THEN BEGIN
        PRINT, 'Error index: ', Error_status
        PRINT, 'Error message:', !ERR_STRING ;
        CATCH, /CANCEL
        close,/all
        heap_gc
        return
     ENDIF
 endif
version='V20'
dirin='/misc/wantage_static/cpoulsen/cloud_ecv/postproc//'


if keyword_set(night) then version='V23'


year=['2008','2007','2009']
month=i2s(indgen(12)+1,2)
year=['2008'];,'2009']
month=['01','06']
month=['01']
month=['03','09','06','12']


nyear=n_elements(year)
nmonth=n_elements(month)
for y=0,nyear-1 do begin

if keyword_set(wat) then nn='WAT'
if keyword_set(ice) then nn='ICE'
if keyword_set(all) then nn='ALL'
if keyword_set(sec) then nn='SEC'
if keyword_set(cems) then nn='cems'



    for m=0,nmonth-1 do begin
fileout='bad_file'+i2s(year(y))+i2s(month(m))+nn+'.txt'
openw,1,fileout

print,year(y),month(m)
day=i2s(indgen(31)+1,2)

       if month(m) eq '02' then day=i2s(indgen(28)+1,2)
       if month(m) eq '09' or month(m) eq '04' or month(m) eq '06' or month(m) eq '11' then day=i2s(indgen(30)+1,2)
;day=['01','06','10']
       nday=n_elements(day)


;      for d=0,nday-1 do begin
      for d=0,nday-1 do begin

if keyword_set(wat) then ext='*'+version+'*WAT*prim*.nc'
if keyword_set(ice) then ext='*'+version+'*ICE*prim*.nc'
if keyword_set(all) then ext='*'+version+'*PP*prim*.nc'
if keyword_set(sec) then ext='*'+version+'*sec*.nc'
          file=file_search(dirin+'/'+year(y)+'/'+month(m)+'/'+day(d)+'/'+ext,count=adim)

          print,'adim',adim
          for i=0,adim-1 do begin
print,file(i)

if ~keyword_set(sec) then  x=ncdf_get(file(i),'ctt')
if ~keyword_set(sec) and keyword_set(night) then  x=ncdf_get(file(i),'ctt')
if keyword_set(sec) then  x=ncdf_get(file(i),'ctt')
              szx=size(x)

              if szx(2) eq 1 then printf,1,'rm -f '+file(i)
              if n_tags(x) eq 0 then printf,1,'rm -f '+file(i)
              if n_tags(x) eq 0 then print,'n_tags',n_tags(x)
;              if szx(2) eq 1 then spawn,'rm -f '+file(i)
              if szx(2) eq 1 then goto,skip
              rr=range(x.value)

              print,'range phase',rr,year(y)+'/'+month(m)+'/'+day(d)
;if keyword_set(wat) and rr(1) ne 1 then stop
;if keyword_set(ice) and rr(1) ne 1 then stop
;if keyword_set(all) and rr(1) ne 2 then stop

if keyword_set(all) then begin
 x=ncdf_get(file(i),'phase')
whwat=where(x.value eq 1 ,nwat)
whice=where(x.value eq 2,nice)

if nice eq 0 or nwat eq 0 then rr(1) = 0
print,'wat ice ',nwat,nice
endif

              if keyword_set(jobid) and rr(1) le 0  then begin
;work out jobid
                  
                  str1=strmid(file_basename(file(i)),66,12)
                  print,'str1',str1

                  filein='/work/ralspace/rsg_group/cloud_ecv/jobs/file_names//aatsr_files_'+year+month(m)+day(d)+'.txt'
                  openr,2,filein
                  readf,2,norbits
                  filex=''
                  for n=0,norbits-1 do begin
                      readf,2,filex
str2=strmid(filex,14,8)
str3=strmid(filex,23,4)
;print,str3
str4=str2+str3
                   print,'str4',str4

                      if str1 eq str4 then begin
                          jobids=n
                      printf,1,'job id',n+1
print,'job id',n+1

                      endif
                  endfor
                  close,2
              endif

              if rr(1) le 0 then printf,1,'rm -f '+file(i)
;              if rr(1) le 0 then spawn,'rm -f '+file(i)
              skip:
          endfor
          

      endfor
close,1
  endfor

endfor


end


end
