;==========================================================================
;+
;	pro PLOT_FORTRAN_SIMULATIONS
;
;	Description
; 
; 
;	Use
; 
;	Parameters
;      

;	Keywords
;       
;    filein: input file to plot must have the .out extension
;    extra_name:
;    ps: out put file to pdf file
;    ice: run for ice clouds
;    inst : specify instrument;
;    outdir: outdir directory to output ps file to
;	History
;
;	Bugs
;
;	C.Poulsen : 25 November 2011
; $Id$
;-
;==========================================================================
pro plot_fortran_simulations,filein=filein,extra_name=extra_name,ps=ps,ice=ice,inst=inst,outdir=outdir

;if n_elements(inst) eq 0 then inst ='AATSR'
if n_elements(inst) eq 0 then inst ='MODIS'
if n_elements(extra_name) eq 0 then extra_name =''
if n_elements(filein) eq 0 then filein='/disks/scratch/cpoulsen/tsf_MODIS_WAT_test_rts_14_14_30_0_0_0_780.primary.nc'
if n_elements(outdir) eq 0 then outdir='~/Temp/orac/plots/'

fortran=1

lopdh=alog10([0.01,0.5,1,3,5,7,10,12,15,20,25,30,50,100])
if keyword_set(ice) and ~keyword_set(ml) then cre=[3.,5,8,10,12,15,17,20,25,30,40,50,60,70] 
if ~keyword_set(ice)  and ~keyword_set(ml) then cre=[1.,3,5,7,9,10,13,15,17,19,21,23,25,27] 
nre=n_elements(cre)
lza=[0.] 
sza=[30.] 
raz=[0.]
pc=[780.]
no2=n_elements(lopdh)
nsza=n_elements(sza)
nlza=n_elements(lza)
nraz=n_elements(raz)
npc=n_elements(pc)
nsurf=1
;
;create the true varable array
;
 true_par=fltarr(nre,no2,5,nsza,nlza,nraz,npc,nsurf)
 for ire=0,nre-1 do for ih=0,no2-1 do begin
     for isza=0,nsza-1 do begin
         for ilza=0,nlza-1 do begin
             for iraz=0,nraz-1 do begin
                 for ipc=0,npc-1 do begin
                     for isurf=0,nsurf-1 do begin
                         
                         
                         true_par(ire,ih,0,isza,ilza,iraz,ipc,isurf)=lopdh(ih)
                         true_par(ire,ih,1,isza,ilza,iraz,ipc,isurf)=cre(ire)
                         true_par(ire,ih,2,isza,ilza,iraz,ipc,isurf)=pc(0)
                         true_par(ire,ih,3,isza,ilza,iraz,ipc,isurf)=270.7

                         
                     endfor ;isurf 
                 endfor ;ipc                        
             endfor ;iraz                     
         endfor ;ilza             
     endfor ;         isza     
 endfor ;            ire opd
 


ret=read_orac_nc(filein=filein)
;
;if keyword_set(ice) then ice=x
;if ~keyword_set(ice) then wat=x

nx=sqrt(n_elements(ret(0,*).lat))
ny=nx


 new_title=['!6 Cloud optical depth','!6 Effective radius '+special_char(/mu)+'m',$
             '!6 Cloud top pressure HPa','!6 Surface temperature K'];,'!6 Cloud fraction','!6 Cloud water path']

psfi=outdir+'test_sim_plots_v2'+extra_name
psfi=psfi+'_'+inst
if keyword_set(fortran) then psfi=psfi+'_fortran'
psfi=psfi+'.ps'
if keyword_set(ps) then ps_open,psfi
set_a4,/rs,ro=2,co=4,/la

mask=[[0,1, 2,3],[4,5,6,7]]
p1=[0.02,0.01,0.99,0.99]
p2=[0.1,0.27,0.9,0.95]

newline=string(10b)
yti='!C !C !6 Effective radius '

xti=' !C !C!6 Cloud optical depth '
npar=n_elements(new_title)
chs=1.5

xlabn=trim_zero(sig_figs(((10^float(lopdh))),2))
ylabn=trim_zero(sig_figs(((float(cre))),2))
for i=0,npar-1 do begin
    
    d=transpose(reform(ret(0,*).xn(i),ny,nx))
    if i eq 0 then cblog=1
    if i eq 0 then begin
        if i eq 0 then nrange=[0.001,100.] 
        
        cc_box,d,xlab=xlabn,ylab=ylabn,_EXTRA=extra,$
          xtitle=xti,ytitle=yti,$
          chars=chs*0.95,labsi=chs*0.75,$
          crd=0.25,plg=plg,pchar=chs*0.75,$
          nlg=4,/xst,/yst,/bcb,/noname,$
          position=ypos(p1,p2,mask=mask),brd=[-0.1,-.05],dtitle=new_title(i),/cblog,minv=.001
        
        
        cc_box,transpose(true_par(*,*,i)),xlab=xlabn,ylab=ylabn,_EXTRA=extra,$
          xtitle=xti,ytitle=yti,$
          chars=chs*0.95,labsi=chs*0.75,$
          crd=0.25,plg=plg,pchar=chs*0.75,$
          nlg=4,/xst,/yst,/bcb,/noname,$
          position=ypos(p1,p2,mask=mask),brd=[-0.1,-.05],dtitle='True '+new_title(i),/cblog,minv=.001 ;,range=nrange
   endif else begin
       
       if i eq 2 then nrange=[500,1000] 
       if i eq 1 then nrange=[0,30] 
       if i eq 3 then nrange=[268,272] 
       cc_box,d,xlab=xlabn,ylab=ylabn,_EXTRA=extra,$
             xtitle=xti,ytitle=yti,$
             chars=chs*0.95,labsi=chs*0.75,$
             crd=0.25,plg=plg,pchar=chs*0.75,$
             nlg=4,/xst,/yst,/bcb,/noname,$
             position=ypos(p1,p2,mask=mask),brd=[-0.1,-.05],dtitle=new_title(i),range=nrange
      
      
      cc_box,transpose(true_par(*,*,i)),xlab=xlabn,ylab=ylabn,_EXTRA=extra,$
             xtitle=xti,ytitle=yti,$
             chars=chs*0.95,labsi=chs*0.75,$
             crd=0.25,plg=plg,pchar=chs*0.75,$
             nlg=4,/xst,/yst,/bcb,/noname,$
             position=ypos(p1,p2,mask=mask),brd=[-0.1,-.05],dtitle='True '+new_title(i),range=nrange
   endelse
   
;
;difference to truth
;
   if i eq 0 then diff=10^reform(ret(0,*).xn(i),ny,nx)-10^true_par(*,*,i)
   if i ne 0 then diff=reform(ret(0,*).xn(i),ny,nx)-true_par(*,*,i)
   if i eq 0 then diffrange=[-10,40] 
    if i eq 1 then diffrange=[-20,20] 
   if i eq 2 then diffrange=[-150,150] 
   if i eq 3 then diffrange=[-2,2] 
  
   cc_box,transpose(diff),xlab=xlabn,ylab=ylabn,_EXTRA=extra,$
          xtitle=xti,ytitle=yti,$
          chars=chs*0.95,labsi=chs*0.75,$
          crd=0.25,plg=plg,pchar=chs*0.75,$
          nlg=4,/xst,/yst,/bcb,/noname,$
          position=ypos(p1,p2,mask=mask),brd=[-0.1,-.05],dtitle='diff with true '+new_title(i),range=diffrange
;percentage diff

   if i eq 0 then diff=((10^reform(ret(0,*).xn(i),ny,nx)-10^true_par(*,*,i))/10^true_par(*,*,i))*100.0
   if i ne 0 then diff=((reform(ret(0,*).xn(i),ny,nx)-true_par(*,*,i))/true_par(*,*,i))*100.0
   if i eq 0 then diffrange=[-20,20] 
    if i eq 1 then diffrange=[-30,30] 
   if i eq 2 then diffrange=[-20,20] 
   if i eq 3 then diffrange=[-0.8,0.8] 
  
   cc_box,transpose(diff),xlab=xlabn,ylab=ylabn,_EXTRA=extra,$
          xtitle=xti,ytitle=yti,$
          chars=chs*0.95,labsi=chs*0.75,$
          crd=0.25,plg=plg,pchar=chs*0.75,$
          nlg=4,/xst,/yst,/bcb,/noname,$
          position=ypos(p1,p2,mask=mask),brd=[-0.1,-.05],dtitle='% diff with true '+new_title(i),range=diffrange


any_key
endfor ;npar

set_a4,/rs,ro=2,co=3,/la
new_titlex0=new_title+' x0'

for i=0,npar-1 do begin

   d=transpose(reform(ret(0,*).x0(i),ny,nx))



   if i eq 0 then cblog=1
   if i eq 0 then begin

 if i eq 0 then nrange=[0.001,100.] 
 d=transpose(10^reform(ret(0,*).x0(i),ny,nx))
      cc_box,d,xlab=xlabn,ylab=ylabn,_EXTRA=extra,$
             xtitle=xti,ytitle=yti,$
             chars=chs*0.95,labsi=chs*0.75,$
             crd=0.25,plg=plg,pchar=chs*0.75,$
             nlg=4,/xst,/yst,/bcb,/noname,$
             position=ypos(p1,p2,mask=mask),$
             brd=[-0.1,-.05],dtitle=new_titlex0(i),$
             /cblog,minv=.001,title=trim_zero(d(0,0))
      
         endif else begin

   if i eq 2 then nrange=[500,1000] 
   if i eq 1 then nrange=[0,30] 

   if i eq 3 then nrange=[268,272] 
      cc_box,d,xlab=xlabn,ylab=ylabn,_EXTRA=extra,$
             xtitle=xti,ytitle=yti,$
             chars=chs*0.95,labsi=chs*0.75,$
             crd=0.25,plg=plg,pchar=chs*0.75,$
             nlg=4,/xst,/yst,/bcb,/noname,$
             position=ypos(p1,p2,mask=mask),brd=[-0.1,-.05],$
             dtitle=new_titlex0(i),range=nrange,title=trim_zero(d(0,0))
      
   endelse

endfor ;npar
   
any_key


;
;
;errors
;

set_a4,/rs,ro=2,co=3,/la
new_titlexe=new_title+' sx'

for i=0,npar-1 do begin

;d=(transpose(reform(ret(0,*).sx(i,i),ny,nx)))
d=(transpose(reform(ret(0,*).sx(i),ny,nx)))
 
if ~keyword_set(fortran) then d=sqrt(transpose(reform(ret(0,*).sx(i,i),ny,nx)))
   if i eq 0 then cblog=1
   if i eq 0 then begin

 if i eq 0 then nrange=[0.001,10.] 

      cc_box,d,xlab=xlabn,ylab=ylabn,_EXTRA=extra,$
             xtitle=xti,ytitle=yti,$
             chars=chs*0.95,labsi=chs*0.75,$
             crd=0.25,plg=plg,pchar=chs*0.75,$
             nlg=4,/xst,/yst,/bcb,/noname,$
             position=ypos(p1,p2,mask=mask),brd=[-0.1,-.05],dtitle=new_titlexe(i),range=nrange
      
         endif else begin

   if i eq 2 then nrange=[0,100] 
   if i eq 1 then nrange=[0,10] 

   if i eq 3 then nrange=[0,2] 
      cc_box,d,xlab=xlabn,ylab=ylabn,_EXTRA=extra,$
             xtitle=xti,ytitle=yti,$
             chars=chs*0.95,labsi=chs*0.75,$
             crd=0.25,plg=plg,pchar=chs*0.75,$
             nlg=4,/xst,/yst,/bcb,/noname,$
             position=ypos(p1,p2,mask=mask),brd=[-0.1,-.05],dtitle=new_title(i),range=nrange
      
   endelse

endfor ;npar

any_key


set_a4,/rs,ro=2,co=3,/la


;
;plot zero iteration
;
if keyword_set(fortran) then  nmeas=5


nref=3
for i=0,nmeas-1 do begin
   
   d=transpose(reform(ret(0,*).y(i),ny,nx))
   new_title=['0.67','0.87','1.6','11','12']
   if nmeas eq 6 then new_title=['0.55',new_title]
   if nmeas eq 6 then nref=4
   if nmeas eq 7 then new_title=['','0.55',new_title]
   
   if i le nref-1 then nrange=[0,1]
   if i gt nref-1 then nrange=[250,275] 
   
   cc_box,d,xlab=xlabn,ylab=ylabn,_EXTRA=extra,$
          xtitle=xti,ytitle=yti,$
          chars=chs*0.95,labsi=chs*0.75,$
          crd=0.25,plg=plg,pchar=chs*0.75,$
          nlg=4,/xst,/yst,/bcb,/noname,$
          position=ypos(p1,p2,mask=mask),brd=[-0.1,-.05],dtitle=new_title(i),range=nrange
   

endfor


nref=3


set_a4,/rs,ro=2,co=3,/la
nref=3

for i=0,npar-1 do begin
 new_title=['!6 Cloud optical depth','!6 Effective radius '+special_char(/mu)+'m',$
             '!6 Cloud top pressure HPa','!6 Surface temperature K'];,'!6 Cloud fraction','!6 Cloud water path']
new_title=new_title +' ae'

   d=transpose(reform(ret(0,*).ae(i),ny,nx))
if i lt 3 then d=d/100.0
      cc_box,d,xlab=xlabn,ylab=ylabn,_EXTRA=extra,$
             xtitle=xti,ytitle=yti,$
             chars=chs*0.95,labsi=chs*0.75,$
             crd=0.25,plg=plg,pchar=chs*0.75,$
             nlg=4,/xst,/yst,/bcb,/noname,$
             position=ypos(p1,p2,mask=mask),brd=[-0.1,-.05],$
             dtitle=new_title(i),range=range,title=trim_zero(d(0,0))
  endfor;npar



set_a4,/rs,ro=2,co=3,/la
for i=0,nmeas-1 do begin
  new_title=['fitted 0.67','fitted 0.87','fitted 1.6','fitted 11','fitted 12']
 if nmeas eq 6 then new_title=['fitted 0.55',new_title]
 if nmeas eq 7 then new_title=['','fitted 0.55',new_title]
   d=transpose(reform(ret(0,*).yn(i),ny,nx))
   if i le nref-1 then nrange=[0,1]
   if i gt nref-1 then nrange=[250,275] 
      cc_box,d,xlab=xlabn,ylab=ylabn,_EXTRA=extra,$
             xtitle=xti,ytitle=yti,$
             chars=chs*0.95,labsi=chs*0.75,$
             crd=0.25,plg=plg,pchar=chs*0.75,$
             nlg=4,/xst,/yst,/bcb,/noname,$
             position=ypos(p1,p2,mask=mask),brd=[-0.1,-.05],dtitle=new_title(i),range=nrange

endfor



set_a4,/rs,ro=2,co=3,/la
for i=0,nmeas-1 do begin
  new_title=['first guess 0.67','first guess 0.87','first guess 1.6','first guess 11','first guess 12']
 if nmeas eq 6 then new_title=['first guess 0.55',new_title]
 if nmeas eq 7 then new_title=['','first guess 0.55',new_title]
   d=transpose(reform(ret(0,*).y0(i),ny,nx))
   if i le nref-1 then nrange=[0,1]
   if i gt nref-1 then nrange=[250,275] 
      cc_box,d,xlab=xlabn,ylab=ylabn,_EXTRA=extra,$
             xtitle=xti,ytitle=yti,$
             chars=chs*0.95,labsi=chs*0.75,$
             crd=0.25,plg=plg,pchar=chs*0.75,$
             nlg=4,/xst,/yst,/bcb,/noname,$
             position=ypos(p1,p2,mask=mask),brd=[-0.1,-.05],$
             dtitle=new_title(i),range=nrange,title=trim_zero(d(0,0))

endfor


set_a4,/rs,ro=2,co=3,/la
for i=0,nmeas-1 do begin
  new_title=['residual 0.67','residual 0.87','residual 1.6','residual 11','residual 12']
 if nmeas eq 6 then new_title=['residual 0.55',new_title]
 if nmeas eq 7 then new_title=['','residual 0.55',new_title]
   d=transpose(reform(ret(0,*).y(i),ny,nx)-reform(ret(0,*).yn(i),ny,nx))
   if i le nref-1 then nrange=[-.02,.02]
   if i eq nref-1 then nrange=[-.03,.03]
   if i gt nref-1 then nrange=[-2,2] 
      cc_box,d,xlab=xlabn,ylab=ylabn,_EXTRA=extra,$
             xtitle=xti,ytitle=yti,$
             chars=chs*0.95,labsi=chs*0.75,$
             crd=0.25,plg=plg,pchar=chs*0.75,$
             nlg=4,/xst,/yst,/bcb,/noname,$
             position=ypos(p1,p2,mask=mask),brd=[-0.1,-.05],dtitle=new_title(i),range=nrange

endfor

any_key
set_a4,/rs,ro=2,co=3,/la
;
;cost
;
;
 d=transpose(reform(ret(0,*).cost,ny,nx))
cc_box,d,xlab=xlabn,ylab=ylabn,_EXTRA=extra,$
             xtitle=xti,ytitle=yti,$
             chars=chs*0.95,labsi=chs*0.75,$
             crd=0.25,plg=plg,pchar=chs*0.75,$
             nlg=4,/xst,/yst,/bcb,/noname,$
             position=ypos(p1,p2,mask=mask),brd=[-0.1,-.05],dtitle='cost',range=[0,20]

 d=transpose(reform(ret(0,*).conv,ny,nx))
cc_box,d,xlab=xlabn,ylab=ylabn,_EXTRA=extra,$
             xtitle=xti,ytitle=yti,$
             chars=chs*0.95,labsi=chs*0.75,$
             crd=0.25,plg=plg,pchar=chs*0.75,$
             nlg=4,/xst,/yst,/bcb,/noname,$
             position=ypos(p1,p2,mask=mask),brd=[-0.1,-.05],dtitle='conv',range=[-1,2]

d=transpose(reform(ret(0,*).ni,ny,nx))
cc_box,d,xlab=xlabn,ylab=ylabn,_EXTRA=extra,$
             xtitle=xti,ytitle=yti,$
             chars=chs*0.95,labsi=chs*0.75,$
             crd=0.25,plg=plg,pchar=chs*0.75,$
             nlg=4,/xst,/yst,/bcb,/noname,$
             position=ypos(p1,p2,mask=mask),brd=[-0.1,-.05],dtitle='iterations',range=[0,25]


any_key

if keyword_set(ps) then ps_close,/pdf


end
