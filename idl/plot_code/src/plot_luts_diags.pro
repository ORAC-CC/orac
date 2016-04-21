;
; This routine plots the nakajima king plot for designated LUTs!
; keywords:
; ps : write to pdf file
; avhrr : read luts from avhrr instrument otherwise defaults to atsr
; saddir : directory of lut files to read
; inst : instrument name e.g. AVHRR-NOAA18
; isat: index of satellite zenith
; isol: index of solar zenith
; iphi: index of relative azimuth
; outdir: output directory for pdf

;how to run
;idl>plot_luts_diags,/avhrr,/ps

pro plot_luts_diags,ps=ps,avhrr=avhrr,saddir=saddir,inst=inst,isat=isat,isol=isol,iphi=iphi,outdir=outdir

; xice= rd_cldmodel_luts(/ice,inst='AVHRR-NOAA18')      
; xwat= rd_cldmodel_luts(/ice,inst='AVHRR-NOAA18')      

  if ~keyword_set(avhrr) then begin
; default to aatsr
     idi='~/Temp/orac/sad_dir/'
     xice= rd_cldmodel_luts(/ice)      
     xwat= rd_cldmodel_luts(/wat)    
     inst='AATSR'
  endif else begin
     
     if ~keyword_set(inst) then inst= 'AVHRR-NOAA18'
     if ~keyword_set(saddir) then begin
        idi='/misc/oxford1/rsg/Data/projects/ecv_clouds/traq_luts/sad_dir/'
     endif else begin
        idi=saddir
     endelse

     xice= rd_cldmodel_luts(/ice,inst=inst,idi=idi,avhrr=avhrr)      
     xwat= rd_cldmodel_luts(/wat,inst=inst,idi=idi,avhrr=avhrr)   

  endelse  

;ndvi= (nir-vis)/(nir_vis)
ilopd=12
;ichanvis=1
;ichannir=

;set default angulat geometry
;ire=4
if ~keyword_set(isat) then isat=2
if ~keyword_set(isol) then isol=3
if ~keyword_set(iphi) then iphi=6
;print,x.solar.wl

if strpos(inst,'VHRR') gt 0  then nir_index=3
if inst eq 'AATSR' then nir_index=4 ;3.7
;nir_index=3 ;1.6

nir_ice=fltarr(n_elements(xice.solar.LOPD),n_elements(xice.solar.lutre))
nir_wat=fltarr(n_elements(xwat.solar.LOPD),n_elements(xwat.solar.lutre))


ndvi_ice=fltarr(n_elements(xice.solar.LOPD),n_elements(xice.solar.lutre))
ndvi_wat=fltarr(n_elements(xwat.solar.LOPD),n_elements(xwat.solar.lutre))

nvis_ice=fltarr(n_elements(xice.solar.LOPD),n_elements(xice.solar.lutre))
nvis_wat=fltarr(n_elements(xwat.solar.LOPD),n_elements(xwat.solar.lutre))

;loop over optical depth
for ilopd=0,n_elements(xice.solar.lopd)-1 do begin


nir_ice(ilopd,*)= xice.solar.rbd[nir_index, ilopd, *, isat, isol,iphi]
nvis_ice(ilopd,*)=xice.solar.rbd[1, ilopd, *, isat, isol,iphi]

endfor


;loop over optical depth
for ilopd=0,n_elements(xwat.solar.lopd)-1 do begin
nir_wat(ilopd,*)= xwat.solar.rbd[nir_index, ilopd, *, isat, isol,iphi]
nvis_wat(ilopd,*)=xwat.solar.rbd[1, ilopd, *, isat, isol,iphi]

endfor



ndvi_wat=(nir_wat-nvis_wat)/(nir_wat+nvis_wat)
if ~keyword_set(outdir) then outdir='~/Temp/'
ndvi_ice=(nir_ice-nvis_ice)/(nir_ice+nvis_ice)
psfi=outdir+'orac_luts_diags'+inst
if keyword_set(inst) then psfi=psfi+inst
if nir_index eq 4 then psfi=psfi+'_37'
if nir_index ne 4 then psfi=psfi+'_16'

if  strpos(inst,'VHRR') gt 0 and nir_index eq 3 then psfi=psfi+'_37'
if  strpos(inst,'VHRR') gt 0 and nir_index ne 3 then psfi=psfi+'_16'

psfi=psfi+'.ps'
if keyword_set(ps) then ps_open,psfi

set_a4,/rs,ro=2

nstart=7
for i=nstart,n_elements(xice.solar.lopd)-1 do begin
if i eq nstart then begin
plot,xice.solar.LUTRE,ndvi_ice(i,*),xtitle='effective radius', ytitle='NDVI',th=3,yrange=[-1,1],title='RBD'
endif

if i gt nstart then begin
oplot,xice.solar.LUTRE,ndvi_ice(i,*),th=3,col=i*10

endif
endfor

for i=nstart,n_elements(xwat.solar.lopd)-1 do begin
oplot,xwat.solar.LUTRE,ndvi_wat(i,*),col=i*10,th=3,line=1
endfor


lstr=['ICE', 'WAT']
col=[1,1]
lstys=[0,1]
p_label,lstr,quad=2,col=col,charsize=1,thick=3,psyms=4,linestyles=lstys

print,'sat',xwat.solar.sat(isat)
print,'sol',xwat.solar.sol(isol)
print,'phi',xwat.solar.phi(iphi)

nopd_wat=n_elements(xwat.solar.rbd[1, *, 0, isat, isol,iphi])
nre_wat=n_elements(xwat.solar.rbd[1, 0, *, isat, isol,iphi])


nopd_ice=n_elements(xice.solar.rbd[1, *, 0, isat, isol,iphi])
nre_ice=n_elements(xice.solar.rbd[1, 0, *, isat, isol,iphi])

any_key
;najaima and king plot
chanin=1
if nir_index eq 4 then chanin=3
if strpos(inst,'VHRR') gt 0 and nir_index eq 3 then chanin=3

	set_a4,/rs,ro=2
nk_ref_grid, xwat.solar.rbd[1, 1:nopd_wat-1,  1:nre_wat-1, isat, isol,iphi], xwat.solar.rbd[nir_index, 1:nopd_wat-1,  1:nre_wat-1, isat, isol,iphi], xwat.solar.lopd[1:nopd_wat-1],xwat.solar.lutre[ 1:nre_wat-1], chanin, 'WATER', ad=ad, overplot=overplot, col = col, psym=psym,/label

nk_ref_grid, xice.solar.rbd[1, *, *, isat, isol,iphi], xice.solar.rbd[nir_index, *, *, isat, isol,iphi], xice.solar.lopd,xice.solar.lutre, chanin, 'ICE', ad=ad, col = 80, psym=psym,/label



;plot rbd
any_key
set_a4,/rs,ro=2,co=2
mask=[[0,1],[2,3]]

 p1=[0.02,0.02,0.99,0.93]
	 p2=[0.1,0.1,0.98,0.96]
quick_cc,xwat.solar.rbd[3, *, *, isat, isol,iphi],10^xwat.solar.lopd,xwat.solar.LUTRE,xtitle='opd',ytitle='RE',title='WAT 1.6',chars=1,range=[0,60],position=ypos(p1,p2,mask=mask),dtitle='RBD'


quick_cc,xice.solar.rbd[3, *, 0:9, isat, isol,iphi],10^xice.solar.lopd,xice.solar.LUTRE[0:9],xtitle='opd',ytitle='RE',title='ICE 1.6',chars=1,range=[0,60],position=ypos(p1,p2,mask=mask),dtitle='RBD'


quick_cc,xwat.solar.rbd[1, *, *, isat, isol,iphi],10^xwat.solar.lopd,xwat.solar.LUTRE,xtitle='opd',ytitle='RE',title='WAT .67',chars=1,range=[0,60],position=ypos(p1,p2,mask=mask),dtitle='RBD'


quick_cc,xice.solar.rbd[1, *, 0:9, isat, isol,iphi],10^xice.solar.lopd,xice.solar.LUTRE[0:9],xtitle='opd',ytitle='RE',title='ICE .67',chars=1,range=[0,60],position=ypos(p1,p2,mask=mask),dtitle='RBD'


any_key

quick_cc,xice.solar.rbd[3, *, *, isat, isol,iphi],10^xice.solar.lopd,xice.solar.LUTRE,xtitle='opd',ytitle='RE',title='ICE 1.6',chars=1,range=[0,60],position=ypos(p1,p2,mask=mask),dtitle='RBD'

if keyword_set(ps) then ps_close,/pdf

end
