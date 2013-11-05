;==========================================================================
;+
;	pro RENAME_ORAC
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
;	C.Poulsen : 02 November 2011
; $Id$
;-
;==========================================================================
pro rename_orac,ice=ice,wat=wat,ret=ret

 fileout='mk_cloud_sim_data_ice.sav'
 ret=rd_sav(fileout)
ret=zero_struct(ret)
if n_elements(ice) eq 0 then ice=wat
if n_elements(wat) eq 0 then wat=ice

;
;read in above and refill it
;
nph=2
nmeas=5
nstate=5

for p=0,nph-1 do begin
        if p eq 0 then x=wat
        if p eq 1 then x=ice
        if p eq 0 then pho=1.0
        if p eq 1 then pho=1.0
        igood=where(wat.meas(*,0) ge 0,ngood)
        ngood=n_elements(wat.conv)
        ret(p,*).y(0:nmeas-1)=reform(transpose(x.meas(igood,*)),nmeas,1,ngood)
        ret(p,*).Y0(0:nmeas-1)=reform(transpose(x.y0(igood,*)),nmeas,1,ngood)
        ret(p,*).YN(0:nmeas-1)=reform(transpose((x.ymfit(igood,*)+x.meas(igood,*))),nmeas,1,ngood)
        ret(p,*).XN=reform(transpose(x.state(igood,*)),nstate,1,ngood)
 
        for i=0,nstate-1 do begin
           ret(p,*).sx(i,i)=reform(transpose(x.uncertainty(igood,i)),1,ngood)
        endfor
       ret(p,*).COST=reform(x.cost(igood),1,ngood) 
        ret(p,*).CONV=reform(x.conv(igood),1,ngood) 
        ret(p,*).TC = reform(x.ctt(igood),1,ngood) 
        ret(p,*).ITYPE=p
        ret(p,*).ZC= reform(x.cth(igood),1,ngood)
        ret(p,*).NI=   reform(x.iterations(igood),1,ngood) 
        ret(p,*).WHITE_SKY_ALBEDO=ret(p,*).WHITE_SKY_ALBEDO;reform(transpose(x.albedo(igood,index_chans_ref)),nchans_ref,1,ngood)
        ret(p,*).CLEARSKY_BT= 0.0;reform(x.meas(igood,4)*0.0,1,ngood) 
        ret(p,*).X0 =  reform(transpose(x.x0(igood,*)),nstate,1,ngood)
        ret(p,*).ae =  reform(transpose(x.ae(igood,*)),nstate,1,ngood)
;        ret(p,*).XE=  reform(transpose(x.uncertainty(igood,*)),nstate,1,ngood)  
        ret(p,*).COLUMN_DENSITY_ERROR=reform(4/3*((x.state(igood,0)*x.state(igood,1)^3)/pho),1,ngood)

endfor ;phase



end
