;==========================================================================
;+
;	pro CREATE_H_SAV
;
;	Description
;       create a dummy h file
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
;	C.Poulsen : 17 April 2013
; $Id$
;-
;==========================================================================
function create_h_sav

temp1=fltarr(416)
temp2=intarr(3)
mwla=[0.645832 ,    0.856874   ,   1.62808  ,    11.0263  ,    12.0424]
s={NAME:'Band01',INST:'MODIS-AQUA',MWL:mwla,I0:6654.7548,MODTRAN:2,MO3XSEC:2.6825364e-21,MH2OXSEC:1.0689331e-25,MCO2XSEC:0.0000000,MCH4XSEC:0.0000000,MN2OXSEC:0.0000000,MSO2XSEC:0.0000000,MO3TC:9.0209256e+18,MH2OTC:9.9668842e+22,MCO2TC:7.1343327e+21,MCH4TC:3.4154408e+19,MN2OTC:6.3939793e+18,MSO2TC:2.9310859e+15,X:0,HSREMIS_PCU:temp1,HSREMIS_PCM:0.0000000,SOLAR_FLAG:1,THERMAL_FLAG:0,IMODIS:2,YE:0.0049998998,YE_COREG:2.0000000,YE_HOMOG:0.75000000,RTTOV_ID:temp1,RTTOV_SOLAR:0,ST:0,IVIEW:indgen(5)*0} 
;RTTOV:STRUCT-><Anonymous>Array[1)



lcot={NX:1,I0:0,I1:0,AP:-1.0000000,AE:1000.0000,FG:-1.0000000,SHORT_NAME:'LCOT',LONG_NAME:'Log10(OpticalDepth)',UNIT:'-'}
srf={NX:1,I0:4,I1:4,AP:1.0000000,AE:0.10000000,FG:1.0000000,SHORT_NAME:'SRFC',LONG_NAME:'Surfacereflectancescalingfactor',UNIT:'-'}

so2={NX:0,AP:1.00000,AE:-999.00000,FG:-999.00000,SHORT_NAME:'SO2',LONG_NAME:'ColumnSO2',UNIT:'DU'}

frac={NX:1,I0:4,I1:4,AP:1.00000,AE:-999.00000,FG:-999.00000,SHORT_NAME:'F',LONG_NAME:'Areafraction',UNIT:'-'}

cre={NX:1,I0:1,I1:1,AP:10.000000,AE:1000.0000,FG:10.000000,SHORT_NAME:'RE',LONG_NAME:'Effectiveradius',UNIT:'microns'}


ts={NX:1,I0:3,I1:3,AP:-999.00000,AE:1.0000000,FG:-999.00000,SHORT_NAME:'TS',LONG_NAME:'Surfacetemperature',UNIT:'K',AE_LAND:6.00000}

pc={NX:1,I0:2,I1:2,AP:4.8164799,AE:10.000000,FG:4.8164799,SHORT_NAME:'PC',LONG_NAME:'Cloudpressure',UNIT:'Z*km'}


svx={LCOT:lcot   ,CRE: cre       ,PC:     pc     ,TS:     ts     ,FRAC:  frac   ,SO2:   so2     ,SRF:srf}

;   X:               STRUCT    -> <Anonymous> Array(1)


sv={L2:0,TWORE:0,SRF:1,CSRF:0,ZSTAR:1,IRET:BYTArr(7),WRET:indgen(5),NRET:4,NX:LONArr(7),N_TOTAL:5,ID:'1111001',UNDEFINED:-999.00000,X:svx}
 latr=[-90.0,      90.0]
 lonr=[-180.0   ,  180.0]
sg={LATR:latr,LONR:lonr,UG:INTArr(3),VG:INTArr(3),LAT:FLtArr(451690),LON:FLtArr(451690),ID:'modis-scan',SAT:1,NEQ:0,LL:0,SD:0}


x={FILE:'/home/cluster/cpoulsen/Data/projects/ecv_clouds/modis_data_rr/2008/03/20/MYD021KM.A2008080.0035.005.2009',OFILE:'$CLDMODEL_PATH/out/2008/03/20/_MYD021KM.A2008080.0035.005.2009312180933_380-750_gv3_spi_1111001_v3p15.st',PROC_DATE:'20110717195431',DATA_DATE:'200803200035',S:s,SV:sv,U:LONarr(451690),V:LONarr(451690),TIME:DblArr(451690),SG:sg,DIA:2,TYPES:STRArr(2),LUTRE_RANGE:DBLArr(2,2),LOPD_RANGE:DblArr(2,2),NSTR:64,RSA:0,BAUM:0,SPI:1,YE:FLtArr(5),CTEST:-1,IDC:0,MAX_IT:20,SYI:0,HX:0,VERSION:3.15000,NX:5,NY:5,NPIX:451690,NST:2}

;,_EXTRA:STRUCT-><Anonymous>Array(1)

return,x
end

