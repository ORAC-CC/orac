;===============================================================================
;+
; RD_CLDMODEL_LUTS
;
; Read RT LUTs used in cloud model FM
;
; PARAMETERS
;	
;
; KEYWORDS
;	ICE	Read ICE LUT instead of water
;
; R.S. 09/10/08
;-
;===============================================================================
function rd_cldmodel_luts,ice=ice,inst=inst,idi=idi,wat=wat,avhrr=avhrr
;	idi='~blatter/Data/cloud-model/luts/'
;        idi='/home/cluster/cpoulsen/orac_svn/sad/'
        if ~keyword_set(idi) then idi='~/Temp/orac/sad_dir/'
	if keyword_set(ice) then type='ICE' else type='WAT'
        if ~keyword_set(inst) then inst='AATSR'
if inst eq 'AVHRR-NOAA18' then begin
	tirchans=['3b','4','5']
	solchans=['1','2','3a','3b']

	solchans=[0,1,2,3]
	tirchans=[3,4,5]
     endif

if inst eq 'AATSR'   then begin
	tirchans=[5,6,7]
	solchans=[1,2,3,4,5]
     endif

;        inst='MODIS-AQUA'

        ;inst='ATSR-2'
	xt=rd_sad_cp(idi=idi,fx=0,ao=type,ch=tirchans,inst=inst,/tir,avhrr=avhrr)
	xs=rd_sad_cp(idi=idi,fx=0,ao=type,ch=solchans,inst=inst,/nobex,avhrr=avhrr)
	return,{solar:xs,irrd:xt}
	;return,{solar:xs}
end
