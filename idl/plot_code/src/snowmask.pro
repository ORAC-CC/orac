;==========================================================================
;+
;	pro SNOWMASK
;
;	Description
; Programe to create the mask from the paper www.atmos-meas-tech.net/3/1005/2010
; Masks to show cloud-free snow-covered areas by comparing the
;'Brightness Temperature' or Top Of Atmosphere Reflectance of the
; differnt AATSR chanels
; Colour number is accumilated by where statements, +0.2 for snow and
;	+0 for other,
; it is then plotted
; to give a range of certainly snow (1) to certainly cloud/other (0)
;	Use
; 
;	Parameters
; Input data from 3 TIR chanels:
; BT_37 for the 3.7 micrometer chanel
;	BT_108 for the 10.8 micrometer chanel
; BT_12 for the 12 micrometer chanel
; And the 4 VIS-NIR chanels:
; RTOA_87 for the 0.87 micrometer chanel
; RTOA_16 fir the 1.6 micrometer chanel
; RTOA_66 for teh 0.66 micrometer chanel
; RTOA_55 for the 0.55 micrometer chanel
; Colour number is the plotting parameter for the mask: col_num
;	Keywords
; I FILE_NAME File of data to be analysed
; O snow mask
;	FAST : If temporary files have been made for testing this uses them,
;	comented out at the moment
;	FILE : Inputs data files from plot_ret_cldmodel_cloud
;	DATAIN :  input the data rather than a file name for speed, this
;	from plot_ret_cldmodel_cloud is the BT and RTOA data only
;	DATAOUT : Output the colour number for plotting the istomina snowmask
;	CHANIN : Input array of which data is in each column of datain
;	CTHCH : Input cloud top height data
;	FLAG : Output the snowmask; the flag array
;	LANDDATA : Input the land/sea map
;	AC1 and AC2 : Input the albedo data for channels 1 and 2, use uncorrected
;	TRIAL : Outputs the trial array for checking that the mask works
;	without visible channels, i.e. at night
;	OPTIC : Input optical depth data and complete test
;	EFFR : Input effective radius data and complete test 
;
;	Date
;	rsgguest2 : 22 July 2014
; $Id$
;
;	For a quick check of the programe/data use:
; snowmask,/fast
;	which saves the file to a temporary one for quick access
;
;==========================================================================
pro snowmask,file=file,fast=fast,datain=datain,dataout=col_num,chanin=chanin,cthch=cthch,flag=flag,landdata=landdata,ac1=ac1,ac2=ac2, $
	trial=trial,optic=optic,effr=effr

;read in the file
if ~keyword_set(file)  and ~keyword_set(datain) then file_s ='/misc/wantage_static/cpoulsen/cloud_ecv/postproc/2008/06/20/ESA_Cloud_cci_RAL_AATSR_1-59.3--59.3_ORACV27.0_ENV_20140617174012_200806200204_V27.0ICE.secondary.nc'

if ~keyword_set(file)  and ~keyword_set(datain) then file_p ='/misc/wantage_static/cpoulsen/cloud_ecv/postproc/2008/06/20/ESA_Cloud_cci_RAL_AATSR_1-59.3--59.3_ORACV27.0_ENV_20140617174012_200806200204_V27.pp.primary.nc'

; read a fast temporaray version of the file
if ~keyword_set(fast) then begin
;x_prim=ncdf_read(file_p)
x_sec=ncdf_read(file)
;x=ncdf_read(file)
endif else begin
goto,skipread
endelse


;Load in data from file, only required fields
;Matrix of data



;============================================================================
if keyword_set(file) or keyword_set(datain) then goto, skipdat
;Cloud Top Height for use as a final decider in snow or not

if ~keyword_set(cthch) then begin
CTHyes=is_tag(x_sec,'CTH')
if CTHyes eq 0 then print, 'no cth'
if CTHyes eq 0 then goto, skipcth1
cth=x_prim.CTH*x_prim.CTH_ATT.SCALE_FACTOR
endif
skipcth1:
;Criterion for being cloud-free snow-covered area

;TIR criterion BT-Brightness-Temperature
;Variables from data file, all numbers are in micrometers to
;1 decimal place
;is_tag statements to test for data
BT_37yes=is_tag(x_sec,'BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_5')
BT_108yes=is_tag(x_sec,'BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6')
BT_12yes=is_tag(x_sec,'BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7')

print, BT_37yes, BT_108yes, BT_12yes

;stop

;if data is present (yes=1) the data is got from file
if BT_37yes eq 1 then BT_37=x_sec.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_5*x_sec.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_5_ATT.SCALE_FACTOR
if BT_108yes eq 1 then BT_108=x_sec.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6*x_sec.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_6_ATT.SCALE_FACTOR
if BT_12yes eq 1 then BT_12=x_sec.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7*x_sec.BRIGHTNESS_TEMPERATURE_IN_CHANNEL_NO_7_ATT.SCALE_FACTOR


;VIS_NIR criterion RTOA-Top Of Atmosphere Reflectance
;is_tag statements to test for data
RTOA_16yes=is_tag(x_sec,'REFLECTANCE_IN_CHANNEL_NO_4')
RTOA_87yes=is_tag(x_sec,'REFLECTANCE_IN_CHANNEL_NO_3')
RTOA_66yes=is_tag(x_sec,'REFLECTANCE_IN_CHANNEL_NO_2')
RTOA_55yes=is_tag(x_sec,'REFLECTANCE_IN_CHANNEL_NO_1')
;if data is present (yes=1) the data is got from file
if RTOA_87yes eq 1 then RTOA_87=x_sec.REFLECTANCE_IN_CHANNEL_NO_3*x_sec.REFLECTANCE_IN_CHANNEL_NO_3_ATT.SCALE_FACTOR
if RTOA_16yes eq 1 then RTOA_16=x.REFLECTANCE_IN_CHANNEL_NO_4*x.REFLECTANCE_IN_CHANNEL_NO_4_ATT.SCALE_FACTOR
if RTOA_66yes eq 1 then RTOA_66=x_sec.REFLECTANCE_IN_CHANNEL_NO_2*x_sec.REFLECTANCE_IN_CHANNEL_NO_2_ATT.SCALE_FACTOR
if RTOA_55yes eq 1 then RTOA_55=x_sec.REFLECTANCE_IN_CHANNEL_NO_1*x_sec.REFLECTANCE_IN_CHANNEL_NO_1_ATT.SCALE_FACTOR

;if no data present (yes=0) then put in 0's

nx=n_elements(BT_108[*,1])
ny=n_elements(BT_108[1,*])

if BT_37yes eq 0 then BT_37=fltarr(nx,ny)
if BT_108yes eq 0 then BT_108=fltarr(nx,ny)
if BT_12yes eq 0 then BT_12=fltarr(nx,ny)
if RTOA_87yes eq 0 then RTOA_87=fltarr(nx,ny)
if RTOA_16yes eq 0 then RTOA_16=fltarr(nx,ny)
if RTOA_66yes eq 0 then RTOA_66=fltarr(nx,ny)
if RTOA_55yes eq 0 then RTOA_55=fltarr(nx,ny)


skipdat:

;==============================================================================

if ~keyword_set(file)  and ~keyword_set(datain) then goto, skipchan

if keyword_set(datain) then begin
;Channels in the cci, 2088 data
;0.645832     0.856874      3.70000      11.0263      12.0424

help,datain
help,chanin
help,cthch


;where statements to test for data, finds which columns
;are present so which data is present, may need adjusting for 
;other data sets, TIR data
;BT : Brightness Temperature
BT_37yes=where(chanin eq 3.7000,nn)
BT_108yes=where(chanin eq 11.0263)
BT_12yes=where(chanin eq 12.0424)
;print,BT_37yes

;if data is present (yes=1) the data is got from file
if  n_elements(BT_37yes) ge 1 then BT_37=datain(BT_37yes,*)
if  n_elements(BT_108yes) ge 1 then BT_108=datain(BT_108yes,*)
if  n_elements(BT_12yes) ge 1 then BT_12=datain(BT_12yes,*)

ele=n_elements(BT_37)
print, 'number of pixels', ele

;VIS_NIR criterion RTOA-Top Of Atmosphere Reflectance
;where statements to test for data, VIS data
;RTOA : Reflectance at the Top Of the Atmosphere
RTOA_16yes=where(chanin eq 1.6,nn)
RTOA_87yes=where(chanin eq 0.856874,nn)
RTOA_66yes=where(chanin eq 0.645832,nn)
RTOA_55yes=where(chanin eq 0.55,nn)
;if data is present (yes=1) the da
if  n_elements(RTOA_87yes) ge 1 then RTOA_87=datain(RTOA_87yes,*)
if  n_elements(RTOA_16yes) ge 1 then RTOA_16=datain(RTOA_16yes,*)
if  n_elements(RTOA_66yes) ge 1 then RTOA_66=datain(RTOA_66yes,*)
if  n_elements(RTOA_55yes) ge 1 then RTOA_55=datain(RTOA_55yes,*)

print, 'BTyes', BT_37yes, BT_108yes, BT_12yes
print, 'RTOAyes', RTOA_87yes, RTOA_16yes, RTOA_66yes, RTOA_55yes

if keyword_set(cthch) then begin
	CTH=cthch(*)
;	CTH=CTH(alb)
	CTHyes=1
endif

if RTOA_16yes lt 0 then RTOA_16yes =-1
if RTOA_87yes lt 0 then RTOA_87yes =-1
if RTOA_66yes lt 0 then RTOA_66yes =-1
if RTOA_55yes lt 0 then RTOA_55yes =-1
if BT_37yes lt 0 then BT_37yes =-1
if BT_12yes lt 0 then BT_12yes =-1
if BT_108yes lt 0 then BT_108yes =-1
endif

skipchan:


; create a structure to save to a temporary file
;data={BT_37:BT_37[*,15000:15512],BT_108:BT_108[*,15000:15512],BT_12:BT_12[*,15000:15512],RTOA_87:RTOA_87[*,15000:15512],RTAO_16:RTOA_16[*,15000:15512],RTOA_66:RTOA_66[*,15000:15512],ROTA_55:RTOA_55[*,15000:15512],CTH:CTH[*]}
;wr_sav,'~/Temp/temp_channels.sav',data

skipread:

;x=rd_sav('~/Temp/temp_channels.sav')

nx=n_elements(cth)
;Make arrays for colour number and variable to add
col_num=fltarr(nx,1)
colouradd=0.2

;===================================================================

		;Istomina Eqautions
 	 	;where statements for all data and applies colour numbering,
		;+0.2 for snow and +0 for cloud/other
		;the colour number can then be plotted

		if BT_37yes lt 0 then print, 'skip BT37', BT_37yes
		if BT_37yes lt 0 then goto, skip37 
		;if there is no 3.7 micrometer data then skip the TIR criterion

;apply equation 5

		if BT_108yes lt 0 then print, 'skip BT108', BT_108yes
		if BT_108yes lt 0 then goto, skip108
		;no 10.8 micrometer data, skip eq 5

		e5_index=where(ABS((BT_37-BT_108)/BT_37) le 0.02,nn_e5)
		;in the BT array where is the equation satisfied
		if nn_e5 ne 0 then col_num[e5_index]=col_num[e5_index]+colouradd
		;get the indicies of the pixels with this critereion
;		help,e5_index ;show them
		print,'nn_e5',nn_e5 ;print this on screen


skip108: ;skips 10.8 micrometer criterion

;apply equation 6
	
	if BT_12yes lt 0 then print, 'skip BT12', BT_12yes
	if BT_12yes lt 0 then goto, skip12
	;no 12 micrometer data, skip eq 6
	
 	e6_index=where(ABS((BT_37-BT_12)/BT_37) le 0.025,nn_e6)
	if nn_e6 ne 0 then col_num[e6_index]=col_num[e6_index]+colouradd
;	help, e6_index
	print, 'nn_e6', nn_e6

if keyword_set(effr) then begin
;effective radius greater than 45 microns
	print, range(effr)
	rei=where(effr ge 45, nre)
	if nre gt 0 then col_num[rei]=col_num[rei]+(colouradd/2.)
	print, 'Re testing', nre
endif  

if keyword_set(optic) then begin
;If effective radius greater then 45 microns and over land then test
	print, range(optic)
	opt=where(optic[rei] ge 10 and col_num[rei] gt 0., nod)
	if nod gt 0 then col_num[rei[opt]]=col_num[rei[opt]]+(colouradd/2.)
	print, 'optical testing', nod
endif 

	trial=col_num ;required in my version to test TIR only, but in actual do not have this but use if statement at the end
 	print, 'trial active', range(trial)
 
skip12: ;skips 12 micrometer criterion
skip37: ;skips the TIR criterion

	if RTOA_87yes lt 0 then print, 'skip RTOA87', RTOA_87yes
	if RTOA_87yes lt 0 then goto, skip87
 	;if there is no 0.87 micrometer data
 	;then skip to the final VIS_NIR criterion

;apply equation 7 (or 1)

	if RTOA_16yes lt 0 then print, 'skip RTOA16', RTOA_16yes
  if RTOA_16yes lt 0 then goto, skip16
	;no 1.6 micrometer data, skip eq 7

	e7_index=where((RTOA_87-RTOA_16)/RTOA_87 gt 0.8,nn_e7)
	if nn_e7 ne 0 then col_num[e7_index]=col_num[e7_index]+colouradd 
;	help, e7_index
	print, 'nn_e7', nn_e7


skip16:
	
	if RTOA_66yes lt 0 then print, 'skip RTOA66', RTOA_66yes
	if RTOA_66yes lt 0 then goto, skip66
	;if there is no 0.66 micormeter data then skip to the plotting

;apply equation 8 (or 2)

	e8_index=where((RTOA_87-RTOA_66)/RTOA_87 lt 0.1, nn_e8) 
	if nn_e8 ne 0 then col_num[e8_index]=col_num[e8_index]+colouradd
	e82_index=where((RTOA_87-RTOA_66)/RTOA_87 le -0.6, nn_e82)
	e83_index=where((RTOA_87-RTOA_66)/RTOA_87 ge 0.3 and col_num gt 0.2, nn_e83)
;it was found that in equation 8 sea came out as <-0.6 and land as >0.3
;	help, e8_index
	print, 'nn_e8', nn_e8
	print, 'sea from eq 10', nn_e82
	print, 'land from eq 11', nn_e83


skip87:

;apply equation 9 (or 3)

	if RTOA_66yes lt 0 then print, 'skip RTOA66 2nd', RTOA_66yes
	IF RTOA_66yes lt 0 then goto, skip66
	;if there is no 0.66 micormeter data then skip to the plotting, 
  ;second test incase of 0.87 and 0.66 both missing

	if RTOA_55yes lt 0 then print, 'skip RTOA55', RTOA_55yes
	if RTOA_55yes lt 0 then goto, skip55
	;no 0.5 micrometer data, skip eq 9 go to plotting

	e9_index=where(ABS(RTOA_66-x.RTOA_55)/RTOA_66 lt 0.4, nn_e9)
	if nn_e9 ne 0 then col_num[e9_index]=col_num[e9_index]+colouradd
;	help, e9_index
	print, 'nn_e9', nn_e9


skip55:
skip66:


print, range(col_num)
;check the range is sensible
flag=col_num ;create new colour number array so that we can see the mask before cth limiting
;This also means the outputted istomina mask is the raw data from the
;above tests
if nn_e82 gt 0 then flag[e82_index]=2.0 ;put flag to 2 so it can be sceened out easily
if nn_e83 gt 0 then flag[e83_index]=0.3 ;put flag to 0.3 so it can be screened out easily
;the indecies are from the second limits of equation 8

;====================================================================


;Final tests and out put of snow mask
if CTHyes eq 0 then goto, skipcth
print, 'range cth', range(cth)
low=where(cth lt 0, nlow)
print, 'less than 0', nlow
skipcth:

if keyword_set(ac1) and keyword_set(ac2) then begin
;Checks that the albdeo indicates snow, tends to cut out
;pixels over sea. Change there flag to stop it flagging as something else
	print, 'ac1', range(ac1)
	print, 'ac2', range(ac2)
	sub=where(ac1 lt 0.2 and ac2 lt 0.1, nsub)
	if nsub gt 0 then flag(sub)=1.2
endif

flgd=where(flag ge 0.4 and flag lt 1.1, nflgd)
if nflgd gt 0 then flag[flgd]=0.5
;Make all that flagged twice or more, but are not sea, flag=0.5 for
;ease of picking out

if RTOA_87yes lt 0 and RTOA_66yes lt 0 then goto, skipsl

;Day time algorithm

land=where(landdata[e83_index] gt 0.5, n_land)
if nn_e83 gt 0 then flag[e83_index[land]]=3.0

ls_lim=2.9 ;Upper height limit for snow over sea (km)

lndsnw=where(landdata[flgd] gt 0.5, nls)
if nls gt 0 then flag[flgd[lndsnw]]=1.0
ssnw=where(landdata[flgd] lt 0.5 and cth[flgd] lt ls_lim, nss)
if nss gt 0 then flag[flgd[ssnw]]=1.0

ss=where(landdata lt 0.5, nss)
cloud=where(cth[ss] gt ls_lim and flag[ss] lt 1.0, n_cloud) 
if n_cloud gt 0 then flag[ss[cloud]]=0

;cth_lim=2.9 ;Upper height limit for cth for sea (km)
;This does override the albedo test, so open sea will still plot as
;such and not as low albedo, but cloud over sea goes.
sea=where(landdata[e82_index] lt 0.5 and cth[e82_index] le ls_lim, n_sea)
if n_sea gt 0 then flag[e82_index[sea]]=2.0

puddle=where(landdata[e82_index] gt 0.5, npuddle)
;Stops flagging of sea on land areas, puts as melted snow
if npuddle gt 0 then flag[e82_index[puddle]]=1.0


snowflag=where(flag eq 1, n_snow) ;if flag equal to 1 then it is snow
if n_snow gt 0 then flag[snowflag]=-99 
;put snow pixels to -99 so they are plotted as white in the mask, ie are taken out of the image

cloud=where(flag lt 0.2 and flag ne -99, n_cloud)
;To check values print the number that are cloud, snow, land and sea
print, 'land flag', n_land
print, 'sea flag', n_sea
print, 'cloud', n_cloud
print, 'flag', range(flag)
;check the range is okay, should be -99-3

percentage_snow=((n_elements(snowflag))/float(n_elements(flag)))*100 
;calculate the percentage is pixels that are flagged as snow and print
;for checking
if n_snow gt 0 then print, 'snow', percentage_snow


lcd=where(flag eq 0 and cth lt ls_lim, nlcd)
;check to see how many clouds are very low/at ground level
if nlcd gt 0 then print, 'no. low cloud', nlcd

;If no VIS data skip to here at complete night time algorithm  
skipsl:

;Night time algorithm

;if keyword_set(trial) and keyword_set(landdata) then begin
;if  RTOA_87yes lt 0 and  RTOA_66yes lt 0 then begin
;trial=flag
print, range(trial)
	cth_lim=2.9
	snowt=where(trial ge 0.2 and cth le cth_lim, nst)
	if nst gt 0 then trial[snowt]=-9
print, 'snow', nst

	seat=where(trial gt 0 and trial lt 1.0)
	st=where(landdata[seat] lt 0.5, nst)
print, 'cloud', nst
	if nst gt 0 then trial[seat[st]]=0

	sm=where(trial ge 0.4)
	landt=where(cth[sm] lt 5 and landdata[sm] gt 0.5, nl)
	if nl gt 0 then trial[sm[landt]]=-9
print, 'land', nl

sn=where(trial gt 0., ncld)
	lant=where(cth[sn] ge 5, n_cld)
	if nl gt 0 then trial[sn[lant]]=0
print, 'cloud', n_cld

if keyword_set(ac1) and keyword_set(ac2) then begin
	sub=where(ac1 lt 0.2 and ac2 lt 0.1, nsub)
	if nsub gt 0 then trial(sub)=1.2
endif

print, range(trial)
;Check range is ok 
;endif 

;Put those cloud top heights of less than zero to flag 2.2, so thay
;cna be spotted, not many have been found in the data
if nlow gt 0 then flag[low]=2.2
;if nlow gt 0 then stop

;This can be outputted as an array of indexes not to be used in
;further analysis for clouds. is where there is thought to be clear snow
snowflag=where(flag eq -99, nsnow)

;programe ends, criterion applied, plot is plotted and mask is made
end
