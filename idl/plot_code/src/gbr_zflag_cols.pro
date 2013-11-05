;==========================================================================
;+
;	pro GBR_ZFLAG_COLS
;
;	Description
; Set and load colour table for ATSR Cloud flagging (A. Stevens clzones)
;
;	Parameters
; COLS return structure of colors
; 
;	Keywords
;
;	Date
;	B. Latter : 22 November 2001
;-
;==========================================================================
pro gbr_zflag_cols,cols
; Cloud color scheme
;   -1  0   1   2   3   4   5   6   7   8   9  10  11  12  13  14  15  16  17
r=[255,180,  0,  0,255,  0, 91,238,130, 67,179,125,  0,255,240,240,255, 50,100]
g=[  0,180,227,126,255,  0, 50,177, 87,117,208,175,150,  0,240,240,255, 50,100]
b=[  0,180,255,255,255,200,143,  0,  0,  0, 93,  0,  0,  0,160, 55,  0, 50,100]

tvlct,r,g,b

rgb=[[r],[g],[b]]

zones=['unclassified',$
	'ice',$
	'thin cloud over sea',$
	'possible thin cloud over sea',$
	'thick cloud',$
	'sea',$
	'dirty water e.g. river estuary',$
	'desert',$
	'desert/sparse vegetation',$
	'desert/dried vegetation',$
	'vegetation with possible cloud',$
	'vegetaion with thin cloud',$
	'vegetation',$
	'invalid data',$
	'sunglint - originally flagged as thin cloud',$
	'sunglint - originally flagged as possible cloud',$
	'sunglint - originally flagged as cloud']
; short version
zid=['unk',$
	'ice',$
	'tcl',$
	'ptcl',$
	'cl',$
	'sea',$
	'dw',$
	'd',$
	'dsv',$
	'ddv',$
	'vpc',$
	'vtc',$
	'v',$
	'inval',$
	'sgtc',$
	'sgptc',$
	'sgc']

cols={rgb:rgb,zones:zones,zid:zid}
end
