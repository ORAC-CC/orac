;===============================================================================
;+
; CLDMODEL_RANGES
;
; Return valid ranges of OPD,RE for a specic retrieval TYPE
;
; PARAMETERS
;	T	Type string e.g ICE,LIQUID,BBCERRADO,CONTCLEAN,DESERT,MARCLEAN
;
; KEYWORDS
;	
;
; R.S. 11/05/09
; $Id: cldmodel_ranges.pro 1461 2012-04-04 13:22:21Z rsiddans $
;-
;===============================================================================
function cldmodel_ranges,t
	nt=n_elements(t)
	if nt gt 1 then begin
;
; deal with multiple types
;
		for i=0,nt-1 do begin
			r=cldmodel_ranges(t(i))
			if i eq 0 then rs=replicate(r,nt)
			rs(i)=r
		endfor
		return,rs
	endif
	t1=strupcase(t(0))
	t1=strreplace(t1,['WAT'],['LIQUID'])
	sp=strpos(t1,'-')
	if sp gt 0 then t1=strmid(t1,0,sp)
	types=['ICE','LIQUID','BBCERRADO','CONTCLEAN','DESERT','MARCLEAN','VOLCOX','VOLC']
	minre=[0.1  ,0.1     ,0          ,0          ,0       ,0         ,   0    ,   0  ]
	maxre=[200. ,30      ,0.2        ,0.4        ,8.      ,8         ,   10   ,  10  ]
	minod=[0.1  ,0.1     ,0.         ,0          ,0       ,0         ,   0    ,   0  ]
	maxod=[999. ,999     ,999        ,3          ,3       ,3         ,  999   , 999  ]
	it=(where(types eq t1,nw))(0)
	if nw eq 0 then message,'Type not recognised: '+t1
	return,{type:types(it),re:[minre(it),maxre(it)],lopd:alog10_nz([minod(it),maxod(it)])}
end
