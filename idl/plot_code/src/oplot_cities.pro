;===============================================================================
;+
; OPLOT_CITIES
;
; Overplot cities on a map
;
; PARAMETERS
;	
;
; KEYWORDS
;	NAME	Annotate with name of city
;	_EXTRA set to XYOUTS
;	PSYM	plot symbol
;	POP	Only plot cities with population greater than POP million
;		IF 2 number specified this defines a population range.
;		(NB entries in cities.txt far from complete !, but
;		does include top 30 by population from 
;		http://www.un.org/esa/population/publications/wup2001/wup2001dh.pdf)
;	MLL	Only plot the city if it has the biggest population within the
;		given radius from each city
;	UNCONFUSE Set to only plot big sities in Brazil and Asia to avoid
;		confusing map
;	COLOR	As plot keyword
;	SYMSI	As plot keyword
;	CHARSIZE As plot keyword
;
; R.S. 02/09/04
; $Id: oplot_cities.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
pro trans_odd,x
	tr=[['BeijingBeijing','Beijing'],$
		['GuangzhouGuangdong','Guangzhou'],$
		['WuhanHubei','Wuhan'],$
		['NanjingJiangsu','Nanjing'],$
		['XianShaanxi','Xian'],$
		['JinanShandong','Jinan'],$
		['HarbinHeilongjiang','Harbin'],$
		['ChangchunJilin','Changchun'],$
		['ShenyangLiaoning','Shenyang'],$
		['ChengduSichuan','Chengdu']]
	nt=n_elements(tr(0,*))
	for i=0,nt-1 do begin
		wh=where(x.name eq tr(0,i),nw)
		if nw gt 0 then x(wh).name=tr(1,i)
	endfor
end
pro oplot_cities,_EXTRA=extra,psym=psym,name=name,pop=pop,color=color,symsi=symsi,mll=mll,charsize=chs,unconfuse=uncon
	x=rd_struct('$RS_HOME/population/cities.str')
	trans_odd,x
	if n_elements(pop) gt 0 then begin
		if n_elements(pop) eq 2 then begin
			wh=where(x.population/1000. gt pop(0) and x.population/1000 lt pop(1))
		endif else begin
			wh=where(x.population/1000. gt pop(0))
		endelse
		if wh(0) eq -1 then return
		x=x(wh)
;for i=0,n_elements(x)-1 do if strpos(x(i).name,'-') gt 0 then print,x(i).country+'    '+x(i).name
	endif
	if keyword_set(uncon) then begin
		wh=where(x.name ne 'Sao-Paulo')
		x=x(wh)
	endif
	if n_elements(psym) eq 0 then psym=2
	if keyword_set(mll) then begin
		nc=n_elements(x)
		whok=-1
		for i=0,nc-1 do begin
			whg=where(x.population gt x(i).population,ng)
			if ng gt 0 then begin
				dd=llsub(x(whg).lat,x(whg).lon,x(i).lat,x(i).lon,/dis)
				whd=where(dd lt mll,nd)
				if nd eq 0 then begin
					if whok(0) eq -1 then whok=i else whok=[whok,i]
				endif
			endif
		endfor
		x=x(whok)
	endif
	cc=convert_coord(x.lon,x.lat,/data,/to_norm)
	wh=where(cc(0,*) gt !x.window(0) and cc(0,*) lt !x.window(1) $
		and cc(1,*) gt !y.window(0) and cc(1,*) lt !y.window(1),nw)
	if nw gt 0 then begin
		plots,x(wh).lon,x(wh).lat,psym=psym,noclip=0,color=color,symsi=symsi
		if keyword_set(name) then $
			xyouts,x(wh).lon,x(wh).lat,$
				strreplace(x(wh).name,'_',' '),_EXTRA=extra,noclip=0,charsize=chs
;for i=0,n_elements(wh)-1 do print,x(wh(i)).name,x(wh(i)).population
	endif
end
