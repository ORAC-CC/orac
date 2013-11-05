;===============================================================================
;+
; C_TDSTRUCT
;
; Create string containing C structure type def
;
; PARAMETERS
;	X	Structure
;
; KEYWORDS
;	ANUM	-
;	HFI	Name of header file which will be written to
;		contain struct defs.
;
; R.S. 14/02/03
; $Id: c_tdstruct.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function ptr_down,s
	if strmid(s,0,2) eq '*(' then return,strmid(s,2,strlen(s)-3)
	if strmid(s,0,1) eq '*' then return,strmid(s,1,strlen(s)-1)
	return,'&('+s+')'
end
function malloc_str,n,t,nd,id,ds
	ph='*'
	ph1=''
	for i=id,nd-2 do begin
		ph=ph+'*'
		ph1=ph1+'*'
	endfor
	s='if ( ( '+n+'=('+t+ph+')malloc('+ds+$
		'*sizeof('+t+ph1+') ) ) == NULL ) {\'+$
		'printf("Insufficient memory : '+n+'");\exit(1);\}\'
	return,s
end
function c_tdstruct,x,anum=anum,hfi=hfi
	if n_elements(hfi) eq 0 then hfi='rd_struct.h'
	sn=strlowcase(tag_names(x,/structure_name))
	if sn eq '' then message,'anon struct !'
	tn=strlowcase(tag_names(x))
	stn='(x->'+tn+')'
	hc=''
	hc=hc+' \\'+'struct '+sn+' {'
	vm=['undef','char','short','int','float','double','complex','char','struct']
	nt=n_tags(x)
	rc=''
	pt='void rd_'+sn+'(FILE *in, struct '+sn+' *x)'
	rc=rc+pt+'\{\\int n,nt,n1,ii0,ii1,ii2,ii3,ii4,*dims;\char *name;\\'
	rc=rc+' \\name=rd_struct_name(in);\\free(name);\\'
	rc=rc+' \\name=rd_struct_name(in);\\free(name);\\'
	sc=''
	for i=0,nt-1 do begin
		v=x.(i)
		s=size(v)
		t=s(s(0)+1)
		nd=s(0)
		ph=''
		for k=0,nd-1 do ph=ph+'*'
sc=sc+'/*\ '+sn+'.'+tn(i)+'\*/'
		sc=sc+' \name=rd_struct_name(in);\dims=rd_struct_dims(in);\'
		sc=sc+' \nt=(*(dims+((*dims)+2)));\'
		if nd gt 1 then sc=sc+'n1=(*(dims+1));\'
		nd0=nd
		if t gt 0 and t lt 6 then begin
			hc=hc+vm(t)+' '+ph+tn(i)
			vt=vm(t)
			readcmd='fread('+ptr_down(ph+stn(i))+',sizeof('+vt+'),nt,in)'
		endif else if t eq 7 then begin
			vt='char'
			if nd gt 1 then begin
message,/info,'> 1-D string arrays stored as 1-D !'
nd=1
			endif
			if nd eq 0 then begin
				readcmd=stn(i)+'=*(rd_struct_strs(in,dims))'
				ph='*'
			endif else begin
				readcmd=stn(i)+'=rd_struct_strs(in,dims)'
				ph='**'
			endelse
			hc=hc+vt+' '+ph+tn(i)
		endif else if t eq 8 then begin
			if nd gt 1 then begin
message,/info,'> 1-D struct arrays stored as 1-D !'
nd=1
			endif
			if s(s(0)+2) eq 1 then nd=0
			subn=+strlowcase(tag_names(v,/structure_name))
			if subn eq '' then message,'Anon structure !: '+tn
			if nd eq 0 then begin
				readcmd='rd_'+subn+'(in,&'+stn(i)+')'
				ph=''
			endif else begin
				readcmd='for(ii0=0;ii0<nt;ii0++) rd_'+subn+'(in,'+stn(i)+'+ii0)'
				ph='*'
			endelse
			vt='struct '+subn
			hc=hc+vt+' '+ph+tn(i)
		endif else message,'Type not implemented: '+trim_zero(t)+' '+tn(i)
		pt1=ptr_down(ph+stn(i))
		tn1=stn(i)
		if nd gt 1 then begin
			sc=sc+'n = 0;\\'
			for id=nd-1,1,-1 do begin
				lv='ii'+trim_zero(id)
				nd1='(*(dims+'+trim_zero(id+1)+'))'
				sc=sc+malloc_str(tn1,vt,nd,nd-id-1,nd1)
				sc=sc+'for('+lv+'=0;'+lv+'<'+nd1+';'+lv+'++){\'
				tn1='*('+tn1+'+'+lv+')'
				if id eq 1 then begin
					sc=sc+'if ( n == 0 ) '+malloc_str(pt1,vt,nd,nd,'nt')
					sc=sc+tn1+'=('+pt1+')+n;\\n+=n1;'
				endif
			endfor
			for id=1,nd-1 do sc=sc+'\}\'
		endif else begin
			if nd gt 0 and t ne 7 then sc=sc+malloc_str(tn1,vt,1,1,'nt')
		endelse
		sc=sc+readcmd+';\' 

		if nd ne 0 then begin
			sc=sc+'for(ii0=0;ii0<'+trim_zero(nd0+3)+';ii0++) x->sz_'+tn(i)+'[ii0]=(*(dims+ii0));\\'
			hc=hc+'; int sz_'+tn(i)+'['+trim_zero(s(0)+3)+']'
		endif
		sc=sc+'free(name);\\free(dims);\\'
		hc=hc+';\\'
	endfor
	rc=rc+' \'+sc+'name=rd_struct_name(in);\\free(name);\}'
	hc=hc+'};\\ \\'+pt+';\\'
	return,[hc,rc]
end
