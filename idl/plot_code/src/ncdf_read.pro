;===============================================================================
;+
; NCDF_READ
;
; Read all of an NCDF file into a structure.
; Not very efficient for large files as this routine repeatedly concatenates
; structures.
;
; PARAMETERS
;	FILE	File to be read
;
; KEYWORDS
;	NOGET	Do not read the data, only the attributes
;
; R.S. 28/04/05
;- Nb.B cannot have tag names that begin with a number
; bug fix
;===============================================================================
function ncdf_read,file,noget=noget,notr=notr
if not keyword_set(notr) then begin
catch,Error_status
IF Error_status NE 0 THEN BEGIN
PRINT, 'Error index: ', Error_status
PRINT, 'Error message:', !ERR_STRING ;
CATCH, /CANCEL
close,/all
heap_gc
return,0
ENDIF
endif
	l=ncdf_open(file)
	a=ncdf_inquire(l)
	if a.ndims eq 0 then return,0
	dim_names=strarr(a.ndims)
	dims=lonarr(a.ndims)
	for i=0,a.ndims-1 do begin
		ncdf_diminq,l,i,name,size
		dim_names(i)=name
		dims(i)=size
	endfor
	if a.ndims gt 0 then st={dim_names:dim_names,dims:dims}
	for i=0,a.ngatts-1 do begin
		name=ncdf_attname(/global,l,i)
		inq=ncdf_attinq(/global,l,name)
		ncdf_attget,/global,l,name,v
 ;                      print,'bname',name
;print,'iii',i,a.ngatts
;print,'bname',name
;                print,'a', inq.DATATYPE
		if inq.DATATYPE eq 'CHAR' then v=string(v)
		st1=create_struct(name,{name:name,value:v})
		if n_tags(st) eq 0 then st=st1 else st=create_struct(st,st1)
	endfor
	for i=0,a.nvars-1 do begin
		id=ncdf_varinq(l,i)
		if not keyword_set(noget) then ncdf_varget,l,id.name,v
		sta=id
		for iatt=0,id.natts-1 do begin
			name=ncdf_attname(l,id.name,iatt)
			inq=ncdf_attinq(l,id.name,name)
			ncdf_attget,l,id.name,name,att
;                        print,'b1',id.name
;                        print,'b2',name
;                        print,'b3',att
;                        print,'b4',sta
 ;                       print,'b5',sta
;
;check name has no dashes in it or the numebr 2
;
pos=strpos(id.name,'-')
if pos ge 0 then begin
id.name=strcompress(strmid(id.name,0,pos)+strmid(id.name,pos+1,10),/remove_all)
;print,id.name
endif

pos=strpos(id.name,'2')
if pos eq 0 then begin
print,id.name
id.name=strcompress(strmid(id.name,0,pos)+strmid(id.name,pos+1,10),/remove_all)

endif
;                        print,'b', inq.DATATYPE
			if inq.DATATYPE eq 'CHAR' then att=string(att)
			sta1=create_struct(name,att)
			sta=create_struct(sta,sta1)
		endfor
		if not keyword_set(noget) then $
			st1=create_struct(id.name,v,id.name+'_att',sta) $
		else st1=create_struct(id.name+'_att',sta)
		if n_tags(st) eq 0 then st=st1 else st=create_struct(st,st1)
	endfor
	ncdf_close,l
	return,st
end
