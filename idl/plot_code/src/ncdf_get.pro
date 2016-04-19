;+
; NCDF_GET
;
; Get one variable from an NCDF file
;
; PARAMETERS
;	FI	Filename
;	VARNAME	Variable name (string)
;
; KEYWORDS
;	LUN	Set/return NCDF FILE ID
;	NOCLOSE Don't close file
;	A2	Return Attribute information in different (more complete) way
;	NOGET	Don't actually get values, just other stuff
;	TUC	If variable not found, try upper case version of variable name
;	UNDO	Undo scaling using scale_factor and add_offset if present
;
; $Id: ncdf_get.pro 3437 2015-03-23 13:14:24Z rsiddans $
;-
function ncdf_get,fi,varname,lun=l,noclose=noclose,a2=a2,noget=noget,tuc=tuc,count=count,offset=offset,undo=undo
        if not keyword_set(l) then l=ncdf_open(fi)
	pq=!quiet
	!quiet=1
	v=ncdf_varid(l,varname)
	if keyword_set(tuc) and v lt 0 then v=ncdf_varid(l,strupcase(varname))
	!quiet=pq
	if v ge 0 then begin
                id=ncdf_varinq(l,v)
		if id.datatype eq 'UNKNOWN' then message,/info,'Unknown data type: '+varname+' (cannot read it!)'
		atts=0
		d=id
;
; get attributes first
;
                for iatt=0,id.natts-1 do begin
                        name=ncdf_attname(l,id.name,iatt)
			name0=name
                        inq=ncdf_attinq(l,id.name,name)
			if inq.datatype eq 'UNKNOWN' then begin
				message,/info,'Unknown data type: '+varname+'/Att: '+name+' (cannot read it!)'
			endif else begin
                        	ncdf_attget,l,id.name,name,att
                        	if inq.DATATYPE eq 'CHAR' then att=string(att)
				tmp=tag_names(d) 
                        	chk=where(tmp eq strupcase(name),nchk)
				if nchk gt 0 then name=name+strtrim(nchk,2)
				if keyword_set(a2) then begin
                        		d1=create_struct(name,{name:name0,value:att})
					if n_tags(atts) eq 0 then atts=d1 else atts=create_struct(atts,d1)
				endif else begin
                        		d1=create_struct(name,att)
                        		d=create_struct(d,d1)
				endelse
			endelse
                endfor
		if keyword_set(a2) then d=create_struct(d,'ATTS',atts)
		if keyword_set(noget) or id.datatype eq 'UNKNOWN' then d=-999 else begin
			ncdf_varget,l,v,val,count=count,offset=offset
			if keyword_set(undo) then begin
				if is_tag(atts,'scale_factor') then val=val*atts.scale_factor.value
				if is_tag(atts,'add_offset') then val=val+atts.add_offset.value
			endif
		endelse
                d=create_struct(d,{value:val})
	endif else begin
		message,/info,'Variable not in NCDF file: '+varname
		d=-999
	endelse
	if not keyword_set(noclose) then ncdf_close,l
	return,d
end

