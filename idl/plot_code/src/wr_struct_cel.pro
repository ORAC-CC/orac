;===============================================================================
;+
; WR_STRUCT_CEL
;
; Write structure prototype for C
;
; PARAMETERS
;	OFI	See RD_STRUCT1,/MKC
;	CEL
;
; KEYWORDS
;	
;
; R.S. 14/02/03
; $Id: wr_struct_cel.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
pro wr_struct_cel,ofi,cel
	get_lun,lun
	openw,lun,ofi
	icom=0
	for j=0,n_elements(cel)-1 do begin
		ss=strsplit(cel(j),'\',/ext)
		nt=0
		for i=0,n_elements(ss)-1 do begin
			os=strtrim(ss(i),2)
			if os eq '/*' then icom=1
			bs=byte(os)
			wh=where(bs eq 123,nop)
			wh=where(bs eq 125,ncl)
			if ss(i) eq '}' or ss(i) eq '};' then begin
				nt=nt-1
				if nt lt 0 then message,'too many }'
			endif
			if icom eq 0 then for k=0,nt-1 do os='	'+os
			if nop gt ncl then nt=nt+nop-ncl
			printf,lun,os
			if os eq '*/' then icom=0
		endfor
		printf,lun,''
	endfor
	close_file,lun
end
