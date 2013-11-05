;+
; CLDMODEL_SV_LEVELS
;
; Return some sensible colour bar levels for a given short name from 
; cloud-model state-vector structure
;
; PARAMETERS
;	S	Short name for sv
;
; KEYWORDS
;	CBS	Return suggested value for STEP keyword in call to CBAR
;
; R.S. 19/12/2008
;-
function cldmodel_sv_levels,s,cbs=cbs
	u=strupcase(s)
	sp=strpos(u,'/')
	if sp gt 0 then u=strtrim(strmid(u,0,sp),2)
	sp=strpos(u,'-')
	if sp gt 0 then u=strtrim(strmid(u,0,sp),2)
	cbs=0
	if u eq 'PC' then begin
		lev=findgen(21)/20*16
	endif else if u eq 'SRFC' then begin
		lev=findgen(21)/20*0.4+0.8
	endif else if u eq 'RE' then begin
		lev=[0.,0.5,1.,2.,4,6,8,10,12,15,17,20,25,30,40,50,60,80,100]
		cbs=1
	endif else if u eq 'LCOT' then begin
		lev=[0.,0.01,0.02,0.05,0.1,0.2,0.5,1.,2.,5,10,20,50,100,200]
		cbs=1
	endif else if u eq 'TS' then begin
		lev=findgen(21)/20*100+240
	endif else if u eq 'H2O' then begin
		lev=findgen(21)/20*2
	endif else if u eq 'F' then begin
		lev=findgen(22)/20
	endif else if u eq 'SO2' then begin
		lev=[-100,-50,-20,-10,0,20,50,100,200,500,1000]
		cbs=1
	endif else message,'Unknown short name: '+u
print,u,cbs
	return,lev
end
