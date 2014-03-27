;===============================================================================
;+
; MK_CLDMODEL_FALSE_COLOR
;
; Return 32 bit false colour index from standard RET_CLDMODEL inputs 
;
; PARAMETERS
;	X	From RD_RET_CLDMODEL (if multi-type retrieval then need
;		to reduce to 1 type, e.g. run xc=ret_min_cost(x,h.types) and
;		pass XC into here
;	H	From RD_RET_CLDMODEL
;
; KEYWORDS
;	PINK 	Return SEVIRI pink plot
;	SIM	Return result based on FM simulations not measurements
;	I37	False colour using 3.7 micron reflectance and 0.6,1.6
;
; R.S. 04/04/11
; $Id: mk_cldmodel_false_color.pro 1106 2011-08-08 09:04:56Z rsiddans $
;-
;===============================================================================
function mk_cldmodel_false_color_cloud,x,h,pink=pink,sim=sim,i37=i37,n32=n32,tc=tc,alb=alb
	sz=size(x)
	if sz(0) ne 1 then message,'X should be 1D'
	if keyword_set(sim) then y=transpose(x.yn) else y=transpose(x.y)
	if keyword_set(alb) then  alb=transpose(x.alb)
	nx=n_elements(y(*,0))
	if keyword_set(pink) then begin
                ich=get_nns([8.7,11,12],h.s.mwl)      ; identify std false colour channel
                im1=reform(y(*,ich),nx,1,3)
                im=([[[apply_rgb_range(im1(*,*,2)-im1(*,*,1),[-4.,2.])]],$
                        [[apply_rgb_gamma(apply_rgb_range(im1(*,*,1)-im1(*,*,0),[0.,15]),1./2.5)]],$
                        [[apply_rgb_range(im1(*,*,1),[261.,289])]]]);/255
	endif else if keyword_set(i37) then begin
                ic=get_nns([0.8,0.63],h.s.mwl)
                im=[[mk_3p7_reflectance(transpose(y),h.s)*2],[y(*,ic)]]
                im=im*450
                wh=where(im gt 255,nw) & if nw gt 0 then im(wh)=255
                wh=where(im lt 0,nw) & if nw gt 0 then im(wh)=0
                im=((im/255)^0.7)*255
	endif else if keyword_set(tc) then begin
        	ich=get_nns([0.67,0.55,0.45],h.s.mwl)      ; true-ish colour
print,ich
        	im=reform(y(*,ich),nx,1,3)
im(*,*,2)=im(*,*,0)+(im(*,*,2)-im(*,*,0))*8
;stop
        	im=im*450
        	wh=where(im gt 255,nw) & if nw gt 0 then im(wh)=255
        	wh=where(im lt 0,nw) & if nw gt 0 then im(wh)=0
        	im=((im/255)^0.7)*255
	endif else if keyword_set(alb) then begin
        	;ich=get_nns([1.6,0.8,0.63],h.s.mwl)
                ich=[2,1,0]; identify std false colour channel
        	im=reform(alb(*,ich),nx,1,3)
        	im=im*450
        	wh=where(im gt 255,nw) & if nw gt 0 then im(wh)=255
        	wh=where(im lt 0,nw) & if nw gt 0 then im(wh)=0
        	im=((im/255)^0.7)*255
	endif else begin
        	ich=get_nns([1.6,0.8,0.63],h.s.mwl)      ; identify std false colour channel
        	im=reform(y(*,ich),nx,1,3)
        	im=im*450
        	wh=where(im gt 255,nw) & if nw gt 0 then im(wh)=255
        	wh=where(im lt 0,nw) & if nw gt 0 then im(wh)=0
        
	im=((im/255)^0.7)*255
	endelse
        im=reform(im)
;
;set cmax to full range so albedo and false color images plotted on
;the same scale
;
        if not keyword_set(n32) then im=mk_32bit_image(im(*,0),im(*,1),im(*,2),cmax=255,cmin=0)


	return,im

end
