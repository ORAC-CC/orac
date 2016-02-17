pro quick_cim_prc,sg,u,v,d,_EXTRA=extra,ll=ll,ext=ext,chars=chs,brd=brd,crd=crd,pext=pext,cpos=cpos,axti=axti,nodata=nodata,kml=kmlfi,eop_col=eop_col,eop_thick=eop_thick,eop_y=eop_y,eop_x=eop_x

	md=min(d(where(finite(d) eq 1)))

	if not keyword_set(axti) then sat=sg.sat else sat=0
	if sat eq 1 then begin
		pl_boxmap,u,v,d,lonr=range(u),latr=range(v),brd=brd, $
                          crd=crd,_EXTRA=extra,ext=ext,$
			chsrs=chs

	endif else begin
		if n_elements(nodata) eq 0 then nodata=md-999
		di=standard_grid_1d2d(d,u,v,nodata=nodata,ur=ur,vr=vr,$
                                      ll=ll,lonr=lonr,latr=latr,rll=sg,ori=ori)
		if keyword_set(pext) then call_procedure,pext,di, $
                                                         _EXTRA=extra,ur=ur,vr=vr
		if sg.sd eq 1 then begin
			quick_cim,di,_EXTRA=extra,min_v=md-100, $
                                  chars=chs,brd=brd,crd=crd,cpos=cpos

		endif else begin
                   if keyword_set(kmlfi) then begin
                      quick_cim_kml,kmlfi,di,_EXTRA=extra,$
                                    min_v=md-100,/add,/noclose,$
                                    lonr=lonr,latr=latr

                   endif else begin
                      quick_cim,di,_EXTRA=extra,min_v=md-100,$
                                chars=chs,brd=brd,crd=crd,cpos=cpos

                      if n_elements(lonr) gt 0 then begin
                         !p.multi(0)=!p.multi(0)+1
                         plot,lonr,latr,/nodata,/xst,/yst, $
                              chars=chs*0.5,position=cpos,xminor=1,yminor=1

                         !p.multi(0)=!p.multi(0)+1
                         kmap_set,/adv,pos=cpos,lonr=lonr,latr=latr, $
                                  /nobo,/hir,ori=ori
                      endif
                      
                      if keyword_set(ext) then call_procedure,ext,_EXTRA=extra

                      
                   endelse
                endelse
		if keyword_set(ext) then call_procedure,ext,_EXTRA=extra, $
                                                        u=u,v=v,cpos=cpos ;,eop_col=eop_col,eop_thick=eop_thick,eop_y=eop_y,eop_x=eop_x

	endelse
     end
