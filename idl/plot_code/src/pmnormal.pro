;===============================================================================
;+
; PMNORMAL.PRO
;
; This routine takes normalised coordinates in the plot region and 
; returns another pair which give the normalised position in the whole 
; plot window. These can then be used by xyouts,plots etc. This means
; that normalised coorinates can be used even when p.multi is set to
; get multiple plots, if this routine is used to convert them.
;
; PARAMETERS
;	X,Y	plot region normalised coordinates.
;	MX,MY   plot window normalised coordinates.
;
; KEYWORDS
;	WINDOW Set this for normalisation within plot window, rather than region.
;
; R.Siddans 10/2/95
;-
;===============================================================================
pro pmnormal,x,y,mx,my,window=window
	if not keyword_set(window) then begin
		dx=!x.window(1)-!x.window(0)
		dy=!y.window(1)-!y.window(0)
		mx=!x.window(0)+x*dx
		my=!y.window(0)+y*dy
	endif else begin
		if !p.multi(1) le 1 then nc=1 else nc=!p.multi(1)
		if !p.multi(2) le 1 then nr=1 else nr=!p.multi(2)
		plot_num=nr*nc-!p.multi(0)-1
		if !p.multi(4) eq 0 then begin
			nums=indgen(nc,nr)          	 ; row major
		endif else begin	
			nums=transpose(indgen(nr,nc))    ; column major
		endelse
		x_plot_size=1.0/float(nc)
		y_plot_size=1.0/float(nr)
		x0=fltarr(nc,nr)
		y0=fltarr(nr,nc)
		for i=0,nr-1 do x0(0,i)=findgen(nc)*x_plot_size
		for i=0,nc-1 do y0(0,i)=1.0-(findgen(nr)+1.0)*y_plot_size
		y0=transpose(y0)
		plot_x0=x0(where(nums eq plot_num))
		plot_y0=y0(where(nums eq plot_num))
		mx=plot_x0+x*x_plot_size
		my=plot_y0+y*y_plot_size
	endelse
end
