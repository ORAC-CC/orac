;==============================================================================
;+
;	pro ERROR_BAR
;
;	Description
; This program plots points with x and y error bars onto the current plot.
; Modified version to include vertical marks at end of bars
; The program works with vectors of points.
; Error bars run from X-DX to X+DX and Y-DY to Y+DY
;
;	Arguments
;   X	The x coord(s) of the point
;   Y   The y coord(s) of the point
;   DX1  Estimated error on X
;   DY1  Estimated error on Y
;
;	Keywords
;  I  THICK As PLOT keyword
;  I  COLOR As PLOT keyword
;  I  LINESTYLE As PLOT keyword
;  I  DXV Vertical marks on end of horizontal bars
;  I  DYH	Horizontal marks on end of vertical bars
;  I  PSTYLE structure containing colors, linestyles and thickness
;  I  IND index of pstyle arrays to use
;
;	Date
;	R. Siddans: 1/1/97
;	B. Latter : 20th Feb 2001 + added bar ends & pstyles
; $Id: error_bar.pro 222 2009-06-08 15:56:30Z blatter $
;-
;==============================================================================
pro error_bar,x,y,dx1,dy1,thick=thick,color=color,linestyle=linest,$
		dxv=dxv,dyh=dyh,pstyle=pstyle,ind=ind
	if n_elements(color) eq 0 then color=!p.color ; !c(0)
	if n_elements(thick) eq 0 then thick=!p.thick
	if n_elements(linest) eq 0 then linest=0
	if (size(dx1))(1) eq 0 then dx1=0
	if (size(dy1))(1) eq 0 then dy1=0
	if n_elements(dx1) eq 1 then dx=replicate(dx1,n_elements(x)) $
	else dx=dx1
	if n_elements(dy1) eq 1 then dy=replicate(dy1,n_elements(y)) $
	else dy=dy1
;
; get axix min/max values
;
	xmin=!x.crange(0)
	xmax=!x.crange(1)
	ymin=!y.crange(0)
	ymax=!y.crange(1)
	if !x.type eq 1 then begin
		xmin=10.^xmin
		xmax=10.^xmax
	endif
	if !y.type eq 1 then begin
		ymin=10.^ymin
		ymax=10.^ymax
	endif
	if keyword_set(dxv) then dxv=(ymax-ymin)/50.
	if keyword_set(dyh) then dyh=(xmax-xmin)/50.
	for i=0,n_elements(x)-1 do begin
		x1=max([x(i)-dx(i),xmin])
		x2=min([x(i)+dx(i),xmax])
		y1=max([y(i)-dy(i),ymin])
		y2=min([y(i)+dy(i),ymax])
;plot x error bar
		plots,[x1,x2],[y(i),y(i)],noclip=0,$
			thick=thick,color=color,linest=linest     
		if keyword_set(dxv) then begin
;plot vertical marks on ends of x error bar
			plots,[x1,x1],[y(i)-dxv,y(i)+dxv],noclip=0,$
				thick=thick,color=color,linest=linest     
			plots,[x2,x2],[y(i)-dxv,y(i)+dxv],noclip=0,$
				thick=thick,color=color,linest=linest     
		endif
;plot y error bar
		plots,[x(i),x(i)],[y1,y2],noclip=0,$
			thick=thick,color=color,linest=linest     
		if keyword_set(dyh) then begin
;plot vertical marks on ends of x error bar
			plots,[x(i)-dyh,x(i)+dyh],[y1,y1],noclip=0,$
				thick=thick,color=color,linest=linest     
			plots,[x(i)-dyh,x(i)+dyh],[y2,y2],noclip=0,$
				thick=thick,color=color,linest=linest     
		endif
	endfor
end


