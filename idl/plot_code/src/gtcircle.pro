;===============================================================================
;+
; GTCIRCLE.PRO
;
; This routine returns the latitude and longitude of points on a great
; circle through two other lat/lon points. The 2-d array returned is
; of size (2,n_elements(ANGS)), the first dimension giving latitude and
; longitude.
;
; 	If LON2 is not specified then LAT2 is taken as being the bearing
;		in degrees from point 1 (clockwise is positive).
;
;
; PARAMETERS
;	LAT1	Coords of the two points which define the circle
;	LON1	Coords of the two points which define the circle
;	LAT3	Coords of the two points which define the circle
;	LON3	Coords of the two points which define the circle
;	ANGS3	Angles subtended at the centre of the earth of points
;		along the great circle and the first point (radians)
;		(or distance from first point if DIST is specified).
;		If angs are positive, then the points go from point1 towards
;		point2. The points go from point1 away from point2 if ANGS
;		are negative
;
; KEYWORDS
;	DISTANCE Set this so THETAS represent surface distance (KM) from 
;		first, point along the great circle.
;	REARTH  Set equal to radius of earth to be used
;	PLOT	Set this to plot the great circle produced.
;
; R.S. 28/6/95
; $Id: gtcircle.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
;===============================================================================
function gtcircle,lat1,lon1,lat3,lon3,angs3,plot=plot,rearth=rearth,$
	distance=dist


	pi=!dpi
	dgr=pi/180.
	if n_elements(angs3) eq 0 then begin
;
; LON2 not specified so sort out a bearing...
;
		angs2=lon3
		dll=0.01
;		lon2=lon1+dll*sin((lat3-90.)*dgr)
;		lat2=lat1+dll*cos((lat3-90.)*dgr)
		lon2=lon1+dll*sin(lat3*dgr)
		lat2=lat1+dll*cos(lat3*dgr)
	endif else begin
		angs2=angs3
		lon2=lon3
		lat2=lat3
	endelse
	if not keyword_set(rearth) then rearth=6371.
	if keyword_set(dist) then angs=angs2/rearth else angs=angs2
;
; convert lats/lons to radians
;
	theta1=dgr*lat1
	theta2=dgr*lat2
	phi1=dgr*lon1
	phi2=dgr*lon2

	tan_theta1=tan(theta1)
	tan_theta2=tan(theta2)
	cos_phi1=cos(phi1)
	sin_phi1=sin(phi1)
	cos_phi2=cos(phi2)
	sin_phi2=sin(phi2)
	cos_theta1=cos(theta1)
	sin_theta1=sin(theta1)
	cos_theta2=cos(theta2)
	sin_theta2=sin(theta2)
;
; get lat/lon of unit normal to great circle plane
;
	phi_n=atan(tan_theta1*cos_phi2-tan_theta2*cos_phi1,$
		tan_theta2*sin_phi1-tan_theta1*sin_phi2)
	if tan_theta1 ne 0. then $
		theta_n=atan(-(cos_phi1*cos(phi_n)+sin_phi1*sin(phi_n)),$
			tan_theta1) $
	else if tan_theta2 ne 0. then $
		theta_n=atan(-(cos_phi2*cos(phi_n)+sin_phi2*sin(phi_n)),$
			tan_theta2) $
	else theta_n=pi/2.
;
; rotate first point into equatorial plane to find first
; angle which should be rotated back into great circle plane in next step.
;
	xdd=cos_theta1*cos_phi1
	ydd=cos_theta1*sin_phi1
	zdd=sin_theta1

	rota=-(pi/2-theta_n)
	cos_rota=cos(rota)
	sin_rota=sin(rota)
	cos_phi_n=cos(phi_n)
	sin_phi_n=sin(phi_n)

	xd=xdd*cos_phi_n+ydd*sin_phi_n
	yd=-xdd*sin_phi_n+ydd*cos_phi_n
	zd=zdd

	x=xd*cos_rota+zd*sin_rota
	y=yd
	z=-xd*sin_rota+zd*cos_rota

	ang1=atan(x,y)
;
; do same to second point to get sign of angles....
;
	xdd=cos_theta2*cos_phi2
	ydd=cos_theta2*sin_phi2
	zdd=sin_theta2

	rota=-(pi/2-theta_n)
	cos_rota=cos(rota)
	sin_rota=sin(rota)
	cos_phi_n=cos(phi_n)
	sin_phi_n=sin(phi_n)

	xd=xdd*cos_phi_n+ydd*sin_phi_n
	yd=-xdd*sin_phi_n+ydd*cos_phi_n
	zd=zdd

	x=xd*cos_rota+zd*sin_rota
	y=yd
	z=-xd*sin_rota+zd*cos_rota

	ang2=atan(x,y)
	if ang2 lt ang1 then angs=-angs
;
; generate circle lying in equatorial plane, in cartersian coods...
; z axis points to N-pole, x axis points to 0N,0E, y-axis to
; 0N,90E
;
	angs=angs+ang1
	z=0.
	x=sin(angs)
	y=cos(angs)
;
; rotate into the great circle plane
; first rotate about x-axis by 90-lat of normal to cicle plane, 
; then about z axis by the -longitude of the normal.
;
	rota=pi/2.-theta_n
	cos_rota=cos(rota)
	sin_rota=sin(rota)
	cos_phi_n=cos(-phi_n)
	sin_phi_n=sin(-phi_n)

	xd=x*cos_rota+z*sin_rota
	yd=y
	zd=-x*sin_rota+z*cos_rota

	xdd=xd*cos_phi_n+yd*sin_phi_n
	ydd=-xd*sin_phi_n+yd*cos_phi_n
	zdd=zd
;
; now work out lat,lon of these points
;
	lons2=atan(ydd,xdd)/dgr
	lats2=asin(zdd)/dgr
;
; plot if required
;
	if keyword_set(plot) then begin
		set_a4,/land
		if !d.name eq 'PS' then th=4 else th=2
		map_set,lat1,lon1,lim=[20,-45,70,5]
		map_continents,mlinethick=th
		oplot,lons2,lats2
		plots,lon1,lat1,psym=2
		xyouts,lon1,lat1,'A',align=1.	
		plots,lon2,lat2,psym=2
		xyouts,lon2,lat2,'B',align=1.	
	endif
	coords=[transpose(lats2),transpose(lons2)]
	return,coords
end
