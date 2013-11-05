;+
; GEO_LL2PIX
;
; Convert lat,lon to row,column number in seviri image.
; Based on routine from SPT_Tool.pro
;
; NAME:     
;   SPT_TL_GEOREF
;
; PURPOSE:
;   Convert geographical Latitude and Longitude into line and column number. 
;   The defaulf Reference Grid and Scanning Angle are from MSG VIS_IR.
;
; CATEGORY:
;   SPT Tool
;
; CALLING SEQUENCE:
;
;   Result = SPT_TL_GEOREF(Lat, Long [,/REF_LON] 
;                          [,/HRVIS] [,MFG_IR] [,/MFG_VIS] [,/VERBOSE] 
;                          [,/SILENT] )
;
; INPUTS:
;
;   Lat:        geographical latitude from -90 (South) to 90 (North)
;   Long:       geographical longitude from -90 (West) to 90 (East), 
;               when REF_LON=0 
;
; KEYWORD PARAMETERS:
;
;   REF_LON:    Centre of the rectified image Longitude
;   HRVIS:      MSG HRV Reference Grid
;   MFG_IR:     Meteosat First Generation Low Resolution Reference Grid and 
;               Scanning Angles
;   MFG_VIS:    Meteosat First Generation Visible Reference Grid and Scanning 
;               Angles
;   VERBOSE:    print verbose output                            
;   SILENT:     force silent behaviour
;   NPIXELS:	Return number of pixels in image
;   ZOFF        Set altitude above surface for which lat/lon provide
;               (deal with parallax) 
;   NOROUND	Do not round results to integer values
;
; OUTPUTS:
;
;   Line:       Line number from 1 to MaxLine; first line is South. In case 
;               of error value 9999 is returned 
;   Column:     Column number from 1 to MaxCol;  first Column is East. In 
;               case of error value 9999 is returned 
;
;   RETURN  :  0 -  Success
;             >0 -  Failure
;
; MODIFICATION HISTORY:
;   Written by: Marco Clerici, 10.02.04
;
; $Id: geo_ll2pix.pro 410 2010-09-14 13:50:11Z rsiddans $
;-

FUNCTION geo_ll2pix,Lat, Long, $
                        REF_LON=ref_lon, HRVIS=hrvis,       $
                        MFG_IR= mfg_ir,  MFG_VIS=mfg_vis,   $
                        VERBOSE=verbose, SILENT=silent,$
			npixels=i_npixels,zoff=zoff,$
			noround=noround


    routine = 'SPT_TL_GEOREF'
    
    rl_resol_angle  = 17.83                       ; default MSG
    i_npixels       = 3712   ; default low resolution
    
    ; Handle the keywords

	if n_elements(zoff) eq 0 then zoff=0d0
    IF KEYWORD_SET(silent) THEN VERBOSE=0

    IF KEYWORD_SET(HRVIS) THEN BEGIN
        i_npixels   = 11136
    ENDIF ELSE IF KEYWORD_SET(MFG_IR) THEN BEGIN
        i_npixels       = 2500
        rl_resol_angle  = 18.0                  
    ENDIF ELSE IF KEYWORD_SET(MFG_VIS) THEN BEGIN
        i_npixels       = 5000
        rl_resol_angle  = 18.0                  
    ENDIF

    IF KEYWORD_SET(REF_LON)  THEN rflon = ref_lon   ELSE  rflon = 0.d

    ; Check input arguments
    
    npoints = N_ELEMENTS(Lat)

    IF (npoints NE N_ELEMENTS(Long)) THEN BEGIN
        IF NOT keyword_set(SILENT) THEN $
            message, ' Lat/Long array must have the same dimension'
        RETURN, 1
    ENDIF

    idx = WHERE((Lat GT 90.) OR (Lat LT -90.))
    IF (idx(0) NE -1 ) THEN BEGIN
        IF NOT keyword_set(SILENT) THEN $
            message, ' Latitude out of range'
        RETURN, 1
    ENDIF
    idxl = WHERE((Long - rflon GT 90.) OR (Long- rflon  LT -90.),ndxl)

    ; Definitions

    CCMTL_EARTH_EQUATORIAL_RADIUS   = 6378.140d0+zoff
    CCMTL_EARTH_POLAR_RADIUS        = 6356.755d0+zoff
    CCMTL_ORBIT_DISTANCE            = 42164.0d
    REAL_ZERO                       = 0.d

    ; Preprocessing

    IMAGE_CENTRE_X      = i_npixels/2.d - .5
    IMAGE_CENTRE_Y      = i_npixels/2.d - .5

    rl_resol            = rl_resol_angle*!DTOR / i_npixels

    ; Set as Undef
    Column              = 9999
    line                = 9999

    ; Derive quantities from Earth radia 

    rl_eq_radius_sq     = (CCMTL_EARTH_EQUATORIAL_RADIUS * CCMTL_EARTH_EQUATORIAL_RADIUS)
    rl_pol_radius_sq    = (CCMTL_EARTH_POLAR_RADIUS * CCMTL_EARTH_POLAR_RADIUS)
    rl_epsilon_sq       = (rl_eq_radius_sq - rl_pol_radius_sq) / rl_eq_radius_sq
    rl_pol_eq_radius_sqs= rl_pol_radius_sq / rl_eq_radius_sq
    rl_eq_pol_radius_sqs= rl_eq_radius_sq / rl_pol_radius_sq

    ; Convert the longitude and latitude passed into radians

    RLat = Lat * !DTOR * 1.d
    RLong = (Long - rflon)*!DTOR * 1.d

    ; Convert geographical latitude to geocentric latitude

    RLat = ATAN(TAN(RLat) * rl_pol_eq_radius_sqs)

    ; Calculate some intermediate values

    rl_cos_lat = COS(RLat)
    rl_sin_lat = SIN(RLat)
    rl_cos_long = COS(RLong)
    rl_sin_long = SIN(RLong)

    ; Establish the reference level ( the Earth Radius at point P )

    rl_check    = (1.0 - (rl_epsilon_sq * rl_cos_lat * rl_cos_long))

    rl_ref_level = CCMTL_EARTH_POLAR_RADIUS / SQRT(1.0 - rl_epsilon_sq * rl_cos_lat * rl_cos_lat)

    ; Establish the reference vector (centered at the Satellite position and pointing to P)

    rl_ref_vector0 = CCMTL_ORBIT_DISTANCE - rl_ref_level * rl_cos_lat * rl_cos_long
    rl_ref_vector1 = - rl_ref_level * rl_cos_lat * rl_sin_long
    rl_ref_vector2 = rl_ref_level * rl_sin_lat

    ; Distance fron the Satellite to the point P
    ss = (rl_ref_vector0 * rl_ref_vector0) + (rl_ref_vector1 * rl_ref_vector1) + $
         (rl_ref_vector2 * rl_ref_vector2)

    rl_ref_normal = SQRT(SS)

    ; Check if the point is visible
    idxv = WHERE(ss + rl_ref_level*rl_ref_level GT CCMTL_ORBIT_DISTANCE*CCMTL_ORBIT_DISTANCE,ndxv)

    ; Calculate the relevant angles

    rl_vector_alpha = FLTARR(npoints)

    rl_vector_alpha = ATAN(rl_ref_vector1, rl_ref_vector0)

    idx = WHERE((ABS(rl_ref_vector1) LT REAL_ZERO) AND (ABS(rl_ref_vector0) LT REAL_ZERO))
    IF (idx(0) NE -1) THEN $
        rl_vector_alpha(idx) = ASIN(1.0d) 

    rl_vector_beta = ASIN(rl_ref_vector2 / rl_ref_normal)

    ; Use the angles to gain the pixel position

    Column0 = (rl_vector_alpha / rl_resol)
    line0   = (rl_vector_beta /  rl_resol)

if keyword_set(noround) then begin
	column=column0
	line=line0
endif else begin
    idx         = WHERE(Column0 GE 0.d)
    Column      = LONG(Column0) - 0.5

    IF (idx(0) NE -1) THEN $
        Column(idx) = LONG(Column0(idx)) + 0.5 

    idx         = WHERE(line0 GE 0.d)
    line        = LONG(line0) - 0.5
    IF (idx(0) NE -1) THEN $
        line(idx)   = LONG(line0(idx)) + 0.5 
endelse

    Column  = Column + IMAGE_CENTRE_X
    Line    = Line + IMAGE_CENTRE_Y

    IF KEYWORD_SET(VERBOSE) THEN BEGIN
        PRINT, routine + ' - Column      = ', Column
        PRINT, routine + ' - Row         = ', line
    ENDIF

    ; Patch for MSG/HRV: (0,0) point is [5566,5566]
    IF KEYWORD_SET(HRVIS) THEN BEGIN
        Column  = Column - 2
        Line    = Line - 2
    ENDIF
    
    ; Cast array to scalar, if only 1 element is present
    IF N_ELEMENTS(Column) EQ 1 THEN Column = Column(0)
    IF N_ELEMENTS(Line  ) EQ 1 THEN Line   = Line  (0)
    IF ( ndxv gt 0 ) then begin
	column(idxv)=-999
	line(idxv)=-999
    ENDIF
    IF ( ndxl gt 0 ) then begin
	column(idxl)=-999
	line(idxl)=-999
    ENDIF
	return,{column:column,line:line}
END
