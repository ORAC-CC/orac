;+
; NAME:     
;   GEO_PIX2LL based on Eumetsat tool
;   SPT_TL_REFGEO
;
; PURPOSE:
;   Convert line and column number into geographical Latitude and Longitude . 
;   The defaulf Reference Grid and Scanning Angle are from MSG VIS_IR
;
; CATEGORY:
;   SPT Tool
;
; CALLING SEQUENCE:
;
;   Result = GEO_PIX2LL(Line, Column, Lat, Long [,/REF_LON] [,/HRVIS] 
;                          [,/MFG_IR]  [,MFG_VIS] [,/VERBOSE] [,/SILENT])
;
; INPUTS:
;   Line:       Array of line number from 1 to MaxLine; first line is South. 
;   Column:     Array of column number from 1 to MaxCol; first Column is East .
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
;   NPIXELS: 	Return number of pixels in image
;   ZOFF	Set altitude above surface for which lat/lon for
;		given pixel required (deal with parallax)
;
; OUTPUTS:
;
;   Lat:    array of geographical latitude from -90 (South) to 90 (North). 
;           In case of error, value 999.d is returned 
;   Long:   array of geographical longitude from -90 (West) to 90 (East), 
;           when REF_LON=0.In case of error, value 999.d is returned 
;
;   RETURN  :  0 -  Success
;             >0 -  Failure
;
; MODIFICATION HISTORY:
;   Written by: Marco Clerici, 11.02.04
;
; $Id: geo_pix2ll.pro 410 2010-09-14 13:50:11Z rsiddans $
;-

FUNCTION GEO_PIX2LL, Line, Column, Lat, Lon,    $
                        REF_LON=ref_lon,  HRVIS=hrvis,  $
                        MFG_IR= mfg_ir,  MFG_VIS=mfg_vis, $
                        VERBOSE=verbose, SILENT=silent,npixels=i_npixels,zoff=zoff

    routine = 'SPT_TL_REFGEO'

    ; Handle the keywords

    rl_resol_angle  = 17.83                         ; default MSG
    i_npixels       = 3712			; default low resolution

    if n_elements(zoff) eq 0 then zoff=0
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

    ; Patch for MSG/HRV: (0,0) point is [5566,5566]
     IF KEYWORD_SET(HRVIS) THEN BEGIN
         Column  = Column + 2
         Line    = Line + 2
     ENDIF

    ; Definitions

    CCMTL_EARTH_EQUATORIAL_RADIUS = 6378.140d0+zoff
    CCMTL_EARTH_POLAR_RADIUS      = 6356.755d0+zoff
    CCMTL_ORBIT_DISTANCE          = 42164.0d
    REAL_ZERO                     = 0.d

    ; Preprocessing

    npoints             = N_ELEMENTS(Line)

    IF (npoints NE N_ELEMENTS(Column)) THEN BEGIN
        IF NOT keyword_set(SILENT) THEN $
            message,/info,' Line/Column array must have the same dimension'
        RETURN, 1
    ENDIF


    IMAGE_CENTRE_X      = i_npixels * 0.5 - .5
    IMAGE_CENTRE_Y      = i_npixels * 0.5 - .5

;    print ,' IMAGE_CENTRE_Y ',  IMAGE_CENTRE_Y 
;    print,'i_npixels',i_npixels
    rl_resol            = rl_resol_angle*!DTOR / i_npixels
;    print,' rl_resol npoints  ', rl_resol,npoints  

    rl_eq_radius_sq     = (CCMTL_EARTH_EQUATORIAL_RADIUS * CCMTL_EARTH_EQUATORIAL_RADIUS)
    rl_pol_radius_sq    = (CCMTL_EARTH_POLAR_RADIUS * CCMTL_EARTH_POLAR_RADIUS)
    rl_epsilon_sq       = (rl_eq_radius_sq - rl_pol_radius_sq) / rl_eq_radius_sq
    rl_pol_eq_radius_sqs= rl_pol_radius_sq / rl_eq_radius_sq
    rl_eq_pol_radius_sqs= rl_eq_radius_sq / rl_pol_radius_sq

    ; Calculate satellite view angles 
    rl_vector_alpha = double(rl_resol * (column - IMAGE_CENTRE_X))
    rl_vector_beta =  double(rl_resol * (line - IMAGE_CENTRE_Y))

    ; Calculate some handy intermediate coefficients

    cos_rl_vector_beta=COS(rl_vector_beta)
    cos_rl_vector_alpha=COS(rl_vector_alpha)
    sin_rl_vector_beta=SIN(rl_vector_beta)
    sin_rl_vector_alpha=SIN(rl_vector_alpha)
    rl_acoeff = (cos_rl_vector_beta * cos_rl_vector_beta) + $
                ((rl_eq_pol_radius_sqs * sin_rl_vector_beta) * sin_rl_vector_beta)

    rl_bcoeff = (cos_rl_vector_alpha * cos_rl_vector_beta * CCMTL_ORBIT_DISTANCE)

    ; Get the normal for the vector

    rl_ref_check = (rl_bcoeff * rl_bcoeff) + $
                   (rl_acoeff * (rl_eq_radius_sq -   (CCMTL_ORBIT_DISTANCE * CCMTL_ORBIT_DISTANCE)))

    idx         = WHERE(rl_ref_check LT REAL_ZERO)

;    IF (idx(0) NE -1 AND NOT KEYWORD_SET(SILENT)) THEN BEGIN
;        IF NOT keyword_set(SILENT) THEN $
;            message,/info,' Out of visible disk/globe REFGEO'
;        RETURN, 1
;    ENDIF

    sss = ((rl_bcoeff * rl_bcoeff) + $
          (rl_acoeff * (rl_eq_radius_sq - (CCMTL_ORBIT_DISTANCE * CCMTL_ORBIT_DISTANCE))))

    rl_ref_normal = (rl_bcoeff - SQRT(sss)) / rl_acoeff

    ; Estimate the vector

    rl_ref_normal_cos_rl_vector_beta=cos_rl_vector_beta*rl_ref_normal
    rl_ref_vector0 = CCMTL_ORBIT_DISTANCE - (cos_rl_vector_alpha * rl_ref_normal_cos_rl_vector_beta)
    rl_ref_vector1 = - rl_ref_normal_cos_rl_vector_beta * sin_rl_vector_alpha
    rl_ref_vector2 = rl_ref_normal * sin_rl_vector_beta

    ss01 = rl_ref_vector0 * rl_ref_vector0 + rl_ref_vector1 * rl_ref_vector1

    ; Calculate the reference level

    ss = ss01 + (rl_ref_vector2 * rl_ref_vector2)

    rl_ref_level = SQRT(ss)

    rl_xycoeff = SQRT(ss01)

    rl_long = ATAN(rl_ref_vector1, rl_ref_vector0)
    
    idx = WHERE( (ABS(rl_ref_vector1) LT REAL_ZERO) AND (ABS(rl_ref_vector0) LT REAL_ZERO))

    IF (idx(0) NE -1 ) THEN $
        rl_long(idx) = ASIN(1.0) 

    idx = WHERE(rl_ref_vector0 LT 0.0)

    IF (idx(0) NE -1) THEN $
        rl_long(idx) = rl_long(idx) + SIGN((2.0*ASIN(1.0)),rl_ref_vector1(0))

    ; Convert from geocentric latitude to geographical latitude

    rl_lat  = ATAN((rl_ref_vector2 * rl_eq_pol_radius_sqs), rl_xycoeff) 

    idx    = WHERE((rl_xycoeff / rl_ref_level) EQ REAL_ZERO)

    IF (idx(0) NE -1) THEN BEGIN
        rl_lat(idx) = SIGN((ASIN(1.0)), rl_ref_vector2(0))
        rl_long(idx) = 0.0
    ENDIF

    ; Set the outgoing latitude and longitude

    lat    = rl_lat  * !RADEG
    lon    = rl_long * !RADEG + rflon

    IF KEYWORD_SET(VERBOSE) THEN BEGIN
        PRINT, routine + ' - Lat         = ', Lat   
        PRINT, routine + ' - Long        = ', Lon
    ENDIF
    ; Cast array to scalar, if only 1 element is present
    IF N_ELEMENTS(Lat ) EQ 1 THEN Lat      = Lat(0)
    IF N_ELEMENTS(Lon ) EQ 1 THEN Lon      = Lon(0)

    return,{lat:lat,lon:lon}
END
