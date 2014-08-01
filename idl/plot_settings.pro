;+
; NAME:
;   PLOT_SETTINGS
;
; PURPOSE:
;   Specifies the properties of diagrams made with PLOT_ORAC. This routine needs
;   to be recompiled after any changes are made. 
;
; CATEGORY:
;   ORAC plotting tools
;
; CALLING SEQUENCE:
;   settings = PLOT_SETTINGS(suffix, instrument)
;
; INPUTS:
;   suffix     = The file suffix for the file to be plotted. Possibilities are:
;      ALB, CLF, CONFIG, GEO, LOC, LSF, LWRTM, MSI, PRTM, SWRTM, UV, 
;      PRIMARY, SECONDARY
;   instrument = A string specifying the swath to be plotted. This is expected
;      to be one of the values of $label in trunk/tools/test-preproc.sh
;
; OPTIONAL INPUTS:
;   None.
;	
; KEYWORD PARAMETERS:
;   None.
;	
; OUTPUTS:
;   suffix = An array of structures (one per field in the file) each containing:
;      NAME:    Name of the field in the NCDF file.
;      MODE:    The format of the data, denoting the style of plot. Values are:
;         0) Line plot [n]; 1) One map plot [nx,ny];
;         2) Series of map plots [nx,ny,n]; 3) One map plot [nxy]; 
;         4) Series of map plots [n,nxy]; 5) Levels of map plots [nl,nxy];
;         6) Two series of map plots [n,nl,nxy].
;      TITLE:   The description to be written above the plot.
;      LOG:     If nonzero, use a logarithmic colourbar.
;      BOTTOM:  If nonzero, use a colourbar that does not start at zero.
;      ABS:     If nonzero, use a colourbar centred on zero.
;      FILTER:  When plotting PRIMARY or SECONDARY, apply a filter. The given
;         value sets which bits of the quality control flag to discount a point
;         from plotting.
;      BTF:     The print format of the colourbar labels. Default (G0.4).
;      FULL:    If nonzero, use a typical [minimum value, maximum value] 
;         colourbar range rather than the preferred percentile version.
;      RANGE:   Override the automated colourbar range selection.
;      BLABELS: For flag data, the descriptions to print on the colourbar rather
;         than numbers.
;      NLEVELS: The number of colour levels to be used in the plot. Default 250.
; 
; OPTIONAL OUTPUTS:
;   None.
;
; RESTRICTIONS:
;   This routine needs to be recompiled after any changes are made for them to
;   be expressed in plotting. Recommendations on how to avoid that gladly 
;   accepted.
;
; MODIFICATION HISTORY:
;   15 Jul 2014 - ACP: Initial version (povey@atm.ox.ac.uk).
;   22 Jul 2014 - ACP: More sensible array indexing (using variable i). Removed
;      Ch number fields from plotting.
;   25 Jul 2014 - ACP: Added additional illumination flags.
;   28 Jul 2014 - ACP: Added falsecolour and difference plots.
;-
FUNCTION PLOT_SETTINGS, suffix, inst
   ON_ERROR, 2
   COMPILE_OPT LOGICAL_PREDICATE, STRICTARR, STRICTARRSUBS

   ;; cut out first full stop
   suff=STRMID(suffix,STRPOS(suffix,'.')+1)
   suff=STRMID(suff,0,STRPOS(suff,'.'))
   suff=STRUPCASE(suff)

   out=REPLICATE({name:'', mode:-1, title:'', log:0, bottom:0, abs:0, filter:0, $
        btf:'(g0.4)', full:0, range:[!values.f_nan,0.], $
        blabels: STRARR(20), nlevels:250},35)

   ;; Mode key:
   ;; 0) Line plot [n]
   ;; 1) One map plot [nx,ny]
   ;; 2) Series of map plots [nx,ny,n]
   ;; 3) One map plot [nxy]
   ;; 4) Series of map plots [n,nxy]
   ;; 5) Levels of map plots [nl,nxy]
   ;; 6) Two series of map plots [n,nl,nxy]
   ;; 10) Other
   i=0
   case suff of
      'ALB': begin
         out[i].name='alb_data'
         out[i].mode=2
         out[i].log=1
         out[i].title=FMT('Surface albedo','%')
         ++i

         out[i].name='emis_data'
         out[i].mode=2
         out[i].log=1
         out[i].title=FMT('Surface emissivity','%')
         ++i

         out[i].name='alb_abs_ch_numbers'
         out[i].mode=0
         out[i].full=1
         ++i

         out[i].name='emis_abs_ch_numbers'
         out[i].mode=0
         out[i].full=1
         ++i
      end
      'CLF': begin
         out[i].name='cflag'
         out[i].mode=1
         out[i].full=1
         out[i].title=FMT('Cloud flag')
         out[i].btf=''
         out[i].blabels[0:1]=['Clear','Cloudy']
         out[i].range=[0,1]
         out[i].nlevels=2
         ++i
      end
      'CONFIG': begin
         out[i].name='msi_instr_ch_numbers'
         out[i].mode=0
         out[i].full=1
         ++i

         out[i].name='msi_abs_ch_numbers'
         out[i].mode=0
         out[i].full=1
         ++i

         out[i].name='msi_abs_ch_wl'
         out[i].mode=0
         out[i].full=1
         ++i

         out[i].name='msi_ch_swflag'
         out[i].mode=0
         out[i].full=1
         ++i

         out[i].name='msi_ch_lwflag'
         out[i].mode=0
         out[i].full=1
         ++i

         out[i].name='msi_ch_procflag'
         out[i].mode=0
         out[i].full=1
         ++i

;         out[i].name='alb_abs_ch_numbers'
;         out[i].mode=0
;         out[i].full=1
;         ++i

;         out[i].name='emis_abs_ch_numbers'
;         out[i].mode=0
;         out[i].full=1
;         ++i
      end
      'GEO': begin
         out[i].name='solzen'
         out[i].mode=2
         out[i].full=1
         out[i].title=FMT('Solar zenith angle','!9'+STRING(176b)+'!X')
         ++i

         out[i].name='satzen'
         out[i].mode=2
         out[i].full=1
         out[i].title=FMT('Satellite zenith angle','!9'+STRING(176b)+'!X')
         ++i

         out[i].name='solaz'
         out[i].mode=2
         out[i].full=1
         out[i].title=FMT('Solar azimuth angle','!9'+STRING(176b)+'!X')
         ++i

         out[i].name='relazi'
         out[i].mode=2
         out[i].full=1
         out[i].title=FMT('Relative azimuth angle','!9'+STRING(176b)+'!X')
         ++i
      end
      'LOC': begin
         out[i].name='lon'
         out[i].mode=1
         out[i].full=1
         out[i].title=FMT('Longitude','!9'+STRING(176b)+'!X')
         ++i

         out[i].name='lat'
         out[i].mode=1
         out[i].full=1
         out[i].title=FMT('Latitude','!9'+STRING(176b)+'!X')
         ++i
      end
      'LSF': begin
         out[i].name='lsflag'
         out[i].mode=1
         out[i].full=1
         out[i].title=FMT('Land sea flag')
         out[i].btf=''
         out[i].blabels[0:1]=['Sea','Land']
         out[i].range=[0,1]
         out[i].nlevels=2
         ++i
      end
      'LWRTM': begin
;         out[i].name='lw_channel_abs_ids'
;         out[i].mode=0
;         out[i].full=1
;         ++i

;         out[i].name='lw_channel_instr_ids'
;         out[i].mode=0
;         out[i].full=1
;         ++i

;         out[i].name='lw_channel_wvl'
;         out[i].mode=0
;         out[i].full=1
;         ++i

;         out[i].name='counter_lw'
;         out[i].full=1
;         out[i].mode=3
;         ++i

         out[i].name='solza_lw'
         out[i].full=1
         out[i].mode=4
         out[i].title=FMT('RTM LW solar azimuth angle','!9'+STRING(176b)+'!X')
         ++i

         out[i].name='satza_lw'
         out[i].full=1
         out[i].mode=4
         out[i].title=FMT('RTM LW satellite azimuth angle','!9'+STRING(176b)+'!X')
         ++i

         out[i].name='relazi_lw'
         out[i].full=1
         out[i].mode=4
         out[i].title=FMT('RTM LW relative azimuth angle','!9'+STRING(176b)+'!X')
         ++i

         out[i].name='emiss_lw'
         out[i].log=1
         out[i].mode=4
         out[i].title=FMT('RTM LW atmospheric emissivity','%')
         ++i

         out[i].name='tac_lw'
         out[i].full=1
         out[i].mode=6
         ++i

         out[i].name='tbc_lw'
         out[i].full=1
         out[i].mode=6
         ++i

         out[i].name='rbc_up_lw'
         out[i].full=1
         out[i].mode=6
         ++i

         out[i].name='rac_up_lw'
         out[i].full=1
         out[i].mode=6
         ++i

         out[i].name='rac_down_lw'
         out[i].full=1
         out[i].mode=6
         ++i
      end
      'MSI': begin
         out[i].name='msi_data'
         out[i].mode=2
         out[i].title=FMT('Imager data','% || K')
         out[i].full=1
         ++i

;         out[i].name='msi_instr_ch_numbers'
;         out[i].mode=0
;         out[i].full=1
;         ++i

;         out[i].name='msi_abs_ch_numbers'
;         out[i].mode=0
;         out[i].full=1
;         ++i

;         out[i].name='msi_abs_ch_wl'
;         out[i].mode=0
;         out[i].full=1
;         ++i

;         out[i].name='msi_ch_swflag'
;         out[i].mode=0
;         out[i].full=1
;         ++i

;         out[i].name='msi_ch_lwflag'
;         out[i].mode=0
;         out[i].full=1
;         ++i

;         out[i].name='msi_ch_procflag'
;         out[i].mode=0
;         out[i].full=1
;         ++i

         out[i].name='time_data'
         out[i].mode=1
         out[i].full=1
         ++i
      end
      'PRTM': begin
;         out[i].name='i_pw'
;         out[i].mode=3
;         out[i].full=1
;         ++i

;         out[i].name='j_pw'
;         out[i].mode=3
;         out[i].full=1
;         ++i

;         out[i].name='counter_pw'
;         out[i].mode=3
;         out[i].full=1
;         ++i

         out[i].name='lon_pw'
         out[i].mode=3
         out[i].full=1
         out[i].title=FMT('RTM longitude','!9'+STRING(176b)+'!X')
         ++i

         out[i].name='lat_pw'
         out[i].mode=3
         out[i].full=1
         out[i].title=FMT('RTM latitude','!9'+STRING(176b)+'!X')
         ++i

         out[i].name='skint_pw'
         out[i].mode=3
         out[i].bottom=1
         out[i].title=FMT('RTM skin temperature','K')
         ++i

         out[i].name='explnsp_pw'
         out[i].mode=3
         out[i].bottom=1
         out[i].title=FMT('RTM surface pressure','hPa')
         ++i

         out[i].name='lsf_pw'
         out[i].mode=3
         out[i].full=1
         out[i].title=FMT('RTM surface albedo','%')
         ++i

         out[i].name='satzen_pw'
         out[i].mode=3
         out[i].full=1
         out[i].title=FMT('RTM satellite zenith angle','!9'+STRING(176b)+'!X')
         ++i

         out[i].name='solzen_pw'
         out[i].mode=3
         out[i].full=1
         out[i].title=FMT('RTM solar zenith angle','!9'+STRING(176b)+'!X')
         ++i

         out[i].name='pprofile_lev'
         out[i].mode=5
         out[i].bottom=1
         out[i].title=FMT('RTM pressure','hPa')
         ++i

         out[i].name='tprofile_lev'
         out[i].mode=5
         out[i].bottom=1
         out[i].title=FMT('RTM temperature','K')
         ++i

         out[i].name='gphprofile_lev'
         out[i].mode=5
         out[i].bottom=1
         out[i].title=FMT('RTM geopotential','m')
         ++i
      end
      'SWRTM': begin
;         out[i].name='sw_channel_abs_ids'
;         out[i].mode=0
;         out[i].full=1
;         ++i

;         out[i].name='sw_channel_instr_ids'
;         out[i].mode=0
;         out[i].full=1
;         ++i

;         out[i].name='sw_channel_wvl'
;         out[i].mode=0
;         out[i].full=1
;         ++i

;         out[i].name='counter_sw'
;         out[i].full=1
;         out[i].mode=3
;         ++i

         out[i].name='solza_sw'
         out[i].full=1
         out[i].mode=4
         out[i].title=FMT('RTM SW solar azimuth angle','!9'+STRING(176b)+'!X')
         ++i

         out[i].name='satza_sw'
         out[i].full=1
         out[i].mode=4
         out[i].title=FMT('RTM SW satellite azimuth angle','!9'+STRING(176b)+'!X')
         ++i

         out[i].name='relazi_sw'
         out[i].full=1
         out[i].mode=4
         out[i].title=FMT('RTM SW relative azimuth angle','!9'+STRING(176b)+'!X')
         ++i

         out[i].name='tac_sw'
         out[i].mode=6
         out[i].full=1
         ++i

         out[i].name='tbc_sw'
         out[i].mode=6
         out[i].full=1
         ++i
      end
      'UV': begin
         out[i].name='uscan'
         out[i].mode=1
         out[i].full=1
         ++i

         out[i].name='vscan'
         out[i].mode=1
         out[i].full=1
         ++i
      end
      'PRIMARY': begin
         out.filter=64

         out[i].name='cot'
         out[i].mode=1
         out[i].log=1
         out[i].title=FMT('Optical thickness','-')
         ++i

         out[i].name='cot_uncertainty'
         out[i].mode=1
         out[i].title=FMT('Optical thickness !Ms!N','-')
         ++i

         out[i].name='ref'
         out[i].mode=1
         out[i].title=FMT('Effective radius','!Mm!Xm')
         ++i

         out[i].name='ref_uncertainty'
         out[i].mode=1
         out[i].title=FMT('Effective radius !Ms!N','!Mm!Xm')
         ++i

         out[i].name='ctp'
         out[i].mode=1
         out[i].title=FMT('Cloud top pressure','Pa')
         ++i

         out[i].name='ctp_uncertainty'
         out[i].mode=1
         out[i].title=FMT('Cloud top pressure !Ms!N','Pa')
         ++i

         out[i].name='stemp'
         out[i].mode=1
         out[i].bottom=1
         out[i].title=FMT('Surface temperature','K')
         ++i

         out[i].name='stemp_uncertainty'
         out[i].mode=1
         out[i].title=FMT('Surface temperature !Ms!N','K')
         ++i

         out[i].name='cwp'
         out[i].mode=1
         out[i].title=FMT('Cloud water path','g m!E-2!N')
         ++i

         out[i].name='cwp_uncertainty'
         out[i].mode=1
         out[i].title=FMT('Cloud water path !Ms!N','g m!E-2!N')
         ++i

         out[i].name='cc_total'
         out[i].mode=1
         out[i].title=FMT('Cloud fraction','%')
         ++i

         out[i].name='cc_total_uncertainty'
         out[i].mode=1
         out[i].title=FMT('Cloud fraction !Ms!N','%')
         ++i

         out[i].name='cth'
         out[i].mode=1
         out[i].title=FMT('Cloud top height','km')
         ++i

         out[i].name='cth_uncertainty'
         out[i].mode=1
         out[i].title=FMT('Cloud top height !Ms!N','K')
         ++i

         out[i].name='ctt'
         out[i].mode=1
         out[i].title=FMT('Cloud top temperature','K')
         ++i

         out[i].name='ctt_uncertainty'
         out[i].mode=1
         out[i].title=FMT('Cloud top temperature !Ms!N','K')
         ++i

         out[i].name='convergence'
         out[i].mode=1
         out[i].full=1
         out[i].title=FMT('Convergence flag')
         out[i].btf=''
         out[i].blabels[0:1]=['Yes','No']
         out[i].range=[0,1]
         out[i].nlevels=2
         ++i

         out[i].name='niter'
         out[i].mode=1
         out[i].full=1
         out[i].title=FMT('Iterations','-')
         ++i

         out[i].name='qcflag'
         out[i].mode=1
         out[i].full=1
         out[i].title=FMT('Quality control flag','-')
         ++i

         out[i].name='phase'
         out[i].mode=1
         out[i].full=1
         out[i].title=FMT('Phase selection')
         out[i].btf=''
         out[i].blabels[0:1]=['Ice','Water']
         out[i].range=[0,1]
         out[i].nlevels=2
         ++i

         out[i].name='costja'
         out[i].mode=1
         out[i].title=FMT('A priori cost','-')
         ++i

         out[i].name='costjm'
         out[i].mode=1
         out[i].title=FMT('Measurement cost','-')
         ++i

         out[i].name='lsflag'
         out[i].mode=1
         out[i].full=1
         out[i].title=FMT('Land sea flag')
         out[i].btf=''
         out[i].blabels[0:1]=['Sea','Land']
         out[i].range=[0,1]
         out[i].nlevels=2
         ++i

         out[i].name='illum'
         out[i].mode=1
         out[i].full=1
         out[i].title=FMT('Illumination flag')
         out[i].btf=''
         out[i].blabels[0:11]=['Day','Twilight','Night','Daynore','Day -VIS1', $
                               'Day -VIS2','Day -IR1','Day -IR2','Day -IR3', $
                               'Night -IR1','Night -IR2','Night -IR3']
         out[i].range=[1,12]
         out[i].nlevels=12
         ++i

         out[i].name='satellite_zenith_view_no1'
         out[i].mode=1
         out[i].full=1
         out[i].title=FMT('Nadir satellite zenith','!9'+STRING(176b)+'!X')
         ++i

         out[i].name='solar_zenith_view_no1'
         out[i].mode=1
         out[i].full=1
         out[i].title=FMT('Nadir solar zenith','!9'+STRING(176b)+'!X')
         ++i

         out[i].name='rel_azimuth_view_no1'
         out[i].mode=1
         out[i].full=1
         out[i].title=FMT('Nadir relative azimuth','!9'+STRING(176b)+'!X')
         ++i

         out[i].name='lon'
         out[i].mode=1
         out[i].full=1
         out[i].title=FMT('Longitude','!9'+STRING(176b)+'!X')
         ++i

         out[i].name='lat'
         out[i].mode=1
         out[i].full=1
         out[i].title=FMT('Latitude','!9'+STRING(176b)+'!X')
         ++i

         out[i].name='time'
         out[i].mode=1
         out[i].full=1
         out[i].title=FMT('Observation time','JDN')
         ++i
      end
      'SECONDARY': begin 
         out[i].name='scanline_u'
         out[i].mode=1
         out[i].full=1
         out[i].title=FMT('Scan u','-')
         ++i

         out[i].name='scanline_v'
         out[i].mode=1
         out[i].full=1
         out[i].title=FMT('Scan v','-')
         ++i

         out[i].name='cot_ap'
         out[i].mode=1
         out[i].title=FMT('A priori: Optical thickness','-')
         ++i

         out[i].name='cot_fg'
         out[i].mode=1
         out[i].title=FMT('First guess: Optical thickness','-')
         ++i

         out[i].name='ref_ap'
         out[i].mode=1
         out[i].title=FMT('A priori: Effective radius','!Mm!Xm')
         ++i

         out[i].name='ref_fg'
         out[i].mode=1
         out[i].title=FMT('First guess: Effective radius','!Mm!Xm')
         ++i

         out[i].name='ctp_ap'
         out[i].mode=1
         out[i].title=FMT('A priori: Cloud top pressure','Pa')
         ++i

         out[i].name='ctp_fg'
         out[i].mode=1
         out[i].title=FMT('First guess: cloud top pressure','Pa')
         ++i

         out[i].name='reflectance_residual_in_channel_no_'
         out[i].mode=1
         out[i].abs=1
         out[i].title=FMT('Ref residual Ch 1','%')
         ++i

         out[i].name='reflectance_residual_in_channel_no_'
         out[i].mode=1
         out[i].abs=1
         out[i].title=FMT('Ref residual Ch 2','%')
         ++i

         out[i].name='reflectance_residual_in_channel_no_'
         out[i].mode=1
         out[i].abs=1
         out[i].title=FMT('Ref residual Ch 3','%')
         ++i

         out[i].name='brightness_temperature_residual_in_channel_no_'
         out[i].mode=1
         out[i].abs=1
         out[i].btf='(i3)'
         out[i].title=FMT('BT residual Ch 4','K')
         ++i

         out[i].name='brightness_temperature_residual_in_channel_no_'
         out[i].mode=1
         out[i].abs=1
         out[i].btf='(i3)'
         out[i].title=FMT('BT residual Ch 5','K')
         ++i

         out[i].name='brightness_temperature_residual_in_channel_no_'
         out[i].mode=1
         out[i].abs=1
         out[i].btf='(i3)'
         out[i].title=FMT('BT residual Ch 6','K')
         ++i

         out[i].name='reflectance_in_channel_no_'
         out[i].mode=1
         out[i].title=FMT('Reflectance Ch 1','%')
         ++i

         out[i].name='reflectance_in_channel_no_'
         out[i].mode=1
         out[i].title=FMT('Reflectance Ch 2','%')
         ++i

         out[i].name='reflectance_in_channel_no_'
         out[i].mode=1
         out[i].title=FMT('Reflectance Ch 3','%')
         ++i

         out[i].name='brightness_temperature_in_channel_no_'
         out[i].mode=1
         out[i].bottom=1
         out[i].title=FMT('BT Ch 4','K')
         ++i

         out[i].name='brightness_temperature_in_channel_no_'
         out[i].mode=1
         out[i].bottom=1
         out[i].title=FMT('BT Ch 5','K')
         ++i

         out[i].name='brightness_temperature_in_channel_no_'
         out[i].mode=1
         out[i].bottom=1
         out[i].title=FMT('BT Ch 6','K')
         ++i

         out[i].name='albedo_in_channel_no_'
         out[i].mode=1
         out[i].title=FMT('Albedo Ch 1','-')
         ++i

         out[i].name='albedo_in_channel_no_'
         out[i].mode=1
         out[i].title=FMT('Albedo Ch 2','-')
         ++i

         out[i].name='albedo_in_channel_no_'
         out[i].mode=1
         out[i].title=FMT('Albedo Ch 3','-')
         ++i

         out[i].name='albedo_in_channel_no_'
         out[i].mode=1
         out[i].title=FMT('Albedo Ch 4','-')
         ++i

         out[i].name='firstguess_reflectance_in_channel_no_'
         out[i].mode=1
         out[i].title=FMT('First guess: Ref Ch 1','%')
         ++i

         out[i].name='firstguess_reflectance_in_channel_no_'
         out[i].mode=1
         out[i].title=FMT('First guess: Ref Ch 2','%')
         ++i

         out[i].name='firstguess_reflectance_in_channel_no_'
         out[i].mode=1
         out[i].title=FMT('First guess: Ref Ch 3','%')
         ++i

         out[i].name='firstguess_brightness_temperature_in_channel_no_'
         out[i].mode=1
         out[i].bottom=1
         out[i].title=FMT('First guess: BT Ch 4','K')
         ++i

         out[i].name='firstguess_brightness_temperature_in_channel_no_'
         out[i].mode=1
         out[i].bottom=1
         out[i].title=FMT('First guess: BT Ch 5','K')
         ++i

         out[i].name='firstguess_brightness_temperature_in_channel_no_'
         out[i].mode=1
         out[i].bottom=1
         out[i].title=FMT('First guess: BT Ch 6','K')
         ++i

         out[i].name='stemp_fg'
         out[i].mode=1
         out[i].title=FMT('First guess: Surface temperature','K')
         ++i

         out[i].name='degrees_of_freedom_signal'
         out[i].mode=1
         out[i].full=1
         out[i].title=FMT('Signal degrees of freedom','-')
         ++i

         vars=[8,14,24,20]
         if STREGEX(inst,'.*M[OY]D.*',/boolean) then begin
            out[vars].name+='1'
            out[vars+1].name+='2'
            out[vars+2].name+='6'
            out[vars+3].name+='20'
            out[vars[0:2]+4].name+='31'
            out[vars[0:2]+5].name+='32'
         endif else if STREGEX(inst,'.*ATSR.*',/boolean) then begin
            out[vars].name+='2'
            out[vars+1].name+='3'
            out[vars+2].name+='4'
            out[vars+3].name+='5'
            out[vars[0:2]+4].name+='6'
            out[vars[0:2]+5].name+='7'
         endif else if STREGEX(inst,'.*AVHRR.*',/boolean) then begin
            out[vars].name+='1'
            out[vars+1].name+='2'
            out[vars+2].name+='3'
            out[vars+3].name+='4'
            out[vars[0:2]+4].name+='5'
            out[vars[0:2]+5].name+='6'
         endif else MESSAGE,'No match found for inst.'
      end
      'FALSE': begin
         out[i].name='false_colour'
         out[i].title=FMT('False colour')
         out[i].range=[0,1]
         out[i].mode=10
      end
      'DIFF': begin
         out[i].name='difference'
         out[i].range=[0,3]
         out[i].mode=10
      end
      else: out[i].name=' '
   endcase

   RETURN, out[WHERE(out.name ne '')]

END
