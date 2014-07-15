FUNCTION PLOT_SETTINGS, suffix, inst
   ON_ERROR, 0
   COMPILE_OPT LOGICAL_PREDICATE, STRICTARR, STRICTARRSUBS

   ;; cut out first full stop
   suff=STRMID(suffix,STRPOS(suffix,'.')+1)
   suff=STRMID(suff,0,STRPOS(suff,'.'))
   suff=STRUPCASE(suff)

   str={name:'', mode:-1, title:'', log:0, bottom:0, abs:0, filter:0, $
        btf:'(g0.4)', outline:1, full:0, syms:0.1, range:[!values.f_nan,0.], $
        blabels: STRARR(10), nlevels:250}

   ;; Mode key:
   ;; 0) Line plot [n]
   ;; 1) One map plot [nx,ny]
   ;; 2) Series of map plots [nx,ny,n]
   ;; 3) One map plot [nxy]
   ;; 4) Series of map plots [n,nxy]
   ;; 5) Levels of map plots [nl,nxy]
   ;; 6) Two series of map plots [n,nl,nxy]
   case suff of
      'ALB': begin
         out=REPLICATE(str,4)

         out[2].name='alb_abs_ch_numbers'
         out[2].mode=0
         out[2].full=1
         out[2].outline=0

         out[3].name='emis_abs_ch_numbers'
         out[3].mode=0
         out[3].full=1
         out[3].outline=0

         out[0].name='alb_data'
         out[0].mode=2
         out[0].log=1
         out[0].title=FMT('Surface albedo','%')

         out[1].name='emis_data'
         out[1].mode=2
         out[1].log=1
         out[1].title=FMT('Surface emissivity','%')
      end
      'CLF': begin
         out=str
         
         out.name='cflag'
         out.mode=1
         out.full=1
         out.title=FMT('Cloud flag')
         out.btf=''
         out.blabels[0:1]=['Clear','Cloudy']
         out.range=[0,1]
         out.nlevels=2
      end
      'CONFIG': begin
         out=REPLICATE(str,8)
         
         out[0].name='msi_instr_ch_numbers'
         out[0].mode=0
         out[0].full=1
         out[0].outline=0

         out[1].name='msi_abs_ch_numbers'
         out[1].mode=0
         out[1].full=1
         out[1].outline=0

         out[2].name='msi_abs_ch_wl'
         out[2].mode=0
         out[2].full=1
         out[2].outline=0

         out[3].name='msi_ch_swflag'
         out[3].mode=0
         out[3].full=1
         out[3].outline=0

         out[4].name='msi_ch_lwflag'
         out[4].mode=0
         out[4].full=1
         out[4].outline=0

         out[5].name='msi_ch_procflag'
         out[5].mode=0
         out[5].full=1
         out[5].outline=0

         out[6].name='alb_abs_ch_numbers'
         out[6].mode=0
         out[6].full=1
         out[6].outline=0

         out[7].name='emis_abs_ch_numbers'
         out[7].mode=0
         out[7].full=1
      end
      'GEO': begin
         out=REPLICATE(str,4)

         out[0].name='solzen'
         out[0].mode=2
         out[0].full=1
         out[0].title=FMT('Solar zenith angle','!9'+STRING(176b)+'!X')

         out[1].name='satzen'
         out[1].mode=2
         out[1].full=1
         out[1].title=FMT('Satellite zenith angle','!9'+STRING(176b)+'!X')

         out[2].name='solaz'
         out[2].mode=2
         out[2].full=1
         out[2].title=FMT('Solar azimuth angle','!9'+STRING(176b)+'!X')

         out[3].name='relazi'
         out[3].mode=2
         out[3].full=1
         out[3].title=FMT('Relative azimuth angle','!9'+STRING(176b)+'!X')
      end
      'LOC': begin
         out=REPLICATE(str,2)

         out[0].name='lon'
         out[0].mode=1
         out[0].full=1
         out[0].title=FMT('Longitude','!9'+STRING(176b)+'!X')

         out[1].name='lat'
         out[1].mode=1
         out[1].full=1
         out[1].title=FMT('Latitude','!9'+STRING(176b)+'!X')
      end
      'LSF': begin
         out=str

         out.name='lsflag'
         out.mode=1
         out.full=1
         out.title=FMT('Land sea flag')
         out.btf=''
         out.blabels[0:1]=['Sea','Land']
         out.range=[0,1]
         out.nlevels=2
      end
      'LWRTM': begin
         out=REPLICATE(str,13)

         out[0].name='lw_channel_abs_ids'
         out[0].mode=0
         out[0].full=1
         out[0].outline=0

         out[1].name='lw_channel_instr_ids'
         out[1].mode=0
         out[1].full=1
         out[1].outline=0

         out[2].name='lw_channel_wvl'
         out[2].mode=0
         out[2].full=1
         out[2].outline=0

         out[3].name='counter_lw'
         out[3].full=1
         out[3].mode=3

         out[4].name='solza_lw'
         out[4].full=1
         out[4].mode=4
         out[4].title=FMT('RTM LW solar azimuth angle','!9'+STRING(176b)+'!X')

         out[5].name='satza_lw'
         out[5].full=1
         out[5].mode=4
         out[5].title=FMT('RTM LW satellite azimuth angle','!9'+STRING(176b)+'!X')

         out[6].name='relazi_lw'
         out[6].full=1
         out[6].mode=4
         out[6].title=FMT('RTM LW relative azimuth angle','!9'+STRING(176b)+'!X')

         out[7].name='emiss_lw'
         out[7].log=1
         out[7].mode=4
         out[7].title=FMT('RTM LW atmospheric emissivity','%')

         out[8].name='tac_lw'
         out[8].full=1
         out[8].mode=6

         out[9].name='tbc_lw'
         out[9].full=1
         out[9].mode=6

         out[10].name='rbc_up_lw'
         out[10].full=1
         out[10].mode=6

         out[11].name='rac_up_lw'
         out[11].full=1
         out[11].mode=6

         out[12].name='rac_down_lw'
         out[12].full=1
         out[12].mode=6
      end
      'MSI': begin
         out=REPLICATE(str,8)

         out[1].name='msi_instr_ch_numbers'
         out[1].mode=0
         out[1].full=1
         out[1].outline=0

         out[2].name='msi_abs_ch_numbers'
         out[2].mode=0
         out[2].full=1
         out[2].outline=0

         out[3].name='msi_abs_ch_wl'
         out[3].mode=0
         out[3].full=1
         out[3].outline=0

         out[4].name='msi_ch_swflag'
         out[4].mode=0
         out[4].full=1
         out[4].outline=0

         out[5].name='msi_ch_lwflag'
         out[5].mode=0
         out[5].full=1
         out[5].outline=0

         out[6].name='msi_ch_procflag'
         out[6].mode=0
         out[6].full=1
         out[6].outline=0

         out[7].name='time_data'
         out[7].mode=1
         out[7].full=1

         out[0].name='msi_data'
         out[0].mode=2
         out[0].title=FMT('Imager data','% || K')
      end
      'PRTM': begin
         out=REPLICATE(str,13)

         out[0].name='i_pw'
         out[0].mode=3
         out[0].full=1

         out[1].name='j_pw'
         out[1].mode=3
         out[1].full=1

         out[2].name='counter_pw'
         out[2].mode=3
         out[2].full=1

         out[3].name='lon_pw'
         out[3].mode=3
         out[3].full=1
         out[3].title=FMT('RTM longitude','!9'+STRING(176b)+'!X')

         out[4].name='lat_pw'
         out[4].mode=3
         out[4].full=1
         out[4].title=FMT('RTM latitude','!9'+STRING(176b)+'!X')

         out[5].name='skint_pw'
         out[5].mode=3
         out[5].bottom=1
         out[5].title=FMT('RTM skin temperature','K')

         out[6].name='explnsp_pw'
         out[6].mode=3
         out[6].bottom=1
         out[6].title=FMT('RTM surface pressure','hPa')

         out[7].name='lsf_pw'
         out[7].mode=3
         out[7].full=1
         out[7].title=FMT('RTM surface albedo','%')

         out[8].name='satzen_pw'
         out[8].mode=3
         out[8].full=1
         out[8].title=FMT('RTM satellite zenith angle','!9'+STRING(176b)+'!X')

         out[9].name='solzen_pw'
         out[9].mode=3
         out[9].full=1
         out[9].title=FMT('RTM solar zenith angle','!9'+STRING(176b)+'!X')

         out[10].name='pprofile_lev'
         out[10].mode=5
         out[10].bottom=1
         out[10].title=FMT('RTM pressure','hPa')

         out[11].name='tprofile_lev'
         out[11].mode=5
         out[11].bottom=1
         out[11].title=FMT('RTM temperature','K')

         out[12].name='gphprofile_lev'
         out[12].mode=5
         out[12].bottom=1
         out[12].title=FMT('RTM geopotential','m')
      end
      'SWRTM': begin
         out=REPLICATE(str,9)

         out[0].name='sw_channel_abs_ids'
         out[0].mode=0
         out[0].full=1
         out[0].outline=0

         out[1].name='sw_channel_instr_ids'
         out[1].mode=0
         out[1].full=1
         out[1].outline=0

         out[2].name='sw_channel_wvl'
         out[2].mode=0
         out[2].full=1
         out[2].outline=0

         out[3].name='counter_sw'
         out[3].full=1
         out[3].mode=3

         out[4].name='solza_sw'
         out[4].full=1
         out[4].mode=4
         out[4].title=FMT('RTM SW solar azimuth angle','!9'+STRING(176b)+'!X')

         out[5].name='satza_sw'
         out[5].full=1
         out[5].mode=4
         out[5].title=FMT('RTM SW satellite azimuth angle','!9'+STRING(176b)+'!X')

         out[6].name='relazi_sw'
         out[6].full=1
         out[6].mode=4
         out[6].title=FMT('RTM SW relative azimuth angle','!9'+STRING(176b)+'!X')

         out[7].name='tac_sw'
         out[7].mode=6
         out[7].full=1

         out[8].name='tbc_sw'
         out[8].mode=6
         out[8].full=1
      end
      'UV': begin
         out=REPLICATE(str,2)

         out[0].name='uscan'
         out[0].mode=1
         out[0].full=1

         out[1].name='vscan'
         out[1].mode=1
         out[1].full=1
      end
      'PRIMARY': begin
         out=REPLICATE(str,30)
         out.filter=01

         out[0].name='cot'
         out[0].mode=1
         out[0].log=1
         out[0].title=FMT('Optical thickness','-')

         out[1].name='cot_uncertainty'
         out[1].mode=1
         out[1].title=FMT('Optical thickness !Ms!N','-')

         out[2].name='ref'
         out[2].mode=1
         out[2].title=FMT('Effective radius','!Mm!Xm')

         out[3].name='ref_uncertainty'
         out[3].mode=1
         out[3].title=FMT('Effective radius !Ms!N','!Mm!Xm')

         out[4].name='ctp'
         out[4].mode=1
         out[4].title=FMT('Cloud top pressure','Pa')

         out[5].name='ctp_uncertainty'
         out[5].mode=1
         out[5].title=FMT('Cloud top pressure !Ms!N','Pa')

         out[6].name='stemp'
         out[6].mode=1
         out[6].bottom=1
         out[6].title=FMT('Surface temperature','K')

         out[7].name='stemp_uncertainty'
         out[7].mode=1
         out[7].title=FMT('Surface temperature !Ms!N','K')

         out[8].name='cwp'
         out[8].mode=1
         out[8].title=FMT('Cloud water path','g m!E-2!N')

         out[9].name='cwp_uncertainty'
         out[9].mode=1
         out[9].title=FMT('Cloud water path !Ms!N','g m!E-2!N')

         out[10].name='cc_total'
         out[10].mode=1
         out[10].title=FMT('Cloud fraction','%')

         out[11].name='cc_total_uncertainty'
         out[11].mode=1
         out[11].title=FMT('Cloud fraction !Ms!N','%')

         out[12].name='cth'
         out[12].mode=1
         out[12].title=FMT('Cloud top height','km')

         out[13].name='cth_uncertainty'
         out[13].mode=1
         out[13].title=FMT('Cloud top height !Ms!N','K')

         out[14].name='ctt'
         out[14].mode=1
         out[14].title=FMT('Cloud top temperature','K')

         out[15].name='ctt_uncertainty'
         out[15].mode=1
         out[15].title=FMT('Cloud top temperature !Ms!N','K')

         out[16].name='convergence'
         out[16].mode=1
         out[16].full=1
         out[16].title=FMT('Convergence flag')
         out[16].btf=''
         out[16].blabels[0:1]=['Yes','No']
         out[16].range=[0,1]
         out[16].nlevels=2

         out[17].name='niter'
         out[17].mode=1
         out[17].full=1
         out[17].title=FMT('Iterations','-')

         out[18].name='qcflag'
         out[18].mode=1
         out[18].full=1
         out[18].title=FMT('Quality control flag','-')

         out[19].name='phase'
         out[19].mode=1
         out[19].full=1
         out[19].title=FMT('Phase selection')
         out[19].btf=''
         out[19].blabels[0:1]=['Ice','Water']
         out[19].range=[0,1]
         out[19].nlevels=2

         out[20].name='costja'
         out[20].mode=1
         out[20].title=FMT('A priori cost','-')

         out[21].name='costjm'
         out[21].mode=1
         out[21].title=FMT('Measurement cost','-')

         out[22].name='lsflag'
         out[22].mode=1
         out[22].full=1
         out[22].title=FMT('Land sea flag')
         out[22].btf=''
         out[22].blabels[0:1]=['Sea','Land']
         out[22].range=[0,1]
         out[22].nlevels=2

         out[23].name='illum'
         out[23].mode=1
         out[23].full=1
         out[23].title=FMT('Illumination flag')
         out[23].btf=''
         out[23].blabels[0:3]=['Day','Twilight','Night','Daynore']
         out[23].range=[1,4]
         out[23].nlevels=4

         out[24].name='satellite_zenith_view_no1'
         out[24].mode=1
         out[24].full=1
         out[24].title=FMT('Nadir satellite zenith','!9'+STRING(176b)+'!X')

         out[25].name='solar_zenith_view_no1'
         out[25].mode=1
         out[25].full=1
         out[25].title=FMT('Nadir solar zenith','!9'+STRING(176b)+'!X')

         out[26].name='rel_azimuth_view_no1'
         out[26].mode=1
         out[26].full=1
         out[26].title=FMT('Nadir relative azimuth','!9'+STRING(176b)+'!X')

         out[27].name='lon'
         out[27].mode=1
         out[27].full=1
         out[27].title=FMT('Longitude','!9'+STRING(176b)+'!X')

         out[28].name='lat'
         out[28].mode=1
         out[28].full=1
         out[28].title=FMT('Latitude','!9'+STRING(176b)+'!X')

         out[29].name='time'
         out[29].mode=1
         out[29].full=1
         out[29].title=FMT('Observation time','JDN')
      end
      'SECONDARY': begin 
         out=REPLICATE(str,32)

         out[0].name='scanline_u'
         out[0].mode=1
         out[0].full=1
         out[0].title=FMT('Scan u','-')

         out[1].name='scanline_v'
         out[1].mode=1
         out[1].full=1
         out[1].title=FMT('Scan v','-')

         out[2].name='cot_ap'
         out[2].mode=1
         out[2].title=FMT('A priori: Optical thickness','-')

         out[3].name='cot_fg'
         out[3].mode=1
         out[3].title=FMT('First guess: Optical thickness','-')

         out[4].name='ref_ap'
         out[4].mode=1
         out[4].title=FMT('A priori: Effective radius','!Mm!Xm')

         out[5].name='ref_fg'
         out[5].mode=1
         out[5].title=FMT('First guess: Effective radius','!Mm!Xm')

         out[6].name='ctp_ap'
         out[6].mode=1
         out[6].title=FMT('A priori: Cloud top pressure','Pa')

         out[7].name='ctp_fg'
         out[7].mode=1
         out[7].title=FMT('First guess: cloud top pressure','Pa')

         out[8].name='reflectance_residual_in_channel_no_'
         out[8].mode=1
         out[8].abs=1
         out[8].title=FMT('Ref residual Ch 1','%')

         out[9].name='reflectance_residual_in_channel_no_'
         out[9].mode=1
         out[9].abs=1
         out[9].title=FMT('Ref residual Ch 2','%')

         out[10].name='reflectance_residual_in_channel_no_'
         out[10].mode=1
         out[10].abs=1
         out[10].title=FMT('Ref residual Ch 3','%')

         out[11].name='brightness_temperature_residual_in_channel_no_'
         out[11].mode=1
         out[11].abs=1
         out[11].btf='(i3)'
         out[11].title=FMT('BT residual Ch 4','K')

         out[12].name='brightness_temperature_residual_in_channel_no_'
         out[12].mode=1
         out[12].abs=1
         out[12].btf='(i3)'
         out[12].title=FMT('BT residual Ch 5','K')

         out[13].name='brightness_temperature_residual_in_channel_no_'
         out[13].mode=1
         out[13].abs=1
         out[13].btf='(i3)'
         out[13].title=FMT('BT residual Ch 6','K')

         out[14].name='reflectance_in_channel_no_'
         out[14].mode=1
         out[14].title=FMT('Reflectance Ch 1','%')

         out[15].name='reflectance_in_channel_no_'
         out[15].mode=1
         out[15].title=FMT('Reflectance Ch 2','%')

         out[16].name='reflectance_in_channel_no_'
         out[16].mode=1
         out[16].title=FMT('Reflectance Ch 3','%')

         out[17].name='brightness_temperature_in_channel_no_'
         out[17].mode=1
         out[17].bottom=1
         out[17].title=FMT('BT Ch 4','K')

         out[18].name='brightness_temperature_in_channel_no_'
         out[18].mode=1
         out[18].bottom=1
         out[18].title=FMT('BT Ch 5','K')

         out[19].name='brightness_temperature_in_channel_no_'
         out[19].mode=1
         out[19].bottom=1
         out[19].title=FMT('BT Ch 6','K')

         out[20].name='albedo_in_channel_no_'
         out[20].mode=1
         out[20].title=FMT('Albedo Ch 1','-')

         out[21].name='albedo_in_channel_no_'
         out[21].mode=1
         out[21].title=FMT('Albedo Ch 2','-')

         out[22].name='albedo_in_channel_no_'
         out[22].mode=1
         out[22].title=FMT('Albedo Ch 3','-')

         out[23].name='albedo_in_channel_no_'
         out[23].mode=1
         out[23].title=FMT('Albedo Ch 4','-')

         out[24].name='firstguess_reflectance_in_channel_no_'
         out[24].mode=1
         out[24].title=FMT('First guess: Ref Ch 1','%')

         out[25].name='firstguess_reflectance_in_channel_no_'
         out[25].mode=1
         out[25].title=FMT('First guess: Ref Ch 2','%')

         out[26].name='firstguess_reflectance_in_channel_no_'
         out[26].mode=1
         out[26].title=FMT('First guess: Ref Ch 3','%')

         out[27].name='firstguess_brightness_temperature_in_channel_no_'
         out[27].mode=1
         out[27].bottom=1
         out[27].title=FMT('First guess: BT Ch 4','K')

         out[28].name='firstguess_brightness_temperature_in_channel_no_'
         out[28].mode=1
         out[28].bottom=1
         out[28].title=FMT('First guess: BT Ch 5','K')

         out[29].name='firstguess_brightness_temperature_in_channel_no_'
         out[29].mode=1
         out[29].bottom=1
         out[29].title=FMT('First guess: BT Ch 6','K')

         out[30].name='stemp_fg'
         out[30].mode=1
         out[30].title=FMT('First guess: Surface temperature','K')

         out[31].name='degrees_of_freedom_signal'
         out[31].mode=1
         out[31].full=1
         out[31].title=FMT('Signal degrees of freedom','-')

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
   endcase

   RETURN, out

END

