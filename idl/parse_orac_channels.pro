;+
; NAME:
;   PARSE_ORAC_CHANNELS
;
; PURPOSE:
;   Converts the inputs of PLOT_FALSE into the strings required for reading
;   data and labelling plots.
;
; CATEGORY:
;   ORAC plotting tools
;
; CALLING SEQUENCE:
;    channels = PARSE_ORAC_CHANNELS(instrument, numbers, strings, /abs_ref)
;
; INPUTS:
;   instrument = A string specifying the swath to be plotted. This is expected
;      to be one of the values of $label in trunk/tools/test-preproc.sh
;   numbers    = Values passed into the FALSE or DIFF keywords of PLOT_FALSE.
;
; OPTIONAL INPUTS:
;   None.
;
; KEYWORD PARAMETERS:
;   ABS_REF    = Rather than specify channels using their number in the sensor,
;      reference them by their offset within ORAC (i.e. 0=0.67, 1=0.87, 2=1.60,
;      3=3.70, 4=11.0, 5=12.0 um).
;
; OUTPUTS:
;   channels   = An array of strings giving the names of the requested fields
;      in an ORAC 'secondary' output file.
;
; OPTIONAL OUTPUTS:
;   strings    = Strings giving the wavelengths of the requested channels, as
;      needed to print plot titles.
;
; RESTRICTIONS:
;   None known.
;
; MODIFICATION HISTORY:
;   28 Jul 2014 - ACP: Initial version (povey@atm.ox.ac.uk).
;-
FUNCTION PARSE_ORAC_CHANNELS, inst, num, str_num, abs_ref=abs_ref
   ON_ERROR, 0
   COMPILE_OPT LOGICAL_PREDICATE, STRICTARR, STRICTARRSUBS, HIDDEN

   if KEYWORD_SET(abs_ref) then begin
      ;; channels are referenced absolutely, rather by instrument number
      if STREGEX(inst,'.*M[OY]D.*',/boolean) then begin
         chs=(['reflectance_in_channel_no_1', $
               'reflectance_in_channel_no_2', $
               'reflectance_in_channel_no_6', $
               'brightness_temperature_in_channel_no_20', $
               'brightness_temperature_in_channel_no_31', $
               'brightness_temperature_in_channel_no_32'])[num]
      endif else if STREGEX(inst,'.*ATSR.*',/boolean) then begin
         chs=(['reflectance_in_channel_no_2', $
               'reflectance_in_channel_no_3', $
               'reflectance_in_channel_no_4', $
               'brightness_temperature_in_channel_no_5', $
               'brightness_temperature_in_channel_no_6', $
               'brightness_temperature_in_channel_no_7'])[num]
      endif else if STREGEX(inst,'.*AVHRR.*',/boolean) then begin
         chs=(['reflectance_in_channel_no_1', $
               'reflectance_in_channel_no_2', $
               'reflectance_in_channel_no_3', $
               'brightness_temperature_in_channel_no_4', $
               'brightness_temperature_in_channel_no_5', $
               'brightness_temperature_in_channel_no_6'])[num]
      endif else MESSAGE,'No match found for inst.'

      str_num=(['0.67','0.87','1.60','3.70','11.0','12.0'])[num]
   endif else begin
      ;; channels are regerenced with the instrument number
      n=N_ELEMENTS(num)
      chs=STRARR(n)
      str_num=STRARR(n)
      if STREGEX(inst,'.*M[OY]D.*',/boolean) then begin
         for k=0,n-1 do case num[k] of
            1:  begin
               str_num[k]='0.67'
               chs[k]='reflectance_in_channel_no_1'
            end
            2:  begin
               str_num[k]='0.87'
               chs[k]='reflectance_in_channel_no_2'
            end
            6:  begin
               str_num[k]='1.60'
               chs[k]='reflectance_in_channel_no_6'
            end
            20: begin
               str_num[k]='3.70'
               chs[k]='brightness_temperature_in_channel_no_20'
            end
            31: begin
               str_num[k]='11.0'
               chs[k]='brightness_temperature_in_channel_no_31'
            end
            32: begin
               str_num[k]='12.0'
               chs[k]='brightness_temperature_in_channel_no_32'
            end
         endcase
      endif else if STREGEX(inst,'.*ATSR.*',/boolean) then begin
         for k=0,n-1 do case num[k] of
            2: begin
               str_num[k]='0.67'
               chs[k]='reflectance_in_channel_no_1'
            end
            3: begin
               str_num[k]='0.87'
               chs[k]='reflectance_in_channel_no_2'
            end
            4: begin
               str_num[k]='1.60'
               chs[k]='reflectance_in_channel_no_6'
            end
            5: begin
               str_num[k]='3.70'
               chs[k]='brightness_temperature_in_channel_no_20'
            end
            6: begin
               str_num[k]='11.0'
               chs[k]='brightness_temperature_in_channel_no_31'
            end
            7: begin
               str_num[k]='12.0'
               chs[k]='brightness_temperature_in_channel_no_32'
            end
         endcase
      endif else if STREGEX(inst,'.*AVHRR.*',/boolean) then begin
         for k=0,n-1 do case num[k] of
            1: begin
               str_num[k]='0.67'
               chs[k]='reflectance_in_channel_no_1'
            end
            2: begin
               str_num[k]='0.87'
               chs[k]='reflectance_in_channel_no_2'
            end
            3: begin
               str_num[k]='1.60'
               chs[k]='reflectance_in_channel_no_6'
            end
            4: begin
               str_num[k]='3.70'
               chs[k]='brightness_temperature_in_channel_no_20'
            end
            5: begin
               str_num[k]='11.0'
               chs[k]='brightness_temperature_in_channel_no_31'
            end
            6: begin
               str_num[k]='12.0'
               chs[k]='brightness_temperature_in_channel_no_32'
            end
         endcase
      endif
   endelse

   RETURN, chs
END
