;+
; NAME:
;   COMPARE_NCDF_FILES
;
; PURPOSE:
;   To compare the data contents of two NCDF files. Differences are printed to
;   the screen.
;
; CATEGORY:
;   ORAC test framework
;
; CALLING SEQUENCE:
;   COMPARE_NCDF_FILES, old_root, new_root, suffix, thresh, pass
;
; INPUTS:
;   old_root = Root filename and path of original file.
;   new_root = Root filename and path of new file for comparions.
;   suffix   = Suffix to above roots to reference desired files. This is split
;              off so, in the event of differences, the suffix is printed rather
;              than the entire path.
;   thresh   = The check for numerical changes includes an output of how
;              many values have changed by more than the expected rounding error.
;              This sets that threshold.
;   pass     = Return value. Set to zero if a test is failed, left alone other.
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   None
;
; OUTPUTS:
;   1) Printout to screen describing differences.
;   2) Variable pass set to 0 if a test is failed.
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Only inspects the file extensions alb,clf,geo,loc,lsf,msi,uv,lwrtm,prtm,
;   swrtm,config.
;   Assumes filenames start with a common identifier, followed by ORACV
;   and the version number, followed by unique date/time information, ending
;   in an extension listed above.
;   Not able to deal with comparing files produced by different institutions.
;
; MODIFICATION HISTORY:
;   Written by ACPovey (povey@atm.ox.ac.uk)
;   20 Aug 2015 - AP: Original version
;-
@../idl/print_colour.pro
PRO COMPARE_NCDF_FILES, oldroot, newroot, suffix, thres, pass
   sthres = STRING(thres,format='(g0.1)')

   id1=NCDF_OPEN(oldroot+suffix)
   id2=NCDF_OPEN(newroot+suffix)

   inq1=NCDF_INQUIRE(id1)
   inq2=NCDF_INQUIRE(id2)
   if inq1.nvars ne inq2.nvars then begin
      PRINT, 'Files '+FILE_BASENAME(oldroot)+' and '+FILE_BASENAME(newroot)+ $
             ' contain different numbers of variables.'
      pass=0
   endif

   ;; Loop over all variables in new file
   for j=0,inq2.nvars-1 do begin
      var=NCDF_VARINQ(id2,j)
      vid=NCDF_VARID(id1,var.name)
      if vid eq -1 then begin
         ;; Variable only in new file (warnings printed by NCDF_VARID and nccmp)
         continue
      endif
      NCDF_VARGET,id1,vid,c1
      NCDF_VARGET,id2,j,c2
      if ~ARRAY_EQUAL(c1,c2) then begin
         if N_ELEMENTS(c1) ne N_ELEMENTS(c2) then begin
            ;; Array dimensions have changed
            PRINT_COLOUR, var.name, /red, /nonewline
            PRINT,STRING(format='(" (",I0,")")',j) + $
                  ' - Size mismatch: [' + $
                  STRJOIN(STRING(SIZE(c1,/dim),format='(I0)'),',') + $
                  '] vs ['+ $
                  STRJOIN(STRING(SIZE(c2,/dim),format='(I0)'),',') + ']'
            pass=0
         endif else begin
            ;; See if scale and/or offset attributes are available
            offset = 0.
            scale  = 1.
            for i=0,var.natts-1 do begin
               att_name=NCDF_ATTNAME(id2,j,i)
               if STRCMP(att_name, 'add_offset', /fold) then $
                  NCDF_ATTGET,id2,j,att_name,offset
               if STRCMP(att_name, 'scale_factor', /fold) then $
                  NCDF_ATTGET,id2,j,att_name,scale
            endfor

            ;; Change in value greater than some rounding error threshold
            trash=WHERE(ABS(DOUBLE(c2-c1)/(c1+offset/scale)) gt thres,nt)
            if nt gt 0 then begin
               PRINT_COLOUR, var.name, /red, /nonewline
               PRINT,suffix,j,nt, format='(" (",A0,",",I0,'+ $
                     '") - ",I0," points are different by > '+sthres+'")'
            endif else begin
               PRINT_COLOUR, var.name, /green, /nonewline
               PRINT,suffix,j, format='(" (",A0,",",I0,'+ $
                       '") - 0 points are different by > '+sthres+'")'
            endelse
            pass=0
         endelse
      endif
   endfor
   NCDF_CLOSE,id1
   NCDF_CLOSE,id2

END
