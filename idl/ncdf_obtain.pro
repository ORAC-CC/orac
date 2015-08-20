;+
; NAME:
;   NCDF_OBTAIN
;
; PURPOSE:
;   Fetch a field from an open NetCDF file, applying the appropriate scale
;   factor, offset, and fill value.
;
; CATEGORY:
;   ORAC plotting tools
;
; CALLING SEQUENCE:
;   field = NCDF_OBTAIN(file_id, name [, fill])
;
; INPUTS:
;   file_id = An ID number returned by NCDF_OPEN.
;   name    = The name of the field to be opened.
;
; OPTIONAL INPUTS:
;   None.
;
; KEYWORD PARAMETERS:
;   None.
;
; OUTPUTS:
;   field   = The requested data.
;
; OPTIONAL OUTPUTS:
;   fill    = The fill value applied to the data.
;
; RESTRICTIONS:
;   None.
;
; MODIFICATION HISTORY:
;   15 Jul 2014 - ACP: Initial version (povey@atm.ox.ac.uk).
;    2 Feb 2015 - ACP: Change selection of fill value to be based on output format.
;-
FUNCTION NCDF_OBTAIN_FILL, in
   ON_ERROR, 2
   COMPILE_OPT HIDDEN
   ;; determine appropriate fill value
   case SIZE(in,/type) of
      5: fill = !values.d_nan
      4: fill = !values.f_nan
      3: fill = -2147483647l
      2: fill = -32767
      1: fill = 255
      else: MESSAGE, 'Fill value not meaningful for this field. '+ $
                     'Use a different routine.'
   endcase
   return, fill
END

FUNCTION NCDF_OBTAIN, fid, name, fill
   ON_ERROR, 2
   COMPILE_OPT HIDDEN, LOGICAL_PREDICATE, STRICTARR, STRICTARRSUBS

   ;; read data array from NCDF file and apply necessary scalling
   vid=NCDF_VARID(fid,name)
   if vid lt 0 then RETURN, -1
   NCDF_VARGET,fid,vid,data
   vq=NCDF_VARINQ(fid,vid)
   nfill=0

   for aid=0,vq.natts-1 do begin
      nm=NCDF_ATTNAME(fid,vid,aid)
      case nm of
         '_FillValue': begin
            NCDF_ATTGET,fid,vid,nm,nc_fill

            ;; replace fill value
            p_fill=WHERE(data eq nc_fill,nfill,comp=p_valid,ncomp=nvalid)
         end
         'scale_factor': NCDF_ATTGET,fid,vid,nm,sc
         'add_offset': NCDF_ATTGET,fid,vid,nm,off
         else:
      endcase
   endfor

   if KEYWORD_SET(sc) then begin
      fill = NCDF_OBTAIN_FILL(sc)
      if KEYWORD_SET(off) then begin
         out = REPLICATE(sc,SIZE(data,/dim))
         if nvalid gt 0 then out[p_valid] = off + sc*data[p_valid]
      endif else begin
         out = REPLICATE(sc,SIZE(data,/dim))
         if nvalid gt 0 then out[p_valid] = sc*data[p_valid]
      endelse
   endif else begin
      if KEYWORD_SET(off) then begin
         fill = NCDF_OBTAIN_FILL(off)
         out = REPLICATE(off,SIZE(data,/dim))
         if nvalid gt 0 then out[p_valid] = off + data[p_valid]
      endif else begin
         fill = NCDF_OBTAIN_FILL(data[0])
         out = data
      endelse
   endelse
   if nfill gt 0 then out[p_fill] = fill

   RETURN, out

END
