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
;-
FUNCTION NCDF_OBTAIN, fid, name, fill
   ON_ERROR, 2
   COMPILE_OPT HIDDEN, LOGICAL_PREDICATE, STRICTARR, STRICTARRSUBS

   ;; read data array from NCDF file and apply necessary scalling
   vid=NCDF_VARID(fid,name)
   NCDF_VARGET,fid,vid,data

   ;; determine appropriate fill value
   vq=NCDF_VARINQ(fid,vid)
   case vq.datatype of
      'DOUBLE': fill = !values.d_nan
      'FLOAT': fill = !values.f_nan
      'LONG': fill = -999l
      'INT': fill = -999
      'BYTE': fill = 255
      else: MESSAGE, 'Fill value not meaningful for this field. '+ $
                     'Use a different routine.'
   endcase
      
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
      if KEYWORD_SET(off) then begin
         out = REPLICATE(sc,SIZE(data,/dim))
         if nvalid gt 0 then out[p_valid] = off + sc*data[p_valid] 
      endif else begin
         out = REPLICATE(sc,SIZE(data,/dim))
         if nvalid gt 0 then out[p_valid] = sc*data[p_valid] 
      endelse
   endif else begin
      if KEYWORD_SET(off) then begin
         out = REPLICATE(off,SIZE(data,/dim))
         if nvalid gt 0 then out[p_valid] = off + data[p_valid] 
      endif else begin
         out = data
      endelse
   endelse
   if nfill gt 0 then out[p_fill] = fill

   RETURN, out

END
