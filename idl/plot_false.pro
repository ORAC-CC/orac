;+
; NAME:
;   PLOT_FALSE
;
; PURPOSE:
;   Produce quick-look plots for the outputs of ORAC. Specifically, make
;   false-colour images or plot the difference between two channels. This is
;   done via the FALSE or DIFF keywords, to which you pass the channels you
;   wish plotted.
;
;   The other parameters of these plots are automatically selected where 
;   possible, though most can be controlled through the PLOT_SETTINGS routine.
;
; CATEGORY:
;   Plotting tools
;
; CALLING SEQUENCE:
;   PLOT_FALSE, instrument, revision, folder, false=array, diff=array,
;            [, root=string] [, xsize=value] [, ysize=value] 
;            [, nx=value] [, ny=value] [, font_size=value]
;            [, scale=value] [, label=string] [, frames=value]
;            [, /keep_ps] [, \wat|/ice] [, /abs_ref]
;
; INPUTS:
;   instrument = A string specifying the swath to be plotted. This is expected
;      to be one of the values of $label in trunk/tools/test-preproc.sh
;   revision   = The revision number to be plotted.
;   false      = INTARR(3,*). Each row should denote three channels with which
;      to produce a false-colour image, being the red, green, and blue channels
;      respectively. These should be specified by the channel in the sensor.
;   diff       = INTARR(2,*). Each row should denote two channels with which
;      to produce a difference image, being the first channel minus the second.
;      These should be specified by the channel in the sensor.
;
; OPTIONAL INPUTS:
;   folder     = The path to the testoutput folder. Defaults to the values of
;      the enviroment variable TESTOUT.
;   ROOT       = Root path of the files to be plotted (i.e. $TESTOUT/V1/DAYMYD/
;      DAYMYD_UoOx_MODIS_ORACV1_AQUA_20000101000000_200806200405_V1.0).
;      Overrides the automatic file selection. If used with COMPARE, that keyword
;      should be set to the appropriate root path for the previous revision.
;   X|YSIZE    = Horizontal|Vertical extent of the output panes.
;   NX|Y       = Horizontal|Vertical number of plots.
;   FONT_SIZE  = Desired font size.
;   SCALE      = A number to multiply both NX|Y. A useful way to zoom in.
;   LABEL      = A very short description that should be printed at the top-left
;      of each page.
;   FRAMES      = The number of figures across which to plot the swath.
;	
; KEYWORD PARAMETERS:
;   KEEP_PS    = Do not delete the postscript plots after combining them into
;      a PDF.
;   WAT|ICE    = Search for the WAT|ICE cloud phase outputs rather than the
;      postprocessed file.
;   ABS_REF    = Rather than specify channels using their number in the sensor,
;      reference them by their offset within ORAC (i.e. 0=0.67, 1=0.87, 2=1.60,
;      3=3.70, 4=11.0, 5=12.0 um).
;	
; OUTPUTS:
;   - All outputs are made into the same folder as the input data.
;   - During processing, scratch postscripts are produced named ROOT.N###.eps.
;   - After processing, these are combined into a single PDF file called:
;      Default) ROOT.orac.pdf
;      COMPARE) ROOT.orac.comp.pdf
;      PREPROC) ROOT.preproc.pdf
;      PREPROC, COMPARE) ROOT.preproc.comp.pdf
; 
; OPTIONAL OUTPUTS:
;   None.
;
; RESTRICTIONS:
;   None.
;
; MODIFICATION HISTORY:
;   28 Jul 2014 - ACP: Initial version (povey@atm.ox.ac.uk).
;-
PRO PLOT_FALSE, inst, rev, fdr, false=false, diff=diff, stop=stop, $
                root=root, xsize=xs, ysize=ys, nx=nx, ny=ny, $
                font_size=font_s, scale=scale, label=label, $
                frames=frames, keep_ps=keep_ps, ice=ice, $
                short=short, wat=wat, abs_ref=abs_ref
   ON_ERROR, KEYWORD_SET(stp) ? 0 : 2
   COMPILE_OPT LOGICAL_PREDICATE, STRICTARR, STRICTARRSUBS

   ;; process inputs
   if ~KEYWORD_SET(fdr) then fdr=GETENV('TESTOUT')
   revision=STRING(rev,format='(i0)')
   if SIZE(inst,/type) ne 7 then MESSAGE,'INSTRUMENT must be a string.'
   if ~KEYWORD_SET(frames) then frames=1
   SET_PLOT,'ps'

   if KEYWORD_SET(root) then begin
      ;; root filenames provided as arguments
      nroot=N_ELEMENTS(root)
      folder=STRMID(root[0],0,STRPOS(root[0],'/',/reverse_search))
   endif else begin
      FIND_ORAC, fdr, inst, rev, folder, root, nroot, label=label
   endelse

   ;; plot ORAC retrieval
   tag='.false'
   suff = ['.primary.nc', '.secondary.nc']
   if KEYWORD_SET(wat) then begin
      tag='WAT'+tag
      suff='WAT'+suff
   endif else if KEYWORD_SET(ice) then begin
      tag='ICE'+tag
      suff='ICE'+suff
   endif

   ;; save current colourbar and set greyscale for plot titles
   TVLCT, save_ct, /get
   LOADCT,0,/silent

   ;; loop over chunks
   for i=0,nroot-1 do begin
      if ~FILE_TEST(root[i]+suff[0],/regular) then CONTINUE
      if ~FILE_TEST(root[i]+suff[1],/regular) then CONTINUE

      ;; fetch lat/lon
      fid=NCDF_OPEN(root[i]+suff[0])
      lat=NCDF_OBTAIN(fid,'lat')
      lon=NCDF_OBTAIN(fid,'lon')
      qcf=NCDF_OBTAIN(fid,'qcflag')
      NCDF_CLOSE,fid
      fid=NCDF_OPEN(root[i]+suff[1])

      ;; determine field size (for chunked plotting)
      sze=SIZE(lat,/dim)
      nl=sze[0]
      line=sze[1]

      ;; initialise plotting area
      SELECT_FRAMES, lat, lon, line, nl, frames, cent, lim

      plot_set=INIT_PLOT_SET(frames, lim, cent, label, font_s=font_s, $
                             xsize=xs, ysize=ys, nx=nx, ny=ny, scale=scale)
      plot_set.tag = root[i]+tag

      ;; plot requested false colour images
      if KEYWORD_SET(false) then for j=0,N_ELEMENTS(false[0,*])-1 do begin
         chs=PARSE_ORAC_CHANNELS(inst, false[*,0], str_num, abs_ref=abs_ref)
         set=PLOT_SETTINGS('.FALSE.')
         set.title+=': '+str_num[0]+','+str_num[1]+','+str_num[2]

         ;; open data fields
         red=NCDF_OBTAIN(fid, chs[0], fill_r)
         grn=NCDF_OBTAIN(fid, chs[1], fill_g)
         blu=NCDF_OBTAIN(fid, chs[2], fill_b)

         ;; determine plotting filter (all points not missing)
         filt = red ne fill_r AND grn ne fill_g AND blu ne fill_b
         ;; there appear to be negative reflectances for no good reason !!FIX!!
         filt = filt AND red ge 0. AND grn ge 0. AND blu ge 0.

         WRAP_MAPPOINTS, [[[red]], [[grn]], [[blu]]], lat, lon, false=3, $
                         debug=stop, short=short, $
                         set, plot_set, filt, line, nl, 0.1
      endfor

      ;; plot requested false colour images
      if KEYWORD_SET(diff) then for j=0,N_ELEMENTS(diff[0,*])-1 do begin
         chs=PARSE_ORAC_CHANNELS(inst, diff[*,0], str_num, abs_ref=abs_ref)
         set=PLOT_SETTINGS('.DIFF.')
         set.title=FMT(str_num[0]+'-'+str_num[1]+' difference')

         ;; open data fields
         plus=NCDF_OBTAIN(fid, chs[0], fill_p)
         mnus=NCDF_OBTAIN(fid, chs[1], fill_m)

         ;; determine plotting filter (all points not missing)
         filt = plus ne fill_p AND mnus ne fill_m
         if KEYWORD_SET(qc_filt) then filt = filt AND ~(qcf AND qc_filt)

         WRAP_MAPPOINTS, plus - mnus, lat, lon, $
                         debug=stop, short=short, $
                         set, plot_set, filt, line, nl, 0.1
      endfor

      ;; tidy up
      DEVICE,/close
      NCDF_CLOSE,fid

      ;; merge individual PDFs and delete pieces
      CD,folder,current=cur_cd
      SPAWN,'gs -dNOPAUSE -sDEVICE=pdfwrite -sOUTPUTFILE='+root[i]+tag+'.pdf '+ $
            '-c "<< /PageSize ['+ $
            STRING(ROUND(plot_set.xs*170./6),format='(i0)')+' '+ $
            STRING(ROUND(plot_set.ys*170./6),format='(i0)')+ $
            '] >> setpagedevice" '+ $
            '-dBATCH '+root[i]+tag+'.N[0-9][0-9].eps > /dev/null'
      if ~KEYWORD_SET(keep_ps) then $
         FILE_DELETE,FILE_SEARCH(root[i]+tag+'.N[0-9][0-9].eps')
      CD,cur_cd
   endfor
   SET_PLOT,'x'
   TVLCT, save_ct

   if KEYWORD_SET(stop) then STOP
END
