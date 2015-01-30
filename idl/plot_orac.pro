;+
; NAME:
;   PLOT_ORAC
;
; PURPOSE:
;   Produce quick-look plots for the outputs of ORAC. The parameters of these
;   plots are automatically selected where possible, though most can be
;   controlled through the PLOT_SETTINGS routine.
;
; CATEGORY:
;   ORAC plotting tools
;
; CALLING SEQUENCE:
;   PLOT_ORAC, instrument, revision, folder [, /compare [, /diff_only]]
;           [, /preproc] [, prev_revision=value] [, root=string] [, xsize=value]
;           [, ysize=value] [, nx=value] [, ny=value] [, font_size=value]
;           [, scale=value] [, label=string] [, /relative] [, frames=value]
;           [, /keep_ps] [, /wat|/ice] [, /secondary] [, /diff_only]
;           [, /full] [, /clear] [, add=string]
;
; INPUTS:
;   instrument = A string specifying the swath to be plotted. This is expected
;      to be one of the values of $label in trunk/tools/test-preproc.sh
;   revision   = The revision number to be plotted.
;
; OPTIONAL INPUTS:
;   folder     = The path to the testoutput folder. Defaults to the values of
;      the enviroment variable TESTOUT.
;   PREV_REVISION = When using COMPARE, use this keyword to specify the previous
;      revision to be compared against if you do not want the software to
;      determine that automatically.
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
;   FRAMES     = The number of figures across which to plot the swath.
;   ADD        = A string to add to the end of the filename. 
;
; KEYWORD PARAMETERS:
;   COMPARE    = Plot the differences between this revision and the previous.
;      If there are none, plot the image with a grey border.
;   PREPROC    = Plot the contents of the preprocessor output rather than the
;      main processor.
;   RELATIVE   = In a COMPARE plot, rather than plotting the absolute difference
;      between the revisions, plot the relative difference.
;   KEEP_PS    = Do not delete the postscript plots after combining them into
;      a PDF.
;   WAT|ICE    = Search for the WAT|ICE cloud phase outputs rather than the
;      postprocessed file.
;   SECONDARY  = Plot the contents of the secondary output file after the primary
;   DIFF_ONLY  = When COMPARE plotting, only plot fields that have changed
;      (leaving out the grey-bordered plots).
;   FULL       = Use the full range of the data for all colourbar plots.
;   CLEAR      = Filter out points labelled as PHASE=0 (clear/unknown).
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
;   When not using the ROOT keyword, assumes the folder structure of ORAC outputs
;   is $TESTOUT/V####/INST/*.SUFFIX.nc, where # is a number; INST is the label
;   for the datafile (such as DAYMYD); and SUFFIX is one of the suffixes accepted
;   by PLOT_SETTINGS.
;
; MODIFICATION HISTORY:
;   15 Jul 2014 - ACP: Initial version (povey@atm.ox.ac.uk).
;   22 Jul 2014 - ACP: Replaced LINES with FRAMES.
;   28 Jul 2014 - ACP: Fixed processing of chunked outputs. Moved shared code
;      into subroutines. Expanded definition of FILTER.
;   19 Aug 2014 - ACP: Bug in DIFF_ONLY keyword.
;   19 Sep 2014 - ACP: New RTM output field names.
;   07 Oct 2014 - ACP: Switched RTM array ordering.
;   09 Dec 2014 - ACP: Change plot title to something useful.
;   20 Jan 2015 - ACP: Change layers plotted for RTM to be more interesting.
;-
PRO PLOT_ORAC, inst, rev, fdr, stop=stop, compare=comp, preproc=preproc, $
               prev_revision=old, root=root, xsize=xs, ysize=ys, nx=nx, ny=ny, $
               font_size=font_s, scale=scale, label=label, relative=rel, $
               frames=frames, keep_ps=keep_ps, ice=ice, secondary=secondary, $
               diff_only=diff_only, short=short, wat=wat, suffix=suff, $
               full=full, clear=clear, add=add
   ON_ERROR, KEYWORD_SET(stop) ? 0:2
   COMPILE_OPT LOGICAL_PREDICATE, STRICTARR, STRICTARRSUBS

   ;; process inputs
   if ~KEYWORD_SET(fdr) then fdr=GETENV('TESTOUT')
   if SIZE(inst,/type) ne 7 then MESSAGE,'INSTRUMENT must be a string.'
   kc=KEYWORD_SET(comp)
   if ~KEYWORD_SET(frames) then frames=1
   SET_PLOT,'ps'

   if KEYWORD_SET(root) then begin
      ;; root filenames provided as arguments
      nroot=N_ELEMENTS(root)
      folder=STRMID(root[0],0,STRPOS(root[0],'/',/reverse_search))
      if kc then begin
         if N_ELEMENTS(comp) ne nroot then $
            MESSAGE,'Mismatched root filename arrays given.'
         oldroot=comp
      endif
   endif else begin
      FIND_ORAC, fdr, inst, rev, folder, root, nroot, label=label, $
                 old=old, comp=comp, oldfolder, oldroot
   endelse

   if KEYWORD_SET(preproc) then begin
      ;; plot preprocessor outputs
      tag='.preproc'
      if ~KEYWORD_SET(suff) then $
         suff='.' + ['msi','clf','lsf','alb','geo','loc', $
                     'prtm','lwrtm','swrtm','uv','config'] + '.nc'
   endif else begin
      ;; plot ORAC retrieval
      tag='.orac'
      if ~KEYWORD_SET(suff) then $
         suff = (KEYWORD_SET(secondary) ? (['.primary.nc','.secondary.nc']) : $
                 (['.primary.nc']))
      if KEYWORD_SET(wat) then begin
         tag='WAT'+tag
         suff='WAT'+suff
      endif else if KEYWORD_SET(ice) then begin
         tag='ICE'+tag
         suff='ICE'+suff
      endif
   endelse
   if kc then tag+='.comp'
   if KEYWORD_SET(add) then tag+='.'+add

   ;; save current colourbar and set greyscale for plot titles
   TVLCT, save_ct, /get
   LOADCT,0,/silent

   ;; loop over chunks
   for i=0,nroot-1 do begin
      if KEYWORD_SET(preproc) then begin
         ;; fetch lat/lon for both data grids
         fid=NCDF_OPEN(root[i]+'.loc.nc')
         lat=NCDF_OBTAIN(fid,'lat')
         lon=NCDF_OBTAIN(fid,'lon')
         NCDF_CLOSE,fid

         fid=NCDF_OPEN(root[i]+'.prtm.nc')
         NCDF_DIMINQ,fid,NCDF_DIMID(fid,'nlon_rtm'),nm,nl2
         NCDF_DIMINQ,fid,NCDF_DIMID(fid,'nlat_rtm'),nm,line2
         lat_rtm=REPLICATE(1.,nl2)#NCDF_OBTAIN(fid,'lat_rtm')
         lon_rtm=NCDF_OBTAIN(fid,'lon_rtm')#REPLICATE(1.,line2)
         NCDF_CLOSE,fid

         ;; determine field sizes (for chunked plotting)
         sze=SIZE(lat,/dim)
         nl1=sze[0]
         line1=sze[1]
      endif else begin
         ;; fetch lat/lon
         fid=NCDF_OPEN(root[i]+suff[0])
         lat=NCDF_OBTAIN(fid,'lat')
         lon=NCDF_OBTAIN(fid,'lon')
         qcf=NCDF_OBTAIN(fid,'qcflag')
         if KEYWORD_SET(clear) then phs=NCDF_OBTAIN(fid,'phase')
         NCDF_CLOSE,fid

         ;; determine field size (for chunked plotting)
         sze=SIZE(lat,/dim)
         nl1=sze[0]
         line1=sze[1]
      endelse

      SELECT_FRAMES, lat, lon, line1, nl1, frames, cent, lim

      plot_set=INIT_PLOT_SET(frames, lim, cent, label, font_s=font_s, $
                             xsize=xs, ysize=ys, nx=nx, ny=ny, scale=scale)

      ;; loop over files
      for j=0,N_ELEMENTS(suff)-1 do begin
         if ~FILE_TEST(root[i]+suff[j],/regular) then CONTINUE
         plot_set.tag = root[i]+tag

         ;; open file
         fid=NCDF_OPEN(root[i]+suff[j])
         if kc then fid2=NCDF_OPEN(oldroot[i]+suff[j])
         inq=NCDF_INQUIRE(fid)

         ;; loop over variables
         set=PLOT_SETTINGS(suff[j], inst)
         for k=0,N_ELEMENTS(set)-1 do begin
            ;; read values
            data=NCDF_OBTAIN(fid, set[k].name, fill)
            if kc then data2=NCDF_OBTAIN(fid2, set[k].name, fill2)

            ;; filter out missing values
            if FINITE(fill) then begin
               filt = data ne fill
               if kc then filt = filt AND data2 ne fill2
            endif else begin
               filt = FINITE(data)
               if kc then filt = filt AND FINITE(data2)
            endelse
            ;; apply requested filter
            if KEYWORD_SET(qcf) then filt = filt AND ~(qcf AND set[k].filter)
            if KEYWORD_SET(clear) then filt=filt AND phs gt 0
            ;; set output if plotting a comparison
            plot_set.col=0
            if kc then begin
               pq=WHERE(filt)
               if ARRAY_EQUAL(data[pq],data2[pq]) then begin
                  if KEYWORD_SET(diff_only) then CONTINUE else plot_set.col=100
               endif else begin
                  ;; make a difference plot, overriding plot settings
                  set[k].abs=1
                  set[k].log=0
                  set[k].nlevels=250
                  set[k].btf='(g0.4)'
                  set[k].range[0] = !values.f_nan
                  if KEYWORD_SET(rel) then data=DOUBLE(data)/DOUBLE(data2)-1d0 $
                  else data=DOUBLE(data)-DOUBLE(data2)
               endelse
            endif
            if KEYWORD_SET(full) then set[k].full=1

            ;; select appropriate plot
            case set[k].mode of
               0: begin
                  ;; if at end of page, start new sheet
                  PLOT_POSITION, plot_set, pos, bpos, debug=stop

                  ;; determine range for plot colourbar
                  ran = FINITE(set[k].range[0]) ? set[k].range : $
                        SELECT_RANGE(data[WHERE(filt)], set[k])

                  ;; print title
                  XYOUTS,/normal,align=0.5,.5*(pos[0]+pos[2]), $
                         color=plot_set.col, $
                         pos[3] + (set[k].mode gt 0 ? 2.:.5)* $
                         !d.y_ch_size/!d.y_size, $
                         set[k].title eq '' ? FMT(set[k].name) : set[k].title

                  ;; plot line
                  PLOT,data,/noerase,position=pos,color=plot_set.col, $
                       xrange=[0,N_ELEMENTS(data)-1],xstyle=1, $
                       yrange=ran,ystyle=1,ylog=set[k].log,psym=-4
               end
               1: WRAP_MAPPOINTS, data, lat, lon, $
                                  debug=stop, short=short, $
                                  set[k],plot_set,filt,line1,nl1,0.1
               2: for l=0,N_ELEMENTS(data[0,0,*])-1 do $
                  WRAP_MAPPOINTS, data[*,*,l], lat, lon, $
                                  debug=stop, short=short, $
                                  set[k],plot_set,filt[*,*,l],line1,nl1,0.1,l
               3: WRAP_MAPPOINTS, data, lat_rtm, lon_rtm, $
                                  debug=stop, short=short, $
                                  set[k],plot_set,filt,line2,nl2,0.5
               4: for l=0,N_ELEMENTS(data[*,0,0])-1 do $
                  WRAP_MAPPOINTS, data[l,*,*], lat_rtm, lon_rtm, $
                                  debug=stop, short=short, $
                                  set[k],plot_set,filt[l,*,*],line2,nl2,0.5,l
               5: for l=10,N_ELEMENTS(data[*,0,0])-1,20 do $
                  WRAP_MAPPOINTS, data[l,*,*], lat_rtm, lon_rtm, $
                                  debug=stop, short=short, $
                                  set[k],plot_set,filt[l,*,*],line2,nl2,0.5,l
               6: for m=0,N_ELEMENTS(data[*,0,0,0])-1 do $
                  for l=10,N_ELEMENTS(data[0,*,0,0])-1,20 do $
                     WRAP_MAPPOINTS, data[m,l,*,*], lat_rtm, lon_rtm, $
                                     debug=stop, short=short, $
                                     set[k],plot_set,filt[m,l,*,*],line2,nl2,0.5,l,m
            endcase
         endfor
      endfor
      DEVICE,/close

      ;; Change plot titles to something useful
      ff=FILE_SEARCH(root[i]+tag+'.N[0-9][0-9].eps', count=nff)
      for j=0,nff-1 do begin
         SPAWN, "cat "+ff[j]+" | sed 's/%%Title:.*/" + $
                "%%Title: "+inst+' V'+plot_set.label+"/' >! "+root[i] + $
                'scratch.eps'
         FILE_MOVE, root[i]+'scratch.eps', ff[j], /overwrite
      endfor

      ;; merge individual PDFs and delete pieces
      CD,folder,current=cur_cd
      SPAWN,'gs -dNOPAUSE -sDEVICE=pdfwrite -sOUTPUTFILE='+root[i]+tag+'.pdf '+ $
            '-c "<< /PageSize ['+ $
            STRING(ROUND(plot_set.xs*170./6),format='(i0)')+' '+ $
            STRING(ROUND(plot_set.ys*170./6),format='(i0)')+ $
            '] >> setpagedevice" '+ $
            '-dBATCH '+root[i]+tag+'.N[0-9][0-9].eps > /dev/null'
      CD,cur_cd
      if ~KEYWORD_SET(keep_ps) then begin
         fdel = FILE_SEARCH(root[i]+tag+'.N[0-9][0-9].eps', count=ndel)
         if ndel gt 0 then FILE_DELETE, fdel
      endif
   endfor
   SET_PLOT,'x'
   TVLCT, save_ct

   if KEYWORD_SET(stop) then STOP
END
