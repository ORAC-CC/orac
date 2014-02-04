;+
; NAME:
;   COMPARE_ORAC_OUT
; 
; PURPOSE:
;   To compare the NetCDF files output by the ORAC processor to determine
;   if changes to the code between your current working version and a previous
;   committed version have affected the results.
;
;   This is used in run-time mode by the scripts test_preproc.sh and 
;   test_orac.sh. To produce the SAV file necessary for that, run the following
;   in IDL:
;      .COMPILE compare_orac_out
;      RESOLVE_ALL
;      SAVE,filename='compare_orac_out.sav',/routines
;
; CATEGORY:
;   ORAC test framework
;
; CALLING SEQUENCE:
;   COMPARE_ORAC_OUT, folder, revision_number
;
; INPUTS:
;   folder = full path of the folder in which the preprocessor files are found
;   revision_number = a string giving the revision number of your working copy
;   mode   = if set to 'preproc', compares the preprocessor outputs. Otherwise,
;            it compares the main processor outputs
;
; OPTIONAL INPUTS:
;   None
;	
; KEYWORD PARAMETERS:
;   None
;	
; OUTPUTS:
;   1) Prints each unique root file name it finds.
;   2) If a variable in a file is found to be of a different size between the
;      two versions, it will print:
;         VARIABLE_NAME (EXT,j) - Size mismatch: [x,y,...] [u,v,...]
;      where EXT is the extension of the file, j is the 0-referenced subscript
;      of that variable within the file, and x,y,u,v are the dimensions of the
;      two arrays.
;   3) If the variables are of the same size but their elements are not, it
;      will print a similar line, but giving the number of points that have a
;      fractional difference greater than 2d-7 (~ rounding error).
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
;   01 Nov 2013 - V1.00
;   27 Jan 2014 - V1.10: Added functionality for main processor
;   04 Feb 2013 - V1.11: Added CONFIG file.
;-
PRO COMPARE_ORAC_OUT
   args=COMMAND_LINE_ARGS()
   fdr=args[0]
   revision=args[1]
   mode=args[2]
   
   ;; determine the list of experiments to be considered from start of file name
   if mode eq 'preproc' $
      then a='.'+['alb','config','clf','geo','loc','lsf','msi','uv', $
               'lwrtm','prtm','swrtm']+'.nc' $
      else a='.'+['primary','secondary']+'.nc'
   f=FILE_SEARCH(fdr+'/*'+a[0],count=nf)
   for i=0,nf-1 do begin
      f[i]=STRMID(f[i],STRPOS(f[i],'/',/reverse_search)+1)
      f[i]=STRMID(f[i],0,STRPOS(f[i],'_ORACV'))
   endfor
   list=f[UNIQ(f,SORT(f))]

   ;; loop over all experiments
   foreach inst,list do begin
      PRINT,inst
      pass=1
     
      ;; find root name of specified revision and latest revision before that
      fs=FILE_SEARCH(fdr+'/'+inst+'_ORACV'+'*'+a[0],count=nf1)
      fs=fs[SORT(fs)]
      p=WHERE(STRMATCH(fs,'*ORACV'+revision+'*'),np)
      if (np eq 0) || ((p[0]-1) lt 0) then begin
         PRINT,'--- All files of the same version. ---'
         CONTINUE
      endif
      ;; most recent file of specified revision
      newf=fs[p[np-1]]
      newpath=STRMID(newf,0,STRLEN(newf)-STRLEN(a[0]))
      ;; most recent file of a previous version
      oldf=fs[p[0]-1]
      oldpath=STRMID(oldf,0,STRLEN(oldf)-STRLEN(a[0]))

      ;; open all files with that roof
      for i=0,N_ELEMENTS(a)-1 do begin
         id1=NCDF_OPEN(oldpath+a[i])
         id2=NCDF_OPEN(newpath+a[i])

         ;; loop over all variables in that file
         inq=NCDF_INQUIRE(id1)
         for j=0,inq.nvars-1 do begin
            var=NCDF_VARINQ(id1,j)
            NCDF_VARGET,id1,j,c1
            NCDF_VARGET,id2,j,c2
            if ~ARRAY_EQUAL(c1,c2) then begin
               if N_ELEMENTS(c1) ne N_ELEMENTS(c2) then begin
                  PRINT,var.name+' ('+a[i]+','+ $
                        STRING(j,format='(I0)')+') - Size mismatch: ['+ $
                        STRING(SIZE(c1,/dim),format='(I0)')+'] vs ['+ $
                        STRING(SIZE(c2,/dim),format='(I0)')+']'
                  pass=0
               endif else begin
                  trash=WHERE(ABS((c2-c1)/c1) gt 2d-7,nt)
                  PRINT,var.name,a[i],j,nt, format='(A0," (",A0,",",I0,'+ $
                        '") - ",I0," points are different by > 2d-7")'
                  pass=0
               endelse
            endif
         endfor
         NCDF_CLOSE,id1
         NCDF_CLOSE,id2
      endfor

      if pass then PRINT,'--- PASSED ---'
   endforeach

END
