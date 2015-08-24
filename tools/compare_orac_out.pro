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
;   COMPARE_ORAC_OUT
;
; INPUTS:
;   NOTE - These are provided as command line arguments and NOT passed to the
;          routine in the usual fashion.
;   folder = full path of the folder in which the preprocessor files are found
;   revision_number = a string giving the revision number of your working copy
;   mode   = if set to 'preproc', compares the preprocessor outputs. Otherwise,
;            it compares the main processor outputs
;   ninst  = an integer giving the number of instruments to be evaluated
;   list   = the remaining arguments are ninst strings giving the labels for
;            the files to be evaluated
;   thresh = OPTIONAL. The check for numerical changes includes an output of how
;            many values have changed by more than the expected rounding error.
;            This sets that threshold. Default 1d-6.
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
;      fractional difference greater than THRESH (~ rounding error).
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Assumes file system of ORAC testoutput directory ./VXXXX/INST/basename.suffix
;   Not able to deal with comparing files with different basenames.
;
; MODIFICATION HISTORY:
;   Written by ACPovey (povey@atm.ox.ac.uk)
;   01 Nov 2013 - AP: Original version
;   27 Jan 2014 - AP: Added functionality for main processor
;   04 Feb 2014 - AP: Added CONFIG file.
;   14 Feb 2014 - AP: Added arguments NINST and LABELS to limit scope of
;                 script when only processing a few cases. Fixed version number
;                 bug.
;   29 Apr 2014 - AP: New folder structure.
;   11 Jul 2014 - AP: Changed threshold to 1d-6 rather than 2d-7.
;   14 Jul 2014 - AP: Made threshold an argument.
;   01 Aug 2014 - AP: Can deal with fields being removed.
;   30 Jan 2014 - AP: Remove UV file. Change formatting for points differing
;                 below rounding error threshold.
;   20 Aug 2015 - AP: Made specification of instruments and file extensions
;                 mandatory, replacing the mode argument. Added use of nccmp to
;                 check changes in metadata (i.e. attributes). Replaced fatal
;                 errors with text warnings. Added colour to printing.
;-

PRO COMPARE_ORAC_OUT
   quiet  = !quiet
   !quiet = 1

   args=COMMAND_LINE_ARGS(count=nargs)
   fdr=args[0]
   revision=args[1]
   ;; Fetch list of instruments to compare
   ninst=FIX(args[2])
   list=STRARR(ninst)
   for i=0,ninst-1 do list[i]=args[3+i]
   ;; Fetch list of file extensions to compare
   next=FIX(args[3+ninst])
   a=STRARR(next)
   for i=0,next-1 do a[i]=args[4+ninst+i]+'.nc'
   thres = nargs gt 4+ninst+next ? FLOAT(args[4+ninst+next]) : 1d-6


   ;; available versions
   ver=FILE_SEARCH(fdr+'/V*',/test_dir,count=nver)
   if nver lt 2 then begin
      PRINT, 'Insufficient file revisions found.', fdr, revision
      RETURN
   endif
   ;; translate version # into numeral for sorting
   ver=ver[SORT(FIX(STRMID(ver,TRANSPOSE(STRPOS(ver,'V'))+1)))]
   p=WHERE(STRMATCH(ver,'*V'+revision),np)
   if np ne 1 then begin
      PRINT, 'Bad folder structure.', fdr, revision
      RETURN
   endif
   old=ver[p-1]
   new=ver[p]
   PRINT, 'Comparing '+new+' against '+old


   ;; Loop over all experiments
   foreach inst,list do begin
      PRINT_COLOUR, inst, /yellow, /bold
      pass=1

      ;; Find root file names
      fs=FILE_SEARCH(old+'/'+inst+'/*'+a[0],count=nf1)
      if nf1 eq 0 then begin
         PRINT, 'No old files.'
         CONTINUE
      endif
      oldf=fs[(SORT(fs))[nf1-1]]
      oldroot=STRMID(oldf,0,STRLEN(oldf)-STRLEN(a[0]))
      fs=FILE_SEARCH(new+'/'+inst+'/*'+a[0],count=nf1)
      if nf1 eq 0 then begin
         PRINT, 'No new files.'
         CONTINUE
      endif
      newf=fs[(SORT(fs))[nf1-1]]
      newroot=STRMID(newf,0,STRLEN(newf)-STRLEN(a[0]))

      ;; Open all files with that root
      for i=0,N_ELEMENTS(a)-1 do begin
         COMPARE_NCDF_FILES, oldroot, newroot, a[i], thres, pass

         SPAWN, 'nccmp -m '+oldroot+a[i]+' '+newroot+a[i], screen
         if TOTAL(STRLEN(screen)) gt 0 then begin
            PRINT, screen
            pass = 0
         endif
      endfor

      if pass then PRINT_COLOUR, '--- PASSED ---', /green
   endforeach

   !quiet = quiet
END
