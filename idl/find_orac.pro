;+
; NAME:
;   FIND_ORAC
;
; PURPOSE:
;   Assuming the Trac repository structure, this returns the path to a requested
;   revision for an instrument given the base path. Also find the previous 
;   revision (numerically) available for comparions.
;
; CATEGORY:
;   Plotting tools
;
; CALLING SEQUENCE:
;   FIND_ORAC, base_folder, inst, revision, folder, root, nroot [, label=string]
;           [, old=path] [, /compare] [, old_folder] [, old_root]
;
; INPUTS:
;   base_folder = The path to the testoutput folder. Defaults to the values of
;      the enviroment variable TESTOUT.
;   instrument  = A string specifying the swath to be plotted. This is expected
;      to be one of the values of $label in trunk/tools/test-preproc.sh
;   revision    = The revision number to be plotted.
;
; OPTIONAL INPUTS:
;   LABEL       = A very short description that should be printed at the top-left
;      of each page.
;	
; KEYWORD PARAMETERS:
;   COMPARE     = Plot the differences between this revision and the previous.
;      If there are none, plot the image with a grey border.
;	
; OUTPUTS:
;   folder      = Path to requested revision.
;   root        = Base filename(s) of requested revision.
;   nroot       = Length of returned root array.
;   oldfolder   = Path to requested previous revision.
;   oldroot     = Base filename(s) of requested previous revision.
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
PRO FIND_ORAC, fdr, inst, rev, folder, root, nroot, label=label, $
               old=old, compare=comp, oldfolder, oldroot
   ON_ERROR, 2
   COMPILE_OPT LOGICAL_PREDICATE, STRICTARR, STRICTARRSUBS

   ver=FILE_SEARCH(fdr+'/V*',/test_dir,count=nver)
   if nver le 0 then MESSAGE,'No data found. Please check FOLDER.'
   ver_num=LONG(STRMID(ver,TRANSPOSE(STRPOS(ver,'V',/reverse_search))+1))
   s=SORT(ver_num)
   p=WHERE(ver_num[s] eq rev,np)
   if np ne 1 then MESSAGE,'Requested revision not be found.'
   folder=ver[s[p[0]]]+'/'+inst+'/'

   ;; find root filenames (assuming location preproc file exists)
   root=FILE_SEARCH(folder+'*.loc.nc',count=nroot)
   if nroot le 0 then MESSAGE,'No .LOC.NC files could be found.'
   for i=0,nroot-1 do root[i]=STRMID(root[i],0,STRLEN(root[i])-7)

   if ~KEYWORD_SET(label) then label=STRING(ver_num[s[p[0]]],format='(i0)')

   if KEYWORD_SET(comp) then begin
      if KEYWORD_SET(old) then begin
         ;; find root folder for requested previous revision
         p=WHERE(ver_num[s] eq old,np)
         if np ne 1 then MESSAGE,'Requested previous revision not be found.'
         oldfolder=ver[s[p[0]]]+'/'+inst+'/'
      endif else begin
         ;; find previous version
         if p[0]-1 lt 0 then MESSAGE,'No previous revision found.'
         oldfolder=ver[s[p[0]-1]]+'/'+inst+'/'
      endelse

      ;; find previous root filenames
      oldroot=FILE_SEARCH(oldfolder+'*.loc.nc',count=noroot)
      if noroot ne nroot then MESSAGE,'Mismatched revisions compared.'
      for i=0,nroot-1 do $
         oldroot[i]=STRMID(oldroot[i],0,STRLEN(oldroot[i])-7)
   endif

END
