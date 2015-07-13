;; Script to inspect headers of ORAC Fortran 90 files and check if they conform
;; to the standard formatting.
;;
;; 13 Jul 2015, AP: Original version
;;

line=''

;; Loop over all F90 files in this directory
f=FILE_SEARCH('~/orac/trunk/src/*F90',count=nf)
for fi=0,nf-1 do begin
   ;; Open file
   OPENR, id, f[fi], /get_lun
   lead = FILE_BASENAME(f[fi])
   module=0

   ;; Dashed line as the first line. If not, quit as may be external code.
   READF, id, line
   if line ne '!-------------------------------------------------------------------------------' then begin
      PRINT, lead+': Top line. Stopping this file check'
      GOTO, file_end
   endif

   ;; Name field should be the filename followed by blank line
   READF, id, line
   if line ne '! Name: '+lead then PRINT, lead+': Name'
   READF, id, line
   if line ne '!' then PRINT, lead+': Gap after name'

   ;; Purpose, followed by a continuous block of text
   READF, id, line
   if line ne '! Purpose:' then PRINT, lead+': Purpose'
   while line ne '!' do begin
      ;; Check if file defines a module
      READF, id, line
      if STREGEX(line, '.*[Mm]odule.*') ge 0 then module=1
   endwhile

   if ~module then begin
      ;; Modules don't have to define these fields
      
      ;; If this isn't defined, the formatting is often completely off so quit
      READF, id, line
      if line ne '! Description and Algorithm details:' then begin
         PRINT, lead+': Description. Stopping this file check'
         GOTO, file_end
      endif

      ;; Formatting is free-form, so simply search for start of arguments
      while line ne '! Arguments:' do READF, id, line

      ;; Check heading for arguments
      READF, id, line
      if STREGEX(line,"^! Name +Type +In/Out/Both +Description") lt 0 then PRINT, lead+': Argument header'
      READF, id, line
      if line ne '! ------------------------------------------------------------------------------' then PRINT, lead+': Arg line.'
      
      ;; Could generate a lot of the arguments automatically from the top of the
      ;; code. For now, just ignore them
      while line ne '!' do READF, id, line

      ;; Label for history section
      READF, id, line
      if line ne '! History:' then PRINT, lead+': History'
   endif else begin
      ;; For modules, skip over free-format lines until History section located
      while line ne '! History:' do READF, id, line
   endelse

   ;; Check comment lines
   READF, id, line
   repeat begin
      ;; If the first character is a number, assume it's a date and begins a
      ;; new comment
      if STREGEX(STRMID(line, 2, 1), '[0-9]') ge 0 then begin
         ;; Check for YYYY/MM/DD, ??: formatting
         if STREGEX(line, "^! [0-9][0-9][0-9][0-9]/[0-9][0-9]/[0-9][0-9], [A-Z][A-Z]:*") lt 0 then PRINT, lead+': Bad comment start. ', line

      endif else if STREGEX(line, '.*ECV work.*') ge 0 then begin
         ;; Check for approriate style of old/new code line
         if line ne '!    **************** ECV work starts here *************************************' then PRINT, lead+': *** ECV work ***'
      endif else begin
         ;; Check for three-space indenting beneath comment
         if STREGEX(line, "^!    [A-z0-9\(']+") lt 0 then PRINT, lead+': Bad comment continuation. ', line
      endelse

      ;; Check we haven't reached the end of a badly formatted header
      READF, id, line
      if STRMID(line, 0, 1) ne '!' then begin
         PRINT, lead+': Blank line encountered.'
         GOTO, file_end
      endif
   endrep until line eq '!'

   ;; Check presence and use of SVN Id keyword
   READF, id, line
   if line eq '! $Id$' then PRINT, lead+': SVN Keyword not set.'
   if STREGEX(line, "^! \$Id.*\$$") lt 0 then PRINT, lead+': ID.'
   READF, id, line
   if line ne '!' then PRINT, lead+': After Id'

   ;; Check bugs section
   READF, id, line
   if line ne '! Bugs:' then PRINT, lead+': Bugs'
   READF, id, line
   if STREGEX(line, '.*None.*') ge 0 then if line ne '! None known.' then PRINT, lead+': None known'

   ;; Tidy up
file_end:
   CLOSE, id
   FREE_LUN, id
endfor


END
