;; Script to standardise formatting of an ORAC comment section. Attempts to
;; deal with most of the formats used, but is not exhaustive.
;;
;; 13 Jul 2015, AP: Original version
;;

PRO WRAP, a, l, o
   if STRLEN(a) le l then begin
      o = a
      a = ''
   endif else begin
      p = STRPOS(STRMID(a, 0, l), ' ', /reverse_search)
      o = STRMID(a, 0, p)
      a = STRMID(a, p+1)
   endelse
END

;; Settings for script
f='~/orac/trunk/pre_processing/read_mcd43c3.F90' ; File to process
st_rd_th=1 ; 1=dates printed like "1st" or "15th", 0=dates printed like "1" or "15"

;; Open file and setup
line=''
OPENR, id, f, /get_lun
out = STRARR(300)
nout = -1
buf = ''
if st_rd_th then expected_len=13 else expected_len=11
date_skip = st_rd_th ? '3':''

;; Find History section
while line ne '! History:' do READF, id, line

;; Parse comments
while not EOF(id) do begin
   ;; Read line
   READF, id, line
   col = STRPOS(line, ':')                      ; Locate colon
   year = STREGEX(line, '[0-9][0-9][0-9][0-9]') ; Locate year

   if STRPOS(line, 'ECV work starts here') ge 0 then begin
      ;; Exception for **** style lines

      ;; Wrap previous comment
      WRAP, buf, 78, sect
      ++nout
      out[nout] = '! '+sect
      while buf ne '' do begin
         WRAP, buf, 75, sect
         ++nout
         out[nout] = '!    '+sect
      endwhile

      ;; Write contents unaltered
      ++nout
      out[nout] = line
      buf = ''
   endif else if (col ge 0 and year ge 0) or line eq '!' then begin
      ;; Wrap previous comment
      if buf ne '' then begin
         WRAP, buf, 78, sect
         ++nout
         out[nout] = '! '+sect
         while buf ne '' do begin
            WRAP, buf, 75, sect
            ++nout
            out[nout] = '!    '+sect
         endwhile
      endif

      ;; Check to see if we've reached the end of a badly formatted header
      if line eq '!' or STRPOS(line, 'Bugs:') ge 0 then BREAK

      ;; Parse date
      num = STREGEX(line, '[0-9]')
      com = STRPOS(STRMID(line, 0, col), ',', /reverse_search)
      date = STRMID(line, num, com-num)
      ;; Sort out date string
      if STREGEX(STRMID(line, num+1, 1), '[0-9]') eq 0 then begin
         len = '2' ; Number of characters occupied by day
         ;; Identify if we have long or short month strings
         if com-num gt expected_len then $
            date = STRMID(date,0,expected_len-5)+STRMID(date,4,5,/rev)
      endif else begin
         len = '1'
         if com-num gt expected_len-1 then $
            date = STRMID(date,0,expected_len-4)+STRMID(date,4,5,/rev)
      endelse
 
      ;; Read date string 
      READS, date, format='(c(CDI'+len+','+date_skip+'X,CMoA,X,CYI4))', day

      ;; Parse name (assumed between comma and colon)
      name_sec = STRMID(line, com+2, col-com-2)
      if STRPOS(name_sec, 'Sayer') ge 0 then name='AY' else begin
         ;; Take the first letter and first capital letter (not perfect)
         last = STREGEX(STRMID(name_sec, 1), '[A-Z]')+1
         name = STRMID(name_sec, 0, 1)+STRMID(name_sec, last, 1)
      endelse

      ;; Form new line
      buf = STRING(day, format='(c(CYI04,"/",CMOI02,"/",CDI02,", "))')+name+':'

      ;; Parse possible comments after colon
      comment = STRTRIM(STRMID(line, col+1), 2)
      if STRLEN(comment) gt 0 then buf += ' '+comment
   endif else begin
      ;; Parse continuation line
      comment = STRTRIM(STRMID(line, 1), 2)
      buf = buf+' '+comment
   endelse
endwhile

;; Close and tidy
CLOSE, id
FREE_LUN, id
PRINT, TRANSPOSE(out[0:nout])

END
