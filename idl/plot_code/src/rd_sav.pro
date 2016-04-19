;===========================================================================
;+
;	function RD_SAV
;
;	Description
; RESTORE saved variable and return to allow renaming.
; 'wr_sav.pro' will always save variable as X, calling this code allows the
; saved variable to be renamed for current code usage.
; New wr_structure code using SAVE/RESTORE (much faster and works on all
; data types)
; Code also compatible with R.Siddans rd_/wr_struct.pro (.str suffix)
;
;	Parameters
; FILENAME filename to restore (.sav added if required)
;
;	Keywords 
;  I  _EXTRA extra inputs passed to rd_struct.pro
;
;	Date
;	B. Latter : 14th June 2001
;-
;===========================================================================
function rd_sav,filename,_extra=extra
sav_file_suffix='.sav' ; standard suffix to use
fname=fname(filename,a=path,b=b) ; check for suffix and remove
; read old style structure if '.str' suffix
if b eq '.str' then return,rd_struct(filename,_extra=extra)
if b ne sav_file_suffix then fname=fname+b
; remove '$' and other wildcards for v5.5
if !version.release gt 5.4 then path=expand_env(path)
if not file_test(path+fname+sav_file_suffix) then return,0
restore,path+fname+sav_file_suffix
return,x
end
