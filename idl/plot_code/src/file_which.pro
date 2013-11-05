; $Id: //depot/Release/IDL_81/idl/idldir/lib/file_which.pro#1 $
;
; Copyright (c) 2000-2011, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.
;

function file_which, path, file, INCLUDE_CURRENT_DIR=inc_cur_dir
;+
; NAME:
;       FILE_WHICH
;
; PURPOSE:
;       Look in the directories given by a file search path for a
;       specific file.
;
;       This command is modeled after the Unix which(1) command.
;
; CATEGORY:
;       File Manipulation.
;
; CALLING SEQUENCE:
;       Result = FILE_WHICH([PATH,] FILE)
;
; INPUTS:                 
;       PATH: A search path to be searched.If Path is not
;		present, the value of the IDL !PATH system
;               variable is used.
;       FILE: The file to look for in the directories given by
;               PATH.
;
; KEYWORDS:
;	INCLUDE_CURRENT_DIR:
;	If set, include the current working directory at the
;	head of the PATH. This is useful when trying to simulate
;	the way IDL searches for routines, because it looks in the
;	current directory and then looks at the value of !PATH.
;
; OUTPUTS:
;       If the file is found, the full path is returned. If the
;       file is not found, a NULL scalar string is returned.
;
; RESTRICTIONS:
;       None.
;
; PROCEDURE:
;       The path is separated into it's component directories, and each
;       directory is searched in turn. The first directory that has
;       the file is used. If the file does not exist in any of the
;       directories, a NULL string is returned.
;
; EXAMPLES:
;       To find the location of this routine:
;
;               Result = FILE_WHICH('file_which.pro')
;
;       To find the location of the Unix ls command:
;
;               Result = FILE_WHICH(getenv('PATH'), 'ls')
;
; REVISION HISTORY:
;       11 January 2000         Written AB, RSI
;-

    on_error, 2			; Return to caller on error

    if (n_params() eq 2) then begin
      l_path = path
      l_file = file
    endif else begin
      l_file = path
      l_path = !PATH
    endelse

    if (l_file eq '') then return, ''	; Null file yields null result

    ;t = systime(1)
    os_family = !VERSION.OS_FAMILY
    sep = PATH_SEP()

    path_elts = STRTOK(l_path, PATH_SEP(/SEARCH_PATH), /EXTRACT)
    if (keyword_set(inc_cur_dir)) then begin
	CD, CURRENT=current
	path_elts = [ current, path_elts ]
    endif
    nelts = n_elements(path_elts)
    for i = 0, nelts - 1 do begin
      str = path_elts[i]
      c = strmid(str, 0, 1, /REVERSE_OFFSET)	; Last character
      switch (os_family) of
        'vms': break
        'Windows': if (c eq '/') then break
	    ; Fall through
        else: if (c ne sep) then str = str + sep
      endswitch
      str = str + l_file
      
      if (FILE_TEST(str)) then begin
	;print, systime(1)-t
	return, str		; This is the answer
      endif
    endfor

  ; If we get here, we exhaused the path without finding the file
  ;print, systime(1)-t
  return, ''		; Report failure

end
