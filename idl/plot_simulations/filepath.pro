; $Id: //depot/idl/IDL_71/idldir/lib/filepath.pro#1 $
;
; Copyright (c) 1989-2009, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.
;

FUNCTION FILEPATH, FILENAME, ROOT_DIR=root_dir, SUBDIRECTORY=subdir, $
	TERMINAL = TERMINAL, TMP = TMP
;+
; NAME:
;	FILEPATH
;
; PURPOSE:
;	Given the name of a file in the IDL distribution,
;	FILEPATH returns the fully-qualified path to use in
;	opening the file. Operating system dependencies
;	are taken into consideration. This routine is used by RSI to
;	make the User Library portable.
;
; CATEGORY:
;	File Management.
;
; CALLING SEQUENCE:
;	Result = FILEPATH('filename' [, SUBDIRECTORY = subdir])
;
; INPUTS:
;    filename:	The lowercase name of the file to be opened. No device
;		or directory information should be included.
;
; KEYWORDS:
;    ROOT_DIR: The name of the directory from which the resulting path
;	should be based. If not present, the value of !DIR is used.
;	This keyword is ignored if TERMINAL or TMP are specified.
;
;    SUBDIRECTORY:	The name of the subdirectory in which the file
;		should be found. If this keyword is omitted, the main
;		directory is used.  This variable can be either a scalar
;		string or a string array with the name of each level of
;		subdirectory depth represented as an element of the array.
;
;    TERMINAL:	Return the filename of the user's terminal.
;
;    TMP:	The file is a scratch file.  Return a path to the
;		proper place for temporary files under the current operating
;		system.
;
; OUTPUTS:
;	The fully-qualified file path is returned.  If one of the subdirectory
;	keywords is not specified, the file is assumed to exist in the
;	main distribution directory.
;
; COMMON BLOCKS:
;	None.
;
; RESTRICTIONS:
;	ROOT_DIR, TERMINAL, and TMP are mutually exclusive. Only one of
;	these should be used in a single call to FILEPATH. SUBDIRECTORY
;	does not make sense with TERMINAL or TMP.
;
; EXAMPLE:
;	To get a path to the file DETERM in the "userlib" subdirectory to the
;	IDL "lib" subdirectory, enter:
;
;		path = FILEPATH("determ", SUBDIRECTORY = ["lib", "userlib"])
;
;	The variable "path" contains a string that is the fully-qualified file
;	path for the file DETERM.
;
; MODIFICATION HISTORY:
;	December, 1989, AB, RSI (Formalized from original by DMS)
;	October, 1990, SG, RSI (added support for MSDOS)
;	February, 1991, SMR, RSI (added string array support for multi-level
;	    			  directories)
;	21 April 1993, AB, Added ROOT_DIR keyword.
;       14 July  1994, KDB, RSI - Corrected logic error in VMS section
;           of the ROOT_DIR keyword. Any sub-directory specification was
;           being ignored when using ROOT_DIR.
;	March, 1995, DJE, Add a ':' if root_dir is specified on the Mac.
;	29 July 1995, Robert.M.Candey.1@gsfc.nasa.gov, Changed VMS case for
;	    no specified path to not append '.][000000]'
;	April, 1996, DJE, Remove call to STRLOWCASE(SUBDIR).
;	August, 1996, AJH, used environment variables to define TMP on Win32
;	12 January 1998, AB, General cleanup and added 2 improvements for VMS
;           supplied by Paul Hick (pphick@ucsd.edu): (1) Add a colon to the
;           end of ROOT_DIR if it doesn't end in a ':' or ']' to allow
;           root_dir to be a logical name without the trailing ':', and
;           (2) Remove instances of '.][' that result when using rooted
;           logical names for ROOT_DIR. These changes make it easier to use
;           the same FILEPATH call across VMS and other operating systems.
;	28 January 1999, AB, use new behavior of GETTMP('IDL_TMPDIR') to obtain
;	    the correct TMP directory. This means that internal IDL and PRO
;	    code will all treat temporary files the same way.
;	11 February 2009, AY, Remove VMS and MacOS (Mac Classic) branches.
;	    Refactor and simplify code using path separator character returned
;	    by path_sep instead of string constant.
;	
;-


ON_ERROR,2		; Return to caller if an error occurs

do_tmp = KEYWORD_SET(TMP)		;get temporary path if existing
path = ''

IF (KEYWORD_SET(TERMINAL)) THEN BEGIN
  if ((fstat(0)).isagui) then begin
    MESSAGE, 'No terminal device available with IDLde (GUI) interface'
  endif else begin
    path = '/dev/tty'
  endelse

  return, path
ENDIF

sep = PATH_SEP()

IF (do_tmp) THEN BEGIN
  root_dir = GETENV('IDL_TMPDIR')
ENDIF ELSE BEGIN
  IF (not KEYWORD_SET(ROOT_DIR)) THEN root_dir = !DIR
  IF (KEYWORD_SET(SUBDIR)) THEN BEGIN
    ;if the SUBDIR keyword is set then concatenate the directories using
    ; the proper separator character for the current OS.
    FOR i = 0, N_ELEMENTS(SUBDIR) - 1 DO BEGIN
      path = path + SUBDIR[i]
      IF(i NE N_ELEMENTS(SUBDIR) - 1) THEN path = path + sep
    ENDFOR
  ENDIF
ENDELSE


len=strlen(root_dir)
if ((len gt 0) and (STRMID(root_dir, len-1, 1) ne sep)) then path = sep + path
IF ((path ne '') and (path NE sep)) THEN path = path + sep

path = root_dir + path

RETURN, path + filename

END
