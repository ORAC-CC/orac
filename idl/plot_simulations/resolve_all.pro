; $Id: //depot/idl/IDL_71/idldir/lib/resolve_all.pro#1 $
;
; Copyright (c) 1995-2009, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.
;+
; NAME:
;	RESOLVE_ALL
;
; PURPOSE:
;	Resolve (by compiling) all procedures and functions.
;	This is useful when preparing .sav files containing all the IDL
;	routines required for an application.
; CATEGORY:
;	Programming.
; CALLING SEQUENCE:
;	RESOLVE_ALL
; INPUTS:
;	None.
; KEYWORD PARAMETERS:
;	CLASS = if set, a list of object class names. RESOLVE_ALL's rules
;	    for finding uncompiled functions and procedures are not able to
;	    find object definitions or methods, because those things are not
;	    known to IDL until the object classes are actually instantiated
;	    and the methods called. However, if CLASS is set, RESOLVE_ALL
;	    will ensure that the __DEFINE files for those classes and their
;	    superclasses are compiled and execute. If then locates all methods
;	    for those classes and their superclasses and makes sure they are
;	    also compiled.
;	CONTINUE_ON_ERROR = if set, continue when a routine fails to
;	    resolve, otherwise throw an error and stop.
;	QUIET = if set, produce no messages.
;	RESOLVE_EITHER = A scalar or array of routine names to resolve.
;	    Use this keyword instead of RESOLVE_FUNCTION or RESOLVE_PROCEDURE
;	    if you do not know the type of the routine being resolved.
;	    If the routines are already compiled, they are not recompiled.
;	RESOLVE_FUNCTION = a scalar or array of function names to resolve.
;	    If the routines are already compiled, they are not recompiled.
;	RESOLVE_PROCEDURE = a scalar or array of procedure names to resolve.
;	    If the routines are already compiled, they are not recompiled.
; 	SKIP_ROUTINES = an optional string array containing the names
; 	    of routines to NOT resolve.  This is useful when a library
; 	    file containing the designated routines will be later included.
;	UNRESOLVED = if CONTINUE_ON_ERROR is set, this output parameter will
;	    contain the names of the unresolved procedures and functions
;	    in a string array.  Routines in the SKIP_ROUTINES list are
;	    also included in this result.
; OUTPUTS:
;	No explicit outputs.
; COMMON BLOCKS:
;	None.
; SIDE EFFECTS:
; RESTRICTIONS:
;	Will not resolve procedures or functions that are called via
;	CALL_PROCEDURE, CALL_FUNCTION, or EXECUTE, or object methods.
;	Only explicit calls are resolved.
;
;	If an unresolved procedure or function is not in the IDL
;	search path, an error occurs, and no additional routines
;	are resolved unless CONTINUE_ON_ERROR is specified.
;
;	This routine does not support the idea of a function and procedure
;	both having the same name, and does not handle that case. This is
;	generally not a good idea anyway, as it is confusing.
;
; PROCEDURE:
;	This routine iteratively determines the names of unresolved calls
;	to user-written or library procedures and functions, and then
;	compiles them.  The process stops when there are no unresolved
;	routines. If the CLASS keyword is set, this routine first ensures
;	that all the class definitions are compiled (from the __define.pro)
;	file, and that all methods for those classes and their superclasses
;	are compiled.
;
; EXAMPLE:
;	RESOLVE_ALL.
;
; MODIFICATION HISTORY:
; 	Written by:
;	DMS, RSI, January, 1995.
;	DMS, RSI, April, 1997, Added SKIP_ROUTINES keyword.
;	AB, RSI, April 1998, Added CONTINUE_ON_ERROR keyword. Reorganized
;		the body of the resolving code.
;	DMS, Aug, 1998.  Added UNRESOLVED keyword.
;	AB, 13 January 1999, Added RESOLVE_EITHER keyword. Removed the old
;		restriction that only one of the RESOLVE_ keywords are
;		processed in a single call.
;	AB, 6 February 2003, Added CLASS keyword.
;	CT, August 2004: Fix RESOLVE_FUNCTION keyword so it works.
;-

pro resolve_all_class, class, quiet, cont, skipr
  ; Resolve the specified classes. This means that the __DEFINE procedures
  ; have been executed for all classes and their superclasses, and that
  ; all methods available from the current directory and !PATH for those
  ; classes and superclasses have been compiled.
  ;
  ; entry:
  ;	quiet - Quiet keyword from main routine.
  ;	cont - TRUE if should keep going when a routine fails to resolve,
  ;	    FALSE to throw an error and stop.
  ;	do_func - TRUE to process functions, FALSE to process procedures.
  ;	skipr - Uppercase names of routines to skip resolving. This argument
  ;		can be undefined, scalar, or array.
  ;
  ; exit:
  ;	The classes and methods have been resolved if possible.
  ;	The natural action of the main RESOLVE_ALL body will handle
  ;     resolving any functions or procedures that are called by the
  ;     code resolved here.


  COMPILE_OPT hidden
  on_error, 2		; Return to caller if error not caught


  num_skipr = n_elements(skipr)


  ; Get the character used to separate directories in a path, and the
  ; character used to separate paths in a search path.
  psep = path_sep()
  spsep = path_sep(/SEARCH_PATH)

  ; Force every specified class to have its structure defined by instantiating
  ; a copy. This causes IDL to locate and compile any necessary __DEFINE files,
  ; for the classes, and any of their superclasses. After each instantiation,
  ; query for the names of any superclasses, and add these names to a local
  ; copy of the classes we started with. The end result will be a list of
  ; all of the requested classes, and all of the superclasses they use.
  ;
  ; Once this is done, all the classes and their superclasses will be defined,
  ; as will any methods contained in their __DEFINE file. Missing will be
  ; any methods defined in a separate file.
  l_class = class
  for i=0, n_elements(class)-1 do begin
    catch, error
    if error eq 0 then begin
      if (~ execute('a={' + class[i] + '}')) then goto, bad_class
      if (~ quiet) then MESSAGE,/INFORMATIONAL,/NONAME, $
	  'Resolved class structure: ' + strupcase(class[i])
      super = obj_class(class[i], /superclass, count=count)
      if (count ne 0) then l_class = [l_class, super ]
    endif else begin
      ; Object class structure definition not found.
      ; If we're supposed to halt on error, then reissue it so that we jump
      ;
      ; Otherwise, then we continue on error. Take it out of the resolve
      ; list, and let the user see the error. Add it to the skip list so
      ; that the rest of RESOLVE_ALL won't keep trying to resolve it.
      ;
    bad_class:
    catch, /cancel
      if (~ cont) then MESSAGE, /REISSUE_LAST $		; Will usually longjmp
      else if (~ quiet) then HELP, /LAST_MESSAGE
      l_class[i] = ''			; Don't resolve its methods
      s = strupcase(class[i] + '__DEFINE')
      skipr = num_skipr gt 0 ? [skipr, s] : [s];
      num_skipr = num_skipr + 1
    endelse

  endfor
  catch, /cancel
  MESSAGE, /RESET

  ; Force all the names to lowercase. This is necessary on operating systems
  ; with case sensitive file names (Unix) and harmless elsewhere. Then,
  ; eliminate any duplicates so that we only try to resolve the methods for
  ; each class once.
  ;
  l_class = strlowcase(l_class)
  l_class = l_class[uniq(l_class, sort(l_class))]

  ; Search for all files in the current directory or referenced via
  ; !PATH, and build a list of the routine names those files are expected
  ; to contain. Given that list, RESOLVE_ROUTINE will be used to force their
  ; compilation, finishing the process of resolving the class.
  path_elts = [ '.', strtok(!path, spsep, /EXTRACT) ]
  resolve = ''
  for i=0, n_elements(l_class)-1 do begin
    a = file_search(path_elts + psep + l_class[i] + '__*.pro', $
	count=count)
    if (count ne 0) then resolve = [resolve, a ]
    a = file_search(path_elts + psep + l_class[i] + '__*.sav', $
	count=count)
    if (count ne 0) then resolve = [resolve, a ]
    if (~ quiet) then MESSAGE,/INFORMATIONAL,/NONAME, $
	  'Identified method files: ' + strupcase(l_class[i])
  endfor

  ; Extract the routine name from the file name by tossing the directory
  ; part of the path, and the file extension, and converting __ to :: for
  ; methods.
  ;
  ; Use strpos to find the final directory delimiter and add one to it
  ; to reference the following character. If there is no directory delimiter,
  ; STRPOS returns -1, and adding 1 to that makes 0, which is the beginning
  ; of the name also.
  start = strpos(resolve, psep, /REVERSE_SEARCH) + 1
  len = strlen(resolve) - start - 4
  for i=0, n_elements(resolve)-1 do begin
    s = strupcase(strmid(resolve[i], start[i], len[i]))
    pos = STRPOS(s, '__', /REVERSE_SEARCH)
    if ((pos gt 0) and (strmid(s, pos) ne '__DEFINE')) then begin
        ; Make sure we don't have multiple underscores in our method name.
        while (STRMID(s, pos-1, 1) eq '_') do pos--
	    STRPUT, s, '::', pos
	endif
    resolve[i] = s
  endfor

  ; Remove skip routines from the unresolved list.
  if num_skipr gt 0 then begin
    ; Remove skip routines from the unresolved list.
    for i=0, n_elements(resolve)-1 do $
      if (total(resolve[i] eq skipr) ne 0) then resolve[i] = ''
  endif

  ; Use RESOLVE_ROUTINE to force the compilation of the routines we've
  ; identified. Do it one at a time so that we can control error handling
  ; and continuation.
  for i=0, n_elements(resolve)-1 do begin
    if (s = resolve[i]) then begin
      if (cont) then catch, error else error = 0
      if error eq 0 then begin
        RESOLVE_ROUTINE, s, /NO_RECOMPILE, /EITHER
      endif else begin
        ; In case this branch has a programming error, avoid infinite loop
	catch, /cancel
	MESSAGE, /RESET

	; Current routine was not found. Let the user see the error.
	; Add it to the skip list so that the rest of RESOLVE_ALL won't
        ; keep trying to resolve it.
        skipr = num_skipr gt 0 ? [skipr, resolve[i]] : [resolve[i]]
        num_skipr = num_skipr + 1
	if quiet eq 0 then help,/last_message
      endelse
    endif
  endfor
end







function resolve_all_body, quiet, cont, do_func, skipr
;
; Resolve all unresolved procedures or functions if possible.
;
; entry:
;	quiet - Quiet keyword from main routine.
;	cont - TRUE if should keep going when a routine fails to resolve,
;	    FALSE to throw an error and stop.
;	do_func - TRUE to process functions, FALSE to process procedures.
;	skipr - Uppercase names of routines to skip resolving. This argument
;		can be undefined, scalar, or array.
;
; exit:
;	All routines of the specified type have been resolved if possible.
;	Returns the number of routines for which an attempt to resolve was
;	made.
;
;	If cont is TRUE, any routines that fail to resolve are concatenated
;	to skipr so that subsequent calls won't try again.

    COMPILE_OPT hidden
    on_error, 2		; Return to caller if error not caught

    n_unresolved = 0
    num_skipr = n_elements(skipr)
    a = ROUTINE_INFO(FUNCTIONS=do_func, /UNRESOLVED)
    if num_skipr gt 0 then begin
        ; Remove skip routines from the unresolved list.
        j = 0L
        for i=0, n_elements(a)-1 do if total(a[i] eq skipr) eq 0 then begin
            a[j] = a[i]
            j = j + 1
        endif
        if j gt 0 then a = a[0:j-1] else a = ''
    endif
    if strlen(a[0]) gt 0 then begin
	num = n_elements(a)
        n_unresolved = n_unresolved + num
        for i=0, num-1 do begin
            name = a[i]
            if (cont) then catch, error else error = 0
            if error eq 0 then begin
                resolve_routine, name, IS_FUNCTION=do_func
            endif else begin
                ; Current routine was not found. Let the user see the
                ; error, and then add it to the skip list.
                if quiet eq 0 then help,/last_message
                skipr = num_skipr gt 0 ? [skipr, name] : [name]
                num_skipr = num_skipr + 1
                catch, /cancel
                MESSAGE, /RESET
            endelse
        endfor
    endif

    return, n_unresolved
end







PRO resolve_all, CLASS=class, CONTINUE_ON_ERROR=cont, QUIET = quiet, $
	RESOLVE_EITHER=resolve_either, RESOLVE_FUNCTION=resolve_function, $
	RESOLVE_PROCEDURE=resolve_procedure, SKIP_ROUTINES=skip_routines, $
        UNRESOLVED=skipr

    on_error, 2		; Return to caller if error not caught

    if n_elements(quiet) ne 0 then begin
        quiet_save=!quiet
        !quiet = quiet
    endif else quiet = 0

    local_cont = keyword_set(cont)
    if n_elements(skip_routines) gt 0 then skipr = strupcase(skip_routines)

    ; If the CLASS keyword is present, resolve the classes. However,
    ; skip over empty strings.
    n_class = n_elements(class);
    if (n_class ne 0) then begin
	idx = where(strcompress(class, /REMOVE_ALL) ne '', c)
	if (c ne 0) then begin
            resolve_all_class, (c eq n_class) ? class : class[idx], $
		quiet, local_cont, skipr
	endif
    endif

    if keyword_set(resolve_either) then $
	resolve_routine, /NO_RECOMPILE, /EITHER, resolve_either

    if keyword_set(resolve_procedure) then $
	resolve_routine, /NO_RECOMPILE, resolve_procedure

    if keyword_set(resolve_function) then $
	resolve_routine, /NO_RECOMPILE, /IS_FUNCTION, resolve_function



    repeat begin
        cnt = 0
        cnt = cnt + resolve_all_body(quiet, local_cont, 0, skipr)
        cnt = cnt + resolve_all_body(quiet, local_cont, 1, skipr)
    endrep until cnt le 0

done:
    if n_elements(quiet_save) ne 0 then !quiet = quiet_save

end
