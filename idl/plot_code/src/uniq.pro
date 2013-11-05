; $Id: //depot/Release/IDL_81/idl/idldir/lib/uniq.pro#1 $
;
; Copyright (c) 1988-2011, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.

;+
; NAME:
;	UNIQ
;
; PURPOSE:
;	Return the subscripts of the unique elements in an array.
;
;	Note that repeated elements must be adjacent in order to be
;	found.  This routine is intended to be used with the SORT
;	function.  See the discussion of the IDX argument below.
;
;	This command is inspired by the Unix uniq(1) command.
;
; CATEGORY:
;	Array manipulation.
;
; CALLING SEQUENCE:
;	UNIQ(Array [, Idx])
;
; INPUTS:
;	Array:	The array to be scanned.  The type and number of dimensions
;		of the array are not important.  The array must be sorted
;		into monotonic order unless the optional parameter Idx is 
;		supplied.
;
; OPTIONAL INPUT PARAMETERS:
;	IDX:	This optional parameter is an array of indices into Array
;		that order the elements into monotonic order.
;		That is, the expression:
;
;			Array(Idx)
;
;		yields an array in which the elements of Array are
;		rearranged into monotonic order.  If the array is not
;		already in monotonic order, use the command:
;
;			UNIQ(Array, SORT(Array))
;
;		The expression below finds the unique elements of an unsorted
;		array:
;
;			Array(UNIQ(Array, SORT(Array)))
;
; OUTPUTS:
;	An array of indicies into ARRAY is returned.  The expression:
;
;		ARRAY(UNIQ(ARRAY))
;
;	will be a copy of the sorted Array with duplicate adjacent
;	elements removed.
;
; COMMON BLOCKS:
;	None.
;
; MODIFICATION HISTORY:
;	1988, AB, Written.
;	29 July 1992, ACY - Corrected for case of all elements the same.
;	Nov, 1995.  DMS, Return a 0 if argument is a scalar.
;
;-
;

function UNIQ, ARRAY, IDX
  compile_opt idl2, hidden

; Check the arguments.
  s = size(ARRAY)
  if (s[0] eq 0) then return, 0		;A scalar
  if n_params() ge 2 then begin		;IDX supplied?
     q = array[idx]
     indices = where(q ne shift(q,-1), count)
     if (count GT 0) then return, idx[indices] $
     else return, n_elements(q)-1
  endif else begin
     indices = where(array ne shift(array, -1), count)
     if (count GT 0) then return, indices $
     else return, n_elements(ARRAY)-1
  endelse
end
