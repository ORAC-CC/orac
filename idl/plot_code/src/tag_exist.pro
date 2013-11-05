;+
; NAME:        
;	TAG_EXIST()
; PURPOSE:              
; 	To test whether a tag name exists in a structure.
; EXPLANATION:               
;	Routine obtains a list of tagnames and tests whether the requested one
;	exists or not. The search is recursive so if any tag names in the 
;	structure are themselves structures the search drops down to that level.
;	(However, see the keyword TOP_LEVEL).
;               
; CALLING SEQUENCE: 
;	status = TAG_EXIST(str, tag, [ INDEX =, /TOP_LEVEL ] )
;    
; INPUT PARAMETERS:     
;	str  -  structure variable to search
;	tag  -  tag name to search for, scalar string
;
; OUTPUTS:
;	Function returns 1b if tag name exists or 0b if it does not.
;                              
; OPTIONAL INPUT KEYWORD:
;	TOP_LEVEL = If set, then only the top level of the structure is
;			    searched.
;
; OPTIONAL OUTPUT KEYWORD:
;	INDEX = index of matching tag, scalar longward, -1 if tag name does
;		not exist
;
; EXAMPLE:
;	Determine if the tag 'THICK' is in the !P system variable
;	
;	IDL> print,tag_exist(!P,'THICK')
;
; PROCEDURE CALLS:
;	DATATYPE()
;
; MODIFICATION HISTORY:     : 
;	Written,       C D Pike, RAL, 18-May-94               
;	Passed out index of matching tag,  D Zarro, ARC/GSFC, 27-Jan-95     
;	William Thompson, GSFC, 6 March 1996    Added keyword TOP_LEVEL
;	Zarro, GSFC, 1 August 1996    Added call to help 
;-            

function tag_exist, str, tag,index=index, top_level=top_level

;
;  check quantity of input
;
if n_params() lt 2 then begin
   print,'Use:  status = tag_exist(structure, tag_name)
   return,0b
endif

;
;  check quality of input
;

if datatype(str,1) ne 'Structure' or datatype(tag,1) ne 'String' then begin
   if datatype(str) ne 'STC' then help,str
   if datatype(tag) ne 'STR' then help,tag
   print,'Use: status = tag_exist(str, tag)'
   print,'str = structure variable'
   print,'tag = string variable'
   return,0b
endif

i=-1
tn = tag_names(str)
nt = where(tn eq strupcase(tag)) & index=nt(0)
if nt(0) eq -1 then begin
   status = 0b
   if not keyword_set(top_level) then begin
      for i=0,n_elements(tn)-1 do begin
        if datatype(str.(i),1) eq 'Structure' then $
		status=tag_exist(str.(i),tag,index=index)
        if status eq 1b then return,status
      endfor
   endif
   return,0b
endif else begin
   return,1b
endelse
end

