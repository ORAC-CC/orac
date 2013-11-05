; $Id: //depot/idl/IDL_71/idldir/lib/map_struct_append.pro#1 $
;
; Copyright (c) 2004-2009, ITT Visual Information Solutions. All
;       rights reserved. Unauthorized reproduction is prohibited.

; Undocumented helper routines needed for
; MAP_SET, MAP_GRID, MAP_CONTINENTS.
;
; CT, RSI, June 2004: Split off from map_set.pro

;------------------------------------------------------------------------
PRO map_struct_append, In_struct, Name, Value, SUPERCEDE = super
    ;
    ; Append/Replace the tagname Name, with a given value to the In_Struct.
    ; If SUPERCEDE is set, and a tag with the given Name already exists,
    ;   replace its value.
    ; If SUPERCEDE is NOT set, and the given tag exists, do nothing.
    ;
    COMPILE_OPT hidden

    ntags=N_TAGS(In_struct)

    if ntags eq 0 then begin        ;If In_struct is undef, just return the tag
        In_struct = CREATE_STRUCT(name, value)
    endif else begin
        tnames=tag_names(IN_STRUCT)
        index=where(tnames eq NAME, count)
        if count eq 0 then $        ;No match, append
          In_Struct = CREATE_STRUCT(in_struct, name, value) $
        else if keyword_set(super) then $
          in_struct.(index[0]) = value ;Overwrite value?
                                    ;Otherwise, tag is already there, don't add
endelse
end



