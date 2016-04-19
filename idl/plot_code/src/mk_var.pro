;============================================================================
;+
; function MK_VAR
;
; Description
;	Create variable from size information (like IDLs mk_array, but includes
;	scalar variables too) - NOTE: doesn't do structures
;
; Arguments
;	X size of variable to be created (from IDLs SIZE function)
;
; Keywords
;  _EXTRA passed to mk_array function
;
; Date
;	B. Latter : 24th October 2000
;-
;============================================================================
function mk_var,x,_Extra=extra

;check dimension of variable, then treat as scalar or vector (no structures)
if x(0) eq 0 then begin
;scalar variables
	case long(x(1)) of
		1:	y=0b
		2:	y=0
		3:	y=0l
		4:	y=0e
		5:	y=0d
		6:	y=complex(0)
		7:	y=''
	else:	begin
		print,'type not defined, check file'
		stop
		end
	endcase
endif else begin
;arrays
	y=make_array(size=x,_Extra=extra)
endelse
return,y
end
