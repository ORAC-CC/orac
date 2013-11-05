;+
; DEF_TH
; Return default plot thickness depending on device
; $Id: def_th.pro 404 2010-09-14 13:22:25Z rsiddans $
;-
function def_th
	if !d.name eq 'PS' then return,4 else return,2
end
