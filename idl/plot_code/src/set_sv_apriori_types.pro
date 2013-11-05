;===============================================================================
;+
; SET_SV_APRIORI_TYPES
;
; Modify SV structure to include type dependent a priori values for
; effective radius
;
; PARAMETERS
;	SV	Standard SV structure
;	TYPES	Strings identifying types (tag names of lut structure)
;
; KEYWORDS
;	
;
; R.S. 08/07/11
; $Id$
;-
;===============================================================================
function set_sv_apriori_types,sv,types
	nt=n_elements(types)
	if n_elements(sv.x) gt 1 then message,'Expected scalar SV.X!'
	s=struct_rep(sv,'x',replicate(sv.x,nt))
	s=create_struct(s,{types:types})
;
; define standard first guess, a priori value and error for each known type
;
	aps={   liquid:{fg:8.,ap:8.,ae:1000000.},$
		ice:{fg:23.,ap:23.,ae:1000000.},$
		bbcerrado:{fg:0.8,ap:0.8,ae:0.1},$
		contclean:{fg:0.8,ap:0.8,ae:0.1},$
		desert:{fg:0.8,ap:0.8,ae:0.1},$
		marclean:{fg:0.8,ap:0.8,ae:0.1},$
		volcox:{fg:0.8,ap:0.8,ae:0.1}}
	tn=tag_names(aps)
	for it=0,nt-1 do begin
		ia=(where(tn eq types(it),na))(0)
		if na eq 0 then message,'Type not found: '+types(it)
		s.x(it).cre.fg=aps.(ia).fg
		s.x(it).cre.ap=aps.(ia).ap
		s.x(it).cre.ae=aps.(ia).ae
             endfor
;
;cot
;

cotin=0.8

	aps={   liquid:{fg:cotin,ap:cotin,ae:1000000.},$
		ice:{fg:cotin,ap:cotin,ae:1000000.},$
		bbcerrado:{fg:0.8,ap:0.8,ae:0.1},$
		contclean:{fg:0.8,ap:0.8,ae:0.1},$
		desert:{fg:0.8,ap:0.8,ae:0.1},$
		marclean:{fg:0.8,ap:0.8,ae:0.1},$
		volcox:{fg:0.8,ap:0.8,ae:0.1}}


	tn=tag_names(aps)
	for it=0,nt-1 do begin
		ia=(where(tn eq types(it),na))(0)
		if na eq 0 then message,'Type not found: '+types(it)
		s.x(it).lcot.fg=aps.(ia).fg
		s.x(it).lcot.ap=aps.(ia).ap
		s.x(it).lcot.ae=aps.(ia).ae
             endfor

;
;ctp
;
	aps={   liquid:{fg:800,ap:800,ae:10000000.},$
		ice:{fg:400,ap:400,ae:10000000.},$
		bbcerrado:{fg:0.8,ap:0.8,ae:0.1},$
		contclean:{fg:0.8,ap:0.8,ae:0.1},$
		desert:{fg:0.8,ap:0.8,ae:0.1},$
		marclean:{fg:0.8,ap:0.8,ae:0.1},$
		volcox:{fg:0.8,ap:0.8,ae:0.1}}
	tn=tag_names(aps)
	for it=0,nt-1 do begin
		ia=(where(tn eq types(it),na))(0)
		if na eq 0 then message,'Type not found: '+types(it)
		s.x(it).pc.fg=aps.(ia).fg
		s.x(it).pc.ap=aps.(ia).ap
		s.x(it).pc.ae=aps.(ia).ae
             endfor

;
;surface reflectance parameter
;


	aps={   liquid:{fg:1.,ap:1.,ae:.000001},$
		ice:{fg:1.,ap:1.,ae:.000001},$
		bbcerrado:{fg:0.8,ap:0.8,ae:0.1},$
		contclean:{fg:0.8,ap:0.8,ae:0.1},$
		desert:{fg:0.8,ap:0.8,ae:0.1},$
		marclean:{fg:0.8,ap:0.8,ae:0.1},$
		volcox:{fg:0.8,ap:0.8,ae:0.1}}
	tn=tag_names(aps)
	for it=0,nt-1 do begin
		ia=(where(tn eq types(it),na))(0)
		if na eq 0 then message,'Type not found: '+types(it)
		s.x(it).srf.fg=aps.(ia).fg
		s.x(it).srf.ap=aps.(ia).ap
		s.x(it).srf.ae=aps.(ia).ae
             endfor



	return,s
end
