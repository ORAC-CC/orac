;==========================================================================
;+
;	function RSG_SCRATCH_DIR
;
;	Description
; Group local scratch disk for system
; This may change for use on other processing clusters (eg. SCARF or CEMS)
; Note: This is not the workdir which should be this directroy plus an
; optional sub directory added by the job manager to define task
; specific unique sub-directories.
; 
;	Use
; tmp_dir=rsg_scratch_dir()
;
;	Parameters
; 
;	Keywords
;
;	Date
;	B.Latter : 02 August 2012
; $Id: rsg_scratch_dir.pro 1849 2013-03-19 15:42:09Z blatter $
;-
;==========================================================================
function rsg_scratch_dir
chk=getenv('RSG_SCRATCH_DIR') ; check for environment variable

; To work on other systems, could check for host names etc to identify
; different known configurations. On RSG cluster, the local disk is
; always /disks/scratch/$USER/
; Need to return environment variable, not decode as may pass scripts
; between systems, so only resolve at final rutime
if chk ne '' then begin
	dir='${RSG_SCRATCH_DIR}'
; Ensure dir ends with a '/' if not blank
	if strmid(chk,strlen(chk)-1) ne '/' then dir=dir+'/'
endif else begin
	host=getenv('HOST')
; Base on hostname, define which system running on e.g.:
;  RSG hosts: apps*,rsgXX,paris, aberdeen, birmingham, didcot, etc.
;  SCARF hosts: start scarfXXX?
;  CEMS hosts: start lotusXXX?
	hst_chk=strmid(host,0,3) ; simple to catch rsgXX
	case hst_chk of
		'rsg':dir='/disks/scratch/${USER}/' ; rsg system
		'app':dir='/disks/scratch/${USER}/' ; rsg system
; CEMS-Lotus (same on all CEMS and JASMIN)
		'lot':dir='/group_workspaces/cems/rsgnceo/scratch/${USER}/'
; SCARF
		'sca':dir='/work/scratch/${USER}/'
; Default to RSG system settings
		else: begin
			dir='/disks/scratch/${USER}/' ; rsg system
		end
	endcase 
endelse

return,dir
end
