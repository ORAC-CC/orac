;==========================================================================
;+
;	function RSG_WORKDIR
;
;	Description
; 
;	Use
; Return local work directory for user using rsg_scratch_dir.pro which
; in turn checks for {RSG_SCRATCH_DIR} e.g. /disks/scratch/{USER}/
; This code then checks for environment variable RSG_SCRATCH_SUB
; (e.g. set by vm_control for identifying unique working space) and
; appends this to above path (eg parent process id)
; Note: Base local mount id defined by rsg_scratch_dir.pro which
; should be called by this code. The rsg_scratch_dir.pro should be
; modified to run code on other systems, not this function.
;
;	Parameters
;  I  SUB Sub-dir below {RSG_SCRATCH_DIR}/{RSG_SCRATCH_SUB}/ to add
; 
;	Keywords
;
;	Date
;	B.Latter : 21 September 2007
; $Id: rsg_workdir.pro 1676 2012-08-06 13:20:42Z blatter $
;-
;==========================================================================
function rsg_workdir,sub
; Check for environment setting of local work disk. If no, use default
wrkdir=rsg_scratch_dir()
; Check for environment variable for sub-dir (e.g. from job manager)
env=getenv('RSG_SCRATCH_SUB')
if env ne '' then wrkdir=wrkdir+env+'/'
; Add user specified subdir
if n_elements(sub) gt 0 then wrkdir=wrkdir+sub+'/'
; Create dir if not already present
if ~file_test(wrkdir,/dir) then file_mkdir,wrkdir
return,wrkdir
end
