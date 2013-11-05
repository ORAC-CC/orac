;===============================================================================
;+
; PS_CLOSE
;
; This close a PS file and returns plotting to X.
;
; PARAMETERS
;
; KEYWORDS
;	QUIET	Quiet operation
;	PDF	Convert to PDF after write
;	PNG	Convert to PNG after write
;	FILE	Return the name of the file just closed
;	HQPDF	Convert to PDF in way which preserves quality of any images
;	NDPS	Do not delete PS after conversion to PDF or PNG
;
; R.S. 24/11/95
; $Id: ps_close.pro 808 2011-04-08 08:52:35Z rsiddans $
;-
;===============================================================================
pro ps_close,quiet=quiet,pdf=pdf,png=png,file=ofi,hqpdf=hqpdf,ndps=ndps
	if !d.name ne 'PS' then return
	f=fstat(!d.unit)
	device,/close
	set_plot,'x'
	!p.font=-1
	ofi=f.name
; Avoid long path problems by working in target dir
	odir=file_dirname(ofi,/mark_dir) ; output dir
	if odir eq './' then chdir='' else chdir='cd '+odir+' ; '
	ofi=file_basename(ofi) ; file without path
	lfile=ofi ; input filename, without path

	if keyword_set(hqpdf) or keyword_set(pdf) then begin
		ofi=new_name(ofi)+'.pdf'
		if keyword_set(hqpdf) then cmd='ps2pdf -dMaxSubsetPct=100 -dCompatibilityLevel=1.3 -dSubsetFonts=true -dEmbedAllFonts=true -dAutoFilterColorImages=false -dAutoFilterGrayImages=false -dColorImageFilter=/FlateEncode -dGrayImageFilter=/FlateEncode -dMonoImageFilter=/FlateEncode' $
		else cmd='ps2pdf'
		cmd2=chdir+cmd+' '+lfile+' '+ofi
		if not keyword_set(ndps) then cmd2=cmd2+' ; \rm '+lfile
		spawn,cmd2
	endif else if keyword_set(png) then begin
		ofi=new_name(ofi)+'.png'
		cmd2=chdir+'ps2png '+lfile+' '+ofi
		if not keyword_set(ndps) then cmd2=cmd2+' ; \rm '+lfile
		spawn,cmd2
	endif
	if not keyword_set(quite) then message,'Closed: '+odir+ofi,/info
end
