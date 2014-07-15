PRO PLOT_POSITION, plot_set, pos, bpos
   ;; if at end of page, start new sheet
   if ~(plot_set.gridi OR plot_set.gridj) then begin
      DEVICE,/close
      ++plot_set.sheet
      filen=plot_set.tag+'.N'+STRING(plot_set.sheet,format='(i02)')+'.eps'
      DEVICE,/encapsulated,font_s=plot_set.font_s,filen=filen, $
             xsize=plot_set.xs,ysize=plot_set.ys
      XYOUTS,/normal, 0.5*plot_set.cs/plot_set.xs, $
             1.0-1.5*plot_set.cs/plot_set.ys, '!16'+plot_set.label+'!X'
   endif

   ;; determine plot position
   pos=[plot_set.x0[plot_set.gridi], plot_set.y0[plot_set.gridj], $
        plot_set.x1[plot_set.gridi], plot_set.y1[plot_set.gridj]]
   bpos=[pos[0]*!d.x_size-2.2*!d.y_ch_size,pos[1]*!d.y_size, $
         pos[0]*!d.x_size-1.7*!d.y_ch_size,pos[3]*!d.y_size]

   ;; iterate plot position
   if plot_set.gridi ge (plot_set.nx-1) then begin
      plot_set.gridi = 0
      plot_set.gridj = plot_set.gridj ge (plot_set.ny-1) ? 0 : plot_set.gridj+1
   endif else ++plot_set.gridi

END
