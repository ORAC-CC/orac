PRO COLOUR_WHTEDIFF,N_Colours,Back=Back,grey=grey
; set up colour table
; Use largest possible palate otherwise force to N_colours (including
; black and white)
; note that the colour table is slightly different between a screen
; and paper device

;     N         X      PS
;     0       black   white
;     1         colours
;     2         colours
; .    ...
; n_colours-2   colours
; n_colours-1 white   black

; Date        Author Comment
; ISAMS epoch   AL   Created
; 16 Apr 2002  RGG   commented
; 23 Aug 2002  RGG   Limit total number of colours to 256 if n_colours
;                    not specified
; 25 Mar 2004  SMD   Modified colour_ps to do blue to white to red.
; 18 Dec 2006  GET   Added /GREY keyword - makes the mid colour an 90%
;                    grey colour, rather than white. Means that data
;                    points which lie mid way along the colour bar
;                    (eg. zero values) are distinguishable from
;                    missing data in a plot with a white background.
  if keyword_set(grey) then begin
      r=2.55*[  0, 90,100]
      g=2.55*[  0, 90,  0]
      b=2.55*[100, 90,  0]
  endif else begin
      r=2.55*[  0,100,100]
      g=2.55*[  0,100,  0]
      b=2.55*[100,100,  0]
  endelse

  If (N_Elements(N_Colours) Eq 0) Then N_Colours = !d.n_colors < 256
; Smooth Colour Range over Number of Available colours
  If ( (!d.name eq 'X')or (!d.name eq 'WIN') or (!d.name eq 'Z') or (!d.name eq 'PS') ) then begin
    If (!d.name Eq 'PS') Then DEVICE,BITS=8,/COLOR
    r = byte(interpol(r,findgen(3)/2., findgen(N_Colours - 2)/(N_Colours - 3)))
    g = byte(interpol(g,findgen(3)/2., findgen(N_Colours - 2)/(N_Colours - 3)))
    b = byte(interpol(b,findgen(3)/2., findgen(N_Colours - 2)/(N_Colours - 3)))
  ENDIf
  If (!d.name eq 'LJ') Then DEVICE,DEPTH=4,/FLOYD

; First colour black
; Last colour white
  If (N_Elements(Back) Eq 0) Then begin
    r = [0, r, 255]
    g = [0, g, 255]
    b = [0, b, 255]
  EndIf Else Begin
    r = [0, reverse(r), 255]
    g = [0, reverse(g), 255]
    b = [0, reverse(b), 255]
  EndElse
  TVLCT, r,g,b

; Set up !p.color to point to white
  If (!d.name Eq 'PS') then !p.color= 0 else !p.color= N_colours -1


RETURN
END
