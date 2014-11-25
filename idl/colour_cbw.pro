PRO COLOUR_cbw,Colours,Greys
; set up colour table to be N colours then M Grey levels 
; Use largest possible palate otherwise force to colours+ greys + 2 (including
; black and white)
; note that the colour table is slightly different between a screen
; and paper device

;     N            X       PS
;     0          black    white
;     1         colours
;     2         colours
;    ...
; colours       colours
; colours+1       black   black   
; colours+2        grey    grey
; colours+greys   white   white
; colours+greys+1 white   black

; Date        Author Comment
; 17 Dec 2003  RGG   NEW, derived from colour_ps
; 25 Nov 2014  ACP   Fixed management of greys.

  r=2.55*[25, 35,  70,   0,   0,   0,   0,  0,   0,  70, 100, 100, 100, 85, 100]
  g=2.55*[40, 0,   0,    0,  55, 100, 100, 70, 100, 100, 100,  70,  55, 25,   0]
  b=2.55*[55, 35,  85, 100,  85, 100,  70,  0,   0,   0,   0,  40,   0 , 0,   0]

  If (N_Elements(Colours) Eq 0) Then Colours = 0
  If (N_Elements(Greys) Eq 0) Then Greys = 0

  If (Colours+Greys+2 Gt !d.n_colors) Then Stop, 'Too many Grey and colour levels for device'
; Smooth Colour Range over Number of Available colours
  If ( (!d.name eq 'X')or (!d.name eq 'WIN') or (!d.name eq 'Z') or (!d.name eq 'PS') ) then begin
    If (!d.name Eq 'PS') Then DEVICE,BITS=8,/COLOR
    If (Colours Gt 1) Then Begin
      rc = byte(interpol(r,findgen(15)/14., findgen(Colours)/(Colours - 1)))
      gc = byte(interpol(g,findgen(15)/14., findgen(Colours)/(Colours - 1)))
      bc = byte(interpol(b,findgen(15)/14., findgen(Colours)/(Colours - 1)))
    Endif
    ; Make black, white, and requested greys
    rg = byte(255 * findgen(Greys+2)/(Greys + 1))
    
  ENDIf
  If (!d.name eq 'LJ') Then DEVICE,DEPTH=4,/FLOYD

  r = [0, rc, rg[1:*]]
  g = [0, gc, rg[1:*]]
  b = [0, bc, rg[1:*]]
  TVLCT, r,g,b

; Set up !p.color to point to white 
  If (!d.name Eq 'PS') then !p.color= 0 else !p.color= Greys+Colours+1

RETURN
END
