;+
; NAME:
;   PRINT_COLOUR
;
; PURPOSE:
;   Print to terminal using ECHO in order to have coloured text.
;
; CATEGORY:
;   Visual interface
;
; CALLING SEQUENCE:
;   PRINT_COLOUR, string [, /bold|underline|background|blink] [, /nonewline]
;              [, /black|red|green|yellow|blue|magenta|cyan]
;
; INPUTS:
;   string = Text to be printed to the screen.
;
; OPTIONAL INPUTS:
;   None
;
; KEYWORD PARAMETERS:
;   BOLD       = Print bold text.
;   UNDERLINE  = Underline the text.
;   BACKGROUND = Rather than colour the text, colour its background.
;   BLINK      = The text should blink this colour.
;   NONEWLINE  = Do not include a newline after printing this text.
;   BLACK|RED|GREEN|YELLOW|BLUE|MAGENTA|CYAN = Selection of colour. Default is
;                white.
;
; OUTPUTS:
;   Printout to screen.
;
; OPTIONAL OUTPUTS:
;   None
;
; RESTRICTIONS:
;   Only tested on CSH and BASH.
;
; MODIFICATION HISTORY:
;   Written by ACPovey (povey@atm.ox.ac.uk)
;   20 Aug 2015 - AP: Original version
;-
PRO PRINT_COLOUR, out, bold=bold, underline=underline, background=background, blink=blink, black=black, red=red, green=green, yellow=yellow, blue=blue, magenta=magenta, cyan=cyan, nonewline=nonewline
   if KEYWORD_SET(bold) then $
      style='1' $
   else if KEYWORD_SET(underline) then $
      style='4' $
   else if KEYWORD_SET(background) then $
      style='3' $
   else if KEYWORD_SET(blink) then $
      style='5' $
   else $
      style='0'

   if KEYWORD_SET(black) then $
      colour='30' $
   else if KEYWORD_SET(red) then $
      colour='31' $
   else if KEYWORD_SET(green) then $
      colour='32' $
   else if KEYWORD_SET(yellow) then $
      colour='33' $
   else if KEYWORD_SET(blue) then $
      colour='34' $
   else if KEYWORD_SET(magenta) then $
      colour='35' $
   else if KEYWORD_SET(cyan) then $
      colour='36' $
   else $
      colour='37'

   if KEYWORD_SET(nonewline) then $
      newline='-n ' $
   else $
      newline=''

   SPAWN, 'echo '+newline+'"\033['+style+';'+colour+'m'+out+'\033[0m"'

END
