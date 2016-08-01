# Python routine to add ASCII colour flags to text printed to a terminal. Values
# drawn from the tables at http://misc.flogisoft.com/bash/tip_colors_and_formatting
# 22 Jun 2016, ACP: Initial version

import re
import sys

# ---- DEFINE DICTIONARY ----
terminal_format = {}

# Text formatting
terminal_format['reset']          = '0'
terminal_format['bold']           = '1'
terminal_format['dim']            = '2'
terminal_format['underlined']     = '4'
terminal_format['blink']          = '5'
terminal_format['reverse']        = '7'
terminal_format['hidden']         = '8'
terminal_format['bold off']       = '21'
terminal_format['dim off']        = '22'
terminal_format['underlined off'] = '24'
terminal_format['blink off']      = '25'
terminal_format['reverse off']    = '27'
terminal_format['hidden off']     = '28'


# Font colour
terminal_format['black']         = '30'
terminal_format['red']           = '31'
terminal_format['green']         = '32'
terminal_format['yellow']        = '33'
terminal_format['blue']          = '34'
terminal_format['magenta']       = '35'
terminal_format['cyan']          = '36'
terminal_format['light gray']    = '37'
terminal_format['default']       = '39'
terminal_format['dark gray']     = '90'
terminal_format['light red']     = '91'
terminal_format['light green']   = '92'
terminal_format['light yellow']  = '93'
terminal_format['light blue']    = '94'
terminal_format['light magenta'] = '95'
terminal_format['light cyan']    = '96'
terminal_format['white']         = '97'

# Background colour
terminal_format['back black']         = '40'
terminal_format['back red']           = '41'
terminal_format['back green']         = '42'
terminal_format['back yellow']        = '43'
terminal_format['back blue']          = '44'
terminal_format['back magenta']       = '45'
terminal_format['back cyan']          = '46'
terminal_format['back light gray']    = '47'
terminal_format['back default']       = '49'
terminal_format['back dark gray']     = '100'
terminal_format['back light red']     = '101'
terminal_format['back light green']   = '102'
terminal_format['back light yellow']  = '103'
terminal_format['back light blue']    = '104'
terminal_format['back light magenta'] = '105'
terminal_format['back light cyan']    = '106'
terminal_format['back white']         = '107'

# Font and background colour for 256 colour terminals
for i in range(0,257):
   terminal_format['%d'.format(i)]      = '38;5;%d'.format(i)
   terminal_format['back %d'.format(i)] = '48;5;%d'.format(i)

# Called by re.sub to replace format code with ASCII code
def cprint_sub(match):
    # Extract list of requested formats
    formats = match.group(1).split(',')
    # Form a list of the corresponding codes for those formats
    codes = [terminal_format[form.strip()] for form in formats]
    # Merge those into a string
    return '\033[' +';'.join(codes)  + 'm'

def cformat(text_in,     # String to be printed
            start=None,  # Format specifiers to be appended to front of string
            reset=True): # If True, resets formatting after printing.
    """Add ASCII colour flags to a string"""

    # Ignore colour on virtual terminals
    if not sys.stdout.isatty():
       return text_in

    # Lazy way of formatting entire line
    if start:
        text = '\C{' + start + '}' + text_in
    else:
        text = text_in

    # Reset formatting after printing
    if reset:
        text += '\033[0m'

    return re.sub(r'\\C\{(.+?)\}', cprint_sub, text)

def cprint(text,        # String to be printed
           start=None,  # Format specifiers to be appended to front of string
           reset=True): # If True, resets formatting after printing.
    """Wrapper print statement for cformat"""
    print cformat(text, start, reset)

