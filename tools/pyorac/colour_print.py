"""Routines to add ASCII colour flags to text printed to a terminal. Values
drawn from the tables at http://misc.flogisoft.com/bash/tip_colors_and_formatting
"""

TERMINAL_FORMAT = {
    # Text formatting
    'reset': '0',
    'bold': '1',
    'dim': '2',
    'underlined': '4',
    'blink': '5',
    'reverse': '7',
    'hidden': '8',
    'bold off': '21',
    'dim off': '22',
    'underlined off': '24',
    'blink off': '25',
    'reverse off': '27',
    'hidden off': '28',
    # Font colour
    'black': '30',
    'red': '31',
    'green': '32',
    'yellow': '33',
    'blue': '34',
    'magenta': '35',
    'cyan': '36',
    'light gray': '37',
    'default': '39',
    'dark gray': '90',
    'light red': '91',
    'light green': '92',
    'light yellow': '93',
    'light blue': '94',
    'light magenta': '95',
    'light cyan': '96',
    'white': '97',
    # Background colour
    'back black': '40',
    'back red': '41',
    'back green': '42',
    'back yellow': '43',
    'back blue': '44',
    'back magenta': '45',
    'back cyan': '46',
    'back light gray': '47',
    'back default': '49',
    'back dark gray': '100',
    'back light red': '101',
    'back light green': '102',
    'back light yellow': '103',
    'back light blue': '104',
    'back light magenta': '105',
    'back light cyan': '106',
    'back white': '107',
}
# Font and background colour for 256 colour terminals
for i in range(0, 257):
    TERMINAL_FORMAT['{:d}'.format(i)] = '38;5;{:d}'.format(i)
    TERMINAL_FORMAT['back {:d}'.format(i)] = '48;5;{:d}'.format(i)


def colour_format(text_in, start=None, reset=True):
    """Add ASCII colour flags to a string.

    Args:
    :str text_in: String to be printed.
    :str start: Format specifiers to be appended to the string.
    :bool reset: When True, formatting is reset after printing.
    """
    from re import sub
    from sys import stdout

    def cprint_sub(match):
        """Called by re.sub() to replace a format code with an ASCII code."""
        # Extract list of requested formats
        formats = match.group(1).split(',')
        # Form a list of the corresponding codes for those formats
        codes = [TERMINAL_FORMAT[form.strip()] for form in formats]
        # Merge those into a string
        return '\033[' + ';'.join(codes) + 'm'

    # Ignore colour on virtual terminals
    if not stdout.isatty():
        return text_in

    # Lazy way of formatting entire line
    if start:
        text = r'\C{' + start + '}' + text_in
    else:
        text = text_in

    # Reset formatting after printing
    if reset:
        text += '\033[0m'

    return sub(r'\\C{(.+?)}', cprint_sub, text)


def colour_print(text, start=None, reset=True):
    """Wrapper print statement for cformat.

    Args:
    :str text_in: String to be printed.
    :str start: Format specifiers to be appended to the string.
    :bool reset: When True, formatting is reset after printing.
    """
    print(colour_format(text, start, reset))
