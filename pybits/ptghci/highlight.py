from pygments import highlight
from pygments.lexers.haskell import HaskellLexer
from pygments.formatters import Terminal256Formatter

from pygments.styles import get_style_by_name


def hl(s, config, style_name=None):
    if style_name:
        style = get_style_by_name(style_name)
    else:
        style = get_style_by_name(config.style)
    # Don't try to highlight if the string already has escape sequences
    if '\033[' in s:
        return s
    else:
        # Sometimes highlight adds an extra newline, so we remove it
        return highlight(s, HaskellLexer(),
                         Terminal256Formatter(style=style)).strip()

