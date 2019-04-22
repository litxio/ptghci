
from pygments.styles import get_style_by_name, ClassNotFound, get_all_styles
from prompt_toolkit.styles.pygments import style_from_pygments_cls
import colors

from ..response import Response
from ..highlight import hl


def handle_style(command, args, session, config, dispatcher):
    argss = args.strip()
    if not argss:
        test_code = 'let s = "Hello world" in toUpper <$> s'
        lines = []
        for name in get_all_styles():
            hltext = hl(test_code, config, style_name=name)
            lines.append('{:40}{}'.format(colors.color(name+':',
                                                       style='bold'),
                                          hltext))
        resp = Response.from_value('\n'.join(lines))
    else:
        try:
            style = style_from_pygments_cls(get_style_by_name(argss))
            session.style = style
            config.style = argss
            resp = Response.from_value("Style changed to "+argss)
        except ClassNotFound:
            resp = Response.from_error_message("Could not find style "+argss)
    return resp
