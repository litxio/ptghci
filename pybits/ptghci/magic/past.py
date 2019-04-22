
import math
import argparse
import shlex

from ptghci.response import Response
from ptghci.highlight import hl

class ArgParseException(Exception):
    def __init__(self, message):
        self.message = message

class AP(argparse.ArgumentParser):
    def __init__(self, *args, **kwargs):
        self.last_msg = ''
        super().__init__(*args, **kwargs)
    def error(self, msg):
        self.last_msg = msg
        raise Exception(msg)

def parse_args(argstr: str, magic_prefix: str):
    parser = AP(prog=magic_prefix+'past')
    parser.add_argument('-n', metavar='N', type=int, 
                        help="Number of lines of history to show"
                             " (will include past sessions)")
    args = shlex.split(argstr)
    try:
        return parser.parse_args(args)
    except SystemExit:
        message = parser.format_usage()+parser.last_msg
        raise ArgParseException(message)

def handle_past(command, args, session, config, dispatcher) -> Response:
    try:
        argvals = parse_args(args, config.magic_prefix)
    except ArgParseException as det:
        return Response.from_error_message(det.message)
    lines = []
    hist_strings = session.history.get_strings()
    session_start = len(hist_strings) - session.get_cur_lineno()
    if argvals.n is None:
        limit = session.get_cur_lineno()
    else:
        limit = argvals.n
    width = 3+math.floor(math.log10(len(hist_strings)))
    for (i, h) in enumerate(hist_strings[-limit:]):
        hist_idx = i + 1 + (len(hist_strings) - limit)
        if hist_idx <= session_start:
            user_idx = "p%d" % hist_idx
        else:
            user_idx = hist_idx - session_start
        h = hl(h, config)
        h_lines = h.split('\n')
        for (j, hline) in enumerate(h_lines):
            if j == 0:
                line = ('{:>%s}: {}' % width).format(user_idx, hline)
            else:
                indent = ' '*(width+2)
                line = ('{}{}').format(indent, hline)

            lines.append(line)
    return Response.from_value('\n'.join(lines))

