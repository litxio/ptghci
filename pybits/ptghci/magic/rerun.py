import re

from ..response import Response

def to_hist_index(idx_str, session):
    if idx_str.startswith('p'):
        return int(idx_str[1:])
    else:
        return session.prompt_no_to_hist_index(int(idx_str))


def handle_rerun(command, args, session, config, dispatcher):
    syntax_err = False
    idxs = []
    hist_strings = session.history.get_strings()
    session_start = len(hist_strings) - session.get_cur_lineno()
    if not args:
        # Rerun previous line
        idxs.append(len(hist_strings))  # Note we start from 1
    else:
        args = args.replace(' ', '')
        ranges = args.split(',')
        for r in ranges:
            m = re.match(r'(p?\d+)-(p?\d+)', r)
            if m:
                start = to_hist_index(m.group(1), session)
                end = to_hist_index(m.group(2), session)
                idxs.extend(range(start, end+1))
            elif re.match(r'p?\d+', r):
                idxs.append(to_hist_index(r, session))
            else:
                syntax_err = True

    if syntax_err:
        resp = Response.from_error_message(
                ('Syntax error: %rerun expects an integer, '
                 "range, or comma-separated list of integers (prefixed by 'p' "
                 "for history from past sessions) "
                 'and ranges. Example: %rerun 3,4-5,p8,p23-p24'))

    elif any([i >= len(hist_strings) for i in idxs]):
        resp = Response.from_error_message(
                'Syntax error: %s outside range of past history' %
                session.format_hist_idx(max(idxs)))

    else:
        session.pt_print('=== Executing: ===')
        for idx in idxs:
            session.pt_print(hist_strings[idx-1])
        session.pt_print('=== Output: ===')
        success = True
        for idx in idxs:
            msg = dispatcher.dispatch(hist_strings[idx-1])
            success = msg.success and success

        resp = Response(Response.Stream)

    return resp
