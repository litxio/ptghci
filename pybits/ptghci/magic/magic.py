import math
import re
import subprocess
import shlex
import webbrowser
from . import run
from ..highlight import hl
from pygtrie import CharTrie
from ..response import Response

from .past import handle_past
from .rerun import handle_rerun
from .style import handle_style

GHCI_COMMANDS = [":abandon", ":add", ":back", ":break", ":browse", ":cd",
                 ":cmd", ":continue", ":ctags", ":def", ":delete",  # ":edit",
                 ":doc", ":etags", ":force", ":forward", ":help", ":history",
                 ":info", ":kind", ":load", ":main", ":module", ":print",
                 ":quit", ":reload", ":run", ":set", ":show", ":sprint",
                 ":step", ":trace", ":type", ":undef", ":unset", ":!"]
GHCI_COMMANDS_TRIE = CharTrie({c: c for c in GHCI_COMMANDS})


def command_list(config):
    return list(set([config.magic_prefix + c[1:] for c in GHCI_COMMANDS]
                    + [config.magic_prefix + k for k in MAGIC_COMMANDS]))


def handle_magic(entry: str, session, config, dispatcher) -> Response:
    parts = entry.split(' ', 1)
    command = parts[0][1:]
    if len(parts) > 1:
        args = parts[1]
    else:
        args = ''

    # print("looking for magic %s; candidates are %s" % (repr(command), MAGIC_COMMANDS))
    if command in MAGIC_COMMANDS or MAGIC_COMMANDS.has_subtrie(command):
        matches = list(MAGIC_COMMANDS[command:])
        if len(matches) == 1:
            handler = matches[0]
            resp = handler(command, args, session, config, dispatcher)
        else:
            resp = Response.from_error_message(
                    'Ambigous magic command prefix: %s' % command)

    elif (':'+command) in GHCI_COMMANDS_TRIE \
            or GHCI_COMMANDS_TRIE.has_subtrie(':'+command):
        resp = handle_ghci_command(command, args, session, config, dispatcher)
    else:
        resp = Response.from_error_message('Unknown magic: %s' % command)
    return resp



def handle_hoogle(command, args, session, config, dispatcher):
    try:
        hoogle_output = subprocess.check_output(['hoogle']+shlex.split(args))
        resp = Response.from_value(hl(hoogle_output.decode(), config))
    except subprocess.CalledProcessError as det:
        resp = Response.from_error_message("Hoogle call returned error %d: %s"
                % (det.returncode, det.output))
    return resp




def handle_opendoc(command, args, session, config, dispatcher):
    resp = dispatcher.engine.find_doc(args)
    if resp.success:
        webbrowser.open(resp.content)
    return resp


def handle_opensource(command, args, session, config, dispatcher):
    resp = dispatcher.engine.find_source(args)
    if resp.success:
        webbrowser.open(resp.content)
    return resp



def handle_run(command, args, session, config, dispatcher):
    return run.run(session, dispatcher, command, args)


def handle_DBG(command, args, session, config, dispatcher):
    try:
        res = str(eval(args, {'session': session}))
    except BaseException as det:
        res = str(det)
    return Response.from_value(res)


def handle_info(command, args, session, config, dispatcher):
    resp = dispatcher.dispatch(":"+command+' '+(args or ''))

    # TODO Don't try to highlight if we get an error message
    if resp.success:
        resp.content = hl(resp.content, config)
    return resp


def handle_ghci_command(command, args, session, config, dispatcher):
    return dispatcher.dispatch(":"+command+' '+(args or ''))


MAGIC_COMMANDS = CharTrie({
    "past": handle_past,
    "hoogle": handle_hoogle,
    "rerun": handle_rerun,
    "opendoc": handle_opendoc,
    "opensource": handle_opensource,
    "style": handle_style,
    "run": handle_run,
    "DBG": handle_DBG,
    "info": handle_info
})
