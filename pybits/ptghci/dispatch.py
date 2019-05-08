
"""
Dispatch a line of input from the user to the appropriate handler.
"""
import traceback
from .magic import magic
from .exceptions import Exiting
from .response import Response


class Dispatcher():
    def __init__(self, session, config, engine):
        self.session = session
        self.config = config
        self.engine = engine

    def dispatch(self, entry) -> Response:
        entry = entry.lstrip()
        if entry.startswith(self.config.magic_prefix):
            return magic.handle_magic(entry, self.session, self.config, self)
        elif entry.startswith(':'):
            return self.dispatch_ghci_command(entry)
        elif '\n' not in entry and entry.endswith('?'):
            return self.exec_ghci(':info '+entry[:-1])
        else:
            return self.exec_ghci(entry, stream=True)

    def dispatch_ghci_command(self, entry):
        # Make sure it's not something that will break ptGHCi before executing
        parts = entry.split(' ')
        if any(parts[0].startswith(s) for s in (':set', ':unset')):
            for p in parts[1:]:
                if p in ('+m', '+s', 'prompt', 'prompt-cont',
                         'prompt-function', 'prompt-cont-function'):
                    return Response.from_error_message(
                                    "Not yet supported by ptghci: %s\n" % p)
        elif parts[0].startswith(':tr'):
            return Response.from_error_message(
                ":trace is not yet supported by ptghci")

        # elif parts[0].startswith(':q'):
        #     raise Exiting()
        return self.exec_ghci(entry, stream=False)


    def exec_ghci(self, entry, stream=False) -> Response:
        try:
            if stream:
                return self.engine.exec_stream(entry)
            else:
                return self.engine.execute(entry)

        except (EOFError, Exiting):
            raise
        except Exception as det:
            traceback.print_exc()
            return Response.from_error_message(
                            'Error getting response from engine: %s' % det)
