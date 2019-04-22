from prompt_toolkit.completion import Completer, Completion, merge_completers,\
    WordCompleter
import sys
import re
from .magic import magic

keywords = ["as", "case, of", "class", "data", "data family", "data instance", "default", "deriving", "deriving instance", "do", "forall", "foreign", "hiding", "if, then, else", "import", "infix, infixl, infixr", "instance", "let, in", "mdo", "module", "newtype", "proc", "qualified", "rec", "type", "type family", "type instance", "where"]

meta_re = re.compile(r'(\d+) (\d+) "(.*)"$')
startpos_re = re.compile(r'.*?(\S+)$')

def make_completer(engine, config):
    return merge_completers([
        WordCompleter(magic.command_list(config), WORD=True),
        GhciCompleter(engine),
        WordCompleter(keywords)])

class GhciCompleter(Completer):
    def __init__(self, engine):
        self.engine = engine

    def get_completions(self, document, complete_event):
        resp = self.engine.get_completions(
            document.current_line_before_cursor)
        if resp is None:
            return
        else:
            start_chars, candidates = resp

            # Replacement starts after the front_chars
            m = startpos_re.match(document.current_line_before_cursor)
            if m:
                for c in candidates:
                    start = len(start_chars) - len(document.current_line_before_cursor)
                    yield Completion(c, start_position=start)#-len(m.group(1)))

