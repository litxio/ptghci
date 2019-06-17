import re
import os
from multiprocessing.pool import ThreadPool
from .lexer import PtgHaskellLexer
from pygments.styles import get_style_by_name
from prompt_toolkit.application import get_app
from prompt_toolkit import print_formatted_text
from prompt_toolkit.filters import is_searching
from prompt_toolkit.lexers import PygmentsLexer
from prompt_toolkit.styles.pygments import style_from_pygments_cls
from prompt_toolkit.styles import Style, merge_styles
from prompt_toolkit.key_binding import KeyBindings
from prompt_toolkit.formatted_text import ANSI

from . import indent, completer, vimcursor
from .highlight import hl


def default_continuation(prompt_width, line_number, wrap_count):
    if prompt_width <= 3:
        return '.'*prompt_width
    else:
        return '...' + (' '*(prompt_width-3))


class Session():
    def __init__(self, config, engine, PtPromptSession, history,
                 pt_print=print_formatted_text):
        self.config = config
        self.engine = engine
        self.pt_print = pt_print
        bindings = self.setup_key_bindings()
        lexer = PygmentsLexer(PtgHaskellLexer)
        self.initial_lines = len(list(history.load_history_strings()))
        pg_style = style_from_pygments_cls(get_style_by_name(config.style))
        bottom_toolbar_style = Style.from_dict({
            'bottom-toolbar':      config.type_bar_style,
            'bottom-toolbar.text': config.type_bar_text_style
        })
        style = merge_styles([pg_style, bottom_toolbar_style])

        self._psession = PtPromptSession(
            ":", lexer=lexer,
            vi_mode=config.vi_mode, multiline=True,
            # enable_open_in_editor=True, # Handle this manually
            key_bindings=bindings,
            history=history,
            tempfile_suffix=".hs",
            style=style,
            complete_while_typing=False,
            completer=completer.make_completer(engine, config),
            enable_suspend=True,
            prompt_continuation=default_continuation,
            bottom_toolbar=self.bottom_toolbar if config.type_bar_enabled
                           else None)
        self._psession.message = self.in_prompt_message

        if config.vi_mode:
            vimcursor.setup_vim_cursor()

        self._cur_lineno = 1#len(self._psession.history.get_strings())+1

        self.threadpool = ThreadPool(processes=1)
        self.bottom_text = None
        self.last_cursor_word = None

    def setup_key_bindings(self):
        bindings = KeyBindings()

        @bindings.add('escape', 'c-m', filter=~is_searching)  # Meta-enter
        def kp(event):
            buf = event.app.current_buffer
            buf.newline()
            # if not re.match(r"^\s*$", buf.document.text):
            #     buf.validate_and_handle()

        @bindings.add('c-i')  # Tab
        def _(event):
            """
            If previous char is alphanumeric or ., trigger completion;
            otherwise insert spaces
            """
            buf = event.app.current_buffer
            char = buf.document.char_before_cursor
            if char.isalnum() or char == '.':
                if buf.complete_state:
                    buf.complete_next()
                else:
                    buf.start_completion(select_first=True)
            else:
                buf.insert_text('  ', fire_event=False)

        @bindings.add('f2')
        def _(event):
            event.app.current_buffer.open_in_editor(validate_and_handle=True)

        indent.register_indent_trigger_bindings(bindings)
        return bindings

    def in_prompt_message(self):
        return ANSI(self.config.input_prompt.format(
            lineno=self.get_cur_lineno()))

    def out_prompt_message(self):
        return ANSI(self.config.out_prompt.format(lineno=self.get_cur_lineno()))

    def prompt(self):
        return self._psession.prompt()

    def advance_lineno(self):
        self._cur_lineno \
            = len(self.history.get_strings()) - self.initial_lines + 1

    def get_cur_lineno(self):
        return self._cur_lineno

    @property
    def history(self):
        return self._psession.history

    @property
    def style(self):
        return self._psession.style

    @style.setter
    def style(self, newstyle):
        self._psession.style = newstyle

    @property
    def buffer(self):
        return self._psession.default_buffer

    def prompt_no_to_hist_index(self, lineno: int):
        session_start = (len(self.history.get_strings())
                         - self.get_cur_lineno())
        return int(lineno) + session_start

    def format_hist_idx(self, hist_idx: int):
        session_start = (len(self.history.get_strings())
                         - self.get_cur_lineno())
        if hist_idx > session_start:
            return str(hist_idx - session_start)
        else:
            return "h%d" % hist_idx

    def bottom_toolbar(self):
        # This needs to complete quickly, so we run the engine asynchronously
        word = self.buffer.document.get_word_under_cursor(WORD=True)
        if word and word != self.last_cursor_word:
            self.threadpool.apply(self.bottom_toolbar_async)
            self.last_cursor_word = word
        elif not word:
            self.bottom_text = None

        return self.bottom_text

    def bottom_toolbar_async(self):
        rv = None
        try:
            word = self.buffer.document.get_word_under_cursor(WORD=True)
            if word:
                resp = self.engine.get_type(word, False)
                if resp.success:
                    rv = ANSI(hl(resp.content.strip(), self.config))
        except NameError:
            pass

        self.bottom_text = rv
        get_app().invalidate()
