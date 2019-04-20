
# Set vi keybindings
import sys
from prompt_toolkit.key_binding.vi_state import InputMode, ViState
from prompt_toolkit.application.current import get_app


def setup_vim_cursor():
    def get_input_mode(self):
        return self._input_mode 

    def set_input_mode(self, mode):
        shape = {InputMode.NAVIGATION: 2, InputMode.REPLACE: 3}.get(mode, 6)
        out = getattr(sys.stdout, 'buffer', sys.stdout)
        out.write(u'\x1b[{} q'.format(shape).encode('ascii'))
        sys.stdout.flush()
        self._input_mode = mode

    ViState._input_mode = InputMode.INSERT
    ViState.input_mode = property(get_input_mode, set_input_mode)

    # Doesn't seem to work
    # get_app().ttimeoutlen = 0.01
    # get_app().timeoutlen = 0.1
