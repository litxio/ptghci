import sys
import time
import os
import traceback
import colors
import signal
import threading
from prompt_toolkit.history import FileHistory, ThreadedHistory
from prompt_toolkit import print_formatted_text, ANSI
from prompt_toolkit.shortcuts import PromptSession
from ptghci import settings, engine, dispatch, \
    session, exceptions, response

try:
    from pdb_clone import pdbhandler; pdbhandler.register()
except ImportError:
    pass

def int_handler(sig, frame):
    #print("Python - Got SIGINT")
    signal.default_int_handler(sig, frame)


def print_detect_ansi(val, *more_vals, **kwargs):
    """
    If any val has escape codes, wrap all the vals in ANSI and print with
    print_formatted_text.  Otherwise, print with plain old python print.
    This is much faster than printing everything with print_formatted_text.
    """
    has_escape = '\x1b' in val or any(['\x1b' in more_vals])
    if has_escape:
        print_formatted_text(ANSI(val), *[ANSI(v) for v in more_vals],
                             **kwargs)
    else:
        print(val, *more_vals, **kwargs)


class App:
    def __init__(self, PtPromptSession=PromptSession,
                 history=None,
                 pt_print=print_detect_ansi,
                 oop_engine=False):

        if isinstance(threading.current_thread(), threading._MainThread):
            signal.signal(signal.SIGINT, int_handler)

        self.config = settings.Settings.load()
        if self.config.settings_path:
            print("Using settings file %s" % self.config.settings_path)
        if history is None:
            self.history = ThreadedHistory(FileHistory(
                os.path.expanduser(self.config.history_path)))
        else:
            self.history = history
        self.PtPromptSession = PtPromptSession
        self.pt_print = pt_print
        self.oop_engine = oop_engine

    def run(self):

        try:
            if self.oop_engine:
                self.ngin = engine.Engine.oop_engine(self.config, pt_print=self.pt_print)
            else:
                self.ngin = engine.Engine(self.config, pt_print=self.pt_print)

            self.session = session.Session(self.config, self.ngin,
                                           self.PtPromptSession, self.history,
                                           self.pt_print)

            self.dispatcher = dispatch.Dispatcher(self.session, self.config,
                                                  self.ngin)

            load_messages = self.ngin.get_load_messages()
            self.pt_print(load_messages)
            while True:
                try:
                    # Wait a bit for any information on the stdout socket to be
                    # printed out.  TODO: this is not a good solution.
                    time.sleep(.05)
                    entry = self.session.prompt()
                except KeyboardInterrupt:
                    self.ngin.send_interrupt()
                    self.session.buffer.reset()
                    continue

                if entry.strip() == '':
                    continue

                try:
                    # Dispatch may print to stdout/stderr before returning, so
                    # we need to print the output prompt before calling it.
                    self.pt_print(self.session.out_prompt_message())

                    # Dispatcher: figure out how to handle the user's command,
                    # do it, and return in 'content' any content that should be
                    # printed out.  dispatch may also print content directly to
                    # stdout.
                    try:
                        resp = self.dispatcher.dispatch(entry)

                        if resp.kind == response.Response.Value:
                            self.pt_print(resp.content)
                        elif resp.kind == response.Response.Error:
                            print(colors.color('Error: ', fg='red',
                                               style='bold')
                                  + resp.content)
                    except Exception as det:
                        if isinstance(det, EOFError) \
                                or isinstance(det, exceptions.Exiting):
                            raise
                        print("An error occurred!  Please report this as a "
                              "bug: ", det, file=sys.stderr)
                        traceback.print_exc()

                except KeyboardInterrupt:
                    self.pt_print("\n\033[1;31mInterrupted\033[0m\n")
                finally:
                    self.session.advance_lineno()

        except (EOFError, exceptions.Exiting):
            # Happens when the user presses Ctrl-D at the prompt
            print("Exiting...")
            return #sys.exit(0)
        finally:
            self.finish()

    def finish(self):
        try:
            self.ngin.finish()
        except (NameError, AttributeError):
            pass

if __name__ == '__main__':
    app = App()
    app.run()
