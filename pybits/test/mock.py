from ptghci import app
from prompt_toolkit import print_formatted_text, ANSI
import io
import queue
import re
import threading
from prompt_toolkit.document import Document

def lines_to_doc(lines):
    return Document('\n'.join(lines))

class MockDoc:
    def __init__(self, lines, row=None, col=None):
        self.lines = lines
        if row is not None:
            self.cursor_position_row = row
        else:
            self.cursor_position_row = len(lines)-1

        if col is not None:
            self.cursor_position_col = col
        else:
            self.cursor_position_col = len(lines[-1])

    @property
    def current_line(self):
        return self.lines[self.cursor_position_row]

    @property
    def current_line_before_cursor(self):
        return self.current_line[:self.cursor_position_col]


class MockBuf:
    def __init__(self, lines):
        self.document = MockDoc(lines)

def pretty_to_string(pretty_text):
    buffer = io.BytesIO()
    buffer.encoding = 'utf8'
    buffer.fileno = lambda: 0

    print_formatted_text(pretty_text, file=buffer)
    return strip_ansi(buffer.getvalue().decode())


ansi_escape = re.compile(r'(\x9B|\x1B\[)[0-?]*[ -/]*[@-~]')
def strip_ansi(line):
    return ansi_escape.sub('', line)

class MockHistory:
    def __init__(self):
        self.hist_lines = []

    def add_line(self, line):
        self.hist_lines.append(line)

    def get_strings(self):
        return self.hist_lines


class PromptDriver(threading.Thread):
    def __init__(self):
        self.input_queue = queue.Queue()
        self.output_queue = queue.Queue()
        self.output_buf = []
        super().__init__()
        self.daemon = True

    def next_input(self):
        return self.input_queue.get()

    def send_line(self, line):
        self.input_queue.put(line)

    def next_output(self, timeout=None):
        val = self.output_queue.get(timeout=timeout)
        self.output_buf.append(val)
        return val

    def ptprint(self, line, file=None):
        clean = pretty_to_string(line).strip()
        self.output_queue.put(clean)

    def run(self):
        outer = self
        class MockPrompt:
            def __init__(self, *args, **kwargs):
                self.args = args
                self.kwargs = kwargs
                self.history = MockHistory()

            def prompt(self):
                v = outer.next_input()
                self.history.add_line(v)
                return v
        self.app = app.App(MockPrompt, self.ptprint, oop_engine=True)
        self.app.run()

    def expect(self, regex, timeout=None):
        while True:
            for (i, line) in enumerate(self.output_buf):
                m = re.search(regex, line)
                if m:
                    before = self.output_buf[:i]+[line[:m.start()]]
                    after = [line[m.end()+1:]]+self.output_buf[i+1:]
                    self.output_buf = list(after)
                    return m.group(0), before, after
            try:
                self.next_output(timeout)
            except queue.Empty:
                print("Timed out waiting for %s"%regex)
                print("Output buffer is:")
                for line in self.output_buf:
                    print(line)
                raise

    def __enter__(self):
        self.start()
        # Wait for GHCi to be ready
        self.expect('ghci', 30)
        return self

    def __exit__(self, type, value, traceback):
        self.app.finish()


# Example use of PromptDriver
if __name__ == '__main__':
    with PromptDriver() as driver:
        driver.send_line('1+1')
        driver.send_line('2+3')
        driver.send_line(r'%past')
        print(driver.expect(r'1\+1', 1))
        print(driver.expect(r'2\+3', 1))

