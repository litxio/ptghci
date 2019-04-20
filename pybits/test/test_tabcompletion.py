from ptghci.indent import get_haskell_indent, shiftwidth,\
    indent_after_bare_where, indent_case
from ptghci.completer import *
from prompt_toolkit.document import Document
from mock import *
import pytest

def test_complete(completer):
    """ Quick check of the three completion types (magic, keyword, ghci)"""
    assert list(completer.get_completions(Document("Rationa"), None))[0].text\
        == 'Rational'
    assert list(completer.get_completions(Document("%opend"), None))[0].text\
        == '%opendoc'
    c3 = list(completer.get_completions(Document("wher"), None))[0]
    assert c3.text == 'where' and c3.start_position == -4

    # Check that the completion isn't thrown off by a non-word character
    # before the identifier
    c4 = list(completer.get_completions(Document("(Intege"), None))[0]
    assert c4.text == 'Integer' and c4.start_position == -6

    # Check that the completion isn't thrown off by a string literal before the
    # identifier
    c5 = list(completer.get_completions(Document('foo "hello world" Intege'), None))[0]
    assert c5.text == 'Integer' and c4.start_position == -6
