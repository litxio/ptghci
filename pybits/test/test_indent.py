from ptghci.indent import get_haskell_indent, shiftwidth,\
    indent_after_bare_where, indent_case
from prompt_toolkit.document import Document
from mock import *


def test_op_indent():
    assert get_haskell_indent(lines_to_doc(["x = 5 +", ""])) == shiftwidth


def test_let_indent1():
    assert get_haskell_indent(lines_to_doc(["let x = 1 in", ""])) == 4

def test_let_indent2():
    assert get_haskell_indent(lines_to_doc(["let x = 1", "y ="])) == 4

def test_do_indent1():
    assert get_haskell_indent(lines_to_doc(["do", ""])) == shiftwidth

def test_do_indent2():
    # Make sure we indent past the do, not just to the then
    assert get_haskell_indent(lines_to_doc(["if True then do", ""])) == 15

def test_case_indent1():
    assert get_haskell_indent(lines_to_doc(["case x of", ""])) == indent_case

def test_case_indent2():
    assert get_haskell_indent(lines_to_doc(["case x of",
                                       "  1 -> True",
                                       ""])) is None # i.e. continue indent

def test_eq_indent1():
    assert get_haskell_indent(lines_to_doc(["foo x =", ""])) == shiftwidth

def test_do_indent1():
    assert get_haskell_indent(lines_to_doc(["do", ""])) == shiftwidth


def test_if_then_indent():
    assert get_haskell_indent(lines_to_doc(["if x == 1", "then"])) == 3

def test_if_then_else_indent():
    assert get_haskell_indent(lines_to_doc(["if x == 1", "   then 4", "else"])) == 3

def test_bare_where_indent():
    assert get_haskell_indent(lines_to_doc(["where", ""])) == indent_after_bare_where

def test_where_indent_sameline():
    assert get_haskell_indent(lines_to_doc(["where foo = 1",
                                       ""])) == 6

def test_where_indent_type():
    assert get_haskell_indent(lines_to_doc(["where f :: Int",
                                       "-> "])) == 8

def test_where_indent_guard():
    assert get_haskell_indent(lines_to_doc(["where f x",
                                       "| x == 1"])) == 8

def test_where_indent_after():
    assert get_haskell_indent(lines_to_doc(["foo x = y+x",
                                       "  where",
                                       "y = "])) == 4

def test_newtype_deriving_indent():
    assert get_haskell_indent(lines_to_doc(["newtype Foo = Foo",
                                       "deriving "])) == shiftwidth

def test_type_sig_indent1():
    assert get_haskell_indent(lines_to_doc(["foo :: Foo",
                                       "-> "])) == 4
def test_type_sig_indent2():
    assert get_haskell_indent(lines_to_doc(["foo",
                                       ":: "])) == shiftwidth

def test_type_sig_unindent2():
    assert get_haskell_indent(lines_to_doc(["foo",
                                       "  :: Int",
                                       ""])) == 0

def test_type_sig_indent3():
    assert get_haskell_indent(lines_to_doc(["foo :: Foo",
                                       "    -> Bar",
                                       "-> "])) is None

def test_type_sig_indent4():
    assert get_haskell_indent(lines_to_doc(["foo :: Monad m",
                                       "    => "])) == 4

def test_type_sig_indent5():
    assert get_haskell_indent(lines_to_doc(["foo :: Monad m",
                                       "    => Int",
                                       "    -> "])) is None


def test_sumtype_indent1():
    assert get_haskell_indent(lines_to_doc(["data A = A",
                                       "| "])) == 7
