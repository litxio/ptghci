

from ptghci.indent import get_haskell_indent, shiftwidth,\
    indent_after_bare_where, indent_case
from prompt_toolkit.document import Document
from mock import *

def test_past():
    with PromptDriver() as driver:
        driver.send_line('1+1')
        driver.send_line('2+3')
        driver.send_line(r'%past')
        driver.expect(r'1\+1', 1)
        driver.expect(r'2\+3', 1)


def test_rerun():
    with PromptDriver() as driver:
        driver.send_line('7+14')
        driver.expect('^21$', 1)
        driver.send_line('%rerun 1')
        driver.expect(r'=== Executing: ===', 1)
        driver.expect(r'^7\+14$', 1)
        driver.expect(r'=== Output: ===', 1)
        driver.expect(r'^21$', 1)


def test_hoogle():
    with PromptDriver() as driver:
        driver.send_line('%hoogle List')
        driver.expect(r'module Data.List', 1)


def test_style():
    with PromptDriver() as driver:
        driver.send_line('%style')
        driver.expect(r'native:', 1)
        driver.send_line('%style native')
        driver.expect(r'Style changed to native', 1)
