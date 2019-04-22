import re
import prompt_toolkit
from prompt_toolkit.filters import is_searching, vi_insert_mode, emacs_insert_mode, is_read_only
from prompt_toolkit.filters.utils import is_true
from prompt_toolkit.document import Document

shiftwidth = 2
indent_let = 4
indent_if = 3
indent_after_bare_where = 2
indent_before_where = 2
indent_where = 6
indent_do = 3
indent_case = 2
indent_guard = 2
indent_in = 1

def indent(line):
    """ Returns the number of spaces line is indented"""
    m = re.match(r'^\s*\S', line)
    if m:
        return len(m.group(0)[:-1])
    else:
        return 0

def indent_current_line(buf, count):
    cur_col = buf.document.cursor_position_col
    old_txt = buf.document.current_line[:cur_col].lstrip()
    buf.delete_before_cursor(cur_col)
    buf.insert_text((' '*count)+old_txt)

def register_indent_trigger_bindings(keybindings):

    insert_mode = vi_insert_mode | emacs_insert_mode

    # Check for 'indent keys', i.e. tokens that trigger indent check
    @keybindings.add(' ')
    def _(event):
        buf = event.app.current_buffer
        if check_indent_keys(buf.document):
            indent_amt = get_haskell_indent(buf.document)
            if indent_amt:
                indent_current_line(buf, indent_amt)
        event.app.current_buffer.insert_text(' ')

    @keybindings.add('c-m', filter=~is_searching) # Enter
    def _(event):
        buf = event.app.current_buffer

        # Submit if we're at the end of input on a single-line entry;
        # or if we're at the end of a multi-line entry and the current line
        # is blank.
        if should_submit_on_enter(buf):
            buf.validate_and_handle()
            return

        if is_true(insert_mode):
            cur_indent = indent(buf.document.current_line)
            buf.insert_text('\n')
            indent_amt = get_haskell_indent(buf.document)
            if indent_amt is None:
                indent_current_line(buf, cur_indent)
            elif indent_amt > 0:
                indent_current_line(buf, indent_amt)

    def indent_to_match(buf, left, right):
        doc = buf.document
        matchpos = doc.find_enclosing_bracket_left(left, right)
        if matchpos:
            (matchrow, matchcol) = doc.translate_index_to_position(
                matchpos + doc.cursor_position)
            if matchrow != doc.cursor_position_row:
                indent_current_line(buf, matchcol)

    # indent closing brace, paren or bracket
    @keybindings.add('}')
    def _(event):
        buf = event.app.current_buffer
        indent_to_match(buf, '{', '}')
        buf.insert_text('}')

    @keybindings.add(')')
    def _(event):
        buf = event.app.current_buffer
        indent_to_match(buf, '(', ')')
        buf.insert_text(')')

    @keybindings.add(']')
    def _(event):
        buf = event.app.current_buffer
        indent_to_match(buf, '[', ']')
        buf.insert_text(']')

operator_re = r'[!#$%&*+./<>?@\\^|~-]+'
continue_re = re.compile(r'.*(->|=>|::|\bin|\bwhere|\blet|\bdo|=|\bof|'
                         + operator_re + r')\s*$')
def should_submit_on_enter(buf):
    """
    Check whether we should submit the buffer, after enter was pressed.
    """
    # This is a bit hacky - we want to look for unmatched brackets in the
    # entire document, but find_enclosing_bracket_left starts from the 
    # current position.  So create a new document.
    doc = Document(buf.document.text, cursor_position=len(buf.document.text))
    if (doc.find_enclosing_bracket_left("(", ")")
        or doc.find_enclosing_bracket_left("[", "]")
        or doc.find_enclosing_bracket_left("{", "}")
        or continue_re.match(doc.current_line)):
        return False
    else:
        return True
    # if len(buf.document.current_line.strip()) == 0:
    #     return True
    # elif doc.is_cursor_at_the_end and len(buf.document.lines) == 1:
    #     else:
    #         return True


def check_indent_keys(document):
    """ Check whether the word before the cursor was an indent key (which
    will trigger a reevaluation of the line's indent)"""

    lw_start = document.find_previous_word_beginning() or 0
    lw_end = 0# document.find_previous_word_ending() or 0
    col = document.cursor_position_col
    #print('Prev token from', lw_start, 'to', lw_end)
    last_tok = document.current_line[col+lw_start:col+lw_end]
    #print('Prev token from', lw_start, 'to', lw_end, ':', last_tok)
    return re.match(
        r'\{|\}|\(|\)|\[|\]|,|where|let|deriving|in|::|->|=>|\||=',
        last_tok)

# backtrack to find guard clause
def calc_indent_guard(document, pos):
    if len(document.lines) > 1:
        l = document.lines[-2]
    else:
        l = ''
    c = document.cursor_position_row - 1
    s = indent(document.current_line)

    while c >= 1:
        if s == 0 and len(l) > 0:
            # top-level start, stop looking
            return indent_guard
        elif re.search(r'^\s+[|,=]\s+', l):
            # guard block found
            return re.search(r'[|,=]', l).start()
        else:
            if s > 0 and s <= pos:
                # found less deeper indentation (not starting with `,` or `=`)
                # stop looking
                return s + indent_guard
        c -= 1
        l = document.lines[c]
        s = indent(l)

    return None

def get_haskell_indent(document):
    line = document.current_line
    row = document.cursor_position_row
    if row > 0:
        prevline = document.lines[row-1]
    else:
        prevline = ''

    #print('line is', line)
    #print('prevline is', prevline)

    # reset
    if re.match(r'^\s*$', prevline) and not re.match(r'^\s*\S', line):
        return 0

    # TODO
    #   { foo :: Int
    # >>,
    #
    #   |
    #   ...
    # >>,
    # TODO


    # Note - I've expanded this to match any non-operator chars before
    # end of line as long as there's an operator at the end -- and I've
    # removed the first let-related match since this encompasses that.
    if re.match(r'.*'+operator_re+r'\s*$', prevline):
        return indent(prevline) + shiftwidth

    # let x = 1 in
    # >>>>x
    if re.search(r'\blet\b.*?\bin\b\s*$', prevline)\
            and not re.match(r'^\s*\bin\b', line):
        return re.search(r'\blet\b', prevline).start() + indent_let

    # let x = 1
    # let y = 2
    #
    # let x = 1
    # >>>>y = 2
    #
    # let x = 1
    # y 2
    if re.search(r'\blet\b\s+.+$', prevline):
        #print("MATCH1")
        if re.match(r'^\s*\blet\b', line):
            l = re.search(r'\blet\b', prevline)
            #print("MATCH1")
            if l: # s:isSYN('haskellLet', v:lnum - 1, l:s + 1)
                return l.start()
        elif re.search(r'\s=($|\s)', line):
            l = re.search(r'\blet\b', prevline)
            #print("MATCH2")
            if l: # s:isSYN('haskellLet', v:lnum - 1, l:s + 1)
                return l.start() + indent_let


    # foo =
    # >>bar
    m = re.search(r'=\s*$', prevline)
    if m:
        return indent(prevline) + shiftwidth

    # do
    # >>foo
    m = re.match(r'.*(\bdo\b)\s*$', prevline)
    if m:
        return m.start(1) + shiftwidth


    # do foo
    # >>>bar
    m = re.match(r'.*\b(do)\b\s+\S+.*$', prevline)
    if m:
      #if s:isSYN('haskellKeyword', v:lnum - 1, l:s + 1)
      return m.start(1) + indent_do


    # case foo of
    # >>bar -> quux
    m = re.match(r'.*\b(case)\b.+\bof\b\s*$', prevline)
    if m:
        # if get(g:,'haskell_indent_case_alternative', 0)
        #     return indent(v:lnum - 1) + &shiftwidth
        # else
        return m.start(1) + indent_case

    # if handling
    if not re.search(r'\belse\b', prevline):
        # TODO I don't get how the original regex works to indent 'else' when
        # 'then' is on the second line... my solution may be too lenient
        #m = re.match(r'.*\bif\b.*\b(then)\b.*', prevline)
        m = re.search(r'\b(then)\b.*', prevline)
        if m:
            return m.start(1)

        m = re.search(r'\bif\b', prevline)
        if m:
            return m.start() + indent_if

    # where
    # >>foo
    #
    m = re.search(r'\bwhere\b\s*$', prevline)
    if m:
        return indent(prevline) + indent_after_bare_where


    # where foo
    # >>>>>>bar
    #
    # where foo :: Int
    # >>>>>>>>>>-> Int
    #
    # where foo x
    # >>>>>>>>|
    if re.search(r'\bwhere\b\s+\S+.*$', prevline):
        if re.search(r'^\s*[=-]>\s', line) and re.search(r' :: ', prevline):
            return re.search(':: ', prevline).start()
        elif re.search(r'^\s*\|', line): # Guard
            s = re.search(r'\bwhere\b', prevline)
            return s.start() + indent_where + indent_guard
            # if s:isSYN('haskellWhere', v:lnum - 1, l:s + 1)
            #     return l:s + g:haskell_indent_where + g:haskell_indent_guard
        else:
            s = re.search(r'\bwhere\b', prevline)
            #if s:isSYN('haskellWhere', v:lnum - 1, l:s + 1)
            return s.start() + indent_where


    # newtype Foo = Foo
    # >>deriving
    if re.search(r'^\s*\b(newtype|data)\b[^{]+', prevline)\
            and re.search(r'^\s*\bderiving\b', line):
        return indent(prevline) + shiftwidth

    # foo :: Int
    # >>>>-> Int
    #
    # foo
    #   :: Int
    # foo
    if re.search(r'\s::\s', prevline):
        if re.search(r'^\s*[-=]>', line):
            return re.search(r'::\s', prevline).start()
        elif re.search(r'^\s+::', prevline):
            return re.search(r'::\s', prevline).start() - shiftwidth
 
    # foo :: Int
    #     -> Int
    # >>>>-> Int
    #
    # foo :: Monad m
    #     => Functor f
    # >>>>=> Int
    #
    # foo :: Int
    #     -> Int
    # foo x
    #
    # foo
    #   :: Int
    #   -> Int
    # foo x

    # TODO come back to this later
    # if re.search(r'^\s*[-=]>', prevline):
    #     if re.search(r'^\s*[-=]>', line):
    #         return re.search(r'[-=]', prevline).start()
    #     else
    #         #if s:isInBlock(l:hlstack):
    #         return re.search(r'[^-=]', prevline).start()
    #     else
    #       let l:m = matchstr(l:line, '^\s*\zs\<\S\+\>\ze')
    #       let l:l = l:prevline
    #       let l:c = v:lnum - 1

    #       while l:c >= 1
    #         # fun decl
    #         if l:l =~ ('^\s*' . l:m . '\(\s*::\|\n\s\+::\)')
    #           let l:s = match(l:l, l:m)
    #           if re.search(r'\C^\s*\<default\>', l:l).start() > -1
    #             return l:s - 8
    #           else
    #             return l:s
    #           endif
    #         # empty line, stop looking
    #         elseif l:l =~ '^$'
    #            return 0
    #         endif
    #         let l:c -= 1
    #         let l:l = getline(l:c)
    #       endwhile

    #       return 0

    #   | otherwise = ...
    # foo
    #
    #   | foo
    # >>, bar
    #
    #   | foo
    # >>= bar
    #
    #   | Foo
    # >>deriving
    if re.search(r'^\s+\|', prevline): #&& !s:isInBlock(l:hlstack):
        if re.search(r'\s*[,=]', line):
            return prevline.index('|')
        elif re.search(r'^\s*\bderiving\b', line):
            return prevline.index('|')
        elif not re.search(r'^\s*\|', line):
            return prevline.index('|') - indent_guard


    # foo :: ( Monad m
    #        , Functor f
    #        )
    #>>>>>=> Int
    if re.search(r'^\s*\)', prevline) and re.search(r'^\s*=>', line):
        s = re.search(r'\)', prevline)
        return s.start() - (shiftwidth + 1)

    
    # foo
    # >>{
    if re.search(r'^\s*\{', line):
        s = indent(prevline) or 0
        return s + shiftwidth

    #  in foo
    # where bar
    #
    # or
    #
    # foo
    # >>where
    if re.search(r'^\s*\bwhere\b', line):
        if re.search(r'^\s+in\s+', prevline):
            return re.search(r'in', prevline).start() - indent_in

        return (indent(prevline) or 0) + indent_before_where


    # let x = 1
    #     y = 2
    # >in x + 1
    if re.search(r'^\s*\bin\b', line):
        c = document.cursor_position_row - 1

        while c >= 0:
            l = document.lines[c]
            s = re.search(r'\blet\b', l)
            if s:# && l:s >= 1 && s:isSYN('haskellLet', c, s + 1)
                return s.start() + indent_in
            elif re.search(r'^\S', l):
                return None
            c -= 1


    # data Foo
    # >>= Bar
    #
    #   |
    #   ...
    # >>=
    #
    # foo
    # >>=
    if re.search(r'^\s*=', line):
        if re.search(r'^\bdata\b\s+[^=]+\s*$', prevline):
            return re.search(r'\bdata\b', prevline).start() + shiftwidth
        else:
            s = calc_indent_guard(document, line.index('='))
            if s:
                return s
            else:
                return shiftwidth


    #   |
    #   ...
    # >>|
    #
    # data Foo = Bar
    # >>>>>>>>>|
    if re.search(r'^\s*\|', line):
        if re.search(r'^\s*\bdata\b.+=.+$', prevline):
            return prevline.index('=')
        else:
            s = calc_indent_guard(document, line.index('|'))
        if s:
            return s

    # foo
    # >>:: Int
    if re.search(r'^\s*::\s', line):
        return indent(prevline) + shiftwidth

    return None


