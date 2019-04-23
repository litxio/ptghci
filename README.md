# ptGHCi

## Overview

ptGHCi is an interactive command shell for Haskell designed for high-productivity interactive coding.  It is implemented as a wrapper around GHCi based on Python's `prompt-toolkit` library and heavily inspired by IPython.  

## Features

### Syntax highlighting

Highlighting is based on the `pygments` library, with a variety of styles available through the `%style` [magic command](#magic-commands).

![Syntax highlighting](https://litxio.github.io/ptghci/images/syntax_highlight.svg)

### Multiline commands with automatic indentation

Use `<Alt-Enter>` (a.k.a. `<Meta-Enter>`) to insert a new line.  ptGHCi will also intelligently insert a new line when Enter is pressed after an operator, following keywords like `do` and `while`, or when within unclosed brackets.

![Auto-indent](https://litxio.github.io/ptghci/images/autoindent.svg)

### Real-time type display

With `typeBarEnabled` on, ptGHCi shows the type of the identifier under the cursor in real-time while you type.  Also shows the types of tab-completed suggestions.

![Type bar](https://litxio.github.io/ptghci/images/type_bar.svg)

### Tab completion

In a menu, with the ability to cycle through options with <Tab>

![Tab completion](https://litxio.github.io/ptghci/images/tab_completion.svg)

### Edit command in external editor

ptGHCi prompt not powerful enough for you?  Press `<F2>` to edit the current entry at the prompt in an external editor of your choosing.

![External editor](https://litxio.github.io/ptghci/images/external_editor.svg)

### Show and re-run command history

Use the `%past` magic command to list prior commands entered into the prompt during the current session, and `%rerun` to rerun past commands.  Useful for restoring bindings after a `:reload`.  

![Command history](https://litxio.github.io/ptghci/images/history.svg)

## Installation

Use `stack` to install the `ptghci` binary on your `$PATH`; ptGHCi uses libpython and you will need to use `pip` to install the Python requirements:

```
git clone https://github.com/litxio/ptghci
cd ptghci
pip3 install -r pybits/requirements.txt
stack install
```

## Usage and configuration

Just run `ptghci` to start a session; any command line arguments will be passed to GHCi.  The command used to start GHCi can be set via the `ghciCommand` setting in the configuration file.  By default it is `stack ghci` if `stack` is on `$PATH`, otherwise just `ghci`.

ptGHCi uses a yaml configuration file, which it will look for in the following locations in order of decreasing priority:

 * ./ptghci.yaml
 * ./.ptghci.yaml
 * $HOME/.ptghci.yaml

The file `ptghci.yaml.defaults` lists the available options and their defaults.  

### Magic commands

Special "magic" commands understood by ptGHCi start with `%` by default:

 * `%past [-n N]`: Lists the prior commands entered into the prompt during the current session.  Use `%past -n N` to list up to N past commands, including commands from prior sessions.
 * `%rerun <lines or ranges>`: Re-runs past commands, provided as a line number, range, or comma-separated list of line numbers (prefixed by 'p' for history from past sessions) and ranges. Example: %rerun 3,4-5,p8,p23-p24
 * `%hoogle <identifier>`: Runs hoogle for the identifier
 * `%style [style_name]`: Without an argument, lists the available styles with sample code.  With an argument, sets the style to the given style name.  

 If you find it annoying to have to distinguish between `:` GHCi commands and '%' ptGHCi commands, you can set the `magicPrefix` configuration option to ":" and use colon for everything.


## Known limitations

 * Reading from STDIN with `getLine` and the like is not yet supported
 * Running the GHCi debugger with `:trace` is not yet supported
 * I know of no reason that it should not be possible to run ptGHCi on Windows, but this has not yet been tested.  Pkg-config, pcre-light, and zeromq4-haskell will need to be installed correctly with their dependencies.  If anyone tries installing on Windows I would love to hear about the results.
