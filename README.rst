emacs.d
=======

Emacs configuration files.

The configuration in `config.org <config.org>`_ uses `Org mode
<http://orgmode.org>`_ and `use-package
<https://github.com/jwiegley/use-package>`_.

Initialization
==============

This repository should be cloned to ``~/.emacs.d``::

    git clone https://github.com/tschutter/emacs.d.git ~/.emacs.d

On Windows, set the ``HOME`` environment variable to value of
``APPDATA``.  This is how the Windows version of Emacs locates the
``.emacs.d`` directory.

Packages
========

To update packages, display the package list with ``M-x
list-packages``, mark the packages needing updates with ``U``, and
update those packages with ``x``.

In ``init.el`` search for ``(ensure-package-installed`` to see the
list of packages that is checked for installation every time Emacs is
started.

Keybindings
===========

This is not just another keybindings cheat sheet.  This section lists
all of the keybindings added or enabled by ``init.el``.  For notation
information, see `Emacs Key Notation
<http://www.emacswiki.org/emacs/EmacsKeyNotation>`_ and `Emacs's Key
Notation: What's the difference between "<return>" and "RET"?
<http://ergoemacs.org/emacs/emacs_key_notation_return_vs_RET.html>`_.

**Buffers**
    ============= ======
    Key Sequence  Action
    ============= ======
    C-<tab>       Switch to next buffer not starting with *
    S-C-<tab>     Switch to previous buffer not starting with *
    <kp-subtract> Close current buffer and switch to next buffer not starting with *
    <kp-4>        Show list of recently opened files
    C-x C-r       Rename the current buffer and the file it is visiting
    ============= ======

**Bookmarks**
    ============= ======
    Key Sequence  Action
    ============= ======
    C-<f3>        Set F3 quick bookmark
    <f3>          Jump to F3 quick bookmark
    C-<f4>        Set F4 quick bookmark
    <f4>          Jump to F4 quick bookmark
    <kp-1>        Show list of bookmarks
    <kp-2>        Set a bookmark
    <kp-3>        Jump to a bookmark
    ============= ======

**Editing**
    ============= ======
    Key Sequence  Action
    ============= ======
    C-z           Undo
    C-S-z         Redo
    C-j           Join line with next (like vi)
    M-SPC         Collapse all spaces and newlines at point to one space
    C-c g         Goto line
    C-S-<up>      Move current line up one line
    C-S-<down>    Move current line down one line
    C-c d         Insert date time string at point
    C-=           Expand region
    C-x C-x       Exchange point and mark
    ============= ======

**Macros**
    ============= ======
    Key Sequence  Action
    ============= ======
    C-x (         Start recording macro
    C-x )         End recording macro
    C-x e         Execute last macro
    C-x C-k C-n   Cycle forwards through macro ring
    C-x C-k C-b   Cycle backwards through macro ring
    C-x C-k C-k   Execute last macro
    C-x C-k n     Name last macro
    C-x C-k b     Bind last macro to a key
    C-x C-k r     Apply last macro to current region
    C-x C-k C-c   Set value of counter (use before applying macro)
    C-x C-k C-i   Insert value of counter (use in macro)
    C-x C-k C-e   Edit last macro
    ============= ======

**Programming**
    ============= ======
    Key Sequence  Action
    ============= ======
    M-;           Comment region (DWIM)
    M-n           Jump to next instance of symbol at point
    M-p           Jump to previous instance of symbol at point
    M-;           Comment region
    M-<up>        Jump to previous flycheck error
    M-<down>      Jump to next flycheck error
    <f5>          Compile current buffer
    C-p           Move point to previous error and highlight it
    C-n           Move point to next error and highlight it
    C-x p l       Load pymacs and ropemacs
    ============= ======

**Python**
    ============= ======
    Key Sequence  Action
    ============= ======
    C-c >         Shift lines in region 4 columns to the right
    C-c <         Shift lines in region 4 columns to the left
    C-c C-v       Run pycheck on current buffer
    C-c h         Lookup symbol in the Python HTML indexes
    C-x p l       Load ropemacs
    ropemacs-mode Enable ropemacs
    C-c C-f       Find the occurrences of a the symbol at point (ropemacs)
    C-c C-g       Goto the definition of the symbol at point (ropemacs)
    C-c C-d       Show the documentation of the symbol at point (ropemacs)
    M-/           Select from a list of completions (ropemacs)
    ============= ======

**Other**
    ============= ======
    Key Sequence  Action
    ============= ======
    S-<left>      Select the window to the left of the current one
    S-<right>     Select the window to the right of the current one
    S-<up>        Select the window above the current one
    S-<down>      Select the window below the current one
    <kp-7>        Generate the diary window
    <kp-8>        Open diary
    <kp-9>        Display a three-month calendar
    C-h n         Display man page
    super-j       Jump to a Web site from a programmable hotlist
    ============= ======

Functions Not Bound to Keys
===========================

``url-humanify``
    Take the URL at point and make it human readable.

``git-timemachine``
    Enable git timemachine for file of current buffer.  Use 'n' and
    'p' keys to move between different versions.

``list-packages``
    List available and installed MELPA packages.

``package-install``
    Install a new MELPA package.

Interesting Minor Modes
=======================

``auto-revert-tail-mode``
    Tail a file such as a system log, so that changes made to that
    file by other programs are continuously displayed.  Replaces the
    older non-standard ``live-mode``.
