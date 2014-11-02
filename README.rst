emacs.d
=======

Emacs configuration files.

This repository should be cloned to ~/.emacs.d::

    git clone https://github.com/tschutter/emacs.d.git ~/.emacs.d

Keybindings
===========

**Buffers**
    ============ ======
    Key Sequence Action
    ============ ======
    S-C-Tab      Switch to previous buffer not starting with *
    C-Tab        Switch to next buffer not starting with *
    kp-subtract  Close current buffer and switch to next buffer not starting with *
    kp-4         Show list of recently opened files
    C-x C-r      Rename current buffer and file it is visiting
    ============ ======

**Bookmarks**
    ============ ======
    Key Sequence Action
    ============ ======
    C-<F3>       Set F3 bookmark
    <F3>         Jump to F3 bookmark
    C-<F4>       Set F4 bookmark
    <F4>         Jump to F4 bookmark
    kp-1         Show list of bookmarks
    kp-2         Set a bookmark
    kp-3         Jump to a bookmark
    ============ ======

**Editing**
    ============ ======
    Key Sequence Action
    ============ ======
    C-z          Undo
    C-S-z        Redo
    C-j          Join line with next (like vi)
    M-SPC        Collapse all spaces and newlines at point to one space
    C-c g        Goto line
    C-c d        Insert date time string at point
    C-S-<up>     Move current line up one line
    C-S-<down>   Move current line down one line
    ============ ======

**Programming**
    ============ ======
    Key Sequence Action
    ============ ======
    M-<up>       Jump to previous flycheck error
    M-<down>     Jump to next flycheck error
    <F5>         Compile current buffer
    C-p          Move point to previous error and highlight it
    C-n          Move point to next error and highlight it
    C-x p l      Load pymacs and ropemacs
    ============ ======

**Other**
    ============ ======
    Key Sequence Action
    ============ ======
    S-<left>     Select the window to the left of the current one
    S-<right>    Select the window to the right of the current one
    S-<up>       Select the window above the current one
    S-<down>     Select the window below the current one
    kp-7         Generate the diary window
    kp-8         Open diary
    kp-9         Display a three-month calendar
    super-j      Jump to a Web site from a programmable hotlist
    ============ ======
