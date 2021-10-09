Settings
========

Evil's behaviour can be adjusted by setting some variables.  The list
of all available variables and their current values can be inspected
by doing::

  M-x customize-group RET evil RET

To change the value of a variable, you can use this interface, or add
a ``setq`` form to your Emacs init file, preferably before Evil is
loaded. [#order]_

.. code-block:: elisp

   (setq evil-shift-width 0)
   ;; Load Evil
   (require 'evil)

What follows is a non-exhaustive list of the most relevant
customization options.


The initial state
-----------------

The initial state of a buffer is determined by its major mode.  Evil
maintains an association between major modes and their corresponding
states, which is most easily modified using the function
:elisp:ref:`evil-set-initial-state`.

.. elisp:autofunction:: evil-set-initial-state

If no state can be found, Evil uses the default initial state.

.. elisp:autovariable:: evil-default-state

Alternatively, it is possible to select the initial state based on the
buffer *name* rather than its major mode.  This is checked first, so
it takes precedence over the other methods for setting the state.

.. elisp:autovariable:: evil-buffer-regexps


Keybindings and other behaviour
-------------------------------

Evil comes with a rich system for modifying its key bindings
:ref:`chapter-keymaps`.  For the most common tweaks, the following
variables are available.

.. elisp:autovariable:: evil-toggle-key

.. elisp:autovariable:: evil-want-C-i-jump

.. elisp:autovariable:: evil-want-C-u-delete

.. elisp:autovariable:: evil-want-C-u-scroll

.. elisp:autovariable:: evil-want-C-d-scroll

.. elisp:autovariable:: evil-want-C-w-delete

.. elisp:autovariable:: evil-want-C-w-in-emacs-state

.. elisp:autovariable:: evil-want-Y-yank-to-eol

.. elisp:autovariable:: evil-disable-insert-state-bindings


Search
------

.. elisp:autovariable:: evil-search-module

.. elisp:autovariable:: evil-regexp-search

.. elisp:autovariable:: evil-search-wrap

.. elisp:autovariable:: evil-flash-delay

.. elisp:autovariable:: evil-ex-hl-update-delay

.. elisp:autovariable:: evil-ex-search-incremental


Indentation
-----------

.. elisp:autovariable:: evil-auto-indent

.. elisp:autovariable:: evil-shift-width

.. elisp:autovariable:: evil-shift-round

.. elisp:autovariable:: evil-indent-convert-tabs


Cursor movement
---------------

In standard Emacs terms, the cursor is generally understood to be
located between two characters.  In Vim, and therefore also Evil, this
is the case in insert state, but in other states the cursor is
understood to be *on* a character, and that this character is not a
newline.

Forcing this behaviour in Emacs is the source of some potentially
surprising results (especially for traditional Emacs users---users
used to Vim may find the default behavior to their satisfaction). Many
of them can be tweaked using the following variables.

.. elisp:autovariable:: evil-repeat-move-cursor

.. elisp:autovariable:: evil-move-cursor-back

.. elisp:autovariable:: evil-move-beyond-eol

.. elisp:autovariable:: evil-cross-lines

.. elisp:autovariable:: evil-respect-visual-line-mode

.. elisp:autovariable:: evil-track-eol

.. elisp:autovariable:: evil-start-of-line


Cursor display
--------------

A state may change the appearance of the cursor.  Use the variable
:elisp:ref:`evil-default-cursor` to set the default cursor, and the
variables ``evil-normal-state-cursor``, ``evil-insert-state-cursor``
etc. to set the cursors for specific states.  The acceptable values
for all of them are the same.

.. elisp:autovariable:: evil-default-cursor


Window management
-----------------

.. elisp:autovariable:: evil-auto-balance-windows

.. elisp:autovariable:: evil-split-window-below

.. elisp:autovariable:: evil-vsplit-window-right


Parenthesis highlighting
------------------------

These settings concern the integration between Evil and
``show-paren-mode``.  They take no effect if this mode is not enabled.

.. elisp:autovariable:: evil-show-paren-range

.. elisp:autovariable:: evil-highlight-closing-paren-at-point-states


Miscellaneous
-------------

.. elisp:autovariable:: evil-want-fine-undo

.. elisp:autovariable:: evil-undo-system

.. elisp:autovariable:: evil-backspace-join-lines

.. elisp:autovariable:: evil-kbd-macro-suppress-motion-error

.. elisp:autovariable:: evil-mode-line-format

.. elisp:autovariable:: evil-mouse-word

.. elisp:autovariable:: evil-bigword

.. elisp:autovariable:: evil-esc-delay

.. elisp:autovariable:: evil-intercept-esc

.. elisp:autovariable:: evil-kill-on-visual-paste

.. elisp:autovariable:: evil-echo-state

.. elisp:autovariable:: evil-complete-all-buffers

.. elisp:autovariable:: evil-want-empty-ex-last-command


.. rubric:: Footnotes

.. [#order] Strictly speaking, the order only matters if the variable
   affects the way Evil is loaded.  This is the case with some
   variables.
