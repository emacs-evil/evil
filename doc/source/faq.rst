Frequently Asked Questions
==========================

Problems with the escape key in the terminal
--------------------------------------------

A common problem when using Evil in terminal mode is a certain delay
after pressing the escape key. Even more, when pressing the escape key
followed quickly by another key the command is recognized as
:kbd:`M-<key>` instead of two separate keys: :kbd:`ESC` followed by
:kbd:`<key>`. In fact, it is perfectly valid to simulate
:kbd:`M-<key>` by pressing :kbd:`ESC <key>` quickly (but see below).

The reason for this is that in terminal mode a key sequence involving
the meta key (or alt key) always generates a so called "escape
sequence", i.e. a sequence of two events sent to Emacs, the first
being :kbd:`ESC` and the second the key pressed simultaneously. The
problem is that pressing the escape key itself also generates the
:kbd:`ESC` event. Thus, if Emacs (and therefore Evil) receives an
:kbd:`ESC` event there is no way to tell whether the escape key has
been pressed (and no further event will arrive) or a :kbd:`M-<key>`
combination has been pressed (and the :kbd:`<key>` event will arrive
soon). In order to distinguish both situations Evil does the
following. After receiving an :kbd:`ESC` event Evil waits for a short
time period (specified by the variable :elisp:ref:`evil-esc-delay`
which defaults to 0.01 seconds) for another event. If no other event
arrives Evil assumes that the plain escape key has been pressed,
otherwise it assumes a :kbd:`M-<key>` combination has been pressed and
combines the :kbd:`ESC` event with the second one. Because a
:kbd:`M-<key>` sequence usually generates both events in very quick
succession, 0.01 seconds are usually enough and the delay is hardly
noticeable by the user.

If you use a terminal multiplexer like *tmux* or *screen* the
situation may be worse. These multiplexers have exactly the same
problem recognizing :kbd:`M-<key>` sequences and often introduce their
own delay for the :kbd:`ESC` key. There is no way for Evil to
influence this delay. In order to reduce it you must reconfigure your
terminal multiplexer.

Note that this problem should not arise when using Evil in graphical
mode. The reason is that in this case the escape key itself generates
a different command, namely ``escape`` (a symbol) and hence Evil can
distinguish whether the escape key or a :kbd:`M-<key>` combination has
been pressed. But this also implies that pressing :kbd:`ESC` followed
by <key> cannot be used to simulate :kbd:`M-<key>` in graphical mode!


Underscore is not a word character
----------------------------------

An underscore ``_`` is a word character in Vim. This means that word
motions like :kbd:`w` skip over underlines in a sequence of letters as
if it was a letter itself.  In contrast, in Evil the underscore is
often a non-word character like operators, e.g. ``+``.

The reason is that Evil uses Emacs' definition of a word and this
definition does often not include the underscore. In Emacs word
characters are determined by the syntax-class of the buffer. The
syntax-class usually depends on the major-mode of this buffer. This
has the advantage that the definition of a "word" may be adapted to
the particular type of document being edited. Evil uses Emacs'
definition and does not simply use Vim's definition in order to be
consistent with other Emacs functions. For example, word characters
are exactly those characters that are matched by the regular
expression character class ``[:word:]``.

If you would be satisfied by having the :kbd:`*` and :kbd:`#` searches
use symbols instead of words, this can be achieved by setting the
:elisp:ref:`evil-symbol-word-search` variable to ``t``.

If you want the underscore to be recognised as word character for other
motions, you can modify its entry in the syntax-table:

.. code-block:: elisp

   (modify-syntax-entry ?_ "w")

This gives the underscore the 'word' syntax class. You can use a
mode-hook to modify the syntax-table in all buffers of some mode,
e.g.:

.. code-block:: elisp

   (add-hook 'c-mode-common-hook
             (lambda () (modify-syntax-entry ?_ "w")))

This gives the underscore the word syntax-class in all C-like buffers.

Similarly to Emacs' definition of a word, the definition of a "symbol" is also
dependent on the syntax-class of the buffer, which often includes the
underscore. The default text objects keymap associates kbd::`o` with the symbol
object, making kbd::`cio` a good alternative to Vim's kbd::`ciw`, for example.
The following will swap between the word and symbol objects in the keymap:

.. code-block:: elisp

   (define-key evil-outer-text-objects-map "w" 'evil-a-symbol)
   (define-key evil-inner-text-objects-map "w" 'evil-inner-symbol)
   (define-key evil-outer-text-objects-map "o" 'evil-a-word)
   (define-key evil-inner-text-objects-map "o" 'evil-inner-word)

This will not change the motion keys, however. One way to make word motions
operate as symbol motions is to alias the ``evil-word`` *thing* [#thingatpt]_ to
the ``evil-symbol`` thing:

.. code-block:: elisp

   (defalias 'forward-evil-word 'forward-evil-symbol)


.. rubric:: Footnotes

.. [#thingatpt] Many of Evil's text objects and motions are defined in
   terms of the *thingatpt* library, which in this case are defined
   entirely in terms of ``forward-THING`` functions.  Thus aliasing
   one to another should make all motions and text objects implemented
   in terms of that *thing* behave the same.
