Extension
=========

The main functionality of Evil is implemented in terms of reusable
macros.  Package writers can use these to define new commands.


Motions
-------

A *motion* is a command which moves the cursor, such as :kbd:`w` or
:kbd:`e`.  Motions are defined with the macro
:elisp:ref:`evil-define-motion`.  Motions not defined in this way
should be declared with :elisp:ref:`evil-declare-motion`.

.. elisp:autofunction:: evil-declare-motion

.. elisp:autofunction:: evil-define-motion

For example, this is a motion that moves the cursor forward by a
number of characters:

.. code-block:: elisp

   (evil-define-motion foo-forward (count)
     "Move to the right by COUNT characters."
     :type inclusive
     (forward-char (or count 1)))

The *type* of a motion determines how it works when used together with
an operator.  Inclusive motions include the endpoint in the range
being operated on, while exclusive motions do not.  Line motions
extend the whole range to linewise positions, effectively behaving as
if the endpoint were really at the end of the line.  Blockwise ranges
behave as a "rectangle" on screen rather than a contiguous range of
characters.


Operators
---------

An operator is a command that acts on the text moved over by a motion,
such as :kbd:`c` (change), :kbd:`d` (delete) or :kbd:`y` (yank or
copy, not to be confused with "yank" in Emacs terminology which means
*paste*).

.. elisp:autofunction:: evil-define-operator

For example, this is an operator that performs ROT13 encryption on the
text under consideration:

.. code-block:: elisp

   (evil-define-operator evil-rot13 (beg end)
     "ROT13 encrypt text."
     (rot13-region beg end))

Binding this to :kbd:`g?` (where it is by default) will cause a key
sequence such as :kbd:`g?w` to encrypt from the current cursor to the
end of the word.


Text objects
------------

Text objects are like motions in that they define a range over which
an operator may act.  Unlike motions, text objects can set both a
beginning and an endpoint.  In visual state, text objects alter both
ends of the selection.

Text objects are not directly usable in normal state.  Instead, they
are bound in the two keymaps ``evil-inner-text-ojects-map`` and
``evil-outer-text-objects-map``, which are available in visual and
operator-pending state under the keys :kbd:`i` and :kbd:`a`
respectively.

.. elisp:autofunction:: evil-define-text-object

For eample, this is a text object which selects the next three
characters after the current location:

.. code-block:: elisp

   (evil-define-text-object foo (count)
     "Select three characters."
     (list (point) (+ 3 (point))))

For convenience, Evil provides several functions returning a list of
positions which can be used for defining text objects.  All of them
follow the convention that a positive *count* selects text after the
current location, while negative *count* selects text before it.

.. note::

   The *thingatpt* library is used quite extensively in Evil to define
   text objects, and this dependency leaks through in the following
   functions.  A *thing* in this context is any symbol for which there
   is a function called ``forward-THING`` [#thing]_ which moves past a
   number of *things*.

.. elisp:autofunction:: evil-select-inner-object

.. elisp:autofunction:: evil-select-an-object

.. elisp:autofunction:: evil-select-paren


Range types
-----------

A *type* is a transformation acting on a pair of buffer positions.
Evil defines the types ``inclusive``, ``line``, ``block`` and
``exclusive``, which are used for motion ranges and visual selection.
New types may be defined with the macro *evil-define-type*.

.. elisp:autofunction:: evil-define-type


States
------

States are defined with the macro :elisp:ref:`evil-define-state`,
which takes care to define the necessary hooks, keymaps and variables,
as well as a toggle function ``evil-NAME-state`` and a predicate
function ``evil-NAME-state-p`` for checking whether the state is
active.

.. elisp:autofunction:: evil-define-state

For example:

.. code-block:: elisp

   (evil-define-state test
     "Test state."
     :tag " <T> "
     (message (if (evil-test-state-p)
                  "Enabling test state."
                "Disabling test state.")))


.. rubric:: Footnotes

.. [#thing] There are many more ways that a *thing* can be defined,
   but the definition of ``forward-THING`` is perhaps the most
   straightforward way to go about it.
