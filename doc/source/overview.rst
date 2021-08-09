Overview
========

Evil is an extensible vi layer for Emacs.  It emulates the main
features of Vim, [#vim]_ turning Emacs into a modal editor.  Like Emacs in
general, Evil is extensible in Emacs Lisp.


Installation via package.el
---------------------------

Evil is available as a package from MELPA stable, MELPA unstable and
NonGNU ELPA. This is the recommended way of installing Evil.

To set up `package.el` to work with one of the MELPA repositories, you
can follow the instructions on `melpa.org
<https://melpa.org/#/getting-started>`_.

Alternatively you can use NonGNU ELPA. It is part of the default
package archives as of Emacs 28. For older Emacs versions you'll need
to add it yourself:

.. code-block:: elisp

   (add-to-list 'package-archives
                (cons "nongnu" (format "http%s://elpa.nongnu.org/nongnu/"
                                       (if (gnutls-available-p) "s" ""))))

Once that is done, you can execute the following commands::

  M-x package-refresh-contents
  M-x package-install RET evil RET

Finally, add the following lines to your Emacs init file:

.. code-block:: elisp

   (require 'evil)
   (evil-mode 1)


Manual installation
-------------------

First, install `goto-chg` and `cl-lib`.  If you have an Emacs version
of 24.3 or newer, you should already have `cl-lib`.

Evil lives in a git repository.  To download Evil, do::

  git clone --depth 1 https://github.com/emacs-evil/evil.git

Then add the following lines to your Emacs init file:

.. code-block:: elisp

   (add-to-list 'load-path "path/to/evil")
   (require 'evil)
   (evil-mode 1)

Ensure that your replace ``path/to/evil`` with the actual path to
where you cloned Evil.


Modes and states
----------------

The next time Emacs is started, it will come up in *normal state*,
denoted by ``<N>`` in the mode line.  This is where the main vi
bindings are defined.  Note that you can always disable normal state
with :kbd:`C-z`, which switches to an "Emacs state" (denoted by
``<E>``) in which vi keys are completely disabled.  Press :kbd:`C-z`
again to switch back to normal state.

state
  Evil uses the term *state* for what is called a "mode" in regular vi
  usage, because *modes* are understood in Emacs terms to mean
  something else.

Evil defines a number of states by default:

normal state (``<N>``)
  This is the default "resting state" of Evil, in which the main body
  of vi bindings are defined.

insert state (``<I>``)
  This is the state for insertion of text, where non-modified keys
  will insert the corresponding character in the buffer.

visual state (``<V>``)
  A state for selecting text regions.  Motions are available for
  modifying the selected region, and operators are available for
  acting on it.

replace state (``<R>``)
  A special state mostly similar to insert state, except it replaces
  text instead of inserting.

operator-pending state (``<O>``)
  A special state entered after launching an operator, but before
  specifying the corresponding motion or text object.

motion state (``<M>``)
  A special state useful for buffers that are read-only, where motions
  are available but editing operations are not.

Emacs state (``<E>``)
  A state that as closely as possible mimics default Emacs behaviour,
  by eliminating all vi bindings, except for :kbd:`C-z`, to re-enter
  normal state.


.. rubric:: Footnotes

.. [#vim] Vim is the most popular version of *vi*, a modal text editor
   with many implementations.  Vim also adds some functions of its
   own, like visual selection and text objects.  For more information
   see `the official Vim website <https://vim.org>`_.
