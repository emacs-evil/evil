Internals
=========

Command properties
------------------

Evil defines *command properties* to store information about commands
[#command]_, such as whether they should be repeated.  A command
property is a ``:keyword`` with an associated value, e.g.
``:repeat nil``.

.. elisp:autofunction:: evil-add-command-properties

.. elisp:autofunction:: evil-set-command-properties

.. elisp:autofunction:: evil-get-command-properties

.. elisp:autofunction:: evil-get-command-property

.. elisp:autofunction:: evil-define-command


For setting repeat properties, use the following functions:

.. elisp:autofunction:: evil-declare-repeat

.. elisp:autofunction:: evil-declare-not-repeat

.. elisp:autofunction:: evil-declare-change-repeat


.. rubric:: Footnotes

.. [#command] In this context, a *command* may mean any Evil motion,
   text object, operator or indeed other Emacs commands, which have
   not been defined through the Evil machinery.
