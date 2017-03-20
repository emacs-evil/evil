![An extensible vi layer for Emacs](https://raw.githubusercontent.com/emacs-evil/evil/master/doc/logo.png)

Evil is an **e**xtensible **vi** **l**ayer
for [Emacs](http://www.gnu.org/software/emacs/). It emulates the main features
of [Vim](http://www.vim.org/), and provides facilities for writing custom
extensions. Also see our page on [EmacsWiki](http://emacswiki.org/emacs/Evil).

# Download

Evil lives in a git repository. To download Evil, do

```
git clone https://github.com/emacs-evil/evil
```

# Install

Move Evil to `~/.emacs.d/evil`. Then add the following lines to `~/.emacs`:

```elisp
(add-to-list 'load-path "~/.emacs.d/evil")
(require 'evil)
(evil-mode 1)
```

## Dependencies

* Evil requires [undo-tree.el](http://www.emacswiki.org/emacs/UndoTree) in the
`load-path` for linear undo and undo branches.

* For the motions `g;` `g,` and for the last-change-register `.`, Evil requires the
[goto-chg.el](https://www.emacswiki.org/emacs/GotoChg) package,
which provides the functions `goto-last-change` and `goto-last-change-reverse`.

# Documentation

A brief
[PDF manual](https://raw.githubusercontent.com/emacs-evil/evil/master/doc/evil.pdf) is
available.

# Mailing list

Evil is discussed at the
[gmane.emacs.vim-emulation](http://lists.ourproject.org/cgi-bin/mailman/listinfo/implementations-list)
mailing list.

# IRC

Visit us on `irc.freenode.net #evil-mode`.

# Bug reports

Bug reports and feature requests should be made on the
[issue tracker](https://github.com/emacs-evil/evil/issues) here on github.

**Before** sending a bug report, please take note of the following comments.

1. Please provide a full description of the configuration of your system. This includes

  - Emacs version,
  - Evil version (e.g., the hash in the git repository),
  - Whether you use Emacs/Evil in X mode or in terminal mode,
  - Whether you use Emacs/Evil in terminal mode with some terminal multiplexer
    like **tmux** or **screen** (and see below).

2. Test the bug in a clean Emacs environment without any additional packages
   loaded (besides Evil itself). You can easily get such an environment by
   executing either `make emacs` (for X) or `make terminal` (for terminal mode)
   in Evil's source directory. If your bug is related to some other packages,
   try to load only this package directly from the clean environment.

3. If you've just updated your Evil from the repository, **do not forget to
   recompile** Evil by executing `make` in Evil's source directory.

4. If possible, try to give a minimal example how to reproduce the error
   starting from a fresh Emacs. The minimal example could be some buffer content
   and a sequence of key-strokes that show up the error.

5. If the bug you want to report is related to the behavior of some commands or
   motions, please note the following. One goal of Evil is to get a behavior as
   close to **Vim** as possible unless there is a good reason not to do so. The
   reason is that many users come from Vim or use both, Vim and Evil, and we
   want to keep the number of annoying differences between both as small as
   possible, especially in common commands. In case you get an unexpected
   behavior, please compare the behavior with plain Vim (i.e., without any
   customization), if there's a difference please file the bug with a
   description of this difference (and possibly a reference to Vim's
   documentation). If you realize that both, Vim and Evil, behave the same but
   you want a different behavior, you may make a **feature request** for some
   customization option (but the default behavior will probably not be changed
   in favor for Vim compatibility). Evil already contains several customization
   options and sometimes the desired changes can be achieved as easily as
   redefining some key-bindings.

# FAQ

## Problems with the **Escape** key in terminal mode

A common problem when using Evil in terminal mode is a certain delay after
pressing the Escape key. Even more, when pressing the Escape key followed
quickly by another key the command is recognized as M-<key> instead of two
separate command ESC followed by <key>. In fact, it is perfectly valid to
simulate M-<key> by pressing ESC <key> quickly (but see below).

The reason for this is that in terminal mode a key sequence involving the
Meta-key (or Alt-key) always generates a so called "escape sequence", i.e., a
sequence of two events sent to Emacs, the first being ESC the second the key
pressed simultaneously. The problem is that pressing the Escape-key itself also
generates the ESC event. Thus, if Emacs (and therefore Evil) receives an ESC
event there is no way to tell whether the Escape key has been pressed (and no
further event will arrive) or a M-<key> combination has been pressed (and the
<key> event will arrive soon). In order to distinguish both situations Evil does
the following. After receiving an ESC event Evil waits for a short time period
(specified by the variable `evil-esc-delay` which defaults to 0.01 seconds) for
another event. If no other event arrives Evil assumes that the plain Escape key
has been pressed, otherwise it assumes a M-<key> combination has been pressed
and combines the ESC event with the second one. Because a M-<key> sequence
usually generates both events in very quick succession, 0.01 seconds are usually
enough and the delay is hardly noticeable by the user.

But if you use a terminal multiplexer like **tmux** or **screen** the situation
may be worse. Those multiplexers have exactly the same problem recognizing
M-<key> sequences and often introduce their own delay for the ESC key. There is
no way for evil to influence this delay. In order to reduce it you must
reconfigure your terminal multiplexer.

Note that this problem should not arise when using Evil in X mode. The reason is
that in this case the Escape key itself generates a different command, namely
'escape (a symbol) and hence Evil can distinguish whether the Escape key or a
M-<key> combination has been pressed. But this also implies that pressing ESC
followed by <key> cannot be used to simulate M-<key> in X mode!

## Underscore "_" is not a word character

An underscore "_" is a word character in Vim. This means that word-motions like
`w` skip over underlines in a sequence of letters as if it was a letter itself.
In contrast, in Evil the underscore is often a non-word character like
operators, e.g. `+`.

The reason is that Evil uses Emacs' definition of a word and this definition
does often not include the underscore. In Emacs word characters are determined
by the syntax-class of the buffer. The syntax-class usually depends on the
major-mode of this buffer. This has the advantage that the definition of a
"word" may be adapted to the particular type of document being edited. Evil uses
Emacs' definition and does not simply use Vim's definition in order to be
consistent with other Emacs functions. For example, word characters are exactly
those characters that are matched by the regular expression character class
`[:word:]`.

If you want the underscore to be recognised as word character, you can modify
its entry in the syntax-table:

```elisp
(modify-syntax-entry ?_ "w")
```

This gives the underscore the word syntax-class. You can use a mode-hook to
modify the syntax-table in all buffers of some mode, e.g.:

```elisp
(add-hook 'c-mode-common-hook #'(lambda () (modify-syntax-entry ?_ "w")))
```

This gives the underscore the word syntax-class in all C-like buffers.
