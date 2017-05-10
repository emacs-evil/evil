# Bug reports

Bug reports and feature requests should be made on the
[issue tracker](https://github.com/emacs-evil/evil/issues) here on github.

**Before** sending a bug report, please take note of the following comments.

1. Please provide a full description of the configuration of your
   system. This includes

  - Emacs version,
  - Evil version (e.g., the hash in the git repository),
  - Whether you use Emacs/Evil in X mode or in terminal mode,
  - Whether you use Emacs/Evil in terminal mode with some terminal multiplexer
    like **tmux** or **screen** (and see below).

2. Test the bug in a clean Emacs environment without any additional
   packages loaded (besides Evil itself). You can easily get such an
   environment by executing either `make emacs` (for X) or `make
   terminal` (for terminal mode) in Evil's source directory. If your
   bug is related to some other package, try to load only this package
   from the clean environment after using `M-x package-initialize`
   first.

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

# Pull Requests

1. Please follow the
   [Emacs Lisp Style Guide](https://github.com/bbatsov/emacs-lisp-style-guide/)
2. When in doubt, adhere to the style conventions in the source code,
   but don't be afraid of suggesting code enhancements in case you
   encounter suboptimal code.
3. If your pull requests adds a feature or fixes a regression, add
   tests covering it to `evil-tests.el`.
