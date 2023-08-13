![An extensible vi layer for Emacs](https://raw.githubusercontent.com/emacs-evil/evil/master/doc/logo.png)

[![Build status](https://github.com/emacs-evil/evil/actions/workflows/test.yml/badge.svg)](https://github.com/emacs-evil/evil/actions/workflows/test.yml)
[![MELPA](https://melpa.org/packages/evil-badge.svg)](https://melpa.org/#/evil)
[![MELPA Stable](https://stable.melpa.org/packages/evil-badge.svg)](https://stable.melpa.org/#/evil)
[![NonGNU ELPA](http://elpa.nongnu.org/nongnu/evil.svg)](http://elpa.nongnu.org/nongnu/evil.html)
[![Documentation Status](https://readthedocs.org/projects/evil/badge/?version=latest)](https://evil.readthedocs.io/en/latest/?badge=latest)
[![License: GPL v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)

Evil is an **e**xtensible **vi** **l**ayer
for [Emacs](http://www.gnu.org/software/emacs/). It emulates the main features
of [Vim](http://www.vim.org/), and provides facilities for writing custom
extensions. Also see our page on [EmacsWiki](http://emacswiki.org/emacs/Evil).

# Installation

See the 
[official documentation](https://evil.readthedocs.io/en/latest/overview.html#installation-via-package-el)
for installation instructions. We recommend using *package.el*.

As a quickstart, you can add the following code to your Emacs init
file.

```elisp
;; Set up package.el to work with MELPA
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)
(package-refresh-contents)

;; Download Evil
(unless (package-installed-p 'evil)
  (package-install 'evil))

;; Enable Evil
(require 'evil)
(evil-mode 1)
```

## Dependencies

* Evil requires Emacs 24.1 or later.

* Evil requires any of the following for `C-r`:
  * `undo-redo` from Emacs 28
  * The [undo-tree](https://gitlab.com/tsc25/undo-tree) package
    (available via GNU ELPA)
  * The [undo-fu](https://gitlab.com/ideasman42/emacs-undo-fu) package
    (available via MELPA)

* For the motions `g;` `g,` and for the last-change-register `.`, Evil
  requires the [goto-chg.el](https://github.com/emacs-evil/goto-chg)
  package (available via MELPA and NonGNU ELPA), which provides the
  functions `goto-last-change` and `goto-last-change-reverse`.

* For Emacs 24.1 and 24.2 Evil also requires
  [cl-lib](https://elpa.gnu.org/packages/cl-lib.html).

# Documentation

The latest version of the documentation is readable online
[here](https://evil.readthedocs.io/en/latest/index.html). It is also
available as
[PDF](https://readthedocs.org/projects/evil/downloads/pdf/latest/) and
as [EPUB](https://readthedocs.org/projects/evil/downloads/epub/latest/).

# Mailing list

Evil is discussed at the
[gmane.emacs.vim-emulation](http://lists.ourproject.org/cgi-bin/mailman/listinfo/implementations-list)
mailing list.

# IRC

Visit us on `irc.libera.chat #evil-mode`.

# Contribution

See
[CONTRIBUTING.md](https://github.com/emacs-evil/evil/blob/master/CONTRIBUTING.md)
for guidelines for issues and pull requests.
