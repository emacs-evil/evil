;;; evil.el --- extensible vi layer

;; Author: Frank Fischer <frank.fischer at mathematik.tu-chemnitz.de>
;;      Vegard Øye <vegard_oye at hotmail.com>
;;      Nikolai Weibull <now at bitwi.se>
;; Maintainer: <implementations-list at lists.ourproject.org>
;; Created: 2011-03-01
;; Version: 0.1
;; Keywords: emulation, vim
;; URL: http://gitorious.org/evil
;;
;; This file is not part of GNU Emacs.

;;; Commentary:

;; Evil is an extensible vi layer for Emacs. It emulates the main
;; features of Vim, and provides facilities for writing custom
;; extensions.
;;
;; Evil lives in a Git repository. To obtain Evil, do
;;
;;      git clone git://gitorious.org/evil/evil.git
;;
;; Evil is discussed at <implementations-list at lists.ourproject.org>,
;; a mailing list to which you can subscribe at:
;;
;; http://lists.ourproject.org/cgi-bin/mailman/listinfo/implementations-list
;;
;; Subscription is not required; we usually reply within a few days
;; and CC our replies back to you.

;;; Code:

(require 'evil-vars)
(require 'evil-common)
(require 'evil-states)
(require 'evil-types)
(require 'evil-motions)
(require 'evil-undo)
(require 'evil-digraphs)
(require 'evil-insert)
(require 'evil-replace)
(require 'evil-visual)
(require 'evil-operators)
(require 'evil-search)
(require 'evil-repeat)
(require 'evil-maps)

(provide 'evil)

;;; evil.el ends here
