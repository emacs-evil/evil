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
;; a mailing list you can subscribe to at:
;;
;; http://lists.ourproject.org/cgi-bin/mailman/listinfo/implementations-list
;;
;; Subscription is not required; we usually reply within a few days
;; and CC our replies back to you.

;;; License:

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 59 Temple Place - Suite 330, Boston, MA
;; 02111-1307, USA.

;;; Code:

(require 'evil-vars)
(require 'evil-common)
(require 'evil-core)
(require 'evil-states)
(require 'evil-repeat)
(require 'evil-macros)
(require 'evil-search)
(require 'evil-ex)
(require 'evil-digraphs)
(require 'evil-types)
(require 'evil-commands)
(require 'evil-maps)
(require 'evil-integration)

(provide 'evil)

;;; evil.el ends here
