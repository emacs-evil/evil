;;; evil-cjk.el --- CJK support for word motions

;; Author: tarao.gnn at gmail.com
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>
;;
;; This file is NOT part of GNU Emacs.

;;; License:

;; This file is part of Evil.
;;
;; Evil is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; Evil is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with Evil.  If not, see <http://www.gnu.org/licenses/>.

(require 'evil-common)

;;; Code:

(eval-when-compile
  ;; These are taken from word_boundary_p function in category.c
  (defmacro categoryp (x) `(and (integerp ,x) (>= ,x #x20) (<= ,x #x7e)))
  (defmacro word-boundary-p (ch1 ch2)
    "Return t if there is a word boundary between two
word-consistuent characters CH1 and CH2 if they appear in this
order, else return nil."
    (cond
     ((>= emacs-major-version 23)
      `(let (tail result)
         (if (eq (aref char-script-table ,ch1) (aref char-script-table ,ch2))
             (setq tail   word-separating-categories
                   result nil)
           (setq tail   word-combining-categories
                 result t))
         (let ((cat1 (char-category-set ,ch1))
               (cat2 (char-category-set ,ch2)))
           (when (and cat1 cat2)
             (while (consp tail)
               (let ((elt (car tail)))
                 (when (and (consp elt)
                            (or (null (car elt))
                                (and (categoryp (car elt))
                                     (aref cat1 (car elt))
                                     (not (aref cat2 (car elt)))))
                            (or (null (cdr elt))
                                (and (categoryp (cdr elt))
                                     (not (aref cat1 (cdr elt)))
                                     (aref cat2 (cdr elt)))))
                   (setq result (not result))
                   (setq tail nil)))
               (setq tail (cdr tail))))
           result)))
     (t
      ;; TODO: to support (< emacs-major-version 23), we need to
      ;; implement a function which emulates CHAR_CHARSET macro in
      ;; charset.h.
      '(progn nil)
      ))))

(defun evil-cjk-word-boundary-p (ch1 ch2)
  "Test if there is a word boundary between two characters.
Return t if there is a word boundary between two word-consistuent
characters CH1 and CH2 if they appear in this order, else return
nil.  This function acts exactly the same as `word-boundary-p' if
`evil-cjk-emacs-word-boundary' is non-nil."
  (if evil-cjk-emacs-word-boundary
      (word-boundary-p ch1 ch2)
    (let ((word-separating-categories evil-cjk-word-separating-categories)
          (word-combining-categories evil-cjk-word-combining-categories))
      (word-boundary-p ch1 ch2))))

(defun evil-move-word-cjk (count)
  "Move by COUNT words being sensitive to CJK word boundary."
  (let ((regexp (format "[%s]" evil-word)))
    (evil-motion-loop (var count)
      (cond
       ((< var 0)
        (re-search-backward regexp nil t)
        ;; instead of (skip-chars-backward evil-word), move backward until
        ;; a word boundary is found
        (let ((limit (save-excursion (skip-chars-backward evil-word) (point))))
          (while (and (not (bobp))
                      (> (point) limit)
                      (not (evil-cjk-word-boundary-p
                            (char-before (point)) (char-after (point)))))
            (backward-char))))
       (t
        (re-search-forward regexp nil t)
        ;; instead of (skip-chars-forward evil-word), move forward until
        ;; a word boundary is found
        (let ((limit (save-excursion (skip-chars-forward evil-word) (point))))
          (while (and (not (eobp))
                      (< (point) limit)
                      (not (evil-cjk-word-boundary-p
                            (char-before (point)) (char-after (point)))))
            (forward-char))))))))

(provide 'evil-cjk)

;;; evil-cjk.el ends here
