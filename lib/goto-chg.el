;;; goto-chg.el --- goto last change
;;--------------------------------------------------------------------
;;
;; Copyright (C) 2002-2008,2013 David Andersson
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be
;; useful, but WITHOUT ANY WARRANTY; without even the implied
;; warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR
;; PURPOSE.  See the GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public
;; License along with this program; if not, write to the Free
;; Software Foundation, Inc., 59 Temple Place, Suite 330, Boston,
;; MA 02111-1307 USA
;;
;;-------------------------------------------------------------------
;;
;; Author: David Andersson <l.david.andersson(at)sverige.nu>
;; Maintainer: Vasilij Schneidermann <v.schneidermann@gmail.com>
;; Created: 16 May 2002
;; Version: 1.7
;; Keywords: convenience, matching
;;
;;; Commentary:
;;
;; Goto Last Change
;;
;; Goto the point of the most recent edit in the buffer.
;; When repeated, goto the second most recent edit, etc.
;; Negative argument, C-u -, for reverse direction.
;; Works by looking into buffer-undo-list to find points of edit.
;;
;; You would probably like to bind this command to a key.
;; For example in your ~/.emacs:
;;
;;   (require 'goto-chg)
;;
;;   (global-set-key [(control ?.)] 'goto-last-change)
;;   (global-set-key [(control ?,)] 'goto-last-change-reverse)
;;
;; Works with emacs-19.29, 19.31, 20.3, 20.7, 21.1, 21.4, 22.1 and 23.1
;; Works with XEmacs-20.4 and 21.4 (but see todo about `last-command' below)
;;
;;--------------------------------------------------------------------
;; History
;;
;; Ver 1.7 2017-09-17 Vasilij Schneidermann
;;    Make it work with undo-tree-mode (see
;;    <https://github.com/martinp26/goto-chg>)
;; Ver 1.6 2013-12-12 David Andersson
;;    Add keywords; Cleanup comments
;; Ver 1.5 2013-12-11 David Andersson
;;    Autoload and document `goto-last-change-reverse'
;; Ver 1.4 2008-09-20 David Andersson
;;    Improved property change description; Update comments.
;; Ver 1.3 2007-03-14 David Andersson
;;    Added `goto-last-change-reverse'
;; Ver 1.2 2003-04-06 David Andersson
;;    Don't let repeating error depthen glc-probe-depth.
;; Ver 1.1 2003-04-06 David Andersson
;;    Zero arg describe changes. Negative arg go back.
;;    Autoload. Remove message using nil in stead of an empty string.
;; Ver 1.0 2002-05-18 David Andersson
;;    Initial version
;;
;;--------------------------------------------------------------------
;;
;;todo: Rename "goto-chg.el" -> "gotochange.el" or "goto-chgs" ?
;;todo: Rename function goto-last-change -> goto-last-edit ?
;;todo: Rename adjective "-last-" -> "-latest-" or "-most-recent-" ?
;;todo: There are some, maybe useful, funcs  for region undo
;;       in simple.el in emacs 20. Take a look.
;;todo: Add functionality to visit changed point in text order, not only in
;;        chronological order. (Naa, highlight-changes-mode does that).
;;todo: Inverse indication that a change has been saved or not
;;todo: Highlight the range of text involved in the last change?
;;todo: See session-jump-to-last-change in session.el?
;;todo: Unhide invisible text (e.g. outline mode) like isearch do.
;;todo: XEmacs sets last-command to `t' after an error, so you cannot reverse
;;        after "No furter change info". Should we bother?
;;todo: Try distinguish "No further change info" (end of truncated undo list)
;;        and "No further changes" (end of a complete undo list).
;;
;;--------------------------------------------------------------------

;;; Code:

(defvar glc-default-span 8 "*goto-last-change don't visit the same point twice. glc-default-span tells how far around a visited point not to visit again.")
(defvar glc-current-span 8 "Internal for goto-last-change.\nA copy of glc-default-span or the ARG passed to goto-last-change.")
(defvar glc-probe-depth 0 "Internal for goto-last-change.\nIt is non-zero between successive goto-last-change.")

;;todo: Find begin and end of line, then use it somewhere

(defun glc-center-ellipsis (str maxlen &optional ellipsis)
  "Truncate STRING in the middle to length MAXLEN.
If STRING is max MAXLEN just return the string.
Optional third argument is the replacement, which defaults to \"...\"."
  (if (<= (length str) maxlen)
      str
    ;; else
    (let* ((lipsis (or ellipsis "..."))
           (i (/ (- maxlen (length lipsis)) 2)))
      (concat (substring str 0 i)
              lipsis
              (substring str (- i))))))

(defun glc-adjust-pos2 (pos p1 p2 adj)
  ;; Helper function to glc-adjust-pos
  ;; p1, p2: interval where an edit occured
  ;; adj: amount of text added (positive) or removed (negativ) by the edit
  ;; Return pos if well before p1, or pos+adj if well after p2, or nil if too close
  (cond ((<= pos (- p1 glc-current-span))
         pos)
        ((> pos (+ p2 glc-current-span))
         (+ pos adj))
        ((zerop glc-current-span)
         p1)
        (t
         nil)))

(defun glc-adjust-pos (pos e)
  "Given POS, a buffer position before the edit E, compute and return
the \"same\" buffer position after E happened.
Exception: return nil if POS is closer than `glc-current-span' to the edit E.
\nInsertion edits before POS returns a larger value.
Deletion edits before POS returns a smaller value.
\nThe edit E is an entry from the `buffer-undo-list'. See for details."
  (cond ((atom e)                       ; nil==cmd boundary, or, num==changed pos
         pos)
        ((numberp (car e))              ; (beg . end)==insertion
         (glc-adjust-pos2 pos (car e) (car e) (- (cdr e) (car e))))
        ((stringp (car e))              ; (string . pos)==deletion
         (glc-adjust-pos2 pos (abs (cdr e)) (+ (abs (cdr e)) (length (car e))) (- (length (car e)))))
        ((null (car e))                 ; (nil prop val beg . end)==prop change
         (glc-adjust-pos2 pos (nth 3 e) (nthcdr 4 e) 0))
        (t                              ; (marker . dist)==marker moved
         pos)))

;; If recursive in stead of iterative (while), it tends to fill the call stack.
;; (Isn't it tail optimized?)
(defun glc-adjust-list (r)
  "R is list of edit entries in chronological order.
Pick the point of the first edit entry and update that point with
the second, third, etc, edit entries. Return the final updated point,
or nil if the point was closer than `glc-current-span' to some edit in R.
\nR is basically a reversed slice from the buffer-undo-list."
  (if r
      ;; Get pos
      (let ((pos (glc-get-pos (car r))))
        (setq r (cdr r))
        ;; Walk back in reverse list
        (while (and r pos)
          (setq pos (glc-adjust-pos pos (car r))
                r (cdr r)))
        pos)
    ;; else
    nil))

(defun glc-get-pos (e)
  "If E represents an edit, return a position value in E, the position
where the edit took place. Return nil if E represents no real change.
\nE is an entry in the buffer-undo-list."
  (cond ((numberp e) e)                 ; num==changed position
        ((atom e) nil)                  ; nil==command boundary
        ((numberp (car e)) (cdr e))     ; (beg . end)==insertion
        ((stringp (car e)) (abs (cdr e))) ; (string . pos)==deletion
        ((null (car e)) (nthcdr 4 e))   ; (nil ...)==text property change
        ((atom (car e)) nil)            ; (t ...)==file modification time
        (t nil)))                       ; (marker ...)==marker moved

(defun glc-get-descript (e &optional n)
  "If E represents an edit, return a short string describing E.
Return nil if E represents no real change.
\nE is an entry in the buffer-undo-list."
  (let ((nn (or (format "T-%d: " n) "")))
    (cond ((numberp e) "New position")  ; num==changed position
          ((atom e) nil)                ; nil==command boundary
          ((numberp (car e))            ; (beg . end)==insertion
           (if (and n (< n 2))
               (format "%sInserted %d chars \"%s\"" nn (- (cdr e) (car e))
                       (glc-center-ellipsis (buffer-substring (car e) (cdr e)) 60))
             ;; else
             ;; An older insert. The inserted text cannot easily be computed.
             ;; Just show the char count.
             (format "%sInserted %d chars" nn (- (cdr e) (car e)))))
          ((stringp (car e))            ; (string . pos)==deletion
           (format "%sDeleted \"%s\"" nn (glc-center-ellipsis (car e) 60)))
          ((null (car e))               ; (nil ...)==text property change
           (format "%sProperty change" nn))
          ((atom (car e)) nil)          ; (t ...)==file modification time
          (t nil))))                    ; (marker ...)==marker moved

(defun glc-is-positionable (e)
  "Return non-nil if E is an insertion, deletion or text property change.
\nE is an entry in the buffer-undo-list."
  (and (not (numberp e)) (glc-get-pos e)))

(defun glc-is-filetime (e)
  "Return t if E indicates a buffer became \"modified\",
that is, it was previously saved or unchanged. Nil otherwise."
  (and (listp e) (eq (car e) t)))

;;;###autoload
(defun goto-last-change (arg)
"Go to the point where the last edit was made in the current buffer.
Repeat the command to go to the second last edit, etc.
\nTo go back to more recent edit, the reverse of this command, use \\[goto-last-change-reverse]
or precede this command with \\[universal-argument] - (minus).
\nIt does not go to the same point twice even if there has been many edits
there. I call the minimal distance between distinguishable edits \"span\".
Set variable `glc-default-span' to control how close is \"the same point\".
Default span is 8.
The span can be changed temporarily with \\[universal-argument] right before \\[goto-last-change]:
\\[universal-argument] <NUMBER> set current span to that number,
\\[universal-argument] (no number) multiplies span by 4, starting with default.
The so set span remains until it is changed again with \\[universal-argument], or the consecutive
repetition of this command is ended by any other command.
\nWhen span is zero (i.e. \\[universal-argument] 0) subsequent \\[goto-last-change] visits each and
every point of edit and a message shows what change was made there.
In this case it may go to the same point twice.
\nThis command uses undo information. If undo is disabled, so is this command.
At times, when undo information becomes too large, the oldest information is
discarded. See variable `undo-limit'."
  (interactive "P")
  (cond ((not (eq this-command last-command))
         ;; Start a glc sequence
         ;; Don't go to current point if last command was an obvious edit
         ;; (yank or self-insert, but not kill-region). Makes it easier to
         ;; jump back and forth when copying seleced lines.
         (setq glc-probe-depth (if (memq last-command '(yank self-insert-command)) 1 0)
               glc-direction 1
               glc-current-span glc-default-span)
         (if (< (prefix-numeric-value arg) 0)
             (error "Negative arg: Cannot reverse as the first operation"))))
  (cond ((null buffer-undo-list)
         (error "Buffer has not been changed"))
        ((eq buffer-undo-list t)
         (error "No change info (undo is disabled)")))
  (cond ((numberp arg)                  ; Numeric arg sets span
         (setq glc-current-span (abs arg)))
        ((consp arg)                    ; C-u's multiply previous span by 4
         (setq glc-current-span (* (abs (car arg)) glc-default-span))
         (message "Current span is %d chars" glc-current-span))) ;todo: keep message with "waiting" and "is saved"
  (cond ((< (prefix-numeric-value arg) 0)
         (setq glc-direction -1))
        (t
         (setq glc-direction 1)))
  (let (rev                             ; Reversed (and filtered) undo list
        pos                             ; The pos we look for, nil until found
        (n 0)                           ; Steps in undo list (length of 'rev')
        (l buffer-undo-list)
        (passed-save-entry (not (buffer-modified-p)))
        (new-probe-depth glc-probe-depth)
        (undo-tree-p (bound-and-true-p undo-tree-mode))
        glc-seen-canary)
    ;; Walk back and forth in the buffer-undo-list, each time one step deeper,
    ;; until we can walk back the whole list with a 'pos' that is not coming
    ;; too close to another edit.
    (while (null pos)
      (setq new-probe-depth (+ new-probe-depth glc-direction))
      (if (< glc-direction 0)
          (setq rev ()
                n 0
                l buffer-undo-list
                passed-save-entry (not (buffer-modified-p))))
      (if (< new-probe-depth 1)
          (error "No later change info"))
      (if (> n 150)
          (message "working..."))
      ;; Walk forward in buffer-undo-list, glc-probe-depth steps.
      ;; Build reverse list along the way
      (if (not undo-tree-p)
          (while (< n new-probe-depth)
            (cond ((null l)
                   ;(setq this-command t)   ; Disrupt repeat sequence
                   (error "No further change info"))
                  ((glc-is-positionable (car l))
                   (setq n (1+ n)
                         rev (cons (car l) rev)))
                  ((or passed-save-entry (glc-is-filetime (car l)))
                   (setq passed-save-entry t)))
            (setq l (cdr l)))
        (when (not glc-seen-canary)
          (while (and (not (null l)) (not glc-seen-canary) (< n new-probe-depth))
            (cond ((eq 'undo-tree-canary (car l))  ; used by buffer-undo-tree
                   (message "Canary found...")
                   (setq l (undo-tree-current buffer-undo-tree)
                         glc-seen-canary t))
                  ((glc-is-positionable (car l))
                   (setq n (1+ n)
                         rev (cons (car l) rev)))
                  ((or passed-save-entry (glc-is-filetime (car l)))
                   (setq passed-save-entry t)))
            (when (not glc-seen-canary)
              (setq l (cdr l)))))
        (when glc-seen-canary
          (while (< n new-probe-depth)
            (cond ((null l)
                   ;(setq this-command t)	; Disrupt repeat sequence
                   (error "No further change info"))
                  ((glc-is-positionable (car (undo-tree-node-undo l)))
                   (setq n (1+ n)
                         rev (cons (car (undo-tree-node-undo l)) rev)))
                  ((or passed-save-entry (glc-is-filetime (car (undo-tree-node-undo l))))
                   (setq passed-save-entry t)))
            (setq l (undo-tree-node-previous l))))
        (when (null l)
          (error "No further change info")))
      ;; Walk back in reverse list, from older to newer edits.
      ;; Adjusting pos along the way.
      (setq pos (glc-adjust-list rev)))
    ;; Found a place not previously visited, in 'pos'.
    ;; (An error have been issued if nothing (more) found.)
    (if (> n 150)
        (message nil))                  ; remove message "working..."
    (if (and (= glc-current-span 0) (glc-get-descript (car rev) n))
        (message "%s" (glc-get-descript (car rev) n))
      ;; else
      (if passed-save-entry
          (message "(This change is saved)")))
    (setq glc-probe-depth new-probe-depth)
    (goto-char pos)))

;;;###autoload
(defun goto-last-change-reverse (arg)
  "Go back to more recent changes after \\[goto-last-change] have been used.
See `goto-last-change' for use of prefix argument."
  (interactive "P")
  ;; Negate arg, all kinds
  (cond ((eq arg nil)  (setq arg '-))
        ((eq arg '-)   (setq arg nil))
        ((listp arg)   (setq arg (list (- (car arg)))))
        (t (setq arg   (- arg))))
  ;; Make 'goto-last-change-reverse' look like 'goto-last-change'
  (cond ((eq last-command this-command)
         (setq last-command 'goto-last-change)))
  (setq this-command 'goto-last-change)
  ;; Call 'goto-last-change' to do the job
  (goto-last-change arg))

(provide 'goto-chg)

;;; goto-chg.el ends here
