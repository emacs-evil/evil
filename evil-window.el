;;;; Window and scrolling commands

;; TODO: non-repeatable and keep-visual

(require 'evil-motions)

;;; Utility function
(defun evil-num-visible-lines ()
  "Returns the number of currently visible lines."
  (- (window-height) 1))

(defun evil-max-scroll-up ()
  "Returns the maximal number of lines that can be scrolled up."
  (1- (line-number-at-pos (window-start))))

(defun evil-max-scroll-down ()
  "Returns the maximal number of lines that can be scrolled down."
  (if (pos-visible-in-window-p (window-end))
      0
      (1+ (- (line-number-at-pos (point-max))
             (line-number-at-pos (window-end))))))

(defmacro evil-save-column (&rest body)
  "Restores the column after execution of BODY."
  (declare (indent defun))
  `(let ((ocolumn (current-column)))
     ,@body
     (move-to-column ocolumn)))

;;; Scrolling
;; TODO: in vim, scroll commands preserve the current column but the
;; emacs version do not

(defun evil-scroll-line-up (count)
  "Scrolls the window `count' lines upwards."
  ;; :repeatable nil
  ;; :keep-visual t
  (interactive "p")
  (scroll-down count))

(defun evil-scroll-line-down (count)
  "Scrolls the window `count' lines downwards."
  ;; :repeatable nil
  ;; :keep-visual t
  (interactive "p")
  (scroll-up count))

(defun evil-scroll-up (count)
  "Scrolls the window and the cursor `count' lines upwards, default half of the screen."
  ;; :repeatable nil
  ;; :keep-visual t
  (interactive "P")
  (let ((p (point))
        (c (or count (/ (evil-num-visible-lines) 2))))
    (save-excursion
      (scroll-down (min (evil-max-scroll-up) c)))
    (forward-line (- c))
    (when (= (line-number-at-pos p)
             (line-number-at-pos (point)))
      (signal 'beginning-of-buffer nil))))

(defun evil-scroll-down (count)
  "Scrolls the window and the cursor `count' lines downwards, default half of the screen."
  ;; :repeatable nil
  ;; :keep-visual t
  (interactive "P")
  (let ((p (point))
        (c (or count (/ (evil-num-visible-lines) 2))))
    (save-excursion
      (scroll-up (min (evil-max-scroll-down) c)))
    (forward-line c)
    (when (= (line-number-at-pos p)
             (line-number-at-pos (point)))
      (signal 'end-of-buffer nil))))

(defun evil-scroll-page-up (count)
  "Scrolls the window `count' pages upwards."
  ;; :repeatable nil
  ;; :keep-visual t
  (interactive "p")
  (condition-case nil
      (dotimes (i count)
        (scroll-down nil))
    (error
     (if (bobp)
         (signal 'beginning-of-buffer nil)
       (goto-char (point-min))))))

(defun evil-scroll-page-down (count)
  "Scrolls the window `count' pages upwards."
  ;; :repeatable nil
  ;; :keep-visual t
  (interactive "p")
  (condition-case nil
      (dotimes (i count)
        (scroll-up nil))
    (error
     (if (evil-eobp)
         (signal 'end-of-buffer nil)
       (goto-char (point-max))))))

(defun evil-scroll-line-to-top (count)
  "Scrolls line number `count' (or the cursor line) to the top of the window."
  ;; :repeatable nil
  ;; :keep-visual t
  (interactive "P")
  (evil-save-column
    (goto-line (or count (line-number-at-pos (point))))
    (recenter 0)))

(defun evil-scroll-line-to-center (count)
  "Scrolls line number `count' (or the cursor line) to the center of the window."
  ;; :repeatable nil
  ;; :keep-visual t
  (interactive "P")
  (evil-save-column
    (goto-line (or count (line-number-at-pos (point))))
    (recenter nil)))

(defun evil-scroll-line-to-bottom (count)
  "Scrolls line number `count' (or the cursor line) to the bottom of the window."
  ;; :repeatable nil
  ;; :keep-visual t
  (interactive "P")
  (evil-save-column
    (goto-line (or count (line-number-at-pos (point))))
    (recenter -1)))

(defun evil-scroll-bottom-line-to-top (count)
  "Scrolls the line right below the window or line `count' to the top of the window."
  ;; :repeatable nil
  ;; :keep-visual t
  (interactive "P")
  (if count
      (goto-line count)
    (goto-char (window-end))
    (unless (bobp) (backward-char)))
  (recenter 0)
  (evil-first-non-blank))

(defun evil-scroll-top-line-to-bottom (count)
  "Scrolls the line right below the window or line `count' to the top of the window."
  ;; :repeatable nil
  ;; :keep-visual t
  (interactive "P")
  (if count
      (goto-line count)
    (goto-char (window-start)))
  (recenter -1)
  (evil-first-non-blank))

(provide 'evil-window)
