;;;; Window and scrolling commands

(require 'evil-common)
(require 'evil-core)
(require 'evil-motions)

(condition-case nil
    (require 'windmove)
  (error
   (message "evil: Could not load 'windmove', window-commands not available.")
   nil))

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
  (declare (indent defun)
           (debug t))
  `(let ((ocolumn (current-column)))
     ,@body
     (move-to-column ocolumn)))

;;; Scrolling

(evil-define-command evil-scroll-line-up (count)
  "Scrolls the window COUNT lines upwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (scroll-down count))

(evil-define-command evil-scroll-line-down (count)
  "Scrolls the window COUNT lines downwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (scroll-up count))

(evil-define-command evil-scroll-up (count)
  "Scrolls the window and the cursor COUNT lines upwards, default half of the screen."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (evil-save-column
    (let ((p (point))
          (c (or count (/ (evil-num-visible-lines) 2))))
      (save-excursion
        (scroll-down (min (evil-max-scroll-up) c)))
      (forward-line (- c))
      (when (= (line-number-at-pos p)
               (line-number-at-pos (point)))
        (signal 'beginning-of-buffer nil)))))

(evil-define-command evil-scroll-down (count)
  "Scrolls the window and the cursor COUNT lines downwards, default half of the screen."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (evil-save-column
    (let ((p (point))
          (c (or count (/ (evil-num-visible-lines) 2))))
      (save-excursion
        (scroll-up (min (evil-max-scroll-down) c)))
      (forward-line c)
      (when (= (line-number-at-pos p)
               (line-number-at-pos (point)))
        (signal 'end-of-buffer nil)))))

(evil-define-command evil-scroll-page-up (count)
  "Scrolls the window COUNT pages upwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (evil-save-column
    (dotimes (i count)
      (scroll-down nil))))

(evil-define-command evil-scroll-page-down (count)
  "Scrolls the window COUNT pages upwards."
  :repeat nil
  :keep-visual t
  (interactive "p")
  (evil-save-column
    (dotimes (i count)
      (scroll-up nil))))

(evil-define-command evil-scroll-line-to-top (count)
  "Scrolls line number COUNT (or the cursor line) to the top of the window."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (evil-save-column
    (let ((line (or count (line-number-at-pos (point)))))
      (goto-char (point-min))
      (forward-line (1- line)))
    (recenter 0)))

(evil-define-command evil-scroll-line-to-center (count)
  "Scrolls line number COUNT (or the cursor line) to the center of the window."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (evil-save-column
    (let ((line (or count (line-number-at-pos (point)))))
      (goto-char (point-min))
      (forward-line (1- line)))
    (recenter nil)))

(evil-define-command evil-scroll-line-to-bottom (count)
  "Scrolls line number COUNT (or the cursor line) to the bottom of the window."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (evil-save-column
    (let ((line (or count (line-number-at-pos (point)))))
      (goto-char (point-min))
      (forward-line (1- line)))
    (recenter -1)))

(evil-define-command evil-scroll-bottom-line-to-top (count)
  "Scrolls the line right below the window or line COUNT to the top of the window."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (if count
      (progn
        (goto-char (point-min))
        (forward-line (1- count)))
    (goto-char (window-end))
    (evil-adjust))
  (recenter 0)
  (evil-first-non-blank))

(evil-define-command evil-scroll-top-line-to-bottom (count)
  "Scrolls the line right below the window or line COUNT to the top of the window."
  :repeat nil
  :keep-visual t
  (interactive "P")
  (if count
      (progn
        (goto-char (point-min))
        (forward-line (1- count)))
    (goto-char (window-start)))
  (recenter -1)
  (evil-first-non-blank))

;;; Window

(defun evil-resize-window (new-size &optional horizontal)
  "Sets the current window's with or height to `new-size'."
  (let ((wincfg (current-window-configuration))
        (nwins (length (window-list)))
        (count (if horizontal
                   (- new-size (window-width))
                 (- new-size (window-height)))))
    (catch 'done
      (save-window-excursion
        (while (not (zerop count))
          (if (> count 0)
              (progn
                (enlarge-window 1 horizontal)
                (setq count (1- count)))
            (progn
              (shrink-window 1 horizontal)
              (setq count (1+ count))))
          (if (= nwins (length (window-list)))
              (setq wincfg (current-window-configuration))
            (throw 'done t)))))
    (set-window-configuration wincfg)))

(defun evil-get-buffer-tree (wintree)
  "Extracts the buffer tree from a given window-tree."
  (if (consp wintree)
      (cons (car wintree) (mapcar #'evil-get-buffer-tree (cddr wintree)))
    (window-buffer wintree)))

(defun evil-restore-window-tree (win tree)
  "Restores the given buffer-tree layout as subwindows of win."
  (cond
   ((and (consp tree) (cddr tree))
    (let ((newwin (split-window win nil (not (car tree)))))
      (evil-restore-window-tree win (cadr tree))
      (evil-restore-window-tree newwin (cons (car tree) (cddr tree)))))
   ((consp tree)
    (set-window-buffer win (cadr tree)))
   (t
    (set-window-buffer win tree))))

(evil-define-command evil-window-split (&optional count file)
  "Splits the current window horizontally, COUNT lines height, editing a certain `file'."
  :repeat nil
  (interactive "P<f>")
  (let ((new-win (split-window (selected-window) count)))
    (when file
      (evil-edit file))))

(evil-define-command evil-window-vsplit (&optional count file)
  "Splits the current window vertically, COUNT columns width, editing a certain `file'."
  :repeat nil
  (interactive "P<f>")
  (let ((new-win (split-window (selected-window) count t)))
    (when file
      (evil-edit file))))

(evil-define-command evil-split-buffer (buffer)
  "Splits window and switches to another buffer."
  :repeat nil
  (interactive "<b>")
  (evil-window-split)
  (evil-buffer buffer))

(evil-define-command evil-split-next-buffer (&optional count)
  "Splits window and goes to the `count'-th next buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (evil-window-split)
  (evil-next-buffer count))

(evil-define-command evil-split-prev-buffer (&optional count)
  "Splits window and goes to the `count'-th prev buffer in the buffer list."
  :repeat nil
  (interactive "p")
  (evil-window-split)
  (evil-prev-buffer count))

(evil-define-command evil-window-left (count)
  "Move the cursor to new COUNT-th window left of the current one."
  :repeat nil
  (interactive "p")
  (dotimes (i count)
    (windmove-left)))

(evil-define-command evil-window-right (count)
  "Move the cursor to new COUNT-th window right of the current one."
  :repeat nil
  (interactive "p")
  (dotimes (i count)
    (windmove-right)))

(evil-define-command evil-window-up (count)
  "Move the cursor to new COUNT-th window above the current one."
  :repeat nil
  (interactive "p")
  (dotimes (i (or count 1))
    (windmove-up)))

(evil-define-command evil-window-down (count)
  "Move the cursor to new COUNT-th window below the current one."
  :repeat nil
  (interactive "p")
  (dotimes (i (or count 1))
    (windmove-down)))

(evil-define-command evil-window-bottom-right ()
  "Move the cursor to bottom-right window."
  :repeat nil
  (interactive)
  (while (let (success)
           (condition-case nil
               (progn
                 (windmove-right)
                 (setq success t))
             (error nil))
           (condition-case nil
               (progn
                 (windmove-down)
                 (setq success t))
             (error nil))
           success)))

(evil-define-command evil-window-top-left ()
  "Move the cursor to top-left window."
  :repeat nil
  (interactive)
  (while (let (success)
           (condition-case nil
               (progn
                 (windmove-left)
                 (setq success t))
             (error nil))
           (condition-case nil
               (progn
                 (windmove-up)
                 (setq success t))
             (error nil))
           success)))

(evil-define-command evil-window-lru ()
  "Move the cursor to the previous (last accessed) window."
  :repeat nil
  (interactive)
  (select-window (get-lru-window)))

(evil-define-command evil-window-next (count)
  "Move the cursor to the next window in the cyclic order.
With COUNT go to the count-th window in the order starting from
top-left."
  :repeat nil
  (interactive "P")
  (if (not count)
      (select-window (next-window))
    (evil-window-top-left)
    (other-window (1- count))))

(evil-define-command evil-window-prev (count)
  "Move the cursor to the previous window in the cyclic order.
With COUNT go to the count-th window in the order starting from
top-left."
  :repeat nil
  (interactive "P")
  (if (not count)
      (select-window (previous-window))
    (evil-window-top-left)
    (other-window (1- count))))

(evil-define-command evil-window-new (count file)
  "Splits the current window horizontally and opens a new buffer or edits a certain `file'."
  :repeat nil
  (interactive "P<f>")
  (split-window (selected-window) count)
  (if file
      (evil-edit file)
    (let ((buffer (generate-new-buffer "*new*")))
      (set-window-buffer (selected-window) buffer)
      (with-current-buffer buffer
        (evil-normal-state)))))

(evil-define-command evil-window-vnew (count file)
  "Splits the current window vertically and opens a new buffer name or edits a certain `file'."
  :repeat nil
  (interactive "P<f>")
  (split-window (selected-window) count t)
  (if file
      (evil-edit file)
    (let ((buffer (generate-new-buffer "*new*")))
      (set-window-buffer (selected-window) buffer)
      (with-current-buffer buffer
        (evil-normal-state)))))

(evil-define-command evil-window-increase-height (count)
  "Increase current window height by COUNT."
  :repeat nil
  (interactive "p")
  (evil-resize-window (+ (window-height) count)))

(evil-define-command evil-window-decrease-height (count)
  "Decrease current window height by COUNT."
  :repeat nil
  (interactive "p")
  (evil-resize-window (- (window-height) count)))

(evil-define-command evil-window-increase-width (count)
  "Increase current window width by COUNT."
  :repeat nil
  (interactive "p")
  (evil-resize-window (+ (window-width) count) t))

(evil-define-command evil-window-decrease-width (count)
  "Decrease current window width by COUNT."
  :repeat nil
  (interactive "p")
  (evil-resize-window (- (window-width) count) t))

(evil-define-command evil-window-set-height (count)
  "Sets the height of the current window to COUNT."
  :repeat nil
  (interactive "P")
  (evil-resize-window (or count (frame-height)) nil))

(evil-define-command evil-window-set-width (count)
  "Sets the width of the current window to COUNT."
  :repeat nil
  (interactive "P")
  (evil-resize-window (or count (frame-width)) t))

(evil-define-command evil-window-rotate-upwards ()
  "Rotates the windows according to the currenty cyclic ordering."
  :repeat nil
  (interactive)
  (let ((wlist (window-list))
        (blist (mapcar #'(lambda (w) (window-buffer w))
                       (window-list))))
    (setq blist (append (cdr blist) (list (car blist))))
    (while (and wlist blist)
      (set-window-buffer (car wlist) (car blist))
      (setq wlist (cdr wlist)
            blist (cdr blist)))
    (select-window (car (last (window-list))))))

(evil-define-command evil-window-rotate-downwards ()
  "Rotates the windows according to the currenty cyclic ordering."
  :repeat nil
  (interactive)
  (let ((wlist (window-list))
        (blist (mapcar #'(lambda (w) (window-buffer w))
                       (window-list))))
    (setq blist (append (last blist) blist))
    (while (and wlist blist)
      (set-window-buffer (car wlist) (car blist))
      (setq wlist (cdr wlist)
            blist (cdr blist)))
    (select-window (cadr (window-list)))))

(evil-define-command evil-window-move-very-top ()
  "Closes the current window, splits the upper-left one horizontally
and redisplays the current buffer there."
  :repeat nil
  (interactive)
  (unless (one-window-p)
    (let ((b (current-buffer)))
      (delete-window)
      (let ((btree (evil-get-buffer-tree (car (window-tree)))))
        (delete-other-windows)
        (let ((newwin (selected-window))
              (subwin (split-window)))
          (evil-restore-window-tree subwin btree)
          (set-window-buffer newwin b)
          (select-window newwin))))
    (balance-windows)))

(evil-define-command evil-window-move-far-left ()
  "Closes the current window, splits the upper-left one vertically
and redisplays the current buffer there."
  :repeat nil
  (interactive)
  (unless (one-window-p)
    (let ((b (current-buffer)))
      (delete-window)
      (let ((btree (evil-get-buffer-tree (car (window-tree)))))
        (delete-other-windows)
        (let ((newwin (selected-window))
              (subwin (split-window-horizontally)))
          (evil-restore-window-tree subwin btree)
          (set-window-buffer newwin b)
          (select-window newwin))))
    (balance-windows)))

(evil-define-command evil-window-move-far-right ()
  "Closes the current window, splits the lower-right one vertically
and redisplays the current buffer there."
  :repeat nil
  (interactive)
  (unless (one-window-p)
    (let ((b (current-buffer)))
      (delete-window)
      (let ((btree (evil-get-buffer-tree (car (window-tree)))))
        (delete-other-windows)
        (let ((subwin (selected-window))
              (newwin (split-window-horizontally)))
          (evil-restore-window-tree subwin btree)
          (set-window-buffer newwin b)
          (select-window newwin))))
    (balance-windows)))

(evil-define-command evil-window-move-very-bottom ()
  "Closes the current window, splits the lower-right one horizontally
and redisplays the current buffer there."
  :repeat nil
  (interactive)
  (unless (one-window-p)
    (let ((b (current-buffer)))
      (delete-window)
      (let ((btree (evil-get-buffer-tree (car (window-tree)))))
        (delete-other-windows)
        (let ((subwin (selected-window))
              (newwin (split-window)))
          (evil-restore-window-tree subwin btree)
          (set-window-buffer newwin b)
          (select-window newwin))))
    (balance-windows)))

(provide 'evil-window)
