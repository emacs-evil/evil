;;;; Settings and variables

;;; Setters

(defun evil-set-toggle-key (key)
  "Set `evil-toggle-key' to KEY.
KEY must be readable by `read-kbd-macro'."
  (let ((old-key (read-kbd-macro
                  (if (boundp 'evil-toggle-key)
                      evil-toggle-key
                    "C-z")))
        (key (read-kbd-macro key)))
    (with-no-warnings
      (dolist (pair '((evil-motion-state-map evil-emacs-state)
                      (evil-insert-state-map evil-emacs-state)
                      (evil-emacs-state-map evil-exit-emacs-state)))
        (when (boundp (car pair))
          (let ((map (symbol-value (car pair)))
                (fun (cadr pair)))
            (when (keymapp map)
              (define-key map key fun)
              (define-key map old-key nil))))))))

;;; Customization group

(defgroup evil nil
  "Extensible vi layer."
  :group 'emulations
  :prefix 'evil-)

(defcustom evil-auto-indent t
  "Whether to auto-indent when entering Insert state."
  :type  'boolean
  :group 'evil)
(make-variable-buffer-local 'evil-auto-indent)

(defcustom evil-shift-width 4
  "The offset used by \\<evil-normal-state-map>\\[evil-shift-right] \
and \\[evil-shift-left]."
  :type 'integer
  :group 'evil)
(make-variable-buffer-local 'evil-shift-width)

(defcustom evil-shift-round t
  "Whether \\<evil-normal-state-map>\\[evil-shift-right] \
and \\[evil-shift-left] round to the nearest multiple \
of `evil-shift-width'."
  :type 'boolean
  :group 'evil)
(make-variable-buffer-local 'evil-shift-round)

(defcustom evil-default-cursor
  (list (or (frame-parameter nil 'cursor-color) "black") t)
  "The default cursor.
May be a cursor type as per `cursor-type', a color string as passed
to `set-cursor-color', a zero-argument function for changing the
cursor, or a list of the above."
  :type '(set symbol (cons symbol symbol) string function)
  :group 'evil)

(defcustom evil-repeat-move-cursor t
  "Whether \"\\<evil-normal-state-map>\\[evil-repeat]\" \
moves the cursor."
  :type 'boolean
  :group 'evil)

(defcustom evil-cross-lines nil
  "Whether motions may cross newlines."
  :type 'boolean
  :group 'evil)

(defcustom evil-move-cursor-back t
  "Whether the cursor is moved backwards when exiting Insert state."
  :type 'boolean
  :group 'evil)

(defcustom evil-mode-line-format 'before
  "The position of the mode line tag.
`before' means before the mode list, `after' means after it,
and nil means no mode line tag."
  :type 'symbol
  :group 'evil)

(defcustom evil-word "[:word:]_"
  "The characters to be considered as a word.
This should be a regexp set without the enclosing []."
  :type 'string
  :group 'evil)
(make-variable-buffer-local 'evil-word)

(defcustom evil-want-fine-undo nil
  "Whether actions like \"cw\" are undone in several steps."
  :type 'boolean
  :group 'evil)

(defcustom evil-regexp-search t
  "Whether to use regular expressions for searching."
  :type  'boolean
  :group 'evil)

(defcustom evil-search-wrap t
  "Whether search wraps around."
  :type  'boolean
  :group 'evil)

(defcustom evil-flash-delay 2
  "Time in seconds to flash search matches."
  :type  'number
  :group 'evil)

(defcustom evil-fold-level 0
  "Default fold level."
  :type  'integer
  :group 'evil)

(defcustom evil-esc-delay 0.01
  "Time in seconds to wait for another key after ESC."
  :type 'number
  :group 'evil)

(defcustom evil-show-paren-range 0
  "The minimal distance between point and a parenthesis
which causes the parenthesis to be highlighted."
  :type 'integer
  :group 'evil)

(defcustom evil-highlight-closing-paren-at-point-states
  '(not emacs insert replace)
  "The states in which the closing parenthesis at point should be highlighted.
All states listed here highlight the closing parenthesis at
point (which is Vim default behavior), all others highlight the
parenthesis before point (which is Emacs default behavior). If
this list contains the symbol 'not then its meaning is inverted,
i.e., all states listed here highlight the closing parenthesis
before point."
  :type '(repeat symbol)
  :group 'evil)

(defcustom evil-want-C-i-jump t
  "Whether \"C-i\" jumps forward like in Vim."
  :type 'boolean
  :group 'evil)

(defcustom evil-want-C-u-scroll nil
  "Whether \"C-u\" scrolls like in Vim."
  :type 'boolean
  :group 'evil)

(defcustom evil-want-C-w-delete t
  "Whether \"C-w\" deletes a word in Insert state."
  :type 'boolean
  :group 'evil)

(defcustom evil-want-C-w-in-emacs-state nil
  "Whether \"C-w\" prefixes windows commands in Emacs state."
  :type 'boolean
  :group 'evil)

(defcustom evil-want-change-word-to-end t
  "Whether \"cw\" behaves like \"ce\"."
  :type 'boolean
  :group 'evil)

(defcustom evil-echo-state t
  "Whether to signal the current state in the echo area."
  :type 'boolean
  :group 'evil)

(defcustom evil-complete-all-buffers t
  "Whether completion looks for matches in all buffers."
  :type 'boolean
  :group 'evil)

(defcustom evil-complete-next-func
  #'(lambda (arg)
      (require 'dabbrev)
      (let ((dabbrev-search-these-buffers-only
             (unless evil-complete-all-buffers
               (list (current-buffer))))
            dabbrev-case-distinction)
        (condition-case nil
            (if (eq last-command this-command)
                (dabbrev-expand nil)
              (dabbrev-expand (- (abs (or arg 1)))))
          (error (dabbrev-expand nil)))))
  "Completion function used by \
\\<evil-insert-state-map>\\[evil-complete-next]."
  :type 'function
  :group 'evil)

(defcustom evil-complete-previous-func
  #'(lambda (arg)
      (require 'dabbrev)
      (let ((dabbrev-search-these-buffers-only
             (unless evil-complete-all-buffers
               (list (current-buffer))))
            dabbrev-case-distinction)
        (dabbrev-expand arg)))
  "Completion function used by \
\\<evil-insert-state-map>\\[evil-complete-previous]."
  :type 'function
  :group 'evil)

(defcustom evil-complete-next-minibuffer-func 'minibuffer-complete
  "Minibuffer completion function used by \
\\<evil-insert-state-map>\\[evil-complete-next]."
  :type 'function
  :group 'evil)

(defcustom evil-complete-previous-minibuffer-func 'minibuffer-complete
  "Minibuffer completion function used by \
\\<evil-insert-state-map>\\[evil-complete-previous]."
  :type 'function
  :group 'evil)

(defcustom evil-complete-next-line-func
  #'(lambda (arg)
      (let ((hippie-expand-try-functions-list
             '(try-expand-line
               try-expand-line-all-buffers)))
        (hippie-expand arg)))
  "Minibuffer completion function used by \
\\<evil-insert-state-map>\\[evil-complete-next-line]."
  :type 'function
  :group 'evil)

(defcustom evil-complete-previous-line-func
  evil-complete-next-line-func
  "Minibuffer completion function used by \
\\<evil-insert-state-map>\\[evil-complete-previous-line]."
  :type 'function
  :group 'evil)

(defcustom evil-lookup-func 'woman
  "Lookup function used by \
\"\\<evil-motion-state-map>\\[evil-lookup]\"."
  :type 'function
  :group 'evil)

(defcustom evil-toggle-key "C-z"
  "The key used to change to and from Emacs state.
Must be readable by `read-kbd-macro'. For example: \"C-z\"."
  :type 'string
  :group 'evil
  :set #'(lambda (sym value)
           (evil-set-toggle-key value)
           (set-default sym value)))

(defcustom evil-default-state 'normal
  "The default state.
This is the state a mode comes up in when it is not listed
in `evil-emacs-state-modes', `evil-insert-state-modes' or
`evil-motion-state-modes'. The value may be one of `normal',
`insert', `visual', `replace', `operator', `motion' and
`emacs'."
  :type  'symbol
  :group 'evil)

(defcustom evil-emacs-state-modes
  '(archive-mode
    bbdb-mode
    bookmark-bmenu-mode
    bookmark-edit-annotation-mode
    browse-kill-ring-mode
    bzr-annotate-mode
    calc-mode
    cfw:calendar-mode
    completion-list-mode
    Custom-mode
    debugger-mode
    delicious-search-mode
    desktop-menu-blist-mode
    desktop-menu-mode
    doc-view-mode
    dvc-bookmarks-mode
    dvc-diff-mode
    dvc-info-buffer-mode
    dvc-log-buffer-mode
    dvc-revlist-mode
    dvc-revlog-mode
    dvc-status-mode
    dvc-tips-mode
    ediff-mode
    efs-mode
    Electric-buffer-menu-mode
    emms-browser-mode
    emms-mark-mode
    emms-metaplaylist-mode
    emms-playlist-mode
    ert-results-mode
    etags-select-mode
    fj-mode
    gc-issues-mode
    gdb-breakpoints-mode
    gdb-disassembly-mode
    gdb-frames-mode
    gdb-locals-mode
    gdb-memory-mode
    gdb-registers-mode
    gdb-threads-mode
    gist-list-mode
    gnus-article-mode
    gnus-browse-mode
    gnus-group-mode
    gnus-server-mode
    gnus-summary-mode
    google-maps-static-mode
    ibuffer-mode
    jde-javadoc-checker-report-mode
    magit-commit-mode
    magit-diff-mode
    magit-key-mode
    magit-log-mode
    magit-mode
    magit-reflog-mode
    magit-show-branches-mode
    magit-stash-mode
    magit-status-mode
    magit-wazzup-mode
    mh-folder-mode
    monky-mode
    notmuch-hello-mode
    notmuch-search-mode
    notmuch-show-mode
    occur-mode
    org-agenda-mode
    package-menu-mode
    proced-mode
    rcirc-mode
    rebase-mode
    recentf-dialog-mode
    reftex-select-bib-mode
    reftex-toc-mode
    sldb-mode
    slime-inspector-mode
    slime-thread-control-mode
    slime-xref-mode
    sr-buttons-mode
    sr-mode
    sr-tree-mode
    sr-virtual-mode
    tar-mode
    tetris-mode
    tla-annotate-mode
    tla-archive-list-mode
    tla-bconfig-mode
    tla-bookmarks-mode
    tla-branch-list-mode
    tla-browse-mode
    tla-category-list-mode
    tla-changelog-mode
    tla-follow-symlinks-mode
    tla-inventory-file-mode
    tla-inventory-mode
    tla-lint-mode
    tla-logs-mode
    tla-revision-list-mode
    tla-revlog-mode
    tla-tree-lint-mode
    tla-version-list-mode
    twittering-mode
    urlview-mode
    vc-annotate-mode
    vc-dir-mode
    vc-git-log-view-mode
    vc-svn-log-view-mode
    vm-mode
    vm-summary-mode
    w3m-mode
    wab-compilation-mode
    xgit-annotate-mode
    xgit-changelog-mode
    xgit-diff-mode
    xgit-revlog-mode
    xhg-annotate-mode
    xhg-log-mode
    xhg-mode
    xhg-mq-mode
    xhg-mq-sub-mode
    xhg-status-extra-mode)
  "Modes that should come up in Emacs state."
  :type  '(repeat symbol)
  :group 'evil)

(defcustom evil-insert-state-modes
  '(comint-mode
    erc-mode
    eshell-mode
    geiser-repl-mode
    gud-mode
    inferior-apl-mode
    inferior-caml-mode
    inferior-emacs-lisp-mode
    inferior-j-mode
    inferior-python-mode
    inferior-scheme-mode
    inferior-sml-mode
    internal-ange-ftp-mode
    prolog-inferior-mode
    reb-mode
    shell-mode
    slime-repl-mode
    term-mode
    wdired-mode)
  "Modes that should come up in Insert state."
  :type  '(repeat symbol)
  :group 'evil)

(defcustom evil-motion-state-modes
  '(apropos-mode
    Buffer-menu-mode
    calendar-mode
    color-theme-mode
    command-history-mode
    compilation-mode
    dictionary-mode
    help-mode
    Info-mode
    speedbar-mode
    undo-tree-visualizer-mode
    view-mode)
  "Modes that should come up in Motion state."
  :type  '(repeat symbol)
  :group 'evil)

(defcustom evil-overriding-maps
  '((Buffer-menu-mode-map . nil)
    (color-theme-mode-map . nil)
    (comint-mode-map . nil)
    (compilation-mode-map . nil)
    (dictionary-mode-map . nil)
    (Info-mode-map . motion)
    (speedbar-key-map . nil)
    (speedbar-file-key-map . nil)
    (speedbar-buffers-key-map . nil))
  "Keymaps that should override Evil maps.
Entries have the form (MAP-VAR . STATE), where MAP-VAR is
a keymap variable and STATE is the state whose bindings
should be overridden. If STATE is nil, all states are
overridden."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'evil)

(defcustom evil-intercept-maps
  '((edebug-mode-map . nil))
  "Keymaps that should intercept Evil maps.
Entries have the form (MAP-VAR . STATE), where MAP-VAR is
a keymap variable and STATE is the state whose bindings
should be intercepted. If STATE is nil, all states are
intercepted."
  :type '(alist :key-type symbol :value-type symbol)
  :group 'evil)

(defcustom evil-motions
  '(back-to-indentation
    backward-char
    backward-list
    backward-paragraph
    backward-sentence
    backward-sexp
    backward-up-list
    backward-word
    beginning-of-buffer
    beginning-of-defun
    beginning-of-line
    beginning-of-visual-line
    c-beginning-of-defun
    c-end-of-defun
    digit-argument
    down-list
    end-of-buffer
    end-of-defun
    end-of-line
    end-of-visual-line
    exchange-point-and-mark
    forward-char
    forward-list
    forward-paragraph
    forward-sentence
    forward-sexp
    forward-word
    goto-last-change
    ibuffer-backward-line
    ibuffer-forward-line
    isearch-abort
    isearch-cancel
    isearch-complete
    isearch-del-char
    isearch-delete-char
    isearch-edit-string
    isearch-exit
    isearch-highlight-regexp
    isearch-occur
    isearch-other-control-char
    isearch-other-meta-char
    isearch-printing-char
    isearch-query-replace
    isearch-query-replace-regexp
    isearch-quote-char
    isearch-repeat-backward
    isearch-repeat-forward
    isearch-ring-advance
    isearch-ring-retreat
    isearch-toggle-case-fold
    isearch-toggle-input-method
    isearch-toggle-regexp
    isearch-toggle-specified-input-method
    isearch-toggle-word
    isearch-yank-char
    isearch-yank-kill
    isearch-yank-line
    isearch-yank-word-or-char
    keyboard-quit
    left-char
    left-word
    mouse-drag-region
    mouse-save-then-kill
    mouse-set-point
    mouse-set-region
    mwheel-scroll
    move-beginning-of-line
    move-end-of-line
    next-error
    next-line
    paredit-backward
    paredit-backward-down
    paredit-backward-up
    paredit-forward
    paredit-forward-down
    paredit-forward-up
    pop-global-mark
    pop-tag-mark
    pop-to-mark-command
    previous-error
    previous-line
    redo
    right-char
    right-word
    scroll-down
    scroll-up
    undo
    undo-tree-redo
    undo-tree-undo
    universal-argument
    universal-argument-minus
    universal-argument-more
    universal-argument-other-key
    up-list)
  "Non-Evil commands to initialize to motions."
  :type  '(repeat symbol)
  :group 'evil)

(defcustom evil-visual-newline-commands
  '(LaTeX-section
    TeX-font)
  "Commands excluding the trailing newline of a Visual Line selection.
These commands work better without this newline."
  :type  '(repeat symbol)
  :group 'evil)

(defcustom evil-want-visual-char-semi-exclusive nil
  "Visual character selection to beginning/end of line is exclusive.
If non nil then an inclusive visual character selection which
ends at the beginning or end of a line is turned into an
exclusive selection. Thus if the selected (inclusive) range ends
at the beginning of a line it is changed to not include the first
character of that line, and if the selected range ends at the end
of a line it is changed to not include the newline character of
that line."
  :type 'boolean
  :group 'evil)

(defface evil-ex-info '(( ((supports :slant))
                          :slant italic
                          :foreground "red"))
  "Face for the info message in ex mode."
  :group 'evil)

;; Searching
(defcustom evil-ex-interactive-search-highlight 'all-windows
  "Determine in which windows the interactive highlighting should be shown."
  :type '(radio (const :tag "All windows." all-windows)
                (const :tag "Selected window." selected-window)
                (const :tag "Disable highlighting." nil))
  :group 'evil)

(defcustom evil-ex-search-case 'smart
  "The case behaviour of the search command."
  :type '(radio (const :tag "Case sensitive." 'sensitive)
                (const :tag "Case insensitive." 'insensitive)
                (const :tag "Smart case." 'smart))
  :group 'evil)

(defcustom evil-ex-substitute-case nil
  "The case behaviour of the search command."
  :type '(radio (const :tag "Same as interactive search." nil)
                (const :tag "Case sensitive." 'sensitive)
                (const :tag "Case insensitive." 'insensitive)
                (const :tag "Smart case." 'smart))
  :group 'evil)

(defcustom evil-ex-search-interactive t
  "If t search is interactive."
  :type 'boolean
  :group 'evil)

(defcustom evil-ex-search-highlight-all t
  "If t and interactive search is enabled, all matches are
highlighted."
  :type 'boolean
  :group 'evil)

(defcustom evil-ex-substitute-highlight-all t
  "If t all matches for the substitute pattern are highlighted."
  :type 'boolean
  :group 'evil)

(defcustom evil-ex-substitute-interactive-replace t
  "If t and substitute patterns are highlighted,
the replacement is shown interactively."
  :type 'boolean
  :group 'evil)

(defface evil-ex-search '((t :inherit isearch))
  "Face for interactive search."
  :group 'evil)

(defface evil-ex-lazy-highlight '((t :inherit lazy-highlight))
  "Face for highlighting all matches in interactive search."
  :group 'evil)

(defface evil-ex-substitute '((((supports :underline))
                               :underline t
                               :foreground "red"))
  "Face for interactive replacement text."
  :group 'evil)

;;; Variables

(defvar evil-state nil
  "The current Evil state.
To change the state, use `evil-change-state'
or call the state function (e.g., `evil-normal-state').")
(make-variable-buffer-local 'evil-state)
(put 'evil-state 'permanent-local t)

;; these may be used inside `evil-define-state'
(defvar evil-next-state nil
  "The Evil state being switched to.")
(make-variable-buffer-local 'evil-next-state)
(put 'evil-next-state 'permanent-local t)

(defvar evil-previous-state nil
  "The Evil state being switched from.")
(make-variable-buffer-local 'evil-previous-state)
(put 'evil-previous-state 'permanent-local t)

(defvar evil-mode-line-tag nil
  "Mode-Line indicator for the current state.")
(make-variable-buffer-local 'evil-mode-line-tag)
(put 'evil-mode-line-tag 'permanent-local t)
(put 'evil-mode-line-tag 'risky-local-variable t)

(defvar evil-global-keymaps-alist nil
  "Association list of keymap variables.
Entries have the form (MODE . KEYMAP), where KEYMAP
is the variable containing the keymap for MODE.")

(defvar evil-local-keymaps-alist nil
  "Association list of keymap variables that must be
reinitialized in each buffer. Entries have the form
\(MODE . KEYMAP), where KEYMAP is the variable containing
the keymap for MODE.")

(defvar evil-state-properties nil
  "Specifications made by `evil-define-state'.
Entries have the form (STATE . PLIST), where PLIST is a property
list specifying various aspects of the state. To access a property,
use `evil-state-property'.")

(defvar evil-mode-map-alist nil
  "Association list of keymaps to use for Evil modes.
Elements have the form (MODE . KEYMAP), with the first keymaps
having higher priority.")
(make-variable-buffer-local 'evil-mode-map-alist)
(put 'evil-mode-map-alist 'permanent-local t)

(defvar evil-command-properties nil
  "Specifications made by `evil-define-command'.")

(defvar evil-transient-vars '(cua-mode transient-mark-mode)
  "List of variables pertaining to Transient Mark mode.")

(defvar evil-transient-vals nil
  "Association list of old values for Transient Mark mode variables.
Entries have the form (VARIABLE VALUE LOCAL), where LOCAL is
whether the variable was previously buffer-local.")

(defvar evil-no-display nil
  "If non-nil, various Evil displays are inhibited.
Use the macro `evil-without-display' to set this variable.")
(make-variable-buffer-local 'evil-no-display)
(put 'evil-no-display 'permanent-local t)

(defvar evil-type-properties nil
  "Specifications made by `evil-define-type'.
Entries have the form (TYPE . PLIST), where PLIST is a property
list specifying functions for handling the type: expanding it,
describing it, etc.")

(defvar evil-interactive-alist nil
  "Association list of Evil-specific interactive codes.")

(defvar evil-motion-marker nil
  "Marker for storing the starting position of a motion.")
(make-variable-buffer-local 'evil-motion-marker)
(put 'evil-motion-marker 'permanent-local t)

(defvar evil-this-type nil
  "Current motion type.")
(make-variable-buffer-local 'evil-this-type)
(put 'evil-this-type 'permanent-local t)

(defvar evil-this-register nil
  "Current register.")
(make-variable-buffer-local 'evil-this-register)
(put 'evil-this-register 'permanent-local t)

(defvar evil-this-macro nil
  "Current macro register.")
(make-variable-buffer-local 'evil-this-macro)
(put 'evil-this-macro 'permanent-local t)

(defvar evil-this-operator nil
  "Current operator.")
(make-variable-buffer-local 'evil-this-operator)
(put 'evil-this-operator 'permanent-local t)

(defvar evil-this-motion nil
  "Current motion.")
(make-variable-buffer-local 'evil-this-motion)
(put 'evil-this-motion 'permanent-local t)

(defvar evil-this-motion-count nil
  "Current motion count.")
(make-variable-buffer-local 'evil-this-motion-count)
(put 'evil-this-motion-count 'permanent-local t)

(defvar evil-inhibit-operator nil
  "Inhibit current operator.
If an operator calls a motion and the motion sets this variable
to t, the operator code is not executed.")

(defvar evil-inhibit-operator-value nil
  "This variable is used to transfer the value
of `evil-inhibit-operator' from one local scope to another.")

;; used by `evil-define-operator'
(defvar evil-operator-range-beginning nil
  "Beginning of `evil-operator-range'.")

(defvar evil-operator-range-end nil
  "End of `evil-operator-range'.")

(defvar evil-operator-range-type nil
  "Type of `evil-operator-range'.")

(defvar evil-operator-range-motion nil
  "Motion of `evil-operator-range'.")

(defvar evil-markers-alist
  '((?\( . evil-backward-sentence)
    (?\) . evil-forward-sentence)
    (?{ . evil-backward-paragraph)
    (?} . evil-forward-paragraph)
    (?' . evil-jump-backward)
    (?` . evil-jump-backward)
    (?< . evil-visual-beginning)
    (?> . evil-visual-goto-end)
    (?. . (lambda ()
            (let (last-command)
              (goto-last-change nil)))))
  "Association list for markers.
Entries have the form (CHAR . DATA), where CHAR is the marker's
name and DATA is either a marker object as returned by `make-marker',
a variable, a movement function, or a cons cell (STRING NUMBER),
where STRING is a file path and NUMBER is a buffer position.
The global value of this variable holds markers available from
every buffer, while the buffer-local value holds markers available
only in the current buffer.")
(make-variable-buffer-local 'evil-markers-alist)
(put 'evil-markers-alist 'permanent-local t)

(defvar evil-jump-list nil
  "Jump list.")
(make-variable-buffer-local 'evil-jump-list)
(put 'evil-jump-list 'permanent-local t)

(defconst evil-suppress-map (make-keymap)
  "Full keymap disabling default bindings to `self-insert-command'.")
(suppress-keymap evil-suppress-map t)

;; TODO: customize size of ring
(defvar evil-repeat-ring (make-ring 10)
  "A ring of repeat-informations to repeat the last command.")

(defvar evil-repeat-types
  '((t . evil-repeat-keystrokes)
    (change . evil-repeat-changes)
    (motion . evil-repeat-motion)
    (ignore . nil))
  "An alist of defined repeat-types.")

(defvar evil-recording-repeat nil
  "Whether we are recording a repeat.")

(defvar evil-recording-current-command nil
  "Whether we are recording the current command for repeat.")

(defvar evil-repeat-changes nil
  "Accumulated buffer changes for changed-based commands.")

(defvar evil-repeat-info nil
  "Information accumulated during current repeat.")

(defvar evil-repeat-buffer nil
  "The buffer in which the repeat started.
If the buffer is changed, the repeat is cancelled.")

(defvar evil-repeat-pos nil
  "The position of point at the beginning of an change-tracking
  editing command.")

(defvar evil-repeat-keys nil
  "The keys that invoked the current command.")

(defvar evil-last-repeat nil
  "Information about the latest repeat command.
This is a list of three elements (POINT COUNT UNDO-POINTER),
where POINT is the position of point before the latest repeat,
COUNT the count-argument of the latest repeat command and
UNDO-POINTER the head of the undo-list before the last command
has been repeated.")

(defvar evil-repeat-count nil
  "The explicit count when repeating a command.")

(defvar evil-insert-count nil
  "The explicit count passed to an command starting Insert state.")
(make-variable-buffer-local 'evil-insert-count)
(put 'evil-insert-count 'permanent-local t)

(defvar evil-insert-vcount nil
  "The information about the number of following lines the
insertion should be repeated. This is list (LINE COLUMN COUNT)
where LINE is the line-number where the original insertion
started and COLUMN is either a number of function determining the
column where the repeated insertions should take place. COUNT is
number of repeats (including the original insertion).")
(make-variable-buffer-local 'evil-insert-vcount)
(put 'evil-insert-vcount 'permanent-local t)

(defvar evil-insert-skip-empty-lines nil
  "Non-nil of the current insertion should not take place on
  lines at which the insertion point is behind the end of the
  line.")

(defvar evil-insert-lines nil
  "Non-nil if the current insertion command is a line-insertion
command o or O.")
(make-variable-buffer-local 'evil-insert-lines)
(put 'evil-insert-lines 'permanent-local t)

(defvar evil-insert-repeat-info nil
  "Repeat information accumulated during an insertion.")
(make-variable-buffer-local 'evil-insert-repeat-info)
(put 'evil-insert-repeat-info 'permanent-local t)

(defvar evil-replace-alist nil
  "Association list of characters overwritten in Replace state.
The format is (POS . CHAR).")
(make-variable-buffer-local 'evil-replace-alist)
(put 'evil-replace-alist 'permanent-local t)

(defvar evil-echo-area-message nil
  "Previous value of `current-message'.")
(make-variable-buffer-local 'evil-echo-area-message)
(put 'evil-echo-area-message 'permanent-local t)

(defvar evil-write-echo-area nil
  "If set to t inside `evil-save-echo-area', then the echo area
is not restored.")

(defvar evil-last-find nil
  "A pair (FUNCTION . CHAR) describing the lastest character
  search command.")

(defvar evil-last-paste nil
  "Information about the latest paste.
This should be a list (CMD POINT BEG END) where CMD is the last
paste-command (either `evil-paste-before' or `evil-paste-after'),
POINT is the position of point before the paste,
BEG end END are the region of the inserted text.")

(defvar evil-paste-count nil
  "The count argument of the current paste command.")

(defvar evil-temporary-undo nil
  "When undo is disabled in current buffer.
Certain commands depending on undo use this variable
instead of `buffer-undo-list'.")

(defvar evil-undo-list-pointer nil
  "Everything up to this mark is united in the undo-list.")
(make-variable-buffer-local 'evil-undo-list-pointer)
(put 'evil-undo-list-pointer 'permanent-local t)

(defvar evil-flash-timer nil
  "Timer for flashing search results.")

(defvar evil-search-prompt nil
  "String to use for search prompt.")

(defvar evil-inner-text-objects-map (make-sparse-keymap)
  "Keymap for inner text objects.")

(defvar evil-outer-text-objects-map (make-sparse-keymap)
  "Keymap for outer text objects.")

(defvar evil-window-map (make-sparse-keymap)
  "Keymap for window-related commands.")

(defvar evil-input-method nil
  "Input method used in Insert state and Emacs state.")
(make-variable-buffer-local 'evil-input-method)
(put 'evil-input-method 'permanent-local t)

;;; Visual state

(defvar evil-visual-beginning nil
  "The beginning of the Visual selection, a marker.")
(make-variable-buffer-local 'evil-visual-beginning)
(put 'evil-visual-beginning 'permanent-local t)

(defvar evil-visual-end nil
  "The end of the Visual selection, a marker.")
(make-variable-buffer-local 'evil-visual-end)
(put 'evil-visual-end 'permanent-local t)

(defvar evil-visual-point nil
  "The position of point in Visual state, a marker.")
(make-variable-buffer-local 'evil-visual-point)
(put 'evil-visual-point 'permanent-local t)

(defvar evil-visual-mark nil
  "The position of mark in Visual state, a marker.")
(make-variable-buffer-local 'evil-visual-mark)
(put 'evil-visual-mark 'permanent-local t)

(defvar evil-visual-previous-mark nil
  "The position of mark before Visual state, a marker.")
(make-variable-buffer-local 'evil-visual-previous-mark)
(put 'evil-visual-previous-mark 'permanent-local t)

(defvar evil-visual-selection nil
  "The kind of Visual selection.
This is a selection as defined by `evil-define-visual-selection'.")
(make-variable-buffer-local 'evil-visual-selection)
(put 'evil-visual-selection 'permanent-local t)

;; we could infer the direction by comparing `evil-visual-mark'
;; and `evil-visual-point', but destructive operations may
;; displace the markers
(defvar evil-visual-direction 0
  "Whether point follows mark in Visual state.
Negative if point precedes mark, otherwise positive.
See also the function `evil-visual-direction'.")
(make-variable-buffer-local 'evil-visual-direction)
(put 'evil-visual-direction 'permanent-local t)

(defvar evil-visual-properties nil
  "Property list of miscellaneous Visual properties.")
(make-variable-buffer-local 'evil-visual-properties)
(put 'evil-visual-properties 'permanent-local t)

(defvar evil-visual-region-expanded nil
  "Whether the region matches the Visual selection.
That is, whether the positions of point and mark have been
expanded to coincide with the selection's boundaries.
This makes the selection available to functions acting
on Emacs' region.")
(make-variable-buffer-local 'evil-visual-region-expanded)
(put 'evil-visual-region-expanded 'permanent-local t)

(defvar evil-visual-overlay nil
  "Overlay for highlighting the Visual selection.
Not used for blockwise selections, in which case
see `evil-visual-block-overlays'.")
(make-variable-buffer-local 'evil-visual-overlay)
(put 'evil-visual-overlay 'permanent-local t)

(defvar evil-visual-block-overlays nil
  "Overlays for Visual Block selection, one for each line.
They are reused to minimize flicker.")
(make-variable-buffer-local 'evil-visual-block-overlays)
(put 'evil-visual-block-overlays 'permanent-local t)

(defvar evil-visual-alist nil
  "Association list of Visual selection functions.
Elements have the form (NAME . FUNCTION).")

;;; Ex

(defvar evil-ex-map (make-sparse-keymap)
  "Keymap for Ex.
Key sequences bound in this map are immediately executed.")

(defvar evil-ex-completion-map (make-sparse-keymap)
  "Completion keymap for Ex.")
(set-keymap-parent evil-ex-completion-map minibuffer-local-completion-map)
(define-key evil-ex-completion-map (kbd "SPC") #'self-insert-command)

(defvar evil-ex-shell-argument-initialized nil
  "This variable is set to t if shell command completion has been initialized.
See `evil-ex-init-shell-argument-completion'.")

(defvar evil-ex-commands nil
  "Association list of command bindings and functions.")

(defvar evil-ex-history nil
  "History of Ex commands.")

(defvar evil-ex-current-buffer nil
  "The buffer from which Ex was started.")

(defvar evil-ex-expression nil
  "The evaluation tree.")

(defvar evil-ex-tree nil
  "The syntax tree.")

(defvar evil-ex-command nil
  "The current Ex command.")

(defvar evil-ex-previous-command nil
  "The previously executed Ex command.")

(defvar evil-ex-range nil
  "The current range of the Ex command.")

(defvar evil-ex-bang nil
  "The \"!\" argument of the current Ex command.")

(defvar evil-ex-argument nil
  "The current argument of the Ex command.")

(defvar evil-ex-argument-handler nil
  "The argument handler for the current Ex command.")

(defvar evil-ex-argument-types nil
  "Association list of argument handlers.")

(defvar evil-previous-shell-command nil
  "The last shell command.")

;; Searching
(defvar evil-ex-search-history nil
  "The history for the search command.")

(defvar evil-ex-search-direction nil
  "The direction of the current search, either 'forward or 'backward.")

(defvar evil-ex-search-count nil
  "The count if the current search.")

(defvar evil-ex-search-start-point nil
  "The point where the search started.")

(defvar evil-ex-search-overlay nil
  "The overlay for the current search result.")

(defvar evil-ex-search-pattern nil
  "The last search pattern.")

(defvar evil-ex-search-offset nil
  "The last search offset.")

(defvar evil-ex-search-match-beg nil
  "The beginning position of the last match.")

(defvar evil-ex-search-match-end nil
  "The end position of the last match.")

(defvar evil-ex-substitute-pattern nil
  "The last substitute pattern.")

(defvar evil-ex-substitute-replacement nil
  "The last substitute replacement.")

(defvar evil-ex-substitute-flags nil
  "The last substitute flags.")

(defvar evil-ex-substitute-current-replacement nil
  "The actual replacement.")

;; The lazy-highlighting framework.
(defvar evil-ex-active-highlights-alist nil
  "An alist of currently active highlights.")
(make-variable-buffer-local 'evil-ex-active-highlights-alist)
(put 'evil-ex-active-highlights-alist 'permanent-local t)

(defvar evil-ex-hl-update-timer nil
  "Time used for updating highlights.")
(make-variable-buffer-local 'evil-ex-hl-update-timer)
(put 'evil-ex-hl-update-timer 'permanent-local t)

(defvar evil-ex-search-keymap (make-sparse-keymap)
  "Keymap used in ex-search-mode.")
(set-keymap-parent evil-ex-search-keymap minibuffer-local-map)

(defconst evil-version "0.1"
  "The current version of Evil")

(defun evil-version ()
  (interactive)
  (message "Evil version %s" evil-version))

(provide 'evil-vars)

;;; evil-vars.el ends here
