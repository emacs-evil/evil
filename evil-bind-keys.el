;;; evil-bind-keys.el --- `bind-keys' equivalent for evil -*- lexical-binding: t -*-

;; Author: Luigi Sartor Piucco <luigipiucco at gmail.com>
;; Maintainer: Vegard Øye <vegard_oye at hotmail.com>

;; Version: 1.15.0

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

;;; Code:

(require 'cl-lib)
(require 'evil-core)
(require 'bind-key)
(require 'subr-x)

;; Helpers.

(defun evil-bind-keys--split-state-pseudo-key (keys)
  "Split the evil state name from KEYS if it is there.
KEYS should satisfy `key-valid-p'.  First check if (kbd KEYS) returns
a vector, else return nil.  If the first element is an evil state NAME
suffixed with \"-state\", remove the suffix and return a cons (NAME
. KEYS-REST), where KEYS-REST is the vector without the state element.
If the first element is anything else, return nil."
  (and-let* ((key-vec (kbd keys))
             (_ (vectorp key-vec))
             (key-list (append key-vec nil))
             (first (car key-list))
             (state-name (evil--state-pseudo-key first))
             (key-rest (cdr key-list)))
    `(,state-name . ,(vconcat key-rest))))

(defun evil--state-pseudo-key (key)
  "Tests whether KEY (a symbol) is an evil state name suffixed with \"-state\".
Returns the state name, without the \"-state\" suffix."
  (and-let* ((_ (symbolp key))
             (sym-name (symbol-name key))
             (_ (string-suffix-p "-state" sym-name))
             (state-name (string-remove-suffix "-state" sym-name))
             (state-name-sym (intern state-name))
             (_ (evil-state-p state-name-sym)))
    state-name-sym))

;; Shorthand macros.

;;;###autoload
(defmacro evil-bind-keys (&rest args)
  "Bind multiple keys at once, possibly with evil state bindings.

Accepts keyword arguments in ARGS:
:map MAP               - a keymap into which the keybindings should be
                         added
:prefix KEY            - prefix key for these bindings
:prefix-map MAP        - name of the prefix map that should be created
                         for these bindings
:prefix-docstring STR  - docstring for the prefix-map variable
:menu-name NAME        - optional menu string for prefix map
:repeat-docstring STR  - docstring for the repeat-map variable
:repeat-map MAP        - name of the repeat map that should be created
                         for these bindings. If specified, the
                         `repeat-map' property of each command bound
                         (within the scope of the `:repeat-map' keyword)
                         is set to this map.
:exit BINDINGS         - Within the scope of `:repeat-map' will bind the
                         key in the repeat map, but will not set the
                         `repeat-map' property of the bound command.
:continue BINDINGS     - Within the scope of `:repeat-map' forces the
                         same behaviour as if no special keyword had
                         been used (that is, the command is bound, and
                         it's `repeat-map' property set)
:filter FORM           - optional form to determine when bindings apply
                         (unsupported for state-specific bindings)

The rest of the arguments are conses of keybinding string and a
function symbol (unquoted).

Since `evil-bind-keys' defines drop-in replacements, passing only bindings
without evil information acts identical to `bind-keys'."
  (macroexp-progn (evil-bind-keys-form args nil)))

;;;###autoload
(defmacro evil-bind-keys* (&rest args)
  "To `evil-bind-keys' what `bind-keys*' is to `bind-keys'.
ARGS are the same as for `evil-bind-keys'.  Note that for evil
bindings, this does NOT mean using `evil-define-key*', it means that
global bindings in ARGS go to `override-global-map' instead of
`global-map'.  This is to mimic the behavior of `bind-keys*'
vs. `bind-keys'.

Since `evil' defines drop-in replacements, passing only bindings
without evil information acts identical to `bind-keys*'.  Defining
evil bindings in `override-global-map' is rarely needed, because they
have high precedence by default, but this is provided in case the
necessity arises."
  (macroexp-progn (evil-bind-keys-form args override-global-map)))

;; Main function.
(defun evil-bind-keys-form (args keymap)
  "Generates the code for `evil-bind-keys'/`evil-bind-keys*', which see.
ARGS is a quoted argument list, as described in the two macros.
KEYMAP is used as fallback if a :map key is not present.  This is
mostly for internal use."
  (let (map
        prefix-doc
        prefix-map
        prefix
        repeat-map
        repeat-doc
        repeat-type ;; Only used internally
        filter
        menu-name
        pkg)

    ;; Process any initial keyword arguments
    (let ((cont t)
          (arg-change-func 'cddr))
      (while (and cont args)
        (if (cond ((and (eq :map (car args))
                        (not prefix-map))
                   (setq map (cadr args)))
                  ((eq :prefix-docstring (car args))
                   (setq prefix-doc (cadr args)))
                  ((and (eq :prefix-map (car args))
                        (not (memq map '(global-map
                                         override-global-map))))
                   (setq prefix-map (cadr args)))
                  ((eq :repeat-docstring (car args))
                   (setq repeat-doc (cadr args)))
                  ((and (eq :repeat-map (car args))
                        (not (memq map '(global-map
                                         override-global-map))))
                   (setq repeat-map (cadr args))
                   (setq map repeat-map))
                  ((eq :continue (car args))
                   (setq repeat-type :continue
                         arg-change-func 'cdr))
                  ((eq :exit (car args))
                   (setq repeat-type :exit
                         arg-change-func 'cdr))
                  ((eq :prefix (car args))
                   (setq prefix (cadr args)))
                  ((eq :filter (car args))
                   (setq filter (cadr args)) t)
                  ((eq :menu-name (car args))
                   (setq menu-name (cadr args)))
                  ((eq :package (car args))
                   (setq pkg (cadr args))))
            (setq args (funcall arg-change-func args))
          (setq cont nil))))

    (when (or (and prefix-map (not prefix))
              (and prefix (not prefix-map)))
      (error "Both :prefix-map and :prefix must be supplied"))

    (when repeat-type
      (unless repeat-map
        (error ":continue and :exit require specifying :repeat-map")))

    (when (and menu-name (not prefix))
      (error "If :menu-name is supplied, :prefix must be too"))

    (unless map (setq map keymap))

    ;; Process key binding arguments
    (let (first next)
      (while args
        (if (keywordp (car args))
            (progn
              (setq next args)
              (setq args nil))
          (if first
              (nconc first (list (car args)))
            (setq first (list (car args))))
          (setq args (cdr args))))

      (cl-flet
          ((wrap (map bindings)
             (if (and map pkg (not (memq map '(global-map
                                               override-global-map))))
                 `((if (boundp ',map)
                       ,(macroexp-progn bindings)
                     (eval-after-load
                         ,(if (symbolp pkg) `',pkg pkg)
                       ',(macroexp-progn bindings))))
               bindings)))

        (append
         (when prefix-map
           `((defvar ,prefix-map)
             ,@(when prefix-doc `((put ',prefix-map 'variable-documentation ,prefix-doc)))
             ,@(if menu-name
                   `((define-prefix-command ',prefix-map nil ,menu-name))
                 `((define-prefix-command ',prefix-map)))
             ,@(if (and map (not (eq map 'global-map)))
                   (wrap map `((bind-key ,prefix ',prefix-map ,map ,filter)))
                 `((bind-key ,prefix ',prefix-map nil ,filter)))))
         (when repeat-map
           `((defvar ,repeat-map (make-sparse-keymap)
               ,@(when repeat-doc `(,repeat-doc)))))
         (wrap map
               (cl-mapcan
                (lambda (form)
                  (let* ((key (car form))
                         (state-and-keys (evil--split-state-pseudo-key key))
                         (fun (and (cdr form) (list 'function (cdr form)))))
                    (cl-flet ((bind (map filter)
                                (pcase state-and-keys
                                  (`(,state . ,key)
                                   (when filter
                                     (error "For evil bindings, :filter is not supported"))
                                   `((evil-define-key ',state ,map ,key ,fun)))
                                  (_ `((bind-key ,key ,fun ,map ,filter))))))
                      (if prefix-map
                          (bind prefix-map filter)
                        (if (and map (not (eq map 'global-map)))
                            ;; Only needed in this branch, since when
                            ;; repeat-map is non-nil, map is always
                            ;; non-nil
                            `(,(when (and repeat-map (not (eq repeat-type :exit)))
                                 `(put ,fun 'repeat-map ',repeat-map))
                              ,@(bind map filter))
                          (bind nil filter))))))
                first))
         (when next
           (evil-bind-keys-form `(,@(when repeat-map `(:repeat-map ,repeat-map))
                                  ,@(if pkg
                                        (cons :package (cons pkg next))
                                      next)) map)))))))

(provide 'evil-bind-keys)

;;; evil-bind-keys.el ends here
