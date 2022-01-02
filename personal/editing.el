(setq tab-width 2)
(setq css-indent-offset 2)
(defvaralias 'c-basic-offset 'tab-width)
(defvaralias 'cperl-indent-level 'tab-width)

;; Customizations relating to editing a buffer.
(require 'company)
(add-hook 'after-init-hook 'global-company-mode)


;; lispy
(require 'paredit)
(setq lispy-compat '(edebug cider))
(require 'lispy)


(defun eval-with-lispy ()
  (interactive)
  (print (eq major-mode 'clojure-mode))
  (if (eq major-mode 'clojure-mode)
      (cider-eval-defun-at-point)
    (special-lispy-eval)))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (enable-paredit-mode)
            (lispy-mode 1)
            (lispy-define-key lispy-mode-map "C-a" 'crux-move-beginning-of-line)
            (lispy-define-key lispy-mode-map "e" 'eval-with-lispy)
            (define-key lispy-mode-map (kbd "C-a") 'crux-move-beginning-of-line)
            (setq lispy-colon-p nil)))

;; disable eshell keybinding
(global-set-key (kbd "C-x m") nil)

;; Key binding to use "hippie expand" for text autocompletion
;; http://www.emacswiki.org/emacs/HippieExpand
(global-set-key (kbd "M-/") 'hippie-expand)

;; Lisp-friendly hippie expand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Highlight current line
(global-hl-line-mode 1)

;; Interactive search key bindings. By default, C-s runs
;; isearch-forward, so this swaps the bindings.
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; When you visit a file, point goes to the last place where it
;; was when you previously visited the same file.
;; http://www.emacswiki.org/emacs/SavePlace
(require 'saveplace)
(setq-default save-place t)
;; keep track of saved places in ~/.emacs.d/places
(setq save-place-file (concat user-emacs-directory "places"))

;; Emacs can automatically create backup files. This tells Emacs to
;; put all backups in ~/.emacs.d/backups. More info:
;; http://www.gnu.org/software/emacs/manual/html_node/elisp/Backup-Files.html
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default t)

(setq auto-save-interval 50)

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; use 2 spaces for tabs
(defun die-tabs ()
   (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

(setq electric-indent-mode nil)


;; autocompletion
(add-hook 'cider-repl-mode-hook #'company-mode)
(add-hook 'cider-mode-hook #'company-mode)
(define-key company-active-map (kbd "C-s") 'company-select-next)
(define-key company-active-map (kbd "C-r") 'company-select-previous)

;;(setq company-idle-delay nil) ; never start completions automatically
(global-set-key (kbd "M-TAB") #'company-complete) ; use M-TAB, a.k.a. C-M-i, as manual trigger

(add-hook 'cider-repl-mode-hook #'cider-company-enable-fuzzy-completion)
(add-hook 'cider-mode-hook #'cider-company-enable-fuzzy-completion)


;; projectile
; manual https://docs.projectile.mx/en/latest/
(projectile-mode +1)
(define-key projectile-mode-map (kbd "s-p") 'projectile-command-map)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; switch buffer
(global-set-key (kbd "s-b") 'ido-switch-buffer)

;; paredit
(require 'paredit)
(define-key paredit-mode-map (kbd "C-}") 'paredit-forward-slurp-sexp)
(define-key paredit-mode-map (kbd "C-{") 'paredit-forward-barf-sexp)


(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1))) ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)            ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(global-set-key [?\C-c ?d] 'duplicate-line-or-region)


;; select word
(defun select-word ()
"Select a word under cursor.
“word” here is considered any alphenumeric sequence with “_” or “-”."
 (interactive)
 (let (b1 b2)
   (skip-chars-backward "-_A-Za-z0-9")
   (setq b1 (point))
   (skip-chars-forward "-_A-Za-z0-9")
   (setq b2 (point))
   (set-mark b1)
   )
 )

(global-set-key (kbd "M-8") 'select-word)

;; comment line or region
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Comment-Commands.html

;; move-text
(global-set-key (kbd "M-s-<up>") 'move-text-up)
(global-set-key (kbd "M-s-<down>") 'move-text-down)


;; save always
(defun save-buffer-always ()
  "Save the buffer even if it is not modified."
  (interactive)
  (set-buffer-modified-p t)
  (save-buffer))

(global-set-key (kbd "C-x s") 'save-buffer-always)
(global-set-key (kbd "C-x C-s") 'save-buffer-always)


;; yafolding
(require 'yafolding)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; undo tree mode                                                         ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'undo-tree)
;; https://github.com/emacsmirror/undo-tree/blob/master/undo-tree.el#L3092
;;turn on everywhere
(global-undo-tree-mode 1)
;; make ctrl-z undo
(global-set-key (kbd "C-z") 'undo)
;; make ctrl-shift-Z redo
(defalias 'redo 'undo-tree-redo)
(global-set-key (kbd "M-z") 'redo)



;; delete-region
(global-set-key (kbd "s-<backspace>") 'delete-region)

;; delete-line
(defun delete-line ()
  "Deletes a complete line no matter where this command is called"
  (interactive)
  (move-beginning-of-line 1)
  (kill-line)
  (paredit-backward-delete)
  (move-beginning-of-line 1))

(global-set-key (kbd "C-c C-k") 'delete-line)


;; revert-all-buffers
(defun revert-all-buffers ()
  "Refresh all open buffers from their respective files."
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (let ((filename (buffer-file-name buffer)))
        ;; Revert only buffers containing files, which are not modified;
        ;; do not try to revert non-file buffers like *Messages*.
        (when filename
          (if (file-exists-p filename)
              ;; If the file exists, revert the buffer.
              (with-demoted-errors "Error: %S"
                (with-current-buffer buffer
                  (revert-buffer :ignore-auto :noconfirm)))
            ;; If the file doesn't exist, kill the buffer.
            (let (kill-buffer-query-functions) ; No query done when killing buffer
              (kill-buffer buffer)
              (message "Killed non-existing file buffer: %s" buffer))))
        (setq buffer (pop list)))))
  (message "Finished reverting non-file buffers."))


;; use alt chars using keyboard
;(setq ns-right-alternate-modifier t)



;; on Linux, the menu/apps key syntax is <menu>
;; on Windows, the menu/apps key syntax is <apps>
;; make the syntax equal
(define-key key-translation-map (kbd "<apps>") (kbd "<menu>"))

(progn
  ;; define set of key sequences
  (define-prefix-command 'my-leader-key-map)
  (define-key my-leader-key-map (kbd "RET") 'eval-last-sexp)
  (define-key my-leader-key-map (kbd "c") 'easy-kill)
  (define-key my-leader-key-map (kbd "v") 'yank)
  (define-key my-leader-key-map (kbd "a") 'crux-move-beginning-of-line)
  (define-key my-leader-key-map (kbd "e") 'move-end-of-line)
  (define-key my-leader-key-map (kbd "SPC") 'easy-mark))


(progn
  ;; define set of key sequences
  (define-prefix-command 'print-keymap)
  (define-key print-keymap (kbd "[") 'lispy-backward)
  (define-key print-keymap (kbd "]") 'lispy-forward))


;; make the menu key as leader key
(global-set-key (kbd "<XF86WakeUp>") 'my-leader-key-map)
(global-set-key (kbd "<print>") 'print-keymap)

(global-set-key (kbd "C-M-SPC") 'mark-sexp)
(global-set-key (kbd "C-x C-c") 'easy-kill)
(global-set-key (kbd "C-x c") 'easy-kill)
(global-set-key (kbd "C-x v") 'yank)
(global-set-key (kbd "C-x C-v") 'yank)

;; resize buffers horizontally
(global-set-key (kbd "<kp-add>") 'enlarge-window-horizontally)
(global-set-key (kbd "<kp-subtract>") 'shrink-window-horizontally)

;;; backward delete
(global-set-key (kbd "C-r") 'paredit-backward-delete)

;; move to start of line
(global-set-key (kbd "C-a") 'crux-move-beginning-of-line)
