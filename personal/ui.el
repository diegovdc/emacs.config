;; Show line numbers
;; (global-linum-mode)
(global-display-line-numbers-mode -1)
(toggle-scroll-bar -1)

;; Automatically load paredit when editing a lisp file
;; More at http://www.emacswiki.org/emacs/ParEdit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)

(set-frame-font "Monospace 18" nil t);; me gusta
(set-face-attribute 'whitespace-line nil :background nil :foreground "#777777")
;; (when (window-system) (set-frame-font "Fira Code Light 11"))



;; doom modeline
(require 'doom-modeline)
(add-hook 'after-init-hook #'doom-modeline-mode)

(setq doom-modeline-height 1)
(set-face-attribute 'mode-line nil :family "Noto Sans" :height 160)
(set-face-attribute 'mode-line-inactive nil :family "Noto Sans" :height 100)
(setq doom-modeline-buffer-file-name-style 'truncate-with-project)
(setq doom-modeline-buffer-encoding nil)
(setq doom-modeline-vcs-max-length 30)
