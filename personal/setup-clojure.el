;;;;
;; Clojure
;;;;
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

;; Enable paredit for Clojure

(add-hook 'clojure-mode-hook
          (lambda ()
            (enable-paredit-mode)
            (lispy-mode 1)))
(add-hook 'cider-repl-mode-hook
          (lambda ()
            (enable-paredit-mode)
            (lispy-mode 1)
            ))


;; (setq cider-jack-in-dependencies
;;       (delete-dups
;;        (append
;;         lispy-cider-jack-in-dependencies
;;         cider-jack-in-dependencies)))

;; This is useful for working with camel-case tokens, like names of
;; Java classes (e.g. JavaClassName)
(add-hook 'clojure-mode-hook 'subword-mode)

;; Eval buffer on save
(add-hook 'cider-mode-hook
   '(lambda () (add-hook 'after-save-hook
    '(lambda ()
       (if (and (boundp 'cider-mode) cider-mode)
           (cider-eval-file (buffer-file-name)))))))

;; A little more syntax highlighting
(require 'clojure-mode-extra-font-locking)
;; (require 'flycheck-joker)
;; (add-hook 'clojure-mode-hook 'flycheck-mode)

(require 'flycheck-clj-kondo)

;; syntax hilighting for midje
(add-hook 'clojure-mode-hook
          (lambda ()
            (setq inferior-lisp-program "lein repl")
            (font-lock-add-keywords
             nil
             '(("(\\(facts?\\)"
                (1 font-lock-keyword-face))
               ("(\\(background?\\)"
                (1 font-lock-keyword-face))))
            (define-clojure-indent (fact 1))
            (define-clojure-indent (facts 1))
            (rainbow-delimiters-mode)))

;; use C-c to eval inside comment
(setq clojure-toplevel-inside-comment-form t)

(defun cider-clear-repl-buffer* ()
  (interactive)
  (cider-switch-to-repl-buffer)
  (cider-repl-clear-buffer)
  (insert ";; cleared buffer")
  (cider-repl-return)
  (cider-switch-to-last-clojure-buffer))


;;;;
;; Cider
;;;;

;; provides minibuffer documentation for the code you're typing into the repl
;; (add-hook 'cider-mode-hook 'eldoc-mode)

;; go right to the REPL buffer when it's finished connecting
 (setq cider-repl-pop-to-buffer-on-connect t)

;; When there's a cider error, show its buffer and switch to it
(setq cider-show-error-buffer t)
(setq cider-auto-select-error-buffer nil)
(setq cider-auto-select-test-report-buffer nil)

;; Where to store the cider history.
(setq cider-repl-history-file "~/.emacs.d/cider-history")

;; Wrap when navigating history.
(setq cider-repl-wrap-history t)

;; enable paredit in your REPL
(add-hook 'cider-repl-mode-hook 'paredit-mode)

;; Use clojure mode for other extensions
(add-to-list 'auto-mode-alist '("\\.edn$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.boot$" . clojure-mode))
(add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojurescript-mode))
(add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode))



;; key bindings
;; these help me out with the way I usually develop web apps
(defun cider-start-http-server ()
  (interactive)
  ;; (cider-load-current-buffer)
  (let ((ns (cider-current-ns)))
    (cider-repl-set-ns ns)
    (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
    (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))


(defun cider-refresh ()
  (interactive)
  (cider-interactive-eval (format "(user/reset)")))

(defun cider-user-ns ()
  (interactive)
  (cider-repl-set-ns "user"))

(eval-after-load 'cider
  '(progn
     (define-key clojure-mode-map (kbd "s-<return>")  'cider-eval-defun-at-point)
     (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
     (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key clojure-mode-map (kbd "C-c C-SPC") 'cider-clear-repl-buffer*)
     (define-key cider-repl-mode-map (kbd "C-c C-SPC") 'cider-clear-repl-buffer*)
     (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns)
     (define-key cider-mode-map (kbd "C-c C-l") nil)
     (define-key cider-mode-map (kbd "C-c C-l C-r") 'cider-inspect-last-result)
     (define-key clojure-mode-map (kbd "M-.") 'cider-find-var)))

(define-key cider-mode-map (kbd "C-c C-l") nil)
(define-key cider-mode-map (kbd "C-c C-l C-r") 'cider-inspect-last-result)

(dolist (mode '(clojure-mode clojurescript-mode clojurec-mode cider-mode))
  (eval-after-load mode
    (font-lock-add-keywords
     mode '(("(\\(fn\\)[\[[:space:]]"   ; anon funcs 1
             (0 (progn (compose-region (match-beginning 1)
                                       (match-end 1) "λ")
                       nil)))
            ("\\(#\\)("                 ; anon funcs 2
             (0 (progn (compose-region (match-beginning 1)
                                       (match-end 1) "ƒ")
                       nil)))
            ("\\(#\\){"                 ; sets
             (0 (progn (compose-region (match-beginning 1)
                                       (match-end 1) "∈")
                       nil)))))))



;; clj-refactor
(require 'clj-refactor)
;; (require 'lsp)
;; (require 'lsp-ui)


(defun setup-clj-refactor ()
  (clj-refactor-mode 1)
  (yas-minor-mode 1) ; for adding require/use/import statements
  ;; This choice of keybinding leaves cider-macroexpand-1 unbound
  (cljr-add-keybindings-with-prefix "C-c C-m"))

(add-hook 'clojure-mode-hook #'setup-clj-refactor)


;; clojure-lsp
(add-hook 'clojure-mode-hook 'lsp)
(add-hook 'clojurescript-mode-hook 'lsp)
;; (add-hook 'clojurec-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-minimum-prefix-length 1
      lsp-enable-on-type-formatting nil
      lsp-log-io t
      lsp-enable-indentation t
      ;; lsp-enable-indentation nil ; uncomment to use cider indentation instead of lsp
      ;; lsp-enable-completion-at-point nil ; uncomment to use cider completion instead of lsp

      )

(setq lsp-lens-enable t)
(setq lsp-lens-enable t)
(setq lsp-signature-mode nil)
(setq lsp-signature-mode nil)
(setq lsp-ui-mode nil)
(setq lsp-ui-mode nil)
(setq lsp-ui-doc-show-with-cursor nil)
(setq lsp-ui-doc-show-with-cursor nil)
(setq lsp-headerline-breadcrumb-enable nil)
(setq lsp-ui-sideline-enable t)
(setq lsp-modeline-code-actions-enable nil)
(setq lsp-signature-render-documentation nil)
(setq lsp-signature-auto-activate nil)
(setq lsp-completion-enable nil)
(setq lsp-ui-doc-enable nil)
(setq cider-eldoc-display-for-symbol-at-point nil)
                                        ; disable cider showing eldoc during symbol at point

(setq lsp-completion-enable nil)
                                        ; use cider completion

(setq lsp-eldoc-enable-hover t)
                                        ; disable lsp-mode showing eldoc during symbol at point
(setq lsp-signature-mode t)
(setq cider-eldoc-display-for-symbol-at-point nil)

(setq cider-eldoc-display-context-dependent-info nil)


(define-key lispy-mode-map (kbd "M-.") 'cider-find-var)
(define-key lispy-mode-map (kbd "p") nil)
(define-key lispy-mode-map (kbd "g") nil)
;;(setq lsp-enable-completion-at-point nil) ; use cider completion


;; don't kill the REPL when printing large data structures
(setq cider-print-options
      '(("length" 80)
        ("level" 20)
        ("right-margin" 80)))


(require 'cider-eval-sexp-fu)
(setq eval-sexp-fu-flash-duration 1)
