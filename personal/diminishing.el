(require 'diminish)
(eval-after-load "editorconfig" '(diminish 'editorconfig-mode))
(eval-after-load "paredit" '(diminish 'paredit-mode))
(eval-after-load "projectile" '(diminish 'projectile-mode "pj"))
(eval-after-load "company" '(diminish 'company-mode))
(eval-after-load " guru" '(diminish 'guru-mode))
(eval-after-load "which-key" '(diminish 'which-key-mode))
(eval-after-load "flyspell" '(diminish 'flyspell-mode))
;; (eval-after-load "flycheck" '(diminish 'flycheck-mode))
(eval-after-load "cljr" '(diminish 'clj-refactor-mode))
(eval-after-load "subword" '(diminish 'subword-mode))

(defmacro rename-modeline (package-name mode new-name)
  `(eval-after-load ,package-name
     '(defadvice ,mode (after rename-modeline activate)
        (setq mode-name ,new-name))))

(rename-modeline "js2-mode" js2-mode "JS2")
(rename-modeline "clojure-mode" clojure-mode "Clj")
