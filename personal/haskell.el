(require 'haskell-interactive-mode)
(require 'haskell-process)
(custom-set-variables
 '(haskell-process-type 'stack-ghci)
 ;; '(haskell-process-args-stack-ghci '("--package pretty-simple"))
 )

(add-hook 'haskell-mode-hook 'interactive-haskell-mode)

(setq haskell-interactive-mode-eval-mode 'haskell-mode)
