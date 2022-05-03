(global-wakatime-mode)


(require 'yasnippet)
(add-hook 'emacs-startup-hook
          (lambda () (yas-load-directory "~/.emacs.d/snippets")))
