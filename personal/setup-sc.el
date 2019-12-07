(require 'sclang)
(add-hook 'sclang-mode-hook 'sclang-extensions-mode)
(define-key sclang-extensions-mode-map (kbd "C-c C-c") 'sclang-eval-defun)
