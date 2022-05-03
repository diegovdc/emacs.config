(setq dark-theme__ 'spacemacs-dark)

(setq light-theme__ 'spacemacs-light)
(setq prelude-theme dark-theme__)

(defun light-theme ()
  (interactive)
  (disable-theme dark-theme__)
  (load-theme light-theme__))

(defun dark-theme ()
  (interactive)
  (disable-theme light-theme__)
  (load-theme dark-theme__)
  ;; green-phosphore customizations
  (set-cursor-color "white")
  (set-face-attribute 'region nil :background "#fff8a6" :foreground "black")
  (set-face-attribute 'font-lock-comment-face nil :foreground "#999")
  (set-face-attribute 'highlight nil :background "#333" :foreground 'unspecified))
