(setq dark-theme__ 'subatomic256)
(setq light-theme__ 'spacemacs-light)
(setq prelude-theme dark-theme__)

(defun light-theme ()
  (interactive)
  (disable-theme dark-theme__)
  (load-theme light-theme__))

(defun dark-theme ()
  (interactive)
  (disable-theme light-theme__)
  (load-theme dark-theme__))
