;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.


;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html
(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(setq recentf-save-file (concat user-emacs-directory ".recentf"))
(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 40)


;; ido-mode allows you to more easily navigate choices. For example,
;; when you want to switch buffers, ido presents you with a list
;; of buffers in the the mini-buffer. As you start to type a buffer's
;; name, ido will narrow down the list of buffers to match the text
;; you've typed in
;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(ido-mode t)

;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
(setq ido-enable-flex-matching t)

;; Turn this behavior off because it's annoying
(setq ido-use-filename-at-point nil)

;; Don't try to match file across all "work" directories; only match files
;; in the current directory displayed in the minibuffer
(setq ido-auto-merge-work-directories-length -1)

;; Includes buffer names of recently open files, even if they're not
;; open now
(setq ido-use-virtual-buffers t)

;; This enables ido in all contexts where it could be useful, not just
;; for selecting buffer and file names
;; (ido-ubiquitous-mode t)
;; (ido-everywhere t)

;; Shows a list of buffers
(global-set-key (kbd "C-x C-b") 'ibuffer)


;; Enhances M-x to allow easier execution of commands. Provides
;; a filterable list of possible commands in the minibuffer
;; http://www.emacswiki.org/emacs/Smex
(setq smex-save-file (concat user-emacs-directory ".smex-items"))
(smex-initialize)
(global-set-key (kbd "M-x") 'smex)

;; projectile everywhere!
(projectile-global-mode)



(require 'find-file-in-project)
(require 's)

;; No need to be stingy
(setq ffip-limit 4096)

;; Use full project path for ffip

(defun ffip-project-files ()
  "Return an alist of all filenames in the project and their path."
  (let ((file-alist nil))
    (mapcar (lambda (file)
              (let ((file-cons (cons (s-chop-prefix (file-truename (ffip-project-root)) (expand-file-name file))
                                     (expand-file-name file))))
                (add-to-list 'file-alist file-cons)
                file-cons))
            (split-string (shell-command-to-string
                           (format "find %s -type f \\( %s \\) %s | head -n %s"
                                   (or ffip-project-root
                                       (ffip-project-root)
                                       (error "No project root found"))
                                   (ffip-join-patterns)
                                   ffip-find-options
                                   ffip-limit))))))

;; Helper methods to create local settings

(defun ffip--create-exclude-find-options (names)
  (mapconcat (lambda (name)
               (concat "-not -regex \".*" name ".*\"")) names " "))

(defun ffip-local-excludes (&rest names)
  "Given a set of names, will exclude results with those names in the path."
  (set (make-local-variable 'ffip-find-options)
       (ffip--create-exclude-find-options names)))

(defun ffip-local-patterns (&rest patterns)
  "An exhaustive list of file name patterns to look for."
  (set (make-local-variable 'ffip-patterns) patterns))

;; Function to create new functions that look for a specific pattern
(defun ffip-create-pattern-file-finder (&rest patterns)
  (lexical-let ((patterns patterns))
    (lambda ()
      (interactive)
      (let ((ffip-patterns patterns))
        (find-file-in-project)))))

;; Default excludes - override with ffip-local-excludes

(setq-default ffip-find-options
              (ffip--create-exclude-find-options
               '("/node_modules/"
                 "/bower_components/"
                 "/target/"
                 "/out/"
                 "/tests-out/"
                 "/overlays/"
                 "/build/"
                 "/dist/"
                 "/vendor/"
                 ".cask/"
                 "/generated/"
                 "/resources/public/js/compiled/"
                 "/.repl/"
                 "/.tmp/")))


(provide 'setup-ffip)
(global-unset-key (kbd "C-x C-o")) ;; which used to be delete-blank-lines (also bound to C-c C-<return>)
(global-set-key (kbd "C-x C-o js") (ffip-create-pattern-file-finder "*.js"))
(global-set-key (kbd "C-x C-o jn") (ffip-create-pattern-file-finder "*.json"))
(global-set-key (kbd "C-x C-o ht") (ffip-create-pattern-file-finder "*.html"))
(global-set-key (kbd "C-x C-o cs") (ffip-create-pattern-file-finder "*.css"))
(global-set-key (kbd "C-x C-o cl") (ffip-create-pattern-file-finder "*.clj"))
(global-set-key (kbd "C-x C-o el") (ffip-create-pattern-file-finder "*.el"))
(global-set-key (kbd "C-x C-o ed") (ffip-create-pattern-file-finder "*.edn"))
(global-set-key (kbd "C-x C-o md") (ffip-create-pattern-file-finder "*.md"))
