;; Global Setup
(add-to-list 'load-path "~/.emacs.d/lisp" t)

;; Package Manager
(when (>= emacs-major-version 24)
  (require 'package)
  (package-initialize)
  (add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
  )

;; Enable CUA mode but just for the rectangular selection features.
(setq cua-enable-cua-keys nil)
(cua-mode t)

;; Required for color-theme in newer versions of emacs.
(defun plist-to-alist (the-plist)
  (defun get-tuple-from-plist (the-plist)
    (when the-plist
      (cons (car the-plist) (cadr the-plist))))

  (let ((alist '()))
    (while the-plist
      (add-to-list 'alist (get-tuple-from-plist the-plist))
      (setq the-plist (cddr the-plist)))
  alist))

;; UI Tweaks
(menu-bar-mode 0)
(show-paren-mode 1)
(load "color-theme.el")
(load "zenburn.el")
(zenburn)

;; Integration with ack-grep
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

;; Code Writing OCD Features
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace 1)
(setq column-number-mode t)
(load "column-marker.el")

;; Go Setup
(add-hook 'go-mode-hook 'flyspell-prog-mode)
(add-hook 'go-mode-hook (lambda () (interactive) (column-marker-1 80)))
(add-hook 'go-mode-hook (lambda () (local-set-key (kbd "M-.") #'godef-jump)))
(add-hook 'before-save-hook #'gofmt-before-save)

;; C/C++ Setup
(add-hook 'c-mode-common-hook 'flyspell-prog-mode)
(add-hook 'c-mode-common-hook (lambda () (interactive) (column-marker-1 80)))
(setq c-default-style `((c-mode . "stroustrup") (c++-mode . "stroustrup")))
(add-hook 'c-mode-common-hook(lambda() (local-set-key  (kbd "C-c o") 'ff-find-other-file)))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; JavaScript Setup
(setq js-indent-level 2)

;; HTML Setup
(dolist (hook '(html-mode-hook))
      (add-hook hook (lambda () (flyspell-mode 1))))

;; Backup File Shenanigans
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))

;; Better git-grep (http://lethalman.blogspot.com/2014/05/grep-in-git-with-emacs.html)
(defun git-grep (regexp dir)
  (interactive
   (progn
     (grep-compute-defaults)
     (cond
      ((equal current-prefix-arg '(16))
       (list (read-from-minibuffer "Run: " "git grep" nil nil 'grep-history)
    nil))
      (t (let* ((regexp (grep-read-regexp))
    (dir (read-directory-name "In directory: " nil default-directory t)))
     (list regexp dir))))))
  (require 'grep)
  (when (and (stringp regexp) (> (length regexp) 0))
    (let ((command regexp))
      (if (> 4 5)
    (if (string= command "git grep")
     (setq command nil))
  (setq dir (file-name-as-directory (expand-file-name dir)))
  (setq command
     (grep-expand-template "git grep -n -i -e <R>" regexp))
  (when command
    (if (equal current-prefix-arg '(4))
     (setq command
     (read-from-minibuffer "Confirm: " command nil nil 'grep-history))
   (add-to-history 'grep-history command))))
      (when command
  (let ((default-directory dir)
     (compilation-environment '("PAGER=")))
    ;; Setting process-setup-function makes exit-message-function work
    ;; even when async processes aren't supported.
    (compilation-start command 'grep-mode))
  (if (eq next-error-last-buffer (current-buffer))
   (setq default-directory dir))))))
