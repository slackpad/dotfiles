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

;; C/C++ Setup
(add-hook 'c-mode-common-hook 'flyspell-prog-mode)
(add-hook 'c-mode-common-hook (lambda () (interactive) (column-marker-1 80)))
(setq c-default-style `((c-mode . "stroustrup") (c++-mode . "stroustrup")))
(add-hook 'c-mode-common-hook(lambda() (local-set-key  (kbd "C-c o") 'ff-find-other-file)))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; JavaScript Setup
(setq js-indent-level 2)

;; Backup File Shenanigans
(setq backup-directory-alist `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
