;; Global Setup
(add-to-list 'load-path "~/.emacs.d" t)

;; Enable CUA mode but just for the rectangular selection features.
(setq cua-enable-cua-keys nil)
(cua-mode t)

;; UI Tweaks
(menu-bar-mode 0)
(show-paren-mode 1)
(load "color-theme.el")
(load "zenburn.el")

;; Integration with ack-grep
(autoload 'ack-same "full-ack" nil t)
(autoload 'ack "full-ack" nil t)
(autoload 'ack-find-same-file "full-ack" nil t)
(autoload 'ack-find-file "full-ack" nil t)

;; Go Language Support
(require 'go-mode-load)

;; Code Writing OCD Features
(setq-default indent-tabs-mode nil)
(setq-default show-trailing-whitespace 1)
(setq column-number-mode t)
(setq c-default-style
  `((c-mode . "stroustrup") (c++-mode . "stroustrup")))
(load "column-marker.el")
(add-hook 'c-mode-common-hook (lambda () (interactive) (column-marker-1 80)))
(add-hook 'c-mode-common-hook
  (lambda()
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

;; Backup File Shenanigans
(setq backup-directory-alist
      `((".*" . ,temporary-file-directory)))
(setq auto-save-file-name-transforms
      `((".*" ,temporary-file-directory t)))
