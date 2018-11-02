;; init.el --- Scott's Emacs configuration

;; Packages
(require 'package)
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/") t)

(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

;; Always load newest byte code
(setq load-prefer-newer t)

(package-initialize)

;; https://github.com/jwiegley/use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))
(require 'use-package)
(setq use-package-verbose t)

(setq inhibit-startup-message t
      initial-scratch-message "")

;; reduce the frequency of garbage collection by making it happen on
;; each 50MB of allocated data (the default is on every 0.76MB)
(setq gc-cons-threshold 50000000)

;; warn when opening files bigger than 100MB
(setq large-file-warning-threshold 100000000)

(defmacro after (mode &rest body)
  "`eval-after-load' `MODE', wrapping `BODY' in `progn'."
  (declare (indent defun))
  (let ((s (if (symbolp mode)
               (symbol-name mode)
             mode)))
    `(eval-after-load ,(symbol-name mode)
       (quote (progn ,@body)))))

(defun turn-on    (f) `(lambda () (funcall #',f +1)))
(defun turn-off   (f) `(lambda () (funcall #',f -1)))
(defun after-init (f) (if after-init-time (funcall f) (add-hook 'after-init-hook f)))
(defmacro comment (&rest body))

(defun user-file (name)
  "Return a file with `NAME' in `user-emacs-directory'."
  (expand-file-name name user-emacs-directory))

(defun debug-init ()
  (toggle-debug-on-error)
  (after-init 'toggle-debug-on-error))

(use-package paradox
  :ensure t
  :defer t
  :commands paradox-enable
  :init (after-init #'paradox-enable)
  :config
  (setq paradox-execute-asynchronously t
        paradox-automatically-star nil))

(fset 'yes-or-no-p 'y-or-n-p)

;; For loading files outside of package system
(add-to-list 'load-path "~/.emacs.d/vendor")

;; Customization ;;

(add-to-list 'load-path "~/.emacs.d/customizations")
(load "ui.el")
(load "navigation.el")
(load "editing.el")
(load "misc.el")
;; (load "org.el)

;; Languages ;;

(load "setup-elisp.el")
;; (load "setup-ruby.el")
(load "setup-python.el")
(load "setup-clojure.el")
;; (load "setup-web.el)
(load "setup-markdown.el")
(load "setup-shell.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (helm-flycheck flycheck markdown-mode cljr-helm clj-refactor cider clojure-mode-extra-font-locking clojure-mode exec-path-from-shell auto-indent-mode aggressive-indent page-break-lines rainbow-delimiters hl-todo auto-save-buffers-enhanced git-gutter auto-yasnippet yasnippet company-quickhelp helm-company company-flx company undo-tree magit ag projectile helm-dash helm-ls-git helm-unicode helm-descbinds helm-fuzzier helm-flx avy-zap rainbow-mode helm-themes spaceline afternoon-theme use-package paradox))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
