;; GUI customization

;; Frames
(tool-bar-mode -1)
(scroll-bar-mode -1)
(menu-bar-mode -1)
(setq use-file-dialog nil
          use-dialog-box nil)

;;; Screen maximization and full-screen
;; Http://www.emacswiki.org/emacs/FullScreen
;; (set-frame-parameter nil 'fullscreen 'fullboth)
(setq initial-frame-alist
      '((fullscreen . maximized)))

;; Fonts
(setq-default line-spacing nil)
(setq mac-allow-anti-aliasing t)
(global-prettify-symbols-mode t)
(add-to-list 'default-frame-alist '(font . "Fira Code-11"))
(set-face-attribute 'default t :font "Fira Code-11")

(use-package fringe
  :init (fringe-mode 4)
  :config
  (progn
    (setq indicate-empty-lines t
          indicate-buffer-boundaries t
          indicate-unused-lines t)
    (setf (cdr (assq 'continuation fringe-indicator-alist))
          '(nil right-curly-arrow))))

;; Better scrolling
(setq redisplay-dont-pause t
      scroll-margin 1
      scroll-step 1
      scroll-conservatively 10000
      scroll-preserve-screen-position 1)

(setq mouse-wheel-follow-mouse 't
      mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq-default cursor-type 'bar)
(setq cursor-in-non-selected-windows nil)

(setq
      x-select-enable-clipboard t
      x-select-enable-primary t
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; full path in title bar
(setq-default frame-title-format "%b (%f)")

;; no bell
(setq ring-bell-function 'ignore)

;; open files in existing frame
(setq ns-pop-up-frames nil)

;; Themes
(use-package afternoon-theme
  :ensure afternoon-theme
  :init (after-init (lambda () (load-theme 'afternoon t nil))))

(use-package custom
  :defer t
  :config
  (setq custom-theme-directory
	(expand-file-name "themes" user-emacs-directory)))

(comment
 (use-package zenburn-theme :ensure t)
 (use-package spacegray-theme :ensure t)
 (use-package spacemacs-theme :ensure t)
 (use-package flatland-theme :ensure t))

(use-package spaceline
  :ensure spaceline
  :defer t
  :functions (spaceline-spacemacs-theme)
  :init (use-package spaceline-config
          :config
          (setq powerline-default-separator 'wave
                spaceline-workspace-numbers-unicode t
                spaceline-window-numbers-unicode t)
          (spaceline-spacemacs-theme)
          (spaceline-helm-mode)
          (spaceline-info-mode))
  :config
  (advice-add 'load-theme :after (lambda (theme &optional no-confirm no-enable) (powerline-reset))))

(use-package helm-themes :ensure t :defer t)

(use-package rainbow-mode
  :ensure t
  :defer t
  :commands rainbow-mode
  :diminish rainbow-mode
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'rainbow-mode)
    (add-hook 'css-mode-hook 'rainbow-mode)
    (after web-mode (add-hook 'web-mode-hook 'rainbow-mode))))
