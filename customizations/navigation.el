;; Customizations relating to switching buffers and navigation

(use-package uniquify
  :config
  (setq uniquify-buffer-name-style 'forward)
  (setq uniquify-separator "/")
  ;; rename after killing uniquified
  (setq uniquify-after-kill-buffer-p t)
  ;; don't muck with special buffers
  (setq uniquify-ignore-buffers-re "^\\*"))

(use-package winner
  :defer t
  :init (after-init (turn-on #'winner-mode)) :defer t)

(use-package imenu
  :defer t
  :config (setq-default imenu-auto-rescan t))

(use-package avy-zap :ensure t :defer t)

(use-package avy
  :ensure t
  :defer t
  :commands (avy-goto-char-2)
  :init (avy-setup-default))

;; dired
(use-package dired
  :defer t
  :config
  (progn
    (setq dired-auto-revert-buffer t)
    (add-hook 'dired-mode-hook 'dired-hide-details-mode)))

(use-package helm-flx
  :ensure t
  :defer t
  :init (after helm (helm-flx-mode 1)))

(use-package helm-fuzzier
  :ensure t
  :defer t
  :init (after helm-flx (helm-fuzzier-mode 1)))

(use-package helm
  :ensure t
  :functions (helm-autoresize-mode)
  :commands (helm-M-x
	     helm-mini
	     helm-find-files
	     helm-show-kill-ring)
  :init (after-init (turn-on #'helm-mode))
  :config
  (progn
    ;; Remap persistent action to TAB
    (bind-keys :map helm-map
               ("C-z" . helm-select-action)
               ("<tab>" . helm-execute-persistent-action)
               ("C-i" . helm-execute-persistent-action))
    ;; Window setup
    (setq helm-echo-input-in-header-line t
	  helm-autoresize-max-height 38
	  helm-autoresize-min-height 38
          helm-candidate-number-limit 150
	  helm-split-window-default-side 'below
	  helm-split-window-in-side-p t
	  helm-full-frame nil
	  helm-always-two-windows nil)
    (helm-autoresize-mode 1)
    ;; Fuzzy matching
    (setq helm-recentf-fuzzy-match t
    	  helm-buffers-fuzzy-matching t
    	  helm-locate-fuzzy-match t
    	  helm-M-x-fuzzy-match t
    	  helm-apropos-fuzzy-match t
    	  helm-lisp-fuzzy-completion t
    	  helm-M-x-always-save-history t)
    ;; Sub-packages
    (use-package helm-config :demand t)
    (use-package helm-mode
      :diminish helm-mode
      :defer t
      :config
      (progn
        (setq helm-quick-update t
	      helm-idle-delay 0
	      helm-input-idle-delay 0
	      helm-ff-transformer-show-only-basename t
	      helm-ff-file-name-history-use-recentf t
	      helm-ff-skip-boring-files t
	      helm-M-x-requires-pattern 0)
	(add-to-list 'helm-boring-file-regexp-list "\\.DS_Store$")
	(add-to-list 'helm-boring-file-regexp-list "\\.git$"))
      :bind
      (("C-c h" . helm-mini)
       ("C-h a" . helm-apropos)
       ("C-h r" . helm-info-emacs)
       ("C-x C-b" . helm-buffers-list)
       ("C-x C-f" . helm-find-files)
       ("M-g a" . helm-do-grep-ag)
       ("C-x b" . helm-buffers-list)
       ("M-y" . helm-show-kill-ring)
       ("M-x" . helm-M-x)
       ("C-x c o" . helm-occur)
       ("C-x c SPC" . helm-all-mark-rings)))
    (use-package helm-org
      :defer t
      :commands helm-org-in-buffer-headings)))

(use-package helm-descbinds
  :ensure t
  :defer t
  :bind (("C-h b" . helm-descbinds)))

(use-package helm-unicode :ensure t :defer t)

(use-package helm-ls-git
  :ensure t
  :defer t
  :config
  (progn
    (setq helm-ls-git-show-abs-or-relative 'absolute)))

(use-package helm-dash
  :ensure t
  :defer t
  :defines (helm-dash-docsets)
  :functions (helm-dash-install
              helm-dash-pg
              helm-dash-clojure
              helm-dash-web
              helm-dash-installed-docsets)
  :commands (helm-dash-at-point helm-dash-install)
  :preface
  (progn
    (defvar dash-docsets
      '("Clojure" "Java_SE8" "CSS" "HTML" "Bash" "PostgreSQL" "JavaScript"
        "Markdown"))
    (defun helm-dash-install (docset-name)
      (unless (memberm docset-name (helm-dash-installed-docsets))
        (helm-dash-install-docset docset-name)))
    (defun dash-limit (docsets-names)
      (set (make-local-variable 'helm-dash-docsets) docsets-names))
    (defun helm-dash-bash () (dash-limit '("Bash")))
    (defun helm-dash-pg () (dash-limit '("PostgreSQL")))
    (defun helm-dash-web () (dash-limit '("CSS" "HTML" "JavaScript")))
    (defun helm-dash-clojure () (dash-limit '("Clojure" "Java_SE8"))))
  :init
  (progn
    (after sh-script (add-hook 'sh-mode-hook 'helm-dash-bash))
    (after sql (add-hook 'sql-mode-hook 'helm-dash-pg))
    (after css-mode (add-hook 'css-mode-hook 'helm-dash-web))
    (after html-mode (add-hook 'html-mode-hook 'helm-dash-web))
    (after clojure-mode (add-hook 'clojure-mode-hook 'helm-dash-clojure)))
  :config
  (progn
    (setq helm-dash-browser-func 'eww)
    (dolist (docset dash-docsets)
      (helm-dash-install docset))))

(use-package projectile
  :ensure t
  :defer t
  :commands projectile-golbal-mode
  :init (after-init (turn-on #'projectile-global-mode))
  :diminish projectile-mode
  :config
  (progn
    (setq
     projectile-cache-file (user-file "projectile.cache")
     projectile-known-projects-file (user-file "projectile-bookmarks.eld")
     projectile-completion-system 'helm
     projectile-use-git-grep t
     projectile-switch-project-action 'projectile-find-file
     projectile-globally-ignored-files
     (append projectile-globally-ignored-directories '("elpa" "node-modules")))
    (add-to-list 'projectile-globally-ignored-files ".DS_Store")
    (add-to-list 'projectile-project-root-files-bottom-up "project.clj")
    (defadvice projectile-replace
	(before projectile-save-all-and-replace activate)
      (save-some-buffers t))
    (defadvice projectile-switch-project (after projectile-sync-default-directory)
      (when (projectile-project-p)
        (setq default-directory (projectile-project-root))))))

(use-package ag
  :ensure t)
