;; Customizations relating to editing a buffer.

;; magit
(use-package magit
  :ensure t
  :defer t
  :bind (("C-x g" . magit-status))
  :config
  (progn
    (setq magit-save-repository-buffers 'dontask)
    (defadvice magit-status (around magit-fullscreen activate)
      (window-configuration-to-register :magit-fullscreen)
      ad-do-it
      (delete-other-windows))
    (defadvice magit-mode-quit-window (around magit-restore-screen activate)
      (let ((magit-status? (string-match-p "\\magit:.+" (buffer-name))))
        ad-do-it
        (when magit-status?
          (jump-to-register :magit-fullscreen))))))

(use-package abbrev
  :defer t
  :diminish abbrev-mode
  :if (file-exists-p abbrev-file-name)
  :config
  (progn
    (setq save-abbrevs 'silently)
    (setq-default abbrev-mode t)))

(use-package isearch
  :defer t
  :config
  (progn
    ;; Reveal content of subtrees during isearch, alse see reveal-mode
    (setq isearch-invisible 'open)
    ;; Allow deleting chars in the search string, use C-r to search backwards
    (define-key isearch-mode-map [remap isearch-delete-char] 'isearch-del-char)))

(use-package undo-tree
  :ensure t
  :defer t
  :commands (undo-tree)
  :diminish undo-tree-mode
  :init (after-init (turn-on #'global-undo-tree-mode)))

;; company
(use-package company
  :ensure t
  :defer t
  :diminish company-mode
  :commands company-mode
  :init (add-hook 'prog-mode-hook 'company-mode)
  :config
  (progn
    ;; 1. When typing, do not start auto-completion
    ;; 2. On TAB, complete as much as possible inline
    ;; 3. On TAB again, cycle
    ;; 4. Use C-; to open helm company
    (defun overloaded-tab ()
      (interactive)
      (if (and (bound-and-true-p outline-regexp) (looking-at outline-regexp))
          (call-interactively 'outline-cycle)
        (call-interactively 'company-indent-or-complete-common)))
    (bind-key "<tab>" 'overloaded-tab company-mode-map)
    (bind-key "<tab>" 'company-complete-common-or-cycle company-active-map)
    (setq company-tooltip-align-annotations t
          company-auto-complete 'company-explicit-action-p
          company-idle-delay nil
          company-minimum-prefix-length nil
          company-abort-manual-when-too-short t)))

(use-package company-flx
  :ensure t
  :defer t
  :init (after company (company-flx-mode 1)))

(use-package helm-company
  :ensure t
  :defer t
  :init (after company (bind-key "C-;" 'helm-company company-active-map)))

(use-package company-dabbrev
  :defer t
  :config
  (setq company-dabbrev-ignore-case t
	company-dabbrev-ignore-invisible t
	company-dabbrev-downcase nil))

(use-package company-quickhelp
  :ensure t
  :defer t
  :init (after company (bind-key "C-h" 'company-quickhelp-mode company-active-map))
  :config
  (setq
   company-quickhelp-delay 0.4))

(use-package yasnippet
  :ensure t
  :defer t
  :diminish yas-minor-mode
  :commands yas-minor-mode
  :init (add-hook 'prog-mode-hook 'yas-minor-mode)
  :config
  (progn
    (setq yas-snippet-dirs `(,(user-file "snippets"))
	  yas-indent-line 'auto
	  yas-wrap-around-region t)
    (yas-reload-all)))

(use-package auto-yasnippet
  :ensure t
  :defer t
  :commands aya-open-line)

;; git-gutter
(use-package git-gutter
  :ensure t
  :diminish git-gutter-mode
  :config
  (progn
    (global-git-gutter-mode t)
    (add-hook 'git-gutter:update-hooks 'magit-after-revert-hook)
    (add-hook 'git-gutter:update-hooks 'magit-not-reverted-hook)))

;; backup
(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))
(setq auto-save-default nil)
(setq create-lockfiles nil)

;; saveplace
(use-package saveplace
  :defer t
  :init (setq-default save-place t)
  :config (setq save-place-file (user-file "places")))

(use-package savehist
  :defer t
  :init (after-init (turn-on #'savehist-mode))
  :config
  (setq savehist-additional-variables '(search-ring regexp-search-ring)
	savehist-autosave-interval 60
        history-delete-duplicates t
	savehist-file (user-file "savehist")))

;; recentf
(use-package recentf
  :defer t
  :init (after-init (turn-on #'recentf-mode))
  :config
  (setq recentf-max-saved-items 500
	recentf-max-menu-items 50
        rencetf-auto-cleanup 'never))

(use-package autorevert
  :init (after-init (turn-on #'global-auto-revert-mode))
  :config
  (setq auto-revert-interval 2
        auto-revert-check-vc-info nil))

(use-package auto-save-buffers-enhanced
  :ensure t
  :defer t
  :init (after-init (turn-on #'auto-save-buffers-enhanced))
  :config
  (setq auto-save-buffers-enhanced-interval 3
        auto-save-buffers-enhanced-quiet-save-p t))

(use-package hl-line
  :defer t
  :init
  (progn
    (global-hl-line-mode 1)))

(use-package paren
  :defer t
  :init
  (progn
    (show-paren-mode 1)))

(use-package hl-todo
  :ensure t
  :defer t
  :commands hl-todo-mode
  :init (add-hook 'prog-mode-hook 'hl-todo-mode))

(use-package rainbow-delimiters
  :ensure t
  :defer t
  :commands rainbow-delimiters-mode
  :init (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

(use-package paredit
  :ensure t
  :defer t
  :diminish paredit-mode
  :commands paredit-mode
  :preface
  (progn
    (defun minibuffer-paredit-mode-maybe ()
      (if (eq this-command 'eval-expression)
	  (paredit-mode 1)))
    (defun paredit-wrap-round-from-behind ()
      (interactive)
      (forward-sexp -1)
      (paredit-wrap-round)
      (insert " ")
      (forward-char -1))
    (defun paredit-wrap-square-from-behind ()
      (interactive)
      (forward-sexp -1)
      (paredit-wrap-square))
    (defun paredit-wrap-curly-from-behind ()
      (interactive)
      (forward-sexp -1)
      (paredit-wrap-curly)))
  :functions
  (paredit-wrap-round
   paredit-wrap-square
   paredit-wrap-curly)
  :init
  (progn
    (add-hook 'minibuffer-setup-hook 'minibuffer-paredit-mode-maybe)
    (add-hook 'lisps-mode-hook 'paredit-mode)
    (add-hook 'cider-repl-mode-hook 'paredit-mode))
  :config
  (bind-keys
   :map paredit-mode-map
   ("M-(" . paredit-wrap-round)
   ("M-)" . paredit-wrap-round-from-behind)
   ("M-[" . paredit-wrap-square)
   ("M-]" . paredit-wrap-square-from-behind)
   ("M-{" . paredit-wrap-curly)
   ("M-}" . paredit-wrap-curly-from-behind)))

(use-package whitespace
  :init
  (dolist (hook '(prog-mode-hook text-mode-hook))
    (add-hook hook #'whitespace-mode))
  (add-hook 'before-save-hook #'whitespace-cleanup)
  :diminish whitespace-mode
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face tabs empty trailing lines-tail)))

;; comments
(defun toggle-comment-on-line ()
  "comment or uncomment current line"
  (interactive)
  (comment-or-uncomment-region (line-beginning-position) (line-end-position)))
(global-set-key (kbd "C-;") 'toggle-comment-on-line)

;; use 2 spaces for tabs
(defun die-tabs ()
  (interactive)
  (set-variable 'tab-width 2)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

;; fix weird os x kill error
(defun ns-get-pasteboard ()
  "Returns the value of the pasteboard, or nil for unsupported formats."
  (condition-case nil
      (ns-get-selection-internal 'CLIPBOARD)
    (quit nil)))

(setq-default fill-column 80)
(setq-default indent-tabs-mode nil)
(setq-default delete-by-moving-to-trash t)
(setq-default comment-auto-fill-only-comments t)
(delete-selection-mode 1)

(use-package page-break-lines
  :ensure t
  :defer t
  :diminish page-break-lines-mode
  :init
  (after-init (turn-on #'global-page-break-lines-mode)))

(use-package aggressive-indent
  :ensure t
  :defer t
  :commands aggressive-indent-mode-maybe
  :diminish aggressive-indent-mode
  :commands aggressive-indent-mode
  :config
  (progn
    (setq aggressive-indent-comments-too t)
    (add-to-list 'aggressive-indent-excluded-modes 'makefile-mode)
    (add-to-list 'aggressive-indent-excluded-modes 'tuareg-mode)
    (defun aggressive-indent-mode-maybe (flag)
      (if (member major-mode aggressive-indent-excluded-modes)
          (aggressive-indent-mode -1)
        (aggressive-indent-mode flag)))))

(use-package auto-indent-mode
  :ensure t
  :defer t
  :diminish auto-indent-mode
  :config
  (setq auto-indent-indent-style 'aggressive
	auto-indent-blank-lines-on-move nil
	auto-indent-current-pairs nil))

(easy-mmode-define-minor-mode
 savage-indent-mode
 "Savage indent mode"
 nil nil nil
 (progn
   (electric-indent-mode (if savage-indent-mode -1 1))
   (setq show-trailing-whitespace savage-indent-mode)
   (auto-indent-mode savage-indent-mode)
   (aggressive-indent-mode-maybe savage-indent-mode)))

(add-hook 'prog-mode-hook 'savage-indent-mode)
