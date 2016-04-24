;; Clojure/Cider

(use-package clojure-mode
  :ensure t
  :defer t
  :config (add-hook 'clojure-mode-hook (turn-on #'lisps-mode))
  :mode (("\\.edn$" . clojure-mode)
         ("\\.boot$" . clojure-mode)
         ("\\.cljs.*$" . clojure-mode)))

(use-package clojure-mode-extra-font-locking
  :ensure t
  :defer t
  :init (after clojure-mode (require 'clojure-mode-extra-font-locking nil t)))

(use-package cider
  :ensure t
  :defer t
  :preface
  (progn
    (defun cider-repl-redo-last-input ()
      (interactive)
      (save-window-excursion
        (cider-switch-to-default-repl-buffer)
        (cider-repl-previous-input)
        (cider-repl-return)))
    (defun cider-user-ns ()
      (interactive)
      (cider-repl-set-ns "user")))
  :functions (cider-repl-redo-last-input
              cider-user-ns)
  :config
  (progn
    (add-hook 'cider-mode-hook 'cider-turn-on-eldoc-mode)
    (add-hook 'cider-repl-mode-hook 'cider-turn-on-eldoc-mode)
    (add-hook 'cider-repl-mode-hook #'paredit-mode)
    (bind-key "C-c C-t" 'cider-test-jump clojure-mode-map)
    (bind-key "C-c RET" 'cider-repl-redo-last-input cider-mode-map)
    (bind-key "C-M-r" 'cider-refresh clojure-mode-map)
    (bind-key "C-c u" 'cider-user-ns clojure-mode-map)
    (bind-key "C-c u" 'cider-user-ns cider-mode-map)
    (bind-key "C-c r" 'cider-switch-to-repl-buffer cider-mode-map)
    (setq cider-prompt-save-file-on-load nil
          cider-prompt-for-project-on-connect nil
          cider-prompt-for-symbol nil
          cider-show-error-buffer t
          cider-auto-jump-to-error nil
          nrepl-hide-special-buffers nil
          cider-repl-use-pretty-printing t
          cider-overlays-use-font-lock t
          cider-repl-history-file (user-file "cider-history")
          cider-repl-wrap-history t)
    (use-package cider-repl
      :defer t
      :config
      (progn
        (add-hook 'cider-repl-mode-hook 'company-mode)
        (add-hook 'cider-repl-mode-hook (turn-on #'lisps-mode))
        (setq cider-repl-pop-to-buffer-on-connect t)))))

(use-package clj-refactor
  :ensure t
  :defer t
  :diminish clj-refactor-mode
  :init (add-hook 'clojure-mode-hook 'clj-refactor-mode)
  :config
  (progn
    (cljr-add-keybindings-with-prefix "C-c C-m")
    (yas-minor-mode 1)
    (use-package cljr-helm
      :ensure t
      :defer t
      :init
      (after clojure-mode
        (bind-key "C-c <RET> h" 'clojure-mode-map)))))

(defun clj-align-vectors (beg end)
  (interactive "r")
  (align-regexp beg end "^ \\[[^ ]+\\(\\s-+\\)" 1 1 t))
