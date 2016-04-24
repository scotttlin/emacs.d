;; Emacs Lisp

(use-package eldoc
  :ensure t
  :defer t
  :diminish eldoc-mode
  :init (add-hook 'prog-mode-hook 'eldoc-mode))

(defvar lisps-mode-map (make-keymap))

(defun lisps-pretty ()
  (progn
    (push '("<=" . ?≤) prettify-symbols-alist)
    (push '(">=" . ?≥) prettify-symbols-alist)
    (push '("lambda" . ?λ) prettify-symbols-alist)
    (push '("fn" . ?λ) prettify-symbols-alist)
    (turn-on-prettify-symbols-mode)))

(use-package subword :defer t :diminish subword-mode)

(easy-mmode-define-minor-mode
 lisps-mode "Minor mode for lisp-family languages"
 :keymap lisps-mode-map
 (progn
   (lisps-pretty)
   (eldoc-mode 1)
   (subword-mode 1)))

(use-package lisp-mode
  :defer t
  :preface
  (defun imenu-add-use-package ()
    "Recognize `use-package` in imenu"
    (when (string= buffer-file-name (expand-file-name "init.el" "~/.emacs.d"))
      (add-to-list
       'imenu-generic-expression
       '("Packages" "^\\s-*(\\(use-package\\)\\s-+\\(\\(\\sw\\|\\s_\\)+\\)" 2)
       t)))
  :config
  (progn
    (bind-key "C-c C-k" 'eval-buffer emacs-lisp-mode-map)
    (add-hook 'emacs-lisp-mode-hook 'imenu-add-use-package)
    (add-hook 'emacs-lisp-mode-hook (turn-on #'lisps-mode))
    (add-hook 'ielm-mode-hook (turn-on #'lisps-mode))))
