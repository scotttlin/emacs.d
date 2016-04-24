;; Markdwon ;;

(use-package markdown-mode :ensure t :mode "\\.md\\'")


(defvar lisps-outline-regexp "^;;;?\s[*]+\s.+")

(defvar lisps-org-headlines-regexp
  "^\\(;;;? \\*+\\)\\(?: +\\(TODO\\|DONE\\|MAYBE\\|DONE\\)\\)?\\(?: +\\(\\[#.\\]\\)\\)?\\(?: +\\(.*?\\)\\)??\\(?:[      ]+\\(:[[:alnum:]_@#%:]+:\\)\\)?[        ]*$")

(defvar outline-regexp-alist
  `((clojure-mode . ,lisps-outline-regexp)
    (emacs-lisp-mode . ,lisps-outline-regexp)))

(defvar org-headings-regexp-alist
  `((clojure-mode . ,lisps-org-headlines-regexp)
    (emacs-lisp-mode . ,lisps-org-headlines-regexp)))

(defun outline-set-regexp ()
  (let ((cons (assoc major-mode outline-regexp-alist)))
    (when cons
      (setq outline-regexp (cdr cons))
      (setq orgstruct-heading-prefix-regexp (cdr cons)))))

(defun org-headings-set-regexp ()
  (let ((cons (assoc major-mode org-headings-regexp-alist)))
    (when cons (setq org-complex-heading-regexp (cdr cons)))))

(add-hook 'prog-mode-hook 'outline-set-regexp)
(add-hook 'prog-mode-hook 'org-headings-set-regexp)

(comment (advice-add #'outline-show-all :after #'recenter))

(use-package flycheck
  :ensure t
  :defer t
  :diminish flycheck-mode
  :commands flycheck-mode
  :init (add-hook 'prog-mode-hook 'flycheck-mode)
  :preface (defvar flycheck-mode-line-lighter " *")
  :config (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc)))

(use-package helm-flycheck :ensure t :defer t)
