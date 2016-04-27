(use-package python
  :mode (("\\.py\\'" . python-mode)
         ("SConstruct\\'" . python-mode)
         ("SConscript\\'" . python-mode))
  :interpreter ("python" . python-mode)
  :config
  (use-package anaconda-mode
    :ensure t
    :defer t
    :commands anaconda-mode
    :diminish anaconda-mode
    :config
    (progn
      (add-hook 'python-mode-hook 'anaconda-mode)
      (add-hook 'python-mode-hook 'eldoc-mode)))

  (use-package company-anaconda
    :ensure t
    :defer t
    :init
    (after company-mode (add-to-list 'company-backends 'company-anaconda))))
