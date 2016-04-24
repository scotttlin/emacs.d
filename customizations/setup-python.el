(use-package python
  :mode (("\\.py\\'" . python-mode)
         ("SConstruct\\'" . python-mode)
         ("SConscript\\'" . python-mode))
  :interpreter ("python" . python-mode)
  :config
  (use-package py-autopep8
    :config
    (add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)))
