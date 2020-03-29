';;; package --- summary
;;; Code:
;;; Commentary:

;; Bootstrap use-package
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

;; Require use-package
(eval-when-compile
  (require 'use-package))

;; Use-package config
(use-package use-package
  :config
  (setq-default use-package-always-defer t
                use-package-always-demand nil
                use-package-always-ensure t
                use-package-verbose t))

(use-package centaur-tabs
  :demand
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-c <left>" . centaur-tabs-backward)
  ("C-c <right>" . centaur-tabs-forward))

(use-package magit
  :bind
  ("C-x g" . magit-status))

(use-package which-key
  :init
  (which-key-mode))

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled)))

;;(use-package eldoc-mode
;;  :hook (after-init . global-eldoc-mode))

(use-package lsp-mode
  :commands lsp
  :custom
  (lsp-auto-guess-root t)
  (lsp-log-io t)
  (lsp-prefer-flymake nil))

(use-package lsp-ui
  :hook (lsp-mode . lsp-ui-mode)
  :custom
  (lsp-ui-doc-delay 0)
  (lsp-io-doc-include-signature t)
  (lsp-ui-flycheck-enable t)
  (lsp-ui-peek-always-show t))

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package treemacs
  :bind (:map global-map
	      ("C-x t t" . treemacs)
	      ("C-x t 1" . treemacs-select-window))
  :custom
  (treemacs-resize-icons 15))

;;(use-package lsp-treemacs
;;  :bind (:map java-mode-map
;;	      ("C-x e l" . lsp-treemacs-errors-list)
;;	      ("C-x s l" . lsp-treemacs-symbols))
;;  :init
;;  (lsp-treemacs-sync-mode 1))

(use-package company
  :hook (after-init . global-company-mode)
  :custom
  (company-tooltip-align-annotations t)
  (company-require-match 'never)
  (company-dabbrev-downcase nil)
  (company-idle-align-annotations t)
  (company-idle-delay 0)
  (company-minimum-prefix-length 1)
  :bind
  ("C-ä" . company-complete))

(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-ä") 'company-other-backend))

(use-package company-lsp
  :after (company lsp-mode)
  :custom
  (company-lsp-cache-candidates t))

(use-package emmet-mode
  :hook ((css-mode php-mode sgml-mode rjsx-mode web-mode) . emmet-mode))

(use-package add-node-modules-path
  :hook ((web-mode rjsx-mode) . add-node-modules-path))

;; Python mode
;; (use-package python
;;   :ensure nil
;;   :hook (python-mode . lsp)
;;   :custom
;;   (python-indent-guess-indent-offset-verbose nil)
;;   (python-fill-docstring-style 'pep-257-nn))

(use-package anaconda-mode
  :ensure t
  :init (add-hook 'python-mode-hook 'anaconda-mode)
        ;;(add-hook 'python-mode-hook 'anaconda-eldoc-mode)
  :config (use-package company-anaconda
            :ensure t
            :init (add-hook 'python-mode-hook 'anaconda-mode)
            (eval-after-load "company"
              '(add-to-list 'company-backends '(company-anaconda :with company-capf)))))

(use-package pyvenv)

(setq python-shell-interpreter "/usr/bin/python3")

;; Multi cursor config
(use-package multiple-cursors
  :bind
  ("C-<" . mc/mark-next-like-this)
  ("C->" . mc/mark-previous-like-this)
  ("C-c C-<mouse-1>" . mc/add-cursor-on-click)
  ("C-c C-<" . mc/mark-all-like-this))

;; Dired mode icons
(use-package all-the-icons)
(use-package all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)
	   
;; Modeline
(use-package doom-modeline
  :config
  (doom-modeline-mode t))

;; Prompt for restart emacs
(defun lrg/restart-emacs ()
  "Query for restarting Emacs."
  (setq prompt (y-or-n-p "Do you really want to restart Emacs? "))
  (if prompt
    (restart-emacs)))

;; Add restart emacs
(use-package restart-emacs
  :bind
  ("C-c C-q" . (lambda () (interactive) (lrg/restart-emacs)))
  :custom
  (restart-emacs-restore-frames t))

;; Move stuff around
(use-package drag-stuff
  :bind
  ("M-<up>" . drag-stuff-up)
  ("M-<down>" . drag-stuff-down))

;; Ivy config
(use-package ivy
  :config
  (ivy-mode 1)
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t))


;;; package.el ends here
