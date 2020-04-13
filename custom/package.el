;;; package --- summary
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
  :init
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
  :init
  (setq flycheck-checker-error-threshold 2000)
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled)))

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

;; Treemacs
(use-package treemacs
  :bind (:map global-map
	      ("C-x t t" . treemacs)
	      ("C-x t 1" . treemacs-select-window))
  :init
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc
        treemacs-persist-file "~/.emacs.d/cache/treemacs-persist"
        treemacs-last-error-persist-file "~/.emacs.d/cache/treemacs-last-error-persist")
  :config
  (treemacs-follow-mode -1)
  (treemacs-git-mode 'deferred)
  :custom
  (treemacs-resize-icons 15))

(use-package treemacs-magit
  :after treemacs magit)

(use-package lsp-treemacs
 :init
 (lsp-treemacs-sync-mode 1))

;; Company
(use-package company
  :hook (after-init . global-company-mode)
  :config
  (add-to-list 'company-backends 'company-yasnippet t)
  :init
  (setq company-tooltip-align-annotations t
        company-require-match 'never
        company-dabbrev-downcase nil
        company-idle-align-annotations t
        company-idle-delay 0
        company-minimum-prefix-length 1))

;; Set company-capf as this opens the company menu on completions
(global-set-key (kbd "C-ä") 'company-capf)

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

;; PHP mode
(use-package php-mode)

;; Web
(use-package web-mode)

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
  :defer 0.1
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
  ("C-c q" . (lambda ()
               (interactive)
               (lrg/restart-emacs)))
;;  :custom
;;  (restart-emacs-restore-frames t)
)

;; Add which-key tag to restart emacs
(which-key-add-key-based-replacements
  "C-c q" "Restart Emacs")

;; Move stuff around
(use-package drag-stuff
  :bind
  ("M-<up>" . drag-stuff-up)
  ("M-<down>" . drag-stuff-down))

;; Ivy config
(use-package ivy
  :defer 0.1
  :config
  (ivy-mode t)
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t))

;; Rainbow mode
(use-package rainbow-mode
  :hook ((js-mode . rainbow-mode)
	 (web-mode . rainbow-mode)
	 (php-mode . rainbow-mode)
	 (emacs-lisp-mode . rainbow-mode)
	 (python-mode . rainbow-mode)))

;; Dashboard
(use-package dashboard
  :init
  (dashboard-setup-startup-hook)
  :config
  (setq dashboard-startup-banner 4
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        ;; Set custom ascii art dir path
        ;; To make new banners, add them as [number].txt, it won't read strings!
        dashboard-banners-directory "~/.emacs.d/custom/themes/banners/"
        dashboard-page-separator "\n\n"
        dashboard-items '((recents   . 5)
                          (projects  . 5)
                          (bookmarks . 5))))

;; Undo-Tree
(use-package undo-tree
  :config
  (setq undo-tree-visualizer-diff t
        undo-tree-auto-save-history t
        undo-tree-enable-undo-in-region t
        ;; Increase undo-limits by a factor of ten to avoid emacs prematurely
        ;; truncating the undo history and corrupting the tree. See
        ;; https://github.com/syl20bnr/spacemacs/issues/12110
        undo-limit 800000
        undo-strong-limit 12000000
        undo-outer-limit 120000000
        undo-tree-history-directory-alist
        `(("." . ,"~/.emacs.d/cache/undo-tree-hist/")))
  (global-undo-tree-mode +1))

;; Yasnippet
(use-package yasnippet
  :config
  (yas-global-mode t))

(use-package yasnippet-snippets)

;; Smartparens
(use-package smartparens
  :hook
  (prog-mode . smartparens-mode))

;; Hl tosdo highlight
(use-package hl-todo
  :hook
  (prog-mode . hl-todo-mode)
  :config
  (setq hl-todo-highlight-punctuation ":"
        hl-todo-keyword-faces
        `(("TODO"       warning bold)
          ("FIXME"      error bold)
          ("HACK"       font-lock-constant-face bold)
          ("REVIEW"     font-lock-keyword-face bold)
          ("NOTE"       success bold)
          ("DEPRECATED" font-lock-doc-face bold))))

;; HL selections
(use-package hl-anything
  :hook
  (prog-mode . hl-highlight-mode))

;; Rainbow delimiter conf
(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

;; Projectile
(use-package projectile
   :init
   (projectile-mode t)
   :bind
   ("C-c p" . projectile-command-map))


;;; package.el ends here