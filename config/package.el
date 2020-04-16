;; package --- summary
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


;;; ---- VERSION CONRTOL & PROJECTS ----

(use-package magit
  :bind
  ("C-x g" . magit-dispatch)
  ("C-c m" . magit-status))

(use-package projectile
  :demand t
  :config
  (projectile-mode t)
  :bind
  ("C-c p" . projectile-command-map))

;;; ---- UI ----

(use-package centaur-tabs
  :demand t
  :config
  (centaur-tabs-mode t)
  :bind
  ("C-c <left>" . centaur-tabs-backward)
  ("C-c <right>" . centaur-tabs-forward))

(use-package treemacs
  :bind (:map global-map
	      ("C-x t t" . treemacs)
	      ("C-x t 1" . treemacs-select-window))
  :init
  (setq treemacs-follow-after-init t
        treemacs-is-never-other-window t
        treemacs-sorting 'alphabetic-case-insensitive-asc)
  :config
  (treemacs-follow-mode -1)
  (treemacs-git-mode 'deferred)
  :custom
  (treemacs-resize-icons 15))

(use-package treemacs-magit
  :after treemacs magit)

(use-package lsp-treemacs
  :demand t
  :config
  (lsp-treemacs-sync-mode 1))

(use-package all-the-icons)
(use-package all-the-icons-dired)
(add-hook 'dired-mode-hook 'all-the-icons-dired-mode)

(use-package doom-modeline
  :demand t
  :config
  (doom-modeline-mode t))

(use-package dashboard
  :demand t
  :config
  (dashboard-setup-startup-hook)
  :config
  (setq dashboard-startup-banner 4
        dashboard-center-content t
        dashboard-set-heading-icons t
        dashboard-set-file-icons t
        dashboard-page-separator "\n\n"
        dashboard-items '((recents   . 5)
                          (projects  . 5)
                          (bookmarks . 5))))

;;; ---- HIGHLIGHTING ----

(use-package smartparens
  :hook
  (prog-mode . smartparens-mode))

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

(use-package hl-anything
  :hook
  (prog-mode . hl-highlight-mode))

(use-package rainbow-delimiters
  :hook
  (prog-mode . rainbow-delimiters-mode))

(use-package rainbow-mode
  :hook
  (prog-mode . rainbow-mode))

;;; ---- QUALITY OF LIFE ----

(use-package which-key
  :demand t
  :config
  (which-key-mode t))

;; Prompt for restart emacs
(defun lrg/restart-emacs ()
  "Query for restarting Emacs."
  (setq lrq/prompt (y-or-n-p "Do you really want to restart Emacs? "))
  (if lrq/prompt
    (restart-emacs)))

(which-key-add-key-based-replacements
  "C-c q" "Restart Emacs")
(use-package restart-emacs
  :bind
  ("C-c q" . (lambda ()
               (interactive)
               (lrg/restart-emacs))))

(use-package drag-stuff
  :bind
  ("M-<up>" . drag-stuff-up)
  ("M-<down>" . drag-stuff-down))

(use-package multiple-cursors
  :bind
  ("C-<" . mc/mark-next-like-this)
  ("C->" . mc/mark-previous-like-this)
  ("C-c C-<mouse-1>" . mc/add-cursor-on-click)
  ("C-c C-<" . mc/mark-all-like-this))

(use-package undo-tree
  :demand t
  :config
  (global-undo-tree-mode t)
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
        `(("." . ,"~/.emacs.d/local/undo-tree-hist/"))))


;;; ---- MINIBUFFER ----

(use-package dap-mode
  :after lsp-mode
  :config
  (dap-mode t)
  (dap-ui-mode t))

(use-package ivy
  :demand t
  :config
  (ivy-mode t)
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t))

;;; ---- SYNTAX CHECKING & AUTOCOMPLETION ----

(use-package flycheck
  :hook (after-init . global-flycheck-mode)
  :init
  (setq flycheck-checker-error-threshold 2000)
  :config
  (setq-default flycheck-disabled-checkers
              (append flycheck-disabled-checkers
                      '(javascript-jshint json-jsonlist)))
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

(use-package company
  :hook
  (prog-mode . global-company-mode)
  :config
  (add-to-list 'company-backends 'company-yasnippet t)
  :init
  (setq company-tooltip-align-annotations t
        company-require-match 'never
        company-dabbrev-downcase nil
        company-idle-align-annotations t
        company-idle-delay 0
        company-minimum-prefix-length 1))

;; Set company-capf as this always tries to open the company menu
(global-set-key (kbd "C-ä") 'company-capf)

;; When company is active change this to switch backends
(with-eval-after-load 'company
  (define-key company-active-map (kbd "C-ä") 'company-other-backend))

(use-package company-lsp
  :after (company lsp-mode)
  :custom
  (company-lsp-cache-candidates t))

(use-package yasnippet
  :hook
  (prog-mode . yas-global-mode))

(use-package yasnippet-snippets)

;;; ---- LANGUAGES ----

;; PYTHON
(use-package anaconda-mode
  :hook
  (python-mode-hook . (anaconda-mode anaconda-eldoc-mode))
  :config
  (pyvenv-mode t)
  (use-package company-anaconda
    :hook
    (python-mode-hook . anaconda-mode)
    :init
    (eval-after-load "company"
      '(add-to-list 'company-backends '(company-anaconda :with company-capf)))))

;; PHP
(use-package php-mode
  :config
  (use-package company-php
    :init
    (eval-after-load "company"
      '(add-to-list 'company-backends '(company-ac-php-backend :with company-capf)))))



;; HTML & CSS
(use-package web-mode
  :init
  (setq web-mode-markup-indent-offset 4))

(use-package emmet-mode
  :hook
  ((css-mode php-mode sgml-mode rjsx-mode web-mode) . emmet-mode))

;; JAVASCRIPT
(use-package add-node-modules-path
  :demand t
  :hook
  ((flycheck-mode-hook web-mode rjsx-mode) . add-node-modules-path))

(use-package rjsx-mode
  :mode ("\\.js\\'" "\\.jsx\\'")
  :config
  (add-hook 'rjsx-mode (defun my-js-mode-setup ()
                 (flycheck-mode t)
                 (when (executable-find "eslint")
                   (flycheck-select-checker 'javascript-eslint)))))


;;; package.el ends here
