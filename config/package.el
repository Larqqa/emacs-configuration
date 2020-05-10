;;; package.el --- The packages -*- lexical-binding: t; -*-

;; Author: Larqqa

;;; Commentary:

;; This is all the packages I use in Emacs

;;; Code:

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
  :defer t
  :bind
  ("C-x g" . magit-status)
  (:map magit-status-mode-map
        ("C-x g" . magit-dispatch)
        ("q" . (lambda () (interactive) (magit-mode-bury-buffer 16))))
  :config
  ;; allow window to be split vertically rather than horizontally
  (setq split-width-threshold 0)
  (setq split-height-threshold nil)
  ;; full window magit
  (setq magit-display-buffer-function 'magit-display-buffer-fullframe-status-v1))

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

;; (use-package treemacs
;;   :bind (:map global-map
;; 	      ("C-x t t" . treemacs)
;; 	      ("C-x t 1" . treemacs-select-window))
;;   :init
;;   (setq treemacs-follow-after-init t
;;         treemacs-is-never-other-window t
;;         treemacs-sorting 'alphabetic-case-insensitive-asc)
;;   :config
;;   (treemacs-follow-mode -1)
;;   (treemacs-git-mode 'deferred)
;;   :custom
;;   (treemacs-resize-icons 15))

;; (use-package treemacs-magit
;;   :after treemacs magit)

;; (use-package lsp-treemacs
;;   :demand t
;;   :config
;;   (lsp-treemacs-sync-mode 1))

(use-package neotree
  :bind
  ("C-x t t" . neotree-toggle)
  :config
  (setq neo-theme (if (display-graphic-p) 'icons 'arrow))
  (setq neo-smart-open t
        projectile-switch-project-action 'neotree-projectile-action
        neo-window-fixed-size nil))

(use-package all-the-icons
  :if (display-graphic-p)
  :config
  (unless (find-font (font-spec :name "all-the-icons"))
    (all-the-icons-install-fonts t)))

(use-package all-the-icons-dired
  :demand t
  :hook
  (dired-mode-hook . all-the-icons-dired-mode))

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
        ;dashboard-set-file-icons t
        dashboard-page-separator "\n\n"
        dashboard-items '((recents   . 5)
                          (projects  . 5)
                          (bookmarks . 5))))

(use-package default-text-scale
  :hook (text-mode . default-text-scale-mode)
  :bind
  ("C--" . default-text-scale-decrease)
  ("C-+" . default-text-scale-increase))

;;; ---- HIGHLIGHTING ----

(use-package smartparens
  :hook
  (prog-mode . smartparens-mode)
  :config
  (sp-local-pair 'prog-mode "{" nil :post-handlers '(:add ("||\n[i]" "RET")))
  (sp-local-pair 'prog-mode "[" nil :post-handlers '(:add ("||\n[i]" "RET")))
  (sp-local-pair 'prog-mode "(" nil :post-handlers '(:add ("||\n[i]" "RET"))))

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

(use-package highlight-indent-guides
  :hook (prog-mode . highlight-indent-guides-mode)
  :init
  (setq highlight-indent-guides-method 'character))

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

;; Not needed on the Fedora install
;; (if (not (eq system-type 'windows-nt))
;;     (use-package exec-path-from-shell
;;       :demand t
;;       :config
;;       (exec-path-from-shell-initialize)))

(use-package esup
  :commands (esup))

(use-package popup-kill-ring
  :bind ("C-c b" . popup-kill-ring))

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
(use-package yasnippet
  :hook
  (prog-mode . yas-global-mode))

(use-package yasnippet-snippets)

(use-package flycheck
  :hook (prog-mode . flycheck-mode)
  :init
  (setq flycheck-checker-error-threshold 2000)
  :config
  (setq-default flycheck-disabled-checkers
                (append flycheck-disabled-checkers
                        '(javascript-jshint json-jsonlist)))
  :custom
  (flycheck-check-syntax-automatically '(save mode-enabled)))

(use-package flycheck-posframe
  :hook(flycheck-mode . flycheck-posframe-mode))

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

;; Add yasnippet support for all company backends
;; https://github.com/syl20bnr/spacemacs/pull/179
(defvar company-mode/enable-yas t
  "Enable yasnippet for all backends.")

(defun company-mode/backend-with-yas (backend)
  "Add yasnippets to all backends."
  (if (or (not company-mode/enable-yas) (and (listp backend) (member 'company-yasnippet backend)))
      backend
    (append (if (consp backend) backend (list backend))
            '(:with company-yasnippet))))

(use-package company
  :hook
  (prog-mode . company-mode)
  :config
  (setq company-backends (mapcar #'company-mode/backend-with-yas company-backends))
  :init
  (setq company-tooltip-align-annotations t
        company-require-match 'never
        company-dabbrev-downcase nil
        company-idle-align-annotations t
        company-idle-delay 0
        company-minimum-prefix-length 1)
  :bind
  ("C-ä" . company-capf)
  (:map company-mode-map
    ("C-ä" . company-other-backend)))

(use-package company-lsp
  :after (company lsp-mode)
  :custom
  (company-lsp-cache-candidates t))

;;; ---- LANGUAGES ----

;; PYTHON

(use-package pyvenv
  :hook (anaconda-mode . pyvenv-mode))

(use-package anaconda-mode
  :hook
  (python-mode-hook . (anaconda-mode anaconda-eldoc-mode))
  (python-mode . (lambda ()
                   ;; Start pyvenv when opening a python project
                   ;; Expects the venv to be in /venv
                   (interactive)
                   (pyvenv-activate "venv")))
  :config
  (use-package company-anaconda
    :hook
    (python-mode-hook . anaconda-mode)
    :init
    (eval-after-load "company"
      '(add-to-list 'company-backends '(company-anaconda :with company-capf)))))

;; PHP
(use-package php-mode)
(add-hook 'php-mode-hook
      (lambda ()
        (make-local-variable 'c-basic-offset)
        (setq c-basic-offset 2)))

;; HTML & CSS
(use-package web-mode
  :mode
  ("\\.html\\'")
  :init
  (setq web-mode-markup-indent-offset 2))

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
  :init
  (setq js-indent-level 2))

;; JSON
(use-package json-mode
  :mode ("\\.json\\'"))

;;; package.el ends here
