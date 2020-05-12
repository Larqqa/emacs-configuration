;;; config-v2.el --- The configs -*- lexical-binding: t; -*-

;; Author: Larqqa

;;; Commentary:

;; These are all the config settings I use in Emacs

;;; Code:

;; et custom paths for litter
(defconst lrq-config-dir "~/.emacs.d/config/")
(defconst lrq-local-dir "~/.emacs.d/local/")

;; ---- UI ----
(load-theme 'lrq-new t)

(if (eq system-type 'windows-nt)
    (set-face-attribute 'default nil :family "Consolas" :height 110))
 
(setq frame-title-format "%b - LRQmacs"
      icon-title-format frame-title-format)

(setq initial-buffer-choice
      (lambda ()
	(get-buffer "*dashboard*")))

(setq dashboard-banners-directory "~/.emacs.d/config/themes/banners/")

(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

(set-default 'cursor-type 'block)
(blink-cursor-mode -1)

(column-number-mode)

(line-number-mode t)

(global-display-line-numbers-mode)

(add-hook 'prog-mode-hook 'turn-on-visual-line-mode)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(add-hook 'after-init 'global-eldoc-mode)

(show-paren-mode t)
(define-advice show-paren-function (:around (fn) fix)
  "Always highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
        (t (save-excursion
             (ignore-errors (backward-up-list))
             (funcall fn)))))

(global-hl-line-mode)
(setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)

(set-face-attribute hl-line-face nil :underline nil)

;; Less noise at startup. The dashboard/empty scratch buffer is good enough.
(setq inhibit-startup-message t
      inhibit-startup-echo-area-message user-login-name
      inhibit-default-init t
      ;; Avoid pulling in many packages by starting the scratch buffer in
      ;; `fundamental-mode', rather than, say, `org-mode' or `text-mode'.
      initial-major-mode 'fundamental-mode
      initial-scratch-message nil)


;; Emacs "updates" its ui more often than it needs to, so we slow it down
;; slightly from 0.5s:
(setq idle-update-delay 1)

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; Don't blink the paren matching the one at point, it's too distracting.
(setq blink-matching-paren nil)
(setq visible-cursor nil)

;; Don't stretch the cursor to fit wide characters, it is disorienting,
;; especially for tabs.
(setq x-stretch-cursor nil)

;; Reduce the clutter in the fringes; we'd like to reserve that space for more
;; useful information, like git-gutter and flycheck.
(setq indicate-buffer-boundaries nil
      indicate-empty-lines nil)

;; Don't resize windows & frames in steps; it's prohibitive to prevent the user
;; from resizing it to exact dimensions, and looks weird.
(setq window-resize-pixelwise t
      frame-resize-pixelwise t)

;; always avoid GUI
(setq use-dialog-box nil)
;; Don't display floating tooltips; display their contents in the echo-area,
;; because native tooltips are ugly.
(when (bound-and-true-p tooltip-mode)
  (tooltip-mode -1))

 ;; Favor vertical splits over horizontal ones. Screens are usually wide.
(setq split-width-threshold 160
      split-height-threshold nil)

;; Expand the minibuffer to fit multi-line text displayed in the echo-area. This
;; doesn't look too great with direnv, however...
(setq resize-mini-windows 'grow-only
      ;; But don't let the minibuffer grow beyond this size
      max-mini-window-height 0.15)

;;;###package whitespace
(setq whitespace-line-column nil
      whitespace-style
      '(face indentation tabs tab-mark spaces space-mark newline newline-mark
        trailing lines-tail)
      whitespace-display-mappings
      '((tab-mark ?\t [?› ?\t])
        (newline-mark ?\n [?¬ ?\n])
        (space-mark ?\  [?·] [?.])))

;; Explicitly define a width to reduce computation
(setq-default display-line-numbers-width 3)

;; Show absolute line numbers for narrowed regions makes it easier to tell the
;; buffer is narrowed, and where you are, exactly.
(setq-default display-line-numbers-widen t)

;; ---- TYPING ----
(delete-selection-mode 1)

;; Better kill-word funcs
(defun backward-kill-char-or-word ()
  "Backward kill word, or char, or whitespace."
  (interactive)
  (cond
   ((looking-back (rx (char word)) 1)
    (backward-kill-word 1))
   ((looking-back (rx (char blank)) 1)
    (delete-horizontal-space t))
   (t
    (backward-delete-char 1))))

(global-set-key (kbd "<C-backspace>") 'backward-kill-char-or-word)

(defun kill-char-or-word ()
  "Kill word, or char, or whitespace."
  (interactive)
  (cond
   ((looking-at (rx (char word)))
    (kill-word 1))
   ((looking-at (rx (char blank)))
    (delete-horizontal-space))
   (t
    (delete-char 1))))

(global-set-key (kbd "<C-delete>") 'kill-char-or-word)

(global-set-key (kbd "C-'") 'comment-line)

;; Indentation
(setq-default c-basic-offset 2)
(setq-default tab-width 2
              tab-always-indent t
              indent-tabs-mode nil
              fill-column 80)

;; Word wrapping
(setq-default word-wrap t
              truncate-lines t
              truncate-partial-width-windows nil)

(setq sentence-end-double-space nil
      delete-trailing-lines nil
      require-final-newline t
      tabify-regexp "^\t* [ \t]+")  ; for :retab

;; Eliminate duplicates in the kill ring. That is, if you kill the same thing
;; twice, you won't have to use M-y twice to get past it to older entries in the
;; kill ring.
(setq kill-do-not-save-duplicates t)

;; ---- QOL ----
(savehist-mode 1)

(fset 'yes-or-no-p 'y-or-n-p)

(define-key global-map [remap list-buffers] 'buffer-menu-other-window)

(add-hook 'term-mode-hook
   (lambda ()
     (term-set-escape-char ?\C-x)))

(windmove-default-keybindings)

;; Resizing the Emacs frame can be a terribly expensive part of changing the
;; font. By inhibiting this, we halve startup times, particularly when we use
;; fonts that are larger than the system default (which would resize the frame).
(setq frame-inhibit-implied-resize t)

;; Don't ping things that look like domain names.
(setq ffap-machine-p-known 'reject)

;; Font compacting can be terribly expensive, especially for rendering icon
;; fonts on Windows. Whether it has a noteable affect on Linux and Mac hasn't
;; been determined, but we inhibit it there anyway.
(setq inhibit-compacting-font-caches t)

(setq uniquify-buffer-name-style 'forward
      ;; no beeping or blinking please
      ring-bell-function #'ignore
      visible-bell nil)

;; Don't autosave files or create lock/history/backup files. We don't want
;; copies of potentially sensitive material floating around, and we'll rely on
;; git and our own good fortune instead. Fingers crossed!
(setq auto-save-default nil
      create-lockfiles nil
      make-backup-files nil
      ;; But have a place to store them in case we do use them...
      ;; auto-save-list-file-name (concat doom-cache-dir "autosave")
      auto-save-list-file-prefix (concat lrq-local-dir "autosave/")
      auto-save-file-name-transforms `((".*" ,(concat lrq-local-dir "autosave/") t))
      backup-directory-alist `((".*" . ,(concat lrq-local-dir "backup/"))))

;; ---- SCROLLING ----

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate fontification immediately after scrolling.
(setq fast-but-imprecise-scrolling t)

;; Scrolling
(setq hscroll-margin 2
      hscroll-step 1
      ;; Emacs spends too much effort recentering the screen if you scroll the
      ;; cursor more than N lines past window edges (where N is the settings of
      ;; `scroll-conservatively'). This is especially slow in larger files
      ;; during large-scale scrolling commands. If kept over 100, the window is
      ;; never automatically recentered.
      scroll-conservatively 101
      scroll-margin 0
      scroll-preserve-screen-position t
      ;; Reduce cursor lag by a tiny bit by not auto-adjusting `window-vscroll'
      ;; for tall lines.
      auto-window-vscroll nil
      ;; mouse
      mouse-wheel-scroll-amount '(5 ((shift) . 2))
      mouse-wheel-progressive-speed nil)  ; don't accelerate scrolling


;; ---- DIRED ----
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode)))

(require 'dired-x)

;; Dired-jump by default
(global-set-key (kbd "C-x d") 'dired-jump)


;; Reuse dired buffers
(put 'dired-find-alternate-file 'disabled nil)

;; ---- CUSTOM KEYBINDS ----
;; Set keybind for config folder
(which-key-add-key-based-replacements
  "C-c c" "Open config dir")
(global-set-key (kbd "C-c c")
                (lambda ()
                  (interactive)
                  (dired-jump nil "~/.emacs.d/config/")))

;; Close Emacs after prompt
(defun lrq/close-emacs ()
  "Query for closing Emacs."
  (setq lrq/prompt (y-or-n-p "Do you really want to close Emacs? "))
  (if lrq/prompt
    (save-buffers-kill-emacs)))

(which-key-add-key-based-replacements
  "C-x C-c" "Close Emacs")
(global-set-key (kbd "C-x C-c")
                (lambda ()
                  (interactive)
                  (lrq/close-emacs)))

;; Save file, then eval buffer
(which-key-add-key-based-replacements
  "C-c s" "Save & Eval buffer")
(global-set-key (kbd "C-c s")
                (lambda ()
                  (interactive)
                  (save-buffer)
                  (eval-buffer)))

;; Set term keybinding, and bash location
(which-key-add-key-based-replacements
  "C-c t" "Open terminal")
(global-set-key (kbd "C-c t")
                (lambda ()
                  (interactive)
                  (term "/bin/bash")))

(defun insert-phpdoc-comment () (interactive)
       (insert "/**\n * Brief description. Long description. \n * \n * @param \n * @return \n */"))
(add-hook 'php-mode-hook (lambda () (local-set-key (kbd "C-c d") 'insert-phpdoc-comment)))
(add-hook 'rjsx-mode-hook (lambda () (local-set-key (kbd "C-c d") 'insert-phpdoc-comment)))

(defun insert-pythondoc-comment () (interactive)
       (insert "\"\"\"Brief description. \nLong description.\n\n:param \n:return \n\"\"\"\n"))
(add-hook 'python-mode-hook (lambda () (local-set-key (kbd "C-c d") 'insert-pythondoc-comment)))

;; Add two spaces
(defun lrq/insert-tab () (interactive)
       (insert "  "))

;; Remove two spaces
(defun lrq/remove-tab () (interactive)
       (cond
        ((looking-back (rx "  "))
         (backward-delete-char 2))))

;; Force indents
(global-set-key (kbd "§") 'lrq/insert-tab)
(global-set-key (kbd "½") 'lrq/remove-tab)

;; ---- FILE PATHS ----

(setq bookmark-default-file                (concat lrq-config-dir "bookmarks")
      yas/yasnippet-dirs                   (concat lrq-config-dir "snippets")
      dashboard-banners-directory          (concat lrq-config-dir "/themes/banners/")
      
      custom-file                          (concat lrq-local-dir "custom.el")
      auto-save-list-file-prefix           (concat lrq-local-dir "auto-save-list/.saves-")
      desktop-base-file-name               (concat lrq-local-dir "autosave")
      desktop-base-lock-name               (concat lrq-local-dir "autosave-lock")
      desktop-dirname                      (concat lrq-local-dir "desktop")
      savehist-file                        (concat lrq-local-dir "desktop/history")
      pcache-directory                     (concat lrq-local-dir "pcache/")
      request-storage-directory            (concat lrq-local-dir "request")
      server-auth-dir                      (concat lrq-local-dir "server/")
      shared-game-score-directory          (concat lrq-local-dir "shared-game-score/")
      tramp-auto-save-directory            (concat lrq-local-dir "tramp-auto-save/")
      tramp-backup-directory-alist         backup-directory-alist
      tramp-persistency-file-name          (concat lrq-local-dir "tramp-persistency.el")
      url-cache-directory                  (concat lrq-local-dir "url/")
      url-configuration-directory          (concat lrq-local-dir "url/")
      gamegrid-user-score-file-directory   (concat lrq-local-dir "games/")
      recentf-save-file                    (concat lrq-local-dir "recentf")
      mc/list-file                         (concat lrq-local-dir "mc-lists.el")
      hl-highlight-save-file               (concat lrq-local-dir ".hl-save")
      projectile-known-projects-file       (concat lrq-local-dir "projectile-bookmarks.eld")
      lsp-session-file                     (concat lrq-local-dir ".lsp-session-v1")
      anaconda-mode-installation-directory (concat lrq-local-dir "anaconda-mode")
      treemacs-persist-file                (concat lrq-local-dir "treemacs-persist")
      treemacs-last-error-persist-file     (concat lrq-local-dir "treemacs-last-error-persist")
      transient-history-file               (concat lrq-local-dir "transient/history.el")
)

;; Make dirs that error out if not exists
(defun lrq/find-file (filename)
  "Create parent directory if not exists while visiting file.
FILENAME is the file to check"
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))

;; List of files that error out
(lrq/find-file (concat lrq-local-dir "desktop/history"))



;;; config-v2.el ends here
