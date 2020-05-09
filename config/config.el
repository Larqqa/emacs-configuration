;;; config.el --- The configs -*- lexical-binding: t; -*-

;; Author: Larqqa

;;; Commentary:

;; These are all the config settings I use in Emacs

;;; Code:

;; Load theme
(load-theme 'lrq-new t)

;; Font if windows
(if (eq system-type 'windows-nt)
    (set-face-attribute 'default nil :family "Consolas" :height 110))
 
;; Set frame title
(setq frame-title-format "%b - LRQmacs"
      icon-title-format frame-title-format)

;; Set initial buffer
(setq initial-buffer-choice
      (lambda ()
	(get-buffer "*dashboard*")))

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

;; disable menu-bar, tool-bad & scroll-bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; cursor style block
(set-default 'cursor-type 'block)
(blink-cursor-mode -1)

;; Show current column
(column-number-mode)

;; Show parenthesis
(show-paren-mode)

;; global highlight line
(global-hl-line-mode)
(setq hl-line-sticky-flag nil
        global-hl-line-sticky-flag nil)

;; set underlining to nil
(set-face-attribute hl-line-face nil :underline nil)

;; Make window move work by arrows
(windmove-default-keybindings)

;; Display line numbersN
(line-number-mode 1)
(global-display-line-numbers-mode)

;; Enable word wrap
(global-visual-line-mode t)

;; Disable smart indent
;;(setq electric-indent-mode nil)

;; Indent with spaces instead of tabs
(setq indent-tabs-mode nil)

;; Replace selection
(delete-selection-mode 1)

;; Maximize Emacs on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Hide dired details by default
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode)))

;; Save minibuffer history
(savehist-mode 1)

;; Make yes/no prompts y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Focus buffer list on open
(define-key global-map [remap list-buffers] 'buffer-menu-other-window)

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

;; Add escape to C-x in term
(add-hook 'term-mode-hook
   (lambda ()
     (term-set-escape-char ?\C-x)))

;; Require dired-x
(require 'dired-x)

;; Dired-jump by default
(global-set-key (kbd "C-x d") 'dired-jump)

;; Set keybind for config folder
(which-key-add-key-based-replacements
  "C-c c" "Open config dir")
(global-set-key (kbd "C-c c")
                (lambda ()
                  (interactive)
                  (dired-jump nil "~/.emacs.d/config/")))

;; Reuse dired buffers
(put 'dired-find-alternate-file 'disabled nil)

;; Disable page-break-lines
;;y(global-page-break-lines-mode nil)

;; Eldoc hook
(add-hook 'after-init 'global-eldoc-mode)

;; Show paren mode
(show-paren-mode t)
(define-advice show-paren-function (:around (fn) fix)
  "Always highlight enclosing parens."
  (cond ((looking-at-p "\\s(") (funcall fn))
        (t (save-excursion
             (ignore-errors (backward-up-list))
             (funcall fn)))))

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

;; ---- FILE PATHS ----

;; Set custom paths for litter
(defconst lrq-config-dir "~/.emacs.d/config/")
(defconst lrq-local-dir "~/.emacs.d/local/")

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



;;; config.el ends here
