;;; package --- summary
;;; Code:
;;; Commentary:

;; SETTINGS
(load-theme 'lrq-custom t)
 
(setq frame-title-format "%b - LRQmacs"
      icon-title-format frame-title-format)

;; Set initial buffer
(setq initial-buffer-choice
      (lambda ()
	(get-buffer "*dashboard*")))

;;; DOOM

;; These are some of the Emacs Doom configs i liked.

;; Prevent the glimpse of un-styled Emacs by disabling these UI elements early.
(push '(menu-bar-lines . 0) default-frame-alist)
(push '(tool-bar-lines . 0) default-frame-alist)
(push '(vertical-scroll-bars) default-frame-alist)

(setq frame-inhibit-implied-resize t)

;; Define UTF-8 as the charset
(when (fboundp 'set-charset-priority)
  (set-charset-priority 'unicode))
(prefer-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)

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

;; Set custom paths for litter
(defconst doom-local-dir "~/.emacs.d/local/")
(defconst doom-etc-dir "~/.emacs.d/etc/")
(defconst doom-cache-dir "~/.emacs.d/cache/")

(setq abbrev-file-name             (concat doom-local-dir "abbrev.el")
      async-byte-compile-log-file  (concat doom-etc-dir "async-bytecomp.log")
      bookmark-default-file        (concat doom-etc-dir "bookmarks")
      custom-file                  (concat doom-local-dir "custom.el")
      desktop-dirname              (concat doom-etc-dir "desktop")
      desktop-base-file-name       "autosave"
      desktop-base-lock-name       "autosave-lock"
      savehist-file                (concat doom-etc-dir "desktop/history")
      pcache-directory             (concat doom-cache-dir "pcache/")
      request-storage-directory    (concat doom-cache-dir "request")
      server-auth-dir              (concat doom-cache-dir "server/")
      shared-game-score-directory  (concat doom-etc-dir "shared-game-score/")
      tramp-auto-save-directory    (concat doom-cache-dir "tramp-auto-save/")
      tramp-backup-directory-alist backup-directory-alist
      tramp-persistency-file-name  (concat doom-cache-dir "tramp-persistency.el")
      url-cache-directory          (concat doom-cache-dir "url/")
      url-configuration-directory  (concat doom-etc-dir "url/")
      gamegrid-user-score-file-directory (concat doom-etc-dir "games/")
      recentf-save-file            (concat doom-local-dir "recentf")
      mc/list-file                 (concat doom-local-dir "mc-lists.el"))

;; Make dirs that error out if not exists
(defun lrq/find-file (filename)
  "Create parent directory if not exists while visiting file.
FILENAME is file to check"
  (unless (file-exists-p filename)
    (let ((dir (file-name-directory filename)))
      (unless (file-exists-p dir)
        (make-directory dir t)))))

(lrq/find-file (concat doom-etc-dir "desktop/history"))

;; Reduce rendering/line scan work for Emacs by not rendering cursors or regions
;; in non-focused windows.
(setq-default cursor-in-non-selected-windows nil)
(setq highlight-nonselected-windows nil)

;; More performant rapid scrolling over unfontified regions. May cause brief
;; spells of inaccurate fontification immediately after scrolling.
(setq fast-but-imprecise-scrolling t)

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

;; Indentation
(setq-default tab-width 4
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

;; Don't autosave files or create lock/history/backup files. We don't want
;; copies of potentially sensitive material floating around, and we'll rely on
;; git and our own good fortune instead. Fingers crossed!
(setq auto-save-default nil
      create-lockfiles nil
      make-backup-files nil
      ;; But have a place to store them in case we do use them...
      ;; auto-save-list-file-name (concat doom-cache-dir "autosave")
      auto-save-list-file-prefix (concat doom-cache-dir "autosave/")
      auto-save-file-name-transforms `((".*" ,(concat doom-cache-dir "autosave/") t))
      backup-directory-alist `((".*" . ,(concat doom-cache-dir "backup/"))))


;; !DOOM



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

;; Display line numbers
(line-number-mode 1)
(global-display-line-numbers-mode)

;; Enable word wrap
(global-visual-line-mode t)

;; Disable smart indent
(setq electric-indent-mode nil)

;; Use spaces as indentation
(setq indent-tabs-mode nil)

;; set default tab behaviour
(setq c-default-style "linux"
      c-basic-offset 4
      c-tab-always-indent t)

;; Replace selection
(delete-selection-mode 1)

;; Maximize on startup
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; Hide dired details
(add-hook 'dired-mode-hook
          (lambda ()
            (dired-hide-details-mode)))
	       
;; Save desktop on close
;; WIP
;;(desktop-save-mode 1)

;; Save history
(savehist-mode 1)

;; Make yes/no prompts y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Open buffer and focus
(define-key global-map [remap list-buffers] 'buffer-menu-other-window)

;; Close Emacs after prompt
(defun lrq/close-emacs ()
  "Query for closing Emacs."
  (setq prompt (y-or-n-p "Do you really want to close Emacs? "))
  (if prompt
    (save-buffers-kill-emacs)))

(global-set-key (kbd "C-x C-c") (lambda () (interactive) (lrq/close-emacs)))

;; Save and eval
(global-set-key (kbd "C-c C-s")
                (lambda ()
                  (interactive)
                  (save-buffer)
                  (eval-buffer)))

;; Custom backward kill word
(defun dwim-backward-kill-word ()
  "DWIM kill characters backward until encountering the beginning of a word or non-word."
  (interactive)
  (if (thing-at-point 'word) (backward-kill-word 1)
    (let* ((orig-point              (point))
           (orig-line               (line-number-at-pos))
           (backward-word-point     (progn (backward-word) (point)))
           (backward-non-word-point (progn (goto-char orig-point) (backward-non-word) (point)))
           (min-point               (max backward-word-point backward-non-word-point)))

      (if (< (line-number-at-pos min-point) orig-line) (progn (goto-char min-point) (end-of-line) (delete-horizontal-space))
        (delete-region min-point orig-point)
        (goto-char min-point))
      )))

(defun backward-non-word ()
  "Move backward until encountering the beginning of a non-word."
  (interactive)
  (search-backward-regexp "[^a-zA-Z0-9\s\n]")
  (while (looking-at "[^a-zA-Z0-9\s\n]")
    (backward-char))
  (forward-char))

(global-set-key (kbd "C-<backspace>") (lambda () (interactive) (dwim-backward-kill-word)))

;; Set term keybinding, and bash location
(global-set-key (kbd "C-c t") (lambda () (interactive) (term "/bin/bash")))

;; Require dired-x
(require 'dired-x)

;; Dired-jump by default
(global-set-key (kbd "C-x d") 'dired-jump)

;; Set keybind for config folder
(global-set-key (kbd "C-c c") (lambda () (interactive) (dired-jump nil "~/.emacs.d/")))

;; Disable page-break-lines
(global-page-break-lines-mode nil)

;;; config.el ends here
