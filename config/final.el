;;; package --- summary
;;; Code:
;;; Commentary:

;;; DOOM

;; These are derived from some of the Emacs Doom configs i liked.


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
      auto-save-list-file-prefix (concat lrq-local-dir "autosave/")
      auto-save-file-name-transforms `((".*" ,(concat lrq-local-dir "autosave/") t))
      backup-directory-alist `((".*" . ,(concat lrq-local-dir "backup/"))))


;; !DOOM


;;; final.el ends here
