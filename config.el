;;; package --- summaty
;;; Code:
;;; Commentary:

;; SETTINGS
(load-theme 'wombat)
 
(setq frame-title-format "LRQ emacs")

;; disable menu-bar, tool-bad & scroll-bar
(menu-bar-mode -1)
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; cursor style block
(set-default 'cursor-type 'block)

;; Show current column
(column-number-mode)

;; Show parenthesis
(show-paren-mode)

;; global highlight line
(global-hl-line-mode)

;; set underlining to nil
(set-face-attribute hl-line-face nil :underline nil)

;; Make window move work by arrows
(windmove-default-keybindings)

;; Display line numbers
(line-number-mode 1)
(setq display-line-numbers-type t)

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
	       
;; Save sessions when closing  
(desktop-save-mode 1)	       
(savehist-mode 1)	       

;; Make yes/no prompts y/n
(fset 'yes-or-no-p 'y-or-n-p)

;; Dired-jump by default
(global-set-key (kbd "C-x d") 'dired-jump)

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
  "DWIM kill characters backward until encountering the beginning of a
word or non-word."
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

;;; config.el ends here
