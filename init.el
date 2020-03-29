;;; package --- summary -*- lexical-binding: t; -*-
;;; Code:
;;; Commentary:

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(setq package-check-signature nil)

(eval-and-compile
  (defun srs|revert-gc ()
    ;; reset values
    (setq gc-cons-threshold 16777216
          gc-cons-percentage 0.1
          file-name-handler-alist (append last-file-name-handler-alist
		                                  file-name-handler-alist))
    ;; delete any duplicate values
    (cl-delete-duplicates file-name-handler-alist :test 'equal)
    ;; get rid of temporarily variables
    (makunbound 'last-file-name-handler-alist))

  ;; set everything to efficient limits and save values
  (setq gc-cons-threshold most-positive-fixnum
        gc-cons-percentage 0.6
        last-file-name-handler-alist file-name-handler-alist
        file-name-handler-alist nil)

  (add-hook 'after-init-hook 'srs|revert-gc))

(require 'package)
(setq package-enable-at-startup nil)

(setq package-archives '(("melpa-stable" . "https://stable.melpa.org/packages/")
			 ("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)

;; Byte compile warnings off
(setq-default byte-compile-warnings nil)

;; Load user config
(load-file "~/.emacs.d/config.el")
(load-file "~/.emacs.d/package.el")


;;; init.el ends here
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(company-dabbrev-downcase nil t)
 '(company-idle-align-annotations t t)
 '(company-idle-delay 0 t)
 '(company-minimum-prefix-length 1 t)
 '(company-require-match (quote never) t)
 '(company-tooltip-align-annotations t t)
 '(doom-modeline-mode t)
 '(flycheck-check-syntax-automatically (quote (save mode-enabled)) t)
 '(lsp-auto-guess-root t t)
 '(lsp-io-doc-include-signature t t)
 '(lsp-log-io t t)
 '(lsp-prefer-flymake nil t)
 '(lsp-ui-doc-delay 0 t)
 '(lsp-ui-flycheck-enable t t)
 '(lsp-ui-peek-always-show t t)
 '(package-selected-packages
   (quote
    (benchmark-init dired+ drag-stuff restart-emacs doom-modeline all-the-icons-dired all-the-icons move-text move-lines dired which-key use-package multiple-cursors magit lsp-ui flycheck emmet-mode dap-mode company-lsp centaur-tabs add-node-modules-path)))
 '(python-fill-docstring-style (quote pep-257-nn) t)
 '(python-indent-guess-indent-offset-verbose nil t)
 '(restart-emacs-restore-frames t t)
 '(treemacs-resize-icons 15 t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
