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

(setq package-archives
      '(("melpa" . "http://melpa.org/packages/")
        ("gnu" . "http://elpa.gnu.org/packages/")))

(setq custom-theme-load-path '("~/.emacs.d/themes"))

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
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(company-dabbrev-downcase nil)
 '(company-idle-align-annotations t t)
 '(company-idle-delay 0)
 '(company-lsp-cache-candidates t t)
 '(company-minimum-prefix-length 1)
 '(company-require-match (quote never))
 '(company-tooltip-align-annotations t)
 '(custom-safe-themes
   (quote
    ("f6e549814956603c9caa8d0cf4ad98dbf738becfae19e43f9343ca3db3879cfe" "33fab87358ead578a88aa9393681f1b91200f3bf2ea0c3c2a00172f0821ba25c" "ac14db25dfef07310e6d7448f72ed261b9bc33c181a6f2bb608e2a9907b8d6a7" "4b0f3a0eb457638816063842c1661b12a90a231ea2c31a3d85817d0ed331d7cd" "598adff5b8f16b9ebca3240ce6a44b8dfa7a997ecea51576fdc4dd6bdf90ede3" default)))
 '(doom-modeline-mode t)
 '(enable-recursive-minibuffers t)
 '(flycheck-check-syntax-automatically (quote (save mode-enabled)))
 '(ivy-use-virtual-buffers t)
 '(lsp-auto-guess-root t t)
 '(lsp-io-doc-include-signature t t)
 '(lsp-log-io t t)
 '(lsp-prefer-flymake nil t)
 '(lsp-ui-doc-delay 0 t)
 '(lsp-ui-flycheck-enable t t)
 '(lsp-ui-peek-always-show t t)
 '(package-selected-packages
   (quote
    (rainbow-mode company-anaconda anaconda-mode smex elpy multiple-cursorsinit virtualenvwrapper pyvenv ivy helm icicles benchmark-init dired+ drag-stuff restart-emacs doom-modeline all-the-icons-dired all-the-icons move-text move-lines dired which-key use-package multiple-cursors magit lsp-ui flycheck emmet-mode dap-mode company-lsp centaur-tabs add-node-modules-path)))
 '(python-fill-docstring-style (quote pep-257-nn))
 '(python-indent-guess-indent-offset-verbose nil)
 '(restart-emacs-restore-frames t t)
 '(treemacs-resize-icons 15 t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
