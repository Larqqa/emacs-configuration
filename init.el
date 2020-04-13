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

(setq custom-theme-load-path '("~/.emacs.d/custom/themes"))

(package-initialize)

;; Byte compile warnings off
(setq-default byte-compile-warnings nil)

;; Load user config
(load-file "~/.emacs.d/custom/package.el")
(load-file "~/.emacs.d/custom/config.el")


;;; init.el ends here
