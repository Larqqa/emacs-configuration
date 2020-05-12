
;;; lrq-new-theme.el --- Custom face theme for Emacs

;; Copyright (C) 2011-2020 Free Software Foundation, Inc.

;; Author: Larqqa

;; This file is part of GNU Emacs.

;; GNU Emacs is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; GNU Emacs is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This color theme was birthed from my love of VSC's One Dark Pro
;; It is a mix of Doom one/Doom material/Wombat and my own colors

;;; Code:

(deftheme lrq-new
  "My custom theme derived from Wombat.
Added some custom faces to eg. company etc. packages.")

(let ((class '((class color) (min-colors 89))))

  ;; Defining some colors
  (setq
   ;; Usage: first ,(car color) secondary ,@(cdr color)
   bg         '("#282c34" "#0F111A")
   bg-alt     '("#21242b" "#202438")
   base0      '("#1B2229" "black"  )
   base1      '("#1c1f24" "#1e1e1e")
   base2      '("#202328" "#2e2e2e")
   base3      '("#23272e" "#262626")
   base4      '("#3f444a" "#3f3f3f")
   base5      '("#5B6268" "#525252")
   base6      '("#73797e" "#6b6b6b")
   base7      '("#9ca0a4" "#979797")
   base8      '("#DFDFDF" "#dfdfdf")
   fg         '("#bbc2cf" "#bfbfbf")
   fg-alt     '("#5B6268" "#2d2d2d")
  
   red        '("#ff6c6b" "#ff6655")
   teal       '("#4db5bd" "#44b9b1")
   blue       '("#51afef" "#51afef")
   magenta    '("#c678dd" "#c678dd")
   grey       '("#5a5b5a" "#5a5a5a")
   orange     '("#FF8F6E" "#FF875F")
   green      '("#A3DB4F" "#AFD75F")
   yellow     '("#FFCB6B" "#FFD75F")
   dark-blue  '("#3575FF" "#5F87FF")
   violet     '("#C792EA" "#D787D7")
   cyan       '("#89DDFF" "#87DDFF")
   dark-cyan  '("#3BC7FF" "#5FD7FF")
   
   success    green
   error      red
   warning    yellow
   )


  
  (custom-theme-set-faces
   'lrq-new
   `(default ((,class (:background ,(car bg) :foreground ,(car base8)))))
   `(cursor  ((,class (:background ,@(cdr base8)))))

   ;; Highlighting faces
   `(fringe              ((,class (:background ,(car base2)))))
   `(highlight           ((,class (:background ,(car base3)))))
   `(lazy-highlight      ((,class (:background ,(car base3)))))
   `(region              ((,class (:background ,(car base4) :foreground ,(car base8)))))
   `(secondary-selection ((,class (:background ,(car dark-blue) :foreground ,(car base8)))))
   `(isearch             ((,class (:background ,(car base3) :foreground ,(car base5)))))

   ;; Escape and prompt faces
   `(minibuffer-prompt ((,class (:foreground ,(car red)))))
   `(escape-glyph      ((,class (:foreground ,(car yellow) :weight bold))))
   `(homoglyph         ((,class (:foreground ,(car yellow) :weight bold))))

   ;; Font lock faces
   `(font-lock-builtin-face       ((,class (:foreground ,(car blue)))))
   `(font-lock-comment-face       ((,class (:foreground ,(car base6)))))
   `(font-lock-doc-face           ((,class (:foreground ,(car base6)))))
   `(font-lock-constant-face      ((,class (:foreground ,(car red)))))
   `(font-lock-function-name-face ((,class (:foreground ,(car cyan)))))
   `(font-lock-keyword-face       ((,class (:foreground ,(car magenta)))))
   `(font-lock-string-face        ((,class (:foreground ,(car green)))))
   `(font-lock-type-face          ((,class (:foreground ,(car yellow) :weight bold))))
   `(font-lock-variable-name-face ((,class (:foreground ,(car yellow)))))
   `(font-lock-warning-face       ((,class (:foreground ,@(cdr red)))))

   ;; Button and link faces
   `(link         ((,class (:foreground ,(car teal) :underline t))))
   `(link-visited ((,class (:foreground ,(car red) :underline t))))
   `(button       ((,class (:background ,(car base3) :foreground ,(car base8)))))
   `(header-line  ((,class (:background ,@(cdr base3) :foreground ,(car base8)))))

   ;; Gnus faces
   `(gnus-group-news-1      ((,class (:weight bold :foreground "#95e454"))))
   `(gnus-group-news-1-low  ((,class (:foreground "#95e454"))))
   `(gnus-group-news-2      ((,class (:weight bold :foreground "#cae682"))))
   `(gnus-group-news-2-low  ((,class (:foreground "#cae682"))))
   `(gnus-group-news-3      ((,class (:weight bold :foreground "#ccaa8f"))))
   `(gnus-group-news-3-low  ((,class (:foreground "#ccaa8f"))))
   `(gnus-group-news-4      ((,class (:weight bold :foreground "#99968b"))))
   `(gnus-group-news-4-low  ((,class (:foreground "#99968b"))))
   `(gnus-group-news-5      ((,class (:weight bold :foreground "#cae682"))))
   `(gnus-group-news-5-low  ((,class (:foreground "#cae682"))))
   `(gnus-group-news-low    ((,class (:foreground "#99968b"))))
   `(gnus-group-mail-1      ((,class (:weight bold :foreground "#95e454"))))
   `(gnus-group-mail-1-low  ((,class (:foreground "#95e454"))))
   `(gnus-group-mail-2      ((,class (:weight bold :foreground "#cae682"))))
   `(gnus-group-mail-2-low  ((,class (:foreground "#cae682"))))
   `(gnus-group-mail-3      ((,class (:weight bold :foreground "#ccaa8f"))))
   `(gnus-group-mail-3-low  ((,class (:foreground "#ccaa8f"))))
   `(gnus-group-mail-low    ((,class (:foreground "#99968b"))))
   `(gnus-header-content    ((,class (:foreground "#8ac6f2"))))
   `(gnus-header-from       ((,class (:weight bold :foreground "#95e454"))))
   `(gnus-header-subject    ((,class (:foreground "#cae682"))))
   `(gnus-header-name       ((,class (:foreground "#8ac6f2"))))
   `(gnus-header-newsgroups ((,class (:foreground "#cae682"))))

   ;; Message faces
   `(message-header-name    ((,class (:foreground "#8ac6f2" :weight bold))))
   `(message-header-cc      ((,class (:foreground "#95e454"))))
   `(message-header-other   ((,class (:foreground "#95e454"))))
   `(message-header-subject ((,class (:foreground "#cae682"))))
   `(message-header-to      ((,class (:foreground "#cae682"))))
   `(message-cited-text     ((,class (:foreground "#99968b"))))
   `(message-separator      ((,class (:foreground "#e5786d" :weight bold))))

   ;; Company mode faces
   `(company-template-field               ((,class (:foreground ,(car fg-alt) :weight bold))))
   `(company-tooltip                      ((,class (:foreground ,(car blue) :background ,(car bg-alt)))))
   `(company-tooltip-common               ((,class (:foreground ,(car green) :background ,(car bg-alt)))))
   `(company-tooltip-selection            ((,class (:foreground ,(car cyan) :weight bold))))
   `(company-scrollbar-bg                 ((,class (:background ,(car dark-cyan)))))
   `(company-scrollbar-fg                 ((,class (:background ,(car yellow)))))
   `(company-preview                      ((,class (:background ,(car base4)))))
   `(company-preview-common               ((,class (:foreground ,(car cyan) :weight bold))))
   `(company-tooltip-annotation           ((,class (:foreground ,(car orange)))))
   `(company-tooltip-annotation-selection ((,class (:foreground ,(car orange) :weight bold))))

   ;; popup kill ringforeground
   `(popup-face ((,class (:background ,(car bg-alt) :foreground ,(car blue)))))
   `(popup-menu-mouse-face ((,class (:foreground ,(car base8) :background ,(car dark-cyan)))))
   `(popup-scroll-bar-background-face ((,class (:background ,(car dark-cyan)))))
   `(popup-scroll-bar-foreground-face ((,class (:background ,(car yellow)))))
   
    ;; Mode line faces
   `(mode-line ((,class (:background ,@(cdr base4) :foreground ,(car base8)))))
   `(mode-line-inactive ((,class (:background ,@(cdr base4) :foreground ,(car base6)))))
   `(mode-line-highlight ((,class (:background ,(car base3)))))

   ;; Line numbers
   `(line-number ((,class (:foreground ,(car base5)))))

   ;; Dashboard
   `(dashboard-text-banner ((,class (:foreground ,(car blue) :weight normal))))

   ;; Rainbow Delimiter
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,(car violet) :weight normal))))
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,(car blue) :weight normal))))
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,(car orange) :weight normal))))
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,(car green) :weight normal))))
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,(car magenta) :weight normal))))
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,(car yellow) :weight normal))))
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,(car teal) :weight normal))))
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,(car cyan) :weight normal))))
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,(car dark-blue) :weight normal))))
   `(rainbow-delimiters-mismatched-face ((,class (:foreground ,(car red) :background ,@(cdr red) :weight normal))))
   `(rainbow-delimiters-unmatched-face ((,class (:foreground ,(car red) :background ,@(cdr red) :weight normal))))

   ;; Smartparen
   `(show-paren-match    ((,class (:foreground ,(car cyan) :background ,(car base5) :weight bold))))
   `(show-paren-mismatch ((,class (:foreground ,(car bg) :background ,@(cdr red) :weight bold))))

   ;; multiple cursors
    `(mc/cursor-face ((,class (:inherit 'cursor))))

   ;; Js2
   `(js2-warning           ((,class (:underline (:color ,@(cdr red) :style wave)))))
   `(js2-function-param    ((,class (:foreground ,(car red)))))
   `(js2-function-call     ((,class (:foreground ,(car cyan)))))
   `(js2-object-property   ((,class (:foreground ,(car red)))))
   `(js2-jsdoc-tag         ((,class (:foreground ,(car violet)))))
   `(js2-external-variable ((,class (:foreground ,(car yellow)))))
   `(rjsx-attr             ((,class (:foreground ,(car yellow)))))
   `(rjsx-tag              ((,class (:foreground ,(car red)))))
   `(rjsx-tag-bracket-face ((,class (:foreground ,(car teal)))))
   `(rjsx-text             ((,class (:foreground ,(car green)))))
   
   ;; CSS / SCSS
   `(css-proprietary-property ((,class (:foreground ,(car red)))))
   `(css-property             ((,class (:foreground ,(car cyan)))))
   `(css-selector             ((,class (:foreground ,(car yellow)))))
   
   ;; markdown-mode
   `(markdown-header-face             ((,class (:foreground ,(car red) :weight bold))))
   `(markdown-header-delimiter-face   ((,class (:inherit 'markdown-header-face))))
   `(markdown-metadata-key-face       ((,class (:foreground ,(car red)))))
   `(markdown-list-face               ((,class (:foreground ,(car red)))))
   `(markdown-link-face               ((,class (:foreground ,(car red)))))
   `(markdown-url-face                ((,class (:foreground ,(car magenta) :weight normal))))
   `(markdown-italic-face             ((,class (:slant italic :foreground ,(car violet)))))
   `(markdown-bold-face               ((,class (:weight bold   :foreground ,(car orange)))))
   `(markdown-markup-face             ((,class (:foreground ,(car base5) :background ,(car bg-alt)))))
   `(markdown-blockquote-face         ((,class (:slant italic :foreground ,(car grey)))))
   `(markdown-pre-face                ((,class (:foreground ,(car teal) :background ,(car bg-alt)))))
   `(markdown-code-face               ((,class (:foreground ,(car teal) :background ,(car bg-alt)))))
   `(markdown-inline-code-face        ((,class (:inherit 'markdown-code-face))))
   `(markdown-reference-face          ((,class (:foreground ,(car grey)))))
   `(markdown-html-attr-name-face     ((,class (:inherit 'font-lock-variable-name-face))))
   `(markdown-html-attr-value-face    ((,class (:inherit 'font-lock-string-face))))
   `(markdown-html-entity-face        ((,class (:inherit 'font-lock-variable-name-face))))
   `(markdown-html-tag-delimiter-face ((,class (:inherit 'markdown-markup-face))))
   `(markdown-html-tag-name-face      ((,class (:inherit 'font-lock-keyword-face))))
   
   ;; web-mode
   `(web-mode-doctype-face           ((,class (:foreground ,(car base6)))))
   `(web-mode-html-tag-face          ((,class (:foreground ,(car red)))))
   `(web-mode-html-tag-bracket-face  ((,class (:foreground ,(car cyan)))))
   `(web-mode-html-attr-name-face    ((,class (:foreground ,(car yellow)))))
   `(web-mode-html-entity-face       ((,class (:foreground ,(car cyan) :slant italic))))
   `(web-mode-block-control-face     ((,class (:foreground ,(car yellow)))))
   `(web-mode-html-tag-bracket-face  ((,class (:foreground ,(car base8)))))
   
   ;; which-key
   `(which-key-key-face                   ((,class (:foreground ,(car green)))))
   `(which-key-group-description-face     ((,class (:foreground ,(car violet)))))
   `(which-key-command-description-face   ((,class (:foreground ,(car blue)))))
   `(which-key-local-map-description-face ((,class (:foreground ,(car magenta)))))
   
   ;; undo-tree
   `(undo-tree-visualizer-default-face       ((,class (:foreground ,(car base5)))))
   `(undo-tree-visualizer-current-face       ((,class (:foreground ,(car green) :weight bold))))
   `(undo-tree-visualizer-unmodified-face    ((,class (:foreground ,(car base5)))))
   `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,(car blue)))))
   `(undo-tree-visualizer-register-face      ((,class (:foreground ,(car yellow)))))
   
   ;; treemacs
   `(treemacs-root-face          ((,class (:inherit 'font-lock-string-face :weight bold :height 1.2))))
   `(treemacs-file-face          ((,class (:foreground ,(car fg)))))
   `(treemacs-directory-face     ((,class (:foreground ,(car fg)))))
   `(treemacs-tags-face          ((,class (:foreground ,(car base3)))))
   `(treemacs-git-modified-face  ((,class (:foreground ,(car violet)))))
   `(treemacs-git-added-face     ((,class (:foreground ,(car green)))))
   `(treemacs-git-conflict-face  ((,class (:foreground ,(car red)))))
   `(treemacs-git-untracked-face ((,class (:inherit 'font-lock-doc-face))))
   
   ;; ivy
   `(ivy-current-match              ((,class (:background ,(car bg-alt) :foreground ,(car cyan) :distant-foreground nil :extend t))))
   `(ivy-minibuffer-match-face-1    ((,class (:background nil :foreground ,(car base5) :weight light))))
   `(ivy-minibuffer-match-face-2    ((,class (:inherit 'ivy-minibuffer-match-face-1 :foreground ,(car magenta) :background ,(car base1) :weight semi-bold))))
   `(ivy-minibuffer-match-face-3    ((,class (:inherit 'ivy-minibuffer-match-face-2 :foreground ,(car green) :weight semi-bold))))
   `(ivy-minibuffer-match-face-4    ((,class (:inherit 'ivy-minibuffer-match-face-2 :foreground ,(car yellow) :weight semi-bold))))
   `(ivy-minibuffer-match-highlight ((,class (:foreground ,(car violet)))))
   `(ivy-highlight-face             ((,class (:foreground ,(car violet)))))
   `(ivy-confirm-face               ((,class (:foreground ,(car success)))))
   `(ivy-match-required-face        ((,class (:foreground ,(car error)))))
   `(ivy-virtual                    ((,class (:slant italic :foreground ,(car grey)))))
   `(ivy-modified-buffer            ((,class (:weight bold :foreground ,(car yellow)))))
   
   ;; flycheck
   `(flycheck-error     ((,class (:underline (:style wave :color ,(car error))))))
   `(flycheck-warning   ((,class (:underline (:style wave :color ,(car warning))))))
   `(flycheck-info      ((,class (:underline (:style wave :color ,(car success))))))
   
   ;; flycheck-posframe
   `(flycheck-posframe-face            ((,class (:inherit 'default))))
   `(flycheck-posframe-background-face ((,class (:background ,(car bg-alt)))))
   `(flycheck-posframe-error-face      ((,class (:inherit 'flycheck-posframe-face :foreground ,(car error)))))
   `(flycheck-posframe-info-face       ((,class (:inherit 'flycheck-posframe-face :foreground ,(car fg)))))
   `(flycheck-posframe-warning-face    ((,class (:inherit 'flycheck-posframe-face :foreground ,(car warning)))))
   
   ;; term
   `(term               ((,class (:foreground ,(car fg)))))
   `(term-bold          ((,class (:weight bold))))
   `(term-color-black   ((,class (:background ,(car base0)   :foreground ,(car base0)))))
   `(term-color-red     ((,class (:background ,(car red)     :foreground ,(car red)))))
   `(term-color-green   ((,class (:background ,(car green)   :foreground ,(car green)))))
   `(term-color-yellow  ((,class (:background ,(car yellow)  :foreground ,(car yellow)))))
   `(term-color-blue    ((,class (:background ,(car blue)    :foreground ,(car blue)))))
   `(term-color-magenta ((,class (:background ,(car magenta) :foreground ,(car magenta)))))
   `(term-color-cyan    ((,class (:background ,(car cyan)    :foreground ,(car cyan)))))
   `(term-color-white   ((,class (:background ,(car base8)   :foreground ,(car base8)))))
   
   ;; dired
   `(dired-directory  ((,class (:foreground ,(car teal)))))
   `(dired-ignored    ((,class (:foreground ,(car grey)))))
   `(dired-flagged    ((,class (:foreground ,(car red)))))
   `(dired-header     ((,class (:foreground ,(car blue)    :weight bold))))
   `(dired-mark       ((,class (:foreground ,(car orange)  :weight bold))))
   `(dired-marked     ((,class (:foreground ,(car cyan)    :weight bold))))
   `(dired-perm-write ((,class (:foreground ,(car fg)      :underline t))))
   `(dired-symlink    ((,class (:foreground ,(car dark-cyan) :weight bold))))
   `(dired-warning    ((,class (:foreground ,(car warning)))))
   
   ;; centaur-tabs
   `(centaur-tabs-default             ((,class (:background ,(car bg-alt) :foreground ,(car bg-alt)))))
   `(centaur-tabs-selected            ((,class (:background ,(car bg)     :foreground ,(car fg)))))
   `(centaur-tabs-unselected          ((,class (:background ,(car bg-alt) :foreground ,(car fg-alt)))))
   `(centaur-tabs-selected-modified   ((,class (:background ,(car bg)     :foreground ,(car teal)))))
   `(centaur-tabs-unselected-modified ((,class (:background ,(car bg-alt) :foreground ,(car teal)))))

   ;; PHP mode
   `(php-function-call      ((,class (:foreground ,(car cyan)))))
   `(php-method-call        ((,class (:foreground ,(car magenta)))))
   `(php-string-op          ((,class (:foreground ,(car green)))))
   `(php-variable-sigil     ((,class (:foreground ,(car blue)))))
   `(php-variable-name      ((,class (:foreground ,(car red)))))
   `(php-object-op          ((,class (:foreground ,(car orange)))))
   `(php-operator           ((,class (:foreground ,(car teal)))))
   `(php-property-name      ((,class (:foreground ,(car red)))))
   `(php-constant           ((,class (:foreground ,(car yellow)))))
   `(php-doc-annotation-tag ((,class (:foreground ,(car violet)))))
   `(php-doc-$this          ((,class (:foreground ,(car yellow)))))
   `(php-doc-$this-sigil    ((,class (:foreground ,(car yellow)))))
   `(php-doc-class-name     ((,class (:foreground ,(car green)))))
   `(php-doc-variable-sigil ((,class (:inherit 'php-variable-sigil))))
   ))

(custom-theme-set-variables
 'lrq-new
 '(ansi-color-names-vector [,(car base2) ,(car red) ,(car green) ,(car yellow) ,(car blue) ,(car dark-blue) ,(car orange) ,(car base8)]))

(provide-theme 'lrq-new)

;;; lrq-new-theme.el ends here
