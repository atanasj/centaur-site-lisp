;;; init-package.el --- Initialize package configurations.	-*- lexical-binding: t -*-

;; Copyright (C) 2020 Atanas Janackovski

;; Author: Atanas Janackovski <atanas.atanas@gmail.com>

;; Note: the evil bindings are taken from the spacemacs, and the actual
;; configuration is taken from numerous others, including the spacesmacs one,
;; apologies that I cannot remember them all (if/when I do, I will be sure to
;; add).

;; spacemacs credit:

;; Author: Sylvain Benner <sylvain.benner@gmail.com>
;; URL: https://github.com/syl20bnr/spacemacs

;; This file is not part of GNU Emacs.
;;
;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth
;; Floor, Boston, MA 02110-1301, USA.
;;

;;; Commentary:
;;
;; Emacs Package configurations template.
;;

;;; Code:

;; (require 'init-const)
;; (require 'init-custom)
;; (require 'init-funcs)

;; ===========================================================
;; R-IDE
;; ===========================================================

(use-package ess
  :ensure t
  :defer t
  :init
  (require 'ess-site)
  ;; (require 'ess-view)
  ;; (require 'ess-R-data-view)
  ;; (require 'poly-R)
  :config
  ;; (setq ess-set-style 'RStudio) ; allow lsp-mode to control
  ;; auto-width
  (setq ess-auto-width 'window)
  ;; let lsp manage lintr
  (setq ess-use-flymake nil)
  ;; Toggle underscore off no replacement of _ for assign
  (setq ess-smart-S-assign-key nil)
  ;; Stop R repl eval from blocking emacs.
  (setq ess-eval-visibly 'nowait)

  (setq ess-ask-for-ess-directory t
        ess-local-process-name "R"
        ansi-color-for-comint-mode 'filter
        comint-scroll-to-bottom-on-input t
        comint-scroll-to-bottom-on-output t
        comint-move-point-for-output t)
  ;; insert pipes etc...
  (defun tide-insert-assign ()
    "Insert an assignment <-"
    (interactive)
    (insert " <- "))
  (defun tide-insert-pipe ()
    "Insert a %>% and newline"
    (interactive)
    (insert " %>% "))
  (defun tide-insert-assign ()
    "Insert an assignment <-"
    (interactive)
    (insert " <- "))
  ;; set keybindings
  ;; insert pipe
  (define-key ess-r-mode-map (kbd "M-s-'") 'tide-insert-assign)
  (define-key inferior-ess-r-mode-map (kbd "M-s-'") 'tide-insert-assign)
  ;; insert assign
  (define-key ess-r-mode-map (kbd "M-s-\"") 'tide-insert-pipe)
  (define-key inferior-ess-r-mode-map (kbd "M-s-\"") 'tide-insert-pipe)
  )

;; ess-view
;; open a df in an external app
;; TODO need to find out how to display options vertically
(use-package ess-view
  :ensure t
  :after ess
  :diminish
  :config
  (setq ess-view--spreadsheet-program "open") ; open in system default on macos
  (setq ess-view-inspect-and-save-df t)
  ;; enable ess-view package to be triggered from the source doc
  ;; see: <https://github.com/GioBo/ess-view/issues/9>
  (defun ess-view-extract-R-process ()
    "Return the name of R running in current buffer."
    (let*
        ((proc (ess-get-process))         ; Modified from (proc (get-buffer-process (current-buffer)))
         (string-proc (prin1-to-string proc))
         (selected-proc (s-match "^#<process \\(R:?[0-9]*\\)>$" string-proc)))
      (nth 1 (-flatten selected-proc))
      )
    )
  :bind
  (
   ("C-c C-e C-v" . ess-view-inspect-df)
   ;; the below doesn't work on osx
   ;; see <https://github.com/GioBo/ess-view/issues/5>
   ;; ("C-x q" . ess-view-inspect-and-save-df)
   )
  )

;; Insert new chunk for Rmarkdown
(defun aj/r-insert-chunk (header)
  "Insert an r-chunk in markdown mode."
  (interactive "sLabel: ")
  (insert (concat "```{r " header "}\n\n```"))
  (forward-line -1))

(global-set-key (kbd "C-c M-i") 'aj/r-insert-chunk)

;; Bring up empty R script and R console for quick calculations
(defun aj/R-scratch ()
  (interactive)
  (progn
    (delete-other-windows)
    (setq new-buf (get-buffer-create "scratch.R"))
    (switch-to-buffer new-buf)
    (R-mode)
    (setq w1 (selected-window))
    (setq w1name (buffer-name))
    (setq w2 (split-window w1 nil t))
    (if (not (member "*R*" (mapcar (function buffer-name) (buffer-list))))
        (R))
    (set-window-buffer w2 "*R*")
    (set-window-buffer w1 w1name)))

(global-set-key (kbd "C-x 9") 'aj/R-scratch)

;; Not sure if need this as plymode has something similar
(defun aj/ess-r-shiny-run-app (&optional arg)
  "Interface for `shiny::runApp()'. With prefix ARG ask for extra args."
  (interactive)
  (inferior-ess-r-force)
  (ess-eval-linewise
   "shiny::runApp(\".\")\n" "Running app" arg
   '("" (read-string "Arguments: " "recompile = TRUE"))))


;; ===========================================================
;; Polymode
;; ===========================================================

;; basic polymode
(use-package polymode
  :ensure t
  :init
  (use-package poly-R)
  (use-package poly-markdown)
  (use-package poly-noweb)
  :config
  (add-to-list 'auto-mode-alist '("\\.md" . poly-markdown-mode))
  (add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
  (add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))
  ;; Export files with the same name as the main file
  (setq polymode-exporter-output-file-format "%s")
  )

;; ===========================================================
;; IDE Functions
;; ===========================================================

;; (defun tide-draft-rmd ()
;;   "Draft a new Rmd file from a template interactively."
;;   (interactive)
;;   (setq rmd-file
;;         (read-from-minibuffer "Rmd Filename (draft_<date>.Rmd): "
;;                               nil nil t t
;;                               (format "draft_%s.Rmd"
;;                                       (string-trim
;;                                        (shell-command-to-string "date --iso-8601")))))
;;   (setq rmd-template
;;         (read-from-minibuffer
;;          (format "Draft %s from template (mmmisc/basic): " rmd-file)
;;          nil nil t t "mmmisc/basic"))
;;   (symbol-name rmd-template)
;;   (string-match "\\([^/]+\\)/\\([^/]+\\)"
;;                 (symbol-name rmd-template))
;;   (setq template-pkg
;;         (substring
;;          (symbol-name rmd-template)
;;          (match-beginning 1)
;;          (match-end 1)))
;;   (setq template-name
;;         (substring
;;          (symbol-name rmd-template)
;;          (match-beginning 2)
;;          (match-end 2)))
;;   (message "Drafting using template %s from package %s" template-name template-pkg)
;;   (ess-eval-linewise
;;    (format "rmarkdown::draft(file = \"%s\", template = \"%s\",
;;                 package = \"%s\", edit = FALSE)"
;;            rmd-file template-name template-pkg))
;;   )
;; Graphics device management ;;
(defun tide-new-gdev ()
  "create a new graphics device"
  (interactive)
  (ess-eval-linewise "dev.new()"))

(defun tide-cur-gdev ()
  "return current graphics device"
  (interactive)
  (ess-eval-linewise "dev.cur()"))

(defun tide-list-all-gdev ()
  "list all graphics devices"
  (interactive)
  (ess-eval-linewise "dev.list()"))

(defun tide-switch-to-gdev ()
  "Prompt for the number of the graphics device to make current"
  (interactive)
  (setq dev-num
        (read-from-minibuffer "Select R graphics device: "
                              nil nil t t "1"))
  (ess-eval-linewise
   (format "dev.set(%s)" dev-num)))

(defun tide-switch-next-gdev ()
  "switch to next available graphics device"
  (interactive)
  (ess-eval-linewise "dev.set(dev.next())"))

(defun tide-switch-prev-gdev ()
  "switch to previous available graphics device"
  (interactive)
  (ess-eval-linewise "dev.set(dev.prev())"))

(defun tide-save-gdev-pdf ()
  "Save current graphics device as pdf"
  (interactive)
  (ess-eval-linewise "dev.copy2pdf()"))

(defun tide-capture-gdev ()
  "Capture current graphics device as image"
  (interactive)
  (ess-eval-linewise "dev.capture()"))

;; Devtools
(defun tide-devtools-setup ()
  "setup R package in current working directory"
  (interactive)
  (ess-eval-linewise "devtools::setup()"))

;; Shiny
(defun tide-shiny-run-app ()
  "Run a shiny app in the current working directory"
  (interactive)
  (ess-eval-linewise "shiny::runApp()"))

;; ;; Rmarkdowm
;; (defun tide-rmd-rend ()
;;   "Render rmarkdown files with an interactive selection prompt"
;;   (interactive)
;;   (ess-eval-linewise "mmmisc::rend()"))

;; Data Views
;; (defun df-at-point-to-buffer (&optional numrows)
;;   "output a sample of another data.frame to and jump to buffer."
;;   (let ((object (symbol-at-point))
;;         (r-process (ess-get-process))
;;         (r-output-buffer (get-buffer-create "*R-output*"))
;;         (numrows (or numrows 300)))
;;     (ess-command
;;      (format "mmmisc::df_preview(%s, %s)\n" object numrows)
;;      r-output-buffer nil nil nil r-process)
;;     (switch-to-buffer-other-window r-output-buffer)
;;     ))

;; (defun df-sample-small ()
;;   "Sample and print 30 rows of a data.frame"
;;   (interactive)
;;   (df-at-point-to-buffer 30)
;;   )

;; (defun df-sample-medium ()
;;   "Sample and print 300 rows of a data.frame"
;;   (interactive)
;;   (df-at-point-to-buffer 300)
;;   )

;; (defun df-sample-large ()
;;   "Sample and print 3000 rows of a data.frame"
;;   (interactive)
;;   (df-at-point-to-buffer 3000)
;;   )


;;======================================================================
;; (R) markdown mode
;;======================================================================

;; Insert a new (empty) chunk to R markdown ============================
;; ;; (defun insert-chunk ()
;; ;;   "Insert chunk environment Rmd sessions."
;; ;;   (interactive)
;; ;;   (insert "```{r}\n\n```")
;; ;;   (forward-line -1)
;; ;;   )
;; ;; ;; key binding
;; (global-set-key (kbd "C-c i") 'insert-chunk)

;; Mark a word at a point ==============================================
;; http://www.emacswiki.org/emacs/ess-edit.el
(defun ess-edit-word-at-point ()
  (save-excursion
    (buffer-substring
     (+ (point) (skip-chars-backward "a-zA-Z0-9._"))
     (+ (point) (skip-chars-forward "a-zA-Z0-9._")))))
;; eval any word where the cursor is (objects, functions, etc)
(defun ess-eval-word ()
  (interactive)
  (let ((x (ess-edit-word-at-point)))
    (ess-eval-linewise (concat x)))
  )
;; key binding
(define-key ess-mode-map (kbd "C-c r") 'ess-eval-word)



;; ===========================================================
;; Evil keybindings
;; ===========================================================

;; :major-modes
(general-define-key
 :keymaps 'ess-r-mode-map
 :major-modes t
 :states '(normal insert emacs)
 :prefix "SPC"
 :non-normal-prefix "M-SPC"
 ;; "'"  'spacemacs/ess-start-repl
 ;; "si" 'spacemacs/ess-start-repl
 ;; noweb
 "c" '(:ignore t :which-key "R-noweb")
 "cC" 'ess-eval-chunk-and-go
 "cc" 'ess-eval-chunk
 "cd" 'ess-eval-chunk-and-step
 "cm" 'ess-noweb-mark-chunk
 "cN" 'ess-noweb-previous-chunk
 "cn" 'ess-noweb-next-chunk
 ;; REPL
 "s" '(:ignore t :wk "R-repl")
 "sB" 'ess-eval-buffer-and-go
 "sb" 'ess-eval-buffer
 "e" 'ess-eval-paragraph-and-step
 "f" 'ess-eval-function
 "i" 'ess-interrupt
 "o"  'ess-eval-word
 "R" 'ess-eval-region
 "sp" 'ess-eval-paragraph-and-step
 "sd" 'ess-eval-region-or-line-and-step
 "sl" 'ess-eval-line
 "sr" 'ess-eval-region
 "st" 'ess-eval-function
 "sw" 'ess-set-working-directory
 ;; R data viewers
 ;; "vs" 'df-sample-small
 ;; "vm" 'df-sample-medium
 ;; "vl" 'df-sample-large
 ;; Package Dev helpers
 "d" '(:ignore t :wk "R-devtools")
 "di" 'ess-r-devtools-install-package
 "dt" 'ess-r-devtools-test-package
 "dl" 'ess-r-devtools-load-package
 "dc" 'ess-r-devtools-check-package
 "dd" 'ess-r-devtools-document-package
 "df" 'ess-roxy-update-entry
 "ds" 'tide-devtools-setup
 ;; R help
 "h" '(:ignore t :wk "R-help")
 "ho" 'ess-display-help-on-object
 "hi" 'ess-display-index
 "ha" 'ess-display-help-apropos
 ;; Graphics devices
 "g" '(:ignore t :wk "R-graphics")
 "gn" 'tide-new-gdev
 "gc" 'tide-cur-gdev
 "gs" 'tide-switch-to-gdev
 "gl" 'tide-list-all-gdev
 "gp" 'tide-save-gdev-pdf
 "gc" 'tide-capture-gdev
 "gj" 'tide-switch-next-gdev
 "gk" 'tide-switch-prev-gdev
 ;; R Markdown
 "rc" 'aj/r-insert-chunk
 ;; "rr" 'tide-rmd-rend
 ;; "rd" 'tide-draft-rmd
 ;; Shiny
 "Sr" 'tide-shiny-run-app
 )

;; provide ess configuration
(provide 'init-ess)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-package.el ends here
