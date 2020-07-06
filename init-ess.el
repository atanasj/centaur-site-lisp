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
  :config
  ;; auto-width
  (setq ess-auto-width 'window)
  ;; let lsp manage lintr
  (setq ess-use-flymake nil)
  ;; Stop R repl eval from blocking emacs.
  (setq ess-eval-visibly 'nowait)

  (setq ess-ask-for-ess-directory t
        ess-local-process-name "R"
        ansi-color-for-comint-mode 'filter
        comint-scroll-to-bottom-on-input t
        comint-scroll-to-bottom-on-output t
        comint-move-point-for-output t)
  ;; insert pipes etc...
  (defun ess-cent-insert-assign ()
    "Insert an assignment <-"
    (interactive)
    (insert " <- "))
  (defun ess-cent-insert-pipe ()
    "Insert a %>% and newline"
    (interactive)
    (insert " %>% "))
  (defun ess-cent-insert-assign ()
    "Insert an assignment <-"
    (interactive)
    (insert " <- "))
  ;; set keybindings
  ;; insert pipe
  (define-key ess-r-mode-map (kbd "M-s-'") 'ess-cent-insert-assign)
  (define-key inferior-ess-r-mode-map (kbd "M-s-'") 'ess-cent-insert-assign)
  ;; insert assign
  (define-key ess-r-mode-map (kbd "M-s-\"") 'ess-cent-insert-pipe)
  (define-key inferior-ess-r-mode-map (kbd "M-s-\"") 'ess-cent-insert-pipe)
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

;; Bring up empty R script and R console for quick calculations
(defun ess-cent-R-scratch ()
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

(global-set-key (kbd "C-x 9") 'ess-cent-R-scratch)

;; Not sure if need this as plymode has something similar
(defun ess-cent-shiny-run-app (&optional arg)
  "Interface for `shiny::runApp()'. With prefix ARG ask for extra args."
  (interactive)
  (inferior-ess-r-force)
  (ess-eval-linewise
   "shiny::runApp(\".\")\n" "Running app" arg
   '("" (read-string "Arguments: " "recompile = TRUE"))))

;; Graphics device management ;;
(defun ess-cent-new-gdev ()
  "create a new graphics device"
  (interactive)
  (ess-eval-linewise "dev.new()"))

(defun ess-cent-cur-gdev ()
  "return current graphics device"
  (interactive)
  (ess-eval-linewise "dev.cur()"))

(defun ess-cent-list-all-gdev ()
  "list all graphics devices"
  (interactive)
  (ess-eval-linewise "dev.list()"))

(defun ess-cent-switch-to-gdev ()
  "Prompt for the number of the graphics device to make current"
  (interactive)
  (setq dev-num
        (read-from-minibuffer "Select R graphics device: "
                              nil nil t t "1"))
  (ess-eval-linewise
   (format "dev.set(%s)" dev-num)))

(defun ess-cent-switch-next-gdev ()
  "switch to next available graphics device"
  (interactive)
  (ess-eval-linewise "dev.set(dev.next())"))

(defun ess-cent-switch-prev-gdev ()
  "switch to previous available graphics device"
  (interactive)
  (ess-eval-linewise "dev.set(dev.prev())"))

(defun ess-cent-save-gdev-pdf ()
  "Save current graphics device as pdf"
  (interactive)
  (ess-eval-linewise "dev.copy2pdf()"))

(defun ess-cent-capture-gdev ()
  "Capture current graphics device as image"
  (interactive)
  (ess-eval-linewise "dev.capture()"))

;; Devtools
(defun ess-cent-devtools-setup ()
  "setup R package in current working directory"
  (interactive)
  (ess-eval-linewise "devtools::setup()"))

;;======================================================================
;; (R) markdown mode
;;======================================================================

;; Insert new chunk for Rmarkdown
(defun ess-cent-insert-chunk (header)
  "Insert an r-chunk in markdown mode."
  (interactive "sLabel: ")
  (insert (concat "```{r " header "}\n\n```"))
  (forward-line -1))

(global-set-key (kbd "C-c M-i") 'ess-cent-insert-chunk)

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

;; provide ess configuration
(provide 'init-ess)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; init-ess.el ends here
