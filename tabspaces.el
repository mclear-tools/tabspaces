;;; tabspaces.el --- Leverage tab-bar and project for buffer-isolated workspaces  -*- lexical-binding: t -*-

;; Author: Colin McLear <mclear@fastmail.com>
;; Maintainer: Colin McLear
;; Version: 1.0
;; Package-Requires: ((emacs "27.1") (project "0.8.1"))
;; Keywords: convenience, frames
;; Homepage: https://github.com/mclear-tools/tabspaces

;; Copyright (C) 2022 Colin McLear

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; This package provides several functions to facilitate a single frame-based
;; workflow with one workspace per tab, integration with project.el (for
;; project-based workspaces) and buffer isolation per tab (i.e. a "tabspace"
;; workspace). The package assumes project.el and tab-bar.el are both present
;; (they are built-in to Emacs 27.1+).

;; This file is not part of GNU Emacs.

;; Much of the package code is inspired by:

;; - https://github.com/kaz-yos/emacs
;; - https://github.com/wamei/elscreen-separate-buffer-list/issues/8
;; - https://www.rousette.org.uk/archives/using-the-tab-bar-in-emacs/
;; - https://github.com/minad/consult#multiple-sources

;;; Code:

(defgroup tabspaces nil
  "Settings for workspaces."
  :group 'tabspaces)

;;;; Requirements

(require 'tab-bar)
(require 'project)
(require 'vc)
(require 'seq)
(require 'cl-lib)
(require 'dired-x)

(declare-function magit-init "magit-status")
(declare-function magit-status-setup-buffer "magit-status")

;;;; Variables

(defcustom tabspaces-workspace-create-permitted-buffer-names '("*scratch*")
  "List of buffer names kept by `tabspaces-create-workspace'."
  :type 'string
  :group 'tabspaces
  :version "27.1")

(defcustom tabspaces-use-filtered-buffers-as-default nil
  "When t, remap `switch-to-buffer' to `tabspaces-switch-to-buffer'."
  :type 'boolean)

;;;; Create Buffer Workspace

;;;###autoload
(defun tabspaces-create-workspace (&optional arg)
  "Create a new tab/workspace with cleaned buffer lists.

ARG is directly passed to `tabbar-new-tab'.
Only buffers in `tabspaces--workspace-create-permitted-buffer-names'
are kept in the `buffer-list' and `buried-buffer-list'.
This is similar to `elscreen-create'."
  (interactive)
  (tab-bar-new-tab arg)
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Current-Buffer.html
  ;; The current-tab uses `buffer-list' and `buried-buffer-list'.
  ;; A hidden tab keeps these as `wc-bl' and `wc-bbl'.
  (set-frame-parameter nil
                       'buffer-list
                       (seq-filter (lambda (buffer)
                                     (member (buffer-name buffer)
                                             tabspaces-workspace-create-permitted-buffer-names))
                                   (frame-parameter nil 'buffer-list)))
  (set-frame-parameter nil
                       'buried-buffer-list
                       (seq-filter (lambda (buffer)
                                     (member (buffer-name buffer)
                                             tabspaces-workspace-create-permitted-buffer-names))
                                   (frame-parameter nil 'buried-buffer-list))))

;; NOTE: to clone tab/workspace with all buffers use tab-bar-duplicate-tab

;;;; Filter Workspace Buffers

;;;;; Filter Buffers By Tab
;; tab-bar version of separate buffer list filter
;; See https://github.com/wamei/elscreen-separate-buffer-list/issues/8
;; https://github.com/kaz-yos/emacs/blob/master/init.d/200_tab-related.el#L74-L87

(defun tabspaces--tab-bar-buffer-name-filter (buffer-names)
  "Filter BUFFER-NAMES by the current tab's buffer list.
It should be used to filter a list of buffer names created by
other functions, such as `helm-buffer-list'."
  (let ((buffer-names-to-keep
         ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-List.html
         (append (mapcar #'buffer-name (alist-get 'wc-bl (tab-bar--tab)))
                 (mapcar #'buffer-name (alist-get 'wc-bbl (tab-bar--tab))))))
    (seq-filter (lambda (elt)
                  (member elt buffer-names-to-keep))
                buffer-names)))

;;;;; Filtered Switch to Buffer Function

(defun tabspaces--buffer-list-all ()
  "List all buffers for workspace."
  (cl-loop for b in (buffer-list)
           for bn = (buffer-name b)
           collect bn))

(defun tabspaces-switch-to-buffer ()
  "Filter open buffers on a per-workspace basis."
  (interactive)
  (switch-to-buffer
   (completing-read "Switch to workspace buffer: "
                    (tabspaces--tab-bar-buffer-name-filter
                     (tabspaces--buffer-list-all)))))

;;;; Project Workspace Helper Functions

(defun tabspaces--list-tabspaces ()
  "Return a list of `tab-bar' tabs/workspaces."
  (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs)))

(defun tabspaces--project-name ()
  "Get name for project from vc.
If not a in project return buffer filename, or `-' if not visiting a file."
  (let ((buf (buffer-file-name)))
    (cond ((and buf (vc-registered buf))
           (file-name-nondirectory (directory-file-name (vc-root-dir))))
          (t "-"))))

(defun tabspaces--name-tab-by-project-or-default ()
  "Return project name if in a project, or default tab-bar name if not.
The default tab-bar name uses the buffer name along with a counter."
  (let ((project-name (tabspaces--project-name))
        (tab (tab-bar-tab-name-current)))
    (cond ((string= tab project-name)
           (tab-bar-switch-to-tab tab))
          ((string= "-" project-name)
           (tab-bar-tab-name-current-with-count))
          (t (tabspaces--project-name)))))

(defun tabspaces-project-switch-project-open-file (dir)
  "Switch to another project by running an Emacs command.
Open file using `project-find-file'. NOTE: this function does *not*
open or switch to a new workspace. Rather it switches to a new
project and opens a file via `completing-read'. If you prefer to
use the project.el command-menu, then use
`project-switch-project'

When called, this function will use the project corresponding
to the selected directory DIR."
  (interactive (list (project-prompt-project-dir)))
  (let ((default-directory dir)
        (project-current-inhibit-prompt t))
    (call-interactively #'project-find-file)))

;;;; New VC Project
(defun tabspaces--create-new-vc-project ()
  "Initialize a new version control repo and add it to project.el's known projects."
  (let ((project-dir (file-name-as-directory (expand-file-name
                                              (read-directory-name "New project root:")))))
    (progn
      (setq default-directory project-dir)
      (if (featurep 'magit)
          (progn
            (require 'magit)
            (magit-init project-dir))
        (progn
          (mkdir project-dir)
          (call-interactively #'vc-create-repo)))
      ;; make sure project.el remembers new project
      (let ((pr (project--find-in-directory default-directory)))
        (project-remember-project pr)))))

;;;; Interactive Functions
;;;;; Switch to or Create Workspace

;;;###autoload
(defun tabspaces-switch-to-or-create-workspace ()
  "Switch to existing workspace.
If workspace does not exist, then allow the creation of a
new,named workspace on the fly."
  (interactive)
  (let* ((tab-names (mapcar (lambda (tab) (alist-get 'name tab)) (funcall tab-bar-tabs-function)))
         (tab-name (completing-read "Switch to Workspace: " tab-names)))
    (if (member tab-name tab-names)
        (tab-bar-select-tab-by-name tab-name)
      (progn
        (tabspaces-create-workspace)
        (tab-bar-new-tab)
        (tab-bar-rename-tab tab-name)))))

;;;;; Open Project in New Workspace

;;;###autoload
(defun tabspaces-open-existing-project-and-workspace ()
  "Open an existing project as its own workspace."
  (interactive)
  (progn
    (tabspaces-create-workspace)
    (call-interactively #'tabspaces-project-switch-project-open-file)
    (tab-bar-rename-tab (tabspaces--name-tab-by-project-or-default))))

;;;;;  Create & Open New Project in New Workspace

;;;###autoload
(defun tabspaces-create-new-project-and-workspace ()
  "Create & open a new version-controlled project as its own workspace.
Create a `project-todo.org' file. This will use magit if
available, otherwise it will use the built-in vc library."
  (interactive)
  (progn
    (tabspaces-create-workspace)
    (tabspaces--name-tab-by-project-or-default)
    (tabspaces--create-new-vc-project)
    (delete-other-windows)
    (with-temp-buffer (write-file "project-todo.org"))
    (if (featurep 'magit)
        (progn
          (require 'magit)
          (magit-status-setup-buffer))
      (project-vc-dir))
    (dired-jump-other-window)))

;;;;; Switch Workspace

;; Just a wrapper around tab-bar
(defun tabspaces-switch-workspace ()
  "Switch workspace via tab-bar."
  (interactive)
  (call-interactively #'tab-bar-switch-to-tab))

;;;;; Close Workspace
;; Some convenience functions for closing workspaces and buffers
;; these are just wrappers around built-in functions

(defun tabspaces-close-workspace ()
  "Close workspace."
  (interactive)
  (tab-bar-close-tab))

(defun tabspaces-kill-buffers-close-workspace ()
  "Kill all buffers in the workspace and then close the workspace itself."
  (interactive)
  (let ((buf (tabspaces--tab-bar-buffer-name-filter (tabspaces--buffer-list-all))))
    (unwind-protect
        (cl-loop for b in buf
                 do (kill-buffer b))
      (tab-bar-close-tab))))

;;;; Define Minor Mode
;;;###autoload
(defvar tabspaces-prefix-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-w c") 'tabspaces-create-workspace)
    (define-key map (kbd "C-c C-w b") 'tabspaces-switch-to-buffer)
    (define-key map (kbd "C-c C-w d") 'tabspaces-close-workspace)
    (define-key map (kbd "C-c C-w k") 'tabspaces-kill-buffers-close-workspace)
    (define-key map (kbd "C-c C-w n") 'tabspaces-create-new-project-and-workspace)
    (define-key map (kbd "C-c C-w o") 'tabspaces-open-existing-project-and-workspace)
    (define-key map (kbd "C-c C-w s") 'tabspaces-switch-to-or-create-workspace)
    map)
  "Keymap for workspace commands.")

;;;###autoload
(define-minor-mode tabspaces-mode
  "Create a global minor mode for buffer-isolated workspaces.
This uses Emacs `tab-bar' and `project.el'."
  :lighter ""
  :keymap tabspaces-prefix-map
  :global t

  ;; Option to always use filtered buffers when minor mode is enabled.
  (when tabspaces-use-filtered-buffers-as-default
    (define-key (current-global-map) [remap switch-to-buffer] #'tabspaces-switch-to-buffer)))

;;; Provide
(provide 'tabspaces)
;;; tabspaces.el ends here
