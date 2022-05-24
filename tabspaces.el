;;; tabspaces.el --- Leverage tab-bar and project for buffer-isolated workspaces  -*- lexical-binding: t -*-

;; Author: Colin McLear <mclear@fastmail.com>
;; Maintainer: Colin McLear
;; Version: 1.2
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

;;; Acknowledgements
;; Much of the package code is inspired by:

;; - https://github.com/kaz-yos/emacs
;; - https://github.com/wamei/elscreen-separate-buffer-list/issues/8
;; - https://www.rousette.org.uk/archives/using-the-tab-bar-in-emacs/
;; - https://github.com/minad/consult#multiple-sources
;; - https://github.com/florommel/bufferlo

;;; Code:

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

(defgroup tabspaces nil
  "Manage tab/workspace buffers."
  :group 'convenience)

(defcustom tabspaces-default-tab "Default"
  "Specify a default tab by name TAB."
  :group 'tabspaces
  :type 'string)

(defcustom tabspaces-remove-to-default t
  "Add buffer to default tabspace when removed from current tabspace."
  :group 'tabspaces
  :type 'boolean)

(defcustom tabspaces-include-buffers '("*scratch*")
  "Buffers that should always get included in a new tab or frame.
This is a list of regular expressions that match buffer names.
This overrides buffers excluded by `tabspaces-exclude-buffers.'"
  :type '(repeat string)
  :group 'tabspaces)

(defcustom tabspaces-use-filtered-buffers-as-default nil
  "When t, remap `switch-to-buffer' to `tabspaces-switch-to-buffer'."
  :type 'boolean)

(defcustom tabspaces-keymap-prefix (kbd "C-c C-w")
  "Key prefix for the tabspaces-prefix-map keymap."
  :group 'tabspaces
  :type 'string)

;;;; Create Buffer Workspace

(defun tabspaces-reset-buffer-list ()
  "Reset the current tab's `buffer-list'.
Only the current window buffers and buffers in
`tabspaces-include-buffers' are kept in the `buffer-list' and
`buried-buffer-list'."
  (interactive)
  ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Current-Buffer.html
  ;; The current-tab uses `buffer-list' and `buried-buffer-list'.
  ;; A hidden tab keeps these as `wc-bl' and `wc-bbl'.
  (set-frame-parameter nil
                       'buffer-list
                       (let ((window-buffers (mapcar #'window-buffer (window-list))))
                         (seq-filter (lambda (buffer)
                                       (or (member buffer window-buffers)
                                           (member (buffer-name buffer)
                                                   tabspaces-include-buffers)))
                                     (frame-parameter nil 'buffer-list))))
  (set-frame-parameter nil
                       'buried-buffer-list
                       (seq-filter (lambda (buffer)
                                     (member (buffer-name buffer)
                                             tabspaces-include-buffers))
                                   (frame-parameter nil 'buried-buffer-list))))

(defun tabspaces--tab-post-open-function (_tab)
  "Reset buffer list on new tab creation."
  (tabspaces-reset-buffer-list))

;;;; Filter Workspace Buffers

(defun tabspaces-local-buffer-p (buffer)
  "Return whether BUFFER is in the list of local buffers."
  (memq buffer (frame-parameter nil 'buffer-list)))

(defun tabspaces--set-buffer-predicate (frame)
  "Set the buffer predicate of FRAME to `tabspaces-local-buffer-p'."
  (set-frame-parameter frame 'buffer-predicate #'tabspaces-local-buffer-p))

(defun tabspaces--reset-buffer-predicate (frame)
  "Reset the buffer predicate of FRAME if it is `tabspaces-local-buffer-p'."
  (when (eq (frame-parameter frame 'buffer-predicate) #'tabspaces-local-buffer-p)
    (set-frame-parameter frame 'buffer-predicate nil)))

(defun tabspaces-buffer-list (&optional frame tabnum)
  "Return a list of all live buffers associated with the current frame and tab.
A non-nil value of FRAME selects a specific frame instead of the
current one. If TABNUM is nil, the current tab is used. If it is
non-nil, then specify a tab index in the given frame."
  (let ((list
         (if tabnum
             (let ((tab (nth tabnum (frame-parameter frame 'tabs))))
               (if (eq 'current-tab (car tab))
                   (frame-parameter frame 'buffer-list)
                 (or
                  (cdr (assq 'wc-bl tab))
                  (mapcar 'get-buffer
                          (car (cdr (assq #'tabspaces-buffer-list (assq 'ws tab))))))))
           (frame-parameter frame 'buffer-list))))
    (seq-filter #'buffer-live-p list)))

;;;; Project Workspace Helper Functions

(defun tabspaces--list-tabspaces ()
  "Return a list of `tab-bar' tabs/workspaces."
  (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs)))

(defun tabspaces--project-name ()
  "Get name for project from vc.
If not in a project return buffer filename, or `-' if not visiting a file."
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

(defun tabspaces--add-to-default-tabspace (buffer)
  "Add BUFFER to default tabspace buffer list."
  (let ((tab-names (mapcar
                    (lambda (tab) (alist-get 'name tab))
                    (funcall tab-bar-tabs-function))))
    (when (and tabspaces-remove-to-default
               (member tabspaces-default-tab tab-names))
      ;; add buffer to default tabspace
      (tab-bar-select-tab-by-name tabspaces-default-tab)
      (display-buffer buffer)
      (switch-to-buffer buffer t nil)
      (if (one-window-p t)
          (previous-buffer)
        (delete-window))
      (tab-bar-switch-to-recent-tab))))

;;;; Interactive Functions

;;;;; Open Project & File
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
  (let ((project-switch-commands #'project-find-file))
    (project-switch-project dir)))

;;;;; Buffer Functions

(defun tabspaces-remove-selected-buffer (buffer)
  "Remove selected BUFFER from the frame's buffer list.
If `tabspaces-remove-to-default' is t then add the buffer to the
default tabspace."
  (interactive
   (list
    (let ((blst (mapcar (lambda (b) (buffer-name b))
                        (tabspaces-buffer-list))))
      ;; select buffer
      (read-buffer "Remove buffer from tabspace: " nil t
                   (lambda (b) (member (car b) blst))))))
  ;; delete window of buffer
  (cond ((eq buffer (window-buffer (selected-window)))
         (if (one-window-p t)
             (bury-buffer)
           (delete-window)))
        ((get-buffer-window buffer)
         (select-window (get-buffer-window buffer) t)
         (if (one-window-p t)
             (bury-buffer)
           (delete-window)))
        (t
         (message "buffer removed from tabspace")))
  ;; delete buffer from tabspace buffer list
  (delete (get-buffer buffer) (frame-parameter nil 'buffer-list))
  ;; add buffer to default tabspace
  (tabspaces--add-to-default-tabspace buffer))

(defun tabspaces-remove-current-buffer (&optional buffer-or-name)
  "Bury and remove current buffer BUFFER-OR-NAME from the tabspace list.
If `tabspaces-remove-to-default' is t then add the buffer to the
default tabspace."
  (interactive)
  (let ((buffer (or buffer-or-name (current-buffer))))
    (delete (get-buffer buffer) (frame-parameter nil 'buffer-list))
    (bury-buffer buffer-or-name)
    (tabspaces--add-to-default-tabspace buffer)))

(defun tabspaces-switch-to-buffer (buffer &optional norecord force-same-window)
  "Display the local buffer BUFFER in the selected window.
This is the frame/tab-local equivilant to `switch-to-buffer'.
The arguments NORECORD and FORCE-SAME-WINDOW are passed to `switch-to-buffer'."
  (interactive
   (list
    (let ((blst (mapcar #'buffer-name (tabspaces-buffer-list))))
      (read-buffer
       "Switch to local buffer: " blst nil
       (lambda (b) (member (if (stringp b) b (car b)) blst))))))
  (switch-to-buffer buffer norecord force-same-window))

(defun tabspaces-clear-buffers (&optional frame)
  "Clear the tabspace's buffer list, except for the current buffer.
If FRAME is nil, use the current frame."
  (interactive)
  (set-frame-parameter frame 'buffer-list
                       (list (if frame
                                 (with-selected-frame frame
                                   (current-buffer))
                               (current-buffer)))))

;;;;; Switch or Create Workspace
;; Some convenience functions for opening/closing workspaces and buffers.
;; Some of these are just wrappers around built-in functions.
;;;###autoload
(defalias 'tabspaces-switch-or-create-workspace #'tab-bar-switch-to-tab)

;;;;; Close Workspace
(defalias 'tabspaces-close-workspace #'tab-bar-close-tab)

;;;;; Close Workspace & Kill Buffers
(defun tabspaces-kill-buffers-close-workspace ()
  "Kill all buffers in the workspace and then close the workspace itself."
  (interactive)
  (let ((buf (tabspaces-buffer-list)))
    (unwind-protect
        (cl-loop for b in buf
                 do (kill-buffer b))
      (tab-bar-close-tab))))

;;;;; Open or Create Project in Workspace
;;;###autoload
(defun tabspaces-open-or-create-project-and-workspace (&optional project)
  "Open PROJECT from `project--list' in its own workspace.
If PROJECT is already open in its own workspace, switch to that workspace.
If PROJECT does not exist, create it, along with a `project.todo' file, in its own workspace."
  (interactive
   (if (eq project--list 'unset)
       (call-interactively #'project-switch-project)
     (list
      (completing-read "Project Name: " project--list))))
  (cond ((member (list project) project--list)
         (if (member (file-name-nondirectory (directory-file-name project)) (tabspaces--list-tabspaces))
             (tabspaces-switch-or-create-workspace (file-name-nondirectory (directory-file-name project)))
           (tab-bar-new-tab)
           (let ((project-switch-commands #'project-find-file))
             (project-switch-project project))
           (tab-bar-rename-tab (tabspaces--name-tab-by-project-or-default))))
        (t
         (tab-bar-new-tab)
         (setq default-directory project)
         (ignore-errors (mkdir project t))
         (if (featurep 'magit)
             (magit-init project)
           (call-interactively #'vc-create-repo))
         (delete-other-windows)
         (with-temp-buffer (write-file "project-todo.org"))
         (if (featurep 'magit)
             (magit-status-setup-buffer)
           (project-vc-dir))
         (dired-jump-other-window)
         (tab-bar-rename-tab (file-name-nondirectory (directory-file-name (vc-root-dir))))
         ;; make sure project.el remembers new project
         (let ((pr (project--find-in-directory default-directory)))
           (project-remember-project pr)))))

;;;; Define Keymaps
(defvar tabspaces-command-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C") 'tabspaces-clear-buffers)
    (define-key map (kbd "b") 'tabspaces-switch-to-buffer)
    (define-key map (kbd "d") 'tabspaces-close-workspace)
    (define-key map (kbd "k") 'tabspaces-kill-buffers-close-workspace)
    (define-key map (kbd "o") 'tabspaces-open-or-create-project-and-workspace)
    (define-key map (kbd "r") 'tabspaces-remove-current-buffer)
    (define-key map (kbd "R") 'tabspaces-remove-selected-buffer)
    (define-key map (kbd "s") 'tabspaces-switch-or-create-workspace)
    map)
  "Keymap for tabspace/workspace commands after `tabspaces-keymap-prefix'.")
(fset 'tabspaces-command-map tabspaces-command-map)

(defvar tabspaces-mode-map
  (let ((map (make-sparse-keymap)))
    (when tabspaces-keymap-prefix
      (define-key map tabspaces-keymap-prefix 'tabspaces-command-map))
    map)
  "Keymap for Tabspaces mode.")

;;;; Define Minor Mode
;;;###autoload
(define-minor-mode tabspaces-mode
  "Create a global minor mode for `tabspaces', or buffer-isolated workspaces.
This uses Emacs `tab-bar' and `project.el'."
  :lighter ""
  :keymap tabspaces-mode-map
  :global t
  (cond (tabspaces-mode
         ;; Set up tabspace isolated buffers
         (dolist (frame (frame-list))
           (tabspaces--set-buffer-predicate frame)
           (add-hook 'after-make-frame-functions #'tabspaces--set-buffer-predicate)
           (add-to-list 'tab-bar-tab-post-open-functions #'tabspaces--tab-post-open-function)
           ;; Option to always use filtered buffers when minor mode is enabled.
           (when tabspaces-use-filtered-buffers-as-default
             ;; Remap switch-to-buffer
             (define-key (current-global-map) [remap switch-to-buffer] #'tabspaces-switch-to-buffer))))
        (t
         ;; Remove all modifications
         (dolist (frame (frame-list))
           (tabspaces--reset-buffer-predicate frame))
         (define-key (current-global-map) [remap switch-to-buffer] nil)
         (setq tab-bar-tab-post-open-functions (remove #'tabspaces--tab-post-open-function tab-bar-tab-post-open-functions))
         (remove-hook 'after-make-frame-functions #'tabspaces--set-buffer-predicate))))

;;; Provide
(provide 'tabspaces)
;;; tabspaces.el ends here
