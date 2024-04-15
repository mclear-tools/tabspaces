;;; tabspaces.el --- Leverage tab-bar and project for buffer-isolated workspaces  -*- lexical-binding: t -*-

;; Author: Colin McLear <mclear@fastmail.com>
;; Maintainer: Colin McLear
;; Version: 1.5
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

;; This package provides several functions to facilitate a frame-based
;; tab workflow with one workspace per tab, integration with project.el (for
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
This is a list of regular expressions that match buffer names,
which overrides buffers excluded by `tabspaces-exclude-buffers'."
  :group 'tabspaces
  :type '(repeat string))

(defcustom tabspaces-exclude-buffers nil
  "Buffers that should always get excluded in a new tab or frame.
This is a list of regular expressions that match buffer names,
which does not override buffers inside `tabspaces-include-buffers'."
  :group 'tabspaces
  :type '(repeat string))

(defcustom tabspaces-use-filtered-buffers-as-default nil
  "When t, remap `switch-to-buffer' to `tabspaces-switch-to-buffer'."
  :group 'tabspaces
  :type 'boolean)

(defcustom tabspaces-keymap-prefix "C-c TAB"
  "Key prefix for the tabspaces-prefix-map keymap."
  :group 'tabspaces
  :type 'string)

(defcustom tabspaces-initialize-project-with-todo t
  "When Non-nil create a `tabspaces-todo-file-name' file in the project
when creating a workspace for it."
  :group 'tabspaces
  :type 'boolean)

(defcustom tabspaces-todo-file-name "project-todo.org"
  "The name of the TODO file to create if non-existing for new workspaces."
  :group 'tabspaces
  :type 'string)

(defcustom tabspaces-project-switch-commands project-switch-commands
  "Available commands when switch between projects.
Change this value if you wish to run a specific command, such as
`find-file' on project switch.  Otherwise this will default to
the value of `project-switch-commands'."
  :group 'tabspaces
  :type 'sexp)

(defcustom tabspaces-fully-resolve-paths nil
  "Resolve \".\", \"..\", etc. in project paths."
  :group 'tabspaces
  :type 'boolean)

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
                                           (and (member (buffer-name buffer)
                                                        tabspaces-include-buffers)
                                                (not (member (buffer-name buffer)
                                                             tabspaces-exclude-buffers)))))
                                     (frame-parameter nil 'buffer-list))))
  (set-frame-parameter nil
                       'buried-buffer-list
                       (seq-filter (lambda (buffer)
                                     (and (member (buffer-name buffer)
                                                  tabspaces-include-buffers)
                                          (not (member (buffer-name buffer)
                                                       tabspaces-exclude-buffers))))
                                   (frame-parameter nil 'buried-buffer-list))))

(defun tabspaces--tab-post-open-function (_tab)
  "Reset buffer list on new tab creation."
  (tabspaces-reset-buffer-list))

;;;; Filter Workspace Buffers

(defun tabspaces--local-buffer-p (buffer)
  "Return whether BUFFER is in the list of local buffers."
  (or (member (buffer-name buffer) tabspaces-include-buffers)
      (memq buffer (frame-parameter nil 'buffer-list))))

(defun tabspaces--set-buffer-predicate (frame)
  "Set the buffer predicate of FRAME to `tabspaces--local-buffer-p'."
  (set-frame-parameter frame 'buffer-predicate #'tabspaces--local-buffer-p))

(defun tabspaces--reset-buffer-predicate (frame)
  "Reset the buffer predicate of FRAME if it is `tabspaces--local-buffer-p'."
  (when (eq (frame-parameter frame 'buffer-predicate) #'tabspaces--local-buffer-p)
    (set-frame-parameter frame 'buffer-predicate nil)))

(defun tabspaces--buffer-list (&optional frame tabnum)
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
                          (car (cdr (assq #'tabspaces--buffer-list (assq 'ws tab))))))))
           (frame-parameter frame 'buffer-list))))
    (seq-filter #'buffer-live-p list)))

;;;; Project Workspace Helper Functions

;;;###autoload
(defun tabspaces--current-tab-name ()
  "Get name of current tab."
  (cdr (assq 'name (tab-bar--current-tab))))

;;;###autoload
(defun tabspaces--list-tabspaces ()
  "Return a list of `tab-bar' tabs/workspaces."
  (mapcar (lambda (tab) (alist-get 'name tab)) (tab-bar-tabs)))

;;;###autoload
(defun tabspaces--project-name ()
  "Get name for project from vc.
If not in a project return buffer filename, or `-' if not visiting a file."
  (let ((buf (buffer-file-name)))
    (cond ((and buf (vc-registered buf))
           (file-name-nondirectory (directory-file-name (vc-root-dir))))
          (t "-"))))

;;;###autoload
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

;;;###autoload
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
  (let ((project-switch-commands tabspaces-project-switch-commands))
    (project-switch-project dir)))

;;;;; Buffer Functions

(defun tabspaces-remove-buffer (&optional buffer)
  "Bury and remove BUFFER from current tabspace.
 If BUFFER is nil, remove current buffer.  If
 `tabspaces-remove-to-default' is t then add the buffer to the
 default tabspace after remove."
  (let ((buffer (get-buffer (or buffer (current-buffer))))
        (buffer-list (frame-parameter nil 'buffer-list)))
    ;; delete window of buffer
    (cond
     ((eq buffer (window-buffer (selected-window)))
      (if (one-window-p t)
          (bury-buffer)
        (delete-window)))
     ((get-buffer-window buffer)
      (select-window (get-buffer-window buffer) t)
      (if (one-window-p t)
          (bury-buffer)
        (delete-window)))
     (t
      (message (format "Buffer `%s' removed from `%s' tabspace."
                       buffer (tabspaces--current-tab-name)))))
    (bury-buffer buffer)
    ;; Delete buffer from tabspace buffer list
    (delete buffer buffer-list)
    ;; If specified, add the buffer to the default tabspace.
    (when tabspaces-remove-to-default
      (tabspaces--add-to-default-tabspace buffer))))

(defun tabspaces-remove-current-buffer ()
  "Bury and remove current buffer from current tabspace."
  (interactive)
  (tabspaces-remove-buffer))

(defun tabspaces-remove-selected-buffer (buffer)
  "Remove selected BUFFER from the frame's buffer list.
If `tabspaces-remove-to-default' is t then add the buffer to the
default tabspace."
  (interactive
   (list
    (let ((blst (mapcar (lambda (b) (buffer-name b))
                        (tabspaces--buffer-list))))
      ;; select buffer
      (read-buffer (format "Remove buffer from `%s' tabspace: "
                           (tabspaces--current-tab-name))
                   nil t
                   (lambda (b) (member (car b) blst))))))
  (tabspaces-remove-buffer buffer))

(defun tabspaces-switch-to-buffer (buffer &optional norecord force-same-window)
  "Display the local buffer BUFFER in the selected window.
This is the frame/tab-local equivalent to `switch-to-buffer'.
The arguments NORECORD and FORCE-SAME-WINDOW are passed to `switch-to-buffer'."
  (interactive
   (list
    (let ((blst (cl-remove (buffer-name) (mapcar #'buffer-name (tabspaces--buffer-list)))))
      (read-buffer
       "Switch to local buffer: " blst nil
       (lambda (b) (member (if (stringp b) b (car b)) blst))))))
  (switch-to-buffer buffer norecord force-same-window))

;; See https://emacs.stackexchange.com/a/53016/11934
(defun tabspaces--report-dupes (xs)
  (let ((ys  ()))
    (while xs
      (unless (member (car xs) ys) ; Don't check it if already known to be a dup.
        (when (member (car xs) (cdr xs)) (push (car xs) ys)))
      (setq xs  (cdr xs)))
    ys))

(defun tabspaces-switch-buffer-and-tab (buffer &optional norecord force-same-window)
  "Switch to the tab of chosen buffer, or create buffer.
If buffer does not exist in buffer-list user can either create a
new tab with the new buffer or open a new buffer in the current
tab."
  (interactive
   (list
    (let ((blst (cl-remove (buffer-name) (mapcar #'buffer-name (buffer-list)))))
      (read-buffer
       "Switch to tab for buffer: " blst nil
       (lambda (b) (member (if (stringp b) b (car b)) blst))))))

  ;; Action on buffer
  (let* ((tabcand nil)
         (buflst nil)
         ;; Provide flat list of all buffers in all tabs (and print dupe buffers).
         ;; This is the list of all buffers to search through.
         (bufflst (flatten-tree (dolist (tab (tabspaces--list-tabspaces) buflst)
                                  (push (mapcar #'buffer-name (tabspaces--buffer-list nil (tab-bar--tab-index-by-name tab))) buflst))))
         (dupe (member buffer (tabspaces--report-dupes bufflst))))
    ;; Run through conditions:
    (cond
     ;; 1. Buffer exists and is not open in more than one tabspace.
     ((and (get-buffer buffer)
           (not dupe))
      (dolist (tab (tabspaces--list-tabspaces))
        (when (member buffer (mapcar #'buffer-name (tabspaces--buffer-list nil (tab-bar--tab-index-by-name tab))))
          (progn (tab-bar-switch-to-tab tab)
                 (tabspaces-switch-to-buffer buffer)))))
     ;; 2. Buffer exists and is open in more than one tabspace.
     ((and (get-buffer buffer)
           dupe)
      (dolist (tab (tabspaces--list-tabspaces) tabcand)
        (when (member buffer (mapcar #'buffer-name (tabspaces--buffer-list nil (tab-bar--tab-index-by-name tab))))
          (push tabcand tab)))
      (progn
        (tab-bar-switch-to-tab (completing-read "Select tab: " tabcand))
        (tabspaces-switch-to-buffer buffer)))
     ;; 3. Buffer does not exist.
     ((yes-or-no-p "Buffer not found -- create a new workspace with buffer?")
      (switch-to-buffer-other-tab buffer))
     ;; 4. Default -- create buffer in current tabspace.
     (t
      (switch-to-buffer buffer norecord force-same-window)))))

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
(defun tabspaces-switch-or-create-workspace (&optional workspace)
  "Switch to tab if it exists, otherwise create a new tabbed workspace."
  (interactive
   (let ((tabs (tabspaces--list-tabspaces)))
     (cond ((eq tabs nil)
            (tab-new)
            (tab-rename (completing-read "Workspace name: " tabs)))
           (t
            (list
             (completing-read "Select or create tab: " tabs nil nil))))))
  (cond ((member workspace (tabspaces--list-tabspaces))
         (tab-bar-switch-to-tab workspace))
        (t
         (tab-new)
         (tab-rename workspace))))

;;;;; Close Workspace
(defalias 'tabspaces-close-workspace #'tab-bar-close-tab)

;;;;; Close Workspace & Kill Buffers
(defun tabspaces-kill-buffers-close-workspace ()
  "Kill all buffers in the workspace and then close the workspace itself."
  (interactive)
  (let ((buf (tabspaces--buffer-list)))
    (unwind-protect
        (cl-loop for b in buf
                 do (kill-buffer b))
      (tab-bar-close-tab))))

;;;;; Open or Create Project in Workspace

(defvar tabspaces-project-tab-map '()
  "Alist mapping full project paths to their respective tab names.")

(defun tabspaces-rename-existing-tab (old-name new-name)
  "Rename an existing tab from OLD-NAME to NEW-NAME."
  (let ((tabs (tab-bar-tabs)))
    (dolist (tab tabs)
      (when (equal (alist-get 'name tab) old-name)
        (tab-bar-rename-tab-by-name old-name new-name)))))

(defun tabspaces-generate-descriptive-tab-name (project-path existing-tab-names)
  "Generate a unique tab name from the PROJECT-PATH checking against EXISTING-TAB-NAMES."
  (let* ((parts (reverse (split-string (directory-file-name project-path) "/")))
         (base-name (car parts))
         (parent-dir (nth 1 parts))
         (grandparent-dir (nth 2 parts))
         (simple-tab-name base-name)
         (complex-tab-name (if parent-dir
                               (format "%s (%s/%s)" base-name (or grandparent-dir "") parent-dir)
                             base-name)))
    (if (member simple-tab-name existing-tab-names)
        (let ((existing-path (rassoc simple-tab-name tabspaces-project-tab-map)))
          (when existing-path
            ;; Generate a new complex name for the existing conflict
            (let ((new-name-for-existing (tabspaces-generate-complex-name (car existing-path))))
              ;; Rename the existing tab
              (tabspaces-rename-existing-tab simple-tab-name new-name-for-existing)
              ;; Update the map with the new name for the existing path
              (setcdr existing-path new-name-for-existing)))
          ;; Use the complex name for the new tab to avoid future conflicts
          complex-tab-name)
      ;; No conflict, add to map and use the simple name
      (progn
        (add-to-list 'tabspaces-project-tab-map (cons project-path simple-tab-name))
        simple-tab-name))))

(defun tabspaces-generate-complex-name (project-path)
  "Generate a complex name based on the grandparent and parent directory names."
  (let* ((parts (reverse (split-string (directory-file-name project-path) "/")))
         (base-name (car parts))
         (parent-dir (nth 1 parts))
         (grandparent-dir (nth 2 parts)))
    (format "%s (%s/%s)" base-name (or grandparent-dir "") parent-dir)))

;;;###autoload
(defun tabspaces-open-or-create-project-and-workspace (&optional project prefix)
  "Open or create a project and its workspace with a descriptive tab name."
  (interactive
   (list (project-prompt-project-dir) current-prefix-arg))
  (let* ((project-switch-commands tabspaces-project-switch-commands)
         (project (if tabspaces-fully-resolve-paths
                      (expand-file-name project)  ; Resolve relative paths
                    project))
         (existing-tab-names (tabspaces--list-tabspaces))
         (tab-name (tabspaces-generate-descriptive-tab-name project existing-tab-names))
         (session (concat project "." (file-name-nondirectory (directory-file-name project)) "-tabspaces-session.el"))
         (project-directory (file-name-directory project))
         (directory-with-potential-project-content (project--find-in-directory project-directory)))
    ;; Now manage the workspace based on the project state:
    (cond
     ((and (member (list project) project--list)
           (member tab-name existing-tab-names))
      ;; If project and tab exist, switch to it
      (tab-bar-switch-to-tab tab-name))
     ((and (or (member (list project) project--list)
               directory-with-potential-project-content)
           (not (member tab-name existing-tab-names)))
      ;; If project exists, but no corresponding tab, open a new tab
      (tab-bar-new-tab)
      (tab-bar-rename-tab tab-name)
      (let ((default-directory project-directory))
        (if (file-exists-p session)
            (tabspaces-restore-session session)
          (project-switch-project project))
        (unless (member (list project) project--list)
          (project-remember-project directory-with-potential-project-content))))
     (t
      ;; Open new tab and create project
      (tab-bar-new-tab)
      (setq default-directory project-directory)
      (ignore-errors (mkdir project-directory t))
      (if (featurep 'magit)
          (magit-init project-directory)
        (call-interactively #'vc-create-repo))
      (delete-other-windows)
      (when (and tabspaces-initialize-project-with-todo
                 (not (file-exists-p tabspaces-todo-file-name)))
        (with-temp-buffer (write-file tabspaces-todo-file-name)))
      (if (featurep 'magit)
          (magit-status-setup-buffer)
        (project-vc-dir))
      (dired-jump-other-window)
      (tab-bar-rename-tab tab-name)
      ;; Make sure project.el remembers new project
      (let ((pr (project--find-in-directory default-directory)))
        (project-remember-project pr))))))

;;;; Tabspace Sessions

(defconst tabspaces-session-header
  ";; -------------------------------------------------------------------------
;; Tabspaces Session File for Emacs
;; -------------------------------------------------------------------------
" "Header to place in Tabspaces session file.")

(defcustom tabspaces-session t
  "Whether to save tabspaces across sessions."
  :group 'tabspaces
  :type 'boolean)

(defcustom tabspaces-session-auto-restore nil
  "Whether to restore tabspaces on session startup."
  :group 'tabspaces
  :type 'boolean)

(defcustom tabspaces-session-file (concat user-emacs-directory "tabsession.el")
  "File for saving tabspaces session."
  :group 'tabspaces
  :type 'string)

(defvar tabspaces--session-list nil
  "Store `tabspaces' session tabs and buffers.")

;; Helper functions
(defun tabspaces--buffile (b)
  "Get filename for buffers."
  (cl-remove-if nil (buffer-file-name b)))

(defun tabspaces--store-buffers (bufs)
  "Make list of filenames."
  (flatten-tree (mapcar #'tabspaces--buffile bufs)))

;; Save global session
;;;###autoload
(defun tabspaces-save-session ()
  "Save tabspace name and buffers."
  (interactive)
  ;; Start from an empty list.
  (setq tabspaces--session-list nil)
  (let ((curr (tab-bar--current-tab-index)))
    ;; loop over tabs
    (cl-loop for tab in (tabspaces--list-tabspaces)
             do (progn
                  (tab-bar-select-tab-by-name tab)
                  (setq tabspaces--session-list
                        (append tabspaces--session-list
                                (list (cons (tabspaces--store-buffers (tabspaces--buffer-list)) tab))))))
    ;; As tab-bar-select-tab starts counting from 1, we need to add 1 to the index.
    (tab-bar-select-tab (+ curr 1)))
  ;; Write to file
  (with-temp-file tabspaces-session-file
    (point-min)
    (insert ";; -*- mode: emacs-lisp; lexical-binding:t; coding: utf-8-emacs; -*-\n"
            tabspaces-session-header
            ";; Created " (current-time-string) "\n\n"
            ";; Tabs and buffers:\n")
    (insert "(setq tabspaces--session-list '" (format "%S" tabspaces--session-list) ")")))

;; Save current project session
(defun tabspaces-save-current-project-session ()
  "Save tabspace name and buffers for current tab & project."
  (interactive)
  (let ((tabspaces--session-list nil) ;; Start from an empty list.
        (ctab (tabspaces--current-tab-name))
        (current-session (with-current-buffer (buffer-name)
                           (concat (vc-root-dir) "." (tabspaces--current-tab-name) "-tabspaces-session.el"))))
    ;; Get buffers
    (add-to-list 'tabspaces--session-list (cons (tabspaces--store-buffers (tabspaces--buffer-list)) ctab))
    ;; Write to file
    (with-temp-file current-session
      (point-min)
      (insert ";; -*- mode: emacs-lisp; lexical-binding:t; coding: utf-8-emacs; -*-\n"
              tabspaces-session-header
              ";; Created " (current-time-string) "\n\n"
              ";; Tab and buffers:\n")
      (insert "(setq tabspaces--session-list '" (format "%S" tabspaces--session-list) ")"))))

;; Restore session
;;;###autoload
(defun tabspaces-restore-session (&optional session)
  "Restore tabspaces session."
  (interactive)
  (load-file (or session
                 tabspaces-session-file))
  ;; Start looping through the session list, but ensure to start from a
  ;; temporary buffer "*tabspaces--placeholder*" in order not to pollute the
  ;; buffer list with the final buffer from the previous tab.
  (cl-loop for elm in tabspaces--session-list do
           (switch-to-buffer "*tabspaces--placeholder*")
           (tabspaces-switch-or-create-workspace (cdr elm))
           (mapc #'find-file (car elm)))
  ;; Once the session list is restored, remove the temporary buffer from the
  ;; buffer list.
  (cl-loop for elm in tabspaces--session-list do
           (tabspaces-switch-or-create-workspace (cdr elm))
           (tabspaces-remove-selected-buffer "*tabspaces--placeholder*"))
  ;; Finally, kill the temporary buffer to clean up.
  (kill-buffer "*tabspaces--placeholder*"))

;; Restore session used for startup
(defun tabspaces--restore-session-on-startup ()
  "Restore tabspaces session on startup.
Unlike the interactive restore, this function does more clean up to remove
unnecessary tab."
  (load-file tabspaces-session-file)
  ;; Start looping through the session list, but ensure to start from a
  ;; temporary buffer "*tabspaces--placeholder*" in order not to pollute the
  ;; buffer list with the final buffer from the previous tab.
  (cl-loop for elm in tabspaces--session-list do
           (switch-to-buffer "*tabspaces--placeholder*")
           (tabspaces-switch-or-create-workspace (cdr elm))
           (mapc #'find-file (car elm)))
  ;; Once the session list is restored, remove the temporary buffer from the
  ;; buffer list.
  (cl-loop for elm in tabspaces--session-list do
           (tabspaces-switch-or-create-workspace (cdr elm))
           (tabspaces-remove-selected-buffer "*tabspaces--placeholder*"))
  ;; If the tab restore started from an empty tab (e.g. at startup), remove the
  ;; tab by name of "*tabspaces--placeholder*".
  ;; NOTE When restore is interactively called, it is possible that an unnamed
  ;; tab to be incorrectly closed as we call `switch-to-buffer', which would
  ;; make the tab name to be "*tabspaces--placeholder*". At the startup, this
  ;; shouldn't be an issue, but conduct a simple check before closing the tab.
  (if (eq (tab-bar--tab-index-by-name "*tabspaces--placeholder*") 0)
      ;; tab-bar-close-tab counts from 1.
      (tab-bar-close-tab 1))
  ;; Finally, kill the temporary buffer to clean up.
  (kill-buffer "*tabspaces--placeholder*"))


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
    (define-key map (kbd "t") 'tabspaces-switch-buffer-and-tab)
    map)
  "Keymap for tabspace/workspace commands after `tabspaces-keymap-prefix'.")
(fset 'tabspaces-command-map tabspaces-command-map)

(defvar tabspaces-mode-map
  (let ((map (make-sparse-keymap)))
    (when tabspaces-keymap-prefix
      (define-key map (kbd tabspaces-keymap-prefix) 'tabspaces-command-map))
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
             (define-key (current-global-map) [remap switch-to-buffer] #'tabspaces-switch-to-buffer)))
         (when tabspaces-session
           (add-hook 'kill-emacs-hook #'tabspaces-save-session))
         (when tabspaces-session-auto-restore
           (add-hook 'emacs-startup-hook #'tabspaces--restore-session-on-startup)))
        (t
         ;; Remove all modifications
         (dolist (frame (frame-list))
           (tabspaces--reset-buffer-predicate frame))
         (when tabspaces-use-filtered-buffers-as-default
           (define-key (current-global-map) [remap switch-to-buffer] nil))
         (setq tab-bar-tab-post-open-functions (remove #'tabspaces--tab-post-open-function tab-bar-tab-post-open-functions))
         (remove-hook 'after-make-frame-functions #'tabspaces--set-buffer-predicate)
         (remove-hook 'kill-emacs-hook #'tabspaces-save-session)
         (remove-hook 'emacs-startup-hook #'tabspaces-restore-session))))

;;; Provide
(provide 'tabspaces)
;;; tabspaces.el ends here
