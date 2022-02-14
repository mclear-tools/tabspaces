;; emacs-workspace.el  -*- lexical-binding: t; -*-
;; Workspaces for emacs

;;; Emacs Workspaces

;; Copyright (C) 2022 Colin McLear

;; Author: Colin McLear <mclear@fastmail.com>
;; Version: 1.0
;; Package-Requires ((cl-lib "0.5") (tab-bar))
;; Keywords: workspaces, projects, tabs
;; URL: https://github.com/mclear-tools/emacs-workspaces

;;; Commentary:

;; This package provides several functions to facilitate a single frame-based
;; workspace with one workspace per tab, intergration with project.el (for
;; project-based workspaces) and buffer isolation per tab. The package assumes
;; project.el and tab-bar.el are both present (they are built-in to emacs 28+).

;; Much of the package code is from or is inspired by:

;; - https://github.com/kaz-yos/emacs
;; - https://github.com/wamei/elscreen-separate-buffer-list/issues/8
;; - https://www.rousette.org.uk/archives/using-the-tab-bar-in-emacs/
;; - https://github.com/minad/consult#multiple-sources

;; Full documentation is available as an Info manual.

;;; Code:

;;;; Requirements

(require 'tab-bar)
(require 'project)
(require 'vc)

;;;; Buffer Workspaces
(defcustom emacs-workspaces-workspace-create-permitted-buffer-names
  '("*scratch*")
  "List of buffer names kept by `emacs-workspace-create'."
  :group emacs-workspaces
  :type 'string)

(defun emacs-workspaces/create-workspace (&optional arg)
  "Create a new tab/workspace with cleaned buffer lists.

ARG is directly passed to `tab-bar-new-tab'.
Only buffers in `emacs-workspaces--workspace-create-permitted-buffer-names'
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
                                             emacs-workspaces-workspace-create-permitted-buffer-names))
                                   (frame-parameter nil 'buffer-list)))
  (set-frame-parameter nil
                       'buried-buffer-list
                       (seq-filter (lambda (buffer)
                                     (member (buffer-name buffer)
                                             emacs-workspaces-workspace-create-permitted-buffer-names))
                                   (frame-parameter nil 'buried-buffer-list))))

;; NOTE: to clone tab/workspace with all buffers use tab-bar-duplicate-tab

;;;; Filter Workspace Buffers

;;;;; Group Buffers By Tab
;; tab-bar version of separate buffer list filter
;; See https://github.com/wamei/elscreen-separate-buffer-list/issues/8
;; https://github.com/kaz-yos/emacs/blob/master/init.d/200_tab-related.el#L74-L87

(defun emacs-workspaces--tab-bar-buffer-name-filter (buffer-names)
  "Filter BUFFER-NAMES by the current tab's buffer list
It should be used to filter a list of buffer names created by
other functions, such as `helm-buffer-list'."
  (let ((buffer-names-to-keep
         ;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Buffer-List.html
         (append (mapcar #'buffer-name (alist-get 'wc-bl (tab-bar--tab)))
                 (mapcar #'buffer-name (alist-get 'wc-bbl (tab-bar--tab))))))
    (seq-filter (lambda (elt)
                  (member elt buffer-names-to-keep))
                buffer-names)))


;;;;; Filter Buffers for Switch-to-Buffer

(advice-add #'internal-complete-buffer :filter-return #'emacs-workspaces--tab-bar-buffer-name-filter)

;;;;; Project Workspace Commands

(defun emacs-workspaces--name-tab-by-project-or-default ()
  "Return project name if in a project, or default tab-bar name if not.
The default tab-bar name uses the buffer name."
  (let ((project-name (emacs-workspaces--project-name)))
    (if (string= "-" project-name)
        (tab-bar-tab-name-current)
      (emacs-workspaces--project-name))))

(defun emacs-workspaces--project-name ()
  "Return name of project without path"
  (file-name-nondirectory (directory-file-name (if (vc-root-dir) (vc-root-dir) "-"))))

(defun emacs-workspaces/project-switch-project-open-file (dir)
  "Switch to another project by running an Emacs command.
Open file using project-find-file

When called in a program, it will use the project corresponding
to the selected directory DIR."
  (interactive (list (project-prompt-project-dir)))
  (let ((default-directory dir)
        (project-current-inhibit-prompt t))
    (call-interactively 'project-find-file)))

;;; New Project
(defun emacs-workspaces--create-new-vc-project ()
  "Initializes a new version control repo and adds it to project.el's known projects."
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
          (call-interactively 'vc-create-repo)))
      ;; make sure project.el remembers new project
      (let ((pr (project--find-in-directory default-directory)))
        (project-remember-project pr)))))


;;;; Open Project in New Workspace
(defun emacs-workspaces/open-existing-project-and-workspace ()
  "Open an existing project as its own workspace"
  (interactive)
  (progn
    (emacs-workspaces/create-workspace)
    (call-interactively 'emacs-workspaces/project-switch-project-open-file)
    (tab-bar-rename-tab (emacs-workspaces--name-tab-by-project-or-default))))

;;;;  Create & Open New Project in New Workspace
(defun emacs-workspaces/create-new-project-and-workspace ()
  "Create & open a new version-controlled project as its own workspace and create some useful files"
  (interactive)
  (progn
    (emacs-workspaces/create-workspace)
    (emacs-workspaces--name-tab-by-project-or-default)
    (emacs-workspaces--create-new-vc-project)
    (delete-other-windows)
    (with-temp-buffer (write-file "project-todo.org"))
    (if (featurep 'magit)
        (magit-status)
      (project-vc-dir))
    (dired-jump-other-window)))

;;; Provide
(provide 'emacs-workspaces)

;; emacs-workspaces.el ends here
