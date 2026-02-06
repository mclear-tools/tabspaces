;;; tabspaces.el --- Leverage tab-bar and project for buffer-isolated workspaces  -*- lexical-binding: t -*-

;; Author: Colin McLear <mclear@fastmail.com>
;; Maintainer: Colin McLear
;; Version: 1.7
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
  "Key prefix for the tabspaces-prefix-map keymap.
Set to nil to disable automatic keymap binding."
  :group 'tabspaces
  :type '(choice (const :tag "Disabled" nil)
                 string))

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

(defcustom tabspaces-echo-area-enable nil
  "Display tabs in echo area instead of tab-bar when enabled."
  :group 'tabspaces
  :type 'boolean)

(defcustom tabspaces-echo-area-format-function #'tabspaces--echo-area-format-tabs
  "Function to format tabs for echo area display."
  :group 'tabspaces
  :type 'function)

(defcustom tabspaces-echo-area-idle-delay 1.0
  "Number of seconds to wait before showing tabs when idle."
  :group 'tabspaces
  :type 'number
  :set (lambda (symbol value)
         (set-default symbol value)
         ;; Restart timer with new delay if echo area is enabled
         (when (and (boundp 'tabspaces-echo-area-enable)
                    tabspaces-echo-area-enable
                    (boundp 'tabspaces--idle-timer)
                    tabspaces--idle-timer)
           (tabspaces--setup-idle-timer))))

;;;; Echo Area Display

(defvar tabspaces--tabs-visible nil
  "Non-nil when tabs are currently displayed in the echo area.")

(defvar tabspaces--idle-timer nil
  "Timer object for displaying tabs after idle time.")

(defvar tabspaces--original-tab-bar-show nil
  "Original value of `tab-bar-show' before echo area display is enabled.")

(defvar tabspaces--last-echo-display nil
  "Last tabs display string to avoid duplicate messages.")

(defun tabspaces--filter-messages-buffer ()
  "Remove tab display messages from *Messages* buffer."
  (when (and (get-buffer "*Messages*")
             tabspaces--last-echo-display)
    (with-current-buffer "*Messages*"
      (save-excursion
        (let ((inhibit-read-only t))
          (goto-char (point-max))
          (forward-line -1)
          (when (looking-at-p (regexp-quote tabspaces--last-echo-display))
            (delete-region (line-beginning-position)
                           (min (1+ (line-end-position)) (point-max)))))))))

(defun tabspaces--echo-area-format-tabs ()
  "Format all tabs for echo area display using the configured tab-bar formatter.
Returns a formatted string containing all tabs, or nil if only one tab exists."
  (when (> (length (tab-bar-tabs)) 1)
    (let* ((tabs (tab-bar-tabs))
           (current-tab (tab-bar--current-tab-find tabs))
           (tab-strings '()))
      (dotimes (i (length tabs))
        (let* ((tab (nth i tabs))
               (current-p (eq tab current-tab))
               ;; Convert 0-based tab index to 1-based for display
               (display-index (if (< i 9) (1+ i) 0))
               (formatted-name (funcall tab-bar-tab-name-format-function tab display-index)))
          (push formatted-name tab-strings)))
      (mapconcat #'identity (reverse tab-strings) ""))))

(defun tabspaces--echo-area-display (&rest _)
  "Display formatted tabs in the echo area without logging to *Messages*.
Sets the visibility flag to indicate tabs are currently shown.
Optional ARGS are ignored, allowing use as advice."
  (when tabspaces-echo-area-enable
    (let ((tabs-display (funcall tabspaces-echo-area-format-function)))
      (when tabs-display
        (setq tabspaces--tabs-visible t)
        ;; Mark this as a tab display message for filtering
        (setq tabspaces--last-echo-display tabs-display)
        ;; Display the message normally
        (message "%s" tabs-display)
        ;; Then remove it from *Messages* buffer
        (run-with-timer 0.01 nil #'tabspaces--filter-messages-buffer)))))

(defun tabspaces--idle-display ()
  "Display tabs in echo area after idle period.
Only displays if echo area feature is enabled, multiple tabs exist,
and the minibuffer is not active."
  (when (and tabspaces-echo-area-enable
             (> (length (tab-bar-tabs)) 1)
             (not (minibufferp (current-buffer))))
    (tabspaces--echo-area-display)))

(defun tabspaces--setup-idle-timer ()
  "Initialize idle timer to display tabs after inactivity.
Cancels any existing timer before creating a new one."
  (when tabspaces--idle-timer
    (cancel-timer tabspaces--idle-timer))
  (setq tabspaces--idle-timer
        (run-with-idle-timer tabspaces-echo-area-idle-delay t #'tabspaces--idle-display)))

(defun tabspaces--cancel-idle-timer ()
  "Cancel and clear the idle display timer."
  (when tabspaces--idle-timer
    (cancel-timer tabspaces--idle-timer)
    (setq tabspaces--idle-timer nil)))

(defun tabspaces-restart-idle-timer ()
  "Restart the echo area idle timer with current delay settings.
Useful for troubleshooting or after changing the delay value."
  (interactive)
  (when tabspaces-echo-area-enable
    (tabspaces--setup-idle-timer)
    (message "Idle timer restarted with delay: %.1f seconds" tabspaces-echo-area-idle-delay)))

(defun tabspaces-echo-area-timer-status ()
  "Display current status of the echo area idle timer.
Shows if timer is active, the delay setting, and other relevant info."
  (interactive)
  (let ((status-parts '()))
    (push (format "Echo area enabled: %s" (if tabspaces-echo-area-enable "yes" "no")) status-parts)
    (push (format "Idle delay: %.1f seconds" tabspaces-echo-area-idle-delay) status-parts)
    (push (format "Timer active: %s" (if tabspaces--idle-timer "yes" "no")) status-parts)
    (when tabspaces--idle-timer
      (push (format "Timer object: %s" tabspaces--idle-timer) status-parts))
    (push (format "Number of tabs: %d" (length (tab-bar-tabs))) status-parts)
    (message (mapconcat #'identity status-parts ", "))))

(defun tabspaces--echo-area-setup ()
  "Initialize echo area tab display when enabled.
Hides the visual tab-bar and sets up idle timer for tab display."
  (when tabspaces-echo-area-enable
    ;; Ensure tab-bar-mode is enabled for tab functionality
    (unless tab-bar-mode (tab-bar-mode 1))
    ;; Store original setting and hide visual tab-bar
    (setq tabspaces--original-tab-bar-show tab-bar-show)
    (setq tab-bar-show nil)
    ;; Force tab-bar update after brief delay to override other configurations
    (run-with-timer 0.1 nil
                    (lambda ()
                      (setq tab-bar-show nil)
                      (when (fboundp 'tab-bar--update-tab-bar-lines)
                        (tab-bar--update-tab-bar-lines))))
    ;; Configure automatic display via idle timer only
    (tabspaces--setup-idle-timer)))

(defun tabspaces--echo-area-cleanup ()
  "Clean up echo area tab display configuration.
Restores original tab-bar visibility and removes timer."
  ;; Restore original tab-bar visibility setting
  (when (boundp 'tabspaces--original-tab-bar-show)
    (setq tab-bar-show tabspaces--original-tab-bar-show))
  ;; Clean up timer
  (tabspaces--cancel-idle-timer)
  ;; Reset state variables
  (setq tabspaces--tabs-visible nil
        tabspaces--last-echo-display nil))

(defun tabspaces-show-workspaces ()
  "Display current workspaces in the echo area on command."
  (interactive)
  (let ((tabs-display (funcall tabspaces-echo-area-format-function)))
    (if tabs-display
        (message "%s" tabs-display)
      (message "Only one workspace active"))))

(defun tabspaces-toggle-echo-area-display ()
  "Toggle echo area tab display feature on or off.
When enabled, tabs will appear in the echo area after idle time and
during tab operations. When disabled, tabs are only shown in the tab-bar."
  (interactive)
  (if (or tabspaces--tabs-visible tabspaces-echo-area-enable)
      ;; Turn off: disable feature and clear current display
      (progn
        (setq tabspaces-echo-area-enable nil)
        (setq tabspaces--tabs-visible nil)
        (message nil)
        (tabspaces--echo-area-cleanup)
        (run-with-timer 0.5 nil (lambda () (message "Echo area tabs disabled"))))
    ;; Turn on: enable feature and show tabs
    (progn
      (setq tabspaces-echo-area-enable t)
      (tabspaces--echo-area-setup)
      (tabspaces--echo-area-display)
      (run-with-timer 0.5 nil (lambda () (message "Echo area tabs enabled"))))))

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
If BUFFER is nil, remove current buffer. If
`tabspaces-remove-to-default' is t then add the buffer to the
default tabspace after remove, unless we're already in the default tabspace, in which case remove from the default as well."
  (let* ((buffer (get-buffer (or buffer (current-buffer))))
         (buffer-list (frame-parameter nil 'buffer-list))
         (in-default-tab (string= (tabspaces--current-tab-name)
                                  tabspaces-default-tab)))
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
    ;; If specified AND we're not in default tab, add buffer to default tabspace
    (when (and tabspaces-remove-to-default (not in-default-tab))
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
          (push tab tabcand)))
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
                 for n = (buffer-name b)
                 unless (or (member n tabspaces-exclude-buffers)
                            (member n tabspaces-include-buffers))
                 do (kill-buffer b))
      (tab-bar-close-tab))))

;;;;; Open or Create Project in Workspace

(defvar tabspaces-project-tab-map '()
  "Alist mapping full project paths to their respective tab names.")

(defun tabspaces--get-project-for-tab (tab-name)
  "Get project root path for TAB-NAME, or nil if not a project tab.
Handles numbered tabs like \"ProjectName<2>\" by checking both exact
match and base name without suffix."
  (or
   ;; First try exact match
   (car (rassoc tab-name tabspaces-project-tab-map))
   ;; Then try stripping numbered suffix like "<2>"
   (when (string-match "\\`\\(.+\\)<[0-9]+>\\'" tab-name)
     (let ((base-name (match-string 1 tab-name)))
       (car (rassoc base-name tabspaces-project-tab-map))))))

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

;; Function to generate a unique numbered tab name
(defun generate-unique-numbered-tab-name (base-name existing-names)
  (let ((counter 2)
        (new-name base-name))
    (while (member new-name existing-names)
      (setq new-name (format "%s<%d>" base-name counter)
            counter (1+ counter)))
    new-name))

;; Replace read-directory-name so that we can create new projects when necessary
(defun tabspaces--read-directory-name (prompt &optional dir default mustmatch)
  "Read a directory name, and create it if it does not exist."
  (let ((dir-name (read-directory-name prompt dir default mustmatch)))
    (unless (file-directory-p dir-name)
      (when (y-or-n-p (format "Directory %s does not exist. Create it?" dir-name))
        (make-directory dir-name t)))
    dir-name))


;; Replace project-prompt-project-dir for project creation
(defun tabspaces-prompt-project-dir ()
  "Prompt the user for a directory that is one of the known project roots.
The project is chosen among projects known from the project list,
see `project-list-file'.
It's also possible to enter an arbitrary directory not in the list."
  (project--ensure-read-project-list)
  (let* ((dir-choice "... (choose a dir)")
         (choices
          ;; XXX: Just using this for the category (for the substring
          ;; completion style).
          (project--file-completion-table
           (append project--list `(,dir-choice))))
         (pr-dir ""))
    (while (equal pr-dir "")
      ;; If the user simply pressed RET, do this again until they don't.
      (setq pr-dir (completing-read "Select project: " choices nil t)))
    (if (equal pr-dir dir-choice)
        (tabspaces--read-directory-name "Select directory: " nil nil nil)
      pr-dir)))

;;;###autoload
(defun tabspaces-open-or-create-project-and-workspace (&optional project prefix)
  "Open or create a project and its workspace with a descriptive tab name.
With universal argument PREFIX, always create a new tab for the project."
  (interactive
   (list (tabspaces-prompt-project-dir) current-prefix-arg))
  (let* ((project-switch-commands tabspaces-project-switch-commands)
         (project (if tabspaces-fully-resolve-paths
                      (expand-file-name project)  ; Resolve relative paths
                    project))
         (existing-tab-names (tabspaces--list-tabspaces))
         (original-tab-name (or (cdr (assoc project tabspaces-project-tab-map))
                                (tabspaces-generate-descriptive-tab-name project existing-tab-names)))
         (tab-name original-tab-name)
         (session (concat project "." (file-name-nondirectory (directory-file-name project)) "-tabspaces-session.el"))
         (project-directory project)  ; Use the full path as the project directory
         (project-exists (member (list project) project--list))
         (create-new-tab (or prefix (not (member tab-name existing-tab-names)))))

    (message "Tabspaces: Project directory: %s" project-directory)

    ;; Remember project if it exists on disk but is not yet registered
    (let ((pr (and (not project-exists)
                   (project--find-in-directory project-directory))))
      (when pr
        (project-remember-project pr)
        (setq project-exists t)))

    ;; Now manage the workspace based on the project state:
    (cond
     ;; If there is no tab nor project, create both
     ((not project-exists)
      (message "Tabspaces - Creating new project and tab")
      (tab-bar-new-tab)
      (tab-bar-rename-tab tab-name)
      (let ((default-directory project-directory))
        (message "Tabspaces: default directory set to %s" default-directory)
        (if (fboundp 'magit-init)
            (magit-init project-directory)
          (call-interactively #'vc-create-repo))
        (delete-other-windows)
        (when (and tabspaces-initialize-project-with-todo
                   (not (file-exists-p (expand-file-name tabspaces-todo-file-name project-directory))))
          (with-temp-buffer
            (write-file (expand-file-name tabspaces-todo-file-name project-directory))))
        (if (fboundp 'magit-status-setup-buffer)
            (magit-status-setup-buffer project-directory)
          (project-vc-dir))
        (dired-jump-other-window))
      ;; Remember new project
      (let ((pr (project--find-in-directory default-directory)))
        (project-remember-project pr)))

     ;; If project and tab exist, but we want a new tab
     ((and project-exists
           (member tab-name existing-tab-names)
           create-new-tab)
      (message "Tabspaces - Creating new tab for existing project and tab")
      (let ((new-tab-name (generate-unique-numbered-tab-name tab-name existing-tab-names)))
        (tab-bar-new-tab)
        (tab-bar-rename-tab new-tab-name)
        (setq tab-name new-tab-name))
      (project-switch-project project))

     ;; If project and tab exist, switch to it
     ((and project-exists
           (member tab-name existing-tab-names))
      (message "Tabspaces - Switching to existing tab")
      (tab-bar-switch-to-tab tab-name))

     ;; If project exists, but no corresponding tab, open a new tab
     (project-exists
      (message "Tabspaces - Creating new tab for existing project")
      (tab-bar-new-tab)
      (tab-bar-rename-tab tab-name)
      (if (and tabspaces-session-auto-restore
               (file-exists-p session))
          (tabspaces-restore-session session)
        (project-switch-project project)))

     (t
      (message "Tabspaces - No project found or created.")
      nil))

    (message "Tabspaces: Conditional execution completed")

    ;; Update tabspaces-project-tab-map (only for the main tab, not numbered duplicates)
    (unless (string-match-p "<[0-9]+>$" tab-name)
      (setq tabspaces-project-tab-map
            (cons (cons project-directory tab-name)
                  (assq-delete-all project-directory tabspaces-project-tab-map))))))

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

(defcustom tabspaces-session-project-session-store 'project
  "Determines where project session files are stored.
Can be one of:
- 'project (default) - Store in the project root directory
- a string path - Store all project sessions in this directory
- a function - Called with project root path to determine session file location"
  :group 'tabspaces
  :type '(choice
          (const :tag "In project directory" project)
          (directory :tag "In specific directory")
          (function :tag "Custom function")))

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
  "Save all tabspaces with their buffers and window configurations."
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
                                (list (list
                                       (tabspaces--store-buffers (tabspaces--buffer-list))
                                       tab
                                       (window-state-get nil t)))))))
    ;; As tab-bar-select-tab starts counting from 1, we need to add 1 to the index.
    (tab-bar-select-tab (+ curr 1)))
  ;; Write to file
  (with-temp-file tabspaces-session-file
    (point-min)
    (insert ";; -*- mode: emacs-lisp; lexical-binding:t; coding: utf-8-emacs; -*-\n"
            tabspaces-session-header
            ";; Created " (current-time-string) "\n\n"
            ";; Project to tab name mapping:\n")
    (insert "(setq tabspaces-project-tab-map '"
            (format "%S" tabspaces-project-tab-map) ")\n\n"
            ";; Tabs and buffers:\n")
    (insert "(setq tabspaces--session-list '"
            (format "%S" tabspaces--session-list) ")"))
  (message "Global tabspaces session file \'%s\' saved" tabspaces-session-file))

;; Save current project session
(defun tabspaces-save-current-project-session (&optional session-file)
  "Save tabspace name, buffers, and window config for current tab & project.
Optional SESSION-FILE parameter specifies where to save the session file.
If not provided, uses the location specified by
`tabspaces-session-project-session-store'."
  (interactive)
  (unless (vc-root-dir)
    (error "Not in a version controlled project"))
  (let ((tabspaces--session-list nil) ;; Start from an empty list.
        (ctab (tabspaces--current-tab-name))
        (current-session (or session-file
                             (tabspaces--get-project-session-file))))
    ;; Ensure directory exists
    (make-directory (file-name-directory current-session) t)
    ;; Get buffers and window state
    (add-to-list 'tabspaces--session-list
                 (list (tabspaces--store-buffers (tabspaces--buffer-list))
                       ctab
                       (window-state-get nil t))) ;; t means include buffer names
    ;; Write to file
    (with-temp-file current-session
      (point-min)
      (insert ";; -*- mode: emacs-lisp; lexical-binding:t; coding: utf-8-emacs; -*-\n"
              tabspaces-session-header
              ";; Created " (current-time-string) "\n\n"
              ";; Project to tab name mapping:\n")
      (insert "(setq tabspaces-project-tab-map '"
              (format "%S" tabspaces-project-tab-map) ")\n\n"
              ";; Tab and buffers:\n")
      (insert "(setq tabspaces--session-list '"
              (format "%S" tabspaces--session-list) ")"))
    (message "Current project tabspaces session file \'%s\' saved" current-session)))

;; Save all project sessions
(defun tabspaces-save-all-project-sessions ()
  "Save each project tab to its own session file.
Iterates through all tabs, identifies which are associated with projects
via `tabspaces-project-tab-map', and saves each project tab's session
to its respective project directory based on
`tabspaces-session-project-session-store'."
  (let ((curr (tab-bar--current-tab-index))
        (saved-projects '()))
    (condition-case err
        (progn
          (dolist (tab-name (tabspaces--list-tabspaces))
            (let ((project-root (tabspaces--get-project-for-tab tab-name)))
              (when project-root
                ;; Switch to the project tab
                (tab-bar-select-tab-by-name tab-name)
                ;; Get session file path for this project
                (let* ((session-file (tabspaces--get-project-session-file-for-restore project-root))
                       (tabspaces--session-list nil)
                       (ctab tab-name))
                  ;; Ensure directory exists
                  (make-directory (file-name-directory session-file) t)
                  ;; Store buffers and window state
                  (add-to-list 'tabspaces--session-list
                               (list (tabspaces--store-buffers (tabspaces--buffer-list))
                                     ctab
                                     (window-state-get nil t)))
                  ;; Write to file
                  (with-temp-file session-file
                    (point-min)
                    (insert ";; -*- mode: emacs-lisp; lexical-binding:t; coding: utf-8-emacs; -*-\n"
                            tabspaces-session-header
                            ";; Created " (current-time-string) "\n\n"
                            ";; Project to tab name mapping:\n")
                    (insert "(setq tabspaces-project-tab-map '"
                            (format "%S" tabspaces-project-tab-map) ")\n\n"
                            ";; Tab and buffers:\n")
                    (insert "(setq tabspaces--session-list '"
                            (format "%S" tabspaces--session-list) ")"))
                  (push project-root saved-projects)))))
          ;; Restore original tab
          (tab-bar-select-tab (+ curr 1))
          (when saved-projects
            (message "Saved %d project session(s)" (length saved-projects))))
      (error
       (message "Error saving project sessions: %s" (error-message-string err))
       ;; Try to restore original tab even on error
       (ignore-errors (tab-bar-select-tab (+ curr 1)))))))

;; Save non-project tabs to global session
(defun tabspaces-save-non-project-tabs ()
  "Save tabs not associated with projects to the global session file.
This preserves non-project workspaces when using per-project session mode."
  (let ((curr (tab-bar--current-tab-index))
        (non-project-session-list nil))
    (condition-case err
        (progn
          (dolist (tab-name (tabspaces--list-tabspaces))
            (unless (tabspaces--get-project-for-tab tab-name)
              ;; This is a non-project tab
              (tab-bar-select-tab-by-name tab-name)
              (setq non-project-session-list
                    (append non-project-session-list
                            (list (list
                                   (tabspaces--store-buffers (tabspaces--buffer-list))
                                   tab-name
                                   (window-state-get nil t)))))))
          ;; Restore original tab
          (tab-bar-select-tab (+ curr 1))
          ;; Only write if there are non-project tabs
          (when non-project-session-list
            (with-temp-file tabspaces-session-file
              (point-min)
              (insert ";; -*- mode: emacs-lisp; lexical-binding:t; coding: utf-8-emacs; -*-\n"
                      tabspaces-session-header
                      ";; Created " (current-time-string) "\n\n"
                      ";; Non-project tabs only (project tabs saved separately)\n\n"
                      ";; Project to tab name mapping:\n")
              (insert "(setq tabspaces-project-tab-map '"
                      (format "%S" tabspaces-project-tab-map) ")\n\n"
                      ";; Tabs and buffers:\n")
              (insert "(setq tabspaces--session-list '"
                      (format "%S" non-project-session-list) ")"))
            (message "Saved %d non-project tab(s) to global session" (length non-project-session-list))))
      (error
       (message "Error saving non-project tabs: %s" (error-message-string err))
       (ignore-errors (tab-bar-select-tab (+ curr 1)))))))

;; Smart session saver - dispatches based on configuration
(defun tabspaces--save-session-smart ()
  "Save sessions intelligently based on configuration.
If `tabspaces-session-project-session-store' is set, saves each project
tab to its own file and non-project tabs to the global file.
Otherwise, saves everything to the global session file (traditional behavior)."
  (cond
   ;; Per-project saving enabled
   ((and tabspaces-session
         tabspaces-session-project-session-store)
    (tabspaces-save-all-project-sessions)
    (tabspaces-save-non-project-tabs))

   ;; Traditional global saving
   (tabspaces-session
    (tabspaces-save-session))))

;; Restore session functions
(defun tabspaces--get-project-session-file ()
  "Get the session file path based on configuration."
  (let* ((project-root (or (vc-root-dir)
                           (error "Not in a version controlled project")))
         (project-name (file-name-nondirectory (directory-file-name project-root)))
         (session-name (concat "." project-name "-tabspaces-session.el")))
    (cond
     ((eq tabspaces-session-project-session-store 'project)
      (expand-file-name session-name project-root))

     ((stringp tabspaces-session-project-session-store)
      (expand-file-name session-name tabspaces-session-project-session-store))

     ((functionp tabspaces-session-project-session-store)
      (funcall tabspaces-session-project-session-store project-root))

     (t (expand-file-name session-name project-root)))))

(defun tabspaces--get-project-session-file-for-restore (project)
  "Get the session file path for PROJECT based on configuration."
  (let* ((project-name (file-name-nondirectory (directory-file-name project)))
         (session-name (concat "." project-name "-tabspaces-session.el")))
    (cond
     ((eq tabspaces-session-project-session-store 'project)
      (expand-file-name session-name project))

     ((stringp tabspaces-session-project-session-store)
      (expand-file-name session-name tabspaces-session-project-session-store))

     ((functionp tabspaces-session-project-session-store)
      (funcall tabspaces-session-project-session-store project))

     (t (expand-file-name session-name project)))))


;;;###autoload
(defun tabspaces-restore-session (&optional project-or-session-file)
  "Restore tabspaces session.
If PROJECT-OR-SESSION-FILE is:
- nil: if in a project tab and per-project storage is enabled, restore current project's session;
       otherwise restore the global session from `tabspaces-session-file'
- a file path: restore that specific session file
- a project path: restore that project's session based on `tabspaces-session-project-session-store'"
  (interactive)
  (let ((session-file
         (cond
          ;; No argument - check if we're in a project tab with per-project storage
          ((null project-or-session-file)
           (if (and tabspaces-session-project-session-store
                    (project-current))
               ;; We're in a project - restore this project's session
               (let* ((project-root (project-root (project-current)))
                      (project-session (tabspaces--get-project-session-file-for-restore project-root)))
                 (if (file-exists-p project-session)
                     project-session
                   ;; Project session doesn't exist, fall back to global
                   tabspaces-session-file))
             ;; Not in a project or per-project storage disabled - use global
             tabspaces-session-file))
          ;; File path - use directly
          ((file-exists-p project-or-session-file)
           project-or-session-file)
          ;; Project path - get session file location
          (t
           (tabspaces--get-project-session-file-for-restore project-or-session-file)))))

    (if (file-exists-p session-file)
        (progn
          (load-file session-file)
          ;; Use placeholder buffer to avoid pollution
          (cl-loop for elm in tabspaces--session-list do
                   (switch-to-buffer "*tabspaces--placeholder*")
                   (tabspaces-switch-or-create-workspace (cadr elm))
                   (mapc #'find-file (car elm))
                   (when (caddr elm) ; If window state exists
                     (window-state-put (caddr elm) nil 'safe)))
          ;; Clean up placeholder buffer
          (cl-loop for elm in tabspaces--session-list do
                   (tabspaces-switch-or-create-workspace (cadr elm))
                   (tabspaces-remove-selected-buffer "*tabspaces--placeholder*"))
          (kill-buffer "*tabspaces--placeholder*")
          (message "Restored session from %s" session-file))
      (message "No session file found at %s" session-file))))

;; Make sure session file exists
(defun tabspaces--create-session-file ()
  "Create the tabspaces session file if it does not exist."
  (unless (file-exists-p tabspaces-session-file)
    (with-temp-buffer
      (write-file tabspaces-session-file))
    (message "Created tabspaces session file: %s" tabspaces-session-file)))

;; Restore session used for startup
(defun tabspaces--restore-session-on-startup ()
  "Restore tabspaces session on startup.
Unlike the interactive restore, this function does more clean up to remove
unnecessary tab."
  (message "Restoring tabspaces session on startup.")
  (tabspaces--create-session-file)
  (tabspaces-restore-session))

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
    (define-key map (kbd "w") 'tabspaces-show-workspaces)
    (define-key map (kbd "T") 'tabspaces-toggle-echo-area-display)
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
           (add-hook 'kill-emacs-hook #'tabspaces--save-session-smart))
         (when tabspaces-session-auto-restore
           (tabspaces--restore-session-on-startup))
         ;; Setup echo area display if enabled
         (tabspaces--echo-area-setup))
        (t
         ;; Remove all modifications
         (dolist (frame (frame-list))
           (tabspaces--reset-buffer-predicate frame))
         (when tabspaces-use-filtered-buffers-as-default
           (define-key (current-global-map) [remap switch-to-buffer] nil))
         (setq tab-bar-tab-post-open-functions (remove #'tabspaces--tab-post-open-function tab-bar-tab-post-open-functions))
         (remove-hook 'after-make-frame-functions #'tabspaces--set-buffer-predicate)
         (remove-hook 'kill-emacs-hook #'tabspaces--save-session-smart)
         (remove-hook 'emacs-startup-hook #'tabspaces-restore-session)
         ;; Cleanup echo area display
         (tabspaces--echo-area-cleanup))))

;;; Provide
(provide 'tabspaces)
;;; tabspaces.el ends here
