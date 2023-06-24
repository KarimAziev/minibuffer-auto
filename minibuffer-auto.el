;;; minibuffer-auto.el --- Enhancer for default minibuffer completions -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/minibuffer-auto
;; Version: 0.1.0
;; Keywords: tools
;; Package-Requires: ((emacs "28.1"))
;; SPDX-License-Identifier: GPL-3.0-or-later

;; This file is NOT part of GNU Emacs.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Enhancer for default minibuffer completions

;;; Code:

(require 'minibuffer)
(require 'crm)

(defvar minibuffer-auto-command nil)
(defvar minibuffer-auto-default-action nil)

(defvar minibuffer-auto-preview-buffer-name "*minibuffer-auto-preview*")

(defun minibuffer-auto-scroll (direction)
  "Scroll completions up if DIRECTION is positive integer, or down."
  (cond ((memq 'ivy--queue-exhibit post-command-hook)
         nil)
        ((and (or (bound-and-true-p fido-mode)
                  (bound-and-true-p icomplete-mode)
                  (bound-and-true-p fido-vertical-mode))
              (fboundp 'icomplete-forward-completions)
              (fboundp 'icomplete-backward-completions))
         (let ((count (window-height nil t)))
           (when (> count 0)
             (dotimes (_i count)
               (if (> direction 0)
                   (icomplete-forward-completions)
                 (icomplete-backward-completions))))))
        (t
         (if-let ((wind (get-buffer-window "*Completions*" 0)))
             (with-selected-window wind
               (cond ((and (equal (line-end-position)
                                  (point-max))
                           (< (window-height nil t)
                              completions-max-height))
                      (minibuffer-auto-beg-of-buffer))
                     ((and (equal (line-beginning-position)
                                  (point-min))
                           (< (window-height nil t)
                              completions-max-height))
                      (minibuffer-auto-end-of-buffer))
                     ((< (window-height nil t)
                         completions-max-height)
                      (if (> direction 0)
                          (minibuffer-auto-end-of-buffer)
                        (minibuffer-auto-beg-of-buffer)))
                     (t (minibuffer-next-completion
                         (if (> direction 0)
                             (/ completions-max-height 2)
                           (- (/ completions-max-height 2)))))))
           (minibuffer-next-completion direction)))))

;;;###autoload
(defun minibuffer-auto-scroll-completions-up ()
  "Scroll completions window up without selection."
  (interactive)
  (minibuffer-auto-scroll 1))

;;;###autoload
(defun minibuffer-auto-scroll-completions-down ()
  "Scroll completions window up without selection."
  (interactive)
  (minibuffer-auto-scroll -1))

;;;###autoload
(defun minibuffer-auto-beg-of-buffer ()
  "Scroll completions window down without selection."
  (interactive)
  (when-let ((wind (get-buffer-window "*Completions*" 0)))
    (with-selected-window wind
      (goto-char (point-min))
      (minibuffer-next-completion 1)
      (minibuffer-next-completion -1))
    (minibuffer-next-completion 1)))

;;;###autoload
(defun minibuffer-auto-end-of-buffer ()
  "Scroll completions window down without selection."
  (interactive)
  (when-let ((wind (get-buffer-window "*Completions*" 0)))
    (with-selected-window wind
      (goto-char (point-max)))
    (minibuffer-next-completion -1)))

(defvar minibuffer-auto-timer nil)

(defun minibuffer-auto-cleanup-hook ()
  "Cancel `minibuffer-auto-timer' and remove minibuffer hooks."
  (when (timerp minibuffer-auto-timer)
    (cancel-timer minibuffer-auto-timer)
    (setq minibuffer-auto-timer nil))
  (setq minibuffer-auto-default-action nil)
  (setq minibuffer-auto-command nil)
  (minibuffer-auto-remove-preview-buffer)
  (remove-hook 'post-command-hook #'minibuffer-auto-post-command-hook t)
  (remove-hook 'post-self-insert-hook #'minibuffer-auto-post-command-hook t)
  (remove-hook 'after-change-functions #'minibuffer-auto-post-command-hook t)
  (remove-hook 'minibuffer-exit-hook #'minibuffer-auto-cleanup-hook))

(defun minibuffer-auto-complete ()
  "Display a list of possible completions of the current minibuffer contents."
  (when (minibufferp)
    (cond ((memq 'ivy--queue-exhibit post-command-hook)
           nil)
          ((and (or (bound-and-true-p fido-mode)
                    (bound-and-true-p icomplete-mode)
                    (bound-and-true-p fido-vertical-mode)))
           nil)
          (t
           (minibuffer-completion-help)))))

(defun minibuffer-auto-force-complete ()
  "Display a list of possible completions of the current minibuffer contents."
  (when (minibuffer-window-active-p (selected-window))
    (minibuffer-completion-help)))

(defvar minibuffer-auto-def nil)

;;;###autoload
(defun minibuffer-auto-up-to-git-or-home-dir ()
  "Go to the parent directory until .git or home directory reached."
  (interactive)
  (when (minibuffer-window-active-p (selected-window))
    (let ((dir (buffer-substring-no-properties (minibuffer-prompt-end)
                                               (line-end-position))))
      (when (or (not (file-exists-p dir))
                (not (file-directory-p dir)))
        (minibuffer-auto-directory-up)
        (setq dir (buffer-substring-no-properties (minibuffer-prompt-end)
                                                  (line-end-position))))
      (let ((initial-dir dir))
        (while (and (file-exists-p dir)
                    (file-directory-p dir)
                    (not (file-equal-p dir "~/"))
                    (or
                     (file-equal-p dir initial-dir)
                     (not (file-exists-p (expand-file-name ".git" dir)))))
          (minibuffer-auto-directory-up)
          (setq dir (buffer-substring-no-properties (minibuffer-prompt-end)
                                                    (line-end-position)))))
      (minibuffer-auto-complete))))

;;;###autoload
(defun minibuffer-auto-directory-up ()
  "Go to the parent directory preselecting the current one."
  (interactive)
  (when (minibuffer-window-active-p (selected-window))
    (if-let ((beg (save-excursion
                    (cond ((looking-back "~/" 0)
                           (1- (point)))
                          (t (when (re-search-backward "/" nil t
                                                       (if
                                                           (looking-back "/" 0)
                                                           2
                                                         1))
                               (when (file-directory-p (buffer-substring-no-properties
                                                        (minibuffer-prompt-end)
                                                        (point)))
                                 (1+ (point)))))))))
        (progn (delete-region beg
                              (point))
               (when (looking-back "~" 0)
                 (delete-region (1- (point))
                                (point))
                 (insert (file-name-parent-directory "~")))
               (minibuffer-auto-complete))
      (delete-region (minibuffer-prompt-end)
                     (line-end-position)))))

(defun minibuffer-auto-post-command-hook (&rest _)
  "Invoke `minibuffer-auto-complete' with timer."
  (setq minibuffer-auto-def (when minibuffer-default
                              (seq-copy minibuffer-default)))
  (when (timerp minibuffer-auto-timer)
    (cancel-timer minibuffer-auto-timer))
  (when (memq this-command
              '(self-insert-command
                delete-backward-char
                kill-region
                delete-minibuffer-contents
                backward-kill-word))
    (setq minibuffer-auto-timer
          (run-with-timer 0.5 nil
                          #'minibuffer-auto-complete))))

(defun minibuffer-auto-setup-hook ()
  "Setup minibuffer completions."
  (when (active-minibuffer-window)
    (unless (car (overlay-lists))
      (when (minibuffer-window-active-p (selected-window))
        (add-hook 'minibuffer-exit-hook #'minibuffer-auto-cleanup-hook)
        (add-hook 'after-change-functions #'minibuffer-auto-post-command-hook
                  nil t)
        (minibuffer-auto-complete)))))

(defun minibuffer-auto-crm-complete-maybe ()
  "Trigger `crm-complete' if previuos char is crm separator."
  (when (looking-back crm-separator 0)
    (crm-complete)))

(defun minibuffer-auto-crm-completing-read-multiple (oldfun &rest args)
  "Apply OLDFUN (`completing-read-multiple') with ARGS with some hooks.
Triggers complete on the beginning and after inserting `crm-separator'."
  (minibuffer-with-setup-hook
      (lambda ()
        (when (minibuffer-window-active-p (selected-window))
          (add-hook 'post-self-insert-hook
                    #'minibuffer-auto-crm-complete-maybe
                    nil t)
          (crm-complete)))
    (apply oldfun args)))

(defvar minibuffer-auto-exit-actions `((file
                                      find-file-other-window find-file)
                                     (buffer switch-to-buffer-other-window
                                             switch-to-buffer)
                                     (bookmark bookmark-jump-other-window
                                               bookmark-jump)))

(defun minibuffer-auto--metadata ()
  "Return current minibuffer completion metadata."
  (completion-metadata
   (buffer-substring-no-properties
    (minibuffer-prompt-end)
    (max (minibuffer-prompt-end)
         (point)))
   minibuffer-completion-table
   minibuffer-completion-predicate))

(defun minibuffer-auto-ivy-selected-cand ()
  "Return the currently selected item in Ivy."
  (when (and (memq 'ivy--queue-exhibit post-command-hook)
             (boundp 'ivy-text)
             (boundp 'ivy--length)
             (boundp 'ivy-last)
             (fboundp 'ivy--expand-file-name)
             (fboundp 'ivy-state-current))
    (cons
     (completion-metadata-get (ignore-errors (minibuffer-auto--metadata))
                              'category)
     (ivy--expand-file-name
      (if (and (> ivy--length 0)
               (stringp (ivy-state-current ivy-last)))
          (ivy-state-current ivy-last)
        ivy-text)))))

(defun minibuffer-auto-default-candidates ()
  "Return all current completion candidates from the minibuffer."
  (when (minibufferp)
    (let* ((all (completion-all-completions
                 (minibuffer-contents)
                 minibuffer-completion-table
                 minibuffer-completion-predicate
                 (max 0 (- (point)
                           (minibuffer-prompt-end)))))
           (last (last all)))
      (when last (setcdr last nil))
      (cons
       (completion-metadata-get (minibuffer-auto--metadata) 'category)
       all))))

(defun minibuffer-auto-default-top-minibuffer-completion ()
  "Target the top completion candidate in the minibuffer.
Return the category metadatum as the type of the target.

This target finder is meant for the default completion UI and
completion UI highly compatible with it, like Icomplete.
Many completion UIs can still work with Embark but will need
their own target finder.  See for example
`embark--vertico-selected' or `embark--selectrum-selected'."
  (when (and (minibufferp) minibuffer-completion-table)
    (pcase-let* ((`(,category . ,candidates)
                  (minibuffer-auto-default-candidates))
                 (contents (minibuffer-contents))
                 (top (if (test-completion contents
                                           minibuffer-completion-table
                                           minibuffer-completion-predicate)
                          contents
                        (let ((completions (completion-all-sorted-completions)))
                          (if (null completions)
                              contents
                            (concat
                             (substring contents
                                        0 (or (cdr (last completions)) 0))
                             (car completions)))))))
      (cons category (or (car (member top candidates)) top)))))

(defvar minibuffer-auto-targets-finders
  '(minibuffer-auto-ivy-selected-cand
    minibuffer-auto-default-top-minibuffer-completion))

(defun minibuffer-auto-get-current-candidate ()
  "Return cons filename for current completion candidate."
  (let (target)
    (run-hook-wrapped
     'minibuffer-auto-targets-finders
     (lambda (fun)
       (when-let ((result (funcall fun)))
         (when (and (cdr-safe result)
                    (stringp (cdr-safe result))
                    (not (string-empty-p (cdr-safe result))))
           (setq target result)))
       (and target (minibufferp))))
    (if (car target)
        target
      (pcase minibuffer-auto-command
        ('helpful-variable
         (setcar target 'variable))
        ('imenu (setcar target 'imenu)))
      target)))

(defun minibuffer-auto-file-preview (current)
  "Preview CURRENT as file."
  (when (and
         current
         (file-exists-p current)
         (file-name-absolute-p current)
         (file-readable-p current)
         (not (file-remote-p current))
         (not
          (member (file-name-extension current)
                  '("plist" "sqlite" "plstore" "gpg" "eln" "gz" "tar")))
         (not (seq-find (lambda (re)
                          (string-match-p re current))
                        (mapcar #'car file-name-handler-alist))))
    (if (and (file-directory-p current))
        (cond ((not (file-accessible-directory-p current))
               nil)
              ((and (memq 'ivy--queue-exhibit post-command-hook)
                    (fboundp 'ivy-insert-current))
               (when-let ((dir (and
                                (boundp 'ivy--length)
                                (fboundp 'ivy-expand-file-if-directory)
                                (> ivy--length 0)
                                (not (string= current "./"))
                                (ivy-expand-file-if-directory
                                 current))))
                 (progn
                   (when (and (fboundp 'ivy--cd)
                              (fboundp 'ivy--exhibit))
                     (ivy--cd dir)
                     (ivy--exhibit)))))
              ((or (bound-and-true-p fido-mode)
                   (bound-and-true-p icomplete-mode)
                   (bound-and-true-p fido-vertical-mode)))
              (t))
      (when (not (and large-file-warning-threshold
                      (> (file-attribute-size
                          (file-attributes current))
                         large-file-warning-threshold)))
        (let ((buffer (get-buffer-create minibuffer-auto-preview-buffer-name)))
          (if (active-minibuffer-window)
              (progn (with-minibuffer-selected-window
                       (with-current-buffer (get-buffer-create buffer)
                         (let ((inhibit-read-only t))
                           (erase-buffer)
                           (if-let ((buff (get-file-buffer current)))
                               (insert (with-current-buffer buff
                                         (buffer-string)))
                             (insert-file-contents current)
                             (let
                                 ((buffer-file-name (expand-file-name current)))
                               (delay-mode-hooks (set-auto-mode)
                                                 (font-lock-ensure))
                               (when (derived-mode-p 'emacs-lisp-mode)
                                 (let ((inhibit-message t))
                                   (hs-minor-mode)
                                   (when (fboundp 'hs-hide-all)
                                     (hs-hide-all))))))
                           (setq header-line-format
                                 (abbreviate-file-name current))
                           (unless (get-buffer-window (current-buffer))
                             (pop-to-buffer-same-window (current-buffer)))))))
            (find-file current)))))))

(defun minibuffer-auto-with-selected-window (action &rest args)
  "Apply ACTION with ARGS in `minibuffer-selected-window' or in the current."
      (with-selected-window (minibuffer-selected-window)
  (if (minibuffer-selected-window)
        (apply action args))
    (apply action args)))

(defun minibuffer-auto-jump-to-symbol (other-wind found)
  "Jump to FOUND cons cell in OTHER-WIND if non-nil."
  (let ((buff (if (consp found)
                  (car-safe found)
                found))
        (pos (cdr-safe found)))
    (when (and (stringp buff)
               (file-exists-p buff))
      (setq buff (find-file-noselect buff)))
    (when (bufferp buff)
      (when other-wind
        (delete-other-windows)
        (select-window (split-window-right)))
      (with-current-buffer buff
        (when pos (goto-char pos))
        (pop-to-buffer-same-window
         (current-buffer))))))

(defun minibuffer-auto-symbol-action (current other-wind)
  "Try to find and jump to definiton for CURRENT.
If OTHER-WIND do it in other window."
  (when-let* ((symb (read current))
              (result
               (remove nil (append
                            (mapcar
                             (lambda (type)
                               (ignore-errors
                                 (find-definition-noselect
                                  symb
                                  type)))
                             '(nil defvar defface))
                            (list (ignore-errors (symbol-file symb))))))
              (found (car-safe result)))
    (run-with-timer 0.1 nil
                    #'minibuffer-auto-jump-to-symbol
                    other-wind found)
    (abort-minibuffers)))

(defun minibuffer-auto-symbol-action-other-wind (current)
  "Try to find and jump to definiton for CURRENT.
If OTHER-WIND do it in other window."
  (minibuffer-auto-jump-to-symbol t current))

(defun minibuffer-auto-find-and-exit (&optional other-wind)
  "Try to find symbol or file definition and exit minibuffer.
If OTHER-WIND do it in other window."
  (pcase-let ((`(,category . ,current)
               (minibuffer-auto-get-current-candidate)))
    (if-let* ((action (cdr (assq category minibuffer-auto-exit-actions)))
              (fn (nth (if other-wind
                           0
                         1)
                       action)))
        (progn (run-with-timer 0.1 nil fn current)
               (abort-minibuffers))
      (minibuffer-auto-symbol-action current other-wind))))

(defun minibuffer-auto-try-find-symbol (current)
  "Try to find symbol definition for CURRENT minibuffer completion."
  (when-let* ((symb (read current))
              (result
               (remove nil (append
                            (mapcar
                             (lambda (type)
                               (ignore-errors
                                 (find-definition-noselect
                                  symb
                                  type)))
                             '(nil defvar defface))
                            (list (ignore-errors (symbol-file symb))))))
              (found (car-safe result)))
    found))

(defun minibuffer-auto-preview-dwim ()
  "Scroll completions window up without selection."
  (interactive)
  (pcase-let ((`(,category . ,current)
               (minibuffer-auto-get-current-candidate)))
    (minibuffer-auto-action current category)))

(defun minibuffer-auto-action (current &optional category)
  "Apply preview action on minibuffer CURRENT completion with CATEGORY."
  (when (and
         (not category)
         current
         (file-name-absolute-p current)
         (file-exists-p current))
    (setq category 'file))
  (when (stringp current)
    (pcase category
      ('imenu (with-minibuffer-selected-window
                (let ((pos (point)))
                  (imenu current)
                  (unwind-protect
                      (read-key-sequence "")
                    (setq unread-command-events
                          (append (this-single-command-raw-keys)
                                  unread-command-events))
                    (goto-char pos)))))
      ('file
       (minibuffer-auto-file-preview current))
      ('variable
       (with-minibuffer-selected-window
         (let ((inhibit-message t))
           (describe-variable (intern-soft
                               current)))))
      ('library
       (minibuffer-auto-with-selected-window
        'find-library
        current))
      ('minor-mode (with-minibuffer-selected-window
                     (describe-symbol
                      (intern-soft
                       current))))
      ('bookmark (minibuffer-auto-with-selected-window
                  'bookmark-jump
                  current))
      ('buffer
       (when (buffer-live-p (get-buffer current))
         (minibuffer-auto-with-selected-window
          'display-buffer
          (get-buffer current))))
      ('command
       (with-minibuffer-selected-window
         (describe-command
          (intern-soft
           current))))
      (_
       (or
        (when-let ((result (minibuffer-auto-try-find-symbol current)))
          (minibuffer-auto-with-selected-window
           'minibuffer-auto-jump-to-symbol
           nil
           result)
          t)
        (when (functionp minibuffer-auto-default-action)
          (funcall minibuffer-auto-default-action)
          t))))))

;;;###autoload
(defun minibuffer-auto-find-dwim-other-window ()
  "Try to find file or symbol definition and exit minibuffer."
  (interactive)
  (minibuffer-auto-find-and-exit t))

;;;###autoload
(defun minibuffer-auto-find-dwim ()
  "Try to find file or symbol definition and exit minibuffer."
  (interactive)
  (minibuffer-auto-find-and-exit nil))

(defun minibuffer-auto-fido-mode-p ()
  "Return non nil if fido related modes enabled."
  (or (bound-and-true-p fido-mode)
      (bound-and-true-p icomplete-mode)
      (bound-and-true-p fido-vertical-mode)))




(defun minibuffer-auto-insert-action (item &optional separator)
  "Insert or complete ITEM and SEPARATOR.
If word at point is prefix of ITEM, complete it, else insert ITEM.
Optional argument SEPARATOR is a string to insert just after ITEM.
Default value of SEPARATOR is space."
  (let ((parts))
    (setq parts
          (if-let ((current-word
                    (let* ((end (point))
                           (beg (+ end
                                   (save-excursion
                                     (skip-chars-backward
                                      (concat
                                       "A-Za-z0-9"
                                       (mapconcat
                                        #'regexp-quote
                                        (delete-dups (split-string
                                                      (replace-regexp-in-string
                                                       "[A-Za-z0-9]"
                                                       ""
                                                       item)
                                                      "" t))
                                        "")))))))
                      (when (< beg end)
                        (buffer-substring-no-properties beg end)))))
              (progn
                (if (string-prefix-p current-word item)
                    (list (substring-no-properties item (length current-word)))
                  (list (or separator "\n") item)))
            (list item)))
    (apply #'insert parts)))

(defun minibuffer-auto-insert-exit-with-action (action)
  "Call ACTION with current candidate and exit minibuffer."
  (pcase-let ((`(,_category . ,current)
               (minibuffer-auto-get-current-candidate)))
    (progn (run-with-timer 0.1 nil action current)
           (abort-minibuffers))))

;;;###autoload
(defun minibuffer-auto-copy-current ()
  "Copy current minibuffer candidate without exiting minibuffer."
  (interactive)
  (pcase-let ((`(,_category . ,current)
               (minibuffer-auto-get-current-candidate)))
    (when current
      (kill-new current)
      (message "Copied %s" current))))


;;;###autoload
(defun minibuffer-auto-insert ()
  "Call ACTION with current candidate and exit minibuffer."
  (interactive)
  (minibuffer-auto-insert-exit-with-action #'minibuffer-auto-insert-action))

(defvar minibuffer-auto-extra-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-c C-j")
                #'minibuffer-auto-find-dwim)
    (define-key map (kbd "C-j")
                #'minibuffer-auto-preview-dwim)
    (define-key map (kbd "C-c C-o")
                #'minibuffer-auto-find-dwim-other-window)
    (define-key map (kbd "C-M-l") #'minibuffer-auto-up-to-git-or-home-dir)
    (define-key map (kbd "C-c C-i") #'minibuffer-auto-insert)
    (define-key map (kbd "M-w") #'minibuffer-auto-copy-current)
    map))

(defvar-local minibuffer-auto-last-input nil)
(defun minibuffer-auto-pre-command-preview-hook ()
  "Try to find file or symbol definition and exit minibuffer."
  (remove-hook 'pre-command-hook #'minibuffer-auto-pre-command-preview-hook
               t)
  (when (memq this-command '(icomplete-forward-completions
                             icomplete-backward-completions))
    (when-let ((current (cdr (minibuffer-auto-get-current-candidate))))
      (when (and (file-exists-p current)
                 (not (file-directory-p current)))
        (minibuffer-auto-directory-up)))))

(defun minibuffer-auto-remove-preview-buffer ()
  "Remove buffer `minibuffer-auto-preview-buffer-name'."
  (when (buffer-live-p (get-buffer minibuffer-auto-preview-buffer-name))
    (kill-buffer (get-buffer minibuffer-auto-preview-buffer-name))))

(defun minibuffer-auto--minibuffer-setup-local-map ()
  "Hook function for `icomplete-minibuffer-setup-hook'."
  (use-local-map
   (make-composed-keymap (current-local-map)
                         minibuffer-auto-extra-map)))

(defvar-local minibuffer-auto-preview-candidate nil)
(defvar minibuffer-auto-preview-timer nil)
(defun minibuffer-auto-do-preview ()
  "Try to preview current minibuffer candidate."
  (when (minibufferp)
    (let ((cand (minibuffer-auto-get-current-candidate))
          (curr minibuffer-auto-preview-candidate))
      (when (not (equal cand curr))
        (setq minibuffer-auto-preview-candidate cand)
        (when-let ((current (cdr cand)))
          (when (and (file-exists-p current)
                     (not (file-directory-p current)))
            (when (minibuffer-auto-fido-mode-p)
              (add-hook 'pre-command-hook
                        #'minibuffer-auto-pre-command-preview-hook nil t)))
          (minibuffer-auto-action current (car cand)))))))

(defun minibuffer-auto-schedule-preview ()
  "Run preview command after timeout."
  (setq minibuffer-auto-last-input
        (buffer-substring-no-properties
         (minibuffer-prompt-end)
         (line-end-position)))
  (when (timerp minibuffer-auto-preview-timer)
    (cancel-timer minibuffer-auto-preview-timer))
  (setq minibuffer-auto-preview-timer
        (run-with-timer 0.5 nil
                        #'minibuffer-auto-do-preview)))

(defun minibuffer-auto-setup-preview ()
  "Setup auto preview."
  (when (minibufferp)
    (setq minibuffer-auto-command this-command)
    (setq minibuffer-auto-default-action (key-binding (kbd "C-j") nil t))
    (minibuffer-auto--minibuffer-setup-local-map)
    (when (bound-and-true-p icomplete-mode)
      (add-hook 'icomplete-minibuffer-setup-hook
                #'minibuffer-auto--minibuffer-setup-local-map))
    (when (bound-and-true-p icomplete-vertical-mode)
      (add-hook 'icomplete--vertical-minibuffer-setup
                #'minibuffer-auto--minibuffer-setup-local-map))
    (add-hook 'pre-command-hook #'minibuffer-auto-schedule-preview nil t)
    (use-local-map
     (make-composed-keymap (current-local-map)
                           minibuffer-auto-extra-map))))

;;;###autoload
(define-minor-mode minibuffer-auto-crm-mode
  "Autotrigger minibuffer completions for `completing-read-multiple'."
  :lighter " crm-auto"
  :group 'minibuffer
  :global t
  (advice-remove 'completing-read-multiple
                 #'minibuffer-auto-crm-completing-read-multiple)
  (when minibuffer-auto-crm-mode
    (advice-add 'completing-read-multiple :around
                #'minibuffer-auto-crm-completing-read-multiple)))

;;;###autoload
(define-minor-mode minibuffer-auto-mode
  "Autoexpand minibuffer completions."
  :lighter " mini-auto"
  :group 'minibuffer
  :global t
  (remove-hook 'minibuffer-setup-hook #'minibuffer-auto-setup-hook)
  (when minibuffer-auto-mode
    (add-hook 'minibuffer-setup-hook #'minibuffer-auto-setup-hook)))

;;;###autoload
(define-minor-mode minibuffer-auto-preview-mode
  "Enhance minibuffer map with preview commands."
  :lighter " mini-auto"
  :group 'minibuffer
  :global t
  (remove-hook 'minibuffer-setup-hook #'minibuffer-auto-setup-preview)
  (remove-hook 'icomplete-minibuffer-setup-hook
               #'minibuffer-auto--minibuffer-setup-local-map)
  (remove-hook 'icomplete-minibuffer-setup-hook
               #'minibuffer-auto--minibuffer-setup-local-map)
  (when minibuffer-auto-preview-mode
    (add-hook 'minibuffer-setup-hook #'minibuffer-auto-setup-preview)
    (add-hook 'minibuffer-exit-hook #'minibuffer-auto-cleanup-hook)))

(provide 'minibuffer-auto)
;;; minibuffer-auto.el ends here