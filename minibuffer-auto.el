;;; minibuffer-auto.el --- Enhancer for default minibuffer completions -*- lexical-binding: t; -*-

;; Copyright (C) 2023 Karim Aziiev <karim.aziiev@gmail.com>

;; Author: Karim Aziiev <karim.aziiev@gmail.com>
;; URL: https://github.com/KarimAziev/minibuffer-auto
;; Version: 0.1.0
;; Keywords: tools
;; Package-Requires: ((emacs "25.1"))

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

(defun minibuffer-auto-scroll-completions-up ()
  "Scroll completions window up without selection."
  (interactive)
  (if-let ((wind (get-buffer-window "*Completions*" 0)))
      (with-selected-window wind
        (scroll-up-command))
    (minibuffer-next-completion 1)))


(defun minibuffer-auto-scroll-completions-down ()
    "Scroll completions window up without selection."
    (interactive)
    (require 'minibuffer)
    (if-let ((wind (get-buffer-window "*Completions*" 0)))
        (with-selected-window wind
          (scroll-down-command))
      (minibuffer-next-completion -1)))

(defun minibuffer-auto-beg-of-buffer ()
  "Scroll completions window down without selection."
  (interactive)
  (when-let ((wind (get-buffer-window "*Completions*" 0)))
    (with-selected-window wind
      (goto-char (point-min)))))


(defun minibuffer-auto-end-of-buffer ()
  "Scroll completions window down without selection."
  (interactive)
  (when-let ((wind (get-buffer-window "*Completions*" 0)))
    (with-selected-window wind
      (goto-char (point-max))
      (minibuffer-next-completion -1))))

(defvar minibuffer-auto-timer nil)


(defun minibuffer-auto-cleanup-hook ()
  "Cancel `minibuffer-auto-timer' and remove minibuffer hooks."
  (when (timerp minibuffer-auto-timer)
    (cancel-timer minibuffer-auto-timer)
    (setq minibuffer-auto-timer nil))
  (remove-hook 'post-command-hook #'minibuffer-auto-post-command-hook t)
  (remove-hook 'post-self-insert-hook #'minibuffer-auto-post-command-hook t)
  (remove-hook 'minibuffer-exit-hook #'minibuffer-auto-cleanup-hook))

(defun minibuffer-auto-complete ()
  "Display a list of possible completions of the current minibuffer contents."
  (when (minibuffer-window-active-p (selected-window))
    (minibuffer-completion-help)))

(defvar minibuffer-auto-def nil)

(defun minibuffer-auto-post-command-hook ()
  "Invoke `minibuffer-auto-complete' with timer."
  (setq minibuffer-auto-def (when minibuffer-default
                              (seq-copy minibuffer-default)))
  (when (timerp minibuffer-auto-timer)
    (cancel-timer minibuffer-auto-timer)
    (setq minibuffer-auto-timer nil))
  (when (memq this-command '(self-insert-command
                             delete-backward-char kill-region
                             delete-minibuffer-contents
                             backward-kill-word))
    (setq minibuffer-auto-timer (run-with-timer 0.2 nil
                                                'minibuffer-auto-complete))))

(defun minibuffer-auto-setup-hook ()
  "Setup minibuffer completions."
  (when (active-minibuffer-window)
    (unless (car (overlay-lists))
      (when (minibuffer-window-active-p (selected-window))
        (add-hook 'minibuffer-exit-hook #'minibuffer-auto-cleanup-hook)
        (add-hook 'post-command-hook #'minibuffer-auto-post-command-hook
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
                    'minibuffer-auto-crm-complete-maybe
                    nil t)
          (crm-complete)))
    (apply oldfun args)))

;;;###autoload
(define-minor-mode minibuffer-auto-mode
  "Autoexpand minibuffer completions."
  :lighter " mini-auto"
  :group 'minibuffer
  :global t
  (remove-hook 'minibuffer-setup-hook #'minibuffer-auto-setup-hook)
  (advice-remove 'completing-read-multiple
                 #'minibuffer-auto-crm-completing-read-multiple)
  (when minibuffer-auto-mode
    (add-hook 'minibuffer-setup-hook #'minibuffer-auto-setup-hook)
    (advice-add 'completing-read-multiple :around
                #'minibuffer-auto-crm-completing-read-multiple)))

(provide 'minibuffer-auto)
;;; minibuffer-auto.el ends here