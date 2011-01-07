;;; anything-replace-string.el --- replace-string() `anything.el' interface
;; -*- Mode: Emacs-Lisp -*-

;; Copyright (C) 2011 by 101000code/101000LAB

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA  02110-1301 USA

;; Version: 0.2.0
;; Author: k1LoW (Kenichirou Oyama), <k1lowxb [at] gmail [dot] com> <k1low [at] 101000lab [dot] org>
;; URL: http://code.101000lab.org

;;; Install
;; Put this file into load-path'ed directory, and byte compile it if
;; desired.  And put the following expression into your ~/.emacs.
;;
;; (require 'anything-replace-string)
;;
;; and M-x anything-replace-string

;;; Commentary:

;;; Commands:
;;
;; Below are complete command list:
;;
;;  `anything-replace-string'
;;    Replace string from history.
;;
;;; Customizable Options:
;;
;; Below are customizable option list:
;;
;;  `anything-replace-string-separator'
;;    Replace string pair separator
;;    default = " -> "

;;; Code:

(require 'cl)
(require 'anything)

(defgroup anything-replace-string nil
  "replae-string history `anything.el' interface"
  :group 'lisp
  :prefix "anything-replace-string-")

(defvar anything-replace-string-history nil
  "Replace history.")
;;init
(setq anything-replace-string-history nil)

(defvar anything-replace-string-history-candidates nil
  "Replace history.")
;;init
(setq anything-replace-string-history-candidates nil)

(defcustom anything-replace-string-separator
  " -> "
  "Replace string pair separator"
  :type 'string
  :group 'anything-replace-string)

(defadvice replace-string (before anything-replace-string-replace-string(from-string to-string &optional delimited start end) activate)
  (anything-replace-string-push-history from-string to-string))

(defun anything-replace-string-push-history (from-string to-string)
  "Push replace history."
  (push (cons from-string to-string) anything-replace-string-history)
  (push (concat from-string anything-replace-string-separator to-string) anything-replace-string-history-candidates))

(defvar anything-c-source-replace-string
  '((name . "Replace string history")
    (candidates . anything-replace-string-history-candidates)
    (action
     ("Replace String" . (lambda (candidate)
                           (loop with match = nil
                                 until match
                                 for x in anything-replace-string-history
                                 do (if (equal (concat (car x) anything-replace-string-separator (cdr x)) candidate)
                                        (progn
                                          (anything-replace-string-region x)
                                          (setq match t)
                                          (return nil)
                                          ))))))
    (migemo)
    (multiline)))

(defvar anything-c-source-replace-string-dummy
  '((name . "Replace string")
    (dummy)
    (action
     ("Replace String" . (lambda (candidate)
                           (let ((to-string candidate) (prompt "Replace string in region "))
                             (unless (region-active-p)
                               (setq prompt "Replace string "))
                             (setq to-string (read-string (concat prompt candidate " with: ")))
                             (anything-replace-string-region (cons candidate to-string))
                             (anything-replace-string-push-history candidate to-string)
                           ))))))

(defun anything-replace-string-region (x)
  "Replace string."
  (let ((beginning (region-beginning)) (end (region-end)))
    (save-excursion
      (if (region-active-p)
          (progn (goto-char beginning)
                 (while (search-forward (car x) nil t)
                   (if (>= end (point))
                       (replace-match (cdr x) nil t))))
        (goto-char (point-min))
        (while (search-forward (car x) nil t)
          (replace-match (cdr x) nil t))
        ))))

(defun anything-replace-string()
  "Replace string from history."
  (interactive)
  (let ((prompt "Replace string in region: "))
    (unless (region-active-p)
      (setq prompt "Replace string: "))
    (anything (list anything-c-source-replace-string-dummy anything-c-source-replace-string) nil prompt nil nil)))

(provide 'anything-replace-string)
;;; anything-replace-string.el ends here