;;; -*- mode: emacs-lisp; coding: utf-8-unix; indent-tabs-mode: nil -*-

;; Copyright (C) 2008 Takeru Naito
;; Author: Takeru Naito <takeru.naito@gmail.com>
;; Original: cho45 http://lowreal.rubyforge.org/pit/

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published
;; by the Free Software Foundation; either version 2, or (at your
;; option) any later version.

;; This file is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; * Description
;;
;; pit.el manipulate Pit data.
;
;; See http://lowreal.rubyforge.org/pit/
;;
;; * Usage
;;
;; Just put the code like below into your .emacs:
;;
;; (require 'pit)
;;
;; (pit/get 'github.com)
;; (pit/get 'github.com '(require ((user . "Your github user name")
;;                                 (token . "Your github token"))))
;;
;; (pit/set 'github.com)
;; (pit/set 'github.com '(config ((user . "Your github user name")
;;                                (token . "Your github token"))))
;; (pit/set 'github.com '(data ((user . "Your github user name")
;;                              (token . "Your github token"))))

;;; Change Log:

;; 2008-12-19:
;;  * Initial import

(defun x->bool (elt) (not (not elt)))

(defun fold-left (proc init lis)
  (if lis (fold-left proc (funcall proc init (car lis)) (cdr lis)) init))

(defvar pit/directory "~/.pit")
(defvar pit/config (expand-file-name
                    (format "%s/%s.yaml" pit/directory "pit")))

(defun alist/merge (base other)
  (mapcar (lambda (key)
            (or (assoc key other)
                (assoc key base)))
          (delete-duplicates
           (mapcar (lambda (cons)
                     (car cons))
                   (append base other)))))

(defalias 'alist/update 'alist/merge)

(defun pit/profile ()
  (when (file-exists-p pit/config)
    (let* ((profile
            (with-temp-buffer
              (insert-file-contents pit/config)
              (when (re-search-forward "^profile:[ ]*\\(.+\\)" nil t)
                (match-string 1))))
           (profile/file
            (expand-file-name
             (format "%s/%s.yaml" pit/directory profile))))
      (when (file-exists-p profile/file)
        profile/file))))

(defun pit/kyes/all-p (ret keys)
  (x->bool
   (fold-left (lambda (x y)
                (and x (assoc y ret) y))
              t keys)))

(defun pit/set (name &optional opts)
  (let ((profile (pit/load))
        (result
         (if (eq (car opts) 'data)
             (cadr opts)
           (mapcar
            (lambda (pair)
              (let ((key (car pair))
                    (value (cdr pair)))
                (cons key
                      (read-from-minibuffer
                       (format "\[%s\] %s: " name key)
                       value))))
            (or (cadr opts)
                (pit/get name))))))

    (when (eq (or (assoc 'config opts)
                  (assoc name profile))
              result)
      (message "No Changes")
      (assoc name profile))

    (let*
        ((brand-new-profile
          (mapcar (lambda (prof)
                    (if (eq (car prof) name)
                        `(,name ,result)
                      prof))
                  profile))
         (names (mapcar (lambda (pair)
                          (car pair))
                        brand-new-profile)))

      (set-buffer (find-file-noselect (pit/profile)))
      (erase-buffer)
      (insert "--- ")
      (mapcar (lambda (name)
                (insert (format "\n%s: " name))
                (mapcar (lambda (pair)
                          (insert (format "\n  %s: %s "
                                          (car pair)
                                          (cdr pair))))
                        (cadr (assoc name brand-new-profile))))
              names)
      (insert "\n")
      (save-buffer))
    result))

(defun pit/get (name &optional opts)
  (let* ((profile (pit/load))
         (ret (cadr (assoc name profile))))

    (if (eq (car opts) 'require)
        (let*
            ((required (cadr opts))
             (keys (mapcar (lambda (cons)
                             (car cons))
                           required)))
          (when keys
            (if (pit/kyes/all-p ret keys)
                ret
              (pit/set name `(config ,(alist/update required ret))))))
      ret)))

(defadvice message (around
                    silent-message (format-string &rest args)))

(defun pit/load ()
  (when (pit/profile)
    (let ((result))
      (unwind-protect
          (ad-activate-regexp "^silent-message$")
        (with-temp-buffer
          (insert-file-contents (pit/profile))
          (mapc (lambda (pair)
                  (let ((before (car pair))
                        (after (cadr pair)))
                    (replace-regexp before after)
                    (goto-line 0)))
                '((" +$" "")
                  ("---" "(setq result '(")
                  ("^\\([^ ].+\\):" "(\\1 (")
                  ("^[ ]+\\(.+\\):[ ]+?\\(.+\\)" "(\\1 . \"\\2\")")
                  ("\\()\\)\n\\(^[^'].+(\\)" "))\\1\n\\2")))
          (goto-char (point-max))
          (insert "))))")
          (eval-buffer))
        (ad-deactivate-regexp "^silent-message$"))
      result)))

(provide 'pit)