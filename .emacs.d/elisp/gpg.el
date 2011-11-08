;;;
;;; gpg.el -- an easy way to edit GnuPG files encrypted with 
;;;           shared-key cryptography
;;;

;; Author:  Kazu Yamamoto <Kazu@Mew.org>
;; Created: Oct 16, 2003
;; Revised: Mar 25, 2004

;;; Commentary:

;;
;; Use Emacs 20.7 or later and GnuPG ver 1.2.3 or later.
;;
;; 0) Put the following to your ".emacs"
;;
;;	(autoload 'gpg-after-find-file "gpg" nil t)
;;	(add-hook 'find-file-hooks 'gpg-after-find-file)
;;
;; 1) To load GnuPG file, say foo.gpg
;;
;;	C-xC-f + foo.gpg
;;	And type its passphrase.
;;
;; 2) To save the buffer to the file
;;
;;	C-xC-s
;;	If 'gpg-cache-passphrase' is non-nil, the passpharse used 
;;	on decryption is automatically used to encrypt the buffer.
;;
;; Note that your passphrase is never saved to a file. It exists only 
;; in the memory. However, plain text is stored to a temporary file both 
;; on encryption and decryption. The temporary file is created in the
;; same directory of the GnuPG file. You should not use a network file
;; system for the directory. Note also that creating temporary files
;; potentially causes race condition on Emacs 20.7.
;;

;;; Code:

(defconst gpg-version "0.09")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Customizable variables
;;;

(defvar gpg-map nil)

(unless gpg-map
  (setq gpg-map (make-sparse-keymap))
  (define-key gpg-map "\C-x\C-s" 'gpg-save-buffer))

(defvar gpg-program "gpg")

(defvar gpg-cipher "AES"
  "*String name of shared-key cryptography. 
To know names of supported algorithms, type \"gpg --version\".")

(defvar gpg-cache-passphrase t
  "*If non-nil, the passpharse used on decryption is used
to encrypt the buffer when saving.")

(defvar gpg-regex-suffix "\\.gpg$")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Internal variables
;;;

(defvar gpg-rendezvous nil)
(defvar gpg-passphrase nil)

(mapcar 'make-variable-buffer-local
	(list 'gpg-rendezvous
	      'gpg-passphrase))

(defvar gpg-process-encryption "*gpg encryption*")
(defvar gpg-process-decryption "*gpg decryption*")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Sub-functions
;;;

(defun gpg-file-modified-time (file)
  (nth 5 (file-attributes file)))

(defun gpg-file-newer (t1 t2)
  (and t1
       (or (null t2) 
	   (> (nth 0 t1) (nth 0 t2))
	   (and (= (nth 0 t1) (nth 0 t2))
		(> (nth 1 t1) (nth 1 t2))))))

(if (fboundp 'make-temp-file)
    (defalias 'gpg-make-temp-file 'make-temp-file) ;; secure
  (defalias 'gpg-make-temp-file 'make-temp-name)) ;; insecure

(defun gpg-start-process (name buffer program &rest program-args)
  (let ((process-environment (copy-sequence process-environment)))
    (setenv "LANGUAGE" "C")
    (setenv "LC_ALL" "C")
    (setenv "LANG" "C")
    (apply 'start-process name buffer program program-args)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Decrypting and loading
;;;

(defun gpg-buffer-hack ()
  (let ((pass gpg-passphrase) ;; major mode kills local variables
	(buffer-file-name (file-name-sans-extension buffer-file-name)))
    (set-auto-mode)
    (hack-local-variables)
    ;; Since save-buffer() is not used, we don't have to take care of
    ;; make-backup-files
    (auto-save-mode nil)
    (gpg-setup-keymap)
    (setq gpg-passphrase pass)))

(defun gpg-after-find-file ()
  (if (string-match gpg-regex-suffix (buffer-file-name))
      (if (= (buffer-size) 0)
	  (progn
	    (set-auto-mode)
	    (gpg-buffer-hack))
	(gpg-insert-file-contents))))

(defun gpg-insert-file-contents ()
  "Erases the current buffer, decrypting the corresponding file
and inserts it.
If 'gpg-cache-passphrase' is non-nil,
the passphrase used on decryption is stored as a local variable.
To save the buffer and encrypt the file, type \\<gpg-map>\\[gpg-save-buffer].
See also 'gpg-save-buffer'."
  (interactive)
  (let* ((process-connection-type t) ;; 'pty
	 (buf (current-buffer))
	 (file (buffer-file-name))
	 (tfile (gpg-make-temp-file file))
	 (oldt (gpg-file-modified-time tfile))
	 newt pro)
    (setq pro (gpg-start-process gpg-process-decryption buf gpg-program
				 "-d" "--yes" "--output" tfile file))
    (set-process-filter   pro 'gpg-filter)
    (set-process-sentinel pro 'gpg-sentinel)
    (setq gpg-rendezvous t)
    (while gpg-rendezvous
      (sit-for 0.1)
      (discard-input))
    (setq newt (gpg-file-modified-time tfile))
    (cond
     ((gpg-file-newer newt oldt)
      (erase-buffer)
      (if (default-value 'enable-multibyte-characters)
	  (set-buffer-multibyte t))
      (let ((coding-system-for-read 'undecided))
	(insert-file-contents tfile))
      (gpg-buffer-hack)
      (set-buffer-file-coding-system last-coding-system-used)
      (setq buffer-undo-list nil)
      (set-buffer-modified-p nil))
     (t
      (switch-to-buffer (car (buffer-list)))
      (kill-buffer buf)))
    (if (file-exists-p tfile) (delete-file tfile))))

(defun gpg-setup-keymap ()
  "Set \\C-x\\C-s to 'gpg-save-buffer."
  (interactive)
  (let ((map (copy-keymap gpg-map)))
    (set-keymap-parent map (current-local-map))
    (use-local-map map)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Saving and encrypting
;;;

(defun gpg-save-buffer ()
  "Saves the buffer and encrypts the file. 
If the passphrase is cached in the buffer, it is used on encryption
automatically. Otherwise, type your passphrase twice.
See also 'gpg-find-file'."
  (interactive)
  (if (not (buffer-modified-p))
      (message "(No changes need to be saved)")
    (let* ((file (buffer-file-name))
	   (oldt (gpg-file-modified-time file))
	   (tfile (gpg-make-temp-file file))
	   (buf (current-buffer))
	   (process-connection-type t) ;; 'pty
	   pro newt)
      (write-region (point-min) (point-max) tfile nil 'no-msg)
      (setq pro (gpg-start-process gpg-process-encryption buf gpg-program
				   "-c" "--cipher-algo" gpg-cipher
				   "--yes" "--output" file tfile))
      (set-process-filter   pro 'gpg-filter)
      (set-process-sentinel pro 'gpg-sentinel)
      (setq gpg-rendezvous t)
      (while gpg-rendezvous
	(sit-for 0.1)
	(discard-input))
      (setq newt (gpg-file-modified-time file))
      (when (gpg-file-newer newt oldt)
	(set-buffer-modified-p nil)
	(set-visited-file-modtime)
	(message (format "Wrote %s with GnuPG" file)))
      (if (file-exists-p tfile) (delete-file tfile)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;; Filter and sentinel
;;;

(defun gpg-read-passwd (prompt &optional encrypt-p)
  (if (and gpg-cache-passphrase gpg-passphrase)
      (progn
	(sit-for 0.01) ;; Emacs 20.7 
	gpg-passphrase)
    (let ((pass (read-passwd prompt)))
      (if (and gpg-cache-passphrase (not encrypt-p))
	  (setq gpg-passphrase pass))
      pass)))

(defun gpg-filter (process string)
  (let* ((name (process-name process))
	 (regex (concat "^" (regexp-quote gpg-process-encryption)))
	 (encrypt-p (string-match regex name)))
    (set-buffer (process-buffer process))
    (cond
     ((string-match "invalid passphrase" string)
      (message "Passphrase mismatch!")
      (setq gpg-passphrase nil))
     ((string-match "bad key" string)
      (message "Passphrase is wrong!")
      (setq gpg-passphrase nil))
     ((string-match "Enter passphrase:" string)
      (process-send-string process (gpg-read-passwd "Passphrase: " encrypt-p))
      (process-send-string process "\n"))
     ((string-match "Repeat passphrase:" string)
      (process-send-string process (gpg-read-passwd "Passphrase again: "))
      (process-send-string process "\n")))))

(defun gpg-sentinel (process event)
  (set-buffer (process-buffer process))
  (setq gpg-rendezvous nil))

(provide 'gpg)

;;; Copyright Notice:

;; Copyright (C) 2003 Kazu Yamamoto
;; All rights reserved.

;; Redistribution and use in source and binary forms, with or without
;; modification, are permitted provided that the following conditions
;; are met:
;; 
;; 1. Redistributions of source code must retain the above copyright
;;    notice, this list of conditions and the following disclaimer.
;; 2. Redistributions in binary form must reproduce the above copyright
;;    notice, this list of conditions and the following disclaimer in the
;;    documentation and/or other materials provided with the distribution.
;; 3. Neither the name of the team nor the names of its contributors
;;    may be used to endorse or promote products derived from this software
;;    without specific prior written permission.
;; 
;; THIS SOFTWARE IS PROVIDED BY THE TEAM AND CONTRIBUTORS ``AS IS'' AND
;; ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE
;; IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR
;; PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE TEAM OR CONTRIBUTORS BE
;; LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR
;; CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF
;; SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR
;; BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY,
;; WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE
;; OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN
;; IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

;;; gpg.el ends here
