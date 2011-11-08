;;; gesture.el --- A gesture minor mode.

;; Copyright (C) 2003 Yuuichi Teranishi <teranisi@gohome.org>

;; Author: Yuuichi Teranishi <teranisi@gohome.org>

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; see the file COPYING.  If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:
;;
;; Only tested on Emacs 21.3.50.
;;
;;; History:
;;
;; Created on Dec 4 2003.

;;; Code:

(defgroup gesture nil
  "Gesture minor mode to recognize gestures."
  :group 'gesture)

(defcustom gesture-mode nil
  "If non-nil, gesture is recognized.
This variable is buffer-local."
  :type 'boolean
  :group 'gesture)
(make-variable-buffer-local 'gesture-mode)

(defcustom gesture-mode-string " Gesture"
  "String to display in mode line when Gesture Mode is enabled."
  :type 'string
  :group 'gesture)

(defcustom gesture-mouse-button 'mouse-3
  "The mouse button to activate gestures."
  :type '(choice (const :tag "Mouse 1" mouse-1)
		 (const :tag "Mouse 2" mouse-2)
		 (const :tag "Mouse 3" mouse-3))
  :group 'gesture)

(defvar gesture-mode-map (make-sparse-keymap))
(define-key gesture-mode-map
  `[,(intern (concat "drag-"
		     (symbol-name gesture-mouse-button)))] 'gesture-drag-mouse)
(define-key gesture-mode-map
  `[,(intern (concat "down-"
		     (symbol-name gesture-mouse-button)))] 'gesture-down-mouse)

(cond ((fboundp 'add-minor-mode)
       (add-minor-mode 'gesture-mode 'gesture-mode-string gesture-mode-map))
      ((assq 'gesture-mode (default-value 'minor-mode-alist)))
      (t
       (setq-default minor-mode-alist
		     (append (default-value 'minor-mode-alist)
			     '((gesture-mode gesture-mode-string))))
       (setq-default minor-mode-map-alist
		     (append (default-value 'minor-mode-map-alist)
			     '((gesture-mode . gesture-mode-map))))))

(defvar gesture-action-alist nil
  "An internal variable to store major mode, gesture and action.")
(defvar gesture-motion-function-alist nil
  "An internal variable to store the gesture function with `motion' type.")
(defcustom gesture-pre-function-alist nil
  "An internal variable to store the gestre function with `pre' type.")
(defcustom gesture-after-function-alist nil
  "An internal variable to store the gesture function with `after' type.")

(defun turn-on-gesture-mode ()
  "Unequivocally turn on the gesture mode."
  (interactive)
  (gesture-mode 1))

(easy-mmode-define-global-mode
 global-gesture-mode gesture-mode
 turn-on-gesture-mode)

(defun gesture-mode (&optional arg)
  "Gesture mode.
If ARG is non-nil, inverse the current variable `gesture-mode'."
  (interactive "P")
  (let* ((on (if arg
		 (> (prefix-numeric-value arg) 0)
	       (not gesture-mode))))
    (setq gesture-mode on)))

(defvar gesture-mouse-motions-active nil)

(defun gesture-activate-mouse-motions (activatep)
  "Activate/deactivate mouse motion events for the current buffer.
ACTIVATEP non-nil means activate mouse motion events."
  (if activatep
      (progn
	(make-local-variable 'gesture-mouse-motions-active)
	(setq gesture-mouse-motions-active t)
	(make-local-variable 'track-mouse)
	(setq track-mouse t))
    (when gesture-mouse-motions-active
      (kill-local-variable 'gesture-mouse-motions-active)
      (kill-local-variable 'track-mouse))))

(defvar gesture-position-list nil)
(defvar gesture-list nil)
(make-variable-buffer-local 'gesture-position-list)
(make-variable-buffer-local 'gesture-list)

(defun gesture-distance-in-pixel-p (pos1 pos2 pixel)
  "Returen non-nil when distance from POS1 to POS2 is in PIXEL."
  (< (+ (* (- (car pos1) (car pos2))
	   (- (car pos1) (car pos2)))
	(* (- (cdr pos1) (cdr pos2))
	   (- (cdr pos1) (cdr pos2))))
     (* pixel pixel)))

(defun gesture-mouse-motion (e)
  "Motion mouse function for event E."
  (interactive "e")
  (when (car (mouse-pixel-position))
    (if (and gesture-position-list
	     (not (gesture-distance-in-pixel-p
		   (nth 2 (event-start
			   (nth (- (length gesture-position-list) 1)
				gesture-position-list)))
		   (nth 2 (event-start e))
		   10))) ; moved more than 10 pixels
	(let ((gesture (gesture-recognize
			(list (nth 2 (event-start
				      (nth (- (length gesture-position-list) 1)
					   gesture-position-list)))
			      (nth 2 (event-start e))))))
	  (unless (string= (car gesture-list) gesture)
	    (setq gesture-list (cons gesture gesture-list)))
	  (setq gesture-position-list nil))
      (let ((func (cdr (assq major-mode gesture-motion-function-alist))))
	(and func
	     (funcall func e (mapconcat 'identity (reverse gesture-list) ""))))
      (setq gesture-position-list (cons (copy-sequence e)
					gesture-position-list)))))

(defun gesture-down-mouse (e)
  "Down mouse function for event E."
  (interactive "e")
  (define-key gesture-mode-map [mouse-movement] 'gesture-mouse-motion)
  (gesture-activate-mouse-motions 'on))

(defun gesture-drag-mouse (e)
  "Drag mouse function for event E."
  (interactive "e")
  (define-key gesture-mode-map [mouse-movement] 'ignore)
  (gesture-activate-mouse-motions nil)
  (let* ((gesture (mapconcat 'identity (reverse gesture-list) ""))
	 (action (or (cdr (assoc
			   gesture
			   (cdr (assq major-mode gesture-action-alist))))
		     (cdr (assoc
			   gesture
			   (cdr (assq t gesture-action-alist))))))
	 (pfunc (cdr (assq major-mode gesture-pre-function-alist)))
	 (afunc (cdr (assq major-mode gesture-after-function-alist))))
    (gesture-log "%s: %s" gesture (or action "No action"))
    (setq gesture-position-list nil
	  gesture-list nil)
    (when action
      (condition-case err
	  (unwind-protect
	      (progn
		(and pfunc (funcall pfunc e gesture))
		(call-interactively action))
	    (and afunc (funcall afunc e gesture)))
	(error (gesture-log "error: %s" err))))))

(defun gesture-recognize (poslist)
  "Recognize gesture according to POSLIST."
  ;; Just use 0th and 1th
  (if (< (abs (- (car (nth 0 poslist)) (car (nth 1 poslist))))
	 (abs (- (cdr (nth 0 poslist)) (cdr (nth 1 poslist)))))
      (progn
	(if (< (cdr (nth 0 poslist))(cdr (nth 1 poslist)))
	    "D"
	  "U"))
    (if (< (car (nth 0 poslist))(car (nth 1 poslist)))
	"R"
      "L")))

(defun define-gesture (gesture mode action)
  "Define a GESTURE action in the MODE as ACTION.
GESTURE is a string which represents the cursor movement.
\"U\" means moving mouse cursor up.
\"D\" means moving mouse cursor down.
\"R\" means moving mouse cursor right.
\"L\" means moving mouse cursor left.
For example, \"DRLU\" means moving cursor Down-Right-Left-Up.
If MODE is t, the gesture is bound globally."
  (let ((alist (assq mode gesture-action-alist))
	pair)
    (if alist
	(if (setq pair (assoc gesture alist))
	    (setcdr pair action)
	  (setcdr alist (nconc (cdr alist) `((,gesture . ,action)))))
      (setq gesture-action-alist
	    (nconc gesture-action-alist `((,mode (,gesture . ,action))))))))

(defun global-set-gesture (gesture action)
  "Give GESTURE a global binding as ACTION."
  (define-gesture gesture t action))

(defun define-gesture-function (type mode function)
  "Define a gesture function.
TYPE is one of the following:
 motion - function is called every mouse motion event.
 pre    - function is called just before the gesture action.
 after  - function is called after the gesture action.
MODE is the symbol of `major-mode'.
FUNCTION is called with argument EVENT and GESTURE.
In the motion function, GESTURE may not be completed yet."
  (let* ((sym (intern (concat "gesture-"
			      (symbol-name type) "-function-alist")))
	 (pair (assq mode (symbol-value sym))))
    (when pair
      (set sym (delq pair (symbol-value sym))))
    (set sym (cons (cons mode function) (symbol-value sym)))))

(defvar gesture-log nil)
(defun gesture-log (&rest args)
  "Log the gesture.  ARGS are given for `format' function."
  (when gesture-log
    (with-current-buffer (get-buffer-create " *Gesture-Log*")
      (goto-char (point-max))
      (insert (apply 'format args) "\n"))))

;; Sample gesture
(global-set-gesture "D" 'list-buffers)

(provide 'gesture)

;;; gesture.el ends here
