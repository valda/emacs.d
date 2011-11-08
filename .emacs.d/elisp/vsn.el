(defvar vsn-open-exec "c:/bin/vsn-open.vbs"
  "VC内蔵エディタでライン指定表示、チェックをしてくれる実行ファイル")

(defvar vsn-open-wait "200"
  "VC内蔵エディタで表示してから、指定行にジャンプするコマンド開始までの時間(ms)")

(defvar vsn-open-type "VSNET"
  "ファイルを開く、Visual Cのバージョン指定[VS6|VSNET|VSNET2003]")

(defvar vsn-open-key "%g" ;; ←M-g、（or C-g: "^g"）
  "Visual Cのエディタで行番号ジャンプに割り当てているキーの設定")


;; スクリプトの引数設定
(define-process-argument-editing
  "/Wscript\\.exe\\'"
  (lambda (x) (general-process-argument-editing-function x nil t t nil t t)))


(defun vsn-line-open-buffer()
  "Type '\\[vsn-line-open-buffer]': The line of the current file is opened by Visual Studio.NET."
  (interactive)
  (let (
	(fname (concat "\\\"" (buffer-file-name) "\\\""))
	(fpoint (format "%d" (count-lines (point-min) (point))))
	(coding-system-for-read 'binary)
	(coding-system-for-write 'binary))
    (call-process "Wscript.exe" nil 0 nil vsn-open-exec  fname fpoint vsn-open-type vsn-open-wait vsn-open-key)
    (message "vsn-opened %s" fname)))



;;; Dired からVCエディタを開く関数
(defun vsn-dired-open-buffer()
  "Type '\\[vsn-line-open-buffer]': The file chosen by Dired is opened by Visual Studio.NET."
  (interactive)
  (if (eq major-mode 'dired-mode)
      (let ((fname (dired-get-filename))
	    (coding-system-for-read 'binary)
	    (coding-system-for-write 'binary))
	(call-process "Wscript.exe" nil 0 nil vsn-open-exec  fname 0 vsn-open-type vsn-open-wait vsn-open-key)
	(message "vsn-opened %s" fname))))


(provide 'vsn)
