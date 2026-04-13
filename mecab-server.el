;;; mecab-server.el --- MeCab-based SKK server process -*- lexical-binding: t; -*-

(require 'cl-lib)

(defvar mecab-server-host "127.0.0.1"
  "Host on which the MeCab server listens.")
(defvar mecab-server-port 56000
  "Port on which the MeCab server listens.")
(defvar mecab-server-process nil
  "Process for the MeCab server.")

(defvar mecab-server-cmd "C:/Program Files/MeCab/bin/mecab.exe"
  "Full path to the MeCab executable.")

(defvar mecab-server-dict-dir "C:/Program Files/MeCab/dic/ipadic"
  "Dictionary directory for MeCab. Adjust as needed.")

(defvar mecab-server-dict-encoding 'utf-8
  "Coding system used by MeCab dictionary (e.g. 'euc-jp).")

(defvar mecab-server-coding-system 'utf-8
  "Coding system for communication with SKK client (e.g. 'utf-8).")

(defvar mecab-server-cand-size 2
  "Maximum number of candidates to return.")
(defvar mecab-server-search-size 5
  "Number of N-best to generate.")

(defvar mecab-server-debug nil
  "If non-nil, print debug messages for MeCab server.")

;; MeCab 常駐プロセス用変数
(defvar mecab-server--mecab-process nil
  "The persistent MeCab process.")
(defvar mecab-server--mecab-output-buffer nil
  "Buffer to store MeCab process output.")
(defvar mecab-server--query-in-progress nil
  "Non-nil if we are currently waiting for a query result.")

(defun mecab-server-debug-message (format-string &rest args)
  (when mecab-server-debug
    (apply #'message (concat "[mecab-server DEBUG] " format-string) args)))

(defun mecab-server--start-mecab-process-if-needed ()
  "Start the persistent MeCab process if not already running."
  (unless (and mecab-server--mecab-process (process-live-p mecab-server--mecab-process))
    (when mecab-server--mecab-process
      (delete-process mecab-server--mecab-process))
    (setq mecab-server--mecab-output-buffer (get-buffer-create "*mecab-persistent*"))
    (with-current-buffer mecab-server--mecab-output-buffer
      (erase-buffer))
    (let ((args (list "-Oime"
                      "-l1"
                      (format "-N%d" mecab-server-search-size)
                      "-d" mecab-server-dict-dir)))
      (mecab-server-debug-message "Starting persistent MeCab process: %s %s" mecab-server-cmd args)
      (setq mecab-server--mecab-process
            (apply #'start-process "mecab-persistent" mecab-server--mecab-output-buffer mecab-server-cmd args)))
    (set-process-coding-system mecab-server--mecab-process mecab-server-dict-encoding mecab-server-dict-encoding)
    (set-process-filter mecab-server--mecab-process #'mecab-server--mecab-process-filter)
    (set-process-query-on-exit-flag mecab-server--mecab-process nil)))

(defun mecab-server--mecab-process-filter (proc string)
  "Process filter for the persistent MeCab process."
  (mecab-server-debug-message "MeCab process filter received: %s" string)
  (when (buffer-live-p mecab-server--mecab-output-buffer)
    (with-current-buffer mecab-server--mecab-output-buffer
      (goto-char (point-max))
      (insert string))))

(defun mecab-server--flush-mecab-output ()
  "Clear the MeCab output buffer."
  (when (buffer-live-p mecab-server--mecab-output-buffer)
    (with-current-buffer mecab-server--mecab-output-buffer
      (erase-buffer))))

(defun mecab-server--analyze-query (query)
  "Analyze QUERY using the persistent MeCab process."
  (let ((start-time (float-time (current-time)))) ;; 開始時刻取得
    (mecab-server-debug-message "Analyzing query (persistent): '%s'" query)
    (mecab-server--start-mecab-process-if-needed)
    (mecab-server--flush-mecab-output)

    (setq mecab-server--query-in-progress t)
    ;; MeCab へクエリを送信
    (let ((encoded-input (encode-coding-string (concat query "\n") mecab-server-dict-encoding)))
      (process-send-string mecab-server--mecab-process encoded-input))

    ;; EOS が来るまで待つ
    (mecab-server-debug-message "Waiting for MeCab output for query: %s" query)
    (let (candidates done)
      (with-current-buffer mecab-server--mecab-output-buffer
        (while (and (not done) (accept-process-output mecab-server--mecab-process 5))
          (goto-char (point-min))
          ;; 出力をパースする
          (when (search-forward "EOS" nil t)
            (setq done t))))

      (setq mecab-server--query-in-progress nil)
      ;; パースを実行
      (with-current-buffer mecab-server--mecab-output-buffer
        (let ((output (buffer-string)))
          (mecab-server-debug-message "Raw decoded output:\n%s" output)
          (setq candidates (mecab-server--parse-mecab-output output query))))

      (let ((elapsed (- (float-time (current-time)) start-time))) ;; 終了時刻との差分
        (mecab-server-debug-message "Query '%s' analyzed in %.3f seconds" query elapsed))
      candidates)))

(defun mecab-server--parse-mecab-output (output query)
  "Parse MeCab -Oime -N output from persistent process."
  (mecab-server-debug-message "Parsing MeCab output...")
  (let ((lines (split-string output "\n"))
        (candhash (make-hash-table :test 'equal))
        (cands nil))
    (mecab-server-debug-message "Output lines: %s" lines)
    (cl-loop for line in lines do
             (setq line (string-trim-right line))
             (mecab-server-debug-message "Processing line: '%s'" line)
             (cond
              ((string-empty-p line)
               ;; ignore empty line
               )
              ((string= line "EOS")
               ;; End of all candidates
               (cl-return cands))
              ((string-match-p "^lattice-level" line)
               ;; warning line, ignore
               )
              (t
               ;; candidate line
               (when (string-match "EOS$" line)
                 (setq line (replace-match "" t t line)))
               (mecab-server-debug-message "Candidate line after trimming: '%s'" line)
               (unless (gethash line candhash)
                 (puthash line t candhash)
                 (setq cands (append cands (list line)))
                 (when (>= (length cands) mecab-server-cand-size)
                   (mecab-server-debug-message "Reached candidate size limit %d" mecab-server-cand-size)
                   (cl-return cands))))))
    (mecab-server-debug-message "All lines processed. Final candidates: %s" cands)
    cands))

(defun mecab-server-handle-line (proc line)
  (mecab-server-debug-message "Received line from client: '%s'" line)
  (cond
   ((string-prefix-p "0" line)
    (mecab-server-debug-message "Command 0: Quit (no output)")
    (delete-process proc))

   ((string-prefix-p "1" line)
    (mecab-server-debug-message "Command 1: Translate query")
    (let* ((rest (substring line 1))
           (space-pos (string-match " " rest)))
      (mecab-server-debug-message "Rest after removing '1': '%s'" rest)
      (if (or (not space-pos) (= space-pos 0))
          (progn
            (mecab-server-debug-message "No valid query after '1', sending '4' + newline")
            (process-send-string proc "4\n"))
        (let ((query (substring rest 0 space-pos)))
          (mecab-server-debug-message "Extracted query: '%s'" query)
          (let ((candidates (mecab-server--analyze-query query)))
            (mecab-server-debug-message "Candidates for '%s': %s" query candidates)
            (if (and candidates (> (length candidates) 0))
                (progn
                  (mecab-server-debug-message "Sending candidates to client: %s" candidates)
                  (process-send-string proc
                                       (concat "1" (mapconcat (lambda (c) (concat "/" c)) candidates "") "/\n")))
              (mecab-server-debug-message "No candidates found for '%s', sending miss." query)
              (process-send-string proc (concat "4" query "\n"))))))))

   ((string-prefix-p "2" line)
    (mecab-server-debug-message "Command 2: Version request")
    (process-send-string proc "mecab-server-0.1"))

   ((string-prefix-p "3" line)
    (mecab-server-debug-message "Command 3: Host info request")
    (process-send-string proc "localhost:127.0.0.1: "))

   (t
    (mecab-server-debug-message "Unknown command: '%s', sending '4\\n'" line)
    (process-send-string proc "4\n"))))

(defun mecab-server-filter (proc string)
  (mecab-server-debug-message "Filter received data from proc=%s: %s" proc string)
  (let ((buf (gethash proc mecab-server--input-buffer)))
    (unless buf
      (mecab-server-debug-message "No buffer for proc=%s, creating." proc)
      (setq buf (generate-new-buffer (format "*mecab-server-%s*" proc)))
      (puthash proc buf mecab-server--input-buffer))
    (with-current-buffer buf
      (goto-char (point-max))
      (insert string)
      (goto-char (point-min))
      (while (search-forward "\n" nil t)
        (let ((line (buffer-substring (point-min) (1- (point)))))
          (delete-region (point-min) (point))
          (mecab-server-debug-message "Complete command line extracted: '%s'" line)
          (when (> (length line) 0)
            (mecab-server-handle-line proc line)))))))

(defun mecab-server-sentinel (proc event)
  (mecab-server-debug-message "Sentinel called for proc=%s, event=%s" proc event)
  (when (memq (process-status proc) '(closed failed exit))
    (mecab-server-debug-message "Process ended. Cleaning up.")
    (let ((buf (gethash proc mecab-server--input-buffer)))
      (when buf
        (kill-buffer buf)
        (remhash proc mecab-server--input-buffer)))))

(defun mecab-server-start ()
  "Start the MeCab-based SKK server."
  (interactive)
  (unless (process-live-p mecab-server-process)
    (setq mecab-server-process
          (make-network-process
           :name "mecab-server"
           :server t
           :family 'ipv4
           :service mecab-server-port
           :host mecab-server-host
           :coding mecab-server-coding-system
           :filter #'mecab-server-filter
           :sentinel #'mecab-server-sentinel
           :noquery t))
    (mecab-server-debug-message "MeCab server started on %s:%d with dict-encoding %s"
                                mecab-server-host mecab-server-port mecab-server-dict-encoding)))

(defun mecab-server-stop ()
  "Stop the MeCab server."
  (interactive)
  (when (process-live-p mecab-server-process)
    (mecab-server-debug-message "Stopping MeCab server...")
    (delete-process mecab-server-process)
    (setq mecab-server-process nil)
    (mecab-server-debug-message "MeCab server stopped."))
  (when (and mecab-server--mecab-process (process-live-p mecab-server--mecab-process))
    (delete-process mecab-server--mecab-process)
    (setq mecab-server--mecab-process nil)
    (mecab-server-debug-message "Persistent MeCab process stopped.")))

(defvar mecab-server--input-buffer (make-hash-table :test 'eq)
  "Hash table to store partial input per client process.")

(provide 'mecab-server)
;;; mecab-server.el ends here
