;;; skk-async-server.el --- SKK async server with online-aware fallback & timeout -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'async)
(require 'json)
(require 'url)
;; ある場合のみ（オンライン判定の橋渡しに使う）
(require 'net-utils nil 'noerror)
(require 'google-ime-server nil 'noerror)

;; 文字コード指定用変数
(defvar skk-async-server-coding-system 'utf-8
  "Coding system for SKK server communication with ddskk client.")

(defvar skk-async-google-ime-server-coding-system 'utf-8
  "Coding system for communication with Google IME server.")

(defvar skk-async-mecab-server-coding-system 'utf-8
  "Coding system for communication with MeCab server.")

;; タイムアウト秒数（全体予算）
(defvar skk-async-translate-timeout 1
  "Number of seconds to wait before giving up on a translation query.")

;; --- オンライン判定のラッパー -----------------------------------------
(defun skk-async-online-p (&optional fast)
  "オンラインなら t。`google-ime-online-p` または `net-utils-online-p` があれば利用。"
  (cond
   ((fboundp 'google-ime-online-p) (google-ime-online-p fast))
   ((featurep 'net-utils)          (net-utils-online-p fast))
   (t t))) ;; 何も無ければ安全側で online 扱い

;; レース使用の好み（オンライン・低遅延時に Google と MeCab を同時発火）
(defvar skk-async-prefer-race t
  "When non-nil, race Google and MeCab in parallel under good network conditions.")

;; --- 辞書サーバーへの接続設定 -----------------------------------------
(defvar skk-async-dictionary-server-host "localhost")
(defvar skk-async-dictionary-server-port 54000)
(defvar skk-async-dictionary-process nil)
(defvar skk-async-dictionary-callbacks '())
(defvar skk-async-dictionary-output "")

;; Google IMEサーバーの設定
(defvar skk-async-google-ime-server-host "localhost"
  "Host where the Google IME server is running.")
(defvar skk-async-google-ime-server-port 55000
  "Port where the Google IME server is running.")

;; 持続的な Google IME 接続
(defvar skk-async-google-process nil
  "Persistent connection to Google IME server.")
(defvar skk-async-google-callbacks '()
  "Queue of callbacks for queries sent to Google IME server.")
(defvar skk-async-google-output ""
  "Accumulated output from the persistent Google IME connection.")

;; MeCabサーバーの設定
(defvar skk-async-mecab-server-host "localhost"
  "Host where the MeCab server is running.")
(defvar skk-async-mecab-server-port 56000
  "Port where the MeCab server is running.")

;; 持続的な MeCab 接続
(defvar skk-async-mecab-process nil
  "Persistent connection to MeCab server.")
(defvar skk-async-mecab-callbacks '()
  "Queue of callbacks for queries sent to MeCab server.")
(defvar skk-async-mecab-output ""
  "Accumulated output from the persistent MeCab connection.")

;; 利用フラグ
(defvar skk-async-use-google-ime t
  "Non-nil means use the external Google IME server as a fallback.")
(defvar skk-async-use-mecab t
  "Non-nil means use the external MeCab server as a fallback.")

;; SKK辞書サーバー設定
(defvar skk-async-server-port 1178
  "Port for the SKK dictionary server.")
(defvar skk-async-server-process nil
  "Process for the SKK dictionary server.")

;; 辞書キャッシュ
(defvar skk-async-trie (make-hash-table :test 'equal)
  "Trie structure for faster dictionary lookups.")
(defvar skk-async-miss-cache (make-hash-table :test 'equal)
  "Cache for queries that had no results.")

;; デバッグ出力
(defvar skk-async-debug t
  "If non-nil, print debug messages for SKK async server.")

(defun skk-async-debug-message (format-string &rest args)
  (when skk-async-debug
    (apply #'message (concat "[SKK DEBUG] " format-string) args)))

;; ------------------------------------------------------------
;; 入力バッファ(コマンド毎)
;; ------------------------------------------------------------
(defvar skk-async-input-buffer (make-hash-table :test 'eq)
  "Process local input buffers. Key=process object, Value=string.")

(defun skk-async-get-input-buffer (proc)
  (or (gethash proc skk-async-input-buffer) ""))

(defun skk-async-set-input-buffer (proc str)
  (puthash proc str skk-async-input-buffer))

(defun skk-async-clear-input-buffer (proc)
  (remhash proc skk-async-input-buffer))

(defun skk-async-send-response (proc string)
  "Send STRING to PROC as UTF-8 (with trailing newline)."
  (when (process-live-p proc)
    (process-send-string proc (concat string "\n"))))

;; ------------------------------------------------------------
;; Dictionary persistent connection
;; ------------------------------------------------------------
(defun skk-async-connect-dictionary-server ()
  "Ensure a persistent connection to the dictionary server."
  (unless (and skk-async-dictionary-process (process-live-p skk-async-dictionary-process))
    (setq skk-async-dictionary-output "")
    (setq skk-async-dictionary-callbacks '())
    (setq skk-async-dictionary-process
          (make-network-process
           :name "skk-async-dictionary-persistent"
           :host skk-async-dictionary-server-host
           :service skk-async-dictionary-server-port
           :coding 'utf-8
           :noquery t
           :nowait t
           :filter #'skk-async-dictionary-filter
           :sentinel #'skk-async-dictionary-sentinel))
    (skk-async-debug-message "Persistent dictionary server connection established.")))

(defun skk-async-dictionary-filter (proc string)
  (setq skk-async-dictionary-output (concat skk-async-dictionary-output string))
  (when (string-match "\n" skk-async-dictionary-output)
    (let ((line (string-trim-right skk-async-dictionary-output)))
      (setq skk-async-dictionary-output "")
      (let ((cb (pop skk-async-dictionary-callbacks)))
        (when cb (funcall cb line))))))

(defun skk-async-dictionary-sentinel (_proc event)
  (when (string-match-p "failed\\|lost\\|broken" event)
    (setq skk-async-dictionary-process nil)))

(defun skk-async-query-dictionary-server (query callback)
  "Send QUERY to the dictionary server and call CALLBACK with the response."
  (skk-async-connect-dictionary-server)
  (push callback skk-async-dictionary-callbacks)
  (process-send-string skk-async-dictionary-process (concat "1" query "\n")))

;; ------------------------------------------------------------
;; Google persistent connection
;; ------------------------------------------------------------
(defun skk-async-connect-google ()
  "Ensure a persistent connection to the Google IME server is established."
  (unless (and skk-async-google-process (process-live-p skk-async-google-process))
    (setq skk-async-google-output "")
    (setq skk-async-google-callbacks '())
    (setq skk-async-google-process
          (make-network-process
           :name "skk-async-google-persistent"
           :host skk-async-google-ime-server-host
           :service skk-async-google-ime-server-port
           :coding skk-async-google-ime-server-coding-system
           :noquery t
           :nowait t
           :filter #'skk-async-google-filter
           :sentinel #'skk-async-google-sentinel))
    (skk-async-debug-message "Persistent Google IME connection established.")))

(defun skk-async-google-filter (_proc string)
  (setq skk-async-google-output (concat skk-async-google-output string))
  ;; Google IME サーバーは1クエリ1行レスポンス(末尾に\n)の想定
  (when (string-match "\n" skk-async-google-output)
    (let ((line (string-trim-right skk-async-google-output)))
      (setq skk-async-google-output "")
      (let ((cb (pop skk-async-google-callbacks)))
        (when cb (funcall cb line))))))

(defun skk-async-google-sentinel (_proc event)
  (when (string-match-p "failed\\|lost\\|broken" event)
    (setq skk-async-google-process nil)))

(defun skk-async-query-google-ime-server (query callback)
  "Send QUERY to the persistent Google IME server and call CALLBACK with the response."
  (skk-async-connect-google)
  (push callback skk-async-google-callbacks)
  ;; プロトコル: "1<yomi> <LF>"
  (process-send-string skk-async-google-process (format "1%s \n" query)))

;; ------------------------------------------------------------
;; MeCab persistent connection
;; ------------------------------------------------------------
(defun skk-async-connect-mecab ()
  "Ensure a persistent connection to the MeCab server is established."
  (unless (and skk-async-mecab-process (process-live-p skk-async-mecab-process))
    (setq skk-async-mecab-output "")
    (setq skk-async-mecab-callbacks '())
    (setq skk-async-mecab-process
          (make-network-process
           :name "skk-async-mecab-persistent"
           :host skk-async-mecab-server-host
           :service skk-async-mecab-server-port
           :coding skk-async-mecab-server-coding-system
           :noquery t
           :nowait t
           :filter #'skk-async-mecab-filter
           :sentinel #'skk-async-mecab-sentinel))
    (skk-async-debug-message "Persistent MeCab connection established.")))

(defun skk-async-mecab-filter (_proc string)
  (setq skk-async-mecab-output (concat skk-async-mecab-output string))
  ;; MeCab サーバーは1クエリ1行(末尾に\n)の想定
  (when (string-match "\n" skk-async-mecab-output)
    (let ((line (string-trim-right skk-async-mecab-output)))
      (setq skk-async-mecab-output "")
      (let ((cb (pop skk-async-mecab-callbacks)))
        (when cb (funcall cb line))))))

(defun skk-async-mecab-sentinel (_proc event)
  (when (string-match-p "failed\\|lost\\|broken" event)
    (setq skk-async-mecab-process nil)))

(defun skk-async-query-mecab-server (query callback)
  "Send QUERY to the persistent MeCab server and call CALLBACK with the response."
  (skk-async-connect-mecab)
  (push callback skk-async-mecab-callbacks)
  (process-send-string skk-async-mecab-process (format "1%s \n" query)))

;; ------------------------------------------------------------
;; Fallback helpers
;; ------------------------------------------------------------
(defun skk-async-try-mecab (query callback)
  (skk-async-query-mecab-server
   query
   (lambda (mecab-response)
     (if (and mecab-response (string-prefix-p "1/" mecab-response))
         (funcall callback mecab-response)
       (puthash query t skk-async-miss-cache)
       (funcall callback "4\n")))))

(defun skk-async-try-google-ime-then-mecab (query callback)
  "オンライン時を前提に Google→MeCab の順に試す。"
  (skk-async-query-google-ime-server
   query
   (lambda (google-response)
     (if (and google-response (string-prefix-p "1/" google-response))
         (funcall callback google-response)
       (if skk-async-use-mecab
           (skk-async-try-mecab query callback)
         (puthash query t skk-async-miss-cache)
         (funcall callback "4\n"))))))

(defun skk-async-try-google&mecab-race (query callback)
  "Google と MeCab を並列に投げ、先着を採用。"
  (let ((done nil)
        (finish (lambda (res)
                  (unless done
                    (setq done t)
                    (funcall callback res)))))
    (when skk-async-use-google-ime
      (skk-async-query-google-ime-server
       query
       (lambda (r) (when (and r (string-prefix-p "1/" r)) (funcall finish r)))))
    (when skk-async-use-mecab
      (skk-async-query-mecab-server
       query
       (lambda (r) (when (and r (string-prefix-p "1/" r)) (funcall finish r)))))
    (run-at-time skk-async-translate-timeout nil (lambda () (funcall finish "4\n")))))

;; ------------------------------------------------------------
;; 翻訳処理 (辞書 → {オンライン: Google/MeCab | オフライン: MeCab} )
;; ------------------------------------------------------------
(defun skk-async-translate (query callback)
  (if (gethash query skk-async-miss-cache)
      (funcall callback "4\n")
    (let* ((found-result nil)
           (timed-out nil)
           (timer (run-at-time skk-async-translate-timeout nil
                               (lambda ()
                                 (unless (or timed-out found-result)
                                   (setq timed-out t)
                                   (puthash query t skk-async-miss-cache)
                                   (funcall callback "4\n"))))))
      ;; 1) まずローカル辞書
      (skk-async-query-dictionary-server
       query
       (lambda (dict-response)
         (when (and (not found-result) (not timed-out))
           (if (and dict-response (string-prefix-p "1/" dict-response))
               (progn
                 (setq found-result t)
                 (when timer (cancel-timer timer))
                 (funcall callback dict-response))
             ;; 2) 辞書ミス → ネット状態で分岐
             (let* ((online (skk-async-online-p 'fast))
                    ;; google-ime-server 側が「遅い」と判定しているときはレースを避ける
                    (net-slow (and (fboundp 'google-ime-network-slow-p)
                                   (google-ime-network-slow-p))))
               (cond
                ;; (A) オフライン → MeCab を最優先（Google は完全スキップ）
                ((or (not online) (not skk-async-use-google-ime))
                 (if skk-async-use-mecab
                     (skk-async-try-mecab
                      query
                      (lambda (res)
                        (unless (or found-result timed-out)
                          (setq found-result t)
                          (when timer (cancel-timer timer))
                          (funcall callback res))))
                   (puthash query t skk-async-miss-cache)
                   (when timer (cancel-timer timer))
                   (funcall callback "4\n")))
                ;; (B) オンライン & レース志向 & 低遅延っぽい → レース
                ((and online skk-async-prefer-race skk-async-use-google-ime skk-async-use-mecab (not net-slow))
                 (skk-async-try-google&mecab-race
                  query
                  (lambda (res)
                    (unless (or found-result timed-out)
                      (setq found-result t)
                      (when timer (cancel-timer timer))
                      (funcall callback res)))))
                ;; (C) オンライン（レースしない/遅い時）→ Google → MeCab
                (online
                 (skk-async-try-google-ime-then-mecab
                  query
                  (lambda (res)
                    (unless (or found-result timed-out)
                      (setq found-result t)
                      (when timer (cancel-timer timer))
                      (funcall callback res)))))
                ;; フォールバック無し
                (t
                 (puthash query t skk-async-miss-cache)
                 (when timer (cancel-timer timer))
                 (funcall callback "4\n")))))))))))

;; ------------------------------------------------------------
;; SKK サーバー I/O
;; ------------------------------------------------------------
(defun skk-async-server-filter (proc string)
  (let* ((old (skk-async-get-input-buffer proc))
         (new (concat old string)))
    (cond
     ((string-match "\n" new)
      (skk-async-clear-input-buffer proc)
      (let ((line (string-trim new)))
        (cond
         ((string-prefix-p "0" line)
          (when (process-live-p proc) (delete-process proc)))
         ((string-prefix-p "1" line)
          (let ((query (string-trim (substring line 1))))
            (skk-async-translate
             query
             (lambda (response) (skk-async-send-response proc response)))))
         ((string= "2" line)
          (skk-async-send-response proc "pyskkserv 0.1\n"))
         ((string= "3" line)
          (skk-async-send-response
           proc (format "%s:%d\n"
                        skk-async-google-ime-server-host
                        skk-async-google-ime-server-port)))
         (t
          (skk-async-send-response proc "4\n")))))
     ((and (string-prefix-p "0" new) (string= new "0"))
      (skk-async-clear-input-buffer proc)
      (when (process-live-p proc) (delete-process proc)))
     ;; "1<query><space>" で確定とみなす
     ((and (string-prefix-p "1" new) (string-suffix-p " " new))
      (skk-async-clear-input-buffer proc)
      (let* ((line (string-trim new))
             (query (string-trim (substring line 1))))
        (skk-async-translate
         query
         (lambda (response) (skk-async-send-response proc response)))))
     (t
      (skk-async-set-input-buffer proc new)))))

;; ------------------------------------------------------------
;; 起動/停止 と オンライン変化への自動追従
;; ------------------------------------------------------------
(defun skk-async--apply-online-state (online)
  "オンライン状態 ONLINE に応じて Google の ON/OFF と接続を管理。"
  (setq skk-async-use-google-ime (and skk-async-use-google-ime online))
  (if (and online skk-async-use-google-ime)
      (skk-async-connect-google)
    ;; オフライン時はプロセスを畳む（任意）
    (when (process-live-p skk-async-google-process)
      (delete-process skk-async-google-process))
    (setq skk-async-google-process nil))
  (message "SKK: Google IME %s" (if (and online skk-async-use-google-ime) "ENABLED" "DISABLED")))

(with-eval-after-load 'google-ime-server
  ;; google-ime-server 側のオンラインフックがあれば追従
  (add-hook 'google-ime-online-change-hook #'skk-async--apply-online-state))

(defun skk-async-server-start ()
  "Start the SKK dictionary server asynchronously."
  (interactive)
  (async-start
   `(lambda ()
      (setq load-path ',load-path)
      (add-to-list 'load-path ,(expand-file-name "~/.emacs.d/custom-lisp/"))
      (require 'cdb)
      (require 'dictionary-server-main)
      (require 'skk-async-server)
      t)
   (lambda (_result)
     ;; サーバ本体を立ち上げ
     (unless (process-live-p skk-async-server-process)
       (setq skk-async-server-process
             (make-network-process
              :name "skk-async-server"
              :server t
              :family 'ipv4
              :service skk-async-server-port
              :coding 'utf-8
              :noquery t
              :filter #'skk-async-server-filter))
       (when (process-live-p skk-async-server-process)
         (add-to-list 'skkserv-process-alist
                      (list "localhost" skk-async-server-port skk-async-server-process)
                      t))
       (message "SKK dictionary server started on port %d" skk-async-server-port))

     ;; MeCab は常に用意（使うなら即時）
     (when skk-async-use-mecab
       (skk-async-connect-mecab))

     ;; 起動時点のオンライン状態で Google を自動 ON/OFF
     (skk-async--apply-online-state (skk-async-online-p 'fast)))))

(defun skk-async-server-stop ()
  "Stop the SKK dictionary server."
  (interactive)
  (when (process-live-p skk-async-server-process)
    (delete-process skk-async-server-process)
    (setq skk-async-server-process nil)
    (message "SKK dictionary server stopped."))
  ;; Google / MeCab 側も畳む
  (when (process-live-p skk-async-google-process)
    (delete-process skk-async-google-process))
  (setq skk-async-google-process nil)
  (when (process-live-p skk-async-mecab-process)
    (delete-process skk-async-mecab-process))
  (setq skk-async-mecab-process nil))

(defun skk-async-server-stop-and-save ()
  "Stop the server and save the user dictionary."
  (interactive)
  (skk-async-server-stop)
  (message "User dictionary saved."))

(add-hook 'kill-emacs-hook #'skk-async-server-stop-and-save)

(provide 'skk-async-server)
;;; skk-async-server.el ends here
