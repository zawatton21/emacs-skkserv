;;; google-ime-server.el --- Google IME server for SKK -*- lexical-binding: t; -*-

(require 'cl-lib)
(require 'json)
(require 'url)
(require 'url-util)
;; net-utils は任意。無ければオンライン扱いのフォールバックで動く
(require 'net-utils nil 'noerror)

(defvar google-ime-server-host "127.0.0.1"
  "Host on which the Google IME server listens.")

(defvar google-ime-server-port 55000
  "Port on which the Google IME server listens.")

(defvar google-ime-debug t
  "If non-nil, print debug messages for Google IME server.")

(defvar google-ime-server-coding-system 'utf-8
  "Coding system for communication with SKK and Google IME server.")

(defun google-ime-debug-message (format-string &rest args)
  "Print debug messages if `google-ime-debug` is non-nil."
  (when google-ime-debug
    (apply #'message (concat "[GoogleIME DEBUG] " format-string) args)))


;;; ------------------------------------------------------------
;;; チューニング項目（好みに合わせて調整）
;;; ------------------------------------------------------------

;; 短い読み/連打のサーバー呼び過ぎ抑制
(defvar google-ime-min-yomi-len 3
  "Google へ問い合わせる最小読み長。これ未満は問い合わせしない。")
(defvar google-ime-min-interval-ms 250
  "同系統連続クエリの最小間隔（ms）。")
(defvar google-ime-request-budget-ms 300
  "1 リクエストに割く目安時間（ms）。動的 RTT と合わせてタイムアウトを決める。")

;; ネット品質観測 & サーキットブレーカー
(defvar google-ime-ewma-rtt-ms 120.0
  "Google IME の EWMA RTT 推定値（ms）。")
(defvar google-ime-ewma-alpha 0.25
  "EWMA 係数。大きいほど最新 RTT を重視。")
(defvar google-ime-consec-timeouts 0
  "Google への連続タイムアウト回数。")
(defvar google-ime-circuit-open-until 0
  "この epoch(ms) までは Google を停止する（サーキットオープン）。")
(defvar google-ime-circuit-base-sec 5
  "サーキットの基準休止秒。指数バックオフの基底。")
(defvar google-ime-circuit-exp 0
  "指数バックオフの指数（最大 6 程度まで上げる）。")

;; キャッシュ（永続化）
(defvar google-ime-cache (make-hash-table :test 'equal)
  "読み→候補のメモリキャッシュ。")
(defvar google-ime-cache-file (expand-file-name "~/.emacs.d/.google-ime-cache.el")
  "候補キャッシュの保存先。")

;; オンライン状態のブリッジ用フック
(defvar google-ime-online-change-hook nil
  "オンライン状態が変化した時に呼ばれるフック。引数は ONLINE(t/nil)。")


;;; ------------------------------------------------------------
;;; ユーティリティ
;;; ------------------------------------------------------------

(defun google-ime--now-ms () (floor (* 1000 (float-time))))

(defvar google-ime--last-query-time 0)
(defvar google-ime--last-query "")

(defun google-ime--throttle-p (query)
  "短い読み/短時間の連打/接頭辞拡張のとき t。"
  (or (< (length query) google-ime-min-yomi-len)
      (let* ((now (google-ime--now-ms))
             (dt  (- now google-ime--last-query-time)))
        (and (< dt google-ime-min-interval-ms)
             (string-prefix-p google-ime--last-query query)))))

(defun google-ime--mark-queried (query)
  (setq google-ime--last-query-time (google-ime--now-ms)
        google-ime--last-query      query))

(defun google-ime--circuit-open-p ()
  "サーキットオープン（停止中）なら t。"
  (> google-ime-circuit-open-until (google-ime--now-ms)))

(defun google-ime--open-circuit ()
  "Google を一定時間自動停止（指数バックオフ）。"
  (setq google-ime-circuit-exp (min 6 (1+ google-ime-circuit-exp)))
  (let* ((sec (* google-ime-circuit-base-sec (expt 2 google-ime-circuit-exp)))
         (until (+ (google-ime--now-ms) (* 1000 sec))))
    (setq google-ime-circuit-open-until until)
    (google-ime-debug-message "open circuit for %ds" sec)))

(defun google-ime--close-circuit ()
  (setq google-ime-circuit-open-until 0
        google-ime-consec-timeouts 0
        google-ime-circuit-exp 0))

(defun google-ime--rtt-update (elapsed-ms successp)
  "RTT EWMA 更新とサーキット制御。"
  (setq google-ime-ewma-rtt-ms
        (+ (* (- 1 google-ime-ewma-alpha) google-ime-ewma-rtt-ms)
           (* google-ime-ewma-alpha (float elapsed-ms))))
  (if successp
      (google-ime--close-circuit)
    (cl-incf google-ime-consec-timeouts)
    (when (>= google-ime-consec-timeouts 2)
      (google-ime--open-circuit))))

(defun google-ime-network-slow-p ()
  "体感的に遅いと判断する簡易基準。"
  (or (google-ime--circuit-open-p)
      (> google-ime-ewma-rtt-ms 400.0)))

(defun google-ime-online-p (&optional fast)
  "オンラインなら t。`net-utils` が無ければ常に t を返す（安全側）。"
  (if (featurep 'net-utils)
      (net-utils-online-p fast)
    t))

(defun google-ime-cache-load ()
  (when (file-readable-p google-ime-cache-file)
    (condition-case _e
        (with-temp-buffer
          (insert-file-contents google-ime-cache-file)
          (setq google-ime-cache (read (current-buffer))))
      (error (setq google-ime-cache (make-hash-table :test 'equal))))))

(defun google-ime-cache-save ()
  (condition-case _e
      (with-temp-file google-ime-cache-file
        (prin1 google-ime-cache (current-buffer)))
    (error nil)))

(defun google-ime--start-online-bridge ()
  "net-utils の状態変化をこのサーバーにも伝播。"
  (when (featurep 'net-utils)
    (add-hook 'net-utils-online-change-hook
              (lambda (online)
                (run-hook-with-args 'google-ime-online-change-hook online)
                (google-ime-debug-message "Google IME: %s" (if online "online" "offline"))))))


;;; ------------------------------------------------------------
;;; Google IME 取得本体（最適化入り）
;;; ------------------------------------------------------------

(defun google-ime-fetch-candidates (query)
  "Fetch candidates from Google IME API for QUERY.
Return a list of candidates or nil if none."
  ;; 1) キャッシュ命中なら即返し
  (let ((hit (gethash query google-ime-cache)))
    (when hit (cl-return-from google-ime-fetch-candidates hit)))

  ;; 2) デバウンス/短い読みは問い合わせしない
  (when (google-ime--throttle-p query)
    (google-ime-debug-message "throttled: %s" query)
    (cl-return-from google-ime-fetch-candidates nil))

  ;; 3) オフライン or サーキット開 → 即ミス
  (unless (google-ime-online-p 'fast)
    (google-ime-debug-message "offline: %s" query)
    (cl-return-from google-ime-fetch-candidates nil))
  (when (google-ime--circuit-open-p)
    (google-ime-debug-message "circuit-open: %s" query)
    (cl-return-from google-ime-fetch-candidates nil))

  ;; 4) タイムアウトを RTT と予算から動的決定
  (let* ((budget google-ime-request-budget-ms)
         (pad    (max 80 (floor (* 0.5 google-ime-ewma-rtt-ms))))
         (timeout (max 0.15 (/ (float (+ pad (min budget 800))) 1000.0))))

    (let* ((base-url "http://www.google.com/transliterate")
           ;; url-encode-url は URL 全体前提なので、テキスト部は hexify が安全
           (params (format "?langpair=ja-Hira|ja&text=%s," (url-hexify-string query)))
           (url (concat base-url params))
           (t0  (google-ime--now-ms))
           (buf (url-retrieve-synchronously url t t timeout)))
      (unless buf
        (google-ime--rtt-update (* 1000 timeout) nil)
        (google-ime-debug-message "timeout: %.0fms for %s" (* 1000 timeout) query)
        (cl-return-from google-ime-fetch-candidates nil))
      (unwind-protect
          (with-current-buffer buf
            (goto-char (point-min))
            ;; ヘッダ終了
            (unless (search-forward "\n\n" nil t)
              (google-ime--rtt-update (- (google-ime--now-ms) t0) nil)
              (google-ime-debug-message "no-body: %s" query)
              (cl-return-from google-ime-fetch-candidates nil))
            (let* ((json-str (buffer-substring-no-properties (point) (point-max)))
                   (json-str (replace-regexp-in-string "\n" "" json-str))
                   (json-str (replace-regexp-in-string ",]" "]" json-str))
                   (json-array-type 'list)
                   (json-object-type 'alist)
                   (res (ignore-errors (json-read-from-string json-str))))
              (google-ime--rtt-update (- (google-ime--now-ms) t0) t)
              (when (and (listp res) (> (length res) 0))
                ;; [[ "query", [ "候補1", "候補2", ... ]]]
                (let ((cands (nth 1 (car res))))
                  (when (and (listp cands) (> (length cands) 0))
                    (puthash query cands google-ime-cache)
                    (google-ime--mark-queried query)
                    (cl-return-from google-ime-fetch-candidates cands))))))

        (kill-buffer buf))))

  ;; 失敗（候補なし/解析不可）
  nil)


;;; ------------------------------------------------------------
;;; サーバー（プロトコル: "1<yomi><space>" -> "1/<cand1>/<cand2>/\n" or "4\n"）
;;; ------------------------------------------------------------

(defvar google-ime-server-process nil
  "Process for the Google IME server.")

(defun google-ime-server-filter (proc string)
  "Filter for incoming requests to Google IME server."
  (condition-case err
      (progn
        (google-ime-debug-message "recv: %s" string)
        ;; 改行のみ削除（末尾スペースは保持）
        (let ((str (replace-regexp-in-string "\n$" "" string)))
          ;; 入力例: \"1のうしゃして \" → 先頭 '1' + 読み + スペース
          (when (and (>= (length str) 2)
                     (equal (substring str 0 1) "1"))
            (let ((space-pos (string-match " " str)))
              (if (not space-pos)
                  (progn
                    (google-ime-debug-message "no-space: %s" str)
                    (process-send-string proc "4\n"))
                (let* ((query (substring str 1 space-pos))
                       (candidates (google-ime-fetch-candidates query)))
                  (if (and candidates (> (length candidates) 0))
                      (progn
                        (google-ime-debug-message "hit %s -> %s" query candidates)
                        (process-send-string
                         proc (concat "1/" (mapconcat #'identity candidates "/") "/\n")))
                    (google-ime-debug-message "miss %s" query)
                    (process-send-string proc "4\n")))))))
    (error
     (google-ime-debug-message "Error in google-ime-server-filter: %s" err)
     (process-send-string proc "4\n")))))

(defun google-ime-server-start ()
  "Start the Google IME server."
  (interactive)
  (condition-case err
      (progn
        ;; キャッシュのロードとオンライン監視の橋渡し
        (google-ime-cache-load)
        (google-ime--start-online-bridge)

        (unless (process-live-p google-ime-server-process)
          (setq google-ime-server-process
                (make-network-process
                 :name "google-ime-server"
                 :server t
                 :service google-ime-server-port
                 :host google-ime-server-host
                 :coding google-ime-server-coding-system
                 :filter #'google-ime-server-filter
                 :noquery t))
          (message "Google IME server started on %s:%d"
                   google-ime-server-host google-ime-server-port)
          (google-ime-debug-message "Google IME server started.")))

    (error
     (message "Failed to start Google IME server: %s" err))))

(defun google-ime-server-stop ()
  "Stop the Google IME server."
  (interactive)
  (when (process-live-p google-ime-server-process)
    (delete-process google-ime-server-process)
    (setq google-ime-server-process nil)
    (message "Google IME server stopped")
    (google-ime-debug-message "Google IME server stopped."))
  ;; 終了時にキャッシュ保存
  (google-ime-cache-save))

;; Emacs 終了時にもキャッシュ永続化
(add-hook 'kill-emacs-hook #'google-ime-cache-save)

(provide 'google-ime-server)
;;; google-ime-server.el ends here
