;;; dictionary-server-main.el --- Main dictionary server for SKK -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'dictionary-server-plain) ;; テキスト辞書用
(require 'dictionary-server-cdb)   ;; CDB辞書用
(require 'dictionary-server-db)    ;; DB辞書用

(defvar dictionary-server-host "127.0.0.1"
  "Host on which the dictionary server listens.")

(defvar dictionary-server-port 54000
  "Port on which the dictionary server listens.")

(defvar dictionary-server-process nil
  "Process for the dictionary server.")

(defvar dictionary-server-debug t
  "If non-nil, print debug messages for dictionary server.")

(defun dictionary-server-debug-message (fmt &rest args)
  (when dictionary-server-debug
    (apply #'message (concat "[DICTIONARY DEBUG] " fmt) args)))

;; ユーザー定義の辞書リスト: :file, :type (plain|cdb|db), :coding
(defvar skk-async-dictionaries
  `(
    ;; CDB辞書例
    (:file ,(expand-file-name "~/Notes/assets/ddskk/SKK-JISYO.L.utf8.cdb")
           :type cdb :coding utf-8)

    ;; DB辞書例 (SQLite)
    (:file ,(expand-file-name "~/Notes/assets/ddskk/SKK-JISYO.L.utf8.db")
           :type db :coding utf-8)

    ;; テキスト辞書例
    (:file ,(expand-file-name "~/.emacs.d/ddskk/SKK-MY-JISYO.utf8")
           :type plain :coding utf-8)
    )
  "A list of dictionary specs. Each element is a plist:
 (:file \"...\" :type plain|cdb|db :coding 'utf-8 )")

;; --------------------------------------------------
;; Dictionary server internal cache
;; --------------------------------------------------
(defvar dictionary-server-trie (make-hash-table :test 'equal)
  "In-memory cache (trie) for loaded dictionary entries.")
(defvar dictionary-server-miss-cache (make-hash-table :test 'equal)
  "Cache for queries that had no results.")
(defvar dictionary-server-ready nil
  "Non-nil if dictionary loading is complete.")


;; --------------------------------------------------
;; 1. Load all dictionaries (plain/cdb/db)
;; --------------------------------------------------
(defun dictionary-server-load-dictionaries-sync ()
  (clrhash dictionary-server-trie)
  (clrhash dictionary-server-miss-cache)
  (setq dictionary-server-ready nil)

  (dolist (dic skk-async-dictionaries)
    (let ((file   (plist-get dic :file))
          (type   (plist-get dic :type))
          (coding (plist-get dic :coding)))
      (cond
       ;; テキスト辞書
       ((eq type 'plain)
        (when (file-readable-p file)
          (dictionary-server-debug-message "Loading plain dictionary: %s" file)
          (dictionary-server-load-plain-file file (or coding 'utf-8) dictionary-server-trie)))

       ;; CDB辞書
       ((eq type 'cdb)
        (when (file-readable-p file)
          (dictionary-server-debug-message "Initializing cdb for %s" file)
          (dictionary-server-cdb-init file)))  ;; cdb-init

       ;; DB辞書
       ((eq type 'db)
        (when (file-readable-p file)
          (dictionary-server-debug-message "Opening DB for %s" file)
          (dictionary-server-db-open file))))))

  (setq dictionary-server-ready t)
  (dictionary-server-debug-message "All dictionaries loaded."))


;; --------------------------------------------------
;; 2. dictionary-server-lookup
;;    → type=cdb の場合は (dictionary-server-cdb-get file query)
;;    → type=db  の場合は (dictionary-server-db-get-all file query)
;;    → plain はすでに trie に展開済み
;; --------------------------------------------------
(defun dictionary-server-lookup (query)
  "Return a list of candidate strings for QUERY, or nil if not found."
  (unless dictionary-server-ready
    (dictionary-server-debug-message "Dictionaries not ready yet.")
    (return-from dictionary-server-lookup nil))

  ;; まず trie キャッシュを確認
  (let ((cached (gethash query dictionary-server-trie)))
    (if cached
        (and (listp cached) cached) ;; found in trie
      (let ((found nil))
        (dolist (dic skk-async-dictionaries)
          (when (null found)
            (let ((type (plist-get dic :type))
                  (file (plist-get dic :file))
                  (coding (or (plist-get dic :coding) 'utf-8)))
              (cond
               ((eq type 'cdb)
                ;; ddskkの cdb-get はキーに一致する1レコードのみ取得
                (let ((cdb-result (dictionary-server-cdb-get file (encode-coding-string query coding))))
                  (when cdb-result
                    ;; decode & split
                    (let ((cands (dictionary-server-cdb-merge (list cdb-result) coding)))
                      (setq found cands)
                      (puthash query cands dictionary-server-trie)))))
               ((eq type 'db)
                (let ((rows (dictionary-server-db-get-all file query)))
                  (when rows
                    (puthash query rows dictionary-server-trie)
                    (setq found rows))))
               ;; plain 辞書は既に読み込み済み
               (t nil)))))  ;; end of (dolist)
        (unless found
          (puthash query t dictionary-server-miss-cache))
        found))))


;; --------------------------------------------------
;; 3. dictionary-server-filter
;;    - "1<query>" => lookup, 返す
;;    - "0"        => close
;;    - "2"        => version
;;    - "3"        => hostinfo
;;    - else       => "4\n" (miss)
;; --------------------------------------------------
(defun dictionary-server-filter (proc string)
  (dictionary-server-debug-message "Dictionary server received: %s" string)
  (let ((line (string-trim string)))
    (cond
     ;; 0 => 終了
     ((string-prefix-p "0" line)
      (dictionary-server-debug-message "Command 0: close connection")
      (when (process-live-p proc)
        (delete-process proc)))

     ;; 1 => lookup
     ((string-prefix-p "1" line)
      (let ((query (string-trim (substring line 1))))
        (dictionary-server-debug-message "Dictionary query: %s" query)
        (let ((cands (dictionary-server-lookup query)))
          (if (and (listp cands) (> (length cands) 0))
              (process-send-string
               proc (concat "1/"
                            (mapconcat #'identity cands "/")
                            "/\n"))
            (process-send-string proc "4\n")))))

     ;; 2 => version
     ((string= "2" line)
      (dictionary-server-debug-message "Command 2: version")
      (process-send-string proc "dictionary-server 0.1\n"))

     ;; 3 => hostinfo
     ((string= "3" line)
      (dictionary-server-debug-message "Command 3: hostinfo")
      (process-send-string proc
                           (format "%s:%d\n" dictionary-server-host dictionary-server-port)))

     ;; else => miss
     (t
      (dictionary-server-debug-message "Unknown command: %s" line)
      (process-send-string proc "4\n")))))


;; --------------------------------------------------
;; 4. サーバー起動・停止
;; --------------------------------------------------
(defun dictionary-server-start ()
  "Start the dictionary server."
  (interactive)
  (dictionary-server-debug-message "Starting dictionary loading synchronously...")
  (dictionary-server-load-dictionaries-sync)

  (unless (process-live-p dictionary-server-process)
    (setq dictionary-server-process
          (make-network-process
           :name "dictionary-server"
           :server t
           :family 'ipv4
           :host dictionary-server-host
           :service dictionary-server-port
           :coding 'utf-8
           :noquery t
           :filter #'dictionary-server-filter))
    (message "Dictionary server started on %s:%d"
             dictionary-server-host dictionary-server-port)
    (dictionary-server-debug-message "Dictionary server started."))

   ;; CDB ファイル使用の場合はダミーキーで lookup して OS キャッシュをウォームアップする
   (run-at-time 1 nil
                (lambda ()
                  (dictionary-server-debug-message "Warming up CDB with a dummy key...")
                  ;; 例えば「あいうえお」とか適当なキーを引く
                  (dictionary-server-lookup "あいうえお")
                  (dictionary-server-debug-message "Warmup lookup done.")))
)


(defun dictionary-server-stop ()
  "Stop the dictionary server."
  (interactive)
  (when (process-live-p dictionary-server-process)
    (delete-process dictionary-server-process)
    (setq dictionary-server-process nil)
    (message "Dictionary server stopped.")
    (dictionary-server-debug-message "Dictionary server stopped."))

  ;; cdb-uninit
  (dolist (dic skk-async-dictionaries)
    (when (eq (plist-get dic :type) 'cdb)
      (dictionary-server-cdb-uninit (plist-get dic :file))))

  ;; DB close
  (dictionary-server-db-close-all))

(provide 'dictionary-server-main)
;;; dictionary-server-main.el ends here
