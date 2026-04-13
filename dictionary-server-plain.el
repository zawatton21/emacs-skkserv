;;; dictionary-server-plain.el --- Plain dictionary loader for SKK -*- lexical-binding: t; -*-

(require 'cl-lib)

(defun dictionary-server-load-plain-file (file coding trie-table)
  "Load a plain text dictionary FILE (encoded in CODING) into the hash table TRIE-TABLE.
Each line is '読み <tab> /候補1/候補2/...', where ';注釈' は削除。"
  (with-temp-buffer
    (let ((coding-system-for-read coding))
      (insert-file-contents file))
    (goto-char (point-min))

    (while (not (eobp))
      (let ((line (buffer-substring-no-properties
                   (line-beginning-position) (line-end-position))))
        (forward-line 1)
        (when (string-match "^\\([^ \t]+\\)[ \t]+\\(/.*\\)$" line)
          (let ((yomi (match-string 1 line))
                (rest (match-string 2 line)))
            (when (string-prefix-p "/" rest)
              (setq rest (substring rest 1)))
            (let ((parts (split-string rest "/" t))
                  (candidates '()))
              (dolist (p parts)
                ;; ";注釈" を除去
                (setq p (replace-regexp-in-string ";.*" "" p))
                (unless (string-empty-p p)
                  (push p candidates)))
              (setq candidates (nreverse candidates))

              (when candidates
                (let ((existing (gethash yomi trie-table)))
                  (if existing
                      (puthash yomi (append existing candidates) trie-table)
                    (puthash yomi candidates trie-table))))))))))
  ;; No return value needed
  )

(provide 'dictionary-server-plain)
;;; dictionary-server-plain.el ends here
