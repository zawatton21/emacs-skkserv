;;; dictionary-server-cdb.el --- CDB dictionary handling for SKK -*- lexical-binding: t; -*-
(require 'cdb)
(require 'cl-lib)

(defun dictionary-server-cdb-init (path)
  "Initialize a cdb reader associated with PATH using ddskk's cdb.el.
This essentially calls (cdb-init path)."
  (cdb-init path))

(defun dictionary-server-cdb-uninit (path)
  "Uninitialize the cdb reader associated with PATH."
  (cdb-uninit path))

(defun dictionary-server-cdb-get (path key)
  "Return the value string for KEY in the cdb at PATH, or nil if not found.
ddskk's `cdb-get` returns at most one record."
  (cdb-get path key))

(defun dictionary-server-cdb-merge (values coding)
  "Given a list of VALUES (strings) from cdb-get, decode them as CODING,
split by '/', remove ';注釈', and return a list of candidates.
Since ddskk cdb-get returns at most one string, VALUES typically has 0 or 1 element."
  (let ((merged ""))
    (dolist (val values)
      (let ((decoded (decode-coding-string val coding)))
        (when (string-prefix-p "/" decoded)
          (setq decoded (substring decoded 1)))
        (setq merged (concat merged "/" decoded))))
    (when (string-prefix-p "/" merged)
      (setq merged (substring merged 1)))

    (let ((parts (split-string merged "/" t))
          (final-cands '()))
      (dolist (p parts)
        (setq p (replace-regexp-in-string ";.*" "" p))
        (unless (string-empty-p p)
          (push p final-cands)))
      (nreverse final-cands))))

(provide 'dictionary-server-cdb)
;;; dictionary-server-cdb.el ends here
