;;; dictionary-server-db.el --- DB (SQLite) dictionary handling for SKK -*- lexical-binding: t; -*-
(require 'cl-lib)
(require 'emacsql)
(require 'emacsql-sqlite)

(defvar dictionary-server--db-connections (make-hash-table :test 'equal)
  "Hashtable of DB-PATH => emacsql connection.")

(defun dictionary-server-db-open (db-path)
  "Open (or reuse) an emacsql-sqlite connection for DB-PATH."
  (let ((abs (expand-file-name db-path)))
    (or (gethash abs dictionary-server--db-connections)
        (let ((conn (emacsql-sqlite abs)))
          (puthash abs conn dictionary-server--db-connections)
          conn))))

(defun dictionary-server-db-close-all ()
  "Close all DB connections."
  (maphash
   (lambda (_path conn)
     (ignore-errors (emacsql-close conn)))
   dictionary-server--db-connections)
  (clrhash dictionary-server--db-connections))

(defun dictionary-server-db-get-all (db-path query)
  "Return a list of candidate strings from DB-PATH for the given QUERY.
We assume a schema like:
  CREATE TABLE dictionary (yomi TEXT, candidate TEXT)"
  (let* ((conn (dictionary-server-db-open db-path))
         (rows (emacsql conn
                        "SELECT candidate FROM dictionary WHERE yomi = ?"
                        (list query))))
    (mapcar #'car rows)))

(provide 'dictionary-server-db)
;;; dictionary-server-db.el ends here
