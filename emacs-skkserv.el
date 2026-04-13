;;; emacs-skkserv.el --- SKK dictionary server implemented in Emacs Lisp -*- lexical-binding: t; -*-

;; Author: Fujisawa Electric Management Office
;; URL: https://github.com/zawatton21/emacs-skkserv
;; Version: 0.1.0
;; Keywords: i18n, japanese, input-method
;; Package-Requires: ((emacs "27.1") (async "1.9"))

;;; Commentary:
;;
;; emacs-skkserv is a full skkserv protocol implementation in Emacs Lisp.
;; It listens on the standard skkserv port (default 1178) and serves
;; any SKK client — ddskk, CorvusSKK, ibus-skk, etc.
;;
;; Three conversion backends work together asynchronously:
;;
;; 1. Dictionary server — local SKK dictionary files (plain text, CDB,
;;    SQLite) with trie cache for fast lookup.
;; 2. Google IME server — Google transliteration API as fallback, with
;;    persistent disk cache for offline use.
;; 3. MeCab server — morphological analysis for kana-kanji conversion.
;;
;; All backends run as Emacs network processes with non-blocking I/O.
;; When online, Google IME and MeCab can race in parallel for the
;; fastest response.
;;
;; Package structure:
;;
;;   emacs-skkserv.el          — this file (entry point, autoloads)
;;   skk-async-server.el       — skkserv protocol handler
;;   dictionary-server-main.el — dictionary file server
;;   dictionary-server-plain.el — plain text dictionary backend
;;   dictionary-server-cdb.el  — CDB dictionary backend
;;   dictionary-server-db.el   — SQLite dictionary backend
;;   google-ime-server.el      — Google IME backend
;;   mecab-server.el           — MeCab backend
;;
;; Quick start:
;;
;;   (require 'emacs-skkserv)
;;   ;; Configure and start (see README for details)

;;; Code:

;; Autoload the component modules
(require 'dictionary-server-main)
(require 'google-ime-server)
(require 'mecab-server)
(require 'skk-async-server)

(provide 'emacs-skkserv)
;;; emacs-skkserv.el ends here
