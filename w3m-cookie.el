;;; w3m-cookie.el --- Functions for cookie processing

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: Teranishi Yuuichi  <teranisi@gohome.org>
;; Keywords: w3m, WWW, hypermedia

;; This file is a part of emacs-w3m.

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program; if not, you can either send email to this
;; program's maintainer or write to: The Free Software Foundation,
;; Inc.; 59 Temple Place, Suite 330; Boston, MA 02111-1307, USA.

;;; Commentary:

;; This file contains the functions for cookies.  For more detail
;; about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/

;; Reference for version 0 cookie:                                  
;; 	http://www.netscape.com/newsref/std/cookie_spec.html
;; Reference for version 1 cookie:
;; 	http://www.ietf.org/rfc/rfc2965.txt
;;

;;; Code:

(require 'w3m-util)

(defvar w3m-cookies nil
  "A list of cookie elements.
Currently only browser local cookies are stored.")

(defconst w3m-cookie-two-dot-domains-regexp
  (concat "\\.\\("
	  (mapconcat 'identity (list "com" "edu" "net" "org" "gov" "mil" "int")
		     "\\|")
	  "\\)$")
  "A regular expression of top-level domains that only require two matching
'.'s in the domain name in order to set a cookie.")

;;; Cookie accessor.
(defsubst w3m-cookie-url (cookie)
  (aref cookie 0))

(defsubst w3m-cookie-set-url (cookie url)
  (aset cookie 0 url))

(defsubst w3m-cookie-domain (cookie)
  (aref cookie 1))

(defsubst w3m-cookie-set-domain (cookie domain)
  (aset cookie 1 domain))

(defsubst w3m-cookie-secure (cookie)
  (aref cookie 2))

(defsubst w3m-cookie-set-secure (cookie secure)
  (aset cookie 2 secure))

(defsubst w3m-cookie-name (cookie)
  (aref cookie 3))

(defsubst w3m-cookie-set-name (cookie name)
  (aset cookie 3 name))

(defsubst w3m-cookie-value (cookie)
  (aref cookie 4))

(defsubst w3m-cookie-set-value (cookie value)
  (aset cookie 4 value))

(defsubst w3m-cookie-path (cookie)
  (aref cookie 5))

(defsubst w3m-cookie-set-path (cookie path)
  (aset cookie 5 path))

(defsubst w3m-cookie-version (cookie)
  (aref cookie 6))

(defsubst w3m-cookie-set-version (cookie version)
  (aset cookie 6 version))

(defsubst w3m-cookie-expires (cookie)
  (aref cookie 7)) ; not used

(defsubst w3m-cookie-set-expires (cookie expires)
  (aset cookie 7 expires)) ; not used

(defsubst w3m-cookie-ignore (cookie)
  (aref cookie 8)) ; not used

(defsubst w3m-cookie-set-ignore (cookie ignore)
  (aset cookie 7 ignore)) ; not used

(defsubst w3m-cookie-create (&rest args)
  (let ((cookie (make-vector 9 nil)))
    (w3m-cookie-set-url     cookie (plist-get args :url))
    (w3m-cookie-set-domain  cookie (plist-get args :domain))
    (w3m-cookie-set-secure  cookie (plist-get args :secure))
    (w3m-cookie-set-name    cookie (plist-get args :name))
    (w3m-cookie-set-value   cookie (plist-get args :value))
    (w3m-cookie-set-path    cookie (plist-get args :path))
    (w3m-cookie-set-version cookie (or (plist-get args :version) 0))
    (w3m-cookie-set-expires cookie (plist-get args :expires))
    (w3m-cookie-set-ignore  cookie (plist-get args :ignore))
    cookie))

(defun w3m-cookie-store (cookie)
  "Store COOKIE."
  (dolist (c w3m-cookies)
    (when (and (string= (w3m-cookie-domain c)
			(w3m-cookie-domain cookie))
	       (string= (w3m-cookie-path c)
			(w3m-cookie-path cookie))
	       (string= (w3m-cookie-name c)
			(w3m-cookie-name cookie)))
      (setq w3m-cookies (delq c w3m-cookies))))
  (push cookie w3m-cookies))

(defun w3m-cookie-remove (domain path name)
  "Remove COOKIE if stored."
  (dolist (c w3m-cookies)
    (when (and (string= (w3m-cookie-domain c)
			domain)
	       (string= (w3m-cookie-path c)
			path)
	       (string= (w3m-cookie-name c)
			name))
      (setq w3m-cookies (delq c w3m-cookies)))))

(defun w3m-cookie-retrieve (host path &optional secure)
  "Retrieve cookies for DOMAIN and PATH."
  (let ((case-fold-search t)
	cookies)
    (dolist (c w3m-cookies)
      (when (and (string-match (concat 
				(regexp-quote (w3m-cookie-domain c)) "$")
			       host)
		 (string-match (concat
				"^" (regexp-quote (w3m-cookie-path c)))
			       path))
	(if (w3m-cookie-secure c)
	    (if secure
		(push c cookies))
	  (push c cookies))))
    cookies))

;; HTTP URL parser.
(defun w3m-parse-http-url (url)
  "Parse an absolute HTTP URL."
  (let (secure split)
    (when (and (string-match (symbol-value 'w3m-url-components-regexp) url)
	       (or (string= (match-string 2 url) "http")
		   (setq secure (string= (match-string 2 url) "https")))
	       (match-beginning 4)
	       (match-end 4))
      (setq split (save-match-data
		    (split-string (match-string 4 url) ":")))
      (vector secure
	      (nth 0 split)
	      (string-to-number (or (nth 1 split) "80"))
	      (if (eq (length (match-string 5 url)) 0)
		  "/"
		(match-string 5 url))))))

(defsubst w3m-http-url-secure (http-url)
  "Secure flag of the HTTP-URL."
  (aref http-url 0))

(defsubst w3m-http-url-host (http-url)
  "Host name of the HTTP-URL."
  (aref http-url 1))

(defsubst w3m-http-url-port (http-url)
  "Port number of the HTTP-URL."
  (aref http-url 2))

(defsubst w3m-http-url-path (http-url)
  "Path of the HTTP-URL."
  (aref http-url 3))

;;; Cookie parser.
(defvar w3m-cookie-parse-args-syntax-table
  (copy-syntax-table emacs-lisp-mode-syntax-table)
  "A syntax table for parsing sgml attributes.")

(modify-syntax-entry ?' "\"" w3m-cookie-parse-args-syntax-table)
(modify-syntax-entry ?` "\"" w3m-cookie-parse-args-syntax-table)
(modify-syntax-entry ?{ "(" w3m-cookie-parse-args-syntax-table)
(modify-syntax-entry ?} ")" w3m-cookie-parse-args-syntax-table)

(defun w3m-cookie-parse-args (str &optional nodowncase)
  (let (name value results name-pos val-pos st nd)
    (save-excursion
      (save-restriction
	(set-buffer (get-buffer-create " *w3m-cookie-parse-temp*"))
	(set-syntax-table w3m-cookie-parse-args-syntax-table)
	(erase-buffer)
	(insert str)
	(setq st (point-min)
	      nd (point-max))
	(set-syntax-table w3m-cookie-parse-args-syntax-table)
	(narrow-to-region st nd)
	(goto-char (point-min))
	(while (not (eobp))
	  (skip-chars-forward "; \n\t")
	  (setq name-pos (point))
	  (skip-chars-forward "^ \n\t=;")
	  (if (not nodowncase)
	      (downcase-region name-pos (point)))
	  (setq name (buffer-substring name-pos (point)))
	  (skip-chars-forward " \t\n")
	  (if (/= (or (char-after (point)) 0)  ?=) ; There is no value
	      (setq value nil)
	    (skip-chars-forward " \t\n=")
	    (setq val-pos (point)
		  value
		  (cond
		   ((or (= (or (char-after val-pos) 0) ?\")
			(= (or (char-after val-pos) 0) ?'))
		    (buffer-substring (1+ val-pos)
				      (condition-case ()
					  (prog2
					      (forward-sexp 1)
					      (1- (point))
					    (skip-chars-forward "\""))
					(error
					 (skip-chars-forward "^ \t\n")
					 (point)))))
		   (t
		    (buffer-substring val-pos
				      (progn
					(skip-chars-forward "^;")
					(skip-chars-backward " \t")
					(point)))))))
	  (setq results (cons (cons name value) results))
	  (skip-chars-forward "; \n\t"))
	results))))

(defmacro w3m-assoc-ignore-case (name alist)
  (` (assoc* (, name) (, alist) :test
	     (lambda (x y)
	       (string= (downcase x) (downcase y))))))

(defun w3m-cookie-trusted-host-p (host)
  (let ((accept (symbol-value 'w3m-cookie-accept-domains))
	(reject (symbol-value 'w3m-cookie-reject-domains))
	(trusted t)
	regexp tlen rlen)
    (while accept
      (cond 
       ((string= (car accept) ".")
	(setq regexp ".*"))
       ((string= (car accept) ".local")
	(setq regexp "^[^\\.]+$"))
       (t (setq regexp (regexp-quote (car accept)))))
      (when (string-match (concat regexp "$") host)
	(setq tlen (length (car accept))
	      accept nil))
      (pop accept))
    (while reject
      (cond 
       ((string= (car reject) ".")
	(setq regexp ".*"))
       ((string= (car reject) ".local")
	(setq regexp "^[^\\.]+$"))
       (t (setq regexp (regexp-quote (car reject)))))
      (when (string-match (concat regexp "$") host)
	(setq rlen (length (car reject))
	      reject nil))
      (pop reject))
    (if tlen
	(if rlen
	    (if (<= tlen rlen)
		(setq trusted nil)))
      (if rlen
	  (setq trusted nil)))
    trusted))

;;; Version 0 cookie.
(defun w3m-cookie-1-acceptable-p (host domain)
  (let ((numdots 0)
	(tmp domain)
	(last nil)
	(case-fold-search t)
	(mindots 3))
    (while (setq last (string-match "\\." domain last))
      (setq numdots (1+ numdots)
	    last (1+ last)))
    (if (string-match w3m-cookie-two-dot-domains-regexp domain)
	(setq mindots 2))
    (cond
     ((string= host domain)		; Apparently netscape lets you do this
      t)
     ((>= numdots mindots)		; We have enough dots in domain name
      ;; Need to check and make sure the host is actually _in_ the
      ;; domain it wants to set a cookie for though.
      (string-match (concat (regexp-quote domain) "$") host))
     (t
      nil))))

(defun w3m-cookie-1-set (url &rest args)
  ;; Set-Cookie:, version 0 cookie.
  (let ((http-url (w3m-parse-http-url url))
	(case-fold-search t)
	secure domain expires path rest)
    (when http-url
      (setq secure (and (w3m-assoc-ignore-case "secure" args) t)
	    domain (or (cdr-safe (w3m-assoc-ignore-case "domain" args))
		       (w3m-http-url-host http-url))
	    expires (cdr-safe (w3m-assoc-ignore-case "expires" args))
	    path (or (cdr-safe (w3m-assoc-ignore-case "path" args))
		     (file-name-directory
		      (w3m-http-url-path http-url))))
      (while args
	(if (not (member (downcase (car (car args)))
			 '("secure" "domain" "expires" "path")))
	    (setq rest (cons (car args) rest)))
	(setq args (cdr args)))
      (cond
       ((not (w3m-cookie-trusted-host-p (w3m-http-url-host http-url)))
	;; The site was explicity marked as untrusted by the user
	nil)
       ((w3m-cookie-1-acceptable-p (w3m-http-url-host http-url) domain)
	;; Cookie is accepted by the user, and passes our security checks
	(dolist (elem rest)
	  ;; If a CGI script wishes to delete a cookie, it can do so by
	  ;; returning a cookie with the same name, and an expires time
	  ;; which is in the past.
	  (if (and expires
		   (> (w3m-time-lapse-seconds (w3m-time-parse-string expires)
					      (current-time)) 0))
	      (w3m-cookie-remove domain path (car elem)))
	  (unless expires ; Stores only a browser local cookies.
	    (w3m-cookie-store
	     (w3m-cookie-create :url url
				:domain domain
				:name (car elem)
				:value (cdr elem)
				:path path
				:secure secure)))))
       (t
	(unless expires ; Treat only browser local cookies.
	  (message "%s tried to set a cookie for domain %s - rejected."
		   (w3m-http-url-host http-url) domain)))))))

;;; Version 1 cookie.
(defun w3m-cookie-2-acceptable-p (http-url domain)
  ;;   A user agent rejects (SHALL NOT store its information) if the Version
  ;;   attribute is missing.  Moreover, a user agent rejects (SHALL NOT
  ;;   store its information) if any of the following is true of the
  ;;   attributes explicitly present in the Set-Cookie2 response header:

  ;;      *  The value for the Path attribute is not a prefix of the
  ;;         request-URI.

  ;;      *  The value for the Domain attribute contains no embedded dots,
  ;;         and the value is not .local.

  ;;      *  The effective host name that derives from the request-host does
  ;;         not domain-match the Domain attribute.

  ;;      *  The request-host is a HDN (not IP address) and has the form HD,
  ;;         where D is the value of the Domain attribute, and H is a string
  ;;         that contains one or more dots.

  ;;      *  The Port attribute has a "port-list", and the request-port was
  ;;         not in the list.
  )

(defun w3m-cookie-2-set (url &rest args)
  ;; Set-Cookie2:, version 1 cookie.
  ;; Not implemented yet.
  )

;;;###autoload
(defun w3m-cookie-shutdown ()
  "Clear cookies."
  (interactive)
  (setq w3m-cookies nil))

;;;###autoload
(defun w3m-cookie-set (url data &optional version)
  "Register cookies.
URL is the url which corresponds to the cookie.
DATA is the content of Set-Cookie: or Set-Cookie2: field.
VERSION is 0 or 1. If omitted, 0 is assumed."
  (when (and url data)
    (let ((version (or version 0)))
      (apply
       (case version
	 (0 'w3m-cookie-1-set)
	 (1 'w3m-cookie-2-set))
       url (w3m-cookie-parse-args data)))))

;;;###autoload
(defun w3m-cookie-get (url)
  "Get a cookie field string which corresponds to the URL."
  (let* ((http-url (w3m-parse-http-url url))
	 (cookies (and http-url
		       (w3m-cookie-retrieve (w3m-http-url-host http-url)
					    (w3m-http-url-path http-url)
					    (w3m-http-url-secure http-url)))))
    ;; When sending cookies to a server, all cookies with a more specific path
    ;; mapping should be sent before cookies with less specific path mappings.
    (setq cookies (sort cookies
			(lambda (x y)
			  (< (length (w3m-cookie-path x))
			     (length (w3m-cookie-path y))))))
    (when cookies
      (mapconcat (lambda (cookie)
		   (concat (w3m-cookie-name cookie)
			   "=" (w3m-cookie-value cookie)))
		 cookies
		 "; "))))

(provide 'w3m-cookie)

;;; w3m-cookie.el ends here
