;;; -*- mode: Emacs-Lisp; coding: euc-japan -*-

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@pine.kuee.kyoto-u.ac.jp>,
;; Keywords: w3m, WWW, hypermedia

;; w3m-weather.el is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2 of the
;; License, or (at your option) any later version.

;; w3m-weather.el is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with w3m.el; if not, write to the Free Software Foundation,
;; Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA


;;; Commentary:

;; w3m-weather.el is the add-on program to look weather foracast of
;; w3m.el.  For more detail about w3m.el, see:
;;
;;    http://namazu.org/~tsuchiya/emacs-w3m/


;;; How to install:

;; Please put this file to appropriate directory, and if you want
;; byte-compile it.  And add following lisp expressions to your
;; ~/.emacs.
;;
;;     (autoload 'w3m-weather "w3m-weather" "*Display weather report." t)


;;; Code:

(require 'w3m)

(defconst w3m-weather-url-alist
  (eval-when-compile
    (let ((format "http://channel.goo.ne.jp/weather/area/%s.html")
	  (alist
	   '(("ËÌ³¤Æ»¡¦½¡Ã«ÃÏÊı" . "011")
	     ("ËÌ³¤Æ»¡¦ÌÖÁöÃÏÊı" . "021")
	     ("ËÌ³¤Æ»¡¦ËÌ¸«ÃÏÊı" . "022")
	     ("ËÌ³¤Æ»¡¦ÌæÊÌÃÏÊı" . "023")
	     ("ËÌ³¤Æ»¡¦¾åÀîÃÏÊı" . "031")
	     ("ËÌ³¤Æ»¡¦Î±Ë¨ÃÏÊı" . "032")
	     ("ËÌ³¤Æ»¡¦¶üÏ©ÃÏÊı" . "041")
	     ("ËÌ³¤Æ»¡¦º¬¼¼ÃÏÊı" . "042")
	     ("ËÌ³¤Æ»¡¦½½¾¡ÃÏÊı" . "043")
	     ("ËÌ³¤Æ»¡¦ÃÀ¿¶ÃÏÊı" . "051")
	     ("ËÌ³¤Æ»¡¦Æü¹âÃÏÊı" . "052")
	     ("ËÌ³¤Æ»¡¦ÀĞ¼íÃÏÊı" . "061")
	     ("ËÌ³¤Æ»¡¦¶õÃÎÃÏÊı" . "062")
	     ("ËÌ³¤Æ»¡¦¸å»ÖÃÏÊı" . "063")
	     ("ËÌ³¤Æ»¡¦ÅÏÅçÃÏÊı" . "071")
	     ("ËÌ³¤Æ»¡¦ÛØ»³ÃÏÊı" . "072")
	     ("ÀÄ¿¹¸©¡¦ÄÅ·ÚÃÏÊı" . "081")
	     ("ÀÄ¿¹¸©¡¦²¼ËÌÃÏÊı" . "082")
	     ("ÀÄ¿¹¸©¡¦»°È¬¾åËÌÃÏÊı" . "083")
	     ("½©ÅÄ¸©¡¦±è´ßÉô" . "091")
	     ("½©ÅÄ¸©¡¦ÆâÎ¦Éô" . "092")
	     ("´ä¼ê¸©¡¦ÆâÎ¦Éô" . "101")
	     ("´ä¼ê¸©¡¦±è´ßËÌÉô" . "102")
	     ("´ä¼ê¸©¡¦±è´ßÆîÉô" . "103")
	     ("»³·Á¸©¡¦Â¼»³ÃÏÊı" . "111")
	     ("»³·Á¸©¡¦ÃÖ»òÃÏÊı" . "112")
	     ("»³·Á¸©¡¦¾±ÆâÃÏÊı" . "113")
	     ("»³·Á¸©¡¦ºÇ¾åÃÏÊı" . "114")
	     ("µÜ¾ë¸©¡¦Ê¿ÌîÉô" . "121")
	     ("µÜ¾ë¸©¡¦»³±è¤¤" . "122")
	     ("Ê¡Åç¸©¡¦ÃæÄÌ¤ê" . "131")
	     ("Ê¡Åç¸©¡¦ÉÍÄÌ¤ê" . "132")
	     ("Ê¡Åç¸©¡¦²ñÄÅÃÏÊı" . "133")
	     ("¿·³ã¸©¡¦²¼±ÛÃÏÊı" . "141")
	     ("¿·³ã¸©¡¦Ãæ±ÛÃÏÊı" . "142")
	     ("¿·³ã¸©¡¦¾å±ÛÃÏÊı" . "143")
	     ("¿·³ã¸©¡¦º´ÅÏÅç" . "144")
	     ("ÉÙ»³¸©¡¦ÅìÉô" . "151")
	     ("ÉÙ»³¸©¡¦À¾Éô" . "152")
	     ("ÀĞÀî¸©¡¦²Ã²ìÃÏÊı" . "161")
	     ("ÀĞÀî¸©¡¦Ç½ÅĞÃÏÊı" . "162")
	     ("Ê¡°æ¸©¡¦ÎæËÌ" . "171")
	     ("Ê¡°æ¸©¡¦ÎæÆî" . "172")
	     ("ÆÊÌÚ¸©¡¦ÆîÉô" . "181")
	     ("ÆÊÌÚ¸©¡¦ËÌÉô" . "182")
	     ("·²ÇÏ¸©¡¦ÆîÉô" . "191")
	     ("·²ÇÏ¸©¡¦ËÌÉô" . "192")
	     ("ºë¶Ì¸©¡¦ÆîÉô" . "201")
	     ("ºë¶Ì¸©¡¦ËÌÉô" . "202")
	     ("ºë¶Ì¸©¡¦ÃáÉãÃÏÊı" . "203")
	     ("°ñ¾ë¸©¡¦ËÌÉô" . "211")
	     ("°ñ¾ë¸©¡¦ÆîÉô" . "212")
	     ("ÀéÍÕ¸©¡¦ËÌÀ¾Éô" . "221")
	     ("ÀéÍÕ¸©¡¦ËÌÅìÉô" . "222")
	     ("ÀéÍÕ¸©¡¦ÆîÉô" . "223")
	     ("ÅìµşÅÔ" . "231")
	     ("ÅìµşÅÔ¡¦°ËÆ¦½ôÅçËÌÉô" . "232")
	     ("ÅìµşÅÔ¡¦°ËÆ¦½ôÅçÆîÉô" . "233")
	     ("ÅìµşÅÔ¡¦¾®³Ş¸¶" . "234")
	     ("¿ÀÆàÀî¸©¡¦ÅìÉô" . "261")
	     ("¿ÀÆàÀî¸©¡¦À¾Éô" . "262")
	     ("Ä¹Ìî¸©¡¦ËÌÉô" . "271")
	     ("Ä¹Ìî¸©¡¦ÃæÉô" . "272")
	     ("Ä¹Ìî¸©¡¦ÆîÉô" . "273")
	     ("»³Íü¸©¡¦ÃæÀ¾Éô" . "281")
	     ("»³Íü¸©¡¦ÅìÉôÉÙ»Î¸Ş¸Ğ" . "282")
	     ("ÀÅ²¬¸©¡¦ÃæÉô" . "291")
	     ("ÀÅ²¬¸©¡¦À¾Éô" . "292")
	     ("ÀÅ²¬¸©¡¦ÅìÉô" . "293")
	     ("ÀÅ²¬¸©¡¦°ËÆ¦ÃÏÊı" . "294")
	     ("´ôÉì¸©¡¦ÈşÇ»ÃÏÊı" . "301")
	     ("´ôÉì¸©¡¦ÈôÂÍÃÏÊı" . "302")
	     ("»°½Å¸©¡¦ËÌÃæÉô" . "311")
	     ("»°½Å¸©¡¦ÆîÉô" . "312")
	     ("°¦ÃÎ¸©¡¦À¾Éô" . "321")
	     ("°¦ÃÎ¸©¡¦ÅìÉô" . "322")
	     ("µşÅÔÉÜ¡¦ÆîÉô" . "331")
	     ("µşÅÔÉÜ¡¦ËÌÉô" . "332")
	     ("Ê¼¸Ë¸©¡¦ÆîÉô" . "341")
	     ("Ê¼¸Ë¸©¡¦ËÌÉô" . "342")
	     ("ÆàÎÉ¸©¡¦ËÌÉô" . "351")
	     ("ÆàÎÉ¸©¡¦ÆîÉô" . "352")
	     ("¼¢²ì¸©¡¦ÆîÉô" . "361")
	     ("¼¢²ì¸©¡¦ËÌÉô" . "362")
	     ("ÏÂ²Î»³¸©¡¦ËÌÉô" . "371")
	     ("ÏÂ²Î»³¸©¡¦ÆîÉô" . "372")
	     ("ÂçºåÉÜ" . "381")
	     ("Ä»¼è¸©¡¦ÅìÉô" . "391")
	     ("Ä»¼è¸©¡¦À¾Éô" . "392")
	     ("Åçº¬¸©¡¦ÅìÉô" . "401")
	     ("Åçº¬¸©¡¦À¾Éô" . "402")
	     ("Åçº¬¸©¡¦±£´ô½ôÅç" . "403")
	     ("²¬»³¸©¡¦ÆîÉô" . "411")
	     ("²¬»³¸©¡¦ËÌÉô" . "412")
	     ("¹­Åç¸©¡¦ÆîÉô" . "421")
	     ("¹­Åç¸©¡¦ËÌÉô" . "422")
	     ("»³¸ı¸©¡¦À¾Éô" . "431")
	     ("»³¸ı¸©¡¦ÃæÉô" . "432")
	     ("»³¸ı¸©¡¦ÅìÉô" . "433")
	     ("»³¸ı¸©¡¦ËÌÉô" . "434")
	     ("¹áÀî¸©" . "441")
	     ("°¦É²¸©¡¦ÃæÍ½ÃÏÊı" . "451")
	     ("°¦É²¸©¡¦ÅìÍ½ÃÏÊı" . "452")
	     ("°¦É²¸©¡¦ÆîÍ½ÃÏÊı" . "453")
	     ("ÆÁÅç¸©¡¦ËÌÉô" . "461")
	     ("ÆÁÅç¸©¡¦ÆîÉô" . "462")
	     ("¹âÃÎ¸©¡¦ÃæÉô" . "471")
	     ("¹âÃÎ¸©¡¦ÅìÉô" . "472")
	     ("¹âÃÎ¸©¡¦À¾Éô" . "473")
	     ("Ê¡²¬¸©¡¦Ê¡²¬ÃÏÊı" . "481")
	     ("Ê¡²¬¸©¡¦ËÌ¶å½£ÃÏÊı" . "482")
	     ("Ê¡²¬¸©¡¦ÃŞË­ÃÏÊı" . "483")
	     ("Ê¡²¬¸©¡¦ÃŞ¸åÃÏÊı" . "484")
	     ("ÂçÊ¬¸©¡¦ÃæÉô" . "491")
	     ("ÂçÊ¬¸©¡¦ËÌÉô" . "492")
	     ("ÂçÊ¬¸©¡¦À¾Éô" . "493")
	     ("ÂçÊ¬¸©¡¦ÆîÉô" . "494")
	     ("º´²ì¸©¡¦ÆîÉô" . "501")
	     ("º´²ì¸©¡¦ËÌÉô" . "502")
	     ("·§ËÜ¸©¡¦·§ËÜÃÏÊı" . "511")
	     ("·§ËÜ¸©¡¦°¤ÁÉÃÏÊı" . "512")
	     ("·§ËÜ¸©¡¦Å·Áğ°²ËÌÃÏÊı" . "513")
	     ("·§ËÜ¸©¡¦µåËáÃÏÊı" . "514")
	     ("µÜºê¸©¡¦ÆîÉôÊ¿Ìî" . "521")
	     ("µÜºê¸©¡¦ÆîÉô»³±è¤¤" . "522")
	     ("µÜºê¸©¡¦ËÌÉôÊ¿Ìî" . "523")
	     ("µÜºê¸©¡¦ËÌÉô»³±è¤¤" . "524")
	     ("Ä¹ºê¸©¡¦ÆîÉô" . "531")
	     ("Ä¹ºê¸©¡¦ËÌÉô" . "532")
	     ("Ä¹ºê¸©¡¦°í´ôÂĞÇÏÃÏÊı" . "533")
	     ("Ä¹ºê¸©¡¦¸ŞÅçÃÏÊı" . "534")
	     ("¼¯»ùÅç¸©¡¦»§ËàÃÏÊı" . "561")
	     ("¼¯»ùÅç¸©¡¦Âç¶ùÃÏÊı" . "562")
	     ("¼¯»ùÅç¸©¡¦¼ï»ÒÅç" . "563")
	     ("¼¯»ùÅç¸©¡¦²°µ×Åç" . "563")
	     ("±âÈş½ôÅç" . "564")
	     ("²­Æì¸©¡¦ÃæÆîÉô" . "591")
	     ("²­Æì¸©¡¦ËÌÉô" . "592")
	     ("²­Æì¸©¡¦µ×ÊÆÅç" . "593")
	     ("²­Æì¸©¡¦ÂçÅìÅç" . "594")
	     ("²­Æì¸©¡¦µÜ¸ÅÅç" . "595")
	     ("²­Æì¸©¡¦ÀĞ³ÀÅç" . "596")
	     ("²­Æì¸©¡¦Í¿Æá¹ñÅç" . "597"))))
      (mapcar (lambda (area)
		(cons (car area) (format format (cdr area))))
	      alist)))
  "Associative list of regions and urls.")

(defcustom w3m-weather-default-area
  "µşÅÔÉÜ¡¦ÆîÉô"
  "Default region to check weateher."
  :group 'w3m
  :type (cons 'radio
	      (mapcar (lambda (area) (list 'const (car area)))
		      w3m-weather-url-alist)))

(defcustom w3m-weather-filter-functions
  '(w3m-weather-remove-headers
    w3m-weather-remove-footers
    w3m-weather-remove-weather-images
    w3m-weather-remove-washing-images
    w3m-weather-remove-futon-images
    w3m-weather-remove-week-weather-images
    w3m-weather-insert-title)
  "Filter functions to remove useless tags."
  :group 'w3m
  :type 'hook)

;;; Weather:
(defun w3m-weather (area)
  "*Display weather report."
  (interactive
   (list (if current-prefix-arg
	     (completing-read "Input area: " w3m-weather-url-alist nil t)
	   w3m-weather-default-area)))
  (w3m (format "about://weather/%s" area)))

(defun w3m-about-weather (url &rest args)
  (let (area furl)
    (if (and (string-match "^about://weather/" url)
	     (setq area (substring url (match-end 0))
		   furl (cdr (assoc area w3m-weather-url-alist)))
	     (w3m-retrieve furl))
	(w3m-with-work-buffer
	  (run-hook-with-args 'w3m-weather-filter-functions area)
	  "text/html")
      (w3m-message "Unknown URL: %s" url)
      nil)))

(defun w3m-weather-remove-headers (&rest args)
  "Remove header of the weather forecast page."
  (goto-char (point-min))
  (when (search-forward "<!-- area_s_title -->" nil t)
    (delete-region (point-min) (point))
    (when (search-forward "<img src=\"/common/clear.gif\"")
      (let ((start))
	(and (search-backward "<tr>" nil t)
	     (setq start (point))
	     (search-forward "</tr>" nil t)
	     (delete-region start (point)))))))

(defun w3m-weather-remove-footers (&rest args)
  "Remove footer of the weather forecast page."
  (goto-char (point-max))
  (when (search-backward "<!-- /area_7days -->" nil t)
    (delete-region (point) (point-max))
    (forward-line -2)
    (when (looking-at "<div")
      (delete-region (point) (point-max)))))

(defun w3m-weather-remove-weather-images (&rest args)
  "Remove images which stand for weather forecasts."
  (let ((case-fold-search t) start end)
    (goto-char (point-min))
    (and (re-search-forward
	  "\\(<td[^>]*>Å·µ¤</td>\\)[ \t\r\f\n]*<td[^>]*><img src=\"/weather/images/"
	  nil t)
	 (setq start (match-beginning 1)
	       end (match-end 1))
	 (search-forward
	  "<tr bgcolor=\"#FFFFFF\">"
	  (prog2 (forward-line 5) (point) (goto-char (match-end 0)))
	  t)
	 (progn
	   (delete-region end (point))
	   (goto-char start)
	   (when (re-search-forward "\\([ \t\r\f\n]rowspan=\"[0-9]+\"\\)[> \t\r\f\n]" end t)
	     (delete-region (match-beginning 1) (match-end 1)))))))

(defun w3m-weather-remove-washing-images (&rest args)
  "Remove images which stand for washing index."
  (let ((case-fold-search t))
    (goto-char (point-min))
    (while (re-search-forward
	    "<td[^>]*>\\(<img src=\"/weather/images/wash[-0-9]*.gif\"[^>]*><br>\\)"
	    nil t)
      (delete-region (match-beginning 1) (match-end 1)))))

(defun w3m-weather-remove-futon-images (&rest args)
  "Remove images which stand for futon index."
  (let ((case-fold-search t))
    (goto-char (point-min))
    (while (re-search-forward
	    "<td[^>]*>\\(<img src=\"/weather/images/bed[-0-9]*.gif\"[^>]*><br>\\)"
	    nil t)
      (delete-region (match-beginning 1) (match-end 1)))))

(defun w3m-weather-remove-week-weather-images (&rest args)
  "Remove images which stand for the weather forecast for the week."
  (let ((case-fold-search t))
    (goto-char (point-min))
    (while (re-search-forward
	    "<td[^>]*>\\(<img src=\"/weather/images/tk[0-9]*.gif\"[^>]*><br>\\)"
	    nil t)
      (delete-region (match-beginning 1) (match-end 1)))))

(defun w3m-weather-insert-title (area &rest args)
  "Insert title."
  (goto-char (point-min))
  (insert "<head><title>Weather forecast of " area "</title></head><body>")
  (goto-char (point-max))
  (insert "</body>"))


(provide 'w3m-weather)
;;; w3m-weather.el ends here.
