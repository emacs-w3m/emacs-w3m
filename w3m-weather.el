;;; w3m-weather.el --- Add-on program to look weather forecast

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

;; w3m-weather.el is the add-on program of w3m.el to look weather
;; foracast.  For more detail about w3m.el, see:
;;
;;    http://namazu.org/~tsuchiya/emacs-w3m/


;;; How to install:

;; Please put this file to appropriate directory, and if you want
;; byte-compile it.  And add following lisp expressions to your
;; ~/.emacs.
;;
;;     (autoload 'w3m-weather "w3m-weather" "Display weather report." t)


;;; Code:

(require 'w3m)

(defconst w3m-weather-url-alist
  (eval-when-compile
    (let ((format "http://channel.goo.ne.jp/weather/area/%s.html")
	  (alist
	   '(("北海道・宗谷地方" . "011")
	     ("北海道・網走地方" . "021")
	     ("北海道・北見地方" . "022")
	     ("北海道・紋別地方" . "023")
	     ("北海道・上川地方" . "031")
	     ("北海道・留萌地方" . "032")
	     ("北海道・釧路地方" . "041")
	     ("北海道・根室地方" . "042")
	     ("北海道・十勝地方" . "043")
	     ("北海道・胆振地方" . "051")
	     ("北海道・日高地方" . "052")
	     ("北海道・石狩地方" . "061")
	     ("北海道・空知地方" . "062")
	     ("北海道・後志地方" . "063")
	     ("北海道・渡島地方" . "071")
	     ("北海道・檜山地方" . "072")
	     ("青森県・津軽地方" . "081")
	     ("青森県・下北地方" . "082")
	     ("青森県・三八上北地方" . "083")
	     ("秋田県・沿岸部" . "091")
	     ("秋田県・内陸部" . "092")
	     ("岩手県・内陸部" . "101")
	     ("岩手県・沿岸北部" . "102")
	     ("岩手県・沿岸南部" . "103")
	     ("山形県・村山地方" . "111")
	     ("山形県・置賜地方" . "112")
	     ("山形県・庄内地方" . "113")
	     ("山形県・最上地方" . "114")
	     ("宮城県・平野部" . "121")
	     ("宮城県・山沿い" . "122")
	     ("福島県・中通り" . "131")
	     ("福島県・浜通り" . "132")
	     ("福島県・会津地方" . "133")
	     ("新潟県・下越地方" . "141")
	     ("新潟県・中越地方" . "142")
	     ("新潟県・上越地方" . "143")
	     ("新潟県・佐渡島" . "144")
	     ("富山県・東部" . "151")
	     ("富山県・西部" . "152")
	     ("石川県・加賀地方" . "161")
	     ("石川県・能登地方" . "162")
	     ("福井県・嶺北" . "171")
	     ("福井県・嶺南" . "172")
	     ("栃木県・南部" . "181")
	     ("栃木県・北部" . "182")
	     ("群馬県・南部" . "191")
	     ("群馬県・北部" . "192")
	     ("埼玉県・南部" . "201")
	     ("埼玉県・北部" . "202")
	     ("埼玉県・秩父地方" . "203")
	     ("茨城県・北部" . "211")
	     ("茨城県・南部" . "212")
	     ("千葉県・北西部" . "221")
	     ("千葉県・北東部" . "222")
	     ("千葉県・南部" . "223")
	     ("東京都" . "231")
	     ("東京都・伊豆諸島北部" . "232")
	     ("東京都・伊豆諸島南部" . "233")
	     ("東京都・小笠原" . "234")
	     ("神奈川県・東部" . "261")
	     ("神奈川県・西部" . "262")
	     ("長野県・北部" . "271")
	     ("長野県・中部" . "272")
	     ("長野県・南部" . "273")
	     ("山梨県・中西部" . "281")
	     ("山梨県・東部富士五湖" . "282")
	     ("静岡県・中部" . "291")
	     ("静岡県・西部" . "292")
	     ("静岡県・東部" . "293")
	     ("静岡県・伊豆地方" . "294")
	     ("岐阜県・美濃地方" . "301")
	     ("岐阜県・飛騨地方" . "302")
	     ("三重県・北中部" . "311")
	     ("三重県・南部" . "312")
	     ("愛知県・西部" . "321")
	     ("愛知県・東部" . "322")
	     ("京都府・南部" . "331")
	     ("京都府・北部" . "332")
	     ("兵庫県・南部" . "341")
	     ("兵庫県・北部" . "342")
	     ("奈良県・北部" . "351")
	     ("奈良県・南部" . "352")
	     ("滋賀県・南部" . "361")
	     ("滋賀県・北部" . "362")
	     ("和歌山県・北部" . "371")
	     ("和歌山県・南部" . "372")
	     ("大阪府" . "381")
	     ("鳥取県・東部" . "391")
	     ("鳥取県・西部" . "392")
	     ("島根県・東部" . "401")
	     ("島根県・西部" . "402")
	     ("島根県・隠岐諸島" . "403")
	     ("岡山県・南部" . "411")
	     ("岡山県・北部" . "412")
	     ("広島県・南部" . "421")
	     ("広島県・北部" . "422")
	     ("山口県・西部" . "431")
	     ("山口県・中部" . "432")
	     ("山口県・東部" . "433")
	     ("山口県・北部" . "434")
	     ("香川県" . "441")
	     ("愛媛県・中予地方" . "451")
	     ("愛媛県・東予地方" . "452")
	     ("愛媛県・南予地方" . "453")
	     ("徳島県・北部" . "461")
	     ("徳島県・南部" . "462")
	     ("高知県・中部" . "471")
	     ("高知県・東部" . "472")
	     ("高知県・西部" . "473")
	     ("福岡県・福岡地方" . "481")
	     ("福岡県・北九州地方" . "482")
	     ("福岡県・筑豊地方" . "483")
	     ("福岡県・筑後地方" . "484")
	     ("大分県・中部" . "491")
	     ("大分県・北部" . "492")
	     ("大分県・西部" . "493")
	     ("大分県・南部" . "494")
	     ("佐賀県・南部" . "501")
	     ("佐賀県・北部" . "502")
	     ("熊本県・熊本地方" . "511")
	     ("熊本県・阿蘇地方" . "512")
	     ("熊本県・天草芦北地方" . "513")
	     ("熊本県・球磨地方" . "514")
	     ("宮崎県・南部平野" . "521")
	     ("宮崎県・南部山沿い" . "522")
	     ("宮崎県・北部平野" . "523")
	     ("宮崎県・北部山沿い" . "524")
	     ("長崎県・南部" . "531")
	     ("長崎県・北部" . "532")
	     ("長崎県・壱岐対馬地方" . "533")
	     ("長崎県・五島地方" . "534")
	     ("鹿児島県・薩摩地方" . "561")
	     ("鹿児島県・大隅地方" . "562")
	     ("鹿児島県・種子島" . "563")
	     ("鹿児島県・屋久島" . "563")
	     ("奄美諸島" . "564")
	     ("沖縄県・中南部" . "591")
	     ("沖縄県・北部" . "592")
	     ("沖縄県・久米島" . "593")
	     ("沖縄県・大東島" . "594")
	     ("沖縄県・宮古島" . "595")
	     ("沖縄県・石垣島" . "596")
	     ("沖縄県・与那国島" . "597"))))
      (mapcar (lambda (area)
		(cons (car area) (format format (cdr area))))
	      alist)))
  "Associative list of regions and urls.")

(defcustom w3m-weather-default-area
  "京都府・南部"
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
;;;###autoload
(defun w3m-weather (area)
  "Display weather report."
  (interactive
   (list (if current-prefix-arg
	     (completing-read "Input area: " w3m-weather-url-alist nil t)
	   w3m-weather-default-area)))
  (w3m (format "about://weather/%s" area)))

;;;###autoload
(defun w3m-about-weather (url &rest args)
  (let (area furl)
    (if (and (string-match "^about://weather/" url)
	     (setq area (substring url (match-end 0))
		   furl (cdr (assoc area w3m-weather-url-alist)))
	     (w3m-retrieve furl))
	(w3m-with-work-buffer
	  (w3m-decode-buffer furl)
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
	  "\\(<td[^>]*>天気</td>\\)[ \t\r\f\n]*<td[^>]*><img src=\"/weather/images/"
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
