;;; w3m-weather.el --- Add-on program to look weather forecast

;; Copyright (C) 2001 TSUCHIYA Masatoshi <tsuchiya@namazu.org>

;; Authors: TSUCHIYA Masatoshi <tsuchiya@namazu.org>,
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

;; w3m-weather.el is the add-on program of emacs-w3m to look weather
;; foracast.  For more detail about emacs-w3m, see:
;;
;;    http://emacs-w3m.namazu.org/


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
    (let ((format "http://weather.yahoo.co.jp/weather/jp/%s.html")
	  (alist
	   '(("道北・宗谷" . "1a/1100")
	     ("道北・上川" . "1a/1200")
	     ("道北・留萌" . "1a/1300")
	     ("道東・網走" . "1c/1710")
	     ("道東・北見" . "1c/1720")
	     ("道東・紋別" . "1c/1730")
	     ("道東・根室" . "1c/1800")
	     ("道東・釧路" . "1c/1900")
	     ("道東・十勝" . "1c/2000")
	     ("道央・石狩" . "1b/1400")
	     ("道央・空知" . "1b/1500")
	     ("道央・後志" . "1b/1600")
	     ("道南・桧山" . "1d/2400")
	     ("道南・胆振" . "1d/2100")
	     ("道南・日高" . "1d/2200")
	     ("道南・渡島" . "1d/2300")
	     ("青森県・津軽" . "2/3110")
	     ("青森県・下北" . "2/3120")
	     ("青森県・三八上北" . "2/3130")
	     ("岩手県・内陸部" . "3/3310")
	     ("岩手県・沿岸北部" . "3/3320")
	     ("岩手県・沿岸南部" . "3/3330")
	     ("秋田県・沿岸部" . "5/3210")
	     ("秋田県・内陸部" . "5/3220")
	     ("宮城県・平野部" . "4/3410")
	     ("宮城県・山沿い" . "4/3420")
	     ("山形県・村山" . "6/3510")
	     ("山形県・置賜" . "6/3520")
	     ("山形県・庄内" . "6/3530")
	     ("山形県・最上" . "6/3540")
	     ("福島県・中通り" . "7/3610")
	     ("福島県・浜通り" . "7/3620")
	     ("福島県・会津" . "7/3630")
	     ("茨城県・北部" . "8/4010")
	     ("茨城県・南部" . "8/4020")
	     ("栃木県・南部" . "9/4110")
	     ("栃木県・北部" . "9/4120")
	     ("群馬県・南部" . "10/4210")
	     ("群馬県・北部" . "10/4220")
	     ("埼玉県・南部" . "11/4310")
	     ("埼玉県・北部" . "11/4320")
	     ("埼玉県・秩父" . "11/4330")
	     ("千葉県・北西部" . "12/4510")
	     ("千葉県・北東部" . "12/4520")
	     ("千葉県・南部" . "12/4530")
	     ("東京都・父島" . "13/9900")
	     ("東京都・東京" . "13/4400")
	     ("東京都・伊豆諸島北部" . "13/0")
	     ("東京都・伊豆諸島南部" . "13/100")
	     ("神奈川県・東部" . "14/4610")
	     ("神奈川県・西部" . "14/4620")
	     ("新潟県・下越" . "15/5410")
	     ("新潟県・中越" . "15/5420")
	     ("新潟県・上越" . "15/5430")
	     ("新潟県・佐渡" . "15/5440")
	     ("富山県・東部" . "16/5510")
	     ("富山県・西部" . "16/5520")
	     ("石川県・加賀" . "17/5610")
	     ("石川県・能登" . "17/5620")
	     ("福井県・嶺北" . "18/5710")
	     ("福井県・嶺南" . "18/5720")
	     ("山梨県・中西部" . "19/4910")
	     ("山梨県・富士五湖" . "19/4920")
	     ("長野県・北部" . "20/4810")
	     ("長野県・中部" . "20/4820")
	     ("長野県・南部" . "20/4830")
	     ("岐阜県・美濃" . "21/5210")
	     ("岐阜県・飛騨" . "21/5220")
	     ("静岡県・中部" . "22/5010")
	     ("静岡県・伊豆" . "22/5020")
	     ("静岡県・東部" . "22/5030")
	     ("静岡県・西部" . "22/5040")
	     ("愛知県・西部" . "23/5110")
	     ("愛知県・東部" . "23/5120")
	     ("三重県・北中部" . "24/5310")
	     ("三重県・南部" . "24/5320")
	     ("滋賀県・南部" . "25/6010")
	     ("滋賀県・北部" . "25/6020")
	     ("京都府・北部" . "26/400")
	     ("京都府・南部" . "26/6100")
	     ("大阪府" . "27/6200")
	     ("兵庫県・北部" . "28/500")
	     ("兵庫県・南部" . "28/6300")
	     ("奈良県・北部" . "29/6410")
	     ("奈良県・南部" . "29/6420")
	     ("和歌山県・北部" . "30/6510")
	     ("和歌山県・南部" . "30/6520")
	     ("鳥取県・東部" . "31/6910")
	     ("鳥取県・西部" . "31/6920")
	     ("島根県・隠岐" . "32/600")
	     ("島根県・東部" . "32/6810")
	     ("島根県・西部" . "32/6820")
	     ("岡山県・南部" . "33/6610")
	     ("岡山県・北部" . "33/6620")
	     ("広島県・南部" . "34/6710")
	     ("広島県・北部" . "34/6720")
	     ("山口県・西部" . "35/8110")
	     ("山口県・中部" . "35/8120")
	     ("山口県・北部" . "35/8140")
	     ("山口県・東部" . "35/8130")
	     ("徳島県・北部" . "36/7110")
	     ("徳島県・南部" . "36/7120")
	     ("香川県" . "37/7200")
	     ("愛媛県・東予" . "38/7320")
	     ("愛媛県・南予" . "38/7330")
	     ("愛媛県・中予" . "38/7310")
	     ("高知県・中部" . "39/7410")
	     ("高知県・東部" . "39/7420")
	     ("高知県・西部" . "39/7430")
	     ("福岡県・福岡" . "40/8210")
	     ("福岡県・北九州" . "40/8220")
	     ("福岡県・筑豊" . "40/8230")
	     ("福岡県・筑後" . "40/8240")
	     ("佐賀県・南部" . "41/8510")
	     ("佐賀県・北部" . "41/8520")
	     ("長崎県・壱岐対馬" . "42/700")
	     ("長崎県・五島" . "42/800")
	     ("長崎県・南部" . "42/8410")
	     ("長崎県・北部" . "42/8420")
	     ("熊本県・熊本" . "43/8610")
	     ("熊本県・阿蘇" . "43/8620")
	     ("熊本県・天草芦北" . "43/8630")
	     ("熊本県・球磨" . "43/8640")
	     ("大分県・中部" . "44/8310")
	     ("大分県・北部" . "44/8320")
	     ("大分県・西部" . "44/8330")
	     ("大分県・南部" . "44/8340")
	     ("宮崎県・南部平野部" . "45/8710")
	     ("宮崎県・北部平野部"・.・・"45/8720")
	     ("宮崎県・南部山沿い" . "45/8730")
	     ("宮崎県・北部山沿い" . "45/8740")
	     ("鹿児島県・薩摩" . "46/8810")
	     ("鹿児島県・大隅" . "46/8820")
	     ("鹿児島県・種子島・屋久島" . "46/900")
	     ("鹿児島県・奄美" . "46/1000")
	     ("沖縄県・本島中南部" . "47/9110")
	     ("沖縄県・本島北部" . "47/9120")
	     ("沖縄県・久米島" . "47/9130")
	     ("沖縄県・大東島" . "47/9200")
	     ("沖縄県・宮古島" . "47/9300")
	     ("沖縄県・石垣島" . "47/9400")
	     ("沖縄県・与那国島" . "47/9500"))))
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
(defun w3m-about-weather (url no-decode no-cache &rest args)
  (let (area furl)
    (if (and (string-match "^about://weather/" url)
	     (setq area (substring url (match-end 0))
		   furl (cdr (assoc area w3m-weather-url-alist)))
	     (w3m-retrieve furl nil no-cache))
	(w3m-with-work-buffer
	  (w3m-decode-buffer furl)
	  (run-hook-with-args 'w3m-weather-filter-functions area furl)
	  "text/html")
      (w3m-message "Unknown URL: %s" url)
      nil)))

(defun w3m-weather-remove-headers (&rest args)
  "Remove header of the weather forecast page."
  (goto-char (point-min))
  (when (search-forward "\
<TABLE border=\"0\" CELLSPACING=\"1\" CELLPADDING=\"0\" width=\"100%\">
<tr><td>

<table border=\"0\" CELLSPACING=\"0\" CELLPADDING=\"0\" width=\"100%\">
<tr><td bgcolor=\"#dcdcdc\"><b>今日・明日の天気</b></td>" nil t)
    (delete-region (point-min) (match-beginning 0))))

(defun w3m-weather-remove-footers (&rest args)
  "Remove footer of the weather forecast page."
  (goto-char (point-max))
  (when (search-backward "\
<table border=0 cellpadding=2 cellspacing=5 width=\"100%\">
<tr bgcolor=\"#dcdcdc\">
<td colspan=3><b>レジャー天気</b></td></tr>" nil t)
    (delete-region (point) (point-max))))

(defun w3m-weather-insert-title (area url &rest args)
  "Insert title."
  (goto-char (point-min))
  (insert "<head><title>Weather forecast of "
	  area
	  "</title></head>\n"
	  "<body><p align=left><a href=\""
	  url
	  "\">[Yahoo!]</a></p>\n")
  (goto-char (point-max))
  (insert "</body>"))


(provide 'w3m-weather)
;;; w3m-weather.el ends here.
