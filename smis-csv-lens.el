;;; smis-csv-lens.el -- Specialization of csv-lens for SMI-S csv files

;; Copyright (C) 2015 Willem Rein Oudshoorn

;; Author: Willem Rein Oudshoorn <woudshoo@xs4all.nl>
;; Created: June 2015
;; Version: 0.1
;; Keywords: extensions
;; Package-Requires: (csv-lens)

;; This file is not part of GNU Emacs
;; Standard GLP v3 or higher license applies

;;; Commentary:

;;; Code:


(require 'vendor-from-wwn)
(require 'format-human-readable-big-number)
(require 'csv-lens-smis-time)

;;; Support functions for formatting cells.

(defun csv-lens-cell-format-wwn (wwn)
  "Return a nicely formatted WWN."
  (interactive)
  (if (and (vendor-from-wwn/valid-wwn wwn)
           (vendor-from-wwn wwn))
      (concat (vendor-from-wwn/vendor-specific-nice-wwn wwn) " ("  (vendor-from-wwn wwn) ")*" )
    wwn))



(defun csv-lens-cell-format-usagerestriction (usagerestriction)
  "Return a nicely formatted USAGERESTRICTION."
  (interactive)
  (or
   (assoc-default usagerestriction
		  '(("0" .    "Unknown*")
		    ("2" .    "Front-end only*")
		    ("3" .    "Back-end only*")
		    ("4" .    "Not restricted*")))
   usagerestriction))



(defun csv-lens-cell-format-statistictime (statistictime)
  "Return a nicely formatted STATISTICTIME."
  (interactive)
  (if (> (length statistictime) 18)
      (let (year month day hour minute second offset)
	(setq year (substring statistictime 0 4)
	      month (substring statistictime 4 6)
	      day (substring statistictime 6 8)
	      hour (substring statistictime 8 10)
	      minute (substring statistictime 10 12)
	      second (substring statistictime 12 14)
	      offset (number-to-string (/ (string-to-number (substring statistictime -4)) 60)))
	(concat year "-" month "-" day " " hour ":" minute ":" second " (" offset ")*" ))
    statistictime))



(defun csv-lens-cell-format-big-number-of-bytes (big-number-of-bytes)
  ""
  (interactive)
  (format-human-readable-big-number (string-to-number big-number-of-bytes) "%0.1f" *exceptional-format* "B" t :binary))

(defun csv-lens-cell-format-big-number-of-kilobytes (big-number-of-kilobytes)
  ""
  (concat
   (format-human-readable-big-number (* (string-to-number big-number-of-kilobytes) 1024.0) "%0.1f" *exceptional-format* "B" t :binary)
   "*"))


(defun csv-lens-cell-format-big-number-of-blocks (big-number-of-blocks)
 ""
 (concat
  (format-human-readable-big-number (* (string-to-number big-number-of-blocks) 512.0) "%0.1f" *exceptional-format* "B" t :binary)
  "*"))

;;; Setting up the defaults

(defvar csv-lens-default-column-state)
(setq csv-lens-default-column-state
      `(("InstanceID" :key t :diff-function csv-lens-diff-always-nil)
	("ElementType" :diff-function csv-lens-diff-always-nil)
	
	("StatisticTime" :diff-function csv-lens-diff-statistictime)
	(("StatisticTime" "PeriodStartTime" "PeriodEndTime" "IM_OriginalStatisticTime")
	 :format-function csv-lens-cell-format-statistictime)
	
	("UsageRestriction" :format-function csv-lens-cell-format-usagerestriction)
	
	("Consumed" :format-function csv-lens-cell-format-huge-number)
	
	(("NumberOfBlocks" "ConsumableBlocks")
	 :format-function csv-lens-cell-format-big-number-of-blocks)

	(("EMCKBytesSPBWritten" "EMCKBytesSPAWritten"
	  "EMCKBytesSPBRead" "EMCKBytesSPARead"
	  "KBytesWritten" "KBytesTransferred" "KBytesRead")
	 :format-function csv-lens-cell-format-big-number-of-kilobytes)

	(("RequestedSpeed" "Speed" "MaxSpeed")
	 :format-function csv-lens-cell-format-big-number-of-bytes)

	(("OtherIdentifyingInfo" "EMCWWN"
	  "AntecedentFCPortWWN" "AntecedentElementWWN"
	  "DependentFCPortWWN" "DependentElementWWN"
	  "ElementName" "DeviceID"
	  "SwitchWWPN" "PermanentAddress")
	 :format-function csv-lens-cell-format-wwn)
	(t :format-function csv-lens-cell-format-huge-number)))

(provide 'smis-csv-lens)
;;; smis-csv-lens.el ends here
