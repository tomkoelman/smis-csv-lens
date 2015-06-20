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

(defun csv-lens-cell-format-nameformat (nameformat)
  (cond ((s-equals? nameformat "1") "Other*")
        ((s-equals? nameformat "2") "VPD83NAA6 (deprecated)*")
        ((s-equals? nameformat "3") "VPD83NAA5 (deprecated)*")
        ((s-equals? nameformat "4") "BPD83Type2 (deprecated)*")
        ((s-equals? nameformat "5") "BPD83Type1 (deprecated)*")
        ((s-equals? nameformat "6") "BPD83Type0 (deprecated)*")
        ((s-equals? nameformat "7") "SNVM*")
        ((s-equals? nameformat "8") "NodeWWN (deprecated)*")
        ((s-equals? nameformat "9") "NAA*")
        ((s-equals? nameformat "10") "EUI64*")
        ((s-equals? nameformat "11") "T10VID*")
        (t nameformat)))


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


(defun csv-lens-cell-format-usage (usage)
  "Returns a nicely formatted USAGE."
  (interactive)
  (or
   (assoc-default usage
                  '(("1" . "Other*")
                    ("2" . "Unrestricted*")
                    ("3" . "Reserved for ComputerSystem (the block server)*")
                    ("4" . "Reserved by Replication Services*")
                    ("5" . "Reserved by Migration Services*")
                    ("6" . "Local Replica Source*")
                    ("7" . "Remote Replica Source*")
                    ("8" . "Local Replica Target*")
                    ("9" . "Remote Replica Target*")
                    ("10" . "Local Replica Source or Target*")
                    ("11" . "Remote Replica Source or Target*")
                    ("12" . "Delta Replica Target*")
                    ("13" . "Element Component*")
                    ("14" . "Reserved as Pool Contributor*")
                    ("15" . "Composite Volume Member*")
                    ("16" . "Composite LogicalDisk Member*")
                    ("17" . "Reserved for Sparing*")))
   usage))


(defun csv-lens-cell-format-volumetype (type)
  (interactive)
  (or 
   (assoc-default type
		  '(("0" . "Unknown*")
		    ("1" . "Base*")
		    ("2" . "Physical Copy*")
		    ("3" . "Virtual Copy")
		    ("4" . "Remote Copy")))
   type))


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

;(defvar csv-lens-default-column-state)
(setq csv-lens-configurations
      '(("SMIS-8"
	  (("InstanceID" :key t :diff-function csv-lens-diff-always-nil)
	   ("ElementType" :diff-function csv-lens-diff-always-nil)
	   
	   ("StatisticTime"
	    :diff-function csv-lens-diff-statistictime
	    :format-function csv-lens-cell-format-statistictime)
	   
	   (("EMCKBytesSPBWritten" "EMCKBytesSPAWritten" 
	     "EMCKBytesSPBRead" "EMCKBytesSPARead" 
	     "KBytesWritten" "KBytesTransferred" "KBytesRead") 
	    :format-function csv-lens-cell-format-big-number-of-kilobytes)))

	
	("SMIS-default"
	  (("InstanceID" :key t :diff-function csv-lens-diff-always-nil)
	   ("ElementType" :diff-function csv-lens-diff-always-nil)
	   
	   ("StatisticTime" :diff-function csv-lens-diff-statistictime)
	   (("StatisticTime" "PeriodStartTime" "PeriodEndTime" "IM_OriginalStatisticTime"
	     "IM_CollectorTime" "IM_TimeLastSampled") 
	    :format-function csv-lens-cell-format-statistictime)
	   
	   ("UsageRestriction" :format-function csv-lens-cell-format-usagerestriction)
	   ("Usage" :format-function csv-lens-cell-format-usage)
	   ("VolumeType" :format-function csv-lens-cell-format-volumetype)
	   
	   ("Consumed" :format-function csv-lens-cell-format-huge-number)
	   
	   (("NumberOfBlocks" "ConsumableBlocks") 
	    :format-function csv-lens-cell-format-big-number-of-blocks)

	   ("Capacity" :format-function csv-lens-cell-format-big-number-of-bytes)
	   
	   (("EMCKBytesSPBWritten" "EMCKBytesSPAWritten" 
	     "EMCKBytesSPBRead" "EMCKBytesSPARead" 
	     "KBytesWritten" "KBytesTransferred" "KBytesRead") 
	    :format-function csv-lens-cell-format-big-number-of-kilobytes)
	   
	   (("RequestedSpeed" "Speed" "MaxSpeed"
	     "RemainingManagedSpace" "SpaceLimit" "TotalManagedSpace" "ThinProvisionMetaDataSpace") 
	    :format-function csv-lens-cell-format-big-number-of-bytes)

	   ("Nameformat" :format-function csv-lens-cell-format-nameformat)

	   (("OtherIdentifyingInfo" "EMCWWN" 
	     "AntecedentFCPortWWN" "AntecedentElementWWN" 
	     "DependentFCPortWWN" "DependentElementWWN" 
	     "ElementName" "DeviceID" "Name"
	     "SwitchWWPN" "PermanentAddress"
	     "IM_WWNOfExternalVolume"
	     "PreferredWWPN" "ActiveWWPN") 
	    :format-function csv-lens-cell-format-wwn)))))

(provide 'smis-csv-lens)
;;; smis-csv-lens.el ends here
