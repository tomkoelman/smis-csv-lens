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

(defun csv-lens-cell-netapp-format-wwn (potential-wwn)
  "Needs fixing and made robust against non WWNs"
  (interactive)
  (let ((wwn? (subseq potential-wwn 0 (min 17 (length potential-wwn)))))
    (if (and (vendor-from-wwn/valid-wwn wwn?)
	     (vendor-from-wwn wwn?))
	(concat (vendor-from-wwn/vendor-specific-nice-wwn wwn?) " ("  (vendor-from-wwn wwn?) ")*" )
     potential-wwn)))

(defun csv-lens-cell-brocade-port-wwn (wwn)
  "Testing to see if we can get the slot information out of the wwn"
  (if (> (length wwn) 7)
      (let ((normal-description (csv-lens-cell-format-wwn wwn))
	    (vendor-info (vendor-sequence-from-wwn wwn)))
	(or 
	 (pcase (network-address-authority-from-wwn wwn)
	   ("5" (when (> (length vendor-info) 8)
		  (concat "A: " (subseq vendor-info 6) " B: " (subseq vendor-info 0 6) " -- " normal-description)))
	   
	   ("2" (when (> (length vendor-info) 3)
		  (concat "A: " (subseq vendor-info 0 3) " B: " (subseq vendor-info 3) " -- " normal-description))))
     
	 normal-description)))
  wwn)
    
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


(defun csv-lens-cell-format-connectivitymembertype-format (nameformat)
  (cond ((s-equals? nameformat "0") "Unknown*")
        ((s-equals? nameformat "1") "Other*")
	((s-equals? nameformat "2") "Permanent Address*")
        ((s-equals? nameformat "3") "Network Address (NxPort) (FCID)*")
        ((s-equals? nameformat "4") "Switch Port ID*")
        ((s-equals? nameformat "5") "Logical Port Group (Node WWN)*")
        ((s-equals? nameformat "6") "Connectivity Collection (Interface)*")
        ((s-equals? nameformat "7") "Fabric Port WWN (fWWN)*")
        ((s-equals? nameformat "8") "IPv4*")
        ((s-equals? nameformat "9") "IPv6*")
        ((s-equals? nameformat "10") "Remote Switch WWN (SWWN)*")
        ((s-equals? nameformat "11") "Interface with DomainID*")
        ((s-equals? nameformat "12") "Symbolic node name*")
	((s-equals? nameformat "13") "Device Alias*")
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

(defun csv-lens-cell-format-topology (topology)
  (or
   (assoc-default topology
		  '(("0" . "Unknown*")
		    ("1" . "Private Loop*")
		    ("2" . "Public Loop*")
		    ("3" . "Point to Point*")
		    ("4" . "Fabric*")))
   topology))

(defun csv-lens-cell-format-configuration (config)
  (or
   (assoc-default config
		  '(("0" . "Unknown*")
		    ("1" . "Valid*")
		    ("2" . "Invalid*")))
   config))

(defun csv-lens-cell-format-mode (mode)
  (or
   (assoc-default mode
		  '(("0" . "Suspended*")
		    ("1" . "Target*")
		    ("2" . "Initiatior*")))
   mode))

(defun csv-lens-cell-format-port-type (port-type)
  "Return a nicely formatted PORT-TYPE.
This mapping is copied from:

http://www.cisco.com/c/en/us/td/docs/switches/datacenter/mds9000/sw/5_2/programming/guides/smi-s/smi_s.pdf"
  (interactive)
  (or
   (assoc-default port-type
		  '(("0" . "Unknown*")
		    ("1" . "Other*")
		    ("10" . "N*")
		    ("11" . "NL*")
		    ("12" . "F/NL*")
		    ("12" . "Nx*")
		    ("14" . "E*")
		    ("15" . "F*")
		    ("16" . "FL*")
		    ("17" . "B*")
		    ("18" . "G*")
		    ("16004" . "PortChannel*")
		    ("16010" . "FCIP*")
		    ("16011" . "ISCSI-F*")
		    ("16012" . "ISCSI-N*")))
   port-type))

(defun csv-lens-cell-format-port-availabity (availability)
  (interactive)
  (or
   (assoc-default availability
		  '(("0" . "Unknown*")
		    ("1" . "Available*")
		    ("2" . "Not Installed*")
		    ("3" . "No Transceiver*")
		    ("4" . "Incompatible Transceiver*")
		    ("5" . "Not Licensed*")
		    ("6" . "DMTF Reserverd*")))
   availability))


(defun csv-lens-cell-format-link-technology (technology)
  (interactive)
  (or
   (assoc-default technology
		  '(("0" . "Unknown*")
		    ("1" . "Other*")
		    ("2" . "Ethernet*")
		    ("3" . "IB*")
		    ("4" . "FC*")
		    ("5" . "FDDI*")
		    ("6" . "ATM*")
		    ("7" . "Token Ring*")
		    ("8" . "Frame Relay*")
		    ("9" . "Infrared*")
		    ("10" . "BlueTooth*")
		    ("11" . "Wireless LAN*")))
   technology))

(defun csv-lens-cell-format-detailed-port-state (port-state)
  (interactive)
  (or
   (assoc-default port-state
		  '(("0" . "Unknown*")
		    ("1" . "Other*")
		    ("2" . "Transceiver Validation*")
		    ("3" . "Locking To Reference Signal*")
		    ("4" . "Sync*")
		    ("5" . "Invalid Receiver*")
		    ("6" . "No Light*")
		    ("7" . "No Sync*")
		    ("8" . "Laser Fault*")
		    ("9" . "Port Fault*")
		    ("10" . "Diagnostic Fault")))
   port-state))


(defun csv-lens-cell-format-fc4-types (type)
  (or
   (assoc-default type
		  '(("0" . "Unknown*")
		    ("1" . "Other*")
		    ("4" . "ISO/IEC 8802 - 2 LLC*")
		    ("5" . "IP over FC*")
		    ("8" . "SCSI - FCP*")
		    ("9" . "SCSI - GPP*")
		    ("17" . "IPI - 3 Master*")
		    ("18" . "IPI - 3 Slave*")
		    ("19" . "IPI - 3 Peer*")
		    ("21" . "CP IPI - 3 Master*")
		    ("22" . "CP IPI - 3 Slave*")
		    ("23" . "CP IPI - 3 Peer*")
		    ("25" . "SBCCS Channel*")
		    ("26" . "SBCCS Control Unit*")
		    ("27" . "FC-SB-2 Channel*")
		    ("28" . "FC-SB-2 Control Unit*")
		    ("32" . "Fibre Channel Services (FC-GS, FC-GS-2, FC-GS-3)*")
		    ("34" . "FC-SW*")
		    ("36" . "FC - SNMP*")
		    ("64" . "HIPPI - FP*")
		    ("80" . "BBL Control*")
		    ("81" . "BBL FDDI Encapsulated LAN PDU*")
		    ("82" . "BBL 802.3 Encapsulated LAN PDU*")
		    ("88" . "FC - VI*")
		    ("96" . "FC - AV*")
		    ("255" . "Vendor Unique*")))
   type))


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

(defun csv-lens-cell-format-operational-status (status)
  "Returns the operation STATUS as string.
This data is taken for the TPD_FCPort for the 3PAR.  See the 3PAR classes file"
  (interactive)
  (or
   (assoc-default status
		  '(("0" . "Unknown*")
		    ("1" . "Other*")
		    ("2" . "OK*")
		    ("3" . "Degraded*")
		    ("4" . "Stressed*")
		    ("5" . "Predictive Failure*")
		    ("6" . "Error*")
		    ("7" . "Non-Recoverable Error*")
		    ("8" . "Starting*")
		    ("9" . "Stopping*")
		    ("10" . "Stopped*")
		    ("11" . "In Service*")
		    ("12" . "No Contact*")
		    ("13" . "Lost Communication*")
		    ("14" . "Aborted*")
		    ("15" . "Dormant*")
		    ("16" . "Supporting Entity in Error*")
		    ("17" . "Completed*")
		    ("18" . "Power Mode*")
		    ("19" . "Relocating*")))
   status))



(defun csv-lens-cell-format-port-discriminator (status)
  ""
  (interactive)
  (or
   (assoc-default status
		  '(("0" . "Unknown*")
		    ("2" . "Not Applicable*")
		    ("3" . "VF*")
		    ("4" . "FCIP*")
		    ("5" . "IFR*")
		    ("6" . "IFR (Virtual)*")
		    ("7" . "NPIV*")
		    ("8" . "Internal*")
		    ("9" . "Chassis*")
		    ("10" . "FCoE*")
		    ("11" . "FC (Native)*")
		    ("12" . "HBA*")
		    ("13" . "Storage*")))
   status))

(defun csv-lens-cell-format-enabled-default (status)
  ""
  (interactive)
  (or
   (assoc-default status
		  '(("2" . "Enabled*")
		    ("3" . "Disabled*")
		    ("5" . "Not Applicable*")
		    ("6" . "Enabled but Offline*")
		    ("7" . "No Default*")
		    ("9" . "Quiesce*")))
   status))

(defun csv-lens-cell-format-enabled-state (status)
  ""
  (interactive)
  (or
   (assoc-default status
		  '(("0" . "Unknown*")
		    ("1" . "Other*")
		    ("2" . "Enabled*")
		    ("3" . "Disabled*")
		    ("4" . "Shutting Down*")
		    ("5" . "Not Applicable*")
		    ("6" . "Enabled but Offline*")
		    ("7" . "In Test*")
		    ("8" . "Deferred*")
		    ("9" . "Quiesce*")
		    ("10" . "Starting*")
		    ))
   status))


(defun csv-lens-cell-format-requested-state (status)
  ""
  (interactive)
  (or
   (assoc-default status
		  '(("0" . "Unknown*")
		    ("2" . "Enabled*")
		    ("3" . "Disabled*")
		    ("4" . "Shut Down*")
		    ("5" . "No Change*")
		    ("6" . "Offline*")
		    ("7" . "Test*")
		    ("8" . "Deferred*")
		    ("9" . "Quiesce*")
		    ("10" . "Reboot*")
		    ("11" . "Reset")
		    ("12" . "Not Applicable*")
		    ))
   status))

(defun csv-lens-cell-format-device-type-connected (type)
  "Returns the device TYPE of the other side.
This data is taken for the TPD_FCPort for the 3PAR.  See the 3PAR classes file"
  (interactive)
  (or
   (assoc-default type '(
			   ("0" . "Free*")
			   ("1" . "Host*")
			   ("2" . "Disk*")
			   ("3" . "IPort*")
			   ("4" . "FCRC*")
			   ("7" . "Peer*")))
		  type))


    
(defun csv-lens-cell-format-volumetype (type)
  (interactive)
  (or 
   (assoc-default type
		  '(("0" . "Unknown*")
		    ("1" . "Base*")
		    ("2" . "Physical Copy*")
		    ("3" . "Virtual Copy*")
		    ("4" . "Remote Copy*")))
   type))

(defun csv-lens-cell-format-space-limit-determination (type)
  (or
   (assoc-default type
		  '(("2" . "Allocated*")
		    ("3" . "Quota*")
		    ("4" . "Limitless")))
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
	   
	   (("NumberOfBlocks" "ConsumableBlocks" "IM_NumberOfBlocks") 
	    :format-function csv-lens-cell-format-big-number-of-blocks)

	   ("Capacity" :format-function csv-lens-cell-format-big-number-of-bytes)
	   
	   (("EMCKBytesSPBWritten" "EMCKBytesSPAWritten" 
	     "EMCKBytesSPBRead" "EMCKBytesSPARead" 
	     "KBytesWritten" "KBytesTransferred" "KBytesRead") 
	    :format-function csv-lens-cell-format-big-number-of-kilobytes)
	   
	   (("RequestedSpeed" "Speed" "MaxSpeed" "EMCSpaceConsumed" "IM_SpaceConsumed"
	     "RemainingManagedSpace" "SpaceLimit" "TotalManagedSpace" "ThinProvisionMetaDataSpace"
	     "EMCMetaDataAllocatedCapacity" "EMCMetaDataSubscribedCapacity" "ReservedSpace") 
	    :format-function csv-lens-cell-format-big-number-of-bytes)

	   ("Nameformat" :format-function csv-lens-cell-format-nameformat)
	   
	   (("PortType" "AntecedentFCPortType" "DependentFCPortType")
	    :format-function csv-lens-cell-format-port-type)

	   ("PortAvailability" :format-function csv-lens-cell-format-port-availabity)

	   ("DetailedPortState" :format-function csv-lens-cell-format-detailed-port-state)
	   
	   ("ActiveFC4Types" :format-function csv-lens-cell-format-fc4-types)

	   ("LinkTechnology" :format-function csv-lens-cell-format-link-technology)
	   
	   ("ConnectivityMemberType" :format-function  csv-lens-cell-format-connectivitymembertype-format)
	   
	   ("EnabledDefault" :format-function csv-lens-cell-format-enabled-default)
	   ("EnabledState" :format-function csv-lens-cell-format-enabled-state)
	   (("RequestedState" "TransitioningToState") :format-function csv-lens-cell-format-requested-state)
	   ("PortDiscriminator" :format-function csv-lens-cell-format-port-discriminator)

	   ("OperationalStatus" :format-function csv-lens-cell-format-operational-status)
	   ("DeviceTypeConnected" :format-function csv-lens-cell-format-device-type-connected)

	   ("DeviceID" :format-function csv-lens-cell-brocade-port-wwn)
	   
	   (("OtherIdentifyingInfo" "EMCWWN" 
	     "AntecedentFCPortWWN" "AntecedentElementWWN" 
	     "DependentFCPortWWN" "DependentElementWWN" 
	     "ElementName" ; "DeviceID"
	     "Name"
	     "SwitchWWPN" "PermanentAddress"
	     "IM_WWNOfExternalVolume"
	     "ConnectivityMemberID"
	     "PreferredWWPN" "ActiveWWPN" "port*sas_wwn" "port*fcoe_wwpn" "port*wwpn" "WWN" "SystemName"
	     "SwitchWWN" "SwitchPortWWN") 
	    :format-function csv-lens-cell-format-wwn)

	   ("Topology" :format-function csv-lens-cell-format-topology)
	   ("Configuration" :format-function csv-lens-cell-format-configuration)
	   ("Mode" :format-function csv-lens-cell-format-mode)
	   ))
	   
	  
	("netapp"
	 ((("size-total" "size-used" "size-available" "filesystem-size"
	    "recv_data" "send_data") :format-function csv-lens-cell-format-big-number-of-bytes)
	  (("instance_name" "instance_uuid") :format-function csv-lens-cell-netapp-format-wwn)))))

(provide 'smis-csv-lens)
;;; smis-csv-lens.el ends her
