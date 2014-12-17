;;; garoon.el --- A Garoon client. -*- lexical-binding: t -*-

;; Copyright (C) 2014 yewton

;; Author: yewton <yewton@gmail.com>
;; Version: 0.0.1
;; Keywords: garoon tool gpg

;; This program is free software; you can redistribute it and/or
;; modify it under the terms of the GNU General Public License as
;; published by the Free Software Foundation; either version 2, or (at
;; your option) any later version.

;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
;; General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING. If not, write to the
;; Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA.

;;; Commentary:

;; You need gpg 的ななにか

;; A Garron client.

;;; Code:

(eval-when-compile (require 'cl-lib)
                   (require 'dash)
                   (require 'ht))
(require 'mm-decode)
(require 'org)
(require 's)
(require 'soap-client)
(require 'url)
(require 'xml)

;;;; Variables.
(defvar grn--endpoint-host)
(defvar grn--endpoint-port)

;;;; Customize.

(defgroup garoon nil
  "Access Garoon from Emacs."
  :group 'tools)

(defcustom grn-endpoint-url
  "http://onlinedemo2.cybozu.info/scripts/garoon3/grn.exe"
  "The location for the Garoon service."
  :type 'string
  :group 'garoon
  :set (lambda (symbol value)
         (let ((url (url-generic-parse-url value)))
           (setq grn--endpoint-host (url-host url))
           (setq grn--endpoint-port (url-port url)))
         (set-default symbol value)))

(defcustom grn-expire-time
  (encode-time 0 0 0 31 11 2037 t)
  "Default expire time."
  :type (list 'integer 'integer)
  :group 'garoon)

(defcustom grn-prefix-for-temporary-events
  (format "(%c)" (decode-char 'ucs #x4eee))
  "Prefix for temporary event"
  :type 'string
  :group 'garoon)

(defcustom grn-property-name-for-facilities
  "FACILITIES"
  "Property name for facilities."
  :type 'string
  :group 'garoon)

(defcustom grn-property-name-for-plans
  "PLAN"
  "Property name for plans."
  :type 'string
  :group 'garoon)

(defcustom grn-locale
  "jp"
  "Locale."
  :type 'string
  :group 'garoon)

(defcustom grn-debug
  nil
  "When t, enable some debugging facilities."
  :type 'boolean
  :group 'garoon)

;;;; Constants.

(defconst grn-soap-env-ns "http://www.w3.org/2003/05/soap-envelope")
(defconst grn-soap-addr-ns "http://schemas.xmlsoap.org/ws/2003/03/addressing")
(defconst grn-soap-sec-ns "http://schemas.xmlsoap.org/ws/2002/12/secext")
(defconst grn-soap-util-ns "http://schemas.xmlsoap.org/ws/2002/07/utility")
(defconst grn-date-format "%Y-%m-%dT%H:%M:%SZ")
(defconst grn-schedule-api-path "schedule/api?")

;;;; Structures.

(cl-defstruct (grn-action
               (:constructor grn--action-new (name service parameters))
               (:constructor grn-schedule-action-new
                             (name parameters &aux (service "schedule")))
               (:copier grn-action-copy))
  name service parameters)

(defun grn--api-base-url ()
  (format "%s%s" grn-endpoint-url "/cbpapi/"))

(defun grn-action-url (action)
  (format "%s%s/api?" (grn--api-base-url) (grn-action-service action)))

(defun grn-action-response-node-name (action)
  (intern (format "%s:%sResponse"
                  (grn-action-service action)
                  (grn-action-name action))))

;;;; Utilities.

(defun grn--credentials ()
  (let* ((auth-source-creation-prompts
          '((user  . "Garoon user: ")
            (secret . "Garoon password for %u: ")))
         (found (cl-first (auth-source-search :max 1
                                              :host grn--endpoint-host
                                              :port grn--endpoint-port
                                              :require '(:user :secret)
                                              :create t))))
    (when found
      (eval `(ht ,@(--map `(,it ,(plist-get found it))
                          '(:user :secret :save-function)))))))

(defun grn--credentials-username (credentials) (ht-get credentials :user))

(defun grn--credentials-password (credentials)
  (let ((secret (ht-get credentials :secret)))
    (if (functionp secret) (funcall secret) secret)))

(defun grn--credentials-save (credentials)
  (let ((save-function (ht-get credentials :save-function)))
    (when (functionp save-function) (funcall save-function))))

(defun grn--insert-org-timestamp (time &optional with-hm current-time)
  (org-insert-time-stamp time with-hm))

(defun grn--insert-org-time-range (start end &optional current-time)
  (grn--insert-org-timestamp start t current-time)
  (insert "--")
  (grn--insert-org-timestamp end t current-time))

(cl-defun grn--time-set (time &key hour min sec)
  (let* ((decoded (decode-time time))
         (hour (or hour (cl-third decoded)))
         (min (or min (cl-second decoded)))
         (sec (or sec (cl-first decoded))))
    (apply 'encode-time `(,sec ,min ,hour ,@(cl-cdddr decoded)))))

(defun grn--time-start-of-day (time)
  (grn--time-set time :hour 0 :min 0 :sec 0))

(defun grn--time-end-of-day (time)
  (grn--time-set time :hour 24 :min 00 :sec -1))

(defun grn--format-time-string (time &optional universal)
  (format-time-string grn-date-format time universal))

;;;; Envelope construction.

(defun grn--create-timestamp-element (create-time expire-time)
  (s-join "\n"
          `("<Timestamp SOAP-ENV:mustUnderstand=\"1\" Id=\"id\""
            ,(format "xmlns=\"%s\">" grn-soap-util-ns)
            ,(format "<Created>%s</Created>" (grn--format-time-string create-time t))
            ,(format "<Expires>%s</Expires>" (grn--format-time-string expire-time t))
            "</Timestamp>")))

(defun grn-get-password ()
  (or grn-password (read-passwd "Password: ")))

(defun grn--create-security-element (credentials)
  (s-join "\n"
          `(,(format "<Security xmlns:wsu=\"%s\"" grn-soap-util-ns)
            "SOAP-ENV:mustUnderstand=\"1\""
            ,(format "xmlns=\"%s\">" grn-soap-sec-ns)
            "<UsernameToken wsu:Id=\"id\">"
            ,(format "<Username>%s</Username>" (grn--credentials-username credentials))
            ,(format "<Password>%s</Password>" (grn--credentials-password credentials))
            "</UsernameToken>"
            "</Security>")))

(defun grn--create-action-element (name)
  (s-join "\n"
          `("<Action SOAP-ENV:mustUnderstand=\"1\""
            ,(format "xmlns=\"%s\">" grn-soap-addr-ns)
            ,name
            "</Action>")))

(defun grn--create-body-element (action)
  (let ((action-name (grn-action-name action)))
    (s-join "\n"
            `("<SOAP-ENV:Body>"
              ,(format "<%s>" action-name)
              ,(grn-action-parameters action)
              ,(format "</%s>" action-name)
              "</SOAP-ENV:Body>"))))

(defun grn--create-envelope (action create-time expire-time credentials)
  (s-join "\n"
          `("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            ,(format "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"%s\">" grn-soap-env-ns)
            "<SOAP-ENV:Header>"
            ,(grn--create-action-element (grn-action-name action))
            ,(grn--create-security-element credentials)
            ,(grn--create-timestamp-element create-time expire-time)
            ,(format "<Locale>%s</Locale>" grn-locale)
            "</SOAP-ENV:Header>"
            ,(grn--create-body-element action)
            "</SOAP-ENV:Envelope>")))

;;;; Actions.

(defun grn--get-body (node)
  (car (xml-get-children node 'soap:Body)))

(defun grn--get-returns (node response-node-name)
  (car (xml-get-children
        (car (xml-get-children
              (grn--get-body node)
              response-node-name))
        'returns)))

(defun grn--invoke-action (action)
  (let* ((credentials (grn--credentials))
         (url-request-method "POST")
         (url-package-name "garoon.el")
         (url-request-data
          (grn--create-envelope action (current-time) grn-expire-time credentials))
         (url-mime-charset-string "utf-8;q=1, iso-8859-1;q=0.5")
         (url-debug grn-debug)
         (url (grn-action-url action))
         buffer mime-part)
    (setq buffer (url-retrieve-synchronously url))
    (with-current-buffer buffer
      (when grn-debug
        (let ((debug-buffer (get-buffer-create "*GAROON-DEBUG*")))
          (copy-to-buffer debug-buffer (point-min) (point-max))
          (with-current-buffer debug-buffer
            (decode-coding-region (point-min) (point-max) 'utf-8))))
      (declare (special url-http-response-status))
      (when (null url-http-response-status)
        (error "No HTTP response from server"))
      (setq mime-part (mm-dissect-buffer t)))
    (kill-buffer buffer)
    (unless mime-part
      (error "Failed to decode response from server"))
    (grn--credentials-save credentials)
    (with-temp-buffer
      (mm-insert-part mime-part)
      (decode-coding-region (point-min) (point-max) 'utf-8)
      (grn--get-returns
       (car (xml-parse-region))
       (grn-action-response-node-name action)))))

(defun grn--create-simple-parameters-element (&optional params)
  (s-join "\n"
          `("<parameters"
            ,(mapconcat (lambda (param)
                          (when (stringp (cdr param))
                            (format "%s=\"%s\"" (car param) (cdr param))))
                        params
                        " ")
            ">"
            "</parameters>")))

;;;; Concrete actions.

(defun grn--schedule-get-events (start end &optional start-for-daily end-for-daily)
  (let* ((params `((start . ,start)
                  (end . ,end)
                  (start_for_daily . ,(or start-for-daily start))
                  (end_for_daily . ,(or end-for-daily end))))
         (action (grn-schedule-action-new "ScheduleGetEvents"
                                          (grn--create-simple-parameters-element params))))
     (grn--invoke-action action)))

;;;; Sub functions for `grn-get-schedule-events'

(defun grn--get-facilities (event-node)
  (let (result facility)
    (dolist (member (xml-get-children
                     (car (xml-get-children event-node 'members))
                     'member))
      (setq facility (car (xml-get-children member 'facility)))
      (when facility
        (setq result (cons (xml-get-attribute facility 'name) result))))
    result))

(defun grn--handle-repeat-event (datetime event &optional current-time)
  (let* ((condition
          (car (xml-get-children
                (car (xml-get-children event 'repeat_info))
                'condition)))
         (start-hm (mapcar 'string-to-int (s-split ":" (xml-get-attribute condition 'start_time))))
         (end-hm (mapcar 'string-to-int (s-split ":" (xml-get-attribute condition 'end_time))))
         (start (grn--time-set datetime :hour (cl-first start-hm) :min (cl-second start-hm)))
         (end (grn--time-set datetime :hour (cl-first end-hm) :min (cl-second end-hm))))
    (grn--insert-org-time-range start end current-time)))

(defun grn--handle-normal-event (datetime event &optional current-time)
  (let* ((when-node (car (xml-get-children
                          (car (xml-get-children event 'when))
                          'datetime)))
         (start (safe-date-to-time (xml-get-attribute when-node 'start)))
         (end (safe-date-to-time (xml-get-attribute when-node 'end)))
         (zero-time '(0 0)))
    (if (and (equal start zero-time) (equal end zero-time))
        (grn--insert-org-time-range
         (grn--time-start-of-day datetime)
         (grn--time-end-of-day datetime)
         current-time)
      (grn--insert-org-time-range start end current-time))))

(defun grn--require-org-mode ()
  (unless (eq 'org-mode major-mode) (org-mode)))

(defun grn--handle-event (datetime event &optional current-time)
  (let* ((event-type (xml-get-attribute event 'event_type))
         (facilities (grn--get-facilities event))
         (event-id (xml-get-attribute event 'id))
         (schedule-uri (concat grn-endpoint-url
                               (format "/schedule/view?event=%s&bdate=%s"
                                       event-id
                                       (format-time-string "%Y-%m-%d" datetime))))
         (plan (xml-get-attribute event 'plan))
         (detail (xml-get-attribute event 'detail)))
    (grn--require-org-mode)
    (cl-flet ((event-is (expected) (string= event-type (symbol-name expected))))
      (insert "* ")
      (org-insert-link nil schedule-uri detail)
      (newline)
      (cond
       ((event-is 'normal)
        (grn--handle-normal-event datetime event current-time))
       ((event-is 'repeat)
        (grn--handle-repeat-event datetime event current-time))
       ((event-is 'banner)
        (grn--insert-org-timestamp datetime current-time))
       ((event-is 'temporary)
        (insert grn-prefix-for-temporary-events)
        (grn--insert-org-timestamp datetime current-time))
       (t
        (insert event-type)))
      (when facilities
        (org-set-property grn-property-name-for-facilities (s-join " / " facilities)))
      (when (not (string= "" plan))
        (org-set-property grn-property-name-for-plans plan))
      (newline 2))))

;;;; Interactive functions.

(defun grn--quit-garoon-window (n)
  (interactive "p")
  (if (string= (buffer-name) "*GAROON EVENTS*")
      (quit-window)
    (self-insert-command n)))

;;;###autoload
(defun grn-get-schedule-events (&optional datetime)
  "Pop up a buffer showing specified date events."
  (interactive)
  (let* ((datetime (or datetime (org-read-date nil t)))
         (start (grn--format-time-string (grn--time-start-of-day datetime)))
         (end (grn--format-time-string (grn--time-end-of-day datetime)))
         (buffer (get-buffer-create "*GAROON EVENTS*"))
         (inhibit-read-only t)
         (events (grn--schedule-get-events start end start end)))
    (with-current-buffer buffer
      (org-mode)
      (erase-buffer)
      (dolist (event (xml-get-children events 'schedule_event))
        (grn--handle-event datetime event))
      (mark-whole-buffer)
      (ignore-errors (org-sort-entries nil ?t))
      (deactivate-mark)
      (goto-char (point-min))
      (local-set-key "q" #'grn--quit-garoon-window)
      (setq buffer-read-only t)
      (pop-to-buffer buffer))))

(provide 'garoon)

;;; garoon.el ends here
