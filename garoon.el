;;; garoon.el --- A Garoon client. -*- lexical-binding: t -*-

;; Copyright (C) 2014 yewton

;; Author: yewton <yewton@gmail.com>
;; Version: 0.0.1
;; Keywords: garoon tool

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

;; A Garron client.

;;; Code:

(require 's)
(eval-when-compile (require 'cl))

(defgroup garoon nil
  "Access Garoon from Emacs."
  :group 'tools)

(defcustom grn:endpoint-url
  "http://onlinedemo2.cybozu.co.jp/scripts/garoon3/grn.exe"
  "The location for the Garoon service."
  :type 'string
  :group 'garoon)

(defcustom grn:api-base-url
  nil
  "The address of the Garoon API."
  :initialize 'custom-initialize-set
  :set (lambda (s v)
         (set-default s (format "%s%s" grn:endpoint-url "/cbapi/")))
  :set-after '(grn:endpoint-url)
  :type 'string
  :group 'garoon)

(defcustom grn:username
  nil
  "Username for the Garoon service."
  :type 'string
  :group 'garoon)

(defcustom grn:password
  nil
  "Username for the Garoon service."
  :type 'string
  :group 'garoon)

(defcustom grn:expire-time
  (encode-time 0 0 0 31 11 2037 t)
  "Default expire time."
  :type (list 'integer 'integer)
  :group 'garoon)

(defconst grn:soap-env-ns "http://www.w3.org/2003/05/soap-envelope")
(defconst grn:soap-addr-ns "http://schemas.xmlsoap.org/ws/2003/03/addressing")
(defconst grn:soap-sec-ns "http://schemas.xmlsoap.org/ws/2002/12/secext")
(defconst grn:soap-util-ns "http://schemas.xmlsoap.org/ws/2002/07/utility")
(defconst grn:date-format "%Y-%m-%dT%H:%M:%SZ")

(defun grn:create-timestamp-element (create-time expire-time)
  (let ((created (format-time-string grn:date-format create-time t))
        (expires (format-time-string grn:date-format expire-time t)))
    (s-join "\n"
            `("<Timestamp SOAP-ENV:mustUnderstand=\"1\" Id=\"id\""
              ,(format "xmlns=\"%s\">" grn:soap-util-ns)
              ,(format "<Created>%s</Created>" created)
              ,(format "<Expires>%s</Expires>" expires)
              "</Timestamp>"))))

(defun grn:create-security-element ()
  (s-join "\n"
          `(,(format "<Security xmlns:wsu=\"%s\"" grn:soap-util-ns)
            "SOAP-ENV:mustUnderstand=\"1\""
            ,(format "xmlns=\"%s\">" grn:soap-sec-ns)
            "<UsernameToken wsu:Id=\"id\">"
            ,(format "<Username>%s</Username>" grn:username)
            ,(format "<Password>%s</Password>" grn:password)
            "</UsernameToken>"
            "</Security>")))

(defun grn:create-action-element (name)
  (s-join "\n"
          `("<Action SOAP-ENV:mustUnderstand=\"1\""
            ,(format "xmlns=\"%s\">" grn:soap-addr-ns)
            ,name
            "</Action>")))

(defun grn:create-parameters-element (params)
  (s-join "\n"
          `("<parameters"
            ,(mapconcat (lambda (param)
                          (when (stringp (cdr param))
                            (format "%s=\"%s\"" (car param) (cdr param))))
                        params
                        " ")
            ">"
            "</parameters>")))

(defun grn:create-body-element (action-name params)
  (s-join "\n"
          `("<SOAP-ENV:Body>"
            ,(format "<%s>" action-name)
            ,(grn:create-parameters-element params)
            ,(format "</%s>" action-name)
            "</SOAP-ENV:Body>")))

(defun grn:create-envelope (action-name params create-time expire-time)
  (s-join "\n"
          `("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            ,(format "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"%s\">" grn:soap-env-ns)
            "<SOAP-ENV:Header>"
            ,(grn:create-action-element action-name)
            ,(grn:create-security-element)
            ,(grn:create-timestamp-element create-time expire-time)
            "<Locale>jp</Locale>"
            "</SOAP-ENV:Header>"
            ,(grn:create-body-element action-name params)
            "</SOAP-ENV:Envelope>")))

;; (let* ((date "2014-09-16")
;;        (start (format "%sT00:00:00" date))
;;        (end (format "%sT23:59:59" date))
;;        (params `((start . ,start)
;;                  (end . ,end)
;;                  (start_for_daily . ,start)
;;                  (end_for_daily . ,end))))
;;   (insert (grn:create-envelope "ScheduleGetEvents" params (current-time) grn:expire-time)))

(provide 'garoon)

;;; garoon.el ends here
;; Local Variables:
;; eval: (auto-async-byte-compile-mode -1)
;; End:
