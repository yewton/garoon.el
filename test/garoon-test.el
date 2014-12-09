;;; garoon-test.el --- Test for garoon.el -*- lexical-binding: t -*-

;; Copyright (C) 2014 yewton

;; Author: yewton <yewton@gmail.com>
;; Version: 0.0.1
;; Keywords: test

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

;; Test for garoon.el

;;; Code:

(require 's)

(fset 'sdt 'safe-date-to-time)

;;;; Structures.

(ert-deftest grn:api-base-url-test ()
  (should (string= (grn:api-base-url) "http://onlinedemo2.cybozu.info/scripts/garoon3/grn.exe/cbpapi/")))

(ert-deftest grn:action-test ()
  (let* ((name "name")
         (parameters "parameters")
         (action (grn:schedule-action-new name parameters)))
    (should (string= (grn:action-name action) name))
    (should (string= (grn:action-parameters action) parameters))
    (should (string= (grn:action-service action) "schedule"))
    (should (string= (grn:action-url action) "http://onlinedemo2.cybozu.info/scripts/garoon3/grn.exe/cbpapi/schedule/api?"))
    (should (string= (grn:action-response-node-name action) "schedule:nameResponse"))))

;;;; Utilities.

(ert-deftest grn:insert-org-time-range-test ()
  (let ((start (sdt "2014-09-22 00:00:00"))
        (end (sdt "2014-09-22 23:59:59"))
        (current-time (sdt "2014-09-22 12:00:00")))
  (with-temp-buffer
    (grn:insert-org-time-range start end current-time)
    (should (string= (buffer-string) "<2014-09-22 月 00:00>--<2014-09-22 月 23:59>")))))

(ert-deftest grn:time-set-test ()
  (let ((time (sdt "2014-09-22 00:00:00")))
    (should (equal (grn:time-set time :hour 12) (sdt "2014-09-22 12:00:00")))
    (should (equal (grn:time-set time :min 42) (sdt "2014-09-22 00:42:00")))
    (should (equal (grn:time-set time :sec 24) (sdt "2014-09-22 00:00:24")))
    (should (equal (grn:time-set time :hour 12 :min 42) (sdt "2014-09-22 12:42:00")))
    (should (equal (grn:time-set time :hour 12 :sec 24) (sdt "2014-09-22 12:00:24")))
    (should (equal (grn:time-set time :min 42 :sec 24) (sdt "2014-09-22 00:42:24")))
    (should (equal (grn:time-set time :hour 12 :min 42 :sec 24) (sdt "2014-09-22 12:42:24")))))

(ert-deftest grn:time-start-of-day-test ()
  (let ((time (sdt "2014-09-22 12:49:33")))
    (should (equal (grn:time-start-of-day time) (sdt "2014-09-22 00:00:00")))))

(ert-deftest grn:time-end-of-day-test ()
  (let ((time (sdt "2005-12-31 12:49:33")))
    (should (equal (grn:time-end-of-day time) (sdt "2005-12-31 23:59:59")))))

(ert-deftest grn:format-time-string-test ()
  (let ((time (sdt "2014-09-22 12:34:56")))
    (should (string= (grn:format-time-string time) "2014-09-22T12:34:56Z"))))

;;;; Envelope construction.

(ert-deftest grn:create-timestamp-element-test ()
  (let* ((create-time (safe-date-to-time "2014-09-19 00:00:00"))
         (actual (grn:create-timestamp-element create-time grn:expire-time)))
    (should
     (string= actual
              (s-join "\n"
                      '("<Timestamp SOAP-ENV:mustUnderstand=\"1\" Id=\"id\""
                        "xmlns=\"http://schemas.xmlsoap.org/ws/2002/07/utility\">"
                        "<Created>2014-09-18T15:00:00Z</Created>"
                        "<Expires>2037-12-01T00:00:00Z</Expires>"
                        "</Timestamp>"))))))

(ert-deftest grn:create-security-element-test ()
  (should
   (string= (grn:create-security-element)
            (s-join "\n"
                    '("<Security xmlns:wsu=\"http://schemas.xmlsoap.org/ws/2002/07/utility\""
                      "SOAP-ENV:mustUnderstand=\"1\""
                      "xmlns=\"http://schemas.xmlsoap.org/ws/2002/12/secext\">"
                      "<UsernameToken wsu:Id=\"id\">"
                      "<Username>sato</Username>"
                      "<Password>sato</Password>"
                      "</UsernameToken>"
                      "</Security>")))))

(ert-deftest grn:create-action-element-test ()
  (should (string= (grn:create-action-element "Test")
                   (s-join "\n"
                           '("<Action SOAP-ENV:mustUnderstand=\"1\""
                             "xmlns=\"http://schemas.xmlsoap.org/ws/2003/03/addressing\">"
                             "Test"
                             "</Action>")))))

(ert-deftest grn:create-simple-parameters-element-test ()
  (should (string= (grn:create-simple-parameters-element)
                   "<parameters\n\n>\n</parameters>"))
  (let ((params '((hoge . "fuga")
                  (mofu ."mofu"))))
    (should (string= (grn:create-simple-parameters-element params)
                     (s-join "\n"
                             '("<parameters"
                               "hoge=\"fuga\" mofu=\"mofu\""
                               ">"
                               "</parameters>"))))))

(defun test-action ()
  (grn:action-new "TestAction" "test" (grn:create-simple-parameters-element)))

(ert-deftest grn:create-envelope-test ()
  (let ((action (test-action))
        (create-time (sdt "2014-09-22 12:34:56"))
        (expire-time (sdt "2036-02-06 15:28:15")))
    (should (string=
             (grn:create-envelope action create-time expire-time)
             (s-join "\n"
                     '("<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
                       "<SOAP-ENV:Envelope xmlns:SOAP-ENV=\"http://www.w3.org/2003/05/soap-envelope\">"
                       "<SOAP-ENV:Header>"
                       "<Action SOAP-ENV:mustUnderstand=\"1\""
                       "xmlns=\"http://schemas.xmlsoap.org/ws/2003/03/addressing\">"
                       "TestAction"
                       "</Action>"
                       "<Security xmlns:wsu=\"http://schemas.xmlsoap.org/ws/2002/07/utility\""
                       "SOAP-ENV:mustUnderstand=\"1\""
                       "xmlns=\"http://schemas.xmlsoap.org/ws/2002/12/secext\">"
                       "<UsernameToken wsu:Id=\"id\">"
                       "<Username>sato</Username>"
                       "<Password>sato</Password>"
                       "</UsernameToken>"
                       "</Security>"
                       "<Timestamp SOAP-ENV:mustUnderstand=\"1\" Id=\"id\""
                       "xmlns=\"http://schemas.xmlsoap.org/ws/2002/07/utility\">"
                       "<Created>2014-09-22T03:34:56Z</Created>"
                       "<Expires>2036-02-06T06:28:15Z</Expires>"
                       "</Timestamp>"
                       "<Locale>jp</Locale>"
                       "</SOAP-ENV:Header>"
                       "<SOAP-ENV:Body>"
                       "<TestAction>"
                       "<parameters"
                       ""
                       ">"
                       "</parameters>"
                       "</TestAction>"
                       "</SOAP-ENV:Body>"
                       "</SOAP-ENV:Envelope>"))))))

;;;; Actions.

(defvar dummy-schedule-event-response
  (car (xml-parse-file
        (g-test:resource-file-name "schedule-event-response.xml"))))

(ert-deftest grn:get-body-test ()
  (let ((actual (grn:get-body dummy-schedule-event-response)))
    (should-not (null
                 (xml-get-children actual
                                   'schedule:ScheduleGetEventsResponse)))))

(ert-deftest grn:get-returns-test ()
  (let ((actual (grn:get-returns dummy-schedule-event-response
                                 'schedule:ScheduleGetEventsResponse)))
    (should-not (null (xml-get-children actual 'schedule_event)))))

(ert-deftest grn:create-simple-parameters-element-test ()
  (should (string= (grn:create-simple-parameters-element)
                   (s-join "\n"
                           '("<parameters"
                             ""
                             ">"
                             "</parameters>"))))
  (let ((parameters '((hoge . "piyo")
                      (fizz . "buzz"))))
    (should (string= (grn:create-simple-parameters-element parameters)
                     (s-join "\n"
                             '("<parameters"
                               "hoge=\"piyo\" fizz=\"buzz\""
                               ">"
                               "</parameters>"))))))

;;;; Sub functions for `grn:get-schedule-events'
(defvar dummy-normal-event-response
  (car (xml-parse-file
        (g-test:resource-file-name "normal-event.xml"))))

(ert-deftest grn:get-facilities-test ()
  (let* ((event-node dummy-normal-event-response)
         (actual (grn:get-facilities event-node)))
    (should (equal actual
                   '("テスト施設")))))

(defvar dummy-repeat-event-response
  (car (xml-parse-file
        (g-test:resource-file-name "repeat-event.xml"))))

(ert-deftest grn:handle-repeat-event-test ()
  (with-temp-buffer
    (let* ((event dummy-repeat-event-response)
           (datetime (sdt "2014-09-22 00:00:00"))
           (current-time (sdt "2014-09-22 11:00:00")))
      (grn:handle-repeat-event datetime event current-time)
      (should (string= (buffer-string)
                       "<2014-09-22 月 10:00>--<2014-09-22 月 12:00>")))))

(ert-deftest grn:handle-normal-event-test ()
  (with-temp-buffer
    (let* ((event dummy-normal-event-response)
           (datetime (sdt "2014-09-22 00:00:00"))
           (current-time (sdt "2014-09-22 11:00:00")))
      (grn:handle-normal-event datetime event current-time)
      (should (string= (buffer-string)
                       "<2014-09-22 月 10:00>--<2014-09-22 月 12:00>")))))

(ert-deftest grn:handle-event-test ()
  (with-temp-buffer
    (let* ((event dummy-normal-event-response)
           (datetime (sdt "2014-09-22 00:00:00"))
           (current-time (sdt "2014-09-22 11:00:00")))
      (grn:handle-event datetime event current-time)
      (should (string= (buffer-string)
                       (s-join "\n"
                               '("* [[http://onlinedemo2.cybozu.info/scripts/garoon3/grn.exe/schedule/view?event%3D252525&bdate%3D2014-09-22][テストイベント]]"
                                 "  :PROPERTIES:"
                                 "  :FACILITIES: テスト施設"
                                 "  :PLAN:     予定"
                                 "  :END:"
                                 "<2014-09-22 月 10:00>--<2014-09-22 月 12:00>"
                                 ""
                                 "")))))))

;;; garoon-test.el ends here
