;;; garoon-it.el --- Integration test for garoon.el -*- lexical-binding: t -*-

;; Copyright (C) 2014 yewton

;; Author: yewton <yewton@gmail.com>
;; Version: 0.0.1
;; Keywords: test garoon

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

;; Integration test for garoon.el

;;; Code:

(require 's)

(ert-deftest grn:get-schedule-events-test ()
  (grn:get-schedule-events (safe-date-to-time "2014-09-22 00:00:00"))
  (with-current-buffer "*GAROON EVENTS*"
    (let ((actual (buffer-string))
          (expected (s-join "\n"
                            '("* [[http://onlinedemo2.cybozu.info/scripts/garoon3/grn.exe/schedule/view?event%3D259&bdate%3D2014-09-22][サイボウズ株式会社]]"
                              "  :PROPERTIES:"
                              "  :FACILITIES: 社用車１"
                              "  :PLAN:     【往訪】"
                              "  :END:"
                              "[2014-09-22 月 10:00]--[2014-09-22 月 12:00]"
                              ""
                              "* [[http://onlinedemo2.cybozu.info/scripts/garoon3/grn.exe/schedule/view?event%3D260&bdate%3D2014-09-22][マネージャーミーティング]]"
                              "  :PROPERTIES:"
                              "  :FACILITIES: プロジェクターB / 第二会議室"
                              "  :PLAN:     【会議】"
                              "  :END:"
                              "[2014-09-22 月 15:00]--[2014-09-22 月 17:00]"
                              ""
                              "* [[http://onlinedemo2.cybozu.info/scripts/garoon3/grn.exe/schedule/view?event%3D261&bdate%3D2014-09-22][ディベート]]"
                              "  :PROPERTIES:"
                              "  :FACILITIES: セミナールーム"
                              "  :PLAN:     【会議】"
                              "  :END:"
                              "[2014-09-22 月 17:00]--[2014-09-22 月 18:30]"
                              ""
                              ""))))
      (should (string= actual expected)))))
;;; garoon-it.el ends here
