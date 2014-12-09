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
  (let ((time (current-time)))
    (grn:get-schedule-events time)
    (with-current-buffer "*GAROON EVENTS*"
      (let ((actual (buffer-string))
            (datestr (concat "<" (format-time-string "%Y-%m-%d" time))))
        (should (not (null (string-match datestr actual))))))))
;;; garoon-it.el ends here
