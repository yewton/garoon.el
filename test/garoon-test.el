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

(provide 'garoon-test)

(ert-deftest grn:api-base-url-test ()
  (should (string= grn:api-base-url "http://onlinedemo2.cybozu.co.jp/scripts/garoon3/grn.exe/cbapi/")))

;;; garoon-test.el ends here
;; Local Variables:
;; eval: (auto-async-byte-compile-mode -1)
;; End:
