(defvar g-test/test-path
  (directory-file-name (file-name-directory load-file-name))
  "Path to tests directory.")

(defvar g-test/root-path
  (directory-file-name (file-name-directory g-test/test-path))
  "Path to root directory.")

(defvar g-test/vendor-path
  (expand-file-name "vendor" g-test/root-path)
  "Path to vendor directory.")

(load (expand-file-name "garoon" g-test/root-path) 'noerror 'nomessage)

(unless (require 'ert nil t)
  (require 'ert (expand-file-name "ert" g-test/vendor-path)))

(custom-set-variables
 '(grn:username "sato")
 '(grn:password "sato"))

;; Local Variables:
;; eval: (auto-async-byte-compile-mode -1)
;; End:
