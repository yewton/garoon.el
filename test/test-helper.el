(defvar g-test:test-path
  (directory-file-name (file-name-directory load-file-name))
  "Path to tests directory.")

(defvar g-test:root-path
  (directory-file-name (file-name-directory g-test:test-path))
  "Path to root directory.")

(defvar g-test:resource-path
  (directory-file-name
   (expand-file-name "resources" g-test:test-path))
  "Path to resources directory.")

(defun g-test:resource-file-name (file)
  (expand-file-name file g-test:resource-path))

(custom-set-variables
 '(grn:username "sato")
 '(grn:password "sato")
 '(grn:debug t))

(load (expand-file-name "garoon" g-test:root-path) 'noerror 'nomessage)
