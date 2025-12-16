;;; test-helper.el --- Helper for running org-lectures tests. -*- lexical-binding: t -*-

;; Add project root to load-path to find the source files.
(add-to-list 'load-path (file-name-directory (directory-file-name (file-name-directory load-file-name))))

;; Add tests directory to load-path for helper files.
(add-to-list 'load-path (file-name-directory (directory-file-name load-file-name)))

(require 'ert)
(require 'org-lectures)
(require 'org-lectures-index)

(defmacro with-temp-lectures-dir (&rest body)
  "Create a temporary `org-lectures-dir' and run BODY inside it.
The directory is created recursively and deleted after execution.
`org-lectures-dir' is lexically bound to the temporary path."
  `(let* ((temp-dir-name "org-lectures-tests_")
          (temp-parent temporary-file-directory)
          (org-lectures-dir (make-temp-file temp-dir-name t)))
     ;; make-temp-file with a non-nil third arg creates a directory
     (unwind-protect
         (progn ,@body)
       (delete-directory org-lectures-dir t))))


(provide 'test-helper)
