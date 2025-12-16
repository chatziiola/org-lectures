;;; path-helpers-tests.el --- Tests for file path helpers -*- lexical-binding: t -*-

(require 'test-helper)

(ert-deftest org-lectures-path-helpers-test ()
  "Test the file path helper functions."
  (with-temp-lectures-dir
    ;; `org-lectures-dir' is now bound to a temporary directory
    ;; by the `with-temp-lectures-dir` macro.

    ;; Test org-lectures-get-course-info-file
    ;; Case 1: No pre-existing lowercase file
    (should (string-equal (org-lectures-get-course-info-file "EMACS")
                          (expand-file-name "course_EMACS.org" org-lectures-dir)))

    ;; Case 2: Pre-existing lowercase file (remnants of a shady past)
    (let ((expected-lowercase-file (expand-file-name "course_emacs.org" org-lectures-dir)))
      (unwind-protect
          (progn
            ;; Manually create the file to ensure it exists.
            (write-region "" nil expected-lowercase-file)
            (should (string-equal (org-lectures-get-course-info-file "EMACS")
                                  expected-lowercase-file)))
        ;; Cleanup the manually created file.
        (when (file-exists-p expected-lowercase-file)
          (delete-file expected-lowercase-file))))

    ;; Test org-lectures-get-course-lectures-dir
    (should (string-equal (org-lectures-get-course-lectures-dir "EMACS")
                          (expand-file-name "course_EMACS" org-lectures-dir)))))
