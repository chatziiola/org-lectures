;;; create-course-tests.el --- Tests for course creation -*- lexical-binding: t -*-

(require 'test-helper)

(ert-deftest org-lectures--create-new-course-internal-defaults-test ()
  "Test `org-lectures--create-new-course-internal' with default arguments."
  (with-temp-lectures-dir
    (let ((org-lectures-default-institution "Test University")
          (org-lectures-current-semester "Fall 2025"))
      (let* ((course-name "TOC")
             (created-file (org-lectures--create-new-course-internal course-name)))
        ;; Verify file content
        (with-temp-buffer
          (insert-file-contents created-file)
          (org-mode)
          (should (string-equal (org-lectures-index-get-keyword-value "TITLE") ""))
          (should (string-equal (org-lectures-index-get-keyword-value "PROFESSOR") "")))
        ;; Verify index update
        (let* ((index (org-lectures-index-get))
               (course-entry (assoc course-name index)))
          (should (string-equal (plist-get (cdr course-entry) :title) ""))
          (should (string-equal (plist-get (cdr course-entry) :professor) "")))))))

(ert-deftest org-lectures--create-new-course-internal-with-args-test ()
  "Test `org-lectures--create-new-course-internal' with title and professor."
  (with-temp-lectures-dir
    (let ((org-lectures-default-institution "Test University")
          (org-lectures-current-semester "Fall 2025"))
      (let* ((course-name "ALG")
             (title "Algorithms")
             (prof "Dr. Cormen")
             (created-file (org-lectures--create-new-course-internal course-name title prof)))
        ;; Verify file content
        (with-temp-buffer
          (insert-file-contents created-file)
          (org-mode)
          (should (string-equal (org-lectures-index-get-keyword-value "TITLE") title))
          (should (string-equal (org-lectures-index-get-keyword-value "PROFESSOR") prof)))
        ;; Verify index update
        (let* ((index (org-lectures-index-get))
               (course-entry (assoc course-name index)))
          (should (string-equal (plist-get (cdr course-entry) :title) title))
          (should (string-equal (plist-get (cdr course-entry) :professor) prof)))))))
