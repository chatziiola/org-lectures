;;; create-lecture-tests.el --- Tests for lecture creation -*- lexical-binding: t -*-

(require 'test-helper)

(ert-deftest org-lectures--create-new-lecture-internal-test ()
  "Test `org-lectures--create-new-lecture-internal'."
  (with-temp-lectures-dir
    (let* ((course-name "TESTCOURSE")
           (lecture-title "Test Lecture")
           (lecture-professor "Dr. Test")
           (lecture-institution "Test University")
           (note-type "lec")
           (date-str "20251216")
           (expected-filename (expand-file-name "lec_TESTCOURSE_20251216.org" (org-lectures-get-course-lectures-dir course-name))))

      ;; Create a dummy course entry in the index for the lecture to be added to
      (let* ((course-info-file (org-lectures-get-course-info-file course-name))
             (initial-course-entry `(,course-name . (:title "" :professor "" :institution ,lecture-institution :file ,course-info-file :lectures '()))))
        (org-lectures--configure-index-module)
        (push initial-course-entry org-lectures-index--cache)
        (org-lectures-index-write))

      (let ((created-file (org-lectures--create-new-lecture-internal
                           course-name
                           lecture-title
                           lecture-professor
                           lecture-institution
                           note-type
                           date-str)))
        ;; Verify file creation
        (should (file-exists-p created-file))
        (should (string-equal created-file expected-filename))

        ;; Verify file content
        (with-temp-buffer
          (insert-file-contents created-file)
          (org-mode)
          (should (string-equal (org-lectures-index-get-keyword-value "TITLE") lecture-title))
          (should (string-equal (org-lectures-index-get-keyword-value "PROFESSOR") lecture-professor))
          (should (string-equal (org-lectures-index-get-keyword-value "INSTITUTION") lecture-institution))
          (should (string-equal (org-lectures-index-get-keyword-value "COURSE") course-name)))

        ;; Verify index update
        (let* ((index (org-lectures-index-get))
               (course-entry (assoc course-name index))
               (lecture-entry (car (plist-get (cdr course-entry) :lectures))))
          (should (not (null lecture-entry)))
          (should (string-equal (plist-get (cdr lecture-entry) :title) lecture-title))
          (should (string-equal (plist-get (cdr lecture-entry) :file) expected-filename)))))))

(ert-deftest org-lectures--create-new-lecture-internal-with-suffix-test ()
  "Test `org-lectures--create-new-lecture-internal' with a suffix."
  (with-temp-lectures-dir
    (let* ((course-name "SUFFCOURSE")
           (lecture-title "Suffix Lecture")
           (lecture-professor "Dr. Suffix")
           (lecture-institution "Suffix University")
           (note-type "lec")
           (date-str "20251216")
           (suffix "mysuffix")
           (expected-filename (expand-file-name "lec_SUFFCOURSE_20251216_mysuffix.org" (org-lectures-get-course-lectures-dir course-name))))

      ;; Create a dummy course entry
      (let* ((course-info-file (org-lectures-get-course-info-file course-name))
             (initial-course-entry `(,course-name . (:title "" :professor "" :institution ,lecture-institution :file ,course-info-file :lectures '()))))
        (org-lectures--configure-index-module)
        (push initial-course-entry org-lectures-index--cache)
        (org-lectures-index-write))

      (let ((created-file (org-lectures--create-new-lecture-internal
                           course-name
                           lecture-title
                           lecture-professor
                           lecture-institution
                           note-type
                           date-str
                           suffix)))
        ;; Verify file creation
        (should (file-exists-p created-file))
        (should (string-equal created-file expected-filename))

        ;; Verify index update
        (let* ((index (org-lectures-index-get))
               (course-entry (assoc course-name index))
               (lecture-entry (car (plist-get (cdr course-entry) :lectures))))
          (should (not (null lecture-entry)))
          (should (string-equal (plist-get (cdr lecture-entry) :title) lecture-title)))))))
