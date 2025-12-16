;;; filename-generation-tests.el --- Tests for filename generation -*- lexical-binding: t -*--

(require 'test-helper)

(ert-deftest org-lectures--get-note-type-internal-test ()
  "Test `org-lectures--get-note-type-internal'."
  (let ((org-lectures-note-type-alist '(("lecture" . "lec") ("tutorial" . "tut"))))
    (should (string-equal (org-lectures--get-note-type-internal "lecture") "lec"))
    (should (string-equal (org-lectures--get-note-type-internal "tutorial") "tut"))
    (should (not (org-lectures--get-note-type-internal "non-existent")))))

(ert-deftest org-lectures--get-collision-suffix-internal-test ()
  "Test `org-lectures--get-collision-suffix-internal'.

If given no string, a time-based one should one should be returned
If given a string, it should be sluggified, according to `org-lectures-sluggify'
"
  (let ((custom-string  " My Custom Suffix "))
    (should (string-match "[0-9]\\{6\\}" (org-lectures--get-collision-suffix-internal "")))

    (should (string-equal (org-lectures--get-collision-suffix-internal custom-string) (org-lectures-sluggify custom-string)))))

(ert-deftest org-lectures--set-lectures-filename-internal-test ()
  "Test `org-lectures--set-lectures-filename-internal'."
  (let ((course "TEST")
        (note-type "lec")
        (date-str "20251216"))
    ;; Test without suffix
    (should (string-equal (org-lectures--set-lectures-filename-internal course note-type date-str) "lec_TEST_20251216.org"))
    ;; Test with suffix
    (should (string-equal (org-lectures--set-lectures-filename-internal course note-type date-str "mysuffix") "lec_TEST_20251216_mysuffix.org"))))
