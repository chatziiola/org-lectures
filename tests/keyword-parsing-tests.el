;;; keyword-parsing-tests.el --- Tests for keyword value parsing -*- lexical-binding: t -*--

(require 'test-helper)

(ert-deftest org-lectures-index-get-keyword-value-single-test ()
  "Test `org-lectures-index-get-keyword-value' for single keywords in current buffer."
  (with-temp-buffer
    (insert "#+TITLE: My Test File\n#+AUTHOR: Gemini\n#+DATE: <2025-12-16 Tue>\n")
    (should (string-equal (org-lectures-index-get-keyword-value "TITLE")
                          "My Test File"))
    (should (string-equal (org-lectures-index-get-keyword-value "AUTHOR")
                          "Gemini"))
    (should (string-equal (org-lectures-index-get-keyword-value "DATE")
                          "<2025-12-16 Tue>"))
    (should (not (org-lectures-index-get-keyword-value "NON-EXISTENT")))))

(ert-deftest org-lectures-index-get-keyword-value-multiple-test ()
  "Test `org-lectures-index-get-keyword-value' for multiple keywords in current buffer."
  (with-temp-buffer
    (insert "#+TITLE: Multiple Keywords\n#+TAGS: tag1, tag2\n#+VERSION: 1.0\n")
    (let ((result (org-lectures-index-get-keyword-value '("TITLE" "TAGS" "VERSION"))))
      (should (equal result '("Multiple Keywords" "tag1, tag2" "1.0"))))
    (let ((result (org-lectures-index-get-keyword-value '("TITLE" "NON-EXISTENT" "VERSION"))))
      (should (equal result '("Multiple Keywords" nil "1.0"))))))


;; (ert-deftest org-lectures-index-get-keyword-from-file-test ()
;;   "Test `org-lectures-index-get-keyword-value' for multiple keywords in current buffer."
;;   (with-temp-file
;;     (insert "#+TITLE: Multiple Keywords\n#+TAGS: tag1, tag2\n#+VERSION: 1.0\n")
;;     (let ((result (org-lectures-index-get-keyword-value '("TITLE" "TAGS" "VERSION"))))
;;       (should (equal result '("Multiple Keywords" "tag1, tag2" "1.0"))))
;;     (let ((result (org-lectures-index-get-keyword-value '("TITLE" "NON-EXISTENT" "VERSION"))))
;;       (should (equal result '("Multiple Keywords" nil "1.0"))))))
