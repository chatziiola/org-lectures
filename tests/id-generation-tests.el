;;; id-generation-tests.el --- Tests for ID generation -*- lexical-binding: t -*-

(require 'test-helper)

(ert-deftest org-lectures-generate-lecture-id-fixed-time-test ()
  "Test `org-lectures-generate-lecture-id' with a fixed time."
  (let* ((fixed-time (encode-time 0 0 0 1 1 2025))
	 (generated-id (org-lectures-generate-lecture-id "EMACS" fixed-time)))
    (should (string-equal generated-id "lec-EMACS-20250101000000"))
    (should (string-match "^lec-EMACS-20250101000000" generated-id))
    (should (string-match "lec-EMACS-20250101000000$" generated-id))
    (should (string-match "^lec-EMACS-20250101000000$" generated-id))))

;; "lec-EMACS-20251216223204"
(ert-deftest org-lectures-generate-lecture-id-dynamic-time-test ()
  "Test `org-lectures-generate-lecture-id' with dynamic time."
  (let ((generated-id (org-lectures-generate-lecture-id "EMACS")))
    (should (string-match "EMACS" generated-id))
    (should (string-match "^lec-EMACS-" generated-id))
    (should (string-match "[0-9]" generated-id))
    (should (string-match "-[0-9]\\{14\\}$" generated-id))
    (should (string-match "^lec-EMACS-[0-9]\\{14\\}$" generated-id))
    ))
