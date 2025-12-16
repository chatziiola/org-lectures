;;; sluggify-tests.el --- Tests for org-lectures-sluggify -*- lexical-binding: t -*-

(require 'test-helper)

(ert-deftest org-lectures-sluggify-test ()
  "Test the `org-lectures-sluggify' function."
  (should (string-equal (org-lectures-sluggify "Hello World") "hello-world"))
  (should (string-equal (org-lectures-sluggify "  Leading and Trailing Spaces  ") "leading-and-trailing-spaces"))
  (should (string-equal (org-lectures-sluggify "Special Chars!@#$%^&*()") "special-chars"))
  (should (string-equal (org-lectures-sluggify "Multiple---dashes and spaces") "multiple---dashes-and-spaces"))
  (should (string-equal (org-lectures-sluggify "UPPERCASE") "uppercase"))
  (should (string-equal (org-lectures-sluggify "") ""))
  (should (string-equal (org-lectures-sluggify " an-annoying_string ") "an-annoyingstring")))
