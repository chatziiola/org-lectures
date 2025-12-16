;;; org-lectures-index.el --- Indexing for org-lectures -*- lexical-binding: t -*-

;;; Commentary:
;; Indexing and caching functionality for org-lectures.

;;; Code:

(require 'org-element)

(defvar org-lectures-index-dir nil
  "The root directory for the lecture files to be indexed.")

(defvar org-lectures-index-note-type-alist '(("lecture" . "lec"))
  "Alist of note types to search for during indexing.")

(defvar org-lectures-index--cache nil
  "Cache for the course index data loaded from the index file.")

(defun ndk/get-keyword-key-value (kwd)
  "Only to be used by `org-lectures-get-keyword-value'.

Allows for the extraction of KWD from the current buffer.
Works only for buffers using the Org-Mode syntax."
  (let ((data (cadr kwd)))
    (list (plist-get data :key)
          (plist-get data :value))))

(defun org-lectures-index--get-keyword-value-from-buffer (key)
  "Return the value(s) for KEY(s) from the current buffer's Org content.
If KEY is a list, return a list of corresponding values."
  (let ((keyword-map (org-element-map (org-element-parse-buffer 'greater-element)
                                      '(keyword) #'ndk/get-keyword-key-value)))
    (if (listp key)
        (mapcar (lambda (k) (cadr (assoc k keyword-map))) key)
      (cadr (assoc key keyword-map)))))

(defun org-lectures-index-get-keyword-value (key &optional file)
  "Return the value for KEY in an Org buffer.
If FILE is given, find that file and check there. Otherwise, use
the current buffer.
If KEY is a list, return a list of corresponding values."
  (if file
      (with-temp-buffer
        (insert-file-contents file)
        (org-lectures-index--get-keyword-value-from-buffer key))
    (org-lectures-index--get-keyword-value-from-buffer key)))

(defun org-lectures-index--get-course-lectures-dir (course)
  "Return the directory in which lectures for `course' reside"
  (expand-file-name (concat "course_" course) org-lectures-index-dir))

(defun org-lectures-index-rebuild ()
  "Scan all course and lecture files and rebuild the index.
The index is stored in `.org-lectures-index.el` in `org-lectures-index-dir`.
This function reads files into temporary buffers and does not leave them open."
  (interactive)
  (let ((index-file (expand-file-name ".org-lectures-index.el" org-lectures-index-dir))
        (course-files (directory-files org-lectures-index-dir t "course_.*\\.org$"))
        (index-data '()))

    (dolist (course-file course-files)
      (with-temp-buffer
        (insert-file-contents course-file)
        (let* ((course-id (org-lectures-index--get-keyword-value-from-buffer "COURSE"))
               (course-title (org-lectures-index--get-keyword-value-from-buffer "TITLE"))
               (course-prof (org-lectures-index--get-keyword-value-from-buffer "PROFESSOR"))
               (course-inst (org-lectures-index--get-keyword-value-from-buffer "INSTITUTION")))
          (when course-id
	    (let* ((course-lecture-dir (org-lectures-index--get-course-lectures-dir course-id))
                   (lecture-files (when (file-directory-p course-lecture-dir)
                                    (directory-files course-lecture-dir t (concat (regexp-opt (mapcar #'cdr org-lectures-index-note-type-alist)) "_" course-id "_.*\\.org$"))))
                   (lecture-data '()))
              (dolist (lecture-file lecture-files)
                (with-temp-buffer
                  (insert-file-contents lecture-file)
                  (let* ((lecture-title (org-lectures-index--get-keyword-value-from-buffer "TITLE"))
                         (lecture-date (org-lectures-index--get-keyword-value-from-buffer "DATE")))
                    (when lecture-date
                      (push `(,lecture-date . (:title ,lecture-title
                                              :file ,lecture-file))
                            lecture-data)))))
              (push `(,course-id . (:title ,course-title
                                    :professor ,course-prof
                                    :institution ,course-inst
                                    :file ,course-file
                                    :lectures ,lecture-data))
                    index-data))))))

    (with-temp-buffer
      (require 'pp)
      (pp index-data (current-buffer))
      (write-file index-file))
    (message "org-lectures-index rebuilt.")))

(defun org-lectures-index-get ()
  "Load and return the course index.
If the index file does not exist or is stale, it is rebuilt.
The index data is cached in `org-lectures-index--cache`."
  (let ((index-file (expand-file-name ".org-lectures-index.el" org-lectures-index-dir)))
    (when (or (not (file-exists-p index-file))
              (file-newer-than-file-p org-lectures-index-dir index-file))
      (org-lectures-index-rebuild))
    (or org-lectures-index--cache
        (with-temp-buffer
          (insert-file-contents index-file)
          (setq org-lectures-index--cache (read (current-buffer)))))))

(defun org-lectures-index-write ()
  "Write the current in-memory course cache to the index file."
  (let ((index-file (expand-file-name ".org-lectures-index.el" org-lectures-index-dir)))
    (with-temp-buffer
      (require 'pp)
      (pp org-lectures-index--cache (current-buffer))
      (write-file index-file))))

(provide 'org-lectures-index)
