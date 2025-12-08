;;; org-lectures.el --- Chasing simplicity -*- lexical-binding: t -*-

;; This file has been generated from the literate.org file. DO NOT EDIT.
;; Sources are available from https://github.com/chatziiola/org-lectures

;; Copyright (C) 2022-2025 Lamprinos Chatziioannou

;; Author: Lamprinos Chatziioannou
;; Maintainer: Lamprinos Chatziioannou
;; URL: https://github.com/chatziiola/org-lectures

;; Special thanks to:
;; - Gilles Castel (https://castel.dev)
;; - Jethro Kuan (https://github.com/jethrokuan)
;; - David Wilson (https://github.com/daviwil)
;; - Nicolas P. Rougier (https://github.com/rougier)
;; They inspired me not only to modify the "vanilla" setup and create scripts
;; for myself, but also to catch the "bug" of making my setups reproducible and
;; proper—giving back to the amazing Emacs, Org, and FOSS communities.

;; This file is NOT part of GNU Emacs.

;; LICENSE

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; For a deep dive into the ideology of the package, look up the README.org file
;; that you should have received along with it.

;;; Code:

(require 'org)

(defvar org-lectures-dir (expand-file-name "~/org/lectures")
 "Lecture and course files directory.

All courses and their respective lecture files are stored in
subfolders.")

(defvar org-lectures-default-institution "A.U.Th"
  "Default institution to be used on lecture creation.

This variable should be set once, when starting to use this
notetaking set up.

Setting the property INSTITUTION properly in the course
information file will cause all lectures for that course to be
created with that property (thus overwriting this variable).

Even though it certainly is not always useful , it helps automate
most of the lecture notes for any undergrad.")

(defvar org-lectures-current-semester "5"
  "Holds current semester value.

To be updated at the beginning of each semester by the user. This
option is not necessary but helps in the /automatic/ gathering of
data around courses.")

(defvar org-lectures-static-course-files-dir  org-lectures-dir
  "The path to extra course-related subfolders.

This option defaults to `org-lectures-dir'. Thus,
combining the two /folders/, of static information and
lectures (note-taking) into a single folder. Check the README.org
file for more information on the thinking process behind this
choice.")

(defvar org-lectures-org-roam-id-integration t
  "Whether `org-lectures-dir' is a subdirectory of `org-roam-directory'.

If this is true, then upon file creation a unique ID will be
generated, so that course files can be linked and use from within
org-roam.

FIXME. This option is not currently implemented.")

(defvar org-lectures-append-to-inbox nil
  "Whether an entry should be added to the users `inbox.org' file, (found in `org-directory')."
  )

(defvar org-lectures-note-type-alist '(("lecture" . "lec"))
  "Contains the note type. Every pair here will be checked.

The format is '(key . regex).

TODO: Implement it in note creation.
"
  )

(defvar org-lectures-lecture-data-alist '("TITLE" "PROFESSOR" "DATE")
  "WARNING: These get added in reverse in the final prompt.

The variable is heavily /under/-tested, so if you decide to use
it be prepared to encounter strange behaviour. It is intended to
be linked to `org-lectures-note-type-alist' in the future, so
that there is no need to differentiate between course note files
in an unnefficient manner.

At the moment due to spaghetti (at times) coding, only three
arguments will get shown. I'm thinking of restructuring this so
that it (maybe) utilizes consult, or simply refactoring so that
it is not so hastily written. In any case, FIXME.
")

(defvar org-lectures--course-cache nil
  "Cache for the course index data loaded from the index file.")

(defvar org-lectures-file-template
  ":PROPERTIES:
:ID: %i
:END:
#+TITLE: Διάλεξη:
#+FILETAGS: %t
#+DATE: %d
#+COURSE: %c
#+INSTITUTION: %I
"
  "Template for new lecture files.

Use `format-spec` codes:
  %i  -> ID (e.g., \"lec-<course>-\")
  %d  -> date (e.g., \"<2025-10-25>\")
  %c  -> course
  %n  -> institution
  %t  -> filetags")

(defvar org-lectures-default-tag-alist '("lecture" "todo")
  "This variable is used when setting the FILETAGS parameter in new lecture files")

(defun org-lectures-sluggify (s)
  "Given a string return it's /sluggified/ version.
It has only one argument, INPUTSTRING, which is self-described"
    (let* ((s (downcase (string-trim s)))
           (s (replace-regexp-in-string "[^[:alnum:][:space:]-]" "" s))
           (s (replace-regexp-in-string "[[:space:]]+" "-" s)))
      (replace-regexp-in-string "^-\\|-$" "" s)))

(defun ndk/get-keyword-key-value (kwd)
  "Only to be used by `org-lectures-get-keyword-value'.

Allows for the extraction of KWD from the current buffer.
Works only for buffers using the Org-Mode syntax."
  (let ((data (cadr kwd)))
    (list (plist-get data :key)
          (plist-get data :value))))

(defun org-lectures-get-keyword-value (key &optional file)
  "Return the value for KEY in an Org buffer.
If FILE is given, find that file and check there. Otherwise, use
the current buffer.
If KEY is a list, return a list of corresponding values."
  (if file
      (with-temp-buffer
        (insert-file-contents file)
        (org-lectures--get-keyword-value-from-buffer key))
    (org-lectures--get-keyword-value-from-buffer key)))

(defun org-lectures-find-course ()
  "Default driver function of `org-lectures.el'."
  (interactive)
  (let* ((course-answer (org-lectures-select-course-from-list)))
    (cond
     ((string-equal course-answer "NC")
      (org-lectures-create-new-course))
     (t
      (org-lectures-open-course (upcase course-answer))))))

; Minor modification so that I can use it in the publishing functions as well
(defun org-lectures-select-course-from-list ()
  "Show a prompt and return the selected course's ID."
  (let ((courses (org-lectures-get-course-list)))
    (if (not courses)
        (let ((selection (completing-read "Select Course: " '("New Course"))))
          (if (string-equal selection "New Course") "NC" nil))
      (let* (;; Dynamic column widths for pretty alignment
             (max-title-width (apply #'max 0 (mapcar (lambda (c) (length (or (plist-get c :title) ""))) courses)))
             (max-prof-width (apply #'max 0 (mapcar (lambda (c) (length (or (plist-get c :professor) ""))) courses)))
             (max-inst-width (apply #'max 0 (mapcar (lambda (c) (length (or (plist-get c :institution) ""))) courses)))
             (vertico-p (and (fboundp 'vertico-mode) vertico-mode))
             (format-string-vertico (format "%%-5s %%-%ds │ %%-%ds │ %%-%ds" max-title-width max-prof-width max-inst-width))
             (format-string-default (format "%%-5s %%-%ds %%-%ds %%-%ds" max-prof-width max-title-width max-inst-width))
             (course-prompt-alist
              (append
               (mapcar
                (lambda (course-plist)
                  (let* ((course-id (or (plist-get course-plist :course-id) ""))
                         (professor (or (plist-get course-plist :professor) ""))
                         (title (or (plist-get course-plist :title) ""))
                         (institution (or (plist-get course-plist :institution) "")))
                    (cons (if vertico-p
                              (format format-string-vertico course-id title professor institution)
                            (format format-string-default course-id professor title institution))
                          course-id)))
                courses)
               (list (cons "New Course" "NC")))))
        (let* ((selected-prompt (completing-read "Select Course: " course-prompt-alist)))
          (cdr (assoc selected-prompt course-prompt-alist)))))))

(defun org-lectures-get-course-list ()
  "Return a list of course property lists from the index."
  (let ((index (org-lectures--get-index)))
    (mapcar (lambda (course-entry)
              (let* ((course-id (car course-entry))
                     (props (cdr course-entry)))
                (list :course-id course-id
                      :file (plist-get props :file)
                      :title (plist-get props :title)
                      :professor (plist-get props :professor)
                      :institution (plist-get props :institution)))) index)))

(defun org-lectures-create-new-course ()
  "Create a new course.

More specifically this function creates:
1. The course info file (course_<course>.org)
2. The course lectures directory (...)

Function called through `org-lectures-find-course', when the
creation of a new course is necessary. It prompts the user for
input (short title for the course), up to 4 letters which serve
as the course's ID. It checks whether a course with that ID
already exists and if it does, it uses `org-lectures-open-course'
instead of creating any new files. If, however the file does not
exist, and the length of the short title is less than 4 letters a
new org file is created, in `org-lectures-dir', and with
the course's default properties all set up."
        (interactive)
        (let* ((course (downcase (completing-read "Insert short course title:" ())))
               (course-org-file (org-lectures-get-course-info-file course)))
          (cond
           ((file-exists-p course-org-file)
                (org-lectures-open-course (upcase course)))
           ((<= (length course) 4)
                (org-open-file course-org-file)
                (insert ":PROPERTIES:\n:ID: " course "-course\n:END:\n#+TITLE:\n#+PROFESSOR:\n#+INSTITUTION: " org-lectures-default-institution "\n#+SEMESTER: " org-lectures-current-semester "\n#+FILETAGS: course\n#+COURSE: " (upcase course)  "\n")
                (save-buffer)
                (let ((new-course-entry
                       `(,(upcase course) . (:title ""
                                            :professor ""
                                            :institution ,org-lectures-default-institution
                                            :file ,course-org-file
                                            :lectures '()))))
                  (org-lectures--get-index)
                  (push new-course-entry org-lectures--course-cache)
                  (org-lectures--write-index-to-file)))
           (t
            (error "Invalid Course Name. Short title must be less than 5 characters long")))))

(defun org-lectures-open-course-folder (&optional course)
  "Open the selected course's folder (with system default).

Works only if inside an org file with the 'COURSE' property, or
when called by `org-lectures-open-course'"
  (interactive)
  (let* ((course (or course (org-lectures-get-keyword-value "COURSE"))))
    (unless (symbolp course)
      (message (concat "Course " course " folder opened"))
      (shell-command (concat "open " org-lectures-static-course-files-dir "course_" course)))))

(defun org-lectures-dired-course-folder (&optional course)
  "Open the selected course's folder (with Dired).

Works only if inside an org file with the 'COURSE' property, or
when called by `org-lectures-open-course'"
  (interactive)
  (message "org-lectures-dired-course-folder Function will be deprecated in later version")
  (let* ((course (or course (org-lectures-get-keyword-value "COURSE"))))
    (unless (symbolp course)
      (message (concat "Course " course " folder opened")))
    (dired (concat org-lectures-static-course-files-dir "course_" course))))

(defun org-lectures-open-course (course)
  "Get prompt for COURSE lectures.

Open a minibuffer, using `org-lectures-select-lecture-from-course' in which the
user can filter the selected course's lectures, selecting an existing one, or
creating a new one. Gives the option to:
1. Create new lecture
2. Open an already existing lecture
3. Open the course's folder
4. Open the course's info file `course_<course>.org'."
  (let* ((lecture-answer (org-lectures-select-lecture-from-course course)))
    (if (stringp lecture-answer)
	(cond
	 ((string-equal lecture-answer "NL")
	  (org-lectures-create-new-lecture course))
	 ((string-equal lecture-answer "OF")
	  (org-lectures-dired-course-folder course))
	 ((string-equal lecture-answer "INFO")
	  (org-open-file (org-lectures-get-course-info-file course))))
      (org-open-file (car (last lecture-answer))))))

(defun org-lectures-select-lecture-from-course (course &optional publish)
  "Open a COURSE lecture for viewing or create a new one."
  (let* ((course-lectures
          (mapcar (lambda (file)
                    (cons course (append (org-lectures-get-keyword-value org-lectures-lecture-data-alist file)
                                         (list file))))
                  (org-lectures-get-lecture-file-list course))))
    (if (not course-lectures)
        (let ((selection (completing-read "Select Lecture: " '("New Lecture" "Open Course Folder" "Course Info"))))
          (cond ((string-equal selection "New Lecture") "NL")
                ((string-equal selection "Open Course Folder") "OF")
                ((string-equal selection "Course Info") "INFO")
                (t nil)))
      (let* ((max-date-width (apply #'max 0 (mapcar (lambda (l) (length (or (nth 2 l) ""))) course-lectures)))
             (max-title-width (apply #'max 0 (mapcar (lambda (l) (length (or (nth 0 l) ""))) course-lectures)))
             (max-prof-width (apply #'max 0 (mapcar (lambda (l) (length (or (nth 1 l) ""))) course-lectures)))
             (vertico-p (and (fboundp 'vertico-mode) vertico-mode))
             (format-string
              (if vertico-p
                  (format "%%-%ds │ %%-%ds │ %%-%ds" max-date-width max-title-width max-prof-width)
                (format "%%-%ds %%-%ds %%-%ds" max-date-width max-title-width max-prof-width)))
             (lecture-prompt-list
              (append
               (mapcar
                (lambda (lecture)
                  (let ((title (or (nth 0 lecture) ""))
                        (professor (or (nth 1 lecture) ""))
                        (date (or (nth 2 lecture) "")))
                    (cons (format format-string date title professor) lecture)))
                course-lectures)
               (unless publish
                 (list '("New Lecture" . "NL")
                       '("Open Course Folder" . "OF")
                       '("Course Info" . "INFO"))))))
        (let* ((selected-prompt (completing-read "Select Lecture: " lecture-prompt-list)))
          (cdr (assoc selected-prompt lecture-prompt-list)))))))

(defun org-lectures-get-lecture-file-list (course)
  "Return a list of lecture files in COURSE.

If the subdirectory does not exist, it creates it."
  (let* ((course-dir (expand-file-name
		      (concat "course_" course) org-lectures-dir)))

    (unless (file-directory-p course-dir)
      (make-directory course-dir))
    (directory-files
     course-dir					;inside the course directory
     'full					; recursive
     (concat (regexp-opt (mapcar #'cdr org-lectures-note-type-alist)) "_" (upcase course) "_.*\.org"))))	;lecture filenames template

(defun org-lectures-create-new-lecture (&optional COURSE INSTITUTION)
  "Create a new file for COURSE of INSTITUTION.

Populate it according to `org-lectures-file-template'.

Optional arguments exist:

COURSE: to be added in the lecture's '#+COURSE' field,
automatically populated when called through
`org-lectures-open-course'

INSTITUTION: to be added in the lecture's '#+INSTITUTION' field,
automatically populated by 'A.U.Th' if left empty."
  (let ((COURSE (or COURSE ""))
	(INSTITUTION (or INSTITUTION (org-lectures-get-lecture-institution COURSE)))
	(lecture-filename (expand-file-name
			   (org-lectures-set-lectures-filename COURSE)
			   (expand-file-name (concat "course_" COURSE) org-lectures-dir))))
    (let* ((id   (concat "lec-" COURSE "-" (format-time-string "%Y%m%d%H%M%S")))
	   (date (format-time-string "<%Y-%m-%d>"))
	   (tags (string-join (seq-map (lambda (x) (cond ((stringp x) x) ((consp x) (car x)) (t nil))) org-lectures-default-tag-alist) " "))
	   (spec (format-spec-make ?i id ?d date ?c COURSE ?I INSTITUTION ?t tags))
	   (payload (format-spec org-lectures-file-template spec t)))
      (write-region payload nil lecture-filename)
      (let* ((new-lecture-entry `(,date . (:title "Διάλεξη:"
                                           :file ,lecture-filename))))
        (org-lectures--get-index)
        (let ((course-in-cache (assoc COURSE org-lectures--course-cache)))
          (when course-in-cache
            (setf (plist-get (cdr course-in-cache) :lectures)
                  (cons new-lecture-entry (plist-get (cdr course-in-cache) :lectures)))))
        (org-lectures--write-index-to-file)

    (if org-lectures-append-to-inbox
	(write-region (concat "\n* ACTION \[\[" lecture-filename "\]\]\n") nil (expand-file-name "inbox.org" org-directory) t))

    (org-open-file lecture-filename)))))

(defun org-lectures-get-lecture-institution (course)
  "Return the proper institution for a course from the index."
  (if (string-blank-p course)
      org-lectures-default-institution
    (let* ((index (org-lectures--get-index))
           (course-data (cdr (assoc course index))))
      (or (plist-get course-data :institution)
          org-lectures-default-institution))))

(defun org-lectures-get-course-info-file (course)
  "Return the filename of that course's info file"
  (let* ((lower-file (expand-file-name (concat "course_" (downcase course) ".org") org-lectures-dir ))
	 (proper-file (expand-file-name (concat "course_" course ".org") org-lectures-dir )))
  (if (file-exists-p lower-file) ; remnants of a shady past
      lower-file
    proper-file)))

(defun org-lectures--get-note-type ()
  "Interactively select a note type from `org-lectures-note-type-alist'."
  (let ((types org-lectures-note-type-alist))
    (if (= (length types) 1)
        (cdar types)
      (let* ((prompt "Select a title: ")
             (options (mapcar #'car types))
             (choice (completing-read prompt options)))
        (cdr (assoc choice types))))))

(defun org-lectures--get-collision-suffix ()
  "Prompt user for info if lecture file exists, returning a filename suffix."
  (let ((prompt "A lecture with this filename already exists. Enter complementary information (empty appends hour-minute-second): "))
    (let ((user-input (read-string prompt)))
      (if (string-blank-p user-input)
          (format-time-string "%H%M%S" (current-time))
        (org-lectures-sluggify user-input)))))

(defun org-lectures-set-lectures-filename (course)
  "Return a unique lecture filename using the format:
`notetype_COURSE_DATE[_SUFFIX].org'."
  (let* ((note-type (org-lectures--get-note-type))
         (date-str (format-time-string "%Y%m%d" (current-time)))
         (base-filename (format "%s_%s_%s.org" note-type course date-str))
         (course-dir (expand-file-name (concat "course_" course) org-lectures-dir))
         (suffix (when (file-exists-p (expand-file-name base-filename course-dir))
                   (org-lectures--get-collision-suffix))))
    (if suffix
	(format "%s_%s_%s_%s.org" note-type course date-str suffix) base-filename)))

(require 'ox-latex) ; Ensure the latex exporter is available

(defun org-lectures-run-latexmk (file)
  "Starts a continuous latexmk process for the given file."
  (let* ((tex-file-name (concat (file-name-sans-extension file) ".tex"))
	 (process-name (format "lecture-mk-%s" (file-name-nondirectory file)))
         (output-buffer (get-buffer-create (format "*latexmk-%s*" (file-name-nondirectory file))))
         ;; The command components
         (program "latexmk")
         (args (list "-pvc" "-interaction=nonstopmode" "-xelatex" "-shell-escape" tex-file-name)))

    (message "Starting %s with PID %s" program process-name)

    ;; Kill any existing process with the same name first
    (let ((existing-proc (get-process process-name)))
      (when existing-proc (kill-process existing-proc)))

    ;; Start the new process asynchronously
    (org-lectures-export-to-latex file)
    (apply 'start-process process-name output-buffer program args)
    ))


(defun org-lectures-kill-latexmk (file)
  "Kills the continuous latexmk process associated with the given file."
  (let* ((process-name (format "lecture-mk-%s" (file-name-nondirectory file)))
         (proc (get-process process-name)))
    (when proc
      (kill-process proc)
      (message "Killed existing latexmk process: %s" process-name))))

;; --- Setup and Teardown Functions (Revised) ---

(defun org-lectures-setup ()
  "Setup routine for org-lectures-minor-mode."
  (interactive)
  (let* ((file (buffer-file-name))
	(ext (file-name-extension file)))
    (cond
     ((or (null file) (not (string-match "org" ext)))
      (message "Error: Cannot activate org-lectures-minor-mode; buffer is not visiting an Org file.")
      (setq org-lectures-minor-mode nil))
     (t
      ;; 1. Execute the actual latexmk command using start-process
      (org-lectures-run-latexmk file)
      (message "Org Lecture Mode: Asynchronous latexmk -pvc process started.")

      ;; 2. Add the buffer-local write hook
      (add-hook 'after-save-hook 'org-lectures-export-to-latex nil t)
      (message "Org Lecture Mode Activated! Buffer-local write hook added.")))))

(defun org-lectures-teardown ()
  "Teardown routine for org-lectures-minor-mode."
  (interactive)
  ;; Kill the running latexmk process
  (org-lectures-kill-latexmk (buffer-file-name))

  ;; Remove the write hook
  (remove-hook 'after-save-hook 'org-lectures-export-to-latex t)
  (message "Org Lecture Mode Deactivated! Hook and latexmk process removed."))

;; --- The Hook Function Remains the Same ---

(defun org-lectures-export-to-latex (&optional file)
  "Exports the current Org buffer to LaTeX."
  (interactive)
  (let* ((org-file (or file (buffer-file-name)))
	 (tex-file (concat (file-name-sans-extension org-file) ".tex")))
    (if org-file
	(if org-lectures-minor-mode
	    (progn
	      (message "Org Lecture Mode: Exporting buffer %s to LaTeX..." org-file)
	      (message "Org Lecture Mode: Will write to %s" tex-file)
	      ;; Did not use async because it messed with messages
	      (let* ((output-file (org-export-to-file 'latex  tex-file)))
		(message "Org Lecture Mode: Exported to %s. latexmk will now recompile." output-file))
	      )
	  (message "Org Lecture Mode Inactive: Will not export"))
      (error "File does not exist")
      )))

;; --- The Minor Mode Definition (Unchanged) ---

(define-minor-mode org-lectures-minor-mode
  "A minor mode for lecture notes that runs a shell command and exports to LaTeX on save."
  :lighter " Lecture"
  :keymap nil
  (if org-lectures-minor-mode
      (org-lectures-setup)
    (org-lectures-teardown)))

(defun org-lectures--get-keyword-value-from-buffer (key)
  "Return the value(s) for KEY(s) from the current buffer's Org content.
If KEY is a list, return a list of corresponding values."
  (let ((keyword-map (org-element-map (org-element-parse-buffer 'greater-element)
                                      '(keyword) #'ndk/get-keyword-key-value)))
    (if (listp key)
        (mapcar (lambda (k) (cadr (assoc k keyword-map))) key)
      (cadr (assoc key keyword-map)))))

(defun org-lectures-rebuild-index ()
  "Scan all course and lecture files and rebuild the index.
The index is stored in `.org-lectures-index.el` in `org-lectures-dir`.
This function reads files into temporary buffers and does not leave them open."
  (interactive)
  (let ((index-file (expand-file-name ".org-lectures-index.el" org-lectures-dir))
        (course-files (directory-files org-lectures-dir t "course_.*\\.org$"))
        (index-data '()))

    (dolist (course-file course-files)
      (with-temp-buffer
        (insert-file-contents course-file)
        (let* ((course-id (org-lectures--get-keyword-value-from-buffer "COURSE"))
               (course-title (org-lectures--get-keyword-value-from-buffer "TITLE"))
               (course-prof (org-lectures--get-keyword-value-from-buffer "PROFESSOR"))
               (course-inst (org-lectures--get-keyword-value-from-buffer "INSTITUTION")))
          (when course-id
            (let* ((course-lecture-dir (expand-file-name (concat "course_" course-id) org-lectures-dir))
                   (lecture-files (when (file-directory-p course-lecture-dir)
                                    (directory-files course-lecture-dir t (concat (regexp-opt (mapcar #'cdr org-lectures-note-type-alist)) "_" course-id "_.*\\.org$"))))
                   (lecture-data '()))
              (dolist (lecture-file lecture-files)
                (with-temp-buffer
                  (insert-file-contents lecture-file)
                  (let* ((lecture-title (org-lectures--get-keyword-value-from-buffer "TITLE"))
                         (lecture-date (org-lectures--get-keyword-value-from-buffer "DATE")))
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
    (message "org-lectures index rebuilt.")))

(defun org-lectures--get-index ()
  "Load and return the course index.
If the index file does not exist or is stale, it is rebuilt.
The index data is cached in `org-lectures--course-cache`."
  (let ((index-file (expand-file-name ".org-lectures-index.el" org-lectures-dir)))
    (when (or (not (file-exists-p index-file))
              (file-newer-than-file-p org-lectures-dir index-file))
      (org-lectures-rebuild-index))
    (or org-lectures--course-cache
        (with-temp-buffer
          (insert-file-contents index-file)
          (setq org-lectures--course-cache (read (current-buffer)))))))

(defun org-lectures--write-index-to-file ()
  "Write the current in-memory course cache to the index file."
  (let ((index-file (expand-file-name ".org-lectures-index.el" org-lectures-dir)))
    (with-temp-buffer
      (require 'pp)
      (pp org-lectures--course-cache (current-buffer))
      (write-file index-file))))

(provide 'org-lectures)
;;; org-lectures.el ends here
