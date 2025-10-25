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

(defvar org-lectures-file-template
  ":PROPERTIES:
:ID: %i
:END:
#+TITLE: Διάλεξη:
#+FILETAGS: %t
#+DATE: %d
#+COURSE: %c
#+INSTITUTION: %n
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

(defun org-lectures-sluggify (inputString)
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
  "Return the value with KEY in the current org buffer.

More specifically, in the following example, 'Gilbert Strang'
would be what's returned:

File contents:
    ...
    #+Professor: Gilbert Strang
    ...

Command:
    (org-lectures-get-keyword-value \"Professor\")

If FILE argument is given, then instead of searching inside the
current buffer, file is opened and the function is run there.

May also be used with a list of keys in a recursive manner."
  ;; TODO: THAT FILE CHECK SHOULD MOST PROBABLY BE BETTER
  (let ((file (or file buffer-file-name)))
    (if (not (string-blank-p file))
        (with-current-buffer (find-file-noselect file)	;;Anyway: visit that file
          (let ((temp-map				;; This is to avoid multiple calls of the same function - they are unecessary
		 (org-element-map
		     (org-element-parse-buffer 'greater-element)
		     '(keyword) #'ndk/get-keyword-key-value)))
	    (cond
	     ((proper-list-p key)			;; If the KEY element is a list
	      (let ((keyVals '()))
		(cl-loop for title in key do
			 (push (nth 1 (assoc title temp-map)) keyVals))
		keyVals))
	     (t					;; Else it must be a single element
	      (nth 1 (assoc key temp-map)))))))))

(defun org-lectures-find-course ()
  "Default driver function of `org-lectures.el'."
  (interactive)
  (let* ((course-answer (org-lectures-select-course-from-list)))
    (cond
     ;; FIXME - if 4 letters overload create-new-course to automatically create that course.
     ((and (stringp course-answer) (string-equal course-answer "NC"))
        (org-lectures-create-new-course))
     (t
      (org-lectures-open-course (upcase (car course-answer)))))))

; Minor modification so that I can use it in the publishing functions as well
(defun org-lectures-select-course-from-list()
 "Show a prompt and return a course."
  (let* ((course-prompt-list
             (append
              ;; FIXME spaghetti code, I'm thinking of having simply the file-title. No need for more
              ;; shortcourse | professor | longcourse | institution
              (seq-map
	       (lambda (e) (list (format "%-5s %-20s %-35s %-10s" (nth 0 e) (nth 1 e)(nth 2 e) (nth 3 e)) e))
	       (org-lectures-get-course-list))
              (list '("New Course" "NC"))))
         (course-answer
          (car (cdr (assoc (completing-read "Select Course: " course-prompt-list) course-prompt-list)))))
   course-answer))

(defun org-lectures-get-course-list ()
  "Return a list of the courses in `org-lectures-dir'."
  (let ((course-files (directory-files org-lectures-dir 'full "course_.*.org"))
	(out '()))
    (cl-loop for file in course-files do
	     (if (not (file-directory-p file))
		 (push (append
			(org-lectures-get-keyword-value '("INSTITUTION" "TITLE" "PROFESSOR" "COURSE" ) file)
			(list file))
		       out)))
    out))

(defun org-lectures-create-new-course ()
  "Create a new course.

More specifically this function creates:
1. The course info file (course_<course>.org)
2. The course lectures directory (...)
3. TODO anything else here?

Function called through `org-lectures-find-course', when the
creation of a new course is necessary. It prompts the user for
input (short title for the course), up to 4 letters which serve
as the course's ID. It checks whether a course with that ID
already exists and if it does, it uses `org-lectures-open-course'
instead of creating any new files. If, however the filel dows not
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
                (save-buffer))
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
4. Open the course's info file `course_<course>.org')."
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

(defun org-lectures-get-lecture-prompt-string-list (course-lectures)
  "Return the prompt string for displaying COURSE-LECTURES.

Comment: Not an ideal implementation, but to make it more
manageable."
  ;; Get lecture prompt string
  (seq-map (lambda (e)
	     (list (format "%-20s %-25s %-s" (nth 0 e) (nth 1 e)(nth 2 e)) e))
	   course-lectures))

(defun org-lectures-select-lecture-from-course (course &optional publish)
  "Open a COURSE lecture for viewing or create a new one.

Used by `org-lectures-open-course' and
`org-lectures-publish-lecture'. It opens a minibuffer prompt
allowing to select between an existing lecture and creating a new
one, opening the course's folder or having course info. If an
existing course is selected then a list in the form of '((DATE
PROFESSOR TITLE) FILE) is returned, while otherwise it returns
just a string ('NL' 'OF' or 'INFO')

An optional argument of PUBLISH has been added to filter
unecessary options for when called by
`org-lectures-publish-lecture'."
  (let* ((course-lectures '()))
    (cl-loop for file in (org-lectures-get-lecture-file-list course) do
	     ;; These get added in reverse in the final prompt
	     (push (append
		    (org-lectures-get-keyword-value org-lectures-lecture-data-alist file)
		    (list file))
		   course-lectures
		   ))
    (let*  ((lecture-prompt-list
	     (append
	      (org-lectures-get-lecture-prompt-string-list course-lectures)
	      (unless publish
		(list'("New Lecture" "NL")
		     '("Open Course Folder" "OF")
		     '("Course Info" "INFO")))))
	    (lecture-answer
	     (car (cdr (assoc
			(completing-read "Select Lecture: " lecture-prompt-list)
			lecture-prompt-list)))))
      lecture-answer)))

(defun org-lectures-get-lecture-file-list (course)
  "Return a list of lecture files in COURSE.

If the subdirectory does not exist, it creates it."
  (let* ((course-dir (expand-file-name
		      (concat "course_" course) org-lectures-dir)))

    (unless (f-directory-p course-dir)
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
			   ;; This function also checks whether such a func exists
			   (org-lectures-set-lectures-filename COURSE)
			   (expand-file-name (concat "course_" COURSE) org-lectures-dir))))
    (let* ((id   (concat "lec-" COURSE "-"))
	   (date (format-time-string "<%Y-%m-%d>"))
	   ;; Join tags, with a space in-between
	   (tags (string-join (seq-map (lambda (x) (cond ((stringp x) x) ((consp x) (car x)) (t nil))) org-lectures-default-tag-alist) " ")) 
	   (spec (format-spec-make ?i id ?d date ?c COURSE ?I INSTITUTION ?t tags))
	   (payload (format-spec org-lectures-file-template spec t)))
      (write-region payload nil lecture-filename))

    (if org-lectures-append-to-inbox
	(write-region (concat "\n* ACTION \[\[" lecture-filename "\]\]\n") nil (expand-file-name "inbox.org" org-directory) t))

    (org-open-file lecture-filename)))

(defun org-lectures-get-lecture-institution (course)
  "Return the proper institution for completion when creating a lecture.

This ensures that the user needs to only set the INSTITUTION in
the COURSE information file in order for all of its lectures ot
have this property properly filled."
  (if (string-blank-p course)
      org-lectures-default-institution
    (org-lectures-get-keyword-value "INSTITUTION"
				  (expand-file-name (concat "course_" course ".org")
						    org-lectures-dir))))

(defun org-lectures-get-course-info-file (course)
  "Return the filename of that course's info file"
  (let* ((lower-file (expand-file-name (concat "course_" (downcase course) ".org") org-lectures-dir ))
	 (proper-file (expand-file-name (concat "course_" course ".org") org-lectures-dir )))
  (if (f-file-p lower-file) ; remnants of a shady past 
      lower-file
    proper-file)))

(defun org-lectures-set-lectures-filename(course)
  "Return the lecture's title in a format: `notetype_COURSE_DATE.org'.
"
  (let* ((note-datatype
	  (if (= (length org-lectures-note-type-alist) 1)
	      (cdr (car org-lectures-note-type-alist))
	    (cdr (assoc
		  (completing-read "Select a title: " (mapcar #'car org-lectures-note-type-alist))
		  org-lectures-note-type-alist))))
	 (def-filename (concat note-datatype "_" course  "_" (format-time-string "%Y%m%d"(current-time)) ".org"))
	 (lecpath (expand-file-name def-filename
				    (expand-file-name (concat "course_" course) org-lectures-dir)))
	 ;; If the file already exists
	 (extrainfo (if (file-exists-p lecpath)
			(progn
			  (setq-local prompt (read-string
					      "A lecture already existed with this filename. Enter complementary information (empty appends hourminutesecond): "))
			  (if (string-blank-p prompt)
			      (format-time-string "%H%M%S"(current-time))
			    (org-lectures-sluggify prompt))
			  )
		      ;; Else it is an empty string (a blank one)
		      "")))
    (if (string-blank-p extrainfo)
	def-filename
      (concat note-datatype "_" course  "_" (format-time-string "%Y%m%d"(current-time)) "_" extrainfo ".org"))))

(provide 'org-lectures)
;;; org-lectures.el ends here
