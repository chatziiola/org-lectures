;;; org-lectures.el --- Chasing simplicity -*- lexical-binding: t -*-

;; This file has been generated from the literate.org file. DO NOT EDIT.
;; Sources are available from https://github.com/chatziiola/org-lectures

;; Copyright (C) 2022-2023 Lamprinos Chatziioannou

;; Author: Lamprinos Chatziioannou
;; Maintainer: Lamprinos Chatziioannou
;; URL: https://github.com/chatziiola/org-lectures

;; Special thanks to Jethro Kuan (https://github.com/jethrokuan) for inspiring
;; me to tailor my set up to my needs, and to David Wilson
;; (https://github.com/daviwil) and Nicolas P. Rougier
;; (https://github.com/rougier), for inspiring me to give back to the amazing
;; Emacs, Org, and FOSS comunities.

;; This file is NOT part of GNU Emacs.

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

(defun org-lectures-sluggify (inputString)
  "Given a string return it's /sluggified/ version.
It has only one argument, INPUTSTRING, which is self-described"
  (let ((slug-trim-chars '(
			   ;; Combining Diacritical Marks https://www.unicode.org/charts/PDF/U0300.pdf
                           768 ; U+0300 COMBINING GRAVE ACCENT
                           769 ; U+0301 COMBINING ACUTE ACCENT
                           770 ; U+0302 COMBINING CIRCUMFLEX ACCENT
                           771 ; U+0303 COMBINING TILDE
                           772 ; U+0304 COMBINING MACRON
                           774 ; U+0306 COMBINING BREVE
                           775 ; U+0307 COMBINING DOT ABOVE
                           776 ; U+0308 COMBINING DIAERESIS
                           777 ; U+0309 COMBINING HOOK ABOVE
                           778 ; U+030A COMBINING RING ABOVE
                           779 ; U+030B COMBINING DOUBLE ACUTE ACCENT
                           780 ; U+030C COMBINING CARON
                           795 ; U+031B COMBINING HORN
                           803 ; U+0323 COMBINING DOT BELOW
                           804 ; U+0324 COMBINING DIAERESIS BELOW
                           805 ; U+0325 COMBINING RING BELOW
                           807 ; U+0327 COMBINING CEDILLA
                           813 ; U+032D COMBINING CIRCUMFLEX ACCENT BELOW
                           814 ; U+032E COMBINING BREVE BELOW
                           816 ; U+0330 COMBINING TILDE BELOW
                           817 ; U+0331 COMBINING MACRON BELOW
                           )))
    (cl-flet* ((nonspacing-mark-p (char) (memq char slug-trim-chars))
               (strip-nonspacing-marks (s) (ucs-normalize-NFC-string
                                            (apply #'string
                                                   (seq-remove #'nonspacing-mark-p (ucs-normalize-NFD-string s)))))
               (cl-replace (inputString pair) (replace-regexp-in-string (car pair) (cdr pair) inputString)))
      (let* ((pairs `(("[^[:alnum:][:digit:]]" . "_") ;; convert anything not alphanumeric
                      ("__*" . "_")                   ;; remove sequential underscores
                      ("^_" . "")                     ;; remove starting underscore
                      ("_$" . "")))                   ;; remove ending underscore
             (slug (-reduce-from #'cl-replace (strip-nonspacing-marks inputString) pairs)))
        (downcase slug)))))

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
        (let* ((course (downcase (consult--prompt :prompt "Insert short course title:")))
                (course-org-file (expand-file-name (concat "course_" course ".org") org-lectures-dir)))
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
	  (org-open-file (expand-file-name (concat "course_" course ".org") org-lectures-dir ))))
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
	     (push (append
		    (org-lectures-get-keyword-value '( "TITLE"  "PROFESSOR" "DATE") file)
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
     (concat "lec_" (upcase course) "_"))))	;lecture filenames template

(defun org-lectures-create-new-lecture (&optional COURSE INSTITUTION)
  "Create a new file for COURSE of INSTITUTION.

FIXME: This is the old documentation:

It creates a new buffer in org mode with some simple metadata
information (specifically selected for lectures: TITLE, DATE,
INSTITUTION,COURSE). It is designed to be used along with the
`org-lectures-save-lecture-buffer-to-file' function.

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

    ;; Populate lecture
    (write-region
     (concat ":PROPERTIES:\n:ID: lec-" COURSE "-" "\n:END:\n#+TITLE: Διάλεξη:\n#+FILETAGS: lecture\n#+DATE: " (format-time-string "<%Y-%m-%d>") "\n#+FILETAGS: lecture\n#+COURSE: " COURSE "\n#+INSTITUTION: " INSTITUTION "\n")
     nil lecture-filename)
    ;; Add task in inbox
    (write-region (concat "\n* ACTION \[\[" lecture-filename "\]\]\n") nil (expand-file-name "inbox.org" org-directory) t)

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
						    org-lectures-dir))
    ))

(defun org-lectures-set-lectures-filename(course)
  "Return the lecture's title in a format: `lec_COURSE_DATE.org'."
					; This function was modified on <2023-01-10 Tue>, to fix a bug: I could not
					; start a second lecture for the same course on the same day without
					; overwriting the initial lecture
  (let* ((def-filename (concat "lec_" course  "_" (format-time-string "%Y%m%d"(current-time)) ".org"))
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
      (concat "lec_" course  "_" (format-time-string "%Y%m%d"(current-time)) "_" extrainfo ".org"))))

(provide 'org-lectures)
;;; org-lectures.el ends here
