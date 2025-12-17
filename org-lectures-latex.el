;;; org-lectures-latex.el --- Latexmk integration for org-lectures -*- lexical-binding: t -*-

;;; Code:

(require 'ox-latex) ; Ensure the latex exporter is available

(defun org-lectures-latex-run-latexmk (file)
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
    (org-lectures-latex-export-to-latex file)
    (apply 'start-process process-name output-buffer program args)
    ))


(defun org-lectures-latex-kill-latexmk (file)
  "Kills the continuous latexmk process associated with the given file."
  (let* ((process-name (format "lecture-mk-%s" (file-name-nondirectory file)))
         (proc (get-process process-name)))
    (when proc
      (kill-process proc)
      (message "Killed existing latexmk process: %s" process-name))))

;; --- Setup and Teardown Functions (Revised) ---

(defun org-lectures-latex-setup ()
  "Setup routine for org-lectures-latex-minor-mode."
  (interactive)
  (let* ((file (buffer-file-name))
	(ext (file-name-extension file)))
    (cond
     ((or (null file) (not (string-match "org" ext)))
      (message "Error: Cannot activate org-lectures-latex-minor-mode; buffer is not visiting an Org file.")
      (setq org-lectures-latex-minor-mode nil))
     (t
      ;; 1. Execute the actual latexmk command using start-process
      (org-lectures-latex-run-latexmk file)
      (message "Org Lecture Mode: Asynchronous latexmk -pvc process started.")

      ;; 2. Add the buffer-local write hook
      (add-hook 'after-save-hook 'org-lectures-latex-export-to-latex nil t)
      (message "Org Lecture Mode Activated! Buffer-local write hook added.")))))

(defun org-lectures-latex-teardown ()
  "Teardown routine for org-lectures-latex-minor-mode."
  (interactive)
  ;; Kill the running latexmk process
  (org-lectures-latex-kill-latexmk (buffer-file-name))

  ;; Remove the write hook
  (remove-hook 'after-save-hook 'org-lectures-latex-export-to-latex t)
  (message "Org Lecture Mode Deactivated! Hook and latexmk process removed."))

;; --- The Hook Function Remains the Same ---

(defun org-lectures-latex-export-to-latex (&optional file)
  "Exports the current Org buffer to LaTeX."
  (interactive)
  (let* ((org-file (or file (buffer-file-name)))
	 (tex-file (concat (file-name-sans-extension org-file) ".tex")))
    (if org-file
	(if org-lectures-latex-minor-mode
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

(define-minor-mode org-lectures-latex-minor-mode
  "A minor mode for lecture notes that runs a shell command and exports to LaTeX on save."
  :lighter " Lecture"
  :keymap nil
  (if org-lectures-latex-minor-mode
      (org-lectures-latex-setup)
    (org-lectures-latex-teardown)))

(provide 'org-lectures-latex)
