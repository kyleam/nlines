;;; nlines.el --- Display N lines of a file

;; Copyright (C) 2015 Kyle Meyer <kyle@kyleam.com>

;; Author:  Kyle Meyer <kyle@kyleam.com>
;; URL: https://gitlab.com/kyleam/nlines
;; Keywords: convenience, files
;; Version: 0.1.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; Nlines mode displays the results of programs that output N lines of
;; a file.  `nlines-run-command' serves as the single entry point for
;; calling one of three programs: head, tail, or shuf.  To add other
;; programs, customize the variable `nlines-commands'.
;;
;; While `shell-command' or `dired-do-shell-command' can be used to
;; call programs like head and tail, using Nlines has several
;; advantages.
;;
;; - The buffers are named according to the command and file, making
;;   it easy to keep multiple buffers around.
;;
;; - The command that generated the buffer can be called again with a
;;   single key, which is convenient for viewing updates to the
;;   original file.
;;
;; - Commands can take the file name from point, from Dired, or from
;;   completion.
;;
;; - The number of output lines can be controlled by a numeric prefix.
;;
;; - The program column can be called on the output buffer with a
;;   single key.

;;; Code:

(require 'dired)
(require 'pcase)

(defgroup nlines nil
  "Display N lines of a file."
  :group 'files)

(defcustom nlines-default-number-of-lines 10
  "Default number of lines to output."
  :group 'nlines
  :type 'integer)

(defcustom nlines-commands
  '((?h "head" "--lines")
    (?t "tail" "--lines")
    (?s "shuf" "--head-count" nil one-file))
  "List of Nlines commands.

Format of each element should be
(KEY PROGRAM LN-FLAG [ARGS ONE-FILE]).

KEY is a character used to call the command.  PROGRAM is the name
of the program, and LN-FLAG is the program flag that accepts the
number of lines as an argument.  The optional ARGS is a list of
any remaining arguments to pass to PROGRAM.

The program must accept a file or list of files as positional
arguments.  A non-nil value in the fifth position indicates that
the command can only take a single file, not a list of files.

The key \"?\" is reserved for displaying the help buffer."
  :group 'nlines
  :type '(alist :key-type character
                :value-type
                (list string string
                      (choice (const :tag "No additional arguments" nil)
                              (list :tag "Additional arguments" string))
                      (choice (const :tag "Accepts multiple files" nil)
                              (const :tag "Accepts only one file"
                                     one-file)))))

(defcustom nlines-column-delimiters
  '(("csv" . ","))
  "Alist of (EXT . DELIM) pairs.
If a file has the extension EXT, pass DELIM as the argument to
the '--separator' option of the 'column' program."
  :group 'nlines
  :type '(alist :key-type string :value-type string))

(defvar-local nlines--cmd-info nil
  "Command information used to generate Nlines buffer.

This list should have the format
(PROGRAM LN-FLAG NLINES ARGS ONE-FILE FILES).

PROGRAM, LN-FLAG, and ARGS, and ONE-FILE have the same meaning as
described in `nlines-commands'.  NLINES should be a string
indicating the number of lines to pass to LN-FLAG.

FILES is a list of file names.")

;; `nlines--read-command' and `nlines--display-command-help' are based
;; off of Projectile's commander.
(defun nlines--display-command-help ()
  (let ((help-buf "*Nlines Help*"))
    (ignore-errors (kill-buffer help-buf))
    (with-current-buffer (get-buffer-create help-buf)
      (insert "Commands:\n\n")
      (pcase-dolist (`(,ch ,prog _) nlines-commands)
        (insert (format "%c:  Run %s.\n" ch prog)))
      (goto-char (point-min))
      (help-mode)
      (display-buffer (current-buffer) t))
    (nlines--read-command)))

(defun nlines--read-command ()
  (let* ((choices (mapcar #'car nlines-commands))
         (prompt (concat "Nlines command [" choices "] (? for help): "))
         (ch (read-char-choice prompt (cons ?? choices))))
    (if (eq ch ??)
        (nlines--display-command-help)
      (cdr (assq ch nlines-commands)))))

(defun nlines--get-filename-at-point ()
  (let ((fname (thing-at-point 'filename)))
    (when (and fname (file-exists-p fname))
      (substring-no-properties fname))))

(defun nlines--get-files ()
  (let ((files (or (dired-get-marked-files)
                   (nlines--get-filename-at-point)
                   (read-file-name "File: ")
                   (user-error "No file selected"))))
    (if (listp files) files (list files))))

(defun nlines--generate-buffer-file-name (cmd-info)
  "Make buffer file name from CMD-INFO.
CMD-INFO should have the form described by `nlines--cmd-info'."
  (pcase-let ((`(,prog _ _ _ _ ,files) cmd-info))
    (if (= 1 (length files))
        (format "*%s %s*" prog (abbreviate-file-name (car files)))
      (generate-new-buffer-name
       (format "*%s, multiple files*" prog)))))

(defun nlines--fill-buffer (cmd-info)
  "Fill buffer with output of Nlines command.
Construct the command from CMD-INFO, which should have the form
described by `nlines--cmd-info'."
  (interactive)
  (pcase-let ((`(,prog ,ln-flag ,nlines ,args _ ,files) cmd-info))
    (let (buffer-read-only)
      (erase-buffer)
      (apply #'call-process prog nil t nil
             ln-flag nlines (append args files))))
  (set-buffer-modified-p nil)
  (goto-char (point-min)))

;;;###autoload
(defun nlines-run-command (&optional nlines)
  "Call a command in `nlines-commands'.

In Dired, operate on marked files.  Otherwise, try to find a file
at point, or, if there is no file at point, prompt for a file.

NLINES is set to `nlines-default-number-of-lines' or the numeric
prefix value."
  (interactive (list (or (and current-prefix-arg
                              (prefix-numeric-value current-prefix-arg))
                         nlines-default-number-of-lines)))
  (pcase-let* ((`(,prog ,ln-flag ,args ,one-file) (nlines--read-command))
               (files (nlines--get-files))
               (cmd-info (list prog ln-flag (number-to-string nlines)
                               args one-file files))
               (bufname (nlines--generate-buffer-file-name cmd-info)))
    (when (and one-file (> (length files) 1))
      (user-error "This command accepts only one file"))
    (with-current-buffer (get-buffer-create bufname)
      (nlines-mode)
      (nlines--fill-buffer cmd-info)
      (setq nlines--cmd-info cmd-info))
    (pop-to-buffer bufname)))

(defun nlines-refresh-buffer (&optional nlines)
  "Refresh Nlines buffer.
With numeric prefix argument, change the number of lines
displayed to NLINES."
  (interactive "p")
  (when current-prefix-arg
    (setcar (cddr nlines--cmd-info) (number-to-string nlines))
    (rename-buffer (nlines--generate-buffer-file-name nlines--cmd-info)))
  (nlines--fill-buffer nlines--cmd-info))

(defun nlines-switch-command (&optional nlines)
  "Use another command to generate Nlines buffer.
With numeric prefix argument, also change the number of lines
displayed to NLINES."
  (interactive "p")
  (pcase-let ((`(,prog ,ln-flag ,args ,one-file) (nlines--read-command))
              (nlines (or (and current-prefix-arg (number-to-string nlines))
                          (nth 2 nlines--cmd-info)))
              (files (nth 5 nlines--cmd-info)))
    (when (and one-file (> (length files) 1))
      (user-error "This command does not support multiple files"))
    (setq nlines--cmd-info (list prog ln-flag nlines args one-file files))
    (rename-buffer (nlines--generate-buffer-file-name nlines--cmd-info))
    (nlines--fill-buffer nlines--cmd-info)))

(defun nlines-columnify (&optional delim)
  "Run the buffer contents through 'column'.
When the prefix argument DELIM is non-nil, prompt for the
separator.  Otherwise, the separator is determined by
`nlines-column-delimiters'."
  (interactive (list (and current-prefix-arg (read-string "Delimiter: "))))
  (let ((files (nth 5 nlines--cmd-info)))
    (when (> (length files) 1)
      (user-error "Cannot columnify multi-file buffer"))
    (let* ((delim (or delim (cdr (assoc (file-name-extension (car files))
                                        nlines-column-delimiters))))
           (args (cons "--table"
                       (and delim (list "--separator" delim))))
           buffer-read-only)
      (apply #'call-process-region (point-min) (point-max)
             "column" t t nil args))))

(defvar nlines-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "c" #'nlines-columnify)
    (define-key map "g" #'nlines-refresh-buffer)
    (define-key map "o" #'nlines-switch-command)
    map)
  "Keymap for `nlines-mode'.")

;;;###autoload
(define-derived-mode nlines-mode special-mode "Nlines"
  "Mode for viewing N lines of output.
\\<nlines-mode-map>\
Type \\[nlines-refresh-buffer] to refresh output.
Type \\[nlines-columnify] to run 'column' on the output.
Type \\[nlines-switch-command] to run a different Nlines command.
\n\\{nlines-mode-map}"
  (buffer-disable-undo)
  (setq buffer-read-only t))

(provide 'nlines)
;;; nlines.el ends here
