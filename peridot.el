;;; peridot.el --- Tools for writing a novel -*- lexical-binding: t -*-

;; Copyright © 2020 Ashton Wiersdorf

(require 'org)
(require 'selectrum)
(require 'seq)

(defcustom db-directory "peridot" "Directory under which peridot will search for commands")
(defcustom max-character-entry 300 "Maximum length of a character entry")

(defvar-local character-file (concat db-directory "/characters.org"))

(defun peridot-init ()
  "Initilize the Peridot story database. This will create a directory named `peridot/' in the current working directory"
  (interactive)
  (unless (file-directory-p db-directory)
    (mkdir db-directory))
  (if (file-directory-p db-directory)
      (progn
        (unless (file-exists-p character-file)
          (write-region "#+TITLE: Characters\n" nil character-file nil 0)))
    (message (format "\aUnable to create directory %s" db-directory))))

(defun peridot-find-this-character ()
  "Prompts user to pick what character to use, given an alias.
  Just returns the headline if there's only one."
  (interactive)
  (let* ((character-name (word-at-point))
         (character-occurances (matching-character-headlines character-name))
         (character-entries (seq-filter
                             #'(lambda (entry) (string-match-p (regexp-quote character-name) (plist-get entry :raw-value)))
                             (seq-map #'cadr
                                      (seq-filter #'(lambda (entry) (eq (car entry) 'headline)) character-occurances)))))
    (cond ((eq nil character-entries)
           (message (format "No characters named '%s' found" character-name)))
          ((= (length character-entries) 1)
           (jump-to-location (car character-entries) character-file))
          (t (let* ((chosen-one (selectrum-completing-read (format "Which \"%s\"? " character-name) (seq-map #'(lambda (a) (plist-get a :raw-value)) character-entries)))
                    (chosen-location (seq-find #'(lambda (i) (equal chosen-one (plist-get i :raw-value))) character-entries)))
               (jump-to-location chosen-location character-file))))))

(defun matching-character-headlines (character-name)
  "Fetches a list of matching character headlines"
  (org-map-entries #'(lambda () (org-element--current-element 300))
                   (format "/%s/" character-name)
                   (list character-file)))

(defun jump-to-location (headline-desc filename)
  (xref-push-marker-stack)
  (find-file-other-window filename)
  (goto-char (plist-get headline-desc :begin))
  (message (substitute-command-keys "Return to previous location with `\\[xref-pop-marker-stack]', or `\\[delete-window]' to close notes window.")))

;;;###autoload
(define-minor-mode peridot-mode
  "Utilities to help write a novel.

Keybindings
-----------
\\{peridot-map}
"
  :lighter " peridot"
  :keymap (let ((peridot-map (make-sparse-keymap)))
            (define-key peridot-map (kbd "M-.") 'peridot-find-this-character)
            peridot-map))

(provide 'peridot-mode)
