;;; peridot.el --- Tools for writing a novel -*- lexical-binding: t -*-

;; Copyright © 2020 Ashton Wiersdorf

(require 'org)
(require 'selectrum)
(require 'seq)

(defcustom peridot-db-name ".peridot" "Directory under which peridot will search for database")
(defcustom peridot-max-character-entry 300 "Maximum length of a character entry")
(defcustom peridot-inital-entities '("characters" "plot" "world" "research") "Default set of db files to create")

(defun db-directory ()
  "Returns full path to the peridot database directory."
  (concat (locate-dominating-file buffer-file-name peridot-db-name) peridot-db-name))

(defun entity-file (entity-type)
  "Returns full path to the file of a given entity."
  (concat (db-directory) (format "/%s.org" entity-type)))

(defun all-entity-files ()
  "Returns a list of all db files."
  (file-expand-wildcards (format "%s/*.org" (db-directory))))

(defun peridot-init ()
  "Initilize the Peridot story database. This will create a
directory named `.peridot/' in the current working directory"
  (interactive)
  (unless (file-directory-p (db-directory))
    (mkdir peridot-db-name))
  (if (file-directory-p (db-directory))
      (mapcar #'(lambda (entity-type)
                  (unless (file-exists-p (entity-file entity-type))
                    (write-region (format "#+TITLE: %s\n\n" (capitalize entity-type)) nil (entity-file entity-type) nil 0)))
              peridot-inital-entities)
    (message (format "\aUnable to create directory %s" db-directory))))

(defun peridot-find-entity ()
  "Searches all entity files in db for headlines that match the current word."
  (interactive)
  (let* ((entity-name (word-at-point))
         (entity-occurances (matching-entity-headlines entity-name (all-entity-files))))
    
    (pp entity-occurances)
;    (message entity-occurances)
    ))


(defun old--peridot-find-entity (entity-file entity-type-name)
  "Returns a function that when invoked interactively will jump
to the given entity."
  #'(lambda ()
    (interactive)
    (let* ((entity-name (word-at-point))
           (entity-occurances (matching-entity-headlines entity-name entity-file))
           (entity-entries (seq-filter
                               #'(lambda (entry) (string-match-p (regexp-quote entity-name) (plist-get entry :raw-value)))
                               (seq-map #'cadr
                                        (seq-filter #'(lambda (entry) (eq (car entry) 'headline)) entity-occurances)))))
      (cond ((eq nil entity-entries)
             (message (format "No %s named '%s' found" entity-type-name entity-name)))
            ((= (length entity-entries) 1)
             (jump-to-location (car entity-entries) entity-file))
            (t (let* ((chosen-one (selectrum-completing-read (format "Which \"%s\"? " entity-name) (seq-map #'(lambda (a) (plist-get a :raw-value)) entity-entries)))
                      (chosen-location (seq-find #'(lambda (i) (equal chosen-one (plist-get i :raw-value))) entity-entries)))
                 (jump-to-location chosen-location entity-file)))))))
  
(defun matching-entity-headlines (entity-name entity-files)
  "Fetches a list of matching headlines from `entity-files'"
  (org-map-entries #'(lambda () (cons (buffer-name) (cdr (org-element--current-element peridot-max-character-entry))))
                   (format "/%s/" entity-name)
                   entity-files))

;; (defun peridot-find-this-character ()
;;   "Prompts user to pick what character to use, given an alias.
;;   Just returns the headline if there's only one."
;;   (interactive)
;;   (let* ((character-name (word-at-point))
;;          (character-occurances (matching-character-headlines character-name))
;;          (character-entries (seq-filter
;;                              #'(lambda (entry) (string-match-p (regexp-quote character-name) (plist-get entry :raw-value)))
;;                              (seq-map #'cadr
;;                                       (seq-filter #'(lambda (entry) (eq (car entry) 'headline)) character-occurances)))))
;;     (cond ((eq nil character-entries)
;;            (message (format "No characters named '%s' found" character-name)))
;;           ((= (length character-entries) 1)
;;            (jump-to-location (car character-entries) (character-file)))
;;           (t (let* ((chosen-one (selectrum-completing-read (format "Which \"%s\"? " character-name) (seq-map #'(lambda (a) (plist-get a :raw-value)) character-entries)))
;;                     (chosen-location (seq-find #'(lambda (i) (equal chosen-one (plist-get i :raw-value))) character-entries)))
;;                (jump-to-location chosen-location (character-file)))))))

;; (defun matching-character-headlines (character-name)
;;   "Fetches a list of matching character headlines"
;;   (org-map-entries #'(lambda () (org-element--current-element 300))
;;                    (format "/%s/" character-name)
;;                    (list (character-file))))

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
            (define-key peridot-map (kbd "M-.") 'peridot-find-entity)
            peridot-map))

(provide 'peridot-mode)
