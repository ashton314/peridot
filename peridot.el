;;; peridot.el --- Tools for writing a novel -*- lexical-binding: t -*-

;; Copyright © 2020 Ashton Wiersdorf

;; Author: Ashton Wiersdorf <ashton.wiersdorf@pobox.com>
;; Created: 27 Jun 2020
;; URL: https://github.com/ashton314/peridot
;; Version: 0.0.1
;; Package-Requires: ((emacs "26.1") (selectrum 1.0) (xref "25.1"))
;; Keywords: extensions writing

(require 'org)
(require 'selectrum)
(require 'seq)
(require 'xref)

(defgroup peridot nil
  "Tools for writing a novel."
  :prefix "peridot-"
  :link '(url-link "https://github.com/ashton314/peridot"))

(defcustom peridot-db-name ".peridot" "Directory under which peridot will search for database." :type 'string)
(defcustom peridot-max-character-entry 300 "Maximum length of a character entry." :type 'integer)
(defcustom peridot-inital-entities '("characters" "plot" "world" "research") "Default set of db files to create." :type '(repeat string))

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
         (entity-occurances (matching-entity-headlines entity-name (all-entity-files)))
         (entity-matches (seq-filter
                          #'(lambda (entry) (string-match-p (regexp-quote entity-name) (plist-get (cadr entry) :raw-value))) entity-occurances)))

    (pp (cadar entity-matches))

    (cond ((eq nil entity-matches)
           (message (format "No entity matching '%s' found.\a" entity-name)))

          ;; Only one match
          ((= (length entity-matches) 1)
           (jump-to-location (cadar entity-matches) (concat (db-directory) "/" (caar entity-matches))))

          ;; Let user pick from set of matches
          (t (let* ((chosen-one (selectrum-completing-read (format "Which \"%s\"? " entity-name)
                                                           (seq-map #'(lambda (a) (plist-get (cadr a) :raw-value)) entity-matches)))
                    (chosen-location (seq-find #'(lambda (i) (equal chosen-one (plist-get (cadr i) :raw-value))) entity-matches)))
               (jump-to-location (cadr chosen-location) (concat (db-directory) "/" (car chosen-location))))))))

(defun matching-entity-headlines (entity-name entity-files)
  "Fetches a list of matching headlines from `entity-files'"
  (org-map-entries #'(lambda () (cons (buffer-name) (cdr (org-element--current-element peridot-max-character-entry))))
                   (format "/%s/" entity-name)
                   entity-files))

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
  :group 'peridot
  :keymap (let ((peridot-map (make-sparse-keymap)))
            (define-key peridot-map (kbd "M-.") 'peridot-find-entity)
            peridot-map))

(provide 'peridot-mode)
