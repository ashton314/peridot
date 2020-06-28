;;; peridot.el --- Tools for writing a novel -*- lexical-binding: t -*-

;; Copyright © 2020 Ashton Wiersdorf

(require 'org)
(require 'selectrum)

(defvar-local character-file "peridot/characters.org")
(defcustom max-character-entry 300 "Maximum length of a character entry")

(defun find-this-character ()
  "Prompts user to pick what character to use, given an alias.
  Just returns the headline if there's only one."
  (interactive)
  (let* ((character-name (word-at-point))
         (character-entries (matching-character-locations character-name)))
    (cond ((eq nil character-entries)
           (message (format "No characters named '%s' found" character-name)))
          ((= (length character-entries) 1)
           (jump-to-location (car character-entries) character-file))
          (t (message "Don't know how to handle multiple possiblities yet!")))))

(defun matching-character-locations (character-name)
  "Fetches a list of matching character headlines"
  (org-map-entries #'(lambda () (org-element--current-element 300))
                   (format "ALIAS=\"%s\"" character-name)
                   (list character-file)))

(defun jump-to-location (headline-desc filename)
  (xref-push-marker-stack)
  (find-file-other-window filename)
  (goto-char (plist-get (cadr headline-desc) :begin))
  (message (substitute-command-keys "Return to previous location with `\\[xref-pop-marker-stack]'.")))

;;;###autoload
(define-minor-mode peridot-mode
  "Extention of org-mode to help write files."
  :lighter " peridot"
  :keymap (let ((peridot-map (make-sparse-keymap)))
            (define-key peridot-map (kbd "M-.") 'find-this-character)
            peridot-map))

(provide 'peridot-mode)
