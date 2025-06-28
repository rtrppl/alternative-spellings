;; alternative-spellings.el --- -*- lexical-binding: t -*-

;; Maintainer: René Trappel <rtrappel@gmail.com>
;; URL:
;; Version: 0.1
;; Package-Requires: emacs "26"
;; Keywords: search web

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; A package to save and return different spellings
;; for terms such as names and locations.

;;; Code:

(require 'json)

(defvar alternative-spellings-dotfile "~/.alternative-spellings-list")

(defvar alternative-spellings-binding-term " OR ")

(defun alternative-spellings ()
  "Returns a list of OR-connected alternative spellings for a term."
  (interactive)
  (let* ((term-alt_spell (alternative-spellings-return-term-list))
	 (alt_spell-term (alternative-spellings-return-alt-list term-alt_spell))
	 (all-spellings (hash-table-keys alt_spell-term))
	 (selection (completing-read "Pick term to get a list of all alternative spellings: " all-spellings)))
    (when (member selection all-spellings)
      (let* ((term (gethash selection alt_spell-term))
	     (alt-spellings-for-term (gethash term term-alt_spell))
	     (alt-spellings-for-term (alternative-spellings-prepare-list alt-spellings-for-term))
	     (clipboard-string))
	(dolist (item alt-spellings-for-term)
	  (setq clipboard-string (concat clipboard-string "\"" item "\"" alternative-spellings-binding-term)))
	(setq clipboard-string (string-trim-right clipboard-string alternative-spellings-binding-term))
	(kill-new clipboard-string)
	(message "Copied %s to the clipboard." clipboard-string)))))
		      
(defun alternative-spellings-copy ()
  "Copy one alternative spelling into the clipboard."
  (interactive)
  (let* ((term-alt_spell (alternative-spellings-return-term-list))
	 (alt_spell-term (alternative-spellings-return-alt-list term-alt_spell))
	 (all-spellings (hash-table-keys alt_spell-term))
	 (selection (completing-read "Pick term to get a list of all alternative spellings: " all-spellings)))
    (when (member selection all-spellings)
      (let* ((term (gethash selection alt_spell-term))
	     (alt-spellings-for-term (gethash term term-alt_spell))
	     (alt-spellings-for-term (alternative-spellings-prepare-list alt-spellings-for-term))
	     (item (completing-read "Pick alternative spelling: " alt-spellings-for-term)))
	(when (member item alt-spellings-for-term)
	  (kill-new item)
	  (message "Copied %s to the clipboard." item))))))

(defun alternative-spellings-return-term-list ()
  "Returns the hash table TERM-ALT_SPELL."
  (let ((term-alt_spell (make-hash-table :test 'equal)))
    (with-temp-buffer
	 (insert-file-contents alternative-spellings-dotfile)
	 (if (fboundp 'json-parse-buffer)
	     (setq term-alt_spell (json-parse-buffer))))
    term-alt_spell))

(defun alternative-spellings-return-alt-list (term-alt_spell)
  "Returns inverse hash table for TERM-ALT_SPELL."
  (let* ((alt_spell-term (make-hash-table :test 'equal))
	 (all-keys (hash-table-keys term-alt_spell)))
    (dolist (key all-keys)
	  (let* ((line-alt_spellings (gethash key term-alt_spell))
		 (alt_spellings-list (alternative-spellings-prepare-list line-alt_spellings)))
	    (dolist (item alt_spellings-list)
	    (puthash item key alt_spell-term))))
    alt_spell-term))

(defun alternative-spellings-prepare-list (separated-values)
  "Prepares a list from a line of separated values."
  (let ((list-with-values))
    (with-temp-buffer
       (insert separated-values)
       (goto-char (point-min))
       (while (and (re-search-forward "::\\(.*?\\)\\::" nil t)
		      (not (string-empty-p (match-string 1))))
	    (push (match-string 1) list-with-values)))
    list-with-values))

(defun alternative-spellings-add-new-spellings ()
  "Adds new alternative spellings to a term."
 (interactive)
 (let* ((term-alt_spell (alternative-spellings-return-term-list))
	(alt_spell-term (alternative-spellings-return-alt-list term-alt_spell))
	(all-spellings (hash-table-keys alt_spell-term))
	(selection (completing-read "Add an alternative spelling to (leave empty to end): " all-spellings)))
     (when (member selection all-spellings)
       (let* ((term (gethash selection alt_spell-term))
              (new-spelling (read-from-minibuffer "Please provide a new spelling: "))
	    (old-value-term (gethash term term-alt_spell))
	    (new-value-term (concat old-value-term "::" new-spelling "::")))
       (remhash term term-alt_spell)
       (puthash term new-value-term term-alt_spell)
       (message "Added \"%s\" as a new alternative spelling for \"%s\"." new-spelling term)
       (alternative-spellings-update-file term-alt_spell)))
     (when (yes-or-no-p (format "Is %s a new term you want to add alternative spellings for?" selection))
       (let* ((term (gethash selection alt_spell-term)))
	 (puthash selection (concat "::" selection "::") term-alt_spell)
	 (alternative-spellings-update-file term-alt_spell)))))

(defun alternative-spellings-remove-spellings ()
  "Adds new alternative spellings to a term."
 (interactive)
 (let* ((term-alt_spell (alternative-spellings-return-term-list))
	(alt_spell-term (alternative-spellings-return-alt-list term-alt_spell))
	(all-spellings (hash-table-keys alt_spell-term))
	(selection (completing-read "Select spelling to remove: " all-spellings)))
   (when (member selection all-spellings)
     (let* ((term (gethash selection alt_spell-term))
	    (old-value-term (gethash term term-alt_spell))
	    (new-value-term old-value-term))
       (print new-value-term)
       (setq new-value-term (replace-regexp-in-string (concat "::" selection "::") "" new-value-term ))
       (remhash term term-alt_spell)
       (when new-value-term
	 (puthash term new-value-term term-alt_spell))
       (message "Removed \"%s\" as an alternative spelling for \"%s\"." selection term)
       (alternative-spellings-update-file term-alt_spell)))))

(defun alternative-spellings-update-file (hashtable)
  "Writes an updated version of HASHTABLE to the alternative
spelling dotfile."
  (with-temp-buffer
      (let* ((json-data (json-encode hashtable)))
	(insert json-data)
	(write-file alternative-spellings-dotfile))))

;;; alternative-spellings.el ends here
