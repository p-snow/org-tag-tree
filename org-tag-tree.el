;;; org-tag-tree.el --- Managing Org tag hierarchy with an Org tree -*- lexical-binding: t -*-

;; Author: p-snow <public@p-snow.org>
;; Maintainer: p-snow <public@p-snow.org>
;; Version: 0.0.1
;; Package-Requires: ((emacs "30.1"))
;; Homepage: https://github.com/p-snow/org-tag-tree
;; Keywords: org, tag

;; This file is not part of GNU Emacs

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; tag tree is

;;; Code:

(require 'org)
(require 'cl-lib)

;;;; Variables

;;;; Customization

(defgroup org-tag-tree nil
  "Options for `org-tag-tree'."
  :group 'org
  :link '(url-link "https://github.com/p-snow/org-tag-tree"))

(defcustom org-tag-tree-global-tag-files nil
  "A list of Org files in which the global tag trees would be looked for."
  :type '(repeat :tag "List of files" file)
  :group 'org-tag-tree)

(defcustom org-tag-tree-global-tag-matcher "gtag"
  "The tag matcher used to find tag trees globally.

See the \"Match syntax\" section of the org manual for more details."
  :type 'string
  :group 'org-tag-tree)

(defcustom org-tag-tree-buffer-tag-matcher "tag"
  "The tag matcher used to find tag trees locally.

See the \"Match syntax\" section of the org manual for more details."
  :type 'string
  :group 'org-tag-tree)

(defcustom org-tag-tree-exclusive-tag "exclusive"
  "The tag for mutually exclusive groups in the tag trees."
  :type 'string
  :group 'org-tag-tree)

(defcustom org-tag-tree-regexp-tag "regexp"
  "The tag for regular expression headings in the tag trees."
  :type 'string
  :group 'org-tag-tree)

(defcustom org-tag-tree-ignore-tag "ignore"
  "The tag for subtrees to ignore in the tag trees."
  :type 'string
  :group 'org-tag-tree)

(defcustom org-tag-tree-global-tag-persistent nil
  "If non-nil, set tags to `org-tag-persistent-alist'."
  :type 'boolean
  :group 'org-tag-tree)

;;;; Functions

;;;###autoload
(defun org-tag-tree-load-global-tags ()
  "Load global tags by reading tag trees in `org-tag-tree-global-tag-files'.

Tag trees are represented by the root node, which has a tag that matches
`org-tag-tree-global-tag-matcher`.

Global tags are tag definitions available across all Org files. By
default, they are stored in `org-tag-alist', which means they can be
overridden by buffer-local tags declared in the #+TAGS line. If
`org-tag-tree-global-tag-persistent' is non-nil, they are stored in
`org-tag-persistent-alist', making them available even when buffer-local
tags are declared."
  (interactive)
  (when-let ((global-tag-alist
              (cl-reduce
               (lambda (result next)
                 (append result '((:newline)) next))
               (let ((org-tags-exclude-from-inheritance
                      (list org-tag-tree-global-tag-matcher)))
                 (org-map-entries #'org-tag-tree--parse-tree
                                  org-tag-tree-global-tag-matcher
                                  org-tag-tree-global-tag-files)))))
    (setf (if org-tag-tree-global-tag-persistent
              org-tag-persistent-alist
            org-tag-alist)
          global-tag-alist)))

;;;###autoload
(defun org-tag-tree-load-buffer-tags ()
  "Load buffer-local tags from tag trees found in the current Org buffer.

Tag trees are represented by the root node, which has a tag that matches
`org-tag-tree-buffer-tag-matcher`.

The loaded tags replace any buffer-local tags previously declared in a
#+TAGS line."
  (interactive)
  (when (derived-mode-p 'org-mode)
    (setq org-current-tag-alist
          (org--tag-add-to-alist
           org-tag-persistent-alist
           (cl-reduce
            (lambda (result next)
              (append result '((:newline)) next))
            (let ((org-tags-exclude-from-inheritance
                   (list org-tag-tree-buffer-tag-matcher)))
              (org-map-entries #'org-tag-tree--parse-tree
                               org-tag-tree-buffer-tag-matcher))))))
  (setq org-tag-groups-alist
        (org-tag-alist-to-groups org-current-tag-alist)))

(defun org-tag-tree--parse-tree ()
  "Parse the tag tree rooted at the current Org heading."
  (let ((root-keywords (org-get-tags nil 'local))
        tag-alist next-gen-group)
    (if (not (save-excursion (org-goto-first-child)))
        (push (org-tag-tree--parse-tree-node)
              tag-alist)
      (push (if (member org-tag-tree-exclusive-tag root-keywords)
                '(:startgroup)
              '(:startgrouptag))
            tag-alist)
      (when-let ((group-tag (org-tag-tree--parse-tree-node)))
        (push group-tag tag-alist)
        (push '(:grouptags) tag-alist))
      (cl-loop initially (org-goto-first-child)
               do (push (org-tag-tree--parse-tree-node
                         (member org-tag-tree-regexp-tag
                                 (org-get-tags nil 'local)))
                        tag-alist)
               do (when (save-excursion (org-goto-first-child))
                    (push (point-marker) next-gen-group))
               while (org-get-next-sibling))
      (push (if (member org-tag-tree-exclusive-tag root-keywords)
                '(:endgroup)
              '(:endgrouptag))
            tag-alist)
      (when next-gen-group
        (mapc (lambda (elm) (push elm tag-alist))
              (apply #'append
                     (mapcar (lambda (mkr)
                               (org-with-point-at mkr
                                 (org-tag-tree--parse-tree)))
                             next-gen-group)))))
    (reverse tag-alist)))

(defun org-tag-tree--parse-tree-node (&optional regexp)
  "Interpret the current Org entry as a node (a string or REGEXP) in the tag tree."
  (let ((node-tags (org-get-tags nil 'local))
        (node-label (substring-no-properties
                     (org-get-heading t t t t))))
    (when (and (not (member org-tag-tree-ignore-tag node-tags))
               (length> node-label 0))
      (cons (format (if regexp "{%s}" "%s")
                    (mapconcat (lambda (ch)
                                 (let ((ch-str (char-to-string ch)))
                                   ;; Letters, numbers, "@" and "_" are allowed for tag name
                                   ;; See org#Tags
                                   (when (or regexp
                                             (string-match-p "[@_[:digit:][:alpha:]]"
                                                             ch-str))
                                     ch-str)))
                               node-label))
            (let ((org-trust-scanner-tags t))
              (when-let ((key (assoc-default "SELECTION_KEY" (org-entry-properties nil "SELECTION_KEY"))))
                (when (and (stringp key) (length> key 0))
                  (aref key 0))))))))

(provide 'org-tag-tree)

;;; org-tag-tree.el ends here
