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
  ""
  (interactive)
  (setq org-tag-alist
        (cl-reduce
         (lambda (result next)
           (append result '((:newline)) next))
         (let ((org-tags-exclude-from-inheritance
                (list org-tag-tree-global-tag-matcher)))
           (org-map-entries #'org-tag-tree--parse-tree
                            org-tag-tree-global-tag-matcher
                            org-tag-tree-global-tag-files)))))

;;;###autoload
(defun org-tag-tree-load-buffer-tags ()
  "Overwrite the default buffer tags by tag-tree defined in the current Org buffer.

Note that original buffer tags defined with #+TAGS: keyword are no longer in effect after calling this function."
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
  ""
  (let ((root-keywords (org-get-tags nil 'local))
        tag-alist next-gen-group)
    (if (not (save-excursion (org-goto-first-child)))
        (push (org-tag-tree--parse-tree-tag)
              tag-alist)
      (push (if (member org-tag-tree-exclusive-tag root-keywords)
                '(:startgroup)
              '(:startgrouptag))
            tag-alist)
      (when-let ((group-tag (org-tag-tree--parse-tree-tag)))
        (push group-tag tag-alist)
        (push '(:grouptags) tag-alist))
      (cl-loop initially (org-goto-first-child)
               do (push (org-tag-tree--parse-tree-tag
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

(defun org-tag-tree--parse-tree-tag (&optional regexp)
  ""
  (let ((tag-label (substring-no-properties
                    (org-get-heading t t t t))))
    (when (length> tag-label 0)
      (cons (format (if regexp "{%s}" "%s")
                    (mapconcat (lambda (ch)
                                 (let ((ch-str (char-to-string ch)))
                                   ;; Letters, numbers, "@" and "_" are allowed for tag name
                                   ;; See org#Tags
                                   (when (or regexp
                                             (string-match-p "[@_[:digit:][:alpha:]]"
                                                             ch-str))
                                     ch-str)))
                               tag-label))
            (let ((org-trust-scanner-tags t))
              (when-let ((key (assoc-default "SELECTION_KEY" (org-entry-properties nil "SELECTION_KEY"))))
                (when (and (stringp key) (length> key 0))
                  (aref key 0))))))))

(provide 'org-tag-tree)

;;; org-tag-tree.el ends here
