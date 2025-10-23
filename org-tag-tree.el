;;; org-tag-tree.el --- Manage Org tags in Org tree structure -*- lexical-binding: t -*-

;; Author: p-snow <public@p-snow.org>
;; Maintainer: p-snow <public@p-snow.org>
;; Version: 0.0.1
;; Package-Requires: ((emacs "30.1"))
;; Homepage: https://github.com/p-snow/org-tag-tree
;; Keywords: keywords

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

;;;; Variables

;;;; Customization

(defgroup org-tag-tree nil
  "Options for `org-tag-tree'."
  :group 'org
  :link '(url-link "https://github.com/p-snow/org-tag-tree"))

(defcustom org-tag-tree-global-tag-files nil
  "A list of files that contains tag tree for all org files."
  :type '(repeat :tag "List of files" file)
  :group 'org-tag-tree)

;;;; Functions

;;;###autoload
(defun org-tag-tree-load-global-tags ()
  ""
  (interactive)
  (let ((org-tags-exclude-from-inheritance
         (append org-tags-exclude-from-inheritance
                 '("tag" "grouptag"))))
    (setq org-tag-alist
          (append org-tag-alist
                  (mapcan (lambda (elm)
                            (assoc-default 'tag elm))
                          (org-map-entries #'org-tag-tree--tag-entry
                                           "tag|grouptag"
                                           org-tag-tree-global-tag-files))))))

;;;###autoload
(defun org-tag-tree-load-buffer-tags ()
  "Read tag trees from current buffer."
  (interactive)
  )

(defun org-tag-tree--tag-entry ()
  ""
  (let ((keywords org-scanner-tags)
        tag-alist tags)
    (when (member "tag" keywords)
      (add-to-list 'tags (list (substring-no-properties
                                (org-get-heading t t t t))))
      (setf (alist-get 'tag tag-alist)
            (reverse tags)))
    (when (member "grouptag" keywords)
      (add-to-list 'tags '(:startgrouptag))
      (add-to-list 'tags (list (substring-no-properties
                                (org-get-heading t t t t))))
      (when (org-goto-first-child)
        (add-to-list 'tags '(:grouptags))
        (add-to-list 'tags (list (substring-no-properties
                                  (org-get-heading t t t t))))
        (while (org-get-next-sibling)
          (add-to-list 'tags (list (substring-no-properties
                                    (org-get-heading t t t t))))))
      (add-to-list 'tags '(:endgrouptag))
      (setf (alist-get 'tag tag-alist)
            (reverse tags)))
    tag-alist))

(provide 'org-tag-tree)
;;; *org-tag-tree.el ends here
