;;; org-tag-tree-test.el --- Tests for org-tag-tree -*- lexical-binding: t -*-

;;; Commentary:
;; Basic tests that exercise the public entry points as a sanity check.

;;; Code:

(require 'ert)
(require 'org-tag-tree)

(defconst org-tag-tree-test--data-directory
  (expand-file-name
   "data"
   (file-name-directory (or load-file-name buffer-file-name default-directory)))
  "Directory containing fixtures for org-tag-tree tests.")

;; Scenario: Basic global tag tree with single root and children.
(ert-deftest org-tag-tree-load-global-tags/basic ()
  "Loading global tags populates `org-tag-alist' using the provided tag tree."
  (let* ((org-tag-tree-global-tag-files
          (list (expand-file-name "global-tags.org"
                                  org-tag-tree-test--data-directory)))
         (org-tag-tree-global-tag-persistent nil)
         (org-tag-alist nil)
         (org-tag-persistent-alist nil))
    (org-tag-tree-load-global-tags)
    (should (equal org-tag-alist
                   '((:startgrouptag)
                     ("Root")
                     (:grouptags)
                     ("Child1")
                     ("Child2")
                     (:endgrouptag))))
    (should (null org-tag-persistent-alist))))

;; Scenario: Root matcher without children still registers as a tag.
(ert-deftest org-tag-tree-load-global-tags/degenerate ()
  "A matcher with no children produces a single tag entry."
  (let* ((org-tag-tree-global-tag-files
          (list (expand-file-name "global-tags-degenerate.org"
                                  org-tag-tree-test--data-directory)))
         (org-tag-tree-global-tag-persistent nil)
         (org-tag-alist nil)
         (org-tag-persistent-alist nil))
    (org-tag-tree-load-global-tags)
    (should (equal org-tag-alist
                   '(("Solo"))))
    (should (null org-tag-persistent-alist))))

;; Scenario: Multiple global roots separated by newline markers.
(ert-deftest org-tag-tree-load-global-tags/multiple-roots ()
  "Multiple root tag trees are separated by newline markers."
  (let* ((org-tag-tree-global-tag-files
          (list (expand-file-name "global-tags-multiple.org"
                                  org-tag-tree-test--data-directory)))
         (org-tag-tree-global-tag-persistent nil)
         (org-tag-alist nil)
         (org-tag-persistent-alist nil))
    (org-tag-tree-load-global-tags)
    (should (equal org-tag-alist
                   '((:startgrouptag)
                     ("First")
                     (:grouptags)
                     ("ChildA")
                     (:endgrouptag)
                     (:newline)
                     (:startgrouptag)
                     ("Second")
                     (:grouptags)
                     ("ChildB")
                     (:endgrouptag))))
    (should (null org-tag-persistent-alist))))

;; Scenario: Persistence flag moves tree into persistent tag alist.
(ert-deftest org-tag-tree-load-global-tags/persistent ()
  "When persistence is enabled, tags populate `org-tag-persistent-alist'."
  (let* ((org-tag-tree-global-tag-files
          (list (expand-file-name "global-tags.org"
                                  org-tag-tree-test--data-directory)))
         (org-tag-tree-global-tag-persistent t)
         (org-tag-alist nil)
         (org-tag-persistent-alist nil))
    (org-tag-tree-load-global-tags)
    (should (null org-tag-alist))
    (should (equal org-tag-persistent-alist
                   '((:startgrouptag)
                     ("Root")
                     (:grouptags)
                     ("Child1")
                     ("Child2")
                     (:endgrouptag))))))

;; Scenario: Switching persistence toggles between alists.
(ert-deftest org-tag-tree-load-global-tags/persistence-toggle ()
  "Switching persistence updates the appropriate tag alists."
  (let* ((org-tag-tree-global-tag-files
          (list (expand-file-name "global-tags.org"
                                  org-tag-tree-test--data-directory)))
         (org-tag-tree-global-tag-persistent t)
         (org-tag-alist '(stale))
         (org-tag-persistent-alist nil))
    (org-tag-tree-load-global-tags)
    (should (equal org-tag-persistent-alist
                   '((:startgrouptag)
                     ("Root")
                     (:grouptags)
                     ("Child1")
                     ("Child2")
                     (:endgrouptag))))
    (setq org-tag-tree-global-tag-files
          (list (expand-file-name "global-tags-multiple.org"
                                  org-tag-tree-test--data-directory))
          org-tag-tree-global-tag-persistent nil
          org-tag-alist nil
          org-tag-persistent-alist nil)
    (org-tag-tree-load-global-tags)
    (should (equal org-tag-alist
                   '((:startgrouptag)
                     ("First")
                     (:grouptags)
                     ("ChildA")
                     (:endgrouptag)
                     (:newline)
                     (:startgrouptag)
                     ("Second")
                     (:grouptags)
                     ("ChildB")
                     (:endgrouptag))))
    (should (null org-tag-persistent-alist))))

;; Scenario: Complex tree covering regex node, selection key, and exclusive group.
(ert-deftest org-tag-tree-load-global-tags/complex ()
  "Complex tag trees produce the expected tag alist entries."
  (let* ((org-tag-tree-global-tag-files
          (list (expand-file-name "global-tags-complex.org"
                                  org-tag-tree-test--data-directory)))
         (org-tag-tree-global-tag-persistent nil)
         (org-tag-alist nil)
         (org-tag-persistent-alist nil))
    (org-tag-tree-load-global-tags)
    (should (equal org-tag-alist
                   '((:startgroup)
                     ("Exclusive")
                     (:grouptags)
                     ("Alpha")
                     ("Beta")
                     (:endgroup)
                     (:newline)
                     (:startgrouptag)
                     ("Standard")
                     (:grouptags)
                     ("{^Foo.*$}")
                     ("Select" . ?S)
                     (:endgrouptag))))
    (should (null org-tag-persistent-alist))))

;; Scenario: Ignored subtrees are excluded from global tags.
(ert-deftest org-tag-tree-load-global-tags/ignored-subtree ()
  "Subtrees tagged with the ignore keyword are skipped entirely."
  (let* ((org-tag-tree-global-tag-files
          (list (expand-file-name "global-tags-ignored-deep.org"
                                  org-tag-tree-test--data-directory)))
         (org-tag-tree-global-tag-persistent nil)
         (org-tag-alist nil)
         (org-tag-persistent-alist nil))
    (org-tag-tree-load-global-tags)
    (should (equal org-tag-alist
                   '((:startgrouptag)
                     ("Root")
                     (:grouptags)
                     ("Keep")
                     (:endgrouptag))))
    (should (null org-tag-persistent-alist))))

;; Scenario: Exclusive grouping applied to an intermediate node.
(ert-deftest org-tag-tree-load-global-tags/intermediate-exclusive ()
  "Exclusive grouping on a child node is preserved."
  (let* ((org-tag-tree-global-tag-files
          (list (expand-file-name "global-tags-exclusive-middle.org"
                                  org-tag-tree-test--data-directory)))
         (org-tag-tree-global-tag-persistent nil)
         (org-tag-alist nil)
         (org-tag-persistent-alist nil))
    (org-tag-tree-load-global-tags)
    (should (equal org-tag-alist
                   '((:startgrouptag)
                     ("Root")
                     (:grouptags)
                     ("Group")
                     (:endgrouptag)
                     (:startgroup)
                     ("Group")
                     (:grouptags)
                     ("Choice1")
                     ("Choice2")
                     (:endgroup))))
    (should (null org-tag-persistent-alist))))

;; Scenario: Buffer loader merges persistent tags with buffer tree.
(ert-deftest org-tag-tree-load-buffer-tags/basic ()
  "Buffer-local tag trees merge with persistent tags."
  (let* ((org-tag-tree-buffer-tag-matcher "tag")
         (org-tag-persistent-alist
          '((:startgrouptag)
            ("Persistent")
            (:grouptags)
            ("One")
            (:endgrouptag)))
         (org-current-tag-alist nil)
         (org-tag-groups-alist nil)
         (buffer (find-file-noselect
                  (expand-file-name "buffer-tags.org"
                                    org-tag-tree-test--data-directory))))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (org-tag-tree-load-buffer-tags)
          (should (equal org-current-tag-alist
                         '((:startgrouptag)
                           ("Persistent")
                           (:grouptags)
                           ("One")
                           (:endgrouptag)
                           (:startgrouptag)
                           ("Root")
                           (:grouptags)
                           ("Child")
                           (:endgrouptag)
                           (:startgrouptag)
                           ("Child")
                           (:grouptags)
                           ("Leaf")
                           (:endgrouptag))))
          (should (equal org-tag-groups-alist
                       '(("Persistent" "One")
                         ("Root" "Child")
                         ("Child" "Leaf")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;; Scenario: Buffer tree with regex child produces grouped regexp tag.
(ert-deftest org-tag-tree-load-buffer-tags/regexp ()
  "Buffer tag trees handle regexp nodes when building groups."
  (let* ((org-tag-tree-buffer-tag-matcher "tag")
         (org-tag-persistent-alist nil)
         (org-current-tag-alist nil)
         (org-tag-groups-alist nil)
         (buffer (find-file-noselect
                  (expand-file-name "buffer-tags-regexp.org"
                                    org-tag-tree-test--data-directory))))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (org-tag-tree-load-buffer-tags)
          (should (equal org-current-tag-alist
                         '((:startgrouptag)
                           ("Root")
                           (:grouptags)
                           ("{^Foo.*$}")
                           (:endgrouptag))))
          (should (equal org-tag-groups-alist
                         '(("Root" "{^Foo.*$}")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;; Scenario: Buffer-exclusive group emits correct start/end markers.
(ert-deftest org-tag-tree-load-buffer-tags/exclusive ()
  "Exclusive groups in buffer tag trees use group markers."
  (let* ((org-tag-tree-buffer-tag-matcher "tag")
         (org-tag-persistent-alist nil)
         (org-current-tag-alist nil)
         (org-tag-groups-alist nil)
         (buffer (find-file-noselect
                  (expand-file-name "buffer-tags-exclusive.org"
                                    org-tag-tree-test--data-directory))))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (org-tag-tree-load-buffer-tags)
          (should (equal org-current-tag-alist
                         '((:startgroup)
                           ("Exclusive")
                           (:grouptags)
                           ("Choice1")
                           ("Choice2")
                           (:endgroup))))
          (should (equal org-tag-groups-alist
                         '(("Exclusive" "Choice1" "Choice2")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;; Scenario: Ignored buffer subtree drops descendants from results.
(ert-deftest org-tag-tree-load-buffer-tags/ignored-subtree ()
  "Ignoring subtrees prevents tags and descendants from being added."
  (let* ((org-tag-tree-buffer-tag-matcher "tag")
         (org-tag-persistent-alist nil)
         (org-current-tag-alist nil)
         (org-tag-groups-alist nil)
         (buffer (find-file-noselect
                  (expand-file-name "buffer-tags-ignored.org"
                                    org-tag-tree-test--data-directory))))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (org-tag-tree-load-buffer-tags)
          (should (equal org-current-tag-alist
                         '((:startgrouptag)
                           ("Root")
                           (:grouptags)
                           ("Keep")
                           (:endgrouptag))))
          (should (equal org-tag-groups-alist
                         '(("Root" "Keep")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;; Scenario: Buffer with multiple roots separates groups with newline marker.
(ert-deftest org-tag-tree-load-buffer-tags/multiple-roots ()
  "Multiple tag-tree roots in a buffer insert newline separators."
  (let* ((org-tag-tree-buffer-tag-matcher "tag")
         (org-tag-persistent-alist nil)
         (org-current-tag-alist nil)
         (org-tag-groups-alist nil)
         (buffer (find-file-noselect
                  (expand-file-name "buffer-tags-multiple.org"
                                    org-tag-tree-test--data-directory))))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (org-tag-tree-load-buffer-tags)
          (should (equal org-current-tag-alist
                         '((:startgrouptag)
                           ("First")
                           (:grouptags)
                           ("A1")
                           (:endgrouptag)
                           (:newline)
                           (:startgrouptag)
                           ("Second")
                           (:grouptags)
                           ("B1")
                           (:endgrouptag))))
          (should (equal org-tag-groups-alist
                         '(("First" "A1")
                           ("Second" "B1")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;; Scenario: Buffer node with selection key stores the key on the tag.
(ert-deftest org-tag-tree-load-buffer-tags/selection-key ()
  "Selection keys propagate into the tag alist."
  (let* ((org-tag-tree-buffer-tag-matcher "tag")
         (org-tag-persistent-alist nil)
         (org-current-tag-alist nil)
         (org-tag-groups-alist nil)
         (buffer (find-file-noselect
                  (expand-file-name "buffer-tags-selection.org"
                                    org-tag-tree-test--data-directory))))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (org-tag-tree-load-buffer-tags)
          (should (equal org-current-tag-alist
                         '((:startgrouptag)
                           ("Root")
                           (:grouptags)
                           ("Select" . ?S)
                           (:endgrouptag))))
          (should (equal org-tag-groups-alist
                         '(("Root" "Select")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

;; Scenario: Non-default matcher restricts buffer scan to tagged roots.
(ert-deftest org-tag-tree-load-buffer-tags/custom-matcher ()
  "Custom buffer tag matcher selects the appropriate roots."
  (let* ((org-tag-tree-buffer-tag-matcher "buf")
         (org-tag-persistent-alist nil)
         (org-current-tag-alist nil)
         (org-tag-groups-alist nil)
         (buffer (find-file-noselect
                  (expand-file-name "buffer-tags-custom.org"
                                    org-tag-tree-test--data-directory))))
    (unwind-protect
        (with-current-buffer buffer
          (org-mode)
          (org-tag-tree-load-buffer-tags)
          (should (equal org-current-tag-alist
                         '((:startgrouptag)
                           ("Root")
                           (:grouptags)
                           ("Child")
                           (:endgrouptag))))
          (should (equal org-tag-groups-alist
                         '(("Root" "Child")))))
      (when (buffer-live-p buffer)
        (kill-buffer buffer)))))

(provide 'org-tag-tree-test)

;;; org-tag-tree-test.el ends here
