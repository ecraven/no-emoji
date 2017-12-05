;;; no-emoji.el --- Show :emoji-name: instead of emoji characters -*-coding: utf-8; lexical-binding: t; -*-

;; Copyright (C) 2017 craven@gmx.net

;; Author: Peter <craven@gmx.net>
;; URL: https://github.com/ecraven/no-emoji
;; Package-Version: 20171205
;; Package-Requires: ((emacs "24"))
;; Version: 0.1
;; Keywords: extensions
;; Created: 2017-11-29

;;; License:

;; Licensed under the GPLv3.

;;; Commentary:
;;
;; Run M-x no-emoji-minor-mode to replace all emoji with :emoji-name:
;;
;; You can customize `no-emoji' face to alter the appearance.
;;
;; You can adapt the codepoint ranges in `no-emoji-codepoint-ranges' to customize which codepoints will be replaced.
;;
;; You can redefine `no-emoji-displayable-unicode-name' to change the way the display names are generated.
;; Do this *before* enabling the minor mode.
;;
;; This package sets buffer-display-table locally.
;;
;;; Code:
(defgroup no-emoji nil
  "Minor mode for replacing emoji with their names."
  :group 'multimedia
  :prefix "no-emoji-")

(defcustom no-emoji-codepoint-ranges
  '((#x1f000 . #x1f9ff))
  "A list of codepoint ranges (inclusive) that will be replaced."
  :type '(alist :key-type (character :tag "First character")
                :value-type (character :tag "Last character"))
  :group 'no-emoji)

(defface no-emoji `((t (:inherit dired-header)))
  "Face used to highlight emoji replacement text."
  :group 'no-emoji)

(defun no-emoji-displayable-unicode-name (name)
  "Convert NAME to the string that should be shown.

E.g. convert spaces to -, surround with :."
  (concat ":" (replace-regexp-in-string " " "-" (downcase name)) ":"))

(defun no-emoji-update-display-table (fill-p)
  "If FILL-P is true, enter the relevant glyphs into the buffer-local display-table.

If it is false, remove them.

Process every character defined by the ranges in `no-emoji-codepoint-ranges'.
Set `no-emoji' as the face for each glyph."
  (unless buffer-display-table
    (setq buffer-display-table (make-display-table)))
  (let ((names (unicode-property-table-internal 'name)))
    (dolist (range no-emoji-codepoint-ranges)
      (dotimes (i (- (cdr range) (car range)))
        (let ((codepoint (+ (car range) i)))
          (let ((name (get-unicode-property-internal names codepoint)))
            (when name
              (aset buffer-display-table
                    codepoint
                    (if fill-p
                        (vconcat (mapcar
                                  (lambda (c)
                                    (make-glyph-code c 'no-emoji))
                                  (string-to-list (no-emoji-displayable-unicode-name name))))
                      nil)))))))
    buffer-display-table))

;;;###autoload
(define-minor-mode no-emoji-minor-mode
  "Show emoji as :emoji-name:

Also see `no-emoji-codepoint-ranges' and `no-emoji-displayable-unicode-name'."
  :init-value nil
  :lighter " emoji"
  (no-emoji-update-display-table no-emoji-minor-mode))

(provide 'no-emoji)
;;; no-emoji.el ends here
