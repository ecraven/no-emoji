;;; no-emoji.el --- Show :emoji-name: instead of emoji characters -*-coding: utf-8 -*-

;; Copyright (C) 2017 craven@gmx.net

;; Author: Peter <craven@gmx.net>
;; URL: https://github.com/ecraven/no-emoji.el
;; Package-Version: 20171129
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
  :type '(alist :key-type character :value-type character)
  :group 'no-emoji)

(defface no-emoji `((t (:foreground "medium slate blue"))) "Face used to highlight emoji replacement text.")

(defun no-emoji-create-display-table ()
  "Create a display table mapping emojis to their names.
Process every character defined by the ranges in `no-emoji-codepoint-ranges'.
Set `no-emoji' as the face for each glyph."
  (let ((dt (make-display-table))
        (names (unicode-property-table-internal 'name)))
    (dolist (range no-emoji-codepoint-ranges)
      (dotimes (i (- (cdr range) (car range)))
        (let ((cp (+ (car range) i)))
          (let ((name (get-unicode-property-internal names cp)))
            (when name
              (aset dt cp (coerce (mapcar
                                   (lambda (c)
                                     (make-glyph-code c 'no-emoji))
                                   (string-to-list (concat ":" (replace-regexp-in-string " " "-" (downcase name)) ":"))) 'vector)))))))
    dt))

(define-minor-mode no-emoji-minor-mode
  "Show emoji as :emoji-name:
See `no-emoji-codepoint-ranges'."
  :init-value nil
  :lighter " emoji"
  (if no-emoji-minor-mode
      (setq buffer-display-table (no-emoji-create-display-table))
    (setq buffer-display-table nil)))

(provide 'no-emoji)
;;; no-emoji.el ends here
