;;; capf-english-helper.el --- English helper with capf interface

;;; Commentary:
;;
;; English helper with company interface.
;;

;;; Require
(require 'cl)
(require 'cl-lib)
(require 'company-english-helper-data)

;;; Code:

(defun english-helper-annotation (s)
  (let* ((translation (get-text-property 0 :initials s)))
    (format "  %s" translation)))

(defun capf-english-helper-search (&optional interactive)
  (interactive (list t))
  (if interactive
      (let ((completion-at-point-functions (list 'capf-english-helper-search)))
        (completion-at-point))
    (let* ((bds (bounds-of-thing-at-point 'symbol))
           (start (car bds))
           (end (cdr bds))
           (prefix (if (and start end)
                       (buffer-substring-no-properties start end)
                     "")))
      (list start end (company-english-helper-get-items prefix)
            :annotation-function 'english-helper-annotation))))

(defun company-english-helper-get-items (prefix)
  (let* ((prefix-match-candidates
          (cl-remove-if-not
           (lambda (c)  (string-prefix-p (downcase prefix) c))
           english-helper-completions)))
    (company-english-helper-convert-candidates prefix prefix-match-candidates)))

(defun company-english-helper-convert-candidates (input candidates)
  (cond ((company-english-helper-upcase-string-p input)
         (mapcar 'upcase candidates))
        ((company-english-helper-capitalize-string-p input)
         (mapcar 'capitalize candidates))
        (t candidates)))

(defun company-english-helper-upcase-string-p (str)
  (let ((case-fold-search nil))
    (and (> (length str) 1)
         (string-match-p "\\`[A-Z]*\\'" str))))

(defun company-english-helper-capitalize-string-p (str)
  (let ((case-fold-search nil))
    (string-match-p "\\`[A-Z][a-z]*\\'" str)))

(provide 'capf-english-helper)

;;; company-english-helper.el ends here
