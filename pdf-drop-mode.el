;;; pdf-drop-mode.el --- Get DOI from PDF files dropped onto a buffer -*- lexical-binding: t -*-

;; Copyright (C) 2022 Nicolas P. Rougier

;; Author: Nicolas P. Rougier <Nicolas.Rougier@inria.fr>
;; Homepage: https://github.com/rougier/pdf-drop-mode
;; Keywords: convenience
;; Version: 0.2.0

;; Package-Requires: ((emacs "27.1"))

;; This file is not part of GNU Emacs.

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; For a full copy of the GNU General Public License
;; see <https://www.gnu.org/licenses/>.

;;; Documentation
;;
;; pdf-drop-mode is a convenient mode that search for the identification (DOI or
;; axiv-id) of any PDF file that is dropped onto a buffer. To do that, the mode
;; search the identification using several different methods (whose order can be
;; specified):
;;
;; - doi/metadata: search DOI in PDF metadata tags (using exiftool)
;; - doi/content: search the content of the file to try to locate a DOI regex on
;;                the first page (PDF is transformed into text using the pdftotext
;;                utility)
;; - doi/title: ask the user to enter the title of the file and query the crossref
;;              database to try to get a correspondong DOI.
;; - doi/user: ask user to enter the DOI of the PDF.
;; - arxiv/content: search the content of the file to try to locate a arxiv-id regex
;;                  on the first page (PDF is transformed into text using the
;;                  pdftotext utility)
;; - arxiv/user: ask user to enter the arxiv-id of the PDF.
;;
;; If a method succeeds, a user-defined hook is ran with the filename and the ID
;; as arguments (ID is a cons (ID-type . ID)). Else, search has failed.


;;; News

;; Version 0.2.0
;; - API modification for taling into account arxiv papers.
;; - arxiv papers are now identified.

;; Version 0.1.0
;; Initial release

;;; Commentary:
;; Add ISBN support.

;;; Code:
(require 'url-http)

(defcustom pdf-drop-pdftotext-path "pdftotext"
  "Path to the pdftotext executable (used to extract DOI from the content of a pdf)."
  :type 'file)

(defcustom pdf-drop-exiftool-path "exiftool"
  "Path to the exiftool executable (used to extract TITLE from pdf metadata)."
  :type 'file)

(defcustom pdf-drop-search-methods '(isbn/content
                                     doi/metadata
                                     doi/content
                                     arxiv/content
                                     doi/title
                                     doi/user)
  "Ordered list of methods to use to get the an identifier (doi or arxiv-id) from a pdf."
  :type 'list)

(defcustom pdf-drop-search-hook nil
  "A pointer to a function that is called when a search ends.
Called function is provided with the filename and the associated
doi (or nil) as arguments."
  :type 'function)

(defvar pdf-drop--isbn-regex
  "\\(?1:ISBN[-]*\\(1[03]\\)*[ ]*\\(: \\)*\\)\\(?2:\\([0-9Xx][- ]*\\)\\{13\\}\\|\\([0-9Xx][- ]*\\)\\{10\\}\\)"
  "Regular expression matching an ISBN."
  )
;; "\\(ISBN[-]*\\(1[03]\\)*[ ]*\\(: \\)\\{0,1\\}\\)*\\(\\([0-9Xx][- ]*\\)\\{13\\}\\|\\([0-9Xx][- ]*\\)\\{10\\}\\)"
;; "\\(?1:ISBN-*\\(1[03]\\)*\\(: \\)\\{0,1\\}\\)\\(?2:\\([0-9Xx][ -]*\\)\\{13\\}\\|\\([0-9Xx][ -]*\\)\\{10\\}\\)"

;; This will fail if the DOI is split over two lines. Such case may happend when
;; pdftotext is used to convert the PDF to text.
(defvar pdf-drop--doi-regex "\\(10\\.[0-9]\\{4,9\\}/[-+._;()/:A-Z0-9]+\\)"
  "Regular expressions matching a DOI.")

(defvar pdf-drop--crossref-search-url "http://search.crossref.org/dois?q=%s&rows=10"
  "URL to the crossref search API.")

(defvar pdf-drop--crossref-query-url "http://dx.doi.org/%s"
  "URL to the crossref query API.")

;; Adapted from https://github.com/jkitchin/org-ref (org-ref-arxiv.el)
;; Copyright (C) 2015-2021 John Kitchin (GPL v2)
(defvar pdf-drop--arxiv-regex "\\(arXiv:\\)\\([0-9]\\{4\\}[.][0-9]\\{4,5\\}\\)\\(v[0-9]+\\)?"
    "Regular expressions matching an arxiv-id (new format, 2007).")

(defvar pdf-drop--arxiv-search-url "https://ui.adsabs.harvard.edu/abs/%s/exportcitation"
  "URL to arxiv searh yAPI.")

(defvar pdf-drop--arxiv-query-url "http://adsabs.harvard.edu/cgi-bin/bib_query?arXiv:%s"
  "URL to the arxiv query API.")

(defun pdf-drop-validate-isbn (isbn)
  "Check the validity of ISBN by means of checksum."

  (catch 'test
    ;; Get checksum and remove non ISBN characters.
    (let* ((checksum (substring isbn -1))
           (isbn (substring (replace-regexp-in-string "[- ]" "" isbn) 0 -1))
           (check 0))
      ;; (message "isbn: %s" isbn)
      ;; (message "checksum: %s" checksum)
      ;; map isbn string to number list
      (setq isbn (cl-map 'list 'string-to-number (mapcar 'char-to-string isbn)))
      ;;(message "isbnnumlist: %s" isbn)

      ;; Check ISBN-10 or ISBN-13 format
      (if (= (length isbn) 9) ; ISBN-10
          (progn
            ;;(message "ISBN10") ; calc the checksum
            (cl-loop for i downfrom 10 to 2
                     sum (* i (car (nthcdr (- 10 i) isbn))) into sum
                     finally (setq check sum))
            ;;(message "sum: %d" check)
            (setq check (- 11 (% check 11)))
            (cond ((= check 10)(setq check "X"))
                  ((= check 11)(setq check "0"))
                  (t (setq check (number-to-string check)))))
        ;;(message "check: %s" check))
        ;;(message "ISBN13")
        (cl-loop for i from 0 to 11
                 if (cl-oddp i)
                 sum (* (+ (* (% i 2) 2) 1) (car (nthcdr i isbn))) into sum
                 else
                 sum (* (+ (* (% i 2) 2) 1) (car (nthcdr i isbn))) into sum
                 finally (setq check sum))
        ;;(message "check: %d" check)
        (setq check (- 10 (% check 10)))
        (if (= check 10)(setq check "0")(setq check (number-to-string check))))
      ;;(message "checksum: %s" check))
      (if (equal check checksum)
          (message "checksum ok"))
      )))
;;(pdf-drop-validate-isbn isbn)

(setq isbn "0-520-23249-6")
(setq isbn "1-56389-016-X")
(setq isbn "1 56389 016 X")
(setq isbn "978-3-540-19568-9")

(defun pdf-drop-validate-doi (doi)
  "Check if DOI is valid by querying crossref."

  (let* ((url (format "https://hdl.handle.net/%s?noredirect" doi))
         (status (url-http-symbol-value-in-buffer 'url-http-response-status
                                                  (url-retrieve-synchronously url))))
    (eq 200 status)))

;; Adapted from https://github.com/jkitchin/org-ref (org-ref-arxiv.el)
;; Copyright (C) 2015-2021 John Kitchin (GPL v2)
(defun pdf-drop-validate-arxiv-id (arxiv-id)
  "Check if ARXIV-ID is valid by querying arXiv."
  
  (let* ((url (format pdf-drop--arxiv-query-url arxiv-id))
         (status (url-http-symbol-value-in-buffer 'url-http-response-status
                                                  (url-retrieve-synchronously url))))
    (eq 200 status)))

;; Adapted from https://github.com/jkitchin/org-ref (doi-utils.el)
;; Copyright (C) 2015-2021 John Kitchin (GPL v2)
(defun pdf-drop-get-doi-from-title (title &optional score-threshold)
  "Query crossref.org for TITLE and return the corresponding doi.

  When multiple canditates are found, the one with the smallest
  levenshtein distance is chosen if its distance to TITLE is less
  than SCORE-THRESHOLD.  Else, user is asked to choose among 5 best
  candidates.  If no SCORE-THRESHOLD is given, the first candidate
  doi is returned."

  (interactive "sTitle: ")
  (with-current-buffer (url-retrieve-synchronously
                        (format
                         pdf-drop--crossref-search-url
                         (url-hexify-string title)))
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward "<i>\\|</i>" nil t)
      	(replace-match ""))
	    (goto-char (point-min))
	    (while (re-search-forward "&amp;" nil t)
	(replace-match "&"))
         (goto-char (point-min))
         (while (re-search-forward "&quot;" nil t)
           (replace-match "\\\"" nil t)))
    (setq raw-json-string
          (buffer-substring url-http-end-of-headers (point-max)))
    (setq json-string
          (decode-coding-string (encode-coding-string raw-json-string 'utf-8) 'utf-8))
    (setq json-data
          (json-read-from-string json-string))

    (let* (;; (score-threshold (or score-threshold 10))
           (candidates (mapcar (lambda (x)
                                 (let* ((x-full (cdr (assoc 'fullCitation x)))
                                        (x-title (cdr (assoc 'title x)))
                                        (x-display-title (if (string-match "\\([0-9]\\{4\\}\\)" x-full)
                                                             (format "%s (%s)" x-title (match-string 1 x-full))
                                                           (format "%s (????)" x-title)))
                                        (x-doi (cdr (assoc 'doi x))))
                                   (cons x-display-title  x-doi)))
                               json-data))
           (sorted-candidates (sort candidates
                                    #'(lambda (item-1 item-2)
                                        (let ((title-1 (substring (car item-1) 0 -7))
                                              (title-2 (substring (car item-2) 0 -7)))
                                          (< (string-distance title title-1)
                                             (string-distance title title-2))))))
           (best-match (caar sorted-candidates))
           (best-score (string-distance title (substring best-match 0 -7)))
           (selected (if (or (= (length sorted-candidates) 1)
                             (not score-threshold)
                             (< best-score score-threshold))
                         best-match
                       (completing-read "Matched title: " sorted-candidates nil nil
                                        (if (< best-score 10) best-match nil))))
	         (doi (cdr (assoc selected sorted-candidates))))
      doi)))

(defun pdf-drop-get-isbn-from-content (file)
  "Extract isbn from a pdf FILE content (after conversion to text)."

  (with-temp-buffer
    (let ((status (call-process pdf-drop-pdftotext-path nil '(t nil) nil
                                "-f" "1" "-l" "6" "-enc" "UTF-8" file "-")))
      (goto-char (point-min))
      (if (and (eq status 0)
               (> (buffer-size) 0)
               ;;(message "buffer-size")
               (search-forward-regexp pdf-drop--isbn-regex nil t))
          (string-trim (match-string 2) "[ \\t\\n\\r()]+" "[ \\t\\n\\r()]+")
        ))))

(defun pdf-drop-get-doi-from-content (file)
  "Extract doi from a pdf FILE content (after conversion to text)."

  (with-temp-buffer
    (let ((status (call-process pdf-drop-pdftotext-path nil '(t nil) nil
                                "-f" "1" "-l" "1" "-enc" "UTF-8" file "-")))
      (goto-char (point-min))
      (if (and (eq status 0)
               (> (buffer-size) 0)
               (search-forward-regexp pdf-drop--doi-regex nil t))
          (string-trim (match-string 1) "[ \\t\\n\\r()]+" "[ \\t\\n\\r()]+")
        ))))

(defun pdf-drop-get-tag-from-metadata (file tag)
  "Get TAG from FILE metadata."

  (with-temp-buffer
    (let ((status (call-process pdf-drop-exiftool-path nil '(t nil) nil
                                "-s" "-s" "-s" (concat "-" tag) file)))
      (if (and (eq status 0) (> (buffer-size) 0))
          (string-trim (buffer-substring (point-min) (point-max)))))))
  

(defun pdf-drop-get-doi-from-metadata (file)
  "Get DOI from FILE metadata."
  
  (pdf-drop-get-tag-from-metadata file "DOI"))

(defun pdf-drop-get-title-from-metadata (file)
  "Get TITLE from FILE metadata."
  
  (pdf-drop-get-tag-from-metadata file "TITLE"))

(defun pdf-drop-get-arxiv-id-from-content (file)
  "Extract arxiv-id from a pdf FILE content (after conversion to text)."

  (with-temp-buffer
    (let ((status (call-process pdf-drop-pdftotext-path nil '(t nil) nil
                                "-f" "1" "-l" "1" "-enc" "UTF-8" file "-")))
      (goto-char (point-min))
      (if (and (eq status 0)
               (> (buffer-size) 0)
               (search-forward-regexp pdf-drop--arxiv-regex nil t))
          (string-trim (match-string 2) "[ \\t\\n\\r()]+" "[ \\t\\n\\r()]+")))))

(defun pdf-drop--file-dnd-fallback (uri action)
  (let ((dnd-protocol-alist
         (rassq-delete-all 'pdf-drop--file-dnd-protocol
          (copy-alist dnd-protocol-alist))))
    (dnd-handle-one-url nil action uri)))

(defun pdf-drop--file-dnd-protocol (uri action)
  ;; (condition-case nil
  ;;      (pdf-drop--process (substring uri 7))
  ;;    (error
  ;;     (pdf-drop--file-dnd-fallback uri action))))
  (let ((file (dnd-get-local-file-name uri)))
    (pdf-drop--process file)))

(defun pdf-drop--process (file)
  "Try to get the DOI associated to FILE."

  (let ((file-id))
    (catch 'found
      (dolist (method pdf-drop-search-methods)
        (message "method: %s" method)
        (cond ((eq method 'isbn/content)
               (let ((isbn (pdf-drop-get-isbn-from-content file)))
                 (message "ISBN: %s" isbn)
                 (if (and isbn (pdf-drop-validate-isbn isbn))
                     (progn
                       (setq file-id `(isbn . ,isbn))
                       (throw 'found file-id))
                   (message "isbn/content method failed for %s" file))))

              ((eq method 'doi/content)
               (let ((doi (pdf-drop-get-doi-from-content file)))
                 (if (and doi (pdf-drop-validate-doi doi))
                     (progn
                       (setq file-id `(doi . ,doi))
                       (throw 'found file-id))
                   (message "doi/content method failed for %s" file))))

              ((eq method 'arxiv/content)
               (let ((arxiv-id (pdf-drop-get-arxiv-id-from-content file)))
                 (if (and arxiv-id (pdf-drop-validate-arxiv-id arxiv-id))
                     (progn
                       (setq file-id `(arxiv . ,arxiv-id))
                       (throw 'found file-id))
                   (message "arxiv/content method failed for %s" file))))

              ((eq method 'doi/metadata)
               (let ((doi (pdf-drop-get-doi-from-metadata file)))
                 (if (and doi (pdf-drop-validate-doi doi))
                     (progn
                       (setq file-id `(doi . ,doi))
                       (throw 'found file-id))
                   (message "doi/metadata method failed for %s" file))))

              ((eq method 'doi/title)
               (let ((buffer (find-file file)))
                 (with-current-buffer buffer
                   (unwind-protect
                       (let* ((title (read-string
                                      (format "Enter title for %s: "
                                              (file-name-nondirectory file))))
                              (doi (pdf-drop-get-doi-from-title title 5)))
                         (if (and doi (pdf-drop-validate-doi doi))
                             (progn
                               (setq file-id `(doi . ,doi))
                               (throw 'found file-id))
                           (message "doi/title method failed for %s" file)))
                     (kill-buffer buffer)))))

              ((eq method 'doi/user)
               (let ((buffer (find-file file)))
                 (with-current-buffer buffer
                   (unwind-protect
                       (let* ((doi (read-string
                                    (format "Enter DOI for %s: "
                                            (file-name-nondirectory file)))))
                         (if (and doi (pdf-drop-validate-doi doi))
                             (progn
                               (setq file-id `(doi . ,doi))
                               (throw 'found file-id))
                           (message "doi/user method failed for %s" file)))
                     (kill-buffer buffer))))))))

    (if file-id
        (progn
          (message "ID found: %s" file-id)
          (if pdf-drop-search-hook
              (apply pdf-drop-search-hook (list file file-id))))
      (message "Search failed for %s" file))))


(defun pdf-drop--bibtex-from-doi (doi)
  "Retrieve (raw) bibtex information for DOI using crossref API."
  
  (let ((url-mime-accept-string "text/bibliography;style=bibtex"))
    (with-current-buffer
        (url-retrieve-synchronously (format pdf-drop--crossref-query-url doi))
      (decode-coding-string
       (buffer-substring (string-match "@" (buffer-string)) (point)) 'utf-8))))


(defun pdf-drop--bibtex-from-arxiv-id (arxiv-id)
  "Retrieve (raw) bibtex information for ARXIV-ID item using arxiv API."

  (let* ((arxiv-code
          (with-current-buffer
              (url-retrieve-synchronously (format pdf-drop--arxiv-query-url arxiv-id))
            (search-forward-regexp "<link rel=\"canonical\" href=\"http://ui.adsabs.harvard.edu/abs/\\(.*\\)/abstract\"/>")
            (match-string 1)))
         (bibtex (when arxiv-code
                   (with-current-buffer
                       (url-retrieve-synchronously (format pdf-drop--arxiv-search-url arxiv-code))
                     (when (re-search-forward
	                          "<textarea.*>\\(.*\\(?:\n.*\\)*?\\(?:\n\\s-*\n\\|\\'\\)\\)</textarea>"
	                          nil t)
                       (xml-substitute-special (match-string 1)))))))
    bibtex))


(define-minor-mode pdf-drop-mode
  "Minor mode that search for the doi of any pdf file dropped onto a buffer."

  :global t
  :init-value nil

  (let ((item '("^file:" . pdf-drop--file-dnd-protocol)))
    (if pdf-drop-mode
        (add-to-list 'dnd-protocol-alist item)
      (setq dnd-protocol-alist (remove item dnd-protocol-alist)))))
 

(provide 'pdf-drop-mode)
;;; pdf-drop-mode.el ends here
