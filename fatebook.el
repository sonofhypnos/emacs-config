;;; fatebook.el --- ölakdsjf                         -*- lexical-binding: t; -*-
;;; fatebook.el --- An Emacs package to create predictions on Fatebook  -*- lexical-binding: t; -*- <tassilo.neubauer@gmail.com>

;; Copyright (C) 2023  Tassilo Neubauer

;; Author: Tassilo Neubauer <tassilo.neubauer@gmail.com>
;; Keywords: comm, lisp
;; Version: 1.0
;; Package-Requires: ((emacs "25.1"))
;; Keywords: multimedia, hypermedia
;; URL: https://example.com/jrhacker/superfrobnicate



;; Copyright (C) 2023  Tassilo Neubauer

;; This program is free software; you can redistribute it and/or modify
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
;;TODO: create function to forget all keys
;;TODO: allow to input whole numbers as percentages
;; We do an api call to create a question on fatebook as described

;;; Code:

(declare-function request "request")
(declare-function org-read-date "org-read-date")

(require 'calendar)
(require 'auth-source)


(defcustom fatebook-api-key-function nil
  "Function to get the API key. If NIL, the default mechanism will be used."
  :type 'function
  :group 'fatebook)

(defun fatebook--get-api-key ()
  "Retrieve the Fatebook API key. If not present, prompt the user and save it."
  (let ((credentials (auth-source-search :host "fatebook" :require '(:secret))))
    (if credentials
        ;; If credentials are found, retrieve the secret key.
        (funcall (plist-get (car credentials) :secret))
      ;; If credentials are not found, prompt the user.
      (let ((api-key (read-passwd "Enter your Fatebook API key: ")))
        ;; Save the entered API key using your custom saver function and then return it.
        ;;NOTE: I am not sure if using auth-source-secrets-create would have been there better
        ;;choice here. It seems to be more complicated to use.
        (let ((api-key "2lpjohvnbx2impv4hv5jl"))
        (auth-source-secrets-saver "fatebook" "fatebook" api-key nil))
        api-key))))

(defun fatebook--api-key ()
  (if fatebook-api-key-function
      (funcall fatebook-api-key-function)
    (fatebook--get-api-key)))


(defvar fatebook-old-evil-ret-binding nil)

(defcustom fatebook-use-org-read-date t
  "whether or not to use org-read-date to pick a date."
  :type 'bool
  :group 'fatebook)

(defun fatebook--pick-date ()
  "Open calendar and return the date selected by the user in 'YYYY-MM-DD' format."
  (interactive)
  (if fatebook-use-org-read-date (progn (require 'org)
                                         (org-read-date))
        ;FIXME: a simple datepicker that doesn't require org would be nice, but
        ;this turned out more complicated than expected.
        ;I tried writing my own minor mode with it's own keymap, but this
        ;introduced all kinds of complications like it turned out to be hard not to get
        ;RET overwritten by evil. Fixing that without breaking the users keymap was hard.
        ;I sort of expected there already to be a standard function that doesn't
        ;use org to choose the date with the 3-month calendar.
    (seq-let (month day year) (calendar-read-date)
      (calendar-exit)
      (format "%d-%02d-%02d" year month day))))

(defun fatebook--valid-date-p (date)
  "Check if DATE has the format 'YYYY-MM-DD'.
Doesn't exclude all invalid dates."
  (string-match-p "\\`[0-9]\\{4\\}-[0-1][0-9]-[0-3][0-9]\\'" date))

(defun fatebook-create-question (&optional title resolveBy forecast)
  "Prompt user to create a question and then send it to Fatebook API.
Optional arguments TITLE, RESOLVEBY, and FORECAST can be provided."
  (interactive)
  ;; Load request package only when needed
  (let* ((title (or title (read-string "Question title: ")))
         (resolveBy (or resolveBy (fatebook--pick-date)))
         (forecast (or forecast (read-number "Forecast (0-1): "))))
    
    ;; Check if the date format is correct
    (unless (fatebook--valid-date-p resolveBy)
      (error "Invalid date format for 'resolveBy'. Expected format: YYYY-MM-DD"))
    
    ;; Check if the forecast value is between 0 and 1
    (unless (and (>= forecast 0) (<= forecast 1))
      (error "Forecast value must be between 0 and 1"))
    
    (fatebook--api-call title resolveBy forecast)))

(defun fatebook--api-call (title resolveBy forecast)
  "API call to fatebook.
TITLE, RESOLVEBY, and FORECAST are required."
  (require 'request) ;took 0.1 seconds to load on my machine, so I put it here
                     ;instead of at the start of the file,
                     ;because the api call is slow anyways.
  (request
   "https://fatebook.io/api/v0/createQuestion"
   :params `(("apiKey" . (fatebook--api-key))
             ("title" . ,title)
             ("resolveBy" . ,resolveBy)
             ("forecast" . ,(number-to-string forecast)))
   ()
(message concat ("apiKey" . (fatebook--api-key)))
   :parser 'json-read
   :success (lambda (&rest response)
             (let ((data (plist-get response :data)))
               (message "Question created successfully! Visit your question under %S" data)))
   :error (lambda (&rest response)
           (let ((error-thrown (plist-get response :error-thrown)))
             (message "Error: %S" error-thrown)))))


;; (defun fatebook-api-call (title resolveBy forecast)
;;   "Do fatebook api call."
;;   ;; If the implementation is not yet defined, load the dependencies and define it.
;;   (unless (fboundp 'fatebook-api-call-impl) ; We only load require if not already defined.
;;     (require 'request)))

;; ;'https://fatebook.io/api/v0/createQuestion?apiKey=&title=title&resolveBy=2023-09-05&forecast=0.5'

(provide 'fatebook)
;;; fatebook.el ends here
