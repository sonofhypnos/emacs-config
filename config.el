;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!


;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets.
(setq user-full-name "Tassilo Neubauer"
      user-mail-address "tassilo.neubauer@gmail.com")

;; Doom exposes five (optional) variables for controlling fonts in Doom. Here
;; are the three important ones:
;;
;; + `doom-font'
;; + `doom-variable-pitch-font'
;; + `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;;
;; They all accept either a font-spec, font string ("Input Mono-12"), or xlfd
;; font string. You generally only need these two:
;; (setq doom-font (font-spec :family "monospace" :size 12 :weight 'semi-light)
;;       doom-variable-pitch-font (font-spec :family "sans" :size 13))

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:
;;(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org-roam/")



;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.

;;(defun get-newest-file-from-dir  (path)
;;      "Get latest file (including directory) in PATH."
;;      (car (directory-files path 'full nil #'file-newer-than-file-p)))
;;
;;    (defun insert-org-image ()
;;      "Moves image from Dropbox folder to ./media, inserting org-mode link"
;;      (interactive)
;;      (let* ((indir (expand-file-name andre--screenshot-folder))
;;             (infile (get-newest-file-from-dir indir))
;;             (outdir (concat (file-name-directory (buffer-file-name)) "/media"))
;;             (outfile (expand-file-name (file-name-nondirectory infile) outdir)))
;;        (unless (file-directory-p outdir)
;;          (make-directory outdir t))
;;        (rename-file infile outfile)
;;        (insert (concat (concat "[[./media/" (file-name-nondirectory outfile)) "]]")))
;;      (newline)
;;      (newline))

;;(defun org-insert-clipboard-image (&optional file)
;;  (interactive "F")
;;  (shell-command (concat "pngpaste " file))
;;  (insert (concat "[[" file "]]"))
;;  (org-display-inline-images))

(use-package! anki-editor
 :bind (:map org-mode-map
             ("<f12>" . anki-editor-cloze-region-auto-incr)
             ("<f11>" . anki-editor-cloze-region-dont-incr)
             ("<f10>" . anki-editor-reset-cloze-number)
             ("<f9>"  . anki-editor-push-tree))
 :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.
 :config
 (setq anki-editor-create-decks t ;; Allow anki-editor to create a new deck if it doesn't exist
       anki-editor-org-tags-as-anki-tags t)

 (defun anki-editor-cloze-region-auto-incr (&optional arg)
   "Cloze region without hint and increase card number."
   (interactive)
   (anki-editor-cloze-region my-anki-editor-cloze-number "")
   (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
   (forward-sexp))
 (defun anki-editor-cloze-region-dont-incr (&optional arg)
   "Cloze region without hint using the previous card number."
   (interactive)
   (anki-editor-cloze-region (1- my-anki-editor-cloze-number) "")
   (forward-sexp))
 (defun anki-editor-reset-cloze-number (&optional arg)
   "Reset cloze number to ARG or 1"
   (interactive)
   (setq my-anki-editor-cloze-number (or arg 1)))
 (defun anki-editor-push-tree ()
   "Push all notes under a tree."
   (interactive)
   (anki-editor-push-notes '(4))
   (anki-editor-reset-cloze-number))
 ;; Initialize
 (anki-editor-reset-cloze-number)

(setq org-my-anki-file "~/org-roam/anki-stuff.org")
:demand
:config
(add-to-list 'org-capture-templates
             '("a" "Anki basic"
               entry
               (file+headline org-my-anki-file "Dispatch Shelf")
               "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: .main\n:END:\n** Front\n%?\n** Back\n%x\n"))
(add-to-list 'org-capture-templates
             '("A" "Anki cloze"
               entry
               (file+headline org-my-anki-file "Dispatch Shelf")
               "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: .main\n:END:\n** Text\n%x\n** Extra\n"))
(add-to-list 'org-capture-templates
             '("A" "Anki type"
               entry
               (file+headline org-my-anki-file "Dispatch Shelf")
               "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: \n:ANKI_DECK: .main\n:END:\n** Text\n%x\n** Extra\n"))

 (defun make-orgcapture-frame ()
     "Create a new frame and run org-capture."
     (interactive)
     (make-frame '((name . "org-capture") (window-system . x) ))
     (select-frame-by-name "org-capture")
     (org-capture)
     (delete-other-windows)
     )
 )

;;(use-package org-protocol
;;  :demand
;;  :config
;;  (add-to-list 'org-capture-templates
;;               '("p" "Protocol" entry (file "")
;;                 "* TODO %?[[%:link][%:description]] %U\n%i\n" :prepend t))
;;  (add-to-list 'org-capture-templates
;;               '("L" "Protocol Link" entry (file "")
;;                 "* TODO %?[[%:link][%:description]] %U\n" :prepend t)))
;;
;;
;;
;; Org-capture templates

;; Allow Emacs to access content from clipboard.
;;(defvar select-enable-clipboard t
;;      select-enable-primary t)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ORG-ROAM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;journal template copied from here: https://org-roam.discourse.group/t/dailies-capture-templates-best-practices/1043
    (setq org-roam-dailies-capture-templates
          (let ((head
                 (concat
    "#+title: %<%Y-%m-%d (%A)>\n* [/] Do Today\n* [/] Maybe Do Today"
    "\n* Morgenroutine"
    "\n - [ ] start Focusmate Session"
    "\n - [ ] start tracking"
    "\n - [ ] Medis genommen"
    "\n - [ ] Duschen"
    "\n - [ ] Uhr angezogen"
    "\n - [ ] Ziele gesetzt"
    "\n - [ ] gegessen"
    "\n - [ ] Anki"
    "\n - [ ] Zähne geputzt"
    "\n* Evening Routine"
    "\n - [ ] start Focusmate Session"
    "\n - [ ] [[file:../20210528211654-daily_tracking_tabelle.org][Wie oft]] [[file:../20210528194323-tap_impulsivity.org][Impuls unterdrückt]]?"
    "\n - [ ] go through Emails"
    "\n - [ ] go through notes"
    "\n - [ ] go through to-do inbox"
    "\n - [ ] check Habits/Beeminder"
    "\n - [ ] Tasks Reviewed"
    "\n - [ ] Timetracking Reviewed"
    "\n - [ ] ask Journal Questions"
    "\n - [ ] do Anki"
    "\n - [ ] Zähne geputzt!"
    "\n* Inbox"
    "\n* Journal"
    "\n* Evening Journal"
    "\n** What did you achieve today?"
    "\n** What are you grateful for?"
    "\n** What worried you today?"
    "\n** What else is on your mind?")))
            `(("j" "journal" entry
               #'org-roam-capture--get-point
               "* %<%H:%M> %?"
               :file-name "daily/%<%Y-%m-%d>"
               :head ,head
               :olp ("Journal"))
              ("t" "do today" item
               #'org-roam-capture--get-point
               "[ ] %(princ as/agenda-captured-link)"
               :file-name "daily/%<%Y-%m-%d>"
               :head ,head
               :olp ("Do Today"))
              ("m" "maybe do today" item
               #'org-roam-capture--get-point
               "[ ] %(princ as/agenda-captured-link)"
               :file-name "daily/%<%Y-%m-%d>"
               :head ,head
               :olp ("Maybe Do Today")
               :immediate-finish t))))


;;add pdf-viewer. I have no idea whether something similar would be added through the pdf-package in the init-file
;;(use-package pdf-view
;;  :hook (pdf-tools-enabled . pdf-view-midnight-minor-mode)
;;  :hook (pdf-tools-enabled . hide-mode-line-mode)
;;  :config
;;  (setq pdf-view-midnight-colors '("#ABB2BF" . "#282C35")))





(server-start)
(add-to-list 'load-path "~/path/to/org/protocol/")

;; Variable for later use
(setq
   org_notes (concat (getenv "HOME") "/org-roam")
   zot_bib (concat (getenv "HOME") "/repos/bibliography/zotLib.bib")
   org-directory org_notes
   deft-directory org_notes
   org-roam-directory org_notes
   )




(after! org-roam
  :init
  (map! :leader
        :prefix "a"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "u" #'org-roam-switch-to-buffer
        :desc "org-roam-node-find" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-server-mode
        :desc "org-roam-capture" "c" #'org-roam-capture
        :desc "org-roam-dailies-capture-today" "t" #'org-roam-dailies-capture-today
        :desc "org-roam-tag-add" "at" #'org-roam-tag-add
        :desc "org-roam-dailies-find-today" "d" #'org-roam-dailies-find-today
        :desc "org-roam-alias-add" "aa" #'org-roam-alias-add
        :desc "org-roam-tag-delete" "ö" #'org-roam-tag-delete
        :desc "org-roam-buffer-activate" "r" #'org-roam-buffer-activate
        :desc "org-roam-backlinks-mode" "bl" #'org-roam-backlinks-mode)
;;  (add-hook 'find-file-hook
;;    (defun +org-roam-open-buffer-maybe-h ()
;;      (and +org-roam-open-buffer-on-find-file
;;           (memq 'org-roam-buffer--update-maybe post-command-hook)
;;           (not (window-parameter nil 'window-side)) ; don't proc for popups
;;           (not (eq 'visible (org-roam-buffer--visibility)))
;;           (with-current-buffer (window-buffer)
;;             (org-roam-buffer--get-create)))))
        )

;;(use-package! org-roam
;;  :init
;;  (map! :leader
;;        :prefix "n"
;;        :desc "org-roam" "l" #'org-roam-buffer-toggle
;;        :desc "org-roam-node-insert" "i" #'org-roam-node-insert
;;        :desc "org-roam-ref-find" "r" #'org-roam-ref-find
;;        :desc "org-roam-show-graph" "g" #'org-roam-show-graph
;;        :desc "org-roam-capture" "c" #'org-roam-capture
;;        :desc "org-roam-dailies-capture-today" "j" #'org-roam-dailies-capture-today)
;;  (setq org-roam-directory (file-truename "~/.org/braindump/org/")
;;        org-roam-db-gc-threshold most-positive-fixnum
;;        org-id-link-to-org-use-id t)
;;  (add-to-list 'display-buffer-alist
;;               '(("\\*org-roam\\*"
;;                  (display-buffer-in-direction)
;;                  (direction . right)
;;                  (window-width . 0.33)
;;                  (window-height . fit-window-to-buffer))))
;;  :config
;;  (setq org-roam-mode-sections
;;        (list #'org-roam-backlinks-insert-section
;;              #'org-roam-reflinks-insert-section
;;              ;; #'org-roam-unlinked-references-insert-section
;;              ))
;;  (org-roam-setup)
;;  (setq org-roam-capture-templates
;;        '(("d" "default" plain
;;           "%?"
;;           :if-new (file+head "${slug}.org"
;;                              "#+title: ${title}\n")
;;           :immediate-finish t
;;           :unnarrowed t)))
;;  (setq org-roam-capture-ref-templates
;;        '(("r" "ref" plain
;;           "%?"
;;           :if-new (file+head "${slug}.org"
;;                              "#+title: ${title}\n")
;;           :unnarrowed t)))
;;
;;  (add-to-list 'org-capture-templates `("c" "org-protocol-capture" entry (file+olp ,(expand-file-name "reading_and_writing_inbox.org" org-roam-directory) "The List")
;;                                         "* TO-READ [[%:link][%:description]] %^g"
;;                                         :immediate-finish t))
;;  (add-to-list 'org-agenda-custom-commands `("r" "Reading"
;;                                             ((todo "WRITING"
;;                                                    ((org-agenda-overriding-header "Writing")
;;                                                     (org-agenda-files '(,(expand-file-name "reading_and_writing_inbox.org" org-roam-directory)))))
;;                                              (todo "READING"
;;                                                    ((org-agenda-overriding-header "Reading")
;;                                                     (org-agenda-files '(,(expand-file-name "reading_and_writing_inbox.org" org-roam-directory)))))
;;                                              (todo "TO-READ"
;;                                                    ((org-agenda-overriding-header "To Read")
;;                                                     (org-agenda-files '(,(expand-file-name "reading_and_writing_inbox.org" org-roam-directory))))))))
;;  (setq org-roam-dailies-directory "daily/")
;;  (setq org-roam-dailies-capture-templates
;;        '(("d" "default" entry
;;           "* %?"
;;           :if-new (file+head "daily/%<%Y-%m-%d>.org"
;;                              "#+title: %<%Y-%m-%d>\n"))))
;;  ;; (set-company-backend! 'org-mode '(company-capf))
;;  )

;;org-roam server creates an interactive graph from the org-roam files in the browser.
(use-package! org-roam-server
  :config
  (setq org-roam-server-host "127.0.0.1"
        org-roam-server-port 8080
        org-roam-server-authenticate nil
        org-roam-server-export-inline-images t
        org-roam-server-serve-files nil
        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
        org-roam-server-network-poll t
        org-roam-server-network-arrows nil
        org-roam-server-network-label-truncate t
        org-roam-server-network-label-truncate-length 60
        org-roam-server-network-label-wrap-length 20))




;;Bibliography configuration
(setq
 bibtex-completion-notes-path org-directory
 bibtex-completion-bibliography zot_bib
 bibtex-completion-pdf-field "file"
 bibtex-completion-notes-template-multiple-files
 (concat
  "#+TITLE: ${title}\n"
  "#+ROAM_KEY: cite:${=key=}\n"
  "* TODO Notes\n"
  ":PROPERTIES:\n"
  ":Custom_ID: ${=key=}\n"
  ":NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n"
  ":AUTHOR: ${author-abbrev}\n"
  ":JOURNAL: ${journaltitle}\n"
  ":DATE: ${date}\n"
  ":YEAR: ${year}\n"
  ":DOI: ${doi}\n"
  ":URL: ${url}\n"
  ":END:\n\n"
  )
 )

(use-package! org-ref
    :config
    (setq
         org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex
         org-ref-default-bibliography zot_bib
         org-ref-bibliography-notes  (concat org-roam-directory "bibliography.org")
         org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
         org-ref-notes-directory org-roam-directory
         org-ref-notes-function 'orb-edit-notes
    ))

(setq display-line-numbers-type t)

 (use-package! org-roam-bibtex
  :after (org-roam)
  :hook (org-roam-mode . org-roam-bibtex-mode)
  :config
  (setq org-roam-bibtex-preformat-keywords
   '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
  (setq orb-templates
        '(("r" "ref" plain (function org-roam-capture--get-point)
           ""
           :file-name "${slug}"
           :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}\n#+ROAM_TAGS:

- keywords :: ${keywords}

\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"

           :unnarrowed t))))

(use-package! org-noter
    :after org
    :config (setq org-noter-default-notes-file-names '("org-noter.org")
                  org-noter-notes-search-path '("~/org-roam/")
                  org-noter-separate-notes-from-heading t))
;;(use-package! org-noter
;;  :after (:any org pdf-view)
;;  :config
;;  (setq
;;   ;; The WM can handle splits
;;   org-noter-notes-window-location 'other-frame
;;   ;; Please stop opening frames
;;   org-noter-always-create-frame nil
;;   ;; I want to see the whole file
;;   org-noter-hide-other nil
;;   ;; Everything is relative to the main notes file
;;   org-noter-notes-search-path (list org_notes)
;;   )
;;  )
;;;; Actually start using templates
;;(after! org-capture
;;  ;; Firefox and Chrome
;;  (add-to-list 'org-capture-templates
;;               '("P" "Protocol" entry ; key, name, type
;;                 (file+headline +org-capture-notes-file "Inbox") ; target
;;                 "* %^{Title}\nSource: %u, %c\n #+BEGIN_QUOTE\n%i\n#+END_QUOTE\n\n\n%?"
;;                 :prepend t ; properties
;;                 :kill-buffer t))
;;  (add-to-list 'org-capture-templates
;;               '("L" "Protocol Link" entry
;;                 (file+headline +org-capture-notes-file "Inbox")
;;                 "* %? [[%:link][% \"%:description\"]]\n"
;;                 :prepend t
;;                 :kill-buffer t))
;;)


;;from emacs wiki to use emacs on startup as Gnome Application.
;;; save & shutdown when we get an "end of session" signal on dbus
(require 'dbus)

(defun my-register-signals (client-path)
  "Register for the 'QueryEndSession' and 'EndSession' signals from
Gnome SessionManager.

When we receive 'QueryEndSession', we just respond with
'EndSessionResponse(true, \"\")'.  When we receive 'EndSession', we
append this EndSessionResponse to kill-emacs-hook, and then call
kill-emacs.  This way, we can shut down the Emacs daemon cleanly
before we send our 'ok' to the SessionManager."
  (setq my-gnome-client-path client-path)
  (let ( (end-session-response (lambda (&optional arg)
                                 (dbus-call-method-asynchronously
                                  :session "org.gnome.SessionManager" my-gnome-client-path
                                  "org.gnome.SessionManager.ClientPrivate" "EndSessionResponse" nil
                                  t "") ) ) )
         (dbus-register-signal
          :session "org.gnome.SessionManager" my-gnome-client-path
          "org.gnome.SessionManager.ClientPrivate" "QueryEndSession"
          end-session-response )
         (dbus-register-signal
          :session "org.gnome.SessionManager" my-gnome-client-path
          "org.gnome.SessionManager.ClientPrivate" "EndSession"
          `(lambda (arg)
             (add-hook 'kill-emacs-hook ,end-session-response t)
             (kill-emacs) ) ) ) )

;; DESKTOP_AUTOSTART_ID is set by the Gnome desktop manager when emacs
;; is autostarted.  We can use it to register as a client with gnome
;; SessionManager.
(dbus-call-method-asynchronously
 :session "org.gnome.SessionManager"
 "/org/gnome/SessionManager"
 "org.gnome.SessionManager" "RegisterClient" 'my-register-signals
 "Emacs server" (getenv "DESKTOP_AUTOSTART_ID"))

;;Custom Shortcuts
;;(map! :i "ö" #'evil-normal-state)

(map! :leader :desc "execute emacs command" "SPC" #'execute-extended-command)
(map! :leader :desc "projectile find file" ":" #'projectile-find-file)


;;(use-package wakatime-mode
;;  :ensure t)

(add-hook 'after-init-hook 'org-roam-mode)
(add-hook 'after-init-hook 'org-zotxt-mode)

;;(global-wakatime-mode)

;;


;;(require 'company-org-roam)
;;        (use-package company-org-roam
;;                :when (featurep! :completion company)
;;                :after org-roam
;;                :config
;;        (set-company-backend! 'org-mode '(company-org-roam company-yasnippet company-dabbrev))


;; Here are some additional functions/macros that could help you configure Doom:
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.
