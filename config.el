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
(setq org-directory "~/org-roam")
(setq org-roam-directory org-directory)
(setq
 zot_bib (concat (getenv "HOME") "/repos/bibliography/zotLib.bib"))

;;python support
(use-package elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))

(map! :after emr
      :map prog-mode-map
      "M-RET" #'emr-show-refactor-menu)

;;anki support and org templates
(use-package anki-editor

  :init
  (setq-default anki-editor-use-math-jax t) ; github.com/louietan/anki-editor/issues/60#issuecomment-617441799

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
    (anki-editor-cloze-region (cond ((eq my-anki-editor-cloze-number 1)
                                     (progn
                                       (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
                                       1))
                                    (t (1- my-anki-editor-cloze-number))) "")
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

  (setq org-my-anki-file (concat org-roam-directory "anki-stuff.org"))
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
                 "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: .main\n:END:\n** Text\n%?\n** Extra\n%f\n%x"))
  (add-to-list 'org-capture-templates
               '("T" "Anki type"
                 entry
                 (file+headline org-my-anki-file "Dispatch Shelf")
                 "* %<%H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE:1typing\n:ANKI_DECK: .main\n:END:\n** Text\n%?\n** Extra\n%x"))

  (add-to-list 'org-capture-templates
               `("l" "Link" entry (file+headline ,(concat org-roam-directory "/20210510194711-read_and_take_notes.org") "Links")
                 "* [[%:link][%:description]]\n %?\n \n %i\n%T"
                 :immediate-finish t))

  (add-to-list 'org-capture-templates
               '("L" "Protocol Link" entry
                 (file+headline +org-capture-notes-file "Inbox")
                 "* [[%:link][% \"%:description\"]] \n \n %? \n%i \n %T"
                 :prepend t
                 :kill-buffer t))
  (add-to-list 'org-capture-templates
               '("S" "Todo Protocoll" entry
                 (file+headline +org-capture-notes-file "Inbox")
                 "* [[%:link][% \"%:description\"]] \n \n* TODO %? %i \n %T"
                 :prepend t
                 :kill-buffer t))
  (defun make-orgcapture-frame ()
    "Create a new frame and run org-capture."
    (interactive)
    (make-frame '((name . "org-capture") ))
    (select-frame-by-name "org-capture")
    (org-capture)
    (delete-other-windows)
    )
  )

(defun toggle-maximize-buffer () "Maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
           (jump-to-register '_)
         (progn
           (window-configuration-to-register '_)
           (delete-other-windows))))


;;shell support

(use-package vterm
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-shell "zsh")
  (setq vterm-max-scrollback 10000))

(use-package term
  :config
  (setq explicit-shell-file-name "zsh")
  (setq explicit-zsh-args '()) ; I don't know what this is for?
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))




;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; ORG-ROAM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq org-roam-v2-ack t)

(use-package! org-roam
  :after org
  :init
  (map! :leader
        :prefix "a"
        :desc "org-roam-node-insert" "i" #'org-roam-node-insert
        :desc "org-roam-node-find" "f" #'org-roam-node-find
        :desc "org-roam-capture" "c" #'org-roam-capture
        :desc "org-roam-tag-add" "at" #'org-roam-tag-add
        :desc "org-roam-dailies-goto-today" "t" #'org-roam-dailies-goto-today
        :desc "org-roam-dailies-capture-today" "j" #'org-roam-dailies-capture-today
        :desc "org-roam-dailies-goto-date" "d" #'org-roam-dailies-goto-date
        :desc "org-roam-alias-add" "aa" #'org-roam-alias-add
        :desc "org-roam-ref-find" "r" #'org-roam-ref-find
        :desc "org-roam-buffer-toggle" "l" #'org-roam-buffer-toggle
        :desc "org-roam-extract-subtree" "x" #'org-roam-extract-subtree
        )
  :config
  (org-roam-setup)

  (setq daily-template
        (concat
   "#+title: %<%Y-%m-%d>\n* [/] Do Today\n* [/] Maybe Do Today"
   ; The following was replaced by me
   ;"#+title: %<%Y-%m-%d (%A)>\n* [/] Do Today\n* [/] Maybe Do Today"
   "\n* Morgenroutine"
   "\n - [ ] start Focusmate Session"
   "\n - [ ] Kalender angesehen"
   "\n - [ ] start tracking"
   "\n - [ ] check Bedtime yesterday"
   "\n - [ ] Medis genommen"
   "\n - [ ] Duschen"
   "\n - [ ] Uhr angezogen"
   "\n - [ ] Ziele gesetzt"
   "\n - [ ] gegessen"
   "\n - [ ] Anki"
   "\n - [ ] Zähne geputzt"
   "\n* Evening Routine"
   "\n - [ ] start Focusmate Session"
   "\n - [ ] Impuls unterdrückt?"
   "\n - [ ] put reminder for tomorrows Session on your Pillow"
   "\n - [ ] go through notes"
   "\n - [ ] go through to-do inbox"
   "\n - [ ] check Habits/Beeminder"
   "\n - [ ] Tasks Reviewed"
   "\n - [ ] Timetracking Reviewed"
   "\n - [ ] ask Journal Questions"
   "\n - [ ] determine bedtime"
   "\n - [ ] review Anki"
   "\n - [ ] Brush Teeth!"
   "\n* Inbox"
   "\n* Journal"
   "\n* Evening Journal"
   "\n** What did you achieve today?"
   "\n** What are you grateful for?"
   "\n** What worried you today?"
   "\n** What else is on your mind?")
        )
  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
     `(("Journal" "daily" plain "%T\n%?\n"
        :if-new (file+head+olp "%<%Y-%m-%d>.org" ,daily-template ("Journal"))
       )))
;;;;(let ((newhead
;;;;      (org-filename "%<%Y-%m-%d>.org"))
  ;;            `(("j" "journal" entry
  ;;               #'org-roam-capture--get-point
  ;;               "* %<%H:%M> %?"
  ;;               :if-new (file+head "%<%Y-%m-%d>.org"
  ;;                        ,newhead) ;I don't really get scope in elisp yet so this is the save way to
  ;;                                 ;do it for me
  ;;               :olp ("Journal")
  ;;               )
  ;;              ("t" "do today" item
  ;;               #'org-roam-capture--get-point
  ;;               "[ ] %(princ as/agenda-captured-link)"
  ;;               :if-new (file+head "%<%Y-%m-%d>.org"
  ;;                        ,newhead)
  ;;               :olp ("Do Today"))
  ;;              ("m" "maybe do today" item
  ;;               #'org-roam-capture--get-point
  ;;               "[ ] %(princ as/agenda-captured-link)"
  ;;               :if-new (file+head "%<%Y-%m-%d>.org"
  ;;                        ,newhead)
  ;;               :olp ("Maybe Do Today")
  ;;               :immediate-finish t)
  ;;              )
;;;;            )
  ;;)


  (setq org-roam-capture-templates
        '(("d" "default" plain
           "%?"
           :if-new (file+head "${slug}.org"
                              "#+title: ${title}\n#+created: %<%y-%m-%d %H:%M>\n* Next\n* Related\n")
           :immediate-finish t
           :unnarrowed t)))

  ;;here come some nice but non-essential functions for org-roam:
  (defun org-hide-properties ()
    "Hide all org-mode headline property drawers in buffer. Could be slow if it has a lot of overlays."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:\n" nil t)
        (let ((ov_this (make-overlay (match-beginning 0) (match-end 0))))
          (overlay-put ov_this 'display "")
          (overlay-put ov_this 'hidden-prop-drawer t))))
    (put 'org-toggle-properties-hide-state 'state 'hidden))

  (defun org-show-properties ()
    "Show all org-mode property drawers hidden by org-hide-properties."
    (interactive)
    (remove-overlays (point-min) (point-max) 'hidden-prop-drawer t)
    (put 'org-toggle-properties-hide-state 'state 'shown))

  (defun org-toggle-properties ()
    "Toggle visibility of property drawers."
    (interactive)
    (if (eq (get 'org-toggle-properties-hide-state 'state) 'hidden)
        (org-show-properties)
      (org-hide-properties)))



  (setq org-roam-capture-ref-templates
        '(("r" "ref" plain
           "%?\n* Quote\n\" %x\"  "
           :if-new (file+head "${slug}.org"
                              "#+title: ${title}\n#+author:\n")
           :unnarrowed t)))

  ;;  (setq org-roam-capture-ref-templates
  ;;        '(("e" "ref" plain (function org-roam--capture-get-point)
  ;;           "%?\n* related"
  ;;           :file-name "lit/${slug}"
  ;;           :head "#+setupfile:./hugo_setup.org
  ;;#+roam_key: ${ref}
  ;;#+hugo_slug: ${slug}
  ;;#+roam_tags: website
  ;;#+title: ${title}
  ;;
  ;;- source :: ${ref}
  ;;* thoughts
  ;;** "
  ;;           :unnarrowed t
  ;;           )))

  ;;(setq org-roam-link-title-format "%s")
  (require 'org-roam-protocol)


  ;; Org-roam-server currently does not work
;;;;org-roam server creates an interactive graph from the org-roam files in the browser.
  ;;(use-package org-roam-server
  ;;  :config
  ;;  (setq org-roam-server-host "127.0.0.1"
  ;;        org-roam-server-port 8080
  ;;        org-roam-server-authenticate nil
  ;;        org-roam-server-export-inline-images t
  ;;        org-roam-server-serve-files nil
  ;;        org-roam-server-served-file-extensions '("pdf" "mp4" "ogv")
  ;;        org-roam-server-network-poll t
  ;;        org-roam-server-network-arrows nil
  ;;        org-roam-server-network-label-truncate t
  ;;        org-roam-server-network-label-truncate-length 60
  ;;        org-roam-server-network-label-wrap-length 20))
  ;;
  ;;
  ;;(use-package deft
  ;;  :after org
  ;;  :bind
  ;;  ("C-c n d" . deft)
  ;;  :custom
  ;;  (deft-recursive t)
  ;;  (deft-use-filter-string-for-filename t)
  ;;  (deft-default-extension "org")
  ;;  (deft-directory org-directory))
  ;;
  ;;
  ;;Bibliography configuration
  ;;
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
  )

(use-package org-ref
  :config
  :ensure t
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite
        org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex)
  (setq
   org-ref-default-bibliography (list zot_bib)
   org-ref-bibliography-notes  (concat org-roam-directory "bibliography.org")
   org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
   org-ref-notes-directory (concat org-roam-directory "/lit")
   org-ref-notes-function 'orb-edit-notes)
  )




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


;;org-roam-ui support
(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam ;; or :after org
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; more finegrainded undo
(setq evil-want-fine-undo t)
;;safe delete
(setq-default delete-by-moving-to-trash t)

;; add macro for Vim surround for more characters
;;; this macro was copied from here: https://stackoverflow.com/a/22418983/4921402
(defmacro define-and-bind-quoted-text-object (name key start-regex end-regex)
  (let ((inner-name (make-symbol (concat "evil-inner-" name)))
        (outer-name (make-symbol (concat "evil-a-" name))))
    `(progn
       (evil-define-text-object ,inner-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count nil))
       (evil-define-text-object ,outer-name (count &optional beg end type)
         (evil-select-paren ,start-regex ,end-regex beg end type count t))
       (define-key evil-inner-text-objects-map ,key #',inner-name)
       (define-key evil-outer-text-objects-map ,key #',outer-name))))

(define-and-bind-quoted-text-object "pipe" "|" "|" "|")
(define-and-bind-quoted-text-object "slash" "/" "/" "/")
(define-and-bind-quoted-text-object "asterisk" "*" "*" "*")
(define-and-bind-quoted-text-object "dollar" "$" "\\$" "\\$") ;; sometimes your have to escape the regex




(after! pdf-tools
  (add-hook! 'pdf-tools-enabled-hook
    (pdf-view-midnight-minor-mode 1)))



(use-package org-noter
  :config
  (setq org-noter-notes-search-path '("~/org-roam/")))

(use-package org-pdftools
  :hook (org-mode . org-pdftools-setup-link))

(use-package! org-noter-pdftools
  :after org-noter
  :config
  (pdf-tools-install)
  ;; Add a function to ensure precise note is inserted
  (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
    (interactive "P")
    (org-noter--with-valid-session
     (let ((org-noter-insert-note-no-questions (if toggle-no-questions
                                                   (not org-noter-insert-note-no-questions)
                                                 org-noter-insert-note-no-questions))
           (org-pdftools-use-isearch-link t)
           (org-pdftools-use-freestyle-annot t))
       (org-noter-insert-note (org-noter--get-precise-info)))))

  ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
  (defun org-noter-set-start-location (&optional arg)
    "When opening a session with this document, go to the current location.
With a prefix ARG, remove start location."
    (interactive "P")
    (org-noter--with-valid-session
     (let ((inhibit-read-only t)
           (ast (org-noter--parse-root))
           (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
       (with-current-buffer (org-noter--session-notes-buffer session)
         (org-with-wide-buffer
          (goto-char (org-element-property :begin ast))
          (if arg
              (org-entry-delete nil org-noter-property-note-location)
            (org-entry-put nil org-noter-property-note-location
                           (org-noter--pretty-print-location location))))))))
  (with-eval-after-load 'pdf-annot
    (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))


(use-package org-download
  :init
  (map! :leader
        :prefix "d"
        :desc "org-screenshot" "d" #'org-download-screenshot)
  )
;; org-download
;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)
(setq org-image-actual-width nil)

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

;; dark mode for pdfs
(after! pdf-tools
  (add-hook! 'pdf-tools-enabled-hook
    (pdf-view-midnight-minor-mode 1)))


;;(use-package wakatime-mode
;;  :ensure t)

;;(add-hook 'after-init-hook 'org-roam)
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
