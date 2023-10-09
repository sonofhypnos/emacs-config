;;; config.el -*- lexical-binding: t; -*-
;;;
;; TODO Sprinkle in documentation from the org-file
(setq user-full-name "Tassilo Neubauer"
      user-mail-address "tassilo.neubauer@gmail.com")

(setq   org-directory "~/org-roam/"
        org-roam-directory "~/org-roam/"
        projectile-project-search-path '("~/repos" "~/Dropbox/")
        org-fc-diretories '(org-directory)
        org-archive-location (concat org-directory ".archive/%s::")
        t/org-inbox-file (concat org-directory "notes.org")
        t/org-project-file (concat org-directory "projects.org")
        t/org-someday-maybe-file (concat org-directory "someday_maybe.org")
        t/org-archive-file (concat org-directory "archive.org")
        t/journal-file (concat org-directory "journal.org")
        t/writing-ideas (concat org-directory "20210508185546-things_to_write_about.org")
        t/fzi (concat org-directory "fzi_assistant_job.org"))


(after! dired-x
  (setq dired-guess-shell-alist-user
        (list
         (list "\\.pdf\\'" "zathura"))))

                                        ;configure haskell to support renaming stuff
(after! lsp-haskell
  (add-hook 'lsp-after-initialize-hook
            #'(lambda ()
                (lsp--set-configuration
                 '(:haskell (:plugin (:rename (:config (:crossModule t)))))))))

;;go where refile takes you:
(defun +org-search ()
  (interactive)
  (org-refile '(4)))

(custom-set-variables '(org-agenda-files '("~/org-roam/projects.org" "~/org-roam/notes.org"))
                      '(org-refile-targets ((t/org-inbox-file t/org-project-file t/org-someday-maybe-file t/org-archive-file t/journal-file) :maxlevel . 3))) ;not sure about benefits of custom-set-variables


;; default in doom is to low. Not sure where all the memory is going
;; NOTE: gcmh is a package that does some neat things to avoid garbage collection.
;; NOTE: This is not the regular garbage collection threshold!
;; (setq gcmh-high-cons-threshold (*  100 1024 1024)) ;; Increasing the value for gc-collection is actually not recommended longterm. Doing it while idle is not actually working for me, because it hangs way too long!

;; (setq gcmh-low-cons-threshold (* 100 1024 1024)) ;; value if not in emacs. This might solve our issue.
(setq gcmh-verbose t) ;Keeping this on until we have figured out issues.
;; NOTE: you also made a note under garbage collection
;; NOTE: more on gcmh: https://akrl.sdf.org/
;;TODO: figure out if gcmh is causing issues
(defun print-gc-elapsed ()
  (setq gc-message
        (concat (format-time-string "[%F %T.%3N %Z] ")
                (format "GC-elapsed: %f\n" gc-elapsed)))
  (message "%s" gc-message)

  (write-region (format "%s" gc-message) nil "~/gc-logs.txt" 'append))
(add-hook 'post-gc-hook 'print-gc-elapsed)

(after! forge
  (require 'forge)
  (transient-append-suffix 'forge-dispatch '(0)
    ["Edit"
     ("e a" "assignees" forge-edit-topic-assignees)
     ("e r" "review requests" forge-edit-topic-review-requests)]))

;;add curry for functions further down
(defsubst curry (function &rest arguments)
  (lexical-let ((function function)
                (arguments arguments))
               (lambda (&rest more) (apply function (append arguments more)))))

(defsubst rcurry (function &rest arguments)
  (lexical-let ((function function)
                (arguments arguments))
               (lambda (&rest more) (apply function (append more arguments)))))

(defsubst compose (function &rest more-functions)
  (cl-reduce (lambda (f g)
               (lexical-let ((f f) (g g))
                            (lambda (&rest arguments)
                              (funcall f (apply g arguments)))))
             more-functions
             :initial-value function))


;; FIXME check-init-file does not work at the moment.
(defun check-init-file ()
  "Checks for stupid config mistakes"
  (while (re-search-forward "\\(use-package\\(.*\n?\\)\\)*)")
    (if (not (cl-some (rcurry #'string-match-p (match-string 0)) '(":after" ":defer"))))
    (warn "Do not use use-package without ")))

;; Check if langtool is detected if necessary
(use-package! langtool
  :defer-incrementally t
  :config
  (setq langtool-language-tool-jar "~/repos/languagetool/LanguageTool-5.6-stable/languagetool.jar")
  (setq langtool-language-tool-server-jar "~/repos/languagetool/LanguageTool-5.6-stable/languagetool-server.jar")
  (setq langtool-server-user-arguments '("-p" "8081")))

(after! ispell
  ;; Configure `LANG`, otherwise ispell.el cannot find a 'default
  ;; dictionary' even though multiple dictionaries will be configured
  ;; in next line.
  (setenv "LANG" "en_US.UTF-8")
  (setq ispell-program-name "hunspell")
  ;; Configure German, Swiss German, and two variants of English.
  (setq ispell-dictionary "de_DE,en_GB,en_US")
  ;; ispell-set-spellchecker-params has to be called
  ;; before ispell-hunspell-add-multi-dic will work
  (ispell-set-spellchecker-params)
  (ispell-hunspell-add-multi-dic "de_DE,en_GB,en_US")
  ;; For saving words to the personal dictionary, don't infer it from
  ;; the locale, otherwise it would save to ~/.hunspell_de_DE.
  ;; The personal dictionary file has to exist, otherwise hunspell will
  ;; silently not use it.
  (let ((ispell-local "~/.hunspell_personal"))
    (setq ispell-personal-dictionary "~/.hunspell_personal")
    (unless (file-exists-p ispell-local)
      (with-temp-buffer (write-file ispell-local))))


  ;; NOTE: Added because of "Starting 'Look' process..." message in the message buffer which is annoying
  ;; see: https://github.com/company-mode/company-mode/issues/912
  (advice-add 'ispell-lookup-words :around
              (lambda (orig &rest args)
                (shut-up (apply orig args)))))


(after! anki-editor
  ;; :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.

  :config
  (setq-default anki-editor-use-math-jax t)
  (setq anki-editor-org-tags-as-anki-tags t)

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
  (map! :map org-mode-map
        ;;Key-maps have to be set after function definitions!
        "<f12>"  #'anki-editor-cloze-region-dont-incr
        "<f11>"  #'anki-editor-cloze-region-auto-incr
        "<f10>"  #'anki-editor-reset-cloze-number
        "<f9>"   #'anki-editor-push-tree)
  (add-hook org-capture-after-finalize-hook #'anki-editor-reset-cloze-number)

  ;; Initialize
  (anki-editor-reset-cloze-number))

(after! org
  ;;trying to speed up org by disabeling this:
  (setq org-agenda-ignore-properties '(effort appt category))
  (setq org-refile-targets
        '((nil :maxlevel . 3)
          (t/org-inbox-file :maxlevel . 3)
          (t/org-project-file :maxlevel . 5)
          (t/org-someday-maybe-file :maxlevel . 5)
          (t/org-archive-file :maxlevel . 3)
          (t/journal-file :maxlevel . 1)
          (t/writing-ideas :maxlevel . 1)
          (t/fzi :maxlevel . 1)))

  (defun tassilo/enable-anki-editor-mode ()
    (when (org-capture-get :anki)
      (anki-editor-mode)))

  (add-hook 'org-capture-mode-hook 'tassilo/enable-anki-editor-mode)


  (setq org-my-anki-file (concat org-roam-directory "anki-stuff.org")
        org-capture-templates `(
                                ("l" "Link" entry (file+headline +org-capture-notes-file "Links")
                                 "* [[%:link][%:description]]\n %?\n \n %i\n%T"
                                 :immediate-finish t)
                                ("a" "Anki basic"
                                 entry
                                 (file+headline org-my-anki-file "Dispatch Shelf")
                                 "* %<%y-%m-%d %H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: .main\n:END:\n** Front\n%?\n** Back\n%x\n")

                                ("A" "Anki cloze"
                                 entry
                                 (file+headline org-my-anki-file "Dispatch Shelf")
                                 "* %<%y-%m-%d %H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: .main\n:END:\n** Text\n%?\n** Extra\n%f\n%x"
                                 :anki t)
                                ("T" "Anki type"
                                 entry
                                 (file+headline org-my-anki-file "Dispatch Shelf")
                                 "* %<%y-%m-%d %H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE:1typing\n:ANKI_DECK: .main\n:END:\n** Text\n%?\n** Extra\n%x"
                                 :anki t)

                                ("L" "Protocol Link" entry
                                 (file+headline +org-capture-notes-file "Inbox")
                                 "* [[%:link][%:description]] \n \n \n%i \n %T"
                                 :prepend t)
                                ("S" "Todo Protocoll" entry
                                 (file+headline +org-capture-notes-file "Inbox")
                                 "* [[%:link][% \"%:description\"]] \n \n* TODO %? %i \n %T"
                                 :prepend t)
                                ("t" "Personal todo" entry
                                 (file+headline +org-capture-notes-file "Todos")
                                 "* [ ] %?\n%i\n" :prepend t)
                                ("n" "Personal notes" entry
                                 (file+headline +org-capture-notes-file "Inbox")
                                 "* %u %?\n%i\n" :prepend t)
                                ("j" "Journal" entry
                                 (file+olp+datetree +org-capture-journal-file)
                                 "* %U %?\n%i\n" :prepend t)))

  ;; create default apps
  (add-to-list 'org-file-apps '("\\.pdf\\'" . "zathura %s"))
  (add-to-list 'org-file-apps '("\\.md\\'" . "firefox %s"))
  (add-to-list 'org-file-apps '("\\.html\\'" . "firefox %s"))



  ;; org refile hydras
  (defun my/refile (file headline &optional arg)
    "Refile to a specific location.
With a 'C-u' ARG argument, we jump to that location (see
`org-refile').
Use `org-agenda-refile' in `org-agenda' mode."
    (let* ((pos (with-current-buffer (or (get-buffer file) ;Is the file open in a buffer already?
                                         (find-file-noselect file)) ;Otherwise, try to find the file by name (Note, default-directory matters here if it isn't absolute)
                  (or (org-find-exact-headline-in-buffer headline)
                      (error "Can't find headline `%s'" headline))))
           (filepath (buffer-file-name (marker-buffer pos))) ;If we're given a relative name, find absolute path
           (rfloc (list headline filepath nil pos)))
      (if (and (eq major-mode 'org-agenda-mode) (not (and arg (listp arg)))) ;Don't use org-agenda-refile if we're just jumping
          (org-agenda-refile nil rfloc)
        (org-refile arg nil rfloc))))

  (defun t/refile (file headline &optional arg)
    "Refile to HEADLINE in FILE. Clean up org-capture if it's activated.
With a `C-u` ARG, just jump to the headline."
    (interactive "P")
    (let ((is-capturing (and (boundp 'org-capture-mode) org-capture-mode)))
      (cond
       ((and arg (listp arg))           ;Are we jumping?
        (my/refile file headline arg))
       ;; Are we in org-capture-mode?
       (is-capturing          ;Minor mode variable that's defined when capturing
        (t/org-capture-refile-but-with-args file headline arg))
       (t
        (my/refile file headline arg)))
      (when (or arg is-capturing)
        (setq hydra-deactivate t))))

  (defun t/org-capture-refile-but-with-args (file headline &optional arg)
    "Copied from `org-capture-refile' since it doesn't allow passing arguments. This does."
    (unless (eq (org-capture-get :type 'local) 'entry)
      (error
       "Refiling from a capture buffer makes only sense for `entry'-type templates"))
    (let ((pos (point))
          (base (buffer-base-buffer (current-buffer)))
          (org-capture-is-refiling t)
          (kill-buffer (org-capture-get :kill-buffer 'local)))
      (org-capture-put :kill-buffer nil)
      (org-capture-finalize)
      (save-window-excursion
        (with-current-buffer (or base (current-buffer))
          (org-with-wide-buffer
           (goto-char pos)
           (my/refile file headline arg))))
      (when kill-buffer (kill-buffer base))))

  (defmacro t/make-org-refile-hydra (hydraname file keyandheadline)
    "Make a hydra named HYDRANAME with refile targets to FILE.
KEYANDHEADLINE should be a list of cons cells of the form (\"key\" . \"headline\")"
    `(defhydra ,hydraname (:color blue :after-exit (unless (or hydra-deactivate
                                                               current-prefix-arg) ;If we're just jumping to a location, quit the hydra
                                                     (t/org-refile-hydra/body)))
       ,file
       ,@(cl-loop for kv in keyandheadline
                  collect (list (car kv) (list 't/refile file (cdr kv) 'current-prefix-arg) (cdr kv)))
       ("q" nil "cancel")))

  (defhydra t/inbox-hydra (:foreign-keys run)
    "Refile"
    ("A" (my/refile t/org-archive-file "Archive" ) "Archive")
    ;;NOTE: consider not switching archive and real archive again! (the real
    ;;archive should be the default. If you really need to find something, you
    ;;will use project search anyways.)
    ("a" (org-archive-subtree) "really archive")
    ("w" (my/refile t/writing-ideas "New" ) "Writing ideas")
    ("b" (my/refile t/org-project-file "Quick Box") "Quick Box")
    ("e" (my/refile t/org-project-file "Emacs Improvements (this is temporary for better inbox management)") "emacs improvements")
    ("r" (my/refile t/org-someday-maybe-file "Reading List") "Reading List")
    ("s" (my/refile t/org-someday-maybe-file "New") "someday maybe")
    ("n" (my/refile t/org-inbox-file "Inbox") "move back")
    ("l" (org-roam-refile) "org-roam-refile")
    ("f" (org-refile) "refile")
    ;; ("a" t/org-refile-hydra-file-archive "archive" :exit t)
    ;; ("s" t/org-refile-hydra-file-someday-maybe "someday maybe" :exit t)
    ;; ("w" t/org-refile-hydra-file-writing-ideas "writing ideas" :exit t)
    ("p" org-refile-goto-last-stored "Jump to last refile" :exit t)
    ;;TODO: add "move to last refile location"
    ;;TODO: add something to add org-roam links to headings
    ("i" (org-roam-node-insert) "org-roam-node-insert")
    ("q" nil "cancel"))


  (defhydra t/define-projects-hydra (:foreign-keys run)
    "Refile"
    ("A" (my/refile t/org-archive-file "Archive" ) "Archive")
    ("a" (org-archive-subtree) "really archive")
    ("w" (my/refile t/writing-ideas "New" ) "Writing ideas")
    ("b" (my/refile t/org-project-file "Quick Box") "Quick Box")
    ("e" (my/refile t/org-project-file "Emacs Improvements (this is temporary for better inbox management)") "emacs improvements")
    ("r" (my/refile t/org-someday-maybe-file "Reading List") "Reading List")
    ("s" (my/refile t/org-someday-maybe-file "New") "someday maybe")
    ("n" (my/refile t/org-inbox-file "Inbox") "move back")
    ("l" (org-roam-refile) "org-roam-refile")
    ("f" (org-refile) "refile")
    ;; ("a" t/org-refile-hydra-file-archive "archive" :exit t)
    ;; ("s" t/org-refile-hydra-file-someday-maybe "someday maybe" :exit t)
    ;; ("w" t/org-refile-hydra-file-writing-ideas "writing ideas" :exit t)
    ("p" org-refile-goto-last-stored "Jump to last refile" :exit t)
    ;;TODO: add "move to last refile location"
    ;;TODO: add something to add org-roam links to headings
    ("i" (org-roam-node-insert) "org-roam-node-insert")
    ("q" nil "cancel"))
  ;; (global-set-key (kbd "<SPC> m s r") 't/org-refile-hydra/body)

  (defun t/org-refile (&optional args)
    (interactive)
    (cond ((file-equal-p buffer-file-name t/org-inbox-file)
           (t/inbox-hydra/body))
          ((file-equal-p buffer-file-name t/org-project-file)
           (t/define-projects-hydra/body))
          ((org-refile))))

  (map! :after org-roam
        :map org-mode-map
        "<f12>"  #'anki-editor-cloze-region-dont-incr
        "<f11>"  #'anki-editor-cloze-region-auto-incr
        "<f10>"  #'anki-editor-reset-cloze-number
        "<f9>"   #'anki-editor-push-tree
        :localleader
        :prefix "m"
        :prefix "s")
  ;; TODO figure out how to define keybinds based on file?
  ;; enable sound:
  (setq org-clock-play-sound t)
  (setq org-tag-persistent-alist '(("continue?") ("@unterwegs") ("anki" . ?a) ("logbook")
                                   ("high_energy") ("IS_RECURRING" . ?R) ("pause" . ?p) ("FVP" . ?f) ("university")
                                   ("Effort") ("COLUMNS") ("low_energy") ("kein_Datum") ("Fokus") ("leo")
                                   ("Brainstorm" . ?b) ("@pc" . ?p) ("uni" . ?u) ("Computergrafik") ("laughing") ("projekt")
                                   ("@zuhause" . ?z)))

  (setq org-track-ordered-property-with-tag nil
        org-log-into-drawer nil)

  (with-no-warnings
    (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
    (custom-declare-face '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))
                                        ;look here for how to do this
  (setq org-todo-keywords
        '((sequence ; Not sure what the sequence is doing here (where it gets evaluated?)
           "TODO(t)"      ; A task that needs doin            g & is ready to do
           "PROJ(P)"      ; A project, which usually contains other tasks
           "LOOP(r)"      ; A recurring task
           "STRT(s)"      ; A task that is in progress
           "WAIT(W)"      ; Something external is holding up this task
           "HOLD(h)"      ; This task is paused/on hold because of me
           "IDEA(i)"      ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d)"    ; Task successfully completed
           "KILL(k)")   ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"                     ; A task that needs doing
           "[-](S)"                     ; Task is in progress
           "|"
           "[X](D)")                    ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)")
          (sequence
           "[??](C)"                    ; Confusion marker in notes
           "|"
           "[?](w)")                     ; Task is being held up or paused

          (sequence
           "PRO(p)"                     ; Pro in pro-con list
           "CON(c)"
           "|"))                        ; Con in pro and con list
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("[??]" . +org-todo-cancel)   ; Confusion marker in notes
          ("WAIT" . +org-todo-onhold)
          ("HOLD" . +org-todo-onhold)
          ("PRO" . +org-todo-onhold)
          ("CON" . +org-todo-cancel)
          ("PROJ" . +org-todo-project)
          ("NO"   . +org-todo-cancel)
          ("KILL" . +org-todo-cancel)))


  ;; org-agenda filters:
  ;; (setq org-stuck-projects
  ;;       '("+PROJECT/-MAYBE-DONE" ("NEXT" "TODE")))

  (setq org-agenda-custom-commands
        '(("n" "Agenda and all TODOs")
          ("z" "Zuordnen" ;; Zuordnen is for selecting tags
           ((agenda "")
            (tags-todo "")))
                                        ; TODO Add dates view
          ("d" "Date view")             ;selects dates
                                        ; ; TODO Add priority view
                                        ; TODO customize stuck projects

          ("l" "Show Leo's TODOs that are currently due."
           ((tags-todo "+leo")))))
  ;; (org-agenda-overriding-header "Leo's TODOs that are currently due")
  ;; (org-agenda-time-grid nil)
  ;; (org-deadline-warning-days 0)


  ;; TODO add filter for only todos that are due and that are for leo
  ;; NOTE example custom commands:
  ;; (setq org-agenda-custom-commands
  ;;       '(("x" agenda)
  ;;
  ;;         ("y" agenda*)
  ;;         ("w" todo "WAITING")
  ;;         ("W" todo-tree "WAITING")
  ;;         ("u" tags "+boss-urgent")
  ;;         ("v" tags-todo "+boss-urgent")
  ;;         ("U" tags-tree "+boss-urgent")
  ;;         ("f" occur-tree "\\<FIXME\\>")
  ;;         ("h" . "HOME+Name tags searches") ;description for "h" prefix
  ;;         ("hl" tags "+home+Lisa")
  ;;         ("hp" tags "+home+Peter")
  ;;         ("hk" tags "+home+Kim")))
  (setq org-export-with-tasks nil
        org-refile-use-cache t) ; FIXME: this line might cause trouble, but testing it, because refile was unbearably slow




  ;; (defun mdlinks-to-orglinks ()
  ;;     (interactive)
  ;;     (evil-ex "%s/\\[\\(.*?\\)\\](\\(.*?\\))/[[\\1][\\2]]/g"))

  (defun english-format-time-string (time-string)
    (let ((current-locale current-language-environment))
      (set-locale-environment "English")
      (let ((time (format-time-string time-string)))
        (set-locale-environment current-locale)time)))
  ;; "%Y-%02m-%02d %3a %02H:%02M"

  (defun pushblog ()
    "Parse blog entries to blog directory and push to github"
    (interactive)
    (let ((process-buffer-name  "*push-blog*"))
      (start-file-process "tassilos_invocation.sh" process-buffer-name "~/repos/lazyblorg/tassilos_invocation.sh")
      (pop-to-buffer process-buffer-name)))

  (defvar memacs-root org-directory)
  (defvar memacs-file-pattern "photos.org_archive") ;; also possible: "*.org"


  (setq org-link-abbrev-alist      ; FIXME Memacs links don't work at the moment
        '(("tsfile" .
           "/home/tassilo/org-roam/photos.org_archive::/\*.*%s/")))

  (defun my-handle-tsfile-link (querystring)
    "query stuff that is supposed to make memacs links work"
    (message (concat "DEBUG1: querystring: " querystring))
    (message (concat "DEBUG2: "
                     "grep \""
                     querystring
                     "\" "
                     (concat memacs-root memacs-file-pattern)))
    ;; get a list of hits
    (let ((queryresults (split-string
                         (s-trim
                          (shell-command-to-string
                           (concat
                            "grep \""
                            querystring
                            "\" "
                            (concat memacs-root memacs-file-pattern))))
                         "\n" t)))
      (message (concat "DEBUG3: queryresults: " (car queryresults)))
      ;; check length of list (number of lines)
      (cond
       ((= 0 (length queryresults))
        ;; edge case: empty query result
        (message "Sorry, no results found for query: %s" querystring))
       (t
        (with-temp-buffer
          (insert (if (= 1 (length queryresults))
                      (car queryresults)
                    (completing-read "Choose: " queryresults)))
          (org-mode)
          (goto-char (point-min))
          (org-next-link)
          (org-open-at-point))))))

  (org-link-set-parameters
   "tsfile"
   :follow (lambda (path) (my-handle-tsfile-link path))
   :help-echo "Opens the linked file with your default application"
   :face '(:foreground "DarkSeaGreen" :underline t))


  ;;taken from lazyblorg
  (defun my-lazyblorg-test()
    "Saves current blog entry to file and invoke lazyblorg process with it"
    (interactive)
    (save-excursion
      (turn-off-evil-mode)
      (search-backward ":blog:") ;; search begin of current (previous) blog entry
      (beginning-of-line nil)
      (set-mark-command nil) ;; set mark
      (org-cycle nil)        ;; close org-mode heading and sub-headings
      (org-forward-heading-same-level 1)
      (forward-line -1)
      (let ((p (point)) ;; copy region
            (m (mark)))
        (if (< p m)
            (kill-ring-save p m)
          (kill-ring-save m p)))
      (find-file "/tmp/lazyblorg-preview.org") ;; hard coded temporary file (will be overwritten)
      (erase-buffer)                           ;; I told you!
      (yank)                                   ;; paste region from above
      (save-buffer)                            ;; save to disk
      (kill-buffer "lazyblorg-preview.org")    ;; destroy last evidence
      (forward-line -1)                        ;;
      (org-cycle nil) ;; close org-mode heading and sub-headings
      ;; invoke lazyblorg:
      (start-file-process "preview_blog_entry.sh" "*preview-blog*" "/home/tassilo/repos/lazyblorg/preview_blogentry.sh")
      (turn-on-evil-mode)))

  (defun preview-blogentry-current-file ()
    "preview current blogentry in the browser"
    (interactive)
    (start-file-process "preview_blogentry" "*preview_blog_entry*" "~/repos/lazyblorg/preview_blogentry.sh" (buffer-file-name (buffer-base-buffer)))))

(use-package! org-roam
  :defer-incrementally t              ;did the after org thing trigger something
  :config



  (setq org-roam-node-default-sort 'file-atime) ; list files by last access not modifiy time!
  (map! (
         :map org-roam-mode-map
         :localleader
         :prefix "m"
         :desc "org-roam-dailies-goto-today" "t" #'org-roam-dailies-goto-today
         :desc "org-roam-extract-subtree" "x" #'org-roam-extract-subtree)) ;FIXME: these shortcuts do not seem to be evaluated at the right time!

  (setq daily-template
        (concat
         "#+title: %<%Y-%m-%d>\n* [/] Do Today (FDT)\n* [/] Maybe Do Today"
         "\n* Morgenroutine"
         "\n - [ ] Kalender angesehen"
         "\n - [ ] Start tracking"
         "\n - [ ] Medis genommen"
         "\n - [ ] Ziele gesetzt"
         "\n - [ ] Review Anki"
         "\n - [ ] Brush Teeth"
         "\n* Evening Routine"
         "\n - [ ] Review daily list"
         "\n - [ ] review timetracking"
         "\n - [ ] Brush Teeth" ;;put before checking habits
         "\n - [ ] Schedule the day for tomorrow (paper)"
         "\n - [ ] Do active questions"
         "\n - [ ] Answer Journal Questions"
         "\n* Inbox"
         "\n* Journal"
         "\n* Evening Journal"
         "\n** What did you achieve today?"
         "\n** What are you grateful for?"
         "\n** What worried you today?"
         "\n** What else is on your mind?"))

  (defvar t/phrases (list
                     (cons "What subtle things did you notice today?" (cons 1 1))
                     (cons "What meaningfull or important thing should you tell a particular person that you havent't said to them yet?" (cons 1 1))
                     (cons "Think about things you like about other people" (cons 1 1))
                     (cons "If you could go back in time and change one thing about your past, what would it be?" (cons 1 1))
                     (cons "What did I do today that was fun?" (cons 1 1))
                     (cons "What would you do, if you knew you could not fail?" (cons 1 1))
                     (cons "Write a 'thank you' letter to someone" (cons 1 1))
                     (cons "If you could have dinner with anyone currently alive, who would it be?" (cons 1 1))
                     (cons "What are you looking forward to the most?" (cons 1 1))
                     (cons "What surprised you today?" (cons 1 1))
                     (cons "What did I notice today?" (cons 1 1))
                     (cons "What is the most outrageous thing you did recently?" (cons 1 1))
                     (cons "Which experience are you the most thankfull for? Why?" (cons 1 1))
                     (cons "What’s a brave thing you did last week?" (cons 1 1))
                     (cons "When was I at peace today?" (cons 1 1))
                     (cons "What have been your biggest mistakes recently? What have you learned from them?" (cons 1 1))
                     (cons "How did you feel connected to others today?" (cons 1 1))
                     (cons "What was something playfull you did today?" (cons 1 1))
                     (cons "What battles have you fought and overcome in your life?" (cons 1 1))
                     (cons "How would you like to spend your spare time?" (cons 1 1))
                     (cons "What would you do if money were no object?" (cons 1 1))
                     (cons "What’s your secret desire?" (cons 1 1))
                     (cons "What made me feel energized today?" (cons 1 1))
                     (cons "What opportunity presented itself today?" (cons 1 1))
                     (cons "What made me appreciate my city, state or country today?" (cons 1 1))
                     (cons "Who was I happy to meet with, chat with, or run into today?" (cons 1 1))
                     (cons "How was I able to help others today" (cons 1 1))
                     (cons "What compliments did I receive today?" (cons 1 1))
                     (cons "What problem was I able to resolve today?" (cons 1 1))
                     (cons "What was one small victory I had today?" (cons 1 1))
                     (cons "How did you feel, when you woke up today?" (cons 1 1))
                     (cons "What has did you accomplish today?" (cons 1 1))
                     (cons "What was the biggest turning point in your life, and how did that experience change you?" (cons 1 1))
                     (cons "What simple pleasure did I enjoy today?" (cons 1 1))
                     (cons "What could you do to bring more of what really excites you into your life?" (cons 1 1))
                     (cons "Summarized in just a few sentences, what is your life's story?" (cons 1 1))
                     (cons "What would you like the next chapter of this story to be?" (cons 1 1))
                     (cons "What would you say is the greatest accomplishment of your life so far? Brag for a minute." (cons 1 1))
                     (cons "What do you want to make sure you do, achieve, or experience before you're gone?" (cons 1 1))
                     (cons "In recent years, what's the biggest lesson you've learned about yourself?" (cons 1 1))
                     (cons "Who inspires you most, and why do you find them inspiring?" (cons 1 1))
                     (cons "What was the biggest turning point in your life, and how did that experience change you?" (cons 1 1))
                     (cons "What are you taking for granted that you want to remember to be grateful for?" (cons 1 1))
                     (cons "Think for a moment about the biggest problem right now in your life. If that problem was happening to a close friend instead of to you, what would you say to comfort or advise that friend?" (cons 1 1))
                     (cons "What meaningful or important thing should you tell a particular person that you haven't said to them yet?" (cons 1 1))
                     (cons "When are you going to tell this person this meaningful or important thing?" (cons 1 1))
                     (cons "What's one of the best days you've had in your entire life? Describe what happened that day." (cons 1 1))
                     (cons "What in your life that you have the power to change is most limiting your long-term happiness?" (cons 1 1))
                     (cons "What could you start doing now to address what you said is most limiting your happiness?" (cons 1 1))
                     (cons "If you had to have roughly the same work day, 5 days a week, for the next 10 years, what activities would you ideally want this work day to consist of?" (cons 1 1))
                     (cons "What can you do to make your current job closer to this ideal, or to help you get a job that is closer to this ideal?" (cons 1 1))
                     (cons "What is the most important thing that you know you really should do but which you have trouble getting yourself to do?" (cons 1 1))
                     (cons "What could you do now to make it more likely that you actually do this important thing?" (cons 1 1))
                     (cons "What do you think is holding you back from achieving more in your life than you've achieved so far?" (cons 1 1))
                     (cons "What could you start doing now that would help address what you said is holding you back in life?" (cons 1 1))
                     (cons "In your opinion, what is the purpose or meaning of life?" (cons 1 1))
                     (cons "How is the best version of yourself different from the way you sometimes behave?" (cons 1 1))
                     (cons "What has kept you hopeful in life's most challenging moments?" (cons 1 1))
                     (cons "During what period of your life were you the happiest, and why were you so happy then?" (cons 1 1))
                     (cons "Imagine that you received a message from a version of yourself five years in the future. What warnings would the message give you, and what advice would it offer about how best to achieve your goals?" (cons 1 1))
                     (cons "If you knew for a fact that you were going to die exactly 10 years from now, how would you change your current behavior?" (cons 1 1))
                     (cons "Suppose you knew that you were going to die instantly (but painlessly) in exactly 7 days. What would you spend your last week doing?" (cons 1 1))
                     (cons "If you could plan one nearly perfect (but still actually realistic) day for yourself, what would you spend that day doing? Describe that day, from when you wake up until you go to sleep." (cons 1 1))
                     (cons "When is the soonest that you can treat yourself to this perfect day, or to another day that you'll really enjoy and remember?" (cons 1 1))))




  (setq desktop-globals-to-save
        '(desktop-missing-file-warning
          tags-file-name
          tags-table-list
          search-ring
          regexp-search-ring
          register-alist
          file-name-history
          t/phrases))

  (setq org-roam-dailies-directory "daily/")
  (setq org-roam-dailies-capture-templates
        `(
          ("Journal" "daily" plain "%T\n%?\n"
           :if-new (file+head+olp "%<%Y-%m-%d>.org" ,daily-template ("Journal")))))

  (setq +org-roam-open-buffer-on-find-file nil)

  ;; (setq org-roam-db-gc-threshold most-positive-fixnum) ;; Mentioned
  ;; performance optimization in the manual. According to measurements I did on
  ;; my machine this is actually making things worse if you already use a
  ;; reasonable value


  (defun org-hide-properties ()
    "Hide all org-mode headline property drawers in buffer. Could be slow if it has a lot of overlays."
    (interactive)
    (save-excursion
      (goto-char (point-min))
      (while (re-search-forward
              "^ *:properties:\n\\( *:.+?:.*\n\\)+ *:end:" nil t)
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

  ;; (add-hook 'org-roam-mode-hook #'org-hide-properties)
  ;; NOTE: above disabled because org-hide-properties
  ;; is annoying and costs performance. I don't want it by default

  (defun completion-ignore-case-enable ()
    "enable completion in org-mode"
    (setq completion-ignore-case t))
  (add-hook 'org-mode-hook #'completion-ignore-case-enable))


(use-package! websocket
  :after org-roam)

(use-package! org-roam-ui
  :after org-roam
  :config
  (setq org-roam-ui-sync-theme t
        org-roam-ui-follow t
        org-roam-ui-update-on-save t
        org-roam-ui-open-on-start t))

;; see doom readme for biblio for further config info
(use-package! citar  ;right package?
  :after org
  :config
  (setq! bibtex-dialect 'biblatex) ;; NOTE this is how I formated the thing under bibliography!
  ;; NOTE below stuff needs to be set to bibtex-completion-bibliography instead
  ;; of citar-bibliography if working with different completion engine than
  ;; vertico. See https://github.com/doomemacs/doomemacs/tree/master/modules/tools/biblio
  ;; not sure where my files for zotero are currently stored
  ;; once I understand that part I might uncomment this:
  ;; You may also set the respective note and library path variables as well for enhanced functionality:
  (setq!
   citar-bibliography '("~/repos/bibliography/zotLib.bib")
   citar-library-paths '("~/Zotero/storage/") ; TODO I should probably set this to an exported thing
   citar-notes-path `(,(concat org-directory "lit/")))) ; TODO figure out how to fix this?


(use-package! org-download
  :after org
  :init
  (map! :leader
        :map org-mode-map
        :prefix "d"
        :desc "org-screenshot" "d" #'org-download-screenshot)
  (add-hook 'dired-mode-hook 'org-download-enable)
  (setq org-image-actual-width nil))

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
(define-and-bind-quoted-text-object "dot" "." "\\." "\\.")
(define-and-bind-quoted-text-object "dollar" "$" "\\$" "\\$")
(define-and-bind-quoted-text-object "colon" ":" ":" ":")
(define-and-bind-quoted-text-object "code" "ℝ" "\\#\\+BEGIN_SRC" "\\#\\+END_SRC")
(define-and-bind-quoted-text-object "code2" "Π" "\\#\\+begin_src" "\\#\\+end_src")

(global-set-key (kbd "C-c g") 'org-recoll-search)
(global-set-key (kbd "C-c u") 'org-recoll-update-index)

(setq auto-save-default t
      make-backup-files t)


(map! :after spray
      :map spray-mode-map
      "s" #'spray-slower
      "d" #'spray-faster
      "j" #'spray-backward-word
      "k" #'spray-stop
      "l" #'spray-forward-word
      "SPC" #'spray-stop
      "q" #'spray-quit)

(add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil)))

(after! emacs-lisp-mode
  (setq doom-scratch-initial-major-mode emacs-lisp-mode)
  (add-hook 'emacs-lisp-mode-hook
            (lambda ()
              (add-hook 'before-save-hook 'untabify nil t))))

(after! evil
  (map! :map evil-colemak-basics-keymap
        :nv "N" 'evil-scroll-page-down
        :nv "E" 'evil-scroll-page-up)
  (global-hl-todo-mode) ;NOTE: this might be the most terrible place to activate this. I don't know a better one though.
  (setq evil-want-fine-undo t)
  (setq-default delete-by-moving-to-trash t))

(after! lsp
  (setq lsp-pyls-plugins-black-args '("--line-length" "80")))

(use-package! term ;;was something up with term?
  :after org
  :config
  (setq explicit-shell-file-name "zsh")
  (setq explicit-zsh-args '())          ; I don't know what this is for?
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))



;;this is required for benchmark-init to stop complaining
(cl-letf (((symbol-function 'define-obsolete-function-alias) #'defalias))
  (use-package benchmark-init           ;is this overdue?
    :config
    (require 'benchmark-init-modes)     ; explicitly required
    (add-hook 'after-init-hook #'benchmark-init/deactivate)))

(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "$")
                                       ("#+END_SRC" . "ℝ")
                                       ("#+RESULTS:" . "↦")
                                       ("#+begin_src" . "<<")
                                       ("#+end_src" . ">>")))

(setq prettify-symbols-unprettyfy-at-point 'rigth-edge)
(add-hook 'org-mode-hook 'prettify-symbols-mode) ;;is this causing problems?

;;HACK make not everything red because of spell-fu
(remove-hook 'text-mode-hook #'spell-fu-mode)

(after! company
  ;; (setq +lsp-company-backends '(company-tabnine :separate company-capf company-yasnippet))
  (setq company-idle-delay 0.3      ;; this value should not be 0!
        company-minimum-prefix-lenght 2)) ;; this value should not be 0!

(use-package! nyan-mode
  :hook (doom-modeline-mode . nyan-mode))

(menu-bar-mode 1)

;; custom functions
(defun t/random-phrase ()
  (interactive)
  (while (progn
           (setq t/last (seq-random-elt t/phrases))
           (insert (car t/last)))))
(defun t/incr-last ()
  (interactive)
  (setcar (last t/last)
          (1+ (car (last t/last)))))
(defun t/decr-last ()
  (interactive)
  (setcdr (last t/last)
          (1+ (cdr (last t/last)))))

(map! :localleader ;;markdown mappings stolen from here: https://dotdoom.rgoswami.me/config.html
      :map markdown-mode-map
      :prefix ("i" . "Insert")
      :desc "Blockquote"    "q" 'markdown-insert-blockquote
      :desc "Bold"          "b" 'markdown-insert-bold
      :desc "Code"          "c" 'markdown-insert-code
      :desc "Emphasis"      "e" 'markdown-insert-italic
      :desc "Footnote"      "f" 'markdown-insert-footnote
      :desc "Code Block"    "s" 'markdown-insert-gfm-code-block
      :desc "Image"         "i" 'markdown-insert-image
      :desc "Link"          "l" 'markdown-insert-link
      :desc "List Item"     "n" 'markdown-insert-list-item
      :desc "Pre"           "p" 'markdown-insert-pre
      (:prefix ("h" . "Headings")
       :desc "One"   "1" 'markdown-insert-atx-1
       :desc "Two"   "2" 'markdown-insert-atx-2
       :desc "Three" "3" 'markdown-insert-atx-3
       :desc "Four"  "4" 'markdown-insert-atx-4
       :desc "Five"  "5" 'markdown-insert-atx-5
       :desc "Six"   "6" 'markdown-insert-atx-6))

;;open external terminal
(defun run-terminal-here ()
  (interactive "@")
  (shell-command "urxvt > /dev/null 2>&1 & disown" nil nil))

(after! ccls
  ;;function for fixing autocorrect (should be added as a hook at some point (though naively adding to c++-mode would trigger infinite loop))
  (defun t/c++-mode ()
    "FIXME: not actually sure what I used this for?"
    (interactive)
    (progn (c++-mode)
           (setq-local flycheck-checker 'g++-gcc)))

  (setq ccls-initialization-options '(:index (:comments 2) :completion (:detailedLabel t)))
  (set-lsp-priority! 'ccls 2); optional as ccls is the default in Doom
  (defun t/compile-c++ ()
    "Function for compiling icpc exercises"
    (setq-local compile-command
                (concat
                 "g++ -std=gnu++17 -Og -g -Wall -Wextra -Wconversion -fsanitize=address -fsanitize=undefined "
                 (buffer-file-name)
                 "&& cat 1.in | ./a.out | diff 1.out -")))
  (add-hook! 'c-mode-hook (flycheck-select-checker 'c/c++-gcc))) ;;FIXME: hope this fixes flycheck with c++


;; FIXME figure out how to make the python-mypy thing work. Just installing mypy didn't work, but might have been impatient.
(after! flycheck
  (setq flycheck-checkers (delq 'python-mypy flycheck-checkers)))

(after! dap
  (setq dap-python-debugger 'debugpy))

(after! tramp (setq tramp-terminal-type "tramp")) ;fixing hangs because tramp does not understand the shell prompt:


;;helpful for not saving secret stuff on disk
(setq dabbrev-ignored-buffer-regexps '(".*\.gpg$" "^ [*].*"))

(defun my-company-dabbrev-ignore (buffer)
  "configure emacs to not search in encrypted files, or hidden buffers"
  (let (res)
    (dolist (re '("\.gpg$" "^ [*]") res)
      (if (string-match-p re (buffer-name buffer))
          (setq res t)))))
(setq company-dabbrev-ignore-buffers 'my-company-dabbrev-ignore)

;; TODO Disabeling the crypto-hook from doom config, because it seems ate all the memory on my system: [[file:~/org-roam/22-8-15 profiler-report][profile]]   (not sure why)?

(defun tassilo/open-pdf (filepath)
  (find-file filepath))

;;trying things to improve latency issues:
(setq lsp-print-performance t)

;; TODO remap every keybind that uses öäü?

(use-package! copilot
  ;; :hook (prog-mode . copilot-mode) ;disabled because sometimes copilot is just annoying/leading me to do stupid things:
  ;; use some more inconvenient keybind for copilot
  :config
  ;; The tab keybinds are used twice because there is apparently a subtle difference
  ;; see: https://discourse.doomemacs.org/t/how-to-re-bind-keys/56 for more info
  (evil-define-key* 'insert copilot-mode-map
    (kbd "C-c SPC") #'copilot-accept-completion-by-word)
  (evil-define-key* 'insert copilot-mode-map
    (kbd "C-C RET") #'copilot-accept-completion))


(defun ediff-copy-both-to-C ()
  "Ediff function to combine A and B region. For some reason this does not exist
by default."
  (interactive)
  (ediff-copy-diff ediff-current-difference nil 'C nil
                   (concat
                    (ediff-get-region-contents ediff-current-difference 'A ediff-control-buffer)
                    (ediff-get-region-contents ediff-current-difference 'B ediff-control-buffer))))
(defun add-d-to-ediff-mode-map ()
  (define-key ediff-mode-map "d" 'ediff-copy-both-to-C))
(add-hook 'ediff-keymap-setup-hook 'add-d-to-ediff-mode-map)


;; ;; RefTeX settings
;; (after! reftex
;;   (add-hook 'LaTeX-mode-hook 'turn-on-reftex)
;;   (setq reftex-plug-into-AUCTeX t))

(after! tex
  (setq +latex-enable-unicode-math nil)
  (setq +latex-viewers '(zathura))
  (add-hook! 'TeX-mode-hook
    (setq TeX-save-query nil
          TeX-show-compilation nil
          bibtex-files '("/home/tassilo/repos/bachelorthesis-workspace-tassilo/thesis/BachelorArbeit.bib")))
  (add-hook! 'TeX-mode-hook #'hs-minor-mode)
  (add-hook! 'LaTeX-mode-hook #'outline-minor-mode))

(after! vimish-fold
  (map! :map vimish-fold-mode-map
        :mode normal
        :desc "Fold with avy" "z f" #'vimish-fold-avy))

(after! tree-sitter
  ;; adding latex mode to treesitter NOTE: this can be removed once latex mode is merged into main branch of treesitter.
  (cl-pushnew '(LaTeX-mode . latex) tree-sitter-major-mode-language-alist :test #'equal))


(map! (:when (modulep! :tools lookup)
        :leader :desc "execute emacs command" :r "SPC" #'execute-extended-command
        :leader :desc "execute emacs command" :r ":" #'projectile-find-file
        :leader :desc "list buffers" :r "b l" #'list-buffers

        :leader :desc "window management" :r "C-j" #'+evil/window-move-left
        :leader :desc "window management" :r "C-ö" #'+evil/window-move-right
        :leader :desc "window management" :r "C-l" #'+evil/window-move-up
        :leader :desc "window management" :r "C-k" #'+evil/window-move-down))
;;Own custom version of mapping at ~/.emacs.d/modules/config/default/+evil-bindings.el
(map!
 :m [tab] (cmds! (and (modulep! :editor snippets)
                      (evil-visual-state-p)
                      (or (eq evil-visual-selection 'line)
                          (not (memq (char-after) (list ?\( ?\[ ?\{ ?\} ?\] ?\))))))
                 #'yas-insert-snippet
                 ;; Fixes #4548: without this, this tab keybind overrides
                 ;; mode-local ones for modes that don't have an evil
                 ;; keybinding scheme or users who don't have :editor (evil
                 ;; +everywhere) enabled.
                 (or (doom-lookup-key
                      [tab]
                      (list (evil-get-auxiliary-keymap (current-local-map) evil-state)
                            (current-local-map)))
                     (doom-lookup-key
                      (kbd "TAB")
                      (list (evil-get-auxiliary-keymap (current-local-map) evil-state)))
                     (doom-lookup-key (kbd "TAB") (list (current-local-map))))
                 it
                 (fboundp '+fold/toggle)
                 #'+fold/toggle))

(setq c-default-style "k&r")

(use-package! fatebook
  :defer-incrementally t
  :commands (fatebook-create-question))


;;(use-package! elisp-lint
;;  :commands elisp-lint-buffer)


(after! projectile
  (defun remove-project-from-cache (project-path)
    "Function to remove projects that are not in `known projects' from projectiles cache."
    (let ((projectile-cache-file "~/.emacs.d/.local/cache/projectile.cache")
          cache-data)
      (when (file-exists-p projectile-cache-file)
        (with-temp-buffer
          (insert-file-contents projectile-cache-file)
          (setq cache-data (read (current-buffer))))
        (remhash project-path cache-data)
        (with-temp-file projectile-cache-file
          (print cache-data (current-buffer)))))))
;; (load-file "emacs-hm-env.el")
