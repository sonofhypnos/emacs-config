;;; config.el -*- lexical-binding: t; -*-
;;;
; TODO Sprinkle in documentation from the org-file
(setq user-full-name "Tassilo Neubauer"
      user-mail-address "tassilo.neubauer@gmail.com")

; TODO maybe use something like defer-incrementally or is this done by doom?
;; :defer-incrementally SYMBOL|LIST|t
;;   Takes a symbol or list of symbols representing packages that will be loaded
;;   incrementally at startup before this one. This is helpful for large packages
;;   like magit or org, which load a lot of dependencies on first load. This lets
;;   you load them piece-meal during idle periods, so that when you finally do need
;;   the package, it'll load quicker.

;; (after! c++-mode
;;   (map! :map c++-mode-map
;;   "SPC c k" (lambda () (interactive) (manual-entry (current-word)))
;;         )

;;   )
;;   TODO I just learned that function definitions do not get byte-compiled if put into an after! block (probably any use-package block?). Fix this in my code!

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


;;configure haskell to support renaming stuff
(after! lsp-haskell
(add-hook 'lsp-after-initialize-hook
                #'(lambda ()
                (lsp--set-configuration
                '(:haskell (:plugin (:rename (:config (:crossModule t)))))
                ))))
;;go where refile takes you:
(defun +org-search ()
  (interactive)
  (org-refile '(4)))
(custom-set-variables '(org-agenda-files '("~/org-roam/projects.org" "~/org-roam/notes.org"))
                      '(org-refile-targets ((t/org-inbox-file t/org-project-file t/org-someday-maybe-file t/org-archive-file t/journal-file) :maxlevel . 3))) ;not sure about benefits of custom-set-variables


;;default in doom is to low. Not sure where all the memory is going
(setq gcmh-high-cons-threshold (*  100 1024 1024)) ;;give leeway: 1000 mb

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

;;Checking for stupid config mistakes
; TODO make sure this is triggered in the correct buffer on emacs startup
; FIXME check-init-file does not work at the moment check if langtool is detected if necessary

(defun check-init-file ()
 (while (re-search-forward "\\(use-package\\(.*\n?\\)\\)*)")
   (if (not (cl-some (rcurry #'string-match-p (match-string 0)) '(":after" ":defer"))))
   (warn "Do not use use-package without "))) ;; I
;;want to curry this function and then use map, but not quite sure how to do that in elisp

;; (use-package langtool
;;   :defer-incrementally t
;;   :config
;;   (setq langtool-language-tool-jar "~/repos/languagetool/LanguageTool-5.6-stable/languagetool.jar")
;;   (setq langtool-language-tool-server-jar "~/repos/languagetool/LanguageTool-5.6-stable/languagetool-server.jar")
;;   (setq langtool-server-user-arguments '("-p" "8081")))

;;emacs -e "(seq-random-elt '(\"Luan\" \"David\" \"Tassilo\" \"Simon\")"



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
  (let ((ispell-local "~/.hunspell_personal"))
        (setq ispell-personal-dictionary "~/.hunspell_personal")
        (unless (file-exists-p ispell-local)
                (with-temp-buffer (write-file ispell-local))
        ))
  )
;; The personal dictionary file has to exist, otherwise hunspell will
;; silently not use it.

;; (map! :after anki-editor
;;       :map org-mode-map
;;         "<f12>"  #'anki-editor-cloze-region-dont-incr
;;         "<f11>"  #'anki-editor-cloze-region-auto-incr
;;         "<f10>"  #'anki-editor-reset-cloze-number
;;         "<f9>"   #'anki-editor-push-tree)

;; (use-package! anki-editor
;;   :after org-roam
;;   :defer-incrementally t

;;   :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.

;;   :config
;;   (setq-default anki-editor-use-math-jax t)
;;   (setq anki-editor-org-tags-as-anki-tags t)

;;   (defun anki-editor-cloze-region-auto-incr (&optional arg)
;;     "Cloze region without hint and increase card number."
;;     (interactive)
;;     (anki-editor-cloze-region my-anki-editor-cloze-number "")
;;     (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
;;     (forward-sexp))
;;   (defun anki-editor-cloze-region-dont-incr (&optional arg)
;;     "Cloze region without hint using the previous card number."
;;     (interactive)
;;     (anki-editor-cloze-region (cond ((eq my-anki-editor-cloze-number 1)
;;                                      (progn
;;                                        (setq my-anki-editor-cloze-number (1+ my-anki-editor-cloze-number))
;;                                        1))
;;                                     (t (1- my-anki-editor-cloze-number))) "")
;;     (forward-sexp))
;;   (defun anki-editor-reset-cloze-number (&optional arg)
;;     "Reset cloze number to ARG or 1"
;;     (interactive)
;;     (setq my-anki-editor-cloze-number (or arg 1)))
;;   (defun anki-editor-push-tree ()
;;     "Push all notes under a tree."
;;     (interactive)
;;     (anki-editor-push-notes '(4))
;;     (anki-editor-reset-cloze-number))
;;   ;; Initialize
;;   (anki-editor-reset-cloze-number))

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
                (t/fzi :maxlevel . 1)
                ))

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
                        "* %<%y-%m-%d %H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: .main\n:END:\n** Text\n%?\n** Extra\n%f\n%x")
        ("T" "Anki type"
        entry
        (file+headline org-my-anki-file "Dispatch Shelf")
        "* %<%y-%m-%d %H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE:1typing\n:ANKI_DECK: .main\n:END:\n** Text\n%?\n** Extra\n%x")

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

;; org refile hydras
(defun my/refile (file headline &optional arg)
  "Refile to a specific location.
With a 'C-u' ARG argument, we jump to that location (see
`org-refile').
Use `org-agenda-refile' in `org-agenda' mode."
  (let* ((pos (with-current-buffer (or (get-buffer file)	;Is the file open in a buffer already?
				       (find-file-noselect file)) ;Otherwise, try to find the file by name (Note, default-directory matters here if it isn't absolute)
		(or (org-find-exact-headline-in-buffer headline)
		    (error "Can't find headline `%s'" headline))))
	 (filepath (buffer-file-name (marker-buffer pos)));If we're given a relative name, find absolute path
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
     ((and arg (listp arg))	    ;Are we jumping?
      (my/refile file headline arg))
     ;; Are we in org-capture-mode?
     (is-capturing      	;Minor mode variable that's defined when capturing
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

;;;;;;;;;;
;; Here we'll define our refile headlines
;;;;;;;;;;
;; (setq   org-directory "~/org-roam/"
;;         org-roam-directory "~/org-roam/"
;;         org-fc-diretories '(org-directory)
;;         org-archive-location (concat org-directory ".archive/%s::")
;;         t/org-inbox-file (concat org-directory "notes.org")
;;         t/org-project-file (concat org-directory "projects.org")
;;         t/org-someday-maybe-file (concat org-directory "someday_maybe.org")
;;         t/org-archive-file (concat org-directory "archive.org")
;;         t/journal-file (concat org-directory "journal.org")
;;         t/writing-ideas (concat org-directory "20210508185546-things_to_write_about.org")
;;         t/fzi (concat org-directory "fzi_assistant_job.org"))


;; ;;go where refile takes you:
;; (defun +org-search ()
;;   (interactive)
;;   (org-refile '(4)))
;; (custom-set-variables '(org-agenda-files '("~/org-roam/projects.org" "~/org-roam/notes.org"))
;;                       '(org-refile-targets ((t/org-inbox-file t/org-project-file t/org-someday-maybe-file t/org-archive-file t/journal-file) :maxlevel . 3))) ;not sure about benefits of custom-set-variables

;;not sure if I don't add a headline what will happen.
;; (t/make-org-refile-hydra t/org-refile-hydra-file-archive t/org-archive-file
;;                                 (("q" . "Archive")))
;; (t/make-org-refile-hydra t/org-refile-hydra-file-someday-maybe t/org-someday-maybe-file
;;                                 (("r" . "Reading List") ;;TODO figure out how to do deal with nested headline
;;                                  ("n" . "New")))
;; (t/make-org-refile-hydra t/org-refile-hydra-file-writing-ideas t/writing-ideas
;;                                 (("n" . "New")))
;; (t/make-org-refile-hydra t/org-refile-hydra-file-quick t/org-project-file
;; 			    (("q" . "Quick box")
;;                              ("e" . "Emacs Improvements (this is temporary for better inbox management)")))
;; (t/make-org-refile-hydra t/org-refile-hydra-file-inbox t/org-inbox-file)
;TODO: add emacs
;

(defhydra t/inbox-hydra (:foreign-keys run)
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
  ("q" nil "cancel")
  )

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
  ("q" nil "cancel")
  )
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
        :prefix "s"
; TODO figure out how to define keybinds based on file?
        :desc "tassilo's refile" "r" #'t/org-refile)
  ;; enable sound:
  (setq org-clock-play-sound t)

  ;;
(setq org-tag-persistent-alist '(("continue?") ("@unterwegs") ("anki" . ?a) ("logbook")
("high_energy") ("IS_RECURRING" . ?R) ("pause" . ?p) ("FVP" . ?f) ("university")
("Effort") ("COLUMNS") ("low_energy") ("kein_Datum") ("Fokus")
("Brainstorm" . ?b) ("@pc" . ?p) ("uni" . ?u) ("Computergrafik") ("laughing") ("projekt")
("@zuhause" . ?z)))

(setq org-track-ordered-property-with-tag nil
      org-log-into-drawer nil
      )

;;; ; TODO automatically add id on save (ask Hauke if sensible)
;; (defun my-org-add-ids-to-headlines-in-file ()
;;         "Add ID properties to all headlines in the current file which
;; do not already have one."
;;         (interactive)
;;         (org-map-entries #'org-id-get-create))
;; (add-hook 'org-mode-hook
;;         (lambda ()
;;                 (add-hook 'before-save-hook #'my-org-add-ids-to-headlines-in-file nil 'local)))
;;starting to try org-cite (not quite sure how and for what to use it) I mostly
;;want to use citation stuff with my blog, but also do something similar to
;;gwern with archiving entries

  (with-no-warnings
    (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
    (custom-declare-face '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))
  ;look here for how to do this
  (setq org-todo-keywords
        '((sequence   ; Not sure what the sequence is doing here (where it gets evaluated?)
           "TODO(t)"  ; A task that needs doin            g & is ready to do
           "PROJ(P)"  ; A project, which usually contains other tasks
           "LOOP(r)"  ; A recurring task
           "STRT(s)"  ; A task that is in progress
           "WAIT(W)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "|"
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "|"
           "[X](D)")  ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)")
          (sequence
           "[??](C)"  ; Confusion marker in notes
           "|"
           "[?](w)"   ; Task is being held up or paused
           )
          (sequence
           "PRO(p)"   ; Pro in pro-con list
           "CON(c)"
           "|"))   ; Con in pro and con list
        org-todo-keyword-faces
        '(("[-]"  . +org-todo-active)
          ("STRT" . +org-todo-active)
          ("[?]"  . +org-todo-onhold)
          ("[??]" . +org-todo-cancel)  ; Confusion marker in notes
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
  ("z" "Zuordnen"
   ((agenda "")
    (tags-todo "")))))



(setq org-export-with-tasks nil) ;;what is this?
;      org-refile-use-cache t) ;;testing for now (this might have caused major org trouble)

;; (defun mdlinks-to-orglinks ()
;;     (interactive)
;;     (evil-ex "%s/\\[\\(.*?\\)\\](\\(.*?\\))/[[\\1][\\2]]/g"))


(defun pushblog ()
  "Parse blog entries to blog directory and push to github"
  (interactive)
(start-file-process "tassilos_invocation.sh" "*push-blog*" "~/repos/lazyblorg/tassilos_invocation.sh"))

(defvar memacs-root org-directory)
(defvar memacs-file-pattern "photos.org_archive") ;; also possible: "*.org"


(setq org-link-abbrev-alist ; FIXME Memacs links don't work at the moment
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
        (search-backward ":blog:");; search begin of current (previous) blog entry
        (beginning-of-line nil)
        (set-mark-command nil);; set mark
        (org-cycle nil);; close org-mode heading and sub-headings
        (org-forward-heading-same-level 1)
        (forward-line -1)
        (let ((p (point));; copy region
        (m (mark)))
        (if (< p m)
        (kill-ring-save p m)
        (kill-ring-save m p)))
        (find-file "/tmp/lazyblorg-preview.org");; hard coded temporary file (will be overwritten)
        (erase-buffer);; I told you!
        (yank);; paste region from above
        (save-buffer);; save to disk
        (kill-buffer "lazyblorg-preview.org");; destroy last evidence
        (forward-line -1);;
        (org-cycle nil);; close org-mode heading and sub-headings
        ;; invoke lazyblorg:
        (start-file-process "preview_blog_entry.sh" "*preview-blog*" "/home/tassilo/repos/lazyblorg/preview_blogentry.sh")
        (turn-on-evil-mode)))

(defun preview-blogentry-current-file ()
  "preview current blogentry in the browser"
    (interactive)
(start-file-process "preview_blogentry" "*preview_blog_entry*" "~/repos/lazyblorg/preview_blogentry.sh" (buffer-file-name (buffer-base-buffer)))))

(use-package! org-roam
  :defer-incrementally t ;did the after org thing trigger something
  :config
(map! (
        :map org-roam-mode-map
        :localleader
        :prefix "m"
        :desc "org-roam-dailies-goto-today" "t" #'org-roam-dailies-goto-today
        :desc "org-roam-extract-subtree" "x" #'org-roam-extract-subtree)) ;FIXME: these shortcuts do not seem to be evaluated at the right time!

  ; TODO maybe load some of the big stuff here later (loading things like the defvar below took essentially 0 time)
(setq daily-template
      (concat
       "#+title: %<%Y-%m-%d>\n* [/] Do Today (FDT)\n* [/] Maybe Do Today"
       "\n* Morgenroutine"
       "\n - [ ] Lüften!"
       "\n - [ ] Kalender angesehen"
       "\n - [ ] Start tracking"
       "\n - [ ] Medis genommen"
       "\n - [ ] Uhr angezogen"
       "\n - [ ] Ziele gesetzt"
       "\n - [ ] Review Anki"
       "\n - [ ] Brush Teeth"
       "\n* Evening Routine"
       "\n - [ ] genug gegessen? "
       "\n - [ ] Review Anki" ;;put anki first so you can check it positive in habits
       "\n - [ ] Brush Teeth" ;;put before checking habits
       "\n - [ ] Timetracking Reviewed"
       "\n - [ ] Do active questions (in paper)"
       "\n - [ ] Check Habits/Beeminder"
       "\n - [ ] Tasks Reviewed"
       "\n - [ ] Answer Journal Questions (Look at prompts on vocab cards)"
       "\n - [ ] Prepare Backpack"
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
                        (cons "When is the soonest that you can treat yourself to this perfect day, or to another day that you'll really enjoy and remember?" (cons 1 1))
                        ))



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
         :if-new (file+head+olp "%<%Y-%m-%d>.org" ,daily-template ("Journal")))
       ))

;; Might wanna just want to go with default template here.
;; (setq org-roam-capture-templates
;;       '(("d" "default" plain
;;          "%?"
;;          :if-new (file+head "${slug}.org"
;;                             "#+title: ${title}\n#+created: %<%y-%m-%d %H:%M>\n* Next\n* Related\n")
;;          :immediate-finish t
;;          :unnarrowed t)))


(setq org-roam-capture-ref-templates
        '(
          ("r" "ref" plain "%?" :target
        (file+head "${slug}.org" "#+title: ${title}")
        :unnarrowed t
        :jump-to-captured t))

      )

(setq +org-roam-open-buffer-on-find-file nil)
(setq org-roam-db-gc-threshold most-positive-fixnum) ;; Mentioned performance optimization in the manual. I have enough memory anyways


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

(add-hook 'org-roam-mode-hook #'org-hide-properties)

(defun completion-ignore-case-enable ()
  "enable completion in org-mode"
    (setq completion-ignore-case t))
(add-hook 'org-mode-hook #'completion-ignore-case-enable)

;; setup org-capture hook stuff
(defun tassilo/scratch-window-p ()
  "Current fram is a scratchpad window"
  (string= (substring-no-properties (cdr (assoc 'name (frame-parameters))))
                                    "_emacs scratchpad_"))

;; (defun finalize-capture ()
;;   "finalize-capture"
;;   (interactive)
;;   (i3-hide-emacs)
;;   (org-capture-finalize))

;; ; FIXME figure out why map! is not working here:
;; (map! :map org-capture-mode-map
;;       "C-c C-c" #'finalize-capture)

;; macro stolen from here: https://github.com/SqrtMinusOne/dotfiles/blob/master/Emacs.org
(defmacro i3-msg (&rest args)
  `(start-process "emacs-i3-windmove" "i3-msg" "i3-msg" ,@args))
;; (defmacro press-key (&rest args)
;;   "HACK: Simulates keypress. This is useful for some functionality that I can't get to work otherwise (i3-scratchpad)"
;;         ;;sleep is in the command so the keypress to trigger this does not conflict with the keypress to trigger
;;         ;;; FIXME macro does not work
;;   `(shell-command ,(concat "sleep 0.1; xdotool key " ,@args))) ;;; FIXME not sure if @ is doing here. Also not sure if the second comma is needed
(defun press-key (bind)
  ;; (start-process "press-key" "*press-key*" "xdotool" bind)
  ;;       (start-process-shell-command "press-key" "*press-key*" "xdotool" "key" "super+e")
  ;; )
  (shell-command (concat "xdotool key " bind))
  )

(defun i3-hide-scratch ()
  "HACK: Hide emacs scratchpad by simulating key command!
        ;;other things tried:
        ;;[title=\\\"_emacs scratchpad_\\\"]
        ;;(shell-command \\\"sleep 0.1; xdotool key super+e\\\")"
  (and  (tassilo/scratch-window-p)
        ;;No real reason for doing this more then once. This is mostly for checking whether the lags are just inherent in the command (seems to not be the case.)
        (dotimes (i 1)
        (press-key "super+e")))) ;;


;;FIXME: I still do not get why this is slower when not directly evaluated, but instead triggered by org-capture? I tried to evaluate it before anything else.
;; I think one explanation could be that it is something about native compilation?
(defun tassilo/org-capture-prepare-cleanup (&optional args) ;;optional arguments, so advice work
       ;HACK: Using progn twice worked for me opposed to just using just (delete-frame), so as long as it works I won't touch it (Similar use of progn below)
        (and
        (not (org-roam-capture-p))
        (i3-hide-scratch))  ;;delete frame after having synced
        ) ;;not sure why nil here

(defun tassilo/org-capture-cleanup ()
  "Delete capture windows if it is a scratch window"
  (and (tassilo/scratch-window-p)
       ;HACK: Using progn twice worked for me opposed to just using just (delete-frame), so as long as it works I won't touch it (Similar use of progn below)
      (progn
        (and
        (not (org-roam-capture-p))
        (delete-frame)) ;;delete frame after having synced
        nil))) ;;not sure why nil here

(defun tassilo/org-capture-setup ()
  (and (tassilo/scratch-window-p)
        (doom/window-maximize-buffer) ;this does seem to work!
        )) ;For some reason "progn" fixes both of my functions. I might want to find out why in the future, but for now I am happy it works at all.

;; (add-hook 'org-capture-mode-hook #'tassilo/org-capture-setup) ;;TODO check in a while whether this just workes by adding :unnarrow t to templates
(advice-add 'org-capture-finalize :before #'tassilo/org-capture-prepare-cleanup)
;; (remove-hook 'org-capture-prepare-finalize-hook #'tassilo/org-capture-prepare-cleanup)
(add-hook 'org-capture-after-finalize-hook #'tassilo/org-capture-cleanup))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

;; (use-package! org-noter
;;   :after org
;;   :config
;;   (setq org-noter-notes-search-path org-directory))

;; see doom readme for biblio for further config info
;; (after! oc ;right package?
;;   (setq! bibtex-completion-bibliography '("~/repos/bibliography/zotLib.bib"))
;;   ;; not sure where my files for zotero are currently stored
;;   ;; once I understand that part I might uncomment this:
;;   ;; You may also set the respective note and library path variables as well for enhanced functionality:
;;  (setq! bibtex-completion-library-path "~/Zotero/storage/"
;;         bibtex-completion-notes-path (concat org-directory "lit/"))
;;         org-cite-follow-processor 'basic
;;  )

;; (use-package! org-pdftools
;;   :after org
;;   :hook (org-mode . org-pdftools-setup-link)
;;   (pdf-tools-install))

;; (use-package! org-noter-pdftools
;;   :after org-noter
;;   :config
;;   ;; Add a function to ensure precise note is inserted
;;   (defun org-noter-pdftools-insert-precise-note (&optional toggle-no-questions)
;;     (interactive "P")
;;     (org-noter--with-valid-session
;;      (let ((org-noter-insert-note-no-questions (if toggle-no-questions
;;                                                    (not org-noter-insert-note-no-questions)
;;                                                  org-noter-insert-note-no-questions))
;;            (org-pdftools-use-isearch-link t)
;;            (org-pdftools-use-freestyle-annot t))
;;        (org-noter-insert-note (org-noter--get-precise-info)))))
;;   ;; fix https://github.com/weirdNox/org-noter/pull/93/commits/f8349ae7575e599f375de1be6be2d0d5de4e6cbf
;;   (defun org-noter-set-start-location (&optional arg)
;;     "When opening a session with this document, go to the current location.
;; With a prefix ARG, remove start location."
;;     (interactive "P")
;;     (org-noter--with-valid-session
;;      (let ((inhibit-read-only t)
;;            (ast (org-noter--parse-root))
;;            (location (org-noter--doc-approx-location (when (called-interactively-p 'any) 'interactive))))
;;        (with-current-buffer (org-noter--session-notes-buffer session)
;;          (org-with-wide-buffer
;;           (goto-char (org-element-property :begin ast))
;;           (if arg
;;               (org-entry-delete nil org-noter-property-note-location)
;;             (org-entry-put nil org-noter-property-note-location
;;                            (org-noter--pretty-print-location location))))))))
;;   (with-eval-after-load 'pdf-annot
;;     (add-hook 'pdf-annot-activate-handler-functions #'org-noter-pdftools-jump-to-note)))

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

(after! pdf-tools
  (add-hook! 'pdf-tools-enabled-hook))

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
  (setq doom-scratch-initial-major-mode emacs-lisp-mode))

; TODO figure out why auto-tangle was configured?
;; (use-package! org-auto-tangle
;;   :defer t
;;   :hook (org-mode . org-auto-tangle-mode)
;;   :config
;;   (setq org-auto-tangle-default t))

(after! evil
  (setq evil-want-fine-undo t)
  (setq-default delete-by-moving-to-trash t)
  ;; (global-wakatime-mode)
  )

(use-package! openwith
  :after-call pre-command-hook
  :config
 (openwith-mode t) ;keeping openwith-mode disabled until I've found a solution for inline images

(add-to-list 'openwith-associations '("\\.pdf\\'" "zathura" (file)))

    (defadvice org-display-inline-images
    (around handle-openwith
            (&optional include-linked refresh beg end) activate compile)
    (if openwith-mode
        (progn
            (openwith-mode -1)
            ad-do-it ;;not sure what this line is for? should this be add-to-list? why is it not throwing errors? does it evaluate to false?
            (openwith-mode 1))
        ad-do-it)))


(use-package! term ;;was something up with term?
  :after org
  :config
  (setq explicit-shell-file-name "zsh")
  (setq explicit-zsh-args '()) ; I don't know what this is for?
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))

(map! (:when (featurep! :tools lookup)
 :leader :desc "execute emacs command" :r "SPC" #'execute-extended-command
 :leader :desc "execute emacs command" :r ":" #'projectile-find-file
 :leader :desc "list buffers" :r "b l" #'list-buffers

 :leader :desc "window management" :r "C-j" #'+evil/window-move-left
 :leader :desc "window management" :r "C-ö" #'+evil/window-move-right
 :leader :desc "window management" :r "C-l" #'+evil/window-move-up
 :leader :desc "window management" :r "C-k" #'+evil/window-move-down))

;;this is required for benchmark-init to stop complaining
(cl-letf (((symbol-function 'define-obsolete-function-alias) #'defalias))
 (use-package benchmark-init ;is this overdue?
   :config
   (require 'benchmark-init-modes) ; explicitly required
   (add-hook 'after-init-hook #'benchmark-init/deactivate)))

(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "$")
                                       ("#+END_SRC" . "ℝ")
                                       ("#+RESULTS:" . "↦")
                                       ("#+begin_src" . "<<")
                                       ("#+end_src" . ">>")))
(setq prettify-symbols-unprettyfy-at-point 'rigth-edge)
(add-hook 'org-mode-hook 'prettify-symbols-mode) ;;is this causing problems?

;; (after! emr (define-key prog-mode-map (kbd "M-RET") 'em2r-show-refactor-menu))

;; (after! lsp-rust
;;   (setq lsp-rust-server 'rust-analyzer))

;; (after! lsp-mode
;;   ;; (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.my-folder\\'")
;;   ;; ;; or
;;   ;; (add-to-list 'lsp-file-watch-ignored-files "[/\\\\]\\.my-files\\'"))
;;   (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\emacs\\'")
;;   )

;;HACK make not everything red because of spell-fu
(remove-hook 'text-mode-hook #'spell-fu-mode)

;;(defun my/monkeytype-mode-hook ()
;;  "Hooks for monkeytype-mode."
;;  (centered-cursor-mode)
;;  (evil-escape-mode -1)
;;  (evil-insert -1))
;;(add-hook 'monkeytype-mode-hook #'my/monkeytype-mode-hook)
(after! company
  ;; (setq +lsp-company-backends '(company-tabnine :separate company-capf company-yasnippet))
  (setq company-idle-delay 0.3)
  (setq company-minimum-prefix-lenght 2)) ;; this value should not be 0!

(use-package! nyan-mode
  :hook (doom-modeline-mode . nyan-mode))

(after! core-ui (menu-bar-mode 1)) ;;is this causing problems?

;; custom functions
(defun t/random-phrase ()
    (interactive)
      (while (progn
               (setq t/last (seq-random-elt t/phrases))
               ;; calcFunc-random function seems to be gone now!
               ;; (< (* (car (cdr (calcFunc-random '(float 1 0)))) (expt 10 -12))
               ;;                  (/(car (last t/last))
               ;;          (float (+ (car (last t/last))
               ;;                    (cdr (last t/last)))))) ;s+1 / n+2 see laplace rule of succession.
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

(after! evil
  (map! :map evil-colemak-basics-keymap
      :nv "N" 'evil-scroll-page-down
      :nv "E" 'evil-scroll-page-up))


;;open external terminal
(defun run-terminal-here ()
  (interactive "@")
  (shell-command "konsole > /dev/null 2>&1 & disown" nil nil))



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
;; (add-hook 'c++-mode-hook #'t/compile-c++)
;; (add-hook! c++-mode-hook
;;          (flycheck-select-checker 'c/c++-gcc))
) ;;FIXME: hope this fixes flycheck with c++ (actually this is terrible if I am not actually using this checker! (like with computergraphics!))





;; (after! python
;; ;;put following after python config:
;; ;;FIXME: python config makes babel no work (check if in babel mode first?)
;; (setq +python-ipython-repl-args '("-i" "--simple-prompt" "--no-color-info"))
;; (setq +python-jupyter-repl-args '("--simple-prompt"))
;;   (defun t/pyconf ()
;;     (interactive)
;;         (let ((dir (file-name-directory buffer-file-name)))
;;                 (setq-local compile-command
;;                         (concat "chmod +x " (buffer-file-name) ";cat " dir "1.in | " (buffer-file-name)))))

;;         (add-hook 'python-mode-hook #'t/pyconf)


;;         )



(map! :after c-or-c++-mode
;(global-set-key [] 'execute-c-program)
;; (global-set-key [f9] 'code-compile)
;;
(defun execute-c-program ()
  (interactive)
  (defvar foo)
  (setq foo (concat "gcc " (buffer-name) " && ./a.out" ))
  (shell-command foo))


(defun t/cconf ()
  "configure compile command for c-code"
  (interactive)
  (unless (file-exists-p "Makefile")
     (let ((file (file-name-nondirectory buffer-file-name)))
       (setq-local compile-command
        (format "%s -o %s %s"
           (if  (equal (file-name-extension file) "cpp") "g++" "gcc" )
           (file-name-sans-extension file)
           file))))
  ;not sure this is the right hook to call for c and c++
    (add-hook 'c-mode-hook #'t/cconf)))


(after! dap-mode  ;; according to doom config, the line below is everything I need. Not sure if keybinds already added
  (setq dap-python-debugger 'debugpy)
  ;; ; FIXME disabled because of bug with mode-map (not sure this is needed (does doom add dap keybinds?))

;; (map! :map dap-mode-map
;;       :leader
;;       :prefix ("d" . "dap")
;;       ;; basics
;;       :desc "dap next"          "n" #'dap-next
;;       :desc "dap step in"       "i" #'dap-step-in
;;       :desc "dap step out"      "o" #'dap-step-out
;;       :desc "dap continue"      "c" #'dap-continue
;;       :desc "dap hydra"         "h" #'dap-hydra
;;       :desc "dap debug restart" "r" #'dap-debug-restart
;;       :desc "dap debug"         "s" #'dap-debug

;;       ;; debug
;;       :prefix ("d" . "Debug")
;;       :desc "dap debug recent"  "r" #'dap-debug-recent
;;       :desc "dap debug last"    "l" #'dap-debug-last

;;       ;; eval
;;       :prefix ("e" . "Eval")
;;       :desc "eval"                "e" #'dap-eval
;;       :desc "eval region"         "r" #'dap-eval-region
;;       :desc "eval thing at point" "s" #'dap-eval-thing-at-point
;;       :desc "add expression"      "a" #'dap-ui-expressions-add
;;       :desc "remove expression"   "d" #'dap-ui-expressions-remove

;;       :prefix ("b" . "Breakpoint")
;;       :desc "dap breakpoint toggle"      "b" #'dap-breakpoint-toggle
;;       :desc "dap breakpoint condition"   "c" #'dap-breakpoint-condition
;;       :desc "dap breakpoint hit count"   "h" #'dap-breakpoint-hit-condition
;;       :desc "dap breakpoint log message" "l" #'dap-breakpoint-log-message)

  )
;; the stuff below should probably be loaded later

;; trying to configure dap mode might already be added in doom module for dap else:


;; (after! doom
;;   (run-hooks 'after-setting-font-hook)) ;;try this if .. happens again?


;; stuff fror flashcards
;; (after! org-fc ;;(remove require)
;;         :config
;;         (require 'org-fc-hydra)
;;         (evil-define-minor-mode-key '(normal insert emacs) 'org-fc-review-flip-mode
;;         (kbd "RET") 'org-fc-review-flip
;;         (kbd "n") 'org-fc-review-flip
;;         (kbd "s") 'org-fc-review-suspend-card
;;         (kbd "q") 'org-fc-review-quit)

;;         (evil-define-minor-mode-key '(normal insert emacs) 'org-fc-review-rate-mode
;;         (kbd "a") 'org-fc-review-rate-again
;;         (kbd "h") 'org-fc-review-rate-hard
;;         (kbd "g") 'org-fc-review-rate-good
;;         (kbd "e") 'org-fc-review-rate-easy
;;         (kbd "s") 'org-fc-review-suspend-card
;;         (kbd "q") 'org-fc-review-quit))


; TODO figure out how to make i3-mode actually work (keybinds don't seem to be stolen from i3 (though not sure did not really get both to use the same keybinds at the same time))
(after! i3-mode
  (setq i3-flavor 'i3
        i3-config-file "~/.config/regolith/i3/config")

  ;below disabled (because distracting)
        ;; (setq i3-bindings
        ;; '((?\C-l . "focus right")
        ;;         (?\C-h . "focus left")
        ;;         (?\C-k . "focus up")
        ;;         (?\C-j . "focus down")))
        ;; (defun i3--key-binding-config ()
        ;; "Append STR with the key bindings settings according to `i3-bindings' in i3 configuration format. Return the appended string"
        ; FIXME disabled the thing that changes keybinds

        ;; (with-temp-buffer
        ;; (let ((key-binding-string "\n"))
        ;; (dolist (binding i3-bindings)
        ;;         (let* ((mod (--> (car binding)
        ;;                 (event-modifiers it)
        ;;                 (-map 'symbol-name it)
        ;;                 (-map 's-capitalize it)
        ;;                 (s-join "+" it)))
        ;;         (key (-> (car binding)
        ;;                         event-basic-type vector (key-description nil)))
        ;;         (script (if (eq i3-flavor 'sway) "sway-call" "i3-call"))
        ;;         (cmd (concat script " "
        ;;                         (cdr binding) " "
        ;;                         (-> (car binding) vector key-description))))
        ;;         (setq key-binding-string
        ;;                 (concat key-binding-string
        ;;                         "bindsym " mod "+" key " exec --no-startup-id " cmd "\n"))))
        ;; (insert key-binding-string))
        ;; (buffer-string)))


        ;; (add-to-list 'i3-extra-config #'i3--key-binding-config)
        )


;; (after! tramp
;; ;fixing hangs because tramp does not understand the shell prompt:
;;         (setenv "SHELL" "/bin/sh")
;;         (setq tramp-shell-prompt-pattern "\\(?:^\\|\\)[^]#$%>\n]*#?[]#$%>] *\\(\\[[[:digit:];]*[[:alpha:]] *\\)*")
;;   )

;;supposedly helpful for not saving secret stuff on disk
(setq dabbrev-ignored-buffer-regexps '(".*\.gpg$" "^ [*].*"))

(defun my-company-dabbrev-ignore (buffer)
  "configure emacs to not search in encrypted files, or hidden buffers"
  (let (res)
    (dolist (re '("\.gpg$" "^ [*]") res)
      (if (string-match-p re (buffer-name buffer))
          (setq res t)))))
(setq company-dabbrev-ignore-buffers 'my-company-dabbrev-ignore)

;;; TODO Disabeling the crypto-hook from doom config, because it seems ate all the memory on my system: [[file:~/org-roam/22-8-15 profiler-report][profile]]   (not sure why)?

;;setup company modes:
(defun tassilo/open-pdf (filepath)
    (find-file filepath))
;;example
;; (after! js2-mode
;;   (set-company-backend! 'js2-mode 'company-tide 'company-yasnippet))
;;
;;
;;trying things to improve latency issues:
(setq lsp-print-performance t)
;;enabling garbage collection

;trying to use nix installed python server (seems to have worked?)
(use-package lsp-python-ms
  :ensure t
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp)))
  :init
  (setq lsp-python-ms-executable (executable-find "python-language-server")))

