(setq user-full-name "Tassilo Neubauer"
      user-mail-address "tassilo.neubauer@gmail.com")

(setq   org-directory "~/org-roam"
        org-roam-directory org-directory
        projectile-project-search-path '("~/repos")
        zot-bib (concat (getenv "HOME") "/repos/bibliography/zotLib.bib")
        config-file "config.org")

(use-package! anki-editor
  :after org
  :init

  :bind (:map org-mode-map
         ("<f12>" . anki-editor-cloze-region-auto-incr)
         ("<f11>" . anki-editor-cloze-region-dont-incr)
         ("<f10>" . anki-editor-reset-cloze-number)
         ("<f9>"  . anki-editor-push-tree))
  :hook (org-capture-after-finalize . anki-editor-reset-cloze-number) ; Reset cloze-number after each capture.

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
  ;; Initialize
  (anki-editor-reset-cloze-number)

  (setq org-my-anki-file (concat org-roam-directory "anki-stuff.org"))
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
                 "* [[%:link][%:description]] \n \n %? \n%i \n %T"
                 :prepend t))
  (add-to-list 'org-capture-templates
               '("S" "Todo Protocoll" entry
                 (file+headline +org-capture-notes-file "Inbox")
                 "* [[%:link][% \"%:description\"]] \n \n* TODO %? %i \n %T"
                 :prepend t
                 :kill-buffer t))
)

(defun toggle-maximize-buffer () "Maximize buffer"
       (interactive)
       (if (= 1 (length (window-list)))
           (jump-to-register '_)
         (progn
           (window-configuration-to-register '_)
           (delete-other-windows))))

(use-package! vterm
  :after org
  :commands vterm
  :config
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *")
  (setq vterm-shell "zsh")
  (setq vterm-max-scrollback 10000))

(use-package! term
  :after org
  :config
  (setq explicit-shell-file-name "zsh")
  (setq explicit-zsh-args '()) ; I don't know what this is for?
  (setq term-prompt-regexp "^[^#$%>\n]*[#$%>] *"))


(map! (:when (featurep! :tools lookup)
 :leader :desc "projectile find file" :r ":" #'projectile-find-file
 :leader :desc "execute emacs command" :r "SPC" #'execute-extended-command))

(cl-letf (((symbol-function 'define-obsolete-function-alias) #'defalias))
 (use-package benchmark-init
   :config
   (require 'benchmark-init-modes)                             ; explicitly required
   (add-hook 'after-init-hook #'benchmark-init/deactivate)))

(after! org
  :config
  (setq org-export-with-tasks nil))

(use-package! org-roam
  :after org
  :init
  (map! (:map org-mode-map
         :localleader
         :prefix "m"
        :desc "org-roam-extract-subtree" "x" #'org-roam-extract-subtree
        ))
  :config

  (setq daily-template
        (concat
   "#+title: %<%Y-%m-%d>\n* [/] Do Today\n* [/] Maybe Do Today"
   "\n* Morgenroutine"
   "\n - [ ] Kalender angesehen"
   "\n - [ ] Start tracking"
   "\n - [ ] Check Bedtime yesterday"
   "\n - [ ] Medis genommen"
   "\n - [ ] Uhr angezogen"
   "\n - [ ] Ziele gesetzt"
   "\n - [ ] Review Anki"
   "\n - [ ] Brush Teeth"
   "\n* Evening Routine"
   "\n - [ ] Check Habits/Beeminder"
   "\n - [ ] Tasks Reviewed"
   "\n - [ ] Timetracking Reviewed"
   "\n - [ ] Ask Journal Questions"
   "\n - [ ] Determine bedtime"
   "\n - [ ] Review Anki"
   "\n - [ ] Brush Teeth"
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

(add-hook 'org-mode-hook #'org-hide-properties)

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


  ;;(setq org-roam-link-title-format "%s")
  (require 'org-roam-protocol)


  ;;Bibliography configuration
  ;;
  (setq
   bibtex-completion-notes-path org-directory
   bibtex-completion-bibliography zot-bib
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
    ":END:\n\n")))

(use-package! org-ref
  :after org
  :config
  :init
  (setq org-ref-completion-library 'org-ref-ivy-cite
        org-ref-get-pdf-filename-function 'org-ref-get-pdf-filename-helm-bibtex)
  (setq
   org-ref-default-bibliography (list zot-bib)
   org-ref-bibliography-notes  (concat org-roam-directory "bibliography.org")
   org-ref-note-title-format "* TODO %y - %t\n :PROPERTIES:\n  :Custom_ID: %k\n  :NOTER_DOCUMENT: %F\n :ROAM_KEY: cite:%k\n  :AUTHOR: %9a\n  :JOURNAL: %j\n  :YEAR: %y\n  :VOLUME: %v\n  :PAGES: %p\n  :DOI: %D\n  :URL: %U\n :END:\n\n"
   org-ref-notes-directory (concat org-roam-directory "/lit")
   org-ref-notes-function 'orb-edit-notes))

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
;;; this macro was copied from here:2
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
(define-and-bind-quoted-text-object "dollar" "$" "\\$" "\\$") ;; sometimes your have to escape the regex

(after! pdf-tools
  (add-hook! 'pdf-tools-enabled-hook
    (pdf-view-midnight-minor-mode 1)))

(use-package! org-noter
  :after org
  :config
  (setq org-noter-notes-search-path '("~/org-roam/")))

(use-package! org-pdftools
  :after org
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

(use-package! org-download
  :after org
  :init
  (map! :leader
        :prefix "d"
        :desc "org-screenshot" "<C-c>" #'org-download-screenshot))

;; org-download
;; Drag-and-drop to `dired`
(add-hook 'dired-mode-hook 'org-download-enable)
(setq org-image-actual-width nil)

(setq auto-save-default t
      make-backup-files t)

;; dark mode for pdfs
(after! pdf-tools
  (add-hook! 'pdf-tools-enabled-hook
    (pdf-view-midnight-minor-mode 1)))

(map! :after spray
      :map spray-mode-map
      "s" #'spray-slower
      "d" #'spray-faster
      "j" #'spray-backward-word
      "k" #'spray-stop
      "l" #'spray-forward-word
      "SPC" #'spray-stop
      "q" #'spray-quit
      )

(global-wakatime-mode)
(global-activity-watch-mode)

(org-roam-bibtex-mode)

(map! :after emr
      :map prog-mode-map
      "M-RET" #'emr-show-refactor-menu)

(defun post-tangle-config ()
  (and (file-in-directory-p
        buffer-file-name (file-name-directory config-file))
(shell-command "sed -i '/^[^\"]*TODO[^\"]*$/d' config.md; sed -i '/^[^\"]*DONE[^\"]*$/d' config.md")
                ))
(defun private-enable-post-tangle ()
  (add-hook 'after-save-hook #'test-hooks-emacs nil 'local))
(defun post-tangle-config ()
  (shell-command "ls"))
(after! org
 (add-hook 'org-mode-hook #'private-enable-post-tangle))

(use-package! elpy
  :defer t
  :init
  (advice-add 'python-mode :before 'elpy-enable))
