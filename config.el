(setq user-full-name "Tassilo Neubauer"
      user-mail-address "tassilo.neubauer@gmail.com")

(setq   org-directory "~/org-roam/"
        org-roam-directory "~/org-roam/"
        projectile-project-search-path '("~/repos" "~/Dropbox/")
        zot-bib "~/repos/bibliography/zotLib.bib")

(setq org-agenda-files
'("~/org-roam/to-read.org" "/home/tassilo/org-roam/projects.org" "/home/tassilo/org-roam/20210528214526-journaling_tabelle_05_28_2021.org" "/home/tassilo/org-roam/journal.org" "/home/tassilo/org-roam/notes.org" "/home/tassilo/org-roam/someday_maybe.org" "/home/tassilo/org-roam/todos.org"))

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
  (setq ispell-personal-dictionary "~/.hunspell_personal")
  (unless (file-exists-p ispell-personal-dictionary)
  (write-region "" nil ispell-personal-dictionary nil 0)))
;; The personal dictionary file has to exist, otherwise hunspell will
;; silently not use it.

(map! :after anki-editor
      :map org-mode-map
        "<f12>"  #'anki-editor-cloze-region-auto-incr
        "<f11>"  #'anki-editor-cloze-region-dont-incr
        "<f10>"  #'anki-editor-reset-cloze-number
        "<f9>"   #'anki-editor-push-tree)

(use-package! anki-editor
  :after org-roam

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
  (anki-editor-reset-cloze-number))

(after! org
  (with-no-warnings
  (custom-declare-face '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) "") ;; see dooms org module for more examples of how to do this.
  (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) ""))

  (global-evil-motion-trainer-mode 1)
  (setq evil-motion-trainer-threshold 6)
  (emt-add-suggestion 'evil-next-line 'evil-avy-goto-char-timer)
;; See also: (emt-add-suggestions)

(add-hook 'python-mode-hook
  (lambda ()
    (make-variable-buffer-local 'evil-snipe-aliases)
    (push '(?: "def .+:") evil-snip(add-hook 'python-mode-hook
  (lambda ()
    (make-variable-buffer-local 'evil-snipe-aliases)
    (push '(?: "def .+:") evil-snipe-aliases)))
e-aliases)))

  (with-no-warnings
    (custom-declare-face '+org-todo-active  '((t (:inherit (bold font-lock-constant-face org-todo)))) "")
    (custom-declare-face '+org-todo-project '((t (:inherit (bold font-lock-doc-face org-todo)))) "")
    (custom-declare-face '+org-todo-onhold  '((t (:inherit (bold warning org-todo)))) "")
    (custom-declare-face '+org-todo-cancel  '((t (:inherit (bold error org-todo)))) ""))
  (setq org-todo-keywords
        '((sequence   ; Not sure what the sequence is doing here (where it gets evaluated?)
           "TODO(t)"  ; A task that needs doin            g & is ready to do
           "PROJ(P)"  ; A project, which usually contains other tasks
           "LOOP(r)"  ; A recurring task
           "STRT(s)"  ; A task that is in progress
           "WAIT(w)"  ; Something external is holding up this task
           "HOLD(h)"  ; This task is paused/on hold because of me
           "IDEA(i)"  ; An unconfirmed and unapproved task or notion
           "|"
           "PRO(p)"   ; Pro in pro-con list
           "CON(c)"   ; Con in pro and con list
           "DONE(d)"  ; Task successfully completed
           "KILL(k)") ; Task was cancelled, aborted or is no longer applicable
          (sequence
           "[ ](T)"   ; A task that needs doing
           "[-](S)"   ; Task is in progress
           "[?](W)"   ; Task is being held up or paused
           "[??](C)"  ; Confusion marker in notes
           "|"
           "[X](D)")  ; Task was completed
          (sequence
           "|"
           "OKAY(o)"
           "YES(y)"
           "NO(n)"))
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
          ("KILL" . +org-todo-cancel))))

(map! (:after org-roam
        :map org-mode-map
        :localleader
        :prefix "m"
        :desc "org-roam-dailies-goto-today" "t" #'org-roam-dailies-goto-today
        :desc "org-roam-extract-subtree" "x" #'org-roam-extract-subtree))
(after! org
  :config
  (setq org-export-with-tasks nil)
  (add-hook 'focus-out-hook
        (lambda () (org-save-all-org-buffers))))


(use-package! org-roam
  :after org

:config
(setq daily-template
      (concat
       "#+title: %<%Y-%m-%d>\n* [/] Do Today\n* [/] Maybe Do Today"
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
       "\n - [ ] Check Habits/Beeminder"
       "\n - [ ] Tasks Reviewed"
       "\n - [ ] Timetracking Reviewed ([[id:4d96fd27-2523-475a-a791-a67f9996e5a4][Enter Deep Work]])"
       "\n - [ ] Anwer Journal Questions"
       "\n - [ ] Do active questions"
       "\n - [ ] Review Anki"
       "\n - [ ] Brush Teeth"
       "\n - [ ] Prepare Backpack"
       "\n* Inbox"
       "\n* Journal"
       "\n* Evening Journal"
       "\n** What did you achieve today?"
       "\n** What are you grateful for?"
       "\n** What worried you today?"
       "\n** What else is on your mind?"))

(setq org-roam-dailies-directory "daily/")
(setq org-roam-dailies-capture-templates
      `(
        ("Journal" "daily" plain "%T\n%?\n"
         :if-new (file+head+olp "%<%Y-%m-%d>.org" ,daily-template ("Journal")))
       ))

(setq org-roam-capture-templates
      '(("d" "default" plain
         "%?"
         :if-new (file+head "${slug}.org"
                            "#+title: ${title}\n#+created: %<%y-%m-%d %H:%M>\n* Next\n* Related\n")
         :immediate-finish t
         :unnarrowed t)))

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
    (setq completion-ignore-case t))
(add-hook 'org-mode-hook #'completion-ignore-case-enable)

(defun tassilo/scratch-window-p ()
  (string= (substring-no-properties (cdr (assoc 'name (frame-parameters))))
                                    "_emacs scratchpad_"))
(defun tassilo/org-capture-cleanup ()
  "Delete capture windows if it is a scratch window"
  (and (tassilo/scratch-window-p)
       ;This worked for me opposed to just using just (delete-frame), so as long as it works I won't touch it (Similar use of progn below)
      (progn
        (progn
    (start-process "i3-msg" "*i3-msg*" "i3-msg" "scratchpad show")
    (org-roam-db-sync)
     (delete-frame))
     nil)))
(add-hook 'org-capture-after-finalize-hook #'tassilo/org-capture-cleanup)

(defun tassilo/org-capture-setup ()
  (and (tassilo/scratch-window-p)
       (progn
         (delete-other-windows)))) ;For some reason "progn" fixes both of my functions. I might want to find out why in the future, but for now I am happy it works at all.
(add-hook 'org-capture-mode-hook #'tassilo/org-capture-setup)

(require 'org-roam-protocol)

(setq org-my-anki-file (concat org-roam-directory "anki-stuff.org"))

(add-to-list 'org-capture-templates
             `("l" "Link" entry (file+headline ,(concat org-roam-directory "/20210510194711-read_and_take_notes.org") "Links")
               "* [[%:link][%:description]]\n %?\n \n %i\n%T"
               :immediate-finish t))
(add-to-list 'org-capture-templates
            '("a" "Anki basic"
                entry
                (file+headline org-my-anki-file "Dispatch Shelf")
                "* %<%y-%m-%d %H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Basic\n:ANKI_DECK: .main\n:END:\n** Front\n%?\n** Back\n%x\n"))
(add-to-list 'org-capture-templates
            '("A" "Anki cloze"
                entry
                (file+headline org-my-anki-file "Dispatch Shelf")
                "* %<%y-%m-%d %H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE: Cloze\n:ANKI_DECK: .main\n:END:\n** Text\n%?\n** Extra\n%f\n%x"))
(add-to-list 'org-capture-templates
            '("T" "Anki type"
                entry
                (file+headline org-my-anki-file "Dispatch Shelf")
                "* %<%y-%m-%d %H:%M>   %^g\n:PROPERTIES:\n:ANKI_NOTE_TYPE:1typing\n:ANKI_DECK: .main\n:END:\n** Text\n%?\n** Extra\n%x"))
(add-to-list 'org-capture-templates
             '("L" "Protocol Link" entry
               (file+headline +org-capture-notes-file "Inbox")
               "* [[%:link][%:description]] \n \n \n%i \n %T"
               :prepend t))
(add-to-list 'org-capture-templates
             '("S" "Todo Protocoll" entry
               (file+headline +org-capture-notes-file "Inbox")
               "* [[%:link][% \"%:description\"]] \n \n* TODO %? %i \n %T"
               :prepend t
               :kill-buffer t))

(setq org-roam-capture-ref-templates
      '(("r" "ref" plain
         "%u %?\n\n* \" %c\"  "
         :if-new (file+head "${slug}.org"
                            "#+title: ${title}\n#+author:\n")
         :unnarrowed t))))

(defun make-capture-frame ()
    "Create a new frame and run org-capture."
    (interactive)
    (make-frame '((name . "capture")))
    (require 'noflet)
    (select-frame-by-name "capture")
    (delete-other-windows)
    (noflet ((switch-to-buffer-other-window (buf) (switch-to-buffer buf)))
    (org-capture)))

(use-package! websocket
    :after org-roam)

(use-package! org-roam-ui
    :after org-roam
    :config
    (setq org-roam-ui-sync-theme t
          org-roam-ui-follow t
          org-roam-ui-update-on-save t
          org-roam-ui-open-on-start t))

(use-package! org-noter
  :after org
  :config
  (setq org-noter-notes-search-path '("~/org-roam/")))

(use-package! bibtex
  :init
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
  :after org-roam
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
(define-and-bind-quoted-text-object "dollar" "$" "\\$" "\\$") ;; You don't have to
(define-and-bind-quoted-text-object "code" "ℝ" "\\#\\+BEGIN_SRC" "\\#\\+END_SRC")
(define-and-bind-quoted-text-object "code2" "Π" "\\#\\+begin_src" "\\#\\+end_src")

(after! pdf-tools
  (add-hook! 'pdf-tools-enabled-hook
    (pdf-view-midnight-minor-mode 1)))

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

(defun tassilo/post-tangle-config ()
    (and (file-in-directory-p
        buffer-file-name doom-private-dir)
       (async-shell-command "cp config.org README.org && sed -i '/^[^\"]*TODO[^\"]*$/d' README.org")
       (start-process "compile-config" "*compile-config*" "compile-config.sh" "~/.doom.d")))

(defun tassilo/enable-post-tangle ()
  (add-hook 'after-save-hook #'tassilo/post-tangle-config nil 'local))

(after! org
  (add-hook 'org-mode-hook #'tassilo/enable-post-tangle))
(add-to-list 'display-buffer-alist '("*Async Shell Command*" display-buffer-no-window (nil)))

(after! emacs-lisp-mode
  (setq doom-scratch-initial-major-mode emacs-lisp-mode))

(use-package! org-auto-tangle
  :defer t
  :hook (org-mode . org-auto-tangle-mode)
  :config
  (setq org-auto-tangle-default t))

(after! evil
  (setq evil-want-fine-undo t)
  (setq-default delete-by-moving-to-trash t)
  (global-wakatime-mode)
  (global-activity-watch-mode))

(use-package! openwith
  :after-call pre-command-hook
  :config
 ;; (openwith-mode t) ;keeping openwith-mode disabled until I've found a solution for inline images
  (add-to-list 'openwith-associations '("\\.pdf\\'" "zathura" (file)))

    (defadvice org-display-inline-images
    (around handle-openwith
            (&optional include-linked refresh beg end) activate compile)
    (if openwith-mode
        (progn
            (openwith-mode -1)
            ad-do-it
            (openwith-mode 1))
        ad-do-it)))

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
 :leader :desc "execute emacs command" :r "SPC" #'execute-extended-command
 :leader :desc "helm-projectile-rg" :r "l" #'helm-projectile-rg))

(cl-letf (((symbol-function 'define-obsolete-function-alias) #'defalias))
 (use-package benchmark-init
   :config
   (require 'benchmark-init-modes) ; explicitly required
   (add-hook 'after-init-hook #'benchmark-init/deactivate)))

(setq-default prettify-symbols-alist '(("#+BEGIN_SRC" . "$")
                                       ("#+END_SRC" . "ℝ")
                                       ("#+RESULTS:" . "↦")
                                       ("#+begin_src" . "<<")
                                       ("#+end_src" . ">>")))
(setq prettify-symbols-unprettyfy-at-point 'rigth-edge)
(add-hook 'org-mode-hook 'prettify-symbols-mode)

(after! emr (define-key prog-mode-map (kbd "M-RET") 'em2r-show-refactor-menu))

(after! lsp-rust
  (setq lsp-rust-server 'rust-analyzer))

(remove-hook 'text-mode-hook #'spell-fu-mode)

;;(defun my/monkeytype-mode-hook ()
;;  "Hooks for monkeytype-mode."
;;  (centered-cursor-mode)
;;  (evil-escape-mode -1)
;;  (evil-insert -1))
;;(add-hook 'monkeytype-mode-hook #'my/monkeytype-mode-hook)

(after! company
  (setq +lsp-company-backends '(company-tabnine :separate company-capf company-yasnippet))
  (setq company-show-numbers t)
  (setq company-idle-delay 0)
)

(use-package! nyan-mode
  :hook (doom-modeline-mode . nyan-mode))
