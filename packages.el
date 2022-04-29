;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;org
(package! org-mode :recipe (:repo "https://code.orgmode.org/bzg/org-mode.git")) ;see  hlissner/doom-emacs#4534
(package! openwith)
(unpin! org-roam)
(package! org-download
  :recipe (:host github
           :repo "abo-abo/org-download"))
(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam"))
(package! anki-editor)
(package! real-auto-save
  :recipe (:host github
           :repo "ChillarAnand/real-auto-save")) ;; not sure whether this helps with anything?
;autocompletion
(package! company-tabnine
  :recipe (:host github :repo "TommyX12/company-tabnine"))
(package! nyan-mode)

;degugging
(package! benchmark-init)

;not really used
(package! zotxt)
(package! org-ref)
(package! org-noter)
(unpin! helm-bibtex)
(package! org-roam-bibtex
 :recipe (:host github :repo "org-roam/org-roam-bibtex"))
(package! org-pdftools
  :recipe (:host github
           :repo "fuxialexander/org-pdftools"))
(package! md-roam
  :recipe (:host github
           :repo "nobiot/md-roam"))
(package! websocket)
(package! org-recoll)
(package! org-ql)
(package! helm-org-ql)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
(package! htmlize) ;not sure why this was added?
(package! format-all)
        ;tracking
        (package! wakatime-mode)
(package! pdf-tools)
(package! org-noter-pdftools)
(package! mathpix.el
  :recipe (:host github :repo "jethrokuan/mathpix.el"))

;testing out
(package! org-fc
	:recipe (:host github
		:repo "l3kn/org-fc"
                :files (:defaults "awk" "*.org"))) ;flashcards for org trying whether this has better
(package! org-auto-tangle) ; not sure why added?
(package! i3
  :recipe (:host github
           :repo "vava/i3-emacs"))
(package! org-remark
  :recipe (:host github
           :repo "nobiot/org-remark"))
(package! emr) ;not working in C where it would be useful
(package! highlight)


;;(package! activity-watch-mode) maybe add again when debugged

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;;(package! this-package
;;  :recipe (:host github :repo "username/repo"
;;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;;(package! builtin-package :recipe (:nonrecursive t))
;;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with;; see raxod502/straight.el#279)
;;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;;(unpin! pinned-package)
;; ...or multiple packages
;;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED;; will likely break things)
;;(unpin! t)
