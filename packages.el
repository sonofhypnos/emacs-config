;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;tracking
(package! activity-watch-mode)
(package! wakatime-mode)

;autocompletion
(package! company-tabnine
  :recipe (:host github :repo "TommyX12/company-tabnine"))

;org
(package! org-mode :recipe (:repo "https://code.orgmode.org/bzg/org-mode.git")) ;see https://github.com/hlissner/doom-emacs/issues/4534
(package! org-ref)
(package! org-noter)
(package! org-habit-plus
  :recipe (:host github
           :repo "myshevchuk/org-habit-plus"))
(package! org-mind-map)

(package! elpy)
(package! real-auto-save
  :recipe (:host github
           :repo "ChillarAnand/real-auto-save"))
(package! emacs-todoist
  :recipe (:host github
           :repo "abrochard/emacs-todoist"))
(package! org-roam-bibtex
  :recipe (:host github :repo "org-roam/org-roam-bibtex"))
(unpin! helm-bibtex)
(package! spray)
(package! openwith)
(package! org-recoll)
;;(package! helm-swoop)
(package! benchmark-init)
(package! format-all)
(unpin! org-roam)
(package! websocket)
(package! org-roam-ui :recipe (:host github :repo "org-roam/org-roam-ui" :files ("*.el" "out")))
(package! md-roam
  :recipe (:host github
           :repo "nobiot/md-roam"))
(package! org-pdftools
  :recipe (:host github
           :repo "fuxialexander/org-pdftools"))
(package! org-noter-pdftools)
(package! org-download
  :recipe (:host github
           :repo "abo-abo/org-download"))
(package! org-auto-tangle)
(package! org-roam
  :recipe (:host github :repo "org-roam/org-roam"))
(package! org-roam-bibtex
 :recipe (:host github :repo "org-roam/org-roam-bibtex"))
(package! mathpix.el
  :recipe (:host github :repo "jethrokuan/mathpix.el"))
(package! anki-editor)

(package! vimgolf)
(package! pdf-tools)
(package! zotxt)

;testing out
(package! monkeytype)
(package! lispy)
(package! nyan-mode)
(package! i3
  :recipe (:host github
           :repo "vava/i3-emacs"))
(package! emr)

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA , ELPA or emacsmirror:
;;(package! some-package)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;;(package! another-package
;;  :recipe (:host github :repo "username/repo"))

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
