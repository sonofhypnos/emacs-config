;; -*- mode: emacs-lisp; -*-
((emacs29  ; Default profile for Emacs 29.1
  (user-emacs-directory . "~/.emacs.d/")
  (doom-user-dir . "~/.doom.d/")
  ("DOOMDIR" . "~/.doom.d/")
  ("EMACS_BIN" . "emacs-29.1")
  ;; Set this as the default profile
  (is-default . t))

 (emacs28  ; Profile for Emacs 30
  ("EMACS_BIN" . "emacs-28.1.90"))

 (emacs-igc  ; Profile for Emacs 30
  ("EMACS_BIN" . "emacs-29.1")
  (user-emacs-directory . "~/.emacs30.d/")
  (doom-user-dir . "~/.doom.d/")
  ("DOOMDIR" . "~/.doom.d/")
  ))
