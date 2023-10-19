;;; ../.dotfiles/doom.d/emacsQ.el -*- lexical-binding: t; -*-

(add-to-list 'load-path "~/.emacs.d/.local/straight/repos/dash.el/")
(add-to-list 'load-path "~/.emacs.d/.local/straight/repos/emacsql/")
(add-to-list 'load-path "~/.emacs.d/.local/straight/repos/compat/")
(require 'magit-section "~/.emacs.d/.local/straight/repos/magit/lisp/magit-section.el")
(add-to-list 'load-path "~/.emacs.d/.local/straight/repos/evil/")
(add-to-list 'load-path "~/.emacs.d/.local/straight/repos/org-roam/")
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

(add-to-list 'load-path "~/.emacs.d/.local/straight/repos/vertico/")

(setq org-roam-directory "~/org-roam/"
      org-roam-db-location "~/.emacs.d/.local/cache/org-roam.db"
      garbage-collection-messages t
      )



;; ;; ((emacs "26.1") (dash "2.13") (org "9.4") (emacsql "20230228") (magit-section "3.0.0"))
;; ;; (add-to-list 'load-path "~/.emacs.d/evil")
;; (require 'magit-section)
(require 'evil)
(require 'org-roam)
(require 'vertico)
(evil-mode 1)
(vertico-mode 1)
(package-initialize)
(setq comp-speed 2)
(setq package-native-compile t)
;; (setq evil-default-state 'normal)
;;Once you have tried just regular evil, you can also use doom evil by first
;;

;; ;; loading doom core and then loading the below:
;; ;;Load Doom core
;; (setq doom-init-p t)
;; (load "path/to/doom/core/core.el")

;; ;; Load specific Doom module, e.g., Evil
;; ;;load init
;; (load "~/.dotfiles/doom.d/emacsQinit.el")

;; ;; You could also do something like this:
;; ;; (load "path/to/doom/modules/editor/evil/config.el")
;; ;; (load "path/to/doom/modules/editor/evil/autoload.el")
