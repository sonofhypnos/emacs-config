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
(setq doom-theme 'doom-one)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")




;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type t)

;; (use-package! org-roam-bibtex
;;  :after (org-roam)
;;  :hook (org-roam-mode . org-roam-bibtex-mode)
;;  :config
;;  (setq org-roam-bibtex-preformat-keywords
;;   '("=key=" "title" "url" "file" "author-or-editor" "keywords"))
;;  (setq orb-templates
;;        '(("r" "ref" plain (function org-roam-capture--get-point)
;;           ""
;;           :file-name "${slug}"
;;           :head "#+TITLE: ${=key=}: ${title}\n#+ROAM_KEY: ${ref}\n#+ROAM_TAGS:
;;
;;- keywords :: ${keywords}
;;
;;\n* ${title}\n  :PROPERTIES:\n  :Custom_ID: ${=key=}\n  :URL: ${url}\n  :AUTHOR: ${author-or-editor}\n  :NOTER_DOCUMENT: %(orb-process-file-field \"${=key=}\")\n  :NOTER_PAGE: \n  :END:\n\n"
;;
;;           :unnarrowed t))))


;;journal template copied from here: https://org-roam.discourse.group/t/dailies-capture-templates-best-practices/1043
    (setq org-roam-dailies-capture-templates
          (let ((head "#+title: %<%Y-%m-%d (%A)>\n* [/] Do Today\n* [/] Maybe Do Today\n* Journal\n** What did you achieve today?\n** What are you grateful for?\n** What worried you today?\n** What else is on your mind?"))
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
               :olp ("Do Today")
               :immediate-finish t)
              ("m" "maybe do today" item
               #'org-roam-capture--get-point
               "[ ] %(princ as/agenda-captured-link)"
               :file-name "daily/%<%Y-%m-%d>"
               :head ,head
               :olp ("Maybe Do Today")
               :immediate-finish t))))




(server-start)



(after! org-roam
        (map! :leader
        :prefix "a"
        :desc "org-roam" "l" #'org-roam
        :desc "org-roam-insert" "i" #'org-roam-insert
        :desc "org-roam-switch-to-buffer" "b" #'org-roam-switch-to-buffer
        :desc "org-roam-find-file" "f" #'org-roam-find-file
        :desc "org-roam-show-graph" "g" #'org-roam-server-mode
        :desc "org-roam-capture" "c" #'org-roam-capture
        :desc "org-roam-tag-add" "t" #'org-roam-tag-add
        :desc "org-roam-dailies-today" "d" #'org-roam-dailies-today
        :desc "org-roam-alias-add" "a" #'org-roam-alias-add
        :desc "org-roam-tag-delete" "ö" #'org-roam-tag-delete))

;;org-roam server creates an interactive graph from the org-roam files in the browser.
(use-package org-roam-server
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


(use-package wakatime-mode
  :ensure t)

(add-hook 'after-init-hook 'org-roam-mode)
(add-hook 'after-init-hook 'org-zotxt-mode)
(global-wakatime-mode)

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
