;; -*- mode: elisp -*-

;; init logging
(message "Starting Emacs %s" emacs-version)
(add-hook 'after-init-hook (lambda () (message "Initialization complete after %s" (emacs-init-time))) t)

;; minimal UI
(setq-default visible-bell t)
(setq inhibit-startup-message t)
(dolist (mode '(tool-bar-mode scroll-bar-mode))
  (when (fboundp mode) (funcall mode -1)))

;; trust me
(setq disabled-command-function nil)
(defalias 'yes-or-no-p 'y-or-n-p)

;; prefer UTF-8
(prefer-coding-system 'utf-8)

;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen 0)

;; Enable transient mark mode
(transient-mark-mode 1)

;;; AUTOSTART ;;;
;; Open org-agenda at start
(add-hook 'after-init-hook 'org-todo-list)
(add-hook 'after-init-hook 'powerline-center-theme)

;; set line numbers
(line-number-mode 1)

;;maxframe
;; Start fullscreen (cross-platf)
(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)



;; SHORTCUTS
;;; Winmove
(when (fboundp 'windmove-default-keybindings)
  (windmove-default-keybindings))
;;;; Make windmove work in Org mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)
;; org-agenda shortcut (C a t)
(global-set-key "\C-ca" 'org-agenda)
;; THEMES
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme `grandshell t)







;;;;;;;;;;PLUGINS;;;;;;;;;;;
;;;;;;;;;MELPA package Manager stuff;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require 'package)
(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
                    (not (gnutls-available-p))))
       (proto (if no-ssl "http" "https")))
  (when no-ssl
    (warn "\
Your version of Emacs does not support SSL connections,
which is unsafe because it allows man-in-the-middle attacks.
There are two things you can do about this warning:
1. Install an Emacs version that does support SSL and be safe.
2. Remove this warning from your init file so you won't see it again."))
  ;; Comment/uncomment these two lines to enable/disable MELPA and MELPA Stable as desired
  (add-to-list 'package-archives (cons "melpa" (concat proto "://melpa.org/packages/")) t)
  ;;(add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
  (when (< emacs-major-version 24)
    ;; For important compatibility libraries like cl-lib
    (add-to-list 'package-archives (cons "gnu" (concat proto "://elpa.gnu.org/packages/")))))
(package-initialize)


;;;;;;;;;;;;;;;;;;;;;EMACS MODES CONFIG;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



;; Enable Powershell
(require 'powershell)


;;;;Org mode configuration;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Enable Org mode
(require 'org)
;; Make Org mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
;; The above is the default in recent emacsen

(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

(setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("study" . ?l)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.milkbox.net/packages/") t)
(package-initialize)

(ido-mode 1)

;; Files belonging to org-mode Agenda
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(custom-enabled-themes (quote (grandshell)))
 '(custom-safe-themes
   (quote
    ("3860a842e0bf585df9e5785e06d600a86e8b605e5cc0b74320dfe667bcbe816c" "6d19d236838fd93e73c66718d91a4f6ffb57223e8d1c1fbd19879190b3b6f7fa" default)))
 '(org-agenda-files
   (quote
    ("c:/Users/jd/Dropbox/Files/emacsSetup.org" "c:/Users/jd/Dropbox/Files/thoughts.org" "c:/Users/jd/Dropbox/Files/portfolio.org")))
 '(package-selected-packages
   (quote
    (centaur-tabs helm-smex smex powerline grandshell-theme howdoi org-cliplink org-jira org-edna)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;;;;;;;;;;;;end of org-mode config;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;;;;;;;;;SMEX;;;;;;;;
  (require 'smex) ; Not needed if you use package.el
  (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
	            ; when Smex is auto-initialized on its first run.
;;Key Bindings;;
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands);; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

