;;; EMACS CONFIG ;;;

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

;; set line numbers
(line-number-mode 1)

;; reinit last session
(desktop-save-mode 1)
(savehist-mode 1)
(add-to-list 'savehist-additional-variables 'kill-ring) ;; for example

;; Start fullscreen (cross-platf)
;(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

;; remove top menubar
(menu-bar-mode -1)

;; Enable Powershell
(require 'powershell)

;; Enable Ido Mode
(ido-mode 1)


;;; AUTOSTART ;;;
(add-hook 'after-init-hook 'org-todo-list)
(add-hook 'after-init-hook 'powerline-center-theme)


;;; SHORTCUTS ;;;
;; org-agenda shortcut (C a t)
(global-set-key "\C-ca" 'org-agenda)



;;; THEMES ;;;
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes/")
(load-theme `grandshell t)

;;; PLUGINS ;;;
;;; MELPA Package Manager ;;;
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
      
;;; File Embedder ;;;
(defun org-insert-file (filename)
  "Insert Elisp code block recreating file named FILENAME."
  (interactive "f")
  (let ((base64-string
	 (with-temp-buffer
	   (insert-file-contents-literally filename)
	   (base64-encode-region (point-min) (point-max))
	   (buffer-string))))
	(insert (format "#+BEGIN_SRC emacs-lisp :results output silent\n  (with-temp-file %S\n    (insert (base64-decode-string\n      %S)))\n#+END_SRC" filename base64-string))))




;;; Org Mode Configuration ;;;
;; Enable Org mode
(require 'org)
;; Make Org mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

(setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("study" . ?l)))

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
    ("c:/Users/jd/Dropbox/Files/work.org" "c:/Users/jd/Dropbox/Files/study.org" "c:/Users/jd/Dropbox/Files/portfolio.org" "~/.org-jira/SC.org" "~/.org-jira/OIS.org" "~/.org-jira/ELSS.org")))
 '(package-selected-packages
   (quote
    (pcre2el org-super-agenda twittering-mode ## elscreen centaur-tabs helm-smex smex powerline grandshell-theme howdoi org-cliplink org-jira org-edna)))
 '(tool-bar-mode nil))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;;; REGEX Builder ;;;
(defun pcre-regexp-from-list-of-words (words)
  "insert a pcre regexp to match a list of words"
  (interactive "sList of words for regexp: ")
  (insert
   (pcre-to-elisp
    (regexp-opt (split-string words)))))
(global-set-key (kbd "C-c R") 'pcre-regexp-from-list-of-words)


;;; ORG-SUPER-AGENDA ;;;
(require 'org-super-agenda)
(org-super-agenda-mode t)


(setq spacemacs-theme-org-agenda-height nil
      org-agenda-time-grid '((daily today require-timed) "----------------------" nil)
      org-agenda-skip-scheduled-if-done t
      org-agenda-skip-deadline-if-done t
      org-agenda-include-deadlines t
      org-agenda-include-diary t
      org-agenda-block-separator nil
      org-agenda-compact-blocks t
      org-agenda-start-with-log-mode t)

(setq org-agenda-custom-commands
      '(("z" "Super zaen view"
         ((agenda "" ((org-agenda-span 'day)
                      (org-super-agenda-groups
                       '((:name "Today"
                                :time-grid t
                                :date today
                                :todo "TODAY"
                                :scheduled today
                                :order 1)))))
          (alltodo "" ((org-agenda-overriding-header "")
                       (org-super-agenda-groups
                        '(

			
			  
			  (:name "Study"
                                 :tag ("Study" "Daily")
                                 :order 1)

			  (:name "Overdue"
                                 :deadline past			      
                                 :order 2)			  

			  (:name "Due Today"
                                 :deadline today
                                 :order 3)
                          (:name "Due Soon"
                                 :deadline future
                                 :order 4)

			 
                          (:name "Personal Projects"
                                 :tag "Projects"
                                 :order 9)
                          (:name "Emacs"
                                 :tag "Emacs"
                                 :order 10)
                          (:name "To read"
                                 :tag "Read"
                                 :order 11)

					  
			   (:name "Work"
                                 :tag "work"
                                 :order 5)
			  
			  (:name "Orpheus_SC"
                                 :regexp "SC_"
                                 :order 6)
			  (:name "Orpheus_ELSS"
                                 :regexp "ELSS_"
                                 :order 7)
		          (:name "Orpheus_OIS"
                                 :regexp "OIS_"
                                 :order 8)

			  
			   (:name "Exam Dates"
				 :tag "ExamDates"
                                 :order 99)
			  
                          (:discard (:tag ("Chore" "Routine" "Daily")))))))))))


;;; SMEX ;;;
  (require 'smex) ; Not needed if you use package.el
  (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
	            ; when Smex is auto-initialized on its first run.
;;Key Bindings;;
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands);; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;;; elscreen ;;;
(elscreen-start)

;;; org-jira ;;;
(setq jiralib-url "https://orpheus.jira.com")
(require 'org-jira)


