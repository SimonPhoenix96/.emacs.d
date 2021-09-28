;; General Settings
(setq default-directory "~/.emacs.d/files")
(setq default-input-method "nil")

(require 'simpleclip)

(simpleclip-mode 1)

;; Makes *scratch* empty.
(setq initial-scratch-message "")

;; Removes *scratch* from buffer after the mode has been set.
(defun remove-scratch-buffer ()
  (if (get-buffer "*scratch*")
      (kill-buffer "*scratch*")))
(add-hook 'after-change-major-mode-hook 'remove-scratch-buffer)

;; Removes *messages* from the buffer.
(setq-default message-log-max nil)
(kill-buffer "*Messages*")

;; Removes *Completions* from buffer after you've opened a file.
(add-hook 'minibuffer-exit-hook
      '(lambda ()
         (let ((buffer "*Completions*"))
           (and (get-buffer buffer)
                (kill-buffer buffer)))))

;; Don't show *Buffer list* when opening multiple files at the same time.
(setq inhibit-startup-buffer-menu t)

;; Show only one active window when opening multiple files at the same time.
(add-hook 'window-setup-hook 'delete-other-windows)

;; Transparency ;; 
 ;;(set-frame-parameter (selected-frame) 'alpha '(<active> . <inactive>))
 ;;(set-frame-parameter (selected-frame) 'alpha <both>)
(set-frame-parameter (selected-frame) 'alpha '(95 . 100))
(add-to-list 'default-frame-alist '(alpha . (95 . 100)))


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

;; reinstall all packages, useful for migrating to another pc
(defun package-reinstall-all-activated-packages ()
  "Refresh and reinstall all activated packages."
  (interactive)
  (package-refresh-contents)
  (dolist (package-name package-activated-list)
    (when (package-installed-p package-name)
      (unless (ignore-errors                   ;some packages may fail to install
                (package-reinstall package-name))
        (warn "Package %s failed to reinstall" package-name)))))

;; prefer UTF-8
(prefer-coding-system 'utf-8)

;; Disable the splash screen (to enable it agin, replace the t with 0)
(setq inhibit-splash-screen 0)


;; Enable transient mark mode
(transient-mark-mode 1)

;; set line numbers
(line-number-mode 1)

;; reinit last session
;(desktop-save-mode 0)
;(savehist-mode 0)
;(add-to-list 'savehist-additional-variables 'kill-ring) ;; for example

;; Start fullscreen (cross-platf)
;(add-hook 'window-setup-hook 'toggle-frame-fullscreen t)

;; remove top menubar
(menu-bar-mode -1)

;; Enable Powershell
;; (require 'powershell)

;; Enable Ido Mode
(ido-mode 1)


;;; AUTOSTART ;;;
;(add-hook 'after-init-hook 'org-todo-list)
;(add-hook 'after-init-hook 'powerline-center-theme)
(add-hook 'after-init-hook (lambda () (org-agenda nil "z")))

;;; SHORTCUTS ;;;
;; org-agenda shortcut (C a t)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key (kbd "C-c <left>")  'windmove-left)
(global-set-key (kbd "C-c <right>") 'windmove-right)
(global-set-key (kbd "C-c <up>")    'windmove-up)
(global-set-key (kbd "C-c <down>")  'windmove-down)


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
  (add-to-list 'package-archives (cons "melpa-stable" (concat proto "://stable.melpa.org/packages/")) t)
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
R	   (insert-file-contents-literally filename)
	   (base64-encode-region (point-min) (point-max))
	   (buffer-string))))
	(insert (format "#+BEGIN_SRC emacs-lisp :results output silent\n  (with-temp-file %S\n    (insert (base64-decode-string\n      %S)))\n#+END_SRC" filename base64-string))))




;;; Org Mode Configuration ;;;
;; Enable Org mode
(require 'org)
;; Make Org mode work with files ending in .org
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))

(setq org-todo-keywords
  '((sequence "FUTURE" "TODO" "NEXT" "IN-PROGRESS" "WAITING" "DONE")))

(setq org-tag-alist '(("@work" . ?w) ("@home" . ?h) ("@study" . ?s)))

;; Files belonging to org-mode Agenda
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#2d3743" "#ff4242" "#74af68" "#dbdb95" "#34cae2" "#008b8b" "#00ede1" "#e1e1e0"])
 '(auth-source-save-behavior nil)
 '(custom-enabled-themes '(grandshell))
 '(custom-safe-themes
   '("3e335d794ed3030fefd0dbd7ff2d3555e29481fe4bbb0106ea11c660d6001767" "cc0dbb53a10215b696d391a90de635ba1699072745bf653b53774706999208e3" "3860a842e0bf585df9e5785e06d600a86e8b605e5cc0b74320dfe667bcbe816c" "039c01abb72985a21f4423dd480ddb998c57d665687786abd4e16c71128ef6ad" default))
 '(org-agenda-files '("~/.emacs.d/files/"))
 '(package-selected-packages
   '(minimal-theme leuven-theme docker simpleclip sudo-edit py-autopep8 jedi elpy powershell bug-hunter pcre2el org-super-agenda twittering-mode ## elscreen centaur-tabs powerline grandshell-theme howdoi org-cliplink org-jira org-edna))
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
  (interactive "List of words for regexp: ")
  (insert
   (pcre-to-elisp
    (regexp-opt (split-string words)))))
(global-set-key (kbd "C-c R") 'pcre-regexp-from-list-of-words)

;;; ORG-SUPER-AGENDA ;;;
(require 'org-super-agenda)
(org-super-agenda-mode t)


(setq org-agenda-include-deadlines t
      org-agenda-block-separator nil
      org-agenda-compact-blocks t)


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


			  (:name "Overdue"
                                 :deadline past			      
                                 :order 10)			  
			  (:name "Due Today"
                                 :deadline today
                                 :order 11)
			  (:name "Due Soon"
				 :tag "todo"
                                 :deadline future
                                 :order 12)

                          (:name "Projects"
                                 :tag "projects"
                                 :order 20)
                          (:name "To read"
                                 :tag "read"
                                 :order 21)
                          (:name "Games"
                                 :tag "games"
                                 :order 22)
					  
			  (:name "Work"
                                 :tag "work"
                                 :order 30)

			  (:name "Exam Dates"
				 :tag "ExamDates"
                                 :order 99)
			  
                          (:discard (:tag ("weekly")))))))))))




;;; SMEX ;;;
;  (require 'smex) ; Not needed if you use package.el
;  (smex-initialize) ; Can be omitted. This might cause a (minimal) delay
	            ; when Smex is auto-initialized on its first run.
;;Key Bindings;;
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands);; This is your old M-x.
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)


;;; elscreen ;;;
(elscreen-start)

;;; org-jira ;;;
;(setq jiralib-url "https://orpheus.jira.com")
;(require 'org-jira)
;(setq org-jira-working-dir "~/.emacs.d/files/org-jira")

;;; elpy ;;:
(elpy-enable)
;;;;
