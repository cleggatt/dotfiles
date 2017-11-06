(setq package-list '(package evil fill-column-indicator org))
(setq package-archives '(("melpa" . "http://melpa.org/packages/")))
(package-initialize)

(or (file-exists-p package-user-dir)
    (package-refresh-contents))
(dolist (package package-list)
  (unless (package-installed-p package)
    (package-install package)))

(require 'package)

(add-to-list 'default-frame-alist '(height . 48))
(add-to-list 'default-frame-alist '(width . 110))

(setq inhibit-splash-screen t)
(setq mouse-autoselect-window t)
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1)))
(setq mouse-wheel-progressive-speed nil)
(setq mouse-wheel-follow-mouse 't) 
(setq-default fill-column 80)

;(require 'helm-config)

(require 'evil)
(evil-mode 0)

(require 'fill-column-indicator)
(setq fci-rule-column 81)
(setq column-number-mode t)

(setq ispell-program-name "/usr/local/bin/aspell")
(setq ispell-local-dictionary "en_AU") 

(require 'org)

(transient-mark-mode 1)

(global-set-key (kbd "<home>") 'move-beginning-of-line)
(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-agenda-files (list "~/org/atl" "~/org/chris"))
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-deadline-is-shown 'not-today)
(setq org-agenda-span 1)
(setq org-agenda-todo-ignore-timestamp 'future)
(setq org-deadline-warning-days 7)
(setq org-enforce-todo-dependencies t)
(setq org-log-done t)
(setq org-refile-targets '((org-agenda-files :maxlevel . 2)))
(setq org-refile-use-outline-path 'file)
(setq org-use-speed-commands t)

(setq org-highest-priority ?A)
(setq org-lowest-priority ?C)
(setq org-default-priority ?B)

(defun my/org-sort-agenda-items-type-rank (a)
  (let* ((type (get-text-property 0 'type a)))
    (cond
      ;; ((string= type "timestamp") 0)
      ((string= type "deadline") 1)
      ((string= type "upcoming-deadline") 2)
      ((string= type "past-scheduled") 3)
      ;; TEMP: Change to group all scheduled together so we can sort by TODO status first
      ((string= type "scheduled") 3)
      ;; Anything else
      (t 5))))

(defun my/org-sort-agenda-items-custom-rank (a)
  (let* ((rank (my/org-sort-agenda-items-type-rank a))
         (todo-status (get-text-property 0 'todo-state a)))
    (cond
      ((string= todo-status "WAITING") (+ 10 rank))
      (t (* rank 10)))))

(defun my/org-sort-agenda-cmp (a b)
  (cond
    ((> a b) -1)
    ((< a b) +1)
    ;; Equal
    (t nil)))

(defun my/org-sort-agenda-items-user-defined (a b)
  (let* ((a-rank (my/org-sort-agenda-items-custom-rank a))
         (b-rank (my/org-sort-agenda-items-custom-rank b)))
    (my/org-sort-agenda-cmp a-rank b-rank)))

(setq org-agenda-cmp-user-defined 'my/org-sort-agenda-items-user-defined)
(setq org-agenda-sorting-strategy
'((agenda time-up user-defined-down priority-down todo-state-up timestamp-up category-up)
   (todo priority-down category-up)
    (tags todo-state-up priority-down category-up)
    (search category-keep)))

(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE" "TRACKED(k)" "WONT-DO(o)")))

(add-hook 'org-mode-hook 'turn-on-flyspell)
(add-hook 'after-init-hook 'org-agenda-list)
(add-hook 'emacs-startup-hook 'delete-other-windows)
(add-hook 'after-change-major-mode-hook 'fci-mode)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-custom-commands
   (quote
    (("n" "Agenda and all TODOs"
      ((agenda "" nil)
       (alltodo "" nil))
      nil)
     ("z" "Yesterday" tags "CLOSED>=\"<yesterday>\"|TODO=\"IN-PROGRESS\"|TODO=\"WAITING\"" nil)
     ("x" "Friday" tags "CLOSED>=\"<-3d>\"|TODO=\"IN-PROGRESS\"|TODO=\"WAITING\"" nil)
     ("c" "Standup announcements" tags ":standup:"
      ((org-agenda-skip-function
	(quote
	 (org-agenda-skip-entry-if
	  (quote todo)
	  (quote done))))))
     ("v" "Items marked 'soon'" tags "PRIORITY=\"A\""
      ((org-agenda-skip-function
	(quote
	 (org-agenda-skip-entry-if
	  (quote todo)
	  (quote done))))))))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
