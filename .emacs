(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))
(package-initialize)

(add-to-list 'default-frame-alist '(height . 48))
(add-to-list 'default-frame-alist '(width . 110))
(setq inhibit-splash-screen t)

(setq-default fill-column 80)

(require 'evil)
(evil-mode 0)

(require 'fill-column-indicator)
(setq fci-rule-column 81)
(setq column-number-mode t)

(require 'org)

(transient-mark-mode 1)

(global-set-key "\C-cl" 'org-store-link)
(global-set-key "\C-ca" 'org-agenda)
(global-set-key "\C-cc" 'org-capture)
(global-set-key "\C-cb" 'org-iswitchb)

(setq org-agenda-files (list "~/org/chris"))
(setq org-log-done t)
(setq org-enforce-todo-dependencies t)
(setq org-agenda-todo-ignore-timestamp 'future)
(setq org-agenda-skip-scheduled-if-done t)
(setq org-agenda-skip-deadline-if-done t)
(setq org-agenda-skip-scheduled-if-deadline-is-shown 'not-today)
(setq org-agenda-span 1)
(setq org-deadline-warning-days 7)

(defun my/org-sort-agenda-items-type-rank (a)
  (let* ((type (get-text-property 0 'type a)))
    (cond
      ((string= type "timestamp") 0)
      ((string= type "deadline") 1)
      ((string= type "upcoming-deadline") 2)
      ((string= type "past-scheduled") 3)
      ((string= type "scheduled") 4)
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
'((agenda time-up user-defined-down todo-state-down timestamp-up category-up)
   (todo priority-down category-up)
    (tags todo-state-up priority-down category-up)
    (search category-keep)))

(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "|" "DONE(d)" "WONT-DO")))

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
     ("v" "Items marked 'soon'" tags ":soon:"
      ((org-agenda-skip-function
	(quote
	 (org-agenda-skip-entry-if
	  (quote todo)
	  (quote done))))))))))
