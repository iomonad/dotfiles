;;; init.el --- Work emacs config -*- lexical-binding: t -*-

;; Copyright (c) 2014-2021 Clement T.

;; Author: Clement T. <clement@trosa.io>
;; Maintainer: Clement T. <clement@trosa.io>
;; URL: https://github.com/iomonad/dotfiles
;; Created: January 2014
;; Keywords: config
;; Version: 0.3.1

;; This file is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.

;; This file is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs; see the file COPYING.  If not, write to
;; the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor,
;; Boston, MA 02110-1301, USA.

;;; Commentary:

;; iomonad's config file
;; Key points:
;;  - Global completion mode, backed by company
;;  - C/C++/Rust mode with clang linter and completion engine
;;  - Optimised for dvorak layout (w/ Space Cadet shift modifier)
;;  - Linum mode with hacked side-bar
;;  - Magit for git porcelain
;;  - Improved UX With Undotree & SMEX
;;  - Clojure CIDER mode

;;; Changelog:

;; 0.1.1: Config backup init.
;; 0.1.2: Cleanup unused packages (ensime due to Intellij Usage)
;;        and theme bump.
;; 0.1.3: Created common configuration for both desktop & Servers
;; 0.1.4: Backed up configuration from work.
;; 0.1.5: - Code cleanup
;;        - Removed useless modes.
;;        - Improved Org mode configuration
;;        - Added custom set config by editor in different file,
;;          to avoid auto append in this file.
;;        - Switched to Ibuffer Vanilla buffer management.
;; 0.1.6: Added Mode for Yaml, Markdown and Ansible.
;; 0.1.7: Improved defaults, fixed broken packages & improved
;;        cursor visibility with global-line.
;; 0.1.8: Added small hack to set mail mod when mutt open
;;        a new mail buffer
;; 0.1.9: Minor mode additions and added special theme for graphical
;;        mode only.
;; 0.2.0: New year update. This year will be impacted by productivity need
;;        and GDT. Some minor cleanup, and `org-roam adition`.
;; 0.2.1: Org mode cleanup, using it only for roam note taking.
;; 0.3.0: New job configuration update. Mainly clojure and Gnus so
;;        ride of superflue plugins & added projectile support.
;; 0.3.1: Added paredit in lisp modes and raindbow-delimiters mainly
;;        for eye-candy. Also made minor updates in org-mode since
;;        I decided to leave Todoist plateform for work.
;;        Also added Elfeed support. Yes, my life
;;        become more and more in that text editor ...
;;

;;; --------------------------------------------------------------------------
;;; Code:
;;; --------------------------------------------------------------------------


(require 'org)
(require 'linum)
(require 'package)
(require 'whitespace)


;;; --------------------------------------------------------------------------
;;; Package and Melpa configuration:
;;; --------------------------------------------------------------------------


(setq package-user-dir  (expand-file-name
			 (convert-standard-filename "packages")
			 user-emacs-directory)
      package-enable-at-startup nil
      package-archives '(("melpa"      . "http://melpa.org/packages/")
			 ("gnu"        . "http://elpa.gnu.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")))

(let* ((no-ssl (and (memq system-type '(windows-nt ms-dos))
		    (not (gnutls-available-p))))
       (url (concat (if no-ssl "http" "https") "://melpa.org/packages/")))
  (add-to-list 'package-archives (cons "melpa" url) t))

(when (< emacs-major-version 24)
  (add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/")))
(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))
(unless (package-installed-p 'use-package)
  (package-install 'use-package))
(eval-when-compile
  (require 'use-package))


;;; --------------------------------------------------------------------------
;;; Vanilla emacs configuration:
;;; --------------------------------------------------------------------------


(defalias 'yes-or-no-p 'y-or-n-p) ; fck off

(setq debug-on-error t
      gc-cons-threshold 100000000
      load-prefer-newer t
      sentence-end-double-space nil
      frame-title-format "Emacs - %b"
      time-stamp-format "%:y-%02m-%02d %02H:%02M:%02S"
      inhibit-startup-message t
      inhibit-splash-screen t
      case-fold-search t
      kill-whole-line t
      require-final-newline t
      next-line-add-newlines nil
      backup-inhibited t
      make-backup-files nil
      auto-save-default nil
      auto-save-list-file-prefix nil
      vc-follow-symlinks t
      password-cache-expiry nil
      highlight-tabs t
      show-trailing-whitespace t
      whitespace-line-column 80
      whitespace-style '(tabs tab-mark face lines-tail)
      whitespace-global-modes '(not org-mode web-mode)
      uniquify-buffer-name-style 'forward uniquify-separator "/"
      backup-directory-alist `(("." . "~/.emacs.d/saves"))
      make-backup-files 1
      auto-save-list-file-prefix nil
      auto-save-default t
      custom-file "~/.emacs.d/custom.el"
      debug-on-error nil
      debug-on-quit nil
      debug-on-message nil
      vc-follow-symlinks t
      show-trailing-whitespace t
      compilation-scroll-output t
      dired-dwim-target t)

(electric-pair-mode 1)			; Can be annoying
(setq create-lockfiles nil)
(setq show-paren-delay 0)
(setq blink-matching-paren 1)
(show-paren-mode 1)

(tool-bar-mode -1) 			; We are in term mode right ?
(menu-bar-mode -1)

(add-hook 'after-init-hook 'global-hl-line-mode)
(set-face-attribute 'highlight nil :background "black" :foreground 'unspecified)

;; Date
(setq display-time-24hr-format t)
(display-time-mode +1)

;; Init Hook
(setq initial-major-mode 'org-mode)
(setq initial-scratch-message nil)


;;; --------------------------------------------------------------------------
;;; Ibuffer
;;; --------------------------------------------------------------------------


(global-set-key (kbd "C-x b") 'ibuffer)

(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("dired" (mode . dired-mode))
	       ("org" (name . "^.*org$"))

	       ("web" (or (mode . web-mode) (mode . js2-mode)))
	       ("shell" (or (mode . eshell-mode) (mode . shell-mode)))
	       ("programming" (or
			       (mode . python-mode)
			       (mode . c++-mode)
			       (mode . shell-mode)))
	       ("GIT" (or
		       (mode . magit-mode)
		       (name . "magit*")))
	       ("Ansible" (or
			   (mode . ansible-mode)
			   (name . "*\/ansible\/*")))
	       ("clojure" (or
			   (mode . clojure-mode)
			   (mode . cider-mode)
			   (name . "*cider*")))
	       ("planner" (or
			   (name . "^\\*Calendar\\*$")
			   (name . "^diary$")
			   (mode . muse-mode)))
	       ("IRC" (mode . erc-mode))
	       ("gnus" (or
			(mode . message-mode)
			(mode . bbdb-mode)
			(mode . mail-mode)
			(mode . gnus-group-mode)
			(mode . gnus-summary-mode)
			(mode . gnus-article-mode)
			(name . "^\\.bbdb$")
			(name . "^\\.newsrc-dribble")))
	       ("emacs" (or
			 (mode . emacs-elisp-mode)
			 (name . "^\\*scratch\\*$")
			 (name . "*init.el")
			 (name . "^\\*Messages\\*$")))))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-auto-mode 1)
	    (ibuffer-switch-to-saved-filter-groups "default")))

;; Don't show filter groups if there are no buffers in that group
(setq ibuffer-show-empty-filter-groups nil)
;; Don't ask for confirmation to delete marked buffers
(setq ibuffer-expert t)


;;; --------------------------------------------------------------------------
;;; UI configuration:
;;; --------------------------------------------------------------------------


(display-time-mode)
(column-number-mode)
(line-number-mode)
(set-face-background 'vertical-border "black")
(set-face-foreground 'vertical-border (face-background 'vertical-border))
(setq scroll-step            2 ; Scrolling suxx
      scroll-conservatively 10000)


;;; --------------------------------------------------------------------------
;;; Themes
;;; --------------------------------------------------------------------------


(let ((basedir "~/.emacs.d/themes/"))
  (dolist (f (directory-files basedir))
    (if (and (not (or (equal f ".") (equal f "..")))
	     (file-directory-p (concat basedir f)))
	(add-to-list 'custom-theme-load-path (concat basedir f)))))
(add-to-list 'custom-theme-load-path "~/.emacs.d/themes") ; custom path

(if (display-graphic-p) 		; Set theme only on gui mode
    (load-theme 'distinguished t))

(set-face-foreground 'mode-line "white")
(set-face-background 'mode-line "#000000")
(set-face-foreground 'mode-line-inactive "white")
(set-face-background 'mode-line-inactive "#120f0f")


;;; --------------------------------------------------------------------------
;;; Fonts
;;; --------------------------------------------------------------------------

(if (display-graphic-p)
    (set-face-attribute
     'default nil :family "Ubuntu Mono"
                  :height 120
		  :weight 'normal
		  :width 'normal)
  (when (functionp 'set-fontset-font)
    (set-fontset-font "fontset-default"
                    'unicode
                    (font-spec :family "DejaVu Sans Mono"
                               :width 'normal
                               :size 15.5
                               :weight 'normal))))

;;; --------------------------------------------------------------------------
;;; Linum configuration:
;;; --------------------------------------------------------------------------


(set-face-attribute 'linum nil
                    :background "black")

(defface linum-current-line-face
  `((t :background "black"
       :foreground "white"))
  "Face for the currently active Line number")

(defvar my-linum-current-line-number 0)

(defun get-linum-format-string ()
  (setq-local my-linum-format-string
              (let ((w (length (number-to-string
                                (count-lines (point-min) (point-max))))))
                (concat " %" (number-to-string w) "d "))))

(add-hook 'linum-before-numbering-hook 'get-linum-format-string)

(defun my-linum-format (line-number)
  (propertize (format my-linum-format-string line-number) 'face
              (if (eq line-number my-linum-current-line-number)
                  'linum-current-line-face
                'linum)))

(setq linum-format 'my-linum-format)

(defadvice linum-update (around my-linum-update)
  (let ((my-linum-current-line-number (line-number-at-pos)))
    ad-do-it))

(ad-activate 'linum-update)

(global-linum-mode) ; Set global mode


;;; --------------------------------------------------------------------------
;;; Whitespace remover hook:
;;; --------------------------------------------------------------------------


(setq whitespace-style '(face empty lines-tail trailing))
(global-whitespace-mode t)

;; Note: Added to the  C mode only ! Must be added to 'before-save-hook
;; to have a global effect

(add-hook 'c-mode-hook
	  (lambda ()
	    (add-to-list 'write-file-functions
			 'delete-trailing-whitespace)))


;;; --------------------------------------------------------------------------
;;; Org Mode
;;; --------------------------------------------------------------------------


(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-cc" 'org-capture)
(define-key global-map "\C-ca" 'org-agenda)

(setq org-directory "~/Org"
      org-todo-keywords
      '((sequence "TODO(t)"
		  "WAIT(w@/!)"
		  "ONGOING"
		  "|"
		  "DONE(d!)"
		  "DELEGATED"
		  "CANCELED(c@)")))

(setq org-highest-priority ?A)
(setq org-default-priority ?C)
(setq org-lowest-priority ?E)

(setq org-todo-keyword-faces
      '(("TODO" . (:foreground "GoldenRod" :weight bold))
	("WAIT" . (:foreground "IndianRed1" :weight bold))
	("ONGOING" . (:foreground "OrangeRed" :weight bold))
	("CANCELED" . (:foreground "LimeGreen" :weight bold))
	("DELEGATED" . (:foreground "LimeGreen" :weight bold))))

(setq org-tag-persistent-alist
      '((:startgroup . nil)
	("HOME" . ?h)
	("RESEARCH" . ?r)
	("PERSO" . ?c)
	(:endgroup . nil)
	(:startgroup . nil)
	("OS" . ?o)
	("DEV" . ?d)
	("WWW" . ?w)
	(:endgroup . nil)
	(:startgroup . nil)
	("EASY" . ?e)
	("MEDIUM" . ?m)
	("HARD" . ?a)
	(:endgroup . nil)
	("URGENT" . ?u)
	("KEY" . ?k)
	("BONUS" . ?b)
	("noexport" . ?x)))

(setq org-tag-faces
      '(
	("HOME" . (:foreground "GoldenRod" :weight bold))
	("RESEARCH" . (:foreground "GoldenRod" :weight bold))
	("PERSO" . (:foreground "GoldenRod" :weight bold))
	("OS" . (:foreground "IndianRed1" :weight bold))
	("DEV" . (:foreground "IndianRed1" :weight bold))
	("WWW" . (:foreground "IndianRed1" :weight bold))
	("URGENT" . (:foreground "Red" :weight bold))
	("KEY" . (:foreground "Red" :weight bold))
	("EASY" . (:foreground "OrangeRed" :weight bold))
	("MEDIUM" . (:foreground "OrangeRed" :weight bold))
	("HARD" . (:foreground "OrangeRed" :weight bold))
	("BONUS" . (:foreground "GoldenRod" :weight bold))
	("noexport" . (:foreground "LimeGreen" :weight bold))))

(setq org-agenda-dim-blocked-tasks nil
      org-agenda-compact-blocks t
      org-log-done t
      org-startup-truncated nil
      org-use-speed-commands t
      org-use-fast-todo-selection t
      org-fast-tag-selection-single-key t
      org-fast-tag-selection-include-todo t)


;; Load org buffer

(setq org-agenda-files (list "~/Org/agenda.org"))

;;; --------------------------------------------------------------------------
;;; Functions
;;; --------------------------------------------------------------------------


(defun save-all-buffers ()
  (interactive) ; Disable prompt
  (save-some-buffers t))

(defun indent-buffer ()
  "Indent the whole buffer."
  (interactive)
  (indent-region (point-min)
		 (point-max)))

(defun insert-date ()
  "Insert today's date at point"
  (interactive "*")
  (insert (format-time-string "%F")))


;;; --------------------------------------------------------------------------
;;; Custom keybinds
;;; --------------------------------------------------------------------------


;; Core Interactions
(global-set-key (kbd "C-x s") 'save-all-buffers)
(global-set-key (kbd "C-c n") 'indent-buffer)

;; Gnus / Mails
(global-set-key (kbd "C-x m") 'gnus)


;;; --------------------------------------------------------------------------
;;; Hooks
;;; --------------------------------------------------------------------------


;; Save buffer on focus out
(add-hook 'focus-out-hook 'save-all-buffers)

;; On save, remove trailing whitespaces
(add-hook 'write-file-hooks 'delete-trailing-whitespace nil t)


;;; --------------------------------------------------------------------------
;;; Extra-Packages configurations:
;;; --------------------------------------------------------------------------


;; Company completion engine:

(use-package company :ensure t
  :init (setq company-auto-complete nil
              company-tooltip-flip-when-above t
              company-minimum-prefix-length 2
              company-tooltip-limit 20
              company-idle-delay 0.5)
  :config (global-company-mode 1) ; Set mode on every buffers
  :diminish company-mode)

(setq company-backends (delete 'company-semantic company-backends))
(add-to-list 'company-backends 'company-clang)

;; Yasnippet mode

(use-package yasnippet
  :ensure t)
(use-package yasnippet-snippets
  :ensure t)

(setq yas-snippet-dirs
      '("~/.emacs.d/snippets"))

(add-hook 'prog-mode-hook #'yas-minor-mode)

;; Magit & Git HL

(use-package magit
  :ensure t)
(use-package diff-hl
  :ensure t)

(custom-set-faces			; Highlight are too much imo
 '(magit-diff-added ((t (:background "color-16" :foreground "#22aa22"))))
 '(magit-diff-added-highlight ((t (:background "color-16" :foreground "#22aa22"))))
 '(magit-diff-context-highlight ((t (:background "color-16" :foreground "grey50"))))
 '(magit-diff-hunk-heading-highlight ((t (:background "color-243" :foreground "grey30"))))
 '(magit-diff-removed-highlight ((t (:background "color-16" :foreground "#aa2222")))))

(add-hook 'magit-pre-refresh-hook 'diff-hl-magit-pre-refresh)
(add-hook 'magit-post-refresh-hook 'diff-hl-magit-post-refresh)

(global-diff-hl-mode)

;; Undotree

(use-package undo-tree
  :ensure t)

(global-undo-tree-mode)

;; Neotree

(use-package neotree
  :ensure t)

(global-set-key (kbd "C-x t") 'neotree-toggle)
(setq neo-theme 'arrow)

(custom-set-faces
 '(neo-root-dir-face ((t (:foreground "#dddfdb"))))
 '(neo-dir-link-face ((t (:foreground "#776e61"))))
 '(neo-file-link-face ((t (:foreground "#BA36A5")))))

;; SMEX

(use-package smex
  :ensure t)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)

;;
;; C/C++ Mode
;;

(add-hook 'c-mode-common-hook
          (lambda ()
            ;; Add kernel style
            (c-add-style
             "linux-tabs-only"
             '("linux" (c-offsets-alist
                        (arglist-cont-nonempty
                         c-lineup-gcc-asm-reg
                         c-lineup-arglist-tabs-only))))))

(setq c-default-style "linux"
      c-basic-offset 8)

;; Syntax Checking

(use-package flycheck
  :ensure t
  :init (global-flycheck-mode))

(use-package dap-mode
  :ensure t
  :hook
  (lsp-mode . dap-mode)
  (lsp-mode . dap-ui-mode))

;; Projectile

(use-package projectile
  :ensure t)

(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)

;; Clojure

(use-package cider
  :ensure t)

(setq cider-prompt-for-symbol nil
      nrepl-log-messages nil
      nrepl-hide-special-buffers t
      cider-prefer-local-resources t
      cider-save-file-on-load t
      cider-repl-display-help-banner nil
      cider-eval-result-prefix ";; REPL => ")

(setq cider-repl-history-file 		; History in file
      (expand-file-name "~/.emacs.d/.cider-repl-history"))

(use-package rainbow-delimiters
  :ensure t)

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(use-package paredit
  :ensure t)

(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)
(add-hook 'clojure-mode-hook          #'enable-paredit-mode)

;;; Elfeed

(use-package elfeed
  :defer t
  :bind ("C-x w" . elfeed)
  :init (setf url-queue-timeout 30)
  :config
  (require 'feed-setup)
  (push "-k" elfeed-curl-extra-arguments)
  (setf bookmark-default-file (locate-user-emacs-file "local/bookmarks")))


;; Markdown

(use-package markdown-mode
  :ensure t
  :mode (("README\\.md\\'" . gfm-mode)
	 ("\\.md\\'" . markdown-mode)
	 ("\\.markdown\\'" . markdown-mode))
  :init (setq markdown-command "multimarkdown"))

;; Ansible

(use-package yaml-mode
  :ensure t)
(use-package ansible
  :ensure t)

(add-hook 'yaml-mode-hook '(lambda () (ansible 1)))
(put 'downcase-region 'disabled nil)


;;; --------------------------------------------------------------------------
;;; Load autogenerated tweaks
;;; --------------------------------------------------------------------------


(let ((cst-cfg "~/.emacs.d/custom.el"))
  (if (file-exists-p cst-cfg)
      (load cst-cfg)))


(provide 'init) ;;; init.el ends here
