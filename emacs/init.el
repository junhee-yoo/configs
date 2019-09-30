;; refer: https://github.com/shiren/dotfiles/blob/master/emacs.d/init.el

;; 에러시 디버그모드
;; (setq debug-on-error t)

(when window-system
	(menu-bar-mode -1)
	(tool-bar-mode -1)
	(scroll-bar-mode -1)
	(tooltip-mode -1))

(setq inhibit-startup-message t)
(setq initial-scratch-message "")

(setq ad-redefinition-action 'accept) ;; 함수 redefine으로 인한 경고 생략

(set-language-environment "Korean")
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

(setq mac-command-key-is-meta t)
(setq mac-command-modifier 'meta)

(setq echo-keystrokes 0.001) ;; 키입력시 에코창에 표시되는 딜레이 타임, 거이 없게 설정

(setq tab-width 2)

;; 이맥스르 투명하게 하려면 숫자 조절
(set-frame-parameter nil 'alpha 0.85)

(set-variable 'cursor-type 'bar)


;; Paste setup
(defun copy-from-osx ()
	"Copy from osx."
	(shell-command-to-string "pbpaste"))

(defun paste-to-osx (text &optional push)
	(let ((process-connection-type nil))
		(let ((proc (start-process "pbcopy" "*Messages*" "pbcopy")))
			(process-send-string proc text)
			(process-send-eof proc))))

(unless window-system
	(setq interprogram-cut-function 'paste-to-osx)
	(setq interprogram-paste-function 'copy-from-osx))


;; Scroll setup
(setq mouse-wheel-scroll-amount '(1 ((shift) . 1))) ;; one line at a time
(setq mouse-wheel-progressive-speed nil) ;; don't accelerate scrolling
(setq mouse-wheel-follow-mouse 't) ;; scroll window under mouse

(setq scroll-conservatively 200) ;; 스크롤 도중에 센터로 커서 이동하지 않도록
(setq scroll-margin 3) ;; 스크롤시 남기는 여백


;; 백업들 끄기
(setq backup-inhibited t)
(setq make-backup-files nil)
(setq auto-save-default nil)

;; No popup frame(새버퍼열때 현재 프레임에서 열기)
(setq ns-pop-up-frames nil)
(setq pop-up-frames nil)

;; buffer-menu 대신 ibuffer 쓰기
(global-set-key (kbd "C-x C-b") 'ibuffer)
; Ensure ibuffer opens with point at the current buffer's entry.
(defadvice ibuffer
	(around ibuffer-point-to-most-recent) ()
	"Open ibuffer with cursor pointed to most recent buffer name."
	(let ((recent-buffer-name (buffer-name)))
		ad-do-it
		(ibuffer-jump-to-buffer recent-buffer-name)))
(ad-activate 'ibuffer)

;; Set up package
(require 'package)
(add-to-list 'package-archives '("gnu" . "http://elpa.gnu.org/packages/") t)
; (add-to-list 'package-archives '("marmalade" . "http://marmalade-repo.org/packages/") t)
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(eval-when-compile
	(require 'use-package))

(use-package use-package-ensure-system-package
	:ensure t)

(use-package use-package-chords
	:ensure t
	:config (key-chord-mode 1))

(use-package diminish
	:ensure t)

(use-package whitespace-cleanup-mode
	:ensure t
	:diminish whitespace-cleanup-mode
	:delight '(:eval "")
	:init
	(setq whitespace-cleanup-mode-only-if-initially-clean nil)
	(add-hook 'prog-mode-hook 'whitespace-cleanup-mode)
	(add-hook 'lsp-mode-hook 'whitespace-cleanup-mode))
	;;(add-hook 'org-mode-hook 'whitespace-cleanup-mode))


;;; Emacs extend
(use-package which-key
	:ensure t
	:diminish which-key-mode
	:init
	(setq which-key-idle-delay 2)
	(setq which-key-max-description-length 40)
	(setq which-key-max-display-columns nil)
	(which-key-setup-side-window-bottom)
	(which-key-mode))

(use-package dashboard
	:ensure t
	:config
	(dashboard-setup-startup-hook)
	(setq dashboard-items '((recents  . 20)
				(bookmarks . 10)
				(projects . 10))))

(use-package helpful
	:ensure t
	:bind
	("C-h f" . helpful-function)
	("C-h F" . helpful-command)
	("C-h v" . helpful-variable))

(use-package exec-path-from-shell
	:ensure t
	:init
	(when (memq window-system '(mac ns))
		(exec-path-from-shell-initialize)))


;;; Themes
(use-package zenburn-theme
	:disabled
	:ensure t
	:init
	(load-theme 'zenburn t))

(use-package spacemacs-theme
	:ensure t
	:defer t
	:init
	(load-theme 'spacemacs-dark t)
	:config
	(setq spacemacs-theme-org-agenda-height nil)
	(setq spacemacs-theme-org-height nil))

(use-package spaceline-config
	:ensure spaceline
	:init
	(setq powerline-default-separator 'arrow-fade)
	:config
	(spaceline-emacs-theme)
	(spaceline-toggle-buffer-id-on)
	(spaceline-toggle-input-method-on)
	(spaceline-toggle-buffer-modified-on)
	(spaceline-toggle-buffer-encoding-on)
	(spaceline-toggle-buffer-encoding-abbrev-off)
	(spaceline-toggle-process-on)
	(spaceline-toggle-projectile-root-on)
	(spaceline-toggle-version-control-on)
	(spaceline-toggle-flycheck-error-on)
	(spaceline-toggle-flycheck-info-on)
	(spaceline-toggle-flycheck-warning-on)
	(spaceline-toggle-battery-on)
	(spaceline-toggle-major-mode-off)
	(spaceline-toggle-minor-modes-on)
	(spaceline-toggle-line-column-on)
	(spaceline-toggle-org-clock-on)
	(spaceline-toggle-window-number-on)
	(spaceline-info-mode))


;;; Highlighting
(use-package paren
	:init
	(show-paren-mode 1)
	(setq show-paren-delay 0))

(use-package hl-line
	:init
	(global-hl-line-mode +1))

(use-package highlight-thing
	:ensure t
	:diminish highlight-thing-mode
	:init
	(setq highlight-thing-case-sensitive-p t)
	(setq highlight-thing-limit-to-defun t)
	(add-hook 'prog-mode-hook 'highlight-thing-mode))

(use-package rainbow-mode
	:ensure t)

(use-package rainbow-delimiters
	:ensure t
	:init
	(add-hook 'emacs-lisp-mode-hook 'rainbow-delimiters-mode))

(use-package highlight-indent-guides
	:ensure t
	:disabled
	:init
	:config
	(add-hook 'vue-html-mode-hook 'highlight-indent-guides-mode)
	(add-hook 'vue-mode-hook 'highlight-indent-guides-mode)
	(add-hook 'prog-mode-hook 'highlight-indent-guides-mode))

(use-package git-gutter
	:ensure t
	:diminish git-gutter-mode
	:init
	(global-git-gutter-mode +1))


;;; Window
(use-package eyebrowse
	:ensure t
	:init
	(setq eyebrowse-keymap-prefix (kbd "C-j <SPC>"))
	(eyebrowse-mode t)
	:bind
	(:map eyebrowse-mode-map
		("C-j ;" . eyebrowse-last-window-config)
		("C-j 0" . eyebrowse-close-window-config)
		("C-j 1" . eyebrowse-switch-to-window-config-1)
		("C-j 2" . eyebrowse-switch-to-window-config-2)
		("C-j 3" . eyebrowse-switch-to-window-config-3)))

(use-package ace-window
	:ensure t
	:config
	(setq aw-keys '(?1 ?2 ?3 ?4 ?5))
	:bind ("C-x o" . ace-window))

(use-package writeroom-mode
	:diminish writeroom-mode
	:ensure t
	:init
	:config)

(use-package zoom
	:ensure t
	:init
	:config)


;; windmove
(windmove-default-keybindings)
; Make windmove work in org-mode:
(add-hook 'org-shiftup-final-hook 'windmove-up)
(add-hook 'org-shiftleft-final-hook 'windmove-left)
(add-hook 'org-shiftdown-final-hook 'windmove-down)
(add-hook 'org-shiftright-final-hook 'windmove-right)


(use-package org-bullets
	:ensure t
	:init
	(add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package multi-term
	:ensure t
	:init
	(setq multi-term-program "/bin/zsh")
	:bind
	("C-c i" . multi-term))

;; terminal(멀티텀포함)에서 C-j를 글로벌 맵이용하도록 훅
(add-hook 'term-mode-hook
		(lambda ()
	(define-key term-raw-map (kbd "C-j")
	(lookup-key (current-global-map) (kbd "C-j")))))

;; Helm config
(use-package helm
	:ensure t
	:diminish helm-mode
	:bind (("C-c h" . helm-mini)
				 ("C-h a" . helm-apropos)
				 ;; ("C-x C-b" . helm-buffers-list)
				 ;; ("C-x b" . helm-buffers-list)
				 ("M-y" . helm-show-kill-ring)
				 ("M-x" . helm-M-x)
				 ("C-x c o" . helm-occur)
				 ("C-x c s" . helm-swoop)
				 ("C-x c y" . helm-yas-complete)
				 ("C-x c Y" . helm-yas-create-snippet-on-region)
				 ("C-x c b" . my/helm-do-grep-book-notes)
				 ("C-x c SPC" . helm-all-mark-rings)
				 ("C-x C-o" . ffap))
	:init
	(require 'helm-config)
	(setq helm-candidate-number-limit 100)
	(setq helm-yas-display-key-on-candidate t)
	;; for pretty fast updates when hitting RET too quickly
	;; after typing fast:
	(setq helm-idle-delay 0.0 ; update fast sources immediately (doesn't).
				helm-input-idle-delay 0.01  ; this actually updates things
					; reeeelatively quickly.
				helm-quick-update t
				helm-M-x-requires-pattern nil
				helm-ff-skip-boring-files t)
	:config
	(use-package helm-descbinds
		:ensure t
		:config (helm-descbinds-mode)))


;; GPG password in minibuffer
(use-package pinentry
	:init  ; with ~/.gnupg/gpg-agent.conf containing: 'allow-emacs-pinentry'
	(setq epa-pinentry-mode 'loopback))
; start pinentry server in emacs
(pinentry-start)

;; magit
; configure forge to integrate magit with github
(use-package forge
	;	init:
	; (setq magithub-github-hosts '("github.com" "git.linecorp.com" "oss.navercorp.com"))
	:config
	(add-to-list 'forge-alist '("git.linecorp.com" "git.linecorp.com/api"
															"git.linecorp.com" forge-gitlab-repository))
	(add-to-list 'forge-alist '("oss.navercorp.com" "oss.navercorp.com/api"
															"oss.navercorp.com" forge-gitlab-repository))
	:after magit)
;; END_OF_MAGIT


;; LSP_MODE
(use-package lsp-mode
  :hook (XXX-mode . lsp-deferred)
  :commands (lsp lsp-deferred))

; optionally
(use-package lsp-ui :commands lsp-ui-mode)
(use-package company-lsp :commands company-lsp)
(use-package helm-lsp :commands helm-lsp-workspace-symbol)
(use-package lsp-treemacs :commands lsp-treemacs-errors-list)
; optionally if you want to use debugger
(use-package dap-mode)
; (use-package dap-LANGUAGE) to load the dap adapter for your language

;; (with-eval-after-load 'lsp-mode
;;      (require 'lsp-clangd)
;;      (add-hook 'c-mode-hook #'lsp-clangd-c-enable)
;;      (add-hook 'c++-mode-hook #'lsp-clangd-c++-enable)
;;      (add-hook 'objc-mode-hook #'lsp-clangd-objc-enable))
;; END_OF_LSP_MODE



(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(eyebrowse-mode t)
 '(org-agenda-files (quote ("~/workspace/projects/finup/2019-09-23.org")))
 '(package-selected-packages
	 (quote
		(dap-mode forge helm-descbinds helm multi-term org-bullets lsp-clangd magit treemacs treemacs-magit bazel-mode swiper zoom writeroom-mode ace-window eyebrowse projectile exec-path-from-shell helpful git-gutter rainbow-delimiters rainbow-mode highlight-thing spaceline spacemacs-theme dashboard which-key whitespace-cleanup-mode diminish use-package-chords use-package-ensure-system-package))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
