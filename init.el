(setq inhibit-startup-message t)
(set-language-environment "UTF-8")

;; Remover o janelas de menu da UI
(tool-bar-mode -1)
(menu-bar-mode -1)

(global-set-key (kbd "C-s") 'helm-occur)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

;;fullscreen
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

;; Fontes
(set-face-attribute 'default nil :font "Fira Code" :height 120)

;; Remover scrollbar
(scroll-bar-mode -1)

;; Adicionar número de linhas na coluna esquerda
(global-linum-mode t)

;; Não criar backup dos arquivos
(setq make-backup-files nil)

;; Trocar de tela com ctrl + tab
(global-set-key (kbd "C-<tab>") 'other-window)

;; Org mode configuration
(require 'org)
(setq inhibit-splash-screen t)

;; Pacotes
(eval-and-compile
  (customize-set-variable
   'package-archives '(("org" . "https://orgmode.org/elpa/")
                       ("melpa" . "https://melpa.org/packages/")
                       ("gnu" . "https://elpa.gnu.org/packages/")))
  (package-initialize)
  (unless (package-installed-p 'use-package)
    (package-refresh-contents)
    (package-install 'use-package)))

;; Built-in project package
(require 'project)
(global-set-key (kbd "C-x p f") #'project-find-file)

;; Built-in project package
(require 'project)
(global-set-key (kbd "C-x p f") #'project-find-file)


;; General settings
(delete-selection-mode t)
(setq gc-cons-threshold 100000000)
(setq read-process-output-max (* 1024 1024)) ;; 1mb
(setq auto-save-default nil)
(setq make-backup-files nil)
(setq create-lockfiles nil)
(global-display-line-numbers-mode)

(use-package which-key
  :ensure t
  :config
  (which-key-mode))



(use-package expand-region
  :ensure t
  :bind (("C-=" . er/expand-region)
	 ("C--" . er/contract-region)))


;; json-mode
(use-package json-mode
  :ensure t)

(use-package beacon
    :ensure t 
    :config
    (beacon-mode 1))

;; web-mode
(setq web-mode-markup-indent-offset 2)
(setq web-mode-code-indent-offset 2)
(setq web-mode-css-indent-offset 2)
(use-package web-mode
  :ensure t
  :mode (("\\.js\\'" . web-mode)
	 ("\\.jsx\\'" .  web-mode)
	 ("\\.ts\\'" . web-mode)
	 ("\\.tsx\\'" . web-mode)
	 ("\\.html\\'" . web-mode))
  :commands web-mode)


;; company
(setq company-minimum-prefix-length 1
      company-idle-delay 0.0)

(use-package company
  :ensure t
  :bind(
	("C-SPC" . company-complete))
  :config (global-company-mode t))


;; magit
(use-package magit
  :ensure t
  :bind (
	 ("C-x g" . magit-status)))


(defun efs/lsp-mode-setup ()
  (setq lsp-headerline-breadcrumb-segments '(path-up-to-project file symbols))
  (lsp-headerline-breadcrumb-mode))

;; lsp-mode
(setq lsp-log-io nil) ;; Don't log everything = speed
(setq lsp-keymap-prefix "C-c l")
(setq lsp-restart 'auto-restart)
(setq lsp-ui-sideline-show-diagnostics t)
(setq lsp-ui-sideline-show-hover nil)
(setq lsp-ui-sideline-show-code-actions nil)
(use-package lsp-mode
  :ensure t
  :hook (
	 (web-mode . lsp-deferred)
	 (lsp-mode . lsp-enable-which-key-integration)
	 (lsp-mode . efs/lsp-mode-setup)
	 )
  :commands lsp-deferred)


(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-doc-position 'bottom))

;; (use-package company-box
;;   :hook (company-mode . company-box-mode))

(defun enable-minor-mode (my-pair)
  "Enable minor mode if filename match the regexp.  MY-PAIR is a cons cell (regexp . minor-mode)."
  (if (buffer-file-name)
      (if (string-match (car my-pair) buffer-file-name)
	  (funcall (cdr my-pair)))))

(use-package prettier-js
  :ensure t)
(add-hook 'web-mode-hook #'(lambda ()
                             (enable-minor-mode
                              '("\\.jsx?\\'" . prettier-js-mode))
			     (enable-minor-mode
                              '("\\.tsx?\\'" . prettier-js-mode))))

;; Doom mode Line para icones bonitos 
(use-package doom-modeline
    :ensure t
    :init (doom-modeline-mode 1)    
)

(use-package monokai-theme
  :ensure t
  :config
  (load-theme 'monokai t))

;; Habilitar modo transcedente do mark mode
(transient-mark-mode 1)
(setq org-log-done 'time)
(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

(use-package beacon
    :ensure t 
    :config
    (beacon-mode 1))

;; Adicionar sintaxe highlight para Csharp e dotnet support
(use-package dotnet
  :ensure t)
(defun my-csharp-mode-hook ()
  ;; enable the stuff you want for C# here
  (electric-pair-mode 1)       ;; Emacs 24
  (electric-pair-local-mode 1) ;; Emacs 25
  )
(add-hook 'csharp-mode-hook 'dotnet-mode)
(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

;; Adicionar sintaxe highlight para Python
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

(setq package-selected-packages '(eglot yasnippet helm-lsp projectile hydra flycheck avy helm-xref))
(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))
(helm-mode 1)
(require 'helm-xref)
(require 'eglot)

;; habilitando yas-minor-mode para poder ter auto complete de funções não importadas.
(add-hook 'prog-mode-hook #'yas-minor-mode)

;; Vue configuração
(define-derived-mode genehack-vue-mode web-mode "ghVue"
  "A major mode derived from web-mode, for editing .vue files with LSP support.")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . genehack-vue-mode))
(add-hook 'genehack-vue-mode-hook #'eglot-ensure)
(add-to-list 'eglot-server-programs '(genehack-vue-mode "vls"))

(use-package emmet-mode
  :ensure t)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; emmet para html
(add-hook 'css-mode-hook  'emmet-mode) ;; emmet para css

(use-package rainbow-delimiters
  :ensure t)
(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)
(add-hook 'prog-mode-hook #'electric-pair-mode t)
