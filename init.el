(setq inhibit-startup-message t)

;; Remover o janelas de menu da UI
(tool-bar-mode -1)
(menu-bar-mode -1)

;;fullscreen
(add-hook 'emacs-startup-hook 'toggle-frame-maximized)

;; Remover scrollbar
(scroll-bar-mode -1)

;; Atalho para o Shell
(global-set-key (kbd "C-x S") 'shell)

;; Adicionar número de linhas na coluna esquerda
(global-linum-mode t)

;; Setando tamanho das fontes
(set-face-attribute 'default nil :height 120)

;; Não criar backup dos arquivos
(setq make-backup-files nil)

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

;; Trocar de tela com ctrl + tab
(global-set-key (kbd "C-<tab>") 'other-window)

;; Org mode configuration
(require 'org)
(setq inhibit-splash-screen t)

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

;;lsp-ui config
(setq lsp-lens-enable 1)
(setq lsp-ui-sideline-enable nil)
(setq lsp-ui-doc-show-with-mouse nil)

;; Web mode 
(use-package web-mode
  :ensure t)

;; Adicionar sintaxe highlight para Jsx 
(add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) 
(setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
(defun web-mode-init-hook ()
  (setq web-mode-markup-indent-offset 2))

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

(add-hook 'web-mode-hook  'web-mode-init-hook)

(global-set-key (kbd "C-s") 'helm-occur)

(setq-default indent-tabs-mode nil)

;; Adicionar sintaxe highlight para Python
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; LSP configuração
(setq package-selected-packages '(lsp-mode eglot yasnippet helm-lsp projectile hydra flycheck company avy which-key helm-xref dap-mode json-mode))
(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))
(helm-mode 1)
(require 'helm-xref)
(require 'eglot)
;; Vue configuração
(define-derived-mode genehack-vue-mode web-mode "ghVue"
  "A major mode derived from web-mode, for editing .vue files with LSP support.")
(add-to-list 'auto-mode-alist '("\\.vue\\'" . genehack-vue-mode))
(add-hook 'genehack-vue-mode-hook #'eglot-ensure)
(add-to-list 'eglot-server-programs '(genehack-vue-mode "vls"))
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)
;; para auto completar
(use-package company-lsp
  :config (push 'company-lsp company-backends))

(which-key-mode)
(add-hook 'prog-mode-hook #'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      create-lockfiles nil) ;; lock files will kill `npm start'



(use-package emmet-mode
  :ensure t)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; emmet para html
(add-hook 'css-mode-hook  'emmet-mode) ;; emmet para css


(with-eval-after-load 'lsp-mode
  (require 'dap-chrome)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))
