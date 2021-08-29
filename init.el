(setq inhibit-startup-message t)

;; Remover o janelas de menu da UI
(tool-bar-mode -1)
(menu-bar-mode -1)

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
(require 'package)

;; Desativar pacotes ao iniciar
(setq package-enable-at-startup nil)

;; Adicionar packages disponivéis do Melpa
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))


;; Iniciar os pacotes
(package-initialize)


;; Atualizar pacotes
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

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

;; Habilitar modo transcedente do mark mode
(transient-mark-mode 1)
(setq org-log-done 'time)
(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))

(use-package beacon
    :ensure t 
    :config
    (beacon-mode 1)
)

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
  (setq web-mode-markup-indent-offset 4))

;; Adicionar sintaxe highlight para Csharp
(defun my-csharp-mode-hook ()
  ;; enable the stuff you want for C# here
  (electric-pair-mode 1)       ;; Emacs 24
  (electric-pair-local-mode 1) ;; Emacs 25
  )
(add-hook 'csharp-mode-hook 'my-csharp-mode-hook)

(add-hook 'web-mode-hook  'web-mode-init-hook)

(global-set-key (kbd "C-s") 'helm-occur)

(setq-default indent-tabs-mode nil)
(setq web-mode-code-indent-offset 4)
(setq web-mode-indent-style 4)


;; Adicionar sintaxe highlight para Python
(add-hook 'python-mode-hook 'jedi:setup)
(setq jedi:complete-on-dot t)

;; LSP configuração
(package-initialize)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(setq package-selected-packages '(lsp-mode yasnippet helm-lsp projectile hydra flycheck company avy which-key helm-xref dap-mode spacemacs-theme json-mode))
(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))
(load-theme 'spacemacs-light t)
(helm-mode 1)
(require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

(which-key-mode)
(add-hook 'prog-mode-hook #'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      company-idle-delay 0.0
      company-minimum-prefix-length 1
      create-lockfiles nil) ;; lock files will kill `npm start'

(with-eval-after-load 'lsp-mode
  (require 'dap-chrome)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (spacemacs-light)))
 '(custom-safe-themes
   (quote
    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(package-selected-packages
   (quote
    (lsp-jedi jedi htmlize lsp-ui omnisharp lsp-mode yasnippet helm-lsp projectile hydra flycheck company avy which-key helm-xref dap-mode spacemacs-theme json-mode))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
