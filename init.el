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


;; discord rich presence
(require 'elcord)
(elcord-mode)

(use-package emmet-mode
  :ensure t)
(add-hook 'sgml-mode-hook 'emmet-mode) ;; emmet para html
(add-hook 'css-mode-hook  'emmet-mode) ;; emmet para css


(with-eval-after-load 'lsp-mode
  (require 'dap-chrome)
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (yas-global-mode))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-term-color-vector
   [unspecified "#2d2a2e" "#ff6188" "#a9dc76" "#ffd866" "#78dce8" "#ab9df2" "#a1efe4" "#fcfcfa"])
 '(custom-enabled-themes (quote (atom-one-dark)))
 '(custom-safe-themes
   (quote
    ("171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "24168c7e083ca0bbc87c68d3139ef39f072488703dcdd82343b8cab71c0f62a7" "c8b83e7692e77f3e2e46c08177b673da6e41b307805cd1982da9e2ea2e90e6d7" "c1284dd4c650d6d74cfaf0106b8ae42270cab6c58f78efc5b7c825b6a4580417" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" default)))
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#3a81c3")
     ("OKAY" . "#3a81c3")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#42ae2c")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f"))))
 '(package-selected-packages
   (quote
    (atom-one-dark-theme elcord typescript-mode monokai-pro-theme dracula-theme lsp-jedi jedi htmlize lsp-ui omnisharp lsp-mode yasnippet helm-lsp projectile hydra flycheck company avy which-key helm-xref dap-mode spacemacs-theme json-mode)))
 '(pdf-view-midnight-colors (quote ("#655370" . "#fbf8ef"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
