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

;; Org mode configuration
(require 'org)
(setq inhibit-splash-screen t)

;; Habilitar modo transcedente do mark mode
(transient-mark-mode 1)
(setq org-log-done 'time)
(setq org-todo-keywords
  '((sequence "TODO" "IN-PROGRESS" "WAITING" "DONE")))


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

;; Adicionar sintaxe highlight Ocaml
(require 'merlin-company)
(let ((opam-share (ignore-errors (car (process-lines "opam" "config" "var"
   "share")))))
      (when (and opam-share (file-directory-p opam-share))
       ;; Register Merlin
       (add-to-list 'load-path (expand-file-name "emacs/site-lisp" opam-share))
       (autoload 'merlin-mode "merlin" nil t nil)
       ;; Automatically start it in OCaml buffers
       (add-hook 'tuareg-mode-hook 'merlin-mode t)
       (add-hook 'caml-mode-hook 'merlin-mode t)
       ;; Use opam switch to lookup ocamlmerlin binary
       (setq merlin-command 'opam)))

(add-hook 'web-mode-hook  'web-mode-init-hook)

(global-set-key (kbd "C-s") 'helm-occur)

(setq-default indent-tabs-mode nil)
(setq web-mode-code-indent-offset 4)
(setq web-mode-indent-style 4)


;; Neo-tree e all-icons || treemacs
(global-set-key (kbd "C-<tab>") 'other-window)

;; Icons para o Neo-tree
(use-package all-the-icons
  :ensure t)

;; Neo-tree configuração
(use-package neotree
  :ensure t
  :config
  (progn
    (setq neo-theme (if (display-graphic-p) 'icons 'arrow)))
  :bind (("C-\\" . 'neotree-toggle)))
(setq-default neo-show-hidden-files t)


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
 '(ansi-color-names-vector
   ["#0a0814" "#f2241f" "#67b11d" "#b1951d" "#4f97d7" "#a31db1" "#28def0" "#b2b2b2"])
 '(custom-enabled-themes (quote (spacemacs-light)))
 '(custom-safe-themes
   (quote
    ("fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" default)))
 '(hl-todo-keyword-faces
   (quote
    (("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f"))))
 '(package-selected-packages
   (quote
    (csharp-mode omnisharp lsp-ui lsp-mode yasnippet helm-lsp projectile hydra flycheck company avy which-key helm-xref dap-mode spacemacs-theme json-mode)))
 '(pdf-view-midnight-colors (quote ("#b2b2b2" . "#292b2e"))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;; ## added by OPAM user-setup for emacs / base ## 56ab50dc8996d2bb95e7856a6eddb17b ## you can edit, but keep this line
(require 'opam-user-setup "~/.emacs.d/opam-user-setup.el")
;; ## end of OPAM user-setup addition for emacs / base ## keep this line
