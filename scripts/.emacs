(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/") t)
(package-initialize)

; start using environment variables
(package-install 'exec-path-from-shell)
(exec-path-from-shell-initialize)

;;;;;;;;;;;;;;;;;;;;;;;;; configure LSP ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;       

(setq lsp-clients-pylsp-library-directories "/home/taba1uga/.emacs.venv/")
(setq lsp-pylsp-server-command "/home/taba1uga/.emacs.venv/bin/pylsp")

(setq lsp-pylsp-plugins-black-enabled t)
(setq lsp-pylsp-plugins-ruff-enabled t)
(setq lsp-pylsp-plugins-mypy-enabled t)
(setq lsp-pylsp-plugins-ruff-lineLength 88)
(setq lsp-pylsp-plugins-isort-enabled t)
(setq lsp-pylsp-plugins-flake8-enabled nil)

(setq lsp-ui-doc-enable nil)
(setq lsp-eldoc-enable-hover t)
(setq lsp-eldoc-render-all t)
(setq max-mini-window-height 10)

(setq lsp-ui-sideline-enable t)
(setq lsp-ui-sideline-show-hover nil)
(setq lsp-ui-sideline-ignore-duplicate t)
(setq lsp-ui-sideline-show-code-actions t)
(setq lsp-ui-sideline-show-diagnostics t)

(setq lsp-headerline-breadcrumb-enable nil)

(setq lsp-diagnostics-provider :flycheck)
(setq lsp-completion-show-detail t)
(setq lsp-completion-show-kind t)


(setq package-selected-packages '(lsp-mode yasnippet lsp-treemacs helm-lsp
    projectile hydra flycheck company which-key dap-mode lsp-ui yascroll))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(which-key-mode)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)
(add-hook 'python-mode-hook 'lsp)

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      treemacs-space-between-root-nodes nil
      company-idle-delay 0.0
      eldoc-idle-delay 0.5
      company-minimum-prefix-length 0
      lsp-idle-delay 0.1)  ;; clangd is fast

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  (require 'dap-cpptools)
  (yas-global-mode))

(global-yascroll-bar-mode 1)

; to log issues with lsp servers
;; lsp-toggle-trace-io + lsp-workspace-show-log

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      

;;;;;;;;;;;;;;;;;;;;;;;;; configure HELM ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
(setq package-selected-packages '(helm-xref))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))

(helm-mode)
(require 'helm-xref)
(define-key global-map [remap find-file] #'helm-find-files)
(define-key global-map [remap execute-extended-command] #'helm-M-x)
(define-key global-map [remap switch-to-buffer] #'helm-mini)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      

;;;;;;;;;;;;;;;;;;;;;;;;; configure KEYS ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
; custom undo
(global-set-key (kbd "C-z") 'undo)

; custom magit status
(global-set-key (kbd "C-c g") 'magit-status)

; navigate threw errors
  (defun my-next-error () 
    "Move point to next error and highlight it"
    (interactive)
    (progn
      (flycheck-next-error)
      (forward-word)
      )
  )
  (defun my-prev-error () 
    "Move point to prev error and highlight it"
    (interactive)
    (progn
      (flycheck-next-error)
      (backward-word)
      )
  )
; custom error nav
(global-set-key (kbd "M-n") 'my-next-error)
(global-set-key (kbd "M-p") 'my-prev-error)

; custom commenting
(defun toggle-comment ()
  "Toggle comments on the current line or highlighted region."
  (interactive)
  (if mark-active
      (let ((mark (mark))
            (point (point)))
        (if (> (mark) (point))
            (comment-or-uncomment-region
             point
             mark)
          (comment-or-uncomment-region
           mark
           point)))
    (comment-or-uncomment-region
     (line-beginning-position)
     (line-end-position))))

(global-set-key (kbd "<f1>") 'toggle-comment)

(global-set-key (kbd "<f2>") 'view-buffer)

(global-set-key (kbd "<f3>") 'lsp-treemacs-errors-list)

(global-set-key (kbd "<f4>") 'eldoc-doc-buffer)

;; Open terminal
(global-set-key (kbd "<f5>") '(lambda () (interactive) (term (getenv "SHELL"))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      


;;;;;;;;;;;;;;;;;;;;;;;;; configure LOOK ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     

(setq package-selected-packages '(powerline flycheck-color-mode-line auto-dim-other-buffers monokai-theme shackle magit))

(when (cl-find-if-not #'package-installed-p package-selected-packages)
  (package-refresh-contents)
  (mapc #'package-install package-selected-packages))


; enable theme
(load-theme 'monokai t)

; set bottom line 
(require 'powerline)
(powerline-default-theme)
; add bottom line colors
(require 'flycheck-color-mode-line)
(eval-after-load "flycheck"
  '(add-hook 'flycheck-mode-hook 'flycheck-color-mode-line-mode))

; turn Yes or No into y or n
(defalias 'yes-or-no-p 'y-or-n-p)

; enable auto dimming not active frame
;(auto-dim-other-buffers-mode 1)

; enable theme
(load-theme 'monokai t)

; set helm window size
(setq helm-display-buffer-default-height 12)  

; hide unnecessary helm lines
(defadvice helm-display-mode-line (after undisplay-header activate)
    (setq header-line-format nil))

; set up mini buffers              
;; helm
(custom-set-faces
 ;;; custom-set-faces was added by Custom.                                                            ;;; If you edit it by hand, you could mess it up, so be careful.                                     ;;; Your init file should contain only one such instance.                                            ;;; If there is more than one, they won't work right.                                               
'(mode-line ((t (:foreground "white" :background "#ff006e" :box nil))))                              
'(mode-line-inactive ((t (:foreground "white" :background "#800080" :box nil)))))            

;; default                                                                                           
;;; if not working move to the end of the file                                                       
(add-hook 'minibuffer-setup-hook                                                                     
      (lambda ()                                                                                     
        (make-local-variable 'face-remapping-alist)                                                  
        (add-to-list 'face-remapping-alist '(default (:background "orange" :foreground "black")))))  
(set-face-background 'minibuffer-prompt "red")                                                                     
   
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      

;;;;;;;;;;;;;;;;;;;;;;;;; configure GLOBAL ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     

; enable auto paranthesis completion
(electric-pair-mode 1)

; enable snippets completetion
(require 'yasnippet)
(yas-global-mode 1)

(require 'shackle)
(shackle-mode 1)

; make helm window always on bottom
(setq shackle-rules '(("\\`\\*helm.*?\\*\\'" :regexp t :align t :ratio 0.4)))

; disable annoying warning messages
(setq native-comp-async-report-warnings-errors 'silent)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     


;;;;;;;;;;;;;;;;;;;;;;;;; configure PYTHON ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;     
(require 'lsp-mode)
(add-hook 'python-mode-hook #'lsp-deferred)

(defun lsp-python-install-save-hooks ()
  (add-hook 'before-save-hook #'lsp-format-buffer t t)
  (add-hook 'before-save-hook #'lsp-organize-imports t t))

(add-hook 'python-mode-hook #'lsp-python-install-save-hooks)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;      
