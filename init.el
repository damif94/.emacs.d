;;; init.el ---

;; .emacs.d/init.el

;; ===================================
;; MELPA Package Support
;; ===================================
;; Enables basic packaging support


(require 'package)
;;;
;;; Code:
(push "~/.emacs.d/config" load-path)

;; Adds the Melpa archive to the list of available repositories
(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/") t)
(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/") t)


;; Initializes the package infrastructure
(package-initialize)

;; If there are no archived package contents, refresh them
(when (not package-archive-contents)
  (package-refresh-contents))

;; Installs packages

;; myPackages contains a list of package names



(load "auctex-config")
;; ===================================
;; Basic Customization
;; ===================================

(setq inhibit-startup-message t)    ;; Hide the startup message
; (load-theme 'flucui-light t)	    ;; Load material theme
(global-linum-mode t)               ;; Enable line numbers

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(LaTeX-clean-intermediate-suffixes
   (quote
    ("\\.aux" "\\.bbl" "\\.blg" "\\.brf" "\\.fot" "\\.glo" "\\.gls" "\\.idx" "\\.ilg" "\\.ind" "\\.lof" "\\.log" "\\.lot" "\\.nav" "\\.out" "\\.snm" "\\.toc" "\\.url" "\\.synctex\\.gz" "\\.bcf" "\\.run\\.xml" "\\.fls" "-blx\\.bib" "\\.acn" "\\.acr" "\\.alg" "\\.glg" "\\.ist" "\\tex~" "\\.fmt")))
 '(LaTeX-command "latex")
 '(TeX-command "luatex")
 '(TeX-view-program-selection
   (quote
    (((output-dvi has-no-display-manager)
      "dvi2tty")
     ((output-dvi style-pstricks)
      "dvips and gv")
     (output-dvi "xdvi")
     (output-pdf "PDF Tools")
     (output-html "xdg-open"))))
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   (vector "#212121" "#B71C1C" "#558b2f" "#FFA000" "#2196f3" "#4527A0" "#00796b" "#FAFAFA"))
 '(company-backends
   (quote
    (company-capf
     (company-auctex-macros company-auctex-symbols company-auctex-environments)
     (company-auctex-macros company-auctex-symbols company-auctex-environments company-yasnippet)
     (company-auctex)
     (company-yasnippet company-auctex-symbols company-auctex-environments company-auctex-macros)
     company-auctex-bibs company-auctex-labels company-bbdb company-eclim company-semantic company-clang company-xcode company-cmake company-capf company-files
     (company-dabbrev-code company-gtags company-etags company-keywords)
     company-oddmuse company-dabbrev)))
 '(company-idle-delay 0)
 '(custom-enabled-themes (quote (atom-one-dark)))
 '(custom-safe-themes
   (quote
    ("171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "732b807b0543855541743429c9979ebfb363e27ec91e82f463c91e68c772f6e3" "a11808699b77d62f5d10dd73cd474af3057d84cceac8f0301b82ad3e4fb0433e" "a24c5b3c12d147da6cef80938dca1223b7c7f70f2f382b26308eba014dc4833a" "cf9f20cab61999609e47b969f6d7a89c148e16f94ae3f2f127fecfc27dc878d3" default)))
 '(fci-rule-color "#ECEFF1")
 '(haskell-interactive-popup-errors nil)
 '(haskell-mode-hook (quote (interactive-haskell-mode)))
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-process-type (quote cabal-repl))
 '(haskell-tags-on-save t)
 '(hl-sexp-background-color "#efebe9")
 '(indent-tabs-mode t)
 '(ivy-mode t)
 '(line-number-mode nil)
 '(package-selected-packages
   (quote
    (yaml-mode lsp-haskell lsp-ui bash-completion inf-elixir atom-one-dark-theme dash dash-functional alchemist elixir-mode lsp-mode org-chef proof-general ggtags neotree ivy projectile realgud blacken py-autopep8 elpy comment-dwim-2 centaur-tabs magit ediprolog exwm xwidgete undo-tree drag-stuff idris-mode yasnippet-snippets visual-regexp-steroids visual-regexp w3m sx ghci-completion which-key move-text flucui-themes haskell-mode pdf-tools auto-complete-auctex auctex eww-lnum exec-path-from-shell grip-mode impatient-mode use-package s material-theme markdown-mode dracula-theme better-defaults)))
 '(projectile-completion-system (quote ivy))
 '(projectile-globally-ignored-directories
   (quote
    (".idea" ".vscode" ".ensime_cache" ".eunit" ".git" ".hg" ".fslckout" "_FOSSIL_" ".bzr" "_darcs" ".tox" ".svn" ".stack-work" ".ccls-cache" ".cache" ".clangd" "venv")))
 '(projectile-tags-command "ggtags")
 '(show-paren-mode t)
 '(tab-always-indent (quote complete))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#B71C1C")
     (40 . "#FF5722")
     (60 . "#FFA000")
     (80 . "#558b2f")
     (100 . "#00796b")
     (120 . "#2196f3")
     (140 . "#4527A0")
     (160 . "#B71C1C")
     (180 . "#FF5722")
     (200 . "#FFA000")
     (220 . "#558b2f")
     (240 . "#00796b")
     (260 . "#2196f3")
     (280 . "#4527A0")
     (300 . "#B71C1C")
     (320 . "#FF5722")
     (340 . "#FFA000")
     (360 . "#558b2f"))))
 '(vc-annotate-very-old-color nil)
 '(visible-bell t))

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))


(use-package flycheck
  :ensure t
  :init
  (global-flycheck-mode t))
(use-package yasnippet
  :ensure t)
(use-package lsp-mode
  :ensure t
  :hook (haskell-mode . lsp)
  :commands lsp)
(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode)
(use-package lsp-haskell
  :ensure t
  :config
 (setq lsp-haskell-server-path "haskell-language-server-wrapper")
 (setq lsp-haskell-server-args ())
   ;; Comment/uncomment this line to see interactions between lsp client/server.
  (setq lsp-log-io t))

; Reduce the number of times the bell rings
; Turn off the bell for the listed functions.
(setq ring-bell-function
      (lambda ()
        (unless (memq this-command
                      '(isearch-abort
                        abort-recursive-edit
                        exit-minibuffer
                        keyboard-quit
                        previous-line
                        next-line
                        scroll-down
                        scroll-up
                        cua-scroll-down
                        cua-scroll-up))
          (ding))))

 ; Latex Preview Size
(require 'yasnippet)
(yas-reload-all)

(set-default 'preview-scale-function 2.5)
(add-hook 'latex-mode-hook '(show-paren-mode 1))
(global-set-key "\C-cd" 'kill-whole-line)
(global-set-key "\C-ck" "\C-a\C- \C-e\M-w")
(global-set-key (kbd "C-h C-f") 'find-function)
(drag-stuff-global-mode 1)
(drag-stuff-define-keys)

(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(use-package which-key
  :ensure t
  :config (which-key-mode))

(use-package company
  :ensure t
  :init
  (add-hook 'after-init-hook 'global-company-mode))


;;(push '(company-yasnippet :with company-auctex) company-backends)

(setq python-shell-interpreter "python3")

(global-flycheck-mode)
(company-auctex-init)
(setq company-dabbrev-downcase nil)


(delete-selection-mode 1)

(put 'set-goal-column 'disabled nil)


(require 'visual-regexp)
(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)


(pdf-tools-install)
(add-hook 'TeX-after-compilation-finished-functions #'TeX-revert-document-buffer)
(add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))
(provide 'init)
(put 'upcase-region 'disabled nil)
(global-undo-tree-mode)




;; prolog config

(setq load-path (cons "/usr/share/emacs/prolog" load-path))
(autoload 'run-prolog "prolog" "Start a Prolog sub-process." t)
(autoload 'prolog-mode "prolog" "Major mode for editing Prolog programs." t)
(autoload 'mercury-mode "prolog" "Major mode for editing Mercury programs." t)
(setq prolog-system 'swi)
(setq auto-mode-alist (append '(("\\.pl$" . prolog-mode)
                                ("\\.m$" . mercury-mode))
			      auto-mode-alist))
(defun prolog-insert-comment-block ()
  "Insert a PceEmacs-style comment block like /* - - ... - - */ "
  (interactive)
  (let ((dashes "-"))
    (dotimes (_ 36) (setq dashes (concat "- " dashes)))
    (insert (format "/* %s\n\n%s */" dashes dashes))
    (forward-line -1)
    (indent-for-tab-command)))

(global-set-key "\C-cc" 'prolog-insert-comment-block)

(global-set-key "\C-cl" (lambda ()
                          (interactive)
                          (insert ":- use_module(library()).")
                          (forward-char -3)))

(add-hook 'prolog-mode-hook
          (lambda ()
            (require 'flymake)
            (make-local-variable 'flymake-allowed-file-name-masks)
            (make-local-variable 'flymake-err-line-patterns)
            (setq flymake-err-line-patterns
                  '(("ERROR: (?\\(.*?\\):\\([0-9]+\\)" 1 2)
                    ("Warning: (\\(.*\\):\\([0-9]+\\)" 1 2)))
            (setq flymake-allowed-file-name-masks
                  '(("\\.pl\\'" flymake-prolog-init)))
            (flymake-mode 1)))

(defun flymake-prolog-init ()
  (let* ((temp-file   (flymake-init-create-temp-buffer-copy
                       'flymake-create-temp-inplace))
         (local-file  (file-relative-name
                       temp-file
                       (file-name-directory buffer-file-name))))
    (list "swipl" (list "-q" "-t" "halt" "-s " local-file))))

(global-set-key (kbd "M-;") 'comment-dwim-2)


; python config
(elpy-enable)
(setq elpy-rpc-python-command "python3")

(when (require 'flycheck nil t)
  (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
  (add-hook 'elpy-mode-hook 'flycheck-mode))

(require 'py-autopep8)
(add-hook 'elpy-mode-hook 'py-autopep8-enable-on-save)


;projectile
(use-package projectile
  :diminish projectile-mode
  :config (projectile-mode)
  :custom ((projectile-completion-system 'ivy))
  :bind-keymap
  ("C-c p" . projectile-command-map)
  :init
  (setq projectile-switch-project-action #'projectile-dired))


(use-package counsel-projectile
  :config (counsel-projectile-mode))

(use-package magit)


(require 'lsp-mode)
(add-hook 'elixir-mode-hook #'lsp)

(use-package lsp-mode
    :commands lsp
    :ensure t
    :diminish lsp-mode
    :hook
    (elixir-mode . lsp)
    :init
    (add-to-list 'exec-path "~/elixir/elixir-ls/release"))

(defvar lsp-elixir--config-options (make-hash-table))

 (add-hook 'lsp-after-initialize-hook
          (lambda ()
            (lsp--set-configuration `(:elixirLS, lsp-elixir--config-options))))

(add-to-list 'auto-mode-alist '("\\.elixir2\\'" . elixir-mode))



(setq backup-directory-alist
      `(("." . ,(concat user-emacs-directory "backups"))))

(with-eval-after-load 'lsp-mode
  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration))


(use-package inf-elixir
  :bind (("C-c i i" . 'inf-elixir)
         ("C-c i p" . 'inf-elixir-project)
         ("C-c i l" . 'inf-elixir-send-line)
         ("C-c i r" . 'inf-elixir-send-region)
         ("C-c i b" . 'inf-elixir-send-buffer)))

(autoload 'bash-completion-dynamic-complete
  "bash-completion"
  "BASH completion hook")
(add-hook 'shell-dynamic-complete-functions
  'bash-completion-dynamic-complete)


(add-to-list 'load-path "~/.emacs.d/company-elixir/")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(haskell-interactive-face-compile-error ((t (:inherit compilation-error)))))
