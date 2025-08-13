;; 基本设置
(setq initial-buffer-choice
      (lambda ()
        (if (or (buffer-file-name)
                (eq major-mode 'dired-mode))
            (current-buffer)
          (find-file "~/.config/emacs/dashboard.org"))))

;; Load custom file if it exists
(setq custom-file "~/.config/emacs/custom.el")
(when (file-exists-p custom-file)
  (load custom-file :noerror))

;; Disable auto-save for dashboard.org
(add-hook 'find-file-hook
          (lambda ()
            (when (and (buffer-file-name)
                       (string= (buffer-file-name) (expand-file-name "~/.config/emacs/dashboard.org")))
              (setq-local auto-save-default nil))))

;; 禁用工具栏和滚动条
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; 设置包源
(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))
(package-initialize)

;; 安装和配置 eglot
(use-package eglot
  :ensure t
  :config
  (setq eglot-sync-connect 5)
  (setq eglot-autoshutdown t)
  (setq corfu-auto-delay 0.2))

(use-package corfu
  :ensure t
  :hook ((LaTeX-mode . corfu-mode)
         (nix-mode . corfu-mode))
  :config
  (setq corfu-auto t)           ; Enable auto-completion
  (setq corfu-auto-delay 0.2))

(use-package tex
  :ensure auctex
  :hook ((LaTeX-mode . corfu-mode)
         (LaTeX-mode . eglot-ensure))
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t)
  (setq TeX-PDF-mode t)
  (setq TeX-command-default "XeLaTeX")
  (setq-default TeX-engine 'xetex)
  (add-to-list 'TeX-command-list
               '("XeLaTeX" "xelatex -synctex=1 -interaction=nonstopmode %s" TeX-run-command nil t :help "Run XeLaTeX on LaTeX file")
               t)
  (setq TeX-view-program-selection '((output-pdf "PDF Tools")))
  (setq TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view)))
  (setq TeX-source-correlate-mode t)
  (setq TeX-source-correlate-start-server t)
  (with-eval-after-load 'pdf-tools
    (pdf-tools-install :no-query)))

(use-package pdf-tools
  :ensure t
  :mode ("\\.pdf\\'" . pdf-view-mode)
  :config
  (pdf-tools-install))

;; 定义 dashboard 文件路径
(defvar my/dashboard-file (expand-file-name "~/.config/emacs/dashboard.org"))

;; 修改 org-open-file 为全屏 dired
(defun my/org-open-file-dired-full-window (orig-fun path &optional in-emacs line search)
  "在 dashboard 文件中打开目录时全屏显示 dired，其他情况使用默认行为。"
  (if (and (buffer-file-name)
           (string= (buffer-file-name) my/dashboard-file)
           (file-directory-p path))
      (progn
        (delete-other-windows)
        (dired path))
    (funcall orig-fun path in-emacs line search)))

(with-eval-after-load 'org
  (advice-add 'org-open-file :around #'my/org-open-file-dired-full-window))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"
  :hook
  (nix-mode . eglot-ensure)
  (nix-mode . corfu-mode)
  (nix-mode . (lambda ()
                (add-hook 'before-save-hook
                          (lambda ()
                            (when (and buffer-file-name
                                       (eq major-mode 'nix-mode))
                              (call-process "nixfmt" nil 0 nil buffer-file-name)))
                          nil :local)))
  :config
  (with-eval-after-load 'eglot
    (add-to-list 'eglot-server-programs '(nix-mode . ("nixd")))))
 
(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-fullframe-status-v1))
