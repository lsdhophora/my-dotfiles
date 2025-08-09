;; 基本设置
;; 自定义 initial-buffer-choice
(setq initial-buffer-choice
      (lambda ()
        (if (or (buffer-file-name)
                (eq major-mode 'dired-mode))
            (current-buffer)
          (find-file "~/.config/emacs/dashboard.org"))))

(setq custom-file "~/.config/emacs/custom.el")
(load custom-file 'noerror)
(add-hook 'find-file-hook
          (lambda ()
            (when (string= (buffer-file-name) (expand-file-name "~/.config/emacs/dashboard.org"))
              (setq-local auto-save-default nil))))

;; 禁用工具栏和滚动条
(tool-bar-mode -1)
(scroll-bar-mode -1)

;; 设置包源（使用中国镜像加速）
(setq package-archives '(("gnu" . "https://mirrors.ustc.edu.cn/elpa/gnu/")
                         ("melpa" . "https://mirrors.ustc.edu.cn/elpa/melpa/")
                         ("nongnu" . "https://mirrors.ustc.edu.cn/elpa/nongnu/")))
(package-initialize)

;; 安装和配置 lsp-mode（仅用于 LaTeX）
(use-package lsp-mode
  :ensure t
  :hook (LaTeX-mode . lsp) ; 仅在 LaTeX 模式启用 lsp
  :config
  (setq lsp-tex-server 'texlab) ; LaTeX 使用 texlab
  (setq lsp-enable-symbol-highlighting t) ; 高亮当前符号
  (setq lsp-enable-links t) ; 启用超链接（如 \include）
  (setq lsp-response-timeout 10)) ; 响应超时时间

;; 启用 company-mode 进行补全
(use-package company
  :ensure t
  :hook (LaTeX-mode . company-mode) ; 仅在 LaTeX 模式启用补全
  :config
  (setq company-idle-delay 0.2) ; 补全延迟
  (setq company-minimum-prefix-length 1) ; 最小前缀长度
  (add-to-list 'company-backends 'company-capf)) ; 使用 lsp 补全后端

;; LaTeX 配置（AUCTeX）
(use-package tex
  :ensure auctex
  :config
  (setq TeX-auto-save t) ; 自动保存
  (setq TeX-parse-self t) ; 自动解析
  (setq TeX-PDF-mode t) ; 默认 PDF 输出
  (setq TeX-command-default "XeLaTeX") ; 默认使用 XeLaTeX
  (setq-default TeX-engine 'xetex) ; 设置 XeLaTeX 引擎
  (add-to-list 'TeX-command-list
               '("XeLaTeX" "xelatex -synctex=1 %s" TeX-run-command nil t)
               t) ; 确保 XeLaTeX 在列表开头
  
  ;; 使用 GNOME 的 PDF 查看器（如 Evince）
  (setq TeX-view-program-selection '((output-pdf "Evince")))
  (setq TeX-view-program-list
        '(("Evince" "evince --page-index=%(outpage) %o"))))
   
;; 定义目标文件的完整路径
(defvar my/dashboard-file (expand-file-name "~/.config/emacs/dashboard.org"))

;; 修改后的自定义函数（使用 :around advice）
(defun my/org-open-file-dired-full-window (orig-fun path &optional in-emacs line search)
  "在 dashboard 文件中打开目录时全屏显示 dired，其他情况使用默认行为。"
  (if (and (buffer-file-name) ; 确保当前 buffer 有关联的文件
       (string= (buffer-file-name) my/dashboard-file)
       (file-directory-p path))
      (progn
        (delete-other-windows)
        (dired path))
    ;; 非目标文件或非目录：调用原始函数
    (funcall orig-fun path in-emacs line search)))

;; 添加条件化建议（使用 :around 类型）
(with-eval-after-load 'org
  (advice-add 'org-open-file :around #'my/org-open-file-dired-full-window))

(use-package nix-mode
  :ensure t
  :mode "\\.nix\\'"
  :hook ((nix-mode . lsp-deferred)
         (nix-mode . (lambda () (add-hook 'before-save-hook #'lsp-format-buffer nil t))))
  :config
  (require 'lsp-mode)
  (setq lsp-enable-snippet nil)
  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-stdio-connection "nil")
    :major-modes '(nix-mode)
    :priority 0
    :server-id 'nil))
  (setq lsp-nix-nil-formatting-command '("nixfmt")))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status))  ;; 绑定 C-x g 到 magit-status
  :config
  (setq magit-display-buffer-function #'magit-display-buffer-same-window-except-diff-v1))
