;; 起動画面を表示しない
(setq inhibit-startup-message t)

;; ツールバー非表示
(tool-bar-mode 0)

;; CommandとOptionを入れ替える
(setq ns-command-modifier (quote meta))
(setq ns-alternate-modifier (quote super))

;;backspace
(global-set-key "\C-h" 'backward-delete-char)

;; 日本語IM用の設定
(setq default-input-method "MacOSX")

;; 日本語の設定（UTF-8）
(set-language-environment 'Japanese)
(prefer-coding-system 'utf-8)
(custom-set-variables

;; custom-set-variables was added by Custom.
;; If you edit it by hand, you could mess it up, so be careful.
;; Your init file should contain only one such instance.
;; If there is more than one, they won't work right.
 '(custom-enabled-themes (quote (tsdh-dark))))
(custom-set-faces )

;;パッケージリポジトリ追加
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)

;;行番号
(global-linum-mode t)

;; eww
(setq eww-search-prefix "https://www.google.co.jp/search?q=")

;; 自動バックアップ設定
(setq make-backup-files nil)

;; ls 対策 --- http://qiita.com/maangie/items/5a80ae50c13d14368a72
(let ((gls "/usr/local/bin/gls"))
  (if (file-exists-p gls) (setq insert-directory-program gls)))

;;かっこをハイライト
(show-paren-mode t)
; highlight entire bracket expression
(setq show-paren-style 'expression)

;;====================================
;; SHOW-CSS
;;====================================
(autoload 'showcss-mode "show-css"
   "Display the css of the class or id the cursor is at" t)
(defun sm/toggle-showcss()
  "Toggle showcss-mode"
  (interactive)
  (if (derived-mode-p
       'html-mode
       'nxml-mode
       'nxhtml-mode
       'web-mode
       'handlebars-mode)
      (showcss-mode 'toggle)
    (message "Not in an html mode")))
(global-set-key (kbd "C-c C-k") 'sm/toggle-showcss)

;;====================================
;; web-mode
;;====================================

(require 'web-mode)

;;; emacs 23以下の互換
;; (when (< emacs-major-version 24)
;;   (defalias 'prog-mode 'fundamental-mode))

;;; 適用する拡張子
;; (add-to-list 'auto-mode-alist '("\\.phtml$"     . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php$" . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.jsp$"       . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.as[cp]x$"   . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.erb$"       . web-mode))
;; (add-to-list 'auto-mode-alist '("\\.html?$"     . web-mode))

;;; インデント数
(defun web-mode-hook ()
  "Hooks for Web mode."
  (setq web-mode-html-offset   2)
  (setq web-mode-css-offset    2)
  (setq web-mode-script-offset 2)
  (setq web-mode-php-offset    2)
  (setq web-mode-java-offset   2)
  (setq web-mode-asp-offset    2))
(add-hook 'web-mode-hook 'web-mode-hook)

;;====================================
;; auto-complete
;;====================================


(require 'auto-complete)
(require 'auto-complete-config)
(global-auto-complete-mode t)
(define-key ac-completing-map (kbd "C-n") 'ac-next)
(define-key ac-completing-map (kbd "C-p") 'ac-previous)


