;;; init.el --- Emacs個人設定

;;; Commentary:

;;; Code:
;; ctrl-h で backspace
(global-set-key "\C-h" 'delete-backward-char)

;;; load-pathの追加関数
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

(require 'package)
;; ;; MELPAを追加
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

;; ;; MELPA-stableを追加
;; (add-to-list 'package-archives '("melpa-stable" . "https://stable.melpa.org/packages/") t)

;; ;; Marmaladeを追加
;; (add-to-list 'package-archives  '("marmalade" . "http://marmalade-repo.org/packages/") t)

;; ;; Orgを追加
(add-to-list 'package-archives '("org" . "http://orgmode.org/elpa/") t)

;; ;; 初期化
(package-initialize)

;; load-pathに追加するフォルダ
;; 2つ以上指定する場合の形 -> (add-to-load-path "elisp" "xxx" "xxx")
;; $ mkdir ~/.emacs/elisp
;; (add-to-load-path "elisp")

;;; スタートアップ非表示
(setq inhibit-startup-screen t)

;;; toolbar/menubar
(tool-bar-mode 0)
(menu-bar-mode 0)


;;; ファイルのフルパスをタイトルバーに表示
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;;; Windows で英数に DejaVu Sans Mono、日本語にMeiryoを指定
;; (when (eq window-system 'w32)
;;   (set-face-attribute 'default nil
;;                       :family "DejaVu Sans Mono"
;;                       :height 100)
;;   (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Meiryo")))

;;; バックアップを残さない
(setq make-backup-files nil)

;;; 行番号表示
(global-linum-mode t)
;; 行番号
(line-number-mode t)
;; 列番号
(column-number-mode t)

;;; 文字コード
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq locale-coding-system 'utf-8)
;;(setq file-name-coding-system 'sjis)

;;----------------
;; 括弧
;;----------------
;;; 括弧の範囲内を強調表示
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'expression)

;; 括弧の範囲色
(set-face-background 'show-paren-match "#5183b3")

;; かっこの自動補完
;;http://ergoemacs.org/emacs/emacs_insert_brackets_by_pair.html
(electric-pair-mode 1)

;; 左括弧の後に右括弧が入力されたときにカーソルを括弧の中に移動させるマイナーモード cursor-in-brackets.el
;; http://d.hatena.ne.jp/yascentur/20130526/1369550512
;; (when (require 'cursor-in-brackets nil t)
;;   (global-cursor-in-brackets-mode t))

;;----------------
;; インデント
;;----------------

;; タブにスペースを使用する
;; tab width
(setq-default indent-tabs-mode nil)
(setq tab-width 2)

;; javascript mode
(setq js-indent-level 4)

;;----------------
;; インデント
;;----------------

;;; 選択領域の色
(set-face-background 'region "#36f")


;;; 最近使ったファイルをメニューに表示
(recentf-mode 1)
(setq recentf-max-menu-items 10)
(setq recentf-max-saved-items 10)

;;通常のウィンドウ用の設定
(setq-default truncate-lines t)
;;ウィンドウを左右に分割したとき用の設定
(setq-default truncate-partial-width-windows nil)
;;; ツールバー非表示
;;(tool-bar-mode 0)

;; 画面の３分割
(defun split-window-vertically-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-vertically)
    (progn
      (split-window-vertically
       (- (window-height) (/ (window-height) num_wins)))
      (split-window-vertically-n (- num_wins 1)))))
(defun split-window-horizontally-n (num_wins)
  (interactive "p")
  (if (= num_wins 2)
      (split-window-horizontally)
    (progn
      (split-window-horizontally
       (- (window-width) (/ (window-width) num_wins)))
      (split-window-horizontally-n (- num_wins 1)))))
(global-set-key "\C-x@" '(lambda ()
                           (interactive)
                           (split-window-vertically-n 3)))
(global-set-key "\C-x#" '(lambda ()
                           (interactive)
                           (split-window-horizontally-n 3)))


;; "<M-kanji> is undefined"を非表示
(global-set-key [M-kanji] 'ignore)

;;====================================
;;; 折り返し表示ON/OFF
;;====================================
(setq truncate-partial-width-windows nil)
(defun toggle-truncate-lines ()
  "折り返し表示をトグル動作します."
  (interactive)
  (if truncate-lines
      (setq truncate-lines nil)
    (setq truncate-lines t))
  (recenter))

(global-set-key "\C-c\C-l" 'toggle-truncate-lines) ; 折り返し表示ON/OFF

;;
;; Package setting
;;

;; 共通
;; add-node-modules-path
;; [install]
;; M-x package-install => add-node-modules-path

;;-------------------------
;; WEB mode --- http://web-mode.org/
;;
;; [install]
;; M-x package-install => web-mode
;;-------------------------
(require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.php\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.js\\'"   . web-mode))
(add-to-list 'auto-mode-alist '("\\.css\\'"   . web-mode))
(add-to-list 'auto-mode-alist '(".*\\.tsx\\'" . web-mode))

(defun my-web-mode-hook ()
  "Hooks for Web mode."
  ;; HTML element offset indentation
  (setq web-mode-markup-indent-offset 2)
  ;; CSS offset indentation
  (setq web-mode-css-indent-offset 2)
  ;;Script/code offset indentation (for JavaScript, Java, PHP, Ruby, Go, VBScript, Python, etc.)
  (setq web-mode-code-indent-offset 4)
  ;;For <style> parts
  (setq web-mode-style-padding 2)
  ;;For <script> parts
  (setq web-mode-script-padding 4)
  ;;For multi-line blocks
  (setq web-mode-block-padding 0)

)
(add-hook 'web-mode-hook  'my-web-mode-hook)
(eval-after-load 'web-mode
  '(progn
     ;; for React With TypeScript
     (add-hook 'web-mode-hook #'setup-tide-mode)
     (add-hook 'web-mode-hook #'add-node-modules-path)
     (add-hook 'web-mode-hook #'prettier-js-mode)))

;;-------------------------------------
;; flycheck --- https://www.flycheck.org/en/latest/
;; 編集中のlintツール連携
;;
;; [install]
;; M-x package-list-package => flycheck
;;-------------------------------------
(require 'flycheck)
(add-hook 'after-init-hook #'global-flycheck-mode)

;; disable jshint since we prefer eslint checking
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
    '(javascript-jshint)))

;; disable json-jsonlist checking for json files
(setq-default flycheck-disabled-checkers
  (append flycheck-disabled-checkers
          '(json-jsonlist)))

;; customize flycheck temp file prefix
(setq-default flycheck-temp-prefix ".flycheck")

;; use eslint with web-mode
;; https://eslint.org/
;; [install]
;; sudo npm install -g eslint
(flycheck-add-mode 'javascript-eslint 'web-mode)

;; use html-tidy whth web-mode
;; http://www.html-tidy.org/
;; [install] http://binaries.html-tidy.org/
;; wget (rpm download url)
;; sudo rpm -ihv (downloaded rpm filename)
;; (flycheck-add-mode 'html-tidy 'web-mode)

;;-------------------------------------
;; Prettier --- https://github.com/prettier/prettier-emacs
;; script 整形
;;
;; [install]
;; shell) sudo npm install -g prettier
;; emacs) M-x package-install => prettier-js
;;-------------------------------------

(require 'prettier-js)

;;-------------------------------------
;; company --- https://company-mode.github.io/
;; 補完
;;
;; [install]
;; M-x package-install => company
;;-------------------------------------
(require 'company)
(global-company-mode) ; 全バッファで有効にする
(setq company-idle-delay 0) ; デフォルトは0.5
(setq company-minimum-prefix-length 2) ; デフォルトは4
(setq company-selection-wrap-around t) ; 候補の一番下でさらに下に行こうとすると一番上に戻る

;;-------------------------------------
;; tide --- https://github.com/ananthakumaran/tide
;; typescript dev env
;;
;; [install]
;; M-x package-list-package => tide
;;-------------------------------------
(defun setup-tide-mode ()
  (interactive)
  (tide-setup)
  (flycheck-mode +1)
  (setq flycheck-check-syntax-automatically '(save mode-enabled))
  (eldoc-mode +1)
  (tide-hl-identifier-mode +1)
  ;; company is an optional dependency. You have to
  ;; install it separately via package-install
  ;; `M-x package-install [ret] company`
  (company-mode +1)
)

;; aligns annotation to the right hand side
(setq company-tooltip-align-annotations t)

;; formats the buffer before saving
;;(add-hook 'before-save-hook 'tide-format-before-save)
(add-hook 'typescript-mode-hook #'setup-tide-mode)

(eval-after-load 'typescript-mode
    '(progn
       (add-hook 'typescript-mode-hook #'add-node-modules-path)
       (add-hook 'typescript-mode-hook #'prettier-js-mode)))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(org-agenda-files nil)
 '(package-selected-packages
   (quote
    (xref-js2 dumb-jump js2-mode magit gnu-elpa-keyring-update tide ztree yaml-mode php-mode add-node-modules-path rjsx-mode web-mode prettier-js flycheck company))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
;;
;;-------------------------------------
;; rjsx-mode
;; react
;;
;; [install]
;; M-x package-install => rjsx-mode
;;-------------------------------------

(add-to-list 'auto-mode-alist '(".*\\.js\\'" . rjsx-mode))
;;(add-to-list 'auto-mode-alist '(".*\\.tsx\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("components\\/.*\\.js\\'" . rjsx-mode))
(add-to-list 'auto-mode-alist '("containers\\/.*\\.js\\'" . rjsx-mode))
(add-hook 'rjsx-mode-hook
          (lambda ()
            (setq indent-tabs-mode nil) ;;インデントはタブではなくスペース
            (setq js-indent-level 2) ;;スペースは２つ、デフォルトは4
            (setq js2-strict-missing-semi-warning nil))) ;;行末のセミコロンの警告はオフ

(with-eval-after-load 'rjsx-mode
  (define-key rjsx-mode-map "<" nil)
  (define-key rjsx-mode-map (kbd "C-d") nil)
  (define-key rjsx-mode-map ">" nil))
(eval-after-load 'rjsx-mode
    '(progn
       (add-hook 'rjsx-mode-hook #'add-node-modules-path)
       (add-hook 'rjsx-mode-hook #'prettier-js-mode)))

(add-hook 'rjsx-mode-hook
          (lambda ()
            (when (string-equal "js" (file-name-extension buffer-file-name))
              (setup-tide-mode))))
;; configure jsx-tide checker to run after your default jsx checker
;;(flycheck-add-next-checker 'javascript-eslint 'jsx-tide 'append)


;;-------------------------------------
;; php-mode
;;
;; [install]
;; M-x package-list-packages => (select)php-mode
;;-------------------------------------
(add-hook 'php-mode-hook
  (lambda()
     (setq tab-width 4)
   ))
(add-to-list 'auto-mode-alist '(".*\\.php\\'" . php-mode))

;;-------------------------------------
;; dumb-jump
;;
;; [install]
;; M-x package-list-packages => (select)dumb-jump
;;-------------------------------------
(add-hook 'xref-backend-functions #'dumb-jump-xref-activate)
(global-set-key (kbd "C-x j") 'dumb-jump-go)

;;; init.el ends here
