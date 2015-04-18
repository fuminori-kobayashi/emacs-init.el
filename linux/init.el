;;******************************************************************
;; ctrl-h �� backspace
(global-set-key "\C-h" 'delete-backward-char)

;;; load-path�̒ǉ��֐�
(defun add-to-load-path (&rest paths)
  (let (path)
    (dolist (path paths paths)
      (let ((default-directory (expand-file-name (concat user-emacs-directory path))))
        (add-to-list 'load-path default-directory)
        (if (fboundp 'normal-top-level-add-subdirs-to-load-path)
            (normal-top-level-add-subdirs-to-load-path))))))

;; load-path�ɒǉ�����t�H���_
;; 2�ȏ�w�肷��ꍇ�̌` -> (add-to-load-path "elisp" "xxx" "xxx")
;;(add-to-load-path "elisp")

;;; �X�^�[�g�A�b�v��\��
(setq inhibit-startup-screen t)

;;; �c�[���o�[��\��
(tool-bar-mode 0)

;;; �t�@�C���̃t���p�X���^�C�g���o�[�ɕ\��
(setq frame-title-format
      (format "%%f - Emacs@%s" (system-name)))

;;; Windows �ŉp���� DejaVu Sans Mono�A���{���Meiryo���w��
(when (eq window-system 'w32)
  (set-face-attribute 'default nil
                      :family "DejaVu Sans Mono"
                      :height 100)
  (set-fontset-font nil 'japanese-jisx0208 (font-spec :family "Meiryo")))

;;; �o�b�N�A�b�v���c���Ȃ�
(setq make-backup-files nil)

;;; �s�ԍ��\��
(global-linum-mode)

;;; �����R�[�h
(set-language-environment "Japanese")
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(setq file-name-coding-system 'sjis)
(setq locale-coding-system 'utf-8)

;;; ���ʂ͈͓̔��������\��
(setq show-paren-delay 0)
(show-paren-mode t)
(setq show-paren-style 'expression)
;; ���ʂ͈̔͐F
(set-face-background 'show-paren-match-face "#804")

;;; �I��̈�̐F
(set-face-background 'region "#36f")


;;; �ŋߎg�����t�@�C�������j���[�ɕ\��
(recentf-mode 1)
(setq recentf-max-menu-items 10)
(setq recentf-max-saved-items 10)


;;�ʏ�̃E�B���h�E�p�̐ݒ�
(setq-default truncate-lines t)
;;�E�B���h�E�����E�ɕ��������Ƃ��p�̐ݒ�
(setq-default truncate-partial-width-windows nil)

;;�s�ԍ�
(line-number-mode t)
;;��ԍ�
(column-number-mode t)

;; ��ʂ̂R����
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


;; "<M-kanji> is undefined"���\��
(global-set-key [M-kanji] 'ignore)

;;====================================
;;; �܂�Ԃ��\��ON/OFF
;;====================================
(defun toggle-truncate-lines ()
  "�܂�Ԃ��\�����g�O�����삵�܂�."
  (interactive)
  (if truncate-lines
      (setq truncate-lines nil)
    (setq truncate-lines t))
  (recenter))

(global-set-key "\C-c\C-l" 'toggle-truncate-lines) ; �܂�Ԃ��\��ON/OFF

;; �����ʂ̌�ɉE���ʂ����͂��ꂽ�Ƃ��ɃJ�[�\�������ʂ̒��Ɉړ�������}�C�i�[���[�h cursor-in-brackets.el
;; http://d.hatena.ne.jp/yascentur/20130526/1369550512
(when (require 'cursor-in-brackets nil t)
  (global-cursor-in-brackets-mode t))


;;====================================
;; ���|�W�g���̒ǉ�
;; http://mymemo.weby117.com/develop/emacs_10.html
;;====================================
(require 'package)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(package-initialize)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#ad7fa8" "#8cc4ff" "#eeeeec"])
 '(custom-enabled-themes (quote (tsdh-dark))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
