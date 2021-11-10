;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                          78 character box                                ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq nano-font-family-monospaced "Fantasque Sans Mono")
(setq nano-font-size 12)

(setq inhibit-splash-screen t)

(setq-default line-spacing 2)

(setq frame-resize-pixelwise t)

(column-number-mode)
(setq tab-always-indent 'complete)

(progn
  (setq gc-cons-threshold 20000)
  (setq kill-ring-max 1000)
  (setq enable-recursive-minibuffers t)

  (add-hook 'after-save-hook
            'executable-make-buffer-file-executable-if-script-p)

  (progn
    (setq backup-by-copying t
          backup-directory-alist '(("." . "~/.saves/"))
          delete-old-versions t
          kept-new-versions 6
          kept-old-versions 2
          version-control t)
    (setq auto-save-file-name-transforms `((".*" ,temporary-file-directory t)))
    (setq backup-directory-alist `((".*" . ,temporary-file-directory))))

  (setq fill-nobreak-predicate '(fill-single-word-nobreak-p))
  (setq kill-whole-line t)
  (setq whitespace-style '(face trailing lines-tail empty))
  (setq uniquify-buffer-name-style 'post-forward-angle-brackets)

  (eval-after-load 'tramp
    '(add-to-list 'tramp-remote-path "/run/current-system/sw/bin"))

  (progn
    (put 'downcase-region 'disabled nil)
    (put 'upcase-region 'disabled nil)))

(progn
  (setq-default indent-tabs-mode nil)
  (electric-indent-mode -1)

  (setq c-basic-offset 2)
  (setq css-indent-offset 2)
  (setq js-indent-level 2)
  (setq sh-basic-offset 2))

(progn
  (global-auto-revert-mode))

(defmacro save-column (&rest body)
  `(let ((column (current-column)))
     (unwind-protect
         (progn ,@body)
       (move-to-column column))))
(put 'save-column 'lisp-indent-function 0)

(defun move-line-up ()
  (interactive)
  (save-column
    (transpose-lines 1)
    (forward-line -2)))

(defun move-line-down ()
  (interactive)
  (save-column
    (forward-line 1)
    (transpose-lines 1)
    (forward-line -1)))

(defun sort-lines-dwim ()
  "Sort the lines in the buffer (or the region, if active)."
  (interactive)
  (if (region-active-p)
      (call-interactively 'sort-lines)
    (sort-lines nil (point-min) (point-max))))

;; Unset prefixes
(progn
  (global-unset-key (kbd "C-c c"))
  (global-unset-key (kbd "C-M-o"))
  (global-unset-key (kbd "C-M-h")))

(progn
  (global-set-key (kbd "C-c f") 'projectile-find-file)
  (global-set-key (kbd "C-c s") 'magit-status)
  (global-set-key (kbd "C-c g") 'projectile-ag)
  (global-set-key (kbd "M-n") 'move-line-down)
  (global-set-key (kbd "M-p") 'move-line-up))

(progn
  (global-set-key (kbd "C-M-h f") 'describe-function)
  (global-set-key (kbd "C-M-h v") 'describe-variable)
  (global-set-key (kbd "C-M-h k") 'describe-key)
  (global-set-key (kbd "C-M-x") 'eval-defun)
  (global-set-key (kbd "C-c a") 'align-regexp)
  (global-set-key (kbd "C-c b") 'vterm)
  (global-set-key (kbd "C-c d c") 'describe-char)
  (global-set-key (kbd "C-c d f") 'describe-function)
  (global-set-key (kbd "C-c d m") 'describe-mode)
  (global-set-key (kbd "C-c j") 'join-line)
  (global-set-key (kbd "C-c k") 'fundamental-mode)
  (global-set-key (kbd "C-c m") 'make-directory)
  (global-set-key (kbd "C-c n") 'normal-mode)
  (global-set-key (kbd "C-c o") 'occur)
  (global-set-key (kbd "C-c w") 'browse-url)
  (global-set-key (kbd "C-c y") 'browse-kill-ring)
  (global-set-key (kbd "C-c z") 'sort-lines-dwim)
  (global-set-key (kbd "C-h") 'backward-delete-char)
  (global-set-key (kbd "C-x C-b") 'ibuffer)
  (global-set-key (kbd "C-x C-o") 'other-window)
  (global-set-key (kbd "C-x t") 'string-rectangle)
  (global-set-key (kbd "M-/") 'hippie-expand)
  (global-set-key (kbd "M-h") 'backward-kill-word)
  (global-set-key (kbd "RET") 'newline)

  )

(progn
  (global-set-key (kbd "C-x C-+") 'default-text-scale-increase)
  (global-set-key (kbd "C-x C--") 'default-text-scale-decrease))

(progn
  (add-hook 'emacs-lisp-mode-hook 'enable-paredit-mode)
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-hook 'lisp-mode-hook 'enable-paredit-mode))

(progn
  (require 'dired)
  (require 'dired-x)
  (define-key dired-mode-map (kbd "r") 'wdired-change-to-wdired-mode)
  )

(global-whitespace-cleanup-mode)

(setq magit-completing-read-function 'magit-ido-completing-read)
(setq magit-last-seen-setup-instructions "1.4.0")
(setq magit-stage-all-confirm nil)
(setq magit-status-buffer-switch-function 'switch-to-buffer)
(setq magit-unstage-all-confirm nil)

(defun restless-rebuild ()
  (interactive)
  (compile "time sudo make -C /os"))

(defun os-restart-x ()
  (interactive)
  (compile "sudo systemctl restart display-manager"))

(global-set-key (kbd "C-c R") #'restless-rebuild)

(selectrum-mode 1)
(selectrum-prescient-mode 1)
(prescient-persist-mode 1)

(add-hook 'elixir-mode-hook 'lsp)

(progn
  (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
  (setq nov-variable-pitch t)
  (setq nov-text-width 60))

(progn
  (require 'which-key)
  (which-key-mode))

(setq lsp-keymap-prefix "C-M-l")

(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

(require 'nano-layout)
(require 'nano-theme-dark)

(setq font-lock-maximum-decoration nil)
(setq font-lock-maximum-size nil)

(require 'nano-faces)
(nano-faces)

(require 'nano-theme)
(nano-theme)

(require 'nano-session)
(require 'nano-modeline)

(setq org-return-follows-link t)

(windmove-default-keybindings)

(fset 'yes-or-no-p 'y-or-n-p)

(temp-buffer-resize-mode)
(setq temp-buffer-max-height 8)

(setq window-min-height 1)

(require 'uniquify)
(setq uniquify-buffer-name-style 'reverse
      uniquify-separator " • "
      uniquify-after-kill-buffer-p t
      uniquify-ignore-buffers-re "^\\*")

(when (fboundp 'tool-bar-mode)
  (blink-cursor-mode -1)
  (tool-bar-mode -1)
  (menu-bar-mode -1)
  (scroll-bar-mode -1))
