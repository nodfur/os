1;;; -*- lexical-binding: t -*-

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; 68 character box                                               ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;(set-frame-font "DM Mono-15" nil t)
(set-frame-font "DM Mono-12" nil t)
;(global-linum-mode)
(setq-default line-spacing 2)

(setq ag-arguments '("--smart-case" "--stats" "-W" "200"))

(progn
  ;(load "/src/wisp/etc/wisp.el")

  (defun setup-sexps ()
    (interactive)
    (lispy-mode -1)
    (paredit-mode 1))

  (add-hook 'clojure-mode-hook 'setup-sexps)
  (add-hook 'scheme-mode-hook 'setup-sexps)
  (add-hook 'emacs-lisp-mode-hook 'setup-sexps)
  (add-hook 'eval-expression-minibuffer-setup-hook 'setup-sexps)
  (add-hook 'ielm-mode-hook 'setup-sexps)
  (add-hook 'lisp-mode-hook 'setup-sexps)
  (add-hook 'lisp-interaction-mode-hook 'setup-sexps)
  (add-hook 'slime-repl-mode-hook 'setup-sexps)
  (add-hook 'wisp-mode-hook 'setup-sexps)

  (defun override-slime-del-key ()
    (define-key slime-repl-mode-map
      (read-kbd-macro paredit-backward-delete-key) nil))

  ;; (add-hook 'slime-repl-mode-hook 'override-slime-del-key)
  )

(global-page-break-lines-mode)

(setq display-time-24hr-format t
      display-time-day-and-date nil
      display-time-default-load-average nil)

(display-time-mode 0)
(display-battery-mode 1)

(set-fringe-mode 24)

(defun dark-mode ()
  (interactive)
  (load-theme 'zenburn t)
  (custom-set-faces
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.
   '(default ((t (:background "#000"))))
   '(fringe ((t (:background "#000"))))
   '(line-number ((t (:background "#000"))))
   '(linum ((t (:background nil :foreground "#333"))))

   '(treemacs-directory-face ((t (:foreground "#bbb" :height 0.8))))
   '(treemacs-file-face ((t (:foreground "#999" :height 0.8))))
   '(treemacs-git-modified-face ((t (:foreground "#9c9" :height 0.8))))
   '(treemacs-root-face ((t (:foreground "#9c9" :height 1.2))))
   '(treemacs-tags-face ((t (:foreground "#99b" :height 0.8))))

   '(org-code ((t (:family inherit))))
   '(org-block ((t (:background "#111"))))

   '(window-divider ((t (:foreground "#222"))))
   '(window-divider-first-pixel ((t (:foreground "#222"))))
   '(window-divider-last-pixel ((t (:foreground "#222")))))
  )

(dark-mode)

(setq window-divider-default-right-width 4)
(window-divider-mode 1)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background "#000" :height 130))))
 '(fixed-pitch ((t (:family "DM Mono" :background "#111"))))
 '(fringe ((t (:background "#000"))))
 '(line-number ((t (:background "#000"))))

 '(treemacs-directory-face ((t (:foreground "#bbb" :height 0.8))))
 '(treemacs-file-face ((t (:foreground "#999" :height 0.8))))
 '(treemacs-git-modified-face ((t (:foreground "#9c9" :height 0.8))))
 '(treemacs-root-face ((t (:foreground "#9c9" :height 1.2))))
 '(treemacs-tags-face ((t (:foreground "#99b" :height 0.8))))

 '(window-divider ((t (:foreground "#222"))))
 '(window-divider-first-pixel ((t (:foreground "#222"))))
 '(window-divider-last-pixel ((t (:foreground "#222")))))

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(default ((t (:background "#fff"))))
;;  '(fringe ((t (:background "#fff"))))
;;  '(line-number ((t (:background "#fff"))))

;;  '(treemacs-directory-face ((t (:foreground "#666" :height 0.9 :family "Source Sans Pro"))))
;;  '(treemacs-file-face ((t (:foreground "#000" :height 0.9 :family "Source Sans Pro"))))
;;  '(treemacs-git-modified-face ((t (:foreground "#383" :height 0.9 :family "Source Sans Pro"))))
;;  '(treemacs-git-untracked-face ((t (:foreground "#383" :slant italic :height 0.9 :family "Source Sans Pro"))))
;;  '(treemacs-git-ignored-face ((t (:foreground "#bbb" :height 0.9 :family "Source Sans Pro"))))
;;  '(treemacs-root-face ((t (:foreground "#9c9" :height 1.2 :family "Source Sans Pro"))))
;;  '(treemacs-tags-face ((t (:foreground "#558" :height 0.9 :family "Source Sans Pro"))))

;;  '(window-divider ((t (:foreground "#ddd"))))
;;  '(window-divider-first-pixel ((t (:foreground "#ddd"))))
;;  '(window-divider-last-pixel ((t (:foreground "#ddd")))))

(require 'cl-lib)

(require 'gmail nil t)

(setq inhibit-splash-screen t)

(setq frame-resize-pixelwise t)

(column-number-mode)
(setq tab-always-indent 'complete)

;(add-hook 'after-init-hook 'global-company-mode)

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

  ;; (eval-after-load 'tramp
  ;;   '(progn
  ;;      (add-to-list 'tramp-remote-path "/run/current-system/sw/bin")
  ;;      ;; (slime-setup
  ;;      ;;  '(slime-fancy
  ;;      ;;    slime-asdf
  ;;      ;;    slime-banner
  ;;      ;;    slime-media
  ;;      ;;    slime-buffer-streams
  ;;      ;;    slime-compiler-notes-tree
  ;;      ;;    slime-tramp))

  ;;      (push (list "^urbion$"
  ;;                  (lambda (emacs-filename)
  ;;                    (cl-subseq emacs-filename (length "/ssh:urbion:")))
  ;;                  (lambda (lisp-filename)
  ;;                    (concat "/ssh:urbion:" lisp-filename)))
  ;;            slime-filename-translations)))

  (progn
    (put 'downcase-region 'disabled nil)
    (put 'upcase-region 'disabled nil)))

(progn
  (setq-default indent-tabs-mode nil)
  (electric-indent-mode -1)

  (setq c-basic-offset 2)
  (setq css-indent-offset 2)
  (setq js-indent-level 2)
  (setq typescript-indent-level 2)
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

(defun magit-save ()
  (interactive)
  (save-some-buffers)
  (magit-git-command "git save"))

(progn
  (global-set-key (kbd "C-c f") 'projectile-find-file)
  (global-set-key (kbd "C-c s") 'magit-status)
  (global-set-key (kbd "C-c S") 'magit-save)

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

(defun my-zoom-in ()
  (interactive)
  (default-text-scale-increase))

(defun my-zoom-out ()
  (interactive)
  (default-text-scale-decrease))

(progn
  (global-set-key (kbd "C-x C-+") 'my-zoom-in)
  (global-set-key (kbd "C-x C--") 'my-zoom-out))

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

(defun compile-without-saving (command)
  (let ((x compile-command))
    (compile command)
    (setq compile-command x)))

(defun restless-rebuild ()
  (interactive)
  (compile-without-saving "sudo /os/rebuild"))

(defun os-restart-x ()
  (interactive)
  (compile-withou-saving "sudo systemctl restart display-manager"))

(global-set-key (kbd "C-c R") #'restless-rebuild)
(global-set-key (kbd "C-c c") #'recompile)

(selectrum-mode 1)
(selectrum-prescient-mode 1)
(prescient-persist-mode 1)

(add-hook 'elixir-mode-hook 'lsp)
(add-hook 'c-mode-hook 'lsp)
(add-hook 'zig-mode-hook 'lsp)
(add-hook 'typescript-mode-hook 'lsp)

(add-to-list 'auto-mode-alist '("\\.tsx?\\'" . typescript-mode))

(setq lsp-idle-delay 0.1)
(setq lsp-enable-on-type-formatting nil)
(setq lsp-before-save-edits nil)

;; (progn
;;   (add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))
;;   (setq nov-variable-pitch t)
;;   (setq nov-text-width 60))

(progn
  (require 'which-key)
  (which-key-mode))

(setq lsp-keymap-prefix "C-M-l")

(setq projectile-mode-line-prefix " ")
(require 'projectile)
(define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
(projectile-mode +1)

(setq font-lock-maximum-decoration nil)
(setq font-lock-maximum-size nil)

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

(setq inferior-lisp-program "sbcl")

(with-eval-after-load 'geiser-guile
  (add-to-list 'geiser-guile-load-path "/src/guix"))

(progn
  (global-set-key (kbd "C-:") 'avy-goto-char)
  (global-set-key (kbd "C-'") 'avy-goto-char-2)
  (global-set-key (kbd "M-g g") 'avy-goto-line)
  (global-set-key (kbd "M-g w") 'avy-goto-word-1)
  (global-set-key (kbd "M-g e") 'avy-goto-word-0)

  (setq avy-keys '(?a ?o ?e ?u ?h ?t ?n ?s)))

(setq image-auto-resize 'fit-width)

(add-hook 'image-minor-mode-hook
          (lambda ()
            (interactive)
            (setq left-fringe-width 0)
            (setq right-fringe-width 0)))

;; *

(quail-define-package
 "iast-postfix" "UTF-8" "InR<" t
  "Input method for Indic transliteration with postfix modifiers.

     Long vowels are dealt with by doubling.

     |                  | postfix | examples             |
     |------------------+---------+----------------------|
     | macron           |         | aa  -> ā    ee  -> ē |
     | diacritic below  | .       | d.  -> ḍ    rr. -> ṝ |
     | diacritic above  | '       | s'  -> ś    n'  -> ṅ |
     | tilde            | ~       | n~  -> ñ             |
  "
  nil t nil nil nil nil nil nil nil nil t)

(setq doom-modeline-buffer-encoding nil)

;; (doom-modeline-def-modeline 'my-simple-line
;;   '(bar matches buffer-info remote-host
;;         buffer-position parrot selection-info)
;;   '(misc-info battery minor-modes input-method
;;               major-mode process vcs checker))

;; (doom-modeline-set-modeline 'my-simple-line 'default)

(defun doom-modeline--font-height ()
  "Calculate the actual char height of the mode-line."
  (let ((height (face-attribute 'mode-line :height)))
    ;; WORKAROUND: Fix tall issue of 27 on Linux
    ;; @see https://github.com/seagle0128/doom-modeline/issues/271
    (round
     (* (if (or (<= doom-modeline-height 0)
                (and (>= emacs-major-version 27)
                     (not (eq system-type 'darwin))))
            0.65
                                        ;(if doom-modeline-icon 1.68 1.25)
          )
        (cond ((integerp height) (/ height 10))
              ((floatp height) (* height (frame-char-height)))
              (t (frame-char-height)))))))

;; (progn
;;   (doom-modeline-mode 0)
;;   (doom-modeline-mode 1))

;; (require 'treemacs)

;; (setq treemacs-user-mode-line-format 'none)

;; (setq treemacs-indentation 1)
;; (setq treemacs-width 30)
;; (setq treemacs-no-png-images t)

;; (treemacs)
;; (treemacs-follow-mode)
;; (treemacs-tag-follow-mode)
;; (treemacs-git-mode 'deferred)
;; (treemacs-filewatch-mode)

;; (defun my-treemacs-hook ()
;;   (interactive)
;;   (setq-local line-spacing 0))

;; (treemacs-resize-icons 22)

;; (with-current-buffer (treemacs-get-local-buffer)
;;   (my-treemacs-hook))

;; (add-hook 'treemacs-mode-hook 'my-treemacs-hook)

(server-start)
(require 'org-protocol)

(defun narrow-to-paragraph ()
  (interactive)
  (setq cursor-type nil)
  (mark-paragraph)
  (next-line)
  (narrow-to-region (point) (mark))
  (deactivate-mark))

(defun narrow-to-next-paragraph ()
  (interactive)
  (widen)
  (forward-paragraph)
  (narrow-to-paragraph)
  (deactivate-mark))

(global-set-key (kbd "C-c P") 'narrow-to-paragraph)
(global-set-key (kbd "C-c N") 'narrow-to-next-paragraph)

(setq compilation-scroll-output 'first-error)

(progn
  (require 'ansi-color)
  (defun colorize-compilation-buffer ()
    (ansi-color-apply-on-region compilation-filter-start (point)))
  (add-hook 'compilation-filter-hook 'colorize-compilation-buffer))

(defun make-transparent ()
  (interactive)
  (set-frame-parameter nil 'alpha 90))

(defun simple-mode-line-render (left right)
  "Return a string of `window-width' length containing LEFT, and RIGHT
 aligned respectively."
  (let* ((available-width (- (window-width) (length left) 2)))
    (format (format " %%s %%%ds " available-width) left right)))

;; (setq-default mode-line-format
;;               '(" %* " mode-line-buffer-identification " " mode-line-modes
;;                 "" "(%l:%c)"))

(defun sha256-region ()
  (interactive)
  (save-excursion
    (shell-command-on-region
     (mark) (point)
     "echo -n 0x; sha256sum | head -c6" t t)))

(setq org-cite-insert-processor 'citar
      org-cite-follow-processor 'citar
      org-cite-activate-processor 'citar)

(require 'telega)
(setq telega-tdlib-max-version "1.8.1")

(require 'zig-mode)
(defun zig-format-buffer ()
  "Format the current buffer using the zig fmt."
  (interactive)
  (when buffer-file-name
    (let ((file-buffer (current-buffer)))
      (let ((fmt-buffer (generate-new-buffer "*zig-fmt*")))
        (set-process-sentinel
         (start-process "zig-fmt"
                        fmt-buffer
                        zig-zig-bin
                        "fmt"
                        buffer-file-name)
         (lambda (process _e)
           (if (> (process-exit-status process) 0)
               (when zig-format-show-buffer
                 (progn
                   (pop-to-buffer fmt-buffer)
                   (when zig-ansi-color-for-format-errors
                     (ansi-color-apply-on-region (point-min) (point-max)))
                   (compilation-mode)
                   (when zig-return-to-buffer-after-format
                     (pop-to-buffer file-buffer))))
             (revert-buffer :ignore-auto :noconfirm)
             (kill-buffer fmt-buffer))))))))

(defun fix-stuff ()
  (interactive)

  ;; This doesn't work until after stuff is already loaded, so I keep it
  ;; down here for easy evaluation...
  (delight
   '((emacs-lisp-mode "Elisp" :major)
     (eldoc-mode nil t)
     (paredit-mode nil t)
     (company-mode nil t)
     (which-key-mode nil t)
     (whitespace-cleanup-mode nil t)
     (projectile-mode nil t)
     (flycheck-mode nil t)
     (lsp-mode nil t)
     (abbrev-mode nil t)
     (page-break-lines-mode nil t))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values '((lsp-enabled-clients deno-ls)
                                (lsp-disabled-clients ts-ls))))

(when (string-equal system-type "darwin")
  (setq ring-bell-function 'ignore)
  (setq mac-command-modifier 'meta)
  (setq mac-option-modifier 'super))

