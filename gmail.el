(add-to-list 
 'load-path
 "/run/current-system/sw/share/emacs/site-lisp/mu4e")

(require 'gmail-params)

(require 'mu4e)
(require 'smtpmail)
(require 'pdf-occur)
(require 'pdf-tools)
(pdf-tools-install)

(setq mail-user-agent 'mu4e-user-agent
      message-send-mail-function 'smtpmail-send-it
      mu4e-sent-messages-behavior 'delete
      mu4e-get-mail-command "gmail-mbsync -a"
      mu4e-html2text-program "html2text -utf8 -width 72"

      mu4e-maildir-shortcuts '(("/INBOX" . ?i)
                               ("/[Gmail]/Sent Mail" . ?s)
                               ("/[Gmail]/All Mail" . ?a))

      mu4e-bookmarks
      '(("flag:unread AND NOT flag:trashed" "Unread messages" ?u)
        ("date:today..now" "Today's messages" ?t)
        ("date:7d..now" "Last 7 days" ?w)
        ("file:/pdf$/" "PDF messages" ?p))

      mu4e-drafts-folder "/[Gmail]/Drafts"
      mu4e-sent-folder "/[Gmail]/Sent Mail"
      mu4e-trash-folder "/[Gmail]/Trash"
      ;; mu4e-update-interval (* 60 1)
      mu4e-change-filenames-when-moving t
      mu4e-view-show-addresses t

      starttls-use-gnutls t
      smtpmail-starttls-credentials '(("smtp.googlemail.com" 587 nil nil))
      smtpmail-smtp-server "smtp.googlemail.com"
      smtpmail-smtp-service 587

      message-kill-buffer-on-exit t

      auth-sources '(password-store))

(provide 'gmail)
