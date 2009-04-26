;; Red Hat Linux default .emacs initialization file

;; Are we running XEmacs or Emacs?
(defvar running-xemacs (string-match "XEmacs\\|Lucid" emacs-version))

; (load-file "~/emacs/erlang-start.el")
; (load-file "~/emacs/erlang.el")

(setq
 auto-mode-alist
 (append 
  '(("\\.erl$" . erlang-mode)
    ("\\.yrl$" . erlang-mode)
    ("\\.app$" . erlang-mode)
    ("\\.rel$" . erlang-mode)
    ("\\.ebuild$" . ebuild-mode)
    ("\\.eclass$" . ebuild-mode)
    )
  auto-mode-alist))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun ebuild-mode ()
  (shell-script-mode)
  (sh-set-shell "bash")
  (make-local-variable 'tab-width)
  (setq tab-width 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Set up the keyboard so the delete key on both the regular keyboard
;; and the keypad delete the character under the cursor and to the right
;; under X, instead of the default, backspace behavior.

(defun switch-to-other-buffer ()
  (interactive)
  (switch-to-buffer (other-buffer)))

(global-set-key [delete] 'delete-char)
(global-set-key [kp-delete] 'delete-char)
(global-set-key "\C-x\C-b" 'switch-to-other-buffer)
(global-set-key "\C-xg" 'goto-line)
(global-set-key "\C-x\C-k" 'copy-region-as-kill)
(global-set-key "\M-F" 'fill-paragraph)

;; Turn on font-lock mode for Emacs
(cond ((not running-xemacs)
       (global-font-lock-mode t)
))

;; Visual feedback on selections
(setq-default transient-mark-mode t)

;; Always end a file with a newline
(setq require-final-newline t)

;; Stop at the end of the file, not just add lines
(setq next-line-add-newlines nil)

;; No autobackups, no "~"-files (when non-nil)

(setq backup-inhibited t)

;; Enable wheelmouse support by default
(cond (window-system
       (mwheel-install)
))
