;;; .emacs --- Personal Emacs init file  -*- lexical-binding: t; coding: utf-8 -*-

;; Updated 2026-11-02 for GNU Emacs 31 (Homebrew) and Erlang/OTP 29.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Packages (GNU ELPA, NonGNU ELPA and MELPA)
;;
;; Emacs 27+ calls `package-initialize' itself before this file is
;; loaded, so the packages in ~/.emacs.d/elpa are already available.
;; To update the packages:  M-x package-refresh-contents
;;                          M-x package-upgrade-all

(require 'package)
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

;; Packages that you installed yourself (used by `package-autoremove'
;; and `package-install-selected-packages').
(setq package-selected-packages '(erlang erlang-ts))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Erlang
;;
;; The Erlang mode is loaded from the OTP installation that `erl'
;; comes from (lib/tools-*/emacs), so the mode always matches the
;; installed OTP version, also after "brew upgrade".  If no OTP
;; installation is found, the MELPA package `erlang' is used.

(defun my-erlang-find-root ()
  "Return the root directory of the installed Erlang/OTP, or nil."
  (let ((erl (executable-find "erl")))
    (or (and erl
             ;; .../lib/erlang/bin/erl -> .../lib/erlang
             (let ((root (file-name-directory
                          (directory-file-name
                           (file-name-directory (file-truename erl))))))
               (and (file-directory-p (expand-file-name "lib" root))
                    (directory-file-name root))))
        (seq-find #'file-directory-p
                  '("/opt/homebrew/lib/erlang"
                    "/usr/local/lib/erlang"
                    "/usr/lib/erlang")))))

(defun my-erlang-find-emacs-dir (root)
  "Return the newest lib/tools-*/emacs directory below ROOT, or nil."
  (when root
    (car (last (sort (file-expand-wildcards
                      (expand-file-name "lib/tools-*/emacs" root))
                     #'string-version-lessp)))))

(defvar erlang-root-dir)                ; defined in erlang.el
(defvar treesit-language-source-alist)  ; defined in treesit.el
(declare-function sh-set-shell "sh-script")

(defvar my-erlang-root (my-erlang-find-root)
  "Root directory of the Erlang/OTP installation.")

(when my-erlang-root
  (setq erlang-root-dir my-erlang-root)
  (add-to-list 'exec-path (expand-file-name "bin" my-erlang-root))
  (let ((dir (my-erlang-find-emacs-dir my-erlang-root)))
    (when dir
      (add-to-list 'load-path dir))))

(if (require 'erlang-start nil t)
    (dolist (pattern '("\\.[eh]rl\\'"
                       "\\.yrl\\'"
                       "\\.xrl\\'"
                       "\\.escript\\'"
                       "\\.app\\'"
                       "\\.app\\.src\\'"
                       "\\.rel\\'"
                       "/rebar\\.config\\'"
                       "/sys\\.config\\'"))
      (add-to-list 'auto-mode-alist (cons pattern 'erlang-mode)))
  (display-warning 'init "Erlang mode not found (no OTP and no MELPA erlang)"))

;; Optional: the tree-sitter Erlang mode (MELPA package `erlang-ts').
;; To use it in place of the classic mode, change nil to t below.
;; If the grammar is missing or too old, do: M-x treesit-install-language-grammar RET erlang
(defvar my-erlang-use-tree-sitter nil
  "When non-nil, open Erlang files in `erlang-ts-mode'.")

(with-eval-after-load 'treesit
  (add-to-list 'treesit-language-source-alist
               '(erlang "https://github.com/WhatsApp/tree-sitter-erlang")))

(when (and my-erlang-use-tree-sitter
           (fboundp 'treesit-language-available-p)
           (treesit-language-available-p 'erlang)
           (require 'erlang-ts nil t))
  (add-to-list 'auto-mode-alist '("\\.[eh]rl\\'" . erlang-ts-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Gentoo ebuilds

(defun ebuild-mode ()
  "Shell script mode for Gentoo ebuild and eclass files."
  (interactive)
  (sh-mode)
  (sh-set-shell "bash")
  (setq-local tab-width 4))

(add-to-list 'auto-mode-alist '("\\.ebuild\\'" . ebuild-mode))
(add-to-list 'auto-mode-alist '("\\.eclass\\'" . ebuild-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Keys
;;
;; The delete key on the keyboard and on the keypad deletes the
;; character at the cursor (to the right), not the one before it.

(defun switch-to-other-buffer ()
  "Switch to the most recently used other buffer."
  (interactive)
  (switch-to-buffer (other-buffer)))

(keymap-global-set "<delete>"    #'delete-char)
(keymap-global-set "<kp-delete>" #'delete-char)
(keymap-global-set "C-x C-b"     #'switch-to-other-buffer)
(keymap-global-set "C-x g"       #'goto-line)
(keymap-global-set "C-x C-k"     #'copy-region-as-kill)
(keymap-global-set "M-F"         #'fill-paragraph)

;; Swedish keyboard on macOS
;;
;; On a Swedish Mac keyboard, these characters need the Option key:
;;
;;   @  Option-2          [  Option-8          {  Option-Shift-8
;;   $  Option-4          ]  Option-9          }  Option-Shift-9
;;   |  Option-7          \  Option-Shift-7    ~  Option-¨
;;
;; When the Option key is Meta, Emacs gets a Meta key in place of the
;; character.  Thus Emacs must translate these Meta keys back to the
;; characters.  The function below does this.
;;
;; - GUI Emacs: the left Option key is Meta, and the right Option key
;;   types the characters as usual.
;; - Emacs in Terminal: set Terminal > Settings > Profiles > Keyboard >
;;   "Use Option as Meta Key".
;; - If you do not want the left Option key as Meta, remove the
;;   `setq' of `ns-alternate-modifier' and `ns-right-alternate-modifier'.
;;
;; The translations replace these default Meta keys: M-2, M-4, M-7,
;; M-8 and M-9 (digit arguments; use C-2 and so on), M-/
;; (`dabbrev-expand'), M-( (`insert-parentheses') and M-)
;; (`move-past-close-and-reindent').
;;
;; Not translated: Option-< also types "|", but M-< is
;; `beginning-of-buffer'.  The old "Swedish - Legacy" layout types "@"
;; with Option-', which is not translated.

(defvar ns-alternate-modifier)        ; defined only in GUI Emacs on macOS
(defvar ns-right-alternate-modifier)

(defconst my-swedish-mac-meta-keys
  '(("M-2" . "@")
    ("M-4" . "$")
    ("M-7" . "|")
    ;; Shift-7 is "/", so Option-Shift-7 comes as M-/.
    ("M-/" . "\\")
    ("M-8" . "[")
    ("M-9" . "]")
    ;; Shift-8 is "(" and Shift-9 is ")".
    ("M-(" . "{")
    ("M-)" . "}")
    ;; "¨" is the key to the right of "å".
    ("M-\N{DIAERESIS}" . "~"))
  "Meta keys that are translated to characters for a Swedish Mac keyboard.
Each element is (KEY . CHARACTER).  KEY is the key that Emacs gets
when Option is Meta, and CHARACTER is the character that the Option
key types on a Swedish keyboard.")

(defun my-swedish-mac-keyboard ()
  "Set up Emacs for a Swedish keyboard on macOS.
In GUI Emacs, make the left Option key Meta and the right Option key
a normal Option key.  Then translate the Meta keys in
`my-swedish-mac-meta-keys' to the characters that the Option key
types on a Swedish keyboard."
  (interactive)
  (when (featurep 'ns)
    (setq ns-alternate-modifier 'meta
          ns-right-alternate-modifier 'none))
  (pcase-dolist (`(,key . ,char) my-swedish-mac-meta-keys)
    (keymap-set key-translation-map key char)))

(when (eq system-type 'darwin)
  (my-swedish-mac-keyboard))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Editing
;;
;; Font lock, transient mark mode and mouse wheel support are on by
;; default in current Emacs, so they are not set here.

;; Always end a file with a newline
(setq require-final-newline t)

;; Stop at the end of the file, do not add lines
(setq next-line-add-newlines nil)

;; No backup files ("~" files)
(setq make-backup-files nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Buffer stash
;;
;; C-x C-c first asks to save changed files, then saves all buffers
;; (also unsaved changes) to the hidden directory
;; ~/.emacs.d/.buffer-stash/, then stops Emacs.  At the next start,
;; the buffers are restored and the hidden directory is deleted.
;; The library is ~/.emacs.d/lisp/buffer-stash.el.

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(require 'buffer-stash)

(defun my-stash-buffers-and-quit (&optional arg)
  "Save and stash all buffers, then stop Emacs like \\[save-buffers-kill-terminal].
First Emacs asks \"Save file ...?\" for each changed file buffer.
Then it asks about each buffer that is too large to stash.  Then it
stashes the buffers and exits.  It does not ask the save questions a
second time, because the unsaved changes are in the stash.  With the
prefix ARG, all file buffers are saved without a question.

If you cancel the exit, the new stash is removed again, so that an
old state does not come back at the next start.  In an emacsclient
frame, Emacs does not stop, so no stash is made."
  (interactive "P")
  (cond
   ((frame-parameter nil 'client)
    ;; Only this client frame closes; the Emacs server continues.
    (save-buffers-kill-terminal arg))
   ((condition-case err
        (progn (buffer-stash-save arg) t)
      (error
       (message "Cannot stash buffers: %s" (error-message-string err))
       (sit-for 2)
       nil))
    ;; If Emacs exits, `kill-emacs' does not run the unwind form.
    ;; The unwind form runs only if you cancel the exit.
    (unwind-protect
        (when (buffer-stash-confirm-exit)
          (kill-emacs))
      (buffer-stash-discard-last-save)))
   (t
    ;; The stash failed: quit the usual way, with all its questions.
    (save-buffers-kill-terminal arg))))

(keymap-global-set "C-x C-c" #'my-stash-buffers-and-quit)

;; Restore the stashed buffers at startup.  The hook runs after Emacs
;; opens the files given on the command line, so it does not replace them.
;; When a stash waits, do not show the startup screen: it comes after
;; the hook and would cover the restored current buffer.
(when (buffer-stash-pending-p)
  (setq inhibit-startup-screen t))
(add-hook 'emacs-startup-hook #'buffer-stash-restore)

;;; .emacs ends here
