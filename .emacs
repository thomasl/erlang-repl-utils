;;; .emacs --- Personal Emacs init file  -*- lexical-binding: t -*-

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

;; Claude -- added these to bind keys properly with MacOS and Swedish keyboard
;; - new emacs appears to use these for other purposes now, not what we want
;; - also set Terminals > Settings > Profiles > Keyboard > "Use Option as Meta Key"
;;   to use left key,
;; - if NOT desired, do not set ns-alternate-modifier and ns-right-alternate-modifier
;;   (see below)

(defvar ns-alternate-modifier)        ; defined only in GUI Emacs on macOS
(defvar ns-right-alternate-modifier)

(when (eq system-type 'darwin)
  (when (featurep 'ns)
    (setq ns-alternate-modifier 'meta
          ns-right-alternate-modifier 'none))
  (keymap-set key-translation-map "M-8" "[")
  (keymap-set key-translation-map "M-9" "]")
  (keymap-set key-translation-map "M-(" "{")
  (keymap-set key-translation-map "M-)" "}"))

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

;;; .emacs ends here
