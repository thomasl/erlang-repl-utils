;;; buffer-stash.el --- Stash all buffers in a hidden area and restore them  -*- lexical-binding: t; -*-

;; Author: Thomas Lindgren
;; Version: 1.1
;; Package-Requires: ((emacs "27.1"))
;; Keywords: convenience, files

;; Install in ~/.emacs.d/lisp/
;; See also .emacs for usage

;;; Commentary:

;; This library saves the state of all buffers to a hidden directory
;; and restores it later.  It does not change your real files.  The
;; usual use: C-x C-c stashes all buffers and stops Emacs, and the next
;; start of Emacs restores them.  See "Installation" below.
;;
;; Contents of this commentary:
;;   Commands and functions
;;   Installation
;;   What occurs when you quit
;;   What is saved
;;   Point
;;   Current buffer
;;   Size limit
;;   The stash directory
;;   Options
;;   Limits
;;
;;;; Commands and functions
;;
;; `buffer-stash-save'      Offers to save changed files, then saves
;;                          all buffers to `buffer-stash-directory'.
;;                          With a prefix argument, saves all changed
;;                          files without a question.
;; `buffer-stash-restore'   Restores all buffers from that directory.
;;                          If all buffers restore without errors, it
;;                          deletes the directory.  If an error occurs,
;;                          it keeps the directory, so that no data is
;;                          lost, and shows a warning.
;; `buffer-stash-confirm-exit'
;;                          Asks the last questions before Emacs exits
;;                          after a stash.  Returns non-nil if Emacs
;;                          can exit.
;; `buffer-stash-discard-last-save'
;;                          Deletes the stash that the last save made.
;;                          Use it when Emacs did not exit after a save.
;; `buffer-stash-unstashed-changes'
;;                          Returns the file buffers whose unsaved
;;                          changes are not saved and not stashed.
;;
;;;; Installation
;;
;; Put this file in a directory in `load-path', for example
;; ~/.emacs.d/lisp/.  Then put this in your init file:
;;
;;   (add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;;   (require 'buffer-stash)
;;
;;   (defun my-stash-buffers-and-quit (&optional arg)
;;     "Stash all buffers, then stop Emacs."
;;     (interactive "P")
;;     (if (frame-parameter nil 'client)
;;         ;; In an emacsclient frame, Emacs does not stop: no stash.
;;         (save-buffers-kill-terminal arg)
;;       (buffer-stash-save arg)
;;       (unwind-protect
;;           (when (buffer-stash-confirm-exit)
;;             (kill-emacs))
;;         ;; `kill-emacs' does not run this form.  It runs only if
;;         ;; Emacs did not exit, for example after C-g or "no".
;;         (buffer-stash-discard-last-save))))
;;
;;   (keymap-global-set "C-x C-c" #'my-stash-buffers-and-quit)
;;   (add-hook 'emacs-startup-hook #'buffer-stash-restore)
;;
;; Use `emacs-startup-hook' for the restore, not a direct call.  The
;; hook runs after Emacs opens the files on the command line, so the
;; restore does not replace them.
;;
;; Do not call `save-buffers-kill-terminal' after `buffer-stash-save'.
;; It asks the save questions a second time, and it asks "Modified
;; buffers exist; exit anyway?" also when the changes are in the stash.
;;
;;;; What occurs when you quit
;;
;; With the installation above, C-x C-c does these steps:
;;
;;  1. Save.  Emacs asks "Save file ...?" for each file buffer with
;;     unsaved changes (`save-some-buffers').  These are the usual
;;     questions and answers of Emacs.  With C-u C-x C-c, all files are
;;     saved without a question.  Set `buffer-stash-offer-save' to nil
;;     to skip this step.
;;  2. Size check.  For each buffer that still has unsaved text and is
;;     larger than `buffer-stash-max-size', the library asks if you
;;     want to stash it anyway.  See "Size limit".
;;  3. Stash.  The library writes the stash.  Nothing is written before
;;     this step, except the files that you saved in step 1.
;;  4. Last checks (`buffer-stash-confirm-exit'):
;;     - If file changes are not saved and not stashed, Emacs asks
;;       "Unsaved changes are not saved or stashed (...); exit anyway?".
;;       This occurs only if you answered no in step 1 and in step 2.
;;     - If processes run and `confirm-kill-processes' is non-nil,
;;       Emacs asks if it can kill them.
;;     - Emacs runs `kill-emacs-query-functions'.
;;     - If `confirm-kill-emacs' is non-nil and no question was asked
;;       in this step, Emacs asks "Really exit Emacs?".
;;  5. Exit.  Emacs stops (`kill-emacs').
;;
;; If you type C-g at a question, or answer no in step 4, Emacs does
;; not stop.  If the stash was already written, it is deleted again
;; (`buffer-stash-discard-last-save').  Files that you saved in step 1
;; stay saved.
;;
;;;; What is saved
;;
;; - File buffers: the file name and point.  If the buffer has unsaved
;;   changes, the full buffer text is also saved.  On restore, the file
;;   is opened again and the unsaved text is put back.  The real file
;;   on disk is not written.  If you saved the file after the stash,
;;   the buffer is not marked as modified.
;; - Dired buffers: the directory name and point.
;; - Other buffers with text (for example *scratch*): the text, the
;;   major mode, `default-directory', point and the modified flag.  If
;;   the major mode cannot start, `fundamental-mode' is used.
;;
;; What is not saved: internal buffers (name starts with a space),
;; buffers with a live process (shells, terminals), empty buffers that
;; do not visit a file, and the buffers that match
;; `buffer-stash-exclude-names' or `buffer-stash-exclude-modes'.
;;
;; On restore, a file buffer whose file was deleted is skipped if it
;; had no unsaved changes, because no data is lost.  This is not an
;; error.
;;
;;;; Point
;;
;; For each buffer, the library saves the position, the line and
;; column, and some text before and after point.  On restore:
;; - If the text is the same as at save time, point goes back to the
;;   same position.
;; - If a file changed on disk after the save, the library searches
;;   for the saved text near the saved line.  If it cannot find the
;;   text, it uses the saved line and column.
;; - In Dired, point goes back to the same file name.
;; - Point is also set in all windows that show the buffer.
;;
;; The variable `buffer-stash-context-length' (default 40) sets how
;; many characters before point and after point are saved.
;; - A larger value gives a more unique search text.  Use it if your
;;   files have many lines that are the same.
;; - A smaller value gives a better chance to find point when the
;;   text near point changed.
;; - The value 0 turns off the search.  Then point goes back to the
;;   saved line and column if the file changed.
;; A side of the text that is shorter than 8 characters is not used
;; for the search, because it can match in too many places.
;; To change the value:
;;   M-x customize-variable RET buffer-stash-context-length RET
;; or put this in your init file:
;;   (setq buffer-stash-context-length 60)
;;
;;;; Current buffer
;;
;; The save also records which buffer the selected window shows.
;; After the restore, that buffer is shown in the selected window.  If
;; you start Emacs with a file on the command line, that file stays in
;; the window.  Set `buffer-stash-select-current' to nil to turn this
;; off.
;;
;;;; Size limit
;;
;; `buffer-stash-max-size' (default 10 million) is the largest buffer
;; text, in bytes, that the save writes without a question.  A
;; character that is not ASCII uses more than one byte.  The check
;; applies only to buffers whose text is saved: buffers that do not
;; visit a file, and file buffers with unsaved changes.  A file that
;; you saved in step 1 has no unsaved text, so it gets no question.
;; Set the variable to nil for no limit.
;;
;; For each buffer that is larger, the save asks one question, as
;; Emacs does when it asks to save buffers:
;;
;;   Buffer big.log is too large to stash (12000000 bytes, limit
;;   10000000).  Stash it anyway? (y, n, !, ., q, or C-h)
;;
;;   y          Stash this buffer.
;;   n          Do not stash the text of this buffer.
;;   !          Stash this buffer and all the remaining large buffers.
;;   .          Stash this buffer, but not the remaining large buffers.
;;   q or RET   Do not stash this buffer or the remaining large buffers.
;;   C-h        Show help.
;;   C-g        Stop the save.  Nothing is stashed.  If the save was
;;              started by a quit command, Emacs does not stop.
;;
;; All size questions come before the stash is written.  If you
;; answer no:
;; - A buffer that does not visit a file is not saved.  Emacs does not
;;   ask about it again: its text is lost when Emacs exits.
;; - For a file buffer with unsaved changes, only the file name and
;;   point are saved.  The restore opens the file as it is on disk.
;;   Before the exit, `buffer-stash-confirm-exit' asks if you want to
;;   exit without these changes.
;; The names of these buffers are in `buffer-stash-last-too-large'.
;;
;; Set `buffer-stash-ask-too-large' to nil to skip the questions; then
;; large buffers are not stashed and a warning shows their names.  In
;; batch mode, the save does not ask (also not in step 1).
;;
;;;; The stash directory
;;
;; The stash is in `buffer-stash-directory' (default
;; ~/.emacs.d/.buffer-stash/).  Only you can read it (mode 700),
;; because it can contain private text.  It has these files:
;;   index.eld    A list of the saved buffers (Lisp data).  It is read
;;                with `read'; it is not evaluated.
;;   NNNN.txt     The text of one buffer, in the coding system
;;                utf-8-emacs, which can encode all Emacs text.
;;
;; The save writes to the directory ".buffer-stash.tmp" first, and
;; then renames it.  Thus, a stash is never half written.  If a stash
;; is already there when you save (for example because a restore
;; failed), it is moved to ".buffer-stash.previous", so that its data
;; is not lost.  An older ".previous" directory is replaced.
;;
;; If a restore fails, the stash stays.  Correct the problem, then do
;; M-x buffer-stash-restore again.
;;
;;;; Options
;;
;; Use M-x customize-group RET buffer-stash RET, or `setq':
;;
;;   `buffer-stash-directory'       Where the stash is.
;;   `buffer-stash-offer-save'      Ask to save files first (default t).
;;   `buffer-stash-max-size'        Size limit in bytes (default 10000000,
;;                                  nil for no limit).
;;   `buffer-stash-ask-too-large'   Ask about large buffers (default t).
;;   `buffer-stash-context-length'  Text saved around point (default 40).
;;   `buffer-stash-select-current'  Show the current buffer after the
;;                                  restore (default t).
;;   `buffer-stash-exclude-names'   Regexps of buffer names not saved.
;;   `buffer-stash-exclude-modes'   Major modes of buffers not saved
;;                                  (only buffers without a file).
;;
;; A change takes effect at the next save or restore.
;;
;;;; Limits
;;
;; - The window layout, the mark, the scroll position, undo history
;;   and text properties (for example colors that a mode did not make)
;;   are not saved.
;; - Buffers with processes (shells, terminals, REPLs) are not saved.
;; - There is one stash for each `buffer-stash-directory'.  If you run
;;   two Emacs sessions, the session that quits last writes the stash
;;   (the earlier stash moves to ".previous").
;; - In an emacsclient frame, C-x C-c closes only the frame (see the
;;   installation example), so no stash is made.  Stop the Emacs
;;   server with C-x C-c in a frame that is not a client frame, or
;;   call `buffer-stash-save' yourself before `kill-emacs'.

;;; Code:

(require 'cl-lib)

(defgroup buffer-stash nil
  "Stash all buffers in a hidden area and restore them."
  :group 'convenience
  :prefix "buffer-stash-")

(defcustom buffer-stash-directory
  (locate-user-emacs-file ".buffer-stash/")
  "Hidden directory where `buffer-stash-save' puts the buffers."
  :type 'directory)

(defcustom buffer-stash-exclude-names
  '("\\` "
    "\\`\\*Messages\\*\\'"
    "\\`\\*Completions\\*\\'"
    "\\`\\*Backtrace\\*\\'"
    "\\`\\*Warnings\\*\\'"
    "\\`\\*Compile-Log\\*\\'"
    "\\`\\*Async-native-compile-log\\*\\'"
    "\\`\\*Native-compile-Log\\*\\'"
    "\\`\\*Buffer List\\*\\'"
    "\\`\\*GNU Emacs\\*\\'")
  "Regexps for buffer names that `buffer-stash-save' does not save."
  :type '(repeat regexp))

(defcustom buffer-stash-exclude-modes
  '(special-mode compilation-mode comint-mode eshell-mode term-mode
    Info-mode help-mode Buffer-menu-mode ibuffer-mode
    completion-list-mode minibuffer-mode minibuffer-inactive-mode)
  "Major modes (and modes derived from them) that are not saved.
This applies only to buffers that do not visit a file.  Buffers in
these modes show generated data that cannot be restored as text."
  :type '(repeat symbol))

(defcustom buffer-stash-context-length 40
  "Number of characters before and after point that are saved.
On restore, this text is used to find point again if the file
changed on disk after the save.  The library searches for the text
near the saved line.  If it cannot find the text, it uses the saved
line and column.

A larger value gives a more unique search text.  A smaller value
gives a better chance to find point when the text near point
changed.  The value 0 turns off the search.  A side of the text that
is shorter than `buffer-stash--min-context' characters is not used.

A change takes effect at the next `buffer-stash-save'."
  :type 'natnum)

(defcustom buffer-stash-max-size 10000000
  "Largest buffer text, in bytes, that `buffer-stash-save' writes to the stash.
The size is the number of bytes in the full buffer text (narrowing
is ignored).  This is the size of the text in the stash file.  A
character that is not ASCII uses more than one byte.

The check applies only to buffers whose text is saved: buffers that
do not visit a file, and file buffers with unsaved changes.  A file
buffer without changes is always saved, because only its file name
is saved.

If a buffer is too large, `buffer-stash-save' asks if you want to
stash it anyway (see `buffer-stash-ask-too-large').  If you answer no:
- A buffer that does not visit a file is not saved.
- For a file buffer with unsaved changes, only the file name and
  point are saved.  The restore opens the file as it is on disk,
  without the unsaved changes.
The names of the buffers that are not stashed are in
`buffer-stash-last-too-large'.

The value nil means that there is no limit."
  :type '(choice (const :tag "No limit" nil) natnum))

(defcustom buffer-stash-offer-save t
  "If non-nil, `buffer-stash-save' first offers to save file buffers.
It calls `save-some-buffers', which asks \"Save file ...?\" for each
file buffer with unsaved changes, as Emacs does when it quits.  A
buffer that you save does not need its text in the stash, so it does
not get a size question.  In batch mode, the save does not ask."
  :type 'boolean)

(defcustom buffer-stash-ask-too-large t
  "If non-nil, ask before a buffer that is too large is not stashed.
`buffer-stash-save' asks one question for each buffer whose text is
larger than `buffer-stash-max-size', as `save-some-buffers' does.
If you answer y, the buffer is stashed anyway.  If nil, or in batch
mode, large buffers are not stashed and a warning shows their names."
  :type 'boolean)

(defcustom buffer-stash-select-current t
  "If non-nil, the restore shows the buffer that was current at save time.
The buffer is shown in the selected window.  This does not occur if
the selected window already shows a file when the restore starts,
for example a file that you gave on the command line."
  :type 'boolean)

(defconst buffer-stash--min-context 8
  "Shortest context string that is used to search for point.
A shorter string can match in too many places.")

(defconst buffer-stash--index-name "index.eld"
  "Name of the index file in the stash directory.")

(defconst buffer-stash--format-version 1
  "Version of the stash format.")

(defconst buffer-stash--coding 'utf-8-emacs-unix
  "Coding system for stash files.  It can encode all Emacs text.")

(defvar buffer-stash-last-too-large nil
  "Names of the buffers that the last `buffer-stash-save' did not save fully.
These buffers are larger than `buffer-stash-max-size'.")

(defvar buffer-stash--last-stashed-text nil
  "Buffers whose text the last `buffer-stash-save' wrote to the stash.")

(defvar buffer-stash--last-save nil
  "Data about the last save, used by `buffer-stash-discard-last-save'.
A cons (FINAL-DIR . PREVIOUS-DIR-OR-NIL).")

;;;; Helpers

(defun buffer-stash--dir ()
  "Return the stash directory as an absolute name without a final slash."
  (directory-file-name (expand-file-name buffer-stash-directory)))

(defun buffer-stash--kind (buf)
  "Return how BUF is saved: `file', `dired', `text', or nil to skip it."
  (with-current-buffer buf
    (cond
     ((cl-some (lambda (re) (string-match-p re (buffer-name buf)))
               buffer-stash-exclude-names)
      nil)
     ((get-buffer-process buf) nil)
     (buffer-file-name 'file)
     ((derived-mode-p 'dired-mode) 'dired)
     ((cl-some (lambda (mode) (derived-mode-p mode))
               buffer-stash-exclude-modes)
      nil)
     ((> (buffer-size) 0) 'text))))

(defun buffer-stash--write-text (buf file)
  "Write the full text of BUF to FILE.
Text properties, narrowing and buffer-local write hooks are ignored."
  (with-temp-buffer
    (insert-buffer-substring-no-properties
     buf
     (with-current-buffer buf (save-restriction (widen) (point-min)))
     (with-current-buffer buf (save-restriction (widen) (point-max))))
    (let ((coding-system-for-write buffer-stash--coding))
      ;; A VISIT value that is not t, nil or a string stops the
      ;; "Wrote file" message.
      (write-region (point-min) (point-max) file nil 'silent))))

(defun buffer-stash--read-text (file)
  "Return the text in stash FILE as a string."
  (unless (file-readable-p file)
    (error "Stash data file is missing: %s" file))
  (with-temp-buffer
    (let ((coding-system-for-read buffer-stash--coding))
      (insert-file-contents file))
    (buffer-string)))

(defun buffer-stash--read-index (file)
  "Read and return the index data in FILE."
  (with-temp-buffer
    (let ((coding-system-for-read buffer-stash--coding))
      (insert-file-contents file))
    (goto-char (point-min))
    (read (current-buffer))))

(defun buffer-stash--same-as-file-p (file)
  "Return non-nil if the current buffer text is the same as FILE on disk."
  (and (file-readable-p file)
       (let ((buf (current-buffer))
             (coding buffer-file-coding-system)
             (case-fold-search nil))
         (with-temp-buffer
           (let ((coding-system-for-read coding)
                 (disk (current-buffer)))
             (insert-file-contents file)
             (zerop (with-current-buffer buf
                      (save-restriction
                        (widen)
                        (compare-buffer-substrings
                         buf nil nil disk nil nil)))))))))

(defun buffer-stash--set-mode (mode)
  "Turn on major MODE in the current buffer.
If MODE fails, use `fundamental-mode'.  The text stays in the buffer."
  (when (and mode (fboundp mode) (not (eq mode major-mode)))
    (condition-case err
        (funcall mode)
      (error
       (message "buffer-stash: cannot start %s in %s (%s); using fundamental-mode"
                mode (buffer-name) (error-message-string err))
       (fundamental-mode)))))

;;;; Point

(declare-function dired-get-filename "dired")
(declare-function dired-goto-file "dired")

(defun buffer-stash--point-data ()
  "Return a plist with the location of point in the current buffer.
The plist has the position, the line and column, the text just
before and after point and, in Dired, the file name at point."
  (save-restriction
    (widen)
    (let ((pt (point))
          (n buffer-stash-context-length))
      (list :point pt
            :line (line-number-at-pos pt t)
            :column (current-column)
            :before (buffer-substring-no-properties (max (point-min) (- pt n)) pt)
            :after (buffer-substring-no-properties pt (min (point-max) (+ pt n)))
            :dired-file (and (derived-mode-p 'dired-mode)
                             (ignore-errors (dired-get-filename nil t)))))))

(defun buffer-stash--line-pos (line column)
  "Return the position of LINE and COLUMN, in the accessible buffer."
  (save-excursion
    (goto-char (point-min))
    (forward-line (1- line))
    (move-to-column (or column 0))
    (point)))

(defun buffer-stash--context-at-p (pos before after)
  "Return non-nil if the text BEFORE is just before POS and AFTER just after."
  (and (<= (point-min) (- pos (length before)))
       (<= (+ pos (length after)) (point-max))
       (string= before (buffer-substring-no-properties (- pos (length before)) pos))
       (string= after (buffer-substring-no-properties pos (+ pos (length after))))))

(defun buffer-stash--nearest-match (string origin)
  "Return the start of the match of STRING that is nearest to ORIGIN, or nil.
Return nil also if STRING is shorter than `buffer-stash--min-context'."
  (when (>= (length string) buffer-stash--min-context)
    (let ((case-fold-search nil) fwd bwd)
      (save-excursion
        (goto-char origin)
        (when (search-forward string nil t)
          (setq fwd (match-beginning 0)))
        (goto-char (min (point-max) (+ origin (length string))))
        (when (search-backward string nil t)
          (setq bwd (point))))
      (cond ((and fwd bwd) (if (<= (- origin bwd) (- fwd origin)) bwd fwd))
            (t (or fwd bwd))))))

(defun buffer-stash--find-point (entry exact)
  "Return the position in the current buffer that ENTRY describes.
If EXACT is non-nil, the text is the same as at save time, so use the
saved position.  If not, the text can be different.  Then try these
steps, in sequence:
 1. The saved position, if the text around it did not change.
 2. The nearest match of the text before and after point together.
 3. The nearest match of the text after point.
 4. The nearest match of the text before point.
 5. The saved line and column.
\"Nearest\" means nearest to the saved line and column."
  (let* ((pos (or (plist-get entry :point) (point-min)))
         (line (plist-get entry :line))
         (before (or (plist-get entry :before) ""))
         (after (or (plist-get entry :after) ""))
         (estimate (if line
                       (buffer-stash--line-pos line (plist-get entry :column))
                     pos))
         m)
    (cond
     (exact pos)
     ((and (or (> (length before) 0) (> (length after) 0))
           (buffer-stash--context-at-p pos before after))
      pos)
     ((setq m (buffer-stash--nearest-match (concat before after) estimate))
      (+ m (length before)))
     ((buffer-stash--nearest-match after estimate))
     ((setq m (buffer-stash--nearest-match before estimate))
      (+ m (length before)))
     (t estimate))))

(defun buffer-stash--set-point (buf pos)
  "Move point in BUF to POS, also in all windows that show BUF."
  (with-current-buffer buf
    (goto-char (max (point-min) (min pos (point-max))))
    (dolist (win (get-buffer-window-list buf nil t))
      (set-window-point win (point)))))

(defun buffer-stash--restore-point (buf entry exact)
  "Move point in BUF to the place that ENTRY describes.
EXACT has the same meaning as in `buffer-stash--find-point'."
  (buffer-stash--set-point
   buf (with-current-buffer buf (buffer-stash--find-point entry exact))))

;;;; Current buffer

(defun buffer-stash--current-buffer ()
  "Return the buffer that you use now: the buffer of the selected window.
If the minibuffer is selected, use the window that was selected
before it."
  (let ((win (selected-window)))
    (when (and (window-minibuffer-p win) (minibuffer-selected-window))
      (setq win (minibuffer-selected-window)))
    (window-buffer win)))

(defun buffer-stash--select (buf shown)
  "Show BUF in the selected window, if that is necessary and permitted.
SHOWN is the buffer that the selected window showed before the
restore.  If SHOWN visits a file, do not change the window."
  (when (and buffer-stash-select-current
             (buffer-live-p buf)
             (not (and (buffer-live-p shown)
                       (buffer-local-value 'buffer-file-name shown))))
    (let ((pos (with-current-buffer buf (point))))
      (switch-to-buffer buf nil t)
      ;; `switch-to-buffer' can use an old window point; use the
      ;; restored point.
      (set-window-point (selected-window) pos))))

;;;; Save

(defun buffer-stash--size (buf)
  "Return the size of the full text of BUF in bytes."
  (with-current-buffer buf
    (save-restriction
      (widen)
      (- (position-bytes (point-max)) (position-bytes (point-min))))))

(defun buffer-stash--too-large-p (buf)
  "Return non-nil if the text of BUF is larger than `buffer-stash-max-size'."
  (and buffer-stash-max-size
       (> (buffer-stash--size buf) buffer-stash-max-size)))

(defun buffer-stash--text-saved-p (buf kind)
  "Return non-nil if the text of BUF (of type KIND) is written to the stash."
  (or (eq kind 'text)
      (and (eq kind 'file) (buffer-modified-p buf))))

(defun buffer-stash--ask-too-large (bufs)
  "Ask for each buffer in BUFS if you want to stash it anyway.
BUFS are buffers that are larger than `buffer-stash-max-size'.
Ask one question for each buffer, as `save-some-buffers' does.  The
answers are the same as for that function:
  y         Stash this buffer.
  n         Do not stash this buffer.
  !         Stash this and all the remaining buffers.
  period    Stash this buffer, but not the remaining buffers.
  q or RET  Do not stash this or the remaining buffers.
  ?         Show help.
Type \\[keyboard-quit] to stop the save; then nothing is written.
Do not ask if `buffer-stash-ask-too-large' is nil or if Emacs runs in
batch mode.  Return the list of buffers to stash anyway."
  (let (yes)
    (when (and bufs buffer-stash-ask-too-large (not noninteractive))
      (map-y-or-n-p
       (lambda (buf)
         (format "Buffer %s is too large to stash (%d bytes, limit %d).  Stash it anyway? "
                 (buffer-name buf) (buffer-stash--size buf) buffer-stash-max-size))
       (lambda (buf) (push buf yes))
       bufs
       '("buffer" "buffers" "stash anyway")))
    yes))

(defun buffer-stash--save-one (buf kind n dir allowed)
  "Save BUF of type KIND as entry number N in DIR.  Return its index entry.
If BUF is too large and ALLOWED is nil, do not write its text.
Return nil if BUF is not saved at all."
  (with-current-buffer buf
    (let ((entry (append
                  (buffer-stash--point-data)
                  (list :kind kind
                       :name (buffer-name)
                       :mode major-mode
                       :directory (expand-file-name default-directory)
                       :coding buffer-file-coding-system
                       :modified (buffer-modified-p)))))
      (when (eq kind 'file)
        (setq entry (plist-put entry :file buffer-file-name)))
      (if (not (buffer-stash--text-saved-p buf kind))
          entry
        (if (and (not allowed) (buffer-stash--too-large-p buf))
            (progn
              (push (buffer-name) buffer-stash-last-too-large)
              ;; A file buffer is still opened again, without the
              ;; unsaved text.  A buffer without a file is not saved.
              (and (eq kind 'file) entry))
          (let ((data (format "%04d.txt" n)))
            (buffer-stash--write-text buf (expand-file-name data dir))
            (push buf buffer-stash--last-stashed-text)
            (plist-put entry :data data)))))))

;;;###autoload
(defun buffer-stash-save (&optional arg)
  "Save all buffers to the hidden area `buffer-stash-directory'.
Steps:
 1. If `buffer-stash-offer-save' is non-nil, ask \"Save file ...?\" for
    each file buffer with unsaved changes (`save-some-buffers').  With
    the prefix ARG, save all of them without a question, as
    \\[universal-argument] \\[save-buffers-kill-terminal] does.
 2. Ask about each buffer that is too large (see below).
 3. Write the stash.

The stash does not change real files.  The data is first written to a
temporary directory, and then moved into place.  If an older stash
is already there (for example, because a restore failed), it is
moved to a directory with the suffix \".previous\".

If the text of a buffer is larger than `buffer-stash-max-size',
first ask if you want to stash it anyway (see
`buffer-stash--ask-too-large').  All questions come before the
save writes data.  If you type \\[keyboard-quit] at a question, the stash is not
written and the `quit' signal continues to the caller.  (Files that
you saved before that stay saved.)

Return the number of saved buffers."
  (interactive "P")
  (when (and buffer-stash-offer-save (not noninteractive))
    ;; PRED t: ask about all buffers, as `save-buffers-kill-emacs' does.
    (save-some-buffers arg t))
  (let* ((final (buffer-stash--dir))
         (tmp (concat final ".tmp"))
         (old (concat final ".previous"))
         (current (buffer-stash--current-buffer))
         (n 0)
         (too-large
          (cl-remove-if-not
           (lambda (buf)
             (let ((kind (buffer-stash--kind buf)))
               (and kind
                    (buffer-stash--text-saved-p buf kind)
                    (buffer-stash--too-large-p buf))))
           (buffer-list)))
         (asked (and too-large buffer-stash-ask-too-large (not noninteractive)))
         (allowed (buffer-stash--ask-too-large too-large))
         entries errors moved-old)
    (setq buffer-stash-last-too-large nil
          buffer-stash--last-stashed-text nil)
    (when (file-exists-p tmp)
      (delete-directory tmp t))
    (make-directory tmp t)
    (set-file-modes tmp #o700)          ; Stash can contain private text.
    (dolist (buf (buffer-list))
      (let ((kind (buffer-stash--kind buf)))
        (when kind
          (condition-case err
              (let ((entry (buffer-stash--save-one buf kind (cl-incf n) tmp
                                                  (memq buf allowed))))
                (when entry
                  (when (eq buf current)
                    (setq entry (plist-put entry :current t)))
                  (push entry entries)))
            (error (push (format "%s: %s" (buffer-name buf)
                                 (error-message-string err))
                         errors))))))
    (with-temp-file (expand-file-name buffer-stash--index-name tmp)
      (setq buffer-file-coding-system buffer-stash--coding)
      (insert ";; -*- mode: lisp-data; coding: utf-8-emacs-unix -*-\n"
              ";; Written by buffer-stash.el.  Do not edit.\n")
      (let ((print-length nil) (print-level nil) (print-circle nil))
        (prin1 (list :version buffer-stash--format-version
                     :entries (reverse entries))
               (current-buffer)))
      (insert "\n"))
    (when (file-exists-p final)
      (when (file-exists-p old)
        (delete-directory old t))
      (rename-file final old)
      (setq moved-old t))
    (rename-file tmp final)
    (setq buffer-stash--last-save (cons final (and moved-old old)))
    (setq buffer-stash-last-too-large (nreverse buffer-stash-last-too-large))
    ;; If you answered the questions, you know the result: no warning.
    (when (and buffer-stash-last-too-large (not asked))
      (display-warning
       'buffer-stash
       (format "These buffers are larger than `buffer-stash-max-size' (%s bytes); their text was not saved: %s"
               buffer-stash-max-size
               (mapconcat #'identity buffer-stash-last-too-large ", "))
       :warning))
    (when errors
      (display-warning 'buffer-stash
                       (concat "Some buffers were not saved:\n"
                               (mapconcat #'identity errors "\n"))
                       :warning))
    (message "buffer-stash: saved %d buffer(s) to %s" (length entries) final)
    (length entries)))

;;;###autoload
(defun buffer-stash-discard-last-save ()
  "Undo the last `buffer-stash-save' of this session.
Delete the stash it wrote and put back an older stash that it moved
away.  Use this when Emacs did not exit after a save, so that an old
stash does not come back later."
  (interactive)
  (pcase buffer-stash--last-save
    (`(,final . ,old)
     (when (file-exists-p final)
       (delete-directory final t))
     (when (and old (file-exists-p old))
       (rename-file old final))
     (setq buffer-stash--last-save nil))))

;;;; Exit

(defun buffer-stash-unstashed-changes ()
  "Return the file buffers with unsaved text that is not in the stash.
These are file buffers with unsaved changes whose text the last
`buffer-stash-save' did not write, for example because you answered
no at the size question.  If Emacs exits, these changes are lost."
  (cl-remove-if-not
   (lambda (buf)
     (and (buffer-file-name buf)
          (buffer-modified-p buf)
          (not (memq buf buffer-stash--last-stashed-text))))
   (buffer-list)))

(defun buffer-stash-confirm-exit ()
  "Ask the last questions before Emacs exits after `buffer-stash-save'.
Return non-nil if Emacs can exit.  This does the same checks as
`save-buffers-kill-emacs', but it does not ask to save buffers
again, because `buffer-stash-save' asked already and the unsaved
text is in the stash:
 - If unsaved changes are not in the stash (see
   `buffer-stash-unstashed-changes'), ask if you want to exit.
 - If `confirm-kill-processes' is non-nil and processes run, ask
   if you want to kill them.
 - Run `kill-emacs-query-functions'.
 - If `confirm-kill-emacs' is non-nil and no question was asked
   above, call it."
  (let ((confirm confirm-kill-emacs)
        (lost (buffer-stash-unstashed-changes))
        (procs (and confirm-kill-processes
                    (fboundp 'process-list)
                    (cl-remove-if-not
                     (lambda (p)
                       (and (memq (process-status p) '(run stop open listen))
                            (process-query-on-exit-flag p)))
                     (process-list)))))
    (and (or (null lost)
             (progn
               (setq confirm nil)
               (yes-or-no-p
                (format "Unsaved changes are not saved or stashed (%s); exit anyway? "
                        (mapconcat #'buffer-name lost ", ")))))
         (or (null procs)
             (progn
               (setq confirm nil)
               (yes-or-no-p
                (format "Active processes exist (%s); kill them and exit anyway? "
                        (mapconcat #'process-name procs ", ")))))
         (run-hook-with-args-until-failure 'kill-emacs-query-functions)
         (or (null confirm)
             (funcall confirm "Really exit Emacs? ")))))

;;;; Restore

(defun buffer-stash--restore-file (entry text)
  "Restore the file buffer in ENTRY.  TEXT is its unsaved text or nil."
  (let ((file (plist-get entry :file))
        (coding (plist-get entry :coding)))
    (cond
     ((and (null text) (not (file-exists-p file)))
      ;; No unsaved changes, and the file is gone.  Nothing is lost.
      (message "buffer-stash: %s does not exist; skipped" file)
      nil)
     (t
      (let ((buf (find-file-noselect file)))
        (cond
         ((and text (buffer-modified-p buf))
          ;; The buffer was already changed in this session.  Do not
          ;; overwrite it; put the stashed text in a new buffer.  That
          ;; function also restores point.
          (buffer-stash--restore-text
           (plist-put (copy-sequence entry) :name
                      (concat (buffer-name buf) "<stashed>"))
           text))
         (text
          ;; Put back the unsaved text.  It is the same text as at save
          ;; time, so the saved position is exact.
          (with-current-buffer buf
              (let ((inhibit-read-only t))
                (widen)
                (erase-buffer)
                (insert text))
              (when (coding-system-p coding)
                (setq buffer-file-coding-system coding))
              ;; If you saved the file after the stash was made, the
              ;; text is the same as the file: do not mark it modified.
              (set-buffer-modified-p
               (not (buffer-stash--same-as-file-p file))))
          (buffer-stash--restore-point buf entry t)
          buf)
         (t
          ;; No unsaved text.  The file can have changed on disk after
          ;; the save, so search for point with the saved context.
          (buffer-stash--restore-point buf entry nil)
          buf)))))))

(defun buffer-stash--restore-dired (entry)
  "Restore the Dired buffer in ENTRY."
  (let ((dir (plist-get entry :directory)))
    (if (and dir (file-directory-p dir))
        (progn
          (require 'dired)
          (let* ((buf (dired-noselect dir))
                 (file (plist-get entry :dired-file))
                 (pos (and file (file-exists-p file)
                           (with-current-buffer buf
                             (save-excursion (dired-goto-file file))))))
            ;; Go to the same file name.  If that file is gone, the
            ;; listing changed, so search with the saved context.
            (if pos
                (buffer-stash--set-point buf pos)
              (buffer-stash--restore-point buf entry nil))
            buf))
      (message "buffer-stash: directory %s does not exist; skipped" dir)
      nil)))

(defun buffer-stash--restore-text (entry text)
  "Restore the non-file buffer in ENTRY with TEXT."
  (let* ((name (plist-get entry :name))
         (old (get-buffer name))
         ;; Replace a buffer of the same name only if it is safe, for
         ;; example the default *scratch* buffer at startup.
         (buf (if (and old
                       (or (buffer-modified-p old)
                           (get-buffer-process old)
                           (buffer-local-value 'buffer-file-name old)))
                  (generate-new-buffer name)
                (get-buffer-create name)))
         (dir (plist-get entry :directory)))
    (with-current-buffer buf
      (let ((inhibit-read-only t))
        (erase-buffer)
        (insert text))
      (buffer-stash--set-mode (plist-get entry :mode))
      (when (and dir (file-directory-p dir))
        (setq default-directory dir))
      (setq buffer-undo-list nil)
      (set-buffer-modified-p (plist-get entry :modified)))
    ;; The text is the same as at save time, so the position is exact.
    (buffer-stash--restore-point buf entry t)
    buf))

(defun buffer-stash--restore-one (entry dir)
  "Restore one ENTRY from stash DIR.  Signal an error if it fails."
  (let* ((data (plist-get entry :data))
         (text (and data (buffer-stash--read-text (expand-file-name data dir)))))
    (pcase (plist-get entry :kind)
      ('file  (buffer-stash--restore-file entry text))
      ('dired (buffer-stash--restore-dired entry))
      ('text  (buffer-stash--restore-text entry (or text "")))
      (kind   (error "Unknown entry type %S" kind)))))

;;;###autoload
(defun buffer-stash-restore ()
  "Restore all buffers from the hidden area `buffer-stash-directory'.
If all buffers are restored without errors, delete the hidden area.
If an error occurs, keep the hidden area and show a warning.
Then show the buffer that was current at save time in the selected
window, if `buffer-stash-select-current' is non-nil.
Return non-nil if the restore was successful."
  (interactive)
  (let* ((final (buffer-stash--dir))
         (dir (file-name-as-directory final))
         (index (expand-file-name buffer-stash--index-name dir)))
    (if (not (file-exists-p index))
        (progn
          (when (called-interactively-p 'interactive)
            (message "buffer-stash: there is nothing to restore"))
          nil)
      (condition-case err
          (let* ((data (buffer-stash--read-index index))
                 (shown (window-buffer (selected-window)))
                 (count 0)
                 current failed)
            (unless (eql (plist-get data :version) buffer-stash--format-version)
              (error "Unknown stash format version %S"
                     (plist-get data :version)))
            (dolist (entry (plist-get data :entries))
              (condition-case err2
                  (let ((buf (buffer-stash--restore-one entry dir)))
                    (when (and buf (plist-get entry :current))
                      (setq current buf))
                    (cl-incf count))
                (error (push (format "%s: %s" (plist-get entry :name)
                                     (error-message-string err2))
                             failed))))
            (buffer-stash--select current shown)
            (if failed
                (progn
                  (display-warning
                   'buffer-stash
                   (format "%d buffer(s) were not restored.  The stash is kept in %s.\n%s"
                           (length failed) final
                           (mapconcat #'identity (nreverse failed) "\n"))
                   :error)
                  nil)
              (delete-directory final t)
              (message "buffer-stash: restored %d buffer(s)" count)
              t))
        (error
         (display-warning
          'buffer-stash
          (format "Cannot restore the stash in %s: %s.  The stash is kept."
                  final (error-message-string err))
          :error)
         nil)))))

(provide 'buffer-stash)
;;; buffer-stash.el ends here
