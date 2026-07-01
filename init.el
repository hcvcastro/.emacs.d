;;; config.el --- Collabora configure UI -*- lexical-binding: t; -*-

(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(when (< emacs-major-version 27)
  (package-initialize))

;; custom variables
(setq custom-file "~/.emacs.d/custom-file.el")
(load custom-file)

(require 'transient)
(require 'magit)
(require 'widget)
(require 'gnus)
(require 'eglot)

;; Highlight corresponding parentheses when cursor is on one
(show-paren-mode t)

;; Highlight tabulations
;;(setq-default highlight-tabs t)

;; which key minor mode
(which-key-mode t)
(define-key help-map "\C-h" 'which-key-C-h-dispatch)

;; Ask "y" or "n" instead of "yes" or "no".
(fset 'yes-or-no-p 'y-or-n-p)

;; Turn off visibility of highlighting
(highlight-changes-visible-mode nil)

;; Toggle line highlighting in all buffers
(global-hl-line-mode t)

;; The Global HL-Line mode highlight appears in all windows
(setq global-hl-line-sticky-flag t)

;; set the auto revert on
(global-auto-revert-mode t)

;; code style linux
(setq c-default-style "linux")

;; No backup files
(setq make-backup-files nil)

;; disable backup files
(setq backup-inhibited t)

;; turn off auto save
(setq auto-save-default nil)

;; save history commands
(savehist-mode 1)

;; debugging preferences
(setq gud-chdir-before-run nil)
(setq comint-prompt-read-only t)
;;(setq gdbmi-debug-mode t)
;;(setq compilation-auto-jump-to-first-error t)
(setq gdb-show-main t)
(setq compilation-scroll-output t)

;; delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;;    \e[0 q or \e[ q: reset to whatever's defined in the profile settings
;;    \e[1 q: blinking block
;;    \e[2 q: steady block
;;    \e[3 q: blinking underline
;;    \e[4 q: steady underline
;;    \e[5 q: blinking I-beam
;;    \e[6 q: steady I-beam

(defun hcv-change-cursor-shape (&optional args)
  (if (eq (current-buffer) (window-buffer))
      (if (with-current-buffer (current-buffer) buffer-read-only)
	  (send-string-to-terminal "\e[2 q")
	(if overwrite-mode (send-string-to-terminal "\e[1 q")
	  (send-string-to-terminal "\e[5 q")))))

;; change cursor shape
(add-hook 'buffer-list-update-hook 'hcv-change-cursor-shape)
(add-hook 'read-only-mode-hook 'hcv-change-cursor-shape)
(add-hook 'window-configuration-change-hook 'hcv-change-cursor-shape)
(add-hook 'overwrite-mode-hook 'hcv-change-cursor-shape)

;; Behave like vi's o command
(defun open-next-line (arg)
  "Move to the next line and then opens a line.
    See also `newline-and-indent'."
  (interactive "p")
  (end-of-line)
  (open-line arg)
  (forward-line 1)
  (indent-according-to-mode))

;; Behave like vi's O command
(defun open-previous-line (arg)
  "Open a new line before the current one.
     See also `newline-and-indent'."
  (interactive "p")
  (beginning-of-line)
  (open-line arg)
  (indent-according-to-mode))

(global-set-key (kbd "C-o") 'open-next-line)
(global-set-key (kbd "M-o") 'open-previous-line)

;; compile error for jslint
;; https://github.com/Fuco1/compile-eslint/blob/master/compile-eslint.el
(defun compile-eslint--find-filename ()
  "Find the filename for current error."
  (save-match-data
    (save-excursion
      (when (re-search-backward (rx bol (group "/" (+ any)) eol))
        (list (match-string 1))))))

(let ((form `(eslint
              ,(rx-to-string
                '(and (group (group (+ digit)) ":" (group (+ digit)))
                      (+ " ") (or "error" "warning")))
              compile-eslint--find-filename
              2 3 2 1)))
  (if (assq 'eslint compilation-error-regexp-alist-alist)
      (setf (cdr (assq 'eslint compilation-error-regexp-alist-alist)) (cdr form))
    (push form compilation-error-regexp-alist-alist)))

(push 'eslint compilation-error-regexp-alist)

;; no scratch message
(setq initial-scratch-message nil)
;; initial buffer -> dired
(if (= (length command-line-args) 1) (setq initial-buffer-choice default-directory))

;; defaul prog mode read only buffers
(add-hook 'prog-mode-hook (lambda () (if (and (buffer-file-name)
					      (not (string-match (expand-file-name user-emacs-directory)
								 (buffer-file-name))))
					 (read-only-mode t))))
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'prog-mode-hook 'superword-mode)
;;(add-hook 'java-mode-hook #'lsp)

;; apply ansi color to compilation buffer
(defun hcv-ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'hcv-ansi-colorize-buffer)

(defun hcv-async-shell-command (command &optional output-buffer preview)
  "Run COMMAND via `async-shell-command', echoing it first into the buffer.
The echoed line is PREVIEW when given (e.g. to also show the environment a
command runs under), otherwise COMMAND itself, so the buffer always opens
with the exact command that is about to run."
  (async-shell-command
   (concat "echo " (shell-quote-argument (or preview command)) "; " command)
   output-buffer))

(defvar build-default-directory (purecopy command-line-default-directory)
  "Default directory used for build output paths.")

(defvar hcv-num-cores
  (substring (shell-command-to-string "grep -c ^processor /proc/cpuinfo") nil -1)
  "Number of CPU cores, used for parallel make.")

(if (string-match-p "projects" user-emacs-directory)
	(setq build-default-directory (replace-regexp-in-string "projects" "build" build-default-directory))
	(setq build-default-directory (replace-regexp-in-string "develop" "build" build-default-directory)))

(defvar hcv-cool-workspace-dir (expand-file-name "~/develop/online/bin")
  "Source workspace for COOL.")
(defvar hcv-cool-build-dir (expand-file-name "~/build/online/bin")
  "Build directory root for COOL.")

;; custom local variables default directory
(dir-locals-set-class-variables
 'my-default-dir (list (cons 'nil (list (cons 'default-directory command-line-default-directory)))))
(dir-locals-set-directory-class command-line-default-directory 'my-default-dir)

;; --- Per-project paths (COOL) ---
(defvar hcv-cool-configure-ac "configure.ac"
  "Path to COOL's configure.ac file, relative to the source tree.")
(defvar hcv-cool-config-default-ac "~/cool-config.default"
  "Path to a file with default values for COOL options.")
(defvar hcv-cool-config-list nil
  "Current option list for COOL, populated by `hcv-configure'.")
(defvar hcv-cool-default-build-dir ""
  "Build directory for COOL.  Set by the workspace setup block.")
(defvar hcv-cool-config-status nil
  "Path to the `config.status' script in COOL's build directory.")
(defvar hcv-cool-configure-file "configure"
  "Script to run from the source tree to configure COOL.")
(defvar hcv-cool-project-name "Collabora Online Configure"
  "Display name for the COOL configure UI.")
(defvar hcv-cool-config-buffer (format "*%s*" hcv-cool-project-name)
  "Buffer name for the COOL configure UI.")

(defvar hcv-cool-derived-defaults
  '(("--with-lokit-path"
     . (lambda ()
         (expand-file-name "engine/include" command-line-default-directory)))
    ("--with-lo-path"
     . (lambda ()
         (expand-file-name "instdir" hcv-co-default-build-dir))))
  "Derived default values for COOL options.
Each entry is (OPTION-STRING . THUNK).  The thunk is called with no
arguments and must return a string.  Applied after `config.status' and
the defaults file; only fills options whose current value is empty.")

;; --- Per-project paths (Office) ---
(defvar hcv-co-config-list nil
  "Current option list for Office, populated by `hcv-configure'.")
(defvar hcv-co-config-status nil
  "Path to the `config.status' script in Office's build directory.")
(defvar hcv-co-configure-file "autogen.sh"
  "Script to run from the source tree to configure Office.")
(defvar hcv-co-project-name "Collabora Office Configure"
  "Display name for the Office configure UI.")
(defvar hcv-co-config-buffer (format "*%s*" hcv-co-project-name)
  "Buffer name for the Office configure UI.")
(defvar hcv-co-configure-ac "engine/configure.ac")
(defvar hcv-co-config-default-ac "~/co-config.default")
(defvar hcv-co-default-build-dir)

;; --- Shared ---
(defvar hcv-env-variables
  '(("CXXFLAGS" . "C++ compiler flags. Passed to every C++ invocation. Typical values: `-g -O0' for debugging, `-O2' for release, plus warning flags like `-Werror -Wshadow'."))
  "Environment variables to expose as editable fields in the configure UI.
Each entry is either a string NAME (no description) or a cons (NAME . DESCRIPTION).")

(defvar hcv--options-obarray (obarray-make)
  "Private obarray for configure option symbols, to avoid polluting the global obarray.")

(defvar hcv-extra-variable
  '("EXTRA" . "Extra raw flags appended to the configure command. \
Inserted verbatim (no shell-quoting), so you can pass anything: \
multiple flags, env vars, shell expansions. Auto-populated from \
options found in config.status that are not declared in configure.ac.")
  "Free-form extra options field exposed in the configure UI.
Cons (NAME . DESCRIPTION).  The value is inserted at the end of the
generated configure command without quoting.")

(defun hcv--shell-split (string)
  "Split STRING into shell-style tokens.
Respects single and double quotes.  Does not handle escape sequences."
  (let ((pos 0)
        (len (length string))
        (tokens '())
        current)
    (while (< pos len)
      (let ((c (aref string pos)))
        (cond
         ((memq c '(?\s ?\t ?\n))
          (when current (push current tokens) (setq current nil))
          (setq pos (1+ pos)))
         ((eq c ?\')
          (let ((end (string-match "'" string (1+ pos))))
            (unless end (error "hcv--shell-split: unterminated single quote"))
            (setq current (concat (or current "")
                                  (substring string (1+ pos) end))
                  pos     (1+ end))))
         ((eq c ?\")
          (let ((end (string-match "\"" string (1+ pos))))
            (unless end (error "hcv--shell-split: unterminated double quote"))
            (setq current (concat (or current "")
                                  (substring string (1+ pos) end))
                  pos     (1+ end))))
         (t
          (setq current (concat (or current "") (char-to-string c))
                pos     (1+ pos))))))
    (when current (push current tokens))
    (nreverse tokens)))

(defun hcv--apply-derived-defaults (alist)
  "For each (OPTION . THUNK) in ALIST, set option to (funcall THUNK) if empty.
OPTION is looked up in `hcv--options-obarray'; if the symbol doesn't
exist there (i.e. the option wasn't parsed from configure.ac), it's
skipped.  Only symbols whose current value is an empty string are
updated, so values set by `config.status' or the defaults file win."
  (dolist (entry alist)
    (let* ((option (car entry))
           (thunk  (cdr entry))
           (sym    (intern-soft option hcv--options-obarray)))
      (when (and sym
                 (stringp (symbol-value sym))
                 (string-empty-p (symbol-value sym)))
        (set sym (funcall thunk))))))

(defun hcv--insert-description (desc)
  "Insert DESC as an indented, faded help block under a widget.
Preserves newlines (renders as multiple lines) but collapses runs of
horizontal whitespace.  Each line is indented with four spaces."
  (when (and desc (not (string-empty-p desc)))
    (let* ((trimmed  (string-trim desc))
           ;; Collapse horizontal whitespace only (spaces + tabs), keep \n.
           (no-tabs  (replace-regexp-in-string "[ \t]+" " " trimmed))
           ;; Strip leading spaces at the start of each line (M4 indentation).
           (clean    (replace-regexp-in-string "^ +" "" no-tabs))
           ;; Indent every line with 4 spaces.
           (indented (replace-regexp-in-string "\n" "\n    " clean)))
      ;; Blank line before, indented block, blank line after.
      (widget-insert "\n")
      (let ((start (point)))
        (widget-insert "    ")
        (widget-insert indented)
        (add-text-properties start (point) '(face shadow)))
      (widget-insert "\n\n"))))

(defun hcv--decode-m4-quadrigraphs (string)
  "Decode autoconf/M4 quadrigraphs in STRING to their literal characters.
These appear in AS_HELP_STRING text where literal `[', `]', `#' and
braces must be escaped from M4 quoting (e.g. `--enable-foo@<:@=no@:>@'
stands for `--enable-foo[=no]').  Returns STRING unchanged when nil."
  (if (null string)
      string
    (let ((s string))
      (dolist (pair '(("@<:@" . "[") ("@:>@" . "]") ("@%:@" . "#")
                      ("@{:@" . "{") ("@:}@" . "}") ("@&t@" . "")))
        (setq s (replace-regexp-in-string
                 (regexp-quote (car pair)) (cdr pair) s t t)))
      s)))

(defun hcv-read-configure-option (configure-string configure-description
                                                   &optional dual-form)
  "Parse CONFIGURE-STRING and return a symbol describing the option.
When DUAL-FORM is non-nil, the help text showed both `--foo' and
`--foo=VALUE' variants: emit `toggle-field' widget type (checkbox +
optional value).  An option written `--foo[=VALUE]' (optional value) is
likewise rendered as a `toggle-field'."
  (let* ((configure-string (hcv--decode-m4-quadrigraphs configure-string))
         (configure-description (hcv--decode-m4-quadrigraphs configure-description))
         ;; `--foo[=VALUE]' means the value is optional.
         (optional-value (string-match-p "\\[=?" configure-string))
         ;; Option name: everything before the first `[' or `=' (the
         ;; bracket of an optional value comes before its `=').
         (name-end (let ((b (string-match "\\[" configure-string))
                         (e (string-match "=" configure-string)))
                     (cond ((and b e) (min b e))
                           (b b)
                           (e e)
                           (t (length configure-string)))))
         (option-name (substring configure-string 0 name-end))
         (configure-symbol (intern option-name hcv--options-obarray))
         ;; Autoconf negative form (--without-/--disable-), or nil.
         (negative (hcv--negative-option-name option-name))
         (has-value (or optional-value (string-match-p "=" configure-string))))
    (set configure-symbol nil)
    (put configure-symbol 'configure-string configure-string)
    (put configure-symbol 'configure-description configure-description)
    (cond
     ;; A flag-with-optional-value option, rendered as a checkbox plus an
     ;; optional value field.  This covers options whose help text shows
     ;; both `--foo' and `--foo=VALUE' (DUAL-FORM), the explicit optional
     ;; `--foo[=VALUE]' form, and any valued `--with-'/`--enable-' option,
     ;; so the latter can be turned on/off with an optional value instead
     ;; of being a bare value field.
     ((or dual-form optional-value (and negative has-value))
      (set configure-symbol (cons nil ""))   ; (enabled . value)
      (put configure-symbol 'widget-type 'toggle-field)
      (put configure-symbol 'format (concat option-name "[=%v]")))
     ((not has-value)
      (put configure-symbol 'widget-type 'checkbox))
     (t
      (set configure-symbol "")
      (put configure-symbol 'format (concat option-name "=%v"))
      (put configure-symbol 'widget-type
           (cond
            ((string-match-p "file" configure-string) 'file)
            ((string-match-p "\\(path\\|dir\\|prefix\\)" configure-string) 'directory)
            (t 'editable-field)))))
    ;; Record the negative form so the UI can offer it as a second
    ;; checkbox.  Reset on every refresh; `hcv-read-config-status'
    ;; re-selects it from config.status.
    (put configure-symbol 'negatable negative)
    (put configure-symbol 'negative-selected nil)
    configure-symbol))

(defun hcv-read-configure-options (configure-ac-file)
  "Return the list of option symbols parsed from CONFIGURE-AC-FILE.
CONFIGURE-AC-FILE must be a path to a `configure.ac' or equivalent M4
source that uses `AC_ARG_ENABLE' / `AC_ARG_WITH' together with
`AS_HELP_STRING'.  Each returned element is a symbol interned in
`hcv--options-obarray'.  Returns nil if the file does not exist."
  (let ((config-list nil)
        (case-fold-search nil))
    (if (file-exists-p configure-ac-file)
        (with-temp-buffer
          (insert-file-contents configure-ac-file)
          (goto-char (point-max))
          (while (re-search-backward
                  "\\(_ARG_ENABLE\\|_ARG_WITH\\)[^A]*AS_HELP_STRING[^[]*\\[\\([^]]*\\)[^[]*\\[\\([^]]*\\)"
                  nil t)
            (let* ((config-string (match-string 2))
                   (description   (match-string 3))
                   (after-match   (match-end 0))
                   (body-end      (min (+ after-match 1200) (point-max)))
                   (body          (buffer-substring-no-properties after-match body-end))
                   (option-name   (car (split-string config-string "=")))
                   (has-valued    (string-match-p
                                   (concat (regexp-quote option-name) "=") body))
                   (has-flag-only (string-match-p
                                   (concat (regexp-quote option-name) "[[:space:]\n]")
                                   body))
                   ;; Look for an extra [...] block (like the `Usage:'
                   ;; addendum in AC_ARG_WITH).  It lives AFTER the
                   ;; AS_HELP_STRING close paren but BEFORE the next
                   ;; AC_ARG_* or the closing `,)'.
                   (extra-match   (and (string-match
                                        "\\`[^A]*?)\\s-*\\[\\([^]]+\\)\\]"
                                        body)
                                       (match-string 1 body)))
                   (full-desc     (if extra-match
                                      (concat description "\n\n" extra-match)
                                    description)))
              (push (hcv-read-configure-option config-string full-desc
                                               (and has-valued has-flag-only))
                    config-list))))
      (message "hcv: configure.ac file not found: %s" configure-ac-file))
    (nconc config-list
           (hcv-read-system-module-options
            configure-ac-file (mapcar #'symbol-name config-list)))))

(defun hcv--split-m4-args (argstr)
  "Split ARGSTR into a list of top-level m4 arguments.
Commas inside `[...]' quoting are not treated as separators."
  (let ((args nil) (start 0) (depth 0) (i 0) (n (length argstr)))
    (while (< i n)
      (let ((ch (aref argstr i)))
        (cond
         ((eq ch ?\[) (setq depth (1+ depth)))
         ((eq ch ?\]) (setq depth (1- depth)))
         ((and (eq ch ?,) (= depth 0))
          (push (substring argstr start i) args)
          (setq start (1+ i)))))
      (setq i (1+ i)))
    (push (substring argstr start) args)
    (nreverse args)))

(defun hcv--m4-unbracket (str)
  "Trim whitespace and a surrounding `[...]' quote from STR."
  (let ((str (string-trim str)))
    (if (and (string-prefix-p "[" str) (string-suffix-p "]" str))
        (string-trim (substring str 1 -1))
      str)))

(defun hcv-read-system-module-options (configure-ac-file &optional explicit-names)
  "Return option symbols implied by `libo_CHECK_SYSTEM_MODULE' calls.
The macro (see `engine/m4/libo_externals.m4') generates `--enable-/--disable-'
and `--with-system-/--without-system-' switches that never appear literally in
CONFIGURE-AC-FILE, so the AS_HELP_STRING scan in `hcv-read-configure-options'
misses them.  Mirror the macro's own logic: argument $1 is the lowercase name,
$4 selects the enable switch, $5 selects the system switch.

EXPLICIT-NAMES is the list of option names already declared explicitly in this
same parse run; such options are skipped to avoid duplicates.  Dedup is done
against this per-run list rather than the persistent `hcv--options-obarray',
since these functions run on every buffer refresh and the obarray would
otherwise already hold the symbols from a previous run, dropping every option."
  (let ((config-list nil)
        (seen nil)
        (case-fold-search nil))
    (when (file-exists-p configure-ac-file)
      (with-temp-buffer
        (insert-file-contents configure-ac-file)
        (goto-char (point-min))
        (while (re-search-forward
                "^[ \t]*libo_CHECK_SYSTEM_MODULE(\\(.*\\))" nil t)
          (let* ((args   (mapcar #'hcv--m4-unbracket
                                 (hcv--split-m4-args (match-string 1))))
                 (name   (nth 0 args))
                 (enable (or (nth 3 args) ""))
                 (system (or (nth 4 args) "")))
            (unless (or (null name) (string-empty-p name))
              (dolist (opt
                       (list
                        ;; enable/disable switch ($4)
                        (cond
                         ((string= enable "enabled")
                          (cons (concat "--disable-" name)
                                (format "Disable %s support." name)))
                         ((string= enable "disabled")
                          (cons (concat "--enable-" name)
                                (format "Enable %s support." name))))
                        ;; system switch ($5)
                        (cond
                         ((string= system "system")
                          (cons (concat "--without-system-" name)
                                (format "Build and bundle the internal %s." name)))
                         ((member system '("" "internal" "system-if-linux"))
                          (cons (concat "--with-system-" name)
                                (format "Use %s from the operating system." name))))))
                (when (and opt
                           (not (member (car opt) explicit-names))
                           (not (member (car opt) seen)))
                  (push (car opt) seen)
                  (push (hcv-read-configure-option (car opt) (cdr opt))
                        config-list))))))))
    (nreverse config-list)))

(defun hcv--distro-configs (dir &optional prefix)
  "Return the list of distro-config names found under DIR.
Each name is relative to the top `distro-configs' directory, without the
`.conf' extension; subdirectories contribute a `SUBDIR/' prefix,
mirroring autogen.sh's `--with-distro' lookup.  PREFIX is used by the
recursion."
  (let (names)
    (dolist (entry (directory-files dir nil "\\`[^.]"))
      (let ((full (expand-file-name entry dir)))
        (cond
         ((file-directory-p full)
          (setq names (nconc names
                             (hcv--distro-configs
                              full (concat (or prefix "") entry "/")))))
         ((string-suffix-p ".conf" entry)
          (push (concat (or prefix "")
                        (substring entry 0 (- (length entry) (length ".conf"))))
                names)))))
    names))

(defun hcv--file-contains-p (file string)
  "Return non-nil if FILE exists and contains STRING."
  (and (file-readable-p file)
       (with-temp-buffer
         (insert-file-contents file)
         (goto-char (point-min))
         (search-forward string nil t))))

(defun hcv-read-distro-option (config-list configure-ac-file)
  "Prepend the autogen.sh `--with-distro' option to CONFIG-LIST when applicable.
Only added when the sibling `autogen.sh' of CONFIGURE-AC-FILE handles a
`--with-distro' argument: autogen.sh expands a set from distro-configs/
into real configure options, so the option is not declared in
configure.ac and only exists for autogen.sh-driven trees (the engine,
not COOL).

Rendered as a `toggle-field' (checkbox + optional value), with no
`--without-' form.  The value is a distro-config name; the available
names (if any) are listed in the option's description."
  (let* ((dir     (or (file-name-directory configure-ac-file) ""))
         (autogen (concat dir "autogen.sh")))
    (if (not (hcv--file-contains-p autogen "--with-distro"))
        config-list
      (let* ((distro-dir (concat dir "distro-configs"))
             (configs (and (file-directory-p distro-dir)
                           (sort (hcv--distro-configs distro-dir) #'string<)))
             (sym (intern "--with-distro" hcv--options-obarray)))
        (unless (and (boundp sym) (consp (symbol-value sym)))
          (set sym (cons nil "")))
        (put sym 'widget-type 'toggle-field)
        (put sym 'format "--with-distro[=%v]")
        ;; Offer the available configs through a popup next to the field.
        (put sym 'choices configs)
        ;; autogen.sh expands --with-distro itself, so there is no
        ;; --without- counterpart: keep it off the negative-checkbox path.
        (put sym 'negatable nil)
        (put sym 'negative-selected nil)
        (put sym 'configure-description
             (concat "autogen.sh option (not declared in configure.ac): apply "
                     "a set of options from distro-configs/."
                     (when configs
                       (concat "  Available: "
                               (mapconcat #'identity configs ", ") "."))))
        (cons sym config-list)))))

(defun hcv-read-extra-variable (config-list)
  "Append the extra free-form entry to CONFIG-LIST and return it.
Uses `hcv-extra-variable' for the name and description."
  (let* ((entry hcv-extra-variable)
         (name  (car entry))
         (desc  (cdr entry))
         (sym   (intern name hcv--options-obarray)))
    (unless (and (boundp sym) (stringp (symbol-value sym)))
      (set sym ""))
    (put sym 'widget-type 'raw-extra)
    (put sym 'format (concat name "=%v"))
    (put sym 'tag 'extra-variable)
    (when desc
      (put sym 'configure-description desc))
    (push sym config-list)
    config-list))

(defun hcv-read-default-environment (config-list)
  "Prepend env-variable entries to CONFIG-LIST and return it."
  (dolist (entry hcv-env-variables config-list)
    (let* ((name (if (consp entry) (car entry) entry))
           (desc (and (consp entry) (cdr entry)))
           (sym  (intern name hcv--options-obarray)))
      (set sym (or (getenv name) ""))
      (put sym 'widget-type 'editable-field)
      (put sym 'format (concat name "=%v"))
      (put sym 'tag 'env-variable)
      (when desc
        (put sym 'configure-description desc))
      (push sym config-list))))

(defun hcv--negative-option-name (name)
  "Return the autoconf negative form of option NAME, or nil if it has none.
`--with-X' maps to `--without-X' and `--enable-X' to `--disable-X'."
  (cond
   ((string-prefix-p "--with-" name)
    (concat "--without-" (substring name (length "--with-"))))
   ((string-prefix-p "--enable-" name)
    (concat "--disable-" (substring name (length "--enable-"))))))

(defun hcv--positive-option-name (name)
  "Return the positive form of the negative option NAME, or nil if none.
Inverse of `hcv--negative-option-name': `--without-X' maps to `--with-X'
and `--disable-X' to `--enable-X'."
  (cond
   ((string-prefix-p "--without-" name)
    (concat "--with-" (substring name (length "--without-"))))
   ((string-prefix-p "--disable-" name)
    (concat "--enable-" (substring name (length "--disable-"))))))

(defun hcv--apply-option-value (sym value)
  "Apply parsed VALUE to option SYM according to its widget type.
VALUE is the string found after `=' in a `NAME=VALUE' entry, or nil when
the option appeared as a bare flag.  The value is coerced to a
representation valid for SYM's widget type.  If no valid value can be
derived for the type (e.g. a valued option that came without a value, or
any other type mismatch), SYM is left untouched so it keeps its default.

This keeps the configure UI working when `config.status' contains an
option with a value that does not match the option's declared type:
instead of forcing an invalid value (which later makes `widget-create'
error and aborts the whole buffer), the option simply falls back to its
default."
  (pcase (get sym 'widget-type)
    ('toggle-field
     (set sym (cons t (or value ""))))
    ('checkbox
     ;; A checkbox is a presence flag; its widget value must be boolean.
     ;; Any appearance in the config means the flag is enabled, and the
     ;; flag is emitted without a value anyway, so a value is ignored.
     (set sym t))
    ((or 'editable-field 'directory 'file)
     ;; Valued widgets require a string; keep the default otherwise.
     (when (stringp value)
       (set sym value)))
    (_
     ;; Unknown/unrendered symbol: preserve the previous lenient behaviour.
     (if value (set sym value) (set sym t)))))

(defun hcv-read-config-status (config-status)
  "Read values from CONFIG-STATUS and update their symbols in `hcv--options-obarray'.
CONFIG-STATUS must be a path to an executable `config.status' script
\(typically produced by a previous `configure' run\).  Runs it with
`--config' and parses the quoted argument list.

For each `NAME=VALUE' entry, finds the matching symbol in
`hcv--options-obarray' and sets its value.  Options not declared in
configure.ac are accumulated into the EXTRA symbol so they can be
reviewed in the configure UI and round-tripped on the next run.

Returns non-nil if at least one option was read, nil otherwise."
  (let (config-list list-item config-symbol config-value)
    (when (file-exists-p config-status)
      (let ((raw (shell-command-to-string (concat config-status " --config"))))
        (setq config-list
              (condition-case err
                  (hcv--shell-split raw)
                (error
                 (message "hcv: failed to parse config.status output: %s"
                          (error-message-string err))
                 nil)))))
    (let ((had-items (> (length config-list) 0))
          pos
          name
          (extra-sym (intern-soft "EXTRA" hcv--options-obarray))
          (orphans '()))
      (while config-list
        (setq list-item (pop config-list))
        (setq pos (string-match "=" list-item))
        (if pos
            (setq name         (substring list-item 0 pos)
                  config-value (substring list-item (1+ pos)))
          (setq name         list-item
                config-value nil))
        (setq config-symbol (intern-soft name hcv--options-obarray))
        (cond
         ;; Known widget option: assign to existing symbol.
         ((and config-symbol
               (get config-symbol 'widget-type)
               (not (eq (get config-symbol 'widget-type) 'raw-extra)))
          (hcv--apply-option-value config-symbol config-value))
         ;; Negative form (--without-/--disable-) of a known negatable
         ;; option: select its negative checkbox instead of orphaning it.
         ((let* ((positive (hcv--positive-option-name name))
                 (pos-sym  (and positive
                                (intern-soft positive hcv--options-obarray))))
            (when (and pos-sym (get pos-sym 'negatable))
              (put pos-sym 'negative-selected t)
              t)))
         ;; Orphan (or EXTRA itself): stash for the EXTRA field.
         (t
          (push (if config-value
                    (concat name "=" (shell-quote-argument config-value))
                  name)
                orphans))))
      ;; Dump orphans into EXTRA, preserving original config.status order.
      (when extra-sym
        (set extra-sym
             (if orphans
                 (mapconcat #'identity (nreverse orphans) " ")
               "")))
      had-items)))

(defun hcv--unquote-value (value)
  "Strip surrounding double quotes from VALUE if present."
  (if (and (>= (length value) 2)
           (string-prefix-p "\"" value)
           (string-suffix-p "\"" value))
      (substring value 1 -1)
    value))

(defun hcv-read-config-default (configure-default)
  "Read default option values from CONFIGURE-DEFAULT and apply them.
..."
  (let (lines had-items pos)
    (if (file-exists-p configure-default)
        (let ((raw (with-temp-buffer
                     (insert-file-contents configure-default)
                     (buffer-string))))
          (setq lines
                (seq-filter (lambda (line)
                              (let ((trimmed (string-trim line)))
                                (and (not (string-empty-p trimmed))
                                     (not (string-prefix-p "#" trimmed)))))
                            (split-string raw "\n"))))
      (message "hcv: default config file not found: %s" configure-default))

    (setq had-items (> (length lines) 0))

    (dolist (config-string lines)
      (setq pos (string-match "=" config-string))
      (let* ((name (if pos (substring config-string 0 pos) config-string))
             (value (and pos (hcv--unquote-value
                              (substring config-string (1+ pos)))))
             (positive (hcv--positive-option-name name))
             (pos-sym  (and positive
                            (intern-soft positive hcv--options-obarray))))
        (if (and pos-sym (get pos-sym 'negatable))
            ;; Negative form of a known negatable option.
            (put pos-sym 'negative-selected t)
          (hcv--apply-option-value (intern name hcv--options-obarray) value))))
    had-items))

(when (string-match-p hcv-cool-workspace-dir
                      (expand-file-name command-line-default-directory))

  ;; --- Build paths ---
  (setq hcv-cool-build-dir
        (concat hcv-cool-build-dir
                (substring (expand-file-name command-line-default-directory)
                           (length hcv-cool-workspace-dir))))
  (setq hcv-cool-default-build-dir hcv-cool-build-dir)
  (setq hcv-co-default-build-dir (concat hcv-cool-default-build-dir "engine/"))
  (setq hcv-cool-config-status (concat hcv-cool-default-build-dir "config.status"))
  (setq hcv-co-config-status (concat hcv-co-default-build-dir "config.status"))

;; --- COOL config ---
  (setq hcv-cool-config-list (hcv-read-configure-options hcv-cool-configure-ac))
  (setq hcv-cool-config-list (hcv-read-default-environment hcv-cool-config-list))
  (setq hcv-cool-config-list (hcv-read-extra-variable hcv-cool-config-list))
  (unless (hcv-read-config-status hcv-cool-config-status)
    (hcv-read-config-default hcv-cool-config-default-ac))
  (hcv--apply-derived-defaults hcv-cool-derived-defaults)

  ;; --- Office config (same workspace, separate source tree) ---
  (setq hcv-co-config-list (hcv-read-configure-options hcv-co-configure-ac))
  (setq hcv-co-config-list (hcv-read-default-environment hcv-co-config-list))
  (setq hcv-co-config-list (hcv-read-extra-variable hcv-co-config-list))
  (unless (hcv-read-config-status hcv-co-config-status)
    (hcv-read-config-default hcv-co-config-default-ac))

  ;; --- Default compile command and TAGS (unused by the transient but
  ;;     handy when invoking `M-x compile' or `M-.' outside the menu) ---
  (setq compile-command (concat "make -j " hcv-num-cores " -C " hcv-cool-build-dir))

  ;; --- Bootstrap autogen if the configure script is missing ---
  (if (not (file-exists-p (expand-file-name hcv-cool-configure-file
                                            (expand-file-name command-line-default-directory))))
      (hcv-async-shell-command "./autogen.sh"))

  (global-set-key "\C-cc" 'hcv-main-commands))

(eval-when-compile
  (require 'wid-edit))

(defun hcv-config-list-update (configure-ac configure-status configure-default-ac
                                            &optional derived-defaults)
  "Build and return a fresh config list from CONFIGURE-AC.
Apply values from CONFIGURE-STATUS if available; otherwise fall back to
CONFIGURE-DEFAULT-AC.  If DERIVED-DEFAULTS is a non-nil alist of
\(OPTION . THUNK), apply derived values for any option still unset after
loading status/defaults."
  (let ((config-list (hcv-read-distro-option
                      (hcv-read-extra-variable
                       (hcv-read-default-environment
                        (hcv-read-configure-options configure-ac)))
                      configure-ac)))
    (unless (hcv-read-config-status configure-status)
      (hcv-read-config-default configure-default-ac))
    (when derived-defaults
      (hcv--apply-derived-defaults derived-defaults))
    config-list))

(defun hcv--bold-label-format (format)
  "Return FORMAT with its literal label (the text before %v) emboldened.
Valued widgets render their option name as plain `:format' text, unlike
checkboxes whose `:button-suffix' name is shown with the bold
`widget-button' face.  Applying the same face here keeps the option
names visually consistent across the configure buffer."
  (if (string-match "%v" format)
      (concat (propertize (substring format 0 (match-beginning 0))
                          'face 'widget-button)
              (substring format (match-beginning 0)))
    format))

(defun hcv--add-negative-checkbox (config positive-checkbox)
  "Render the `--without-/--disable-' checkbox for CONFIG when negatable.
POSITIVE-CHECKBOX is the widget to uncheck when the negative one is
selected (and vice versa), or nil when CONFIG has no positive checkbox
\(as for a plain valued option, where the negative form simply overrides
the value field at command-build time).  The created widget is stored
under CONFIG's `negative-widget' property; its initial state comes from
the `negative-selected' property set while reading config.status."
  (let ((negative (get config 'negatable)))
    (when negative
      ;; Render the negative checkbox on its own line, flush left, so the
      ;; --without-/--disable- box lines up in the same column as its
      ;; positive option instead of crowding the same line.
      (widget-insert "\n")
      (let ((nw (widget-create 'checkbox
                               :button-suffix negative
                               (and (get config 'negative-selected) t))))
        (put config 'negative-widget nw)
        ;; Mutual exclusion: selecting one form clears the other.
        (when positive-checkbox
          (widget-put positive-checkbox :notify
                      (lambda (w &rest _)
                        (when (widget-value w)
                          (widget-value-set nw nil))))
          (widget-put nw :notify
                      (lambda (w &rest _)
                        (when (widget-value w)
                          (widget-value-set positive-checkbox nil)))))))))

(defun hcv--insert-choice-radios (config checkbox field)
  "Render CONFIG's `choices' as radio buttons below its value field.
Selecting a radio fills FIELD with the chosen value and enables CHECKBOX.
The radio matching the current value (if any) starts selected."
  (let* ((choices (get config 'choices))
         (current (and (consp (symbol-value config)) (cdr (symbol-value config))))
         (items   (mapcar (lambda (c) (list 'item :tag c :value c)) choices))
         (fld field)
         (chk checkbox))
    (widget-insert "\n")
    (apply #'widget-create 'radio-button-choice
           :value (and (member current choices) current)
           :indent 4
           :notify (lambda (w &rest _)
                     (let ((v (widget-value w)))
                       (when v
                         (widget-value-set fld v)
                         (widget-value-set chk t))))
           items)))

(defun hcv-configure (config-list-var configure-buffer configure-title
                                      configure-ac configure-status configure-default-ac
                                      source-dir build-dir configure-file
                                      &optional derived-defaults)
  "Render the configure buffer for CONFIGURE-BUFFER.
CONFIG-LIST-VAR is the symbol of the global variable holding the option
list for this project.  CONFIGURE-BUFFER is the name of the buffer to
display.  CONFIGURE-TITLE is inserted at the top as a header.
CONFIGURE-AC, CONFIGURE-STATUS and CONFIGURE-DEFAULT-AC are the inputs
used to refresh the list.  SOURCE-DIR, BUILD-DIR and CONFIGURE-FILE are
passed to the execute/copy buttons.  Optional DERIVED-DEFAULTS is an
alist of (OPTION . THUNK) used to fill any option still empty after
loading status/defaults.

`raw-extra' widgets (the EXTRA field) are always rendered at the end of
the buffer, regardless of their position in the option list."
  (switch-to-buffer configure-buffer)
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert configure-title)

  (set config-list-var
       (hcv-config-list-update configure-ac configure-status configure-default-ac
                               derived-defaults))

  ;; Split into non-EXTRA (rendered first) and EXTRA (rendered last),
  ;; preserving the original order within each group.
  (let ((rest   '())
        (extras '()))
    (dolist (config (symbol-value config-list-var))
      (if (eq (get config 'widget-type) 'raw-extra)
          (push config extras)
        (push config rest)))
    (dolist (config (append (nreverse rest) (nreverse extras)))
      (let* ((type  (get config 'widget-type))
             (value (symbol-value config))
             (desc  (get config 'configure-description)))
        (cond
         ((eq type 'checkbox)
          (let ((w (widget-create 'checkbox
                                  :button-suffix (symbol-name config)
                                  value)))
            (put config 'widget w)
            (hcv--add-negative-checkbox config w)))
         ((eq type 'toggle-field)
          (let* ((enabled (and (consp value) (car value)))
                 (valstr  (if (consp value) (cdr value) ""))
                 (cb (widget-create 'checkbox
                                    :button-suffix (symbol-name config)
                                    enabled))
                 (_  (widget-insert "="))
                 (fd (widget-create 'editable-field
                                    :size 30
                                    valstr)))
            (put config 'widget (cons cb fd))
            (hcv--add-negative-checkbox config cb)
            ;; Offer the known values as radio buttons below the field.
            (when (get config 'choices)
              (hcv--insert-choice-radios config cb fd))))
         ((eq type 'raw-extra)
          (let ((w (widget-create 'editable-field
                                  :format (hcv--bold-label-format "Extra: %v")
                                  :size 60
                                  value)))
            (put config 'widget w)))
         ((memq type '(editable-field directory file))
          ;; Plain valued options (env vars, --prefix, …): not negatable,
          ;; so just a labelled value field.  Negatable valued options are
          ;; parsed as `toggle-field' and handled above.
          (let ((w (widget-create type
                                  :format (hcv--bold-label-format
                                           (get config 'format))
                                  value)))
            (put config 'widget w))))
        (hcv--insert-description desc))))

  ;; Closures capture config-list-var, source-dir, build-dir, configure-file.
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (hcv-execute-configure config-list-var source-dir build-dir configure-file))
                 "Configure")
  (widget-insert " ")
  (widget-create 'push-button
                 :notify (lambda (&rest _)
                           (hcv-execute-copy-clipboard config-list-var source-dir build-dir configure-file))
                 "Copy")
  (use-local-map widget-keymap)
  (widget-setup))

(defun hcv-get-shell-command (config-list source-dir build-dir configure-file)
  "Build a shell command string to run CONFIGURE-FILE with CONFIG-LIST."
  (let (out-list)
    (dolist (config config-list)
      (let* ((widget (get config 'widget))
             (type   (get config 'widget-type))
             (neg-w  (get config 'negative-widget)))
        (cond
         ;; Negative form selected: emit --without-/--disable- and skip
         ;; the positive form entirely.
         ((and neg-w (widget-value neg-w))
          (push (get config 'negatable) out-list))
         ((eq type 'checkbox)
          (when (widget-value widget)
            (push (symbol-name config) out-list)))
         ((eq type 'toggle-field)
          (let ((enabled (widget-value (car widget)))
                (valstr  (widget-value (cdr widget))))
            (when enabled
              (push (if (string-empty-p valstr)
                        (symbol-name config)
                      (concat (symbol-name config) "=" (shell-quote-argument valstr)))
                    out-list))))
         ((eq type 'raw-extra)
          (let ((value (widget-value widget)))
            (when (not (string-empty-p value))
              (push value out-list))))
         ((memq type '(editable-field directory file))
          (let ((value (widget-value widget)))
            (when (not (string-empty-p value))
              (push (concat (symbol-name config) "=" (shell-quote-argument value))
                    out-list)))))))
    (concat "cd " (shell-quote-argument build-dir)
            " && "
            (shell-quote-argument
             (expand-file-name configure-file source-dir))
            " "
            (mapconcat #'identity (nreverse out-list) " "))))

(defun hcv-execute-configure (config-list-var source-dir build-dir configure-file)
  "Run the configure command for the project identified by CONFIG-LIST-VAR.
Uses SOURCE-DIR, BUILD-DIR and CONFIGURE-FILE to build the command."
  (let ((cmd (hcv-get-shell-command (symbol-value config-list-var)
                                    source-dir build-dir configure-file)))
    (make-directory build-dir t)
    (hcv-async-shell-command cmd)))

(defun hcv-execute-copy-clipboard (config-list-var source-dir build-dir configure-file)
  "Copy the configure command for the project identified by CONFIG-LIST-VAR.
Uses SOURCE-DIR, BUILD-DIR and CONFIGURE-FILE to build the command."
  (let ((cmd (hcv-get-shell-command (symbol-value config-list-var)
                                    source-dir build-dir configure-file)))
    (xclip-set-selection 'clipboard cmd)))

(defun hcv-configure-cool ()
  "Open the Collabora Online configure buffer."
  (interactive)
  (hcv-configure 'hcv-cool-config-list
                 hcv-cool-config-buffer
                 (concat hcv-cool-project-name ".\n\n")
                 hcv-cool-configure-ac
                 hcv-cool-config-status
                 hcv-cool-config-default-ac
                 (expand-file-name command-line-default-directory)
                 hcv-cool-default-build-dir
                 hcv-cool-configure-file
                 hcv-cool-derived-defaults))

(defun hcv-configure-co ()
  "Open the Collabora Office configure buffer."
  (interactive)
  (hcv-configure 'hcv-co-config-list
                 hcv-co-config-buffer
                 (concat hcv-co-project-name ".\n\n")
                 hcv-co-configure-ac
                 hcv-co-config-status
                 hcv-co-config-default-ac
		 (expand-file-name "engine/" command-line-default-directory)
                 hcv-co-default-build-dir
                 hcv-co-configure-file))

(defun hcv-compile-cool ()
  "Run make for COOL directly, without prompting."
  (interactive)
  (let ((cmd (concat "make -j " hcv-num-cores " -C " hcv-cool-default-build-dir)))
    (add-to-history 'compile-history cmd)
    (compile cmd)))

(defun hcv-compile-co ()
  "Run make for Office directly, without prompting."
  (interactive)
  (let ((cmd (concat "make -j " hcv-num-cores " -C " hcv-co-default-build-dir)))
    (add-to-history 'compile-history cmd)
    (compile cmd)))

(defun hcv-run-cool ()
  "Run the configured run command for COOL."
  (interactive)
  (let* ((default-cmd (concat "cd " hcv-cool-default-build-dir " && make run"))
         (cmd (read-shell-command "Run command: " default-cmd)))
    (hcv-async-shell-command cmd)))

(defun hcv-run-co ()
  "Run Collabora Office (soffice.bin) from the build instdir."
  (interactive)
  (let* ((default-cmd (concat hcv-co-default-build-dir
                              "instdir/program/soffice.bin"
                              " --norestore --nologo --writer"))
         (cmd (read-shell-command "Run command: " default-cmd)))
    (hcv-async-shell-command cmd)))

(defun hcv-head-config-cool ()
  "Show head of config.log for COOL."
  (interactive)
  (hcv-async-shell-command (concat "cd " hcv-cool-default-build-dir " && head config.log")))

(defun hcv-head-config-co ()
  "Show head of config.log for Office."
  (interactive)
  (hcv-async-shell-command (concat "cd " hcv-co-default-build-dir " && head config.log")))

(defun hcv-full-config-cool ()
  "Open the full config.log for COOL in view-mode."
  (interactive)
  (let ((log (concat hcv-cool-default-build-dir "config.log")))
    (if (file-exists-p log)
        (view-file log)
      (message "hcv: config.log not found: %s" log))))

(defun hcv-full-config-co ()
  "Open the full config.log for Office in view-mode."
  (interactive)
  (let ((log (concat hcv-co-default-build-dir "config.log")))
    (if (file-exists-p log)
        (view-file log)
      (message "hcv: config.log not found: %s" log))))

(defun hcv-tags-build-cool ()
  "Regenerate TAGS for COOL by running `make tags' in its build dir."
  (interactive)
  (compile (concat "make -C " hcv-cool-default-build-dir " tags")))

(defun hcv-tags-build-co ()
  "Regenerate TAGS for Office by running `make tags' in its build dir."
  (interactive)
  (compile (concat "make -C " hcv-co-default-build-dir " tags")))

(defun hcv-tags-load-cool ()
  "Set `tags-table-list' to COOL's TAGS files."
  (interactive)
  (setq tags-table-list (list (concat hcv-cool-default-build-dir "TAGS")
                              (concat hcv-cool-default-build-dir "browser/TAGS")))
  (message "Tags table list switched to COOL"))

(defun hcv-tags-load-co ()
  "Set `tags-table-list' to Office's TAGS files."
  (interactive)
  (setq tags-table-list (list (concat hcv-co-default-build-dir "TAGS")))
  (message "Tags table list switched to Office"))

(defun hcv-clean-cool ()
  "Run make clean for COOL."
  (interactive)
  (let ((cmd (concat "make -C " hcv-cool-default-build-dir " clean")))
    (add-to-history 'compile-history cmd)
    (compile cmd)))

(defun hcv-clean-co ()
  "Run make clean for Office."
  (interactive)
  (let ((cmd (concat "make -C " hcv-co-default-build-dir " clean")))
    (add-to-history 'compile-history cmd)
    (compile cmd)))

(defun hcv--assert-build-preconditions (&optional fresh)
  "Signal `user-error' if required files for a build-all are missing.
When FRESH is non-nil, require defaults files even if config.status exists."
  (let (missing)
    (unless (file-exists-p hcv-cool-configure-ac)
      (push (format "COOL configure.ac: %s" hcv-cool-configure-ac) missing))
    (unless (file-exists-p hcv-co-configure-ac)
      (push (format "Office configure.ac: %s" hcv-co-configure-ac) missing))
    (when (or fresh
              (not (file-exists-p hcv-cool-config-status)))
      (unless (file-exists-p (expand-file-name hcv-cool-config-default-ac))
        (push (format "COOL defaults: %s" hcv-cool-config-default-ac) missing)))
    (when (or fresh
              (not (file-exists-p hcv-co-config-status)))
      (unless (file-exists-p (expand-file-name hcv-co-config-default-ac))
        (push (format "Office defaults: %s" hcv-co-config-default-ac) missing)))
    (when missing
      (user-error "hcv-build-all: missing required files:\n  %s"
                  (mapconcat #'identity (nreverse missing) "\n  ")))))

(defun hcv-build-all (&optional fresh)
  "Configure + make Office, then configure + make COOL.
Configures a project only if its config.status is missing.

With prefix arg FRESH, force-reload defaults from disk (ignoring
config.status and any in-memory widget edits) and always regenerate
configure for both projects."
  (interactive "P")
  (hcv--assert-build-preconditions fresh)
  (when fresh
    (setq hcv-cool-config-list
          (hcv-read-default-environment
           (hcv-read-configure-options hcv-cool-configure-ac)))
    (hcv-read-config-default hcv-cool-config-default-ac)
    (setq hcv-co-config-list
          (hcv-read-default-environment
           (hcv-read-configure-options hcv-co-configure-ac)))
    (hcv-read-config-default hcv-co-config-default-ac))
  (let (steps)
    ;; Office first (COOL depends on Office's LOKit).
    (when (or fresh (not (file-exists-p hcv-co-config-status)))
      (push (hcv-get-shell-command
             hcv-co-config-list
             (expand-file-name "engine/" command-line-default-directory)
             hcv-co-default-build-dir
             hcv-co-configure-file)
            steps))
    (push (concat "make -j " hcv-num-cores " -C " hcv-co-default-build-dir) steps)
    ;; Then COOL.
    (when (or fresh (not (file-exists-p hcv-cool-config-status)))
      (push (hcv-get-shell-command
             hcv-cool-config-list
             (expand-file-name command-line-default-directory)
             hcv-cool-default-build-dir
             hcv-cool-configure-file)
            steps))
    (push (concat "make -j " hcv-num-cores " -C " hcv-cool-default-build-dir) steps)
    (make-directory hcv-co-default-build-dir t)
    (make-directory hcv-cool-default-build-dir t)
    (let ((cmd (mapconcat #'identity (nreverse steps) " && \\\n")))
      (add-to-history 'compile-history cmd)
      (compile cmd))))

;;; --- Run the Qt app (coda-qt) on an X display -----------------------------
;; Shown in the COOL menu only when the current build is configured/built with
;; --enable-qtapp.  Launches <build>/qt/coda-qt directly on a chosen display,
;; forcing software rendering so it also works on GPU-less / headless servers.

(defcustom hcv-coda-qt-display (or (getenv "DISPLAY") ":99")
  "X display the coda-qt run menu starts with."
  :type 'string)

(defcustom hcv-coda-qt-chromium-flags
  "--disable-gpu --disable-gpu-compositing --disable-software-rasterizer --no-sandbox"
  "Flags passed to the embedded Chromium via QTWEBENGINE_CHROMIUM_FLAGS."
  :type 'string)

(defcustom hcv-coda-qt-debug-port 9222
  "Chromium remote-debugging port used by the coda-qt \"Debug\" run commands.
They set QTWEBENGINE_REMOTE_DEBUGGING to this and add --remote-allow-origins=*
to the Chromium flags (required on Chromium >=111 / Qt 6.5); then connect Chrome
DevTools to http://127.0.0.1:PORT (from a PC, tunnel it with coda-qt-devtools.sh)."
  :type 'integer)

(defvar hcv-coda-qt-software-env
  '("LIBGL_ALWAYS_SOFTWARE=1"
    "QT_QUICK_BACKEND=software"
    "QT_OPENGL=software"
    "QTWEBENGINE_DISABLE_SANDBOX=1")
  "Environment forcing software rendering for Qt (no GPU).
The embedded Chromium flags are added separately from
`hcv-coda-qt-chromium-flags' so they can be edited in the run UI.")

(defcustom hcv-coda-qt-accessibility t
  "When non-nil, the X11 Run joins the headless accessibility session (Orca).
If a11y-start.sh's env file exists for the chosen display (see
`hcv-coda-qt-a11y-env-file'), its D-Bus / Qt-a11y variables are exported into
coda-qt along with QT_ACCESSIBILITY=1, and --force-renderer-accessibility is
added to the Chromium flags, so Orca can read the UI."
  :type 'boolean)

(defcustom hcv-coda-qt-a11y-env-file "/tmp/a11y-session%s.env"
  "Sourceable env file written by a11y-start.sh; %s is the X display number."
  :type 'string)

(defun hcv-coda-qt--a11y-env (display)
  "Environment entries (\"VAR=VAL\") that join the a11y session on DISPLAY, or nil.
Reads the `export VAR=...' lines from the a11y env file for DISPLAY's number
\(skipping DISPLAY and QT_ACCESSIBILITY) and appends QT_ACCESSIBILITY=1.
Returns nil when `hcv-coda-qt-accessibility' is off or the env file is absent —
i.e. when the headless a11y stack isn't running."
  (when hcv-coda-qt-accessibility
    (let* ((dnum (if (string-match ":\\([0-9]+\\)" display) (match-string 1 display) "0"))
           (file (format hcv-coda-qt-a11y-env-file dnum)))
      (when (file-readable-p file)
        (with-temp-buffer
          (insert-file-contents file)
          (goto-char (point-min))
          (let (vars)
            (while (re-search-forward
                    "^[ \t]*export[ \t]+\\([A-Za-z_][A-Za-z0-9_]*\\)=\\(.*\\)$" nil t)
              (let ((name (match-string 1))
                    (val  (string-trim (match-string 2) "[ \t'\"]+" "[ \t'\"]+")))
                (unless (member name '("DISPLAY" "QT_ACCESSIBILITY"))
                  (push (concat name "=" val) vars))))
            (nreverse (cons "QT_ACCESSIBILITY=1" vars))))))))

(defun hcv-coda-qt-binary ()
  "Path to the Qt app binary for the current COOL build."
  (expand-file-name "qt/coda-qt" hcv-cool-default-build-dir))

(defun hcv-coda-qt-available-p ()
  "Non-nil if the current build offers the Qt app.
True when the qt/coda-qt binary exists, or the build was configured with
--enable-qtapp (so the entry appears as soon as you configure for Qt)."
  (or (and (stringp hcv-cool-default-build-dir)
           (not (string-empty-p hcv-cool-default-build-dir))
           (file-executable-p (hcv-coda-qt-binary)))
      (and (stringp hcv-cool-config-status)
           (file-exists-p hcv-cool-config-status)
           (with-temp-buffer
             (insert-file-contents hcv-cool-config-status)
             (and (re-search-forward "--enable-qtapp" nil t) t)))))

(defvar hcv-coda-qt-buffer "*coda-qt parameters*"
  "Buffer name for the coda-qt run-parameters UI.")

(defvar hcv-coda-qt-project-name "coda-qt"
  "Header label for the coda-qt run-parameters UI.")

(defun hcv-coda-qt--display-connects-p (display)
  "Whether X DISPLAY accepts a connection.
Returns t/nil via xdpyinfo (or xset as a fallback); returns the symbol
`unknown' when neither probing tool is installed."
  (cond
   ((executable-find "xdpyinfo")
    (eq 0 (call-process "xdpyinfo" nil nil nil "-display" display)))
   ((executable-find "xset")
    (eq 0 (call-process "xset" nil nil nil "-display" display "q")))
   (t 'unknown)))

(defcustom hcv-coda-qt-wayland-display "wayland-1"
  "WAYLAND_DISPLAY the coda-qt run UI starts with for the native-Wayland Run."
  :type 'string)

(defun hcv-coda-qt--runtime-dir ()
  "XDG_RUNTIME_DIR, used to locate the Wayland socket."
  (or (getenv "XDG_RUNTIME_DIR") (format "/run/user/%d" (user-uid))))

(defun hcv-coda-qt--wayland-reachable-p (wd)
  "Non-nil if the Wayland socket WD exists under XDG_RUNTIME_DIR."
  (file-exists-p (expand-file-name wd (hcv-coda-qt--runtime-dir))))

(defun hcv-coda-qt--detect-wayland ()
  "Name of a running Wayland session's socket under XDG_RUNTIME_DIR, or nil.
wlroots auto-picks wayland-N, so detect it rather than assume.  Prefers a
socket other than Emacs's own WAYLAND_DISPLAY (e.g. the headless labwc one)."
  (let* ((dir  (hcv-coda-qt--runtime-dir))
         (self (getenv "WAYLAND_DISPLAY"))
         (socks (and (file-directory-p dir)
                     (seq-filter (lambda (f) (string-match-p "\\`wayland-[0-9]+\\'" f))
                                 (directory-files dir)))))
    (or (seq-find (lambda (s) (not (equal s self))) socks)
        (car socks))))

(defcustom hcv-coda-qt-examples-dir "test/samples"
  "Directory of example documents offered in the run UI's Document radio.
Relative to the worktree (`command-line-default-directory') unless absolute."
  :type 'string)

(defcustom hcv-coda-qt-extra-examples '("test/data/hello.odt")
  "Extra example documents for the radio, added after those in
`hcv-coda-qt-examples-dir'.  Each is relative to the worktree unless absolute."
  :type '(repeat string))

(defcustom hcv-coda-qt-document nil
  "Last document chosen in the run UI, persisted across sessions.
nil means fall back to the first example."
  :type '(choice (const :tag "First example" nil) string))

(defun hcv-coda-qt--examples ()
  "Absolute paths of example documents: those under `hcv-coda-qt-examples-dir'
plus `hcv-coda-qt-extra-examples' (existing files only)."
  (let* ((dir (expand-file-name hcv-coda-qt-examples-dir command-line-default-directory))
         (from-dir (when (file-directory-p dir)
                     (seq-filter #'file-regular-p (directory-files dir t "\\`[^.]"))))
         (extra (seq-filter
                 #'file-exists-p
                 (mapcar (lambda (f) (expand-file-name f command-line-default-directory))
                         hcv-coda-qt-extra-examples))))
    (append from-dir extra)))

(defun hcv-coda-qt--run-cmd (env-list document buffer-tag)
  "Launch the current build's Qt app with ENV-LIST exported, opening DOCUMENT.
Output streams to *coda-qt BUFFER-TAG*.  cwd = the build's qt/ dir (coda-qt
loads resources relative to it); the binary is invoked by absolute path since
it lives out-of-tree under the build dir.  Forces software rendering via
ENV-LIST.  The async buffer opens with the exact command and environment."
  (let* ((process-environment (append env-list process-environment))
         (default-directory (file-name-directory (hcv-coda-qt-binary)))
         (run (concat (shell-quote-argument (hcv-coda-qt-binary))
                      (when (and document (not (string-empty-p document)))
                        (concat " " (shell-quote-argument document)))))
         (preview (concat (mapconcat #'identity env-list " ") " " run)))
    (hcv-async-shell-command run (format "*coda-qt %s*" buffer-tag) preview)))

(defun hcv-coda-qt--launch (display document chromium-flags &optional debug)
  "Launch coda-qt under X11 on DISPLAY opening DOCUMENT.
CHROMIUM-FLAGS is exported as QTWEBENGINE_CHROMIUM_FLAGS.  With DEBUG non-nil,
enable Chromium remote debugging (QTWEBENGINE_REMOTE_DEBUGGING +
--remote-allow-origins=*).  Aborts if DISPLAY is not reachable (start the X
session first)."
  (pcase (hcv-coda-qt--display-connects-p display)
    ('nil (user-error "Display %s is not reachable — start the X session first"
                      display))
    ('unknown (message "coda-qt: cannot verify display %s (no xdpyinfo/xset); launching anyway"
                       display)))
  (let* ((a11y (hcv-coda-qt--a11y-env display))
         (flags (string-trim
                 (concat (or chromium-flags "")
                         (when a11y " --force-renderer-accessibility")
                         (when debug " --remote-allow-origins=*"))))
         (dbg (when debug
                (list (format "QTWEBENGINE_REMOTE_DEBUGGING=%d" hcv-coda-qt-debug-port)))))
    (when a11y (message "coda-qt: joining accessibility session (Orca) on %s" display))
    (when debug (message "coda-qt: remote debugging on port %d — connect DevTools to http://127.0.0.1:%d"
                         hcv-coda-qt-debug-port hcv-coda-qt-debug-port))
    (hcv-coda-qt--run-cmd
     (append (list (concat "DISPLAY=" display)
                   (concat "QTWEBENGINE_CHROMIUM_FLAGS=" flags))
             dbg
             hcv-coda-qt-software-env
             a11y)
     document (if debug (concat display " debug") display))))

(defun hcv-coda-qt--launch-wayland (wd document chromium-flags &optional debug)
  "Launch coda-qt natively under Wayland on WAYLAND_DISPLAY WD opening DOCUMENT.
Sets QT_QPA_PLATFORM=wayland.  With DEBUG non-nil, enable Chromium remote
debugging (QTWEBENGINE_REMOTE_DEBUGGING + --remote-allow-origins=*).  Aborts if
the Wayland socket is absent (start the Wayland session first)."
  (unless (hcv-coda-qt--wayland-reachable-p wd)
    (user-error "Wayland socket %s not found in %s — start the Wayland session first"
                wd (hcv-coda-qt--runtime-dir)))
  (let* ((flags (string-trim
                 (concat (or chromium-flags "")
                         (when debug " --remote-allow-origins=*"))))
         (dbg (when debug
                (list (format "QTWEBENGINE_REMOTE_DEBUGGING=%d" hcv-coda-qt-debug-port)))))
    (when debug (message "coda-qt: remote debugging on port %d — connect DevTools to http://127.0.0.1:%d"
                         hcv-coda-qt-debug-port hcv-coda-qt-debug-port))
    (hcv-coda-qt--run-cmd
     (append (list (concat "WAYLAND_DISPLAY=" wd)
                   "QT_QPA_PLATFORM=wayland"
                   (concat "XDG_RUNTIME_DIR=" (hcv-coda-qt--runtime-dir))
                   (concat "QTWEBENGINE_CHROMIUM_FLAGS=" flags))
             dbg
             hcv-coda-qt-software-env)
     document (concat "wl " wd (if debug " debug" "")))))

(defun hcv-coda-qt--remember (display wayland document chromium-flags)
  "Persist DISPLAY, WAYLAND, DOCUMENT and CHROMIUM-FLAGS for future sessions.
Only the values that actually changed are saved, so `custom-file' is not
rewritten when nothing was edited.  Saved through Customize, so the generic
defaults stay in this file and your personal choices live in your customizations."
  (unless (equal display hcv-coda-qt-display)
    (customize-save-variable 'hcv-coda-qt-display display))
  (unless (equal wayland hcv-coda-qt-wayland-display)
    (customize-save-variable 'hcv-coda-qt-wayland-display wayland))
  (unless (equal document hcv-coda-qt-document)
    (customize-save-variable 'hcv-coda-qt-document document))
  (unless (equal chromium-flags hcv-coda-qt-chromium-flags)
    (customize-save-variable 'hcv-coda-qt-chromium-flags chromium-flags)))

(defvar-local hcv-coda-qt--widgets nil
  "Plist of the run-parameter widgets in the current coda-qt buffer.")

(defun hcv-coda-qt--field (key)
  "Current value of run-parameter widget KEY (:display/:wayland/:document/:chromium)."
  (widget-value (plist-get hcv-coda-qt--widgets key)))

(defun hcv-coda-qt--do-run (platform &optional debug)
  "Persist the fields, launch coda-qt for PLATFORM (`x11' or `wayland'), then
close the params buffer (unless the launch aborted — unreachable target).
With DEBUG non-nil, launch with Chromium remote debugging enabled."
  (let ((d (hcv-coda-qt--field :display))
        (wl (hcv-coda-qt--field :wayland))
        (doc (hcv-coda-qt--field :document))
        (c (hcv-coda-qt--field :chromium)))
    (hcv-coda-qt--remember d wl doc c)
    (if (eq platform 'wayland)
        (hcv-coda-qt--launch-wayland wl doc c debug)
      (hcv-coda-qt--launch d doc c debug))
    (when (get-buffer hcv-coda-qt-buffer)
      (kill-buffer hcv-coda-qt-buffer))))

(defun hcv-coda-qt-run-x11 ()
  "Run coda-qt under X11 from the params buffer."
  (interactive) (hcv-coda-qt--do-run 'x11))

(defun hcv-coda-qt-run-wayland ()
  "Run coda-qt natively under Wayland from the params buffer."
  (interactive) (hcv-coda-qt--do-run 'wayland))

(defun hcv-coda-qt-run-x11-debug ()
  "Run coda-qt under X11 with Chromium remote debugging enabled."
  (interactive) (hcv-coda-qt--do-run 'x11 t))

(defun hcv-coda-qt-run-wayland-debug ()
  "Run coda-qt natively under Wayland with Chromium remote debugging enabled."
  (interactive) (hcv-coda-qt--do-run 'wayland t))

(defun hcv-coda-qt-cancel ()
  "Close the coda-qt params buffer without launching."
  (interactive) (when (get-buffer hcv-coda-qt-buffer) (kill-buffer hcv-coda-qt-buffer)))

(defvar hcv-coda-qt-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m widget-keymap)
    (define-key m "x" #'hcv-coda-qt-run-x11)
    (define-key m "X" #'hcv-coda-qt-run-x11)
    (define-key m "w" #'hcv-coda-qt-run-wayland)
    (define-key m "W" #'hcv-coda-qt-run-wayland)
    (define-key m "d" #'hcv-coda-qt-run-x11-debug)
    (define-key m "D" #'hcv-coda-qt-run-wayland-debug)
    (define-key m "q" #'hcv-coda-qt-cancel)
    m)
  "Keymap for the coda-qt params buffer: single-key run shortcuts X / W (run),
d / D (run with remote debugging, X11 / Wayland), q (cancel). Active when point
is not inside an editable field (where those letters type normally); the buffer
opens with point outside the fields.")

(defun hcv-coda-qt ()
  "Open a widget buffer to set coda-qt run parameters, with Run buttons.
Modeled on the Collabora configure UI: edit the fields if needed, then press
a Run button or just X (X11) / W (Wayland) / q (cancel)."
  (interactive)
  (switch-to-buffer hcv-coda-qt-buffer)
  (kill-all-local-variables)
  (let ((inhibit-read-only t)) (erase-buffer))
  (remove-overlays)
  (widget-insert (format "%s — run parameters.\n\n" hcv-coda-qt-project-name))
  (let (display wayland document chromium radio)
    (setq display (widget-create 'editable-field
                                 :format (hcv--bold-label-format "Display:  %v")
                                 :size 12
                                 hcv-coda-qt-display))
    (widget-insert "\n")
    (setq wayland (widget-create 'editable-field
                                 :format (hcv--bold-label-format "Wayland:  %v")
                                 :size 12
                                 (or (hcv-coda-qt--detect-wayland)
                                     hcv-coda-qt-wayland-display)))
    (widget-insert "\n")
    (let* ((examples (hcv-coda-qt--examples))
           (hello (expand-file-name "test/data/hello.odt" command-line-default-directory))
           ;; persisted choice, else hello.odt if available, else the first example.
           (doc-default (or (and (stringp hcv-coda-qt-document)
                                 (not (string-empty-p hcv-coda-qt-document))
                                 hcv-coda-qt-document)
                            (car (member hello examples))
                            (car examples))))
      (setq document (widget-create 'editable-field
                                    :format (hcv--bold-label-format "Document: %v")
                                    :size 60
                                    doc-default))
      (widget-insert "\n")
      ;; Example documents; picking one fills the Document field above. The
      ;; field stays the source of truth, so you can also type any other path.
      (when examples
        (setq radio
              (apply #'widget-create 'radio-button-choice
                     :value (if (member doc-default examples) doc-default (car examples))
                     :indent 10
                     :notify (lambda (w &rest _)
                               (when (stringp (widget-value w))
                                 (widget-value-set document (widget-value w))))
                     (mapcar (lambda (f)
                               (list 'item :tag (file-name-nondirectory f) :value f))
                             examples)))))
    (widget-insert "\n")
    (setq chromium (widget-create 'editable-field
                                  :format (hcv--bold-label-format "Chromium: %v")
                                  :size 60
                                  hcv-coda-qt-chromium-flags))
    (setq hcv-coda-qt--widgets
          (list :display display :wayland wayland :document document
                :chromium chromium :examples-radio radio))
    (widget-insert "\n\n")
    ;; Buttons and shortcuts both go through the run commands, which read the
    ;; buffer-local widgets, persist, launch, then close the buffer (unless the
    ;; launch aborts because the display/socket was unreachable).
    (widget-create 'push-button
                   :notify (lambda (&rest _) (hcv-coda-qt-run-wayland)) "Run Wayland")
    (widget-insert "  ")
    (widget-create 'push-button
                   :notify (lambda (&rest _) (hcv-coda-qt-run-x11)) "Run X11")
    (widget-insert "  ")
    (widget-create 'push-button
                   :notify (lambda (&rest _) (hcv-coda-qt-run-wayland-debug)) "Debug Wayland")
    (widget-insert "  ")
    (widget-create 'push-button
                   :notify (lambda (&rest _) (hcv-coda-qt-run-x11-debug)) "Debug X11")
    (widget-insert "  ")
    (widget-create 'push-button
                   :notify (lambda (&rest _) (hcv-coda-qt-cancel)) "Cancel")
    (widget-insert "\n\n(W: Wayland   X: X11   D: Debug Wayland   d: Debug X11   q: Cancel)"))
  (use-local-map hcv-coda-qt-map)
  (widget-setup)
  (goto-char (point-min)))

;;; --- Run Collabora Office (soffice.bin) on a display ----------------------
;; Same UI as coda-qt, for the desktop soffice binary.  Reuses the session
;; display / Wayland values and the display/socket checks; soffice selects its
;; backend via GDK_BACKEND (it uses the gtk3 VCL plugin).

(defvar hcv-co-run-buffer "*soffice parameters*"
  "Buffer name for the Collabora Office run-parameters UI.")

(defcustom hcv-co-run-args "--norestore --nologo"
  "Default soffice.bin arguments in the Office run UI.
No module flag (e.g. --writer): soffice auto-detects the module from the
selected Document.  Add a flag yourself to force a module / open a blank doc."
  :type 'string)

(defvar hcv-co-software-env
  '("LIBGL_ALWAYS_SOFTWARE=1" "SAL_USE_VCLPLUGIN=gtk3")
  "Environment to run soffice software-rendered with GTK, so GDK_BACKEND
selects X11 vs Wayland.")

(defun hcv-co--soffice ()
  "Path to the current build's soffice.bin."
  (expand-file-name "instdir/program/soffice.bin" hcv-co-default-build-dir))

(defun hcv-co-run-available-p ()
  "Non-nil if the current build has a runnable soffice.bin."
  (and (boundp 'hcv-co-default-build-dir)
       (stringp hcv-co-default-build-dir)
       (file-executable-p (hcv-co--soffice))))

(defun hcv-co--run-cmd (env-list args document buffer-tag)
  "Launch soffice.bin with ENV-LIST exported, ARGS and DOCUMENT appended.
ARGS is a shell fragment (split by the shell); DOCUMENT, when non-empty, is
the file soffice opens.  Output streams to *soffice BUFFER-TAG* (command echoed)."
  (let* ((soffice (hcv-co--soffice))
         (process-environment (append env-list process-environment))
         (default-directory (file-name-directory soffice))
         (run (concat (shell-quote-argument soffice)
                      (if (and args (not (string-empty-p args))) (concat " " args) "")
                      (if (and document (not (string-empty-p document)))
                          (concat " " (shell-quote-argument document)) "")))
         (preview (concat (mapconcat #'identity env-list " ") " " run)))
    (hcv-async-shell-command run (format "*soffice %s*" buffer-tag) preview)))

(defun hcv-co--launch-x11 (display args document)
  "Launch soffice under X11 on DISPLAY with ARGS opening DOCUMENT.
Aborts if the display is unreachable."
  (pcase (hcv-coda-qt--display-connects-p display)
    ('nil (user-error "Display %s is not reachable — start the X session first"
                      display))
    ('unknown (message "office: cannot verify display %s (no xdpyinfo/xset); launching anyway"
                       display)))
  (hcv-co--run-cmd (append (list (concat "DISPLAY=" display) "GDK_BACKEND=x11")
                           hcv-co-software-env)
                   args document display))

(defun hcv-co--launch-wayland (wd args document)
  "Launch soffice natively under Wayland on WAYLAND_DISPLAY WD with ARGS opening DOCUMENT."
  (unless (hcv-coda-qt--wayland-reachable-p wd)
    (user-error "Wayland socket %s not found in %s — start the Wayland session first"
                wd (hcv-coda-qt--runtime-dir)))
  (hcv-co--run-cmd (append (list (concat "WAYLAND_DISPLAY=" wd) "GDK_BACKEND=wayland"
                                 (concat "XDG_RUNTIME_DIR=" (hcv-coda-qt--runtime-dir)))
                           hcv-co-software-env)
                   args document (concat "wl " wd)))

(defun hcv-co--remember (display wayland args document)
  "Persist the session DISPLAY/WAYLAND, soffice ARGS and DOCUMENT (when changed).
The document is shared with coda-qt (`hcv-coda-qt-document')."
  (unless (equal display hcv-coda-qt-display)
    (customize-save-variable 'hcv-coda-qt-display display))
  (unless (equal wayland hcv-coda-qt-wayland-display)
    (customize-save-variable 'hcv-coda-qt-wayland-display wayland))
  (unless (equal args hcv-co-run-args)
    (customize-save-variable 'hcv-co-run-args args))
  (unless (equal document hcv-coda-qt-document)
    (customize-save-variable 'hcv-coda-qt-document document)))

(defvar-local hcv-co--widgets nil
  "Plist of the run-parameter widgets in the current soffice buffer.")

(defun hcv-co--field (key)
  "Current value of soffice run widget KEY (:display/:wayland/:args/:document)."
  (widget-value (plist-get hcv-co--widgets key)))

(defun hcv-co--do-run (platform)
  "Persist fields, launch soffice for PLATFORM, then close the buffer
(unless the launch aborted)."
  (let ((d (hcv-co--field :display))
        (wl (hcv-co--field :wayland))
        (args (hcv-co--field :args))
        (doc (hcv-co--field :document)))
    (hcv-co--remember d wl args doc)
    (if (eq platform 'wayland)
        (hcv-co--launch-wayland wl args doc)
      (hcv-co--launch-x11 d args doc))
    (when (get-buffer hcv-co-run-buffer) (kill-buffer hcv-co-run-buffer))))

(defun hcv-co-run-x11 ()
  "Run soffice under X11 from the params buffer."
  (interactive) (hcv-co--do-run 'x11))

(defun hcv-co-run-wayland ()
  "Run soffice natively under Wayland from the params buffer."
  (interactive) (hcv-co--do-run 'wayland))

(defun hcv-co-run-cancel ()
  "Close the soffice params buffer without launching."
  (interactive) (when (get-buffer hcv-co-run-buffer) (kill-buffer hcv-co-run-buffer)))

(defvar hcv-co-run-map
  (let ((m (make-sparse-keymap)))
    (set-keymap-parent m widget-keymap)
    (define-key m "x" #'hcv-co-run-x11)
    (define-key m "X" #'hcv-co-run-x11)
    (define-key m "w" #'hcv-co-run-wayland)
    (define-key m "W" #'hcv-co-run-wayland)
    (define-key m "q" #'hcv-co-run-cancel)
    m)
  "Keymap for the soffice params buffer: single-key X / W / q run shortcuts.")

(defun hcv-co-run ()
  "Open a widget buffer to run Collabora Office (soffice) on a display.
Edit the fields if needed, then press a Run button or just X / W / q."
  (interactive)
  (switch-to-buffer hcv-co-run-buffer)
  (kill-all-local-variables)
  (let ((inhibit-read-only t)) (erase-buffer))
  (remove-overlays)
  (widget-insert "Collabora Office — run parameters.\n\n")
  (let (display wayland args document radio)
    (setq display (widget-create 'editable-field
                                 :format (hcv--bold-label-format "Display:  %v")
                                 :size 12 hcv-coda-qt-display))
    (widget-insert "\n")
    (setq wayland (widget-create 'editable-field
                                 :format (hcv--bold-label-format "Wayland:  %v")
                                 :size 12
                                 (or (hcv-coda-qt--detect-wayland)
                                     hcv-coda-qt-wayland-display)))
    (widget-insert "\n")
    (setq args (widget-create 'editable-field
                              :format (hcv--bold-label-format "Args:     %v")
                              :size 60 hcv-co-run-args))
    (widget-insert "\n")
    ;; Document field + example radios (shared with coda-qt). Empty = none
    ;; (soffice opens whatever Args implies, e.g. a blank --writer).
    (let* ((examples (hcv-coda-qt--examples))
           (hello (expand-file-name "test/data/hello.odt" command-line-default-directory))
           (doc-default (or (and (stringp hcv-coda-qt-document)
                                 (not (string-empty-p hcv-coda-qt-document))
                                 hcv-coda-qt-document)
                            (car (member hello examples))
                            (car examples)
                            "")))
      (setq document (widget-create 'editable-field
                                    :format (hcv--bold-label-format "Document: %v")
                                    :size 60 doc-default))
      (widget-insert "\n")
      (when examples
        (setq radio
              (apply #'widget-create 'radio-button-choice
                     :value (if (member doc-default examples) doc-default (car examples))
                     :indent 10
                     :notify (lambda (w &rest _)
                               (when (stringp (widget-value w))
                                 (widget-value-set document (widget-value w))))
                     (mapcar (lambda (f)
                               (list 'item :tag (file-name-nondirectory f) :value f))
                             examples)))))
    (setq hcv-co--widgets (list :display display :wayland wayland :args args
                                :document document :examples-radio radio))
    (widget-insert "\n\n")
    (widget-create 'push-button
                   :notify (lambda (&rest _) (hcv-co-run-wayland)) "Run Wayland")
    (widget-insert "  ")
    (widget-create 'push-button
                   :notify (lambda (&rest _) (hcv-co-run-x11)) "Run X11")
    (widget-insert "  ")
    (widget-create 'push-button
                   :notify (lambda (&rest _) (hcv-co-run-cancel)) "Cancel")
    (widget-insert "\n\n(W: Run Wayland   X: Run X11   q: Cancel)"))
  (use-local-map hcv-co-run-map)
  (widget-setup)
  (goto-char (point-min)))

(transient-define-prefix hcv-main-commands ()
  "Main Commands."
  ["Buffers & Navigation"
   ("b" "Buffer List"    list-buffers)
   ("r" "Recent Files"   recentf-open-files)
   ("m" "Bookmarks"      bookmark-bmenu-list)
   ("g" "Registers"      list-registers)
   ("y" "Yank Menu"      (lambda () (interactive) (redisplay) (popup-menu 'yank-menu)))]
  ["Projects"
   ("w" "Collabora Office…"  hcv-co-commands)
   ("o" "Collabora Online…"  hcv-cool-commands)
   ("a" "Build all"          hcv-build-all)])

(transient-define-prefix hcv-cool-commands ()
  "Collabora Online commands."
  [:description (lambda () (format "%s" hcv-cool-project-name))
   ("c" "Configure…"   hcv-configure-cool)
   ("m" "Make"         hcv-compile-cool)
   ("k" "Clean"        hcv-clean-cool)
   ("r" "Run"          hcv-run-cool)
   ("R" "Run coda-qt…" hcv-coda-qt :if hcv-coda-qt-available-p)
   ("L" "Log (head)"   hcv-head-config-cool)
   ("F" "Log (full)"   hcv-full-config-cool)
   ("t" "Tags: build"  hcv-tags-build-cool)
   ("T" "Tags: load"   hcv-tags-load-cool)])

(transient-define-prefix hcv-co-commands ()
  "Collabora Office commands."
  [:description (lambda () (format "%s" hcv-co-project-name))
   ("c" "Configure…"   hcv-configure-co)
   ("m" "Make"         hcv-compile-co)
   ("k" "Clean"        hcv-clean-co)
   ("r" "Run…"         hcv-co-run :if hcv-co-run-available-p)
   ("L" "Log (head)"   hcv-head-config-co)
   ("F" "Log (full)"   hcv-full-config-co)
   ("t" "Tags: build"  hcv-tags-build-co)
   ("T" "Tags: load"   hcv-tags-load-co)])

