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
(defvar hcv-env-variables '("CXXFLAGS")
  "Environment variables to expose as editable fields in the configure UI.")
(defvar hcv--options-obarray (obarray-make)
  "Private obarray for configure option symbols, to avoid polluting the global obarray.")

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


(defun hcv-read-configure-option (configure-string configure-description)
  "Parse CONFIGURE-STRING and return a symbol describing the option.
CONFIGURE-STRING looks like \"--enable-foo\" or \"--with-bar=VALUE\".
CONFIGURE-DESCRIPTION is stored as a property for later display.
The returned symbol is interned in `hcv--options-obarray' and carries the
properties `configure-string', `configure-description', `widget-type', and
\(for valued options\) `format'."
  (let* ((list-string (split-string configure-string "="))
         (option-name (nth 0 list-string))
         (configure-symbol (intern option-name hcv--options-obarray)))
    (set configure-symbol nil)
    (put configure-symbol 'configure-string configure-string)
    (put configure-symbol 'configure-description configure-description)
    (cond
     ((= (length list-string) 1)
      (put configure-symbol 'widget-type 'checkbox))
     (t
      (set configure-symbol "")
      (put configure-symbol 'format (concat option-name "=%v"))
      (put configure-symbol 'widget-type
           (cond
            ((string-match-p "file" configure-string) 'file)
            ((string-match-p "\\(path\\|dir\\|prefix\\)" configure-string) 'directory)
            (t 'editable-field)))))
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
            (push (hcv-read-configure-option (match-string 2) (match-string 3))
                  config-list)))
      (message "hcv: configure.ac file not found: %s" configure-ac-file))
    config-list))

(defun hcv-read-default-environment (config-list)
  "Prepend env-variable entries to CONFIG-LIST and return it.
Each name in `hcv-env-variables' is interned in `hcv--options-obarray'
and gets its initial value from the current environment (or empty)."
  (dolist (name hcv-env-variables config-list)
    (let ((sym (intern name hcv--options-obarray)))
      (set sym (or (getenv name) ""))
      (put sym 'widget-type 'editable-field)
      (put sym 'format (concat name "=%v"))
      (put sym 'tag 'env-variable)
      (push sym config-list))))

(defun hcv-read-config-status (config-status)
  "Read values from CONFIG-STATUS and update their symbols in `hcv--options-obarray'.
CONFIG-STATUS must be a path to an executable `config.status' script
\(typically produced by a previous `configure' run\).  Runs it with
`--config' and parses the quoted argument list.

For each `NAME=VALUE' entry, finds the matching symbol in
`hcv--options-obarray' and sets its value.  Options without a `=' (flags
like `--enable-foo') are set to t.

Returns non-nil if at least one option was read, nil otherwise."
  (let (config-list list-item list-string config-symbol config-value)
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
          pos)
      (while config-list
        (setq list-item (pop config-list))
        (setq pos (string-match "=" list-item))
        (if pos
            (setq config-symbol (intern (substring list-item 0 pos)
                                        hcv--options-obarray)
                  config-value  (substring list-item (1+ pos)))
          (setq config-symbol (intern list-item hcv--options-obarray)
                config-value  nil))
        (if config-value
            (set config-symbol config-value)
          (set config-symbol t)))
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
  (let (lines had-items config-string pos config-symbol config-value)
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
      (if pos
          (setq config-symbol (intern (substring config-string 0 pos)
                                      hcv--options-obarray)
                config-value  (hcv--unquote-value
                               (substring config-string (1+ pos))))
        (setq config-symbol (intern config-string hcv--options-obarray)
              config-value  nil))
      (if config-value
          (set config-symbol config-value)
        (set config-symbol t)))

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
  (unless (hcv-read-config-status hcv-cool-config-status)
    (hcv-read-config-default hcv-cool-config-default-ac))

  ;; --- Office config (same workspace, separate source tree) ---
  (setq hcv-co-config-list (hcv-read-configure-options hcv-co-configure-ac))
  (setq hcv-co-config-list (hcv-read-default-environment hcv-co-config-list))
  (unless (hcv-read-config-status hcv-co-config-status)
    (hcv-read-config-default hcv-co-config-default-ac))

  ;; --- Default compile command and TAGS (unused by the transient but
  ;;     handy when invoking `M-x compile' or `M-.' outside the menu) ---
  (setq compile-command (concat "make -j " hcv-num-cores " -C " hcv-cool-build-dir))

  ;; --- Bootstrap autogen if the configure script is missing ---
  (if (not (file-exists-p (expand-file-name hcv-cool-configure-file
                                            (expand-file-name command-line-default-directory))))
      (async-shell-command "./autogen.sh"))

  (global-set-key "\C-cc" 'hcv-main-commands))

(eval-when-compile
  (require 'wid-edit))

(defun hcv-config-list-update (configure-ac configure-status configure-default-ac)
  "Build and return a fresh config list from CONFIGURE-AC.
Apply values from CONFIGURE-STATUS if available; otherwise fall back to
CONFIGURE-DEFAULT-AC."
  (let ((config-list (hcv-read-default-environment
                      (hcv-read-configure-options configure-ac))))
    (unless (hcv-read-config-status configure-status)
      (hcv-read-config-default configure-default-ac))
    config-list))

(defun hcv-configure (config-list-var configure-buffer configure-title
                                      configure-ac configure-status configure-default-ac
                                      source-dir build-dir configure-file)
  "Render the configure buffer for CONFIGURE-BUFFER.
CONFIG-LIST-VAR is the symbol of the global variable holding the option
list for this project.  CONFIGURE-BUFFER is the name of the buffer to
display.  CONFIGURE-TITLE is inserted at the top as a header.
CONFIGURE-AC, CONFIGURE-STATUS and CONFIGURE-DEFAULT-AC are the inputs
used to refresh the list.  SOURCE-DIR, BUILD-DIR and CONFIGURE-FILE are
passed to the execute/copy buttons."
  (switch-to-buffer configure-buffer)
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert configure-title)

  (set config-list-var
       (hcv-config-list-update configure-ac configure-status configure-default-ac))

  (dolist (config (symbol-value config-list-var))
    (let* ((type (get config 'widget-type))
           (value (symbol-value config))
           (widget
            (cond
             ((eq type 'checkbox)
              (widget-create 'checkbox
                             :button-suffix (symbol-name config)
                             value))
             ((memq type '(editable-field directory file))
              (widget-create type
                             :format (get config 'format)
                             value)))))
      (put config 'widget widget)
      (when (eq type 'checkbox)
        (widget-insert "\n"))))

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
  "Build a shell command string to run CONFIGURE-FILE with CONFIG-LIST.
Intended for display and copy-paste into a shell, not for direct
execution.

CONFIG-LIST is a list of option symbols (from `hcv--options-obarray'),
each carrying a `widget' property with its current widget.  For each
option, emits `--name' for checked checkboxes and `NAME=VALUE' for
non-empty text/file/directory fields.  Values are shell-quoted.

BUILD-DIR is the directory the command should `cd' into before running
the script.  SOURCE-DIR is the directory containing CONFIGURE-FILE.
CONFIGURE-FILE is the relative filename of the configure script to
invoke."
  (let (out-list)
    (dolist (config config-list)
      (let* ((widget (get config 'widget))
             (type   (get config 'widget-type))
             (value  (widget-value widget)))
        (cond
         ((and (eq type 'checkbox) value)
          (push (symbol-name config) out-list))
         ((and (memq type '(editable-field directory file))
               (not (string-empty-p value)))
          (push (concat (symbol-name config) "=" (shell-quote-argument value))
                out-list)))))
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
    (async-shell-command cmd)))

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
                 hcv-cool-configure-file))

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
    (async-shell-command cmd)))

(defun hcv-run-co ()
  "Run Collabora Office (soffice.bin) from the build instdir."
  (interactive)
  (let* ((default-cmd (concat hcv-co-default-build-dir
                              "instdir/program/soffice.bin"
                              " --norestore --nologo --writer"))
         (cmd (read-shell-command "Run command: " default-cmd)))
    (async-shell-command cmd)))

(defun hcv-head-config-cool ()
  "Show head of config.log for COOL."
  (interactive)
  (async-shell-command (concat "cd " hcv-cool-default-build-dir " && head config.log")))

(defun hcv-head-config-co ()
  "Show head of config.log for Office."
  (interactive)
  (async-shell-command (concat "cd " hcv-co-default-build-dir " && head config.log")))

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
   ("r" "Run"          hcv-run-co)
   ("L" "Log (head)"   hcv-head-config-co)
   ("F" "Log (full)"   hcv-full-config-co)
   ("t" "Tags: build"  hcv-tags-build-co)
   ("T" "Tags: load"   hcv-tags-load-co)])

