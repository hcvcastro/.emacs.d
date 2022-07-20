(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-initialize)

;; custom variables
(setq custom-file "~/.emacs.d/custom-file.el")
(load custom-file)

;; Highlight corresponding parentheses when cursor is on one
(show-paren-mode t)

;; Highlight tabulations
;;(setq-default highlight-tabs t)

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

;; debugging preferences
(setq gud-chdir-before-run nil)
(setq comint-prompt-read-only t)
;;(setq gdbmi-debug-mode t)
;;(setq compilation-auto-jump-to-first-error t)
(setq gdb-show-main t)
(setq compilation-scroll-output t)

;; delete trailing whitespace before save
(add-hook 'before-save-hook 'delete-trailing-whitespace)

;; change cursor shape
(add-hook 'buffer-list-update-hook 'hcv-change-cursor-shape)
(add-hook 'read-only-mode-hook 'hcv-change-cursor-shape)
(add-hook 'window-configuration-change-hook 'hcv-change-cursor-shape)
(add-hook 'overwrite-mode-hook 'hcv-change-cursor-shape)

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

;; generic helpers
(global-set-key "\C-cb" 'list-buffers)
(global-set-key "\C-cr" 'list-registers)
(global-set-key "\C-cf" 'recentf-open-files)
(global-set-key "\C-cl" 'bookmark-bmenu-list)
(global-set-key "\C-cy" '(lambda () (interactive) (popup-menu 'yank-menu)))
(global-set-key "\C-cc" 'compile)
(global-set-key "\C-cx" '(lambda () (interactive) (async-shell-command hcv-run-command)))
(global-set-key "\C-cz" '(lambda () (interactive) (async-shell-command hcv-head-config)))

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

(push 'eslint compilation-error-regexp-alist-alist)

;; no scratch message
(setq initial-scratch-message nil)
;; initial buffer -> dired
(if (= (length command-line-args) 1) (setq initial-buffer-choice default-directory))

;; defaul prog mode read only buffers
(add-hook 'prog-mode-hook 'read-only-mode)
(add-hook 'prog-mode-hook (lambda () (setq show-trailing-whitespace t)))
(add-hook 'prog-mode-hook 'superword-mode)

;; apply ansi color to compilation buffer
(defun hcv-ansi-colorize-buffer ()
  (let ((buffer-read-only nil))
    (ansi-color-apply-on-region (point-min) (point-max))))

(add-hook 'compilation-filter-hook 'hcv-ansi-colorize-buffer)


;; setup default build output directory
(setq build-default-directory (purecopy command-line-default-directory))
(setq build-default-directory (replace-regexp-in-string "projects" "build" build-default-directory))

;; get the number of cpu cores
(setq hcv-num-cores (substring (shell-command-to-string "grep -c ^processor /proc/cpuinfo") nil -1))

;; --- configure custom buffer

;; Collabora Online workspace directory
(setq hcv-cool-workspace-dir (expand-file-name "~/projects/online/bin"))
;; Collabora Online output build directory
(setq hcv-cool-build-dir (expand-file-name "~/build/online/bin"))
;; Libre Office workspace directory
(setq hcv-lo-workspace-dir (expand-file-name "~/projects/online/lib/core"))
;; Libre Office output build directory
(setq hcv-lo-build-dir (expand-file-name "~/build/online/lib/core"))

;; custom local variables default directory
(dir-locals-set-class-variables
 'my-default-dir (list (cons 'nil (list (cons 'default-directory command-line-default-directory)))))
(dir-locals-set-directory-class command-line-default-directory 'my-default-dir)

;; configure.ac file
(setq hcv-configure-ac "configure.ac")
;; default configure parameters preferences directory
(setq hcv-config-default "~/config.default")
(setq hcv-configure nil)
(setq hcv-config-buffer nil)
(setq hcv-config-list nil)

(defun hcv-read-configure-option (configure-string configure-description)
  (let* ((list-string (split-string configure-string "="))
	 (configure-symbol (intern (nth 0 list-string))))
    (set configure-symbol nil)
    (put configure-symbol 'configure-string configure-string)
    (put configure-symbol 'configure-description configure-description)
    (if (> (length list-string) 1)
	(progn
	  (set configure-symbol "")
	  (put configure-symbol 'format (concat (symbol-name configure-symbol) "=%v"))
	  (cond ((string-match "path" configure-string)
		 (if (string-match "file" configure-string)
		     (put configure-symbol 'widget-type 'file)
		   (put configure-symbol 'widget-type 'directory)))
		(t (put configure-symbol 'widget-type 'editable-field))))
      (put configure-symbol 'widget-type 'checkbox))
    configure-symbol))

(defun hcv-read-configure-options (configure)
  (let ((config-list nil)
	(case-fold-search nil))
    (if (file-exists-p configure)
	(with-temp-buffer
	  (insert-file-contents configure)
	  (goto-char (point-max))
	  (while (re-search-backward
		  "\\(_ARG_ENABLE\\|_ARG_WITH\\)[^A]*AS_HELP_STRING[^[]*\\[\\([^]]*\\)[^[]*\\[\\([^]]*\\)"
		  nil t)
	    (push (hcv-read-configure-option (match-string 2) (match-string 3)) config-list))))
    config-list))

(defun hcv-read-default-environment (config-list)
  (setq CXXFLAGS "\"\"")
  (put 'CXXFLAGS 'widget-type 'editable-field)
  (put 'CXXFLAGS 'format (concat (symbol-name 'CXXFLAGS) "=%v"))
  (put 'CXXFLAGS 'tag 'env-variable)
  (push 'CXXFLAGS config-list)
  config-list)

(defun hcv-read-config-status (config-status)
  (let ((config-list nil)
	(list-string nil)
	(config-symbol nil)
	(config-value nil))
    (if (file-exists-p config-status)
	(progn (setq config-list (shell-command-to-string (concat config-status " --config")))
	       (setq config-list (replace-regexp-in-string "'" "\"" config-list))
	       (setq config-list (concat "(" config-list ")"))
	       (setq config-list (read-from-string config-list))
	       (setq config-list (car config-list)))
      (setq config-list nil))
    (while config-list
      (setq list-string (split-string (car config-list) "="))
      (setq config-symbol (intern (nth 0 list-string)))
      (setq config-value (nth 1 list-string))
      (if (> (length list-string) 1)
	  (if (eq (get config-symbol 'tag) 'env-variable)
	      (set config-symbol (concat "\"" config-value "\""))
	    (set config-symbol config-value))
	(set config-symbol t))
      (setq config-list (cdr config-list)))
    (> (length config-list) 0)))

(defun hcv-read-config-default (configure-default)
  (let ((config-list nil)
	(list-string nil)
	(config-symbol nil)
	(config-value nil)
	(config-string nil)
	(pos))
    (if (file-exists-p configure-default)
	(progn (setq config-list (with-temp-buffer (insert-file-contents configure-default)
						   (buffer-string)))
	       (setq config-list (split-string config-list "\n"))))
    (while config-list
      (setq config-string (car config-list))
      (setq list-string (split-string config-string "="))
      (if (> (length list-string) 2)
	  (progn (setq pos (string-match "=" config-string))
		 (push (substring config-string (1+ pos)) list-string)
		 (push (substring config-string 0 pos) list-string)))
      (setq config-symbol (intern (nth 0 list-string)))
      (setq config-value (nth 1 list-string))
      (if (> (length list-string) 1)
	  (set config-symbol config-value)
	(set config-symbol t))
      (setq config-list (cdr config-list)))
     (> (length config-list) 0)))

(setq hcv-run-command "")
(cond ((string-match-p hcv-cool-workspace-dir (expand-file-name command-line-default-directory))
       (setq hcv-cool-build-dir
	     (concat hcv-cool-build-dir
		     (substring (expand-file-name command-line-default-directory)
				(length hcv-cool-workspace-dir))))
       (setq hcv-default-build-dir hcv-cool-build-dir)
       (setq hcv-config-status (concat hcv-default-build-dir "config.status"))
       (setq hcv-configure "configure")
       (setq hcv-config-buffer "*Collabora Online Configure*")
       (put 'hcv-config-buffer 'widget-title "Collabora Online Configure.\n\n")
       (setq hcv-config-list (hcv-read-configure-options hcv-configure-ac))
       (setq hcv-config-list (hcv-read-default-environment hcv-config-list))
       (if (not (hcv-read-config-status hcv-config-status))
	   (hcv-read-config-default hcv-config-default))
       (setq compile-command (concat "make -j " hcv-num-cores " -C " hcv-cool-build-dir))
       (setq tags-table-list (list (concat hcv-default-build-dir "TAGS")
				   (concat hcv-default-build-dir "browser/TAGS")))
       (setq hcv-run-command (concat "cd " hcv-default-build-dir " && make run"))
       (setq hcv-head-config (concat "cd " hcv-default-build-dir " && head config.log"))
       (if (not (file-exists-p hcv-configure))
	   (async-shell-command "./autogen.sh")))
      ((string-match-p hcv-lo-workspace-dir (expand-file-name command-line-default-directory))
       (setq hcv-lo-build-dir
	     (concat hcv-lo-build-dir
		     (substring (expand-file-name command-line-default-directory)
				(length hcv-lo-workspace-dir))))
       (setq hcv-default-build-dir hcv-lo-build-dir)
       (setq hcv-config-status (concat hcv-default-build-dir "config.status"))
       (setq hcv-configure "autogen.sh")
       (setq hcv-config-buffer "*LibreOffice Configure*")
       (put 'hcv-config-buffer 'widget-title "LibreOffice Configure.\n\n")
       (setq hcv-config-list (hcv-read-configure-options hcv-configure-ac))
       (hcv-read-default-environment hcv-config-list)
       (if (not (hcv-read-config-status hcv-config-status))
	   (hcv-read-config-default hcv-config-default))
       (setq compile-command (concat "make -C " hcv-default-build-dir " build-nocheck"))
       (setq tags-file-name (concat hcv-default-build-dir "TAGS"))
       (setq hcv-run-command (concat "cd " hcv-default-build-dir " && instdir/program/soffice.bin --norestore --nologo --writer"))
       (setq hcv-head-config (concat "cd " hcv-default-build-dir " && head config.log"))))

(push hcv-run-command shell-command-history)

(defun hcv-create-tags ()
    (interactive)
    (compile (concat "make -C " hcv-default-build-dir " tags")))


(require 'widget)
(require 'gnus)

(eval-when-compile
  (require 'wid-edit))

(defun hcv-configure ()
  (interactive)
  (switch-to-buffer hcv-config-buffer)
  (kill-all-local-variables)
  (let ((inhibit-read-only t))
    (erase-buffer))
  (remove-overlays)
  (widget-insert (get 'hcv-config-buffer 'widget-title))
  (let ((config-list hcv-config-list)
	(config)
	(type))
    (while config-list
      (setq config (car config-list))
      (setq type (get config 'widget-type))
      (if (eq type 'checkbox)
	  (progn (put config 'widget (widget-create 'checkbox
						    :button-suffix (symbol-name config)
						    (symbol-value config)))
		 (widget-insert "\n")))
      (if (eq type 'editable-field)
	  (progn (put config 'widget (widget-create 'editable-field
						    :format (get config 'format)
						    (symbol-value config)))))
      (if (eq type 'directory)
	  (progn (put config 'widget (widget-create 'directory
						    :format (get config 'format)
						    (symbol-value config)))))
      (if (eq type 'file)
	  (progn (put config 'widget (widget-create 'file
						    :format (get config 'format)
						    (symbol-value config)))))
      (setq config-list (cdr config-list))))
  (widget-create 'push-button
		 :notify 'hcv-execute-configure
		 "Configure")
  (widget-insert " ")
  (widget-create 'push-button
		 :notify 'hcv-execute-copy-clipboard
		 "Copy")
  (use-local-map widget-keymap)
  (widget-setup))

(defun hcv-get-shell-command (config-list)
  (let ((config)
	(type)
	(widget)
	(value)
	(out-list)
	(cmd))
    (while config-list
      (setq config (car config-list))
      (setq widget (get config 'widget))
      (setq type (get config 'widget-type))
      (setq value (widget-value widget))
      (if (and (eq type 'checkbox) value)
	  (push (symbol-name config) out-list))
      (if (and (eq type 'editable-field) (not (string= value "")))
	  (push (concat (symbol-name config) "=" value) out-list))
      (if (and (eq type 'directory) (not (string= value "")))
	  (push (concat (symbol-name config) "=" value) out-list))
      (if (and (eq type 'file) (not (string= value "")))
	  (push (concat (symbol-name config) "=" value) out-list))
      (setq config-list (cdr config-list)))
    (setq cmd (concat "cd " hcv-default-build-dir
		      " && " command-line-default-directory hcv-configure
		      " " (mapconcat #'identity out-list " ")))))

(defun hcv-execute-configure (&rest ignore)
  (let ((cmd (hcv-get-shell-command hcv-config-list)))
    (gnus-make-directory hcv-default-build-dir)
    (async-shell-command cmd)))

(defun hcv-execute-copy-clipboard (&rest ignore)
  (let ((cmd (hcv-get-shell-command hcv-config-list)))
    (xclip-set-selection 'clipboard cmd)))
