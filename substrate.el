;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;       .⌒.	  ▗▄▄▖▗  ▖ ▗▖  ▗▄  ▄▄      ▄▄ ▗  ▖▗▄▄  ▄▄ ▄▄▄▖▗▄▄  ▗▖ ▄▄▄▖▗▄▄▖ ;;;
;;;     .#   #.	  ▐   ▐▌▐▌ ▐▌ ▗▘ ▘▐▘ ▘    ▐▘ ▘▐  ▌▐  ▌▐▘ ▘ ▐  ▐ ▝▌ ▐▌  ▐  ▐    ;;;
;;;    /       \  ▐▄▄▖▐▐▌▌ ▌▐ ▐   ▝▙▄     ▝▙▄ ▐  ▌▐▄▄▘▝▙▄  ▐  ▐▄▄▘ ▌▐  ▐  ▐▄▄▖ ;;;
;;;   (,,,___,,,) ▐   ▐▝▘▌ ▙▟ ▐     ▝▌      ▝▌▐  ▌▐  ▌  ▝▌ ▐  ▐ ▝▖ ▙▟  ▐  ▐    ;;;
;;;       ) (	  ▐▄▄▖▐  ▌▐  ▌ ▚▄▘▝▄▟▘    ▝▄▟▘▝▄▄▘▐▄▄▘▝▄▟▘ ▐  ▐  ▘▐  ▌ ▐  ▐▄▄▖ ;;;
;;;      (___)                                                                 ;;;
;;;                                                                            ;;;
;;; init.el ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; Guardrail
(when (< emacs-major-version 29)
  (error "Emacs Substrate only works with Emacs 29 and newer; you have version %s" emacs-major-version))

;;; Custom variables

(defgroup substrate nil "Custom options for substrate")
(defcustom substrate-initialise-packages t
  "Initialise the substrate package system (straight.el)"
  :type 'boolean :group 'substrate)
(defcustom substrate-enable-windmove t
  "Enable windmove to hop around windows with ctrl+arrow"
  :type 'boolean :group 'substrate)
(defcustom substrate-display-startup-help t
  "Show a help window on startup"
  :type 'boolean :group 'substrate)
(defcustom substrate-enable-which-key t
  "Use which-key to list available key combos"
  :type 'boolean :group 'substrate)
(defcustom substrate-display-line-numbers t
  "Display line numbers on buffers"
  :type 'boolean :group 'substrate)
(defcustom substrate-enable-cua-mode t
  "Enable CUA mode or not"
  :type 'boolean :group 'substrate)
(defcustom substrate-configure-theme t
  "Configure the default theme (evangelion) as part of the substrate init"
  :type 'boolean :group 'substrate)


(defun substrate-init ()
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
  ;;;
  ;;;   Basic settings
  ;;;
  ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Package initialization
  ;;
  (when substrate-initialise-packages
    ;; Set up package and enable melpa
    (require 'package)
    (add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
    (package-initialise)

    ;; Boostrap straight.el
    (defvar bootstrap-version)
    (let ((bootstrap-file
	   (expand-file-name
            "straight/repos/straight.el/bootstrap.el"
            (or (bound-and-true-p straight-base-dir)
		user-emacs-directory)))
	  (bootstrap-version 7))
      (unless (file-exists-p bootstrap-file)
	(with-current-buffer
            (url-retrieve-synchronously
             "https://raw.githubusercontent.com/radian-software/straight.el/develop/install.el"
             'silent 'inhibit-cookies)
	  (goto-char (point-max))
	  (eval-print-last-sexp)))
      (load bootstrap-file nil 'nomessage))
      (setq straight-use-package-by-default t))

  (setopt initial-major-mode 'fundamental-mode)  ; default mode for the *scratch* buffer
  (setopt display-time-default-load-average nil) ; this information is useless for most

  ;; Automatically reread from disk if the underlying file changes
  (setopt auto-revert-avoid-polling t)
  ;; Some systems don't do file notifications well; see
  ;; https://todo.sr.ht/~ashton314/emacs-bedrock/11
  (setopt auto-revert-interval 5)
  (setopt auto-revert-check-vc-info t)
  (global-auto-revert-mode)

  ;; Save history of minibuffer
  (savehist-mode)

  ;; Move through windows with Ctrl-<arrow keys>
  (when substrate-enable-windmove
    (windmove-default-keybindings 'control)) ; You can use other modifiers here

  ;; Fix archaic defaults
  (setopt sentence-end-double-space nil)

  ;; Make right-click do something sensible
  (when (display-graphic-p)
    (context-menu-mode))

  ;; Don't litter file system with *~ backup files; put them all inside
  ;; ~/.emacs.d/backup or wherever
  (defun substrate--backup-file-name (fpath)
    "Return a new file path of a given file path.
If the new path's directories does not exist, create them."
    (let* ((backupRootDir (concat user-emacs-directory "emacs-backup/"))
           (filePath (replace-regexp-in-string "[A-Za-z]:" "" fpath )) ; remove Windows driver letter in path
           (backupFilePath (replace-regexp-in-string "//" "/" (concat backupRootDir filePath "~") )))
      (make-directory (file-name-directory backupFilePath) (file-name-directory backupFilePath))
      backupFilePath))
  (setopt make-backup-file-name-function 'substrate--backup-file-name)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Discovery aids
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Show the help buffer after startup
  (when substrate-display-startup-help
    (add-hook 'after-init-hook 'help-quick))

  ;; which-key: shows a popup of available keybindings when typing a long key
  ;; sequence (e.g. C-x ...)
  (use-package which-key
    :ensure t
    :if substrate-enable-which-key
    :config
    (which-key-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Minibuffer/completion settings
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; For help, see: https://www.masteringemacs.org/article/understanding-minibuffer-completion

  (setopt enable-recursive-minibuffers t)                ; Use the minibuffer whilst in the minibuffer
  (setopt completion-cycle-threshold 1)                  ; TAB cycles candidates
  (setopt completions-detailed t)                        ; Show annotations
  (setopt tab-always-indent 'complete)                   ; When I hit TAB, try to complete, otherwise, indent
  (setopt completion-styles '(basic initials substring)) ; Different styles to match input to candidates

  (setopt completion-auto-help 'always)                  ; Open completion always; `lazy' another option
  (setopt completions-max-height 20)                     ; This is arbitrary
  (setopt completions-detailed t)
  (setopt completions-format 'one-column)
  (setopt completions-group t)
  (setopt completion-auto-select 'second-tab)            ; Much more eager
					;(setopt completion-auto-select t)                     ; See `C-h v completion-auto-select' for more possible values

  (keymap-set minibuffer-mode-map "TAB" 'minibuffer-complete) ; TAB acts more like how it does in the shell

  ;; For a fancier built-in completion option, try ido-mode,
  ;; icomplete-vertical, or fido-mode. See also the file extras/base.el

					;(icomplete-vertical-mode)
					;(fido-vertical-mode)
					;(setopt icomplete-delay-completions-threshold 4000)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Interface enhancements/defaults
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Mode line information
  (setopt line-number-mode t)                        ; Show current line in modeline
  (setopt column-number-mode t)                      ; Show column as well

  (setopt x-underline-at-descent-line nil)           ; Prettier underlines
  (setopt switch-to-buffer-obey-display-actions t)   ; Make switching buffers more consistent

  (setopt show-trailing-whitespace nil)      ; By default, don't underline trailing spaces
  (setopt indicate-buffer-boundaries 'left)  ; Show buffer top and bottom in the margin

  ;; Enable horizontal scrolling
  (setopt mouse-wheel-tilt-scroll t)
  (setopt mouse-wheel-flip-direction t)

  ;; We won't set these, but they're good to know about
  ;;
  ;; (setopt indent-tabs-mode nil)
  ;; (setopt tab-width 4)

  ;; Misc. UI tweaks
  (blink-cursor-mode -1)                                ; Steady cursor
  (pixel-scroll-precision-mode)                         ; Smooth scrolling

  ;; Use common keystrokes by default
  (when substrate-enable-cua-mode
    (cua-mode))

  ;; Display line numbers in programming mode
  (when substrate-display-line-numbers
    (add-hook 'prog-mode-hook 'display-line-numbers-mode)
    (setopt display-line-numbers-width 3))           ; Set a minimum width

  ;; Nice line wrapping when working with text
  (add-hook 'text-mode-hook 'visual-line-mode)

  ;; Modes to highlight the current line with
  (let ((hl-line-hooks '(text-mode-hook prog-mode-hook)))
    (mapc (lambda (hook) (add-hook hook 'hl-line-mode)) hl-line-hooks))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Tab-bar configuration
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  ;; Show the tab-bar as soon as tab-bar functions are invoked
  (setopt tab-bar-show 1)

  ;; Add the time to the tab-bar, if visible
  (add-to-list 'tab-bar-format 'tab-bar-format-align-right 'append)
  (add-to-list 'tab-bar-format 'tab-bar-format-global 'append)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;
;;;   Theme
;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

  (use-package evangelion-theme
    :if substrate-configure-theme
    :ensure t
    :config (load-theme 'evangelion t))

;;; Relegate automatic custom variables to their own file.
  (setq custom-file (expand-file-name "custom-vars.el"))

) ;; End substrate-init

(provide 'substrate)
;;; End of substrate.el
