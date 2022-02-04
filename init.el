;;;;;;;;; NOTES

;; Do the following steps to take advantage of emacs client/server mode on MS Windows.
;; Emacs initalization is done once when the server is started and so opening each new
;; client window becomes essentially instantaneous.
;; 1. Add emacs 'bin' subdirectory to the system path environment variable.
;;    Hit the 'Windows' key. Type 'Advanced System Settings' and hit <return>.
;;    Choose 'Environment Variables'. Go to 'System Variables', select 'Path', and
;;    click 'Edit...'. 
;; 2. Start emacs server on bootup by adding a shortcut pointing to runemacs.exe
;;    with the '--daemon' switch to the
;;    "C:\ProgramData\Microsoft\Windows\Start Menu\Programs\StartUp".
;;    subdirectory. This will start an emacs server on bootup.
;; 3. Add a shortcut pointing to emacsclientw.exe with the '-t' switch to the
;;    "C:/ProgramData/Microsoft/Windows/Start Menu/Programs/GNU Emacs xy.z/" folder.
;;    The Emacs installer probably already put a shortcut there. You'll just need to
;;    edit it to point to emacsclient.exe instead of runemacs.exe (and don't forget
;;    the '-t' switch.)
;; 4. Should also add an emacs client shortcut to Window's Explorer right click menu...

;;;;; UI customization

;;;; set some behaviors  ;;;;;;
(add-hook 'emacs-startup-hook
          (lambda ()
            (message "*** Emacs loaded in %s with %d garbage collections."
                     (format "%.2f seconds"
                             (float-time
                              (time-subtract after-init-time before-init-time)))
                     gcs-done)))

(setq inhibit-startup-message        t
      initial-scratch-message        ""
      insert-default-directory       nil     ; suppress default directory in minibuffer find-file prompts
      column-number-mode             nil
      size-indication-mode           nil
      auto-image-file-mode           t
      visible-bell                   t
      show-paren-mode                t
      search-highlight               t
      scroll-conservatively          200     ; minimize scrolling to keep point on screen
      sort-fold-case                 t       ; Do NOT sort uppercase before lower case
      case-fold-search               t
      line-number-display-limit      nil
      compilation-scroll-output      t	     ; force compilation window to scroll automatically
      fill-column                    5000
      suggest-key-bindings           5       ; always remind me about kbd shortcuts (5 seconds)
      gnuserv-frame                  (selected-frame) ; open files in existing frame
      next-line-add-newlines         nil     ; don't add newlines if cursor goes past last line
      highlight-nonselected-windows  nil
      eol-mnemonic-dos               "DOS"
      eol-mnemonic-unix              "Unix"
      eol-mnemonic-mac               "Mac"
      eol-mnemonic-undecided         "?"
      )

;; coding system stuff
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8)

;;;; pre-load some stuff ;;;;

;; set up the package manager (install & update packages)
(require 'package)		  ; package manager

(package-initialize)              ; add ~/.emacs.d/elpa/ to load-path
(add-to-list 'package-archives
             '("elpy" . "http://jorgenschaefer.github.io/packages/"))
(add-to-list 'package-archives
	     '("melpa" . "https://melpa.org/packages/"))
(add-to-list 'package-archives
             '("org" . "http://orgmode.org/elpa/"))
;; The package manager installs packages to directories under "~/.emacs.d/elpa/"
;; and adds the new package directory to load-path.

;; if package archive signature check fails, see
;; https://emacs.stackexchange.com/questions/233/how-to-proceed-on-package-el-signature-check-failure
;;
;; 1. set package-check-signature to nil, e.g. M-: (setq package-check-signature nil) RET
;; 2. download the package gnu-elpa-keyring-update and run the function with the same name, e.g. M-x package-install RET gnu-elpa-keyring-update RET.
;; 3. reset package-check-signature to the default value allow-unsigned, e.g. M-: (setq package-check-signature "allow-unsigned") RET
;; (setq package-check-signature nil)

;; ;; set up to load and configure packages
;; ;; use-package to simplify the config file
;;   (unless (package-installed-p 'use-package)
;;     (package-refresh-contents)
;;     (package-install 'use-package))
;;   (require 'use-package)
;;   (setq use-package-always-ensure 't)

;; (server-start)			  ; use emacsclientw.exe for file associations and 'sendto' menu.
;; set emacsclient to run 

(tool-bar-mode -1)  ;; no toolbar
(menu-bar-mode -1)  ;; no menubar
(set-fringe-mode 15);; extra fringe

;; control minibuffer completion behavior. See complete.el
(setq PC-meta-flag nil)

;;; window setup ;;;
;; frame title set to file name with path or to buffer name for buffers not associated with files
(setq frame-title-format
      (list 'buffer-file-name
	    (concat " %f")
	    (concat " %b")))
(setq-default icon-title-format "%b")	; set icon title to buffer name

;;;;; Set the size and position of new client windows (window geometry)

;; for initial values, position window as desired and then run these functions (c-x c-e)
;; (frame-position) ;; pixels
;; (frame-text-height) ;; in pixels
;; (frame-text-width)
;; (frame-height) ;; in characters
;; (frame-width)

;; Figure out how to set frame width/height as a proportion of screen size
;; (ffloor (* (display-pixel-width) .40))
;; (- (display-pixel-height) 65)
(setq default-frame-alist
      '((top . 0)
	(left . 101)
 	(width . (text-pixels . 1100))
	(height . (text-pixels . 1370))
 	(cursor-color . "blue")
 	(cursor-type . box)
 	(foreground-color . "black")
 	(background-color . "ivory")
	(font . "-outline-Consolas-normal-normal-normal-mono-*-*-*-*-c-*-iso8859-1")))

;; Consider adapting this to set background color of UNselected windows.
;; (defun highlight-selected-window ()
;;   "Highlight selected window with a different background color."
;;   (walk-windows (lambda (w)
;;                   (unless (eq w (selected-window))
;;                     (with-current-buffer (window-buffer w)
;;                       (buffer-face-set '(:background "#111"))))))
;;   (buffer-face-set 'default))
;; (add-hook 'buffer-list-update-hook 'highlight-selected-window)

;; mode line styling 
(set-face-attribute 'mode-line
                    nil
                    :foreground "black"
                    :background "lightgreen"
                    :box '(:line-width 1 :style released-button))
(set-face-attribute 'mode-line-inactive
                    nil
                    :foreground "black"
                    :background "darkgreen"
                    :box '(:line-width 1 :style released-button))
(require 'diminish)  ;; use diminish to keep the mode line clean

;; ; A long font name has the following form. Change the value of HEIGHT to change font size.
;; ; look in emacs info node 'font X' for details:
;; ; -MAKER-FAMILY-WEIGHT-SLANT-WIDTHTYPE-STYLE-PIXELS-HEIGHT-HORIZ-VERT-SPACING-WIDTH-CHARSET
;; ;; run this code to find list of available fonts
;; ;;(insert (prin1-to-string (x-list-fonts "*")))

;;;; because I'm impatient ;;;
(fset 'yes-or-no-p 'y-or-n-p)
(define-key query-replace-map [return] 'act)

;;;;;; Mode Specific Stuff ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;; stuff to consider
;; o ivy/counsel/swiper for better completion
;; o use-package macro for configuring packages
;; o see https://github.com/daviwil/emacs-from-scratch/blob/210e517353abf4ed669bc40d4c7daf0fabc10a5c/Emacs.org#debugging-with-dap-mode for ideas on org-mode configuration
;; o some interesting config stuff here: https://config.daviwil.com/emacs

;;;;; improved completion framework
(selectrum-mode +1)
;; need to tune up selectrum-minibuffer-map

;;; also consider
(selectrum-prescient-mode +1)
(prescient-persist-mode +1)

;;;;; improved help buffers (the jury is still out)
;; need to set up key bindings
(require 'helpful)		  

;;;;; auto revert non-file buffers only; doesn't seem to do what I want with ibuffers
(setq global-auto-revert-non-file-buffers t)
(global-auto-revert-mode 1)       

;;;;; highlight the current line
(require 'hl-line)		
(global-set-key (kbd "C-c l") 'hl-line-mode)
(copy-face 'highlight 'my-hl-line-face)
(set-face-background 'my-hl-line-face "cornsilk2")
(setq hl-line-face 'my-hl-line-face)

;;;;; completion for complex keystrokes
(require 'which-key)		  
(which-key-mode 1)
(diminish 'which-key-mode)

;;;;; use ace-window instead of other-window
(require 'ace-window)
(setq aw-keys '(?a ?s ?d ?f ?g ?h ?j ?k ?l)
      aw-minibuffer-flag t
;;      aw-scope "visible frames"
      )
(global-unset-key (kbd "C-x o"))
(global-set-key (kbd "M-o") 'ace-window)


;;;;; multi cursor functionality
(require 'multiple-cursors)
(global-set-key (kbd "C-c m m") 'mc/edit-lines)
(global-set-key (kbd "C-c m b") 'mc/edit-beginnings-of-lines)
(global-set-key (kbd "C-c m e") 'mc/edit-ends-of-lines)
(global-set-key (kbd "C-c m a") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c m p") 'mc/mark-previous-like-this)
(global-set-key (kbd "C-c m n") 'mc/mark-next-like-this)

;;;;; keep recent file list across sessions
;; hl-line mode doesn't work well with recentf dialogs
;; consider enabling stripes-mode for recentf dialogs
(require 'recentf)
(recentf-mode 1)
(run-at-time nil (* 5 60) 'recentf-save-list)
(setq recentf-max-saved-items 30)	  
(setq recentf-max-menu-items 30)	  
(global-set-key (kbd "C-c r") 'recentf-open-files)

;;;;; ispell with hunspell as backend ;;;;;
;; Hunspell executable must be installed separately
;; (https://sourceforge.net/projects/hunspell/).
;; Use 3 fingered tap on logitech kbd/pad to correct flagged words
;; or call meta-x ispell 
(add-to-list 'exec-path "c:/Program Files/GNU Emacs 26.2/hunspell/bin/")
(setq ispell-program-name
      (locate-file "hunspell"
		   exec-path exec-suffixes 'file-executable-p))

;; Typically access ispell by way of flyspell-mode
;; Flyspell Bindings:
;; M-$: correct words (using Ispell).
;; C-M-i: automatically correct word.
;; C-;: automatically correct the last misspelled word.
;; M-x flyspell-correct-word (or down-mouse-2): popup correct words.
(setq flyspell-issue-message-flag nil) ;; for efficiency

;;;;; company-mode
;; Provides context sensitive text expansion.
(setq company-selection-wrap-around t
      company-tooltip-align-annotations t
      company-idle-delay 0.1
      company-minimum-prefix-length 3
      company-tooltip-limit 10)
;; (global-company-mode) ;; use hooks to enable per major mode 

;;;;; yasnippet
;; Avoid using snippet keywords that might trigger expansion by company-mode or similar.
(require 'yasnippet)
(setq yas-snippet-dirs '("~/.emacs.d/snippets"))
(yas-global-mode t) ;; maybe try setting yasnippets to run per major mode?

;;;; org mode extensions
;;;; ox-pandoc
;; install pandoc: http://pandoc.org/installing.html
;; Windows installer gives no choice about where to locate pandoc: C:\Users\Dave\AppData\Local\Pandoc\pandoc.exe
(require 'ox-html5slide) ;; export to html5 slide
(require 'ox-ioslide)    ;; export to Google I/O html5 slide
(require 'ox-pandoc)     ;; org exporter for pandoc
(require 'ox-reveal)     ;; reveal.js slideshow exporter
(require 'ox-tufte)      ;; Tufte html exporter

;;;; arc-mode ;;; TODO: fix this up
;; (setq
;;  archive-zip-use-pkzip   nil
;;  archive-zip-extract     '("unzip" "-qq" "-p")
;;  archive-zip-expunge     '("zip" "-q" "-d")
;;  archive-zip-update      '("zip" "-q")
;;  archive-zip-update-case '("zip" "-q" "-k"))

;;;; c-mode ;;;
;(setq c-mode-hook nil)
(add-hook 'c-mode-hook
	  #'(lambda ()
	      (setq comment-column 35
		    truncate-lines t)
	      (message "c-mode-hook done")))

;;;; csv-mode
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(require 'csv-mode)

;;;;; use ibuffer instead of list-buffers
;; maybe take a look at packages 'bufler or 'eproject
(define-key (current-global-map) [remap list-buffers] 'ibuffer)
(setq ibuffer-expert t
      ibuffer-show-empty-filter-groups nil)
(add-hook 'ibuffer-hook
 	  #'(lambda ()
 	      (hl-line-mode 1)
 	      (message "ibuffer-hook done")))

;; (setq ibuffer-saved-filter-groups
;;       ;; Put project groups first.
;;       ;; Figure out a way that a buffer can belong to more than one group.
;;   (quote (("default"      
;;             ("TSS Project"
;; 	     (filename . "C:/DATA/braze/01 - work in progress/10 - Consulting/07-southport-school/"))
;; 	    ("Data Work" (or
;; 			  (mode . ess-mode)
;; 			  (filename . "(R run)")))
;;             ("Org" ;; all org-related buffers
;;               (mode . org-mode))
;;             ("ELisp" ;; all org-related buffers
;;               (mode . emacs-lisp-mode))
;; 	    ("Magit"
;; 	     (mode . magit-mode))
;; 	    ("Help" (or
;; 		     (name . "\*Help\*")
;; 		     (name . "\*Apropos\*")
;; 		     (name . "\*info\*")))
;; 	    ))))

  
(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-auto-mode 1)
	    (ibuffer-switch-to-saved-filter-groups "default"))
	  )

;;;; delim-col
;; useful for formatting data copied from spreadsheet and pasted into R scripts
(setq delimit-columns-str-before ""
      delimit-columns-str-after ""
      delimit-columns-str-separator ""
      delimit-columns-before ""
      delimit-columns-after " "
      delimit-columns-separator "\t"
      delimit-columns-format 'separator
      delimit-columns-extra t)

;;;; projectile ;;;;
(setq projectile-indexing-method        'hybrid   ; could also try 'alien for max speed
      projectile-sort-order             'recently-active ; sort by recency
      projectile-use-git-grep           t         ; git must be installed and on PATH
;;      projectile-completion-system      'selectrum
      projectile-switch-project-action  'projectile-dired)
(setq projectile-project-search-path '("C:/DATA/braze/01 - work in progress/10 - Consulting"
				       "C:/DATA/braze/02 - work/40 - coding-dev/R/development"))
(projectile-mode +1)
(define-key projectile-mode-map (kbd "C-p") 'projectile-command-map)

;; ;;;; install dired+ by way of el-get
;; ;; Mostly I use the emacs lisp package manager (elpy) for, well, managing packages.
;; ;; But not all packages are available that way. el-get may be helpful in those cases.
;; ;; right now, I'm installing it to ease installing dired+. I'm installing el-get using
;; ;; itself by way of elpy, pasting this code (found here:
;; ;; https://emacs.stackexchane.com/questions/38553/dired-missing-from-melpa).
;; (add-to-list 'load-path "~/.emacs.d/el-get/el-get")
;; (unless (require 'el-get nil 'noerror)
;;   (with-current-buffer
;;       (url-retrieve-synchronously
;;       "https://raw.githubusercontent.com/dimitri/el-get/master/el-get-install.el")
;;     (goto-char (point-max))
;;     (eval-print-last-sexp)))
;; (add-to-list 'el-get-recipe-path "~/.emacs.d/el-get-user/recipes")
;; (el-get 'sync)

;;
;; then,
;; 1. restart emacs
;; 2. M-x el-get-install dired+

;;;; dired ;;;
(setq diredp-hide-details-initially-flag nil
      dired-auto-revert-buffer t
      ls-lisp-use-insert-directory-program t ;; force use of external ls (not ls-lisp)
      dired-listing-switches "-alhoD --group-directories-first"
      )

(add-hook 'dired-load-hook ;; Run after loading dired package 
 	  #'(lambda ()
	      (setq dired-x-hands-off-my-keys t)
 	      (load 'dired-x)
	      (load 'dired+)
 	      (message "dired-load-hook done")))

(add-hook 'dired-mode-hook ;; Run at the end of 'dired-mode
	  #'(lambda ()
	      'dired-extra-startup
	      ;; Binding to open file in external program. Default is "W"; this is better.
	      (local-set-key (kbd "C-<return>") 'browse-url-of-dired-file) 
 	      (message "dired-mode-hook done")))

 ;; '(dired-mode-hook
 ;;   (quote
 ;;    ((lambda nil
 ;;       (message "dired-mode-hook done"))
 ;;     diredp-nb-marked-in-mode-name diredp--set-up-font-locking dired-extra-startup)))

(add-hook 'dired-after-readin-hook ;; Run each time a file or directory is read by dired
 	  #'(lambda ()
 	      (hl-line-mode 1)
 	      (message "dired-after-readin-hook done")))

;; Some under-used dired keys
;; w: copy filename to kill ring.
;; W: open file in external program
;; j: jump to file
;; y: file type and summary

;;;; emacs-lisp-mode ;;;
(add-hook 'emacs-lisp-mode-hook
	  #'(lambda ()
	      (electric-pair-local-mode)
	      (setq comment-column 60
		    fill-column 5000
		    truncate-lines nil)
	      (message "emacs-lisp-mode-hook done")))
(diminish 'eldoc-mode)

;;;;; enable linting
;; I can't get either flymake or flycheck to work.
;; The problem with both is in setting non-default linting rules.
;;(global-flycheck-mode)

;;;; ESS-mode ;;;
(setq ess-use-eldoc                  t
      ess-eldoc-show-on-symbol       nil
      ess-use-eldoc                 'script-only
      inferior-ess-own-frame         nil
      inferior-ess-same-window       nil
      ess-help-own-frame             'one  ; all ess help goes to same dedicated frame
      ess-ask-for-ess-directory      nil
      ess-r-versions                 '("R-1" "R-2" "R-3" "R-4" "R-devel" "R-patched")
      ess-bugs-batch-method          'dos
      ess-describe-at-point-method   nil
      ess-developer-packages         '("FDBeye" "FDB1" "FDButils")
      ess-directory-containing-R     "C:/Program Files/"
      inferior-ess-r-program         "c:/Program Files/R/R-4.1.0/bin/x64/rterm.exe"
      ess-eval-visibly               nil
      ess-use-company                t
      ess-use-flymake                nil ; problems with setting up non-default linter settings
      ess-funcmenu-use-p             t
      ess-history-file               nil
      ess-keep-dump-files            "always"
      ess-roxy-tags-param           '("author" "aliases" "concept" "description" "details" "examples" "format" "keywords" "method" "exportMethod" "name" "note" "param" "include" "references" "return" "seealso" "source" "docType" "title" "TODO" "usage" "import" "exportClass" "exportPattern" "S3method" "inheritParams" "importFrom" "importClassesFrom" "importMethodsFrom" "useDynLib" "rdname" "section" "slot")
      ess-swv-processor             'knitr
      ess-tab-always-indent         nil
      ess-user-full-name            "Dave Braze"
      ess-smart-S-assign-key        nil)

(add-hook 'ess-mode-hook
          #'(lambda ()
              (local-set-key (vector '(meta s)) 'nonincremental-repeat-search-forward)
	      (local-set-key (vector '(control =)) 'ess-cycle-assign)
;;              (local-set-key (vector '(control ?:)) 'comment-dwim)
	      (company-mode) ;; Is this needed if ess-use-company is set?
	      (yas-minor-mode-on)
              (font-lock-mode t)
	      (electric-pair-local-mode)
              (setq truncate-lines                 t
		    ess-nuke-trailing-whitespace-p nil ;; leaving trailing whitespace alone. Important for Rmarkdown files.
                    fill-column                    5000
                    comment-column                 40)
              (message "ess-mode-hook done")))

(add-hook 'inferior-ess-mode-hook
	  #'(lambda()
	  (electric-pair-local-mode)
	  (local-set-key (vector '(control =)) 'ess-cycle-assign)
	  (message "ess-inferior-mode-hook done")))

;;;;; setup poly mode for use with ESS.
(require 'polymode)
(require 'poly-markdown)
;; (require 'poly-R)
(add-to-list 'auto-mode-alist '("\\.Snw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rnw" . poly-noweb+r-mode))
(add-to-list 'auto-mode-alist '("\\.Rmd" . poly-markdown+r-mode))

;; See if it makes sense to arrange windows
;; (setq display-buffer-alist
;;       `(("*R Dired"
;;          (display-buffer-reuse-window display-buffer-in-side-window)
;;          (side . right)
;;          (slot . -1)
;;          (window-width . 0.33)
;;          (reusable-frames . nil))
;;         ("*R"
;;          (display-buffer-reuse-window display-buffer-at-bottom)
;;          (window-width . 0.35)
;;          (reusable-frames . nil))
;;         ("*Help"
;;          (display-buffer-reuse-window display-buffer-in-side-window)
;;          (side . right)
;;          (slot . 1)
;;          (window-width . 0.33)
;;          (reusable-frames . nil))))


;; If using the Goulet Emacs for Windows distro, be sure to edit 
;; site-start.el and comment out the line "(require 'ess-site)". 
;; The file is located somewhere like c:/Program Files/GNU Emacs 26.2/share/emacs/site-lisp
(require 'ess-site)

;;;;; font-lock ;;;
(add-hook 'font-lock-mode-hook
	  #'(lambda ()
 	      (setq font-lock-maximum-decoration t)        ; Maximum colors
 	      (set-face-foreground font-lock-comment-face "darkcyan")
 	      (set-face-foreground font-lock-constant-face "darkred")))

(global-font-lock-mode t)	      ; Turn on font-lock in all modes that support it

;; ;; ;;;; magit-mode ;;;
;; (setenv "GIT_ASKPASS" "git-gui--askpass")
;; ;; (setq shell-file-name explicit-shell-file-name)
;; (add-to-list 'exec-path "D:/winbin/Git/bin") ;; why don't I just put git on the PATH?

;;;; markdown-mode ;;;
(require 'markdown-mode)
(add-hook 'markdown-mode-hook
	  #'(lambda ()
;;	      (yas-minor-mode-on)
	      ))

;;;; msb ;;;; mouse buffer menu minor mode
;; (msb-mode)

;;;;  text-mode ;;;
(add-hook 'text-mode-hook
	  #'(lambda ()
	      (visual-line-mode) ;; vlm disables indication of line wrapping in the fringes
	      (diminish 'visual-line-mode)
	      (setq truncate-lines nil
		    fill-column 5000)
	      (local-set-key (vector '(meta s)) 'nonincremental-repeat-search-forward)
	      (message "text-mode-hook done")))

;; ;;;; TRAMP for remote editing ;;;
;; ;; tramp won't work under windows until
;; ;; 1. tramp-sh.el is re-byte-compiled
;; ;; 2. M-x tramp-cleanup-all-connections <ret>
;; ;;
;; ;; To use do method:host:file
;; ;;
;; (require 'tramp)
;; (setq tramp-default-method "plink"
;;       tramp-default-user "braze"
;;       tramp-default-host "camille.haskins.yale.edu"
;;       )

;; ;;;; web-mode ;;;;
; (require 'web-mode)
(add-to-list 'auto-mode-alist '("\\.html\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.jsp\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
(add-to-list 'auto-mode-alist '("\\.markdown\\'" . markdown-mode))
(add-to-list 'auto-mode-alist '("\\.md\\'" . markdown-mode))

;;;;; random stuff

(global-set-key (kbd "C-c t") 'insert-time)
(defun insert-time ()
  "Insert the current time in 24 hour format."
  (interactive)
  (insert (format-time-string "%H:%M")))

(global-set-key (kbd "C-c d") 'insert-date)
(defun insert-date ()
  "Insert the current date in long format."
  (interactive)
  (insert (format-time-string "%B %d, %Y")))

;; need to tune this to control *where* new frame appears.
(global-set-key (kbd "C-b") 'push-buffer-to-new-frame)
(defun push-buffer-to-new-frame ()
  "Push buffer in the current window to its own frame and revert current window to previous frame."
  (interactive)
  (let ((current (current-buffer))
        (selected (selected-window)))
    (if (display-buffer-other-frame current)
        (quit-restore-window selected))))

;; ;;;;;; Global Key Bindings ;;;;;;

;;; Keys ;;;
;; CTL Keys ;;
(global-set-key (vector '(control I)) 'isearch-forward-regexp)
(global-set-key (vector '(control s)) 'nonincremental-re-search-forward)
(global-set-key (vector '(control r)) 'redraw-display)
(global-set-key (vector '(control n)) 'rename-buffer)
(global-set-key (vector '(control f)) 'forward-word)
(global-set-key (vector '(control z)) 'undo)
(global-set-key (kbd "C-l") 'goto-line)
(global-set-key (kbd "C->") 'text-scale-increase)
(global-set-key (kbd "C-<") 'text-scale-decrease)

;; Meta Keys ;;
(global-set-key (vector '(meta s)) 'nonincremental-repeat-search-forward)
(global-set-key (vector '(meta c)) 'center-line)
(global-set-key (vector '(meta f)) 'fill-paragraph)
(global-set-key (vector '(meta up)) 'enlarge-window)
(global-set-key (vector '(meta down)) 'shrink-window)
(global-set-key (vector '(meta right)) 'enlarge-window-horizontally)
(global-set-key (vector '(meta left)) 'shrink-window-horizontally)

;; ctl-x prefix ;;
(global-set-key (vector '(control x) ?l) 'recenter)	; current line to screen center
(global-set-key (vector '(control x) '(control m) ?u) 'set-buffer-eol-conversion-unix)
(global-set-key (vector '(control x) '(control m) ?d) 'set-buffer-eol-conversion-dos)
(global-set-key (vector '(control x) '(control m) ?m) 'set-buffer-eol-conversion-mac)

;; ctl-c prefix ;;
; should fix all the case changing stuff to ensure that we are at the beginning of a word.
(global-set-key (kbd "C-c d") 'downcase-word)  
(global-set-key (kbd "C-c u") 'upcase-word)
(global-set-key (kbd "C-c c") 'capitalize-word)

 ;; other keys ;;
(global-set-key (vector 'home) 'beginning-of-buffer)
(global-set-key (vector 'end) 'end-of-buffer)
(global-set-key (vector 'f11) 'compile)
(global-set-key (vector 'f12) 'recompile)

;;; Custom Section ;;;
 
(put 'narrow-to-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(elpy-modules
   (quote
    (elpy-module-company elpy-module-eldoc elpy-module-pyvenv elpy-module-highlight-indentation elpy-module-yasnippet elpy-module-sane-defaults)))
 '(face-font-family-alternatives
   (quote
    (("Monospace" "DejaVu Sans Mono" "courier" "fixed")
     ("Monospace Serif" "Consolas" "Courier 10 Pitch" "Courier Std" "FreeMono" "Nimbus Mono L" "courier" "fixed")
     ("courier" "Lucida Sans Typewriter" "fixed")
     ("Sans Serif" "helv" "helvetica" "arial" "fixed")
     ("helv" "helvetica" "arial" "fixed"))))
 '(package-check-signature (quote allow-unsigned))
 '(package-selected-packages
   (quote
    (selectrum selectrum-prescient diminish stripes helpful which-key multiple-cursors auto-complete ace-window git-modes gnu-elpa-keyring-update zones company git-commit helm-core ht hydra lv transient with-editor el-get w32-browser poly-R poly-ansible poly-erb poly-markdown poly-noweb poly-org poly-rst poly-ruby poly-slim poly-wdl polymode highlight-chars dired+ dired-quick-sort flx-ido ox-reveal ox-html5slide ox-ioslide ox-pandoc ox-tufte projectile magit lorem-ipsum helm elpy ego csv-mode)))
 '(save-place t nil (saveplace))
 '(sql-mysql-program "C:/Program Files/MySQL/MySQL Server 5.5/bin/mysql")
 '(sql-password "")
 '(sql-product (quote mysql))
 '(sql-user ""))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(dired-directory ((t (:inherit font-lock-function-name-face))))
 '(dired-ignored ((t nil)))
 '(dired-perm-write ((t nil)))
 '(diredp-compressed-file-suffix ((t (:foreground "darkgreen"))))
 '(diredp-deletion ((t nil)))
 '(diredp-dir-heading ((t (:foreground "Blue"))))
 '(diredp-dir-name ((t (:foreground "DarkRed"))))
 '(diredp-dir-priv ((t (:foreground "DarkRed"))))
 '(diredp-exec-priv ((t nil)))
 '(diredp-flag-mark ((t nil)))
 '(diredp-flag-mark-line ((t nil)))
 '(diredp-no-priv ((t nil)))
 '(diredp-omit-file-name ((t (:inherit diredp-ignored-file-name))))
 '(diredp-read-priv ((t nil)))
 '(diredp-write-priv ((t nil)))
 '(table-cell ((t (:background "SlateGray1" :foreground "black" :inverse-video nil)))))
(put 'dired-find-alternate-file 'disabled nil)

