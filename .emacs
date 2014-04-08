; fixes emacs slow startup
(modify-frame-parameters nil '((wait-for-wm . nil)))

; 日本語をデフォルトにする。
;(set-language-environment "Japanese")
; anthy.el をロードできるようにする (必要に応じて)。
;(push "/usr/share/emacs/site-lisp/anthy/" load-path)
; anthy.el をロードする。
;(load-library "anthy")
; japanese-anthy をデフォルトの input-method にする。
;(setq default-input-method "japanese-anthy")

(defun lw:byte-compile-directory(directory)
  "Byte compile every .el file into a .elc file in the given directory. See
   `byte-recompile-directory'."
  (interactive
   (list
    (read-file-name "Lisp directory: ")))
  (byte-recompile-directory directory 0 t))

; set the default width and height
(set-frame-height (selected-frame) 82)
(set-frame-width (selected-frame) 115)
(setq default-fill-column 100)

(defun wc ()
  (interactive)
  (message "Word count: %s" (how-many "\\w+" (point-min) (point-max))))

(setq tex-mode-hook
   '(lambda ()
      (auto-fill-mode 1)
      ))

(setq latex-mode-hook
   '(lambda ()
      (auto-fill-mode 1)
      ))

;; Open files and goto lines like we see from g++ etc. i.e. file:line#
;; (to-do "make `find-file-line-number' work for emacsclient as well")
;; (to-do "make `find-file-line-number' check if the file exists")
(defadvice find-file (around find-file-line-number
                             (filename &optional wildcards)
                             activate)
  "Turn files like file.cpp:14 into file.cpp and going to the 14-th line."
  (save-match-data
    (let* ((matched (string-match "^\\(.*\\):\\([0-9]+\\):?$" filename))
           (line-number (and matched
                             (match-string 2 filename)
                             (string-to-number (match-string 2 filename))))
           (filename (if matched (match-string 1 filename) filename)))
      ad-do-it
      (when line-number
        ;; goto-line is for interactive use
        (goto-char (point-min))
        (forward-line (1- line-number))))))

; add a load path
(setq load-path  (cons (expand-file-name "~/.emacs-lisp/") load-path))

;; default to better frame titles
;; Set the frame's title. %b is the name of the buffer. %+ indicates
;; the state of the buffer: * if modified, % if read only, or -
;; otherwise. Two of them to emulate the mode line. %f for the file
;; name. Incredibly useful!
;; (setq frame-title-format "Emacs: %b %+%+ %f")
;; (setq frame-title-format '("Emacs @ " system-name ": %b %+%+ %f"))
;(setq frame-title-format (concat invocation-name "@" system-name ": %b %+%+ %f"))
(setq frame-title-format (concat  "%b - emacs@" system-name))

;; retitle emacs window
(defun frame-retitle (title)
  (modify-frame-parameters
   nil
   (list
    (cons
     'name
     title
     )
    )
   )
  )

;; ;; set a beautiful title bar
;; (setq frame-title-format
;;       '("%S: " (buffer-file-name "%f"
;;                                  (dired-directory dired-directory "%b"))))

(delete-selection-mode)

; always show columns
(column-number-mode 1)

; syntax highlighting
;(global-font-lock-mode)
; show closing parenthesis
(show-paren-mode 1)
;(setq font-lock-maximum-decoration t )

;; .emacs file font-lock stuff. I got this from someone else, and
;; would like to be able to give credit. Would the author please step
;; forward?
(defun my-recenter (&optional arg)
 "Centre point in window and run font-lock-fontify-block"
  (interactive "P")
  (recenter arg)
  (font-lock-fontify-block (window-height)))

;; (require 'font-lock)
;; (setq font-lock-face-attributes
;;       '((font-lock-comment-face		 "Firebrick")
;; 	(font-lock-string-face	    "SpringGreen4")
;; 	(font-lock-keyword-face		  "RoyalBlue")
;; 	(font-lock-function-name-face	  "Blue")
;; 	(font-lock-variable-name-face	  "GoldenRod")
;; 	(font-lock-type-face	    "DarkGoldenRod")
;; 	(font-lock-reference-face   "Purple")
;; 	))

;; (if (<= sams-Gnu-Emacs-p 19)		; 20.2 doesn't like
;;     (font-lock-make-faces))		; this.

(add-hook 'font-lock-mode-hook
	  '(lambda ()
	     (substitute-key-definition
	      'recenter 'my-recenter (current-global-map))))

(if (fboundp 'global-font-lock-mode)
    (global-font-lock-mode t))
;(setq font-lock-support-mode 'lazy-lock-mode)
(setq font-lock-maximum-decoration t)

;(setq lazy-lock-defer-after-change t)

(setq-default c-basic-offset 4)
(setq-default tab-width 4)

(defun my-c++-mode-hook ()
  (setq tab-width 4)
  (setq c-indent-level 4)
)
(defun my-c-mode-hook ()
  (setq tab-width 4)
  (setq c-indent-level 4)
)

(defun my-js-mode-hook ()
  (setq tab-width 4
        indent-tabs-mode nil
        c-basic-offset 4)
)

; M-x byte-compile-file RE js2.el RET
(autoload 'js2-mode "js2" nil t)
(add-to-list 'auto-mode-alist '("\\.js$" . js2-mode))

; set tab width to 4 spaces


(setq-default indent-tabs-mode nil)
(setq-default undo-limit 40000)
(setq-default undo-strong-limit 60000)


; disable splash/startup screen
(setq inhibit-splash-screen t)

;; inhibit startup message
(setq inhibit-startup-message t)

;; type "y"/"n" instead of "yes"/"no"
;(fset 'yes-or-no-p 'y-or-n-p)

; backup files go into .~ directory now
(add-to-list 'backup-directory-alist (cons "." ".~"))

; preserve case when expanding
(setq dabbrev-case-replace nil)

; emacs clipboard copies to global keyboard
(setq x-select-enable-clipboard t)

;; reread buffer from file
(global-set-key "\C-c\C-r" 'revert-buffer)
(global-set-key (read-kbd-macro "M-C-x p") 'previous-multiframe-window)

;; ctrl+v for pasting
;(global-set-key "\C-v" 'yank)

;; ctrl+i for searching
;(global-set-key (read-kbd-macro "C-i") 'isearch-forward)

; open friend file
(global-set-key (read-kbd-macro "C-;") 'ff-find-other-file)

; smoother scrolling
(setq scroll-step 1)
(setq scroll-conservatively 50)
(setq scroll-preserve-screen-position nil)

(add-hook 'shell-mode-hook
          '(lambda ()
             (local-set-key [home]        ; move to beginning of line, after prompt
                            'comint-bol)
             (local-set-key [up]          ; cycle backward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-previous-input 1)
                                 (previous-line 1))))
             (local-set-key [down]        ; cycle forward through command history
                            '(lambda () (interactive)
                               (if (comint-after-pmark-p)
                                   (comint-next-input 1)
                                 (forward-line 1))))
             ))

; shortcuts
(global-set-key (kbd "<M-backspace>") 'backward-kill-word)

(defvar LIMIT 1)
(defvar time 0)
(defvar mylist nil)

(defun scroll-down-keep-cursor ()
   ;; Scroll the text one line down while keeping the cursor
  (interactive)
  (scroll-down 1))

(defun scroll-up-keep-cursor ()
   ;; Scroll the text one line up while keeping the cursor
  (interactive)
  (scroll-up 1))

(global-set-key (kbd "<C-down>") 'scroll-up-keep-cursor);
(global-set-key (kbd "<C-up>") 'scroll-down-keep-cursor)
(global-set-key (kbd "C-M-g") 'goto-line)

;; M-n and M-p
(global-unset-key "\M-p")
(global-unset-key "\M-n")

(defun scroll-up-in-place (n)
  (interactive "p")
  (previous-line n)
  (scroll-down n))


(defun scroll-down-in-place (n)
  (interactive "p")
  (next-line n)
  (scroll-up n))

(global-set-key "\M-n" 'scroll-down-in-place)
(global-set-key "\M-p" 'scroll-up-in-place)


(defun time-now ()
  (car (cdr (current-time))))

(defun bubble-buffer ()
   (interactive)
   (if (or (> (- (time-now) time) LIMIT) (null mylist))
       (progn (setq mylist (copy-alist (buffer-list)))
          (delq (get-buffer " *Minibuf-0*") mylist)
          (delq (get-buffer " *Minibuf-1*") mylist)))
   (bury-buffer (car mylist))
   (setq mylist (cdr mylist))
   (setq newtop (car mylist))
   (switch-to-buffer (car mylist))
   (setq rest (cdr (copy-alist mylist)))
   (while rest
     (bury-buffer (car rest))
     (setq rest (cdr rest)))
   (setq time (time-now)))

(defun switch-to-last-buffer ()
   (interactive)
   (switch-to-buffer nil)
)

(defun start-gdb ()
  (interactive)
  (other-window 1)
  ;(shell-command "make")
  (execute-extended-command "gdb")
)

(remove-hook 'python-mode-hook 'wisent-python-default-setup)

;(setq python-shell-internal-send-string (lambda (string) nil))
; in cedet semantic-python-get-system-include-path forces starting of the python interpreter in inferior mode. If python-shell-internal-send-string is defined before it, then we can skip loading python automatically (this might break other functionality?)
(defun python-shell-internal-send-string (string) "")

; CEDET
;(load-file "~/.emacs-lisp/cedet-1.1/common/cedet.el")
(load-file "~/.emacs-lisp/cedet_trunk_20140220/cedet-devel-load.el")

;;(semantic-load-enable-semantic-debugging-helpers)      ; Enable prototype help and smart completion
(semantic-load-enable-code-helpers)      ; Enable prototype help and smart completion
(setq global-semantic-stickyfunc-mode nil)
(global-srecode-minor-mode 1)            ; Enable template insertion menu
(require 'semantic/ia)                   ; additional features for names comletion and displaying information of tags
(require 'semantic/bovine/gcc)

(require 'semantic/db)
(global-semanticdb-minor-mode 1)

; remove python hook from CEDET
(remove-hook 'python-mode-hook 'wisent-python-default-setup)

(defun my-cedet-hook ()
  (local-set-key [(control return)] 'semantic-ia-complete-symbol)
  (local-set-key "\C-c?" 'semantic-ia-complete-symbol-menu)
  (local-set-key "\C-c>" 'semantic-complete-analyze-inline)
;;  (local-set-key (kbd "C-M-/") 'semantic-complete-analyze-inline)
  (local-set-key "\C-c=" 'semantic-decoration-include-visit)
  (local-set-key "\C-cj" 'semantic-ia-fast-jump)
  (local-set-key "\C-cq" 'semantic-ia-show-doc)
  (local-set-key "\C-cs" 'semantic-ia-show-summary)
  (local-set-key "\C-cp" 'semantic-analyze-proto-impl-toggle)
;;  (local-set-key "\C-cp" 'semantic-symref)
  (local-set-key "\C-c+" 'semantic-tag-folding-show-block)
  (local-set-key "\C-c-" 'semantic-tag-folding-fold-block)
  (local-set-key "\C-c\C-c+" 'semantic-tag-folding-show-all)
  (local-set-key "\C-c\C-c-" 'semantic-tag-folding-fold-all)
  )
(add-hook 'c-mode-common-hook 'my-cedet-hook)
(add-hook 'c++-mode-common-hook 'my-cedet-hook)

;; gnu global support
(require 'semantic/db-global)
(semanticdb-enable-gnu-global-databases 'c-mode)
(semanticdb-enable-gnu-global-databases 'c++-mode)

;;; ede customization
;;(require 'semantic-lex-spp)
;;(global-ede-mode 'nil)                      ; Enable the Project management system
;;disable header tag
;;(setq global-semantic-stickyfunc-mode nil)

(defun openrave-package-path ()
  (if (executable-find "openrave-config")
      (save-excursion
        (with-temp-buffer
          (call-process "openrave-config" nil t nil "--cflags-only-I")
          (goto-char (point-min))
          (re-search-forward "^-I\\(.*\\)[ \\|$]")
          (match-string 1)))
    ""
    ))

(setq openrave-base-dir (openrave-package-path))
(message "openrave dir: %s" (openrave-package-path))

; Use of this setting will lead to execution of semantic-complete-self-insert
; command when user will press . or > after variables, that are class or
; structure instances, and displaying of list of possible completions for given class or structure.
(defun my-c-mode-cedet-hook ()
;  (local-set-key "." 'semantic-complete-self-insert)
;  (local-set-key ">" 'semantic-complete-self-insert)
)
(add-hook 'c-mode-common-hook 'my-c-mode-cedet-hook)
(add-hook 'c++-mode-common-hook 'my-c-mode-cedet-hook)

(defun my-semantic-hook ()
  (imenu-add-to-menubar "TAGS")
)

(add-hook 'semantic-init-hooks 'my-semantic-hook)

;; (if (file-exists-p "/home/rdiankov/mujin/dev/ros/mujin/openrave_mujin/openrave_git/include/openrave/openrave.h")
;;     (progn    
;;       (semantic-add-system-include "/home/rdiankov/mujin/dev/ros/mujin/openrave_mujin/openrave_git/include" 'c++-mode)
;;       (semantic-add-system-include "/home/rdiankov/mujin/dev/ros/mujin/openrave_mujin/openrave_git/include" 'c-mode)
;;       )
;;   nil)

(if (file-exists-p (concat openrave-base-dir "/openrave/config.h"))
;  (add-to-list 'semantic-lex-c-preprocessor-symbol-file (concat openrave-base-dir "/openrave/config.h"))
  (semantic-add-system-include (concat openrave-base-dir "/openrave"))
)


;; don't enable this since it forces C++ mode on many things
;;(add-to-list 'auto-mode-alist (cons openrave-base-dir 'c++-mode))



;(add-to-list 'semantic-lex-c-preprocessor-symbol-map '("OPENRAVE_API" . ""))
;(semantic-c-add-preprocessor-symbol "OPENRAVE_API" 'nil)

; Emacs Code Browser (ECB)
;; (add-to-list 'load-path "~/.emacs-lisp/ecb-2.40/")
;; (require 'ecb) ; start ecb on startup
;; ;(require 'ecb-autoloads)
;;
;; (ecb-layout-define "rosen-layout" left nil
;;      ;; The frame is already splitted side-by-side and point stays in the
;;      ;; left window (= the ECB-tree-window-column)
;;      ;; 1. Defining the current window/buffer as ECB-methods buffer
;;      (ecb-set-methods-buffer)
;;      ;; 2. Splitting the ECB-tree-windows-column in two windows
;;      ;(ecb-split-ver 0.4 t)
;;      ;; 3. Go to the second window
;;      ;(other-window 1)
;;      ;; 4. Defining the current window/buffer as ECB-history buffer
;;      ;(ecb-set-analyse-buffer)
;;      ;; 5. Make the ECB-edit-window current (see Postcondition above)
;;      (select-window (next-window)))
;;
;; (setq ecb-layout-name "rosen-layout") ; only have methods and history
;; ;; don't show tip of the day at start time of ECB
;; (setq ecb-tip-of-the-day nil)
;; (setq ecb-source-path (quote ("/usr/include")))

;; tabs
(require 'tabbar)
(require 'cl) ; for remove-if

(set-face-attribute
 'tabbar-default-face nil
 :background "gray60")
(set-face-attribute
 'tabbar-unselected-face nil
 :background "gray85"
 :foreground "gray30"
 :box nil)
(set-face-attribute
 'tabbar-selected-face nil
 :background "#f2f2f6"
 :foreground "black"
 :box nil)
(set-face-attribute
 'tabbar-button-face nil
 :box '(:line-width 1 :color "gray72" :style released-button))
(set-face-attribute
 'tabbar-separator-face nil
 :height 0.7)

;; kill *scratch* buffer
;;(kill-buffer (current-buffer))

;; display all buffers, regardless of group
(setq tabbar-buffer-groups-function
      (lambda (buffer)
        (list "All buffers")))

(setq tabbar-buffer-list-function
      (lambda ()
    	  (remove-if
    	   (lambda(buffer)
    	     (find (aref (buffer-name buffer) 0) " *"))
    	   (buffer-list))
          )
      )

;; The following will provide windows style (shift-)control tab behaviour and autoload tabbar-mode. Use a prefix to change groups and no prefix to change the buffer.

(dolist (func '(tabbar-mode tabbar-forward-tab tabbar-forward-group tabbar-backward-tab tabbar-backward-group))
  (autoload func "tabbar" "Tabs at the top of buffers and easy control-tab navigation"))

(defmacro defun-prefix-alt (name on-no-prefix on-prefix &optional do-always)
  `(defun ,name (arg)
     (interactive "P")
     ,do-always
     (if (equal nil arg)
         ,on-no-prefix
       ,on-prefix)))

(defun-prefix-alt shk-tabbar-next (tabbar-forward-tab) (tabbar-forward-group) (tabbar-mode 1))
(defun-prefix-alt shk-tabbar-prev (tabbar-backward-tab) (tabbar-backward-group) (tabbar-mode 1))

;(global-set-key [\C-\S-tab] 'shk-tabbar-prev)

(tabbar-mode 1)

; set keys
(global-set-key (kbd "M-k") 'shk-tabbar-next)
(global-set-key (kbd "M-j") 'shk-tabbar-prev)

;(global-set-key (kbd "C-<tab>") 'tabbar-forward)
;(global-set-key (read-kbd-macro "C-S-<tab>") 'tabbar-forward)
(global-set-key (kbd "C-<tab>") 'switch-to-last-buffer)
;(global-set-key (kbd "C-<tab>") 'bubble-buffer)


(global-set-key (kbd "C-M-l") 'switch-to-buffer)
(global-set-key [f5] 'start-gdb)
;(global-set-key [f9] 'gud-break)
(global-set-key [f10] 'gud-next)
(global-set-key [f11] 'gud-step)
(global-set-key (kbd "<S-f11>") 'gud-step)
(global-set-key (kbd "<S-f9>") 'gud-cont)

; $ fc-list :spacing=mono
; $ xlsfonts
(set-default-font "-adobe-courier-medium-r-normal--12-120-75-75-m-70-iso8859-1")
;(set-default-font "Inconsolata-8")
;(set-default-font "DejaVu Sans Mono:style=Book")

;(set-face-attribute 'default nil :font "-unknown-Inconsolata-normal-normal-normal-*-12-*-*-*-m-0-iso10646-1" :height 90)


; visual bookmarks
(autoload 'bm-toggle   "bm" "Toggle bookmark in current buffer." t)
(autoload 'bm-next     "bm" "Goto bookmark."                     t)
(autoload 'bm-previous "bm" "Goto previous bookmark."            t)

(global-set-key (kbd "<C-f2>") 'bm-toggle)
(global-set-key (kbd "<f2>")   'bm-next)
(global-set-key (kbd "<S-f2>") 'bm-previous)

;; make bookmarks persistent as default
(setq-default bm-buffer-persistence t)

;; Make sure the repository is loaded as early as possible
;(setq bm-restore-repository-on-load t)
(require 'bm)

;; Loading the repository from file when on start up.
(add-hook 'after-init-hook 'bm-repository-load)

;; Restoring bookmarks when on file find.
(add-hook 'find-file-hooks 'bm-buffer-restore)

;; Saving bookmark data on killing a buffer
(add-hook 'kill-buffer-hook 'bm-buffer-save)

;; Saving the repository to file when on exit.
;; kill-buffer-hook is not called when emacs is killed, so we
;; must save all bookmarks first.
;; (add-hook 'kill-emacs-hook '(lambda nil
;;                               (bm-buffer-save-all)
;;                               (bm-repository-save)))

;; Update bookmark repository when saving the file.
;(add-hook 'after-save-hook 'bm-buffer-save)

;; Restore bookmarks when buffer is reverted.
(add-hook 'after-revert-hook 'bm-buffer-restore)

(require 'uncrustify)
(setq uncrustify-args "-l CPP --replace")
(defun my-uncrustify-hook ()
;  (setq uncrustify-uncrustify-on-save t)
;  (add-hook 'uncrustify-init-hooks 'bm-buffer-save)
;  (add-hook 'uncrustify-finish-hooks 'bm-buffer-restore)
;; (message "adding kill hook")
;; (make-local-variable 'kill-buffer-hook)
;; (add-hook 'kill-buffer-hook '(lambda()
;; (interactive)
;; (let* ((uncrustify-current-line (line-number-at-pos)))
;; (save-excursion
;; (message "why sadfasdf")
;; (uncrustify-impl (point-min) (point-max)))
;; (goto-char (point-min)) (forward-line (1- uncrustify-current-line)))))
;; (message "kill hook added")
  (global-set-key (kbd "C-M-]") 'uncrustify)
  (global-set-key (kbd "C-M-\\") 'uncrustify-buffer)
)

; add uncrustify only if ~/.uncrustify.cfg exists
(when (file-readable-p "~/.uncrustify.cfg")
  (add-hook 'c++-mode-hook 'my-uncrustify-hook)
;;  (add-hook 'c-mode-hook 'my-uncrustify-hook)
)


; auto comment out a region
(autoload 'comment-out-region "comment" nil t)
(global-set-key (read-kbd-macro "C-M-?") 'comment-out-region)

;; Mousewheel
;; (defun sd-mousewheel-scroll-up (event)
;;   "Scroll window under mouse up by five lines."
;;   (interactive "e")
;;   (let ((current-window (selected-window)))
;;     (unwind-protect
;;         (progn
;;           (select-window (posn-window (event-start event)))
;;           (scroll-up 5))
;;       (select-window current-window))))
;;
;; (defun sd-mousewheel-scroll-down (event)
;;   "Scroll window under mouse down by five lines."
;;   (interactive "e")
;;   (let ((current-window (selected-window)))
;;     (unwind-protect
;;         (progn
;;           (select-window (posn-window (event-start event)))
;;           (scroll-down 5))
;;       (select-window current-window))))
;;
;; (global-set-key (kbd "<mouse-5>") 'sd-mousewheel-scroll-up)
;; (global-set-key (kbd "<mouse-4>") 'sd-mousewheel-scroll-down)



;; ignore `.svn' and `CVS' directories
(setq grep-find-command
      (concat
       "find . \\( -path '*/.svn' -o -path '*/CVS' \\) -prune -o -type f -print0"
              " | xargs -0 -e grep -i -n -e "))

;(defun c++-setup-boost (boost-root)
;  (when (file-accessible-directory-p boost-root)
;    (let ((cfiles (recur-list-files boost-root "\\(config\\|user\\)\\.hpp")))
;      (dolist (file cfiles)
;        (add-to-list 'semantic-lex-c-preprocessor-symbol-file file)))))

(setq igrep-find-prune-clause "-type d -wholename \"*.svn*\"")
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(gdb-many-windows t)
 '(js2-basic-offset 2))
(custom-set-faces
  ;; custom-set-faces was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 )

; open header files in c++-mode
(add-to-list 'auto-mode-alist '("\\.h\\'" . c++-mode))

(require 'revbufs)

(defun copy-buffer-file-name-as-kill (choice)
  "Copy the buffer-file-name to the kill-ring"
  (interactive "cCopy Buffer Name (F) Full, (D) Directory, (N) Name")
  (let ((new-kill-string)
        (name (if (eq major-mode 'dired-mode)
                  (dired-get-filename)
                (or (buffer-file-name) ""))))
    (cond ((eq choice ?f)
           (setq new-kill-string name))
          ((eq choice ?d)
           (setq new-kill-string (file-name-directory name)))
          ((eq choice ?n)
           (setq new-kill-string (file-name-nondirectory name)))
          (t (message "Quit")))
    (when new-kill-string
      (message "%s copied" new-kill-string)
      (x-select-text new-kill-string)
      (kill-new new-kill-string))))
(global-set-key (read-kbd-macro "M-C-w") 'copy-buffer-file-name-as-kill)


;; python 
; http://www.emacswiki.org/emacs/?action=browse;oldid=PythonMode;id=PythonProgrammingInEmacs
; http://hide1713.wordpress.com/2009/01/30/setup-perfect-python-environment-in-emacs/
;
; sudo apt-get install python-mode
; sudo easy_install rope ropemacs
;(require 'ipython)
;(setq py-python-command-args '( "--colors" "Linux"))

;(require 'python-mode)
;(require 'auto-complete)

;(autoload 'python-mode "python-mode" "Python Mode." t)
;(add-to-list 'auto-mode-alist '("\\.py\\'" . python-mode))
;(add-to-list 'interpreter-mode-alist '("python" . python-mode))
;(defun python-shell (&rest args) nil)
;(setq py-shell-alist nil)
;(setq py-python-command nil)
;(setq py-shell nil)
;(setq py-shell-name nil)

;; (add-hook 'python-mode-hook
;;       (lambda ()
;; 	(set-variable 'py-indent-offset 4)
;; 	;(set-variable 'py-smart-indentation nil)
;; 	(set-variable 'indent-tabs-mode nil)
;; 	(define-key py-mode-map (kbd "RET") 'newline-and-indent)
;; 	;(define-key py-mode-map [tab] 'yas/expand)
;; 	;(setq yas/after-exit-snippet-hook 'indent-according-to-mode)
;; 	(smart-operator-mode-on)
;; 	))


;(require 'pymacs)
;; pymacs

;(autoload 'pymacs-apply "pymacs")
;(autoload 'pymacs-call "pymacs")
;(autoload 'pymacs-eval "pymacs" nil t)
;(autoload 'pymacs-exec "pymacs" nil t)
;(autoload 'pymacs-load "pymacs" nil t)


;; (add-hook 'python-mode-hook '(lambda ()
;;                                (pymacs-load "ropemacs" "rope-")
;;                                (setq ropemacs-enable-autoimport t)
;;                                (eldoc-mode 1) ) t)

;;; Auto-completion
;;;  Integrates:
;;;   1) Rope
;;;   2) Yasnippet
;;;   all with AutoComplete.el

;; auto-complete-config is interesting, but current setting didn't do smart completions

;; (require 'auto-complete-config)
;; (setq ac-modes
;;   '(emacs-lisp-mode
;;     lisp-interaction-mode
;;     python-mode))
;; 
;; (add-hook 'python-mode-hook '(lambda ()
;;                                (add-to-list 'ac-dictionary-directories (expand-file-name "~/.emacs-lisp/ac-dist"))
;;                                (global-auto-complete-mode t)
;;                                (ac-config-default)
;;                                ) t)


;; (require 'yasnippet)
;; (yas/initialize)
;; ;(yas/load-directory "~/.emacs.d/snippets")
;; 
;; ;; Initialize Yasnippet                                                                                        
;; ;;Don't map TAB to yasnippet                                                                                    
;; ;;In fact, set it to something we'll never use because
;; (setq yas/trigger-key (kbd "C-c <kp-multiply>"))
;; 
;; (defun prefix-list-elements (list prefix)
;;   (let (value)
;;     (nreverse
;;      (dolist (element list value)
;;       (setq value (cons (format "%s%s" prefix element) value))))))
;; (defvar ac-source-rope
;;   '((candidates
;;      . (lambda ()
;;          (prefix-list-elements (rope-completions) ac-target))))
;;   "Source for Rope")
;; (defun ac-python-find ()
;;   "Python `ac-find-function'."
;;   (require 'thingatpt)
;;   (let ((symbol (car-safe (bounds-of-thing-at-point 'symbol))))
;;     (if (null symbol)
;;         (if (string= "." (buffer-substring (- (point) 1) (point)))
;;             (point)
;;           nil)
;;       symbol)))
;; (defun ac-python-candidate ()
;;   "Python `ac-candidates-function'"
;;   (let (candidates)
;;     (dolist (source ac-sources)
;;       (if (symbolp source)
;;           (setq source (symbol-value source)))
;;       (let* ((ac-limit (or (cdr-safe (assq 'limit source)) ac-limit))
;;              (requires (cdr-safe (assq 'requires source)))
;;              cand)
;;         (if (or (null requires)
;;                 (>= (length ac-target) requires))
;;             (setq cand
;;                   (delq nil
;;                         (mapcar (lambda (candidate)
;;                                   (propertize candidate 'source source))
;;                                 (funcall (cdr (assq 'candidates source)))))))
;;         (if (and (> ac-limit 1)
;;                  (> (length cand) ac-limit))
;;             (setcdr (nthcdr (1- ac-limit) cand) nil))
;;         (setq candidates (append candidates cand))))
;;     (delete-dups candidates)))
;; (add-hook 'python-mode-hook
;;           (lambda ()
;;                  (auto-complete-mode 1)
;;                  (set (make-local-variable 'ac-sources)
;;                       (append ac-sources '(ac-source-rope) '(ac-source-yasnippet)))
;;                  (set (make-local-variable 'ac-find-function) 'ac-python-find)
;;                  (set (make-local-variable 'ac-candidate-function) 'ac-python-candidate)
;;                  (set (make-local-variable 'ac-auto-start) nil)))
;; 
;; ;;Ryan's python specific tab completion
;; (defun ryan-python-tab ()
;;   ; Try the following:
;;   ; 1) Do a yasnippet expansion
;;   ; 2) Do a Rope code completion
;;   ; 3) Do an indent
;;   (interactive)
;;   (if (eql (ac-start) 0)
;;       (indent-for-tab-command)))
;; 
;; (defadvice ac-start (before advice-turn-on-auto-start activate)
;;   (set (make-local-variable 'ac-auto-start) t))
;; (defadvice ac-cleanup (after advice-turn-off-auto-start activate)
;;   (set (make-local-variable 'ac-auto-start) nil))
;; (define-key py-mode-map "\t" 'ryan-python-tab)

;(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "/var/www/.emacs-lisp/ac-dict")
;(ac-config-default)
