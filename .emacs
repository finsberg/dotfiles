;; ~/.emacs - This is my emacs file

;; User details
(setq user-fill-name "Henrik Nicolay Finsberg")
(setq user-mail-adress "henriknf@simula.no")


;; INSTALL PACKAGES
;; --------------------------------------

(require 'package)

(add-to-list 'package-archives
       '("melpa" . "http://melpa.org/packages/") t)

(package-initialize)
(when (not package-archive-contents)
  (package-refresh-contents))

(defvar myPackages
  '(better-defaults
    ein
    elpy
    flycheck
    material-theme))

(mapc #'(lambda (package)
    (unless (package-installed-p package)
      (package-install package)))
      myPackages)




;; BASIC CUSTOMIZATION
;; --------------------------------------

; Theme
;; (load-theme 'deeper-blue)
(load-theme 'material t) ;; load material theme

;; Enable line numbers globally
(global-linum-mode t) 
(add-hook 'pdf-view-mode-hook (lambda() (linum-mode -1)))


; Clock
(display-time)


; When switching between buffers using the mouse, do not use minibuffer
; it will throw an error
(defun stop-using-minibuffer ()
  "kill the minibuffer"
  (when (and (>= (recursion-depth) 1) (active-minibuffer-window))
    (abort-recursive-edit)))

(add-hook 'mouse-leave-buffer-hook 'stop-using-minibuffer)

;; Bash completions in shell mode
(autoload 'bash-completion-dynamic-complete 
   "bash-completion"
   "BASH completion hook")
 (add-hook 'shell-dynamic-complete-functions
   'bash-completion-dynamic-complete)

;; KEY BINDINGS
;; --------------------------------------


;; Fix some mac issues

(setq mac-option-key-is-meta 'meta)
(setq mac-right-option-modifier nil)

(global-set-key "\M-(" (lambda () (interactive) (insert "{")))
(global-set-key "\M-)" (lambda () (interactive) (insert "}")))

(global-set-key "\M-8" (lambda () (interactive) (insert "[")))
(global-set-key "\M-9" (lambda () (interactive) (insert "]")))


;; ; Dead keys
(define-key key-translation-map [dead-circumflex] "^")
(define-key key-translation-map [dead-diaeresis] "\"")
(define-key key-translation-map [dead-tilde] "~")

; Modify hotkeys for adjusting split panels
(global-set-key (kbd "C--") 'enlarge-window)
(global-set-key (kbd "C-.") 'shrink-window)
(global-set-key (kbd "C-æ") 'enlarge-window-horizontally)
(global-set-key (kbd "C-ø") 'shrink-window-horizontally)


(define-key key-translation-map (kbd "´") (kbd "\\"))

;; Indent / unindent region
(defun my-indent-region (N)
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) (* N 4))
             (setq deactivate-mark nil))
    (self-insert-command N)))

(defun my-unindent-region (N)
  (interactive "p")
  (if (use-region-p)
      (progn (indent-rigidly (region-beginning) (region-end) (* N -4))
             (setq deactivate-mark nil))
    (self-insert-command N)))

(global-set-key (kbd "C-'") 'my-indent-region)
(global-set-key (kbd "C-1") 'my-unindent-region)

;; Spellcheck list possible words
(global-set-key (kbd "C-å") 'flyspell-correct-word-before-point)

;; Comment/Uncomment current line
(defun comment-or-uncomment-region-or-line ()
    "Comments or uncomments the region or the current line if there's no active region."
    (interactive)
    (let (beg end)
        (if (region-active-p)
            (setq beg (region-beginning) end (region-end))
            (setq beg (line-beginning-position) end (line-end-position)))
        (comment-or-uncomment-region beg end)))
(global-set-key (kbd "C-<") 'comment-or-uncomment-region-or-line) ;


;; Spell check
(setq ispell-program-name "/usr/local/bin/aspell")

;; FLYCHECK

(global-flycheck-mode)
(package-install 'exec-path-from-shell)
(exec-path-from-shell-initialize)


;; PYTHON STUFF
;; --------------------------------------
;; Elpy


(require 'elpy)
(elpy-enable)

;; Real time spell check --------

;; (when (require 'flycheck nil t)
;;   (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))
;;   (add-hook 'elpy-mode-hook 'flycheck-mode))


;; ------------- DROP DOWN FUNCTION MENY -------------------


;; ;; A hide-show outline mode for python mode
(add-hook 'python-mode-hook 'local-python-mode-hook)

(defun py-outline-level ()
  "This is so that `current-column` DTRT in otherwise-hidden text"
  ;; from ada-mode.el
  (let (buffer-invisibility-spec)
    (save-excursion
      (skip-chars-forward "\t ")
      (current-column))))

; this fragment originally came from the web somewhere, but the outline-regexp
; was horribly broken and is broken in all instances of this code floating
; around.  Finally fixed by Charl P. Botha <<a href="http://cpbotha.net/">http://cpbotha.net/</a>>
(defun local-python-mode-hook ()
  (setq outline-regexp "[^ \t\n]\\|[ \t]*\\(def[ \t]+\\|class[ \t]+\\)")
  ; enable our level computation
  (setq outline-level 'py-outline-level)
  ; do not use their \C-c@ prefix, too hard to type. Note this overides
  ;some python mode bindings
  (setq outline-minor-mode-prefix "\C-l")
  ; turn on outline mode
  (outline-minor-mode t)
  ; initially hide all but the headers
  (hide-body)
  (show-paren-mode 1)
  )

;; PDF STUFF
;; -------------------------------

(require 'pdf-view)
 
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . pdf-view-mode))
(provide 'init-pdfview)


;; LATEX STUFF
;; --------------------------------------

(setenv "PATH" (concat (getenv "PATH") ":/Library/TeX/texbin/"))  
(setq exec-path (append exec-path '("/Library/TeX/texbin/")))

(add-hook 'LaTeX-mode-hook #'latex-extra-mode)

;; Newline after 80 characters
(add-hook 'text-mode-hook 'turn-on-auto-fill)

;; Next error
(global-set-key (kbd "C-,") 'TeX-next-error)

;; LaTeX Autocomplete
(require 'package)
(package-initialize)

;; Dockview continuous scrolling
(setq doc-view-continuous t)


(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

(require 'ac-math) 
(add-to-list 'ac-modes 'latex-mode)   ; make auto-complete aware of `latex-mode`

 (defun ac-LaTeX-mode-setup () ; add ac-sources to default ac-sources
   (setq ac-sources
         (append '(ac-source-math-latex ac-source-latex-commands)
                 ac-sources))
   )
(add-hook 'LaTeX-mode-hook 'ac-LaTeX-mode-setup)
(global-auto-complete-mode t)
 
(setq ac-math-unicode-in-math-p t)


(eval-after-load "flyspell"
  '(progn
     (fset 'flyspell-emacs-popup 'flyspell-emacs-popup-textual)))

(defun flyspell-emacs-popup-textual (event poss word)
  "A textual flyspell popup menu."
  (require 'popup)
  (let* ((corrects (if flyspell-sort-corrections
		       (sort (car (cdr (cdr poss))) 'string<)
		     (car (cdr (cdr poss)))))
	 (cor-menu (if (consp corrects)
		       (mapcar (lambda (correct)
				 (list correct correct))
			       corrects)
		     '()))
	 (affix (car (cdr (cdr (cdr poss)))))
	 show-affix-info
	 (base-menu  (let ((save (if (and (consp affix) show-affix-info)
				     (list
				      (list (concat "Save affix: " (car affix))
					    'save)
				      '("Accept (session)" session)
				      '("Accept (buffer)" buffer))
				   '(("Save word" save)
				     ("Accept (session)" session)
				     ("Accept (buffer)" buffer)))))
		       (if (consp cor-menu)
			   (append cor-menu (cons "" save))
			 save)))
	 (menu (mapcar
		(lambda (arg) (if (consp arg) (car arg) arg))
		base-menu)))
    (cadr (assoc (popup-menu* menu :scroll-bar t) base-menu))))



(defvar local-packages '(projectile auto-complete epc jedi))

(defun uninstalled-packages (packages)
  (delq nil
	(mapcar (lambda (p) (if (package-installed-p p nil) nil p)) packages)))


;; Use pdf-tools to open PDF files
(setq TeX-view-program-selection '((output-pdf "PDF Tools"))
TeX-view-program-list '(("PDF Tools" TeX-pdf-tools-sync-view))
TeX-source-correlate-start-server t)



;; Update PDF buffers after successful LaTeX runs
(add-hook 'TeX-after-compilation-finished-functions
          #'TeX-revert-document-buffer)


(defun my/scroll-other-window ()
  (interactive)
  (let* ((wind (other-window-for-scrolling))
         (mode (with-selected-window wind major-mode)))
    (if (eq mode 'pdf-view-mode)
        (with-selected-window wind
      (pdf-view-next-line-or-next-page 2))
      (scroll-other-window 2))))

(defun my/scroll-other-window-down ()
  (interactive)
  (let* ((wind (other-window-for-scrolling))
         (mode (with-selected-window wind major-mode)))
    (if (eq mode 'pdf-view-mode)
    (with-selected-window wind
      (progn
        (pdf-view-previous-line-or-previous-page 2)
        (other-window 1)))
      (scroll-other-window-down 2))))


;; Matlab STUFF
;; --------------------------------------

 (autoload 'matlab-mode "matlab" "Matlab Editing Mode" t)
 (add-to-list
  'auto-mode-alist
  '("\\.m$" . matlab-mode))
(setq matlab-indent-function t)
(setq matlab-shell-command "/Applications/MATLAB_R2018a.app/bin/matlab")
(setq matlab-shell-command-switches (list "-nodesktop"))


;; MARKDOWN STUFF
;; --------------------------------------
(setq markdown-command "/usr/local/bin/pandoc")

;; ITERM2 MOUSE SUPPORT
  (unless window-system
      (require 'mouse)
      (xterm-mouse-mode t)
      (defun track-mouse (e)) 
      (setq mouse-sel-mode t)
 )


;; Turn of debugger
(setq debug-on-error nil)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   (quote
    (exec-path-from-shell yaml-mode web-mode vscode-icon sr-speedbar sphinx-mode python-mode python-docstring py-autopep8 projectile pdf-tools mmm-mode matlab-mode material-theme markdown-preview-mode markdown-mode+ magit latex-extra julia-mode jedi flymake-python-pyflakes flycheck elpy ein dockerfile-mode cuda-mode company-math company-auctex bibretrieve better-defaults bash-completion auto-complete-rst auctex-lua auctex-latexmk ac-math ac-ispell))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
