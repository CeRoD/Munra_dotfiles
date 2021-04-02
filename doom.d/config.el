(setq user-full-name "John Doe"
      user-mail-address "john@doe.com")
(require 'cl)

(setq doom-font (font-spec :family "JetBrainsMono NL" :size 24 :weight 'ExtraBold :height 230)
       doom-variable-pitch-font (font-spec :family "JetBrainsMono NL" :size 24 :height 230)
       doom-big-font (font-spec :family "JetBrainsMono NL" :size 24 :height 230)
       doom-serif-font (font-spec :family "JetBrainsMono NL" :size 24 :height 230))

(after! doom-themes
  (setq doom-themes-enable-bold t
        doom-themes-enable-italic t))

(custom-set-faces!
  '(font-lock-comment-face :slant italic)
  '(font-lock-keyword-face :slant italic))

(setq line-spacing 0.2)

(setq doom-theme 'doom-gruvbox)

(defconst doom-frame-transparency 95)
(set-frame-parameter (selected-frame) 'alpha doom-frame-transparency)
(add-to-list 'default-frame-alist `(alpha . ,doom-frame-transparency))
(defun dwc-smart-transparent-frame ()
  (set-frame-parameter
    (selected-frame)
    'alpha (if (frame-parameter (selected-frame) 'fullscreen)
              100
             doom-frame-transparency)))

(after! org
  (require 'org-bullets)  ; Nicer bullets in org-mode
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1)))
  (setq org-directory "~/Storage/Org/"
        org-agenda-files '("~/Storage/Org/agenda.org")
        org-default-notes-file (expand-file-name "notes.org" org-directory)
        org-ellipsis " ▼ "
        org-log-done 'time
        org-journal-dir "~/Org/Storage/journal/"
        org-journal-date-format "%B %d, %Y (%A)"
        org-journal-file-format "%Y-%m-%d.org"
        org-hide-emphasis-markers t
        ;; org-fontify-whole-heading-line t
        ;; ex. of org-link-abbrev-alist in action
        ;; [[arch-wiki:Name_of_Page][Description]]
        ;; org-link-abbrev-alist    ; This overwrites the default Doom org-link-abbrev-list
        ;;   '(("google" . "http://www.google.com/search?q=")
        ;;     ("arch-wiki" . "https://wiki.archlinux.org/index.php/")
        ;;     ("ddg" . "https://duckduckgo.com/?q=")
        ;;     ("wiki" . "https://en.wikipedia.org/wiki/"))
        ;; org-todo-keywords        ; This overwrites the default Doom org-todo-keywords
        ;;   '((sequence
        ;;      "TODO(t)"           ; A task that is ready to be tackled
        ;;      "BLOG(b)"           ; Blog writing assignments
        ;;      "GYM(g)"            ; Things to accomplish at the gym
        ;;      "PROJ(p)"           ; A project that contains other tasks
        ;;      "VIDEO(v)"          ; Video assignments
        ;;      "WAIT(w)"           ; Something is holding up this task
        ;;      "|"                 ; The pipe necessary to separate "active" states and "inactive" states
        ;;      "DONE(d)"           ; Task has been completed
        ;;      "CANCELLED(c)" )))) ; Task has been cancelled
        ))

(setq org-list-demote-modify-bullet '(("+" . "-") ("-" . "+") ("*" . "+") ("1." . "a.")))

(setq-default display-line-numbers-type `relative)

(setq sentence-end-double-space nil )

(setq ;centaur-tabs-set-bar 'over
      ;centaur-tabs-set-icons t
      ;centaur-tabs-gray-out-icons 'buffer
      ;centaur-tabs-height 24
      centaur-tabs-set-bar 'under
      centaur-tabs-set-close-button nil
      centaur-tabs-set-modified-marker t
      ;; centaur-tabs-style "bar"
      centaur-tabs-style "chamfer"
      centaur-tabs-modified-marker "⚫")

(setq fringe-mode 8)

(define-globalized-minor-mode my-global-rainbow-mode rainbow-mode
  (lambda () (rainbow-mode 1)))
(my-global-rainbow-mode 1)

(custom-set-faces
  '(mode-line ((t (:family "Cascadia Code" :size 16))))
  '(mode-line-inactive ((t (:family "Cascadia Code" :size 16)))))

(setq doom-modeline-buffer-file-name-style 'truncate-nil)

(setq doom-modeline-minor-modes nil)

(setq doom-modeline-enable-word-count nil)

(setq doom-modeline-indent-info nil)

(setq doom-modeline-continuous-word-count-modes '(markdown-mode gfm-mode org-mode))

(use-package undo-tree
  :diminish undo-tree-mode
  :config
  (progn
    (global-undo-tree-mode)
    (setq undo-tree-visualizer-timestamps t)
    (setq undo-tree-visualizer-diff t)))

(use-package darkroom
  :commands darkroom-mode
  :config
  (setq display-line-numbers-mode 0)
  (setq darkroom-text-scale-increase 0))

(map! :leader
      :desc "DarkRoom"
      "t z" #'darkroom-mode)

(use-package company
  :ensure t
  :init (add-hook ' after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0
        company-minimum-prefix-length 3
        company-tooltip-limit 5
        company-tooltip-align-annotations t
        company-show-numbers t)
  ;; (setq company-backends
  ;;       '((company-files
  ;;          company-yasnippet
  ;;          company-keywords
  ;;          company-capf)
  ;;         (company-abbrev company-dabbrev)))
)

(add-hook 'c++-mode-hook  'irony-mode)
(add-hook 'c-mode-hook    'irony-mode)
(add-hook 'objc-mode-hook 'irony-mode)
(add-hook 'irony-mode-hook 'irony-cdb-autosetup-compile-options)

(use-package company-irony
  :ensure t
  :config
  ;; (require 'company 'irony)
  :after (company irony)
  (add-to-list 'company-backends '(company-irony :with company-capf)))

(use-package company-anaconda
  :ensure t
  :after (company anaconda-mode)
  :config
  ;;(require 'company)
  (add-to-list 'company-backends '(company-anaconda :with company-capf)))

(use-package beacon
  :ensure t
  :config
  (setq beacon-blink-when-point-moves-vertically 2) ; default nil
  (setq beacon-blink-when-point-moves-horizontally 2) ; default nil
  (setq beacon-blink-when-buffer-changes t) ; default t
  (setq beacon-blink-when-window-scrolls t) ; default t
  (setq beacon-blink-when-window-changes t) ; default t
  (setq beacon-blink-when-focused t) ; default nil
  (setq beacon-blink-duration 0.5) ; default 0.3
  (setq beacon-blink-delay 0.5) ; default 0.3
  (setq beacon-size 50) ; default 40
  ;; (setq beacon-color "yellow") ; default 0.5
  (setq beacon-color 0.5) ; default 0.5
  (setq beacon-push-mark 10) ;????, what this mean?
  (beacon-mode 1))

(use-package nyan-mode
  :ensure t
  :config
  (nyan-mode)
  (nyan-start-animation))

;; (defconst jetbrains-ligature-mode--ligatures
;;    '("-->" "//" "/**" "/*" "*/" "<!--" ":=" "->>" "<<-" "->" "<-"
;;      "<=>" "==" "!=" "<=" ">=" "=:=" "!==" "&&" "||" "..." ".."
;;      "|||" "///" "&&&" "===" "++" "--" "=>" "|>" "<|" "||>" "<||"
;;      "|||>" "<|||" ">>" "<<" "::=" "|]" "[|" "{|" "|}"
;;      "[<" ">]" ":?>" ":?" "/=" "[||]" "!!" "?:" "?." "::"
;;      "+++" "??" "###" "##" ":::" "####" ".?" "?=" "=!=" "<|>"
;;      "<:" ":<" ":>" ">:" "<>" "***" ";;" "/==" ".=" ".-" "__"
;;      "=/=" "<-<" "<<<" ">>>" "<=<" "<<=" "<==" "<==>" "==>" "=>>"
;;      ">=>" ">>=" ">>-" ">-" "<~>" "-<" "-<<" "=<<" "---" "<-|"
;;      "<=|" "/\\" "\\/" "|=>" "|~>" "<~~" "<~" "~~" "~~>" "~>"
;;      "<$>" "<$" "$>" "<+>" "<+" "+>" "<*>" "<*" "*>" "</>" "</" "/>"
;;      "<->" "..<" "~=" "~-" "-~" "~@" "^=" "-|" "_|_" "|-" "||-"
;;      "|=" "||=" "#{" "#[" "]#" "#(" "#?" "#_" "#_(" "#:" "#!" "#="
;;      "&="))

;; (dolist (pat jetbrains-ligature-mode--ligatures)
;;   (set-char-table-range composition-function-table
;;                       (aref pat 0)
;;                       (nconc (char-table-range composition-function-table (aref pat 0))
;;                              (list (vector (regexp-quote pat)
;;                                            0
;;                                     'compose-gstring-for-graphic)))))

(defun my/smarter-move-beginning-of-line (arg)
  "Move point back to indentation of beginning of line.

Move point to the first non-whitespace character on this line.
If point is already there, move to the beginning of the line.
Effectively toggle between the first non-whitespace character and
the beginning of the line.

If ARG is not nil or 1, move forward ARG - 1 lines first.  If
point reaches the beginning or end of the buffer, stop there."
  (interactive "^p")
  (setq arg (or arg 1))

  ;; Move lines first
  (when (/= arg 1)
    (let ((line-move-visual nil))
      (forward-line (1- arg))))

  (let ((orig-point (point)))
    (back-to-indentation)
    (when (= orig-point (point))
      (move-beginning-of-line 1))))

;; remap C-a to `smarter-move-beginning-of-line'
(global-set-key [remap (evil-digit-argument-or-evil-beginning-of-line)]
                'my/smarter-move-beginning-of-line)

(evil-ex-define-cmd "W" #'evil-write)

(setq key-chord-two-keys-delay 0.5)
(key-chord-define evil-insert-state-map "kj" 'evil-normal-state)
(key-chord-mode 1)

(setq confirm-kill-emacs nil)

(use-package minions
  :config
  (setq minions-mode-line-lighter "")
  (setq minions-mode-line-delimiters '("" . ""))
  (minions-mode +1))

(use-package vterm
  :commands vterm
  :config
  (setq vterm-max-scrollback 100000))

(setq x-stretch-cursor t)
