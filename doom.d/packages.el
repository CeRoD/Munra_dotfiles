;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.


;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)

;; Crosshairs
;; This library highlights the current line and the current column.
;;  It combines the features of libraries `hl-line.el', `hl-line+.el',
;;  and `col-highlight.el', which let you highlight the line or column
;;  individually.  See those libraries for more information, in
;;  particular for user options that affect the behavior.
(package! crosshairs)

;; Key-chord lets you bind commands to combinations of key-strokes.
;; Here a “key chord” means two keys pressed simultaneously, or a single
;; key quickly pressed twice. (*)
(package! key-chord)

(package! yaml-mode)

(package! org-bullets)

(package! dimmer)

(package! fish-mode)

(package! beacon)

(package! sublimity)

(package! darkroom)

(package! irony)

(package! rtags)

(package! nyan-mode)

;; (package! ligature)

;;Colorize color names in buffers
;;This minor mode sets background color to strings that match color
;; names, e.g. #0000ff is displayed in white with a blue background.
(package! rainbow-mode)

;; This package implements a menu that lists enabled minor-modes,
;; as well as commonly but not currently enabled minor-modes.
;; This menu is intended as a replacement for the incomplete yet wide list of enabled minor-modes
;; that is displayed in the mode line by default.
(package! minions)

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")


;; Doom's packages are pinned to a specific commit and updated from release to
;; release. The `unpin!' macro allows you to unpin single packages...
;(unpin! pinned-package)
;; ...or multiple packages
;(unpin! pinned-package another-pinned-package)
;; ...Or *all* packages (NOT RECOMMENDED; will likely break things)
;(unpin! t)
