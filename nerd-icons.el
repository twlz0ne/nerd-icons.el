;;; nerd-icons.el --- Nerd icons library -*- lexical-binding: t; -*-

;; Copyright (C) 2019 Gong Qijian <gongqijian@gmail.com>

;; Author: Gong Qijian <gongqijian@gmail.com>
;; Created: 2019/03/29
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4"))
;; URL: https://github.com/twlz0ne/nerd-icons
;; Keywords: convenience

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This package (code mainly taken from https://github.com/domtronn/all-the-icons.el/tree/f996faf)
;; is a utility for using and formatting various Icon
;; fonts within Emacs.  Icon Fonts allow you to propertize and format
;; icons the same way you would normal text.  This enables things such
;; as better scaling of and anti aliasing of the icons.

;; This package provides an interface to the Hack Nerd Fonts:
;; https://github.com/ryanoasis/nerd-fonts/

;; See README.md for more information.

;;; Change Log:

;;  0.1.0  2019/03/29  Initial version.

;;; Code:

(require 'cl-lib)
(require 'nerd-icons-data)
(require 'nerd-icons-faces)

;;; Custom Variables

(defgroup nerd-icons nil
  "Manage how Nerd Icons formats icons."
  :prefix "nerd-icons-"
  :group 'appearance
  :group 'convenience)

(defcustom nerd-icons-color-icons t
  "Whether or not to include a foreground colour when formatting the icon."
  :group 'nerd-icons
  :type 'boolean)

(defcustom nerd-icons-scale-factor 1.2
  "The base Scale Factor for the `height' face property of an icon."
  :group 'nerd-icons
  :type 'number)

(defcustom nerd-icons-default-adjust -0.2
  "The default adjustment to be made to the `raise' display property of an icon."
  :group 'nerd-icons
  :type 'number)

(defvar nerd-icons-icon-spec
  '(;; Meta
    ("\\.tags"          nerd-icons-octicon "tag"                     :height 1.0 :v-adjust 0.0 :face nerd-icons-blue)
    ("^TAGS$"           nerd-icons-octicon "tag"                     :height 1.0 :v-adjust 0.0 :face nerd-icons-blue)
    ("\\.log"           nerd-icons-octicon "bug"                     :height 1.0 :v-adjust 0.0 :face nerd-icons-maroon)

    ;;
    ("\\.key$"          nerd-icons-octicon "key"                     :v-adjust 0.0 :face nerd-icons-lblue)
    ("\\.pem$"          nerd-icons-octicon "key"                     :v-adjust 0.0 :face nerd-icons-orange)
    ("\\.p12$"          nerd-icons-octicon "key"                     :v-adjust 0.0 :face nerd-icons-dorange)
    ("\\.crt$"          nerd-icons-octicon "key"                     :v-adjust 0.0 :face nerd-icons-lblue)
    ("\\.pub$"          nerd-icons-octicon "key"                     :v-adjust 0.0 :face nerd-icons-blue)
    ("\\.gpg$"          nerd-icons-octicon "key"                     :v-adjust 0.0 :face nerd-icons-lblue)

    ("^TODO$"           nerd-icons-octicon "checklist"               :v-adjust 0.0 :face nerd-icons-lyellow)
    ("^LICENSE$"        nerd-icons-octicon "book"                    :height 1.0 :v-adjust 0.0 :face nerd-icons-blue)
    ("^readme"          nerd-icons-octicon "book"                    :height 1.0 :v-adjust 0.0 :face nerd-icons-lcyan)

    ("\\.fish"          nerd-icons-fileicon "terminal"               :face nerd-icons-lpink)
    ("\\.zsh"           nerd-icons-fileicon "terminal"               :face nerd-icons-lcyan)
    ("\\.sh"            nerd-icons-fileicon "terminal"               :face nerd-icons-purple)

    ;; Config
    ("\\.node"          nerd-icons-fileicon "nodejs"                 :height 1.0  :face nerd-icons-green)
    ("\\.babelrc$"      nerd-icons-fileicon "babel"                  :face nerd-icons-yellow)
    ("\\.bashrc$"       nerd-icons-fileicon "script"                 :height 0.9  :face nerd-icons-dpink)
    ("\\.bowerrc$"      nerd-icons-fileicon "bower"                  :height 1.0 :v-adjust 0.0 :face nerd-icons-silver)
    ("^bower.json$"     nerd-icons-fileicon "bower"                  :height 1.0 :v-adjust 0.0 :face nerd-icons-lorange)
    ("\\.ini$"          nerd-icons-octicon "settings"                :v-adjust 0.0 :face nerd-icons-yellow)
    ("\\.eslintignore"  nerd-icons-fileicon "eslint"                 :height 0.9  :face nerd-icons-purple)
    ("\\.eslint"        nerd-icons-fileicon "eslint"                 :height 0.9  :face nerd-icons-lpurple)
    ("\\.git"           nerd-icons-fileicon "git"                    :height 1.0  :face nerd-icons-lred)
    ("nginx"            nerd-icons-fileicon "nginx"                  :height 0.9  :face nerd-icons-dgreen)
    ("apache"           nerd-icons-fileicon "apache"                 :height 0.9  :face nerd-icons-dgreen)
    ("^Makefile$"       nerd-icons-fileicon "gnu"                    :face nerd-icons-dorange)
    ("\\.mk$"           nerd-icons-fileicon "gnu"                    :face nerd-icons-dorange)

    ("\\.dockerignore$" nerd-icons-fileicon "dockerfile"             :height 1.2  :face nerd-icons-dblue)
    ("^\\.?Dockerfile"  nerd-icons-fileicon "dockerfile"             :face nerd-icons-blue)
    ("^Brewfile$"       nerd-icons-faicon "beer"                     :face nerd-icons-lsilver)
    ("\\.npmignore"     nerd-icons-fileicon "npm"                    :face nerd-icons-dred)
    ("^package.json$"   nerd-icons-fileicon "npm"                    :face nerd-icons-red)
    ("^package.lock.json$" nerd-icons-fileicon "npm"                 :face nerd-icons-dred)
    ("^yarn\.lock"      nerd-icons-fileicon "yarn"                   :face nerd-icons-blue-alt)

    ("\.xml$"           nerd-icons-faicon "file-code-o"              :height 0.95 :face nerd-icons-lorange)

    ;; ;; AWS
    ("^stack.*.json$"   nerd-icons-fileicon "aws"                    :face nerd-icons-orange)


    ("^serverless\\.yml$" nerd-icons-faicon "bolt"                   :v-adjust 0.0 :face nerd-icons-yellow)
    ("\\.[jc]son$"      nerd-icons-octicon "settings"                :v-adjust 0.0 :face nerd-icons-yellow)
    ("\\.ya?ml$"        nerd-icons-octicon "settings"                :v-adjust 0.0 :face nerd-icons-dyellow)

    ("\\.pkg$"          nerd-icons-octicon "package"                 :v-adjust 0.0 :face nerd-icons-dsilver)
    ("\\.rpm$"          nerd-icons-octicon "package"                 :v-adjust 0.0 :face nerd-icons-dsilver)

    ("\\.elc$"          nerd-icons-octicon "file-binary"             :v-adjust 0.0 :face nerd-icons-dsilver)

    ("\\.gz$"           nerd-icons-octicon "file-binary"             :v-adjust 0.0 :face nerd-icons-lmaroon)
    ("\\.zip$"          nerd-icons-octicon "file-zip"                :v-adjust 0.0 :face nerd-icons-lmaroon)
    ("\\.7z$"           nerd-icons-octicon "file-zip"                :v-adjust 0.0 :face nerd-icons-lmaroon)

    ("\\.dat$"          nerd-icons-faicon "bar-chart"                :face nerd-icons-cyan :height 0.9)
    ;; lock files
    ("~$"               nerd-icons-octicon "lock"                    :v-adjust 0.0 :face nerd-icons-maroon)

    ("\\.dmg$"          nerd-icons-octicon "tools"                   :v-adjust 0.0 :face nerd-icons-lsilver)
    ("\\.dll$"          nerd-icons-faicon "cogs"                     :face nerd-icons-silver)
    ("\\.DS_STORE$"     nerd-icons-faicon "cogs"                     :face nerd-icons-silver)

    ;; Source Codes
    ("\\.scpt$"         nerd-icons-fileicon "apple"                  :face nerd-icons-pink)
    ("\\.aup$"          nerd-icons-fileicon "audacity"               :face nerd-icons-yellow)

    ("\\.elm"           nerd-icons-fileicon "elm"                    :face nerd-icons-blue)

    ("\\.erl$"          nerd-icons-fileicon "erlang"                 :face nerd-icons-red :v-adjust -0.1 :height 0.9)
    ("\\.hrl$"          nerd-icons-fileicon "erlang"                 :face nerd-icons-dred :v-adjust -0.1 :height 0.9)

    ("\\.eex$"          nerd-icons-fileicon "elixir"                 :face nerd-icons-lorange :v-adjust -0.1 :height 0.9)
    ("\\.ex$"           nerd-icons-fileicon "elixir"                 :face nerd-icons-lpurple :v-adjust -0.1 :height 0.9)
    ("\\.exs$"          nerd-icons-fileicon "elixir"                 :face nerd-icons-lred :v-adjust -0.1 :height 0.9)
    ("^mix.lock$"       nerd-icons-fileicon "elixir"                 :face nerd-icons-lyellow :v-adjust -0.1 :height 0.9)

    ("\\.java$"         nerd-icons-fileicon "java"                   :height 1.0  :face nerd-icons-purple)

    ("\\.go$"           nerd-icons-fileicon "go"                     :height 1.0  :face nerd-icons-blue)

    ("\\.mp3$"          nerd-icons-faicon "volume-up"                :face nerd-icons-dred)
    ("\\.wav$"          nerd-icons-faicon "volume-up"                :face nerd-icons-dred)
    ("\\.m4a$"          nerd-icons-faicon "volume-up"                :face nerd-icons-dred)

    ("\\.jl$"           nerd-icons-fileicon "julia"                  :v-adjust 0.0 :face nerd-icons-purple)
    ("\\.matlab$"       nerd-icons-fileicon "matlab"                 :face nerd-icons-orange)

    ("\\.nix$"          nerd-icons-fileicon "nix"                    :face nerd-icons-blue)

    ("\\.p[ml]$"        nerd-icons-fileicon "perl"                   :face nerd-icons-lorange)
    ("\\.pl6$"          nerd-icons-fileicon "perl6"                  :face nerd-icons-cyan)
    ("\\.pod$"          nerd-icons-fileicon "perldocs"               :height 1.2  :face nerd-icons-lgreen)

    ("\\.php$"          nerd-icons-fileicon "php"                    :face nerd-icons-lsilver)
    ("\\.pony$"         nerd-icons-fileicon "pony"                   :face nerd-icons-maroon)
    ("\\.prol?o?g?$"    nerd-icons-fileicon "prolog"                 :height 1.1  :face nerd-icons-lmaroon)
    ("\\.py$"           nerd-icons-fileicon "python"                 :height 1.0  :face nerd-icons-dblue)

    ("\\.rkt$"          nerd-icons-fileicon "racket"                 :height 1.2 :face nerd-icons-red)
    ("\\.gem$"          nerd-icons-fileicon "ruby-alt"               :face nerd-icons-red)
    ("_?test\\.rb$"        nerd-icons-fileicon "test-ruby"           :height 1.0 :v-adjust 0.0 :face nerd-icons-red)
    ("_?test_helper\\.rb$" nerd-icons-fileicon "test-ruby"           :height 1.0 :v-adjust 0.0 :face nerd-icons-dred)
    ("\\.rb$"           nerd-icons-fileicon "ruby"                   :v-adjust 0.0 :face nerd-icons-lred)
    ("\\.rs$"           nerd-icons-fileicon "rust"                   :height 1.2  :face nerd-icons-maroon)
    ("\\.rlib$"         nerd-icons-fileicon "rust"                   :height 1.2  :face nerd-icons-dmaroon)
    ("\\.r[ds]?x?$"     nerd-icons-fileicon "R"                      :face nerd-icons-lblue)

    ("\\.sbt$"          nerd-icons-fileicon   "sbt"                  :face nerd-icons-red)
    ("\\.scala$"        nerd-icons-fileicon "scala"                  :face nerd-icons-red)
    ("\\.scm$"          nerd-icons-fileicon   "scheme"               :height 1.2 :face nerd-icons-red)
    ("\\.swift$"        nerd-icons-fileicon "swift"                  :height 1.0 :v-adjust -0.1 :face nerd-icons-green)

    ("-?spec\\.ts$"     nerd-icons-fileicon "test-typescript"        :height 1.0 :v-adjust 0.0 :face nerd-icons-blue)
    ("-?test\\.ts$"     nerd-icons-fileicon "test-typescript"        :height 1.0 :v-adjust 0.0 :face nerd-icons-blue)
    ("-?spec\\.js$"     nerd-icons-fileicon "test-js"                :height 1.0 :v-adjust 0.0 :face nerd-icons-lpurple)
    ("-?test\\.js$"     nerd-icons-fileicon "test-js"                :height 1.0 :v-adjust 0.0 :face nerd-icons-lpurple)
    ("-?spec\\.jsx$"    nerd-icons-fileicon "test-react"             :height 1.0 :v-adjust 0.0 :face nerd-icons-blue-alt)
    ("-?test\\.jsx$"    nerd-icons-fileicon "test-react"             :height 1.0 :v-adjust 0.0 :face nerd-icons-blue-alt)

    ("-?spec\\."        nerd-icons-fileicon "test-generic"           :height 1.0 :v-adjust 0.0 :face nerd-icons-dgreen)
    ("-?test\\."        nerd-icons-fileicon "test-generic"           :height 1.0 :v-adjust 0.0 :face nerd-icons-dgreen)

    ("\\.tf\\(vars\\|state\\)?$" nerd-icons-fileicon "terraform"     :height 1.0 :face nerd-icons-purple-alt)

    ;; There seems to be a a bug with this font icon which does not
    ;; let you propertise it without it reverting to being a lower
    ;; case phi
    ("\\.c$"            nerd-icons-fileicon "c-line"                 :face nerd-icons-blue)
    ("\\.h$"            nerd-icons-fileicon "c-line"                 :face nerd-icons-purple)
    ("\\.m$"            nerd-icons-fileicon "apple"                  :v-adjust 0.0 :height 1.0)
    ("\\.mm$"           nerd-icons-fileicon "apple"                  :v-adjust 0.0 :height 1.0)

    ("\\.c\\(c\\|pp\\|xx\\)$"   nerd-icons-fileicon "cplusplus-line" :v-adjust -0.2 :face nerd-icons-blue)
    ("\\.h\\(h\\|pp\\|xx\\)$"   nerd-icons-fileicon "cplusplus-line" :v-adjust -0.2 :face nerd-icons-purple)

    ("\\.csx?$"         nerd-icons-fileicon "csharp-line"            :face nerd-icons-dblue)

    ("\\.cljc?$"        nerd-icons-fileicon "clojure-line"           :height 1.0 :face nerd-icons-blue :v-adjust 0.0)
    ("\\.cljs$"         nerd-icons-fileicon "cljs"                   :height 1.0 :face nerd-icons-dblue :v-adjust 0.0)

    ("\\.coffee$"       nerd-icons-fileicon "coffeescript"           :height 1.0  :face nerd-icons-maroon)
    ("\\.iced$"         nerd-icons-fileicon "coffeescript"           :height 1.0  :face nerd-icons-lmaroon)

    ;; Git
    ("^MERGE_"          nerd-icons-octicon "git-merge"               :v-adjust 0.0 :face nerd-icons-red)
    ("^COMMIT_EDITMSG"  nerd-icons-octicon "git-commit"              :v-adjust 0.0 :face nerd-icons-red)

    ;; Lisps
    ("\\.cl$"           nerd-icons-fileicon "clisp"                  :face nerd-icons-lorange)
    ("\\.l\\(isp\\)?$"  nerd-icons-fileicon "lisp"                   :face nerd-icons-orange)
    ("\\.el$"           nerd-icons-fileicon "elisp"                  :height 1.0 :v-adjust -0.2 :face nerd-icons-purple)

    ;; Stylesheeting
    ("\\.css$"          nerd-icons-fileicon "css3"                   :face nerd-icons-yellow)
    ("\\.scss$"         nerd-icons-fileicon "sass"                   :face nerd-icons-pink)
    ("\\.sass$"         nerd-icons-fileicon "sass"                   :face nerd-icons-dpink)
    ("\\.less$"         nerd-icons-fileicon "less"                   :height 0.8  :face nerd-icons-dyellow)
    ("\\.postcss$"      nerd-icons-fileicon "postcss"                :face nerd-icons-dred)
    ("\\.sss$"          nerd-icons-fileicon "postcss"                :face nerd-icons-dred)
    ("\\.styl$"         nerd-icons-fileicon "stylus"                 :face nerd-icons-lgreen)
    ("stylelint"        nerd-icons-fileicon "stylelint"              :face nerd-icons-lyellow)
    ("\\.csv$"          nerd-icons-octicon "graph"                   :v-adjust 0.0 :face nerd-icons-dblue)

    ("\\.hs$"           nerd-icons-fileicon "haskell"                :height 1.0  :face nerd-icons-red)

    ;; Web modes
    ("\\.inky-haml$"    nerd-icons-fileicon "haml"                   :face nerd-icons-lyellow)
    ("\\.haml$"         nerd-icons-fileicon "haml"                   :face nerd-icons-lyellow)
    ("\\.html?$"        nerd-icons-fileicon "html5"                  :face nerd-icons-orange)
    ("\\.inky-erb?$"    nerd-icons-fileicon "html5"                  :face nerd-icons-lred)
    ("\\.erb$"          nerd-icons-fileicon "html5"                  :face nerd-icons-lred)
    ("\\.hbs$"          nerd-icons-fileicon "moustache"              :face nerd-icons-green)
    ("\\.inky-slim$"    nerd-icons-octicon "dashboard"               :v-adjust 0.0 :face nerd-icons-yellow)
    ("\\.slim$"         nerd-icons-octicon "dashboard"               :v-adjust 0.0 :face nerd-icons-yellow)
    ("\\.jade$"         nerd-icons-fileicon "jade"                   :face nerd-icons-red)
    ("\\.pug$"          nerd-icons-fileicon "pug-alt"                :face nerd-icons-red)

    ;; JavaScript
    ("^gulpfile"        nerd-icons-fileicon "gulp"                   :height 1.0  :face nerd-icons-lred)
    ("^gruntfile"       nerd-icons-fileicon "grunt"                  :height 1.0 :v-adjust -0.1 :face nerd-icons-lyellow)
    ("^webpack"         nerd-icons-fileicon "webpack"                :face nerd-icons-lblue)

    ("\\.d3\\.?js"      nerd-icons-fileicon "d3"                     :height 0.8  :face nerd-icons-lgreen)

    ("\\.re$"            nerd-icons-fileicon "reason"                :height 1.0  :face nerd-icons-red-alt)
    ("\\.rei$"           nerd-icons-fileicon "reason"                :height 1.0  :face nerd-icons-dred)
    ("\\.ml$"            nerd-icons-fileicon "ocaml"                 :height 1.0  :face nerd-icons-lpink)
    ("\\.mli$"           nerd-icons-fileicon "ocaml"                 :height 1.0  :face nerd-icons-dpink)

    ("\\.react"         nerd-icons-fileicon "react"                  :height 1.1  :face nerd-icons-lblue)
    ("\\.d\\.ts$"       nerd-icons-fileicon "typescript"             :height 1.0 :v-adjust -0.1 :face nerd-icons-cyan-alt)
    ("\\.ts$"           nerd-icons-fileicon "typescript"             :height 1.0 :v-adjust -0.1 :face nerd-icons-blue-alt)
    ("\\.js$"           nerd-icons-fileicon "javascript"             :height 1.0 :v-adjust 0.0 :face nerd-icons-yellow)
    ("\\.es[0-9]$"      nerd-icons-fileicon "javascript"             :height 1.0 :v-adjust 0.0 :face nerd-icons-yellow)
    ("\\.jsx$"          nerd-icons-fileicon "jsx-2"                  :height 1.0 :v-adjust -0.1 :face nerd-icons-cyan-alt)
    ("\\.njs$"          nerd-icons-fileicon "nodejs"                 :height 1.2  :face nerd-icons-lgreen)
    ("\\.vue$"          nerd-icons-fileicon "vue"                    :face nerd-icons-lgreen)

    ;; File Types
    ("\\.ico$"          nerd-icons-octicon "file-media"              :v-adjust 0.0 :face nerd-icons-blue)
    ("\\.png$"          nerd-icons-octicon "file-media"              :v-adjust 0.0 :face nerd-icons-orange)
    ("\\.gif$"          nerd-icons-octicon "file-media"              :v-adjust 0.0 :face nerd-icons-green)
    ("\\.jpe?g$"        nerd-icons-octicon "file-media"              :v-adjust 0.0 :face nerd-icons-dblue)
    ("\\.svg$"          nerd-icons-fileicon "svg"                    :height 0.9  :face nerd-icons-lgreen)

    ;; Video
    ("\\.mov"           nerd-icons-faicon "film"                     :face nerd-icons-blue)
    ("\\.mp4"           nerd-icons-faicon "film"                     :face nerd-icons-blue)
    ("\\.ogv"           nerd-icons-faicon "film"                     :face nerd-icons-dblue)

    ;; Fonts
    ("\\.ttf$"          nerd-icons-fileicon "font"                   :v-adjust 0.0 :face nerd-icons-dcyan)
    ("\\.woff2?$"       nerd-icons-fileicon "font"                   :v-adjust 0.0 :face nerd-icons-cyan)

    ;; Doc
    ("\\.pdf"           nerd-icons-octicon "file-pdf"                :v-adjust 0.0 :face nerd-icons-dred)
    ("\\.te?xt"         nerd-icons-octicon "file-text"               :v-adjust 0.0 :face nerd-icons-cyan)
    ("\\.doc[xm]?$"     nerd-icons-fileicon "word"                   :face nerd-icons-blue)
    ("\\.texi?$"        nerd-icons-fileicon "tex"                    :face nerd-icons-lred)
    ("\\.md$"           nerd-icons-octicon "markdown"                :v-adjust 0.0 :face nerd-icons-lblue)
    ("\\.bib$"          nerd-icons-fileicon "bib"                    :face nerd-icons-maroon)
    ("\\.org$"          nerd-icons-fileicon "org"                    :face nerd-icons-lgreen)

    ("\\.pp[st]$"       nerd-icons-fileicon "powerpoint"             :face nerd-icons-orange)
    ("\\.pp[st]x$"      nerd-icons-fileicon "powerpoint"             :face nerd-icons-red)
    ("\\.knt$"          nerd-icons-fileicon "powerpoint"             :face nerd-icons-cyan)

    ("bookmark"         nerd-icons-octicon "bookmark"                :height 1.1 :v-adjust 0.0 :face nerd-icons-lpink)
    ("\\.cache$"        nerd-icons-octicon "database"                :height 1.0 :v-adjust 0.0 :face nerd-icons-green)

    ("^\\*scratch\\*$"  nerd-icons-faicon "sticky-note"              :face nerd-icons-lyellow)
    ("^\\*scratch.*"    nerd-icons-faicon "sticky-note"              :face nerd-icons-yellow)
    ("^\\*new-tab\\*$"  nerd-icons-material "star"                   :face nerd-icons-cyan)

    ("^\\."             nerd-icons-octicon "gear"                    :v-adjust 0.0)
    ("."                nerd-icons-faicon "file-o"                   :height 0.8 :v-adjust 0.0 :face nerd-icons-dsilver)))

;;; Functions

(defun nerd-icons-match-to-alist (file alist)
  "Match FILE against an entry in ALIST using `string-match'."
  (cdr (cl-find-if (lambda (it) (string-match (car it) file)) alist)))

(defun nerd-icons--read-candidates ()
  "Helper to build a list of candidates for all families."
  (cl-reduce 'append (mapcar (lambda (it) (nerd-icons--read-candidates-for-family (car it) t)) nerd-icons-alist)))

(defun nerd-icons--read-candidates-for-family (family &optional show-family)
  "Helper to build read candidates for FAMILY.
If SHOW-FAMILY is non-nil, displays the icons family in the candidate string."
  (let ((data   (cdr (assoc family nerd-icons-alist)))
        (icon-f (nerd-icons--function-name family)))
    (mapcar
     (lambda (it)
       (let* ((icon-name (car it))
              (icon-name-head (substring icon-name 0 1))
              (icon-name-tail (substring icon-name 1))

              (icon-display (propertize icon-name-head 'display (format "%s\t%s" (funcall icon-f icon-name) icon-name-head)))
              (icon-family (if show-family (format "\t[%s]" family) ""))

              (candidate-name (format "%s%s%s" icon-display icon-name-tail icon-family))
              (candidate-icon (funcall (nerd-icons--function-name family) icon-name)))

         (cons candidate-name candidate-icon)))
     data)))

;;;###autoload
(defun nerd-icons-insert (&optional arg family)
  "Interactive icon insertion function.
When Prefix ARG is non-nil, insert the propertized icon.
When FAMILY is non-nil, limit the candidates to the icon set matching it."
  (interactive "P")
  (let* ((standard-output (current-buffer))
         (candidates (if family
                         (nerd-icons--read-candidates-for-family family)
                       (nerd-icons--read-candidates)))
         (prompt     (if family
                         (format "%s Icon: " (funcall (nerd-icons--family-name family)))
                       "Icon : "))

         (selection (completing-read prompt candidates nil t))
         (result    (cdr (assoc selection candidates))))

    (if arg (prin1 result) (insert result))))

;;;###autoload
(defun nerd-icons-icon-for-file (file &rest arg-overrides)
  "Get the formatted icon for FILE.
ARG-OVERRIDES should be a plist containning `:height',
`:v-adjust' or `:face' properties like the normal icon
inserting functions."
  (let* ((icon (nerd-icons-match-to-alist file nerd-icons-icon-spec))
         (args (cdr icon)))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (apply (car icon) args)))

;;; Initialize

(eval-and-compile
  (defun nerd-icons--function-name (name)
    "Get the symbol for an icon function name for icon set NAME."
    (intern (concat "nerd-icons-" (downcase (symbol-name name)))))

  (defun nerd-icons--family-name (name)
    "Get the symbol for an icon family function for icon set NAME."
    (intern ;; (concat "nerd-icons-" (downcase (symbol-name name)) "-family")
            "Hack Nerd Font"))

  (defun nerd-icons--insert-function-name (name)
    "Get the symbol for an icon insert function for icon set NAME."
    (intern (concat "nerd-icons-insert-" (downcase (symbol-name name))))))

(defmacro nerd-icons--define-icon (name alist family &optional font-name)
"Macro to generate functions for inserting icons for icon set NAME.

NAME defines is the name of the iconset and will produce a
function of the for `nerd-icons-NAME'.

ALIST is the alist containing maps between icon names and the
UniCode for the character.  All of these can be found in the data
directory of this package.

FAMILY is the font family to use for the icons.
FONT-NAME is the name of the .ttf file providing the font, defaults to FAMILY."
`(progn
   (defun ,(nerd-icons--function-name name) (icon-name &rest args)
     (let ((icon (cdr (assoc icon-name ,alist)))
           (other-face (when nerd-icons-color-icons (plist-get args :face)))
           (height  (* nerd-icons-scale-factor (or (plist-get args :height) 1.0)))
           (v-adjust (* nerd-icons-scale-factor (or (plist-get args :v-adjust) nerd-icons-default-adjust)))
           (family ,family))
       (unless icon
         (error (format "Unable to find icon with name `%s' in icon set `%s'" icon-name (quote ,name))))
       (let ((face (if other-face
                       `(:family ,family :height ,height :inherit ,other-face)
                     `(:family ,family :height ,height))))
         (propertize icon
                     'face face           ;so that this works without `font-lock-mode' enabled
                     'font-lock-face face ;so that `font-lock-mode' leaves this alone
                     'display `(raise ,v-adjust)
                     'rear-nonsticky t))))
   (defun ,(nerd-icons--insert-function-name name) (&optional arg)
     ,(format "Insert a %s icon at point." family)
     (interactive "P")
     (nerd-icons-insert arg (quote ,name)))))

(nerd-icons--define-icon fileicon nerd-icons-alist/fileicon "Hack Nerd Font")
(nerd-icons--define-icon faicon   nerd-icons-alist/faicon   "Hack Nerd Font")
(nerd-icons--define-icon octicon  nerd-icons-alist/octicon  "Hack Nerd Font")
(nerd-icons--define-icon weather  nerd-icons-alist/weather "Hack Nerd Font")
(nerd-icons--define-icon material nerd-icons-alist/material "Hack Nerd Font")

(provide 'nerd-icons)

;;; nerd-icons.el ends here
