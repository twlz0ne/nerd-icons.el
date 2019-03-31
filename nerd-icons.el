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

(defvar nerd-icons-dir-icon-spec
  '(
    ("trash"            nerd-icons-faicon "trash-o"          :height 1.2 :v-adjust -0.1)
    ("dropbox"          nerd-icons-faicon "dropbox"          :height 1.0 :v-adjust -0.1)
    ("google[ _-]drive" nerd-icons-fileicon "google-drive" :height 1.3 :v-adjust -0.1)
    ("^atom$"           nerd-icons-fileicon "atom"         :height 1.2 :v-adjust -0.1)
    ("documents"        nerd-icons-faicon "book"             :height 1.0 :v-adjust -0.1)
    ("download"         nerd-icons-faicon "cloud-download"   :height 0.9 :v-adjust -0.2)
    ("desktop"          nerd-icons-octicon "device-desktop"  :height 1.0 :v-adjust -0.1)
    ("pictures"         nerd-icons-faicon "picture-o"        :height 0.9 :v-adjust -0.2)
    ("photos"           nerd-icons-faicon "camera-retro"     :height 1.0 :v-adjust -0.1)
    ("music"            nerd-icons-faicon "music"            :height 1.0 :v-adjust -0.1)
    ("movies"           nerd-icons-faicon "film"             :height 0.9 :v-adjust -0.1)
    ("code"             nerd-icons-octicon "code"            :height 1.1 :v-adjust -0.1)
    ("workspace"        nerd-icons-octicon "code"            :height 1.1 :v-adjust -0.1)
    ("test"             nerd-icons-fileicon "test-dir"       :height 0.9)
    ("\\.git"           nerd-icons-fileicon "git"          :height 1.0)
    ("."                nerd-icons-octicon "file-directory"  :height 1.0 :v-adjust -0.1)
    ))

(defvar nerd-icons-weather-icon-spec
  '(
    ("tornado"               nerd-icons-weather "tornado")
    ("hurricane"             nerd-icons-weather "hurricane")
    ("thunderstorms"         nerd-icons-weather "thunderstorm")
    ("sunny"                 nerd-icons-weather "day-sunny")
    ("rain.*snow"            nerd-icons-weather "rain-mix")
    ("rain.*hail"            nerd-icons-weather "rain-mix")
    ("sleet"                 nerd-icons-weather "sleet")
    ("hail"                  nerd-icons-weather "hail")
    ("drizzle"               nerd-icons-weather "sprinkle")
    ("rain"                  nerd-icons-weather "showers" :height 1.1 :v-adjust 0.0)
    ("showers"               nerd-icons-weather "showers")
    ("blowing.*snow"         nerd-icons-weather "snow-wind")
    ("snow"                  nerd-icons-weather "snow")
    ("dust"                  nerd-icons-weather "dust")
    ("fog"                   nerd-icons-weather "fog")
    ("haze"                  nerd-icons-weather "day-haze")
    ("smoky"                 nerd-icons-weather "smoke")
    ("blustery"              nerd-icons-weather "cloudy-windy")
    ("windy"                 nerd-icons-weather "cloudy-gusts")
    ("cold"                  nerd-icons-weather "snowflake-cold")
    ("partly.*cloudy.*night" nerd-icons-weather "night-alt-partly-cloudy")
    ("partly.*cloudy"        nerd-icons-weather "day-cloudy-high")
    ("cloudy.*night"         nerd-icons-weather "night-alt-cloudy")
    ("cxloudy.*day"          nerd-icons-weather "day-cloudy")
    ("cloudy"                nerd-icons-weather "cloudy")
    ("clear.*night"          nerd-icons-weather "night-clear")
    ("fair.*night"           nerd-icons-weather "stars")
    ("fair.*day"             nerd-icons-weather "horizon")
    ("hot"                   nerd-icons-weather "hot")
    ("not.*available"        nerd-icons-weather "na")
    ))

(defvar nerd-icons-mode-icon-spec
  '(
    (emacs-lisp-mode           nerd-icons-fileicon "elisp"              :height 1.0 :v-adjust -0.2 :face nerd-icons-purple)
    (erc-mode                  nerd-icons-faicon "commenting-o"         :height 1.0 :v-adjust 0.0 :face nerd-icons-white)
    (inferior-emacs-lisp-mode  nerd-icons-fileicon "elisp"              :height 1.0 :v-adjust -0.2 :face nerd-icons-lblue)
    (dired-mode                nerd-icons-octicon "file-directory"      :v-adjust 0.0)
    (lisp-interaction-mode     nerd-icons-fileicon "lisp"               :v-adjust -0.1 :face nerd-icons-orange)
    (sly-mrepl-mode            nerd-icons-fileicon "clisp"               :v-adjust -0.1 :face nerd-icons-orange)
    (slime-repl-mode           nerd-icons-fileicon "clisp"               :v-adjust -0.1 :face nerd-icons-orange)
    (org-mode                  nerd-icons-fileicon "org"                :v-adjust 0.0 :face nerd-icons-lgreen)
    (typescript-mode           nerd-icons-fileicon "typescript"         :v-adjust -0.1 :face nerd-icons-blue-alt)
    (js-mode                   nerd-icons-fileicon "javascript"       :v-adjust -0.1 :face nerd-icons-yellow)
    (js-jsx-mode               nerd-icons-fileicon "javascript"       :v-adjust -0.1 :face nerd-icons-yellow)
    (js2-mode                  nerd-icons-fileicon "javascript"       :v-adjust -0.1 :face nerd-icons-yellow)
    (js3-mode                  nerd-icons-fileicon "javascript"       :v-adjust -0.1 :face nerd-icons-yellow)
    (rjsx-mode                 nerd-icons-fileicon "jsx-2"              :v-adjust -0.1 :face nerd-icons-cyan-alt)
    (term-mode                 nerd-icons-octicon "terminal"            :v-adjust 0.2)
    (eshell-mode               nerd-icons-octicon "terminal"            :v-adjust 0.0 :face nerd-icons-purple)
    (magit-refs-mode           nerd-icons-octicon "git-branch"          :v-adjust 0.0 :face nerd-icons-red)
    (magit-process-mode        nerd-icons-octicon "mark-github"         :v-adjust 0.0)
    (magit-diff-mode           nerd-icons-octicon "git-compare"         :v-adjust 0.0 :face nerd-icons-lblue)
    (ediff-mode                nerd-icons-octicon "git-compare"         :v-adjust 0.0 :Face nerd-icons-red)
    (comint-mode               nerd-icons-faicon "terminal"             :v-adjust 0.0 :face nerd-icons-lblue)
    (eww-mode                  nerd-icons-faicon "firefox"              :v-adjust -0.1 :face nerd-icons-red)
    (org-agenda-mode           nerd-icons-octicon "checklist"           :v-adjust 0.0 :face nerd-icons-lgreen)
    (cfw:calendar-mode         nerd-icons-octicon "calendar"            :v-adjust 0.0)
    (ibuffer-mode              nerd-icons-faicon "files-o"              :v-adjust 0.0 :face nerd-icons-dsilver)
    (messages-buffer-mode      nerd-icons-faicon "stack-overflow"       :v-adjust -0.1)
    (help-mode                 nerd-icons-faicon "info"                 :v-adjust -0.1 :face nerd-icons-purple)
    (benchmark-init/tree-mode  nerd-icons-octicon "dashboard"           :v-adjust 0.0)
    (jenkins-mode              nerd-icons-fileicon "jenkins"            :face nerd-icons-blue)
    (magit-popup-mode          nerd-icons-fileicon "git"              :face nerd-icons-red)
    (magit-status-mode         nerd-icons-fileicon "git"              :face nerd-icons-lred)
    (magit-log-mode            nerd-icons-fileicon "git"              :face nerd-icons-green)
    (paradox-menu-mode         nerd-icons-faicon "archive"              :height 1.0 :v-adjust 0.0 :face nerd-icons-silver)
    (Custom-mode               nerd-icons-octicon "settings")

    ;; Special matcher for Web Mode based on the `web-mode-content-type' of the current buffer
    (web-mode             nerd-icons--web-mode-icon)

    (fundamental-mode                   nerd-icons-fileicon "elisp"            :height 1.0 :v-adjust -0.2 :face nerd-icons-dsilver)
    (special-mode                       nerd-icons-fileicon "elisp"            :height 1.0 :v-adjust -0.2 :face nerd-icons-yellow)
    (text-mode                          nerd-icons-octicon "file-text"         :v-adjust 0.0 :face nerd-icons-cyan)
    (ruby-mode                          nerd-icons-fileicon "ruby-alt"       :face nerd-icons-lred)
    (inf-ruby-mode                      nerd-icons-fileicon "ruby-alt"       :face nerd-icons-red)
    (projectile-rails-compilation-mode  nerd-icons-fileicon "ruby-alt"       :face nerd-icons-red)
    (rspec-compilation-mode             nerd-icons-fileicon "ruby-alt"       :face nerd-icons-red)
    (rake-compilation-mode              nerd-icons-fileicon "ruby-alt"       :face nerd-icons-red)
    (sh-mode                            nerd-icons-fileicon "terminal"       :face nerd-icons-purple)
    (shell-mode                         nerd-icons-fileicon "terminal"       :face nerd-icons-purple)
    (fish-mode                          nerd-icons-fileicon "terminal"       :face nerd-icons-lpink)
    (nginx-mode                         nerd-icons-fileicon "nginx"            :height 0.9  :face nerd-icons-dgreen)
    (apache-mode                        nerd-icons-fileicon "apache"         :height 0.9  :face nerd-icons-dgreen)
    (makefile-mode                      nerd-icons-fileicon "gnu"              :face nerd-icons-dorange)
    (dockerfile-mode                    nerd-icons-fileicon "dockerfile"       :face nerd-icons-blue)
    (docker-compose-mode                nerd-icons-fileicon "dockerfile"       :face nerd-icons-lblue)
    (xml-mode                           nerd-icons-faicon "file-code-o"        :height 0.95 :face nerd-icons-lorange)
    (json-mode                          nerd-icons-octicon "settings"          :face nerd-icons-yellow)
    (yaml-mode                          nerd-icons-octicon "settings"          :v-adjust 0.0 :face nerd-icons-dyellow)
    (elisp-byte-code-mode               nerd-icons-octicon "file-binary"       :v-adjust 0.0 :face nerd-icons-dsilver)
    (archive-mode                       nerd-icons-octicon "file-zip"          :v-adjust 0.0 :face nerd-icons-lmaroon)
    (elm-mode                           nerd-icons-fileicon "elm"              :face nerd-icons-blue)
    (erlang-mode                        nerd-icons-fileicon "erlang"         :face nerd-icons-red :v-adjust -0.1 :height 0.9)
    (elixir-mode                        nerd-icons-fileicon "elixir"         :face nerd-icons-lorange :v-adjust -0.1 :height 0.9)
    (java-mode                          nerd-icons-fileicon "java"           :height 1.0  :face nerd-icons-purple)
    (go-mode                            nerd-icons-fileicon "go"             :height 1.0  :face nerd-icons-blue)
    (matlab-mode                        nerd-icons-fileicon "matlab"           :face nerd-icons-orange)
    (perl-mode                          nerd-icons-fileicon "perl"           :face nerd-icons-lorange)
    (cperl-mode                         nerd-icons-fileicon "perl"           :face nerd-icons-lorange)
    (php-mode                           nerd-icons-fileicon "php"              :face nerd-icons-lsilver)
    (prolog-mode                        nerd-icons-fileicon "prolog"         :height 1.1  :face nerd-icons-lmaroon)
    (python-mode                        nerd-icons-fileicon "python"         :height 1.0  :face nerd-icons-dblue)
    (inferior-python-mode               nerd-icons-fileicon "python"         :height 1.0  :face nerd-icons-dblue)
    (racket-mode                        nerd-icons-fileicon "racket"           :height 1.2 :face nerd-icons-red)
    (rust-mode                          nerd-icons-fileicon "rust"           :height 1.2  :face nerd-icons-maroon)
    (scala-mode                         nerd-icons-fileicon "scala"          :face nerd-icons-red)
    (scheme-mode                        nerd-icons-fileicon   "scheme"         :height 1.2 :face nerd-icons-red)
    (swift-mode                         nerd-icons-fileicon "swift"          :height 1.0 :v-adjust -0.1 :face nerd-icons-green)
    (c-mode                             nerd-icons-fileicon "c-line"         :face nerd-icons-blue)
    (c++-mode                           nerd-icons-fileicon "cplusplus-line" :v-adjust -0.2 :face nerd-icons-blue)
    (csharp-mode                        nerd-icons-fileicon "csharp-line"    :face nerd-icons-dblue)
    (clojure-mode                       nerd-icons-fileicon "clojure-line"   :height 1.0  :face nerd-icons-blue)
    (cider-repl-mode                    nerd-icons-fileicon "clojure-line"   :height 1.0  :face nerd-icons-dblue)
    (clojurescript-mode                 nerd-icons-fileicon "cljs"             :height 1.0  :face nerd-icons-dblue)
    (coffee-mode                        nerd-icons-fileicon "coffeescript"   :height 1.0  :face nerd-icons-maroon)
    (lisp-mode                          nerd-icons-fileicon "lisp"             :face nerd-icons-orange)
    (css-mode                           nerd-icons-fileicon "css3"           :face nerd-icons-yellow)
    (scss-mode                          nerd-icons-fileicon "sass"           :face nerd-icons-pink)
    (sass-mode                          nerd-icons-fileicon "sass"           :face nerd-icons-dpink)
    (less-css-mode                      nerd-icons-fileicon "less"           :height 0.8  :face nerd-icons-dyellow)
    (stylus-mode                        nerd-icons-fileicon "stylus"         :face nerd-icons-lgreen)
    (csv-mode                           nerd-icons-octicon "graph"             :v-adjust 0.0 :face nerd-icons-dblue)
    (haskell-mode                       nerd-icons-fileicon "haskell"        :height 1.0  :face nerd-icons-red)
    (haml-mode                          nerd-icons-fileicon "haml"             :face nerd-icons-lyellow)
    (html-mode                          nerd-icons-fileicon "html5"          :face nerd-icons-orange)
    (rhtml-mode                         nerd-icons-fileicon "html5"          :face nerd-icons-lred)
    (mustache-mode                      nerd-icons-fileicon "moustache"        :face nerd-icons-green)
    (slim-mode                          nerd-icons-octicon "dashboard"         :v-adjust 0.0 :face nerd-icons-yellow)
    (jade-mode                          nerd-icons-fileicon "jade"             :face nerd-icons-red)
    (pug-mode                           nerd-icons-fileicon "pug"              :face nerd-icons-red)
    (react-mode                         nerd-icons-fileicon "react"          :height 1.1  :face nerd-icons-lblue)
    (image-mode                         nerd-icons-octicon "file-media"        :v-adjust 0.0 :face nerd-icons-blue)
    (texinfo-mode                       nerd-icons-fileicon "tex"              :face nerd-icons-lred)
    (markdown-mode                      nerd-icons-octicon "markdown"          :v-adjust 0.0 :face nerd-icons-lblue)
    (bibtex-mode                        nerd-icons-fileicon "bib"              :face nerd-icons-maroon)
    (org-mode                           nerd-icons-fileicon "org"              :face nerd-icons-lgreen)
    (compilation-mode                   nerd-icons-faicon "cogs"               :v-adjust 0.0 :height 1.0)
    (objc-mode                          nerd-icons-faicon "apple"              :v-adjust 0.0 :height 1.0)
    (tuareg-mode                        nerd-icons-fileicon "ocaml"            :v-adjust 0.0 :height 1.0)
    (purescript-mode                    nerd-icons-fileicon "purescript"       :v-adjust 0.0 :height 1.0)
    ))

(defvar nerd-icons-url-spec
  '(
    ;; Social media and communities
    ("^\\(https?://\\)?\\(www\\.\\)?del\\.icio\\.us" nerd-icons-faicon "delicious")
    ("^\\(https?://\\)?\\(www\\.\\)?behance\\.net" nerd-icons-faicon "behance")
    ("^\\(https?://\\)?\\(www\\.\\)?dribbble\\.com" nerd-icons-faicon "dribbble")
    ("^\\(https?://\\)?\\(www\\.\\)?facebook\\.com" nerd-icons-faicon "facebook-official")
    ("^\\(https?://\\)?\\(www\\.\\)?glide\\.me" nerd-icons-faicon "glide-g")
    ("^\\(https?://\\)?\\(www\\.\\)?plus\\.google\\.com" nerd-icons-faicon "google-plus")
    ("linkedin\\.com" nerd-icons-faicon "linkedin")
    ("^\\(https?://\\)?\\(www\\.\\)?ok\\.ru" nerd-icons-faicon "odnoklassniki")
    ("^\\(https?://\\)?\\(www\\.\\)?reddit\\.com" nerd-icons-faicon "reddit-alien")
    ("^\\(https?://\\)?\\(www\\.\\)?slack\\.com" nerd-icons-faicon "slack")
    ("^\\(https?://\\)?\\(www\\.\\)?snapchat\\.com" nerd-icons-faicon "snapchat-ghost")
    ("^\\(https?://\\)?\\(www\\.\\)?weibo\\.com" nerd-icons-faicon "weibo")
    ("^\\(https?://\\)?\\(www\\.\\)?twitter\\.com" nerd-icons-faicon "twitter")
    ;; Blogging
    ("joomla\\.org" nerd-icons-faicon "joomla")
    ("^\\(https?://\\)?\\(www\\.\\)?medium\\.com" nerd-icons-faicon "medium")
    ("tumblr\\.com" nerd-icons-faicon "tumblr")
    ("^wordpress\\.com" nerd-icons-faicon "wordpress")
    ;; Programming
    ("^\\(https?://\\)?\\(www\\.\\)?bitbucket\\.org" nerd-icons-octicon "bitbucket")
    ("^\\(https?://\\)?\\(www\\.\\)?codepen\\.io" nerd-icons-faicon "codepen")
    ("^\\(https?://\\)?\\(www\\.\\)?codiepie\\.com" nerd-icons-faicon "codiepie")
    ("^\\(https?://\\)?\\(www\\.\\)?gist\\.github\\.com" nerd-icons-octicon "gist")
    ("^\\(https?://\\)?\\(www\\.\\)?github\\.com" nerd-icons-octicon "mark-github")
    ("^\\(https?://\\)?\\(www\\.\\)?gitlab\\.com" nerd-icons-faicon "gitlab")
    ("^\\(https?://\\)?\\(www\\.\\)?news\\.ycombinator\\.com" nerd-icons-faicon "hacker-news")
    ("^\\(https?://\\)?\\(www\\.\\)?jsfiddle\\.net" nerd-icons-faicon "jsfiddle")
    ("^\\(https?://\\)?\\(www\\.\\)?maxcdn\\.com" nerd-icons-faicon "maxcdn")
    ("^\\(https?://\\)?\\(www\\.\\)?stackoverflow\\.com" nerd-icons-faicon "stack-overflow")
    ;; Video
    ("^\\(https?://\\)?\\(www\\.\\)?twitch\\.tv" nerd-icons-faicon "twitch")
    ("^\\(https?://\\)?\\(www\\.\\)?vimeo\\.com" nerd-icons-faicon "vimeo")
    ("^\\(https?://\\)?\\(www\\.\\)?youtube\\.com" nerd-icons-faicon "youtube")
    ("^\\(https?://\\)?\\(www\\.\\)?youtu\\.be" nerd-icons-faicon "youtube")
    ("^\\(https?://\\)?\\(www\\.\\)?vine\\.co" nerd-icons-faicon "vine")
    ;; Sound
    ("^\\(https?://\\)?\\(www\\.\\)?last\\.fm" nerd-icons-faicon "lastfm")
    ("^\\(https?://\\)?\\(www\\.\\)?mixcloud\\.com" nerd-icons-faicon "mixcloud")
    ("^\\(https?://\\)?\\(www\\.\\)?soundcloud\\.com" nerd-icons-faicon "soundcloud")
    ("spotify\\.com" nerd-icons-faicon "spotify")
    ;; Shopping
    ("^\\(https?://\\)?\\(www\\.\\)?amazon\\." nerd-icons-faicon "amazon")
    ("^\\(https?://\\)?\\(www\\.\\)?opencart\\.com" nerd-icons-faicon "opencart")
    ("^\\(https?://\\)?\\(www\\.\\)?paypal\\.com" nerd-icons-faicon "paypal")
    ("^\\(https?://\\)?\\(www\\.\\)?shirtsinbulk\\.com" nerd-icons-faicon "shitsinbulk")
    ;; Images
    ("^\\(https?://\\)?\\(www\\.\\)?500px\\.com" nerd-icons-faicon "500px")
    ("^\\(https?://\\)?\\(www\\.\\)?deviantart\\.com" nerd-icons-faicon "deviantart")
    ("^\\(https?://\\)?\\(www\\.\\)?flickr\\.com" nerd-icons-faicon "flickr")
    ("^\\(https?://\\)?\\(www\\.\\)?instagram\\.com" nerd-icons-faicon "instagram")
    ("^\\(https?://\\)?\\(www\\.\\)?pinterest\\." nerd-icons-faicon "pinterest")
    ;; Information and books
    ("^\\(https?://\\)?\\(www\\.\\)?digg\\.com" nerd-icons-faicon "digg")
    ("^\\(https?://\\)?\\(www\\.\\)?foursquare\\.com" nerd-icons-faicon "foursquare")
    ("^\\(https?://\\)?\\(www\\.\\)?getpocket\\.com" nerd-icons-faicon "get-pocket")
    ("^\\(https?://\\)?\\(www\\.\\)?scribd\\.com" nerd-icons-faicon "scribd")
    ("^\\(https?://\\)?\\(www\\.\\)?slideshare\\.net" nerd-icons-faicon "slideshare")
    ("stackexchange\\.com" nerd-icons-faicon "stack-exchange")
    ("^\\(https?://\\)?\\(www\\.\\)?stumbleupon\\.com" nerd-icons-faicon "stumbleupon")
    ("^\\(https?://\\)?\\(www\\.\\)?tripadvisor\\." nerd-icons-faicon "tripadvisor")
    ("^\\(https?://\\)?\\(www\\.\\)?yelp\\." nerd-icons-faicon "yelp")

    ("wikipedia\\.org" nerd-icons-faicon "wikipedia-w")
    ;; Various companies and tools
    ("^\\(https?://\\)?\\(www\\.\\)?angel\\.co" nerd-icons-faicon "angellist")
    ("^\\(https?://\\)?\\(www\\.\\)?apple\\.com" nerd-icons-faicon "apple")
    ("^\\(https?://\\)?\\(www\\.\\)?buysellads\\.com" nerd-icons-faicon "buysellads")
    ("^\\(https?://\\)?\\(www\\.\\)?connectdevelop\\.com" nerd-icons-faicon "connectdevelop")
    ("^\\(https?://\\)?\\(www\\.\\)?dashcube\\.com" nerd-icons-faicon "dashcube")
    ("^\\(https?://\\)?\\(www\\.\\)?dropbox\\.com" nerd-icons-faicon "dropbox")
    ("^\\(https?://\\)?\\(www\\.\\)?enviragallery\\.com" nerd-icons-faicon "envira")
    ("^\\(https?://\\)?\\(www\\.\\)?fortawesome\\.com" nerd-icons-faicon "fort-awesome")
    ("^\\(https?://\\)?\\(www\\.\\)?forumbee\\.com" nerd-icons-faicon "forumbee")
    ("^\\(https?://\\)?\\(www\\.\\)?gratipay\\.com" nerd-icons-faicon "gratipay")
    ("^\\(https?://\\)?\\(www\\.\\)?modx\\.com" nerd-icons-faicon "modx")
    ("^\\(https?://\\)?\\(www\\.\\)?pagelines\\.com" nerd-icons-faicon "pagelines")
    ("^\\(https?://\\)?\\(www\\.\\)?producthunt\\.com" nerd-icons-faicon "product-hunt")
    ("sellsy\\.com" nerd-icons-faicon "sellsy")
    ("^\\(https?://\\)?\\(www\\.\\)?simplybuilt\\.com" nerd-icons-faicon "simplybuilt")
    ("^\\(https?://\\)?\\(www\\.\\)?skyatlas\\.com" nerd-icons-faicon "skyatlas")
    ("^\\(https?://\\)?\\(www\\.\\)?skype\\.com" nerd-icons-faicon "skype")
    ("steampowered\\.com" nerd-icons-faicon "steam")
    ("^\\(https?://\\)?\\(www\\.\\)?themeisle\\.com" nerd-icons-faicon "themeisle")
    ("^\\(https?://\\)?\\(www\\.\\)?trello\\.com" nerd-icons-faicon "trello")
    ("^\\(https?://\\)?\\(www\\.\\)?whatsapp\\.com" nerd-icons-faicon "whatsapp")
    ("^\\(https?://\\)?\\(www\\.\\)?ycombinator\\.com" nerd-icons-faicon "y-combinator")
    ("yahoo\\.com" nerd-icons-faicon "yahoo")
    ("^\\(https?://\\)?\\(www\\.\\)?yoast\\.com" nerd-icons-faicon "yoast")
    ;; Catch all
    ("android" nerd-icons-faicon "android")
    ("creativecommons" nerd-icons-faicon "creative-commons")
    ("forums?" nerd-icons-octicon "comment-discussion")
    ("\\.pdf$" nerd-icons-octicon "file-pdf" :v-adjust 0.0 :face nerd-icons-dred)
    ("google" nerd-icons-faicon "google")
    ("\\.rss" nerd-icons-faicon "rss")
    ))

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

(defun nerd-icons-dir-is-submodule (dir)
  "Checker whether or not DIR is a git submodule."
  (let* ((gitmodule-dir (locate-dominating-file dir ".gitmodules"))
         (modules-file  (expand-file-name (format "%s.gitmodules" gitmodule-dir)))
         (module-search (format "submodule \".*?%s\"" (file-name-base dir))))

    (when (and gitmodule-dir (file-exists-p (format "%s/.git" dir)))
      (with-temp-buffer
        (insert-file-contents modules-file)
        (search-forward-regexp module-search (point-max) t)))))

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
(defun nerd-icons--icon-info-for-buffer (&optional f)
  "Get icon info for the current buffer.

When F is provided, the info function is calculated with the format
`nerd-icons-icon-%s-for-file' or `nerd-icons-icon-%s-for-mode'."
  (let* ((base-f (concat "nerd-icons-icon" (when f (format "-%s" f))))
         (file-f (intern (concat base-f "-for-file")))
         (mode-f (intern (concat base-f "-for-mode"))))
    (if (and (buffer-file-name)
             (nerd-icons-auto-mode-match?))
        (funcall file-f (file-name-nondirectory (buffer-file-name)))
      (funcall mode-f major-mode))))

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

;;;###autoload
(defun nerd-icons-icon-for-mode (mode &rest arg-overrides)
  "Get the formatted icon for MODE.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions."
  (let* ((icon (cdr (or (assoc mode nerd-icons-mode-icon-spec)
                        (assoc (get mode 'derived-mode-parent) nerd-icons-mode-icon-spec))))
         (args (cdr icon)))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (if icon (apply (car icon) args) mode)))

;;;###autoload
(defun nerd-icons-icon-for-buffer ()
  "Get the formatted icon for the current buffer.

This function prioritises the use of the buffers file extension to
discern the icon when its `major-mode' matches its auto mode,
otherwise it will use the buffers `major-mode' to decide its
icon."
  (nerd-icons--icon-info-for-buffer))

;;;###autoload
(defun nerd-icons-icon-for-dir (dir &optional chevron padding)
  "Format an icon for DIR with CHEVRON similar to tree based directories.

If PADDING is provided, it will prepend and separate the chevron
and directory with PADDING.

Produces different symbols by inspecting DIR to distinguish
symlinks and git repositories which do not depend on the
directory contents"
  (let* ((matcher (nerd-icons-match-to-alist (file-name-base (directory-file-name dir)) nerd-icons-dir-icon-spec))
         (path (expand-file-name dir))
         (chevron (if chevron (nerd-icons-octicon (format "chevron-%s" chevron) :height 0.8 :v-adjust -0.1) ""))
         (padding (or padding "\t"))
         (icon (cond
                ((file-symlink-p path)
                 (nerd-icons-octicon "file-symlink-directory" :height 1.0))
                ((nerd-icons-dir-is-submodule path)
                 (nerd-icons-octicon "file-submodule" :height 1.0))
                ((file-exists-p (format "%s/.git" path))
                 (format "%s" (nerd-icons-octicon "repo" :height 1.1)))
                (t (apply (car matcher) (cdr matcher))))))
    (format "%s%s%s%s%s" padding chevron padding icon padding)))

;;;###autoload
(defun nerd-icons-icon-for-url (url &rest arg-overrides)
  "Get the formatted icon for URL.
If an icon for URL isn't found in `nerd-icons-url-spec', a globe is used.
ARG-OVERRIDES should be a plist containining `:height',
`:v-adjust' or `:face' properties like in the normal icon
inserting functions."
  (let* ((icon (nerd-icons-match-to-alist url nerd-icons-url-spec))
         (args (cdr icon)))
    (unless icon
      (setq icon '(nerd-icons-faicon "globe"))
      (setq args (cdr icon)))
    (when arg-overrides (setq args (append `(,(car args)) arg-overrides (cdr args))))
    (apply (car icon) args)))

;;;###autoload
(defun nerd-icons-icon-for-weather (weather)
  "Get an icon for a WEATHER status."
  (let ((icon (nerd-icons-match-to-alist weather nerd-icons-weather-icon-spec)))
    (if icon (apply (car icon) (cdr icon)) weather)))

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
