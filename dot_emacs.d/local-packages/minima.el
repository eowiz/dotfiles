;;; minima --- Minimal Emacs package manager -*- lexical-binding: t; -*-

;; Author: eowiz <eowiz@proton.me>
;; Maintaier: eowiz <eowiz@proton.me>
;; Created:
;; Version: 0.0.1
;; Package-Requires: ((emacs "29.0"))
;; Keywords: dotemacs package
;; URL: https://github.com/eowiz/minima

;; This program is free software: you can redistribute it and/or modify
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

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(require 'eshell)

(defgroup minima nil
  "A minimal package support your `.emacs' configuration."
  :group 'lisp)

(defcustom minima-root-dir (locate-user-emacs-file "el-clone")
  "Set minima root directory."
  :group 'minima
  :type 'string)

(defcustom minima-git-clone-command '("git" "clone" "--recursive" "--depth" "1")
  "Set a command to clone a git repository."
  :group 'minima
  :type '(list string))

(defun minima--create-directory ()
  ""
  (unless (file-directory-p minima-root-dir)
    (make-directory minima-root-dir)))

(defcustom minima--buffer-name "*minima*"
  ""
  :group 'minima
  :type 'string)

(defun minima--git-clone-command (repo-url package-name)
  ""
  (append minima-git-clone-command `(,repo-url ,package-name)))

(with-current-buffer (get-buffer-create "*minima*")
  (set (make-local-variable 'eshell-last-input-start) (point-marker))
  (set (make-local-variable 'eshell-last-input-end) (point-marker))
  (set (make-local-variable 'eshell-last-output-start) (point-marker))
  (set (make-local-variable 'eshell-last-output-end) (point-marker))
  (set (make-local-variable 'eshell-last-output-block-begin) (point)))

(defmacro minima--make-process (&key name command)
  ""
  (make-process
   :name name
   :buffer (get-buffer-create minima--buffer-name)
   :command command
   :filter (lambda (proc string)
	     (eshell-output-filter proc string))))

(defmacro minima--make-clone-process (repo-url package-name)
  ""
  (make-process package-name (minima--git-clone-command repo-url package-name)))

(defun minima--clone (repo-url package-name)
  ""
  (shell-command-to-string
   (mapconcat #'shell-quote-argument
	      (minima--git-clone-command repo-url package-name)
	      " ")))

(cl-defun minima--repo-url (&key (fetcher "github") url repo)
  ""
  (or url (format "https://www.%s.com/%s.git" fetcher repo)))

(cl-defun minima--package-name (&key repo name)
  ""
  (or name (file-name-base repo)))

(cl-defun minima-clone (&key (fetcher "github") url repo name)
  ""
  (minima--create-directory)
  
  (let* ((repo-url (or url (format "https://www.%s.com/%s.git" fetcher repo)))
	 (package-name (or name (file-name-base repo)))
	 (default-directory minima-root-dir))
    (unless (file-directory-p (expand-file-name package-name minima-root-dir))
      (message (concat "Install " repo-url "..."))
      (minima--clone repo-url package-name))

    (add-to-list 'load-path (concat minima-root-dir package-name))))

(cl-defmacro minima (&key clone
			  (priority 'high)
			  (disable nil))
  ""
  (if disable
      nil
    `(eval-when-compile
       ,@(minima-clone :repo clone))

    (let ((add-load-path-sexp `(add-to-list 'load-path ,(concat minima-root-dir "/" (minima--package-name :repo clone)))))
      (cond ((eq priority 'high) `(with-delayed-execution-priority-high ,add-load-path-sexp))
	    (t                   `(with-delayed-execution ,add-load-path-sexp)))))
  )

(provide 'minima)

;;; minima.el ends here
