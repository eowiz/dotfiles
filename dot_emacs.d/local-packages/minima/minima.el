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

;; see: https://github.com/takeokunn/.emacs.d

;;; Code:

(eval-when-compile
  (require 'cl-lib))

(defgroup minima nil
  "A minimal package support your \='.emacs\=' configuration."
  :group 'lisp)

(defcustom minima-packages-dir (locate-user-emacs-file "minima")
  "Specify the directory for packages installed by minima."
  :group 'minima
  :type 'string)

(defcustom minima-git-clone-command '("git" "clone" "--recursive" "--depth" "1")
  "Configure the \='git clone\=' command options for minima."
  :group 'minima
  :type '(list string))

(defconst my/saved-file-name-handler-alist file-name-handler-alist)
(setq file-name-handler-alist nil)

(defvar my/delayed-priority-high-configurations '())
(defvar my/delayed-priority-high-configuration-timer nil)

(defvar my/delayed-priority-low-configurations '())
(defvar my/delayed-priority-low-configuration-timer nil)

(add-hook 'emacs-startup-hook
	  (lambda ()
            (setq my/delayed-priority-high-configuration-timer
                  (run-with-timer
                   0.1 0.001
                   (lambda ()
                     (if my/delayed-priority-high-configurations
                         (let ((inhibit-message t))
                           (eval (pop my/delayed-priority-high-configurations)))
                       (progn
                         (cancel-timer my/delayed-priority-high-configuration-timer))))))
            (setq my/delayed-priority-low-configuration-timer
                  (run-with-timer
                   0.3 0.001
                   (lambda ()
                     (if my/delayed-priority-low-configurations
                         (let ((inhibit-message t))
                           (eval (pop my/delayed-priority-low-configurations)))
                       (progn
                         (cancel-timer my/delayed-priority-low-configuration-timer))))))))

(defmacro with-delayed-execution-priority-high (&rest body)
    (declare (indent 0))
    `(setq my/delayed-priority-high-configurations
           (append my/delayed-priority-high-configurations ',body)))

(defmacro with-delayed-execution (&rest body)
  (declare (indent 0))
  `(setq my/delayed-priority-low-configurations
         (append my/delayed-priority-low-configurations ',body)))

;;;###autoload
(defun minima-locate (path)
  "Return the absolute path of PATH relative to the minima-packages-dir.
If PATH is a relative path, it is expanded to an absolute path
using minima-packages-dir as the base directory."
  (expand-file-name path minima-packages-dir))

(defun minima--create-dir ()
  "Create a directory for packages installed by minima if doesn't exist."
  (unless (file-directory-p minima-packages-dir)
    (make-directory minima-packages-dir)))

(cl-defun minima--repo-url (&key (fetcher "github") url repo)
  "Construct a repository URL based on the given FETCHER, URL, and REPO."
  (or url (format "https://www.%s.com/%s.git" fetcher repo)))

(cl-defun minima--package-name (&key repo name)
  "Determine the package name based on the given REPO name or the specified NAME."
  (or name (file-name-base repo)))

(defun minima--git-clone-command (repo-url package-name)
  "Generate a `git clone' command based on the given REPO-URL and PACKAGE-NAME."
  (append minima-git-clone-command `(,repo-url ,package-name)))

(defun minima--clone (repo-url package-name)
  "Run \='git clone\=' with given REPO-URL and PACKAGE-NAME."
  (shell-command-to-string
   (mapconcat #'shell-quote-argument
	      (minima--git-clone-command repo-url package-name)
	      " ")))

(cl-defun minima-clone (&key (fetcher "github") url repo name)
  "Clone a package, add it to \='load-path\='.
Use FETCHER, URL, REPO, and NAME as inputs."
  (minima--create-dir)
  
  (let* ((repo-url (or url (format "https://www.%s.com/%s.git" fetcher repo)))
	 (package-name (or name (file-name-base repo)))
	 (default-directory minima-packages-dir))
    (unless (file-directory-p (expand-file-name package-name minima-packages-dir))
      (message (concat "Install " repo-url "..."))
      (minima--clone repo-url package-name))))

(defun minima--add-load-path-sexp (path)
  "Generate an S-expression to add the located PATH to the \='load-path\='."
  `(add-to-list 'load-path ,path))

(defun minima--with-delayed (priority &rest body)
  "Execute BODY with delayed execution based on PRIORITY.
  
PRIORITY can be either \='high\=' or any other value,
which defaults to normal priority.
This is an internal utility function."
  (let ((with-delayed (cond ((eq priority 'high) 'with-delayed-execution-priority-high)
			    (t                   'with-delayed-execution))))
    (eval `(with-delayed-execution-priority-high ,@body))))

(cl-defmacro minima (&key clone
			  (priority 'high)
			  (path '())
			  (disable nil))
  ""
  `(eval-and-compile
     (eval-when-compile
       (unless ,disable
	 (minima-clone :repo ,clone)))

     ,@(let* ((package-name (minima--package-name :repo clone))
	     (add-load-path-sexp (minima--add-load-path-sexp (minima-locate package-name)))
	     (load-pathes (cl-mapcar #'minima--add-load-path-sexp (cl-mapcar #'minima-locate (cl-mapcar (lambda (p) (concat package-name "/" p)) path)))))
	 (minima--with-delayed priority (minima--add-load-path-sexp (minima-locate package-name)))))
  )

       ;; (let* ((package-name (minima--package-name :repo ,clone))
       ;; 	      (add-load-path-sexp (minima--add-load-path-sexp (minima-locate package-name)))
       ;; 	      (load-pathes (cl-mapcar #'minima--add-load-path-sexp (cl-mapcar #'minima-locate (cl-mapcar (lambda (p) (concat package-name "/" p)) ,path)))))
       ;; 	 (minima--with-delayed ',priority (minima--add-load-path-sexp (minima-locate package-name)))))

;; (minima :clone "emacs-compat/compat")

;;;###autoload
(defun minima-byte-compile ()
  "Byte-compile all \='.el\=' files in the \='minima-packages-dir\=' recursively."
  (interactive)
  (dolist (el (file-expand-wildcards (concat minima-packages-dir "/**/*.el")))
    (byte-compile-file el)))

;;;###autoload
(defun minima-native-compile ()
  "Asynchronously \='native-compile\=' \='minima-packages-dir\=' recursively.
 
This interactive function compiles all Emacs Lisp files
in the \='minima-packages-dir\=' directory."
  (interactive)
  (native-compile-async minima-packages-dir 'recursively))

(provide 'minima)

;;; minima.el ends here
