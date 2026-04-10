;;; ob-futur.el --- Async elisp execution via futur for Org Babel -*- lexical-binding: t -*-

;; Author: Gino Cornejo <gggion123@gmail.com>
;; Maintainer: Gino Cornejo <gggion123@gmail.com>
;; URL: https://github.com/gggion/futurama.el
;; Keywords: processes, async, org

;; Package-Version: 0.0.1
;; Package-Requires: ((emacs "30.1") (futur "1.2") (org "9.7"))

;;; Commentary:

;; Adds :futur-async header argument to elisp/emacs-lisp source blocks.
;;
;; Two modes of operation:
;;
;; :futur-async yes - Run the body in a batch Emacs subprocess via
;; `futur-elisp--funcall'.  The body cannot access the current Emacs
;; state (buffers, windows, markers) but runs fully non-blocking.
;; Return values must be print/read round-trippable.
;;
;;     #+begin_src elisp :futur-async yes
;;     (cl-loop for n from 2 count (cl-every (lambda (d) (/= 0 (% n d)))
;;                                           (number-sequence 2 (cl-isqrt n)))
;;              into primes until (= primes 5000) finally return n)
;;     #+end_src
;;
;; :futur-async bind - Evaluate the body in the current Emacs, expect
;; a futur object as return value, resolve it via `futur-bind'.
;; For code that uses `futur-shell-command-to-string',
;; `futur-process-call', or other futur constructors.
;;
;;     #+begin_src elisp :futur-async bind
;;     (futur-shell-command-to-string "hostname")
;;     #+end_src

;;; Code:

(require 'ob-emacs-lisp)
(require 'futur)
(require 'futur-elisp)

(defconst ob-futur--placeholder "Executing asynchronously..."
  "Placeholder text inserted while a futur is resolving.")

(defun ob-futur--execute:emacs-lisp (orig-fn body params)
  "Around advice for `org-babel-execute:emacs-lisp'.
Dispatches on the value of :futur-async in PARAMS:

- \"yes\" or t: wrap body in `futur-elisp--funcall' (subprocess).
- \"bind\": eval body in current Emacs, resolve futur via `futur-bind'.
- Absent: delegate to ORIG-FN unchanged.

BODY is the source block text.
PARAMS is the header argument alist.

Incompatible with :results output, signals `user-error'.

Also see `ob-futur-mode' to enable this advice."
  (let ((async-val (cdr (assq :futur-async params))))
    (pcase async-val
      ((or "yes" "t" (pred (eq t)))
       (ob-futur--execute-subprocess body params))
      ("bind"
       (ob-futur--execute-bind body params))
      (_
       (funcall orig-fn body params)))))

(defun ob-futur--execute-subprocess (body params)
  "Execute BODY in a batch Emacs subprocess via `futur-elisp--funcall'.
BODY cannot access the current Emacs state.  Return values must be
print/read round-trippable.

PARAMS is the header argument alist.

Called by `ob-futur--execute:emacs-lisp' for :futur-async yes.
Uses `futur-elisp--funcall' for subprocess execution."
  (when (member "output" (cdr (assq :result-params params)))
    (user-error ":futur-async yes is incompatible with :results output"))
  (let* ((result-params (cdr (assq :result-params params)))
         (expanded (org-babel-expand-body:emacs-lisp body params))
         (src-body (format "(progn %s\n)" expanded))
         (src-buf (current-buffer))
         (src-point (point))
         (fut (futur-elisp--funcall
               (lambda ()
                 (eval (read src-body) t)))))
    (ob-futur--bind-and-insert fut src-buf src-point params result-params)
    ob-futur--placeholder))

(defun ob-futur--execute-bind (body params)
  "Evaluate BODY in current Emacs, expect a futur, resolve via `futur-bind'.
BODY should return a futur object from `futur-shell-command-to-string',
`futur-process-call', or similar constructors.

PARAMS is the header argument alist.

Called by `ob-futur--execute:emacs-lisp' for :futur-async bind.
Falls back to synchronous execution if BODY does not return a futur."
  (when (member "output" (cdr (assq :result-params params)))
    (user-error ":futur-async bind is incompatible with :results output"))
  (let* ((lexical (cdr (assq :lexical params)))
         (result-params (cdr (assq :result-params params)))
         (expanded (org-babel-expand-body:emacs-lisp body params))
         (src-body (format "(progn %s\n)" expanded))
         (result (eval (read src-body)
                       (org-babel-emacs-lisp-lexical lexical))))
    (if (not (futur-p result))
        ;; Not a futur
        ;; format synchronously like stock ob-emacs-lisp.
        (org-babel-result-cond result-params
          (let ((print-level nil)
                (print-length nil))
            (if (or (member "scalar" result-params)
                    (member "verbatim" result-params))
                (format "%S" result)
              (format "%s" result)))
          (org-babel-reassemble-table
           result
           (org-babel-pick-name (cdr (assq :colname-names params))
                                (cdr (assq :colnames params)))
           (org-babel-pick-name (cdr (assq :rowname-names params))
                                (cdr (assq :rownames params)))))
      (let ((src-buf (current-buffer))
            (src-point (point)))
        (ob-futur--bind-and-insert result src-buf src-point
                                    params result-params)
        ob-futur--placeholder))))

(defun ob-futur--bind-and-insert (fut buffer point params result-params)
  "Register `futur-bind' on FUT to insert result at POINT in BUFFER.
PARAMS and RESULT-PARAMS control formatting.

Called by `ob-futur--execute-subprocess' and `ob-futur--execute-bind'."
  (futur-bind
   fut
   (lambda (val)
     (ob-futur--insert-result buffer point val params result-params))
   (lambda (err)
     (ob-futur--insert-result
      buffer point
      (format "Error: %s" (error-message-string err))
      params result-params))))

(defun ob-futur--insert-result (buffer point value params result-params)
  "Replace the current result at POINT in BUFFER with VALUE.
PARAMS and RESULT-PARAMS control formatting via `org-babel-result-cond'.

Called from `futur-bind' callback when the futur resolves.

Also see `ob-futur--bind-and-insert' which sets up the callback."
  (when (buffer-live-p buffer)
    (with-current-buffer buffer
      (save-excursion
        (goto-char (min point (point-max)))
        (when (org-babel-where-is-src-block-head)
          (goto-char (org-babel-where-is-src-block-head))
          (let ((formatted
                 (org-babel-result-cond result-params
                   (let ((print-level nil)
                         (print-length nil))
                     (if (or (member "scalar" result-params)
                             (member "verbatim" result-params))
                         (format "%S" value)
                       (format "%s" value)))
                   (org-babel-reassemble-table
                    value
                    (org-babel-pick-name
                     (cdr (assq :colname-names params))
                     (cdr (assq :colnames params)))
                    (org-babel-pick-name
                     (cdr (assq :rowname-names params))
                     (cdr (assq :rownames params)))))))
            (org-babel-remove-result)
            (org-babel-insert-result formatted result-params)))))))

;;;###autoload
(define-minor-mode ob-futur-mode
  "Enable :futur-async header argument for elisp source blocks.
Two modes:

- :futur-async yes  - run body in subprocess, fully non-blocking.
- :futur-async bind - eval body in current Emacs, resolve futur result.

A placeholder is shown while the futur is pending.

Advises `org-babel-execute:emacs-lisp'.

Disable with \\[ob-futur-mode] to remove the advice."
  :global t
  :group 'org-babel
  (if ob-futur-mode
      (advice-add 'org-babel-execute:emacs-lisp
                  :around #'ob-futur--execute:emacs-lisp)
    (advice-remove 'org-babel-execute:emacs-lisp
                   #'ob-futur--execute:emacs-lisp)))

(provide 'ob-futur)
;;; ob-futur.el ends here
