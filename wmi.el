;;;;
;;;; WEB MIXED INDENTATION MINOR MODE
;;;;

(require 'cl)
(require 'cc-mode)
(require 'nxml-mode)
(require 'css-mode)
(require 'js)

;;;; VARIABLES

;;; Configurable variables
(defvar wmi-alien-js-mode 'js-mode
  "Mode to use when indenting javascript")
(defvar wmi-ridiculous-indentation 80
  "Whenver indentations gets past this point, delete all indentation")
(defvar wmi-mode-customization-function nil
  "A hook run instead of standard hooks, for major modes used for indentation.
It takes one argument, the symbol of the mode which is going to be used.
PHP code is indented using c-mode, so this mode gets passed as argument.

Example setup:
 (add-hook 'wmi-mode-customization-function
           (lambda (mode)
             (case mode
               (c-mode (foo))
               (js-mode (foo))
               (css-mode (foo))
               (nxml-mode (foo)))))")

;;; Internal variables
(defvar wmi-original-indent-line-function 0)
(make-variable-buffer-local 'wmi-original-indent-line-function)
(defvar wmi-original-indent-region-function 0)
(make-variable-buffer-local 'wmi-original-indent-region-function)

(defvar wmi-inside-alien-sequence nil)
(defvar wmi-alien-offset 0)
(defvar wmi-character-count-difference 0)
(defvar wmi-previous-alien-mode nil)

;;; Debugging varables
(defvar wmi-debug-text-reuse-calls 0)
(defvar wmi-opt:reuse-buffers t)
(defvar wmi-opt:shorten-text t)
(defvar wmi-opt:reuse-text t)

(defmacro WMI-DEBUG (form &optional disable) nil)

(defun wmi-debugging-enable ()
  (defmacro WMI-DEBUG (form &optional disable)
    (unless disable form))
  (load load-file-name))

(defun wmi-debugging-disable ()
  (defmacro WMI-DEBUG (form &optional disable) nil)
  (load load-file-name))

;;; Utility functions
(defun wmi-replace-regexp (regexp replacement &optional from to)
  (save-excursion
    (goto-char (point-min))
    (save-restriction
      (when (and from to)
        (narrow-to-region from to))
      (while (re-search-forward regexp nil t)
        (replace-match replacement t nil)))))

(defun wmi-replace (original replacement &optional from to)
  (save-excursion
    (goto-char (point-min))
    (save-restriction
      (when (and from to)
        (narrow-to-region from to))
      (while (search-forward original nil t)
        (replace-match replacement t nil)))))

(defun wmi-line-matches-p (regexp)
  (string-match-p
   regexp
   (buffer-substring
    (line-beginning-position)
    (line-end-position))))

(defun wmi-current-indentation ()
  "Like (current-indentation), but counts tabs as single characters"
  (save-excursion
    (back-to-indentation)
    (- (point) (line-beginning-position))))

;;;

(defun* wmi-inside-html-tag-p (tag)
  (let* ((start-point (point))
         (open-tag (save-excursion
                     (when (search-backward
                            (concat "<" tag)
                            (point-min) t)
                       (search-forward ">")
                       (when (< start-point (point))
                         (return-from wmi-inside-html-tag-p nil))
                       (point))))
         (close-tag
          (save-excursion
            (when (search-backward (concat "</" tag ">")
                                   (point-min) t)
              (point)))))
    (and open-tag
         (or (not close-tag)
             (< close-tag open-tag)))))

(defun wmi-kill-c-alien ()
  ;; Unused
  (interactive)
  (let ((c-alien (get-buffer " alien-c-mode")))
    (when c-alien
      (set-buffer c-alien)
      (set-buffer-modified-p nil)
      (kill-buffer))))

(defun wmi-debug-reindent-buffer ()
  ;; Unused
  (interactive)
  (buffer-remove-indentation)
  (indent-region (point-min) (point-max)))

(defun wmi-inside-php-code-p ()
  (flet ((l:search-tag-backward (tag)
           (save-excursion
             (when (and (search-backward tag (point-min) t)
                        (not (memq (face-at-point)
                                   '(font-lock-comment-face
                                     font-lock-string-face))))
               (point)))))
    (let ((open-tag (l:search-tag-backward "<?"))
          (close-tag (l:search-tag-backward "?>")))
      (WMI-DEBUG (message "open: %s close: %s" open-tag close-tag))
      (and open-tag
           (or (not close-tag)
               (< close-tag open-tag))))))

(defun wmi-setup-alien (mode)
  (let* ((hook-symbol (intern (concat (symbol-name mode) "-hook")))
         (old-init-hook (when (boundp hook-symbol)
                          (symbol-value hook-symbol))))
    (when old-init-hook
      (set hook-symbol nil))
    (funcall mode)
    (when old-init-hook
      (set hook-symbol old-init-hook))
    ;;
    (visual-line-mode -1)
    (when (eq mode 'nxml-mode)
      (rng-validate-mode -1))
    (font-lock-mode -1)
    (setq buffer-offer-save nil)
    (run-hook-with-args 'wmi-mode-customization-function mode)
    ))

;;; Indentation sequence setups

(defun wmi-prepare-php-sequence ()
  (save-excursion
    ;; Remove first <?
    (goto-char (point-min))
    (search-forward "<?")
    (when (looking-at "php")
      (forward-char 3))
    (wmi-replace-regexp "." " " (point-min) (point))
    ;;
    (wmi-replace "<?php" "   */")
    (wmi-replace "<?" "*/")
    (wmi-replace "?>" "/*")
    (setq wmi-alien-offset 0)))

(defun wmi-prepare-javascript-sequence ()
  (let ((start (point)))
    (search-backward "<script")
    (search-forward ">")
    (setq wmi-alien-offset (1- (point)))
    (delete-region (point-min) (point))
    (goto-char (- start wmi-alien-offset))))

(defun wmi-prepare-css-sequence ()
  (let ((start (point)))
    (search-backward "<style")
    (search-forward ">")
    (setq wmi-alien-offset (1- (point)))
    (delete-region (point-min) (point))
    (goto-char (- start wmi-alien-offset))))

(defun wmi-prepare-nxml-sequence ()
  (save-excursion
    (wmi-replace-regexp "^\\(.*[^ \t\n].*\\)<\\?"
                        "\\1  ")
    (wmi-replace-regexp "\\?>\\(.*[^ \t\n].*\\)$"
                        "  \\1")
    (setq wmi-alien-offset 0)))

;;;

(defun wmi-setup-alien-indent-sequence (mode)
  (case mode
    (c-mode    (wmi-prepare-php-sequence))
    (css-mode  (wmi-prepare-css-sequence))
    (nxml-mode (wmi-prepare-nxml-sequence))
    (t (if (eq mode wmi-alien-js-mode)
           (wmi-prepare-javascript-sequence)
           (setq wmi-alien-offset 0)))))

(defun wmi-indent-inside-alien ()
  (when (wmi-line-matches-p "^/[/\\*]")
    (indent-line-to 1))
  (indent-according-to-mode)
  (when (and wmi-ridiculous-indentation
             (> (current-indentation)
                wmi-ridiculous-indentation))
    (indent-line-to 0))
  (current-indentation))

(defun wmi-create-alien (mode)
  (let ((alien-buffer-name (concat " alien-" (symbol-name mode)))
        (default-directory "~/"))
    (when (get-buffer alien-buffer-name)
      (kill-buffer alien-buffer-name))
    (set-buffer (save-window-excursion
                  (switch-to-buffer alien-buffer-name)
                  (current-buffer)))
    (wmi-setup-alien mode)))

(defun wmi-secure-alien (mode)
  (let* ((alien-buffer-name (concat " alien-" (symbol-name mode)))
         (reuse-buffer
          (and wmi-opt:reuse-buffers
               (get-buffer alien-buffer-name)
               ;; As of emacs 24, c-mode indentation
               ;; periodically needs restarting. I haven't
               ;; noticed anything similar in emacs 23
               (or (not (eq mode 'c-mode))
                   wmi-inside-alien-sequence))))
    (if reuse-buffer
        (set-buffer alien-buffer-name)
        (wmi-create-alien mode))))

(defun wmi-indent-inside-source (result)
  (when wmi-opt:shorten-text
    (let ((old-indentation (wmi-current-indentation)))
      (indent-line-to result)
      (incf wmi-character-count-difference
            (- (wmi-current-indentation)
               old-indentation))))
  (unless wmi-opt:shorten-text
    (indent-line-to result)))

(defun wmi-get-old-string (old-buffer mode limit)
  (WMI-DEBUG (message "getting old string"))
  (with-current-buffer old-buffer
    (let ((max (if (and wmi-opt:shorten-text
                        limit
                        (not (eq mode 'c-mode)))
                   (min limit (point-max))
                   (point-max))))
      (WMI-DEBUG (assert (or (not limit) (<= limit (point-max)))))
      (WMI-DEBUG (message "max: %s" max))
      (buffer-substring-no-properties (point-min) max))))

(defun wmi-alien-indent (mode &optional limit)
  (WMI-DEBUG (message "alien indent"))
  (let* ((old-buffer (current-buffer))
         (old-position (point))
         result)
    (flet ((c-before-change (&rest rest))
           (c-after-change (&rest rest)))
      (labels ((l:setup-buffer ()
                 (if (and wmi-opt:reuse-text
                          wmi-inside-alien-sequence
                          (eq wmi-previous-alien-mode mode))
                     (progn
                       (forward-line)
                       (WMI-DEBUG (message "text-reuse"))
                       (WMI-DEBUG (incf wmi-debug-text-reuse-calls)))
                     (progn
                       (delete-region (point-min) (point-max))
                       (insert (wmi-get-old-string old-buffer mode limit))
                       (goto-char old-position)
                       (wmi-setup-alien-indent-sequence mode)))))
        ;; When there is an error, create a new buffer and try again
        (condition-case error-message
            (progn (wmi-secure-alien mode)
                   (l:setup-buffer)
                   (condition-case error-message2
                       (setq result (wmi-indent-inside-alien))
                     (error (wmi-create-alien mode)
                            (l:setup-buffer)
                            (setq result (wmi-indent-inside-alien)))))
          (error (WMI-DEBUG (error "%s" error-message))))
        (when result
          (set-buffer old-buffer)
          (goto-char old-position)
          (wmi-indent-inside-source result))
        (setq wmi-previous-alien-mode mode)
        (WMI-DEBUG (message "Alien indent mode: %s, line: %s"
                            mode (line-number-at-pos))
                   ;; 'DISABLED
                   )))
    ))

(defun wmi-indent-line-internal (&optional limit)
  (save-excursion
    (beginning-of-line)
    (let ((mode
           (cond ((wmi-line-matches-p "^[ \t]*\\?>")
                  (let ((php-opening (save-excursion
                                       (when (re-search-backward "^[ \t]*<\\?"
                                                                 nil t)
                                         (current-indentation)))))
                    (if php-opening
                        (progn
                          (indent-line-to (+ 0 php-opening))
                          (setq wmi-previous-alien-mode nil)
                          nil)
                        'nxml-mode)))
                 ((wmi-line-matches-p "^[ \t]*<")
                  'nxml-mode)
                 ((wmi-inside-php-code-p)
                  'c-mode)
                 ((wmi-inside-html-tag-p "style")
                  'css-mode)
                 ((wmi-inside-html-tag-p "script")
                  wmi-alien-js-mode)
                 (t
                  'nxml-mode))))
      (when mode
        (wmi-alien-indent mode limit)))))

(defun wmi-enable ()
  "If you want to enable to enable wmi, write (wmi 1)"
  ;; Backups
  (make-variable-buffer-local 'indent-region-function)
  (unless (eq indent-line-function 'wmi-indent-line)
    (setq wmi-original-indent-line-function indent-line-function))
  (unless (eq indent-region-function 'wmi-indent-region)
    (setq wmi-original-indent-region-function indent-region-function))
  ;;
  (setq indent-line-function    'wmi-indent-line)
  (setq indent-region-function  'wmi-indent-region)
  (when (fboundp 'fai-mode)
    (setq after-change-indentation nil))
  ;; Make sure all modes have been initialised
  (flet ((wmi-enable ()))
    (dolist (mode (list 'c-mode wmi-alien-js-mode 'css-mode 'nxml-mode))
      (with-temp-buffer (funcall mode)))))

(defun wmi-disable ()
  "If you want to diable to enable wmi, write (wmi -1)"
  (unless (numberp wmi-original-indent-line-function)
    (setq indent-line-function wmi-original-indent-line-function))
  (unless (numberp wmi-original-indent-region-function)
    (setq indent-region-function wmi-original-indent-region-function))
  (setq after-change-indentation t))

;;; Interface
(defun wmi-indent-line ()
  (setq wmi-character-count-difference 0)
  (wmi-indent-line-internal (line-end-position))
  (goto-char (max (save-excursion (back-to-indentation)
                                  (point))
                  (point)))
  (setq deactivate-mark nil
        cua--explicit-region-start nil))

(defun wmi-indent-region (start end)
  (setq wmi-character-count-difference 0)
  (WMI-DEBUG (setq wmi-debug-text-reuse-calls 0))
  (let ((first-time t)
        (last-line (line-number-at-pos end)))
    (unwind-protect
         (save-excursion
           (goto-char start)
           (loop (if (and (<= (line-number-at-pos) last-line)
                          (not (= (point) (point-max))))
                     (progn (wmi-indent-line-internal
                             (+ end wmi-character-count-difference))
                            (forward-line))
                     (return))
                 (setq wmi-inside-alien-sequence t)))
      (setq wmi-inside-alien-sequence nil)))
  (message "WMI: Indentation complete")
  )

(define-minor-mode web-mixed-indentation-mode
    "A minor mode for indentation of files containing PHP, XHTML, CSS and JavaScript code. Being a minor mode, it can be used in conjunction with any major mode.It works by overriding the indentation function, figuring out the language, removing all the confusing text and indenting according to an appropriate major mode.

For reasons of efficency, modes providing indentation are initialized without running the standard hooks. However, a special hook is provided called wmi-mode-customization-function. You can read this varable's documentation to find out more.

The project is hosted at
http://github.com/sabof/web-mixed-indentation-mode

Example usage:
(require 'wmi)
(add-hook 'php-mode-hook  (lambda () (wmi 1)))
(add-hook 'css-mode-hook  (lambda () (wmi 1)))
(add-hook 'nxml-mode-hook (lambda () (wmi 1)))
(add-hook 'js-mode-hook   (lambda () (wmi 1)))"
  :lighter " W"
  (if web-mixed-indentation-mode
      (wmi-enable)
      (wmi-disable)))

(defalias 'wmi 'web-mixed-indentation-mode)
(defvaralias 'wmi 'web-mixed-indentation-mode)

(provide 'wmi)