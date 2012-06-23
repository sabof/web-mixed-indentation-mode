(require 'cl)
(require 'cc-mode)
(require 'nxml-mode)
(require 'css-mode)
(require 'js)

;;;; VARIABLES

;;; Configurable variables
(defvar wmi-use-tabs nil)
(defvar wmi-alien-js-mode 'js-mode
  "Mode to use when indenting javascript")
(defvar wmi-ridiculous-indentation 80
  "Whenver indentations gets past this point, delete all indentation")

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
(defvar wmi-debugging-enabled t)
(defvar wmi-opt:reuse-buffers t)
(defvar wmi-opt:shorten-text t)
(defvar wmi-opt:reuse-text t)

(defmacro WMI-DEBUG (form &optional disable)
  (if (and wmi-debugging-enabled
           (not disable))
      form
      nil))

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
      ;; (message "open: %s close: %s" open-tag close-tag)
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
    ;; All per-mode indentation-related
    ;; customizations go here
    (setq indent-tabs-mode nil)
    (cond ((eq mode 'nxml-mode)
           (rng-validate-mode -1))
          ((eq mode 'c-mode)
           (when (fboundp 'ywb-php-lineup-arglist-intro)
             (c-set-offset 'arglist-intro 'ywb-php-lineup-arglist-intro))
           (when (fboundp 'ywb-php-lineup-arglist-close)
             (c-set-offset 'arglist-close 'ywb-php-lineup-arglist-close))))

    (font-lock-mode -1))
  (setq buffer-offer-save nil))

;;; Indentation sequence setups

(defun wmi-prepare-php-sequence ()
  (flet ((l:delete-first-comment-end-maybe ()
           (goto-char (point-min))
           (let ((first-open (save-excursion
                               (when (search-forward "/*" nil t)
                                 (point))))
                 (first-closed (save-excursion
                                 (when (search-forward "*/" nil t)
                                   (point)))))
             (when (or (and first-closed
                            (not first-open))
                       (and first-closed first-open
                            (< first-closed first-open)))
               (wmi-replace-regexp "." " "
                                   (point-min) first-closed)))))
    (save-excursion
      (wmi-replace "<?php" "   */")
      (wmi-replace "<?" "*/")
      (wmi-replace "?>" "/*")
      (l:delete-first-comment-end-maybe)
      (setq wmi-alien-offset 0))))

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

(defun wmi-setup-alien-indent-sequence (mode)
  (cond ((eq mode 'c-mode)
         (wmi-prepare-php-sequence))
        ((eq mode 'css-mode)
         (wmi-prepare-css-sequence))
        ((eq mode 'nxml-mode)
         (wmi-prepare-nxml-sequence))
        ((eq mode wmi-alien-js-mode)
         (wmi-prepare-javascript-sequence))
        (t (setq wmi-alien-offset 0))))

(defun wmi-indent-inside-alien ()
  ;; (indent-line-to 0)
  (when (wmi-line-matches-p "^/[/\\*]")
    (indent-line-to 1))
  (indent-according-to-mode)
  (when (and wmi-ridiculous-indentation
             (> (current-indentation)
                wmi-ridiculous-indentation))
    (indent-line-to 0))
  (current-indentation))

(defun wmi-current-indentation ()
  "Like (current-indentation), but counts <tabs> as single characters"
  (save-excursion
    (back-to-indentation)
    (- (point) (line-beginning-position))))

(defun wmi-create-alien (mode)
  (let ((alien-buffer-name (concat " alien-" (symbol-name mode)))
        (default-directory "~/"))
    (when (get-buffer alien-buffer-name)
      (kill-buffer alien-buffer-name))
    (set-buffer (save-window-excursion
                  (switch-to-buffer alien-buffer-name)
                  (current-buffer)))
    (wmi-setup-alien mode)))

(defun wmi-secure-buffer (mode)
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
        (condition-case error-message
            (progn (wmi-secure-buffer mode)
                   (l:setup-buffer)
                   (condition-case error-message2
                       (setq result (wmi-indent-inside-alien))
                     (error (wmi-create-alien mode)
                            (l:setup-buffer)
                            (setq result (wmi-indent-inside-alien)))))
          (error (WMI-DEBUG (message "%s" error-message))))
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
                                       (when (re-search-backward "^[ \t]*<\\?" nil t)
                                         (current-indentation)))))
                    (if php-opening
                        (progn
                          (indent-line-to php-opening)
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

(defun wmi-enable-php-mixed-indentation ()
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
  (setq indent-tabs-mode nil)
  ;; Make sure all modes have been initialised
  (flet ((wmi-enable-php-mixed-indentation ()))
    (dolist (mode (list 'c-mode wmi-alien-js-mode 'css-mode 'nxml-mode))
      (with-temp-buffer (funcall mode)))))

(defun wmi-disable-php-mixed-indentation ()
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
  (when wmi-use-tabs
    (tabify start (+ end wmi-character-count-difference)))
  (message "WMI: Indentation complete")
  )

(define-minor-mode web-mixed-indentation-mode ()
  :lighter " W"
  (if web-mixed-indentation-mode
      (wmi-enable-php-mixed-indentation)
      (wmi-disable-php-mixed-indentation)))

(defalias 'wmi 'web-mixed-indentation-mode)
(defvaralias 'wmi 'web-mixed-indentation-mode)

(provide 'wmi)