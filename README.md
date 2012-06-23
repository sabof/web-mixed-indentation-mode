web-mixed-indentation-mode
==========================

A minor mode for automatic indentation of PHP/XHTML/JavaScript/CSS soup.

It's unassuming - it only depends on packages present in standard emacs. It doesn't even need PHP mode.
It can be used with any major mode whatsoever - the only thing it handles is indentation.

To use it add

(require 'wmi)
(add-hook 'php-mode-hook  (lambda () (wmi 1)))
(add-hook 'css-mode-hook  (lambda () (wmi 1)))
(add-hook 'nxml-mode-hook (lambda () (wmi 1)))
(add-hook 'js-mode-hook   (lambda () (wmi 1)))

to your .emacs