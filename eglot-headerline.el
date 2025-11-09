;;; eglot-headerline.el --- Major mode for the Crow programming language.
;; -*- coding: utf-8; lexical-binding: t -*-

;; Copyright (C) 2025  soerlemans

;; Author: soerlemans <https://github.com/soerlemans>
;; Keywords: languages crow
;; URL: https://github.com/soerlemans/eglot-headerline
;;
;; This file is not part of GNU Emacs.

;; MIT License
;;
;; Copyright (c) 2025 soerlemans
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.

;;; Commentary:

;; TODO: Write Commentary

;;; Code:
(dolist (pkg '(eglot jsonrpc))
	(require pkg))

;;; Variables:

;;; Functions:
(defun eglot-headerline--documentSymbol ()
  "Return the list of symbols from the current buffer via Eglot."
	(interactive)
	(let ((server (eglot--current-server-or-lose)))
		(eglot--request
		 server
		 "textDocument/documentSymbol"
		 `(:textDocument ,(eglot--TextDocumentIdentifier))
		 ))
	)

(defun eglot-headerline--symbol-at-point (symbols)
	"Return list of symbol names containing point, using SYMBOLS tree."
	(interactive)
	(let (path)
		(cl-labels
				((walk (symbols-inner)
					 (mapc (lambda (symbol)
									 (let* ((range (plist-get symbol :range))
													(start (eglot--lsp-position-to-point (plist-get range :start)))
													(end   (eglot--lsp-position-to-point (plist-get range :end)))
													(name (plist-get symbol :name))
													(children (plist-get symbol :children))
													(detail (plist-get symbol :detail)))
										 (when (and (>= (point) start) (<= (point) end))
											 ;; Add the symbols name if our point is between its start and end.
											 (push name path)
											 (if children
													 (walk children)
												 (push detail path)
												 ))
										 ))
								 symbols-inner)))
			(walk symbols))
		(nreverse path)))

(defun eglot-headerline--breadcrumb ()
	"Compute the breadcrumb for the current context, of POINT."
	(interactive)
	(when-let ((symbols (eglot-headerline--documentSymbol)))
		(let* ((path (eglot-headerline--symbol-at-point symbols))
					 (crumbs (string-join path "::")))
			crumbs)
		))

(defun eglot-headerline--hook ()
	"Hooking function to add and remove.."
	;; (setq-local header-line-format (eglot-headerline--breadcrumb)))
	(setq-local header-line-format
							(propertize (eglot-headerline--breadcrumb)
													'face '(:foreground "black" :background "white")
													)))

(defun enable-eglot-headerline ()
	"Enable the eglot headerline."
	(interactive)
	(add-hook 'post-command-hook #'eglot-headerline--hook nil t))

(defun disable-eglot-headerline ()
	"Disable the eglot headerline."
	(interactive)
	(remove-hook 'post-command-hook #'eglot-headerline--hook t)
	(setq-local header-line-format '()))

;;; Define minor mode:
(define-minor-mode eglot-headerline-mode
	"Toggle the highlighting of the current namespace/class/function in the headerline."
	:lighter "Toggle the highlighting of the current function in the header-line."
	(if eglot-headerline-mode
			(enable-eglot-headerline)
		(disable-eglot-headerline)
		))

(provide 'eglot-headerline-mode)
;;; eglot-headerline.el ends here
