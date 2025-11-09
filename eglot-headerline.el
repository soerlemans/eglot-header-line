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
;; Symbol kind specification as of writing.
;; See symbol kind at:
;; https://microsoft.github.io/language-server-protocol/specifications/lsp/3.17/specification/#textDocument_documentSymbol).
;;
;; export namespace SymbolKind {
;;   export const File = 1;
;;	 export const Module = 2;
;;	 export const Namespace = 3;
;;	 export const Package = 4;
;;	 export const Class = 5;
;;	 export const Method = 6;
;;	 export const Property = 7;
;;	 export const Field = 8;
;;	 export const Constructor = 9;
;;	 export const Enum = 10;
;;	 export const Interface = 11;
;;	 export const Function = 12;
;;	 export const Variable = 13;
;;	 export const Constant = 14;
;;	 export const String = 15;
;;	 export const Number = 16;
;;	 export const Boolean = 17;
;;	 export const Array = 18;
;;	 export const Object = 19;
;;	 export const Key = 20;
;;	 export const Null = 21;
;;	 export const EnumMember = 22;
;;	 export const Struct = 23;
;;	 export const Event = 24;
;;	 export const Operator = 25;
;;	 export const TypeParameter = 26;|
;; }
(unless (boundp '+symbol-kind-init+)
	;; Enum spec.
	(defconst +symbol-kind-namespace+ 3)
	(defconst +symbol-kind-class+ 5)
	(defconst +symbol-kind-method+ 6)
	(defconst +symbol-kind-enum+ 10)
	(defconst +symbol-kind-interface+ 11)
	(defconst +symbol-kind-function+ 12)
	(defconst +symbol-kind-struct+ 23)

	(defconst +symbol-kind-init+ t))

(defun eglot-headerline--symbol-kind-to-face (kind)
	"Match a given symbol kind to a font lock face."
	(interactive)
	(cl-case kind
		(+symbol-kind-namespace+ 'font-lock-constant-face)
		(+symbol-kind-class+ 'font-lock-type-face)
		(+symbol-kind-method+ 'font-lock-function-name-face)
		(+symbol-kind-enum+ 'font-lock-type-face)
		(+symbol-kind-interface+ 'font-lock-type-face)
		(+symbol-kind-function 'font-lock-function-name-face)
		(+symbol-kind-struct+ 'font-lock-type-face)

		;; (t 'font-lock-keyword-face) ; Default
		(t 'default) ; Default
		))

(defun eglot-headerline--swap-face (face)
  "Return a face spec like FACE but with foreground and background swapped."
	(interactive)
  (let ((fg (face-foreground face nil 'default))
        (bg (face-background face nil 'default)))
    ;; `(:foreground ,bg :background ,fg :inherit ,face) ; This breaks it but is the way we should approach this.
    `(:foreground ,bg :background ,fg)
		))

(defun eglot-headerline--propertize (str kind)
	"TODO: Document."
	(interactive)
	(let* ((face (eglot-headerline--symbol-kind-to-face kind))
				 (hl-face (eglot-headerline--swap-face face)))

		(message "kind: %S" (eglot-headerline--symbol-kind-to-face kind))

		(propertize str 'face hl-face)
		))

;;; Functions:
(defun eglot-headerline--documentSymbol ()
  "Return the list of symbols from the current buffer via Eglot."
	(interactive)
	(let ((server (eglot--current-server-or-lose)))
		(eglot--request
		 server
		 "textDocument/documentSymbol"
		 `(:textDocument ,(eglot--TextDocumentIdentifier)))
		))


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
													(detail (plist-get symbol :detail))
													(kind (plist-get symbol :kind)))
										 (when (and (>= (point) start) (<= (point) end))
											 ;; Add the symbols name if our point is between its start and end.
											 (let* ((face (eglot-headerline--symbol-kind-to-face kind))
															(name-prop (eglot-headerline--propertize name kind))
															(sep-prop (eglot-headerline--propertize "::" t))
															;; (detail-prop (eglot-headerline--propertize detail t))
															(spacer-prop (eglot-headerline--propertize " # " t)))
												 (push name-prop path)
												 (if children
														 (progn ; True.
															 (push sep-prop path)
															 (walk children))
													 (when-let ((detail-prop (eglot-headerline--propertize detail t))) ; False.
														 (push spacer-prop path)
														 (push detail path))
													 )))
										 ))
								 symbols-inner)))
			(walk symbols))
		(nreverse path)))

(defun eglot-headerline--breadcrumb ()
	"Compute the breadcrumb for the current context, of POINT."
	(interactive)
	(when-let ((symbols (eglot-headerline--documentSymbol)))
		(let* ((path (eglot-headerline--symbol-at-point symbols)))
			path)
		))

(defun eglot-headerline--hook ()
	"Hooking function to add and remove.."
	(setq-local header-line-format (eglot-headerline--breadcrumb)))

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
