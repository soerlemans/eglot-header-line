# eglot-header-line
Add on for Emacs eglot showing breadcrumb information in the header-line.

![screenshot.png](https://github.com/soerlemans/eglot-header-line/raw/refs/heads/main/assets/screenshot.png)

## Install
Install and hook eglot-managed-mode for automatic activation.
Using `use-package` and `vc-package-install` intergration:

```emacs-lisp
(use-package eglot-header-line
	:ensure t
	:after eglot
	:vc (:url "https://github.com/soerlemans/eglot-header-line")
	:hook
	(eglot-managed-mode . eglot-header-line-mode))
```

## Demo
Here you can see it update the header-line in seperate buffers of a C++ and Python project:

![demo.gif](https://github.com/soerlemans/eglot-header-line/raw/refs/heads/main/assets/demo.gif)

The package depends directly on `eglot` and uses`eglot-request` which caches requests so its performant.
