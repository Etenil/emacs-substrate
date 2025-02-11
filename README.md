# Emacs Substrate

A clean basis to organically grow your config onto.

**NOTICE:** Requires Emacs 29.1 or better.

## Description

This is a minimal Emacs base config. Here's the short of the philosophy:

 - Focus on using default, built-in Emacs behavior
 - Emacs-lisp centric, encourage configuration through code
 - Allow update and upgrade of substrate
 - No magic

## Installation

To install emacs-substrate, clone this repository somewhere, then create the files:

```lisp
;; ~/.emacs.d/early-init.el
(add-to-list 'load-path "<path-where-emacs-substrate-was-cloned>")

(require 'early-substrate)
```

```lisp
;; ~/.emacs.d/init.el
(require 'substrate)

;; Set custom variables here

(substrate-init)

;; The rest of your config below
```

## Screenshots

What you should see on opening Emacs up with Bedrock installed: a simple splash screen, [modus-vivendi](https://protesilaos.com/emacs/modus-themes) active, and the `help-quick` display at the bottom.

![Emacs using Bedrock configuration showing the splash screen with the quick help at the bottom](screenshots/basic_splash.png)

Basic code editing: line numbers and `hl-line-mode`.

![Editing the source code of Emacs Bedrock's init.el file while using Bedrock configuration](screenshots/basic_code_editing.png)

## Philosophy

TODO

## Requirements

Emacs 29.1 or later.

Emacs 29.1 is, as of 2023-09-04, the latest stable release. The specific features from Emacs 29.1 that Bedrock relies on are:

 - The `use-package` macro for configuration
 - Enhancements to the built-in completion help (`completions-auto-select`, `completion-auto-help`, etc.)
 - Built-in tree-sitter support
 - Built-in LSP client (Eglot)

## Credits

Creator and maintainer:

 - Ashton Wiersdorf https://lambdaland.org
