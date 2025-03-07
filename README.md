# Emacs Substrate

A clean basis to organically grow your config onto.

```
     .⌒.      ▗▄▄▖▗  ▖ ▗▖  ▗▄  ▄▄      ▄▄ ▗  ▖▗▄▄  ▄▄ ▄▄▄▖▗▄▄  ▗▖ ▄▄▄▖▗▄▄▖
   .#   #.    ▐   ▐▌▐▌ ▐▌ ▗▘ ▘▐▘ ▘    ▐▘ ▘▐  ▌▐  ▌▐▘ ▘ ▐  ▐ ▝▌ ▐▌  ▐  ▐
  /       \   ▐▄▄▖▐▐▌▌ ▌▐ ▐   ▝▙▄     ▝▙▄ ▐  ▌▐▄▄▘▝▙▄  ▐  ▐▄▄▘ ▌▐  ▐  ▐▄▄▖
 (,,,___,,,)  ▐   ▐▝▘▌ ▙▟ ▐     ▝▌      ▝▌▐  ▌▐  ▌  ▝▌ ▐  ▐ ▝▖ ▙▟  ▐  ▐
     ) (      ▐▄▄▖▐  ▌▐  ▌ ▚▄▘▝▄▟▘    ▝▄▟▘▝▄▄▘▐▄▄▘▝▄▟▘ ▐  ▐  ▘▐  ▌ ▐  ▐▄▄▖
    (___)
```

**NOTICE:** Requires Emacs 29.1 or newer.

## Description

This is a minimal Emacs base config. Here's the short of the philosophy:

 - Focus on using default, built-in Emacs behaviour
 - Emacs-lisp centric, encourage configuration through code
 - Provide an upgrade path for Substrate
 - Stay minimal, only include as little packages and config as possible
 - Encourage tweaking and coding of Emacs and its ecosystem
 - Keep to the project vision, even though users are free to turn features off

## Installation

### With automated script

Copy and paste the following into a terminal:

```
curl https://raw.githubusercontent.com/Etenil/emacs-substrate/refs/heads/main/installer/installer.sh | bash
```

This will install emacs-substrate in your `~/.emacs.d` folder. You can set the environment variable `EMACS_DIR` before running the script to install it in a different folder.

### Manually

To install emacs-substrate, clone this repository somewhere, then create the following 2 files:

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

### How to update

Emacs-substrate's code lives within the local clone of the git repository. Updating your installation is as simple as running `git pull` within that folder. If using the installer, do as follows:

```
pushd $HOME/.emacs.d/emacs-substrate
git pull
popd
```

And restart emacs.

## Customisation

### Available settings

Substrate can be configured through Emacs's built-in customisation framework. To access it, use `M-x customize` and search for `substrate`. Alternatively, set the options with `setopt` in your init file. The available options are:

- `substrate-enable-windmove`: Enable windmove to hop around windows with ctrl+arrow - default `t`
- `substrate-display-startup-help`: Show a help window on startup - default `t`
- `substrate-enable-which-key`: Use which-key to list available key combos - default `t`
- `substrate-display-line-numbers`: Display line numbers on buffers - default `t`
- `substrate-enable-cua-mode`: Enable CUA mode (`C-c` for copy, `C-p` for paste etc.) - default `t`
- `substrate-configure-theme`: Configure the default theme - default `t`
- `substrate-enable-evil`: Enable evil-mode and the Vi-like keyboard mapping - default `nil`

If you'd rather use `setopt` to configure those variables, it can be used like so:

```lisp
;; Configure all the opposite defaults for substrate
(setopt substrate-enable-windmove nil)
(setopt substrate-display-startup-help nil)
(setopt substrate-enable-which-key nil)
(setopt substrate-display-line-numbers nil)
(setopt substrate-enable-cua-mode nil)
(setopt substrate-configure-theme nil)
(setopt substrate-enable-evil t)
```

### Setting a different theme

To set a different theme, first disable the default theme from Substrate like so:

```lisp
(setopt substrate-configure-theme t)
```

Then set your favourite theme. Instead of using `load-theme`, use `substrate-set-theme`, as it will also work when running emacs as a daemon.

```lisp
(use-package nord-theme
	:config (substrate-set-theme 'nord))

;; Or

(straight-use-package 'nord-theme)
(substrate-set-theme 'nord)
```

## Screenshots

What you should see on opening Emacs up with Substrate installed: a simple splash screen, [modus-vivendi](https://protesilaos.com/emacs/modus-themes) active, and the `help-quick` display at the bottom.

![Emacs using Substrate configuration showing the splash screen with the quick help at the bottom](screenshots/substrate-home-screen.png)

Basic code editing: line numbers and `hl-line-mode`.

![Editing the source code of Emacs Substrate's substrate.el file while using Substrate configuration](screenshots/substrate-editing.png)

## Requirements

- Emacs 29.1 or later.
- Git

## Credits

This project is a radical fork of [Emacs-bedrock](https://codeberg.org/ashton314/emacs-bedrock)

Maintainer of Emacs-substrate:

- [Gene Pasquet](https://github.com/Etenil)

Creator and maintainer of Emacs-bedrock:

- [Ashton Wiersdorf](https://lambdaland.org)

G
