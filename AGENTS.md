# AGENTS.md

This file provides guidance to AI coding agents working in this repository.

## Overview

This is a personal Emacs configuration centered on a monolithic `init.el`.
Package management uses Elpaca with `use-package`. The configuration is tuned
for startup performance and includes language support for day-to-day
development.

## Configuration Structure

### Main Files

- `init.el` - Main configuration file with all settings and package declarations
- `early-init.el` - Performance optimizations executed before package initialization
- `custom.el` is gitignored and not tracked

### Key Directories

- `elpaca/` - Package manager directory (builds, cache, repos)
- `elpa/` - Legacy package installations
- `.cache/` - Runtime cache including copilot language server
- `eln-cache/` - Native compilation cache
- `transient/`, `tree-sitter/`, `undo/`, `url/` - Various runtime directories

## Package Management

Uses Elpaca (modern alternative to straight.el) with use-package declarations:

```elisp
(elpaca elpaca-use-package)
(elpaca-wait)
```

## Code Organization

The `init.el` file is organized into clear sections marked with Japanese comments:

- 基本設定 (Basic Settings)
- WSL2 設定 (WSL2 Settings)
- フォント設定 (Font Settings)
- Package configurations grouped by functionality

## Important Configuration Patterns

### Adding New Packages

Follow the existing `use-package` style:

```elisp
(use-package package-name
  :ensure t
  :config
  ;; Configuration here
  )
```

### Disabling Packages

Packages are usually disabled by commenting out with a `;; ` prefix, not
deleted. Preserve that convention unless removal is clearly intentional.

### Performance Considerations

- Garbage collection is deferred during startup in `early-init.el`
- Many packages use lazy loading such as `:defer t`
- Native compilation is enabled

## Language Support

Primary languages configured:

- Ruby (enhanced-ruby-mode, rubocop, rspec-mode)
- Python (python-mode, lsp-pyright)
- JavaScript/TypeScript (js2-mode, typescript-mode, web-mode)
- C/C++ (cc-mode with custom style)

## Key Bindings

Uses custom prefix keys:

- `C-q` - Custom command prefix
- `C-c` - Mode-specific bindings
- Many packages use their default keybindings

## Development Tools

- **LSP**: lsp-mode with lsp-ui for language server support
- **Completion**: Corfu for in-buffer completion, Vertico for minibuffer
- **Git**: Magit with extensive configuration
- **Project Management**: Projectile
- **Syntax Checking**: Flycheck

## Common Tasks

### Reload Configuration

```elisp
M-x eval-buffer  ;; While in init.el
```

### Update Packages

```elisp
M-x elpaca-update-all
```

### Check Package Load Times

```elisp
M-x use-package-report
```

## Notes

- Comments and section headers are in Japanese
- Configuration uses lexical binding
- WSL2-specific settings are included for Windows Subsystem for Linux compatibility
- Input method support for Japanese (mozc/ibus/uim)
