<!--suppress ALL -->
<h1 align="center">
    <br>
    <img src="./images/logo.webp" alt="EPry" width="200">
    <br>
    EPry
    <br>
</h1>

<p align="center"><strong>🚀 EPry: Supercharging Ruby Development in Emacs</strong></p>

## 🎯 Goal

Welcome to EPry, my first dive into Lisp, crafted to infuse Emacs with Ruby IDE features!
Although in its initial stages, EPry already provides basic debugging tools and a straightforward interface to enhance your Ruby coding sessions.

Currently, it supports essential command execution, project-specific settings, and preliminary debugging features. As I continue to grow my Lisp expertise, I am excited to develop EPry further—enriching it with more capabilities and polishing the experience.

Join me on this vibrant journey to make Ruby development in Emacs not just easier, but a true joy!

## 🛠️ Installation

EPry can be seamlessly integrated into your Emacs setup using `straight.el`, a modern, pure Emacs Lisp package manager that can handle packages from a variety of sources. To install EPry, ensure you have `straight.el` set up and then configure the package using the following `use-package` directive in your Emacs configuration:

```elisp
;; epry : https://github.com/QuirkQ/EPry
(use-package epry
  :after ruby-mode
  :straight (epry :type git :host github :repo "QuirkQ/EPry"))
```

## 📑 Configuration

### `epry-shell-path`
- Default: "/opt/homebrew/bin/fish" 🐟
- Purpose: Sets the shell for executing Ruby commands.
- Change: Modify to match your preferred shell, like /bin/bash.

### `epry-prefix-command`
- Default: "mise x -- "
- Purpose: Initializes non-interactive shell environments for Ruby commands.
- Customize: Update as needed for your environment manager setup.

## 🚀 Join the EPry Journey!

I'm on a mission to make Ruby development in Emacs delightful, and I'd love your help! From bug fixes to feature enhancements or even documentation improvements, all contributions are welcome.

### 🤝 How to Contribute
- **Feedback & Ideas**: Share your thoughts and help EPry grow!
- **Code Contributions**: Have a fix or a new feature? Send a pull request!
- **Docs**: Help new users by refining our guides.

### 🌟 Get Involved
Visit the [GitHub repository](https://github.com/QuirkQ/EPry) for issues, guidelines, and more. Whether you're a seasoned programmer or just starting out, your contributions are invaluable.

**Thanks for helping me build a fantastic Ruby development experience in Emacs! Let’s make something amazing together!**
