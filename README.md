# thinklucent

[![Built with Lisp](https://img.shields.io/badge/built%20with-Lisp-blueviolet)](https://lisp-lang.org)
[![License](https://img.shields.io/github/license/myTerminal/thinklucent.svg)](https://opensource.org/licenses/MIT)  
[![ko-fi](https://ko-fi.com/img/githubbutton_sm.svg)](https://ko-fi.com/Y8Y5E5GL7)

A simple implementation for automatic keyboard backlight in Linux for ThinkPad (and beyond)

## Background

After using an Apple MacBook Pro for four years, I collected quite a few elements from my computing workflow in macOS and carried over most of them to my Linux setup. However, one of those that I couldn't get over to this side of the wall was automatic keyboard backlight. The backlight could be turned ON and OFF, but would stay at the same state or brightness level regardless of whether I was interacting with the computer. After a few failed attempts at implementing a simple script to achieve it in Bash and on Node.js and then later on with Rust, I figured Lisp could be the one, and hence comes *thinklucent*.

*thinklucent* has been initially designed to be a script that watches for keyboard (and other input) events to keep the keyboard backlight ON for a reasonable time interval and then automatically turn if OFF until the next user interaction.

## Installation

There are a few different ways to get *thinklucent*.

### Compile from source

    # Clone project to the local workspace
    git clone https://github.com/myTerminal/thinklucent.git

    # Switch to the project directory
    cd thinklucent

    # Install with `make`
    make install

### Through a package manager

*thinklucent* will soon be available to install from your operating system's package manager.

## How to Use

A simple way to use *thinklucent* is to run it in a command-line terminal with no command arguments.

    sudo thinklucent

It does need to be run as `root` so that it can work with keyboard backlight.

### Running as a service

*thinklucent* can also be run as an init service, and currently only supports [Runit](http://smarden.org/runit). Support for more init systems will be implemented soon.

### Further help with commands

To learn more about usage, refer to `manpage`:

    man thinklucent

## Updating

In order to update *thinklucent*, simply run:

    thinklucent-update

## Uninstalling

In order to uninstall *thinklucent*, simply run:

    thinklucent-uninstall

## External Dependencies

Being written with Common Lisp, *thinklucent* depends on [SBCL](https://www.sbcl.org). In most cases, it will be automatically installed while generating the binary, but if it doesn't please install it before running the installation.

The other required programs are as follows:

 - [libfixposix-dev](https://github.com/sionescu/libfixposix)

## Similar project(s)

- [tp-auto-kbbl](https://crates.io/crates/tp-auto-kbbl)

## To-do

* Add support for more init systems like SystemD, OpenRC, SysVinit, etc.
* Support for more hardware
