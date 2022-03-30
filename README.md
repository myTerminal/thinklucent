# thinklucent

[![Crates.io version](https://img.shields.io/crates/v/thinklucent)](https://crates.io/crates/thinklucent)
[![Crates.io downloads](https://img.shields.io/crates/d/thinklucent)](https://crates.io/crates/thinklucent)  
[![Code Climate](https://codeclimate.com/github/myTerminal/thinklucent.png)](https://codeclimate.com/github/myTerminal/thinklucent)
[![Coverage Status](https://img.shields.io/coveralls/myTerminal/thinklucent.svg)](https://coveralls.io/r/myTerminal/thinklucent?branch=master)  
[![License](https://img.shields.io/github/license/myTerminal/thinklucent.svg)](https://opensource.org/licenses/MIT)

A simple implementation for automatic keyboard backlight in Linux for ThinkPad (and beyond)

## Background

After using an Apple MacBook Pro for four years, I collected quite a few elements from my computing workflow in macOS and carried over most of them to my Linux setup. However, one of those that I couldn't get over to this side of the wall was automatic keyboard backlight. The backlight could be turned ON and OFF, but would stay at the same state or brightness level regardless of whether I was interacting with the computer. After a few failed attempts at implementing a simple script to achieve it in Bash and on Node.js, I figured Rust may be a more appropriate platform for this, and hence comes *thinklucent*.

*thinklucent* has been initially designed to be a script that watches for keyboard (and other input) events to keep the keyboard backlight ON for a reasonable time interval and then automatically turn if OFF until the next user interaction. A few planned features include integration with major init systems (systemd, runit, openrc, etc.), and maybe even compatibility with systems other than ThinkPads.

## Installation

There are a few different ways to get *thinklucent*.

### As a binary crate using Cargo

If you already have [Cargo](https://github.com/rust-lang/cargo) installed, *thinklucent* can be installed directly from [crates.io](https://crates.io) using the below command:

    cargo install thinklucent

Once installed, in order to update and get the latest version, install it with `--force`:

    cargo install thinklucent --force

Uninstalling is also as easy as:

    cargo uninstall thinklucent

### As a native application package

#### Compile from source

    # Clone project to local
    git clone https://github.com/myTerminal/thinklucent.git

    # Switch to project directory
    cd thinklucent

    # Install with `make`
    make install

Uninstalling would need only a single command:

    make uninstall

Re-installation is also possible with:

    make reinstall

#### Through an existing package manager in your system

*thinklucent* will hopefully soon be available to install from your operating system's package manager.

## How to Use

> (Coming soon)

### Further help with commands

To learn more about usage, refer to `manpage`:

    man thinklucent

## Similar project(s)

- [tp-auto-kbbl](https://crates.io/crates/tp-auto-kbbl)

## To-do

* init-system integration
* Support for more hardware
