SHELL = /bin/sh

ifeq ($(PREFIX),)
    PREFIX := /usr/local
endif
MANPREFIX := $(PREFIX)/share/man
QUICKLISP_DIR := ~/quicklisp

define SYSTEMD_SERVICE_CONTENTS
[Unit]
Description=thinklucent service
StartLimitIntervalSec=0

[Service]
Type=simple
Restart=always
RestartSec=1
User=root
ExecStart=$(PREFIX)/bin/thinklucent

[Install]
WantedBy=multi-user.target
endef
export SYSTEMD_SERVICE_CONTENTS

help:
	@echo "Use one of the following options:"
	@echo " - install"
	@echo " - uninstall"
	@echo " - reinstall"
	@echo " - update"

primary-deps:
	@echo "Making sure SBCL is installed..."
ifneq ($(shell command -v sbcl),)
	@echo "SBCL found."
else ifneq ($(shell command -v xbps-query),)
	sudo xbps-install -Syu sbcl
else ifneq ($(shell command -v pacman),)
    sudo pacman -Sy sbcl
else ifneq ($(shell command -v dnf),)
    sudo dnf install -y sbcl
else ifneq ($(shell command -v apt),)
    sudo apt install -y sbcl
else
	@echo "Could not determine steps to install SBCL! Please install SBCL and try again."
	exit 1
endif

secondary-deps:
	@echo "Installing optional dependencies..."
ifneq ($(shell command -v xbps-query),)
	sudo xbps-install -Syu libfixposix-devel
else ifneq ($(shell command -v beep),)
	sudo pacman -Sy libfixposix
else ifneq ($(shell command -v beep),)
	sudo dnf install -y libfixposix
else ifneq ($(shell command -v beep),)
	sudo apt install -y libfixposix-dev
else
	@echo "Could not install some dependencies! Please install manually."
endif

quicklisp:
ifeq ("$(wildcard $(QUICKLISP_DIR))", "")
	@echo "Setting up Quicklisp..."
	curl https://beta.quicklisp.org/quicklisp.lisp -o /tmp/quicklisp.lisp
	sbcl --load /tmp/quicklisp.lisp --non-interactive --eval "(quicklisp-quickstart:install)"
	sbcl --load ~/quicklisp/setup.lisp --non-interactive --eval "(ql:add-to-init-file)"
else
	@echo "Quicklisp found."
endif

binary:
	@echo "Generating binary..."
	sbcl --non-interactive --load build.lisp
	@echo "Binary generated."

place:
	@echo "Installing binary..."
	sudo install ./thinklucent-bin $(PREFIX)/bin/thinklucent
	sudo install ./scripts/* $(PREFIX)/bin/
	@echo "Binary installed."

manpage:
	@echo "Creating manpage..."
	sudo rsync ./man/thinklucent.1 $(MANPREFIX)/man1/
	@echo "Manpage created."

service:
	@echo "Looking for a known init system..."
ifneq ($(shell command -v runit),)
	@echo "'Runit' found. Attempting to create a service..."
	sudo mkdir /etc/sv/thinklucent
	sudo ln -s $(PREFIX)/bin/thinklucent /etc/sv/thinklucent/run
	sudo ln -s /etc/sv/thinklucent /var/service
	@echo "Runit service created and started."
else ifneq ($(shell command -v systemctl),)
	@echo "'SystemD' found. Attempting to create a service..."
	@echo "$$SYSTEMD_SERVICE_CONTENTS" | sudo tee /etc/systemd/system/thinklucent.service
	systemctl enable thinklucent.service
	systemctl start thinklucent.service
	@echo "SystemD service created and started."
else
	@echo "No known init system found."
endif

install: primary-deps secondary-deps quicklisp binary place manpage service
	@echo "thinklucent is now installed."

uninstall:
	@echo "Uninstalling thinklucent..."
	sudo rm $(PREFIX)/bin/thinklucent*
	sudo rm $(MANPREFIX)/man1/thinklucent.1
ifneq ($(shell command -v runit),)
	sudo rm -rf /var/service/thinklucent
	sudo rm -rf /etc/sv/thinklucent
else ifneq ($(shell command -v systemctl),)
	systemctl stop thinklucent.service
	systemctl disable thinklucent.service
	sudo rm -rf /etc/systemd/system/thinklucent.service
endif
	@echo "thinklucent has been uninstalled."

reinstall: uninstall install

get-latest:
	git pull origin main

update: get-latest reinstall
