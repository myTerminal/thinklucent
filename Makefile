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

crater-get:
	@echo "Setting up Crater for temporary use..."
	git clone https://github.com/crater-space/cli /tmp/crater-cli

primary-deps:
	@echo "Making sure SBCL is installed..."
ifneq ($(shell command -v sbcl),)
	@echo "SBCL found."
else
	@echo "SBCL not found!"
	@echo "Attemping to install SBCL using Crater..."
	/tmp/crater-cli/crater install sbcl
endif

secondary-deps:
	@echo "Looking for 'libfixposix'..."
ifneq ($(shell command -v libfixposix),)
	@echo "'libfixposix' found."
else
	@echo "'libfixposix' not found!"
	@echo "Attemping to install 'libfixposix' using Crater..."
	/tmp/crater-cli/crater install libfixposix
endif

crater-remove:
	@echo "Removing Crater..."
	rm -rf /tmp/crater-cli

req: crater-get primary-deps secondary-deps crater-remove

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
	mkdir -p $(MANPREFIX)
	sudo cp ./man/thinklucent.1 $(MANPREFIX)/man1/
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

install: req quicklisp binary place manpage service
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
