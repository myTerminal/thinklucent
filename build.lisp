(ql:quickload "cl-inotify")
(ql:quickload "bt-semaphore")
(load "package.lisp")
(load "src/shell.lisp")
(load "src/hardware.lisp")
(load "src/main.lisp")

(sb-ext:save-lisp-and-die "thinklucent-bin"
                          :toplevel 'main:main
                          :executable t)
