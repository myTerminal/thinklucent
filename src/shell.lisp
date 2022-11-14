;;;; -------------------------------------------------------------------------
;;;; Functions to interact with the shell

(in-package :shell)

(defun execute-in-system (command-string)
  "Executes the supplied command in the underlying system."
  (uiop:run-program command-string
                    :input :interactive
                    :output :interactive
                    :error-output t
                    :ignore-error-status t))
