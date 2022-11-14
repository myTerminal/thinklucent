;;;; -------------------------------------------------------------------------
;;;; Function to interact with the hardware

(in-package :hardware)

(defun enable-or-disable-lights (enablep)
  "Enables of disables keyboard backlight."
  (execute-in-system (concatenate 'string
                                  "echo "
                                  (if enablep
                                      "1"
                                      "0")
                                  " > /sys/class/leds/tpacpi::kbd_backlight/brightness")))
