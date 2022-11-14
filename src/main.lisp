;;;; -------------------------------------------------------------------------
;;;; main

(in-package :main)

(use-package '#:cl-inotify)

(defparameter *counter* 10)

(defun main ()
  "The main entry point to the program."
  (bt:make-thread
   (lambda ()
     (loop
      (sleep 1)
      (if (<= *counter* 0)
          (enable-or-disable-lights nil)
          (decf *counter*)))))

  (with-inotify (inotify T ("/dev/input" :all-events))
    (do-events (event inotify :blocking-p T)
      (setf *counter* 10)
      (enable-or-disable-lights t))))
