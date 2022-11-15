;;;; -------------------------------------------------------------------------
;;;; main

(in-package :main)

(use-package '#:cl-inotify)

(defconstant +delay+ 10)

(defun main ()
  "The main entry point to the program."
  (let ((count +delay+))
    (bt:make-thread
     (lambda ()
       (loop
        (sleep 1)
        (if (<= count 0)
            (enable-or-disable-lights nil)
            (decf count)))))

    (with-inotify (inotify T ("/dev/input" :all-events))
      (do-events (event inotify :blocking-p T)
        (setf count +delay+)
        (enable-or-disable-lights t)))))
