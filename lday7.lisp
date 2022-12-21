;;;; 2022/12/18

(load #p"~/quicklisp/setup.lisp")
(ql:quickload "str")

(defpackage :lday7
  (:use :common-lisp)
  (:export
   #:read-datas
   #:main
   #:part1
   #:part2
   #:execute-cmds
   #:getsize)
  )


(in-package :lday7)

(defvar *my-files*
  (make-hash-table :test #'equal))

(defun getsize (dir)
  (gethash dir *my-files*))

(defun setsize (dir size)
  (setf (gethash dir *my-files*) size))

(defun incsize (dir size)
  (incf (gethash dir *my-files*) size))

(defun reset-my-files ()
  (progn
    (clrhash *my-files*)
    (setsize "/" 0)))

(defun read-datas (fname)
  (let* ((s (uiop:read-file-string fname))
          (datas (str:split "$ " s)))
    (mapcar #'str:lines (cddr datas))))


(defun getsize (dir)
  (gethash dir *my-files*))

(defun setsize (dir size)
  (setf (gethash dir *my-files*) size))

(defun incsize (dir size)
  (incf (gethash dir *my-files*) size))

(defun reset-my-files ()
  (progn
    (clrhash *my-files*)
    (setsize "/" 0)))


(defun cmd-ls (dir content)
  (reduce #'+
          (mapcar (lambda(entry)
                    (let* ((elts (str:words entry))
                           (dn (car elts))
                           (name (cadr elts)))
                      (if (string= dn "dir")
                          (setsize (str:join "/" (list dir name))
                                   0)
                          (ignore-errors (parse-integer dn)))))
                  content)))

(defun cmd-cd (cur dest)
  (if (string= dest "..")
      (let* ((dirs (str:split "/" cur))
             (new (str:join "/" (butlast dirs))))
        (values new (getsize cur)))
      (values (str:join "/" (list cur dest))
              0)))

(defun execute-cd (cur dest)
  (multiple-value-bind (new size) (cmd-cd cur dest)
    (progn
      (incsize new size)
      new)))

(defun execute-cmds (cmds)
  (let ((cur "/"))
    (progn (reset-my-files)
           (mapc (lambda(cmd)
                   (let ((c (car cmd)))
                     (if (string= c "ls")
                         (let ((size (cmd-ls cur (cdr cmd))))
                           (setsize cur size))
                         (let ((dest (cadr (str:words c))))
                           (setq cur (execute-cd cur dest))))))
                 cmds)
           (loop
             (if (string= "/" cur)
                 (return (getsize cur))
                 (setq cur (execute-cd cur "..")))))))

(defun part1 ()
  (loop for v being the hash-values in *my-files*
        when (<= v 100000)
          sum v))

(defun part2 ()
  (let* ((root-size (getsize "/"))
         (free-space (- 70000000 root-size))
         (needed (- 30000000 free-space)))
    (loop for v being the hash-values in *my-files*
          when (>= v needed)
            minimize v)))


(defun main ()
  (let ((datas (read-datas "input.txt")))
    (execute-cmds datas)
    (format T "~&Part1: ~A~%" (part1))
    (format T "~&Part2: ~A~%" (part2))))
