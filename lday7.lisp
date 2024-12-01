;;;; 2022/12/18

(load #p"~/quicklisp/setup.lisp")
(ql:quickload "str")

(if (find-package :lday7)
    (progn
      (unuse-package :lday7)
      (delete-package :lday7)))

(defpackage :lday7
  (:use :common-lisp)
  (:export
   #:read-datas
   #:main
   #:part1
   #:part2
   #:execute-cmds)
  )


(in-package :lday7)

(defun setsize (size dir root-fs)
  (setf (gethash dir root-fs) size))

(defun incsize (size dir root-fs)
  (incf (gethash dir root-fs) size))

(defun mkdir (dir root-fs)
  (setsize 0 dir root-fs))

(defconstant root-dir '("/"))

(defun initial-root-fs ()
  (let ((root-fs (make-hash-table :test #'equal)))
    (progn
      (mkdir root-dir root-fs)
      root-fs)))

(defun read-datas (fname)
  (let* ((s (uiop:read-file-string fname))
          (datas (str:split "$ " s)))
    (mapcar #'str:lines (cddr datas))))

(defun cmd-ls (content dir fs)
  (flet ((read-entry (entry)
           (destructuring-bind (dn name) (str:words entry)
             (if (string= dn "dir")
                 (mkdir (append dir (list name)) fs)
                 (let ((size (parse-integer dn)))
                   (incsize size dir fs))))))
    (progn
      (mapc #'read-entry content)
      (values dir fs))))

(defun cmd-cd (dest dir fs)
  (if (string= dest "..")
      (values (butlast dir)
              (gethash dir fs))
      (values (append dir (list dest))
              0)))

(defun execute-cd (dest dir fs)
  (multiple-value-bind (new size) (cmd-cd dest dir fs)
    (progn
      (incsize size new fs)
      (values new fs))))

(defun execute-cmds (cmds fs)
  (let ((cur root-dir))
    (flet ((exe-one-cmd (cmd)
             (let ((c (car cmd)))
               (if (string= c "ls")
                   (cmd-ls (cdr cmd) cur fs)
                   (let ((dest (cadr (str:words c))))
                     (setq cur (execute-cd dest cur fs)))))))
      (progn (mapc #'exe-one-cmd cmds)
             (loop
               (if (equal cur root-dir)
                   (return fs)
                   (setq cur (execute-cd ".." cur fs))))))))

(defun part1 (fs)
  (loop for v being the hash-values in fs
        when (<= v 100000)
          sum v))

(defun part2 (fs)
  (let* ((occupied (gethash root-dir fs))
         (free-space (- 70000000 occupied))
         (needed (- 30000000 free-space)))
    (loop for v being the hash-values in fs
          when (>= v needed)
            minimize v)))


(defun main ()
  (let* ((datas (read-datas "day7.txt"))
         (root-fs (execute-cmds datas (initial-root-fs))))
    (format T "~&Part1: ~A~%" (part1 root-fs))
    (format T "~&Part2: ~A~%" (part2 root-fs))))
