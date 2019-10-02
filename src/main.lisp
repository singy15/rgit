(defpackage rgit
  (:use 
    :cl)
  (:export
    :init
    :sync
    :commit
    :status
    :restore
    :main))
(in-package :rgit)

(defvar +config-path+ "./rgitfile")

(defparameter *config* nil)
(defparameter *verbose* nil)
(defparameter *updated* 0)
(defparameter *fetched* 0)
(defparameter *skipped* 0)
(defparameter *entries* 0)
(defparameter *not-found* 0)

;; Do when unknown option passed
(defun unknown-opts (condition)
  (format t "Warning: Unknown option ~s~%" (opts:option condition))
  (invoke-restart 'opts:skip-option))

;; Do when option designated
(defmacro when-opts ((opts opt) &body body)
  `(let ((it (getf ,opts ,opt)))
     (when it
       ,@body)))

;; Logging
(defmacro logging (&body body)
  `(when *verbose*
     ,@body))

;; Show process block message
(defmacro proc-block (proc-name &body body)
  `(progn
     (format t "~A..." ,proc-name)
     ,@body
     (format t "DONE~%")))

;; Read file
(defun slurp (path)
  (with-open-file (s path :direction :input)
    (let ((buf (make-string (file-length s))))
      (read-sequence buf s)
      buf)))

;; File copy
(defun copy-file (src dst)
  (with-open-file (ins 
                   src 
                   :direction 
                   :input 
                   :element-type '(unsigned-byte 8)
                   :if-does-not-exist nil)
    (when ins
      (with-open-file (outs 
                       dst 
                       :direction 
                       :output 
                       :element-type '(unsigned-byte 8)
                       :if-exists :supersede)
        (loop for byte = (read-byte ins nil)
              while byte
              do (write-byte byte outs))))))

;; Compare MD5
(defun p-hashequal (x y)
  (equal (coerce (md5:md5sum-file (pathname x)) 'list)
         (coerce (md5:md5sum-file (pathname y)) 'list)))

;; Is file
(defun p-file (path)
  (not (equal "" (file-namestring (pathname path)))))

;; Read rgitfile
(defun read-config ()
  (setf *config* (read-from-string (slurp +config-path+))))

;; init command
(defun init (&optional (target nil))
  (format t "Initialize repository~%")

  (proc-block "creating rgitfile"
    (with-open-file (out +config-path+ :direction :output :if-exists :supersede)
      (write-line "(:target ((\"/snapshot/target/directory/\" . \"mapped/\")))~%" out))))

;; Fetch file
(defun fetch-file (src dst)
  (logging (format t "FETCH ~A~%" (pathname src)))
  (copy-file (pathname src) (pathname dst)))

;; Update file
(defun update-file (src dst)
  (logging (format t "UPDATE ~A~%" (pathname src)))
  (copy-file (pathname src) (pathname dst)))

;; Skip processing file
(defun skip-file (src)
  (logging (format t "SKIP ~A~%" src)))

;; Synchronize directory
(defun sync-dir (p)
  (mapc
    (lambda (p)
      (let ((ls (directory (format nil "~A**/*.*" (car p))))
            (del-ls (directory (format nil "~A**/*.*" (cdr p)))))
        ; Create directory
        (ensure-directories-exist (cdr p))
        
        ; Walk directory
        (mapc
          (lambda (src)
            ; Remove from delete list
            (setf del-ls
                (remove-if (lambda (x) 
                             (equal
                               (namestring (enough-namestring x (car (directory "."))))
                               (namestring (merge-pathnames (pathname (enough-namestring src (car p))) (pathname (cdr p))))))
                           del-ls))
            
            ; Process file
            (when (p-file src)
              (let ((dst (merge-pathnames 
                           (enough-namestring (pathname src) (pathname (car p)))
                           (pathname (cdr p)))))
                ; Create directory
                (ensure-directories-exist (directory-namestring dst))
                
                ; Fetch file
                ; TODO : filter needed
                (when (and (not (search "$" (namestring (pathname src)))))
                  (if (probe-file (pathname dst))
                      ;; File already fetched
                      (if (not (p-hashequal (pathname src) (pathname dst)))
                          ;; File modified
                          (update-file src dst)

                          ;; File not modified
                          (skip-file src))

                      ;; File not fetched yet
                      (fetch-file src dst))))))
          (directory (format nil "~A**/*.*" (car p))))

          ; Delete missing files
          (mapcar 
            (lambda (x)
              (when (p-file x)
                (delete-file x)))
            del-ls)))
    (getf *config* :target)))

;; Synchronize files
(defun sync-file (p)
  ; Create directory
  (ensure-directories-exist (cdr p))
  
  ;; Process file
  (if (probe-file (pathname (car p)))
      ;; File found
      (progn
        (if (probe-file (pathname (cdr p)))
            ;; File already fetched
            (if (not (p-hashequal (pathname (car p)) (pathname (cdr p))))
                ;; File modified
                (update-file (car p) (cdr p))

                ;; File not modified
                (skip-file (car p)))
            
            ;; File not fetched yet
            (fetch-file (car p) (cdr p))))

      ;; File not found
      (progn
        (logging 
          (format t "file not found, skip : ~A~%" (car p))))))

;; sync command
(defun sync ()
  (format t "Synchronize~%")

  ;; Read config
  (read-config)
    
  ;; Synchronize
  (proc-block "synchronizing"
    (logging (format t "~%"))
    (mapc
      (lambda (p)
        (if (p-file (car p))
            (sync-file p)
            (sync-dir p)))
      (getf *config* :target))))

;; Definition of command line arguments
(opts:define-opts
  (:name :help
   :description "print this help text"
   :short #\h
   :long "help")
  (:name :verbose
   :description "verbose output"
   :short #\v
   :long "verbose"))

;; Program entry point
(defun main ()
  (multiple-value-bind (opts args)
    (handler-case
        (handler-bind ((opts:unknown-option #'unknown-opts))
          (opts:get-opts))
      (opts:missing-arg (condition)
        (format t "fatal: option ~s needs an argument!~%"
                (opts:option condition)))
      (opts:arg-parser-failed (condition)
        (format t "fatal: cannot parse ~s as argument of ~s~%"
                (opts:raw-arg condition)
                (opts:option condition)))
      (opts:missing-required-option (con)
        (format t "fatal: ~a~%" con)
        (opts:exit 1)))

  ;; Help
  (when-opts (opts :help)
    (opts:describe
     :prefix "rgit"
     :suffix ""
     :usage-of "rgit"
     :args     "[FREE-ARGS]"))

  ;; Check verbose option
  (when-opts (opts :verbose)
    (setf *verbose* t))

  ;; Execute sub-command
  (if (> (length args) 0)
    (cond
      ((equal "sync" (car args)) (sync))
      ((equal "init" (car args)) (init))
      (t (write-line "Unknown command, rgit -h to show help.")))
    (write-line "Command not specified, rgit -h to show help."))))

(in-package :cl-user)

