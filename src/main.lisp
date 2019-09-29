(defpackage rgit
  (:use 
    :cl)
  (:export
    :init
    :sync
    :commit
    :status
    :restore))
(in-package :rgit)

(defun init (&optional (target nil))
  (format t "initialize repository~%")
  (format t "creating rgitfile...")
  (with-open-file (out "./rgitfile" :direction :output :if-exists :supersede)
    (write-line (format nil "~A~%" 
                        "(:target ((\"/snapshot/target/directory/\" . \"mapped/\")))")
                out))
  (format t "DONE~%")
  (format t "creating rgitignore...")
  (with-open-file (out "./rgitignore" :direction :output :if-exists :supersede)
    ; TODO
    nil)
  (format t "DONE~%"))

(defun slurp (path)
  (with-open-file (s path :direction :input)
    (let ((buf (make-string (file-length s))))
      (read-sequence buf s)
      buf)))

(defun read-config (path)
  (let (conf 
        conf-ls)
    (setf conf (slurp path))
    (setf conf-ls (split-sequence:split-sequence #\Newline conf))
    (mapcar 
      (lambda (x)
        (cons (car (split-sequence:split-sequence #\Space x))
              (cadr (split-sequence:split-sequence #\Space x))))
      (remove-if (lambda (x) (equal x "")) conf-ls))))

(defun byte-copy (infile outfile)
  (with-open-file (instream infile :direction :input :element-type '(unsigned-byte 8)
                            :if-does-not-exist nil)
    (when instream
      (with-open-file (outstream outfile :direction :output :element-type '(unsigned-byte 8)
                                 :if-exists :supersede)
        (loop for byte = (read-byte instream nil)
           while byte
           do (write-byte byte outstream))))))

(defun read-config-plist ()
  (let ((conf (read-from-string (slurp "./rgitfile"))))
    conf))

; TODO: can be replaced with enough-pathname
(defun abs2rel (abs-path root)
  (pathname (subseq (namestring abs-path) 
                    (length (directory-namestring root)))))

(defun sync ()
  (let ((conf (read-config-plist)))
    (format t "synchronize~%")
    
    ; Synchronize
    (format t "synchronizing...")
    ; * (equal (coerce (md5:md5sum-file #p"~/wk/notes/note") 'list) (coerce (md5:md5sum-file #p"~/wk/notes/task") 'list))
    (mapc
      (lambda (p)
        (let ((ls (directory (format nil "~A**/*.*" (car p))))
              (del-ls (directory (format nil "~A**/*.*" (cdr p)))))
           ; Create parent
          (ensure-directories-exist (cdr p))
          
          ; Probe directory
          (mapc
            (lambda (s)
              ; Remove from delete list
              (setf del-ls
                  (remove-if (lambda (x) 
                               (equal
                                 (namestring (enough-namestring x (car (directory "."))))
                                 (namestring (merge-pathnames (pathname (enough-namestring s (car p))) (pathname (cdr p))))))
                             del-ls))
              
              ; Create directory and fetch file
              (when (not (equal "" (file-namestring (pathname s))))
                (let ((dst (merge-pathnames 
                             (abs2rel s (car p))
                             (pathname (cdr p)))))
                  ; Create directory
                  (ensure-directories-exist (directory-namestring dst))
                  
                  ; Fetch file
                  ; TODO : check update before copy
                  (byte-copy (pathname s) (pathname dst)))))
            (directory (format nil "~A**/*.*" (car p))))

            ; Delete missing files
            (mapcar 
              (lambda (x)
                (when (not (equal "" (file-namestring (pathname x))))
                  (delete-file x)))
              del-ls)))
      (getf conf :target))
    (format t "DONE~%")))

; (defun sync ()
;   (let ((conf (read-config "./rgitfile")))
;     (format t "synchronizing~%")
;     
;     ; Check updated files
;     (format t "checking update...")
;     nil
;     (format t "DONE~%")
; 
;     ; Create directories
;     (format t "creating directories...")
;     (mapc (lambda (x) 
;             (when (not (equal "" (directory-namestring (cdr x))))
;               (ensure-directories-exist (directory-namestring (cdr x)))))
;           conf)
;     (format t "DONE~%")
; 
;     ; Fetch updated files
;     (format t "fetching files...")
;     (format t "~%")
;     (mapc (lambda (x) 
;             (format t "~A~%" x)
;             (byte-copy (car x) (cdr x))
;             )
;           conf)
;     (format t "DONE~%")))

(in-package :cl-user)

