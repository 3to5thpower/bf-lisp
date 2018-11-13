;;brainfuck interpreter
;;To run brainfuck code, evaluate these expression.
;;(let ((*readtable* *bf-readtable*)) (load "~~.bf"))



;;make readtable
(defvar *bf-readtable* (make-instance 'readtable))


;;define of pointer and memory
(declaim ((simple-array (unsigned-byte 8) (*)) *memory*))
(defvar *memory* (make-array 30000 :element-type '(unsigned-byte 8)))
(defvar *pointer* 0)

(defun ct-pos-nega-chars (pos-char nega-char stream char)
  (let ((chars (loop for char := (read-char stream nil)
                  while (find char `(,pos-char ,nega-char)) collect char
                  finally (unread-char char stream))))
    (- (count pos-char chars)
       (count nega-char chars)
       (if (char= char nega-char) 1 -1))))

;;define of + and -
(defun plmi-reader (stream char)
  `(incf (aref *memory* *pointer*) ,(ct-pos-nega-chars #\+ #\- stream char)))

(set-macro-character #\+ #'plmi-reader nil *bf-readtable*)
(set-macro-character #\- #'plmi-reader nil *bf-readtable*)


;;define of > and <
(defun gtlt-reader (stream char)
  `(incf *pointer* ,(ct-pos-nega-chars #\> #\< stream char)))

(set-macro-character #\> #'gtlt-reader nil *bf-readtable*)
(set-macro-character #\< #'gtlt-reader nil *bf-readtable*)


;;define of , and .
(defun comma-reader (stream char)
  (declare (ignore stream char))
  '(setf (aref *memory* *pointer*) (char-code (read-char *standard-input*))))

(defun dot-reader (stream char)
  (declare (ignore stream char))
  '(princ (code-char (aref *memory* *pointer*))))

(set-macro-character #\, #'comma-reader nil *bf-readtable*)
(set-macro-character #\. #'dot-reader nil *bf-readtable*)


;;define of [ and ]
(defun opscuare-reader (stream char)
  (declare (ignore char))
  (let ((body (loop :for form := (read stream t)
                :until (eql form ']) :collect form)))
    `(loop :until (zerop (aref *memory* *pointer*))
        :do (progn ,@body))))

(defun clscuare-reader (stream char)
  (declare (ignore stream char))
  '])

(set-macro-character #\[ #'opscuare-reader nil *bf-readtable*)
(set-macro-character #\] #'clscuare-reader nil *bf-readtable*)



;;define readermacro which enable us to identify newline
(defun newline-reader (stream char)
  (declare (ignore stream char))
  't)

(set-macro-character #\newline #'newline-reader nil *bf-readtable*)


;;comment function
(set-macro-character #\; (get-macro-character #\;) nil *bf-readtable*)

;;error-handler
(defmacro with-simple-handler (&body body)
  `(handler-case
       (let ((*error-output* (make-broadcast-stream))) ,@body)
     (condition (c) (format t "~&~a~%" c))))

(defun execute-brainfuck (file)
  (with-simple-handler
      (let ((*readtable* *bf-readtable*))
        (load file))))
