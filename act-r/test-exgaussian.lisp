(defun remg (mu sigma tau)
  (+ (random-distributions:random-normal mu sigma) (random-distributions:random-exponential (coerce tau 'double-float))))

(defun test-remg (n outfile)
  (let ((random-nums
	 (loop
	    for i from 0
	    until (eq i (- n 1))
	    collect (remg 0 1 1))))

    (print random-nums)

    (with-open-file (out outfile :direction :output :if-exists :append :if-does-not-exist :create)
      (loop
	 for i from 0
	 until (eq i (- n 1))
	 do (format out "~A~%" (nth i random-nums))))))
	 
