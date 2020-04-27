(defvar *onset* 0)
(defvar *trial-id* 0)

; TODO: Update *attended-location* for each of the 3 attended locations. One file for each.
(defvar *attended-location* 90)
(defvar *location* nil)
(defvar *frequency* nil)
(defvar *outfile* nil)
(defvar *noise-id* nil)
(defvar *s-standard* 62.40089)
(defvar *t-standard* 190.12009)
(defvar *s-deviant* 66.56591)
(defvar *t-deviant* 192.65439)

;;; To Run:
;;; Subjects directory and output directory must include the trailing / in the provided path.
;;; Subjects directory should contain CSV of every subject and standard location
;;; Output directory will contain model output for each subject and location
;;; Noise file contains exgaussian parameters for noise function, otherwise average is used.
;;; If Noise file is used, then a summary of parameters will be saved to the output directory

(defun run-experiment (subjects-directory output-directory &optional noise-file)
  (let* ((files (directory (concatenate 'string subjects-directory "*.csv")))
	(experiment-params (loop
			      for trial-file in files
			      collect (run-subject trial-file output-directory noise-file))))

    (if (not (eq noise-file nil))
	(save-params (apply #'concatenate 'string (list output-directory "params-summary.csv")) experiment-params))
    
    experiment-params))

(defun save-params (params-file params)
  (with-open-file (out params-file :direction :output :if-exists :supersede :if-does-not-exist :create)
    (format out "Trial ID, Noise ID, Standard Sigma, Standard Tau, Deviant Sigma, Deviant Tau~%")
    (loop
       for param-set in params
       do (format out "~A,~A,~A,~A,~A,~A~%"
		  (nth 0 param-set)
		  (nth 1 param-set)
		  (nth 2 param-set)
		  (nth 3 param-set)
		  (nth 4 param-set)
		  (nth 5 param-set)))))
  

(defun run-subject (trial-file output-directory &optional noise-file)
  ;(describe trial-file)
  ;(print (pathname-name trial-file))
					;(print (pathname-type trial-file))
  (setf *trial-id* 0)
					; Get random sigma and tau from file to use for trial noise
  (let* ((run-id (pathname-name trial-file))
	 (id-parts (read-from-string (substitute #\SPACE #\_ (format nil "(~a)~%" run-id))))
	 (subject-id (get-subject-id run-id))
	 (attended-location (get-attended-location run-id))
	 (directory (apply #'concatenate 'string (list output-directory subject-id "/")))
	 (out-file (apply #'concatenate 'string (list directory (write-to-string attended-location) "." "csv"))))
    
    (if (not (eq noise-file nil))
	(add-random-noise noise-file))
    
    (start-output out-file)

    (print out-file)
    ;(break)
    
    (with-open-file (input trial-file)
      (read-line input nil)
      (loop
	 for line = (read-line input nil)
	 while line
	 do (run-trial (get-ttl line) attended-location)))
					;return params
    (list run-id *noise-id* *s-standard* *t-standard* *s-deviant* *t-deviant*)))

(defun get-attended-location (run-id)
  (let* ((id-parts (read-from-string (substitute #\SPACE #\_ (format nil "(~a)~%" run-id))))
	 (location (write-to-string (nth 1 id-parts)))
	 (attended-location (parse-integer location :junk-allowed t)))
    attended-location))

(defun get-subject-id (run-id)
  (let* ((id-parts (read-from-string (substitute #\SPACE #\_ (format nil "(~a)~%" run-id))))
	 (subject-id (write-to-string (first id-parts))))
    subject-id))

(defun start-output (out-file)
  (setf *outfile* out-file)
  
  (with-open-file (out (ensure-directories-exist out-file) :direction :output :if-exists :supersede :if-does-not-exist :create)
    ;(format out "Standard Sigma:,~A~%",*s-standard*)
    ;(format out "Standard Tau:,~A~%",*s-tau*)
    ;(format out "Deviant Sigma:,~A~%",*s-sigma*)
    ;(format out "Deviant Tau:,~A~%~%",*s-tau*)
    (format out "Trial ID,Attended Location,Location,Frequency,Response Time~%")))
  

(defun get-ttl (line)
  (let ((trial (read-from-string (substitute #\SPACE #\, (format nil "(~a)~%" line)))))
    (parse-ttl (write-to-string (nth 1 trial)))))

(defun parse-ttl (ttl-string)
  (map 'list #'digit-char-p (coerce ttl-string 'list)))
  

(defun run-trial (ttl attended-location)
  (reset)
  (install-device (open-exp-window "Responses"))
  (setf *attended-location* attended-location)
  
  (clear-exp-window)

  (setf *location* (get-location (nth 1 ttl)))
  (setf *frequency* (get-frequency (nth 0 ttl)))

  (print `(new-other-sound ,*frequency* 2.4 0 0.05 ,*onset* ,*location* 'tone))
    
  (eval `(new-other-sound ,*frequency* 2.4 0 0.05 ,*onset* ,*location* 'tone))
  (print `(sgp :sigma-standard ,*s-standard* :sigma-deviant ,*s-deviant* :tau-standard ,*t-standard* :tau-deviant ,*t-deviant*))
  
  (eval `(sgp :sigma-standard ,*s-standard* :sigma-deviant ,*s-deviant* :tau-standard ,*t-standard* :tau-deviant ,*t-deviant*))

  (print `(add-dm (attended-location isa attended location ,attended-location)))
  (eval `(add-dm (attended-location isa attended location ,attended-location)))

  
  (print `(set-buffer-chunk 'imaginal 'attended-location))
  (print `(goal-focus goal))
  
  (eval `(set-buffer-chunk 'imaginal 'attended-location))
  (eval `(goal-focus goal))  
  ;(break)
  (run 100))

(defun add-random-noise( noise-file)

  (with-open-file (input noise-file)
					;skip header row
    (read-line input nil)
					; pick random number between 0 and 42
    (let* ((random-id (random 42))
	   (param-list
	    (loop
	       for line = (read-line input nil)
	       for i from 0
	       until (eq i random-id)
	       collect (get-params line)))
	   (params (first (last param-list))))
      
      (setf *noise-id* (nth 0 params))
      (setf *s-standard* (nth 2 params))
      (setf *t-standard* (nth 3 params))
      (setf *s-deviant* (nth 5 params))
      (setf *t-deviant* (nth 6 params)))))

(defun get-params (line)
  (read-from-string(substitute #\SPACE #\, (format nil "(~a)~%" line))))


(defun write-trial-result (time)
  (with-open-file (out *outfile* :direction :output :if-exists :append :if-does-not-exist :create)
    (format out "~5D,~5D,~5D,~5D,~5D~%"
	    *trial-id*
	    *attended-location*
	    *location*
	    *frequency*
	    time))
  
  (setf *trial-id* (+ *trial-id* 1)))

(defun get-location (location-id)
  (case location-id
    (1 -90)
    (2 -45)
    (3 0)
    (4 45)
    (5 90)))

(defun get-frequency (frequency-id)
  (case frequency-id
    (1 25)
    (2 75)))


(defmethod rpm-window-key-event-handler ((win rpm-window) key)
  (add-text-to-exp-window :text  (string-upcase (string key)) :x 130 :y 150)
  (write-trial-result (get-time)))

(clear-all)

(define-model cmsaa-test

  ;(sgp :randomize-time t :sigma-standard)
  (sgp :SIGMA-STANDARD 62.40089 :SIGMA-DEVIANT 66.56591 :TAU-STANDARD 190.12009 :TAU-DEVIANT 192.65439) 
  
  (chunk-type goal state)
  (chunk-type attended location)
  (chunk-type location-based-sound location freq)

  (add-dm
    (goal isa goal state find-sound)
    (location-based-sound location 0 freq 25))
 
  (p find-the-sound
    =goal>
      state find-sound
    =imaginal>
      - location nil
    ?aural-location>
      state free
    - buffer requested
    ==>
    -goal>
    =imaginal>  
    +aural-location>
      isa audio-event) ;need to implement this functionality so that automatic buffer stuffing does not occur
	
  (p detected
    =aural-location>
      ?aural>
      state free
    =imaginal>
    ==>
    +aural>
      event =aural-location)
	
  (p sound-respond-low
    =aural>
      isa sound
      content 25
    ?manual>
      state free
  ==>
	+manual>
      cmd press-key
      key "d")

  (p sound-respond-high
    =aural>
      isa sound
      content 75
    ?manual>
      state free
    ==>
    +manual>
      cmd press-key
      key "k"))

  ;(new-other-sound 75 2.4 0 0.05 0 0 'tone))
