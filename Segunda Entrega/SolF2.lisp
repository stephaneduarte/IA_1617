(load "datastructures.fas")
(load "auxfuncs.fas")


;;; TAI position
(defun make-pos (c l)
  (list c l))
(defun pos-l (pos)
  (first pos))
(defun pos-c (pos)
  (second pos))

;;; TAI acceleration
(defun make-acce (c l)
  (list c l))
(defun acce-l (pos)
  (first pos))
(defun acce-c (pos)
  (second pos))

;;; TAI velocity
(defun make-vel (c l)
  (list c l))
(defun vel-l (pos)
  (first pos))
(defun vel-c (pos)
  (second pos))


;; Solution of phase 1

(defun getTrackContent (pos track)
  (nth (pos-c pos) (nth (pos-l pos) (track-env track))))

;; Pedir 0,4
(defun isObstaclep (pos track)
  "check if the position pos is an obstacle"
  (or (< (pos-l pos) 0) (< (pos-c pos) 0)
      (>= (pos-l pos) (pos-l (track-size track)))
      (>= (pos-c pos) (pos-c (track-size track)))
      (null (getTrackContent pos track))))

;; Pedir 0,4
(defun isGoalp (st) 
  "check if st is a solution of the problem"
  (let ((current-position (state-pos st))
	(track (state-track st)))
    (and (member current-position (track-endpositions track) :test #'equalp)
	 T)))

;; Pedir 1,2
(defun nextState (st act)
  "generatnIXSB6
e the nextState after state st and action act from prolem"
  (let ((new-state (make-state :action act :track (state-track st))))
    (setf (state-vel new-state)
	  (make-vel (+ (vel-l (state-vel st)) (acce-l act))
		    (+ (vel-c (state-vel st)) (acce-c act))))
    (setf (state-pos new-state)
	  (make-pos (+ (pos-l (state-pos st)) (vel-l (state-vel new-state)))
		    (+ (pos-c (state-pos st)) (vel-c (state-vel new-state)))))
    (setf (state-cost new-state)
	  (cond ((isGoalp new-state) -100)
		((isObstaclep (state-pos new-state) (state-track new-state)) 20)
		(T 1)))
    (when (= (state-cost new-state) 20)
      (setf (state-vel new-state) (make-vel 0 0))
      (setf (state-pos new-state) (make-pos (pos-l (state-pos st))
					    (pos-c (state-pos st)))))
    (values new-state)))



;; Solution of phase 2

;;; Pedir 
(defun nextStates (st)
  "generate all possible next states"
  (let* ((new_st '()))
  (dolist (el (possible-actions) new_st)
    (push (nextState st el) new_st)
  )))

;;; limdepthfirstsearch 
(defun limdepthfirstsearch (problem lim)
  "limited depth first search
     st - initial state
     problem - problem information
     lim - depth limit"
	(recursive-dls (make-node :state (problem-initial-state problem)) problem lim))

	
;RECURSIVE-DLS (node,problem,limit) returns a solution, or failure/cutoff
(defun recursive-dls (node problem limit)
;if problem.Goal-Test(node.State) then return Solution(node)
  (if (funcall (problem-fn-isGoal problem) (node-state node))
    (return-from recursive-dls (solution node '())))
;else if limit = 0 then return cutoff
  (if (= 0 limit)
    (return-from recursive-dls ':corte))
;cutoff_occurred? <- false
  (let* ((cutoff_occurred 0))
  ;for each action in problem.Actions(node.State) do
  ;child <- Child-Node(problem,node)
      (dolist (el (funcall (problem-fn-nextStates problem) (node-state node)))
        ;result <- RECURSIVE-DLS(child,problem,limit -1)
          (let ((result (recursive-dls (make-node :state el :parent node) problem (- limit 1))))
            ;if result = cutoff then cutoff_occurred? <- true
            (if (equal result ':corte)
              (setf cutoff_occurred 1)
              ;else if result != failure then return result
              (if (not (equal result NIL))
                (return-from recursive-dls result)
                (return-from recursive-dls NIL)
            )
          ))
      )
      ;if cutoff_ocurred? then return cutoff else return failure
      (if (= 1 cutoff_occurred)
        (return-from recursive-dls ':corte)
        (return-from recursive-dls NIL)
      )
  ))

;solution - percorre node a node, indo de filho para pai, e junta a lista
(defun solution (node lst)
    (if (equal node NIL)
        (return-from solution (reverse lst))
        (solution (node-parent node) (append lst (list (node-state node))))
    )
)
				      

;iterlimdepthfirstsearch (prob) returns a solution, or failure
(defun iterlimdepthfirstsearch (problem)
  "limited depth first search
     st - initial state
     problem - problem information
     lim - limit of depth iterations"
    ;for depth <- 0 to infinite do
	(let ((depth 0))
	(loop
        ;result <- limdepthfirstsearch(prob,limit)
        (let ((result (limdepthfirstsearch problem depth)))
            ;if result != cutoff then return result
            (if (not (equal result ':corte))
                (return-from iterlimdepthfirstsearch result)
            )
        )
        (setf depth (+ depth 1))
    )
    )
)

