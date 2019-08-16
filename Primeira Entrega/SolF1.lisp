
;;; These functions, and any other ones needed must be implemented

;;; Utilizar estes includes para os testes na versao local
;;; comentar antes de submeter
;(load "datastructures.lisp")
;(load "auxfuncs.lisp")

;;; Utilizar estes includes para a versao a submeter
; tirar o comentario antes de submeter
(load "datastructures.fas")
(load "auxfuncs.fas")

(defun pos_line (pos)
	(nth 0 pos))

(defun pos_row (pos)
	(nth 1 pos))

(defun isObstaclep (pos track) 
  "check if there is an obstacle at position pos of the track"
  (let ((obstacles_list (track-env track)))
  (if (nth (pos_row pos) (nth (pos_line pos) obstacles_list))
  	NIL
  	t)
  ))

(defun isGoalp (st) 
  "check if st is a goal state"
  (let* ((car_position (state-pos st))
  		(track (state-track st))
  		(end_positions (track-endpositions track)))
  (dolist (el end_positions)
  	(when (eq (equal el car_position) t)
  		(return t)
  	))))

(defun nextState (st act)
  "generate the nextState after state st and action act"
  (let* ((car_position (state-pos st))
  		(car_vel (state-vel st))
  		(track (state-track st))
  		;new aux State
  		(new_vel ())
  		(new_pos ())
  		(nextState (make-STATE :POS ()
  							:VEL ()
  							:ACTION act
  							:COST 1
  							:TRACK track
  							)))
  ;Velocity - v = car_vel + aceleration
  (push (+ (nth 1 act) (nth 1 car_vel)) new_vel)
  (push (+ (nth 0 act) (nth 0 car_vel)) new_vel)
  (setf (state-vel nextState) new_vel)
  ;Position - p = car_pos + velocity
  (push (+ (nth 1 car_position) (nth 1 new_vel)) new_pos)
  (push (+ (nth 0 car_position) (nth 0 new_vel)) new_pos)
  (setf (state-pos nextState) new_pos)
  ;Cost
  (if (isObstaclep new_pos track) ;check if there is an obstacle at the new position of the track
  	(setf (state-cost nextState) 20))
  (if (isGoalp nextState) ;check if the newState is a goal state
  	(setf (state-cost nextState) -100))
  nextState
))