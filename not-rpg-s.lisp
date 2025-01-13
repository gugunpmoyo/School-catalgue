;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Sword

(defclass sword (weapon) ())

(defmethod damage-type append ((weapon sword))
  '(:slashing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Spear

(defclass spear (weapon) ())

(defmethod damage-type append ((weapon spear))
  '(:piercing))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Club

(defclass club (weapon) ())

(defmethod damage-type append ((weapon club))
  '(:blunt))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; The best weapon in existence

(defclass very-blunt-and-pointy-sword (club spear sword) ())

#+(or)
(print (damage-type (make-instance 'very-blunt-and-pointy-sword)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Defence

(defclass defense (weapon)
  ((absorber :initarg :absorber
             :initform 200
             :accessor absorber)))

(defmethod absorber ((object defense))
  (+ (call-next-method) (absorber object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Defensive-mixin

(defclass defensive-mixin () ())

(defmethod hit :after ((player defensive-mixin) defense &key max-damage)
  (declare (ignore max-damage))
  (format t "DEFENCE HAS BEEN ACTIVATED~%"))

(defmethod hit :around ((player defensive-mixin) (defense defense)
                        &key max-damage)
  (call-next-method)
  (let* ((health (health player))
	 (absorber (absorber defense))
	 (damage (damage defense))
	 (added-health (+ health absorber))
	 (damage (min (if max-damage (min damage max-damage) damage) added-health))
	 (new-health (max 0 (- added-health damage))))
    (setf (health player) new-health)
    (values t damage (health player))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ghost-inside mixin

(defclass ghost-inside-mixin () ())

(defmethod hit :after ((monster ghost-inside-mixin) weapon &key max-damage)
  (declare (ignore max-damage))
  (when (zerop (health monster))
    (change-class monster 'ghost :health 50)))

(defmethod hit :around ((monster ghost-inside-mixin) (weapon weapon)
                        &key max-damage)
  (declare (ignore max-damage))
  (multiple-value-bind (hitp damage remaining-health) (call-next-method)
    (let ((remaining-damage (- (damage weapon) damage)))
      (if (and hitp
               (not (zerop remaining-damage))
               (zerop remaining-health))
          (multiple-value-bind (hitp-2 damage-2 remaining-health-2)
              (hit monster weapon :max-damage remaining-damage)
            (values hitp-2
                    (+ damage damage-2)
                    remaining-health-2))
          (values hitp damage remaining-health)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;Defensive weapons

(defclass defensive-sword (defensive-mixin sword) ())
(defclass defensive-spear (defensive-mixin spear) ())
(defclass defensive-club (defensive-mixin club) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Magical items

(defclass magical-mixin ()
  ((modifier :initarg :modifier
             :initform 0
             :accessor modifier)))

(defmethod damage ((object magical-mixin))
  (+ (call-next-method) (modifier object)))

(defmethod damage-type append ((weapon magical-mixin))
  '(:magical))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Magical weapons

(defclass magical-sword (magical-mixin sword) ())
(defclass magical-spear (magical-mixin spear) ())
(defclass magical-club (magical-mixin club) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Epic mixin

(defclass epic-mixin ()
  ((multiplier :accessor multiplier
               :initarg :multiplier
               :initform 2)))

(defmethod damage ((object epic-mixin))
  (* (call-next-method) (multiplier object)))

(defmethod damage-type append ((weapon epic-mixin))
  '(:epic))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Epic weapons

(defclass epic-sword (epic-mixin sword) ())
(defclass epic-spear (epic-mixin spear) ())
(defclass epic-club (epic-mixin club) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Epic magical weapons

(defclass epic-magical-sword (epic-mixin magical-mixin sword) ())
(defclass epic-magical-spear (epic-mixin magical-mixin spear) ())
(defclass epic-magical-club (epic-mixin magical-mixin club) ())

#+(or)
(progn
  CL-USER> (typep (make-instance 'epic-magical-sword)
                  'magical-epic-sword)
  NIL
  CL-USER> (typep (make-instance 'epic-magical-sword)
                  '(and magical-mixin epic-mixin sword))
  T)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Holy-mixin

(defclass holy-mixin (magical-mixin)
  ((multiplier :accessor multiplier
               :initarg :multiplier
	       :initform 2)))

(defmethod damage ((object holy-mixin))
  (* (call-next-method) (multiplier object)))

(defmethod damage-type append ((weapon holy-mixin))
  '(:holy))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Holy weapons

(defclass holy-sword (holy-mixin sword) ())
(defclass holy-spear (holy-mixin spear) ())
(defclass holy-club (holy-mixin club) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Beings

(defclass being ()
  ((health :initarg :health
           :initform (alexandria:required-argument :health)
           :accessor health)
   (on-death :initarg :on-death
             :initform (list #'on-death-hook)
             :accessor on-death)
   (weapon :initarg :weapon
	   :accessor weapon)))

(defmethod (setf health) :after (newval (being being))
  (when (and (zerop newval)
	     (not (zerop (health being))))
    (loop for hook in (on-death being)
          do (funcall hook being))))

(defun on-death-hook (being)
    (when (zerop (health being)) 
      (format t "VICTOR ~%YOUR OPPONENT HAS BEEN DEFEATED~%")))

(defmethod print-object ((object being) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "(~D health)" (health object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Monsters

(defclass monster (being) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Players

(defclass player (being) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hitting beings

(defmethod hit ((being being) (weapon weapon) &key max-damage)
  (let* ((health (health being))
         (damage (damage weapon))
         (damage (min (if max-damage (min damage max-damage) damage) health))
         (new-health (max 0 (- health damage))))
    (setf (health being) new-health)
    (values t damage (health being))))

(defmethod hit (being (weapon null) &key max-damage)
  (declare (ignore max-damage))
  (error "What are you going to hit that ~(~A~) with? Nothing!?"
         (type-of being)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hitting test

(defun test-hitting-monster ()
  (let ((monster (make-instance 'monster :health 100))
        (sword (make-instance 'sword :damage 10)))
    (flet ((test-hit (expected-damage expected-remaining-health)
             (multiple-value-bind (hitp damage remaining-health)
                 (hit monster sword)
               (assert hitp)
               (assert (= damage expected-damage))
               (assert (= remaining-health
                          expected-remaining-health)))))
      (test-hit 10 90)
      (setf (damage sword) 30)
      (test-hit 30 60)
      (test-hit 30 30)
      (test-hit 30 0)
      (test-hit 0 0)
      (test-hit 0 0)
      (setf (health monster) 10)
      (test-hit 10 0)
      (test-hit 0 0)
      (test-hit 0 0))))

(defun test-ghost-magical-weapons ()
  (let ((ghost (make-instance 'ghost :health 100))
        (sword (make-instance 'sword :damage 50))
        (magical-sword (make-instance 'magical-sword :damage 50)))
    ;; (format t "Hitting a ghost with a sword.~%")
    (multiple-value-bind (hitp damage remaining-health) (hit ghost sword)
      (assert (null hitp))
      (assert (= 0 damage))
      (assert (= 100 remaining-health (health ghost))))
    ;; (format t "Hitting a ghost with a magical sword.~%")
    (multiple-value-bind (hitp damage remaining-health) (hit ghost magical-sword)
      (assert (not (null hitp)))
      (assert (= 50 damage (damage magical-sword)))
      (assert (= 50 remaining-health)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ghost

(defclass ghost (undead) ()
  (:default-initargs :health 400))

(defmethod hit ((monster ghost) (weapon weapon) &key max-damage)
  (declare (ignore max-damage))
  (if (member :magical (damage-type weapon))
      (call-next-method)
      (values nil 0 (health monster))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Slime

(defclass slime (monster) ()
  (:default-initargs :health 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ogre

(defclass ogre (ghost-inside-mixin monster) ()
  (:default-initargs :health 800))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Undead

(defclass undead (monster)())

(defmethod hit ((monster undead) (weapon weapon) &key max-damage)
  (declare (ignore max-damage))
  (if (member :holy (damage-type weapon))
      (call-next-method)
      (values nil 0 (health monster))))

(defmethod hit ((monster undead) (weapon holy-mixin)
		&key max-damage)
  (declare (ignore max-damage))
  (call-next-method)(call-next-method))

(defmethod hit :around ((monster monster) (weapon weapon)
                        &key max-damage)
  (let* ((health (health monster))
         (damage (damage weapon))
         (damage (min (if max-damage (min damage max-damage) damage) health))
         (new-health (max 0 (- health damage))))
    (setf (health monster) new-health)
    (values t damage (health monster))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zombie

(defclass zombie (undead)
  ((regeneration :initarg :regeneration
		 :accessor regeneration
		 :initform 100)))

(defmethod hit :around ((monster zombie) (weapon weapon)
			&key max-damage)
  (let* ((health (health monster))
         (damage (damage weapon))
	 (regeneration (regeneration monster))
	 (damage (min (if max-damage (min damage max-damage) damage) health))
	 (new-health (max 0 (- health damage))))
    (if (zerop new-health)
	0
	(setf new-health (+ regeneration new-health)))
    (setf (health monster) new-health)
    (values t damage (health monster))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Boss-Ghost-Zombie

(defclass boss-ghost-zombie (zombie ghost) ()
   (:default-initargs :health 1000))

 
