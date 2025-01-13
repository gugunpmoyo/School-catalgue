;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Beings

(defclass being ()
  ((health :initarg :health
	   :accessor health
	   :initform 100)
   (on-death :initarg :on-death
             :initform (list #'on-death-hook)
             :accessor on-death)))

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
    (format stream "(~D healtH)" (health object))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Monsters

(defclass monster (being)
  ((health :initarg :health :accessor monster-health :initform 100)
   (weapon :initarg :weapon :accessor monster-weapon :initform (make-instance 'weapon))
   (rage-threshold :initarg :rage-threshold :accessor rage-threshold :initform 30)
   (rage-damage :initarg :rage-damage :accessor rage-damage :initform 5)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Players

(defclass player (being)
  ((health :initarg :health :accessor player-health :initform 100)
   (weapon :initarg :weapon :accessor player-weapon :initform (make-instance 'weapon))
   (healing-power :initarg :healing-power :accessor healing-power :initform 10)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Weapons

(defclass weapon ()
  ((damage :initarg :damage :accessor weapon-damage :initform 1)))


(defmethod print-object ((object weapon) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "(~d DAMAGE)" (damage object))))

(defgeneric damage-type (weapon)
  (:method-combination append))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hitting beings

(defmethod hit (being (weapon null) &key max-damage)
  (declare (ignore max-damage))
  (error "What are you going to hit that ~(~A~) with? Nothing!?"
         (type-of being)))

(defmethod hit ((attacker monster) (target player) &key max-damage)
  (declare (ignore max-damage)) 
  (let ((damage (weapon-damage (monster-weapon attacker))))
    (when (<= (monster-health attacker) (rage-threshold attacker))
      (incf damage (rage-damage attacker))
      (format t "~&~a is in rage mode!~%" attacker))
    (decf (player-health target) damage)
    (format t "~&~a hits ~a with ~d damage!~%" attacker target damage)))

(defmethod hit ((attacker player) (target monster) &key max-damage)
  (declare (ignore max-damage))
  (let ((damage (weapon-damage (player-weapon attacker))))
    (decf (monster-health target) damage)
    (format t "~&~a hits ~a with ~d damage!~%" attacker target damage)))

(defmethod heal ((player player))
  (let ((heal-amount (healing-power player)))
    (incf (player-health player) heal-amount)
    (format t "~&~a heals with ~d health!~%" player heal-amount)))

(defun battle (player monster)
  (loop while (and (> (player-health player) 0) (> (monster-health monster) 0))
        do (progn
             (hit player monster)
             (when (> (monster-health monster) 0)
               (hit monster player))
             (when (and (> (player-health player) 0) (< (random 4) 1))
               (heal player))))
  (if (> (player-health player) 0)
      (format t "~&~a wins!~%"  player)
      (format t "~&~a wins!~%"  monster)))
 
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hitting tests

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
      ;(setf (damage sword) 30)
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

(defclass ghost (monster) ()
  (:default-initargs :health 400))

(defmethod hit ((monster ghost) weapon &key max-damage)
  (declare (ignore max-damage))
  (if (member :magical (damage-type weapon))
      (call-next-method)
      (values nil 0 (health monster))))

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
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Defence

(defclass defensive-weapon (defensive-mixin weapon) ())

(defclass defense (weapon)
  ((absorption :initarg :absorption
	       :accessor weapon-absorption
	       :initform 5)))

(defmethod absorption ((object defense))
  (+ (call-next-method) (absorption object)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Defensive-mixin

(defclass defensive-mixin () ())

(defmethod hit :after ((player defensive-mixin) defense &key max-damage)
  (declare (ignore max-damage))
  (call-next-method)
  (format t "DEFENCE HAS BEEN ACTIVATED~%"))

(defmethod hit :around ((attacker monster) (target player) &key max-damage)
  (declare (ignore max-damage))
  (let* ((weapon (player-weapon target))
         (damage (weapon-damage (monster-weapon attacker)))
         (absorbed (if (typep weapon 'defensive-weapon)
                       (min damage (weapon-absorption weapon))
                       0))
         (reduced-damage (- damage absorbed)))
    (decf (player-health target) reduced-damage)
    (format t "~&~a hits ~a for ~d damage!~%" attacker target reduced-damage)
    (when (> absorbed 0)
      (format t "~&~a's weapon absorbs ~d damage!~%" target absorbed))
    (when (typep weapon 'defensive-weapon)
      (format t "DEFENCE HAS BEEN ACTIVATED~%"))))

(defmethod hit ((attacker player) (target monster) &key max-damage)
  (declare (ignore max-damage))
  (let ((damage (weapon-damage (player-weapon attacker))))
    (decf (monster-health target) damage)
    (format t "~&~a hits ~a for ~d damage!~%" attacker target damage)))

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

(defmethod hit ((monster undead) (weapon holy-mixin) &key max-damage)
  (declare (ignore max-damage))
  (call-next-method)(call-next-method))

(defmethod hit :around ((monster monster) (weapon weapon) &key max-damage)
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

(defmethod hit :around ((monster zombie) (weapon weapon) &key max-damage)
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









































#+(or)
(progn
  (defclass my-class (my-superclass) ())
  (defclass my-superclass (my-supersuperclass) ())
  (defclass my-supersuperclass () ())

  (defmethod frob ((x my-class))
    (when (next-method-p) (call-next-method)))
  (defmethod frob ((x my-superclass))
    (when (next-method-p) (call-next-method)))
  (defmethod frob ((x my-supersuperclass))
    (when (next-method-p) (call-next-method)))

  (defmethod frob :around ((x my-class))
    (when (next-method-p) (call-next-method)))
  (defmethod frob :around ((x my-superclass))
    (when (next-method-p) (call-next-method)))
  (defmethod frob :around ((x my-supersuperclass))
    (when (next-method-p) (call-next-method)))

  (defmethod frob :before ((x my-class)))
  (defmethod frob :before ((x my-superclass)))
  (defmethod frob :before ((x my-supersuperclass)))

  (defmethod frob :after ((x my-class)))
  (defmethod frob :after ((x my-superclass)))
  (defmethod frob :after ((x my-supersuperclass))))


;; Vehicles

(defclass vehicle () ())

(defclass horse (vehicle) ())

(defclass bicycle (vehicle) ())

(defgeneric move! (vehicle speed &optional destination))

(defmethod move! ((x horse) (speed integer) &optional destination)
  (declare (ignore destination))
  (format t "~S: NEIG~A~%"
          x
          (make-string speed :initial-element #\H)))

(defmethod move! ((x vehicle) speed &optional destination)
  (if destination
      (format t "Yes, we're moving at ~D km/h towards ~A!~%"
              speed destination)
      (format t "Yes, we're moving at ~D km/h!~%" speed)))

(defmethod move! ((x vehicle) (speed (eql t)) &optional destination)
  (declare (ignore destination))
  (format t "This vehicle is entering light speed.~%"))

(defmethod move! (vehicle speed &optional destination)
  (if destination
      (format t "~S is moving ~S fast towards ~A!~%"
              vehicle speed destination)
      (format t "~S is moving ~S fast!~%"
              vehicle speed)))

;; Different methods

(defclass plane () ())

(defclass pigeon () ())

(defgeneric move (thing)
  (:method ((thing plane)) t))

(defgeneric fly (thing)
  (:method ((thing plane)) t)
  (:method ((thing pigeon)) t))

#+(or)
(progn
  (defun foo (x y z))
  (defun foo (x y &optional z)) ; backwards compatible
  (defun foo (x y z w))) ; backwards incompatible

#+(or)
(progn
  (defgeneric act (action actor what where))

  (act :pick-up actor box location)
  (act :drop actor box location)

  (defgeneric pick-up (actor what where))
  (defgeneric drop (actor what where &key how))

  (pick-up actor box location)
  (drop actor box location :how how))
