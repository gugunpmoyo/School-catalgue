;; C-x h M-x untabify
;; C-x h M-x indent-region
;; in your init file for emacs, add:
;; (setq-default indent-tabs-mode nil)

;; aggressive-indent
;; electric-indent

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Weapons

(defclass weapon ()
  ((damage :initarg :damage
           :accessor weapon-damage
           :accessor damage
           :initform 50)))

(defmethod print-object ((object weapon) stream)
  (print-unreadable-object (object stream :type t)
    (format stream "(~d damage)" (damage object))))

(defgeneric damage-type (weapon)
  (:method-combination append))

(defmethod absorption ((object weapon)) 0)
(defmethod absorption ((object null)) 0)

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Beings

(defclass being ()
  ((health :initarg :health
           :accessor health)
   (max-health :initarg :health
               :accessor max-health)
   (weapon :initarg :weapon
           :accessor weapon)
   (on-death :initarg :on-death
             :initform (list #'on-death-hook)
             :accessor on-death))
  (:default-initargs :health 100 :weapon nil))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Monsters

(defclass monster (being)
  ((rage-threshold :initarg :rage-threshold :accessor rage-threshold :initform 150)
   (rage-damage :initarg :rage-damage :accessor rage-damage :initform 50))
  (:default-initargs :health 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Players

(defclass player (being)
  ((name :initarg :name :accessor name)
   (mana :initarg :mana :accessor mana)
   (healing-power :initarg :healing-power :accessor healing-power :initform 50)
   (health-threshold :initarg :health-threshold :accessor health-threshold :initform 100)
   (inventory :initform () :accessor inventory)
   (level :initform 1 :accessor level)
   (experience :initform 0 :accessor experience)
   (special-ability :initarg :special-ability :accessor special-ability)
   (unique-items :initform () :accessor unique-items)
   (perk :initarg :perk :accessor perk)))

(defun create-warrior (name)
  (make-instance 'player
                 :name name
                 :health 250
                 :mana 20
                 :special-ability 'shield-block
                 :unique-items '("Battle Axe" "Steel Armor")
                 :perk 'extra-health))

(defun create-mage (name)
  (make-instance 'player
                 :name name
                 :health 150
                 :mana 100
                 :special-ability 'fireball
                 :unique-items '("Wand of Wisdom" "Robe of Mana")
                 :perk 'extra-mana))

(defun create-thief (name)
  (make-instance 'player
                 :name name
                 :health 120
                 :mana 50
                 :special-ability 'backstab
                 :unique-items '("Dagger of Speed" "Cloak of Shadows")
                 :perk 'critical-hit))

(defun create-zombie (name)
  (make-instance 'player
                 :name name
                 :health 200
                 :mana 0
                 :special-ability 'undead-resilience
                 :unique-items '("Rotting Claws" "Bone Shield")
                 :perk 'poison-immune))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Defensive-mixin

(defclass defensive-mixin ()
  ((absorption :initarg :absorption
               :accessor absorption
               :initform 20)))

(defmethod damage-type append ((weapon defensive-mixin))
  '(:defensive))

(defclass defensive-spear (defensive-mixin spear) ())
(defclass defensive-club (defensive-mixin club) ())

(defun test-defensive-weapon ()
  (let* ((sword (make-instance 'sword :damage 10))
         (player (make-instance 'player :weapon sword))
         (spear (make-instance 'defensive-spear))
         (monster (make-instance 'monster :health 100 :weapon spear)))
    (assert (= 20 (absorption spear)))
    (flet ((test-hit (expected-damage expected-remaining-health
                      &optional (expected-hitp t))
             (multiple-value-bind (hitp damage remaining-health)
                 (hit player monster)
               (assert (eq hitp expected-hitp))
               (assert (= damage expected-damage))
               (assert (= remaining-health
                          expected-remaining-health)))))
      (test-hit 0 100)
      (test-hit 0 100)
      (setf (damage sword) 20)
      (test-hit 0 100)
      (test-hit 0 100)
      (setf (damage sword) 30)
      (test-hit 10 90)
      (setf (damage sword) 50)
      (test-hit 30 60)
      (test-hit 30 30)
      (test-hit 30 0)
      (test-hit 0 0)
      (test-hit 0 0))))

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

(defun test-ghost-magical-weapons ()
  (let* ((ghost (make-instance 'ghost :health 100))
         (sword (make-instance 'sword :damage 50))
         (player (make-instance 'player :weapon sword)))
    (multiple-value-bind (hitp damage remaining-health) (hit player ghost)
      (assert (null hitp))
      (assert (= 0 damage))
      (assert (= 100 remaining-health (health ghost))))
    (let ((magical-sword (make-instance 'magical-sword :damage 50)))
      (setf (weapon player) magical-sword))
    (multiple-value-bind (hitp damage remaining-health) (hit player ghost)
      (assert (not (null hitp)))
      (assert (= 50 damage (damage (weapon player))))
      (assert (= 50 remaining-health)))))

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
;;; Ghost

(defclass ghost (monster) ()
  (:default-initargs :health 400))

(defmethod hit ((who being) (whom ghost) &key)
  (let ((weapon (weapon who)))
    (if (member :magical (damage-type weapon))
        (call-next-method)
        (values nil 0 (health whom)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ghost-inside mixin

(defclass ghost-inside-mixin () ())

(defmethod hit :after (who (whom ghost-inside-mixin) &key)
  (when (zerop (health whom))
    (change-class whom 'ghost :health 50)))

(defmethod hit :around ((who being) (whom ghost-inside-mixin) &key)
  (multiple-value-bind (hitp damage remaining-health) (call-next-method)
    (let* ((weapon (weapon who))
           (remaining-damage (- (damage weapon) damage)))
      (if (and hitp
               (not (zerop remaining-damage))
               (zerop remaining-health))
          (multiple-value-bind (hitp-2 damage-2 remaining-health-2)
              (hit who whom :max-damage remaining-damage)
            (values hitp-2
                    (+ damage damage-2)
                    remaining-health-2))
          (values hitp damage remaining-health)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Ogre

(defclass ogre (ghost-inside-mixin monster) ()
  (:default-initargs :health 800))

(defun test-hitting-ogre ()
  (let* ((sword (make-instance 'sword :damage 10))
         (player (make-instance 'player :weapon sword))
         (ogre (make-instance 'ogre :health 100)))
    (flet ((test-hit (expected-damage expected-remaining-health
                      &optional (expected-hitp t))
             (multiple-value-bind (hitp damage remaining-health)
                 (hit player ogre)
               (assert (eq hitp expected-hitp))
               (assert (= damage expected-damage))
               (assert (= remaining-health
                          expected-remaining-health)))))
      (test-hit 10 90)
      (setf (damage sword) 30)
      (test-hit 30 60)
      (test-hit 30 30)
      (assert (typep ogre 'ogre))
      (test-hit 30 0)
      (assert (typep ogre 'ghost))
      (test-hit 0 50 nil)
      (test-hit 0 50 nil))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Slime

(defclass slime (monster) ()
  (:default-initargs :health 100))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Undead

(defclass undead (monster) ())

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; zombie

(defclass zombie (monster)
  ((regeneration :initarg :regeneration
                 :accessor regeneration)
   (max-regeneration :initarg :regeneration
		     :accessor max-regeneration))
  (:default-initargs :regeneration 50))

(defmethod hit :around ((who being) (whom zombie) &key)
  (multiple-value-bind (hitp damage remaining-health) (call-next-method)
    (let* ((weapon (weapon who))
           (remaining-damage (- (damage weapon) damage))
           (regeneration (regeneration whom)))
      (if (zerop (health whom))
          0
          (setf (health whom) (max (+ remaining-health regeneration)
				(max-regeneration whom))))
      (setf remaining-health (health whom))
      (if (and hitp
               (not (zerop remaining-damage))
               (zerop remaining-health))
          (multiple-value-bind (hitp-2 damage-2 remaining-health-2)
              (hit who whom :max-damage remaining-damage)
            (values hitp-2
                    (+ damage damage-2)
                    remaining-health-2))
          (values hitp damage remaining-health)))))

(defun test-hitting-zombie ()
  (let* ((holy-sword (make-instance 'holy-sword :damage 10))
         (player (make-instance 'player :weapon holy-sword))
         (monster (make-instance 'zombie :health 100)))
    (flet ((test-hit (expected-damage expected-remaining-health)
             (multiple-value-bind (hitp damage remaining-health)
                 (hit player monster)
               (assert hitp)
               (assert (= damage expected-damage))
               (assert (= remaining-health
                          expected-remaining-health)))))
      (test-hit 20 130)
      (setf (damage holy-sword) 20)
      (test-hit 40 140)
      (test-hit 40 150)
      (setf (damage holy-sword) 50)
      (test-hit 100 100)
      (test-hit 100 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Boss-Ghost-Zombie

(defclass boss-ghost-zombie (zombie ghost) ()
  (:default-initargs :health 1000))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hitting beings

;; compute the damage to be dealt
(defgeneric compute-damage (who whom weapon shield &key &allow-other-keys))

(defmethod compute-damage (who whom (weapon weapon) (shield null) &key)
  (max 0 (- (damage weapon) (absorption shield))))

(defmethod compute-damage (who whom (weapon weapon) (shield weapon) &key)
  (max 0 (- (damage weapon) (absorption shield))))

;;compute-damage modified for undead and holy mixin 
(defmethod compute-damage ((who monster) whom weapon shield &key)
  (let ((rage-mode-p (> (health who) (rage-threshold who))))
    (when rage-mode-p
      (format t "~&~a is in rage mode!~%" who))
    (if (and (typep whom 'undead)
	     (member :holy (damage-type weapon)))
	(+ (call-next-method)(call-next-method)
	   (if rage-mode-p (rage-damage who) 0))
	(+ (call-next-method)
	   (if rage-mode-p (rage-damage who) 0)))))

;; deal a hit from one being to another being
(defgeneric hit (who whom &key &allow-other-keys))

(defmethod hit ((who being) (whom being) &key max-damage)
  (let* ((weapon (weapon who))
         (shield (weapon whom))
	 (health (health whom))
	 (regeneration (regeneration whom)))
    (when (null weapon)
      (error "What are you going to hit that ~(~A~) with? Nothing!?"
             (type-of whom)))
    (let* ((absorption (absorption shield)))
      (let* ((damage (compute-damage who whom weapon shield))
             (damage (min (health whom) damage))
             (damage (if max-damage (min max-damage damage) damage)))
        (format t "~&~a hits ~a with ~a, dealing ~d damage!~%"
                who whom weapon damage)
	(if (and (typep whom 'zombie)
		 (zerop health))
            0
            (setf health (max (+ health regeneration)
                              (max-regeneration whom))))
        (when (< 0 absorption)
          (format t "~&~a's weapon absorbed ~d damage!~%" whom absorption))
        (decf (health whom) damage)
        (values t
                damage
                (health whom))))))

;; heals the being
(defgeneric heal (who))

(defmethod heal ((being being)))

(defmethod heal :around ((player player))
  (when (< (health-threshold player) (health player))
    (call-next-method)))

(defmethod heal ((player player))
  (let ((heal-amount (healing-power player))
        (health (health player)))
    (setf (health player) (min (+ health heal-amount)
                               (max-health player)))
    (format t "~&~a heals with ~d health!~%" player heal-amount)))

(defun battle (being-1 being-2)
  (flet ((activate-defense (being)
           (let* ((shield (weapon being))
                  (absorption (absorption shield))
                  (control (if (< 0 absorption)
                               "~&~a's defense has been activated!~%"
                               "~&~a has no defensive weapons!~%")))
             (format t control (type-of being)))))
    (map nil #'activate-defense (list being-1 being-2)))
  (loop while (and (< 0 (health being-1))
                   (< 0 (health being-2)))
        when (< 0 (health being-1))
          do (hit being-1 being-2)
        when (< 0 (health being-2))
          do (hit being-2 being-1)
        when (< 0 (health being-1))
          do (heal being-1)
        when (< 0 (health being-2))
          do (heal being-2))
  (format t "~&~a wins!~%" (if (< 0 (health being-1))
                               being-1
                               being-2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Hitting tests

(defun test-hitting-monster ()
  (let* ((holy-sword (make-instance 'holy-sword :damage 10))
         (player (make-instance 'player :weapon holy-sword))
         (monster (make-instance 'zombie :health 100)))
    (flet ((test-hit (expected-damage expected-remaining-health)
             (multiple-value-bind (hitp damage remaining-health)
                 (hit player monster)
               (assert hitp)
               (assert (= damage expected-damage))
               (assert (= remaining-health
                          expected-remaining-health)))))
      (test-hit 20 80)
      (setf (damage holy-sword) 20)
      (test-hit 40 40)
      (test-hit 40 0))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Leveling up

(defun level-up (player)
  (incf (slot-value player 'level))
  (ecase (slot-value player 'perk)
    (extra-health (incf (slot-value player 'health) 20))
    (extra-mana (incf (slot-value player 'mana) 20))
    (critical-hit (format t "~a feels their strikes become deadlier!~%" (name player)))
    (poison-immune (format t "~a grows more resilient in their undead form!~%" (name player))))
  (format t "~a leveled up to level ~d!~%" (name player) (slot-value player 'level)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Special ability

(defun use-special-ability (player target)
  (ecase (slot-value player 'special-ability)
    (shield-block (format t "~a blocks the next attack!~%" (name player)))
    (fireball (format t "~a casts a fireball at ~a, dealing massive damage!~%" (name player) target))
    (backstab (format t "~a backstabs ~a for critical damage!~%" (name player) target))
    (undead-resilience (format t "~a regenerates health due to their undead nature!~%" (name player)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; Inventory

(defun show-inventory (player)
  (format t "~a's Inventory: ~a~%" (name player) (slot-value player 'inventory)))

(defun add-to-inventory (player item)
  (push item (slot-value player 'inventory))
  (format t "~a added ~a to their inventory.~%" (name player) item))

(defun show-unique-items (player)
  (format t "~a's Unique Items: ~a~%" (name player) (slot-value player 'unique-items)))

(defun player-action (player action who)
  (case action
    (:attack (format t "~a attacks ~a!~%" (name player) who))
    (:special-ability (use-special-ability player who))
    (:show-inventory (show-inventory player))
    (:show-unique-items (show-unique-items player))
    (t (format t "~a doesn't know how to ~a!~%" (name player) action))))

 
