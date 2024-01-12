;;;; -*- Mode: Lisp -*-
;;;; Gesu.lisp

;;; HASH-TABLE A SUPPORTO DI DEF-CLASS
(defparameter *classes-specs* (make-hash-table))

(defun add-class-spec (name class-spec)
  (setf (gethash name *classes-specs*) class-spec))

(defun get-class-spec (name)
  (gethash name *classes-specs*))

;;; DEF-CLASS
(defun def-class (class-name parents &rest part)
  (cond ((null class-name)
     (error "Class-name e' nullo."))
    ((not (symbolp class-name))
     (error "Class-name non e' un simbolo."))
    ((not (listp parents))
     (error "Parents non e' una lista."))
    ((not (check-parents parents))
     (error "(Almeno) uno dei parents indicati non e' stato definito."))
    ((not (null (member class-name parents)))
     (error "Class-name non puo' essere uguale ad un parent."))
    ((not (listp part))
     (error "L'insieme di campi deve essere una lista."))
    ((not (null (get-class-spec class-name)))
     (error "Non e' possibile ridefinire una classe."))
    (t (add-class-spec class-name
               (list parents
                 (inherit-from-p
                  (build-parts part) parents)))
       class-name)))

;(defun inherit-from-p (a b)
;  T
;)

;;; IS-CLASS
(defun is-class (class-name)
  (if (symbolp class-name)
      (if (get-class-spec class-name)
      T
    NIL)
    NIL))

;;; CHECK-PARENTS
(defun check-parents (parents)
  (if (null parents)
      T
    (if (is-class (car parents))
    (check-parents (cdr parents))
      NIL)))

;;; BUILD-PARTS
(defun build-parts (part-l)
  (if (null part-l)
      NIL
    (cons (process-part (car part-l))
		(build-parts (cdr part-l)))))

;;; PROCESS-PART		
(defun process-part (input-l)
	(if (null input-l)
      (error "Part in input non e' una lista.")
    (if (equal 'methods (car input-l))
		(mapcar 'process-methods-l (cdr input-l))
      (if (equal 'fields (car input-l))
		(mapcar 'process-field (cdr input-l))))))

;;; PROCESS-METHODS-LIST
(defun process-methods-l (methods-l)
	 (cons (car methods-l)
		    (process-method (car methods-l)
				    (cdr methods-l))))

;;; PROCESS-FIELD
(defun process-field (field-l)        ;(NAME "Eve") (AGE 21 INTEGER)
	(cond ((null field-l)
		NIL)
	 ((= (list-length field-l) 1)
	  (cons (car field-l)
			(cons NIL T)))
	 ((= (list-length field-l) 2)
			(cons (car field-l)
					(cons (second field-l) T)))
	 ((= (list-length field-l) 3)
    (if (check-types-in-field (cadr field-l) (caddr field-l))
      (cons (car field-l)
				(cons (second field-l)
						(third field-l)))
      (error "Il valore indicato non rispetta il tipo specificato.")))
	 (t NIL)))

;;; CHECK-TYPES-IN-FIELD
(defun check-types-in-field (value type)
  (cond ((typep value type) T)
        ;((is-instance value type) T)
        (t NIL)))

;;; PROCESS-METHOD	 
(defun process-method (method-name method-spec)
  (setf (fdefinition method-name)
	(lambda (this &rest arglist)
	  (apply (field this method-name)
		 (append (list this) arglist))))
  (eval (rewrite-method method-spec)))

;;; INHERIT-FROM-P
(defun inherit-from-p (s-input-l parents-l)
  (if (null parents-l)
      s-input-l
    (inherit-from-p
     (parts-to-be-inherited s-input-l
			    (cadr (get-class-spec (car parents-l)))
			    s-input-l)
     (cdr parents-l))))


;;; PARTS-TO-BE-INHERITED
;quando non ha piu' nulla da controllare della sottoclasse rispetto ai parent, unisce i nuovi fields specificati a quelli puramente ereditati
(defun parts-to-be-inherited (s-input-l s-parent-l c-input-l)
  (if (null s-input-l)
    (if (or (null s-parent-l) (null (car s-parent-l)))
        c-input-l
      (if (symbolp (first s-parent-l))
        (append (cons (append (car c-input-l) (car (list s-parent-l)))  
        (append (cdr c-input-l) (cdr (list s-parent-l)))))

        (append (cons (append (car c-input-l) (car s-parent-l))
        (append (cdr c-input-l) (cdr (list s-parent-l)))))))

    (if (null s-parent-l)
	    c-input-l
      (parts-to-be-inherited (cddr s-input-l) 
			     (cons (field-remover (car s-input-l)   
					   (car s-parent-l))
            (method-replacer (cadr s-input-l) (cdr s-parent-l)))
			     c-input-l))))


;;; FIELD-REMOVER
;ogni field nella superclasse, ricorsivamente, viene rimosso se ridefinito dalla sottoclasse
(defun field-remover (field-list s-parent-l)
  (if (null s-parent-l)
      NIL
    (cond ((equal (caar field-list) (caar s-parent-l))
      (if (check-type-in-subclass (second (car field-list)) (cddr (car field-list)) (cddr (car s-parent-l)))
        (field-remover (cdr field-list) (cdr s-parent-l))
        (error "Il valore indicato non rispetta il tipo specificato.")))
      (t (cons (car s-parent-l) (field-remover field-list (cdr s-parent-l)))))))



;;; METHOD-REPLACER
; controlla se due campi method hanno lo stesso nome, se si mantiene quello della sottoclasse, se no restituisce i metodi non comuni
(defun method-replacer (method-list s-parent-l)
  (if (null method-list)
    (cons s-parent-l nil)
    (if (null s-parent-l)
        NIL
        (cond ((equal (caar method-list) (caar s-parent-l))
            (method-replacer method-list (cdr s-parent-l)))
            (t (cons (car s-parent-l) (method-replacer method-list (cdr s-parent-l))))))))



;;; CHECK-TYPE-IN-SUBCLASS
; controllo se il typeClass e' una classe e se il Supertipo e' contenuto nei genitori della classe typeClass
(defun check-type-in-subclass (value typeClass typeSuper)
  (cond ((subtypep typeClass typeSuper) T)
        ((is-class typeClass)
          (cond ((member typeSuper (car (get-class-spec typeClass))) T)))
        ((equal typeClass T)
          (if (typep value typeSuper) 
            T
          (NIL))) 
        (t (error "Sottotipo piu' ampio del supertipo, non viene rispettata la gerarchia tra tipi."))))       ;;;;;;;;;;;;;;;, CORREGGI ERROREEEEEEEE


;;; REWRITE-METHOD
(defun rewrite-method (method-spec)
  (list 'lambda (append '(this)
			(car method-spec))
	(cons 'progn (cdr method-spec))))

;;;; end of file -- Gesu.lisp

