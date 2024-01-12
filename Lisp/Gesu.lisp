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
        (cond ((= (count-elements (caar s-parent-l)) 2)
              (parts-to-be-inherited (cddr s-input-l) 
                (cons (field-remover (car s-input-l)   
		             NIL)
                (method-replacer (cadr s-input-l) (car s-parent-l)))
                c-input-l))

                ((and (= (count-elements (caar s-parent-l)) 3) (null (cdr s-parent-l)))
                (parts-to-be-inherited (cddr s-input-l) 
                (cons (field-remover (car s-input-l)   
                 (car s-parent-l))
                (method-replacer (cadr s-input-l) NIL))
                c-input-l))

                (t (parts-to-be-inherited (cddr s-input-l) 
                  (cons (field-remover (car s-input-l)   
		                (car s-parent-l))
                (method-replacer (cadr s-input-l) (cdr s-parent-l)))
                 c-input-l))))))

;;; COUNT-ELEMENTS conta gli elementi di una lista
(defun count-elements (item)
  "Count the number of elements in a list or a dotted pair."
  (cond ((null item) 0)  ; End of a regular list
        ((not (consp item)) 1)  ; Single element (non-list)
        ((null (cdr item)) 1)  ; Single cons cell with NIL as cdr
        ((not (consp (cdr item))) 2)  ; Dotted pair
        (t (+ 1 (count-elements (cdr item))))))  ; Regular list element


;************************************

;;; FIELD-REMOVER
(defun field-remover (new-list old-list)
  (controllo-tipo (cadr (remove-and-find-matches old-list new-list)))
  (car (remove-and-find-matches old-list new-list))
)

;;; REMOVE-AND-FIND-MATCHES 
; trova duplicati in una lista
; restituisce una lista dove il car e' una lista con tutti gli elementi che appartengono alla prima lista ma NON appartengono alla seconda, e il cdr e' una lista contenenti tutte le coppie uguali 
(defun remove-and-find-matches (list-a list-b)
  (match-helper list-a list-b nil nil))

(defun match-helper (list-a list-b acc matches)
  (if (null list-a)
      (list (reverse acc) (reverse matches))
      (let* ((current (first list-a))
             (rest (rest list-a))
             (match (find-if (lambda (item) (equal (first current) (first item))) list-b)))
        (if match
            (match-helper rest list-b acc (cons (list current match) matches))
            (match-helper rest list-b (cons current acc) matches)))))

;;; CONTROLLO-TIPO
;prende in inpput una lista di coppie, ogni coppia e' formata da due liste ((name value type) (name value type))
(defun controllo-tipo (list)
  (if (null list)
    nil
    (if  (check-type-in-subclass (cadr (cadar list)) (cddr (cadar list)) (cddaar list))
      (controllo-tipo (cdr list))
      (error "Sottotipo piu' ampio del supertipo, non viene rispettata la gerarchia tra tipi.")))) ;;;;;;;;;; MIGLIORA FRASE ERRORE

;;; METHOD-REPLACER
(defun method-replacer (new-list old-list)
  (car (remove-and-find-matches old-list new-list)))

;************************************


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
        (t NIL)))       


;;; REWRITE-METHOD
(defun rewrite-method (method-spec)
  (list 'lambda (append '(this)
			(car method-spec))
	(cons 'progn (cdr method-spec))))

;;;; end of file -- Gesu.lisp

