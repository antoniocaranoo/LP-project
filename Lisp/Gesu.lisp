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
                  (build-parts (reverse-lists part)) parents)))
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

;;; REVERSE-LISTS
(defun reverse-lists (part)
  (if (eql (caar part) 'methods)
    (reverse part)
    part))

;;; PROCESS-METHODS-LIST
(defun process-methods-l (methods-l)
	 (cons (car methods-l)
		    (process-method (car methods-l)
				   (cdr methods-l))))

;;; PROCESS-FIELD
(defun process-field (field-l)        
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
        ((is-instance value type) T)
        (t (error "value ~s for field written is not of type ~s" value type))))


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
    (if (null s-parent-l)
        c-input-l
      (if (null (car s-parent-l))
        (append (cons (car c-input-l)  
        (append (cdr c-input-l) (cdar (list s-parent-l)))))
        
        (if (null (cdr s-parent-l))
        (append (cons (append (car c-input-l) (car s-parent-l))
         (cdr c-input-l)))
          ; se la classe ha solo methods
          (if (and (not (= (count-elements (cdaar c-input-l)) 2)) (null       
              (cdr c-input-l))
              (null (cadr (remove-and-find-matches (car s-parent-l)
              (car c-input-l)))) 
              (null (cadr (remove-and-find-matches (car s-parent-l) (cdr 
              c-input-l)))) 
              (= (count-elements (car s-parent-l)) (count-elements 
              (car (remove-and-find-matches (car s-parent-l) (car 
              c-input-l))))))
                (append (cons (car s-parent-l)
                (list (append (car c-input-l) (cadr s-parent-l)))))

                (append (cons (append (car c-input-l) (car s-parent-l))
                (list (append (cdr c-input-l) (cadr s-parent-l)))))
                ))))

    (if (null s-parent-l)
	    c-input-l
        (cond ((= (count-elements (caar s-parent-l)) 2)
              (parts-to-be-inherited (cddr s-input-l) 
                (cons (field-remover (car s-input-l)   
		             NIL)
                (list (method-replacer (cadr s-input-l) (car s-parent-l))))
                c-input-l))

                ((and (= (count-elements (caar s-parent-l)) 3) (null (cdr s-parent-l)))
                (parts-to-be-inherited (cddr s-input-l) 
                (cons (field-remover (car s-input-l)   
                 (car s-parent-l))
                (method-replacer (cadr s-input-l) NIL))
                c-input-l))
                ; tutta sta roba se la classe ha solo methods e il genitore ha fields e methods
                ((and (= (length (car s-input-l)) 1) (null (cdr s-input-l))
                (null (cadr (remove-and-find-matches (car s-parent-l) (car s-input-l)))) 
                (null (cadr (remove-and-find-matches (car s-parent-l) (cdr s-input-l)))) 
                (= (count-elements (car s-parent-l)) (count-elements 
                (car (remove-and-find-matches (car s-parent-l) (car s-input-l))))))
                  (parts-to-be-inherited (cddr s-input-l) 
                    (cons (field-remover NIL (car s-parent-l))
                      (method-replacer s-input-l (cdr s-parent-l))) 
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


;;; FIELD-REMOVER
(defun field-remover (new-list old-list)
  (controllo-tipo (cadr (remove-and-find-matches old-list new-list)))
  (car (remove-and-find-matches old-list new-list)))

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
      (error "type of field ~s is a supertype of inherited type." (caar list))))) ;;;;;;;;;; MIGLIORA FRASE ERRORE

;;; METHOD-REPLACER
(defun method-replacer (new-list old-list)
  (car (remove-and-find-matches old-list new-list)))


;;; CHECK-TYPE-IN-SUBCLASS
; controllo se il typeClass e' una classe e se il Supertipo e' contenuto nei genitori della classe typeClass
(defun check-type-in-subclass (value typeClass typeSuper)
  (cond ((equal typeClass T)
          (if (typep value typeSuper) 
            T
          (error "value ~s for field X is not of type ~s" value typeSuper))) 
          ((is-class typeClass)
            (if ((member typeSuper (car (get-class-spec typeClass))) 
            T
            (error "value ~s for field X is not of type ~s" (value) (typeClass)))))
        ((if (subtypep typeClass typeSuper) 
          (if (typep value typeClass)
          T
          (error "value ~s for field X is not of type ~s" (value) (typeClass)))))
        (t NIL)))       

;************* ISTANZE ************

;;; MAKE 
(defun make (class-name &rest field-value)
  (cond ((null (is-class class-name))
	 (error "La classe che si desidera istanziare non e' stata
definita in precedenza."))
	((not (evenp (list-length field-value)))
	 (error "Gli argomenti sono in numero dispari."))
	((not (<= (list-length field-value)
		  (* 2 (list-length (caadr (get-class-spec
					   class-name))))))
	 (error "Il numero di field-value passati eccede il numero di
field-value della classe da istanziare."))
	((has-duplicates field-value)
	 (error "Sono presenti uno o piu' field-name duplicati."))
	((null (valid-field-check (make-inherit-from-p 
    (cond ((null (process-field-make (form-couples field-value))) nil)
          ((not (null (process-field-make (form-couples field-value))))
            (list (process-field-make (form-couples field-value)))))
    (list class-name))  
				 (cadr (get-class-spec class-name))))
	 (error "Uno o piu' field-value non validi."))
	(t (cons 'oolinst (cons class-name (make-inherit-from-p
					    (list (process-field-make (form-couples field-value)))
					    (list class-name)))))))

;;; MAKE-INHERIT-FROM-P
(defun make-inherit-from-p (s-input-l parents-l)
  (if (null parents-l)
      s-input-l
    (make-inherit-from-p
     (make-parts-to-be-inherited s-input-l
			    (cadr (get-class-spec (car parents-l)))
			    s-input-l)
     (cdr parents-l))))

;;; MAKE-PARTS-TO-BE-INHERITED
(defun make-parts-to-be-inherited (s-input-l s-parent-l c-input-l)
  (if (null s-input-l)
    (if (or (null s-parent-l) (null (car s-parent-l)))
        c-input-l
      (if (= (count-elements (caar s-parent-l)) 2)
        (append (cons (car c-input-l)  
        (list (append (cdr c-input-l) (cdr (list s-parent-l))))))

        (append (cons (append (car c-input-l) (car s-parent-l))
        (list (append (cdr c-input-l) (cadr s-parent-l)))))

        (append (cons (append (car c-input-l) (car s-parent-l))
        (append (cdr c-input-l) (cdr s-parent-l))))))
    (if (null s-parent-l)
	    c-input-l
      (make-parts-to-be-inherited (cddr s-input-l) 
       (list (field-remover (car s-input-l)   
       (car s-parent-l))
       (car (method-replacer (cadr s-input-l) (cdr s-parent-l)))) 
       c-input-l))))

;;; FORM-COUPLES
(defun form-couples (field-l)
    (if (null field-l)
    NIL
    (if (and (symbolp (car field-l)) (= (length field-l) 2)) 
        (list field-l)
        (cons (list (first field-l) (second field-l)) (form-couples (cddr field-l))))))

;;; PROCESS-FIELD-MAKE
(defun process-field-make (field-l)        
	(if (null field-l) 
      NIL
      (if (= (length (car field-l)) 2)
         (cons
            (cond ((= (list-length (car field-l)) 2)
    		        (cons (caar field-l)
    		    	  (cons (cadar field-l) T)))
              ((= (list-length (car field-l)) 3)
               (if (check-types-in-field (cadar field-l) (cddar field-l))
                    (cons (caar field-l) (cons (cadar field-l) (cddar field-l)))
            (error "Il valore indicato non rispetta il tipo specificato."))))
          (process-field-make (cdr field-l))))))


;;; VALID-FIELD-CHECK
; risponde T se pairs-l e' NULL quindi se lo abbiamo controllato tutto 
(defun valid-field-check (pairs-l class-field-l)
  (if (and (null (caar (remove-and-find-matches (car pairs-l) (car class-field-l)))) 
        (null (controllo-tipo (cadr (remove-and-find-matches (car pairs-l) (car class-field-l))))))
      T
      NIL))


;;; IS-THERE-FIELD 
;controlla che (NAME "EVE" . T) sia presente nella lista del genitore (((NAME "EVE" . T) (AGE 21 . INTEGER))) 
(defun is-there-field (field-name s-parent-l)
  (if (null s-parent-l)
      NIL
    (if (equal field-name (caar s-parent-l))
	    T
      (is-there-field field-name (cdr s-parent-l)))))


;;; HAS-DUPLICATES	
(defun has-duplicates (field-value-l)
  (cond ((null field-value-l) nil)
        ((member (car field-value-l) (cddr field-value-l)) t)
        (t (has-duplicates (cddr field-value-l)))))


;;; RETRIEVE-PARENTS
(defun retrieve-parents (parent)
  (if (null parent)
      NIL
    (append (car (get-class-spec parent))
	    (retrieve-parents (caar (get-class-spec parent))))))


;;; IS-INSTANCE
(defun is-instance (value &optional (class-name T))
  (if (and (listp value)
	   (>= (list-length value) 2)
	   (symbolp class-name)
	   (eql (car value) 'oolinst)
	   (not (null (is-class (second value))))
	   (valid-field-check (cddr value)
			     (cadr (get-class-spec (second value)))))
      (cond ((eql (second value) class-name) T)
      ((null (eql class-name T))
	     (not (null (member class-name
				(retrieve-parents (second value))))))
	    ((eql class-name T) T))
    (error "Value non e' un'istanza o class-name non rispetta il
formato atteso in input per is-instance.")))

;************ FIELD **************
;;; FIELD
(defun field (instance field-name)
  (if (and (not (null instance))
	     (not (null field-name))
	     (is-instance instance)
	     (symbolp field-name))
        (if (not (null (get-field (caddr instance) field-name)))
	         (cadr (get-field (caddr instance) field-name))
	         (error "field-name o metodo non trovato nell'istanza."))
        (error "Parametri in input per field non validi.")))


;;; GET-FIELD
(defun get-field (field-pairs field-name)
  (if (null field-pairs)
      NIL
    (if (equal (caar field-pairs)
	       field-name)
	(cons field-name
	      (cdr (car field-pairs)))
      (get-field (cdr field-pairs) field-name))))

;************** FIELD* **************
;;; FIELD*
(defun field* (instance &rest field-name-l)
  (get-field-w-list instance field-name-l))					

;;; GET-FIELD-W-LIST				
(defun get-field-w-list (instance field-name-l)
  (cond ((null field-name-l)
	 (error "Lista di attributi non puo' essere vuota."))
	((= (list-length field-name-l) 1)
	 (if (symbolp instance)
	     (field (eval instance) (car field-name-l))
	   (field instance (car field-name-l))))
	(t (if (symbolp instance)
	       (get-field-w-list (field (eval instance)
				    (car field-name-l))
				(cdr field-name-l))
	     (get-field-w-list (field instance (car field-name-l))
			      (cdr field-name-l))))))

;************** METODI **************
;;; PROCESS-METHOD	 
(defun process-method (method-name method-spec)
  (setf (fdefinition method-name)
	(lambda (this &rest arglist)
	  (apply (method-finder this method-name)
		 (append (list this) arglist))))
  (eval (rewrite-method method-spec)))


;;; METHOD-FINDER
(defun method-finder (instance method-name)
  (if (and (not (null instance))
	     (not (null method-name))
	     (is-instance instance)
	     (symbolp method-name))
        (if (not (null (get-field (cadddr instance) method-name)))
	         (cdr (get-field (cadddr instance) method-name))
	         (error "no method for field ~s found." method-name))
        (error "Parametri in input per method non validi.")))


;;; REWRITE-METHOD
(defun rewrite-method (method-spec) 
    (cons 'lambda 
        (cons (append (list 'this) (car method-spec)) 
              (cdr method-spec))))

;;;; end of file -- Gesu.lisp