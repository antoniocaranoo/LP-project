;;;; -*- Mode: Lisp -*- 
;;;; ool.lisp
; VERSIONE ESASME
;;;; Cantaluppi Camilla 894557
;;;; Carano Antonio 902447
;;;; Ladisa Roberto 899699

;;; HASH-TABLE A SUPPORTO DI DEF-CLASS
(defparameter *classes-specs* (make-hash-table))

;; Questa funzione aggiunge la specifica di una classe (class-spec) alla 
;; tabella hash *classes-specs* usando come chiave il nome della classe 
;; (name). 
;; e' utilizzata per memorizzare le informazioni delle classi definite.
(defun add-class-spec (name class-spec)
  (setf (gethash name *classes-specs*) class-spec))

;; Recupera la specifica di una classe dalla tabella hash *classes-specs* 
;; utilizzando il nome della classe (name) come chiave.
(defun get-class-spec (name)
  (gethash name *classes-specs*))

;; Definisce una nuova classe (class-name) con i genitori (parents) 
;; specificati e ulteriori attributi (part). Esegue vari controlli per 
;; assicurarsi che la classe sia definita correttamente, quindi aggiunge la 
;; sua specifica alla tabella hash *classes-specs*.
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

;; Verifica se un dato simbolo (class-name) e' una classe definita, 
;; controllando se esiste nella tabella hash *classes-specs*.
;;; IS-CLASS
(defun is-class (class-name)
  (if (symbolp class-name)
      (if (get-class-spec class-name)
	  T
	  NIL)
      NIL))

;; Verifica se tutti i genitori (parents) di una classe sono classi definite.
;;; CHECK-PARENTS
(defun check-parents (parents)
  (if (null parents)
      T
      (if (is-class (car parents))
	  (check-parents (cdr parents))
	  NIL)))

;; Costruisce l'elenco degli attributi (part-l) di una classe, elaborandoli 
;; uno per uno.
;;; BUILD-PARTS
(defun build-parts (part-l)
  (if (null part-l)
      NIL
      (cons (process-part (car part-l))
	    (build-parts (cdr part-l)))))

;; Elabora un singolo attributo di una classe, distinguendo tra metodi e 
;; campi.
;;; PROCESS-PART		
(defun process-part (input-l)
  (if (null input-l)
      (error "Part in input non e' una lista.")
      (if (equal 'methods (car input-l))
	  (mapcar 'process-methods-l (cdr input-l))
	  (if (equal 'fields (car input-l))
	      (mapcar 'process-field (cdr input-l))))))

;; Inverte l'ordine delle liste all'interno di un elenco di attributi, se 
;; necessario.
;;; REVERSE-LISTS
(defun reverse-lists (part)
  (if (eql (caar part) 'methods)
      (reverse part)
      part))

;; Elabora una lista di metodi.
;;; PROCESS-METHODS-LIST
(defun process-methods-l (methods-l)
  (cons (car methods-l)
	(process-method (car methods-l)
			(cdr methods-l))))

;; Elabora un campo (field), controllando la sua lunghezza e validita'.
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

;; Controlla se un dato valore corrisponde al tipo specificato per un campo.
;;; CHECK-TYPES-IN-FIELD
(defun check-types-in-field (value type)
  (if (is-class type)
	(if (is-instance (eval value) type)
	t 
	(error "value ~s for field written is not of type ~s" value type))
   (if (typep value type)
   t
   (error "value ~s for field written is not of type ~s" value type))))

;; Determina quali attributi una classe deve ereditare dai suoi genitori.
;;; INHERIT-FROM-P
(defun inherit-from-p (s-input-l parents-l)
  (if (null parents-l)
      s-input-l
      (inherit-from-p
       (parts-to-be-inherited s-input-l
			      (cadr (get-class-spec (car parents-l)))
			      s-input-l)
       (cdr parents-l))))

;; Aiuta nella determinazione degli attributi da ereditare, confrontando gli 
;; attributi della sottoclasse con quelli dei genitori.
;;; PARTS-TO-BE-INHERITED
(defun parts-to-be-inherited (s-input-l s-parent-l c-input-l)
  (if (null s-input-l)
      (if (null s-parent-l)
          c-input-l
	  (if (null (car s-parent-l))
              (append (cons (car c-input-l)  
			    (list (append (cadr c-input-l) (cadr s-parent-l)))))
              
              (if (null (cdr s-parent-l))
		  (append (cons (append (car c-input-l) (car s-parent-l))
				(cdr c-input-l)))
		  (if (and (not (= (count-elements (cdaar c-input-l)) 2)) (null       
									   (cdr c-input-l))
			   (null (cadr (remove-and-find-matches (car s-parent-l)
								(car c-input-l)))) 
			   (null (cadr (remove-and-find-matches (car s-parent-l) (cdr 
										  c-input-l)))) 
			   (= (count-elements (car s-parent-l)) (count-elements 
								 (car (remove-and-find-matches (car 
								 s-parent-l) (car 
														 c-input-l))))))
                      (append (cons (car s-parent-l)
				    (list (append (car c-input-l) (cadr s-parent-l)))))

                      (append (cons (append (car c-input-l) (car s-parent-l))
				    (list (append (cadr c-input-l) (cadr s-parent-l)))))
                      ))))

      (if (null s-parent-l)
	  c-input-l
          (cond ((= (count-elements (caar s-parent-l)) 2)
		 (parts-to-be-inherited (cddr s-input-l) 
					(cons (field-remover (car s-input-l)   
							     NIL)
					      (list (method-replacer (cadr s-input-l) (car 
						  s-parent-l))))
					c-input-l))

                ((and (= (count-elements (caar s-parent-l)) 3) (null (cdr 
				s-parent-l)))
                 (parts-to-be-inherited (cddr s-input-l) 
					(cons (field-remover (car s-input-l)   
							     (car s-parent-l))
					      (method-replacer (cadr s-input-l) NIL))
					c-input-l))
		
                ((and (= (count-elements (caar s-input-l)) 2) (null (cdr 
				s-input-l))
                      (null (cadr (remove-and-find-matches (car s-parent-l) 
					  (car s-input-l)))) 
                      (null (cadr (remove-and-find-matches (car s-parent-l) 
					  (cdr s-input-l)))) 
                      (= (count-elements (car s-parent-l)) (count-elements 
							    (car (remove-and-find-matches (car 
								s-parent-l) (car s-input-l))))))
                 (parts-to-be-inherited (cddr s-input-l) 
					(cons (field-remover NIL (car s-parent-l))
					      (method-replacer s-input-l (cdr s-parent-l))) 
					c-input-l))
                
                (t (parts-to-be-inherited (cddr s-input-l) 
					  (cons (field-remover (car s-input-l)   
							       (car s-parent-l))
						(list (method-replacer (cadr s-input-l) (cadr 
						s-parent-l))))
					  c-input-l))))))

;; Conta gli elementi in una lista o una coppia puntata.
;;; COUNT-ELEMENTS conta gli elementi di una lista
(defun count-elements (item)
  (cond ((null item) 0)  
        ((not (consp item)) 1)  
        ((null (cdr item)) 1)  
        ((not (consp (cdr item))) 2)  
        (t (+ 1 (count-elements (cdr item))))))  

;; Rimuove i campi gia' definiti in una lista di nuovi campi.
;;; FIELD-REMOVER
(defun field-remover (new-list old-list)
  (controllo-tipo (cadr (remove-and-find-matches old-list new-list)))
  (car (remove-and-find-matches old-list new-list)))

;; trova duplicati in una lista
;; restituisce una lista dove il car e' una lista con 
;; tutti gli elementi che appartengono alla prima lista ma 
;; NON appartengono alla seconda, e il cdr e' una lista 
;; contenenti tutte le coppie uguali 
;;; REMOVE-AND-FIND-MATCHES 
(defun remove-and-find-matches (list-a list-b)
  (match-helper list-a list-b nil nil))

(defun match-helper (list-a list-b acc matches)
  (if (null list-a)
      (list (reverse acc) (reverse matches))
      (let* ((current (first list-a))
             (rest (rest list-a))
             (match (find-if (lambda (item) (equal (first current) (first 
			 item))) list-b)))
        (if match
            (match-helper rest list-b acc (cons (list current match) matches))
            (match-helper rest list-b (cons current acc) matches)))))

;; ;prende in inpput una lista di coppie, ogni coppia e' 
;; formata da due liste ((name value type) (name value type))
;; Controlla i tipi di una lista di coppie di attributi.
;;; CONTROLLO-TIPO
(defun controllo-tipo (list)
  (if (null list)
      nil
      (if  (check-type-in-subclass (cadr (cadar list)) (cddr (cadar list)) 
	  (cddaar list))
	   (controllo-tipo (cdr list))
	   (error "type of field ~s is a supertype of inherited type." (caar 
	   list))))) 

;; Sostituisce i metodi esistenti in una lista di metodi.
;;; METHOD-REPLACER
(defun method-replacer (new-list old-list)
  (car (remove-and-find-matches old-list new-list)))

;; controllo se il typeClass e' una classe e se il 
;; Supertipo e' contenuto nei genitori della classe typeClass
;;; CHECK-TYPE-IN-SUBCLASS
(defun check-type-in-subclass (value typeClass typeSuper)
  (if (equal typeClass 'T)
	(if (typep value typeSuper) 
		T
	(error "value ~s for field X is not of type ~s" value typeSuper))
  
  	(if (is-class typeClass)
		(if (or (member typeSuper (car (get-class-spec typeClass))) (eql 
		typeClass typeSuper))
    		T
    		(error "value ~s for field X is not of type ~s" value typeSuper))

		(if (subtypep typeClass typeSuper)
			(if (typep value typeClass)
    			T
    		(error "value ~s for field X is not of type ~s" value typeClass))

			(error "value ~s for field X is not of type ~s" value typeClass)) 
			)))

;;************* ISTANZE ************

;; Crea un'istanza di una classe, controllando la validita' dei valori dei 
;; campi.
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
				   (cond ((null (process-field-make (form-couples 
				   field-value))) nil)
					 ((not (null (process-field-make (form-couples 
					 field-value))))
					  (list (process-field-make (form-couples field-value)))))
				   (list class-name))  
				  (cadr (get-class-spec class-name))))
	 (error "Uno o piu' field-value non validi."))
	(t (cons 'oolinst (cons class-name (make-inherit-from-p
					    (list (process-field-make (form-couples field-value)))
					    (list class-name)))))))

;; Determina gli attributi da includere in un'istanza, tenendo conto 
;; dell'ereditarieta'.
;;; MAKE-INHERIT-FROM-P
(defun make-inherit-from-p (s-input-l parents-l)
  (if (null parents-l)
      s-input-l
      (make-inherit-from-p
       (make-parts-to-be-inherited s-input-l
				   (cadr (get-class-spec (car parents-l)))
				   s-input-l)
       (cdr parents-l))))

;; Aiuta a determinare gli attributi da includere in un'istanza.
(defun make-parts-to-be-inherited (s-input-l s-parent-l c-input-l)
  (if (null s-input-l)
      (if (or (null s-parent-l) (null (car s-parent-l)))
          c-input-l
          (if (= (count-elements (caar s-parent-l)) 2)
              (append (cons (car c-input-l)
                            (list (append (cdr c-input-l) (cdr (list 
							s-parent-l))))))
              (append (cons (append (car c-input-l) (car s-parent-l))
                            (list (append (cdr c-input-l) (cadr 
							s-parent-l)))))))
      (if (null s-parent-l)
          c-input-l
          (make-parts-to-be-inherited (cddr s-input-l)
                                      (list (field-remover (car s-input-l) 
									  (car s-parent-l))
                                            (car (method-replacer (cadr 
											s-input-l) (cdr s-parent-l))))
                                      c-input-l))))

;; Forma coppie di nome-valore dai campi forniti.
;;; FORM-COUPLES
(defun form-couples (field-l)
  (if (null field-l)
      NIL
      (if (and (symbolp (car field-l)) (= (length field-l) 2)) 
          (list field-l)
          (cons (list (first field-l) (second field-l)) (form-couples (cddr 
		  field-l))))))

;; Elabora i campi durante la creazione di un'istanza.
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
                      (cons (caar field-l) (cons (cadar field-l) (cddar 
					  field-l)))
		      (error "Il valore indicato non rispetta il tipo specificato.
			  "))))
           (process-field-make (cdr field-l))))))

;; Verifica la validita' dei campi di un'istanza.
;;; VALID-FIELD-CHECK
(defun valid-field-check (pairs-l class-field-l)
  (if (and (null (caar (remove-and-find-matches (car pairs-l) (car 
  class-field-l)))) 
           (null (controllo-tipo (cadr (remove-and-find-matches (car pairs-l) 
		   (car class-field-l))))))
      T
      NIL))

;; Controlla se un campo specificato e' presente in una lista di campi.
;;; IS-THERE-FIELD 
(defun is-there-field (field-name s-parent-l)
  (if (null s-parent-l)
      NIL
      (if (equal field-name (caar s-parent-l))
	  T
	  (is-there-field field-name (cdr s-parent-l)))))

;; Verifica se ci sono duplicati in una lista di campi.
;;; HAS-DUPLICATES	
(defun has-duplicates (field-value-l)
  (cond ((null field-value-l) nil)
        ((member (car field-value-l) (cddr field-value-l)) t)
        (t (has-duplicates (cddr field-value-l)))))

;; Recupera i genitori di una classe.
;;; RETRIEVE-PARENTS
(defun retrieve-parents (parent)
  (if (null parent)
      NIL
      (append (car (get-class-spec parent))
	      (retrieve-parents (caar (get-class-spec parent))))))

;; Verifica se un valore e' un'istanza di una classe specificata.
;;; IS-INSTANCE
(defun is-instance (value &optional (class-name T))
  (if (and (list value)
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

;;************ FIELD **************

;; Recupera il valore di un campo da un'istanza.
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

;; Aiuta a recuperare un campo specifico da un'istanza.
;;; GET-FIELD
(defun get-field (field-pairs field-name)
  (if (null field-pairs)
      NIL
      (if (equal (caar field-pairs)
		 field-name)
	  (cons field-name
		(cdr (car field-pairs)))
	  (get-field (cdr field-pairs) field-name))))

;;************** FIELD* **************

;; Fornisce un modo per accedere a piu' campi di un'istanza 
;; contemporaneamente.
;;; FIELD*
(defun field* (instance &rest field-name-l)
  (get-field-w-list instance field-name-l))					

;; Aiuta field* a recuperare una lista di campi da un'istanza.
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

;;************** METODI **************

;; Elabora un metodo, collegandolo a un'istanza.
;;; PROCESS-METHOD	 
(defun process-method (method-name method-spec)
  (setf (fdefinition method-name)
	(lambda (this &rest arglist)
	  (apply (method-finder this method-name)
		 (append (list this) arglist))))
  (eval (rewrite-method method-spec)))

;; Trova un metodo in un'istanza.
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

;; Riscrive la specifica di un metodo per l'esecuzione.
;;; REWRITE-METHOD
(defun rewrite-method (method-spec) 
  (cons 'lambda 
        (cons (append (list 'this) (car method-spec)) 
              (cdr method-spec))))

;;;; end of file -- ool.lisp