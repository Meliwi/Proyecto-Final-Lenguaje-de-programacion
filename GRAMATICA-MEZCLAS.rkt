#lang eopl
;INTEGRANTES:
;1871074-MICHELLE GONZÁLEZ HERNÁNDEZ
;1832127-MELISSA GONZÁLEZ NEBRIJO
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Especificación léxica

(define lexica
  '((white-sp (whitespace) skip)
    (comment ("/*" (arbno (not #\newline)) "*/") skip) ;comentario basado en css
    (string ("'" letter (arbno (or letter digit)) "'") string);Cadena de caracateres basado en el uso de textos en Ruby 
    (identifier (letter (arbno (or letter digit "?" "!" "_"))) symbol)
    (number (digit (arbno digit)) number)
    (number ("-" digit (arbno digit)) number)
    (number (digit (arbno digit) "." digit (arbno digit)) number)
    (number ("-" digit (arbno digit) "." digit (arbno digit)) number)))

;Especificación gramatical

(define gramatica
  '(
    (program (expression) execute) ;término usado en el manejo de servidores en java
    (expression (number) num)
    (expression (string) string)
    (expression (identifier) tag); término usado en el manejo de los estados de los repositorios en git
    (expression ("true")true-exp)
    (expression ("false")false-exp)
    ;Primitivas:
    (expression ("(" expression primitive-binary expression ")") bin-primitive-exp)
    (expression (primitive-unary "("expression")") un-primitive-exp)
    (expression ("|(" expression primitive-binary-base expression ")")bin-primitive-base-exp)
    (expression (primitive-unary-base "(" expression ")") un-primitive-base-exp)
    (expression ("*(" expression primitive-binary-bool expression ")")bin-primitive-bool-exp)
    (expression (primitive-unary-bool "("expression ")")un-primitive-bool-exp)
    (expression (primitive-pred-list expression)predicate-primitive-list)
    (expression (primitive-binary-list "(" expression expression ")")bin-primitive-list-exp)
    (expression (primitive-unary-list expression)un-primitive-list-exp)
    (expression ("empty")primitive-empty-list)
    (expression (primitive-unary-string "("expression")")un-primitive-string-exp)
    (expression ("#(" expression primitive-binary-string expression ")")bin-primitive-string-exp)
    
    (expression ( "x32" "["(arbno number) "]") 32base)
    (expression ( "x16" "["  (arbno number) "]")hexa)
    (expression ( "x8""["  (arbno number) "]")oct)
    (exp ("const" identifier "=" expression)constant);declaración basada en javascript 
    (exp ("val-define" identifier "=" expression) unique-var-define); En kotlin se hace uso de "val" para constantes, aquí las usaremos
                                                                             ; para la variable unica. 
    (exp ("val-undefine" identifier) unique-var-undefine) ; Declaración de variables similar a la usada en Go 
    ;Condicionales:
    (expression ("if" "(" expression ")" "{" expression "}" "else" "{" expression "}")if-condition); condicional basado en C++

    ;Declaración:
    (expression
     ( "def" "(" (separated-list exp "~") ")" "{"expression"}") def-exp);término usado para declarar funciones en Ruby
    
    ;Procedimientos:
    (expression
     ("function" "(" (separated-list identifier "~") ")" "{" expression "}")function-exp);El término fue basado en el lenguaje Ada y la estructura del cuerpo en c#

    ;Asignación:
    (expression
     ("assign" identifier "=" expression) assign-exp) ;La estructura del cuerpo fue basado en la asignación de Ruby,pero se añadió la
                                                      ;el término assign para diferenciar a la producción de las otras. 
    
    ;Evaluar procedimiento:
    (expression
     ("[" expression (separated-list expression "~") "]")app-exp);Esta producción es similar al llamado de funciones en python

    ;Recusividad:
    (expression
     ("long" "(" (arbno "const" identifier "(" (separated-list identifier "~")")" "{" expression"}")")""<"expression">")long-exp)
    ;Declaración de funciones recursivas basada en scala

    ;Listas:
    (expression
     ("list" "<"(separated-list expression ",")">" )list-exp);Listas con implementación similar a la usada en c#

    ;Begin;
    (expression ("{-" expression (arbno ";" expression) "-}") seq-exp)
    
    ;primitivas para enteros
    (primitive-binary ("+") add-prim)
    (primitive-binary ("-") substract-prim)
    (primitive-binary ("*") mult-prim)
    (primitive-binary ("%") rest-prim)
    (primitive-binary ("/") div-prim)
    (primitive-unary ("add1") incr-prim)
    (primitive-unary ("sub1") decr-prim)

    ;primitivas para hexadecimales, octales y base32

    (primitive-binary-base ("+") add-prim-base)
    (primitive-binary-base ("-") substract-prim-base)
    (primitive-binary-base ("*")mult-prim-base)
    (primitive-unary-base ("|add1") incr-prim-base)
    (primitive-unary-base ("|sub1") decr-prim-base)
    
    ;primitivas sobre booleanos
    (primitive-binary-bool ("<") smaller-than-prim)
    (primitive-binary-bool (">") bigger-than-prim)
    (primitive-binary-bool ("<=")less-or-equal-prim)
    (primitive-binary-bool (">=")bigger-or-equal-prim)
    (primitive-binary-bool ("==")equal-prim)
    (primitive-binary-bool ("!=")different-prim)
    (primitive-binary-bool ("and")and-prim)
    (primitive-binary-bool ("or")or-prim)
    (primitive-unary-bool ("not") not-prim)

    ;primitivas para cadenas
    (primitive-unary-string ("length") length-prim)
    (primitive-binary-string ("#append")append-string-prim)
    
    ;primitivas para listas 
    (primitive-pred-list ("empty?") empty?-prim)
    (primitive-pred-list ("list?")list?-prim)
    (primitive-binary-list ("cons") cons-prim)
    (primitive-binary-list ("append") append-prim)
    (primitive-unary-list ("car") car-prim)
    (primitive-unary-list ("cdr")cdr-prim)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Construidos automáticamente

(sllgen:make-define-datatypes lexica gramatica)

(define show-the-datatypes
  (lambda () (sllgen:list-define-datatypes lexica gramatica)))

(sllgen:make-stream-parser lexica gramatica)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Parser y Scanner 

(define scan&parse
  (sllgen:make-string-parser lexica gramatica))

(define just-scan
  (sllgen:make-string-scanner lexica gramatica))

;El Interpretador (FrontEnd + Evaluación + señal para lectura )

(define interpretador
  (sllgen:make-rep-loop  "--> "
    (lambda (pgm) (eval-program  pgm)) 
    (sllgen:make-stream-parser 
      lexica
      gramatica)))

(define eval-program
  (lambda (pgm)
    (cases program pgm
      (execute (body)
                 (eval-expression body (empty-env))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Tipo de dato procval 
(define-datatype procval procval?
  (closure
   (ids (list-of symbol?))
   (body expression?)
   (env environment?)))

;apply-procedure: evalua el cuerpo de un procedimientos en el ambiente extendido correspondiente
(define apply-procedure
  (lambda (proc args)
    (cases procval proc
      (closure (ids body env)
               (eval-expression body (extend-env (make-const-list (length ids)) ids args env))))))

;make-const-list
(define make-const-list
  (lambda (n)
    (if (zero? n) empty (cons "const" (make-const-list (- n 1))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;evaluar-expresion:
;Evalua la expresión en el ambiente de entrada
(define eval-expression
  (lambda (exp env)
    (cases expression exp
      (num (number) number)
      (string (string) string)
      (tag (id) (apply-env env id))
      (true-exp ()#t)
      (false-exp () #f)
      (bin-primitive-exp (first-exp operation second-exp)
                (eval-ope-bin (eval-expression first-exp env)
                              operation
                              (eval-expression second-exp env)))
      (un-primitive-exp (operation expression)
                        (eval-ope-un operation (eval-expression expression env)))
      (bin-primitive-base-exp(first-exp operation second-exp)
                         (eval-ope-bin-base (eval-expression first-exp env)
                                            operation
                                            (eval-expression second-exp env)))
      (un-primitive-base-exp (operation expression)
                             (eval-ope-un-base operation (eval-expression expression env)))
      (bin-primitive-bool-exp(first-exp operation second-exp)
                             (eval-ope-bin-bool (eval-expression first-exp env)
                                                operation
                                                (eval-expression second-exp env)))
      (un-primitive-bool-exp(operation expression)
                            (eval-ope-un-bool (eval-expression expression env)))
      (predicate-primitive-list (operation expression)
                                (eval-pred-prim-list operation (eval-expression expression env)))
      (bin-primitive-list-exp (operation first-exp second-exp)
                (eval-bin-prim-list (eval-expression first-exp env)operation (eval-expression second-exp env)))
      (un-primitive-list-exp (operation expression)
                             (eval-un-prim-list operation (eval-expression expression)))
      (primitive-empty-list () empty)
      (un-primitive-string-exp (operation expression)
                               (eval-un-prim-string operation (eval-expression expression env)))
      (bin-primitive-string-exp (operation first-exp second-exp)
                                (eval-bin-prim-string (eval-expression first-exp env)
                                                      operation
                                                      (eval-expression second-exp env)))
      (32base (list-of-numbers)
              (eval-base list-of-numbers 32))
      (hexa (list-of-numbers)
            (eval-base list-of-numbers 16))
      (oct (list-of-numbers)
           (eval-base list-of-numbers 8))

      (if-condition (test-exp true-exp false-exp)
                    (if (eval-expression test-exp env)
                        (eval-expression true-exp env)
                        (eval-expression false-exp env)))

      (def-exp (defs body)
                 (let
                     ((consts (exp->const defs))
                       (ids (exp->ids defs))
                       (vals (eval-let-exp (exp->vals defs) env) ))
                     (eval-expression body (extend-env consts ids vals env))))

      (function-exp (ids body)
                            (closure ids body env))
      (assign-exp (id exp)
                (cond
                  [(and(not (equal? (apply-env env id) "#undefine#")) (equal? (apply-env-const env id) "unique-val"))
                    (begin
                      (setref!
                       (apply-env-ref env id)
                       (eval-expression exp env))
                      1)]
                   [(equal? (apply-env-const env id) "unique-val")(eopl:error 'eval-expression
                                                                      "Attempt to assign an already define unique-val ~s")]
                   [else (eopl:error 'eval-expression
                                                                      "Attempt to assign a constant ~s")]
                    )
                )
      (app-exp (exp exps)
                (let ((proc (eval-expression exp env))
                     (args (eval-rands exps env)))
                 (if (procval? proc)
                     (apply-procedure proc args)
                     (eopl:error 'eval-expression
                                 "Attempt to apply non-procedure ~s" proc))))
      
      (long-exp (proc-names idss bodies long-body)
                  (eval-expression long-body
                                   (extend-env-recursively "const" proc-names idss bodies env)))
      (list-exp (exps)
                (if (null? (cdr exps)) (list (eval-expression (car exps) env))
                    (cons (eval-expression (car exps) env) (eval-expression (list-exp(cdr exps)) env))))
      (seq-exp (exp exps)
               (let loop ((acc (eval-expression exp env))
                             (exps exps))
                    (if (null? exps) acc
                        (loop (eval-expression (car exps) env)
                              (cdr exps)))))
      )))

; Funciones auxiliares para aplicar eval-expression a cada elemento de una 
; Lista de operandos (expresiones)

;eval-rands:
(define eval-rands
  (lambda (rands env)
    (map (lambda (x) (eval-rand x env)) rands)))

(define eval-rand
  (lambda (rand env)
    (cases expression rand
      (tag (id)
               (indirect-target
                (let ((ref (apply-env-ref env id)))
                  (cases target (primitive-deref ref)
                    (direct-target (expval) ref)
                    (indirect-target (ref1) ref1)))))
      (else
       (direct-target (eval-expression rand env))))))

;eval-let-exp:
(define eval-let-exp
  (lambda (rands env)
    (map (lambda (x) (eval-let x env))rands)))

;eval-let-exp-rand:
(define eval-let
  (lambda (rand env)
    (direct-target (eval-expression rand env))))

;eval-ope-bin
;Evalua las operaciones binarias con los valores de entrada.

(define eval-ope-bin
  (lambda (f-val ope s-val)
    (cases primitive-binary ope
      (add-prim () (+ f-val s-val)) 
      (substract-prim () (- f-val s-val))
      (mult-prim () (* f-val s-val))
      (rest-prim () (modulo f-val s-val))
      (div-prim () (/ f-val s-val)))))

;eval-ope-un
;Evalua las operaciones unarias con los valores de entrada

(define eval-ope-un
  (lambda (ope val)
    (cases primitive-unary ope
      (incr-prim ()(+ val 1))
      (decr-prim ()(- val 1)))))


;eval-ope-bin-base
;Evalua las operasciones binarias con valores de distintas bases
(define eval-ope-bin-base
  (lambda (f-val ope s-val)
    (cases primitive-binary-base ope
      (add-prim-base () (+ f-val s-val))
      (substract-prim-base() (- f-val s-val))
      (mult-prim-base () (* f-val s-val)))))

;eval-ope-un-base
;Evalua las operaciones unarias con valores de distintas bases
(define eval-ope-un-base
  (lambda (ope val)
    (cases primitive-unary-base ope
      (incr-prim-base () (+ val 1))
      (decr-prim-base ()(- val 1)))))

;eval-ope-bin-bool
;Evalua las operaciones binarias con booleanos
(define eval-ope-bin-bool
  (lambda (f-val ope s-val)
    (cases primitive-binary-bool ope
      (smaller-than-prim ()(< f-val s-val))
      (bigger-than-prim() (> f-val s-val))
      (less-or-equal-prim () (<= f-val s-val))
      (bigger-or-equal-prim () (>= f-val s-val))
      (equal-prim () (equal? f-val s-val))
      (different-prim () (not (equal? f-val s-val)))
      (and-prim () (and f-val s-val))
      (or-prim () (or f-val s-val)))))
       
;eval-ope-un-bool
;Evalua las operaciones unarias con booleanos
(define eval-ope-un-bool
  (lambda (ope exp)
    (cases primitive-unary-bool ope
      (not-prim () (not exp)))))


;eval-pred-prim-list
;Evalua los predicados con listas
(define eval-pred-prim-list
  (lambda (pred l)
    (cases primitive-pred-list pred
      (empty?-prim () (null? l))
      (list?-prim () (list? l)))))

;eval-bin-prim-list
;Evalua las operaciones binarias con listas. 
(define eval-bin-prim-list
  (lambda (f-exp ope s-exp)
    (cases primitive-binary-list ope
      (cons-prim () (cons f-exp s-exp))
      (append-prim (append f-exp s-exp)))))

;eval-un-prim-list
;Evalúa las operaciones unarias con listas
(define eval-un-prim-list
  (lambda (ope l)
    (cases primitive-unary-list ope
      (car-prim () (car l))
      (cdr-prim () (cdr l)))))

;eval-bin-prim-string
;Evalúa la operación binaria con strings
(define eval-bin-prim-string
  (lambda (s1 ope s2)
    (cases primitive-binary-string ope
      (append-string-prim (string-append s1 s2)))))

;eval-un-prim-string
;Evalúa la operación unaria con strings

(define eval-un-prim-string
  (lambda (ope s)
    (cases primitive-unary-string ope
      (length-prim () (string-length s)))))

;eval-base:
;Hace la conversión de base

(define (eval-base id list-of-numbers)
  (if (null? list-of-numbers) 0
      (+ (car list-of-numbers) (* id (eval-base (cdr list-of-numbers))))))

;return-exp-const-type
(define return-exp-const-type
  (lambda (expression)
    (cases exp expression
      (constant (id exp)  "const")
      (unique-var-define (id exp) "unique-val")
      (unique-var-undefine (id) "unique-val" ))))

;exp-const
(define exp->const
  (lambda (exps)
    (if (null? exps) empty
        (cons (return-exp-const-type (car exps) ) (exp->const (cdr exps))))))

;funcion auxiliar exp->ids
(define return-exp-ids
  (lambda (expression)
    (cases exp expression
      (constant (id exp)id)
      (unique-var-define (id exp) id)
      (unique-var-undefine (id) id))))


;exp->ids
(define exp->ids
  (lambda (exps)
    (if (null? exps)empty
        (cons (return-exp-ids (car exps))(exp->ids (cdr exps))))))


;funcion auxiliar exp->vals
(define return-exp-vals
  (lambda (expression)
    (cases exp expression
      (constant (id exp)  exp)
      (unique-var-define (id exp) exp)
      (unique-var-undefine (id) (string "#undefine#") ))))

;exp-vals 
(define exp->vals
  (lambda (exps)
    (if (null? exps)empty
        (cons (return-exp-vals (car exps))(exp->vals (cdr exps))))))

;apply-env-const
(define apply-env-const
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (const syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (return-const const pos)
                                 (apply-env-const env sym)))))))

;return-cost
(define return-const
  (lambda (l pos)
    (if (equal? pos 0) (car l)
        (return-const (cdr l) (- pos 1)))))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Ambientes:

;Definición del tipo de dato ambiente
(define-datatype environment environment?
  (empty-env-record)
  (extended-env-record
   (const (list-of string?))
   (syms (list-of symbol?))
   (vec vector?)
   (env environment?)))

;Función que crea un ambiente vacio 
(define empty-env  
  (lambda ()
    (empty-env-record))) 

;extend-env
;Función que crea un ambiente extendido

(define extend-env
  (lambda (const syms vals env)
    (extended-env-record const syms (list->vector vals) env)))

;Función que crea un ambiente extendido para procedimientos recursivos
(define extend-env-recursively
  (lambda (const proc-names idss bodies old-env)
    (let ((len (length proc-names)))
      (let ((vec (make-vector len)))
        (let ((env (extended-env-record (make-const-list len) proc-names vec old-env)))
          (for-each
            (lambda (pos ids body)
              (vector-set! vec pos (direct-target (closure ids body env))))
            (iota len) idss bodies)
          env)))))

;iota: number -> list
;Función que retorna una lista de los números desde 0 hasta end
(define iota
  (lambda (end)
    (let loop ((next 0))
      (if (>= next end) '()
        (cons next (loop (+ 1 next)))))))

;Función que busca un símbolo en un ambiente
(define apply-env
  (lambda (env sym)
      (deref (apply-env-ref env sym))))

;apply-env-ref
(define apply-env-ref
  (lambda (env sym)
    (cases environment env
      (empty-env-record ()
                        (eopl:error 'apply-env-ref "No binding for ~s" sym))
      (extended-env-record (const syms vals env)
                           (let ((pos (rib-find-position sym syms)))
                             (if (number? pos)
                                 (a-ref pos vals)
                                 (apply-env-ref env sym)))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Definición tipos de datos referencia y blanco

(define-datatype target target?
  (direct-target (expval expval?))
  (indirect-target (ref ref-to-direct-target?)))

(define-datatype reference reference?
  (a-ref (position integer?)
         (vec vector?)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Blancos y referencias:

;expval? 
(define expval?
  (lambda (x)
    (or (number? x) (procval? x) (boolean? x) (string? x) (list? x))))

;ref-to-direct-target?
(define ref-to-direct-target?
  (lambda (x)
    (and (reference? x)
         (cases reference x
           (a-ref (pos vec)
                  (cases target (vector-ref vec pos)
                    (direct-target (v) #t)
                    (indirect-target (v) #f)))))))

;deref
(define deref
  (lambda (ref)
    (cases target (primitive-deref ref)
      (direct-target (expval) expval)
      (indirect-target (ref1)
                       (cases target (primitive-deref ref1)
                         (direct-target (expval) expval)
                         (indirect-target (p)
                                          (eopl:error 'deref
                                                      "Illegal reference: ~s" ref1)))))))
;primitive-deref
(define primitive-deref
  (lambda (ref)
    (cases reference ref
      (a-ref (pos vec)
             (vector-ref vec pos)))))
;setref!
(define setref!
  (lambda (ref expval)
    (let
        ((ref (cases target (primitive-deref ref)
                (direct-target (expval1) ref)
                (indirect-target (ref1) ref1))))
      (primitive-setref! ref (direct-target expval)))))

;primitive-setfref!
(define primitive-setref!
  (lambda (ref val)
    (cases reference ref
      (a-ref (pos vec)
             (vector-set! vec pos val)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Funciones Auxiliares

; Funciones auxiliares para encontrar la posición de un símbolo
; en la lista de símbolos de un ambiente

;rib-find-position
(define rib-find-position 
  (lambda (sym los)
    (list-find-position sym los)))

;list-find-position
(define list-find-position
  (lambda (sym los)
    (list-index (lambda (sym1) (eqv? sym1 sym)) los)))

;list-index
(define list-index
  (lambda (pred ls)
    (cond
      ((null? ls) #f)
      ((pred (car ls)) 0)
      (else (let ((list-index-r (list-index pred (cdr ls))))
              (if (number? list-index-r)
                (+ list-index-r 1)
                #f))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Pruebas:
;;;;;;Uso de declaración;;;;;;;;;

;Función utilizando condicional

(scan&parse "def (const x=9~const y=3){
 if(x) {(x*y)} else {(x+y)}}")

;Función que halla el area de un cuadrado

(scan&parse "def (val-define AreaCuadrado=
function(a~b){(a*b)})
{[AreaCuadrado 2~2]}")

;Función que halla el area de una esfera

(scan&parse "def (const pi=3.1416~ val-define AreaEsfera=
function(pi~r){((4* pi)* (r * r))})
{[AreaEsfera pi~2]}")

;;;;;;Uso de listas;;;;;;;;;;;;;;;;;;;
(scan&parse "def(const listaUno=list<'Hola'>~const listaDos = list <'amigos'>)
{append (listaUno listaDos)}")

(scan&parse "def( const lista1 = list< 'estudiante2' ,5,4 , x8[0 5 1]  >){ length( car lista1 ) }")

;;;;;;Uso de recursión;;;;;;;;;;;;;;

;Funcion factorial

(scan&parse " long (const factorial (n){
if (*(n > 0)) {(n * [factorial sub1(n)])}
else {1}}) <[factorial 6 ]>")

;Funcion que suma todos los números desde cero hasta el parámetro n.

(scan&parse "long (const suma(n){
if (*(n > 0)){(n + [suma sub1(n)])} else {0}})
<[suma 2]>")

