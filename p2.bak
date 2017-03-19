; Programming Project, Part 2
; Ryan Rose, rtr29 | Ben Moore, bcm45 | Aaron Magid, ahm64

; Load the parser and lexical analyzer
(load "simpleParser.scm")


; ------------------------------------------------------------------------------
; test
; ------------------------------------------------------------------------------
(define testPrograms '(("Test1.txt" 150)("Test2.txt" -4)("Test3.txt" 10)("Test4.txt" 16)("Test5.txt" 220)("Test6.txt" 5)("Test7.txt" 6)("Test8.txt" 10)("Test9.txt" 5)("Test10.txt" -39)("Test15.txt" true)("Test16.txt" 100)("Test17.txt" false)("Test18.txt" true)("Test19.txt" 128)("Test20.txt" 12)))

(define testInterpreter
  (lambda (testPrograms passed failed)
    (cond
      ((null? testPrograms) (display-all "Passed: " passed " - " "Failed: " failed))
      (else 
       (display (caar testPrograms))
       (display " - ")
       (display (if (eqv? (interpreter (caar testPrograms)) (cadar testPrograms)) "PASSED" "FAILED"))
       (newline)
       (if (eqv? (interpreter (caar testPrograms)) (cadar testPrograms)) (testInterpreter (cdr testPrograms) (+ passed 1) failed) (testInterpreter (cdr testPrograms) passed (+ failed 1)))))))

(define (display-all . vs)
  (for-each display vs))

; ------------------------------------------------------------------------------
; interpret - the primary call to interpret a file
; inputs:
;  fd - file name of code to be interpreted
; outputs:
;  The evaluation of the code
; ------------------------------------------------------------------------------
(define interpret
  (lambda (fd)
    (interpret (parser fd) '(() ())) ))

; ------------------------------------------------------------------------------
; interpreter
; inputs:
;  pt - parse tree
;  s - state
; outputs:
;  The return value of the code and a state
; ------------------------------------------------------------------------------
(define interpreter
  (lambda (pt s)
    (cond
      ((null? pt) s)
      ((null? (caar pt)) (interpreter (cdr pt) s))
      ((eqv? (caar pt) 'var) (interpreter (cdr pt) (decVal (cadar pt) (car (m_eval (if (null? (cddar pt)) (cddar pt) (caddar pt)) s)) (cdr (m_eval (if (null? (cddar pt)) (cddar pt) (caddar pt)) s))))) 
      ((eqv? (caar pt) '=) (interpreter (cdr pt) (m_assign (cdar pt) s)))  ; if "="
      ((eqv? (caar pt) 'return) (if (boolean? (car (m_eval (cadar pt) s))) (if (car (m_eval (cadar pt) s)) 'true 'false) (car (m_eval (cadar pt) s))))                                                                        ; if "return"
      ((eqv? (caar pt) 'if) (interpreter (cdr pt) (m_if (cadar pt) (caddar pt) (if (null? (cdddar pt)) '() (car (cdddar pt))) s)))  ; if "if"
      ((eqv? (caar pt) 'while) (interpreter (cdr pt) (m_while (cadar pt) (caddar pt) s)))  ; if "while"
      (else (error "interpreter ERROR: Invalid statement.")))))

; ------------------------------------------------------------------------------
; m_eval - evaluates an expression
; inputs:
;  pt - parse tree
;  s - state
; outputs:
;  Returns the value of the expression as well as an updated state
; ------------------------------------------------------------------------------
(define m_eval
  (lambda (pt s)
    (cond
      ((null? pt) (cons '() s))
      ((eqv? pt 'true) (cons #t s))
      ((eqv? pt 'false) (cons #f s))
      ((atom? pt) (if (or (eqv? (getVal pt s) 'NULL) (null? (getVal pt s))) (error "VAR ERROR: Variable used before declaration or assignment.") (cons (getVal pt s) s)))
      ((eqv? (car pt) '+) (cons (+ (car (m_eval (cadr pt) s)) (car (m_eval (caddr pt) (cdr (m_eval (cadr pt) s))))) (cdr (m_eval (caddr pt) (cdr (m_eval (cadr pt) s))))))
      ((eqv? (car pt) '-)
       (if (null? (cddr pt)) (cons (- (car (m_eval (cadr pt) s))) (cdr (m_eval (cadr pt) s))) (cons (- (car (m_eval (cadr pt) s)) (car (m_eval (caddr pt) (cdr (m_eval (cadr pt) s))))) (cdr (m_eval (caddr pt) (cdr (m_eval (cadr pt) s)))))))
      ((eqv? (car pt) '*) (cons (* (car (m_eval (cadr pt) s)) (car (m_eval (caddr pt) (cdr (m_eval (cadr pt) s))))) (cdr (m_eval (caddr pt) (cdr (m_eval (cadr pt) s))))))
      ((eqv? (car pt) '/) (cons (floor (/ (car (m_eval (cadr pt) s)) (car (m_eval (caddr pt) (cdr (m_eval (cadr pt) s)))))) (cdr (m_eval (caddr pt) (cdr (m_eval (cadr pt) s))))))
      ((eqv? (car pt) '%) (cons (modulo (car (m_eval (cadr pt) s)) (car (m_eval (caddr pt) (cdr (m_eval (cadr pt) s))))) (cdr (m_eval (caddr pt) (cdr (m_eval (cadr pt) s))))))
      ((eqv? (car pt) '==) (cons (eqv? (car (m_eval (cadr pt) s))  (car (m_eval (caddr pt) (cdr (m_eval (cadr pt) s))))) (cdr (m_eval (caddr pt) (cdr (m_eval (cadr pt) s))))))
      ((eqv? (car pt) '!=) (cons (not (eqv? (car (m_eval (cadr pt) s))  (car (m_eval (caddr pt) (cdr (m_eval (cadr pt) s)))))) (cdr (m_eval (caddr pt) (cdr (m_eval (cadr pt) s))))))
      ((eqv? (car pt) '>) (cons (> (car (m_eval (cadr pt) s))  (car (m_eval (caddr pt) (cdr (m_eval (cadr pt) s))))) (cdr (m_eval (caddr pt) (cdr (m_eval (cadr pt) s))))))
      ((eqv? (car pt) '>=) (cons (>= (car (m_eval (cadr pt) s))  (car (m_eval (caddr pt) (cdr (m_eval (cadr pt) s))))) (cdr (m_eval (caddr pt) (cdr (m_eval (cadr pt) s))))))
      ((eqv? (car pt) '<) (cons (< (car (m_eval (cadr pt) s))  (car (m_eval (caddr pt) (cdr (m_eval (cadr pt) s))))) (cdr (m_eval (caddr pt) (cdr (m_eval (cadr pt) s))))))
      ((eqv? (car pt) '<=) (cons (<= (car (m_eval (cadr pt) s))  (car (m_eval (caddr pt) (cdr (m_eval (cadr pt) s))))) (cdr (m_eval (caddr pt) (cdr (m_eval (cadr pt) s))))))
      ((eqv? (car pt) '!) (cons (not (car (m_eval (cadr pt) s))) (cdr (m_eval (cadr pt) s))))
      ((eqv? (car pt) '&&) (cons (and (car (m_eval (cadr pt) s))  (car (m_eval (caddr pt) (cdr (m_eval (cadr pt) s))))) (cdr (m_eval (caddr pt) (cdr (m_eval (cadr pt) s))))))
      ((eqv? (car pt) '||) (cons (or (car (m_eval (cadr pt) s))  (car (m_eval (caddr pt) (cdr (m_eval (cadr pt) s))))) (cdr (m_eval (caddr pt) (cdr (m_eval (cadr pt) s))))))
      (else (error "ERROR: Unknown operator/statement.")) )))

; ------------------------------------------------------------------------------
; m_assign - handles an assigment statement
; inputs:
;  pt - parse tree
;  s - state
; outputs:
;  The updated state
; ------------------------------------------------------------------------------
(define m_assign
  (lambda (pt s)
    (setVal (car pt) (car (m_eval (cadr pt) s)) (cdr (m_eval (cadr pt) s))) ))

; ------------------------------------------------------------------------------
; m_if - handles a conditional block
; inputs:
;  condition - The condition on which to run the block
;  ifblock - The block to run if condition is true
;  elseblock - The block to run if condition is false (optional)
;  state - The state before the condition is evaluated
; outputs:
;  The final state after evaluating the condition and, if applicable, running the block
; ------------------------------------------------------------------------------
(define m_if
  (lambda (condition ifblock elseblock state)
    (cond
      ((null? condition) (error "CONDITION ERROR: Condition cannot be null."))
      ((null? ifblock) (error "CONDITION ERROR: Block cannot be null."))
      ((null? state) (error "CONDITION ERROR: State cannot be null."))
      ((car (m_eval condition state)) (interpreter (cons ifblock '()) (cdr (m_eval condition state))))
      (else (if (null? elseblock) (cdr (m_eval condition state)) (interpreter (cons elseblock '()) (cdr (m_eval condition state))))))))
      
; ------------------------------------------------------------------------------
; decVal - declares and initializes a variable
; inputs:
;  name - variable name
;  value - variable value
;  state - the current state
; outputs:
;  The updated state
; ------------------------------------------------------------------------------
(define decVal 
  (lambda (name value state)
    (cond
      ; if name is null, error
      ((null? name) (error "DECVAL ERROR: Failed adding variable to state."))
      ; if the var name already exists, error
      ((not (eqv? (getVal name state) 'NULL)) (error "DECVAL NAMESPACE ERROR: Namespace for var already occupied."))
      (else
       ; add name and value to state
       (cons (cons name (car state)) (cons (cons value (cadr state)) '()) )))))

; ------------------------------------------------------------------------------
; setVal - sets the value of an initialized variable
; inputs:
;  name - variable name
;  value - variable value
;  state - the current state
; outputs:
;  The updated state
; ------------------------------------------------------------------------------
(define setVal
  (lambda (name value state)
    (cond
      ; if the names or values of states are null, error
      ((and (null? (car state)) (null? (cadr state))) (error "SETVAL ERROR: Variable not found."))
      ; if it finds the var, set var 
      ((eqv? name (caar state)) (cons (car state) (cons (cons value (cdadr state)) '())))    
      ; else recurse on the next state value 
      (else (cons (cons (caar state) (car (setValRec name value state))) (cons (cons (caadr state) (cadr (setValRec name value state))) '()))) )))

; helper to shorten recursive line
(define setValRec
  (lambda (name value state)
    (setVal name value (cons (cdar state) (cons (cdadr state) '()))) ))

; ------------------------------------------------------------------------------
; getVal - wrapper method for getVal* to deconstruct state variable as necessary
; inputs:
;  name - the name of the variable to find
;  state - the state to look in
; outputs:
;  See return values for getVal*
; ------------------------------------------------------------------------------
(define getVal
  (lambda (name state)
    (cond
      ((null? name) (error "GETVAL ERROR: Name cannot be null."))
      ((null? state) (error "GETVAL ERROR: State cannot be null."))
      ((or (integer? name) (boolean? name)) name)
      (else (getVal* name (car state) (cadr state))))))

; ------------------------------------------------------------------------------
; getVal* - gets the value of a given variable
; inputs:
;  name - the name of the variable to find
;  vars - the list of variable names from the current state
;  vals - the list of values in the current state
; outputs:
;  Value of variable, if initialized
;  '() if defined but not initialized
;  NULL if not defined
; ------------------------------------------------------------------------------
(define getVal*
  (lambda (name vars vals)
    (cond
      ((and (null? vars) (null? vals)) 'NULL)
      ((and (not (null? vars)) (not (null? vals)))
       (if (eqv? name (car vars)) (car vals) (getVal* name (cdr vars) (cdr vals))))
      (else (error "STATE MISMATCH ERROR: Different number of Variables and Values.")))))

; ------------------------------------------------------------------------------
; m_while - handles a WHILE loop
; inputs:
;  condition - The condition on which to run the block
;  block - The block to run if condition is true
;  state - The state before the condition is evaluated
; outputs:
;  The final state after the condition evaluates to false
; ------------------------------------------------------------------------------
(define m_while
  (lambda (condition block state)
    (cond
      ((null? condition) (error "LOOP ERROR: Condition cannot be null."))
      ((null? block) (error "LOOP ERROR: Block cannot be null."))
      ((null? state) (error "LOOP ERROR: State cannot be null."))
      ((car (m_eval condition state)) (m_while condition block (interpreter (cons block '()) (cdr (m_eval condition state)))) )
      (else (cdr (m_eval condition state))))))

; ------------------------------------------------------------------------------
; atom?
; ------------------------------------------------------------------------------
(define atom?
  (lambda (x)
    (and (not (pair? x)) (not (null? x))) ))