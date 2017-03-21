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
       (display (if (eqv? (interpret (caar testPrograms)) (cadar testPrograms)) "PASSED" "FAILED"))
       (newline)
       (if (eqv? (interpret (caar testPrograms)) (cadar testPrograms)) (testInterpreter (cdr testPrograms) (+ passed 1) failed) (testInterpreter (cdr testPrograms) passed (+ failed 1)))))))

; Shorthand for testing
(define test
  (lambda ()
    (testInterpreter testPrograms 0 0)))

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
    (interpreter (parser fd) '((() ())))))

; ------------------------------------------------------------------------------
; interpreter
; inputs:
;  pt - parse tree
;  s - state
; outputs:
;  The return value of the code and a state
; ------------------------------------------------------------------------------
; ------------------------------------------------------------------------------
; ABSTRACTIONS
; ------------------------------------------------------------------------------
(define getRemainingStatements (lambda (pt) (cdr pt)))
(define getFirstOperation (lambda (pt) (caar pt)))
(define getOperands (lambda (pt) (cdar pt)))
(define getFirstOperand (lambda (pt) (cadar pt)))
(define getSecondPlusOperands (lambda (pt) (cddar pt)))
(define getThirdPlusOperands (lambda (pt) (cdddar pt)))
(define getThirdOperand (lambda (pt) (car (getThirdPlusOperands pt))))
(define getSecondOperand (lambda (pt) (caddar pt)))



(define interpreter
  (lambda (pt s)
    (cond
      ((null? pt) s)
      ((null? (getFirstOperation pt)) (interpreter (getRemainingStatements pt) s))
      ((eqv? (getFirstOperation pt) 'var) (interpreter (getRemainingStatements pt) (decVal (getFirstOperand pt) (car (m_eval (if (null? (getSecondPlusOperands pt)) (getSecondPlusOperands pt) (getSecondOperand pt)) s)) (cdr (m_eval (if (null? (getSecondPlusOperands pt)) (getSecondPlusOperands pt) (getSecondOperand pt)) s))))) 
      ((eqv? (getFirstOperation pt) '=) (interpreter (getRemainingStatements pt) (m_assign (getOperands pt) s)))  ; if "="
      ((eqv? (getFirstOperation pt) 'return) (if (boolean? (car (m_eval (getFirstOperand pt) s))) (if (car (m_eval (getFirstOperand pt) s)) 'true 'false) (car (m_eval (getFirstOperand pt) s)))) ; if "return"
      ((eqv? (getFirstOperation pt) 'if) (interpreter (getRemainingStatements pt) (m_if (getFirstOperand pt) (getSecondOperand pt) (if (null? (getThirdPlusOperands pt)) '() (getThirdOperand pt)) s)))  ; if "if"
      ((eqv? (getFirstOperation pt) 'while) (interpreter (getRemainingStatements pt) (m_while (getFirstOperand pt) (getSecondOperand pt) s)))  ; if "while"
      (else (error "interpreter ERROR: Invalid statement.")))))

; ------------------------------------------------------------------------------
; m_eval - evaluates an expression
; inputs:
;  st - statement
;  s - state
; outputs:
;  Returns the value of the expression as well as an updated state
; ------------------------------------------------------------------------------
; ------------------------------------------------------------------------------
; ABSTRACTIONS
; ------------------------------------------------------------------------------
(define getStOperator (lambda (st) (car st)))
(define getStFirstOperand (lambda (st) (cadr st)))
(define getStSecondOperand (lambda (st) (caddr st)))
(define getStRemainingOperands (lambda (st) (cddr st)))

(define m_eval
  (lambda (st s)
    (cond
      ((null? st) (cons '() s))
      ((eqv? st 'true) (cons #t s))
      ((eqv? st 'false) (cons #f s))
      ((atom? st) (if (or (eqv? (getVal st s) 'NULL) (null? (getVal st s))) (error "VAR ERROR: Variable used before declaration or assignment.") (cons (getVal st s) s)))
      ((eqv? (getStOperator st) '+) (cons (+ (car (m_eval (getStFirstOperand st) s)) (car (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s))))) (cdr (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s))))))
      ((eqv? (getStOperator st) '-)
       (if (null? (getStRemainingOperands st)) (cons (- (car (m_eval (getStFirstOperand st) s))) (cdr (m_eval (getStFirstOperand st) s))) (cons (- (car (m_eval (getStFirstOperand st) s)) (car (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s))))) (cdr (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s)))))))
      ((eqv? (getStOperator st) '*) (cons (* (car (m_eval (getStFirstOperand st) s)) (car (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s))))) (cdr (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s))))))
      ((eqv? (getStOperator st) '/) (cons (floor (/ (car (m_eval (getStFirstOperand st) s)) (car (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s)))))) (cdr (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s))))))
      ((eqv? (getStOperator st) '%) (cons (modulo (car (m_eval (getStFirstOperand st) s)) (car (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s))))) (cdr (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s))))))
      ((eqv? (getStOperator st) '==) (cons (eqv? (car (m_eval (getStFirstOperand st) s))  (car (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s))))) (cdr (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s))))))
      ((eqv? (getStOperator st) '!=) (cons (not (eqv? (car (m_eval (getStFirstOperand st) s))  (car (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s)))))) (cdr (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s))))))
      ((eqv? (getStOperator st) '>) (cons (> (car (m_eval (getStFirstOperand st) s))  (car (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s))))) (cdr (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s))))))
      ((eqv? (getStOperator st) '>=) (cons (>= (car (m_eval (getStFirstOperand st) s))  (car (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s))))) (cdr (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s))))))
      ((eqv? (getStOperator st) '<) (cons (< (car (m_eval (getStFirstOperand st) s))  (car (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s))))) (cdr (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s))))))
      ((eqv? (getStOperator st) '<=) (cons (<= (car (m_eval (getStFirstOperand st) s))  (car (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s))))) (cdr (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s))))))
      ((eqv? (getStOperator st) '!) (cons (not (car (m_eval (getStFirstOperand st) s))) (cdr (m_eval (getStFirstOperand st) s))))
      ((eqv? (getStOperator st) '&&) (cons (and (car (m_eval (getStFirstOperand st) s))  (car (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s))))) (cdr (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s))))))
      ((eqv? (getStOperator st) '||) (cons (or (car (m_eval (getStFirstOperand st) s))  (car (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s))))) (cdr (m_eval (getStSecondOperand st) (cdr (m_eval (getStFirstOperand st) s))))))
      (else (error "ERROR: Unknown operator/statement.")) )))

; ------------------------------------------------------------------------------
; m_assign - handles an assigment statement
; inputs:
;  st - statement
;  s - state
; outputs:
;  The updated state
; ------------------------------------------------------------------------------
; ------------------------------------------------------------------------------
; ABSTRACTIONS
; ------------------------------------------------------------------------------
(define getVar (lambda (st) (car st)))

(define m_assign
  (lambda (st s)
    (setVal (getVar st) (car (m_eval (getStFirstOperand st) s)) (cdr (m_eval (getStFirstOperand st) s))) ))

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
      ((not (nameAvailable name (caar state))) (error "DECVAL NAMESPACE ERROR: Namespace for var already occupied."))
      (else
       ; add name and value to state
       (cons (cons (cons name (caar state)) (cons (cons value (cadar state)) '())) (cdr state))))))

; Check to see if this variable is already defined on this layer of the state. That would be illegal. However, if the variable is declared on a previous layer,
; it can legally be redeclared on this layer.
(define nameAvailable
  (lambda (name varsLayer)
    (cond
      ((null? varsLayer) #t)
      ((eqv? (car varsLayer) name) #f)
      (else (nameAvailable name (cdr varsLayer))))))

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
      ((null? state) (error "SETVAL ERROR: Variable not found!"))
      ((eqv? #f (setVal* name value (car state))) (cons (car state) (setVal name value (cdr state))))
      (else (cons (setVal* name value (car state)) (cdr state))))))

(define setVal*
  (lambda (name value state)
    (cond
      ; if the names or values of states are null, error
      ((and (null? (car state)) (null? (cadr state))) #f)
      ; if it finds the var, set var 
      ((eqv? name (caar state)) (cons (car state) (cons (cons value (cdadr state)) '())))    
      ; else recurse on the next state value 
      (else (cons (cons (caar state) (car (setValRec name value state))) (cons (cons (caadr state) (cadr (setValRec name value state))) '()))) )))

; helper to shorten recursive line
(define setValRec
  (lambda (name value state)
    (setVal* name value (cons (cdar state) (cons (cdadr state) '()))) ))

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
      ((null? state) 'NULL)
      ((or (integer? name) (boolean? name)) name)
      (else
       (if (eqv? (getVal* name (caar state) (cadar state)) 'NULL) (getVal name (cdr state)) (getVal* name (caar state) (cadar state)))))))

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