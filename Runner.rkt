#lang racket
(require "Utility.rkt")
(require "Parser.txt")
(define resolve
  (lambda (environment varname)
    (cond
      ((null? environment) #false)
      ((equal? (caar environment) varname) (cadar environment))
      (else (resolve (cdr environment) varname))
      )
    )
  )
(define resolve_scope
  (lambda (scope varname)
    (cond
      ((null? scope) #false)
      ((equal? (caar scope) varname) (cadar scope))
      (else (resolve_scope (cdr scope) (varnmae))
            )
      )
    )
  )
(define push_scope_to_env
  (lambda (list-of-varname list-of-value env)
    (let ((new_scope (extend-scope list-of-varname list-of-value '()))
          (pop_off_env (pop_env_to_global_scope env)))
      (cons new_scope pop_off_env)
    (pop_env_to_global_scope env)
    )
  )

(define pop_env_to_global_scope
  (lambda (env)
    ((null? env) #false)
    ((equal? (length env) 1)
     (if (equal? (car env) 'global) env
         #false))
    ((else (pop_env_to_global_scope (cdr env)))
     )
    )
  )
         

      
(define resolve_env
  (lambda (enviornment varname)
    (cond
      ((null? enviornment) #false)
      ((null? (car enviornment)) (resolve_env (cdr environment) varname))
      ((equal? 'global (car (car environment))) (resolve_scope (cdr (car environment)) varname)
      (else (let ((resolved_result (resolve_scope (car environment))))
              (if (equal? resolved_result #false)
                  (resolve_env (cdr environment) varname)
                  resolved_result
                  )
              )
            )
      )
      )
    )
(define extend-env
  (lambda (list-of-varname list-of-value env) ;((x y z) (1 2 3) env)
    (cond
      ((null? list-of-varname) env)
      ((null? list-of-value) env)
      (else (extend-env (cdr list-of-varname) (cdr list-of-value)
       (cons (list (car list-of-varname)
                   (car list-of-value))
             env)))
      )
    )
  )
(define run-let-exp
  (lambda (parsed-code env)
    (let ((list_of_names getVarnames (elementAt parsed-code 1))
          (list-of-values getValues (elementAt parsed-code 1))
          (new_env (extend-env list-of-names list-of-values env))
          (body (elementAt parsed-code 2))
          (run-neo-parsed-code parsed-code new-env)
          )
      )



      
(define run-neo-parsed-code
  (lambda (parsed-code env)
    (cond
      ((null? parsed-code) '())
      ((equal? (car parsed-code) 'num-exp)
       (cadr parsed-code));(num-exp 22)
      ((equal? (car parsed-code) 'var-exp)
       (resolve env (cadr parsed-code)))
      ;(bool-exp op (neo-exp) (neo-exp))
      ((equal? (car parsed-code) 'bool-exp)
       (run-bool-exp (cadr parsed-code)
                     (run-neo-parsed-code (caddr parsed-code) env)
                     (run-neo-parsed-code (cadddr parsed-code) env)))
      ;(math-exp op (neo-exp) (neo-exp))
      ((equal? (car parsed-code) 'math-exp)
       (run-math-exp (cadr parsed-code)
                     (run-neo-parsed-code (caddr parsed-code) env)
                     (run-neo-parsed-code (cadddr parsed-code) env)))
      ((equal? (car parsed-code) 'ask-exp)
       (if (run-neo-parsed-code (cadr parsed-code) env)
           (run-neo-parsed-code (caddr parsed-code) env)
           (run-neo-parsed-code (cadddr parsed-code) env)))
      ((equal? (car parsed-code) 'func-exp)
       (run-neo-parsed-code (cadr (caddr parsed-code)) env))
      ((equal? (car parsed-code) 'let-exp)
       (run-let-exp parsed-code)
      (else (run-neo-parsed-code
             (cadr parsed-code)
             (push_sope_to_env (cadr (cadr (cadr parsed-code)))
                               (map (lambda (exp) (run-neo-parsed-code exp env)) (caddr parsed-code))
                               env
                               )
             
             )
            )
      )
    ) 
  )