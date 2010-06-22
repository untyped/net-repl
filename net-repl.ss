#lang scheme/base

(require mzlib/thread
         scheme/contract
         scheme/tcp)

; (parameter natural)
(define net-repl-port (make-parameter 9876))

; (parameter (U natural #f))
(define net-repl-timeout (make-parameter #f))

; natural integer boolean -> tcp-listener
;
; Just like tcp-listen, but only listens on localhost for security.
(define (net-repl-listen port-k max-allow-wait reuse?)
  (tcp-listen port-k max-allow-wait reuse? "127.0.0.1"))

(define (net-repl-handler exn)
  (void))

; ec -> (U sexp stx compiled) -> any
;
; Evals the given expression, and returns it value.  Uses
; the current-eval parameter to eval all expressions,
; except the expression (close) which calls the given
; escape continuation
(define net-repl-eval
  (let ([eval (current-eval)])
    (lambda (exit)
      (lambda (expr)
        (if (equal?
             (if (syntax? expr)
                 (syntax->datum expr)
                 expr)
             '(#%top-interaction close))
            (exit)
            (eval expr))))))


; input-port output-port -> void
(define (net-repl-connection ip op)
  (let/ec exit
    (parameterize
        ([current-input-port ip]
         [current-output-port op]
         [current-error-port op]
         [current-eval (net-repl-eval exit)])
      (read-eval-print-loop)))
  (close-input-port ip)
  (close-output-port op))

; -> void
;
; Runs the net-repl is a new thread
(define (run-net-repl)
  (thread
   (lambda ()
     (run-server (net-repl-port)
                 net-repl-connection
                 (net-repl-timeout)
                 net-repl-handler
                 net-repl-listen))))

(provide/contract
 [net-repl-port    (parameter/c natural-number/c)]
 [net-repl-timeout (parameter/c (or/c natural-number/c #f))]
 [run-net-repl     (-> thread?)])

