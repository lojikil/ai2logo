; A simple test that took approximately 15 min to hack out
; test1 & beyond will be more realistic Logo systems...
(define (read-logo-list)
	(base-let (l (read-logo-expression))
		(cond
			((and (pair? l) (eq? (car l) 'EOL)) '())
			(else (cons l (read-logo-list))))))
(define (read-logo-string)
	(base-let (l (read-char))
		(cond
			((eq? l #\') '())
			(else (cons l (read-logo-string))))))
(define (numeric? c)
	(or (and (char>=? c #\0) (char<=? c #\9)) (char=? c #\.)))
(define (alphanumeric? c)
	(or 
		(and (char-ci>=? c #\a) (char-ci<=? c #\z))
		(and (char>=? c #\0) (char<=? c #\9)) 
		(char=? #\! c)
		(and (char>=? c #\#) (char<=? c #\&)) 
		(and (char>=? c #\*) (char<=? c #\/))
		(and (char>=? c #\^) (char<=? c #\`))
		(and (char>=? c #\{) (char<=? c #\~))))
(define (whitespace? c)
	(or
		(char=? c #\linefeed)
		(char=? c #\tab)
		(char=? c #\space)))
(define (read-logo-symbol)
	(base-let (l (read-char))
		(cond
			((not (alphanumeric? l)) '())
			(else (cons l (read-logo-symbol))))))
(define (read-logo-number)
	(base-let (l (read-char))
		(cond
			((not (numeric? l)) '())
			(else (cons l (read-logo-number))))))
(define (read-logo-expression)
	(base-let (r (read-char))
		(cond
			((eof-object? r) #e) ; if this is lower, causes a Bus Error. Investigate...
			((whitespace? r) (read-logo-expression))
			((eq? r #\[) ; list
				(read-logo-list))
			((eq? r #\") ; quote
				(cons 'quoted (apply string (read-logo-symbol))))
			((eq? r #\:) ; value of...
				(cons 'unquoted (apply string (read-logo-symbol))))
			((eq? r #\') ; string literal
				(apply string (read-logo-string)))
			((numeric? r)
				(coerce (apply string (cons r (read-logo-number))) :int))
			((alphanumeric? r) ; symbol of some sort
				(apply string (cons r (read-logo-symbol))))
			((eq? r #\]) ; end of list...
				'(EOL)))))
(define (logo-but-last lst)
	"remove everything but the last element"
	(if (null? (cdr lst))
		'()
		(cons (car lst) (logo-but-last (cdr lst)))))
(define (logo-env-set e a s)
	#f
	)
(define (logo-env-get e a)
	#f
	)
(define (logo-eval e) ; e == environment...
	(base-let (r (read-logo-expression))
		(cond
			((eof-object? r) 'quit)
			((eq? r "sum") (+ (logo-eval e) (logo-eval e)))
			((eq? r "mul") (* (logo-eval e) (logo-eval e)))
			((eq? r "sub") (- (logo-eval e) (logo-eval e)))
			((eq? r "div") (/ (logo-eval e) (logo-eval e)))
			((eq? r "sin") (sin (logo-eval e)))
			((eq? r "cos") (cos (logo-eval e)))
			((eq? r "tan") (tan (logo-eval e)))
			((eq? r "first") (car (logo-eval e)))
			((eq? r "butfirst") (cdr (logo-eval e)))
			((eq? r "butlast") (logo-but-last (logo-eval e)))
			((eq? r "quit") 'quit)
			((number? r) r)
			((pair? r) r))))
(define (logo-repl)
	(display (format "Welcome to Phantom Logo test0~%"))
	(base-let (sent #f)
		(while (not (eq? sent 'quit))
			(display "> ")
			(set! sent (logo-eval '()))
			(display sent)
			(newline))))
