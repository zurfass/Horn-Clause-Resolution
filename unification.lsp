; Algorithm to find substitutions to make two sentences identical.
; Variables take the form of a single char followed by any number of integers.
; Constants, predicates and functions are >1 chars in length.

(defun unify (x y theta)
	(if (eq theta 'failure)
	    'failure
	    (if (eq x y)
			theta
			(if (isVariable x)
		    	(unify-var x y theta)
		    	(if (isVariable y)
					(unify-var y x theta)
					(if (and (isCompound x) (isCompound y))
			    		(unify (cdr x) (cdr y) (unify (car x) (car y) theta))
			    		(if (and (isList x) (isList y))
							(unify (cdr x) (cdr y) (unify (car x) (car y) theta))
							'failure
			    		)
					)
		    	)
			)
	    )
	)
)

(defun unify-var (var x theta)
		(if (member var (mapcar #'car theta))
			(unify (cdr (assoc var theta)) x theta)
			(if (member x (mapcar #'cdr theta))
				(unify var (cdr (assoc x theta)) theta)
				(if (and (listp x) (member var x))
					'failure
					(push (cons var x) theta)
				)
			)
		)
)

; OCCUR-CHECK?
; Checks if variable symbol in one setence already occurs in knowledge-base.
(defun occur-check (var theta)
	(unless (eq theta nil)
		(member var (mapcar #'car theta))
	)
)

; Checks if the given symbol is a variable using the rules stated above.
; First make sure symbol is not a list then check its chars.

(defun isVariable (symbol)
	(if (listp symbol)
		NIL
		(if (alpha-char-p (char (string symbol) 0))
			(if (> (length (string symbol)) 1)
				(if (digit-char-p (char (string symbol) 1))
					(if (> (length (string symbol)) 2)
						(if (digit-char-p (char (string symbol) 2))
							T
							NIL
						)
						T
					)
					NIL
				)
				T
			)
			NIL
		)
	)
)

(defun isCompound (sentence)
	(listp sentence)
)

(defun isList (sentence)
	(listp sentence)
)

(defun isNot (symbol)
	(and (not (isList symbol))
		 (eq #\~ (char (string symbol) 0))
	)
)

