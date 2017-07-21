(load "unification.lsp")

; Example query:
; (Knows John x)		Who does John know?

; Example sentences (facts):
; (Knows John Jane)		John knows Jane
; (Knows y Bill)		Everyone knows Bill
; (Knows y (Mother y))	Someone knows Mother of someone
; (Knows x Elizabeth)	Everyone knows Elizabeth

(setq s1 '(Knows John Jane))
(setq s2 '(Knows y Bill))
(setq s3 '(Knows y (Mother y)))
(setq s4 '(Knows x Elizabeth))

;  More examples:

(setq p1 '(Greedy x))
(setq p2 '(Evil x))
(setq p3 '(King John))
(setq p4 '(Evil Greedy King))


;  Horn-clause resolution
(defun hc-resolution (qry KB)
	(let ((query qry)
		  (counter 0))
		(loop until (or (null query)
						(> counter 10))
			  do	(incf counter)
					(setq query (resolve-with-all-facts query KB))
					(print counter)
					(print-sentence query)
		)
		query
	)
)

;  Print sentence to console
(defun print-sentence (sen)
	(format t "~{~a~^ ~}" sen)
)

;  Resolve query with all facts in KB
(defun resolve-with-all-facts (qry KB)
	(let ((query qry))
		(dolist (rule KB)
				(setq query (resolve query rule))
		)
		query
	)
)

;  Resolve:  negated query + Horn clause
(defun resolve (qry rule)
	(let ((sub-list (unify (cadr qry) (car rule) nil)))
		(if (eq sub-list 'failure)
			qry
			(change-vars
				(if (null sub-list)
					(already-identical-merge qry rule)
					(already-identical-merge (make-subs sub-list qry)
											 (make-subs sub-list rule)
					)
				)
			)
		)
	)
)

;  Find variables in query
(defun query-vars (qry)
	(let ((var-list nil))
		(dolist (x qry)
				(unless (isNot x)
						(dolist (y x)
								(when (and (isVariable y)
										   (not (assoc y var-list))
									  )
									  (push (cons y (gentemp (string y))) var-list)
								)
						)
				)
		)
		var-list
	)
)

;  Change variable names
(defun change-vars (qry)
	(let ((var-list (query-vars qry)))
		(mapcar #'(lambda (x) (if (isNot x)
							  x
							  (mapcar #'(lambda (y) (if (isVariable y)
														(cdr (assoc y var-list))
														y
													)
										)
										x
							  )
						  )
				)
				qry
		)
	)
)

;  Merge query with definite clause if first sentences are identical
(defun already-identical-merge (qry hrn)
	(append (cddr qry) (cdr hrn))
)

;  Returns definite clause with substitutions made
(defun make-subs (sub-list hrn)
	(mapcar #'(lambda (x) (if (isNot x)
							  x
							  (mapcar #'(lambda (y) (if (assoc y sub-list)
														(cdr (assoc y sub-list))
														y
													)
										)
										x
							  )
						   )
			  )
			  hrn
	)
)

