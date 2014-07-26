;;; a lambda-man that shouts its current position
;;; and goes right.
	DUM  2        ; 2 top-level declarations
	LDC  2        ; declare constant down
	LDF  step     ; declare function step 
	LDF  init     ; init function
	RAP  2        	; load declarations into environment and run init
	RTN   	        ; final return
init:
	LD   1 0      ; world		 
	CDR	      ; (ManState, ...)	 
	CAR	      ; ManState	 
	CDR	      ; ((manX,manY)	,)
	CAR	      ; (manX,manY)	
	DBUG	      ;	           
	LD   1 0      ; world		 
	CDR	      ; (ManState, ...)	 
	CAR	      ; ManState	 
	CDR	      ; ((manX,manY)	,)
	CAR	      ; (manX,manY)	
	LD   0 1      ; var step
	CONS
	RTN           ; return ((x,y), step)
step:
	LDC  17
	LD   0 1      ; world		 
	CDR	      ; (ManState, ...)	 
	CAR	      ; ManState	 
	CDR	      ; ((manX,manY)	,)
	CAR	      ; (manX,manY)
	DBUG
	LD   0 1      ; world		 
	CDR	      ; (ManState, ...)	 
	CAR	      ; ManState	 
	CDR	      ; ((manX,manY)	,)
	CAR	      ; (manX,manY)
	CAR	      ; manX
	CGT	      ; 17 > manX
	TSEL mada mou
mada:	
	LD   0 0      ; state
	LDC  1        ; var right
	CONS
	RTN           ; return (s+1, down)
mou:	
	LD   0 0      ; state
	LDC  0        ; var up
	CONS
	RTN           ; return (s+1, down)
