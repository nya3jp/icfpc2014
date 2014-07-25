LDC - load constant
--------------------

Synopsis: load an immediate literal;
          push it onto the data stack
Syntax:  LDC $n
Example: LDC 3
Effect:
  %s := PUSH(SET_TAG(TAG_INT,$n),%s)
  %c := %c+1


LD - load from environment
--------------------

Synopsis: load a value from the environment;
          push it onto the data stack
Syntax:  LD $n $i
Example: LD 0 1
Effect:
  $fp := %e
  while $n > 0 do            ; follow chain of frames to get n'th frame
  begin
    $fp := FRAME_PARENT($fp)
    $n := $n-1
  end
  $v := FRAME_VALUE($fp, $i) ; i'th element of frame
  %s := PUSH($v,%s)          ; push onto the data stack
  %c := %c+1
Notes:
  Values within a frame are indexed from 0.


ADD - integer addition
--------------------

Synopsis: pop two integers off the data stack;
          push their sum
Syntax: ADD
Effect:
  $y,%s := POP(%s)
  $x,%s := POP(%s)
  if TAG($x) != TAG_INT then FAULT(TAG_MISMATCH)
  if TAG($y) != TAG_INT then FAULT(TAG_MISMATCH)
  $z := $x + $y
  %s := PUSH(SET_TAG(TAG_INT,$z),%s)
  %c := %c+1


SUB - integer subtraction
--------------------

Synopsis: pop two integers off the data stack;
          push the result of subtracting one from the other
Syntax: SUB
Effect:
  $y,%s := POP(%s)
  $x,%s := POP(%s)
  if TAG($x) != TAG_INT then FAULT(TAG_MISMATCH)
  if TAG($y) != TAG_INT then FAULT(TAG_MISMATCH)
  $z := $x - $y
  %s := PUSH(SET_TAG(TAG_INT,$z),%s)
  %c := %c+1


MUL - integer multiplication
--------------------

Synopsis: pop two integers off the data stack;
          push their product
Syntax: MUL
Effect:
  $y,%s := POP(%s)
  $x,%s := POP(%s)
  if TAG($x) != TAG_INT then FAULT(TAG_MISMATCH)
  if TAG($y) != TAG_INT then FAULT(TAG_MISMATCH)
  $z := $x * $y
  %s := PUSH(SET_TAG(TAG_INT,$z),%s)
  %c := %c+1


DIV - integer division
--------------------

Synopsis: pop two integers off the data stack;
          push the result of the integer division of one of the other
Syntax: DIV
Effect:
  $y,%s := POP(%s)
  $x,%s := POP(%s)
  if TAG($x) != TAG_INT then FAULT(TAG_MISMATCH)
  if TAG($y) != TAG_INT then FAULT(TAG_MISMATCH)
  $z := $x / $y
  %s := PUSH(SET_TAG(TAG_INT,$z),%s)
  %c := %c+1


CEQ - compare equal
--------------------

Synopsis: pop two integers off the data stack;
          test if they are equal;
          push the result of the test
Syntax: CEQ
Effect:
  $y,%s := POP(%s)
  $x,%s := POP(%s)
  if TAG($x) != TAG_INT then FAULT(TAG_MISMATCH)
  if TAG($y) != TAG_INT then FAULT(TAG_MISMATCH)
  if $x == $y then
    $z := 1
  else
    $z := 0
  %s := PUSH(SET_TAG(TAG_INT,$z),%s)
  %c := %c+1


CGT - compare greater than
--------------------

Synopsis: pop two integers off the data stack;
          test if the first is strictly greater than the second;
          push the result of the test
Syntax: CGT
Effect:
  $y,%s := POP(%s)
  $x,%s := POP(%s)
  if TAG($x) != TAG_INT then FAULT(TAG_MISMATCH)
  if TAG($y) != TAG_INT then FAULT(TAG_MISMATCH)
  if $x > $y then
    $z := 1
  else
    $z := 0
  %s := PUSH(SET_TAG(TAG_INT,$z),%s)
  %c := %c+1


CGTE - compare greater than or equal
--------------------

Synopsis: pop two integers off the data stack;
          test if the first is greater than or equal to the second;
          push the result of the test
Syntax: CGTE
Effect:
  $y,%s := POP(%s)
  $x,%s := POP(%s)
  if TAG($x) != TAG_INT then FAULT(TAG_MISMATCH)
  if TAG($y) != TAG_INT then FAULT(TAG_MISMATCH)
  if $x >= $y then
    $z := 1
  else
    $z := 0
  %s := PUSH(SET_TAG(TAG_INT,$z),%s)
  %c := %c+1


ATOM - test if value is an integer
--------------------

Synopsis: pop a value off the data stack;
          test the value tag to see if it is an int;
          push the result of the test
Syntax: ATOM
Effect:
  $x,%s := POP(%s)
  if TAG($x) == TAG_INT then
    $y := 1
  else
    $y := 0
  %s := PUSH(SET_TAG(TAG_INT,$y),%s)
  %c := %c+1


CONS - allocate a CONS cell
--------------------

Synopsis: pop two values off the data stack;
          allocate a fresh CONS cell;
          fill it with the two values;
          push the pointer to the CONS cell
Syntax: CONS
Effect:
  $y,%s := POP(%s)
  $x,%s := POP(%s)
  $z := ALLOC_CONS($x,$y)
  %s := PUSH(SET_TAG(TAG_CONS,$z),%s)
  %c := %c+1


CAR - extract first element from CONS cell
--------------------

Synopsis: pop a pointer to a CONS cell off the data stack;
          extract the first element of the CONS;
          push it onto the data stack
Syntax: CAR
Effect:
  $x,%s := POP(%s)
  if TAG($x) != TAG_CONS then FAULT(TAG_MISMATCH)
  $y := CAR($x)
  %s := PUSH($y,%s)
  %c := %c+1


CDR - extract second element from CONS cell
--------------------

Synopsis: pop a pointer to a CONS cell off the data stack;
          extract the second element of the CONS;
          push it onto the data stack
Syntax: CDR
Effect:
  $x,%s := POP(%s)
  if TAG($x) != TAG_CONS then FAULT(TAG_MISMATCH)
  $y := CDR($x)
  %s := PUSH($y,%s)
  %c := %c+1


SEL - conditional branch

Synopsis: pop an integer off the data stack;
          test if it is non-zero;
          push the return address to the control stack;
          jump to the true address or to the false address
Syntax:  SEL $t $f
Example: SEL 335 346  ; absolute instruction addresses
Effect:
  $x,%s := POP(%s)
  if TAG($x) != TAG_INT then FAULT(TAG_MISMATCH)
  %d := PUSH(SET_TAG(TAG_JOIN,%c+1),%d)   ; save the return address
  if $x == 0 then
    %c := $f
  else
    %c := $t


JOIN - return from branch

Synopsis: pop a return address off the control stack, branch to that address
Syntax:  JOIN
Effect:
  $x,%d := POP(%d)
  if TAG($x) != TAG_JOIN then FAULT(CONTROL_MISMATCH)
  %c := $x


LDF - load function

Synopsis: allocate a fresh CLOSURE cell;
          fill it with the literal code address and the current
            environment frame pointer;
          push the pointer to the CLOSURE cell onto the data stack
Syntax:  LDF $f
Example: LDF 634      ; absolute instruction addresses
Effect:
  $x := ALLOC_CLOSURE($f,%e)
  %s := PUSH(SET_TAG(TAG_CLOSURE,$x),%s)
  %c := %c+1


AP - call function

Synopsis: pop a pointer to a CLOSURE cell off the data stack;
          allocate an environment frame of size $n;
          set the frame's parent to be the environment frame pointer
            from the CLOSURE cell;
          fill the frame's body with $n values from the data stack;
          save the stack pointer, environment pointer and return address
            to the control stack;
          set the current environment frame pointer to the new frame;
          jump to the code address from the CLOSURE cell;
Syntax:  AP $n
Example: AP 3      ; number of arguments to copy
Effect:
  $x,%s := POP(%s)            ; get and examine function closure
  if TAG($x) != TAG_CLOSURE then FAULT(TAG_MISMATCH)
  $f := CAR_CLOSURE($x)
  $e := CDR_CLOSURE($x)
  $fp := ALLOC_FRAME($n)      ; create a new frame for the call
  FRAME_PARENT($fp) := $e
  $i := $n-1
  while $i != -1 do           ; copy n values from the stack into the frame in reverse order
  begin
    $y,%s := POP(%s)
    FRAME_VALUE($fp,$i) := $y
    $i := $i-1
  end
  %d := PUSH(%e,%d)                     ; save frame pointer
  %d := PUSH(SET_TAG(TAG_RET,%c+1),%d)  ; save return address
  %e := $fp                             ; establish new environment
  %c := $f                              ; jump to function


RTN - return from function call

Synopsis: pop a stack pointer, return address and environment frame
            pointer off of the control stack;
          restore the stack and environment;
          jump to the return address
Syntax:  RTN
Effect:
  $x,%d := POP(%d)            ; pop return address
  if TAG($x) == TAG_STOP then MACHINE_STOP
  if TAG($x) != TAG_RET then FAULT(CONTROL_MISMATCH)
  $y,%d := POP(%d)            ; pop frame pointer
  %e := $y                    ; restore environment
  %c := $x                    ; jump to return address
Notes:
  Standard ABI convention is to leave the function return value on the
  top of the data stack. Multiple return values on the stack is possible,
  but not used in the standard ABI.

  The latest hardware revision optimizes the deallocation of the 
  environment frame. If the environment has not been captured by LDF
  (directly or indirectly) then it can be immediately deallocated.
  Otherwise it is left for GC.


DUM - create empty environment frame

Synopsis: Prepare an empty frame;
          push it onto the environment chain;
Syntax:  DUM $n
Example: DUM 3      ; size of frame to allocate
Effect:
  $fp := ALLOC_FRAME($n)       ; create a new empty frame of size $n
  FRAME_PARENT($fp) := %e      ; set its parent frame
  %e := SET_TAG(TAG_DUM,$fp)   ; set it as the new environment frame
  %c := %c+1
Notes:
  To be used with RAP to fill in the frame body.


RAP - recursive environment call function

Synopsis: pop a pointer to a CLOSURE cell off the data stack;
          the current environment frame pointer must point to an empty
            frame of size $n;
          fill the empty frame's body with $n values from the data stack;
          save the stack pointer, parent pointer of the current environment
             frame and return address to the control stack;
          set the current environment frame pointer to the environment
            frame pointer from the CLOSURE cell;
          jump to the code address from the CLOSURE cell;
Syntax:  RAP $n
Example: RAP 3      ; number of arguments to copy
Effect:
  $x,%s := POP(%s)            ; get and examine function closure
  if TAG($x) != TAG_CLOSURE then FAULT(TAG_MISMATCH)
  $f := CAR_CLOSURE($x)
  $fp := CDR_CLOSURE($x)
  if TAG($fp) != TAG_DUM then FAULT(FRAME_MISMATCH)
  if FRAME_SIZE($fp) != $n then FAULT(FRAME_MISMATCH)
  $i := $n-1
  while $i != -1 do           ; copy n values from the stack into the empty frame in reverse order
  begin
    $y,%s := POP(%s)
    FRAME_VALUE($fp,$i) := $y
    $i := $i-1
  end
  $fpp := FRAME_PARENT($fp)
  %d := PUSH($fpp,%d)                   ; save frame pointer
  %d := PUSH(SET_TAG(TAG_RET,%c+1),%d)  ; save return address
  %e := $fp                             ; establish new environment
  %c := $f                              ; jump to function


STOP - terminate co-processor execution

Synopsis: terminate co-processor execution and signal the main proessor.
Syntax:  STOP
Effect:
  MACHINE_STOP
Notes:
  This instruction is no longer part of the standard ABI. The standard ABI
  calling convention is to use a TAG_STOP control stack entry. See RTN.
Tail call extensions

TSEL - tail-call conditional branch

Synopsis: pop an integer off the data stack;
          test if it is non-zero;
          jump to the true address or to the false address
Syntax:  TSEL $t $f
Example: TSEL 335 346  ; absolute instruction addresses
Effect:
  $x,%s := POP(%s)
  if TAG($x) != TAG_INT then FAULT(TAG_MISMATCH)
  if $x == 0 then
    %c := $f
  else
    %c := $t
Notes:
  This instruction is the same as SEL but it does not push a return address


TAP - tail-call function

Synopsis: pop a pointer to a CLOSURE cell off the data stack;
          allocate an environment frame of size $n;
          set the frame's parent to be the environment frame pointer
            from the CLOSURE cell;
          fill the frame's body with $n values from the data stack;
          set the current environment frame pointer to the new frame;
          jump to the code address from the CLOSURE cell;
Syntax:  TAP $n
Example: TAP 3      ; number of arguments to copy
Effect:
  $x,%s := POP(%s)            ; get and examine function closure
  if TAG($x) != TAG_CLOSURE then FAULT(TAG_MISMATCH)
  $f := CAR_CLOSURE($x)
  $e := CDR_CLOSURE($x)
  $fp := ALLOC_FRAME($n)      ; create a new frame for the call
  FRAME_PARENT($fp) := $e
  $i := $n
  while $i != 0 do            ; copy n values from the stack into the frame in reverse order
  begin
    $y,%s := POP(%s)
    FRAME_VALUE($fp,$i) := $y
    $i := $i-1
  end
  %e := $fp                   ; establish new environment
  %c := $f                    ; jump to function
Notes:
  This instruction is the same as AP but it does not push a return address

  The latest hardware revision optimizes the case where the environment
  frame has not been captured by LDF and the number of args $n in the
  call fit within the current frame. In this case it will overwrite the
  frame rather than allocating a fresh one.

TRAP - recursive environment tail-call function

Synopsis: pop a pointer to a CLOSURE cell off the data stack;
          the current environment frame pointer must point to an empty
            frame of size $n;
          fill the empty frame's body with $n values from the data stack;
          set the current environment frame pointer to the environment
            frame pointer from the CLOSURE cell;
          jump to the code address from the CLOSURE cell;
Syntax:  TRAP $n
Example: TRAP 3      ; number of arguments to copy
Effect:
  $x,%s := POP(%s)            ; get and examine function closure
  if TAG($x) != TAG_CLOSURE then FAULT(TAG_MISMATCH)
  $f := CAR_CLOSURE($x)
  $fp := CDR_CLOSURE($x)
  if TAG($fp) != TAG_DUM then FAULT(FRAME_MISMATCH)
  if FRAME_SIZE($fp) != $n then FAULT(FRAME_MISMATCH)
  $i := $n
  while $i != 0 do            ; copy n values from the stack into the empty frame in reverse order
  begin
    $y,%s := POP(%s)
    FRAME_VALUE($fp,$i) := $y
    $i := $i-1
  end
  $fpp := FRAME_PARENT($fp)
  %e := $fp                   ; establish new environment
  %c := $f                    ; jump to function
Notes:
  This instruction is the same as RAP but it does not push a return address
Pascal extensions

ST - store to environment

Synopsis: pop a value from the data stack and store to the environment
Syntax:  ST $n $i
Example: ST 0 1
Effect:
  $fp := %e
  while $n > 0 do            ; follow chain of frames to get n'th frame
  begin
    $fp := FRAME_PARENT($fp)
    $n := $n-1
  end
  $v,%s := POP(%s)           ; pop value from the data stack
  FRAME_VALUE($fp, $i) := $v ; modify i'th element of frame
  %c := %c+1
Debug extensions

DBUG - printf debugging

Synopsis: If tracing is enabled, suspend execution and raise a trace
          interrupt on the main processor. The main processor will read
          the value and resume co-processor execution. On resumption
          the value will be popped from the data stack. If tracing is not
          enabled the value is popped from the data stack and discarded.
Syntax:  DBUG
Effect:
  $x,%s := POP(%s)
  %c := %c+1
Notes:
  This is the formal effect on the state of the machine. It does
  also raise an interrupt but this has no effect on the machine state.

BRK - breakpoint debugging
