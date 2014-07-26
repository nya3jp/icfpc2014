init:
        LDC 28
        LDF step
        CONS
        RTN

step:
        LD 0 0
        LDC 1
        CONS
        RTN

get_pos:
        ;; state -> (x, y)
        LD 0 0
        LDC 1
        LDF index
        AP 2
        LDC 1
        LDF index
        AP 2
        RTN

get_dir:
        ;; state -> dir
        LD 0 0
        LDC 1
        LDF index
        AP 2
        LDC 2
        LDF index
        AP 2
        RTN

index:
        ;; array, index -> value
        LD 0 1
        TSEL index_nonzero index_zero
index_nonzero:
        LD 0 0
        CDR
        ST 0 0
        LD 0 1
        LDC 1
        SUB
        ST 0 1
        LDC 283
        TSEL index index
index_zero:
        LD 0 0
        CAR
        RTN
