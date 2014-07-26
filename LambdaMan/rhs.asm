init:
        LDC 28
        LDF step
        CONS
        RTN

step:
        ;; _, state -> (_, move)
        LD 0 0
        LD 0 1
        LDF decide_move
        AP 1
        CONS
        RTN

decide_move:
        ;; state -> move
        LD 0 0
        LDC 283
        LDC 283
        LDC 283
        LDF decide_move_body
        AP 4
        RTN
decide_move_body:
        ;; state, [dir], [(x, y)], [(ax, ay)] -> move
        LD 0 0
        LDF get_dir
        AP 1
        LDC 1
        ADD
        LDF mod4
        AP 1
        ST 0 1
        
        LD 0 0
        LDF get_pos
        AP 1
        ST 0 2
decide_move_retry:
        LD 0 2
        LD 0 1
        LDF move_adj
        AP 2
        ST 0 3

        LD 0 0
        LD 0 3
        LDF index_map
        AP 2

        TSEL decide_move_empty decide_move_wall
decide_move_empty:
        LD 0 1
        RTN
decide_move_wall:
        LD 0 1
        LDC 3
        ADD
        LDF mod4
        AP 1
        ST 0 1
        LDC 283
        TSEL decide_move_retry decide_move_retry

move_adj:
        ;; (x, y), dir -> (ax, ay)
        LD 0 1
        LDC 0
        CEQ
        TSEL move_adj_north move_adj_not_north
move_adj_north:
        ;; y -= 1
        LD 0 0
        CAR
        LD 0 0
        CDR
        LDC 1
        SUB
        CONS
        RTN
move_adj_not_north:     
        LD 0 1
        LDC 1
        CEQ
        TSEL move_adj_east move_adj_not_east
move_adj_east:
        ;; x += 1
        LD 0 0
        CAR
        LDC 1
        ADD
        LD 0 0
        CDR
        CONS
        RTN
move_adj_not_east:     
        LD 0 1
        LDC 2
        CEQ
        TSEL move_adj_south move_adj_west
move_adj_south: 
        ;; y += 1
        LD 0 0
        CAR
        LD 0 0
        CDR
        LDC 1
        ADD
        CONS
        RTN
move_adj_west:
        ;; x -= 1
        LD 0 0
        CAR
        LDC 1
        SUB
        LD 0 0
        CDR
        CONS
        RTN

mod4:
        ;; n -> n % 4
        LDC 4
        LD 0 0
        CGT
        TSEL mod4_end mod4_sub
mod4_sub:
        LD 0 0
        LDC 4
        SUB
        ST 0 0
        LDC 283
        TSEL mod4 mod4
mod4_end:
        LD 0 0
        RTN
        

index_map:
        ;; state, (x, y) -> cell
        LD 0 0
        LDC 0
        LDF index
        AP 2

        LD 0 1
        CDR
        LDF index
        AP 2

        LD 0 1
        CAR
        LDF index
        AP 2

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
