main:
  LDC 0  ; map
  LDF main_body
  AP 1
  RTN
main_body:
  LD 1 0  ; world
  LDF get_map
  AP 1
  LDF convert_2d_array
  AP 1
  ST 0 0  ; map
  LD 0 0  ; map
  LDF step
  CONS
  RTN
  RTN
step:
  LDC 0  ; dir
  LDC 0  ; x
  LDC 0  ; y
  LDF step_body
  AP 3
  RTN
step_body:
  LD 1 1  ; world
  LDF get_my_pos
  AP 1
  ST 0 2  ; y
  ST 0 1  ; x
  LD 1 0  ; map
  LD 0 2  ; y
  LD 0 1  ; x
  LDC 1
  LDF set_2d_array
  AP 4
  ST 1 0  ; map
  LD 1 0  ; map
  LD 1 1  ; world
  LDF decide
  AP 2
  ST 0 0  ; dir
  LD 0 0  ; dir
  DBUG
  LD 1 0  ; map
  LD 0 0  ; dir
  CONS
  RTN
  RTN
decide:
  LDC 0  ; best_dir
  LDC 0  ; best_score
  LDC 0  ; cdir
  LDC 0  ; cx
  LDC 0  ; cy
  LDC 0  ; dir
  LDC 0  ; score
  LDF decide_body
  AP 7
  RTN
decide_body:
  LD 1 1  ; world
  LDF get_my_pos
  AP 1
  ST 0 4  ; cy
  ST 0 3  ; cx
  LD 1 1  ; world
  LDF get_my_dir
  AP 1
  ST 0 2  ; cdir
  LDC 0
  ST 0 0  ; best_dir
  LDC -1000000
  ST 0 1  ; best_score
  LDC 0
  ST 0 5  ; dir
ephemeral000000_cond:
  LDC 4
  LD 0 5  ; dir
  CGT
  TSEL ephemeral000000_body ephemeral000000_else
ephemeral000000_body:
  LD 0 3  ; cx
  LD 0 4  ; cy
  LD 0 5  ; dir
  LDF adj_move
  AP 3
  LD 0 5  ; dir
  LDC 0
  LDC 1
  LD 1 0  ; map
  LD 1 1  ; world
  LDF eval_move
  AP 7
  ST 0 6  ; score
  LD 0 5  ; dir
  LD 0 2  ; cdir
  LDC 2
  ADD
  LDC 4
  LDF _builtin_mod
  AP 2
  CEQ
  TSEL ephemeral000001_then ephemeral000001_else
ephemeral000001_then:
  LD 0 6  ; score
  LDC 1
  SUB
  ST 0 6  ; score
  LDC 283283283
  TSEL ephemeral000001_end ephemeral000001_end
ephemeral000001_else:
ephemeral000001_end:
  LD 0 5  ; dir
  LD 0 6  ; score
  CONS
  DBUG
  LD 0 6  ; score
  LD 0 1  ; best_score
  CGT
  TSEL ephemeral000002_then ephemeral000002_else
ephemeral000002_then:
  LD 0 5  ; dir
  ST 0 0  ; best_dir
  LD 0 6  ; score
  ST 0 1  ; best_score
  LDC 283283283
  TSEL ephemeral000002_end ephemeral000002_end
ephemeral000002_else:
ephemeral000002_end:
ephemeral000000_next:
  LD 0 5  ; dir
  LDC 1
  ADD
  ST 0 5  ; dir
  LDC 283283283
  TSEL ephemeral000000_cond ephemeral000000_cond
ephemeral000000_else:
ephemeral000000_exit:
  LD 0 0  ; best_dir
  RTN
  RTN
eval_move:
  LDC 0  ; __3
  LDC 0  ; cell_score
  LDC 0  ; choices
  LDC 0  ; future_score
  LDC 0  ; ndir
  LDC 0  ; nscore
  LDF eval_move_body
  AP 6
  RTN
eval_move_body:
  LD 1 0  ; cx
  LD 1 1  ; cy
  LD 1 4  ; dist
  LD 1 5  ; map
  LD 1 6  ; world
  LDF eval_cell
  AP 5
  ST 0 1  ; cell_score
  LDC 1
  LD 0 1  ; cell_score
  LDC 0
  CGTE
  SUB
  TSEL ephemeral000004_then ephemeral000004_else
ephemeral000004_then:
  LD 0 1  ; cell_score
  RTN
  LDC 283283283
  TSEL ephemeral000004_end ephemeral000004_end
ephemeral000004_else:
ephemeral000004_end:
  LDC -1000000
  ST 0 3  ; future_score
  LDC 0  ; nil
  ST 0 2  ; choices
  LDC 0
  ST 0 4  ; ndir
ephemeral000005_cond:
  LDC 4
  LD 0 4  ; ndir
  CGT
  TSEL ephemeral000005_body ephemeral000005_else
ephemeral000005_body:
  LD 0 4  ; ndir
  LD 1 2  ; cdir
  LDC 2
  ADD
  LDC 4
  LDF _builtin_mod
  AP 2
  CEQ
  TSEL ephemeral000006_then ephemeral000006_else
ephemeral000006_then:
  LDC 283283283
  TSEL ephemeral000005_next ephemeral000005_next
  LDC 283283283
  TSEL ephemeral000006_end ephemeral000006_end
ephemeral000006_else:
ephemeral000006_end:
  LD 1 0  ; cx
  LD 1 1  ; cy
  LD 0 4  ; ndir
  LDF adj_move
  AP 3
  LD 1 5  ; map
  LDF get_cell
  AP 3
  LDC 0
  CGT
  TSEL ephemeral000007_then ephemeral000007_else
ephemeral000007_then:
  LD 0 4  ; ndir
  LD 0 2  ; choices
  CONS
  ST 0 2  ; choices
  LDC 283283283
  TSEL ephemeral000007_end ephemeral000007_end
ephemeral000007_else:
ephemeral000007_end:
ephemeral000005_next:
  LD 0 4  ; ndir
  LDC 1
  ADD
  ST 0 4  ; ndir
  LDC 283283283
  TSEL ephemeral000005_cond ephemeral000005_cond
ephemeral000005_else:
ephemeral000005_exit:
  LD 0 2  ; choices
  LDF len
  AP 1
  LDC 2
  CGTE
  TSEL ephemeral000008_then ephemeral000008_else
ephemeral000008_then:
  LD 1 3  ; branches
  LDC 1
  ADD
  ST 1 3  ; branches
  LD 1 3  ; branches
  LDC 3
  CGTE
  TSEL ephemeral000009_then ephemeral000009_else
ephemeral000009_then:
  LD 0 1  ; cell_score
  RTN
  LDC 283283283
  TSEL ephemeral000009_end ephemeral000009_end
ephemeral000009_else:
ephemeral000009_end:
  LDC 283283283
  TSEL ephemeral000008_end ephemeral000008_end
ephemeral000008_else:
ephemeral000008_end:
  LD 0 2  ; choices
  ST 0 0  ; __3
ephemeral000010_next:
  LD 0 0  ; __3
  ATOM
  TSEL ephemeral000010_else ephemeral000010_prebody
ephemeral000010_prebody:
  LD 0 0  ; __3
  CAR
  ST 0 4  ; ndir
  LD 0 0  ; __3
  CDR
  ST 0 0  ; __3
ephemeral000010_body:
  LD 1 0  ; cx
  LD 1 1  ; cy
  LD 0 4  ; ndir
  LDF adj_move
  AP 3
  LD 0 4  ; ndir
  LD 1 3  ; branches
  LD 1 4  ; dist
  LDC 1
  ADD
  LD 1 5  ; map
  LD 1 6  ; world
  LDF eval_move
  AP 7
  ST 0 5  ; nscore
  LD 0 5  ; nscore
  LD 0 3  ; future_score
  CGT
  TSEL ephemeral000011_then ephemeral000011_else
ephemeral000011_then:
  LD 0 5  ; nscore
  ST 0 3  ; future_score
  LDC 283283283
  TSEL ephemeral000011_end ephemeral000011_end
ephemeral000011_else:
ephemeral000011_end:
  LDC 283283283
  TSEL ephemeral000010_next ephemeral000010_next
ephemeral000010_else:
ephemeral000010_exit:
  LD 0 1  ; cell_score
  LD 0 3  ; future_score
  ADD
  RTN
  RTN
eval_cell:
  LDC 0  ; __12
  LDC 0  ; cell
  LDC 0  ; ghost
  LDC 0  ; gx
  LDC 0  ; gy
  LDC 0  ; star
  LDF eval_cell_body
  AP 6
  RTN
eval_cell_body:
  LD 1 4  ; world
  LDF get_my_vitality
  AP 1
  LDC 150
  DIV
  ST 0 5  ; star
  LD 1 4  ; world
  LDF get_ghosts
  AP 1
  ST 0 0  ; __12
ephemeral000013_next:
  LD 0 0  ; __12
  ATOM
  TSEL ephemeral000013_else ephemeral000013_prebody
ephemeral000013_prebody:
  LD 0 0  ; __12
  CAR
  ST 0 2  ; ghost
  LD 0 0  ; __12
  CDR
  ST 0 0  ; __12
ephemeral000013_body:
  LD 0 2  ; ghost
  CDR
  CAR
  LDF unpair
  AP 1
  ST 0 4  ; gy
  ST 0 3  ; gx
  LDC 1
  LD 1 0  ; x
  LD 0 3  ; gx
  CEQ
  LDC 0
  CEQ
  SUB
  LDC 1
  LD 1 1  ; y
  LD 0 4  ; gy
  CEQ
  LDC 0
  CEQ
  SUB
  MUL
  TSEL ephemeral000014_then ephemeral000014_else
ephemeral000014_then:
  LDC 1
  LD 1 2  ; dist
  LD 0 5  ; star
  CGTE
  SUB
  TSEL ephemeral000015_then ephemeral000015_else
ephemeral000015_then:
  LDC 10000
  RTN
  LDC 283283283
  TSEL ephemeral000015_end ephemeral000015_end
ephemeral000015_else:
ephemeral000015_end:
  LDC -1000000
  RTN
  LDC 283283283
  TSEL ephemeral000014_end ephemeral000014_end
ephemeral000014_else:
ephemeral000014_end:
  LDC 283283283
  TSEL ephemeral000013_next ephemeral000013_next
ephemeral000013_else:
ephemeral000013_exit:
  LD 1 0  ; x
  LD 1 1  ; y
  LD 1 3  ; map
  LDF get_cell
  AP 3
  ST 0 1  ; cell
  LD 0 1  ; cell
  LDC 0
  CEQ
  TSEL ephemeral000016_then ephemeral000016_else
ephemeral000016_then:
  LDC -1000000
  RTN
  LDC 283283283
  TSEL ephemeral000016_end ephemeral000016_end
ephemeral000016_else:
ephemeral000016_end:
  LD 0 1  ; cell
  LDC 2
  CEQ
  TSEL ephemeral000017_then ephemeral000017_else
ephemeral000017_then:
  LDC 100
  RTN
  LDC 283283283
  TSEL ephemeral000017_end ephemeral000017_end
ephemeral000017_else:
ephemeral000017_end:
  LD 0 1  ; cell
  LDC 3
  CEQ
  TSEL ephemeral000018_then ephemeral000018_else
ephemeral000018_then:
  LDC 1000
  RTN
  LDC 283283283
  TSEL ephemeral000018_end ephemeral000018_end
ephemeral000018_else:
ephemeral000018_end:
  LDC 0
  RTN
  RTN
get_cell:
  LD 0 2  ; map
  LD 0 1  ; y
  LD 0 0  ; x
  LDF get_2d_array
  AP 3
  RTN
  RTN
adj_move:
  LD 0 2  ; dir
  LDC 0
  CEQ
  TSEL ephemeral000019_then ephemeral000019_else
ephemeral000019_then:
  LD 0 1  ; y
  LDC 1
  SUB
  ST 0 1  ; y
  LDC 283283283
  TSEL ephemeral000019_end ephemeral000019_end
ephemeral000019_else:
  LD 0 2  ; dir
  LDC 1
  CEQ
  TSEL ephemeral000020_then ephemeral000020_else
ephemeral000020_then:
  LD 0 0  ; x
  LDC 1
  ADD
  ST 0 0  ; x
  LDC 283283283
  TSEL ephemeral000020_end ephemeral000020_end
ephemeral000020_else:
  LD 0 2  ; dir
  LDC 2
  CEQ
  TSEL ephemeral000021_then ephemeral000021_else
ephemeral000021_then:
  LD 0 1  ; y
  LDC 1
  ADD
  ST 0 1  ; y
  LDC 283283283
  TSEL ephemeral000021_end ephemeral000021_end
ephemeral000021_else:
  LD 0 2  ; dir
  LDC 3
  CEQ
  TSEL ephemeral000022_then ephemeral000022_else
ephemeral000022_then:
  LD 0 0  ; x
  LDC 1
  SUB
  ST 0 0  ; x
  LDC 283283283
  TSEL ephemeral000022_end ephemeral000022_end
ephemeral000022_else:
ephemeral000022_end:
ephemeral000021_end:
ephemeral000020_end:
ephemeral000019_end:
  LD 0 0  ; x
  LD 0 1  ; y
  RTN
  RTN
convert_2d_array:
  LDC 0  ; __23
  LDC 0  ; __24
  LDC 0  ; array
  LDC 0  ; cell
  LDC 0  ; h
  LDC 0  ; row
  LDC 0  ; w
  LDC 0  ; x
  LDC 0  ; y
  LDF convert_2d_array_body
  AP 9
  RTN
convert_2d_array_body:
  LD 1 0  ; map
  LDF len
  AP 1
  ST 0 4  ; h
  LD 1 0  ; map
  LDC 0
  LDF _builtin_index
  AP 2
  LDF len
  AP 1
  ST 0 6  ; w
  LD 0 4  ; h
  LD 0 6  ; w
  LDC 0
  LDF make_array
  AP 2
  LDF make_array
  AP 2
  ST 0 2  ; array
  LDC 0
  ST 0 8  ; y
  LD 1 0  ; map
  ST 0 0  ; __23
ephemeral000025_next:
  LD 0 0  ; __23
  ATOM
  TSEL ephemeral000025_else ephemeral000025_prebody
ephemeral000025_prebody:
  LD 0 0  ; __23
  CAR
  ST 0 5  ; row
  LD 0 0  ; __23
  CDR
  ST 0 0  ; __23
ephemeral000025_body:
  LDC 0
  ST 0 7  ; x
  LD 0 5  ; row
  ST 0 1  ; __24
ephemeral000026_next:
  LD 0 1  ; __24
  ATOM
  TSEL ephemeral000026_else ephemeral000026_prebody
ephemeral000026_prebody:
  LD 0 1  ; __24
  CAR
  ST 0 3  ; cell
  LD 0 1  ; __24
  CDR
  ST 0 1  ; __24
ephemeral000026_body:
  LD 0 2  ; array
  LD 0 8  ; y
  LD 0 7  ; x
  LD 0 3  ; cell
  LDF set_2d_array
  AP 4
  ST 0 2  ; array
  LD 0 7  ; x
  LDC 1
  ADD
  ST 0 7  ; x
  LDC 283283283
  TSEL ephemeral000026_next ephemeral000026_next
ephemeral000026_else:
ephemeral000026_exit:
  LD 0 8  ; y
  LDC 1
  ADD
  ST 0 8  ; y
  LDC 283283283
  TSEL ephemeral000025_next ephemeral000025_next
ephemeral000025_else:
ephemeral000025_exit:
  LD 0 2  ; array
  RTN
  RTN
set_2d_array:
  LD 0 0  ; array
  LD 0 1  ; i
  LD 0 0  ; array
  LD 0 1  ; i
  LDF get_array
  AP 2
  LD 0 2  ; j
  LD 0 3  ; value
  LDF set_array
  AP 3
  LDF set_array
  AP 3
  RTN
  RTN
get_2d_array:
  LD 0 0  ; array
  LD 0 1  ; i
  LDF get_array
  AP 2
  LD 0 2  ; j
  LDF get_array
  AP 2
  RTN
  RTN
get_my_pos:
  LD 0 0  ; world
  CDR
  CAR
  CDR
  CAR
  LDF unpair
  AP 1
  RTN
  RTN
get_my_dir:
  LD 0 0  ; world
  CDR
  CAR
  CDR
  CDR
  CAR
  RTN
  RTN
get_my_vitality:
  LD 0 0  ; world
  CDR
  CAR
  CAR
  RTN
  RTN
get_ghosts:
  LD 0 0  ; world
  CDR
  CDR
  CAR
  RTN
  RTN
get_map:
  LD 0 0  ; world
  CAR
  RTN
  RTN
make_array:
  LDC 0  ; width
  LDF make_array_body
  AP 1
  RTN
make_array_body:
  LDC 1
  ST 0 0  ; width
ephemeral000027_cond:
ephemeral000027_next:
  LDC 1
  LD 0 0  ; width
  LD 1 0  ; n
  CGTE
  SUB
  TSEL ephemeral000027_body ephemeral000027_exit
ephemeral000027_body:
  LD 0 0  ; width
  LDC 2
  MUL
  ST 0 0  ; width
  LDC 283283283
  TSEL ephemeral000027_cond ephemeral000027_cond
ephemeral000027_exit:
  LD 0 0  ; width
  LD 0 0  ; width
  LD 1 1  ; value
  LDF _make_tree
  AP 2
  CONS
  RTN
  RTN
_make_tree:
  LD 0 0  ; width
  LDC 1
  CEQ
  TSEL ephemeral000028_then ephemeral000028_else
ephemeral000028_then:
  LD 0 1  ; value
  RTN
  LDC 283283283
  TSEL ephemeral000028_end ephemeral000028_end
ephemeral000028_else:
ephemeral000028_end:
  LD 0 0  ; width
  LDC 2
  DIV
  LD 0 1  ; value
  LDF _make_tree
  AP 2
  LD 0 0  ; width
  LDC 2
  DIV
  LD 0 1  ; value
  LDF _make_tree
  AP 2
  CONS
  RTN
  RTN
get_array:

  ; left_size = car(array) / 2
  LD 0 0
  CAR
  LDC 2
  DIV
  ; array = cdr(array)
  LD 0 0
  CDR
  ST 0 0
  LDF ephemeral000029_body
  AP 1
  RTN
ephemeral000029_body:
  ; while left_size:
  LD 0 0
  TSEL ephemeral000029_descent ephemeral000029_exit
ephemeral000029_descent:
  ; if index >= left_size:
  LD 1 1
  LD 0 0
  CGTE
  TSEL ephemeral000029_right ephemeral000029_left
ephemeral000029_right:
  ; array = cdr(array)
  LD 1 0
  CDR
  ST 1 0
  ; index -= left_size
  LD 1 1
  LD 0 0
  SUB
  ST 1 1
  ; left_size /= 2
  LD 0 0
  LDC 2
  DIV
  ST 0 0
  LDC 283283283
  TSEL ephemeral000029_body ephemeral000029_body
ephemeral000029_left:
  ; array = car(array)
  LD 1 0
  CAR
  ST 1 0
  ; left_size /= 2
  LD 0 0
  LDC 2
  DIV
  ST 0 0
  LDC 283283283
  TSEL ephemeral000029_body ephemeral000029_body
ephemeral000029_exit:
  ; return array
  LD 1 0
  RTN
  
set_array:
  LDC 0  ; left_size
  LDC 0  ; tree
  LDC 0  ; width
  LDF set_array_body
  AP 3
  RTN
set_array_body:
  LD 1 0  ; array
  CAR
  ST 0 2  ; width
  LD 0 2  ; width
  LDC 2
  DIV
  ST 0 0  ; left_size
  LD 1 0  ; array
  CDR
  ST 0 1  ; tree
  LD 0 2  ; width
  LD 0 1  ; tree
  LD 1 1  ; index
  LD 1 2  ; value
  LD 0 0  ; left_size
  LDF _set_tree_impl
  AP 4
  CONS
  RTN
  RTN
_set_tree_impl:
  LD 0 3  ; left_size
  LDC 0
  CEQ
  TSEL ephemeral000030_then ephemeral000030_else
ephemeral000030_then:
  LD 0 2  ; value
  RTN
  LDC 283283283
  TSEL ephemeral000030_end ephemeral000030_end
ephemeral000030_else:
ephemeral000030_end:
  LD 0 1  ; index
  LD 0 3  ; left_size
  CGTE
  TSEL ephemeral000031_then ephemeral000031_else
ephemeral000031_then:
  LD 0 0  ; tree
  CAR
  LD 0 0  ; tree
  CDR
  LD 0 1  ; index
  LD 0 3  ; left_size
  SUB
  LD 0 2  ; value
  LD 0 3  ; left_size
  LDC 2
  DIV
  LDF _set_tree_impl
  AP 4
  CONS
  RTN
  LDC 283283283
  TSEL ephemeral000031_end ephemeral000031_end
ephemeral000031_else:
ephemeral000031_end:
  LD 0 0  ; tree
  CAR
  LD 0 1  ; index
  LD 0 2  ; value
  LD 0 3  ; left_size
  LDC 2
  DIV
  LDF _set_tree_impl
  AP 4
  LD 0 0  ; tree
  CDR
  CONS
  RTN
  RTN
_builtin_mod:
  LD 0 0  ; a
  LD 0 0  ; a
  LD 0 1  ; b
  DIV
  LD 0 1  ; b
  MUL
  SUB
  RTN
  RTN
_builtin_index:
ephemeral000032_cond:
ephemeral000032_next:
  LD 0 1  ; index
  LDC 0
  CGT
  TSEL ephemeral000032_body ephemeral000032_exit
ephemeral000032_body:
  LD 0 0  ; array
  CDR
  ST 0 0  ; array
  LD 0 1  ; index
  LDC 1
  SUB
  ST 0 1  ; index
  LDC 283283283
  TSEL ephemeral000032_cond ephemeral000032_cond
ephemeral000032_exit:
  LD 0 0  ; array
  CAR
  RTN
  RTN
unpair:
  LD 0 0  ; pair
  CAR
  LD 0 0  ; pair
  CDR
  RTN
  RTN
len:
  LDC 0  ; _
  LDC 0  ; __33
  LDC 0  ; n
  LDF len_body
  AP 3
  RTN
len_body:
  LDC 0
  ST 0 2  ; n
  LD 1 0  ; list
  ST 0 1  ; __33
ephemeral000034_next:
  LD 0 1  ; __33
  ATOM
  TSEL ephemeral000034_else ephemeral000034_prebody
ephemeral000034_prebody:
  LD 0 1  ; __33
  CAR
  ST 0 0  ; _
  LD 0 1  ; __33
  CDR
  ST 0 1  ; __33
ephemeral000034_body:
  LD 0 2  ; n
  LDC 1
  ADD
  ST 0 2  ; n
  LDC 283283283
  TSEL ephemeral000034_next ephemeral000034_next
ephemeral000034_else:
ephemeral000034_exit:
  LD 0 2  ; n
  RTN
  RTN
