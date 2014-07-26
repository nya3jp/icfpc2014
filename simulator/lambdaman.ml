type linstruction =
  | Ildc of int
  | Ild  of int * int
  | Iadd
  | Isub
  | Imul
  | Idiv
  | Iceq
  | Icgt
  | Icgte
  | Iatom
  | Icons
  | Icar
  | Icdr
  | Isel (* conditional branch *)
  | Ijoin
  | Ildf
  | Iap
  | Irtn
  | Idum
  | Istop
  | Itap
  | Itsel
  | Itrap
  | Idbug
