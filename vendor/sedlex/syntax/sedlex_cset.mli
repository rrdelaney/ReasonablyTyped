(* The package sedlex is released under the terms of an MIT-like license. *)
(* See the attached LICENSE file.                                         *)
(* Copyright 2005, 2013 by Alain Frisch and LexiFi.                       *)

(** Representation of sets of unicode code points. *)

type t = (int * int) list

val min_code: int
val max_code: int

val empty: t
val any: t
val union: t -> t -> t
val difference: t -> t -> t
val intersection: t -> t -> t
val is_empty: t -> bool
val eof: t
val singleton: int -> t
val interval: int -> int -> t

val letter: t
val digit: t
val extender: t
val base_char: t
val ideographic: t
val combining_char: t
val blank: t
val tr8876_ident_char: t

