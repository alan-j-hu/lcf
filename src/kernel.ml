type prop =
  | Hyp of int
  | True
  | False
  | Not of prop
  | And of prop * prop
  | Or of prop * prop
  | Implies of prop * prop

type ctx = prop list

module type S = sig
  type thm

  val hyp : ctx -> int -> thm

  val truth : ctx -> thm

  val and_intro : thm -> thm -> thm

  val and_elim1 : thm -> thm

  val and_elim2 : thm -> thm

  val implies_intro : ctx -> prop -> thm -> thm

  val implies_elim : thm -> thm -> thm

  val or_intro1 : thm -> prop -> thm

  val or_intro2 : prop -> thm -> thm

  val or_elim : thm -> thm -> thm -> thm
end
