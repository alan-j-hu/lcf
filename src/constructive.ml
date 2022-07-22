type term =
  | Ax
  | Abs of term
  | App of term * term
  | Pair of term * term
  | Fst of term
  | Snd of term
  | Inl of term
  | Inr of term
  | Case of term * term * term
  | Var of int

module Make (K : Kernel.S) : Kernel.S with type thm = K.thm * term = struct
  type thm = K.thm * term

  let hyp ctx idx = (K.hyp ctx idx, Var idx)

  let truth ctx = (K.truth ctx, Ax)

  let and_intro (thm1, term1) (thm2, term2) =
    (K.and_intro thm1 thm2, Pair(term1, term2))

  let and_elim1 (thm, term) = (thm, Fst term)

  let and_elim2 (thm, term) = (thm, Snd term)

  let implies_intro ctx hyp (thm, term) =
    (K.implies_intro ctx hyp thm, term)

  let implies_elim (thm1, term1) (thm2, term2) =
    (K.implies_elim thm1 thm2, App(term1, term2))

  let or_intro1 (thm, term) p = (K.or_intro1 thm p, Inl term)

  let or_intro2 p (thm, term) = (K.or_intro2 p thm, Inr term)

  let or_elim (thm1, term1) (thm2, term2) (thm3, term3) =
    (K.or_elim thm1 thm2 thm3, Case(term1, term2, term3))
end
