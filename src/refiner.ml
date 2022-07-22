open Kernel

module type S = sig
  type thm
  type goal = ctx * prop
  type tactic = goal -> goal list * (thm list -> thm)

  val hyp : int -> tactic

  val and_intro : tactic

  val and_elim1 : prop -> tactic

  val and_elim2 : prop -> tactic

  val or_intro1 : tactic

  val or_intro2 : tactic

  val or_elim : prop -> prop -> tactic

  val implies_intro : tactic

  val implies_elim : prop -> tactic
end

module Make (K : Kernel.S) : S with type thm = K.thm = struct
  type thm = K.thm
  type goal = ctx * prop
  type tactic = goal -> goal list * (thm list -> thm)

  let hyp idx (ctx, p) =
    if List.nth ctx idx = p then
      ( []
      , function
        | [] -> K.hyp ctx idx
        | _ -> failwith "" )
    else
      failwith "Refiner: Hypothesis index out of bounds"

  let and_intro = function
    | (ctx, And(p1, p2)) ->
      ( [(ctx, p1); (ctx, p2)]
      , function
        | [thm1; thm2] -> K.and_intro thm1 thm2
        | _ -> failwith "" )
    | _ -> failwith ""

  let and_elim1 p2 = function
    | (ctx, p1) ->
      ( [(ctx, And(p1, p2))]
      , function
        | [thm] -> K.and_elim1 thm
        | _ -> failwith "" )

  let and_elim2 p1 = function
    | (ctx, p2) ->
      ( [(ctx, And(p1, p2))]
      , function
        | [thm] -> K.and_elim1 thm
        | _ -> failwith "" )

  let or_intro1 = function
    | (ctx, Or(p1, p2)) ->
      ( [(ctx, p1)]
      , function
        | [thm] -> K.or_intro1 thm p2
        | _ -> failwith "" )
    | _ -> failwith ""

  let or_intro2 = function
    | (ctx, Or(p1, p2)) ->
      ( [(ctx, p2)]
      , function
        | [thm] -> K.or_intro2 p1 thm
        | _ -> failwith "" )
    | _ -> failwith ""

  let or_elim p1 p2 (ctx, p3) =
    ( [(ctx, Or(p1, p2)); (p1 :: ctx, p3); (p2 :: ctx, p3)]
    , function
      | [thm1; thm2; thm3] -> K.or_elim thm1 thm2 thm3
      | _ -> failwith "" )

  let implies_intro = function
    | (ctx, Implies(hyp, con)) ->
      ( [(hyp :: ctx, con)]
      , function
        | [thm] -> K.implies_intro ctx hyp thm
        | _ -> failwith "" )
    | _ -> failwith ""

  let implies_elim hyp (ctx, con) =
    ( [(ctx, Implies(hyp, con)); (ctx, hyp)]
    , function
      | [thm1; thm2] -> K.implies_elim thm1 thm2
      | _ -> failwith "" )
end
