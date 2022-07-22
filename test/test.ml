open Lcf

module M = Refiner.Make(Constructive.Make(Erased))

let a_implies_a a ctx = match M.implies_intro (ctx, Implies(a, a)) with
  | [goal], _ ->
    let _, f = M.hyp 0 goal in
    f []
  | _ -> failwith "Unreachable"

let _ = a_implies_a False []
