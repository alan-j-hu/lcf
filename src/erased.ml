open Kernel

type thm = ctx * prop

let same_ctx ctx1 ctx2 x =
  if ctx1 = ctx2 then
    x
  else
    failwith "Contexts differ!"

let hyp ctx idx = (ctx, List.nth ctx idx)

let truth ctx = (ctx, True)

let and_intro (ctx1, p1) (ctx2, p2) = same_ctx ctx1 ctx2 (ctx1, And(p1, p2))

let and_elim1 = function
  | ctx, And(p1, _) -> (ctx, p1)
  | _, _ -> failwith "Expected `and' prop!"

let and_elim2 = function
  | ctx, And(_, p2) -> (ctx, p2)
  | _, _ -> failwith "Expected `and' prop!"

let implies_intro ctx hyp (ctx', con) =
  if ctx' = hyp :: ctx then
    (ctx, Implies(hyp, con))
  else
    failwith "Expected `implies' prop!"

let implies_elim (ctx1, p1) (ctx2, p2) = match p1 with
  | Implies(hyp, con) ->
    if hyp = p2 then
      same_ctx ctx1 ctx2 (ctx1, con)
    else
      failwith "Hypotheses differ!"
  | _ -> failwith "Expected `implies' prop!"

let or_intro1 (ctx, p1) p2 = (ctx, Or(p1, p2))

let or_intro2 p1 (ctx, p2) = (ctx, Or(p1, p2))

let or_elim (ctx1, p1) (ctx2, con) (ctx3, con') = match p1 with
  | Or(p2, p3) ->
    if con = con' then
      same_ctx ctx3 (p3 :: ctx1) (same_ctx ctx2 (p2 :: ctx1) (ctx1, con))
    else
      failwith "Conclusions differ!"
  | _ -> failwith "Expected `or' prop!"
