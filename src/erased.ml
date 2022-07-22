open Kernel

type thm = ctx * prop

let hyp ctx idx = (ctx, List.nth ctx idx)

let truth ctx = (ctx, True)

let and_intro (ctx1, p1) (ctx2, p2) =
  if ctx1 = ctx2 then
    (ctx1, And(p1, p2))
  else
    failwith ""

let and_elim1 = function
  | ctx, And(p1, _) -> (ctx, p1)
  | _, _ -> failwith ""

let and_elim2 = function
  | ctx, And(_, p2) -> (ctx, p2)
  | _, _ -> failwith ""

let implies_intro ctx hyp (ctx', con) =
  if ctx' = hyp :: ctx then
    (ctx, Implies(hyp, con))
  else
    failwith ""

let implies_elim (ctx1, p1) (ctx2, p2) = match p1 with
  | Implies(hyp, con) ->
    if hyp = p2 then
      if ctx1 = ctx2 then
        (ctx1, con)
      else
        failwith ""
    else
      failwith ""
  | _ -> failwith ""

let or_intro1 (ctx, p1) p2 = (ctx, Or(p1, p2))

let or_intro2 p1 (ctx, p2) = (ctx, Or(p1, p2))

let or_elim (ctx1, p1) (ctx2, con) (ctx3, con') = match p1 with
  | Or(p2, p3) ->
    if con = con' then
      if ctx2 = p2 :: ctx1 then
        if ctx3 = p3 :: ctx1 then
          (ctx1, con)
        else
          failwith ""
      else
        failwith ""
    else
      failwith ""
  | _ -> failwith ""
