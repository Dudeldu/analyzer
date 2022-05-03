
module Pat =
struct
  type ('a, 'k, 'r) t = 'a -> 'k -> 'r

  let arg: _ t = fun x k -> k x
  let ignore: _ t = fun _ k -> k

  let nil: _ t = fun x k ->
    match x with
    | [] -> k
    | _ -> invalid_arg "nil"

  let ( ^:: ) (p1: _ t) (p2: _ t): _ t = fun x k ->
    match x with
    | x1 :: x2 ->
      let k = p1 x1 k in
      let k = p2 x2 k in
      k
    | [] -> invalid_arg "^::"

  let map (p: _ t) ~(f): _ t = fun x k -> p x (f k)
  let map_result (p: _ t) ~(f): _ t = fun x k -> f (p x k)
  let (>>) (p: ('a, 'k, 'r) t) (k: 'k) (a: 'a): 'r = p a k
end

type access = [
  | `Read
  | `Write
]

let r: (Cil.exp, _, _) Pat.t = fun x k -> fun (a, acc) ->
  match a with
  | `Read ->
    x :: k (a, acc)
  | `Write ->
    k (a, acc)

let p0: (Cil.exp list, _, _) Pat.t = Pat.(r ^:: ignore ^:: r ^:: nil)
let ac0: access -> Cil.exp list = fun a -> p0 [Cil.one; Cil.zero] snd (a, [])


type special = [
  | `Lock of Cil.exp
]

let p1 = Pat.(arg ^:: ignore ^:: arg ^:: nil)
let a1 = Pat.(arg ^:: ignore ^:: arg ^:: nil >> fun e1 e2 -> `Lock e1)
let s1: special = a1 [Cil.one]
