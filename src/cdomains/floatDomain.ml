open Pretty
open PrecisionUtil
open Round

module type FloatArith = sig
  type t

  val neg : t -> t
  (** Negating an flaot value: [-x] *)
  val add : t -> t -> t
  (** Addition: [x + y] *)
  val sub : t -> t -> t
  (** Subtraction: [x - y] *)
  val mul : t -> t -> t
  (** Multiplication: [x * y] *)
  val div : t -> t -> t
  (** Division: [x / y] *)

  (** {b Comparison operators} *)
  val lt : t -> t -> IntDomain.IntDomTuple.t
  (** Less than: [x < y] *)
  val gt : t -> t -> IntDomain.IntDomTuple.t
  (** Greater than: [x > y] *)
  val le : t -> t -> IntDomain.IntDomTuple.t
  (** Less than or equal: [x <= y] *)
  val ge : t -> t -> IntDomain.IntDomTuple.t
  (** Greater than or equal: [x >= y] *)
  val eq : t -> t -> IntDomain.IntDomTuple.t
  (** Equal to: [x == y] *)
  val ne : t -> t -> IntDomain.IntDomTuple.t
  (** Not equal to: [x != y] *)
end

module type FloatDomainBase = sig
  include Lattice.S
  include FloatArith with type t := t

  val of_const : float -> t
end

(* TODO: I think we should add a new CWE number? Where do we do that? *)
(* TODO: I have removed all parameters to this function... Can we
         actually know anything about what is wrong in our domain? *)
let set_overflow_flag () =
  Messages.warn 
    ~category:Messages.Category.Integer.overflow 
    ~tags:[CWE 189; CWE 191] 
    "Some problem with float... TODO: Better Warning!"

(* TODO: We need to use "norm" like we are doing in the IntDomain.
    ---> Where do we need it then? When is it applied in the intdomain? *)
(* FIXME: In IntDomain, we also have a cast parameter... do we also want
    that here? *)


let norm v = 
  let normed = match v with
  | Some (low, high) -> 
    if compare low high > 0 then failwith "lower bound bigger than upper bound"
    else if Float.is_finite low && Float.is_finite high then v 
    else None
  | _ -> None
  in match normed with
  | Some _ -> v
  | None -> set_overflow_flag (); None
  (* if Ints_t.compare x y > 0 then None
  else (
    let min_ik = min_int ik in
    let max_ik = max_int ik in
    let underflow = Ints_t.compare min_ik x > 0 in
    let overflow = Ints_t.compare max_ik y < 0 in
    if underflow || overflow then (
      set_overflow_flag ~cast ~underflow ~overflow ik;
      if should_wrap ik then (* could add [|| cast], but that's GCC implementation-defined behavior: https://gcc.gnu.org/onlinedocs/gcc/Integers-implementation.html#Integers-implementation *)
        (* We can only soundly wrap if at most one overflow occurred, otherwise the minimal and maximal values of the interval *)
        (* on Z will not safely contain the minimal and maximal elements after the cast *)
        let diff = Ints_t.abs (Ints_t.sub max_ik min_ik) in
        let resdiff = Ints_t.abs (Ints_t.sub y x) in
        if Ints_t.compare resdiff diff > 0 then
          top_of ik
        else
          let l = Ints_t.of_bigint @@ Size.cast ik (Ints_t.to_bigint x) in
          let u = Ints_t.of_bigint @@ Size.cast ik (Ints_t.to_bigint y) in
          if Ints_t.compare l u <= 0 then
            Some (l, u)
          else
            (* Interval that wraps around (begins to the right of its end). We can not represent such intervals *)
            top_of ik
      else if not cast && should_ignore_overflow ik then
        let tl, tu = BatOption.get @@ top_of ik *)

let set_overflow_flag ~cast ~underflow ~overflow ik =
  failwith "todo set overflow flag"
  (*let signed = Cil.isSigned ik in
  if !GU.postsolving && signed && not cast then
    Goblintutil.svcomp_may_overflow := true;

  let sign = if signed then "Signed" else "Unsigned" in
  match underflow, overflow with
  | true, true ->
    M.warn ~category:M.Category.Integer.overflow ~tags:[CWE 190; CWE 191] "%s integer overflow and underflow" sign
  | true, false ->
    M.warn ~category:M.Category.Integer.overflow ~tags:[CWE 191] "%s integer underflow" sign
  | false, true ->
    M.warn ~category:M.Category.Integer.overflow ~tags:[CWE 190] "%s integer overflow" sign
  | false, false -> assert false*)
  
(* TODO: We need to use "norm" like we are doing in the IntDomain.
    Where do we need it then? When is it applied in the intdomain? *)
(* FIXME: In IntDomain, we also have a cast parameter... do we also want
    that here? *)

module FloatInterval = struct
  type t = (float * float) option [@@deriving eq, ord, to_yojson]

  let norm v = 
    match v with
    | Some (low, high) -> 
      if Float.is_finite low && Float.is_finite high then 
        if low > high then failwith "invalid Interval"
        else v
      else None
    | _ -> None

  (**just for norming the arbitraries, so correct intervals get created, but no failwith if low > high*)
  let norm_arb v = 
    match v with
    | Some (f1, f2) ->
      if Float.is_finite f1 && Float.is_finite f2 
      then Some(min f1 f2, max f1 f2) 
      else None
    | _ -> None

  let of_const f = norm @@ Some (f, f)

  let hash = Hashtbl.hash

  let show = function
    | None -> "Float[Top]"
    | Some (low, high) -> "Float [" ^ string_of_float low ^ "," ^ string_of_float high ^ "]"

  let pretty () x = text (show x)

  let printXml f (x : t) =
    BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (show x)

  let name () = "FloatInterval"

  let invariant _ (x : t) = failwith "todo invariant"

  let tag (x : t) = failwith "todo tag" (**Quote printable.ml line 24: Unique ID, given by HConsed, for context identification in witness *)

  (**for QCheck: should describe how to generate random values and shrink possilbe counter examples *)
  let arbitrary () = QCheck.map norm_arb (QCheck.option (QCheck.pair QCheck.float QCheck.float)) 

  let relift x = x 

  let leq v1 v2 = 
    match v1, v2 with
    | _, None -> true
    | None, Some _ -> false
    | Some (l1, h1), Some (l2, h2) -> l1 >= l2 && h1 <= h2

  let join v1 v2 = 
    match v1, v2 with
    | None, _ | _, None -> None
    | Some (l1, h1), Some (l2, h2) -> Some (min l1 l2, max h1 h2)

  let meet v1 v2 = 
    match v1, v2 with
    | None, v | v, None -> v
    | Some (l1, h1), Some (l2, h2) -> 
      let (l, h) = (max l1 l2, min h1 h2) in
      if l <= h 
      then Some (l, h) 
      else failwith "meet results in empty Interval"

  (** [widen x y] assumes [leq x y]. Solvers guarantee this by calling [widen old (join old new)]. *)
  let widen v1 v2 = (**TODO: support 'threshold_widening' option *)
    match v1, v2 with
    | None, _ | _, None -> None
    | Some (l1, h1), Some (l2, h2) ->
      (**If we widen and we know that neither interval contains +-inf or nan, it is ok to widen only to +-max_float, 
         because a widening with +-inf/nan will always result in the case above -> Top *)
      let low = if l1 <= l2 then l1 else (-.Float.max_float) in 
      let high = if h1 >= h2 then h1 else Float.max_float in
      norm @@ Some (low, high)

  let narrow v1 v2 = (**TODO: support 'threshold_narrowing' and 'narrow_by_meet' option *)
    match v1, v2 with (**we cannot distinguish between the lower bound beeing -inf or the upper bound beeing inf. Also there is nan *)
    | None, _ -> v2
    | _, _ -> v1

  (** If [leq x y = false], then [pretty_diff () (x, y)] should explain why. *)
  let pretty_diff () (x, y) =
    Pretty.dprintf "%a instead of %a" pretty x pretty y

  let bot () = failwith "no bot exists"

  let is_bot _ = false

  let top () = None

  let is_top = Option.is_none

  let neg = Option.map (fun (low, high) -> (-.high, -.low))

  (** evaluation of the binary operations *)
  let eval_binop eval_operation op1 op2 =
    match (op1, op2) with 
    | Some v1, Some v2 -> norm @@ eval_operation v1 v2 
    | _ -> None

  let eval_int_binop eval_operation op1 op2 =
    let a, b =
      match (op1, op2) with 
      | Some v1, Some v2 -> eval_operation v1 v2 
      | _ -> (0, 1)
    in
    IntDomain.IntDomTuple.of_interval IBool
      (Big_int_Z.big_int_of_int a, Big_int_Z.big_int_of_int b)

  let eval_add (l1, h1) (l2, h2) = 
    Some (add Down l1 l2, add Up h1 h2)

  let eval_sub (l1, h1) (l2, h2) = 
    Some (sub Down l1 l2, sub Up h1 h2)

  let eval_mul (l1, h1) (l2, h2) =
    let mul1u = mul Up l1 l2 in
    let mul2u = mul Up l1 h2 in
    let mul3u = mul Up h1 l2 in
    let mul4u = mul Up h1 h2 in
    let mul1d = mul Down l1 l2 in
    let mul2d = mul Down l1 h2 in
    let mul3d = mul Down h1 l2 in
    let mul4d = mul Down h1 h2 in
    let high = max (max (max mul1u mul2u) mul3u) mul4u in
    let low = min (min (min mul1d mul2d) mul3d) mul4d in
    Some (low, high)

  let eval_div (l1, h1) (l2, h2) =
    if l2 <= 0. && h2 >= 0. then None
    else
      let div1u = div Up l1 l2 in
      let div2u = div Up l1 h2 in
      let div3u = div Up h1 l2 in
      let div4u = div Up h1 h2 in
      let div1d = div Down l1 l2 in
      let div2d = div Down l1 h2 in
      let div3d = div Down h1 l2 in
      let div4d = div Down h1 h2 in
      let high = max (max (max div1u div2u) div3u) div4u in
      let low = min (min (min div1d div2d) div3d) div4d in
      Some (low, high)


  let eval_lt (l1, h1) (l2, h2) = 
    if h1 < l2 then (1, 1)
    else if l1 >= h2 then (0, 0) 
    else (0, 1)

  let eval_gt (l1, h1) (l2, h2) =
    if l1 > h2 then (1, 1)
    else if h1 <= l2 then (0, 0) 
    else (0, 1)

  let eval_le (l1, h1) (l2, h2) =
    if h1 <= l2 then (1, 1)
    else if l1 > h2 then (0, 0) 
    else (0, 1)

  let eval_ge (l1, h1) (l2, h2) =
    if l1 >= h2 then (1, 1)
    else if h1 < l2 then (0, 0) 
    else (0, 1)

  let eval_eq (l1, h1) (l2, h2) =
    if h1 < l2 || h2 < l1 then (0, 0)
    else if h1 = l1 && h2 = l2 && l1 = l2 then (1, 1)
    else (0, 1)

  let eval_ne (l1, h1) (l2, h2) =
    if h1 < l2 || h2 < l1 then (1, 1)
    else if h1 = l1 && h2 = l2 && l1 = l2 then (0, 0)
    else (0, 1)

  let add = eval_binop eval_add

  let sub = eval_binop eval_sub

  let mul = eval_binop eval_mul

  let div = eval_binop eval_div

  let lt = eval_int_binop eval_lt

  let gt = eval_int_binop eval_gt

  let le = eval_int_binop eval_le

  let ge = eval_int_binop eval_ge

  let eq = eval_int_binop eval_eq

  let ne = eval_int_binop eval_ne

end

module FloatDomTupleImpl = struct
  include Printable.Std (* for default invariant, tag, ... *)
  module F1 = FloatInterval
  open Batteries

  type t = F1.t option [@@deriving to_yojson, eq, ord]

  let name () = "floatdomtuple"

  type 'a m = (module FloatDomainBase with type t = 'a)
  (* only first-order polymorphism on functions 
     -> use records to get around monomorphism restriction on arguments (Same trick as used in intDomain) *)
  type 'b poly_in = { fi : 'a. 'a m -> 'b -> 'a }
  type 'b poly_pr = { fp : 'a. 'a m -> 'a -> 'b }
  type 'b poly2_pr = { f2p : 'a. 'a m -> 'a -> 'a -> 'b }
  type poly1 = { f1 : 'a. 'a m -> 'a -> 'a }
  type poly2 = { f2 : 'a. 'a m -> 'a -> 'a -> 'a }

  let create r x (f1 : float_precision) =
    let f b g = if b then Some (g x) else None in
    f f1 @@ r.fi (module F1)

  let create r x =
    (* use where values are introduced *)
    create r x (float_precision_from_node_or_config ())

  let opt_map2 f =
    curry @@ function Some x, Some y -> Some (f x y) | _ -> None

  let exists = function Some true -> true | _ -> false
  let for_all = function Some false -> false | _ -> true

  let mapp r = BatOption.map (r.fp (module F1))

  let map r a = BatOption.(map (r.f1 (module F1)) a)
  let map2 r xa ya = opt_map2 (r.f2 (module F1)) xa ya
  let map2p r xa ya = opt_map2 (r.f2p (module F1)) xa ya

  let map2int r xa ya =
    Option.map_default identity
      (IntDomain.IntDomTuple.top_of IBool) (opt_map2 (r.f2p (module F1)) xa ya)

  let ( %% ) f g x = f % g x

  let show x =
    Option.map_default identity ""
      (mapp { fp= (fun (type a) (module F : FloatDomainBase with type t = a) x -> F.name () ^ ":" ^ F.show x); } x)

  let to_yojson =
    [%to_yojson: Yojson.Safe.t option]
    % mapp { fp= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.to_yojson); }
  let hash x =
    Option.map_default identity 0
      (mapp { fp= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.hash); } x)

  let of_const =
    create { fi= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.of_const); }

  let top =
    create { fi= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.top); }
  let bot =
    create { fi= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.bot); }
  let is_bot =
    exists
    % mapp { fp= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.is_bot); }
  let is_top =
    for_all
    % mapp { fp= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.is_top); }


  let leq =
    for_all
    %% map2p { f2p= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.leq); }

  let pretty () x =
    Option.map_default identity nil
      (mapp { fp= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.pretty ()); } x)

  (* f1: one and only unary op *)
  let neg =
    map { f1= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.neg); }

  (* f2: binary ops *)
  let join =
    map2 { f2= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.join); }
  let meet =
    map2 { f2= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.meet); }
  let widen =
    map2 { f2= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.widen); }
  let narrow =
    map2 { f2= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.narrow); }
  let add =
    map2 { f2= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.add); }
  let sub =
    map2 { f2= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.sub); }
  let mul =
    map2 { f2= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.mul); }
  let div =
    map2 { f2= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.div); }

  (* f2: binary ops which return an integer *)
  let lt =
    map2int { f2p= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.lt); }
  let gt =
    map2int { f2p= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.gt); }
  let le =
    map2int { f2p= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.le); }
  let ge =
    map2int { f2p= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.ge); }
  let eq =
    map2int { f2p= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.eq); }
  let ne =
    map2int { f2p= (fun (type a) (module F : FloatDomainBase with type t = a) -> F.ne); }

  let pretty_diff () (x, y) = dprintf "%a instead of %a" pretty x pretty y

  let printXml f x =
    BatPrintf.fprintf f "<value>\n<data>\n%s\n</data>\n</value>\n" (show x)
end
