open Prelude.Ana
open Analyses

type spec_modules = { spec : (module MCPSpec)
                    ; dom  : (module Lattice.S)
                    ; glob : (module Lattice.S)
                    ; cont : (module Printable.S)
                    ; var  : (module Printable.S)
                    ; acc  : (module MCPA) }

let analyses_list  : (int * spec_modules) list ref = ref []
let analyses_list' : (int * spec_modules) list ref = ref []
let dep_list       : (int * (int list)) list ref   = ref []
let dep_list'      : (int * (string list)) list ref= ref []

let analyses_table = ref []

let register_analysis =
  let count = ref 0 in
  fun ?(dep=[]) (module S:MCPSpec) ->
    let s = { spec = (module S : MCPSpec)
            ; dom  = (module S.D : Lattice.S)
            ; glob = (module S.G : Lattice.S)
            ; cont = (module S.C : Printable.S)
            ; var  = (module S.V : Printable.S)
            ; acc  = (module S.A : MCPA)
            }
    in
    let n = S.name () in
    analyses_table := (!count,n) :: !analyses_table;
    analyses_list' := (!count,s) :: !analyses_list';
    dep_list'      := (!count,dep) :: !dep_list';
    count := !count + 1


type unknown = Obj.t

module type DomainListPrintableSpec =
sig
  val assoc_dom : int -> (module Printable.S)
  val domain_list : unit -> (int * (module Printable.S)) list
end

module type DomainListMCPASpec =
sig
  val assoc_dom : int -> (module MCPA)
  val domain_list : unit -> (int * (module MCPA)) list
end

module type DomainListLatticeSpec =
sig
  val assoc_dom : int -> (module Lattice.S)
  val domain_list : unit -> (int * (module Lattice.S)) list
end

module PrintableOfLatticeSpec (D:DomainListLatticeSpec) : DomainListPrintableSpec =
struct
  let assoc_dom n =
    let f (module L:Lattice.S) = (module L : Printable.S)
    in
    f (D.assoc_dom n)

  let domain_list () =
    let f (module L:Lattice.S) = (module L : Printable.S) in
    List.map (fun (x,y) -> (x,f y)) (D.domain_list ())
end

module PrintableOfMCPASpec (D:DomainListMCPASpec) : DomainListPrintableSpec =
struct
  let assoc_dom n =
    let f (module L:MCPA) = (module L : Printable.S)
    in
    f (D.assoc_dom n)

  let domain_list () =
    let f (module L:MCPA) = (module L : Printable.S) in
    List.map (fun (x,y) -> (x,f y)) (D.domain_list ())
end

exception DomListBroken of string

module DomListPrintable (DLSpec : DomainListPrintableSpec)
  : Printable.S with type t = (int * unknown) list
=
struct
  include Printable.Std (* for default invariant, tag, ... *)

  open DLSpec
  open List
  open Obj

  type t = (int * unknown) list

  let unop_fold f a (x:t) =
    let f a n d =
      f a n (assoc_dom n) d
    in
    fold_left (fun a (n,d) -> f a n d) a x

  let pretty () x =
    let f a n (module S : Printable.S) x = Pretty.dprintf "%s:%a" (S.name ()) S.pretty (obj x) :: a in
    let xs = unop_fold f [] x in
    match xs with
    | [] -> text "[]"
    | x :: [] -> x
    | x :: y ->
      let rest  = List.fold_left (fun p n->p ++ text "," ++ break ++ n) nil y in
      text "[" ++ align ++ x ++ rest ++ unalign ++ text "]"

  let show x =
    let xs = unop_fold (fun a n (module S : Printable.S) x ->
        let analysis_name = assoc n !analyses_table in
        (analysis_name ^ ":(" ^ S.show (obj x) ^ ")") :: a) [] x
    in
    IO.to_string (List.print ~first:"[" ~last:"]" ~sep:", " String.print) (rev xs)

  let to_yojson xs =
    let f a n (module S : Printable.S) x =
      let name = BatList.assoc n !analyses_table in
      (name, S.to_yojson (obj x)) :: a
    in `Assoc (unop_fold f [] xs)

  let binop_fold f a (x:t) (y:t) =
    let f a n d1 d2 =
      f a n (assoc_dom n) d1 d2
    in
    try if length x <> length y
      then raise (DomListBroken "binop_fold : differing lengths")
      else fold_left (fun a (n,d) -> f a n d @@ assoc n y) a x
    with Not_found -> raise (DomListBroken "binop_fold : assoc failure")

  let equal   x y = binop_fold (fun a n (module S : Printable.S) x y -> a && S.equal (obj x) (obj y)) true x y
  let compare x y = binop_fold (fun a n (module S : Printable.S) x y -> if a <> 0 then a else S.compare (obj x) (obj y)) 0 x y

  let hashmul x y = if x=0 then y else if y=0 then x else x*y

  let hash     = unop_fold (fun a n (module S : Printable.S) x -> hashmul a @@ S.hash (obj x)) 0

  let name () =
    let domain_name (n, (module D: Printable.S)) =
      let analysis_name = assoc n !analyses_table in
      analysis_name ^ ":(" ^ D.name () ^ ")"
    in
    IO.to_string (List.print ~first:"[" ~last:"]" ~sep:", " String.print) (map domain_name @@ domain_list ())

  let printXml f xs =
    let print_one a n (module S : Printable.S) x : unit =
      BatPrintf.fprintf f "<analysis name=\"%s\">\n" (List.assoc n !analyses_table);
      S.printXml f (obj x);
      BatPrintf.fprintf f "</analysis>\n"
    in
    unop_fold print_one () xs

  let invariant c = unop_fold (fun a n (module S : Printable.S) x ->
      Invariant.(a && S.invariant c (obj x))
    ) Invariant.none

  let arbitrary () =
    let arbs = map (fun (n, (module D: Printable.S)) -> QCheck.map ~rev:(fun (_, o) -> obj o) (fun x -> (n, repr x)) @@ D.arbitrary ()) @@ domain_list () in
    MyCheck.Arbitrary.sequence arbs
end

module DomVariantPrintable (DLSpec : DomainListPrintableSpec)
  : Printable.S with type t = int * unknown
=
struct
  include Printable.Std (* for default invariant, tag, ... *)

  open DLSpec
  open List
  open Obj

  type t = int * unknown

  let unop_map f ((n, d):t) =
    f n (assoc_dom n) d

  let pretty () = unop_map (fun n (module S: Printable.S) x ->
      Pretty.dprintf "%s:%a" (S.name ()) S.pretty (obj x)
    )

  let show = unop_map (fun n (module S: Printable.S) x ->
      let analysis_name = assoc n !analyses_table in
      analysis_name ^ ":" ^ S.show (obj x)
    )

  let to_yojson x =
    `Assoc [
      unop_map (fun n (module S: Printable.S) x ->
          let name = BatList.assoc n !analyses_table in
          (name, S.to_yojson (obj x))
        ) x
    ]

  let equal (n1, x1) (n2, x2) =
    n1 = n2 && (
      let module S = (val assoc_dom n1) in
      S.equal (obj x1) (obj x2)
    )

  let compare (n1, x1) (n2, x2) =
    let r = Stdlib.compare n1 n2 in
    if r <> 0 then
      r
    else
      let module S = (val assoc_dom n1) in
      S.compare (obj x1) (obj x2)

  let hash = unop_map (fun n (module S: Printable.S) x ->
      Hashtbl.hash (n, S.hash (obj x))
    )

  let name () =
    let domain_name (n, (module S: Printable.S)) =
      let analysis_name = assoc n !analyses_table in
      analysis_name ^ ":" ^ S.name ()
    in
    IO.to_string (List.print ~first:"" ~last:"" ~sep:" | " String.print) (map domain_name @@ domain_list ())

  let printXml f = unop_map (fun n (module S: Printable.S) x ->
      BatPrintf.fprintf f "<analysis name=\"%s\">\n" (List.assoc n !analyses_table);
      S.printXml f (obj x);
      BatPrintf.fprintf f "</analysis>\n"
    )

  let invariant c = unop_map (fun n (module S: Printable.S) x ->
      S.invariant c (obj x)
    )

  let arbitrary () =
    let arbs = map (fun (n, (module S: Printable.S)) -> QCheck.map ~rev:(fun (_, o) -> obj o) (fun x -> (n, repr x)) @@ S.arbitrary ()) @@ domain_list () in
    QCheck.oneof arbs
end

module DomListLattice (DLSpec : DomainListLatticeSpec)
  : Lattice.S with type t = (int * unknown) list
=
struct
  open DLSpec
  open List
  open Obj

  include DomListPrintable (PrintableOfLatticeSpec (DLSpec))

  let binop_fold f a (x:t) (y:t) =
    let f a n d1 d2 =
      f a n (assoc_dom n) d1 d2
    in
    try if length x <> length y
      then raise (DomListBroken "binop_fold : differing lengths")
      else fold_left (fun a (n,d) -> f a n d @@ assoc n y) a x
    with Not_found -> raise (DomListBroken "binop_fold : assoc failure")

  let binop_map (f: (module Lattice.S) -> Obj.t -> Obj.t -> Obj.t) x y =
    List.rev @@ binop_fold (fun a n s d1 d2 -> (n, f s d1 d2) :: a) [] x y

  let unop_fold f a (x:t) =
    let f a n d =
      f a n (assoc_dom n) d
    in
    fold_left (fun a (n,d) -> f a n d) a x

  let narrow = binop_map (fun (module S : Lattice.S) x y -> repr @@ S.narrow (obj x) (obj y))
  let widen  = binop_map (fun (module S : Lattice.S) x y -> repr @@ S.widen  (obj x) (obj y))
  let meet   = binop_map (fun (module S : Lattice.S) x y -> repr @@ S.meet   (obj x) (obj y))
  let join   = binop_map (fun (module S : Lattice.S) x y -> repr @@ S.join   (obj x) (obj y))

  let leq    = binop_fold (fun a n (module S : Lattice.S) x y -> a && S.leq (obj x) (obj y)) true

  let is_top = unop_fold (fun a n (module S : Lattice.S) x -> a && S.is_top (obj x)) true
  let is_bot = unop_fold (fun a n (module S : Lattice.S) x -> a && S.is_bot (obj x)) true

  let top () = map (fun (n,(module S : Lattice.S)) -> (n,repr @@ S.top ())) @@ domain_list ()
  let bot () = map (fun (n,(module S : Lattice.S)) -> (n,repr @@ S.bot ())) @@ domain_list ()

  let pretty_diff () (x,y) =
    let f a n (module S : Lattice.S) x y =
      if S.leq (obj x) (obj y) then a
      else a ++ S.pretty_diff () (obj x, obj y) ++ text ". "
    in
    binop_fold f nil x y
end

module DomVariantLattice0 (DLSpec : DomainListLatticeSpec)
  : Lattice.S with type t = int * unknown
=
struct
  open DLSpec
  open Obj

  include DomVariantPrintable (PrintableOfLatticeSpec (DLSpec))

  let binop_map' (f: int -> (module Lattice.S) -> Obj.t -> Obj.t -> 'a) (n1, d1) (n2, d2) =
    if n1 <> n2
    then raise (DomListBroken "binop_fold : differing variants")
    else f n1 (assoc_dom n1) d1 d2

  let binop_map (f: (module Lattice.S) -> Obj.t -> Obj.t -> Obj.t) =
    binop_map' (fun n s d1 d2 -> (n, f s d1 d2))

  let narrow = binop_map (fun (module S : Lattice.S) x y -> repr @@ S.narrow (obj x) (obj y))
  let widen  = binop_map (fun (module S : Lattice.S) x y -> repr @@ S.widen  (obj x) (obj y))
  let meet   = binop_map (fun (module S : Lattice.S) x y -> repr @@ S.meet   (obj x) (obj y))
  let join   = binop_map (fun (module S : Lattice.S) x y -> repr @@ S.join   (obj x) (obj y))

  let leq    = binop_map' (fun _ (module S : Lattice.S) x y -> S.leq (obj x) (obj y))

  let is_top x = false
  let is_bot x = false
  let top () = failwith "DomVariantLattice0.top"
  let bot () = failwith "DomVariantLattice0.bot"

  let pretty_diff () (x, y) =
    let f _ (module S : Lattice.S) x y =
      if S.leq (obj x) (obj y) then nil
      else S.pretty_diff () (obj x, obj y)
    in
    binop_map' f x y
end

module DomVariantLattice (DLSpec : DomainListLatticeSpec) =
  Lattice.Lift (DomVariantLattice0 (DLSpec)) (Printable.DefaultNames)

module LocalDomainListSpec : DomainListLatticeSpec =
struct
  let assoc_dom n = (List.assoc n !analyses_list).dom
  let domain_list () = List.map (fun (n,p) -> n, p.dom) !analyses_list
end

module GlobalDomainListSpec : DomainListLatticeSpec =
struct
  let assoc_dom n = (List.assoc n !analyses_list).glob
  let domain_list () = List.map (fun (n,p) -> n, p.glob) !analyses_list
end

module ContextListSpec : DomainListPrintableSpec =
struct
  let assoc_dom n = (List.assoc n !analyses_list).cont
  let domain_list () = List.map (fun (n,p) -> n, p.cont) !analyses_list
end

module VarListSpec : DomainListPrintableSpec =
struct
  let assoc_dom n = (List.assoc n !analyses_list).var
  let domain_list () = List.map (fun (n,p) -> n, p.var) !analyses_list
end

module AccListSpec : DomainListMCPASpec =
struct
  let assoc_dom n = (List.assoc n !analyses_list).acc
  let domain_list () = List.map (fun (n,p) -> n, p.acc) !analyses_list
end
