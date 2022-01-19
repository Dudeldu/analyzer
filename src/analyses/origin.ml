
open Prelude.Ana
open Analyses

(* For different kinds of value domains, like addresses, indexes... *)
include PreValueDomain

(** An analysis that tracks the origin of a value.
    It only considers definite values of local variables.
    We do not pass information interprocedurally. *)
module Spec : Analyses.MCPSpec =
struct
  let name () = "origin"

  (* Program point lattice *)
  module PL = Lattice.Flat (Node) (struct
      let top_name = "Unknown node"
      let bot_name = "Unreachable node"
    end)

  (* Variable lattice *)
  module VL = Lattice.Flat (Basetype.Variables) (struct
    let top_name = "Unknown variable"
    let bot_name = "Unreachable variable"
  end)

  (* Origin is Variable and Node *)
  module Origin = Lattice.Prod (VL) (PL)
  module OriginSet = SetDomain.ToppedSet (Origin) (struct let topname = "All" end)
  module ValueOriginPair = Lattice.Prod (AD) (OriginSet)

  (* Map of Variable -> Pair (AddressSet, OriginSet) *)
  module D = struct
    include MapDomain.MapBot (Basetype.Variables) (ValueOriginPair)
    
    type t = MapDomain.MapBot(Basetype.Variables)(ValueOriginPair).t
    
    let check_precision_loss (m1: t) (m2: t) (res: t) =
      let s1 = fold (fun (key: Basetype.Variables.t) (v:ValueOriginPair.t) acc -> 
        if (find key res) != v then acc @ [key] else acc) m1 [] in
      let s2 = fold (fun (key: Basetype.Variables.t) (v:ValueOriginPair.t) acc -> 
        if (find key res) != v then acc @ [key] else acc) m2 [] in
      s1 @ s2
      (*m1 != res or m2 != res*)
    

    let update_blame res (x:varinfo) n  =
      let curr_val_origin_pair = find x res in
      let curr_val = fst curr_val_origin_pair in
      let curr_origin_set = snd curr_val_origin_pair in
      let new_pair = (curr_val, curr_origin_set) in 
      add x new_pair res

    let join_with_fct f (m1: t) (m2: t) =
      (*let _ = Pretty.printf "JOINING %s %s\n" (Pretty.sprint 80 (pretty () m1)) (Pretty.sprint 80 (pretty () m2)) in*)
      let res =  if m1 == m2 then m1 else long_map2 f m1 m2 in
      let losses = check_precision_loss m1 m2 res in
      if List.length losses > 0 then
        (match !MyCFG.current_node with
          | Some n -> 
            ignore @@ Pretty.printf "Precision lost at node %s\n\n" (Node.show n);
            ignore @@ List.fold (fun res x -> 
              update_blame res x n
            ) res losses;
          | _ -> ignore @@ Pretty.printf "Precision lost at unknown node\n\n");
      res
    
      let join = join_with_fct ValueOriginPair.join

  end
  (* No information about globals*)
  module G = Lattice.Unit
  (* No contexts*)
  module C = Lattice.Unit

  include Analyses.IdentitySpec
  let context _ _ = ()

  let is_pointer_var (v: varinfo) =
    match v.vtype with
    | TPtr _ -> true
    | _ -> false

  let get_local = function
    | Var v, NoOffset when is_pointer_var v && not v.vglob -> Some v (* local pointer variable whose address is maybe taken *)
    | _, _ -> None

  let contains s1 s2 =
    let re = Str.regexp_string s2 in
    try ignore (Str.search_forward re s1 0); true
    with Not_found -> false

  (* TODO: make some smart check here *)
  let should_split node = false
    (* let name: string = Node.show node in
    let we_split = contains name "node 11" || contains name "node 14" in
    let _ = if we_split then
      ignore (Pretty.printf "Spliting at %s\n" name);
    in
    we_split *)

  let should_join node x y = 
    match node with
    | Some (n: Node.t) -> not (should_split n)
    | _ -> true

  (** Evaluates expressions *)
  (* let rec eval (state : D.t) (e: exp) (node: PL.t) =
      let int_val = match e with
      | Const c -> (match c with
        | CInt64 (i,_,_) -> I.of_int i
        | _ -> I.top ()
        )
      | Lval lv -> (match get_local lv with
        | Some v -> fst (D.find v state)
        | _ -> I.top ()
        )
      | BinOp (PlusA, e1, e2, t) -> (
        let v1 = fst (eval state e1 node) in
        let v2 = fst (eval state e2 node) in
        I.add v1 v2
      )
      | AddrOf (Var v, _) -> fst (D.find v state)
      | _ -> I.top ()
      in (int_val, node) *)

  (* Taken from arinc *)
  let mayPointTo ctx exp =
    match ctx.ask (Queries.MayPointTo exp) with
    | a when not (Queries.LS.is_top a) && Queries.LS.cardinal a > 0 ->
      let top_elt = (dummyFunDec.svar, `NoOffset) in
      let a' = if Queries.LS.mem top_elt a then (
          M.debug "mayPointTo: query result for %a contains TOP!" d_exp exp; (* UNSOUND *)
          Queries.LS.remove top_elt a
        ) else a
      in
      Queries.LS.elements a'
    | v ->
      M.debug "mayPointTo: query result for %a is %a" d_exp exp Queries.LS.pretty v;
      []
      (*`Top*)

  (* transfer functions *)
  let assign ctx (lval:lval) (rval:exp) : D.t =
    let node = match !MyCFG.current_node with
      | Some n -> `Lifted n
      | _ -> PL.top ()
    in
    match get_local lval with
    (* | Some loc -> D.add loc (eval ctx.local rval node) ctx.local *)
    | Some loc -> 
      let curr_val_origin_pair = D.find loc ctx.local in
      let curr_val = fst curr_val_origin_pair in
      let curr_origin_set = snd curr_val_origin_pair in
      (* let value = IntDomain.IntDomTuple.tag (ctx.ask (Queries.EvalInt rval)) in *)
      let values: Addr.t list = List.map (
        fun x -> 
        let vinfo: varinfo = fst x in 
        Addr.Addr (vinfo, `NoOffset)
        ) (mayPointTo ctx rval) in
      (*let module AddrMap = BatMap.Make (AD.Addr) in*)
      let address_set = List.fold_left (fun s (x: Addr.t) -> AD.add x s) curr_val values in
      let new_origin: Origin.t = (VL.top (), node) in
      let new_origin_set = OriginSet.add new_origin curr_origin_set  in
      let new_pair = (address_set, new_origin_set) in 
      (*let _ = Pretty.printf "assign %s\n" (Pretty.sprint 80 (D.pretty () ctx.local)) in*)
      (*let new_set = values OriginSet.add (value, node) curr_set in*)
      let ret = D.add loc new_pair ctx.local in
      (*let _ = Pretty.printf "after %s\n" (Pretty.sprint 80 (D.pretty () ret)) in*)
      ret
    | None -> ctx.local

  let branch ctx (exp:exp) (tv:bool) : D.t = ctx.local
  (*let node = match !MyCFG.current_node with
    | Some n -> (`Lifted n)
    | _ -> PL.top ()
    in
    let v = eval ctx.local exp node in
    match I.to_bool (fst v) with
    | Some b when b <> tv -> raise Deadcode (* if the expression evaluates to not tv, the tv branch is not reachable *)
    | _ -> ctx.local *)

  let body ctx (f:fundec) : D.t =
    (* Initialize locals to top *)
    List.fold (fun m l -> D.add l (AD.empty (), OriginSet.empty ()) m) ctx.local f.slocals

  let return ctx (exp:exp option) (f:fundec) : D.t =
    (* Do nothing, as we are not interested in return values for now. *)
    ctx.local

  let enter ctx (lval: lval option) (f:fundec) (args:exp list) : (D.t * D.t) list =
    (* Set the formal int arguments to top *)
    let callee_state = List.fold (fun m l -> D.add l (AD.empty (), OriginSet.empty ()) m) (D.bot ()) f.sformals in
    [(ctx.local, callee_state)]

  let set_local_int_lval_top (state: D.t) (lval: lval option) =
    match lval with
    | Some lv ->
      (match get_local lv with
       | Some local -> D.add local (AD.empty (), OriginSet.empty ()) state
       | _ -> state
      )
    |_ -> state

  let combine ctx (lval:lval option) fexp (f:fundec) (args:exp list) fc (au:D.t) : D.t =
    (* If we have a function call with assignment
        x = f (e1, ... , ek)
        with a local int variable x on the left, we set it to top *)
    set_local_int_lval_top ctx.local lval

  let special ctx (lval: lval option) (f:varinfo) (arglist:exp list) : D.t =
    (* When calling a special function, and assign the result to some local int variable, we also set it to top. *)
    set_local_int_lval_top ctx.local lval

  let startstate v = D.bot ()
  let exitstate v = D.top () (* TODO: why is this different from startstate? *)
end

let _ =
  MCP.register_analysis (module Spec : MCPSpec)
