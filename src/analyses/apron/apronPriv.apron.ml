open Prelude.Ana
open Analyses
open GobConfig
(* open BaseUtil *)
module Q = Queries

module A = ApronDomain.A
module ApronComponents = ApronDomain.ApronComponents
open Apron

open CommonPriv

module type S =
  functor (AD: ApronDomain.S2) ->
  sig
    module D: Lattice.S
    module G: Lattice.S

    type apron_components_t := ApronDomain.ApronComponents (AD) (D).t
    val name: unit -> string
    val startstate: unit -> D.t
    val should_join: apron_components_t -> apron_components_t -> bool

    val read_global: Q.ask -> (varinfo -> G.t) -> apron_components_t -> varinfo -> varinfo -> AD.t

    (* [invariant]: Check if we should avoid producing a side-effect, such as updates to
     * the state when following conditional guards. *)
    val write_global: ?invariant:bool -> Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> apron_components_t -> varinfo -> varinfo -> apron_components_t

    val lock: Q.ask -> (varinfo -> G.t) -> apron_components_t -> LockDomain.Addr.t -> apron_components_t
    val unlock: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> apron_components_t -> LockDomain.Addr.t -> apron_components_t

    val sync: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> apron_components_t -> [`Normal | `Join | `Return | `Init | `Thread] -> apron_components_t

    val enter_multithreaded: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> apron_components_t -> apron_components_t
    val threadenter: Q.ask -> (varinfo -> G.t) -> apron_components_t -> apron_components_t

    val thread_join: Q.ask -> (varinfo -> G.t) -> Cil.exp -> apron_components_t -> apron_components_t
    val thread_return: Q.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> ThreadIdDomain.Thread.t -> apron_components_t -> apron_components_t

    val init: unit -> unit
    val finalize: unit -> unit
  end

module type Converter =
  functor (ADGlobal: ApronDomain.S2) -> functor (Priv: S) -> S

(** Takes [ADGlobal], an apron-domain-module to be used for global invariants, a privatizataion [Priv], and returns a privatization that
    internally uses [ADGlobal] to keep global invariants, while complying to the privatzation interface fitting to [ADLocal].
    This is achieved by internally converting back and forth between representations of the different domains. *)
module Converter: Converter = functor (ADGlobal: ApronDomain.S2) -> functor (Priv: S) -> functor (ADLocal: ApronDomain.S2) ->
struct
  module Priv = Priv(ADGlobal)
  module D = Priv.D
  module G = Priv.G

  type t = ADLocal.t
  type global_act = (ADGlobal.t, D.t) ApronDomain.aproncomponents_t
  type apron_components_t = (t, D.t) ApronDomain.aproncomponents_t

  let to_local apr = ADLocal.of_other_dom (module ADGlobal.Man) apr
  let to_global apr = ADGlobal.of_other_dom (module ADLocal.Man) apr

  let to_local_components (comp: global_act): apron_components_t = { apr = to_local comp.apr; priv = comp.priv}
  let to_global_components (comp: apron_components_t): global_act = { apr = to_global comp.apr; priv = comp.priv}

  let name () = "Converter " ^ "(Privatization: " ^ (Priv.name ()) ^ ", local domain: " ^ (ADLocal.Man.name ()) ^ ", global domain: " ^ (ADGlobal.Man.name ()) ^ ")"
  
  let startstate () = Priv.startstate ()

  let should_join a b = Priv.should_join (to_global_components a) (to_global_components b)

  let read_global ask getg st g x =
    let r = Priv.read_global ask getg (to_global_components st) g x in
    to_local r

  let write_global ?(invariant=false) ask getg sideg st g x =
    let r = Priv.write_global ask getg sideg (to_global_components st) g x in
    to_local_components r

  let lock ask getg st m =
    let r = Priv.lock ask getg (to_global_components st) m in
    to_local_components r

  let unlock ask getg sideg st m =
    let r = Priv.unlock ask getg sideg (to_global_components st) m in
    to_local_components r

  let thread_join ask getg exp st =
    let r = Priv.thread_join ask getg exp (to_global_components st) in
    to_local_components r

  let thread_return ask getg sideg tid st =
    let r = Priv.thread_return ask getg sideg tid (to_global_components st) in
    to_local_components r

  let sync ask getg sideg st reason =
    let r = Priv.sync ask getg sideg (to_global_components st) reason in
    to_local_components r

  let enter_multithreaded ask getg sideg st =
    let r = Priv.enter_multithreaded ask getg sideg (to_global_components st) in
    to_local_components r

  let threadenter ask getg st =
    let r = Priv.threadenter ask getg (to_global_components st) in
    to_local_components r

  let init () = Priv.init ()

  let finalize () = Priv.finalize ()
end

module Dummy: S = functor (AD: ApronDomain.S2) ->
struct
  module D = Lattice.Unit
  module G = Lattice.Unit

  let name () = "Dummy"
  let startstate () = ()
  let should_join _ _ = true

  let read_global ask getg st g x = st.ApronDomain.apr
  let write_global ?(invariant=false) ask getg sideg st g x = st

  let lock ask getg st m = st
  let unlock ask getg sideg st m = st

  let thread_join ask getg exp st = st
  let thread_return ask getg sideg tid st = st

  let sync ask getg sideg st reason = st

  let enter_multithreaded ask getg sideg st = st
  let threadenter ask getg st = st

  let init () = ()
  let finalize () = ()
end

module type ProtectionBasedPrivParam =
sig
  (** Whether to be path-sensitive w.r.t. locally written protected globals that have been continuously protected since writing. *)
  val path_sensitive: bool
end

(** Protection-Based Reading. *)
module ProtectionBasedPriv (Param: ProtectionBasedPrivParam): S = functor (AD: ApronDomain.S2) ->
struct
  include ConfCheck.RequireMutexActivatedInit
  open Protection

  (** Locally must-written protected globals that have been continuously protected since writing. *)
  module P =
  struct
    include MustVars
    let name () = "P"
  end

  (** Locally may-written protected globals that have been continuously protected since writing. *)
  (* TODO: is this right? *)
  module W =
  struct
    include MayVars
    let name () = "W"
  end

  module D = Lattice.Prod (P) (W)
  module G = AD

  type apron_components_t = ApronComponents (AD) (D).t
  let global_varinfo = RichVarinfo.single ~name:"APRON_GLOBAL"

  module VM =
  struct
    type t =
      | Local of varinfo
      | Unprot of varinfo
      | Prot of varinfo

    let var_name = function
      | Local g -> g.vname
      | Unprot g -> g.vname ^ "#unprot"
      | Prot g -> g.vname ^ "#prot"
  end
  module V =
  struct
    include ApronDomain.VarMetadataTbl (VM)
    open VM

    let local g = make_var (Local g)
    let unprot g = make_var (Unprot g)
    let prot g = make_var (Prot g)
  end

  let name () = "ProtectionBasedPriv"

  (** Restrict environment to global invariant variables. *)
  let restrict_global apr =
    AD.remove_filter apr (fun var ->
        match V.find_metadata var with
        | Some (Unprot _ | Prot _) -> false
        | _ -> true
      )

  (** Restrict environment to local variables and still-protected global variables. *)
  let restrict_local is_unprot apr w_remove =
    let remove_local_vars = List.map V.local (W.elements w_remove) in
    let apr' = AD.remove_vars apr remove_local_vars in
    (* remove global vars *)
    AD.remove_filter apr' (fun var ->
        match V.find_metadata var with
        | Some (Unprot g | Prot g) -> is_unprot g
        | _ -> false
      )

  let startstate () = (P.empty (), W.empty ())

  let should_join (st1: apron_components_t) (st2: apron_components_t) =
    if Param.path_sensitive then (
      let (p1, _) = st1.priv in
      let (p2, _) = st2.priv in
      P.equal p1 p2
    )
    else
      true

  let read_global ask getg (st: apron_components_t) g x =
    let apr = st.apr in
    let (p, w) = st.priv in
    let g_local_var = V.local g in
    let x_var = Var.of_string x.vname in
    let apr_local =
      if W.mem g w then
        AD.assign_var apr x_var g_local_var
      else
        AD.bot ()
    in
    let apr_local' =
      if P.mem g p then
        apr_local
      else if is_unprotected ask g then (
        let g_unprot_var = V.unprot g in
        let apr_unprot = AD.add_vars apr [g_unprot_var] in
        let apr_unprot = AD.assign_var apr_unprot x_var g_unprot_var in
        (* let oct_unprot' = AD.join oct_local oct_unprot in
           (* unlock *)
           let oct_unprot' = AD.remove_vars oct_unprot' [g_unprot_var; g_local_var] in
           (* add, assign from, remove is not equivalent to forget if g#unprot already existed and had some relations *)
           (* TODO: why removing g_unprot_var? *)
           oct_unprot' *)
        AD.join apr_local apr_unprot
      )
      else (
        let g_prot_var = V.prot g in
        let apr_prot = AD.add_vars apr [g_prot_var] in
        let apr_prot = AD.assign_var apr_prot x_var g_prot_var in
        AD.join apr_local apr_prot
      )
    in
    let apr_local' = restrict_local (is_unprotected ask) apr_local' (W.empty ()) in
    let apr_local' = AD.meet apr_local' (getg (global_varinfo ())) in
    apr_local'

  let write_global ?(invariant=false) ask getg sideg (st: apron_components_t) g x =
    let apr = st.apr in
    let (p, w) = st.priv in
    let g_local_var = V.local g in
    let g_unprot_var = V.unprot g in
    let x_var = Var.of_string x.vname in
    let apr_local = AD.add_vars apr [g_local_var] in
    let apr_local = AD.assign_var apr_local g_local_var x_var in
    let apr_side = AD.add_vars apr_local [g_unprot_var] in
    let apr_side = AD.assign_var apr_side g_unprot_var g_local_var in
    let apr' = apr_side in
    let apr_side = restrict_global apr_side in
    sideg (global_varinfo ()) apr_side;
    let st' =
      (* if is_unprotected ask g then
         st (* add, assign, remove gives original local state *)
         else
         (* restricting g#unprot-s out from oct' gives oct_local *)
         {oct = oct_local; priv = (P.add g p, W.add g w)} *)
      if is_unprotected ask g then
        {st with apr = restrict_local (is_unprotected ask) apr' (W.singleton g)}
      else (
        let p' = P.add g p in
        let w' = W.add g w in
        {apr = restrict_local (is_unprotected ask) apr' (W.empty ()); priv = (p', w')}
      )
    in
    let apr_local' = AD.meet st'.apr (getg (global_varinfo ())) in
    {st' with apr = apr_local'}

  let lock ask getg (st: apron_components_t) m = st

  let unlock ask getg sideg (st: apron_components_t) m: apron_components_t =
    let apr = st.apr in
    let (p, w) = st.priv in
    let (p_remove, p') = P.partition (fun g -> is_unprotected_without ask g m) p in
    let (w_remove, w') = W.partition (fun g -> is_unprotected_without ask g m) w in
    let p_a = P.filter (is_protected_by ask m) p in
    let w_a = W.filter (is_protected_by ask m) (W.diff w p) in
    let big_omega =
      let certain = P.elements p_a in
      let choice = W.elements w_a in
      choice
      |> List.map (fun _ -> [true; false])
      |> List.n_cartesian_product (* TODO: exponential! *)
      |> List.map (fun omega ->
          (* list globals where omega is true *)
          List.fold_left2 (fun acc g omega_g ->
              if omega_g then
                g :: acc
              else
                acc
            ) certain choice omega
        )
    in
    let apr_side = List.fold_left (fun acc omega ->
        let g_prot_vars = List.map V.prot omega in
        let g_local_vars = List.map V.local omega in
        let apr_side1 = AD.add_vars apr g_prot_vars in
        let apr_side1 = AD.assign_var_parallel' apr_side1 g_prot_vars g_local_vars in
        AD.join acc apr_side1
      ) (AD.bot ()) big_omega
    in
    let apr' = apr_side in
    let apr_side = restrict_global apr_side in
    sideg (global_varinfo ()) apr_side;
    let apr_local = restrict_local (fun g -> is_unprotected_without ask g m) apr' w_remove in
    let apr_local' = AD.meet apr_local (getg (global_varinfo ())) in
    {apr = apr_local'; priv = (p', w')}


  let thread_join ask getg exp st = st
  let thread_return ask getg sideg tid st = st

  let sync ask getg sideg (st: apron_components_t) reason =
    match reason with
    | `Return -> (* required for thread return *)
      (* TODO: implement? *)
      begin match ThreadId.get_current ask with
        | `Lifted x (* when CPA.mem x st.cpa *) ->
          st
        | _ ->
          st
      end
    | `Normal
    | `Join (* TODO: no problem with branched thread creation here? *)
    | `Init
    | `Thread ->
      st

  let enter_multithreaded ask getg sideg (st: apron_components_t): apron_components_t =
    let apr = st.apr in
    let (g_vars, gs) =
      AD.vars apr
      |> List.enum
      |> Enum.filter_map (fun var ->
          match ApronDomain.V.find_metadata var with
          | Some (Global g) -> Some (var, g)
          | _ -> None
        )
      |> Enum.uncombine
      |> Tuple2.map List.of_enum List.of_enum
    in
    let g_unprot_vars = List.map V.unprot gs in
    let g_prot_vars = List.map V.prot gs in
    let apr_side = AD.add_vars apr (g_unprot_vars @ g_prot_vars) in
    let apr_side = AD.assign_var_parallel' apr_side g_unprot_vars g_vars in
    let apr_side = AD.assign_var_parallel' apr_side g_prot_vars g_vars in
    let apr_side = restrict_global apr_side in
    sideg (global_varinfo ()) apr_side;
    let apr_local = AD.remove_vars apr g_vars in
    let apr_local' = AD.meet apr_local (getg (global_varinfo ())) in
    {apr = apr_local'; priv = startstate ()}

  let threadenter ask getg (st: apron_components_t): apron_components_t =
    {apr = getg (global_varinfo ()); priv = startstate ()}

  let finalize () = ()
end

module CommonPerMutex = functor(AD: ApronDomain.S2) ->
struct
  include Protection
  module V = ApronDomain.V

  let remove_globals_unprotected_after_unlock ask m oct =
    let newly_unprot var = match V.find_metadata var with
      | Some (Global g) -> is_protected_by ask m g && is_unprotected_without ask g m
      | _ -> false
    in
    AD.remove_filter oct newly_unprot

  let keep_only_protected_globals ask m oct =
    let protected var = match V.find_metadata var with
      | Some (Global g) -> is_protected_by ask m g
      | _ -> false
    in
    AD.keep_filter oct protected

  let finalize () = ProtectionLogging.dump ()
end

(** Per-mutex meet. *)
module PerMutexMeetPriv : S = functor (AD: ApronDomain.S2) ->
struct
  open CommonPerMutex(AD)
  open ExplicitMutexGlobals

  module D = Lattice.Unit
  module G = AD

  type apron_components_t = ApronDomain.ApronComponents (AD) (D).t
  let global_varinfo = RichVarinfo.single ~name:"APRON_GLOBAL"

  module V = ApronDomain.V

  let name () = "PerMutexMeetPriv"

  let startstate () = ()

  let should_join _ _ = true

  let mutex_inits = RichVarinfo.single ~name:"MUTEX_INITS"

  let get_m_with_mutex_inits ask getg m =
    let get_m = getg (mutex_addr_to_varinfo m) in
    let get_mutex_inits = getg (mutex_inits ()) in
    let get_mutex_inits' = keep_only_protected_globals ask m get_mutex_inits in
    AD.join get_m get_mutex_inits'

  let get_mutex_global_g_with_mutex_inits ask getg g =
    let get_mutex_global_g = getg (mutex_global g) in
    let get_mutex_inits = getg (mutex_inits ()) in
    let g_var = V.global g in
    let get_mutex_inits' = AD.keep_vars get_mutex_inits [g_var] in
    AD.join get_mutex_global_g get_mutex_inits'

  let read_global ask getg (st: apron_components_t) g x: AD.t =
    let apr = st.apr in
    (* lock *)
    let apr = AD.meet apr (get_mutex_global_g_with_mutex_inits ask getg g) in
    (* read *)
    let g_var = V.global g in
    let x_var = Var.of_string x.vname in
    let apr_local = AD.add_vars apr [g_var] in
    let apr_local = AD.assign_var apr_local x_var g_var in
    (* unlock *)
    let apr_local' =
      if is_unprotected ask g then
        AD.remove_vars apr_local [g_var]
      else
        apr_local
    in
    apr_local'

  let write_global ?(invariant=false) ask getg sideg (st: apron_components_t) g x: apron_components_t =
    let apr = st.apr in
    (* lock *)
    let apr = AD.meet apr (get_mutex_global_g_with_mutex_inits ask getg g) in
    (* write *)
    let g_var = V.global g in
    let x_var = Var.of_string x.vname in
    let apr_local = AD.add_vars apr [g_var] in
    let apr_local = AD.assign_var apr_local g_var x_var in
    (* unlock *)
    let apr_side = AD.keep_vars apr_local [g_var] in
    sideg (mutex_global g) apr_side;
    let apr_local' =
      if is_unprotected ask g then
        AD.remove_vars apr_local [g_var]
      else
        apr_local
    in
    {st with apr = apr_local'}

  let lock ask getg (st: apron_components_t) m =
    let apr = st.apr in
    let get_m = get_m_with_mutex_inits ask getg m in
    (* Additionally filter get_m in case it contains variables it no longer protects. E.g. in 36/22. *)
    let get_m = keep_only_protected_globals ask m get_m in
    let apr' = AD.meet apr get_m in
    {st with apr = apr'}

  let unlock ask getg sideg (st: apron_components_t) m: apron_components_t =
    let apr = st.apr in
    let apr_side = keep_only_protected_globals ask m apr in
    sideg (mutex_addr_to_varinfo m) apr_side;
    let apr_local = remove_globals_unprotected_after_unlock ask m apr in
    {st with apr = apr_local}

  let thread_join ask getg exp st = st
  let thread_return ask getg sideg tid st = st

  let sync ask getg sideg (st: apron_components_t) reason =
    match reason with
    | `Return -> (* required for thread return *)
      (* TODO: implement? *)
      begin match ThreadId.get_current ask with
        | `Lifted x (* when CPA.mem x st.cpa *) ->
          st
        | _ ->
          st
      end
    | `Join ->
      if (ask.f Q.MustBeSingleThreaded) then
        st
      else
        let apr = st.apr in
        let g_vars = List.filter (fun var ->
            match V.find_metadata var with
            | Some (Global _) -> true
            | _ -> false
          ) (AD.vars apr)
        in
        let apr_side = AD.keep_vars apr g_vars in
        sideg (mutex_inits ()) apr_side;
        let apr_local = AD.remove_filter apr (fun var ->
            match V.find_metadata var with
            | Some (Global g) -> is_unprotected ask g
            | _ -> false
          )
        in
        {st with apr = apr_local}
    | `Normal
    | `Init
    | `Thread ->
      st

  let enter_multithreaded ask getg sideg (st: apron_components_t): apron_components_t =
    let apr = st.apr in
    (* Don't use keep_filter & remove_filter because it would duplicate find_metadata-s. *)
    let g_vars = List.filter (fun var ->
        match V.find_metadata var with
        | Some (Global _) -> true
        | _ -> false
      ) (AD.vars apr)
    in
    let apr_side = AD.keep_vars apr g_vars in
    sideg (mutex_inits ()) apr_side;
    let apr_local = AD.remove_vars apr g_vars in (* TODO: side effect initial values to mutex_globals? *)
    {st with apr = apr_local}

  let threadenter ask getg (st: apron_components_t): apron_components_t =
    {apr = AD.bot (); priv = startstate ()}

  let init () = ()
  let finalize () = finalize ()
end

(** May written variables. *)
module W =
struct
  include MayVars
  let name () = "W"
end

module type ClusterArg = functor (AD: ApronDomain.S2) ->
sig
  module LAD: Lattice.S

  val keep_only_protected_globals: Q.ask -> LockDomain.Addr.t -> LAD.t -> LAD.t
  val keep_global: varinfo -> LAD.t -> LAD.t

  val lock: AD.t -> LAD.t -> LAD.t -> AD.t
  val unlock: W.t -> AD.t -> LAD.t

  val name: unit -> string
end

(** No clustering. *)
module NoCluster:ClusterArg = functor (AD: ApronDomain.S2) ->
struct
  module AD = AD
  open CommonPerMutex(AD)
  module LAD = AD

  let keep_only_protected_globals = keep_only_protected_globals

  let keep_global g oct =
    let g_var = V.global g in
    AD.keep_vars oct [g_var]

  let lock oct local_m get_m =
    AD.meet oct (AD.join local_m get_m)

  let unlock w oct_side =
    oct_side

  let name () = "no-clusters"
end

module type ClusteringArg =
sig
  val generate: varinfo list -> varinfo list list
  val name: unit -> string
end

(** All clusters of size 1 and 2. *)
module Clustering12: ClusteringArg =
struct
  let generate gs =
    List.cartesian_product gs gs
    |> List.filter (fun (g1, g2) -> CilType.Varinfo.compare g1 g2 <= 0) (* filter flipped ordering, keep equals for next step *)
    |> List.map (fun (g1, g2) -> [g1; g2]) (* if g1 = g2, then we get a singleton cluster *)

  let name () = "cluster12"
end

(** All clusters of size 2. *)
(* Caution: It is questionable if this works 100%, see comment https://github.com/goblint/analyzer/pull/417/files#diff-58830344d2e5f5b8023333061fe1170c8a94a5e1ba32b98ccf6739eaf20d9123 *)
module Clustering2: ClusteringArg =
struct
  let generate gs =
    match gs with
    | [_] -> [gs] (* use clusters of size 1 if only 1 variable *)
    | _ ->
      List.cartesian_product gs gs
      |> List.filter (fun (g1, g2) -> CilType.Varinfo.compare g1 g2 < 0) (* filter flipped ordering, forbid equals for just clusters of size 2 *)
      |> List.map (fun (g1, g2) -> [g1; g2])

  let name () = "cluster2"
end

(** All subset clusters. *)
module ClusteringPower: ClusteringArg =
struct
  let generate gs =
    gs
    |> List.map (fun _ -> [true; false])
    |> List.n_cartesian_product (* TODO: exponential! *)
    |> List.map (fun bs ->
      (* list globals where omega is true *)
      List.fold_left2 (fun acc g b ->
          if b then
            g :: acc
          else
            acc
        ) [] gs bs
    )

  let name () = "clusterPow"
end

(** One maximum cluster. *)
module ClusteringMax: ClusteringArg =
struct
  let generate gs =
    [gs]

  let name () = "clusterMax"
end


(** Clusters when clustering is downward-closed. *)
module DownwardClosedCluster (ClusteringArg: ClusteringArg) =  functor (AD: ApronDomain.S2) ->
struct
  module AD = AD
  open CommonPerMutex(AD)

  module VS =
  struct
    include Printable.Std
    include SetDomain.Make (CilType.Varinfo)
  end
  module LAD = MapDomain.MapBot (VS) (AD)

  let keep_only_protected_globals ask m octs =
    (* normal (strong) mapping: contains only still fully protected *)
    (* must filter by protection to avoid later meeting with non-protecting *)
    LAD.filter (fun gs _ ->
        VS.for_all (is_protected_by ask m) gs
      ) octs

  let keep_global g octs =
    let g_var = V.global g in
    (* normal (strong) mapping: contains only still fully protected *)
    let g' = VS.singleton g in
    let oct = LAD.find g' octs in
    LAD.singleton g' (AD.keep_vars oct [g_var])

  let lock_get_m oct local_m get_m =
    let joined = LAD.join local_m get_m in
    if M.tracing then M.traceli "apronpriv" "lock_get_m:\n  get=%a\n  joined=%a\n" LAD.pretty get_m LAD.pretty joined;
    let r = LAD.fold (fun _ -> AD.meet) joined (AD.bot ()) in (* bot is top with empty env *)
    if M.tracing then M.trace "apronpriv" "meet=%a\n" AD.pretty r;
    let r = AD.meet oct r in
    if M.tracing then M.traceu "apronpriv" "-> %a\n" AD.pretty r;
    r

  let lock oct local_m get_m =
    if M.tracing then M.traceli "apronpriv" "cluster lock: local=%a\n" LAD.pretty local_m;
    let r = lock_get_m oct local_m get_m in
    (* is_bot check commented out because it's unnecessarily expensive *)
    (* if AD.is_bot_env r then
      failwith "DownwardClosedCluster.lock: not downward closed?"; *)
    if M.tracing then M.traceu "apronpriv" "-> %a\n" AD.pretty r;
    r

  let unlock w oct_side =
    let vars = AD.vars oct_side in
    let gs = List.map (fun var -> match V.find_metadata var with
        | Some (Global g) -> g
        | _ -> assert false (* oct_side should only contain (protected) globals *)
      ) vars
    in
    let clusters =
      ClusteringArg.generate gs
      |> List.map VS.of_list
      |> List.filter (VS.exists (fun g -> W.mem g w)) (* cluster intersection w is non-empty *)
    in
    let oct_side_cluster gs =
      AD.keep_vars oct_side (gs |> VS.elements |> List.map V.global)
    in
    LAD.add_list_fun clusters oct_side_cluster (LAD.empty ())

  let name = ClusteringArg.name
end

(** Clusters when clustering is arbitrary (not necessarily downward-closed). *)
module ArbitraryCluster (ClusteringArg: ClusteringArg): ClusterArg = functor (AD: ApronDomain.S2) ->
struct
  module AD = AD
  module DCCluster = (DownwardClosedCluster(ClusteringArg))(AD)

  open CommonPerMutex(AD)

  module VS = DCCluster.VS
  module LAD1 = DCCluster.LAD
  module LAD = Lattice.Prod (LAD1) (LAD1) (* second component is only used between keep_* and lock for additional weak mapping *)

  let name = ClusteringArg.name

  let filter_map' f m =
    LAD1.fold (fun k v acc ->
        match f k v with
        | Some (k', v') ->
          LAD1.add k' (AD.join (LAD1.find k' acc) v') acc
        | None ->
          acc
      ) m (LAD1.empty ())

  let keep_only_protected_globals ask m (octs, _) =
    let lad = DCCluster.keep_only_protected_globals ask m octs in
    let lad_weak =
      (* backup (weak) mapping: contains any still intersecting with protected, needed for decreasing protecting locksets *)
      filter_map' (fun gs oct ->
          (* must filter by protection to avoid later meeting with non-protecting *)
          let gs' = VS.filter (is_protected_by ask m) gs in
          if VS.is_empty gs' then
            None
          else
            (* must restrict cluster down to protected (join) *)
            Some (gs', keep_only_protected_globals ask m oct)
        ) octs
    in
    (lad, lad_weak)

  let keep_global g (octs, _) =
    let g_var = V.global g in
    let lad = DCCluster.keep_global g octs in
    let lad_weak =
      (* backup (weak) mapping: contains any still intersecting with protected, needed for decreasing protecting locksets *)
      filter_map' (fun gs oct ->
          (* must filter by protection to avoid later meeting with non-protecting *)
          if VS.mem g gs then
            (* must restrict cluster down to m_g (join) *)
            Some (VS.singleton g, AD.keep_vars oct [g_var])
          else
            None
        ) octs
    in
    (lad, lad_weak)

  let lock oct (local_m, _) (get_m, get_m') =
    if M.tracing then M.traceli "apronpriv" "cluster lock: local=%a\n" LAD1.pretty local_m;
    let r =
      let locked = DCCluster.lock_get_m oct local_m get_m in
      if AD.is_bot_env locked then (
        let locked' = DCCluster.lock_get_m oct local_m get_m' in
        if AD.is_bot_env locked' then
          raise Deadcode
        else
          locked'
      )
      else
        locked
    in
    if M.tracing then M.traceu "apronpriv" "-> %a\n" AD.pretty r;
    r

  let unlock w oct_side =
    (DCCluster.unlock w oct_side, LAD1.bot ())
end

(** Per-mutex meet with TIDs. *)
module PerMutexMeetPrivTID (Cluster: ClusterArg): S  = functor (AD: ApronDomain.S2) ->
struct
  open CommonPerMutex(AD)
  open ExplicitMutexGlobals
  include ConfCheck.RequireThreadFlagPathSensInit

  module NC = Cluster(AD)
  module Cluster = NC
  module LAD = NC.LAD

  (* Map from locks to last written values thread-locally *)
  module L = MapDomain.MapBot_LiftTop(Locksets.Lock)(LAD)

  module LMust = struct
    include Locksets.MustLockset
    let name () = "LMust"
  end

  module D = Lattice.Prod3 (W) (LMust) (L)
  module GMutex = MapDomain.MapBot_LiftTop(ThreadIdDomain.ThreadLifted)(LAD)
  module GThread = Lattice.Prod (LMust) (L)
  module G = Lattice.Lift2(GMutex)(GThread)(struct let bot_name = "bot" let top_name = "top" end)

  module V = ApronDomain.V
  module TID = ThreadIdDomain.Thread

  let name () = "PerMutexMeetPrivTID(" ^ (Cluster.name ()) ^ (if GobConfig.get_bool "exp.apron.priv.must-joined" then  ",join"  else "") ^ ")"

  let compatible (ask:Q.ask) current must_joined other =
    match current, other with
    | `Lifted current, `Lifted other ->
      let not_self_read = (not (TID.is_unique current)) || (not (TID.equal current other)) in
      let may_be_running () =
        if (not (TID.is_must_parent current other)) then
          true
        else
          let created = ask.f Q.CreatedThreads in
          let ident_or_may_be_created creator = TID.equal creator other || TID.may_create creator other in
          if ConcDomain.ThreadSet.is_top created then
            true
          else
            ConcDomain.ThreadSet.exists (ident_or_may_be_created) created
      in
      let may_not_be_joined () =
        try
          not @@ List.mem other (ConcDomain.ThreadSet.elements must_joined)
        with _ -> true
      in
      not_self_read && (not (GobConfig.get_bool "exp.apron.priv.not-started") || (may_be_running ())) && (not (GobConfig.get_bool "exp.apron.priv.must-joined") || (may_not_be_joined ()))
    | _ -> true

  let get_relevant_writes (ask:Q.ask) m v =
    let current = ThreadId.get_current ask in
    let must_joined = ask.f Queries.MustJoinedThreads in
    GMutex.fold (fun k v acc ->
        if compatible ask current must_joined k then
          LAD.join acc (Cluster.keep_only_protected_globals ask m v)
        else
          acc
      ) v (LAD.bot ())

  let get_relevant_writes_nofilter (ask:Q.ask) v =
    let current = ThreadId.get_current ask in
    let must_joined = ask.f Queries.MustJoinedThreads in
    GMutex.fold (fun k v acc ->
        if compatible ask current must_joined k then
          LAD.join acc v
        else
          acc
      ) v (LAD.bot ())

  let merge_all v =
    GMutex.fold (fun _ v acc -> LAD.join acc v) v (LAD.bot ())

  type apron_components_t =  ApronDomain.ApronComponents (AD) (D).t
  let global_varinfo = RichVarinfo.single ~name:"APRON_GLOBAL"


  let startstate () = W.bot (), LMust.top (), L.bot ()

  let should_join _ _ = true

  let mutex_inits = RichVarinfo.single ~name:"MUTEX_INITS"

  let get_m_with_mutex_inits inits ask getg_mutex m =
    let vi = mutex_addr_to_varinfo m in
    let get_m = get_relevant_writes ask m (getg_mutex vi) in
    if M.tracing then M.traceli "apronpriv" "get_m_with_mutex_inits %a\n  get=%a\n" LockDomain.Addr.pretty m LAD.pretty get_m;
    let r =
    if not inits then
      get_m
    else
      let get_mutex_inits = merge_all @@ getg_mutex (mutex_inits ()) in
      let get_mutex_inits' = Cluster.keep_only_protected_globals ask m get_mutex_inits in
      if M.tracing then M.trace "apronpriv" "inits=%a\n  inits'=%a\n" LAD.pretty get_mutex_inits LAD.pretty get_mutex_inits';
      LAD.join get_m get_mutex_inits'
    in
    if M.tracing then M.traceu "apronpriv" "-> %a\n" LAD.pretty r;
    r

  let get_mutex_global_g_with_mutex_inits inits ask getg_mutex g =
    let vi = mutex_global g in
    let get_mutex_global_g = get_relevant_writes_nofilter ask @@ getg_mutex vi in
    if M.tracing then M.traceli "apronpriv" "get_mutex_global_g_with_mutex_inits %a\n  get=%a\n" CilType.Varinfo.pretty g LAD.pretty get_mutex_global_g;
    let r =
    if not inits then
      get_mutex_global_g
    else
      let get_mutex_inits = merge_all @@ getg_mutex (mutex_inits ()) in
      let get_mutex_inits' = Cluster.keep_global g get_mutex_inits in
      if M.tracing then M.trace "apronpriv" "inits=%a\n  inits'=%a\n" LAD.pretty get_mutex_inits LAD.pretty get_mutex_inits';
      LAD.join get_mutex_global_g get_mutex_inits'
    in
    if M.tracing then M.traceu "apronpriv" "-> %a\n" LAD.pretty r;
    r

  let read_global ask getg (st: apron_components_t) g x: AD.t =
    let _,lmust,l = st.priv in
    let apr = st.apr in
    let m = Locksets.Lock.from_var (mutex_global g) in
    (* lock *)
    let tmp = (get_mutex_global_g_with_mutex_inits (not (LMust.mem m lmust)) ask getg g) in
    let local_m = BatOption.default (LAD.bot ()) (L.find_opt m l) in
    (* Additionally filter get_m in case it contains variables it no longer protects. E.g. in 36/22. *)
    let apr = Cluster.lock apr local_m tmp in
    (* read *)
    let g_var = V.global g in
    let x_var = Var.of_string x.vname in
    let apr_local = AD.add_vars apr [g_var] in
    let apr_local = AD.assign_var apr_local x_var g_var in
    (* unlock *)
    let apr_local' =
      if is_unprotected ask g then
        AD.remove_vars apr_local [g_var]
      else
        apr_local
    in
    apr_local'

  let write_global ?(invariant=false) (ask:Q.ask) getg sideg (st: apron_components_t) g x: apron_components_t =
    let w,lmust,l = st.priv in
    let mg = mutex_global g in
    let m = Locksets.Lock.from_var mg in
    let apr = st.apr in
    (* lock *)
    let tmp = (get_mutex_global_g_with_mutex_inits (not (LMust.mem m lmust)) ask getg g) in
    let local_m = BatOption.default (LAD.bot ()) (L.find_opt m l) in
    (* Additionally filter get_m in case it contains variables it no longer protects. E.g. in 36/22. *)
    let apr = Cluster.lock apr local_m tmp in
    (* write *)
    let g_var = V.global g in
    let x_var = Var.of_string x.vname in
    let apr_local = AD.add_vars apr [g_var] in
    let apr_local = AD.assign_var apr_local g_var x_var in
    (* unlock *)
    let apr_side = AD.keep_vars apr_local [g_var] in
    let apr_side = Cluster.unlock (W.singleton g) apr_side in
    let tid = ThreadId.get_current ask in
    let sidev = GMutex.singleton tid apr_side in
    sideg mg sidev;
    let l' = L.add m apr_side l in
    let apr_local' =
      if is_unprotected ask g then
        AD.remove_vars apr_local [g_var]
      else
        apr_local
    in
    {apr = apr_local'; priv = (W.add g w,LMust.add m lmust,l')}

  let lock ask getg (st: apron_components_t) m =
    let apr = st.apr in
    let _,lmust,l = st.priv in
    let get_m = get_m_with_mutex_inits (not (LMust.mem m lmust)) ask getg m in
    let local_m = BatOption.default (LAD.bot ()) (L.find_opt m l) in
    (* Additionally filter get_m in case it contains variables it no longer protects. E.g. in 36/22. *)
    let local_m = Cluster.keep_only_protected_globals ask m local_m in
    let apr = Cluster.lock apr local_m get_m in
    {st with apr}

  let unlock ask getg sideg (st: apron_components_t) m: apron_components_t =
    let apr = st.apr in
    let w,lmust,l = st.priv in
    let apr_local = remove_globals_unprotected_after_unlock ask m apr in
    let w' = W.filter (fun v -> not (is_unprotected_without ask v m)) w in
    let side_needed = W.exists (fun v -> is_protected_by ask m v) w in
    if not side_needed then
      {apr = apr_local; priv = (w',lmust,l)}
    else
      let apr_side = keep_only_protected_globals ask m apr in
      let apr_side = Cluster.unlock w apr_side in
      let tid = ThreadId.get_current ask in
      let sidev = GMutex.singleton tid apr_side in
      sideg (mutex_addr_to_varinfo m) sidev;
      let l' = L.add m apr_side l in
      {apr = apr_local; priv = (w',LMust.add m lmust,l')}

  let thread_join (ask:Q.ask) getg exp (st: apron_components_t) =
    let w,lmust,l = st.priv in
    let tids = ask.f (Q.EvalThread exp) in
    if ConcDomain.ThreadSet.is_top tids then
      st (* TODO: why needed? *)
    else (
      (* elements throws if the thread set is top *)
      let tids = ConcDomain.ThreadSet.elements tids in
      match tids with
      | [tid] ->
        let lmust',l' = getg tid in
        {st with priv = (w, LMust.union lmust' lmust, L.join l l')}
      | _ ->
        (* To match the paper more closely, one would have to join in the non-definite case too *)
        (* Given how we handle lmust (for initialization), doing this might actually be beneficial given that it grows lmust *)
        st
    )

  let thread_return ask getg sideg tid (st: apron_components_t) =
    let _,lmust,l = st.priv in
    sideg tid (lmust,l);
    st

  let sync (ask:Q.ask) getg sideg (st: apron_components_t) reason =
    match reason with
    | `Return -> st (* TODO: implement? *)
    | `Join ->
      if (ask.f Q.MustBeSingleThreaded) then
        st
      else
        let apr = st.apr in
        (* There can be no branched going multi-threaded here *)
        (* TODO: Do we need to remove no longer protected variables here? *)
        (* TODO: Is not potentially even unsound to do so?! *)
        let apr_local = AD.remove_filter apr (fun var ->
            match V.find_metadata var with
            | Some (Global g) -> is_unprotected ask g
            | _ -> false
          )
        in
        {st with apr = apr_local}
    | `Normal
    | `Init
    | `Thread ->
      st

  let enter_multithreaded (ask:Q.ask) getg sideg (st: apron_components_t): apron_components_t =
    let apr = st.apr in
    (* Don't use keep_filter & remove_filter because it would duplicate find_metadata-s. *)
    let g_vars = List.filter (fun var ->
        match V.find_metadata var with
        | Some (Global _) -> true
        | _ -> false
      ) (AD.vars apr)
    in
    let apr_side = AD.keep_vars apr g_vars in
    let apr_side = Cluster.unlock (W.top ()) apr_side in (* top W to avoid any filtering *)
    let tid = ThreadId.get_current ask in
    let sidev = GMutex.singleton tid apr_side in
    let vi = mutex_inits () in
    sideg vi sidev;
    (* Introduction into local state not needed, will be read via initializer *)
    (* Also no side-effect to mutex globals needed, the value here will either by read via the initializer, *)
    (* or it will be locally overwitten and in LMust in which case these values are irrelevant anyway *)
    let apr_local = AD.remove_vars apr g_vars in
    {st with apr = apr_local}

  let threadenter ask getg (st: apron_components_t): apron_components_t =
    let _,lmust,l = st.priv in
    {apr = AD.bot (); priv = (W.bot (),lmust,l)}

  let finalize () = finalize ()

  (* All that follows is stupid boilerplate to give each of these functions the getg and sideg that only deals with TIDs or Mutexes *)

  let sideg_mutex (sideg: varinfo -> G.t -> unit) (g:varinfo) (v:GMutex.t):unit =
    sideg g (`Lifted1 v)

  let sideg_tid (sideg:varinfo -> G.t -> unit) (tid:TID.t) (v:GThread.t):unit =
    sideg (TID.to_varinfo tid) (`Lifted2 v)

  let getg_mutex getg g = match getg g with
    | `Lifted1 v -> v
    | `Bot -> GMutex.bot ()
    | _ -> failwith "wrong either argument"

  let getg_tid getg tid = match getg (TID.to_varinfo tid) with
    | `Lifted2 v -> v
    | `Bot -> GThread.bot ()
    | _ -> failwith "wrong either argument"

  let patch_getside_mutex fn ask getg sideg = fn ask (getg_mutex getg) (sideg_mutex sideg)
  let patch_getside_tid fn ask getg sideg = fn ask (getg_tid getg) (sideg_tid sideg)

  let patch_get_mutex fn ask getg = fn ask (getg_mutex getg)
  let patch_get_tid fn ask getg = fn ask (getg_tid getg)

  let read_global = patch_get_mutex read_global
  let write_global ?(invariant=false) (ask:Q.ask) getg sideg = write_global ~invariant ask (getg_mutex getg) (sideg_mutex sideg)
  let lock = patch_get_mutex lock
  let unlock = patch_getside_mutex unlock
  let thread_join = patch_get_tid thread_join
  let thread_return = patch_getside_tid thread_return
  let sync = patch_getside_mutex sync
  let enter_multithreaded = patch_getside_mutex enter_multithreaded
  let threadenter = patch_get_mutex threadenter
end

module TracingPriv = functor (Priv: S) -> functor (AD: ApronDomain.S2) ->
struct
  module Priv = Priv (AD)
  include Priv

  module D = Priv.D
  module ApronComponents = ApronDomain.ApronComponents (AD) (D)

  let read_global ask getg st g x =
    if M.tracing then M.traceli "apronpriv" "read_global %a %a\n" d_varinfo g d_varinfo x;
    if M.tracing then M.trace "apronpriv" "st: %a\n" ApronComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "apronpriv" "getg %a -> %a\n" d_varinfo x G.pretty r;
      r
    in
    let r = Priv.read_global ask getg st g x in
    if M.tracing then M.traceu "apronpriv" "-> %a\n" AD.pretty r;
    r

  let write_global ?invariant ask getg sideg st g x =
    if M.tracing then M.traceli "apronpriv" "write_global %a %a\n" d_varinfo g d_varinfo x;
    if M.tracing then M.trace "apronpriv" "st: %a\n" ApronComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "apronpriv" "getg %a -> %a\n" d_varinfo x G.pretty r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "apronpriv" "sideg %a %a\n" d_varinfo x G.pretty v;
      sideg x v
    in
    let r = write_global ?invariant ask getg sideg st g x in
    if M.tracing then M.traceu "apronpriv" "-> %a\n" ApronComponents.pretty r;
    r

  let lock ask getg st m =
    if M.tracing then M.traceli "apronpriv" "lock %a\n" LockDomain.Addr.pretty m;
    if M.tracing then M.trace "apronpriv" "st: %a\n" ApronComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "apronpriv" "getg %a -> %a\n" d_varinfo x G.pretty r;
      r
    in
    let r = lock ask getg st m in
    if M.tracing then M.traceu "apronpriv" "-> %a\n" ApronComponents.pretty r;
    r

  let unlock ask getg sideg st m =
    if M.tracing then M.traceli "apronpriv" "unlock %a\n" LockDomain.Addr.pretty m;
    if M.tracing then M.trace "apronpriv" "st: %a\n" ApronComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "apronpriv" "getg %a -> %a\n" d_varinfo x G.pretty r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "apronpriv" "sideg %a %a\n" d_varinfo x G.pretty v;
      sideg x v
    in
    let r = unlock ask getg sideg st m in
    if M.tracing then M.traceu "apronpriv" "-> %a\n" ApronComponents.pretty r;
    r

  let enter_multithreaded ask getg sideg st =
    if M.tracing then M.traceli "apronpriv" "enter_multithreaded\n";
    if M.tracing then M.trace "apronpriv" "st: %a\n" ApronComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "apronpriv" "getg %a -> %a\n" d_varinfo x G.pretty r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "apronpriv" "sideg %a %a\n" d_varinfo x G.pretty v;
      sideg x v
    in
    let r = enter_multithreaded ask getg sideg st in
    if M.tracing then M.traceu "apronpriv" "-> %a\n" ApronComponents.pretty r;
    r

  let threadenter ask getg st =
    if M.tracing then M.traceli "apronpriv" "threadenter\n";
    if M.tracing then M.trace "apronpriv" "st: %a\n" ApronComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "apronpriv" "getg %a -> %a\n" d_varinfo x G.pretty r;
      r
    in
    let r = threadenter ask getg st in
    if M.tracing then M.traceu "apronpriv" "-> %a\n" ApronComponents.pretty r;
    r

  let sync ask getg sideg st reason =
    if M.tracing then M.traceli "apronpriv" "sync\n";
    if M.tracing then M.trace "apronpriv" "st: %a\n" ApronComponents.pretty st;
    let getg x =
      let r = getg x in
      if M.tracing then M.trace "apronpriv" "getg %a -> %a\n" d_varinfo x G.pretty r;
      r
    in
    let sideg x v =
      if M.tracing then M.trace "apronpriv" "sideg %a %a\n" d_varinfo x G.pretty v;
      sideg x v
    in
    let r = sync ask getg sideg st reason in
    if M.tracing then M.traceu "apronpriv" "-> %a\n" ApronComponents.pretty r;
    r
end


let priv_module: (module S) Lazy.t =
  lazy (
    let module Priv: S =
      (val match get_string "exp.apron.privatization" with
         | "dummy" -> (module Dummy : S)
         | "protection" -> (module ProtectionBasedPriv (struct let path_sensitive = false end))
         | "protection-path" -> (module ProtectionBasedPriv (struct let path_sensitive = true end))
         | "mutex-meet" -> (module PerMutexMeetPriv)
         | "mutex-meet-tid" -> (module PerMutexMeetPrivTID (NoCluster))
         | "mutex-meet-tid-cluster12" -> (module PerMutexMeetPrivTID (DownwardClosedCluster (Clustering12)))
         | "mutex-meet-tid-cluster2" -> (module PerMutexMeetPrivTID (ArbitraryCluster (Clustering2)))
         | "mutex-meet-tid-cluster-max" -> (module PerMutexMeetPrivTID (ArbitraryCluster (ClusteringMax)))
         | "mutex-meet-tid-cluster-power" -> (module PerMutexMeetPrivTID (DownwardClosedCluster (ClusteringPower)))
         | _ -> failwith "exp.apron.privatization: illegal value"
      )
    in
    let module Priv =
      (val if get_bool "exp.apron.priv.only-interval" then
          let module IntervalAD = ApronDomain.D2 (ApronDomain.IntervalManager) in
          (module (Converter (IntervalAD) (Priv)): S)
        else
          (module Priv))
    in
    let module Priv = TracingPriv (Priv) in
    (module Priv)
  )

let get_priv (): (module S) =
  Lazy.force priv_module
