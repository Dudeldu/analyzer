(** Incremental terminating top down solver that optionally only keeps values at widening points and restores other values afterwards. *)
(* Incremental: see paper 'Incremental Abstract Interpretation' https://link.springer.com/chapter/10.1007/978-3-030-41103-9_5 *)
(* TD3: see paper 'Three Improvements to the Top-Down Solver' https://dl.acm.org/doi/10.1145/3236950.3236967
 * Option exp.solver.td3.* (default) ? true : false (solver in paper):
 * - term (true) ? use phases for widen+narrow (TDside) : use box (TDwarrow)
 * - space (false) ? only keep values at widening points (TDspace + side) in rho : keep all values in rho
 * - space_cache (true) ? local cache l for eval calls in each solve (TDcombined) : no cache
 * - space_restore (true) ? eval each rhs and store all in rho : do not restore missing values
 * For simpler (but unmaintained) versions without the incremental parts see the paper or topDown{,_space_cache_term}.ml.
 *)

open Prelude
open Analyses
open Constraints
open Messages
open CompareAST
open Cil

module WP =
  functor (S:EqConstrSys) ->
  functor (HM:Hash.H with type key = S.v) ->
  struct
    module S = Constraints.CachingEqConstrSys (S) (* TODO: don't hardcode into just TD3 *)

    include Generic.SolverStats (S) (HM)
    module VS = Set.Make (S.Var)

    type solver_data = {
      mutable st: (S.Var.t * S.Dom.t) list; (* needed to destabilize start functions if their start state changed because of some changed global initializer *)
      mutable infl: VS.t HM.t;
      mutable sides: VS.t HM.t;
      mutable rho: S.Dom.t HM.t;
      mutable wpoint: unit HM.t;
      mutable stable: unit HM.t
    }

    let create_empty_data () = {
      st = [];
      infl = HM.create 10;
      sides = HM.create 10;
      rho = HM.create 10;
      wpoint = HM.create 10;
      stable = HM.create 10
    }

    let clear_data data =
      HM.clear data.infl;
      HM.clear data.stable

    let print_data data str =
      if GobConfig.get_bool "dbg.verbose" then
        Printf.printf "%s:\n|rho|=%d\n|stable|=%d\n|infl|=%d\n|wpoint|=%d\n"
          str (HM.length data.rho) (HM.length data.stable) (HM.length data.infl) (HM.length data.wpoint)

    let exists_key f hm = HM.fold (fun k _ a -> a || f k) hm false

    module P =
    struct
      type t = S.Var.t * S.Var.t [@@deriving eq]
      let hash  (x1,x2)         = (S.Var.hash x1 * 13) + S.Var.hash x2
    end

    module HPM = Hashtbl.Make (P)

    type phase = Widen | Narrow

    exception AbortEq

    let solve box st vs data =
      let term  = GobConfig.get_bool "exp.solver.td3.term" in
      let side_widen = GobConfig.get_string "exp.solver.td3.side_widen" in
      let space = GobConfig.get_bool "exp.solver.td3.space" in
      let cache = GobConfig.get_bool "exp.solver.td3.space_cache" in
      let called = HM.create 10 in
      let called_changed = HM.create 10 in

      let infl = data.infl in
      let sides = data.sides in
      let rho = data.rho in
      let wpoint = data.wpoint in
      let stable = data.stable in

      let abort = GobConfig.get_bool "exp.solver.td3.abort" in
      let destab_infl = HM.create 10 in
      let destab_front = HM.create 10 in
      let destab_dep = HM.create 10 in

      let abort_verify = GobConfig.get_bool "exp.solver.td3.abort-verify" in
      let prev_dep_vals = HM.create 10 in

      let () = print_solver_stats := fun () ->
        Printf.printf "|rho|=%d\n|called|=%d\n|stable|=%d\n|infl|=%d\n|wpoint|=%d\n"
          (HM.length rho) (HM.length called) (HM.length stable) (HM.length infl) (HM.length wpoint);
        print_context_stats rho
      in

      if GobConfig.get_bool "incremental.load" then print_data data "Loaded data for incremental analysis";

      let cache_sizes = ref [] in

      let trace_called () =
        if tracing then (
          let called_pretty () called =
            HM.fold (fun x _ acc -> Pretty.dprintf "%a\n  %a" S.Var.pretty_trace x Pretty.insert acc) called Pretty.nil
          in
          trace "sol2" "called:\n  %a\n" called_pretty called
        )
      in
      let vs_pretty () vs =
        VS.fold (fun x acc -> Pretty.dprintf "%a, %a" S.Var.pretty_trace x Pretty.insert acc) vs Pretty.nil
      in

      let add_infl y x =
        if tracing then trace "sol2" "add_infl %a %a\n" S.Var.pretty_trace y S.Var.pretty_trace x;
        HM.replace infl y (VS.add x (try HM.find infl y with Not_found -> VS.empty));
      in
      let add_sides y x = HM.replace sides y (VS.add x (try HM.find infl y with Not_found -> VS.empty)) in
      let rec destabilize ?(front=true) x =
        if tracing then trace "sol2" "destabilize %a\n" S.Var.pretty_trace x;
        trace_called ();
        let w = HM.find_default infl x VS.empty in
        HM.replace infl x VS.empty;
        if abort then (
          if front then (
            VS.iter (fun y ->
                if tracing then trace "sol2" "front add %a (infl)\n" S.Var.pretty_trace y;
                HM.replace destab_front y ()
              ) w;
            (* Also add front via destab_infl in case infl has already been removed by previous destabilize.
               This fixes 29-svcomp/27-td3-front-via-destab-infl. *)
            VS.iter (fun y ->
                if tracing then trace "sol2" "front add %a (destab_infl)\n" S.Var.pretty_trace y;
                HM.replace destab_front y ()
              ) (HM.find_default destab_infl x VS.empty)
          )
          else (
            HM.replace destab_infl x (VS.union w (HM.find_default destab_infl x VS.empty));
            VS.iter (fun y ->
                HM.replace destab_dep y (VS.add x (try HM.find destab_dep y with Not_found -> VS.empty))
              ) w
          )
        );
        VS.iter (fun y ->
            if tracing then trace "sol2" "stable remove %a\n" S.Var.pretty_trace y;
            HM.remove stable y;
            if not (HM.mem called y) then destabilize ~front:false y
          ) w
      and destabilize_vs x = (* TODO remove? Only used for side_widen cycle. *)
        if tracing then trace "sol2" "destabilize_vs %a\n" S.Var.pretty_trace x;
        let w = HM.find_default infl x VS.empty in
        HM.replace infl x VS.empty;
        VS.fold (fun y b ->
            let was_stable = HM.mem stable y in
            HM.remove stable y;
            HM.mem called y || destabilize_vs y || b || was_stable && List.mem y vs
          ) w false
      and solve ?(abortable=true) x phase (changed: bool): bool =
        if tracing then trace "sol2" "solve %a, phase: %s, changed: %b, abortable: %b, called: %b, stable: %b\n" S.Var.pretty_trace x (match phase with Widen -> "Widen" | Narrow -> "Narrow") changed abortable (HM.mem called x) (HM.mem stable x);
        trace_called ();
        init x;
        assert (S.system x <> None);
        if not (HM.mem called x || HM.mem stable x) then (
          if tracing then trace "sol2" "stable add %a\n" S.Var.pretty_trace x;
          HM.replace stable x ();
          HM.replace called x ();
          let wp = HM.mem wpoint x in
          let old = HM.find rho x in
          let l = HM.create 10 in
          let prev_dep_vals_x = HM.find_default prev_dep_vals x (HM.create 0) in (* used by abort_verify *)
          let eval' =
            if tracing then trace "sol2" "eval' %a abortable=%b destab_dep=%b\n" S.Var.pretty_trace x abortable (HM.mem destab_dep x);
            if abort && abortable && HM.mem destab_dep x then (
              let unasked_dep_x = ref (HM.find destab_dep x) in
              if tracing then trace "sol2" "eval' %a dep=%a\n" S.Var.pretty_trace x vs_pretty !unasked_dep_x;
              let all_dep_x_unchanged = ref true in
              let all_dep_x_unchanged_verify = ref true in
              fun y ->
                let (d, changed) = eval l x y in
                if tracing then trace "sol2" "eval' %a asked %a changed=%b mem=%b\n" S.Var.pretty_trace x S.Var.pretty_trace y changed (VS.mem y !unasked_dep_x);
                if abort_verify then (
                  let prev_d = HM.find_default prev_dep_vals_x y (S.Dom.bot ()) in
                  if not (S.Dom.equal prev_d d) then (
                    (* TODO: this is not a bug? change might be covered by destab_front *)
                    (* if not changed then (
                      ignore (Pretty.eprintf "not changed did change: eval %a %a: \nold=%a\n new=%a\n" S.Var.pretty_trace x S.Var.pretty_trace y S.Dom.pretty prev_d S.Dom.pretty d)
                    ); *)
                    all_dep_x_unchanged_verify := false;
                    if tracing then trace "sol2" "eval' %a asked %a abort %B verify\n  prev=%a\n   now=%a\n" S.Var.pretty_trace x S.Var.pretty_trace y (HM.mem prev_dep_vals_x y) S.Dom.pretty prev_d S.Dom.pretty d;
                  )
                );
                if VS.mem y !unasked_dep_x then (
                  unasked_dep_x := VS.remove y !unasked_dep_x;
                  if changed then
                    all_dep_x_unchanged := false;
                  if tracing then trace "sol2" "eval' %a asked %a checking abort unasked=%a all_unchanged=%b front=%b\n" S.Var.pretty_trace x S.Var.pretty_trace y vs_pretty !unasked_dep_x !all_dep_x_unchanged (HM.mem destab_front x);
                  let should_abort = VS.is_empty !unasked_dep_x && !all_dep_x_unchanged && not (HM.mem destab_front x) in (* must check front here, because each eval might change it for x *)
                  let should_abort_verify = !all_dep_x_unchanged_verify in
                  if should_abort then (
                    if abort_verify && not should_abort_verify then (
                      failwith (Pretty.sprint ~width:max_int (Pretty.dprintf "TD3 abort verify: should not abort %a" S.Var.pretty_trace x));
                    );
                    raise AbortEq
                  )
                );
                d
            )
            else
              fun y ->
                let (d, changed) = eval l x y in
                d
          in
          let tmp =
            try
              if abort && abort_verify then (
                (* collect dep vals for x *)
                let new_dep_vals_x = HM.create (HM.length prev_dep_vals_x) in
                let eval' y =
                  let d = eval' y in
                  HM.replace new_dep_vals_x y d;
                  d
                in
                let tmp = eq x eval' (side x) in
                HM.replace prev_dep_vals x new_dep_vals_x;
                tmp
              )
              else
                eq x eval' (side x)
            with AbortEq ->
              abort_rhs_event x;
              if tracing then trace "sol2" "eq aborted %a\n" S.Var.pretty_trace x;
              HM.remove destab_dep x; (* TODO: safe to remove here? doesn't prevent some aborts? *)
              (* prev_dep_vals remain the same *)
              old
          in
          (* let tmp = if GobConfig.get_bool "ana.opt.hashcons" then S.Dom.join (S.Dom.bot ()) tmp else tmp in (* Call hashcons via dummy join so that the tag of the rhs value is up to date. Otherwise we might get the same value as old, but still with a different tag (because no lattice operation was called after a change), and since Printable.HConsed.equal just looks at the tag, we would uneccessarily destabilize below. Seems like this does not happen. *) *)
          if tracing then trace "sol" "Var: %a\n" S.Var.pretty_trace x ;
          if tracing then trace "sol" "Contrib:%a\n" S.Dom.pretty tmp;
          HM.remove called x;
          HM.remove called_changed x;
          let tmp =
            if not wp then tmp
            else
              if term then
                match phase with Widen -> S.Dom.widen old (S.Dom.join old tmp) | Narrow -> S.Dom.narrow old tmp
              else
                box x old tmp
          in
          if tracing then trace "sol" "Old value:%a\n" S.Dom.pretty old;
          if tracing then trace "sol" "New Value:%a\n" S.Dom.pretty tmp;
          if tracing then trace "cache" "cache size %d for %a\n" (HM.length l) S.Var.pretty_trace x;
          cache_sizes := HM.length l :: !cache_sizes;
          if not (Stats.time "S.Dom.equal" (fun () -> S.Dom.equal old tmp) ()) then (
            if tracing then trace "sol" "Changed\n";
            update_var_event x old tmp;
            HM.replace rho x tmp;
            HM.replace called_changed x ();
            if abort then (
              if HM.mem destab_front x then (
                if HM.mem stable x then (
                  (* If some side during eq made x unstable, then it should remain in destab_front.
                    Otherwise recursive solve might prematurely abort it. *)
                  if tracing then trace "sol2" "front remove %a\n" S.Var.pretty_trace x;
                  HM.remove destab_front x;
                );
                if HM.mem destab_infl x then (
                  VS.iter (fun y ->
                      if tracing then trace "sol2" "pushing front from %a to %a\n" S.Var.pretty_trace x S.Var.pretty_trace y;
                      if tracing then trace "sol2" "front add %a\n" S.Var.pretty_trace y;
                      HM.replace destab_front y ()
                    ) (HM.find destab_infl x)
                );
                HM.remove destab_infl x
              )
            );
            destabilize x;
            (solve[@tailcall]) x phase true
          ) else (
            (* TODO: why non-equal and non-stable checks in switched order compared to TD3 paper? *)
            if not (HM.mem stable x) then (
              (* If some side during eq made x unstable, then it should remain in destab_front.
                 Otherwise recursive solve might prematurely abort it. *)
              if tracing then trace "sol2" "solve still unstable %a\n" S.Var.pretty_trace x;
              (solve[@tailcall]) x Widen changed
            ) else (
              if abort && HM.mem destab_front x then (
                if tracing then trace "sol2" "front remove %a\n" S.Var.pretty_trace x;
                HM.remove destab_front x;
                if tracing then trace "sol2" "not pushing front from %a\n" S.Var.pretty_trace x;
                (* don't push front here *)
                HM.remove destab_infl x
              );
              if term && phase = Widen && HM.mem wpoint x then ( (* TODO: or use wp? *)
                if tracing then trace "sol2" "solve switching to narrow %a\n" S.Var.pretty_trace x;
                if tracing then trace "sol2" "stable remove %a\n" S.Var.pretty_trace x;
                HM.remove stable x;
                (solve[@tailcall]) ~abortable:false x Narrow changed
              ) else if not space && (not term || phase = Narrow) then ( (* this makes e.g. nested loops precise, ex. tests/regression/34-localization/01-nested.c - if we do not remove wpoint, the inner loop head will stay a wpoint and widen the outer loop variable. *)
                if tracing then trace "sol2" "solve removing wpoint %a (%b)\n" S.Var.pretty_trace x (HM.mem wpoint x);
                HM.remove wpoint x;
                changed
              )
              else
                changed
            )
          )
        )
        else if HM.mem called x then
          changed || HM.mem called_changed x
        else
          changed
      and eq x get set =
        if tracing then trace "sol2" "eq %a\n" S.Var.pretty_trace x;
        eval_rhs_event x;
        match S.system x with
        | None -> S.Dom.bot ()
        | Some f -> f get set
      and simple_solve l x y: S.d * bool =
        if tracing then trace "sol2" "simple_solve %a (rhs: %b)\n" S.Var.pretty_trace y (S.system y <> None);
        if S.system y = None then (init y; (HM.find rho y, true (* TODO: ??? *))) else
        if HM.mem rho y || not space then (let changed = solve y Widen false in (HM.find rho y, changed)) else
        if abort then failwith "space abort unimplemented" else
        if HM.mem called y then (init y; HM.remove l y; (HM.find rho y, true (* TODO: ??? *))) else
        (* if HM.mem called y then (init y; let y' = HM.find_default l y (S.Dom.bot ()) in HM.replace rho y y'; HM.remove l y; y') else *)
        if cache && HM.mem l y then (HM.find l y, true (* TODO: ??? *))
        else (
          HM.replace called y ();
          (* TODO: abort? *)
          let tmp = eq y (fun z -> fst (eval l x z)) (side x) in
          HM.remove called y;
          if HM.mem rho y then (HM.remove l y; ignore (solve y Widen false); (HM.find rho y, true (* TODO: ??? *)))
          else (if cache then HM.replace l y tmp; (tmp, true (* TODO: ??? *)))
        )
      and eval l x y: S.d * bool =
        if tracing then trace "sol2" "eval %a ## %a\n" S.Var.pretty_trace x S.Var.pretty_trace y;
        get_var_event y;
        if HM.mem called y then HM.replace wpoint y ();
        let tmp = simple_solve l x y in
        if HM.mem rho y then add_infl y x;
        if tracing then trace "sol2" "eval %a ## %a -> %a\n" S.Var.pretty_trace x S.Var.pretty_trace y S.Dom.pretty (fst tmp);
        tmp
      and side x y d = (* side from x to y; only to variables y w/o rhs; x only used for trace *)
        if tracing then trace "sol2" "side to %a (wpx: %b) from %a ## value: %a\n" S.Var.pretty_trace y (HM.mem wpoint y) S.Var.pretty_trace x S.Dom.pretty d;
        if S.system y <> None then (
          ignore @@ Pretty.printf "side-effect to unknown w/ rhs: %a, contrib: %a\n" S.Var.pretty_trace y S.Dom.pretty d;
        );
        assert (S.system y = None);
        init y;
        if side_widen = "unstable_self" then add_infl x y;
        let op =
          if HM.mem wpoint y then fun a b ->
            if M.tracing then M.traceli "sol2" "side widen %a %a\n" S.Dom.pretty a S.Dom.pretty b;
            let r = S.Dom.widen a (S.Dom.join a b) in
            if M.tracing then M.traceu "sol2" "-> %a\n" S.Dom.pretty r;
            r
          else S.Dom.join
        in
        let old = HM.find rho y in
        let tmp = op old d in
        if tracing then trace "sol2" "stable add %a\n" S.Var.pretty_trace y;
        HM.replace stable y ();
        if not (S.Dom.leq tmp old) then (
          (* if there already was a `side x y d` that changed rho[y] and now again, we make y a wpoint *)
          let sided = VS.mem x (HM.find_default sides y VS.empty) in
          if not sided then add_sides y x;
          (* HM.replace rho y ((if HM.mem wpoint y then S.Dom.widen old else identity) (S.Dom.join old d)); *)
          HM.replace rho y tmp;
          if side_widen <> "cycle" then destabilize y;
          (* make y a widening point if ... This will only matter for the next side _ y.  *)
          let wpoint_if e = if e then HM.replace wpoint y () in
          match side_widen with
          | "always" -> (* Any side-effect after the first one will be widened which will unnecessarily lose precision. *)
            wpoint_if true
          | "never" -> (* On side-effect cycles, this should terminate via the outer `solver` loop. TODO check. *)
            wpoint_if false
          | "sides" -> (* x caused more than one update to y. >=3 partial context calls will be precise since sides come from different x. TODO this has 8 instead of 5 phases of `solver` for side_cycle.c *)
            wpoint_if sided
          | "cycle" -> (* destabilized a called or start var. Problem: two partial context calls will be precise, but third call will widen the state. *)
            (* if this side destabilized some of the initial unknowns vs, there may be a side-cycle between vs and we should make y a wpoint *)
            let destabilized_vs = destabilize_vs y in
            wpoint_if destabilized_vs
          (* TODO: The following two don't check if a vs got destabilized which may be a problem. *)
          | "unstable_self" -> (* TODO test/remove. Side to y destabilized itself via some infl-cycle. The above add_infl is only required for this option. Check for which examples this is problematic! *)
            wpoint_if @@ not (HM.mem stable y)
          | "unstable_called" -> (* TODO test/remove. Widen if any called var (not just y) is no longer stable. Expensive! *)
            wpoint_if @@ exists_key (neg (HM.mem stable)) called (* this is very expensive since it folds over called! see https://github.com/goblint/analyzer/issues/265#issuecomment-880748636 *)
          | x -> failwith ("Unknown value '" ^ x ^ "' for option exp.solver.td3.side_widen!")
        )
      and init x =
        if tracing then trace "sol2" "init %a\n" S.Var.pretty_trace x;
        if not (HM.mem rho x) then (
          new_var_event x;
          HM.replace rho x (S.Dom.bot ())
        )
      in

      let set_start (x,d) =
        if tracing then trace "sol2" "set_start %a ## %a\n" S.Var.pretty_trace x S.Dom.pretty d;
        init x;
        HM.replace rho x d;
        HM.replace stable x ();
        (* solve x Widen *)
      in

      start_event ();

      if GobConfig.get_bool "incremental.load" then (
        let c = S.increment.changes in
        List.(Printf.printf "change_info = { unchanged = %d; changed = %d; added = %d; removed = %d }\n" (length c.unchanged) (length c.changed) (length c.added) (length c.removed));
        (* If a global changes because of some assignment inside a function, we reanalyze,
         * but if it changes because of a different global initializer, then
         *   if not exp.earlyglobs: the contexts of start functions will change, we don't find the value in rho and reanalyze;
         *   if exp.earlyglobs: the contexts will be the same since they don't contain the global, but the start state will be different!
         *)
        print_endline "Destabilizing start functions if their start state changed...";
        (* ignore @@ Pretty.printf "st: %d, data.st: %d\n" (List.length st) (List.length data.st); *)
        List.iter (fun (v,d) ->
          match GU.assoc_eq v data.st S.Var.equal with
          | Some d' ->
              if S.Dom.equal d d' then
                (* ignore @@ Pretty.printf "Function %a has the same state %a\n" S.Var.pretty_trace v S.Dom.pretty d *)
                ()
              else (
                ignore @@ Pretty.printf "Function %a has changed start state: %a\n" S.Var.pretty_trace v S.Dom.pretty_diff (d, d');
                destabilize v
              )
          | None -> ignore @@ Pretty.printf "New start function %a not found in old list!\n" S.Var.pretty_trace v
        ) st;

        print_endline "Destabilizing changed functions...";

        (* We need to destabilize all nodes in changed functions *)
        let filter_map f l =
          List.fold_left (fun acc el -> match f el with Some x -> x::acc | _ -> acc) [] l
        in
        let obsolete_funs = filter_map (fun c -> match c.old with GFun (f,l) -> Some f | _ -> None) S.increment.changes.changed in
        let removed_funs = filter_map (fun g -> match g with GFun (f,l) -> Some f | _ -> None) S.increment.changes.removed in
        (* TODO: don't use string-based nodes, make obsolete of type Node.t BatSet.t *)
        let obsolete = Set.union (Set.of_list (List.map (fun a -> Node.show_id (Function a))  obsolete_funs))
                                 (Set.of_list (List.map (fun a -> Node.show_id (FunctionEntry a))  obsolete_funs)) in

        List.iter (fun a -> print_endline ("Obsolete function: " ^ a.svar.vname)) obsolete_funs;

        (* Actually destabilize all nodes contained in changed functions. TODO only destabilize fun_... nodes *)
        HM.iter (fun k v -> if Set.mem (S.Var.var_id k) obsolete then destabilize k) stable; (* TODO: don't use string-based nodes *)

        (* We remove all unknowns for program points in changed or removed functions from rho, stable, infl and wpoint *)
        (* TODO: don't use string-based nodes, make marked_for_deletion of type unit (Hashtbl.Make (Node)).t *)
        let add_nodes_of_fun (functions: fundec list) (nodes)=
          let add_stmts (f: fundec) =
            List.iter (fun s -> Hashtbl.replace nodes (Node.show_id (Statement s)) ()) (f.sallstmts)
          in
          List.iter (fun f -> Hashtbl.replace nodes (Node.show_id (FunctionEntry f)) (); Hashtbl.replace nodes (Node.show_id (Function f)) (); add_stmts f; Hashtbl.replace nodes (string_of_int (CfgTools.get_pseudo_return_id f)) ()) functions;
        in

        let marked_for_deletion = Hashtbl.create 103 in
        add_nodes_of_fun obsolete_funs marked_for_deletion;
        add_nodes_of_fun removed_funs marked_for_deletion;

        print_endline "Removing data for changed and removed functions...";
        let delete_marked s = HM.iter (fun k v -> if Hashtbl.mem  marked_for_deletion (S.Var.var_id k) then HM.remove s k ) s in (* TODO: don't use string-based nodes *)
        delete_marked rho;
        delete_marked infl;
        delete_marked wpoint;
        delete_marked stable;

        print_data data "Data after clean-up"
      );

      List.iter set_start st;
      List.iter init vs;
      (* If we have multiple start variables vs, we might solve v1, then while solving v2 we side some global which v1 depends on with a new value. Then v1 is no longer stable and we have to solve it again. *)
      let i = ref 0 in
      let rec solver () = (* as while loop in paper *)
        incr i;
        let unstable_vs = List.filter (neg (HM.mem stable)) vs in
        if unstable_vs <> [] then (
          if GobConfig.get_bool "dbg.verbose" then (
            if !i = 1 then print_newline ();
            Printf.printf "Unstable solver start vars in %d. phase:\n" !i;
            List.iter (fun v -> ignore @@ Pretty.printf "\t%a\n" S.Var.pretty_trace v) unstable_vs;
            print_newline ();
            flush_all ();
          );
          List.iter (fun x -> ignore (solve x Widen false)) unstable_vs;
          solver ();
        )
      in
      solver ();
      (* Before we solved all unstable vars in rho with a rhs in a loop. This is unneeded overhead since it also solved unreachable vars (reachability only removes those from rho further down). *)
      (* After termination, only those variables are stable which are
       * - reachable from any of the queried variables vs, or
       * - effected by side-effects and have no constraints on their own (this should be the case for all of our analyses). *)

      (* verifies values at widening points and adds values for variables in-between *)
      let visited = HM.create 10 in
      let check_side x y d =
        HM.replace visited y ();
        let mem = HM.mem rho y in
        let d' = try HM.find rho y with Not_found -> S.Dom.bot () in
        if not (S.Dom.leq d d') then ignore @@ Pretty.printf "TDFP Fixpoint not reached in restore step at side-effected variable (mem: %b) %a from %a: %a not leq %a\n" mem S.Var.pretty_trace y S.Var.pretty_trace x S.Dom.pretty d S.Dom.pretty d'
      in
      let rec eq check x =
        HM.replace visited x ();
        match S.system x with
        | None -> if HM.mem rho x then HM.find rho x else (ignore @@ Pretty.printf "TDFP Found variable %a w/o rhs and w/o value in rho\n" S.Var.pretty_trace x; S.Dom.bot ())
        | Some f -> f (get ~check) (check_side x)
      and get ?(check=false) x =
        if HM.mem visited x then (
          HM.find rho x
        ) else if HM.mem rho x then ( (* `vs` are in `rho`, so to restore others we need to skip to `eq`. *)
          let d1 = HM.find rho x in
          let d2 = eq check x in (* just to reach unrestored variables *)
          if check then (
            if not (HM.mem stable x) && S.system x <> None then ignore @@ Pretty.printf "TDFP Found an unknown in rho that should be stable: %a\n" S.Var.pretty_trace x;
            if not (S.Dom.leq d2 d1) then
              ignore @@ Pretty.printf "TDFP Fixpoint not reached in restore step at %a\n  @[Variable:\n%a\nRight-Hand-Side:\n%a\nCalculating one more step changes: %a\n@]" S.Var.pretty_trace x S.Dom.pretty d1 S.Dom.pretty d2 S.Dom.pretty_diff (d1,d2);
          );
          d1
        ) else (
          let d = eq check x in
          HM.replace rho x d;
          d
        )
      in
      (* restore values for non-widening-points *)
      if space && GobConfig.get_bool "exp.solver.td3.space_restore" then (
        if GobConfig.get_bool "dbg.verbose" then
          print_endline ("Restoring missing values.");
        let restore () =
          let get x =
            let d = get ~check:true x in
            if tracing then trace "sol2" "restored var %a ## %a\n" S.Var.pretty_trace x S.Dom.pretty d
          in
          List.iter get vs;
          HM.iter (fun x v -> if not (HM.mem visited x) then HM.remove rho x) rho
        in
        Stats.time "restore" restore ();
        if GobConfig.get_bool "dbg.verbose" then ignore @@ Pretty.printf "Solved %d vars. Total of %d vars after restore.\n" !Goblintutil.vars (HM.length rho);
        let avg xs = if List.is_empty !cache_sizes then 0.0 else float_of_int (BatList.sum xs) /. float_of_int (List.length xs) in
        if tracing then trace "cache" "#caches: %d, max: %d, avg: %.2f\n" (List.length !cache_sizes) (List.max !cache_sizes) (avg !cache_sizes);
      );

      let reachability xs =
        let reachable = HM.create (HM.length rho) in
        let rec one_var x =
          if not (HM.mem reachable x) then (
            HM.replace reachable x ();
            match S.system x with
            | None -> ()
            | Some x -> one_constraint x
          )
        and one_constraint f =
          ignore (f (fun x -> one_var x; try HM.find rho x with Not_found -> ignore @@ Pretty.printf "reachability: one_constraint: could not find variable %a\n" S.Var.pretty_trace x; S.Dom.bot ()) (fun x _ -> one_var x))
        in
        List.iter one_var xs;
        HM.iter (fun x v -> if not (HM.mem reachable x) then HM.remove rho x) rho;
      in
      reachability vs;

      stop_event ();
      print_data data "Data after solve completed";

      if GobConfig.get_bool "dbg.print_wpoints" then (
        Printf.printf "\nWidening points:\n";
        HM.iter (fun k () -> ignore @@ Pretty.printf "%a\n" S.Var.pretty_trace k) wpoint;
        print_newline ();
      );

      {st; infl; sides; rho; wpoint; stable}

    let solve box st vs =
      let reuse_stable = GobConfig.get_bool "incremental.stable" in
      let reuse_wpoint = GobConfig.get_bool "incremental.wpoint" in
      if GobConfig.get_bool "incremental.load" then (
        let loaded, data = match S.increment.old_data with
          | Some d -> true, Obj.obj d.solver_data
          | _ -> false, create_empty_data ()
        in
        (* This hack is for fixing hashconsing.
         * If hashcons is enabled now, then it also was for the loaded values (otherwise it would crash). If it is off, we don't need to do anything.
         * HashconsLifter uses BatHashcons.hashcons on Lattice operations like join, so we call join (with bot) to make sure that the old values will populate the empty hashcons table via side-effects and at the same time get new tags that are conform with its state.
         * The tags are used for `equals` and `compare` to avoid structural comparisons. TODO could this be replaced by `==` (if values are shared by hashcons they should be physically equal)?
         * We have to replace all tags since they are not derived from the value (like hash) but are incremented starting with 1, i.e. dependent on the order in which lattice operations for different values are called, which will very likely be different for an incremental run.
         * If we didn't do this, during solve, a rhs might give the same value as from the old rho but it wouldn't be detected as equal since the tags would be different.
         * In the worst case, every rhs would yield the same value, but we would destabilize for every var in rho until we replaced all values (just with new tags).
         * The other problem is that we would likely use more memory since values from old rho would not be shared with the same values in the hashcons table. So we would keep old values in memory until they are replace in rho and eventually garbage collected.
         *)
        (* Another problem are the tags for the context part of a S.Var.t.
         * This will cause problems when old and new vars interact or when new S.Dom values are used as context:
         * - reachability is a problem since it marks vars reachable with a new tag, which will remove vars with the same context but old tag from rho.
         * - If we destabilized a node with a call, we will also destabilize all vars of the called function. However, if we end up with the same state at the caller node, without hashcons we would only need to go over all vars in the function once to restabilize them since we have
         *   the old values, whereas with hashcons, we would get a context with a different tag, could not find the old value for that var, and have to recompute all vars in the function (without access to old values).
         *)
        if loaded && GobConfig.get_bool "ana.opt.hashcons" then (
          HM.iter (fun k v ->
            HM.remove data.rho k; (* remove old values *)
            (* call hashcons on contexts and abstract values; results in new tags *)
            let k' = S.Var.relift k in
            let v' = S.Dom.join (S.Dom.bot ()) v in
            HM.replace data.rho k' v';
          ) data.rho;
          HM.iter (fun k v ->
            HM.remove data.stable k;
            HM.replace data.stable (S.Var.relift k) v
          ) data.stable;
          HM.iter (fun k v ->
            HM.remove data.wpoint k;
            HM.replace data.wpoint (S.Var.relift k) v
          ) data.wpoint;
          HM.iter (fun k v ->
            HM.remove data.infl k;
            HM.replace data.infl (S.Var.relift k) (VS.map S.Var.relift v)
          ) data.infl;
          data.st <- List.map (fun (k, v) -> S.Var.relift k, S.Dom.join (S.Dom.bot ()) v) data.st;
        );
        if not reuse_stable then (
          print_endline "Destabilizing everything!";
          data.stable <- HM.create 10;
          data.infl <- HM.create 10
        );
        if not reuse_wpoint then data.wpoint <- HM.create 10;
        let result = solve box st vs data in
        result.rho, Obj.repr result
      )
      else (
        let data = create_empty_data () in
        let result = solve box st vs data in
        result.rho, Obj.repr result
      )
  end

let _ =
  let module WP = GlobSolverFromEqSolver (WP) in
  Selector.add_solver ("td3", (module WP : GenericGlobSolver));
