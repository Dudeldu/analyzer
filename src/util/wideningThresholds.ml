open Cil
module Thresholds = Set.Make(Z)
module ThresholdsFloat = Set.Make(Float)


class extractConstantsVisitor(widening_thresholds,widening_thresholds_incl_mul2,widening_thresholds_float) = object
  inherit nopCilVisitor

  method! vexpr e =
    match e with
    | Const (CInt(i,ik,_)) ->
      widening_thresholds := Thresholds.add i !widening_thresholds;
      widening_thresholds_incl_mul2 := Thresholds.add i !widening_thresholds_incl_mul2;
      (* Adding double value of all constants so that we can establish for single variables that they are <= const *)
      (* This is e.g. needed for Apron. Done here where we still have the set representation to avoid expensive    *)
      (* deduplication and sorting on a list later *)
      widening_thresholds_incl_mul2 := Thresholds.add (Z.mul (Z.of_int 2) i) !widening_thresholds_incl_mul2;
      DoChildren
    | Const (CReal(f,FDouble,_)) -> (**TODO: include other fkinds, if we support them *)
      (** Only add finite constants. Infinite ones (+-inf/nan) are not useful for threshold_widening*)
      if Float.is_finite f then widening_thresholds_float := ThresholdsFloat.add f !widening_thresholds_float;
      DoChildren
    | _ -> DoChildren
end

let widening_thresholds = ResettableLazy.from_fun (fun () ->
    let set = ref Thresholds.empty in
    let set_incl_mul2 = ref Thresholds.empty in
    let set_float = ref ThresholdsFloat.empty in
    let thisVisitor = new extractConstantsVisitor(set,set_incl_mul2,set_float) in
    visitCilFileSameGlobals thisVisitor (!Cilfacade.current_file);
    Thresholds.elements !set, Thresholds.elements !set_incl_mul2, ThresholdsFloat.elements !set_float)

let thresholds () =
  let (ti,_,_) =  ResettableLazy.force widening_thresholds
  in ti

let thresholds_incl_mul2 () =
  let (_,ti_incl_mul2,_) =  ResettableLazy.force widening_thresholds
  in ti_incl_mul2

let thresholds_float () =
  let (_,_,tf) =  ResettableLazy.force widening_thresholds
  in tf

let reset_lazy () =
  ResettableLazy.reset widening_thresholds
