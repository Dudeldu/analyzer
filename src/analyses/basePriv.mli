open Cil
(* Cannot use local module substitutions because ppx_import is still stuck at 4.07 AST: https://github.com/ocaml-ppx/ppx_import/issues/50#issuecomment-775817579. *)

module type S =
sig
  module G: Lattice.S

  val read_global: Queries.ask -> (varinfo -> G.t) -> BaseDomain.BaseComponents.t -> varinfo -> BaseDomain.VD.t
  val write_global: Queries.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> BaseDomain.BaseComponents.t -> varinfo -> BaseDomain.VD.t -> BaseDomain.BaseComponents.t

  val lock: Queries.ask -> (varinfo -> G.t) -> BaseDomain.BaseComponents.t -> LockDomain.Addr.t -> BaseDomain.BaseComponents.t
  val unlock: Queries.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> BaseDomain.BaseComponents.t -> LockDomain.Addr.t -> BaseDomain.BaseComponents.t

  val sync: Queries.ask -> (varinfo -> G.t) -> BaseDomain.BaseComponents.t -> [`Normal | `Join | `Return | `Init | `Thread] -> BaseDomain.BaseComponents.t * (varinfo * G.t) list

  val escape: Queries.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> BaseDomain.BaseComponents.t -> EscapeDomain.EscapedVars.t -> BaseDomain.BaseComponents.t
  val enter_multithreaded: Queries.ask -> (varinfo -> G.t) -> (varinfo -> G.t -> unit) -> BaseDomain.BaseComponents.t -> BaseDomain.BaseComponents.t

  val is_private: Queries.ask -> varinfo -> bool
end

val get_priv : unit -> (module S)
