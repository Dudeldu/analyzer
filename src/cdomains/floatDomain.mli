(** Abstract Domains for floats. These are domains that support the C
  * operations on double/float values. *)

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

  val to_int : Cil.ikind -> t -> IntDomain.IntDomTuple.t

  val of_const : float -> t
  val of_interval : float * float -> t
  val of_string : string -> t
  val of_int: IntDomain.IntDomTuple.t -> t

  val ending : float -> t
  val starting : float -> t

  val minimal: t -> float option
  val maximal: t -> float option

  val is_exact : t -> bool
end

(* Only required for testing *)
module F64Interval : sig
  include FloatDomainBase
end
module F32Interval : sig
  include FloatDomainBase
end

module type FloatDomain = sig
  include Lattice.S
  include FloatArith with type t := t

  val to_int : Cil.ikind -> t -> IntDomain.IntDomTuple.t
  val cast_to : Cil.fkind -> t -> t

  val of_const : Cil.fkind -> float -> t
  val of_interval : Cil.fkind -> float*float -> t
  val of_string : Cil.fkind -> string -> t
  val of_int: Cil.fkind -> IntDomain.IntDomTuple.t -> t

  val top_of: Cil.fkind -> t
  val bot_of: Cil.fkind -> t

  val ending : Cil.fkind -> float -> t
  val starting : Cil.fkind -> float -> t

  val minimal: t -> float option
  val maximal: t -> float option

  val is_exact : t -> bool
  val precision : t -> Cil.fkind
end

module FloatDomTupleImpl : sig
  include FloatDomain
end
