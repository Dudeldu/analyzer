type round_mode =
  | Nearest
  | ToZero
  | Up
  | Down

module type CFloatType = sig
  type t

  val zero: t
  val upper_bound: t
  val lower_bound: t

  val of_float: round_mode -> float -> t
  val to_float: t -> float option
  val to_big_int: t -> Big_int_Z.big_int

  val is_finite: t -> bool
  val pred: t -> t
  val succ: t -> t

  val arbitrary: unit -> t QCheck.arbitrary
  val equal: t -> t -> bool
  val compare: t -> t -> int
  val to_yojson: t -> Yojson.Safe.t
  val to_string: t -> string

  val neg: t -> t
  val add: round_mode -> t -> t -> t
  val sub: round_mode -> t -> t -> t
  val mul: round_mode -> t -> t -> t
  val div: round_mode -> t -> t -> t
  val atof: round_mode -> string -> t
end

let big_int_of_float f =
  let x, n = Float.frexp f in
  let shift = min 52 n in
  let x' = x *. Float.pow 2. (Float.of_int shift) in
  Big_int_Z.mult_big_int
    (Big_int_Z.big_int_of_int64 (Int64.of_float x'))
    (Big_int_Z.power_int_positive_int 2 (n - shift))

module CDouble = struct
  type t = float [@@deriving eq, ord, to_yojson]

  let zero = Float.zero
  let upper_bound = Float.max_float
  let lower_bound = -. Float.max_float

  let of_float _ x = x
  let to_float x = Some x
  let to_big_int = big_int_of_float

  let is_finite = Float.is_finite
  let pred = Float.pred
  let succ = Float.succ

  let arbitrary () = QCheck.float
  let to_string = Float.to_string

  let neg = Float.neg
  external add: round_mode -> t -> t -> t = "add_double"
  external sub: round_mode -> t -> t -> t = "sub_double"
  external mul: round_mode -> t -> t -> t = "mul_double"
  external div: round_mode -> t -> t -> t = "div_double"

  external atof: round_mode -> string -> t = "atof_double"
end

module CFloat = struct
  type t = float [@@deriving eq, ord, to_yojson]

  let zero = Float.zero
  let upper_bound = 3.402823e+38
  let lower_bound = -. 3.402823e+38

  let to_float x = Some x
  let to_big_int = big_int_of_float

  let is_finite = Float.is_finite
  let pred = Float.pred
  let succ = Float.succ

  let arbitrary () = QCheck.float
  let to_string = Float.to_string

  let neg = Float.neg
  external add: round_mode -> t -> t -> t = "add_float"
  external sub: round_mode -> t -> t -> t = "sub_float"
  external mul: round_mode -> t -> t -> t = "mul_float"
  external div: round_mode -> t -> t -> t = "div_float"

  external atof: round_mode -> string -> t = "atof_float"

  let of_float mode x = add mode zero x
end
