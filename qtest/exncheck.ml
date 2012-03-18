(** convenience functions to test exceptional cases *)

let check exn_predicate f arg =
  try ignore (f arg); false
  with exn -> exn_predicate exn

let is_invalid_argument = function
  | Invalid_argument _ -> true
  | _ -> false

let check_invalid_arg f =
  check (function Invalid_argument _ -> true | _ -> false) f
