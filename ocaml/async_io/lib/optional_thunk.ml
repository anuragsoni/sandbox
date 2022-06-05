type t = unit -> unit

let none = Sys.opaque_identity (fun () -> ())

let some fn =
  if none == fn then invalid_arg "Cannot create an Optional_thunk with a none value";
  fn
;;

let is_none fn = fn == none
let is_some fn = not (is_none fn)
let call_if_some fn = if is_some fn then fn ()
let unsafe_call fn = fn ()