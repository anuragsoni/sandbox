module T = struct
  type t = Unix.file_descr

  let equal (a : Unix.file_descr) b = a = b
  let hash (a : Unix.file_descr) = Hashtbl.hash a
end

include Hashtbl.Make (T)