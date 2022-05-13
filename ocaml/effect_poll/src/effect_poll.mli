(** [fork] spawns a new virtual thread, and does not wait for it to complete. *)
val fork : (unit -> unit) -> unit

(** [wait_read] returns when a file_descriptor is ready for reading. *)
val wait_read : Unix.file_descr -> unit

(** [wait_write] returns when a file_descriptor is ready for writing. *)
val wait_write : Unix.file_descr -> unit

val read : Unix.file_descr -> bytes -> pos:int -> len:int -> [ `Ok of int | `Eof ]
val write : Unix.file_descr -> bytes -> pos:int -> len:int -> [ `Ok of int | `Eof ]
val accept : ?cloexec:bool -> Unix.file_descr -> Unix.file_descr * Unix.sockaddr

(** [run] is the entrypoint of the event loop. It accepts a user provided callback and
    runs it to completion. The user callback can spawn async io operations, and spawn new
    fibers. *)
val run : (unit -> unit) -> unit
